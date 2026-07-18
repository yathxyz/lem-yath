#!/usr/bin/env python3
"""Network-free notmuch subset for the real-TUI mail acceptance test."""

from __future__ import annotations

import json
import os
import re
import sys
from pathlib import Path


LOG = Path(os.environ["LEM_YATH_NOTMUCH_LOG"])
STATE = Path(os.environ["LEM_YATH_NOTMUCH_STATE"])


def log(args: list[str]) -> None:
    with LOG.open("a") as stream:
        stream.write(json.dumps(args) + "\n")


def state() -> dict:
    return json.loads(STATE.read_text())


def save(value: dict) -> None:
    STATE.write_text(json.dumps(value) + "\n")


def message(
    message_id: str, sender: str, subject: str, body: list[dict], tags: list[str]
) -> dict:
    return {
        "id": message_id,
        "tags": tags,
        "headers": {
            "From": sender,
            "To": "Yanni <yanni@example.invalid>",
            "Date": "Wed, 15 Jul 2026 20:00:00 +0100",
            "Subject": subject,
        },
        "body": body,
    }


THREAD_MESSAGES = {
    "alpha": ["alpha@example.invalid"],
    "beta": [
        "payment+safe;touch PWNED@example.invalid",
        "reply/second?value@example.invalid",
    ],
}


def decoded_query_value(value: str) -> str:
    return json.loads(f'"{value}"')


def thread_from_query(query: str) -> str | None:
    match = re.fullmatch(r'thread:"((?:\\.|[^"\\])*)"', query)
    return decoded_query_value(match.group(1)) if match else None


def message_ids_from_query(query: str) -> list[str]:
    thread_id = thread_from_query(query)
    if thread_id is not None:
        return THREAD_MESSAGES.get(thread_id, [])
    return [
        decoded_query_value(match)
        for match in re.findall(r'id:"((?:\\.|[^"\\])*)"', query)
    ]


def show_tree(thread_id: str, data: dict) -> list:
    tags = data["tags"]
    if thread_id != "beta":
        first = message(
            "alpha@example.invalid",
            "Alice <alice@example.invalid>",
            "First thread",
            [{"content-type": "text/plain", "content": "First plain body."}],
            tags["alpha@example.invalid"],
        )
        return [[[first, []]]]
    original = message(
        "payment+safe;touch PWNED@example.invalid",
        "Bob <bob@example.invalid>",
        "Second thread",
        [
            {
                "content-type": "multipart/mixed",
                "content": [
                    {"content-type": "text/plain", "content": "Primary plain body."},
                    {"content-type": "text/html", "content": "<p>ignored html</p>"},
                    {
                        "id": 7,
                        "content-type": "application/pdf",
                        "content-disposition": "attachment",
                        "filename": "quarterly report;safe.pdf",
                        "content-length": 1024,
                    },
                ],
            }
        ],
        tags["payment+safe;touch PWNED@example.invalid"],
    )
    reply = message(
        "reply/second?value@example.invalid",
        "Yanni <yanni@example.invalid>",
        "Re: Second thread",
        [{"content-type": "text/plain", "content": "Reply plain body."}],
        tags["reply/second?value@example.invalid"],
    )
    return [[[original, [[reply, []]]]]]


def thread_tags(data: dict, thread_id: str) -> list[str]:
    visible = [
        data["tags"][message_id]
        for message_id in THREAD_MESSAGES[thread_id]
        if "deleted" not in data["tags"][message_id]
    ]
    result = {tag for tags in visible for tag in tags}
    preferred = ["inbox", "unread", "flagged", "deleted"]
    return [tag for tag in preferred if tag in result] + sorted(result - set(preferred))


def apply_tag_changes(data: dict, query: str, changes: list[str]) -> None:
    for message_id in message_ids_from_query(query):
        tags = data["tags"][message_id]
        for change in changes:
            operation, tag = change[0], change[1:]
            if operation == "+" and tag not in tags:
                tags.append(tag)
            elif operation == "-" and tag in tags:
                tags.remove(tag)


def main() -> int:
    args = sys.argv[1:]
    log(args)
    if not args:
        return 2
    data = state()
    if args[0] == "search":
        query = args[-1]
        data["searches"] += 1
        save(data)
        if query == "tag:empty":
            print("[]")
            return 0
        refreshed = " refreshed" if data["searches"] > 1 else ""
        rows = [
            {
                "thread": "alpha",
                "date_relative": "today",
                "authors": "Alice",
                "subject": "First thread",
                "tags": thread_tags(data, "alpha"),
            },
            {
                "thread": "beta",
                "date_relative": "yesterday",
                "authors": "Bob",
                "subject": f"Second thread{refreshed}",
                "tags": thread_tags(data, "beta"),
            },
        ]
        rows = [row for row in rows if row["tags"]]
        if "tag:inbox" in query:
            rows = [row for row in rows if "inbox" in row["tags"]]
        print(json.dumps(rows))
        return 0
    if args[0] == "show":
        if any(argument.startswith("--part=") for argument in args):
            success = [
                "show",
                "--format=raw",
                "--part=7",
                'id:"payment+safe;touch PWNED@example.invalid"',
            ]
            if args == success:
                sys.stdout.buffer.write(
                    Path(os.environ["LEM_YATH_NOTMUCH_PDF"]).read_bytes()
                )
                return 0
            if args == [
                "show", "--format=raw", "--part=8", 'id:"bad@example.invalid"'
            ]:
                sys.stdout.buffer.write(b"not a pdf")
                return 0
            if args == [
                "show", "--format=raw", "--part=9", 'id:"slow@example.invalid"'
            ]:
                import time

                time.sleep(5)
                sys.stdout.buffer.write(
                    Path(os.environ["LEM_YATH_NOTMUCH_PDF"]).read_bytes()
                )
                return 0
            print("unexpected raw-part invocation", file=sys.stderr)
            return 2
        thread_id = thread_from_query(args[-1])
        if thread_id is None:
            print("show requires an exact thread query", file=sys.stderr)
            return 2
        print(json.dumps(show_tree(thread_id, data)))
        return 0
    if args[0] == "tag":
        try:
            separator = args.index("--")
            changes = args[1:separator]
            query = args[separator + 1]
        except (ValueError, IndexError):
            print("malformed tag invocation", file=sys.stderr)
            return 2
        if not changes or any(
            len(change) < 2 or change[0] not in "+-" for change in changes
        ):
            print("invalid tag changes", file=sys.stderr)
            return 2
        if "+failtag" in changes:
            print("injected tag failure", file=sys.stderr)
            return 9
        apply_tag_changes(data, query, changes)
        save(data)
        return 0
    if args == ["new"]:
        data["news"] += 1
        save(data)
        print("Processed 1 new message.")
        return 0
    print("unsupported fake notmuch invocation", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
