"""Prints a macOS crash report's (.ips) exception and the top of the thread that crashed: the
libraries and functions it was in. Managed .NET frames have no name there, and show as "?"."""
import json
import sys

for path in sys.argv[1:]:
    try:
        text = open(path, encoding="utf-8").read()
        report = json.loads(text[text.index("\n") + 1 :])
        thread = report["threads"][report["faultingThread"]]
    except (OSError, ValueError, KeyError, IndexError) as error:
        print(f"  {path}: couldn't read it ({error})")
        continue
    images = report.get("usedImages", [])
    exception = report.get("exception", {})
    print(f"  {path}: {exception.get('type', '?')} {exception.get('signal', '')}, in {report.get('procName', '?')}")
    for frame in thread.get("frames", [])[:20]:
        index = frame.get("imageIndex")
        image = images[index].get("name", "?") if index is not None and index < len(images) else "?"
        print(f"    {image} {frame.get('symbol', '')}".rstrip())
