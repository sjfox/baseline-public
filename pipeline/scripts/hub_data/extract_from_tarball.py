"""Stream a GitHub repository tarball (stdin) and extract files from one subdirectory.

Usage:
  curl -sL https://codeload.github.com/OWNER/REPO/tar.gz/SHA | \
    python3 extract_from_tarball.py --subdir model-output --dest DIR \
      [--date-from YYYY-MM-DD] [--date-to YYYY-MM-DD] [--grep TEXT]

--date-from/--date-to keep only files whose name starts with a date in that range.
--grep keeps only the header line and lines containing TEXT (text files only); this keeps the
large COVID-19 Forecast Hub archive manageable.
"""
import argparse, os, re, sys, tarfile

p = argparse.ArgumentParser()
p.add_argument("--subdir", required=True)
p.add_argument("--dest", required=True)
p.add_argument("--date-from")
p.add_argument("--date-to")
p.add_argument("--grep")
a = p.parse_args()

grep = a.grep.encode() if a.grep else None
date_re = re.compile(r"^(\d{4}-\d{2}-\d{2})")
n = 0
with tarfile.open(fileobj=sys.stdin.buffer, mode="r|gz") as tf:
    for m in tf:
        if not m.isfile():
            continue
        parts = m.name.split("/", 2)              # <repo>-<sha>/<subdir>/<rest>
        if len(parts) < 3 or parts[1] != a.subdir:
            continue
        rel = parts[2]
        d = date_re.match(os.path.basename(rel))
        if a.date_from or a.date_to:
            if not d:
                continue
            if a.date_from and d.group(1) < a.date_from:
                continue
            if a.date_to and d.group(1) > a.date_to:
                continue
        out_path = os.path.join(a.dest, rel)
        os.makedirs(os.path.dirname(out_path), exist_ok=True)
        src = tf.extractfile(m)
        with open(out_path + ".part", "wb") as out:
            if grep:
                first = True
                for line in src:
                    if first or grep in line:
                        out.write(line)
                        first = False
            else:
                out.write(src.read())
        os.replace(out_path + ".part", out_path)
        n += 1
        if n % 500 == 0:
            print(f"{n} files", flush=True)
print(f"extracted {n} files from {a.subdir}/", flush=True)
