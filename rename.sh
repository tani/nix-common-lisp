#!/usr/bin/env bash
set -eu -o pipefail
OLDNAME="fibonacci"
NEWNAME="fibonacci"
find . -type f -not -name "rename.sh" -exec rename "$OLDNAME" "$NEWNAME" {} \;
find . -type f -exec rename "$OLDNAME" "$NEWNAME" {} \;
