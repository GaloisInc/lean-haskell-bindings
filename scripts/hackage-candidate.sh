#!/bin/bash
set -e

user=TODO SET USER
exit 1

cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

echo "Detected package: $pkg-$ver"

dir=$(mktemp -d build-docs.XXXXXX)
# Delete directory once it is not needed.
trap 'rm -r "$dir"' EXIT

HYPERLINK_FLAG="--hyperlink-source"

LOCATION_FLAG=
cabal haddock --hoogle $HYPERLINK_FLAG --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

echo "Copying to directory $dir"
cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs

tar cvz -C $dir --format=ustar -f $dir/$pkg-$ver-docs.tar.gz $pkg-$ver-docs

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u "$user" \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/candidate/docs"
