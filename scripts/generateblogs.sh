#!/bin/bash

# Generate blogs in `blogs/` to `docs/`
# `docs/` is rendered by GH-pages

blogs=`ls blogs/*.lhs`
for blog in $blogs ; do
  output=docs/`basename -s .lhs $blog`.md
  cp blogs/header.md $output
  pandoc --from markdown+lhs --to gfm $blog >> $output
  cat blogs/footer.md >> $output
done

cp blogs/index.md docs/index.md
