#!/bin/bash

# Generate blogs in `blogs/` to `docs/`
# `docs/` is rendered by GH-pages

output_md_format=gfm
blogs=`ls blogs/*.lhs`

for blog in $blogs ; do
  output=docs/`basename -s .lhs $blog`.md

  # log
  echo -n "Compiling $blog to $output... "

  # create empty file
  echo "" > $output

  # append YAML metadata (which is dropped by pandoc)
  runhaskell scripts/getYAMLMetadata.hs $blogs >> $output

  # convert
  pandoc --from markdown+lhs --to $output_md_format $blog \
    | sed "s/sourceCode literate haskell/haskell/g" >> $output
  
  # log
  echo "done."
done

cp blogs/index.md docs/index.md
