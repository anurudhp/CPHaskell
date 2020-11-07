#!/bin/bash

# Generate blogs in `blogs/` to `docs/`
# `docs/` is rendered by GH-pages

output_md_format=markdown
input_dir=blogs
output_dir=docs

blogs=`ls $input_dir/*.lhs`

for blog in $blogs ; do
  output=$output_dir/`basename -s .lhs $blog`.md

  # log
  echo -n "Compiling $blog to $output... "

  # create empty file
  echo -n "" > $output

  # append YAML metadata (which is dropped by pandoc)
  runhaskell scripts/getYAMLMetadata.hs $blog >> $output
  echo "" >> $output

  # convert
  pandoc --from markdown+lhs --to $output_md_format $blog >> $output

  # fix code tags
  sed -i "s/sourceCode literate haskell/haskell/g" $output
  sed -i 's#{.sourceCode .literate .haskell}#haskell#g' $output
  sed -i 's#{.haskell}#haskell#g' $output
  
  # log
  echo "done."
done

echo -n "generating index... "
cp blogs/index.md docs/index.md
echo "done."

