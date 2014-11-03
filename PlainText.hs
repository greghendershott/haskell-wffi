module PlainText(
  blocksToPlainText,
  inlinesToPlainText
) where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Writers.Markdown

blocksToPlainText :: [Block] -> String
blocksToPlainText bs = writePlain def (Pandoc nullMeta bs)

inlinesToPlainText :: [Inline] -> String
inlinesToPlainText is = writePlain def (Pandoc nullMeta [(Plain is)])
