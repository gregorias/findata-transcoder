module Data.Text.Trim (
  trim,
  unindent,
) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Relude

-- | Remove smartly indentation from a multiline string.
--
-- >>> unindent "  func() {\n    return 0;\n  }\n"
-- "func() {\n  return 0;\n}\n"
--
-- >>> unindent "func() {\n    return 0;\n  }\n"
-- "func() {\n    return 0;\n  }\n"
--
-- >>> unindent "\n"
-- "\n"
unindent :: Text -> Text
unindent s =
  let textLines = lines s
      minimumLinesIndent = minimumIndent s
      unindentedLines = case minimumLinesIndent of
        Just indent -> T.drop indent <$> textLines
        Nothing -> textLines
   in T.unlines unindentedLines

-- | Trims whitespace from the provided text.
--
-- >>> trim "\tHello World!  "
-- "Hello World!"
trim :: Text -> Text
trim = T.reverse . dropSpaces . T.reverse . dropSpaces

minimumIndent :: Text -> Maybe Int
minimumIndent =
  listToMaybe . sort . map lineIndent
    . filter (not . T.null . dropSpaces)
    . lines

-- | Amount of preceding spaces on first line
lineIndent :: Text -> Int
lineIndent = T.length . T.takeWhile isSpace

dropSpaces :: Text -> Text
dropSpaces = T.dropWhile isSpace
