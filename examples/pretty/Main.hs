module Main where

import Text.PrettyPrint

-- | A small record layout: a name, a nested list, and a double.
report :: Doc
report =
  text "config"
    <+> braces
      ( nest 2 $
          vcat
            [ text "name" <+> equals <+> doubleQuotes (text "aihc"),
              text "targets" <+> equals <+> brackets (hsep (punctuate comma (map text ["arm64", "llvm"]))),
              text "ratio" <+> equals <+> double 0.75,
              text "count" <+> equals <+> int 3
            ]
      )

main :: IO ()
main = do
  putStrLn (render report)
  putStrLn (renderStyle style {mode = OneLineMode} (hsep [text "one", text "line", parens (double 1.0e-2)]))
