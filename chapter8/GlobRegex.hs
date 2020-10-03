module  GlobRegex
(
    globToRegex,
    matchesGlob
) where

import TextRegex.Posix ((=~))

globToRegex' :: String -> String
globToRegex' :: "" = ""
globToRegex' ('*':cs) = ".*" ++ globtToRegex' cs
globToRegex' ('?':cs) = ',' : globToRegex' cs
globToRegex' ('[':'!':cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

