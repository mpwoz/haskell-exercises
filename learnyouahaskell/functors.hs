import Data.Char
import Data.List

reverseLine = do line' <- fmap reverse getLine
                 putStrLn $ "You said " ++ line' ++ " backwards!"
                 putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"


dash = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
