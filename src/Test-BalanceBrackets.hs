import System.Environment
import Control.Monad
import Data.Maybe
-- stolen from the internet and adapted for tcl
-- Return whether a string contains balanced brackets.  Nothing indicates a
-- balanced string, while (Just i) means an imbalance was found at, or just
-- after, the i'th bracket.  We assume the string contains only brackets.
isBalanced :: Char -> Char -> String -> Maybe String
isBalanced openc closec = bal (-1) 0
  where
    bal :: Int -> Int -> String -> Maybe String
    bal _ 0 [] = Nothing
    bal i _ [] = Just $ "Opening bracket unmatched until end of command." -- unmatched opening
    bal i (-1) _ = Just $ "Unmatched closing bracket at position " ++ show i -- unmatched close
    bal i n (singlec:bs)
      | singlec == openc = bal (i + 1) (n + 1) bs
      | singlec == closec = bal (i + 1) (n - 1) bs
      | singlec == '\\' = case bs of
          (sc:rs) -> if sc == openc || sc == closec then bal (i+2) n rs else bal (i+1) n rs 
      | otherwise = bal (i+1) n bs    

gnarlyBalanced = isBalanced '{' '}'
-- it's better not to check for double quotes and square brackets I think since they can be escaped and not used internally for pub:tcl:perform...

squareBalanced = isBalanced '[' ']'

dquoteBalanced = isBalanced '"' '"'

main = getArgs >>= (mapM (putStrLn . show . (isBalanced '(' ')') ))
