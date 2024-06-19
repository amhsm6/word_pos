import qualified Data.Map as M

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

wordPos :: String -> Integer
wordPos = go $ \_ x -> x
    where go :: (M.Map Char Integer -> Integer -> Integer) -> String -> Integer
          go f "" = f M.empty 1
          go f (x:xs) = go f' xs
              where f' table y = f table' y'
                        where y' = vars + y
                              vars = sum $ map (\x' -> variations $ M.adjust (\x -> x - 1) x' table') $ takeWhile (/=x) $ M.keys table'
                              variations t = factorial (sum $ M.elems t) `div` product (map factorial $ M.elems t)

                              table' = M.alter g x table
                              g Nothing = Just 1
                              g (Just x) = Just $ x + 1

main :: IO ()
main = getLine >>= print . wordPos
