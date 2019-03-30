module Logic.Basic where

-- Ex2. bitwise AND, OR, XOR
data Bit = Z | O deriving (Show, Read)

bitwiseAnd :: [Bit] -> [Bit] -> [Bit]
bitwiseAnd [] bits = replicate (length bits) Z
bitwiseAnd bits [] = replicate (length bits) Z
bitwiseAnd (x:xs) (y:ys) = singleAnd x y : bitwiseAnd xs ys 
  where singleAnd O O = O
        singleAnd _ _ = Z

bitwiseOr :: [Bit] -> [Bit] -> [Bit]
bitwiseOr [] bits = bits
bitwiseOr bits [] = bits
bitwiseOr (x:xs) (y:ys) = singleOr x y : bitwiseOr xs ys 
  where singleOr Z Z = Z
        singleOr _ _ = O

bitFlip :: Bit -> Bit
bitFlip Z = O
bitFlip O = Z

bitwiseXor :: [Bit] -> [Bit] -> [Bit]
bitwiseXor [] bits = map bitFlip bits
bitwiseXor bits [] = map bitFlip bits
bitwiseXor (x:xs) (y:ys) = singleXor x y : bitwiseXor xs ys
  where singleXor Z O = O
        singleXor O Z = O
        singleXor _ _ = Z



