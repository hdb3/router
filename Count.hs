
module Count where
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Word

l1 = [1,2,3,1,2,1]
main = do
    print l1
    print (count l1)

-- count :: Integral t => [t] -> Map.Map t Int
-- -- lazily maing types explicit rather than sprinklng fromINtergarl elsewhere
count :: [Word64] -> [(Word64,Word32)]
count ax = Map.toList $ List.foldl' f Map.empty ax
    where
    f m k = Map.alter f' k m where
        f' Nothing = Just 1
        f' (Just n) = Just (n+1)
    
{-
count [] = []
count (x:[]) = [(x,1)]
-}

