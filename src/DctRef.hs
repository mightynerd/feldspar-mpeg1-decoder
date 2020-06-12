-- A pure haskell implementation of the DCT and inverse DCT
-- directly taken from IEEE standard 1180
--

{-# LANGUAGE TypeSynonymInstances #-}

module DctRef where
  
import Data.List

c :: Int -> Double
c 0 = 1 / (sqrt 2)
c u | u > 7 || u < 0 = error "c : index out of range"
    | otherwise      = 1

dct :: [[Int]] -> [[Int]]
dct pxs = [[ clamp . round $ x u v | v <- [0..7]] | u <- [0..7]]
  where
    x u v = (1/4) * c u * c v * sums
      where
        sums = sum [sum [prod i j | j <- [0..7]] | i <- [0..7]]
        prod i j = px * cos ((fromIntegral ((2*i + 1) * u) * pi)/16)
                      * cos ((fromIntegral ((2*j + 1) * v) * pi)/16)
          where px = fromIntegral $ pxs !! i !! j
    clamp x | x >  2047 =  2047
            | x < -2048 = -2048
            | otherwise = x


idct :: [[Int]] -> [[Int]]
idct coeffs = [[ clamp . round $ x i j | j <- [0..7]] | i <- [0..7]]
  where
    x i j = (1/4) * sum [sum [prod u v | u <- [0..7]] | v <- [0..7]]
      where
        prod u v = c u * c v * coeff
                   * cos ((fromIntegral ((2*i + 1) * u) * pi)/16)
                   * cos ((fromIntegral ((2*j + 1) * v) * pi)/16)
          where coeff = fromIntegral $ coeffs !! u !! v
    clamp x | x >  255 =  255
            | x < -256 = -256
            | otherwise = x

tstDctInput :: [[Int]]
tstDctInput = [ [1,2,3,4,5,6,7 ,8]
              , [9,8,7,6,5,4,3 ,2]
              , [1,2,3,4,5,6,7 ,8]
              , [9,8,7,6,5,4,3 ,2]
              , [1,2,3,4,5,6,7 ,8]
              , [9,8,7,6,5,4,3 ,2]
              , [1,2,3,4,5,6,77,8]
              , [9,8,7,6,5,4,3 ,2]
              ]

dctTest  = putStrLn $ ppMatrix 5 $ dct  tstDctInput
idctTest = putStrLn $ ppMatrix 5 $ idct tstDctInput

invTest = putStrLn $ ppMatrix 5 $ idct . dct $ tstDctInput



type Matrix a = [[a]]

-- | multiply matrix with scalar
(..*) :: Num a => a -> Matrix a -> Matrix a
a ..* m = map2 (a *) m

map2 :: (a -> b) -> Matrix a -> Matrix b
map2 = map . map

infixl 4 ..$
(..$) = map2

-- instance Functor Matrix where
--   fmap = map2


numMatrix :: Int -> Int -> Matrix Int
numMatrix n m = [[i * m + j | j <- [0..m-1]] | i <- [0..n-1]]

numMatrixCol :: Int -> Int -> Matrix Int
numMatrixCol n m = transpose $ numMatrix m n

printMat :: Show a => Matrix a -> IO ()
printMat = putStr . ppMatrix 5

ppMatrix :: Show a => Int -> [[a]] -> String
ppMatrix maxDec m = unlines . map unwords $ padded
  where shown     = (map . map) (trimDec . show) m
        padded    = transpose $ [map (pad w) row | row <- transpose shown
                                                , let w = maximum $ map length row]
        pad w str = replicate (w - length str + 1) ' ' ++ str
        trimDec s = pre ++ take maxDec post
          where pre  = takeWhile (/= '.') s
                post = drop (length pre) s



-- [ [1 2 3 4 5 6 7  8]
--   [9 8 7 6 5 4 3  2]
--   [1 2 3 4 5 6 7  8]
--   [9 8 7 6 5 4 3  2]
--   [1 2 3 4 5 6 7  8]
--   [9 8 7 6 5 4 3  2]
--   [1 2 3 4 5 6 77 8]
--   [9 8 7 6 5 4 3  2]
-- ]
