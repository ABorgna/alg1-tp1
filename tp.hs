import Data.Char (chr,isAsciiLower,ord)

-- | 1
-- Funciones auxiliares
esMin :: Char -> Bool 
esMin c = isAsciiLower c

letANat :: Char -> Integer
letANat c | c `elem` ['a'..'z'] = fromIntegral $ ord c - ord 'a'
          | otherwise = 0

natALet :: Integer -> Char
natALet n | 0 <= n && n <= 26 = chr $ fromIntegral n + ord 'a'

desplazar :: Integer -> Char -> Char
desplazar n c = natALet $ (n + letANat c) `mod` 27

cantMinusc :: String -> Integer
cantMinusc s = fromIntegral $ length $ filter esMin s

contar :: Char -> String -> Integer
contar c s = fromIntegral $ length $ filter (==c) s

-- | 2
-- Encriptar con cifrado Cesar
codificar :: Integer -> String -> String
codificar = undefined

-- | 3
decodificar :: Integer -> String -> String
decodificar = undefined

-- | 4
frec :: String -> [Float]
frec = undefined

-- | 5
rotar :: Integer -> [a] -> [a]
rotar = undefined

-- | 6
-- Distancia
chi2 :: [Float] -> [Float] -> Float
chi2 [] _ = 0
chi2 _ [] = 0
chi2 (x:xs) (y:ys) | length (x:xs) == length (y:ys) = ((x-y)^2)/y + chi2 xs ys

-- | 7
descifrar :: String -> String
descifrar = undefined

