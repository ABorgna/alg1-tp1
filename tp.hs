import Data.Char (chr,isAsciiLower,ord)

-- | 1
-- Funciones auxiliares
esMin :: Char -> Bool 
esMin c = isAsciiLower c

letANat :: Char -> Integer
letANat c | c `elem` ['a'..'z'] = fromIntegral $ ord c - ord 'a'
          | otherwise = 0

natALet :: Integer -> Char
natALet n | 0 <= n && n <= 25 = chr $ fromIntegral n + ord 'a'

desplazar :: Integer -> Char -> Char
desplazar n c = natALet $ (n + letANat c) `mod` 26

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
-- Calcula la frecuencia porcentual de una letra minuscula l en un string
frecLenS :: Char -> String -> Float
frecLenS l s = 100 * (fromIntegral (contar l s) / fromIntegral (cantMinusc s))

-- Calcula la frecuencia porcentual de cada letra minuscula de un primer string en un segundo string
frecSenS :: String -> String -> [Float]
frecSenS [] s2 = []
frecSenS (l:ls) s2 = frecLenS l s2 : frecSenS ls s2

frec :: String -> [Float]
frec s = frecSenS ['a'..'z'] s

-- | 5
rotar :: Integer -> [a] -> [a]
rotar 0 l = l
rotar 1 (l:ls) = ls ++ [l]
rotar n l = rotar 1 (rotar (n-1) l)

-- | 6
-- Distancia
chi2 :: [Float] -> [Float] -> Float
chi2 [] _ = 0
chi2 _ [] = 0
chi2 (x:xs) (y:ys) | length (x:xs) == length (y:ys) = ((x-y)^2)/y + chi2 xs ys

-- | 7
descifrar :: String -> String
descifrar = undefined

