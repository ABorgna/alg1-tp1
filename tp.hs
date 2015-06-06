
-- | 1
-- Funciones auxiliares
esMin :: Char -> Bool
esMin = undefined

letANat :: Char -> Integer
letANat = undefined

natALet :: Integer -> Char
natALet = undefined

desplazar :: Integer -> Char -> Char
desplazar = undefined

cantMinusc :: String -> Integer
cantMinusc = undefined

contar :: Char -> String -> Integer
contar = undefined

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

