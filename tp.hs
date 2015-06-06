import Data.Char (chr,isAsciiLower,ord)
import Data.List (sortBy)

-- | 1
-- Funciones auxiliares
esMin :: Char -> Bool 
esMin c = isAsciiLower c

letANat :: Char -> Integer
letANat c | c `elem` ['a'..'z'] = fromIntegral $ ord c - ord 'a'

natALet :: Integer -> Char
natALet n | 0 <= n && n < 26 = chr $ fromIntegral n + ord 'a'

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
descifrar s = head $ sortBy compararDistancias strPosibles
    where decodificarCon n = decodificar n s

          -- Resultados decodificar con cada key posible
          strPosibles :: [String]
          strPosibles = map decodificarCon [0..25]

          compararDistancias s1 s2 = compare d1 d2
              where d1 = chi2 (frec s1) freqsEsp
                    d2 = chi2 (frec s2) freqsEsp

          -- Frecuencia promedio de cada letra en español
          freqsEsp = [12.52, 1.42, 4.67, 5.85, 13.67, 0.67, 1.01, 0.70, 6.24,
                      0.44, 0.01, 4.96, 3.15, 6.70, 8.67, 2.51, 0.88, 6.86, 
                      7.97, 4.62, 3.92, 0.90, 0.02, 0.22, 0.90, 0.52]
