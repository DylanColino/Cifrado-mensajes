module Solucion where
import Data.Char

-- Completar!
-- Nombre de grupo: {Los 4 fantasticos}
-- Integrante1: { 43248827, Colino Dylan Joel, dylancolino@gmail.com}
-- Integrante2: { 41328248, Gomez Cornejo Maria Agustina, mariagus098@gmail.com}
-- Integrante3: { 44595816, Tomas Urtasun, tomasurtasun819@gmail.com}
-- Integrante4: { 94657008, Marin Sahara Salome, candiotti0317@gmail.com}
-- Integrantes que abandonaron la materia: {}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula letra = ord letra >= 97 && ord letra <= 122

-- EJ 2
letraANatural :: Char -> Int
letraANatural letra = ord letra - ord 'a'

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar letra n | esMinuscula letra == False = letra
                  | 97 <= (ord letra + n) && (ord letra + n) <= 122 = chr (ord letra + n)
                  | 97 < (ord letra + n) = desplazar (chr 97) (ord letra + n - 123)
                  | otherwise = desplazar (chr 122) (ord letra + n - 96)

-- EJ 4 

cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n | esMinuscula x == True = desplazar x n : cifrar xs n 
                | otherwise = x : cifrar xs n

-- EJ 5 

descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (y:ys) n | esMinuscula y == True = desplazar y (-n) : descifrar ys n
                   | otherwise = y : descifrar ys n

--EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (x:xs) = cifrarListaConN (x:xs) 0
                where cifrarListaConN [] _ = []
                      cifrarListaConN (y:ys) n = cifrar y n : cifrarListaConN ys (n + 1)

--EJ 7 
frecuencia :: String -> [Float]
frecuencia palabra = frecuenciaAux palabra (chr 97)
      where frecuenciaAux palabra letra | letra > 'z' = []
                                        | otherwise = porcentaje palabra letra : frecuenciaAux palabra (chr(ord(letra) + 1))

porcentaje :: String -> Char -> Float
porcentaje palabra letra | cantidadMinusculas palabra == 0 = 0
                         | otherwise = (fromIntegral(contarCaracter palabra letra) / fromIntegral(cantidadMinusculas palabra)) * 100

--cuenta cuantas minusculas hay en una palabra
cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (x:xs) | esMinuscula x == False = cantidadMinusculas xs
                          | otherwise = 1 + cantidadMinusculas xs

--dada una letra, cuenta sus repeticiones en una palabra
contarCaracter :: String -> Char -> Int
contarCaracter [] _ = 0
contarCaracter (x:xs) letra | x == letra = 1 + contarCaracter xs letra
                            | otherwise = contarCaracter xs letra


-- EJ 8

cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente palabra n = (letraMasFrecuente(cifrar palabra n), maximo(frecuencia(cifrar palabra n)))
      where maximo [a] = a
            maximo (y:x:xs) | y < x = maximo (x:xs)
                            | y >= x = maximo (y:xs)


letraMasFrecuente :: String -> Char
letraMasFrecuente [a] = a
letraMasFrecuente (y:x:xs) | contarCaracter (y:x:xs) y >= contarCaracter (y:x:xs) x && esMinuscula y == True = letraMasFrecuente (y:xs) 
                           | otherwise = letraMasFrecuente (x:xs)


--EJ 9

esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 = esDescifradoAux s1 s2 0
      where esDescifradoAux s1 s2 n | n > 25 = False
                                    | s2 == cifrar s1 n = True 
                                    | otherwise = esDescifradoAux s1 s2 (n + 1)

--EJ 10

todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:lista) = crearTupla x lista ++ igualCifrado x ++ todosLosDescifrados lista

crearTupla :: String -> [String] -> [(String, String)]
crearTupla palabra [] = []
crearTupla palabra (x:lista) | esDescifrado palabra x == True = (palabra, x) : (x, palabra) : crearTupla palabra lista
                             | otherwise = crearTupla palabra lista

igualCifrado :: String -> [(String, String)]
igualCifrado s1 | cifrar s1 5 == s1 = [(s1, s1)]
                 | otherwise = []

--EJ 11 

expandirClave :: String -> Int -> String
expandirClave palabra numero | length palabra == numero = palabra
                             | length palabra < numero = palabra ++ expandirClave palabra (numero - length palabra)
                             | otherwise = expandirClaveAux palabra numero
      where expandirClaveAux palabra numero | numero == 0 = ""
                                            | otherwise = head palabra : expandirClaveAux (tail palabra) (numero - 1)

-- EJ 12
-- La funcion auxiliar es para poder hacer recursion ya con la clave expandida

cifrarVigenere :: String -> String -> String
cifrarVigenere palabra clave = cifrarVigenereAux palabra (expandirClave clave (length palabra))
    where cifrarVigenereAux [] [] = []
          cifrarVigenereAux (x:palabra) (y:claveExp) = (desplazar x (letraANatural y)) : cifrarVigenereAux palabra claveExp

-- EJ 13
-- Idem comentario del EJ 12

descifrarVigenere :: String -> String -> String
descifrarVigenere palabra clave = descifrarVigenereAux palabra (expandirClave clave (length palabra))
    where descifrarVigenereAux [] [] = []
          descifrarVigenereAux (x:palabra) (y:claveExp) = (desplazar x (-letraANatural y)) : descifrarVigenereAux palabra claveExp

-- EJ 14 

peorCifrado :: String -> [String] -> String
peorCifrado palabra [x] = x
peorCifrado palabra (x:y:lista) | distancia palabra (cifrarVigenere palabra x) <= distancia palabra (cifrarVigenere palabra y) = peorCifrado palabra (x:lista)
                                | otherwise = peorCifrado palabra (y:lista)
      where distancia [] [] = 0
            distancia (x:xs) (y:ys) = absoluto(letraANatural x - letraANatural y) + distancia xs ys

-- En distancia se calcula la distancia de una palabra con su cifrado Vigenere

absoluto :: Int -> Int
absoluto numero | numero < 0 = numero * (-1)          
                | otherwise = numero


-- EJ 15

combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] [] palabra = []
combinacionesVigenere [] claves palabra = []
combinacionesVigenere (x:mensajes) claves palabra = auxiliar x claves palabra  ++ combinacionesVigenere mensajes claves palabra
      where auxiliar mensaje [] palabra = []
            auxiliar mensaje (x:lista) palabra | (cifrarVigenere mensaje x) == palabra = (mensaje, x) : auxiliar mensaje lista palabra
                                               | otherwise = auxiliar mensaje lista palabra
-- La funcion auxiliar es la encargada de crear la tupla

