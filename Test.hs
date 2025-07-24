import Solucion
import Test.HUnit

ejecutar = runTestTT

ej1 = test [
    "prueba 1" ~: (esMinuscula 'A') ~?= False,
    "prueba 2" ~: (esMinuscula 'd') ~?= True,
    "prueba 3" ~: (esMinuscula ' ') ~?= False
    ]

ej2 = test [
    "prueba 1" ~: (letraANatural 'b') ~?= 1,
    "prueba 2" ~: (letraANatural 'z') ~?= 25
    ]

ej3 = test [
    "prueba 1" ~: (desplazar 'A' 26) ~?= 'A',
    "prueba 2" ~: (desplazar ' ' 30) ~?= ' ',
    "prueba 3" ~: (desplazar 'a' 3) ~?= 'd',
    "prueba 4" ~: (desplazar 'z' 5) ~?= 'e',
    "prueba 5" ~: (desplazar 'l' 0) ~?= 'l',
    "prueba 6" ~: (desplazar 'a' (-1)) ~?= 'z',
    "prueba 7" ~: (desplazar 'a' (-26)) ~?= 'a'
    ]

ej4 = test [
    "prueba 1" ~: (cifrar "computaCIon" 3) ~?= "frpsxwdCIrq", 
    "prueba 2" ~: (cifrar "compu tacion" 3) ~?= "frpsx wdflrq", 
    "prueba 3" ~: (cifrar "COMPU tacion" 3) ~?= "COMPU wdflrq", 
    "prueba 4" ~: (cifrar "computacion" 0) ~?= "computacion",
    "prueba 5" ~: (cifrar "compu@gmail.com" 1) ~?= "dpnqv@hnbjm.dpn",
    "prueba 6" ~: (cifrar "introduccion a la programacion" 3) ~?= "lqwurgxfflrq d od surjudpdflrq"
    ] 

ej5 = test [
    "prueba 1" ~: (descifrar "frpsxwdflrq" 3) ~?= "computacion", 
    "prueba 2" ~: (descifrar "frpsx wdflrq" 3) ~?= "compu tacion", 
    "prueba 3" ~: (descifrar "COMPU wdflrq" 3) ~?= "COMPU tacion", 
    "prueba 4" ~: (descifrar "computacion" 0) ~?= "computacion",
    "prueba 5" ~: (descifrar "" 1) ~?= "",
    "prueba 6" ~: (descifrar "lqwurgxfflrq d od surjudpdflrq" 3) ~?= "introduccion a la programacion"
    ]

ej6 = test [
    "prueba 1" ~: (cifrarLista []) ~?= [],
    "prueba 2" ~: (cifrarLista ["computacion"]) ~?= ["computacion"],
    "prueba 3" ~: (cifrarLista ["compu", "labo", "intro"]) ~?= ["compu", "mbcp", "kpvtq"]
    ]
  
ej7 = test [
    "prueba 1" ~: (frecuencia "a") ~?=  [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "prueba 2" ~: (frecuencia "") ~?=  [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "prueba 3" ~: (frecuencia "ABCDE") ~?=  [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "prueba 4" ~: (frecuencia ",2!@") ~?=  [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    "prueba 5" ~: (frecuencia "abcdz") ~?=  [20.0,20.0,20.0,20.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,20.0],
    "prueba 6" ~: (frecuencia "taller") ~?= [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

ej8 = test [
    "prueba 1" ~: (cifradoMasFrecuente "taller" 3) ~?= ('o', 33.333336),
    "prueba 2" ~: (cifradoMasFrecuente "compu" (-2)) ~?= ('a', 20.0), 
    "prueba 3" ~: (cifradoMasFrecuente "codigo" 0) ~?= ('o', 33.333336),
    "prueba 4" ~: (cifradoMasFrecuente "3hola" 5) ~?= ('m', 25.0),
    "prueba 5" ~: (cifradoMasFrecuente "@52" 5) ~?= ('2', 0.0),
    "prueba 6" ~: (cifradoMasFrecuente "a" 0) ~?= ('a', 100.0)
    ]

ej9 = test [
    "prueba 1" ~: (esDescifrado "taller" "compu") ~?= False,
    "prueba 2" ~: (esDescifrado "compu" "compu") ~?= True,
    "prueba 3" ~: (esDescifrado "HOLA" "HOLA") ~?= True,
    "prueba 4" ~: (esDescifrado "frpsx" "compu") ~?= True,
    "prueba 5" ~: (esDescifrado "@513%#" "") ~?= False,
    "prueba 6" ~: (esDescifrado "" "") ~?= True
    ]


ej10 = test [
    "prueba 1" ~: (todosLosDescifrados ["hola6", "todGd","lspe6","5e8ffw"]) ~?=  [("hola6","lspe6"),("lspe6","hola6")],
    "prueba 2" ~: (todosLosDescifrados ["lxou", "iulr","frio"]) ~?=  [("lxou","iulr"),("iulr","lxou"),("lxou","frio"),("frio","lxou"),("iulr","frio"),("frio","iulr")],
    "prueba 3" ~: (todosLosDescifrados []) ~?=  [],
    "prueba 4" ~: (todosLosDescifrados ["lxou", "tisuem","mundo", "buzo"]) ~?=  [],
    "prueba 5" ~: (todosLosDescifrados ["MUNDO", "", "krl", "hetr"]) ~?=  [("MUNDO","MUNDO"),("","")],
    "prueba 6" ~: (todosLosDescifrados ["compu", "frpsx", "mywza"]) ~?=  [("compu", "frpsx"), ("frpsx", "compu")]
    ]


ej11 = test [
    "prueba 1" ~: (expandirClave "compu" 8) ~?= "compucom",
    "prueba 2" ~: (expandirClave "extraordinario" 10) ~?= "extraordin",
    "prueba 3" ~: (expandirClave "buenos" 6) ~?= "buenos",
    "prueba 4" ~: (expandirClave "a" 1) ~?= "a"
    ]

ej12 = test [
    "prueba 1" ~: (cifrarVigenere "computacion" "ip") ~?= "kdueciirqdv",
    "prueba 2" ~: (cifrarVigenere "" "ip") ~?= "",
    "prueba 3" ~: (cifrarVigenere "@3." "abc") ~?= "@3.",
    "prueba 4" ~: (cifrarVigenere "introduccion a la programacion" "compu") ~?= "kbfgifiorcqb p no elqudpgcqudh"
    ]

ej13 = test [
    "prueba 1" ~: (descifrarVigenere "kdueciirqdv" "ip") ~?= "computacion",
    "prueba 2" ~: (descifrarVigenere "" "ip") ~?= "",
    "prueba 3" ~: (descifrarVigenere "kbfgifiorcqb p no elqudpgcqudh" "compu") ~?= "introduccion a la programacion"
    ]

ej14 = test [
    "prueba 1" ~: (peorCifrado "computacion" ["ip", "asdef", "ksy"]) ~?= "asdef",
    "prueba 2" ~: (peorCifrado "" ["ip", "asdef", "ksy"]) ~?= "ip",
    "prueba 3" ~: (peorCifrado "a" ["ehs", "fsdue", "x"]) ~?= "ehs",
    "prueba 4" ~: (peorCifrado "a" ["g"]) ~?= "g"
    ]

ej15 = test [
    "prueba 1" ~: (combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb") ~?= [("hola", "b")],
    "prueba 2" ~: (combinacionesVigenere ["compu", ""] ["a", "b"] "dpnqv") ~?= [("compu", "b")],
    "prueba 3" ~: (combinacionesVigenere ["hola", ""] ["a", "b"] "") ~?= [("", "a"), ("", "b")],
    "prueba 4" ~: (combinacionesVigenere [] [] "compu") ~?= []
    ]
