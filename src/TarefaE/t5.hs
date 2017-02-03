module Main where

import Data.Char

type Tabuleiro = [String]
type Coordenada = Int
type Coordenadas = (Coordenada,Coordenada)
type Orientacao = Char
type Comandos = String

t1 = ["aaa","aaa","aAa","1 2 S","AAL"]
t2 = ["aaa","Aca","aaa","2 1 S","ADAADAL"]
t3 = ["aba","aba","abA","0 2 S","AAESSL"]
t4 = ["aca","aBa","AaA","1 2 S","SLSEALDDAAL"]
t5 = ["abcc","aaac","aaaC","aaDC","0 3 E","SSADAALALDSLS"]
t6 = ["Aca","aca","aca","aca","aca","aaa","2 5 S","AAAAADAADAAAAAL"]
t7 = ["caccc","cacac","cccaC","0 2 S","AAEAAEAADAADAAL"]
t8 = ["BaBaB","BaBaB","BaBaB","BaBaB","4 3 S","LALALALDSSDLALALALESSELALALAL"]
t9 = ["BbBab","babab","BaBaB","baaab","BbBbB","4 4 S","AALAALDAALAALDAALAALDAALDAAL"]
t10 = ["BCDECDB","CaaDaaC","DaaCaaD","EDCBCDE","3 0 O","SLSLSLDSLSLSLDSLSLSLDSLSLSLDEESLSLSLESLSLSLESLSLSL"]
t11 = ["CBBC","BaaB","BaaB","CBBC","BaaB","BaaB","CBBC","0 0 N","LSLALSLSLALSDLSLALSDLSLALSDLSLALSESAASELLSLALSELSLALS"]
t12 = ["aCa","aca","aca","aee","bcd","aac","aaC","0 6 S","AAAASESSDSALEEASSEADSAAL"]
t13 = ["aaaaaa","BBBBBb","aaaaaa","5 1 O","ALALALALAL"]
t14 = ["cBaBc","BBaBB","aaaaa","aaaaa","aaaaa","BBaBB","cBaBc","0 2 N","SAASLDAELALDSAASLDAELALDSAASLDAELALDSAASLDAELALD"]
t15 = ["CdCdC","aaaad","aaaaC","aaaad","cdCdC","0 0 E","SSLSSLESSLSSLESSLSSL"]
t16 = ["AAA","AAA","AAA","AAA","AAA","AAA","AAA","0 6 E","LALALDADAADDLALALDADAADDLALALDADAADDLALALDADAADDLALALDADAADDLALALDADAADDLALALDADAADDLALAL"]
t17 = ["aBCD","aBCD","aBCD","aaaa","aBCD","aBCD","aBCD","0 6 E","SLSLSLDDSSSEAESLSLSLDDSSSEAESLSLSLDDSSSEAESLSLSLDDSSSEAESLSLSLDDSSSEAESLSLSLDDSSSEAESLSLSL"]
t18 = ["AAAcCC","AAAcCC","AAAcCC","AAAcCC","AAAcCC","AAABCC","0 5 S","LALALALALALAEAELALALALALALADADLALALALALALAESLSELALALALALALADADLALALALALAL"]
t19 = ["aacdD","aacaa","bbCaa","0 1 S","SEASLEAADSAL"]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr (tarefa (lines inp)))
     
tarefa :: [String] -> [String]
tarefa txt = prefixo ++ criaTab tab ++ robot tab coord orient ++ prefixoanimacao ++ 
             animacao tab (coordx,coordy) orient comandos ++ sufixoanimacao ++ sufixoRo ++ sufixo
 where tab = takeWhile (all isAlpha) txt
       linhaCoord = txt !! (length txt - 2)
       coordx = read ((words linhaCoord) !! 0)
       coordy = read ((words linhaCoord) !! 1)
       coord = (coordx,coordy)
       orient = head ((words linhaCoord) !! 2)
       comandos = txt !! (length txt -1)

prefixo = ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"",
           "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
           "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
           "<head>",
           "<meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\" />",
           "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />",
           "<title>Light-Bot</title>",
           "<script type=\"text/javascript\" src=\"http://www.x3dom.org/release/x3dom.js\"></script>",
           "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.x3dom.org/release/x3dom.css\"/>",
           "</head>",
           "<body>",
           "<h1>Animação</h1>",
           "<p class=\"case\">",
           "<X3D xmlns=\"http://www.web3d.org/specifications/x3d-namespace\" id=\"boxes\"",
           "showStat=\"false\" showLog=\"false\" x=\"0px\" y=\"0px\" width=\"800px\" height=\"600px\">",
           "<Scene>",
           "<Shape DEF=\"tile\">",
           "<Appearance>",
           "<Material diffuseColor='0 1.0 1.0'/>",
           "</Appearance>",
           "<Box size='3.45 3.45 1.45'/>",
           "</Shape>",
           "<Shape DEF=\"tileL\">",
           "<Appearance>",
           "<Material diffuseColor='0 0 1.0'/>",
           "</Appearance>",
           "<Box size='3.45 3.45 1.45'/>",
           "</Shape>"
           ]

{-|'criaTab', 'aux', 'linhas', 'mostra' e 'mostraP' calculam as coordenadas de todas as posições do tabuleiro.

*Exemplo

>>>criaTab ["aacdD","aacaa","bbCaa"]
1. aux ["aacdD","aacaa","bbCaa"] 0 2
-}
criaTab :: Tabuleiro -> [String]
criaTab l = aux l 0 (length l -1)
{-|
*Exemplo

'>>>aux' ["aacdD","aacaa","bbCaa"] 0 2
1. linhas "aacdD" 0 2 ++ aux ["aacaa","bbCaa"] 0 1
-}
aux :: Tabuleiro -> Coordenada -> Coordenada -> [String]
aux [] _ _ = []
aux (h:t) x y = linhas h x y ++ aux t x (y-1)
{-|
*Exemplo

>>> linhas "aacdD" 0 2
1. mostra 0 2 'a' ++ linhas "acdD" 1 2
-}
linhas :: String -> Coordenada -> Coordenada -> [String]
linhas [] _ _ = []
linhas (h:t) x y = mostra x y h ++ linhas t (x+1) y
{-|
*Exemplo

>>> mostra 0 2 'a'
1. nivel 'a' == 0 = [mostraP 0 2 'a'] 
2. [mostraP 0 2 'a'] ++ mostra x y (chr (ord (toLower 'a') - 1))
-}
mostra :: Coordenada -> Coordenada -> Char -> [String]
mostra x y h = if nivel h == 0
	           then [mostraP x y h]
	           else [mostraP x y h] ++ mostra x y (chr (ord (toLower h) -1))
{-|
*Exemplo

>>>mostraP 0 2 'a' = "<Transform translation='" ++ (show ((fromIntegral 0)*3.5) ++ " " ++ show ((fromIntegral 2)*3.5) ++
                     " " ++ show (nivel 'a')) ++ "'> <Shape USE=\"tile\"/> </Transform>"
-}
mostraP :: Coordenada -> Coordenada -> Char -> String
mostraP x y h = if isUpper h then "<Transform translation='" ++ (show ((fromIntegral x)*3.5) ++ " " ++ show ((fromIntegral y)*3.5) ++ " " ++ show (nivel h)) ++ "'> <Shape USE=\"tileL\"/> </Transform>"
                             else "<Transform translation='" ++ (show ((fromIntegral x)*3.5) ++ " " ++ show ((fromIntegral y)*3.5) ++ " " ++ show (nivel h)) ++ "'> <Shape USE=\"tile\"/> </Transform>"

{-|'nivel' calcula a altura de um caracter do tabuleiro.

*Exemplo

>>>nivel 'a' = (fromIntegral (ord (toLower 'a') - ord 'a'))*1.5
-}
nivel :: Char -> Float
nivel c = (fromIntegral (ord (toLower c) - ord 'a'))*1.5

{-|'robot' calcula as coordenadas e a orientação iniciais do robot.

*Exemplo

>>>robot ["aacdD","aacaa","bbCaa"] (0,1) 'S' = ["<Transform translation='" ++ (show ((fromIntegral 0)*3.5) ++ " " ++ 
                                                show ((fromIntegral 1)*3.5) ++ " " ++ 
                                                show (altura ["aacdD","aacaa","bbCaa"] (0,1))) ++ 
                                                "' rotation='0 0 1 " ++ show (orientacao 'S') ++ "'>",
                                                "<Transform translation='0 0 2.2' rotation='1 0 0 1.57'>"]
-}
robot :: Tabuleiro -> Coordenadas -> Orientacao -> [String]
robot tab (x,y) c = ["<Transform translation='" ++ (show ((fromIntegral x)*3.5) ++ " " ++ show ((fromIntegral y)*3.5) ++ " " ++ show (altura tab (x,y))) ++ "' rotation='0 0 1 " ++ show (orientacao c) ++ "'>",
                     "<Transform translation='0 0 2.2' rotation='1 0 0 1.57'>"]

{-|'altura' calcula  a altura de uma determinada coordenada.

*Exemplo

>>>altura ["aacdD","aacaa","bbCaa"] (0,1)
1. 1.5 + nivel 'a'
-}
altura :: Tabuleiro -> Coordenadas -> Float
altura tab (x,y) = 1.5 + nivel caracter
 where caracter = linha !! x
       linha = tab !! (length tab - (y+1))

{-'orientacao' calcula a rotacao que é necessário efetuar para que o robot fique virado de acordo com a sua orientação.

*Exemplo

>>>'orientacao' 'S' = 0
-}
orientacao :: Orientacao -> Float
orientacao c | c == 'S' = 0
             | c == 'O' = 4.71
             | c == 'N' = 3.14
             | c == 'E' = 1.57

prefixoanimacao = ["<transform DEF=\"robot\">"]

{-|'animacao' calcula a animacao completa de acordo com o input.

*animacao ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL"
1. [
   "<timeSensor DEF=\"time\" cycleInterval=\"" ++ show (fromIntegral (length "SEASLEAADSAL")/4) ++ "\" loop=\"true\"> 
   </timeSensor>","<PositionInterpolator DEF=\"movePOS\" key=\"0" ++ 
   key (length "SEASLEAADSAL" + extra "SEASLEAADSAL",1/(fromIntegral (length "SEASLEAADSAL" + extra "SEASLEAADSAL"))) 1 ++ 
   "\" keyValue=\"0 0 0" ++ fst (proxsPos ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL") ++ "\"> </PositionInterpolator>",
   "<OrientationInterpolator DEF=\"moveROT\" key=\"0" ++ 
   key (length "SEASLEAADSAL",(1/fromIntegral (length "SEASLEAADSAL"))) 1 ++ "\" keyValue=\"0 0 0 0"
   ++ snd (proxsPos ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL") ++ "\"> </OrientationInterpolator>"
   ]
-}
animacao :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> [String]
animacao tab (x,y) o l = 
  [
  "<timeSensor DEF=\"time\" cycleInterval=\"" ++ show (fromIntegral (length l)/4) ++ "\" loop=\"true\"> </timeSensor>",
  "<PositionInterpolator DEF=\"movePOS\" key=\"0" ++ key (length l + extra l,1/(fromIntegral (length l + extra l))) 1 ++ "\" keyValue=\"0 0 0"
  ++ fst (proxsPos tab (x,y) o l) ++ "\"> </PositionInterpolator>",
  "<OrientationInterpolator DEF=\"moveROT\" key=\"0" ++ key (length l,(1/fromIntegral (length l))) 1 ++ "\" keyValue=\"0 0 0 0"
  ++ snd (proxsPos tab (x,y) o l) ++ "\"> </OrientationInterpolator>"
  ]

{-|'key' calcula a fracao de tempo que dura cada comando.

*Exemplo

>>>key (12,1/12) 1
1. " " ++ show (1*(1/12)) ++ key (11,1/12) 2
-}
key :: (Int,Float) -> Float -> String
key (x,y) n |x == 1 = " 1.0"
            |otherwise = " " ++ show (n*y) ++ key (x-1,y) (n+1)

{-|'extra' calcula o numero de comandos Luz e Saltar que existem num comando.

*Exemplo

>>>extra "SEASLEAADSAL" = length "LL" + length "SSS"
-}
extra :: String -> Int
extra l = length (filter (\c -> c == 'L')l) + length (filter (\c -> c == 'S')l)

{-|'proxsPos' calcula o movimento de cada comando.

*Exemplo

>>>proxsPos ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL"
1. ((proxPosSul ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL" [",","0","0","0"]),(roda "SEASLEAADSAL" [",","0","0","0","0"])
-}
proxsPos :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> (String,String)
proxsPos tab (x,y) o l | o == 'S' = ((proxPosSul tab (x,y) o l [",","0","0","0"]),(roda l [",","0","0","0","0"]))
                       | o == 'E' = ((proxPosEste tab (x,y) o l [",","0","0","0"]),(roda l [",","0","0","0","0"]))
                       | o == 'N' = ((proxPosNorte tab (x,y) o l [",","0","0","0"]),(roda l [",","0","0","0","0"]))
                       | o == 'O' = ((proxPosOeste tab (x,y) o l [",","0","0","0"]),(roda l [",","0","0","0","0"]))

{-|'proxPosSul' calcula os movimentos para os comandos Saltar, Avançar e Luz.

*Exemplo

>>>proxPosSul ["aacdD","aacaa","bbCaa"] (0,1) 'S' "SEASLEAADSAL" [",","0","0","0"]
1. presaltoS ++ unwoords salto ++ proxPosSul ["aacdD","aacaa","bbCaa"] (proxPosAS) 'S' "SEASLEAADSAL" salto
-}
proxPosSul :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> [String] -> String
proxPosSul _ _ _ [] _ = []
proxPosSul tab (x,y) o (h:t) [",",x1,y1,z1] | h == 'S' && verificaS && (ord final - ord inicial == 1) = presaltoS ++ unwords salto ++ proxPosSul tab (proxPosAS) o t salto
                                            | h == 'S' && verificaS = presaltoD ++ unwords salto ++ proxPosSul tab (proxPosAS) o t salto
                                            | h == 'S' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosSul tab (x,y) o t [",",x1,y1,z1]
                                            | h == 'A' && verificaA = unwords avanca ++ proxPosSul tab (proxPosAS) o t avanca
                                            | h == 'L' && isUpper caracteri = luz ++ proxPosSul tab (x,y) o t [",",x1,y1,z1]
                                            | h == 'L' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosSul tab (x,y) o t [",",x1,y1,z1]
                                            | h == 'E' = unwords [",",x1,y1,z1] ++ proxPosSul tab (x,y) (proxPosE o) t [",",x1,y1,z1]
                                            | h == 'D' = unwords [",",x1,y1,z1] ++ proxPosSul tab (x,y) (proxPosD o) t [",",x1,y1,z1]
                                            | otherwise = unwords [",",x1,y1,z1] ++ proxPosSul tab (x,y) o t [",",x1,y1,z1]
 where verificaS = validaCoord (dimx,dimy) proxPosAS && validaS inicial final
       verificaA = validaCoord (dimx,dimy) proxPosAS && (inicial == final)
       dimx = length (head tab)
       dimy = length tab
       proxPosAS = proxPosSA (x,y) o
       inicial = toUpper caracteri
       caracteri = (tab !! (length tab - (y + 1))) !! x
       final = toUpper caracterf
       caracterf = (tab !! (length tab - (yf + 1))) !! xf
       yf = snd proxPosAS
       xf = fst proxPosAS
       salto = proxPosSS [read x1,read y1,read z1] inicial final o
       avanca = proxPosA [read x1,read (y1),read z1] o
       luz = proxPosL [read x1,read y1, read z1]
       presaltoS = unwords (sobe [read x1,read y1,read z1])
       presaltoD = unwords (desceS [read x1,read y1,read z1] o)

{-|'proxPosSA' calcula as próximas coordenadas para os comandos Saltar e Avançar.

*Exemplo

>>>proxPosSA (0,1) 'S' = (0,0)
-}
proxPosSA :: Coordenadas -> Orientacao -> (Int,Int)
proxPosSA (x,y) o | o == 'N' = (x,y+1)
                  | o == 'E' = (x+1,y)
                  | o == 'S' = (x,y-1)
                  | o == 'O' = (x-1,y)

{-|'validaCoord' verifica se a próxima coordendada calculada é válida.

*Exemplo

>>>validaCoord (5,3) (0,0) = True
-}
validaCoord :: (Int,Int) -> Coordenadas -> Bool
validaCoord (dimx,dimy) (x,y) = x < dimx && y < dimy && x >= 0 && y >= 0

{-|'validaS' verifica se é possível saltar desde a coordenada inicial para a próxima coordenada calculada, comparando os 
caracteres correspondentes.

*Exemplo

>>>validaS 'A' 'B' = True
-}
validaS :: Char -> Char -> Bool
validaS x y |x == 'A' = y == 'B'
            |otherwise = ord y < ord x || ord y == ord x + 1

{-|'sobe' acrescenta 1.5 à altura.

*Exemplo

>>>sobe [0,0,0] = [",","0","1.5","0"]
-}
sobe :: [Float] -> [String]
sobe [x,y,z] = [",",show x, show (y + 1.5), show z]

{-|'desceS' calcula o movimento necessário para avançar para de seguida descer.

*Exemplo

>>>desceS [0,0,0] 'S' = [",","0","0","3.5"]
-}
desceS :: [Float] -> Orientacao -> [String]
desceS [x,y,z] o | o == 'N' = [",",show x, show y, show (z - 3.5)]
                 | o == 'E' = [",",show (x + 3.5), show y, show z]
                 | o == 'S' = [",",show x, show y, show (z + 3.5)]
                 | o == 'O' = [",",show (x - 3.5), show y, show z]

{-|'proxPosSS' calcula o movimento para o comando saltar.

*Exemplo

>>>proxPosSS [0,0,0] 'a' 'b' 'S' = [",","0","1.5","3.5"]
-}
proxPosSS :: [Float] -> Char -> Char -> Orientacao -> [String]
proxPosSS [x,y,z] ci cf o | o == 'N' = [",",show x, show (y + altura), show (z - 3.5)]
                          | o == 'E' = [",",show (x + 3.5), show (y + altura), show z]
                          | o == 'S' = [",",show x, show (y + altura), show (z + 3.5)]
                          | o == 'O' = [",",show (x - 3.5), show (y + altura), show z]
 where altura = (fromIntegral (ord cf - ord ci)) * 1.5

{-|'proxPosA' calcula o movimento para o comando Avançar.

*Exemplo

>>>proxPosA [0,0,0] 'S' = [",","0","0","3.5"]
-}
proxPosA :: [Float] -> Orientacao -> [String]
proxPosA [x,y,z] o | o == 'N' = [",",show x, show y, show (z - 3.5)]
                   | o == 'E' = [",",show (x + 3.5), show y, show z]
                   | o == 'S' = [",",show x, show y, show (z + 3.5)]
                   | o == 'O' = [",",show (x - 3.5), show y, show z]

{-|'proxPosL' calcula o movimento para o comando Luz (dar um pulo)

*Exemplo

>>>proxPosL [0,0,0] = unwords [",","0","1.5","0"] ++ unwords [",","0","0","0"]
-}
proxPosL :: [Float] -> String
proxPosL [x,y,z] = unwords [",",show x, show (y + 1.5),show z] ++ unwords [",",show x,show y,show z]

{-|'proxPosE' e 'proxPosD' calculam a orientação para os comandos esquerda e direita, respetivamente.

*Exemplo

>>>proxPosE 'S' = 'E'
-}
proxPosE :: Char -> Char
proxPosE c |c == 'N' = 'O'
           |c == 'E' = 'N'
           |c == 'S' = 'E'
           |c == 'O' = 'S'
{-|
*Exemplo

>>>proxPosD 'S' = 'O'
-}
proxPosD :: Char -> Char
proxPosD c |c == 'N' = 'E'
           |c == 'E' = 'S'
           |c == 'S' = 'O'
           |c == 'O' = 'N'

{-|'roda' calcula todos os movimentos de rotações para os comandos esquerda e direita.

*Exemplo

>>>roda "SEASLEAADSAL" [",","0","0","0","0"]
1. unwords [",","0","0","0","0"] ++ roda "EASLEAADSAL" [",","0","0","0","0"]
-}
roda :: Comandos -> [String] -> String
roda [] _ = []
roda (h:t) [",",x,y,z,r] | h == 'E' = unwords rodaE ++ roda t rodaE
                         | h == 'D' = unwords rodaO ++ roda t rodaO
                         | otherwise = unwords [",",x,y,z,r] ++ roda t [",",x,y,z,r]
 where rodaE = [",",x,"1",z,show ((read r::Float) + 1.57)]
       rodaO = [",",x,"1",z,show ((read r::Float) - 1.57)]

{-|'proxPosEste' calcula os movimentos para os comandos Saltar, Avançar e Luz.

*Exemplo

>>>proxPosEste ["aacdD","aacaa","bbCaa"] (0,1) 'E' "SEASLEAADSAL" [",","0","0","0"]
1. presaltoS ++ unwoords salto ++ proxPosEste ["aacdD","aacaa","bbCaa"] (proxPosAS) 'E' "SEASLEAADSAL" salto
-}
proxPosEste :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> [String] -> String
proxPosEste _ _ _ [] _ = []
proxPosEste tab (x,y) o (h:t) [",",x1,y1,z1] | h == 'S' && verificaS && (ord final - ord inicial == 1) = presaltoS ++ unwords salto ++ proxPosEste tab (proxPosAS) o t salto
                                             | h == 'S' && verificaS = presaltoD ++ unwords salto ++ proxPosEste tab (proxPosAS) o t salto
                                             | h == 'S' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosEste tab (x,y) o t [",",x1,y1,z1]
                                             | h == 'A' && verificaA = unwords avanca ++ proxPosEste tab (proxPosAS) o t avanca
                                             | h == 'L' && isUpper caracteri = luz ++ proxPosEste tab (x,y) o t [",",x1,y1,z1]
                                             | h == 'L' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosEste tab (x,y) o t [",",x1,y1,z1]
                                             | h == 'E' = unwords [",",x1,y1,z1] ++ proxPosEste tab (x,y) (proxPosE o) t [",",x1,y1,z1]
                                             | h == 'D' = unwords [",",x1,y1,z1] ++ proxPosEste tab (x,y) (proxPosD o) t [",",x1,y1,z1]
                                             | otherwise = unwords [",",x1,y1,z1] ++ proxPosEste tab (x,y) o t [",",x1,y1,z1]
 where verificaS = validaCoord (dimx,dimy) proxPosAS && validaS inicial final
       verificaA = validaCoord (dimx,dimy) proxPosAS && (inicial == final)
       dimx = length (head tab)
       dimy = length tab
       proxPosAS = proxPosSA (x,y) o
       inicial = toUpper caracteri
       caracteri = (tab !! (length tab - (y + 1))) !! x
       final = toUpper caracterf
       caracterf = (tab !! (length tab - (yf + 1))) !! xf
       yf = snd proxPosAS
       xf = fst proxPosAS
       salto = proxPosSE [read x1,read y1,read z1] inicial final o
       avanca = proxPosAE [read x1,read (y1),read z1] o
       luz = proxPosL [read x1,read y1, read z1]
       presaltoS = unwords (sobe [read x1,read y1,read z1])
       presaltoD = unwords (desceE [read x1,read y1,read z1] o)

{-|'proxPosSE' calcula o movimento para o comando saltar.

*Exemplo

>>>proxPosSE [0,0,0] 'a' 'b' 'E' = [",","0","1.5","3.5"]
-}
proxPosSE :: [Float] -> Char -> Char -> Orientacao -> [String]
proxPosSE [x,y,z] ci cf o | o == 'N' = [",",show (x + 3.5), show (y + altura), show z]
                          | o == 'E' = [",",show x, show (y + altura), show (z + 3.5)]
                          | o == 'S' = [",",show (x - 3.5), show (y + altura), show z]
                          | o == 'O' = [",",show x, show (y + altura), show (z - 3.5)]
 where altura = (fromIntegral (ord cf - ord ci)) * 1.5

{-|'proxPosAE' calcula o movimento para o comando Avançar.

*Exemplo

>>>proxPosAE [0,0,0] 'E' = [",","0","0","3.5"]
-}
proxPosAE :: [Float] -> Orientacao -> [String]
proxPosAE [x,y,z] o | o == 'N' = [",",show (x + 3.5), show y, show z]
                    | o == 'E' = [",",show x, show y, show (z + 3.5)]
                    | o == 'S' = [",",show (x - 3.5), show y, show z]
                    | o == 'O' = [",",show x, show y, show (z - 3.5)]

{-|'desceE' calcula o movimento necessário para avançar para de seguida descer.

*Exemplo

>>>desceE [0,0,0] 'E' = [",","0","0","3.5"]
-}
desceE :: [Float] -> Orientacao -> [String]
desceE [x,y,z] o | o == 'N' = [",",show (x + 3.5), show y, show z]
                 | o == 'E' = [",",show x, show y, show (z + 3.5)]
                 | o == 'S' = [",",show (x - 3.5), show y, show z]
                 | o == 'O' = [",",show x, show y, show (z - 3.5)]

{-|'proxPosNorte' calcula os movimentos para os comandos Saltar, Avançar e Luz.

*Exemplo

>>>proxPosNorte ["aacdD","aacaa","bbCaa"] (0,1) 'N' "SEASLEAADSAL" [",","0","0","0"]
1. presaltoS ++ unwoords salto ++ proxPosNorte ["aacdD","aacaa","bbCaa"] (proxPosAS) 'N' "SEASLEAADSAL" salto
-}
proxPosNorte :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> [String] -> String
proxPosNorte _ _ _ [] _ = []
proxPosNorte tab (x,y) o (h:t) [",",x1,y1,z1] | h == 'S' && verificaS && (ord final - ord inicial == 1) = presaltoS ++ unwords salto ++ proxPosNorte tab (proxPosAS) o t salto
                                              | h == 'S' && verificaS = presaltoD ++ unwords salto ++ proxPosNorte tab (proxPosAS) o t salto
                                              | h == 'S' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosNorte tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'A' && verificaA = unwords avanca ++ proxPosNorte tab (proxPosAS) o t avanca
                                              | h == 'L' && isUpper caracteri = luz ++ proxPosNorte tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'L' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosNorte tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'E' = unwords [",",x1,y1,z1] ++ proxPosNorte tab (x,y) (proxPosE o) t [",",x1,y1,z1]
                                              | h == 'D' = unwords [",",x1,y1,z1] ++ proxPosNorte tab (x,y) (proxPosD o) t [",",x1,y1,z1]
                                              | otherwise = unwords [",",x1,y1,z1] ++ proxPosNorte tab (x,y) o t [",",x1,y1,z1]
 where verificaS = validaCoord (dimx,dimy) proxPosAS && validaS inicial final
       verificaA = validaCoord (dimx,dimy) proxPosAS && (inicial == final)
       dimx = length (head tab)
       dimy = length tab
       proxPosAS = proxPosSA (x,y) o
       inicial = toUpper caracteri
       caracteri = (tab !! (length tab - (y + 1))) !! x
       final = toUpper caracterf
       caracterf = (tab !! (length tab - (yf + 1))) !! xf
       yf = snd proxPosAS
       xf = fst proxPosAS
       salto = proxPosSN [read x1,read y1,read z1] inicial final o
       avanca = proxPosAN [read x1,read (y1),read z1] o
       luz = proxPosL [read x1,read y1, read z1]
       presaltoS = unwords (sobe [read x1,read y1,read z1])
       presaltoD = unwords (desceN [read x1,read y1,read z1] o)

{-|'proxPosSN' calcula o movimento para o comando saltar.

*Exemplo

>>>proxPosSN [0,0,0] 'a' 'b' 'N' = [",","0","1.5","3.5"]
-}
proxPosSN :: [Float] -> Char -> Char -> Orientacao -> [String]
proxPosSN [x,y,z] ci cf o | o == 'N' = [",",show x, show (y + altura), show (z + 3.5)]
                          | o == 'E' = [",",show (x - 3.5), show (y + altura), show z]
                          | o == 'S' = [",",show x, show (y + altura), show (z - 3.5)]
                          | o == 'O' = [",",show (x + 3.5), show (y + altura), show z]
 where altura = (fromIntegral (ord cf - ord ci)) * 1.5

{-|'proxPosAN' calcula o movimento para o comando Avançar.

*Exemplo

>>>proxPosAN [0,0,0] 'N' = [",","0","0","3.5"]
-}
proxPosAN :: [Float] -> Orientacao -> [String]
proxPosAN [x,y,z] o | o == 'N' = [",",show x, show y, show (z + 3.5)]
                    | o == 'E' = [",",show (x - 3.5), show y, show z]
                    | o == 'S' = [",",show x, show y, show (z - 3.5)]
                    | o == 'O' = [",",show (x + 3.5), show y, show z]

{-|'desceN' calcula o movimento necessário para avançar para de seguida descer.

*Exemplo

>>>desceN [0,0,0] 'N' = [",","0","0","3.5"]
-}
desceN :: [Float] -> Orientacao -> [String]
desceN [x,y,z] o | o == 'N' = [",",show x, show y, show (z + 3.5)]
                 | o == 'E' = [",",show (x - 3.5), show y, show z]
                 | o == 'S' = [",",show x, show y, show (z - 3.5)]
                 | o == 'O' = [",",show (x + 3.5), show y, show z]

{-|'proxPosOeste' calcula os movimentos para os comandos Saltar, Avançar e Luz.

*Exemplo

>>>proxPosOeste ["aacdD","aacaa","bbCaa"] (0,1) 'O' "SEASLEAADSAL" [",","0","0","0"]
1. presaltoS ++ unwoords salto ++ proxPosOeste ["aacdD","aacaa","bbCaa"] (proxPosAS) 'O' "SEASLEAADSAL" salto
-}
proxPosOeste :: Tabuleiro -> Coordenadas -> Orientacao -> Comandos -> [String] -> String
proxPosOeste _ _ _ [] _ = []
proxPosOeste tab (x,y) o (h:t) [",",x1,y1,z1] | h == 'S' && verificaS && (ord final - ord inicial == 1) = presaltoS ++ unwords salto ++ proxPosOeste tab (proxPosAS) o t salto
                                              | h == 'S' && verificaS = presaltoD ++ unwords salto ++ proxPosOeste tab (proxPosAS) o t salto
                                              | h == 'S' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosOeste tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'A' && verificaA = unwords avanca ++ proxPosOeste tab (proxPosAS) o t avanca
                                              | h == 'L' && isUpper caracteri = luz ++ proxPosOeste tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'L' = unwords [",",x1,y1,z1] ++ unwords [",",x1,y1,z1] ++ proxPosOeste tab (x,y) o t [",",x1,y1,z1]
                                              | h == 'E' = unwords [",",x1,y1,z1] ++ proxPosOeste tab (x,y) (proxPosE o) t [",",x1,y1,z1]
                                              | h == 'D' = unwords [",",x1,y1,z1] ++ proxPosOeste tab (x,y) (proxPosD o) t [",",x1,y1,z1]
                                              | otherwise = unwords [",",x1,y1,z1] ++ proxPosOeste tab (x,y) o t [",",x1,y1,z1]
 where verificaS = validaCoord (dimx,dimy) proxPosAS && validaS inicial final
       verificaA = validaCoord (dimx,dimy) proxPosAS && (inicial == final)
       dimx = length (head tab)
       dimy = length tab
       proxPosAS = proxPosSA (x,y) o
       inicial = toUpper caracteri
       caracteri = (tab !! (length tab - (y + 1))) !! x
       final = toUpper caracterf
       caracterf = (tab !! (length tab - (yf + 1))) !! xf
       yf = snd proxPosAS
       xf = fst proxPosAS
       salto = proxPosSO [read x1,read y1,read z1] inicial final o
       avanca = proxPosAO [read x1,read (y1),read z1] o
       luz = proxPosL [read x1,read y1, read z1]
       presaltoS = unwords (sobe [read x1,read y1,read z1])
       presaltoD = unwords (desceO [read x1,read y1,read z1] o)

{-|'proxPosSO' calcula o movimento para o comando saltar.

*Exemplo

>>>proxPosSO [0,0,0] 'a' 'b' 'O' = [",","0","1.5","3.5"]
-}
proxPosSO :: [Float] -> Char -> Char -> Orientacao -> [String]
proxPosSO [x,y,z] ci cf o | o == 'N' = [",",show (x - 3.5), show (y + altura), show z]
                          | o == 'E' = [",",show x, show (y + altura), show (z - 3.5)]
                          | o == 'S' = [",",show (x + 3.5), show (y + altura), show z]
                          | o == 'O' = [",",show x, show (y + altura), show (z + 3.5)]
 where altura = (fromIntegral (ord cf - ord ci)) * 1.5

{-|'proxPosAO' calcula o movimento para o comando Avançar.

*Exemplo

>>>proxPosAO [0,0,0] 'O' = [",","0","0","3.5"]
-}
proxPosAO :: [Float] -> Orientacao -> [String]
proxPosAO [x,y,z] o | o == 'N' = [",",show (x - 3.5), show y, show z]
                    | o == 'E' = [",",show x, show y, show (z - 3.5)]
                    | o == 'S' = [",",show (x + 3.5), show y, show z]
                    | o == 'O' = [",",show x, show y, show (z + 3.5)]

{-|'desceO' calcula o movimento necessário para avançar para de seguida descer.

*Exemplo

>>>desceO [0,0,0] 'O' = [",","0","0","3.5"]
-}
desceO :: [Float] -> Orientacao -> [String]
desceO [x,y,z] o | o == 'N' = [",",show (x - 3.5), show y, show z]
                 | o == 'E' = [",",show x, show y, show (z - 3.5)]
                 | o == 'S' = [",",show (x + 3.5), show y, show z]
                 | o == 'O' = [",",show x, show y, show (z + 3.5)]

sufixoanimacao = [
                  "<Route fromNode=\"time\" fromField =\"fraction_changed\" toNode=\"movePOS\" toField=\"set_fraction\"> </Route>", 
                  "<Route fromNode=\"movePOS\" fromField =\"value_changed\" toNode=\"robot\" toField=\"translation\"> </Route>",    
                  "<Route fromNode=\"time\" fromField =\"fraction_changed\" toNode=\"moveROT\" toField=\"set_fraction\"> </Route>", 
                  "<Route fromNode=\"moveROT\" fromField =\"value_changed\" toNode=\"robot\" toField=\"rotation\"> </Route>"
                  ]

{-!-- cabeça -->-}
sufixoRo = [
           "<transform translation='0 2 0'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='1 1 1'></material>", 
           "</appearance>",             
           "<sphere radius='0.8'></sphere>", 
           "</shape>", 
           "</transform>",   


{-!-- tronco -->-}
       
           "<transform translation='0 0.3 0'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='1 1 1'></material>", 
           "</appearance>",             
           "<sphere radius='1.1'></sphere>", 
           "</shape>",
           "</transform>",
         
{-!-- base -->-}     
  
           "<transform translation='0 -1.8 0'>", 
           "<shape>",
           "<appearance>", 
           "<material diffuseColor='1 1 1'></material>", 
           "</appearance>", 
           "<sphere radius='1.5'></sphere>", 
           "</shape>", 
           "</transform>", 

{-!-- olhos -->-} 
  
           "<transform translation='0.2 2.1 0.8'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.15'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='-0.2 2.1 0.8'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.15'></sphere>",
           "</shape>", 
           "</transform>", 

{-!-- boca -->-}

           "<transform translation='-0.3 1.6 0.65'>", 
           "<shape>", 
           "<appearance>",
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='-0.2 1.55 0.6'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>",
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 
   
           "<transform translation='-0.1 1.5 0.6'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 
   
           "<transform translation='0 1.5 0.6'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='0.1 1.5 0.6'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='0.2 1.55 0.6'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='0.3 1.6 0.65'>", 
           "<shape>",
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",            
           "<sphere radius='0.05'></sphere>", 
           "</shape>", 
           "</transform>", 

{-!-- botões -->-}

           "<transform translation='0 0.65 1'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.15'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='0 0.25 1'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.15'></sphere>", 
           "</shape>", 
           "</transform>", 

           "<transform translation='0 -0.15 0.9'>", 
           "<shape>", 
           "<appearance>", 
           "<material diffuseColor='0 0 0'></material>", 
           "</appearance>",             
           "<sphere radius='0.15'></sphere>", 
           "</shape>", 
           "</transform>", 

 {-!-- nariz -->-}   
 
           "<Transform rotation='1 0 0 1.57' translation='0 1.8 0.55'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0.271 0'></material>",
           "</Appearance>",
           "<Cone bottomRadius='0.3' height='1.75'/>",
           "</Shape>",
           "</Transform>",

{-!-- cachecol -->-}

           "<Transform translation='0 1.32 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Cylinder height='0.175' radius='0.7'/>",
           "</Shape>",
           "</Transform>",

           "<Transform translation='.7 0.62 .8'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0 0 1'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.725 0.72 0.85'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.7 0.85 .8'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0 0 1'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.7 0.98 .7'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.7 1.035 .55'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0 0 1'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.7 1.12 .45'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.7 1.22 .35'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0 0 1'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.6 1.32 .25'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Sphere radius='.1'/>",
           "</Shape>",
           "</Transform>",

{-!--cachimbo-->-}

           "<Transform rotation='0.96225 0.19245 -0.19245 1.57' translation='0.1 1.5 0.55'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0.219608 0.137255 0'/>",
           "</Appearance>",
           "<Cylinder radius='0.03'/>",
           "</Shape>",
           "</Transform>",
           "<Transform translation='.45 1.58 1.5'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0.219608 0.137255 0'/>",
           "</Appearance>",
           "<Cylinder height='.2' radius='.1'/>",
           "</Shape>",
           "</Transform>",
  

{-!--braços-->-}  

           "<Transform rotation='0 0 1 1.57' translation='0 0.7 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='.5 .25 .05'/>",
           "</Appearance>",
           "<Cylinder height='4.9' radius='0.1'/>",
           "</Shape>",
           "</Transform>",
           "<Transform rotation='0 0.92848 0.37139 1.57' translation='2 0.7 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='.5 .25 .05'/>",
           "</Appearance>",
           "<Cylinder height='.9' radius='0.05'/>",
           "</Shape>",
           "</Transform>",
           "<Transform rotation='0.84515 -0.50709 0.16903 1.57' translation='1.5 0.7 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='.5 .25 .05'/>",
           "</Appearance>",
           "<Cylinder height='.7' radius='0.03'/>",
           "</Shape>",
           "</Transform>",
           "<Transform rotation='0 0.64018 0.76822 1.57' translation='-1 0.7 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='.5 .25 .05'/>",
           "</Appearance>",
           "<Cylinder height='1' radius='0.05'/>",
           "</Shape>",
           "</Transform>",
           "<Transform rotation='0 0.70711 -0.70711 1' translation='-1.9 0.7 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='.5 .25 .05'/>",
           "</Appearance>",
           "<Cylinder height='.5' radius='0.04'/>",
           "</Shape>",
           "</Transform>",

{-!--chapéu-->-}

           "<Transform translation='0 2.8 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0.003922 0.003922 0.003922'/>",
           "</Appearance>",
           "<Cylinder height='1.3' radius='0.5'/>",
           "</Shape>",
           "</Transform>",

           "<Transform translation='0 2.45 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='0.003922 0.003922 0.003922'/>",
           "</Appearance>",
           "<Cylinder height='0.15'/>",
           "</Shape>",
           "</Transform>",

           "<Transform translation='0 2.57 0'>",
           "<Shape>",
           "<Appearance>",
           "<Material diffuseColor='1 0 0'/>",
           "</Appearance>",
           "<Cylinder height='0.1' radius='0.6'/>",
           "</Shape>",
           "</Transform>",
           "</Transform>",
           "</Transform>"
           ]

sufixo =[
        "</Scene>",
        "</X3D>",
        "</p>",
        "<p> &nbsp; </p>",
        "</body>",
        "</html>"
        ]

