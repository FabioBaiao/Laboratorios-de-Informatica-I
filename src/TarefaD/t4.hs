module Main where

import Data.Char

t1 = ["aaaaA","cAcaa","acaAa","aaaaa","0 0 S"]
t2 = ["aacdD","aacaa","bbCaa","0 1 S"]
t3 = ["aaa","aaa","aAa","1 2 S"]
t4 = ["aba","aba","abA","0 2 S"]
t5 = ["aca","aBa","AaA","1 2 S"]
t6 = ["abcc","aaac","aaaC","aaDC","0 3 E"]
t7 = ["BaBaB","BaBaB","BaBaB","BaBaB","BaBaB","4 3 S"]

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr (tarefa (lines inp)))

tarefa :: [String] -> [String]
tarefa txt = [msg]
 where tab = takeWhile (all isAlpha) txt
       linhaCoord = txt !! (length txt - 1)
       coordx = read ((words linhaCoord) !! 0)
       coordy = read ((words linhaCoord) !! 1)
       coord = (coordx,coordy)
       orient = head ((words linhaCoord) !! 2)
       msg |length (luzesTab tab) < 7 = menor (movePm tab coord orient (fun (luzesTab tab) []))
           |otherwise = movePM tab coord orient (fun (luzesTab tab) [])

type Coordenadas = (Int,Int)
type Tabuleiro = [String]
type Comando = String
type Orientacao = Char

{- |'luzesTab','coordLuzes' e 'linhas' calcula a lista de coordenadas onde existe letra maiúscula no tabuleiro, isto é, onde 
é preciso acender luz.

*Exemplo:

>>>luzesTab ["aaaaA","cAcaa","acaAa","aaaaa"]
1. coordLuzes ["aaaaA","cAcaa","acaAa","aaaaa"] 3
-}
luzesTab :: Tabuleiro -> [Coordenadas]
luzesTab l = coordLuzes l (length l - 1)
{- |
*Exemplo:

>>> coordLuzes ["aaaaA","cAcaa","acaAa","aaaaa"] 2
1. linhas "aaaaA" 0 3 ++ coordLuzes ["cAcaa","acaAa","aaaaa"] (3-1)
-}
coordLuzes :: Tabuleiro -> Int -> [Coordenadas]
coordLuzes [] _ = []
coordLuzes (h:t) y = linhas h 0 y ++ coordLuzes t (y-1)
{- |
*Exemplo:

>>>linhas "aaaaA" 0 3 = [(4,3)]
-}
linhas :: String -> Int -> Int -> [Coordenadas]
linhas [] _ _ = []
linhas (h:t) x y |isUpper h = (x,y) : (linhas t (x+1) y)
                 |otherwise = linhas t (x+1) y

{- |'fun','auxP','aux' e 'aux2' calculam todas as combinações possíveis entre as coordenadas das lâmpadas.

*Exemplo

>>>fun[(4,3),(1,2),(3,1)] []
1. fun [(1,2),(3,1)] (auxP (4,3) [])
-}             
fun :: [Coordenadas] -> [[Coordenadas]] -> [[Coordenadas]]
fun [] l = l
fun (h:t) l = fun t (auxP h l)
{- |
*Exemplo

>>>auxP (4,3) [] 
1. [[(4,3)]]

>>>auxP (1,2) [[(4,3)]]
1. aux (1,2) [(4,3)] 0
-}
auxP :: Coordenadas -> [[Coordenadas]] -> [[Coordenadas]]
auxP x [] = [[x]]
auxP x [l] = aux x l 0
auxP x (h:t) = (aux x h 0) ++ auxP x t
{-|
*Exemplo

>>>aux (1,2) [(4,3)] 0
1. [aux2 (1,2) [(4,3)] 0] ++ aux (1,2) [(4,3)] (0+1)
 -}
aux :: Coordenadas -> [Coordenadas] -> Int -> [[Coordenadas]]
aux x l n | n == length l = [aux2 x l n]
          | otherwise = [aux2 x l n] ++ (aux x l (n+1))
{-| 
*Exemplo

>>>aux2 (1,2) [(4,3)] 0 = [(1,2),(4,3)]

>>>aux2 (1,2) [(4,3)] 1
1. (4,3): (aux2 (1,2) [] (1-1))
-}
aux2 :: Coordenadas -> [Coordenadas] -> Int -> [Coordenadas]
aux2 x [] _ = [x]
aux2 x (h:t) n | n == 0 = (x:h:t)
               | otherwise = h : (aux2 x t (n-1))

{-|'menor' calcula a String de menor comprimento de uma dada lista de String's

*Exemplo

>>>menor ["EAAAEALAEAALEEAAAEAL","EAAAEALDAEAALEAAAEAL","EAAAAEAAALEAEAALEEAEAAL","EAAAAEAAALEAAAEALEAADAL"] = 
"EAAAEALAEAALEEAAAEAL"
-}
menor :: [Comando] -> Comando
menor [] = "IMPOSSIVEL"
menor [x] = x
menor (h1:h2:t) | length h1 <= length h2 = menor (h1:t)
                | otherwise = menor (h2:t)

{-|'movePm' calcula todas as maneiras de acender todas as lâmpadas.

*Exemplo

>>>movePm ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' 
[[(3,1),(1,2),(4,3)],[(1,2),(3,1),(4,3)],[(1,2),(4,3),(3,1)],[(3,1),(4,3),(1,2)],[(4,3),(3,1),(1,2)],[(4,3),(1,2),(3,1)]]
1. move ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' [(3,1),(1,2),(4,3)]
-}
movePm :: Tabuleiro -> Coordenadas -> Orientacao -> [[Coordenadas]] -> [Comando]
movePm _ _ _ [] = []
movePm tab (x,y) o (h:t) | elem '1' (move tab (x,y) o h) = movePm tab (x,y) o t
                         | otherwise = [move tab (x,y) o h] ++ movePm tab (x,y) o t

{-|'movePM' calcula a primeira maneira válida de acender todas as lâmpadas.

*Exemplo

>>>movePM ["BaBaB","BaBaB","BaBaB","BaBaB","BaBaB"] (4,3) 'S' 
[[(0,4),(2,4),(4,4),(0,3),(2,3),(4,3),(0,2),(2,2),(4,2),(0,1),(2,1),(4,1),(0,0),(2,0),(4,0)],...]
1. move ["BaBaB","BaBaB","BaBaB","BaBaB","BaBaB"] (4,3) 'S' 
[(0,4),(2,4),(4,4),(0,3),(2,3),(4,3),(0,2),(2,2),(4,2),(0,1),(2,1),(4,1),(0,0),(2,0),(4,0)]
-}
movePM :: Tabuleiro -> Coordenadas -> Orientacao -> [[Coordenadas]] -> Comando
movePM _ _ _ [] = "IMPOSSIVEL"
movePM tab (x,y) o (h:t) | elem '1' (move tab (x,y) o h) = movePM tab (x,y) o t
                         | otherwise = move tab (x,y) o h
{-|'move' calcula os comandos necessários para acender todas as luzes. Se não for possível devolve "1".

*Exemplo

>>>move ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' [(3,1),(1,2),(4,3)]
1. orientX ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' (3,1) ++ orientY ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) 'E' (3,1)
 ++ "L"
2. orientY ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' (3,1) ++ orientX ["aaaaA","cAaaa","acaAa","aaaaa"] (0,1) 'N' (3,1)
 ++ "L"
-}
move :: Tabuleiro -> Coordenadas -> Orientacao -> [Coordenadas] -> Comando
move _ _ _ [] = []
move tab (x,y) o (h:t) = if elem '1' l1
                         then if elem '1' l2
                              then "1"
                              else l2 ++ move tab h o2 t
                         else l1 ++ move tab h o1 t
 where l1 = orientX tab (x,y) o h ++ orientY tab (fst h,y) oY h ++ "L"
       l2 = orientY tab (x,y) o h ++ orientX tab (x,snd h) oX h ++ "L"
       o1 | y < (snd h) = 'N'
          | y > (snd h) = 'S'
          | x < (fst h) = 'E'
          | x > (fst h) = 'O'
          | x == (fst h) = o
       o2 | x < (fst h) = 'E' 
          | x > (fst h) = 'O'
          | y < (snd h) = 'N'
          | y > (snd h) = 'S'
          | y == (snd h) = o
       oY | x < (fst h) = 'E'
          | x > (fst h) = 'O'
          | x == (fst h) = o
       oX | y < (snd h) = 'N'
          | y > (snd h) = 'S'
          | y == (snd h) = o

{-|'orientX' calcula os comandos necessários para mover o robot até igualar a coordenada x da lâmpada que pretende atingir.

*Exemplo

>>>orientX ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) 'S' (3,1)
1. rodaE 'S' ++ moveASE ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) (3,1)
-}
orientX :: Tabuleiro -> Coordenadas -> Orientacao -> Coordenadas -> Comando
orientX tab (x1,y1) o (x2,y2) | x1 < x2 = rodaE o ++ moveASE tab (x1,y1) (x2,y2)
                              | x1 > x2 = rodaO o ++ moveASO tab (x1,y1) (x2,y2)
                              | x1 == x2 = []

{-|'rodaE' calcula os comandos que são necessários executar para que o robot fique virado para Este.

*Exemplo

>>>rodaE 'S' = "E" 
-}
rodaE :: Orientacao -> Comando
rodaE o | o == 'E' = ""
        | o == 'S' = "E"
        | o == 'O' = "EE"
        | o == 'N' = "D"

{-|'rodaO' calcula os comandos que são necessários executar para que o robot fique virado para Oeste.

*Exemplo

>>>rodaO 'S' = "D" 
-}
rodaO :: Orientacao -> Comando
rodaO o | o == 'O' = ""
        | o == 'N' = "E"
        | o == 'E' = "EE"
        | o == 'S' = "D"

{-|'moveASE' calcula os comandos necessários, um a um, para mover o robot para Este até igualar a coordenada x da lâmpada 
que pretende atingir.

*Exemplo

>>>moveASE ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) (3,1)
1. verificaAS ["aaaaA","cAaaa","acaAa","aaaaa"] (0,0) (1,0) ++ moveASE ["aaaaA","cAaaa","acaAa","aaaaa"] (1,0) (3,1)
-}
moveASE :: Tabuleiro -> Coordenadas -> Coordenadas -> Comando
moveASE tab (xi,yi) (xf,yf) | xi == xf = []
                            | otherwise = (verificaAS tab (xi,yi) (xi+1,yi)) ++ (moveASE tab (xi+1,yi) (xf,yf))

{-|'moveASO' calcula os comandos necessários, um a um, para mover o robot para Oeste até igualar a coordenada x da lâmpada 
que pretende atingir.

*Exemplo

>>>moveASO ["aaaaA","cAaaa","acaAa","aaaaa"] (4,0) (3,1)
1. verificaAS ["aaaaA","cAaaa","acaAa","aaaaa"] (4,0) (3,0) ++ moveASO ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) (3,1)
-}
moveASO :: Tabuleiro -> Coordenadas -> Coordenadas -> Comando
moveASO tab (xi,yi) (xf,yf) | xi == xf = []
                            | otherwise = (verificaAS tab (xi,yi) (xi-1,yi)) ++ (moveASO tab (xi-1,yi) (xf,yf))

{-|'verificaAS' calcula se para ir de uma coordenada para uma adjacente é neccesário Avançar ou Saltar, 
ou então se não é possível.

*Exemplo

>>>'verificaAS' ["aaaaA","cAaaa","acaAa","aaaaa"] (4,0) (3,0)
1. caracter1 == 'A' && caracter2 == 'A' = "A"
2. caracter1 == 'B' && caracter2 == 'A' = "S"
3. caracter1 == 'A' && carcater2 == 'C' = "1"
-}
verificaAS :: Tabuleiro -> Coordenadas -> Coordenadas -> Comando
verificaAS tab  (x1,y1) (x2,y2) | caracter1 == caracter2 = "A"
                                | ord caracter1 > ord caracter2 || ord caracter1 == (ord caracter2) - 1 = "S"
                                | otherwise = "1"
 where caracter1 = toUpper ((tab !! (length tab - (y1 + 1))) !! x1)
       caracter2 = toUpper ((tab !! (length tab - (y2 + 1))) !! x2)

{-|'orientY' calcula os comandos necessários para mover o robot até igualar a coordenada y da lâmpada que pretende atingir.

*Exemplo

>>>orientY ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) 'E' (3,1)
1. rodaN 'E' ++ moveASN ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) (3,1)
-}
orientY :: Tabuleiro -> Coordenadas -> Orientacao -> Coordenadas -> Comando
orientY tab (x1,y1) o (x2,y2) | y1 < y2 = rodaN o ++ moveASN tab (x1,y1) (x2,y2)
                              | y1 > y2 = rodaS o ++ moveASS tab (x1,y1) (x2,y2)
                              | y1 == y2 = []

{-|'rodaN' calcula os comandos que são necessários executar para que o robot fique virado para Norte.

*Exemplo

>>>rodaN 'S' = "EE" 
-}                              
rodaN :: Orientacao -> Comando
rodaN o | o == 'N' = ""
        | o == 'E' = "E"
        | o == 'S' = "EE"
        | o == 'O' = "D"

{-|'rodaS' calcula os comandos que são necessários executar para que o robot fique virado para Sul.

*Exemplo

>>>rodaS 'S' = "" 
-}  
rodaS :: Orientacao -> Comando
rodaS o | o == 'S' = ""
        | o == 'O' = "E"
        | o == 'N' = "EE"
        | o == 'E' = "D"

{-|'moveASN' calcula os comandos necessários, um a um, para mover o robot para Norte até igualar a coordenada y da lâmpada 
que pretende atingir.

*Exemplo

>>>moveASN ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) (3,1)
1. verificaAS ["aaaaA","cAaaa","acaAa","aaaaa"] (3,0) (3,1) ++ moveASN ["aaaaA","cAaaa","acaAa","aaaaa"] (3,1) (3,1)
-}
moveASN :: Tabuleiro -> Coordenadas -> Coordenadas -> Comando
moveASN tab (xi,yi) (xf,yf) | yi == yf = []
                            | otherwise = (verificaAS tab (xi,yi) (xi,yi+1)) ++ moveASN tab (xi,yi+1) (xf,yf)

{-|'moveASS' calcula os comandos necessários, um a um, para mover o robot para Sul até igualar a coordenada y da lâmpada 
que pretende atingir.

*Exemplo

>>>moveASS ["aaaaA","cAaaa","acaAa","aaaaa"] (3,2) (3,1)
1. verificaAS ["aaaaA","cAaaa","acaAa","aaaaa"] (3,2) (3,1) ++ moveASS ["aaaaA","cAaaa","acaAa","aaaaa"] (3,1) (3,1)
-}
moveASS :: Tabuleiro -> Coordenadas -> Coordenadas -> Comando
moveASS tab (xi,yi) (xf,yf) | yi == yf = []
                            | otherwise = (verificaAS tab (xi,yi) (xi,yi-1)) ++ moveASS tab (xi,yi-1) (xf,yf) 