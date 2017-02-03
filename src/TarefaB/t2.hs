module Main where

import Data.Char

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr (tarefa (lines inp)))

tarefa :: [String] -> [String]
tarefa txt = [msg]
 where -- |Tabuleiro
       tab = takeWhile (all isAlpha) txt 
       -- |Linhas das Coordenadas
       linhaCoord = txt !! (length txt - 2) 
       -- |Coordenada x
       coordxi = read ((words linhaCoord) !! 0) 
       -- |Coordenada y
       coordyi = read ((words linhaCoord) !! 1) 
       -- |Orientação
       orient = head ((words linhaCoord) !! 2) 
       -- |Linha dos Comandos
       linhaCom = txt !! (length txt - 1) 
       -- |Primeiro Comando
       primCom = linhaCom !! 0 
       -- |Linha tabuleiro a que corresponde a coordenada y
       linhaTabi = tab !! (length tab - (coordyi + 1)) 
       -- |Caracter a que corresponde as coordenadas
       caracteri = linhaTabi !! coordxi 
       -- |Caracter transformado em maiúsculo para efeitos de comparação
       inicial = toUpper caracteri 
       -- |Posição calculada caso o comando seja saltar ou avançar
       posf = proxPosSA coordxi coordyi orient 
       -- |Coordenada x na posição calculada por 'posf' (coordenada final)
       coordxf = read ((words posf) !! 0)  
       -- |Coordenada y na posição calculada por 'posf' (coordenada final)
       coordyf = read ((words posf) !! 1) 
       -- |Linha tabuleiro a que corresponde a coordenada final y 
       linhaTabf = tab !! (length tab - (coordyf + 1)) 
       -- |Caracter a que corresponde as coordenadas finais
       caracterf = linhaTabf !! coordxf 
       -- |Caracter transformado em maiusculo para efeitos de comparacao
       final = toUpper caracterf 
       -- |Verifica se a posição que deverá corresponder à seguinte pertence ao tabuleiro e se é possível saltar para essa posição
       verificaS = (validaCoordf (length (head txt)) (length txt - 2) (words posf)) && (validaS inicial final)
       -- |Verifica se a posição que deverá corresponder à seguinte pertence ao tabuleiro e se é possível avançar para essa posição
       verificaA = (validaCoordf (length (head txt)) (length txt - 2) (words posf)) && (inicial == final)
       proxCoordS = if verificaS
                    then posf
                    else "ERRO"
       proxCoordA = if verificaA
                    then posf
                    else "ERRO"
       proxCoordL = if isUpper caracteri
                    then linhaCoord
                    else "ERRO"
       msg |primCom == 'S' = proxCoordS
           |primCom == 'A' = proxCoordA
           |primCom == 'L' = proxCoordL
           |primCom == 'D' = proxPosD coordxi coordyi orient
           |otherwise = proxPosE coordxi coordyi orient

type CoordenadaX = Int
type CoordenadaY = Int
type Orientacao = Char
type CaracterTab = Char
type ProxPosicao = String

{- |'proxPosSA' calcula a próxima posição para o comando saltar e avançar dependendo da sua orientação

* Exemplo:

>>>proxPosSA 0 1 S = "0 0 S"
-}
proxPosSA :: CoordenadaX -> CoordenadaY -> Orientacao -> ProxPosicao
proxPosSA x y o | o == 'N' = show x ++ " " ++ show (y+1) ++ " N"
                | o == 'S' = show x ++ " " ++ show (y-1) ++ " S"
                | o == 'E' = show (x+1) ++ " " ++ show y ++ " E"
                | o == 'O' = show (x-1) ++ " " ++ show y ++ " O"

{- |'validaCoordf' verifica se a uma coordenada é válida, isto é, se o x e o y são maiores ou iguais a zero e menores que a 
dimensão do tabuleiro.

*Exemplo:

>>>validaCoordf 5 3 ["0","0","S"] = True
-}
validaCoordf :: Int -> Int -> [String] -> Bool
validaCoordf xtam ytam [xcoord,ycoord,o] = read xcoord < xtam && 
                                           read ycoord < ytam && 
                                           read xcoord >= 0 && 
                                           read ycoord >= 0

{- |'validaS' verifica se o caracter final é o seguinte do inicial ou se é menor.

*Exemplo:

>>>validaS 'a' 'b' = True
1. ord 'b' = ord 'a' + 1
-}
validaS :: CaracterTab -> CaracterTab -> Bool
validaS x y |x == 'A' = y == 'B'
            |otherwise = ord y < ord x || ord y == ord x + 1

{- |'proxPosD' calcula a proxima posição para o comando direita.

*Exemplo: 

>>> proxPosD 0 1 S = "0 1 O"
-}
proxPosD :: CoordenadaX -> CoordenadaY -> Orientacao -> ProxPosicao
proxPosD x y o | o == 'N' = show x ++ " " ++ show y ++ " E"
               | o == 'S' = show x ++ " " ++ show y ++ " O"
               | o == 'E' = show x ++ " " ++ show y ++ " S"
               | o == 'O' = show x ++ " " ++ show y ++ " N"

{- |'proxPosE' calcula a próxima posição para o comando esquerda.

*Exemplo:

>>> proxPosE 0 1 S = "0 1 E"
-}
proxPosE :: CoordenadaX -> CoordenadaY -> Orientacao -> ProxPosicao
proxPosE x y o | o == 'N' = show x ++ " " ++ show y ++ " O"
               | o == 'S' = show x ++ " " ++ show y ++ " E"
               | o == 'E' = show x ++ " " ++ show y ++ " N"
               | o == 'O' = show x ++ " " ++ show y ++ " S"