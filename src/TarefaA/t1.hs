module Main where

import Data.Char

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr (tarefa (lines inp)))

tarefa :: [String] -> [String]
tarefa [] = ["1"]
tarefa txt = [verificaTab txt]

type Entrada = [String]
type Saida = String
type Tabuleiro = [String]
type Coordenadas = String
type Comandos = String

{- |'verificaTab' recebe o input.
Caso o tabuleiro não seja válido dá como resultado a linha em que ocorre o erro. 
Se for válido (ou seja, se for igual a 0 (zero)) verifica o que está abaixo do tabuleiro.

* Exemplo:

>>>verificaTab ["aacdD","aacaa","bbCaa","0 1 S","SEASLEAADSAL"]
1. validaTab ["aacdD","aacaa","bbCaa"]
2. verificaCoord ["0 1 S","SEASLEAADSAL"] 5 3 (3+1)
-}

verificaTab :: Entrada -> Saida
verificaTab [] = "1"
verificaTab l = if (validaTab tab) == 0 
                then verificaCoord (dropWhile (all isAlpha)l) (length (head tab)) (length tab) (length tab + 1)
                else show (validaTab tab)
 where tab = takeWhile (all isAlpha) l

{- |'validaTab' verifica se todas as linhas do tabuleiro tem o mesmo tamanho. Se sim devolve o valor 0 (zero). 
Se não, devolve o número da linha em que ocorre o erro.

* Exemplo:

>>>validaTab ["aacdD","aacaa","bbCaa"] = 0
-}
validaTab :: Tabuleiro -> Int
validaTab [] = 1
validaTab (x:xs) = aux (length x) 1 (x:xs)
 where aux tam n [] = 0
       aux tam n (y:ys) = if length y == tam
                          then aux tam (n+1) ys
                          else n

{- |'verificaCoord' verifica se as coordenadas são válidas. Se forem verifica o que está abaixo.
Se não forem devolve o número da linha a que corresponde a linha das coordenadas.

* Exemplo:

>>>verificaCord ["0 1 S","SEASLEAADSAL"] 5 3 4
1. validaCoord 5 3 "0 1 S"
2. verificaCom ["SEASLEAADSAL"] (4+1)
-}
verificaCoord :: [String] -> Int -> Int -> Int -> Saida
verificaCoord [] _ _ e = show e
verificaCoord (h:t) x y e = if validaCoord x y h then verificaCom t (e+1)
                                                 else show e

{- |'validaCoord' verifica se as coordenadas são maiores ou iguais que 0 e menores que o tamanho do tabuleiro e verifica se a orientação 
é válida.

* Exemplo:

>>>validaCoord 5 3 "0 1 S" = True
-}
validaCoord :: Int -> Int -> Coordenadas -> Bool
validaCoord xtam ytam [] = False
validaCoord xtam ytam lcoord = case words lcoord of
                               [xcoord,ycoord,[o]] -> eNumero xcoord && 
                                                      eNumero ycoord && 
                                                      read xcoord < xtam && 
                                                      read ycoord < ytam && 
                                                      read xcoord >= 0 && 
                                                      read ycoord >= 0 && 
                                                      elem o "NSEO"
                                                      where eNumero [] = False
                                                            eNumero [x] = isDigit x
                                                            eNumero (x:xs) |isDigit x = eNumero xs
                                                                           |otherwise = False 
                               _ -> False

{- |'verificaCom' verifica se os comandos são válidos. Caso não sejam devolve o número da linha a que corresponde a linha dos 
comandos. Se forem verifica ainda se o que está abaixo é vazio'. 
Se não houver nada abaixo dos comandos significa que o tabuleiro é válido. Se houver devolve o número a que corresponde a 
primeira linha abaixo dos comandos. 

* Exemplo:

>>> verificaCom ["SEASLEAADSAL"] 5
1. validaComand "SEASLEAADSAL"

-}
verificaCom :: [String] -> Int -> Saida
verificaCom [] x = show x
verificaCom (h:t) x = if validaComand h then if t == [] then "OK"
                                                        else show (x+1)
                                        else show x

{- |'validaComand' verifica se a linha dos comando é válida.

* Exemplo:

>>> validaComand "SEASLEAADSAL" = True
-}
validaComand :: Comandos -> Bool
validaComand [] = False
validaComand [x] = elem x "ASEDL"
validaComand (x:xs) | elem x "ASEDL" = validaComand xs
                    | otherwise = False