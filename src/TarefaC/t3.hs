module Main where

import Data.Char

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

main = do inp <- getContents
          putStr (outStr (tarefa (lines inp)))

tarefa :: [String] -> [String]
tarefa txt = msg
 where -- |Tabuleiro
       tab = takeWhile (all isAlpha) txt 
       -- |Linha das Coordenadas
       linhaCoord = words (txt !! (length txt - 2)) 
       -- |Linha dos Comandos
       linhaCom = txt !! (length txt - 1) 
       msg = proxsPos tab (luzesTab tab) (length (head tab),length tab) linhaCom (linhaCoord,[],[],0)

type Tabuleiro = [String]
type Comandos = String
type Coordenadas = [String]
-- |Lista de coordenadas onde o comando L é executado com sucesso, mas que não tem coordenadas repetidas
type Luzes = [String] 
-- |Lista de coordenadas onde o comando L é executado com sucesso
type LuzesTotal = [String] 
type Contagem = Int
type Resultado = [String]
type Comando = Char
type Acumulador = (Coordenadas,Luzes,LuzesTotal,Contagem)
type CoordenadaX = Int
type CoordenadaY = Int
type Orientacao = Char
type CaracterTab = Char
-- |Lista de coordenadas em que existe lâmpada no tabuleiro
type LuzesTabuleiro = [String]
-- |Dimensão do tabuleiro
type Dimensao = (Int,Int)

{- |'proxsPos' calcula a proxima posicao usando a posicao calculada com o comando anterior ('proxPos').
No entanto antes de calcular a próxima posição verifica se todas as luzes estão acesas. Caso estejam termina e mostra o 
resultado.

*Exemplos:

>>>proxsPos ["aacdD","aacaa","bbCaa"] ["0 2","4 2"] (5,3) "SEASLEAADSAL" (["0","1","S"],[],[],0)
1. pertence ["0 2","4 2"] [] = False
2. proxsPos ["aacdD","aacaa","bbCaa"] ["0 2","4 2"] (5,3) "EASLEAADSAL" (proxPos ["aacdD","aacaa","bbCaa"] (5,3) 'S' (["0","1","S"],[],[],0))

>>>proxsPos ["aacdD","aacaa","bbCaa"] ["0 2","4 2"] (5,3) "EAADSAL" (["2","0","E"],["2 0"],["2 0"],5)
1. pertence ["0 2","4 2"] ["2 0"] = False
2. proxsPos ["aacdD","aacaa","bbCaa"] ["0 2","4 2"] (5,3) "AADSAL" (proxPos ["aacdD","aacaa","bbCaa"] (5,3) 'E' (["2","0","E"],["2 0"],["2 0"],5))
-}
proxsPos :: Tabuleiro -> LuzesTabuleiro -> Dimensao -> Comandos -> Acumulador -> Resultado
proxsPos tab l (dimx,dimy) [] (coord,coordL,coordLT,y) = if pertence l coordL 
                                                         then coordLT ++ ["FIM " ++ show y]
                                                         else coordLT ++ ["INCOMPLETO"]
proxsPos tab l (dimx,dimy) (h:t) (coord,coordL,coordLT,y) = if pertence l coordL 
                                                            then coordLT ++ ["FIM " ++ show y]
                                                            else proxsPos tab l (dimx,dimy) t (proxPos tab (dimx,dimy) h (coord,coordL,coordLT,y))

{- |'proxPos' calcula a próxima posição dependendo do comando que seja para executar. Caso o comando seja aplicável, mostra 
a nova posição, e acresenta 1 (um) ao contador. 
No caso de o comando ser L (e for válido), verifica se a coordenada em que está já foi executada (para efeitos de 
comparação com as coordenadas existentes no tabuleiro) e também acrescenta essa cooordenada à lista de coordenadas que já 
passaram por este processo.
Caso o comando não há alterações. 

*Exemplos:

>>> proxPos ["aacdD","aacaa","bbCaa"] (5,3) 'S' (["0","1","S"],[],[],0)
1. verificaS = validaCoord 5 3 proxPosAS
2. (["0","0","S"],[],[],0+1)

>>> proxPos ["aacdD","aacaa","bbCaa"] (5,3) 'L' (["2","0","E"],[],[],4)
1. isUpper 'C' = True
2. (["2","0","E"],verificaRep "0 2" [],[]++"2 0",4+1)
-}
proxPos :: Tabuleiro -> Dimensao -> Comando -> Acumulador -> Acumulador
proxPos tab (dimx,dimy) x (coord,coordL,coordLT,y) |x == 'S' && verificaS = (proxPosAS,coordL,coordLT,y+1)
                                                   |x == 'A' && verificaA = (proxPosAS,coordL,coordLT,y+1)
                                                   |x == 'L' && isUpper caracteri = (coord,verificaRep (coordAcende) coordL,coordLT ++ [coordAcende],y+1)
                                                   |x == 'D' = (proxPosD coordxi coordyi orient,coordL,coordLT,y+1)
                                                   |x == 'E' = (proxPosE coordxi coordyi orient,coordL,coordLT,y+1)
                                                   |otherwise = (coord,coordL,coordLT,y)
 where verificaS = validaCoord dimx dimy proxPosAS && validaS inicial final
       verificaA = validaCoord dimx dimy proxPosAS && (inicial == final)
       proxPosAS = proxPosSA coordxi coordyi orient
       coordxi = read (coord !! 0)
       coordyi = read (coord !! 1)
       orient = head (coord !! 2)
       caracteri = (tab !! (length tab - (coordyi + 1))) !! coordxi
       inicial = toUpper caracteri
       coordxf = read ((proxPosAS) !! 0)
       coordyf = read ((proxPosAS) !! 1)
       caracterf = (tab !! (length tab - (coordyf + 1))) !! coordxf
       final = toUpper (caracterf)
       coordAcende = show coordxi ++ " " ++ show coordyi

{- |'proxPosSA' calcula a próxima posição para o comando saltar e avançar dependendo da sua orientação

* Exemplo:

>>>proxPosSA 0 1 S = "0 0 S"
-}
proxPosSA :: CoordenadaX -> CoordenadaY -> Orientacao -> Coordenadas
proxPosSA x y o | o == 'N' = show x:show (y+1):"N":[]
                | o == 'S' = show x:show (y-1):"S":[]
                | o == 'E' = show (x+1):show y:"E":[]
                | o == 'O' = show (x-1):show y:"O":[]

{- |'validaCoord' verifica se a uma coordenada é válida, isto é, se o x e o y são maiores ou iguais a zero e menores que a 
dimensão do tabuleiro.

*Exemplo:

>>>validaCoordf 5 3 ["0","0","S"] = True
-}
validaCoord :: Int -> Int -> Coordenadas -> Bool
validaCoord xtam ytam [xcoord,ycoord,o] = read xcoord < xtam && 
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
proxPosD :: CoordenadaX -> CoordenadaY -> Orientacao -> Coordenadas
proxPosD x y o | o == 'N' = show x:show y:"E":[]
               | o == 'S' = show x:show y:"O":[]
               | o == 'E' = show x:show y:"S":[]
               | o == 'O' = show x:show y:"N":[]

{- |'proxPosE' calcula a próxima posição para o comando esquerda.

*Exemplo:

>>> proxPosE 0 1 S = "0 1 E"
-}
proxPosE :: CoordenadaX -> CoordenadaY -> Orientacao -> Coordenadas
proxPosE x y o | o == 'N' = show x:show y:"O":[]
               | o == 'S' = show x:show y:"E":[]
               | o == 'E' = show x:show y:"N":[]
               | o == 'O' = show x:show y:"S":[]

{- |'pertence' recebe a lista de coordenadas de luzes que existem no tabuleiro e verifica se todas as coordenadas dessa 
lista pertencem à lista das coordenadas que só tem as luzes que estão acesas.

* Exemplo:

>>> pertence ["2 0","4 2"] ["2 0","4 2"] = True

>>> pertence ["2 0","4 2"] ["2 0"] = False
-}
pertence :: Luzes -> Luzes -> Bool
pertence [] _ = True
pertence (h:t) l | elem h l = pertence t l
                 | otherwise = False

{- |'luzesTab','coordLuzes' e 'linhas' calcula a lista de coordenadas onde existe letra maiúscula no tabuleiro, isto é onde 
é preciso acender luz.

*Exemplo:

>>>luzesTab ["aacdD","aacaa","bbCaa"]
1. coordLuzes ["aacdD","aacaa","bbCaa"] 2
-}
luzesTab :: Tabuleiro -> Luzes
luzesTab l = coordLuzes l (length l - 1)
{- |
*Exemplo:

>>> coordLuzes ["aacdD","aacaa","bbCaa"] 2
1. linhas "aacdD" 0 2 ++ coordLuzes ["aacaa","bbCaa"] (2-1)
-}
coordLuzes :: Tabuleiro -> Int -> Luzes
coordLuzes [] _ = []
coordLuzes (h:t) y = linhas h 0 y ++ coordLuzes t (y-1)
{- |
*Exemplo:

>>>linhas "aacdD" 0 2 = ["4 2"]
-}
linhas :: String -> Int -> Int -> Luzes
linhas [] _ _ = []
linhas (h:t) x y |isUpper h = [show x ++ " " ++ show y] ++ linhas t (x+1) y
                 |otherwise = linhas t (x+1) y

{- |'verificaRep' recebe a coordenada em que foi executado o comando L com sucesso e verifica se essa coordenada já existe
na lista das coordenadas que só tem as luzes que estão acesas.

*Exemplo:

>>> verificaRep "4 2" ["2 0"] = ["2 0","4 2"]

>>> verificaRep "2 0" ["2 0] = []
-}
verificaRep :: String -> Luzes -> Luzes
verificaRep x [] = [x]
verificaRep x (h:t) | x == h = t
                    | otherwise = h : verificaRep x t