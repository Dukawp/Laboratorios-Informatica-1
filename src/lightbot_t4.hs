{-|
Module: Tarefa 4
Description :  Modulo que contem todo o codigo da Tarefa 4 para o projecto de LI

Este modulo recebe o tabuleiro de entrada que através da várias funçoes tenta criar uma string de movimentos que resolve o jogo
-}

module Main where
import Data.Char
import Data.List

-- | Declaração da variavel Position
type Position = (Int,Int,Char)
-- | Declaração do array de String que contem o Mapa
type Mapa = [String]
-- | Declação da variavel Pos, Tuplo que vai contar uma posição do Mapa
type Pos = (Int,Int) 
-- | Declaração variavel Lights, vai conter o numero de luzes apagadas
type Lights = [Pos]

-- | Função que recebe do Input e devolve o Output do programa
main = do entrada <- getContents
          let linhas = lines entrada
          putStr $ process linhas

-- | Esta função é responvel por passar um array de Strings para uma string com o codigo html gerado pelo programa
process :: [String] -> String
process x = let revBoard = reverse x
                coord = getCoord (head revBoard)
                board = tail revBoard
                ligths = sortBy (compareDist (tuple coord)) (getLights board)
                ligthsSorted = ordLigths (head ligths) (tail ligths)
            in (resolveMov ligthsSorted board coord)++ "\n"

-- | Funcao principal que retorna a string dos movimentos
resolveMov :: Lights -> [String] -> Position -> String
resolveMov [] _ _ = []
resolveMov l@((lx,ly):lz) b p@(x,y,o) | o == 'E' && lx > x || o == 'O' && lx < x = checkSorA l b p
                                      | o == 'N' && ly > y || o == 'S' && ly < y = checkSorA l b p
                                      | lx == x && ly == y ='L':(resolveMov lz b (x,y,o))
                                      |otherwise = rotate l b p

-- | Funcao para movimentos horizontais
movHor :: Lights -> [String] -> Position -> String
movHor l@((lx,ly):lz) b (x,y,o) | lx > x && o =='E' ='A':(movHor l b (x+1,y,o)) 
                                | lx < x && o =='O' ='A':(movHor l b (x-1,y,o)) 
                                | otherwise = resolveMov l b (x,y,o)

-- | Funcoes para movimentos verticais
movVer :: Lights -> [String] -> Position -> String
movVer l@((lx,ly):lz) b (x,y,o) | ly > y && o =='N' ='A':(movVer l b (x,y+1,o)) 
                                | ly < y && o =='S' ='A':(movVer l b (x,y-1,o)) 
                                | otherwise = resolveMov l b (x,y,o)

-- | Funcao que roda o robot
rotate :: Lights -> [String] -> Position -> String
rotate l@((lx,ly):lz) b p@(x,y,o)  | ly > y && o == 'E' ='E':(resolveMov l b (x,y,rotateE o))
                                   | ly > y && o /= 'N' ='D':(resolveMov l b (x,y,rotateD o))
                                   | ly < y && o == 'E' ='D':(resolveMov l b (x,y,rotateD o))
                                   | ly < y && o /= 'N' ='E':(resolveMov l b (x,y,rotateE o))
                                   | lx > x && o == 'S' ='E':(resolveMov l b (x,y,rotateE o))
                                   | lx > x && o /= 'S' ='D':(resolveMov l b (x,y,rotateD o))
                                   | lx < x && o == 'S' ='D':(resolveMov l b (x,y,rotateD o))
                                   | lx < x && o /= 'S' ='E':(resolveMov l b (x,y,rotateE o))

-- | Verifica se o movimento deve ser saltar ou avançar
checkSorA :: Lights -> [String] -> Position -> String
checkSorA l@((lx,ly):lz) b p@(x,y,o) = case o of 
                                            'N' -> if toLower(getLevel (x,y) b) == toLower(getLevel (x,y+1) b)
                                                    then 'A':(resolveMov l b (x,y+1,o)) 
                                                    else if jumpCheck (getLevel (x,y) b) (getLevel (x,y+1) b)
                                                         then 'S':(resolveMov l b (x,y+1,o)) 
                                                         else rotate l b p
                                            'S' -> if toLower(getLevel (x,y) b) == toLower(getLevel (x,y-1) b)
                                                    then 'A':(resolveMov l b (x,y-1,o)) 
                                                    else if jumpCheck (getLevel (x,y) b) (getLevel (x,y-1) b)
                                                         then 'S':(resolveMov l b (x,y-1,o)) 
                                                         else rotate l b p
                                            'E' -> if toLower(getLevel (x,y) b) == toLower(getLevel (x+1,y) b)
                                                    then 'A':(resolveMov l b (x+1,y,o)) 
                                                    else if jumpCheck (getLevel (x,y) b) (getLevel (x+1,y) b)
                                                         then 'S':(resolveMov l b (x+1,y,o)) 
                                                         else rotate l b p
                                            otherwise -> if toLower(getLevel (x,y) b) == toLower(getLevel (x-1,y) b)
                                                    then 'A':(resolveMov l b (x-1,y,o)) 
                                                    else if jumpCheck (getLevel (x,y) b) (getLevel (x-1,y) b)
                                                         then 'S':(resolveMov l b (x-1,y,o)) 
                                                         else rotate l b p

-- | Funcao devolve o level em char
getLevel :: (Int,Int) -> [String] -> Char
getLevel (x,y) b = ((b!!y)!!x)

-- | Verifica se o salto é possivel
jumpCheck :: Char -- ^ Nivel actual
          -> Char -- ^ Nivel da Proxima posição
          -> Bool -- ^ Devolve verdadeiro oui falto
jumpCheck a b = if (b /= ' ' && (ord(toLower a) - ord(toLower b) == 1 || ord(toLower (a)) - ord(toLower b) == -1))
                    then True
                    else False

-- | Função rodar robot
rotateD :: Char -> Char
rotateD 'N' = 'E'
rotateD 'S' = 'O'
rotateD 'E' = 'S'
rotateD 'O' = 'N'
-- | Função rodar robot
rotateE :: Char -> Char
rotateE 'N' = 'O'
rotateE 'S' = 'E'
rotateE 'E' = 'N'
rotateE 'O' = 'S'

-- | Retira orientacao da posicao
tuple :: Position -> Pos
tuple (a,b,_) = (a,b)

-- | Funcao que devolve a distancia entre 2 pontos 
dist :: Pos -> Pos -> Float
dist (a,b) (c,d) = dist' (fromIntegral a,fromIntegral b) (fromIntegral c,fromIntegral d)

-- | Funcao que devolve a distancia entre 2 pontos
dist' :: (Float, Float) -> (Float, Float) -> Float
dist' (x1, y1) (x2, y2) = sqrt( (x2 - x1)^2 + (y2 - y1)^2 )

-- | Funcao que orderna o arrays de luzes por distancia
ordLigths :: Pos -> Lights -> Lights
ordLigths pos [] = [pos]
ordLigths pos list = let end = sortBy (compareDist pos) list
                      in pos:ordLigths (head end) (tail end)

-- |  compara distancia entre pontos
compareDist:: Pos -> Pos -> Pos -> Ordering
compareDist a b c 
    | dist a b < dist a c = LT
    | dist a b > dist a c = GT
    | dist a b == dist a c = EQ

-- | Recebe uma Mapa e chama uma funçao auxiliar que devolve um array de luzes
getLights :: Mapa -- ^ Mapa contento as luzes e niveis do jogo
          -> Lights -- ^ Array de luzes
getLights b = getLights' b 0

-- | Função auxiliar da funçao getLights que percorre o Mapa e envia uma string para a função auxiliar 
getLights' :: Mapa -- ^ Mapa contento as luzes e niveis do jogo
           -> Int -- ^ Valor y
           -> Lights -- ^ Array de luzes
getLights' [] _ = []
getLights' (l:ls) line = findLight  l line 0 ++ getLights' ls (line+1)

-- | Função auxiliar da funçao getLights que percorre o Mapa a procura de luzes e devolve a posição das luzes num array 
findLight :: String -- ^ String que contem uma linha do Mapa
          -> Int -- ^ Valor y
          -> Int -- ^ Valor x
          -> Lights -- ^ Array de luzes
findLight [] _ _ = []
findLight (l:ls) line col | isUpper l = (col,line) : findLight ls line (col+1)
                          | otherwise = findLight ls line (col+1)

-- | Função que devolve as cordedanas do Robot
getCoord :: String -- ^ String que contem coordenadas e orientacao do bot
         -> Position -- ^ Varialvel que contem coordenadas e oriéntacao do bot no formato (int,int,char)
getCoord x = let l = words x
                 a = read (head l) :: Int
                 b = read (l!!1) :: Int
                 o = head (l!!2)
             in (a,b,o)