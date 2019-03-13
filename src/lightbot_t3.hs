{-|
Module: Tarefa 3
Description :  Modulo que contem todo o codigo da Tarefa 3 para o projecto de LI

Este modulo recebe o tabuleiro de entrada que através da várias funçoes separa o seus elementos e os trabalha até ao final de todos os movimentos do robot e retorna o rusultado final
-}
module Main where
import Data.Char

-- | Declaração da variavel Board
type Board = (Mapa,Pos,Ori,Movs,Save,Tick,Lights, NLights, Size)
-- | Declaração do array de String que contem o Mapa
type Mapa = [String] 
-- | Declação da variavel Pos, Tuplo que vai contar uma posição do Mapa
type Pos = (Int,Int) 
-- | Declaração da variavel Ori que indica a orientaçao do robot
type Ori = Char 
-- | Declaração da variavel Movs que contem a string dos movimentos
type Movs = String  
-- | Declaração da variavel Size, guarda a maior posiçao do tabuleiro
type Size = Pos
-- | Declaração da variavel NLights, que vai conter o numero de luzes do tabuleiro
type NLights = Int
-- | Declaração variavel Lights, vai conter o numero de luzes apagadas
type Lights = [Pos]
-- | Declaração da variavel Tick, conta o numero de movimentos validos
type Tick = Int
-- | Declaração da varievel Save, String usada para salvar a posiçao do robot sempre que acender ou apagar uma luz
type Save = String

-- | Função que recebe do Input e devolve o Output do programa
main = do entrada <- getContents
          let linhas = lines entrada
          putStr $ geraBoard linhas

-- | Esta função é responvel por passar um array de Strings para uma variavel do tipo Board que é usada para chamar a função mov
geraBoard :: [String] -> String
geraBoard input = let mapa = getBoard input
                      pos = getPos (getBot input)
                      ori = getOri (getBot input)
                      move = last input
                      maxX = (length(head mapa))-1
                      maxY = (length mapa)-1
                      lights = getLights (reverse mapa) 0
                      nlights = length lights
                  in mov(mapa,pos,ori,move,"",0,lights,nlights,(maxX,maxY)) ++"\n"


-- | Função que procura no Array a String que contem a posição e orietação do robot e devolve essa String
getBot :: [String] -> String
getBot (h:t) | elem ' ' h = h
             | otherwise = getBot t

-- | Função que retira todas as String anterioes a String que contem um espaço (string da posiçao do robot) e devolve esse array de strings (Mapa)
getBoard :: [String] -> Mapa
getBoard [] = []
getBoard (h:t) | elem ' ' h = []
               | otherwise = h:(getBoard t)

-- | Função que recebe uma string em que os dois primeiros elementos sao a posição do robot e devolve esses mesmos elementos num tuplo
getPos :: String -> Pos
getPos [] = (0,0)
getPos input = let pos = words input
                    in (read(head pos),read(head (tail pos)))  


-- | Recebe uma String com a posiçao e orientaçao do robot separados por espaços e devolve a orientaçao do robot que é a ultima posiçao da string
getOri :: String -> Ori
getOri bot = last bot

-- | Recebe uma Mapa e chama uma funçao auxiliar que devolve um array de luzes
getLights :: Mapa  -- ^ Mapa contento as luzes e niveis do caminho
          -> Int  -- ^ Inteiro usadao para saber qual a cordenada y
          -> Lights -- ^ Array de luzes
getLights [] _ = []
getLights (h:t) y = (getLights' h (0,y))++(getLights t (y+1))

-- | Função auxiliar da funçao getLights que percorre a string a procura de luzes e devolve a posição das luzes num array 
getLights' :: String -- ^ String pertencente ao Mapa
           -> (Int,Int) -- ^ Cordenadas
           -> Lights -- ^ Array de luzes
getLights' "" _ = []
getLights' (h:t) (x,y) | isUpper h = (x,y):(getLights' t (x+1,y))
                       | otherwise = getLights' t (x+1,y)


-- | Função recursiva responsavel pelo ciclo que vai percorrer toda a String de movimentos, chamar a função responsavel por casa movimento e devolve o resultado final
mov :: Board -> String
mov (mapa,_,_,[],fim,tick,lights, nlights, lim) = if nlights == 0
                                                  then fim++"FIM "++(show tick)
                                                  else fim++"INCOMPLETO"
mov (mapa,pos,ori,(h:t),fim,tick,lights, nlights, lim) = if nlights == 0
                                                          then fim++"FIM "++(show tick)
                                                          else case h of 
                                                                      'E' -> mov (mapa,pos,rodaE ori,t,fim,tick+1,lights, nlights, lim)
                                                                      'D' -> mov (mapa,pos,rodaD ori,t,fim,tick+1,lights, nlights, lim)
                                                                      'A' -> case ori of 
                                                                                     'N' -> if (toLower (findPos mapa pos lim) == toLower(findPos mapa (fst pos,snd pos+1) lim)) 
                                                                                                  then mov(mapa,(fst pos,snd pos+1),ori,t,fim,tick+1,lights, nlights, lim) 
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                     'S' -> if (toLower (findPos mapa pos lim) == toLower(findPos mapa (fst pos,snd pos-1) lim))
                                                                                                  then mov(mapa,(fst pos,snd pos-1),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                     'E' -> if (toLower (findPos mapa pos lim) == toLower(findPos mapa (fst pos+1,snd pos) lim)) 
                                                                                                  then mov(mapa,(fst pos+1,snd pos),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                     otherwise -> if (toLower (findPos mapa pos lim) == toLower(findPos mapa (fst pos-1,snd pos) lim)) 
                                                                                                  then mov(mapa,(fst pos-1,snd pos),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                      'S' -> case ori of
                                                                                      'N' -> if (jumpCheck (findPos mapa pos lim) (findPos mapa (fst pos,snd pos+1) lim))
                                                                                                  then mov(mapa,(fst pos,snd pos+1),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                      'S' -> if (jumpCheck (findPos mapa pos lim) (findPos mapa (fst pos,snd pos-1) lim))
                                                                                                  then mov(mapa,(fst pos,snd pos-1),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                      'E' -> if (jumpCheck (findPos mapa pos lim) (findPos mapa (fst pos+1,snd pos) lim))
                                                                                                  then mov(mapa,(fst pos+1,snd pos),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                                      otherwise -> if (jumpCheck (findPos mapa pos lim) (findPos mapa (fst pos-1,snd pos) lim))
                                                                                                  then mov(mapa,(fst pos-1,snd pos),ori,t,fim,tick+1,lights, nlights, lim)
                                                                                                  else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights, nlights, lim)
                                                                      otherwise -> if (isLight pos lights)
                                                                                        then if isUpper(findPos mapa pos lim)
                                                                                                 then mov((turnLight mapa pos (snd lim)),pos,ori,t,fim++show(fst pos)++" "++show(snd pos)++"\n",tick+1,lights, nlights-1, lim)
                                                                                                 else mov((turnLight mapa pos (snd lim)),pos,ori,t,fim++show(fst pos)++" "++show(snd pos)++"\n",tick+1,lights, nlights+1, lim)
                                                                                        else mov(mapa,(fst pos,snd pos),ori,t,fim,tick,lights,nlights,lim)


{- | Verifica se é possivel da o salto

Propriedades do Salto:

 * So pode saltar se a proxima posição for 1 nivel acima;

 * Nunca pode saltar para o mesmo nivel;

 * Podes saltar para qualquer nivel desde que seja inferior ao nivel inicial ou seja pode saltar do nivel 'z' para o 'a'.
 -}
jumpCheck :: Char -- ^ Nivel actual
          -> Char -- ^ Nivel da Proxima posição
          -> Bool -- ^ Devolve verdadeiro oui falto
jumpCheck a b = if (b /= ' ' && (ord(toLower a) - ord(toLower b) >= 1 || ord(toLower (a)) - ord(toLower b) == -1))
                    then True
                    else False

-- | Função para fazer uma rotação a direita ( Recebe a oricientaçao actual devolve a nova)
rodaD :: Ori -> Ori
rodaD pos | pos=='N' = 'E'
          | pos=='E' = 'S'
          | pos=='S' = 'O'
          | otherwise ='N'

-- | Função para fazer uma rotação a esquerda ( Recebe a oricientaçao actual devolve a nova)
rodaE :: Ori -> Ori
rodaE pos | pos=='N' ='O'
          | pos=='O' ='S'
          | pos=='S' ='E'
          | otherwise ='N'
-- | Função que verifica que uma certa posiçao esta dentro do mapa, e se estiver dentro do mapa chama uma funcão auxiliar para saber qual o char dessa posição, caso a posição nao pertença ao mapa devole um " "
findPos :: Mapa -> Pos -> Size -> Char
findPos mapa (x,y) (maxX,maxY) | (x <= maxX) && (x >=0) && (y <= maxY) && (y >=0) = getLinha mapa (x,y) (0,maxY)
                               | otherwise = ' ' 

{- | Função que procura uma posiçao no mapa e retorna o valor dessa posiçao no mapa -}
getLinha :: Mapa -- ^ Mapa
         -> Pos -- ^ Posição a ser procurada
         -> (Int,Int) -- ^ Tuplo contendo a posição actual
         -> Char -- ^ devolve o char
getLinha (h:t) pos (x,y) | snd pos == y = getCol h pos x
                         | snd pos /= y = getLinha t pos (x,y-1)
-- | Função auxiliar da função getLinha, retorna o char da posiçao pretendida numa string (esta função podia tambem ter sido feita recorrendo ao drop)
getCol :: String  -- ^ Linha do mapa que procuramos na funçao anterior
       -> Pos -- ^ Posição a procurar na String
       -> Int  -- ^ Posição actual da string
       -> Char -- ^ Valor da posiçao encontrada
getCol (h:t) pos x | fst pos == x = h
                   | fst pos /= x = getCol t pos (x+1)

-- | Verifica se uma dada posiçao é uma luz comparando com a array de luzes
isLight :: Pos -> Lights -> Bool
isLight _ [] = False
isLight pos (h:t) | pos == h = True
                  | otherwise = isLight pos t


-- | Função que liga e desliga luzes, e retorna o mapa com a luz alterada
turnLight :: Mapa  -- ^ Mapa
          -> Pos  -- ^ Posiçao a Ligar ou desligar a luz
          -> Int  -- ^ Inteiro auxliar para percorrer o mapa
          -> Mapa -- ^ Mapa com o valor da luz alterado
turnLight (h:t) pos count | snd pos == count = (turnLight' h (fst pos) 0):t
                          | snd pos /= count = h:(turnLight t pos (count-1)) 

-- | Função auxiliar de turnLight
turnLight' :: String -- ^ String com uma linha do mapa 
           -> Int -- ^ Posiçao a procurar na string
           -> Int  -- ^ Posiçao Actual
           -> String -- ^ Nova String, que ja contem o valor da luz alterado e que vai ser usada no mapa
turnLight' [] _ _ = []
turnLight' (h:t) x count | x == count = if isUpper h then (toLower h):t else (toUpper h):t
                         | x /= count = h:(turnLight' t x (count+1))