{-|
Module: Tarefa 2
Description :  Modulo que contem todo o codigo da Tarefa 2 para o projecto de LI

Este modulo recebe o tabuleiro de entrada que através da várias funçoes separa o seus elementos e em seguida executa o primeiro movimento da lista de movimentos-}
module Main where
import Data.Char

-- | Declaração da variavel Board
type Board = (Mapa,Pos,Ori,Movs)
-- | Declaração do array de String que contem o Mapa
type Mapa = [String] 
-- | Declação da variavel Pos, Tuplo que vai contar uma posição do Mapa
type Pos = (Int,Int) 
-- | Declaração da variavel Ori que indica a orientaçao do robot
type Ori = Char 
-- | Declaração da variavel Movs que contem a string dos movimentos
type Movs = String  

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
                   in mov (mapa,pos,ori,move)++"\n"

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

-- | Função que recebe uma Bard e verifica se é possivel efectuar o primeiro movimento da string movimentos retornado a nova posição do robot ou retornado "ERRO" em caso de erro
mov :: Board -> String
mov (mapa,pos,ori,move)= case (head move) of 
                                    'E' -> let ori1 = rodaE ori
                                           in (show (fst pos)++" "++show (snd pos)++" "++[ori1])
                                    'D' -> let ori1 = rodaD ori
                                           in (show (fst pos)++" "++show (snd pos)++" "++[ori1])
                                    'A' -> case ori of 
                                                   'N' -> if (toLower (findPos mapa pos) == toLower(findPos mapa (fst pos,snd pos+1))) 
                                                                then (show (fst pos)++" "++show (snd pos+1)++" "++[ori])
                                                                else "ERRO" 
                                                   'S' -> if (toLower (findPos mapa pos) == toLower(findPos mapa (fst pos,snd pos-1)))
                                                                then (show (fst pos)++" "++show (snd pos-1)++" "++[ori])
                                                                else "ERRO"
                                                   'E' ->if (toLower (findPos mapa pos) == toLower(findPos mapa (fst pos+1,snd pos))) 
                                                                then (show (fst pos+1)++" "++show (snd pos)++" "++[ori])
                                                                else "ERRO"
                                                   otherwise -> if (toLower (findPos mapa pos) == toLower(findPos mapa (fst pos-1,snd pos))) 
                                                                then (show (fst pos-1)++" "++show (snd pos)++" "++[ori])
                                                                else "ERRO"
                                    'S' -> case ori of
                                                    'N' -> if (jumpCheck (findPos mapa pos) (findPos mapa (fst pos,snd pos+1)))
                                                                then (show (fst pos)++" "++show (snd pos+1)++" "++[ori])
                                                                else "ERRO" 
                                                    'S' -> if (jumpCheck (findPos mapa pos) (findPos mapa (fst pos,snd pos-1)))
                                                                then (show (fst pos)++" "++show (snd pos-1)++" "++[ori])
                                                                else "ERRO" 
                                                    'E' -> if (jumpCheck (findPos mapa pos) (findPos mapa (fst pos+1,snd pos)))
                                                                then (show (fst pos+1)++" "++show (snd pos)++" "++[ori])
                                                                else "ERRO" 
                                                    otherwise -> if (jumpCheck (findPos mapa pos) (findPos mapa (fst pos-1,snd pos)))
                                                                then (show (fst pos-1)++" "++show (snd pos)++" "++[ori])
                                                                else "ERRO" 
                                    otherwise -> if isUpper (findPos mapa pos)
                                            then (show (fst pos)++" "++show (snd pos)++" "++[ori])
                                            else "ERRO"

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
rodaD :: Ori -- ^ Orientação de entrada
      -> Ori -- ^ Orientaçao apos rotação
rodaD pos | pos=='N' = 'E'
          | pos=='E' = 'S'
          | pos=='S' = 'O'
          | otherwise ='N'

-- | Função para fazer uma rotação a esquerda ( Recebe a oricientaçao actual devolve a nova)
rodaE :: Ori -- ^ Orientação de entrada
      -> Ori -- ^ Orientaçao apos rotação
rodaE pos | pos=='N' ='O'
          | pos=='O' ='S'
          | pos=='S' ='E'
          | otherwise ='N'

-- | Função que verifica que uma certa posiçao esta dentro do mapa, e se estiver dentro do mapa chama uma funcão auxiliar para saber qual o char dessa posição, caso a posição nao pertença ao mapa devole um " "
findPos :: Mapa -> Pos -> Char
findPos mapa pos | (fst pos < length (head mapa)&& (fst pos >=0) && snd pos < length mapa) && (snd pos >=0) = getLinha mapa pos (0,length(mapa)-1) 
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




