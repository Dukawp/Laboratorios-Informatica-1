{-|
Module: Tarefa 1
Description :  Modulo que contem todo o codigo da Tarefa 1 para o projecto de LI

Este modulo recebe o tabuleiro de entrada que através da várias funçoes verifica se erro Input contem algum erro
-}

module Main where
import Data.Char


-- | Função que recebe do Input e devolve o Output do programa
main = do entrada <- getContents
          let linhas = lines entrada
          putStr $ validaBoard linhas

 
{- | Função responsavel por validar o mapa, caso o mapa seja valido chama a função validaBot

Propriedades do Mapa:

 * Um tabuleiro de dimensão 'm' x 'n' (com 'm','n' >0) será representado por 'n' linhas contendo cada uma delas uma sequência de 'm' caracteres alfabéticos;

 * Eses caracteres estão associados a diferentes níveis: o caracter 'a' ou 'A' ao nível 0; o 'b' o 'B' ao 1; e assim sucessivamente;

 * A utilização da letra maiúscula ou minúuscula sinaliza se a posição tem ou não uma lâmpada;

 * A posição com coordenadas (0,0) corresponde ao primeiro caracter da n-ésima linha.
 -} 
validaMapa :: [String]  -- ^ array de strings do Input
           -> (Int,Int)  -- ^ Tuplo que vai contar o limite do mapa
           -> String -- ^ String que retorna o resultado final
validaMapa [] _ = ""
validaMapa (h:t) (x,y) | elem ' ' h = validaBot (h:t) (x,y)
                       | elem ' ' (head t) && validaLinha h = validaMapa t ((length h),y+1)                          
                       | notElem ' ' (head t) && length h /= length (head t) = head t
                       | validaLinha h = validaMapa t ((length h),y+1)
                       | otherwise = h

-- | Percorre a string e verifica se cada letra e minuscula ou maiuscula
validaLinha :: String -> Bool
validaLinha "" = True
validaLinha (h:t) | isLower' h || isUpper h = validaLinha t 
                  | otherwise = False

-- | Função auxiliar do "validaLinha" que verifica se a letra esta entre a-z
-- Esta funçao restring o uso de caracteres especiais ao contrario da isLower original
isLower' :: Char -> Bool
isLower' c = ord c >= ord 'a' && ord c <= ord 'z' 

-- | Função auxiliar do "validaLinha" que verifica se a letra esta entre A-Z
-- Esta funçao restring o uso de caracteres especiais ao contrario da isUpper original
isUpper' :: Char -> Bool
isUpper' c = ord c >= ord 'A' && ord c <= ord 'Z'

{- | Função responsavel por validar o robot, caso o mapa seja valido chama a função validaMov
Esta função quando chama a função auxialiar validaBot' separa a string pelos os espaços num array.

Propriedades do robot:

 * Robot tem de estar sempre numa posição valida, ou seja dentro do mapa

 * Robot tem de ter um 'x' um 'y' e uma orientação

 * A orientaçao apenas podes contar um dos seguintes chars 'NSEO'

 * A string nunca pode ter dois espaço seguidos
 -} 
validaBot :: [String] -- ^ Input ja sem a parte do mapa, a primeira linha é os dados do robot
          -> (Int,Int)  -- ^ tamanho do mapa
          -> String -- ^ String retorno do valor final
validaBot [] _= ""
validaBot (h:t) pos| (validaBot' (words h) pos) && (semSep h) = validaMov t
                   | otherwise = h

-- | Função que verifica se na String existem dois espaço seguidos ou espaço no final da string
semSep :: String -> Bool
semSep [] = True
semSep (x:xs) | (x == ' ' ) && ( xs == [] ) = False
              | (x == ' ' ) && ((head xs) == ' ' ) = False
              | otherwise = semSep xs

-- | Verifica se o array que contem todos os dados contem 3 elementos apenas e recorre a funçoes auxiliares para validar cada elemento
-- Verifica tambem se as cordenadas estao dentro do mapa
validaBot' :: [String] -- ^ String com os dados do robot 
           -> (Int,Int) -- ^ Tamanho do mapa
           -> Bool -- ^ Retorna verdadeiro ou falso
validaBot' [] _ = False
validaBot' (h:t) pos | length (h:t) == 3 && validaDig h && validaDig (head t) && validaOri (last t) && read h < fst pos  && read (head t) < snd pos && read h >= 0  && read (head t) >= 0 = True 
                     | otherwise = False

-- | Verifica se a string tem apenas digitos
validaDig:: String -> Bool
validaDig [] = True
validaDig (h:t) | isDigit h = validaDig t 
                | otherwise = False

-- | Verifica se a String contem apenas uma das letras 'NSEO'
validaOri :: String -> Bool
validaOri [] = False
validaOri (h:t) | (elem h "NSEO") && (length (h:t) == 1) = True 
                | otherwise = False


{- | Função valida a string dos movimentos

Propriedades dos movimentos:

 * Apenas podes conter as letras 'ASEDL'

 * Não pode ter espaços

 * e para o input ser correcto nao pode haver mais nada depois dos movimentos no array
 -} 
validaMov :: [String] -- ^ array do input contendo apenas os movimentos, a nao ser que o Input seja invalido
          -> String -- ^ String de retorno
validaMov [] = ""
validaMov (h:t) | h=="" = h
                | (validaMov' h) && (t==[])= "OK" 
                | (validaMov' h) && (t/=[]) = head t
                | otherwise = h

-- | Verifica se contem apenas os movimentos possiveis (ASEDL)
validaMov' :: String -> Bool
validaMov' "" = True
validaMov' (h:t) | elem h "ASEDL" = validaMov' t 
                 | otherwise = False

-- | Procura no input pela string que contem o erro
procLinha :: [String] -- ^ Input
          -> String -- ^ Erro a procurar
          -> Int -- ^ Retorna o numero da linha com o erro
procLinha [] _ = 1
procLinha (h:t) erro | h == erro = 1
                     | otherwise = 1+procLinha t erro

-- | Função principal que vai chamar todas as funcões auxiliares para validar o Input
-- a variavel val guarda o valor retornado das validações, caso seja diferente de @OK@ entao o input nao é valido e a variavel val tem a string do erro
-- chamamos a funçao 'procLinha' que nos vai dar o numero da linha que conta a string guardada em val
validaBoard :: [String] -> String
validaBoard input = let val = validaMapa input (0,0) -- val vai guarda o valor returno das valições do input             
                    in if (val /= "OK") -- caso val seja igual 'OK' o input esta correcto, caso o val seja diferente este conta a string com o erro
                           then show (procLinha input val)++"\n"
                           else "OK\n"
