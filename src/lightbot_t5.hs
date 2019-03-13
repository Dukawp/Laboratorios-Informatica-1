{-|
Module: Tarefa 5
Description :  Modulo que contem todo o codigo da Tarefa 5 para o projecto de LI

Este modulo recebe o tabuleiro de entrada que através da várias funçoes cria um ambiente gráfico em html e x3dom com o desenho do jogo e os seus movimentos devidamente animados
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
                mov = head revBoard
                coord = getCoord (revBoard!!1) 
                board = drop 2 revBoard
                ligths = getLights board
            in (printHtml ligths board coord mov)++ "\n"

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
getCoord :: String -- ^String que contem coordenadas e orientacao do bot
         -> Position -- ^ Varialvel que contem coordenadas e oriéntacao do bot no formato (int,int,char)
getCoord x = let l = words x
                 a = read (head l) :: Int
                 b = read (l!!1) :: Int
                 o = head (l!!2)
             in (a,b,o)

-- | Função que vai colocando blocos na posicao até a altura indicada
drawBloco :: Pos -- ^ Posição do bloco a ser colocado
          -> Double -- ^ Altura do bloco
          -> String -- ^ String com codigo html do bloco
drawBloco (x,y) 0 = "<transform translation=\""++show x ++ " 0 "++show y++"\"><shape use=\"bloco\"/></transform>\n"
drawBloco (x,y) h = "<transform translation=\""++show x ++ " "++ show (h/3) ++ " "++show y++"\"><shape use=\"bloco\"/></transform>\n" ++ drawBloco (x,y) (h-1.0)

-- | Funcão que gera codigo html da board
drawBoard :: Mapa  -- ^ Mapa do jogo
          -> Int -- ^ Cordenada y
          -> String -- ^ Codigo html do Mapa
drawBoard [] _= ""
drawBoard (h:t) y = drawBoard' h (0,y) ++ drawBoard t (y-1)


-- | Funcão auxiliar da drawBoard, que gera codigo html da board
drawBoard' :: String -- ^ Linha do mapa
           -> (Int,Int) -- ^ Posição no mapa(x,y)
           -> String -- ^ Codigo html do Mapa
drawBoard' [] _ = ""
drawBoard' (h:t) (x,y) = drawBloco (x,y) (fromIntegral (ord (toLower h)-97) :: Double) ++ drawBoard' t ((x+1),y)

-- | Funçao gera codigo html do robot
drawBot :: Position -- ^ Posiçao do robot
        -> Double -- ^ Altura do robot
        -> String -- ^ Codigo html do Mapa
drawBot (x,y,o) h | o == 'N' = "<Transform translation=\" "++ show x ++" " ++ show (h/3) ++" " ++ show (y*(-1)) ++"\" rotation=\"0 1 0 3.14159\" scale=\"0.2 0.2 0.2\" def=\"bot\"><Shape USE=\"Boneco\"/></Transform>\n"
                  | o == 'S' = "<Transform translation=\" "++ show x ++" " ++ show (h/3) ++" " ++ show (y*(-1)) ++"\" rotation=\"0 1 0 0\" scale=\"0.2 0.2 0.2\" def=\"bot\"><Shape USE=\"Boneco\"/></Transform>\n"
                  | o == 'O' = "<Transform translation=\" "++ show x ++" " ++ show (h/3) ++" " ++ show (y*(-1)) ++"\" rotation=\"0 1 0 4.712385\" scale=\"0.2 0.2 0.2\" def=\"bot\"><Shape USE=\"Boneco\"/></Transform>\n"
                  | otherwise = "<Transform translation=\" "++ show x ++" " ++ show (h/3) ++" " ++ show (y*(-1)) ++"\" rotation=\"0 1 0 1.570795\" scale=\"0.2 0.2 0.2\" def=\"bot\"><Shape USE=\"Boneco\"/></Transform>\n"


-- | Função gera codigo html das luzes
drawLights :: Lights -- ^ Array com posiçao das luzes
           -> Mapa -- ^ Mapa do jogo para saber a altura das luzes
           -> String -- ^ String com html das luzes
drawLights [] _ = ""
drawLights ((x,y):t) b = "<Transform translation='"++ show x ++" " ++ show ((fromIntegral (ord (toLower ((b!!y)!!x))-95) :: Double)/3) ++" " ++ show (y*(-1)) ++"'><Shape USE='Poste'/></Transform>\n\
                            \<transform translation='"++show ((fromIntegral x :: Float) + 0.4) ++" " ++ show (((fromIntegral (ord (toLower ((b!!y)!!x))-95) :: Double)/3)+0.5) ++" " ++ show (((fromIntegral y :: Float) -0.4)*(-1)) ++"'>\n\
                            \<shape>\n\
                            \    <appearance>\n\
                            \        <material def='light"++ show x ++ show y ++"' emissiveColor='0 0 0'></material>\n\
                            \    </appearance>\n\
                            \    <sphere radius='.1'></sphere>\n\
                            \</shape>\n\
                            \</transform>\n" ++ drawLights t b

-- | Função que divide os tempos dos movimentos
movDiv :: [Float] -> String
movDiv [] = ""
movDiv (h:t) = show h ++ " " ++ movDiv t

-- | Função que gera o codigo html da animaçao avançar e saltar
drawMov :: String  -- ^ String dos movimentos
        -> Position -- ^ Posicao do Robot
        -> Double -- ^ Altura do robot
        -> Mapa -- ^ Mapa
        -> String -- ^ String final com resultado em html
drawMov [] _ _ _= ""
drawMov (h:t) p@(x,y,o) l b | h == 'S' && o=='N' = show x ++ " "++ show ((fromIntegral (ord (toLower ((b!!(y+1))!!x))-95) :: Double)/3)++ " " ++ show ((y+1)*(-1)) ++ "\n" ++ drawMov t (x,(y+1),o) (fromIntegral (ord (toLower ((b!!(y+1))!!x))-95) :: Double) b
                            | h == 'S' && o=='S' = show x ++ " "++ show ((fromIntegral (ord (toLower ((b!!(y-1))!!x))-95) :: Double)/3)++ " " ++ show ((y-1)*(-1)) ++ "\n" ++ drawMov t (x,(y-1),o) (fromIntegral (ord (toLower ((b!!(y-1))!!x))-95) :: Double) b
                            | h == 'S' && o=='E' = show (x+1) ++ " "++ show ((fromIntegral (ord (toLower ((b!!y)!!(x+1)))-95) :: Double)/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t ((x+1),y,o) (fromIntegral (ord (toLower ((b!!y)!!(x+1)))-95) :: Double) b
                            | h == 'S' && o=='O' = show (x-1) ++ " "++ show ((fromIntegral (ord (toLower ((b!!y)!!(x-1)))-95) :: Double)/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t ((x-1),y,o) (fromIntegral (ord (toLower ((b!!y)!!(x-1)))-95) :: Double) b
                            | h == 'A' && o=='N' = show x ++ " "++ show (l/3)++ " " ++ show ((y+1)*(-1)) ++ "\n" ++ drawMov t (x,(y+1),o) l b
                            | h == 'A' && o=='S' = show x ++ " "++ show (l/3)++ " " ++ show ((y-1)*(-1)) ++ "\n" ++ drawMov t (x,(y-1),o) l b
                            | h == 'A' && o=='E' = show (x+1) ++ " "++ show (l/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t ((x+1),y,o) l b
                            | h == 'A' && o=='O' = show (x-1) ++ " "++ show (l/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t ((x-1),y,o) l b
                            | h == 'D' = show x ++ " "++ show (l/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t (x,y,(rotateD o)) l b 
                            | h == 'E' = show x ++ " "++ show (l/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t (x,y,(rotateE o)) l b
                            | otherwise = show x ++ " "++ show (l/3)++ " " ++ show (y*(-1)) ++ "\n" ++ drawMov t p l b


-- | Função para animar as mudanças de direçao  
drawRot' :: String -- ^ String dos movimentos
         -> Double -- ^ Valor da orientação
         ->  String -- ^ String com html da animaçao 
drawRot' [] _ = ""
drawRot' (h:t) o | h == 'D' = "0 1 0 " ++ show (o-1.570795) ++ "\n" ++ drawRot' t (o-1.570795)
                 | h == 'E' = "0 1 0 " ++ show (o+1.570795) ++ "\n" ++ drawRot' t (o+1.570795)
                 | otherwise = "0 1 0 " ++ show o ++ "\n" ++ drawRot' t o

-- | Função para animar as mudanças de direçao 
drawRot :: String -- ^ String dos movimentos
        -> Char -- ^ Orientação
        -> String -- ^ String com html da animaçao 
drawRot m o | o == 'N' = "0 1 0 3.1415926 \n" ++ drawRot' m 3.1415926
            | o == 'S' = "0 1 0 0.0 \n" ++ drawRot' m 0.0
            | o == 'E' = "0 1 0 1.570795 \n" ++ drawRot' m 1.570795
            | otherwise = "0 1 0 4.7123876 \n" ++ drawRot' m 4.7123876

-- | Funçao que anima as luzes
turnLight :: Lights -- ^ Array com posiçao das luzes
          -> String  -- ^ String dos movimentos
          -> Position 
          -> String
turnLight l m p = turnLight' l m p (length m)

-- | Funcao que anima as luzes
turnLight' :: Lights -- ^ Array com posiçao das luzes
           -> String 
           -> Position 
           -> Int -- ^ Numero de movimentos
           -> String
turnLight' [] _ _ _= "" 
turnLight' _ [] _ _= ""
turnLight' ((lx,ly):lt) m p s = let lista = changeL (lx,ly) m p 0
                                 in if length lista > 1 
                                    then"<ColorInterpolator def='turn"++show lx ++ show ly ++"'\n\
\                                                    key='"++ movDiv [0.0,(1/fromIntegral s)..1.0] ++ "'\n\
\                                                    keyValue='0 0 0\n"++ turnOn 0 lista ++"'>\n\
\                                            </ColorInterpolator>\n\
\                                            <route fromNode='clock'\n\
\                                                    toNode='turn"++show lx ++ show ly ++"'\n\
\                                                    fromField='fraction_changed'\n\
\                                                    toField='set_fraction'></route>\n\
\                                            <route fromNode='turn"++show lx ++show ly ++ "'\n\
\                                                    toNode='light"++show lx ++show ly ++ "'\n\
\                                                    fromField='value_changed'\n\ 
\                                                    toField='set_emissiveColor'></route>\n"++ turnLight' lt m p s
                                    else 
                                        turnLight' lt m p s

-- | Cria uma array com posiçoes em que a lampada muda de estado
changeL :: (Int,Int) -- ^ Lamapda actual
        -> String -- ^ String dos movimentos
        -> Position -- ^ Posicao do robot
        -> Int -- ^ variavel auxiliar que serve como um contador
        ->[Int] -- ^ Array de mudanças de estado
changeL _  [] _ count = [count]
changeL (lx,ly) (h:t) p@(x,y,o) count | lx == x && ly == y && h == 'L' = (count):changeL (lx,ly) t p (count+1)
                                      | otherwise = changeL (lx,ly) t (move h p) (count+1)

-- | Funcção que gera o codigo de cores para ligar e desligar as luzes
turnOn :: Int -- ^ Variavel auxilar usada para contar o numero segementos da mesma cor
       -> [Int] -- ^ Array que limita dos seguentos em que a lamopada esta acessa ou apagada
       -> String
turnOn _ [] = ""
turnOn _ (a:[]) = ""
turnOn s m@(h:z:t) | s < h = "0 0 0\n" ++ turnOn (s+1) m 
                   | s >= h && s < z = "1 1 0\n" ++ turnOn (s+1) m 
                   | otherwise = turnOn s t 

-- | Funçao que simula movimentos do robot
move :: Char -> Position -> Position
move m p@(x,y,o) | (m == 'A' || m == 'S') && o =='N' = (x,(y+1),o)
                 | (m == 'A' || m == 'S') && o =='S' = (x,(y-1),o)
                 | (m == 'A' || m == 'S') && o =='E' = ((x+1),y,o)
                 | (m == 'A' || m == 'S') && o =='O' = ((x-1),y,o)
                 | m == 'D' = (x,y,rotateD o)
                 | m == 'L' = (x,y,o)
                 | otherwise = (x,y,rotateE o)
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

-- | Função principal com o corpo do html e que usa varias funçoes auxiliares para criar o codigo html
printHtml :: Lights -- ^ Array com posiçao das luzes
          -> Mapa  -- ^ Mapa
          -> Position -- ^ Posicao do robot e orientacao
          -> String -- ^ String dos movimentos
          -> String -- ^ String final com o codigo html
printHtml l b p@(x,y,o) m = "\
\ <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
\ \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
\<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\
\<head>\n\
\       <meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\" />\n\
\       <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />\n\
\       <title>LightBot</title>\n\
\       <style>\n\
\          p.case { clear: both; border-top: 1px solid #888; }\n\
\       </style>\n\
\       <script type=\"text/javascript\" src=\"http://www.x3dom.org/release/x3dom.js\"></script>\n\
\       <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.x3dom.org/release/x3dom.css\"/>\n\
\</head>\n\
\<body>\n\
\    <h1>LightBot</h1>\n\
\    <p>\n\
\        <X3D xmlns=\"http://www.web3d.org/specifications/x3d-namespace\" id=\"shadow\" showStat=\"true\" showLog=\"false\" x=\"0px\" y=\"0px\" width=\"1000px\" height=\"1000px\">\n\
\            <Scene> \n\
\                <Background \n\
\                groundAngle='0.9 1.5 1.57' groundColor='0.21 0.18 0.66 0.2 0.44 0.85 0.51 0.81 0.95 0.51 0.81 0.95' \n\
\                skyAngle='0.9 1.5 1.57' skyColor='0.21 0.18 0.66 0.2 0.44 0.85 0.51 0.81 0.95 0.51 0.81 0.95' \n\ 
\                groundTransparency='0.5 0.5 0.5 0.5' skyTransparency='0.5 0.5 0.5 0.5'></background> \n\
\    <group render=\"false\">\n\
\       <shape def=\"bloco\">\n\
\           <appearance>\n\
\               <material diffuseColor='0.588 0.588 0.588' shininess='0.145'></material>\n\
\               <imageTexture url='http://i.imgbox.com/ggCXqy24.png'></imageTexture>\n\
\           </appearance>\n\
\           <box size=\".9 .3333333333333333 .9\"/>\n\
\       </shape>\n\
\       <group def=\"Poste\">\n\
\           <transform translation=\".4 0 .4\">\n\
\               <shape>\n\
\                   <appearance>\n\
\                       <material diffuseColor=\"0 0 0\"/>\n\
\                   </appearance>\n\
\                   <cylinder radius=\"0.03\" height=\"1\"/>\n\
\               </shape>\n\
\           </transform>\n\
\       </group>\n\
\       <group DEF='Boneco' >\n\
\       <transform translation='0 3.6 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.003922 0.003922 0.003922'></material>\n\
\             </appearance>\n\
\             <cylinder height='1' radius='0.5'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 3.2 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.003922 0.003922 0.003922'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.2'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 2.6 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <pixelTexture2D DEF='PixelColors' repeatS='false' repeatT='false' image='2 4 3 16711680 16776960 30464 16711799 255 16742144 65399 8947848'></pixelTexture2D>\n\
\             </appearance>\n\
\             <sphere radius='0.75'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.3 2.7 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.18'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.3 2.7 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.18'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='1 0 0 1.57' translation='0 2.5 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.901961 0.403922 0' emissiveColor='1 0.145098 0.058824' specularColor='1 0.454902 0.360784'></material>\n\
\             </appearance>\n\
\             <cone bottomRadius='0.3' height='1.75'></cone>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 1.975 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.175' radius='0.8'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.1 0.8'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.725 1.2 0.85'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.3 0.8'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.4 0.75'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.5 0.75'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.6 0.7'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.7 1.7 0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\            </shape>\n\
\        </transform>\n\
\        <transform translation='0.6 1.8 0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\        <transform translation='0.55 1.9 0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.55 2 0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.7 1.1 -0.8'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.725 1.2 -0.85'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.7 1.3 -0.8'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.7 1.4 -0.75'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.7 1.5 -0.75'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\        <transform translation='-0.7 1.6 -0.7'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.7 1.7 -0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.6 1.8 -0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.55 1.9 -0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.55 2 -0.65'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.1 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.2 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.1 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='-0.2 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0.96225 0.19245 -0.19245 1.57' translation='0.1 2.2 0.55'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.219608 0.137255 0'></material>\n\
\             </appearance>\n\
\             <cylinder radius='0.03'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0.45 2.3 1.5'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.219608 0.137255 0'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.2' radius='0.1'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 1.15 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <pixelTexture2D USE='PixelColors'></pixelTexture2D>\n\
\             </appearance>\n\
\             <sphere radius='1.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0 1 1.57' translation='0 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.5 0.25 0.05'></materiString -> Position ->al>\n\
\             </appearance>\n\
\             <cylinder height='4.9' radius='0.1'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0.92848 0.37139 1.57' translation='2 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.5 0.25 0.05'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.9' radius='0.05'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0.84515 -0.50709 0.16903 1.57' translation='1.5 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.5 0.25 0.05'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.7' radius='0.03'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0.64018 0.76822 1.57' translation='-1 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.5 0.25 0.05'></material>\n\
\             </appearance>\n\
\             <cylinder height='1' radius='0.05'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0.70711 -0.70711 1' translation='-1.9 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0.5 0.25 0.05'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.5' radius='0.04'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0 1 1.57' translation='2.45 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <box size='0.5 0.5 0.25'></box>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0.95285 0 0.30345 1.57' translation='2.75 1.5 0.05'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.25' radius='0.25'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0.96352 0 0.26764 1.57' translation='2.55 1.85 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.15' radius='0.15'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='0 0 1 1.57' translation='-2.45 1.5 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <box size='0.5 0.5 0.25'></box>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='-0.95285 0 0.30345 1.57' translation='-2.75 1.5 0.05'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.25' radius='0.25'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform rotation='-0.96352 0 0.26764 1.57' translation='-2.55 1.85 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='1 0.039216 0.243137'></material>\n\
\             </appearance>\n\
\             <cylinder height='0.15' radius='0.15'></cylinder>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 1.7 0.95'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 1.25 1.05'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 0.8 1.05'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <material diffuseColor='0 0 0'></material>\n\
\             </appearance>\n\
\             <sphere radius='0.1'></sphere>\n\
\           </shape>\n\
\         </transform>\n\
\         <transform translation='0 -1 0'>\n\
\           <shape>\n\
\             <appearance>\n\
\               <pixelTexture2D USE='PixelColors'></pixelTexture2D>\n\
\             </appearance>\n\
\             <sphere radius='1.75'></sphere>\n\
\           </shape>\n\
\         </transform>\n\ 
\       </group>\n\ 
\       </group>\n" ++ drawBoard b 0 ++ drawBot p (fromIntegral (ord (toLower ((b!!y)!!x))-95) :: Double) ++ drawLights l b ++"\
\       <timesensor def=\"clock\" cycleInterval=\""++ show ((length m)*2)++"\" loop=\"true\"></timesensor>\n\
\       <positionInterpolator def=\"movimento\"\n\
\           key=\"" ++ movDiv [0.0,(1/fromIntegral(length m))..1.0] ++ "\"\n\
\           keyValue=\""++ show x ++ " " ++ show ((fromIntegral (ord (toLower ((b!!y)!!x))-95) :: Double)/3)++" " ++show (y*(-1)) ++ "\n" ++ drawMov m p (fromIntegral (ord (toLower ((b!!y)!!x))-95) :: Double) b ++"\">\n\
\       </positionInterpolator>\n\
\       <route fromNode=\"clock\"\n\
\           toNode=\"movimento\"\n\
\           fromField=\"fraction_changed\"\n\
\           toField=\"set_fraction\"></route>\n\
\ \n\
\        <route fromNode=\"movimento\"\n\
\           toNode=\"bot\"\n\
\           fromField=\"value_changed\" \n\
\           toField=\"translation\"></route>\n\
\       <orientationInterpolator def=\"rotacao\"\n\
\           key=\"" ++ movDiv [0.0,(1/fromIntegral(length m))..1.0] ++ "\"\n\
\           keyValue=\"" ++ drawRot m o ++ "\">\n\
\       </orientationInterpolator>\n\
\       <route fromNode=\"clock\"\n\
\           toNode=\"rotacao\"\n\
\           fromField=\"fraction_changed\"\n\
\           toField=\"set_fraction\"></route>\n\
\ \n\
\        <route fromNode=\"rotacao\"\n\
\           toNode=\"bot\"\n\
\           fromField=\"value_changed\" \n\
\           toField=\"set_rotation\"></route>\n" ++ turnLight l m p ++ "\
\           </Scene>\n\
\        </X3D>\n\
\    </p>  \n\
\    <script type=\"text/javascript\" src=\"http://examples.x3dom.org/example/x3dom.js\"></script>\n\
\</body>\n\
\</html>"
