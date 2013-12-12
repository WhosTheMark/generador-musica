module Main where

import Prelude hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import qualified Data.Map as Map
import System.Random

{- Modelo con la longitud de la secuencia musical y las frencuencias 
   de los eventos de orden 0 y orden 1. -}
type Modelo = (Int, Map.Map Evento Int, Map.Map (Evento,Evento) Int)

{- Distribucion de orden 0: Una lista de eventos con sus probabilidades. -}
type DistribOrd0 = [(Evento, Float)]

{- Distribucion de orden 1: Una lista de eventos con sus probabilidades. -}
type DistribOrd1 = [((Evento,Evento), Float)]

{- Contexto inicial que contiene las distribuciones de orden 0 y orden 1. -}
type ContextoInic = (DistribOrd0,DistribOrd1)

-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50

-- Funcion main
main :: IO ()
main = componer

{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
componer :: IO ()
componer = componer' directorio

{- Funcion auxiliar de componer. Lee las secuencias musicales de los archivos
   dados, crea un modelo con las secuencias concatenadas, crea un contexto en
   base este modelo y crea una composicion a partir del contexto creado. -}
componer' :: String -> IO ()
componer' dir = do
  (seqs, filenames) <- loadMusicXmls dir
  let modelo = crearModelo $ concat seqs
  let contexto = convertir modelo
  composicion <- crearComposicion contexto
  putStrLn $ show composicion
  play $ sequenceToMusic composicion

{- Crea un modelo en base de la lista de eventos dados. Calcula las frecuencias
   de cada evento de orden 0 y orden 1. -}
crearModelo:: [Evento] -> Modelo
crearModelo sec = (length sec, frecOrd0, frecOrd1) where
   (frecOrd0,frecOrd1) = calcularFrecuencia sec (Map.empty,Map.empty)
   calcularFrecuencia [] modelo =  modelo
   calcularFrecuencia [x] (ord0,ord1) = (agregarFrec x ord0, ord1)
   calcularFrecuencia (x:ys@(y:xs)) (ord0,ord1) = calcularFrecuencia ys (agregarFrec x ord0,agregarFrec (x,y) ord1)
   agregarFrec e orden
      | e `Map.member` orden = Map.insert e (frec+1) orden
      | otherwise = Map.insert e 1 orden
      where Just frec = Map.lookup e orden 

{- Obtiene las probabilidades de cada evento en base al modelo dado para
   crear el contexto inicial de una secuencia musical. -}
convertir:: Modelo -> ContextoInic
convertir modelo = obtenerProb modelo where
   obtenerProb (cant, ord0, ord1) = (normalizar dist0,normalizar dist1) where
      dist0 = map (dividir cant) (Map.toList ord0)
      dist1 = map (dividir (cant-1)) (Map.toList ord1)
      dividir len (event, frec) = (event, (fromIntegral frec) / (fromIntegral len))

{- Normaliza una distribucion dada. -}      
normalizar :: [(a,Float)] -> [(a,Float)]
normalizar xs = map normAux xs where
   total = sum $ map snd xs
   normAux (e,x) = (e,x / total)

{- Dado una distribucion y un numero random, se elige un evento en base a las
   probabilidades de los eventos posibles. -}
selectEvent :: [(a,Float)] -> Float -> a
selectEvent dist numero = fst (last (takeWhile (\(_,y) -> y < numero) rango)) where
   (event, prob) = unzip dist
   rango = zip event (scanl (+) 0 prob)
   
{- Crea una composicion dado un contexto. -}
crearComposicion :: ContextoInic -> IO [Evento]
crearComposicion contexto@(dist0,dist1) = do
   numero <- getStdRandom (randomR (0.0,1.0))
   let evento = selectEvent dist0 numero
   invertida <- auxComposicion contexto [evento]
   return (reverse invertida)

{- Subrutina auxiliar para crear una composicion, se seleccionan eventos hasta 
   que la longitud de la secuencia sea igual a 50. -}
auxComposicion :: ContextoInic -> [Evento] -> IO [Evento]
auxComposicion contexto eventos@(e:_)
   | length eventos /= longitud = do
      let dist = calcularProb contexto e
      numero <- getStdRandom (randomR (0.0,1.0))
      let (_,newEvent) = selectEvent dist numero
      auxComposicion contexto (newEvent:eventos)
   | otherwise = return eventos

{- Calcula las probabilidades de una distribucion de orden 1 
   dado un contexto inicial y un evento. -}
calcularProb :: ContextoInic -> Evento -> DistribOrd1
calcularProb (dist0,dist1) evento = normalizar $ auxProbabilidad dist0 dist1 evento [] where
   auxProbabilidad [] _ _ aux = aux
   auxProbabilidad ((x,z):xs) ys event aux
      | (event,x) `elem` (map fst ys) = auxProbabilidad xs ys event (((event,x),0.3*z+0.7*prob):aux) 
      | otherwise = auxProbabilidad xs ys event (((event,x),0.3*z):aux)
      where Just (_,prob) = find (\i -> (fst i) == (event,x)) ys
         
         
{- Recupera las diez secuencias más similares a la k-ésima secuencia 
   de la colección musical en el directorio por defecto, donde la 
   colección musical ha sido ordenada en orden alfabético por el 
   nombre de archivo. Imprime una lista ordenada de las diez 
   secuencias más similares. En cada fila de la lista se debe indicar 
   el número de la secuencia (relativo al orden alfabético de la 
   colección), el nombre de archivo y la distancia a la consulta.
   -}
buscar :: Int -> IO ()
buscar = buscar' directorio
  
{- Verifica si el numero de la secuencia dada esta en el rango de canciones
   disponibles. Luego llama un subrutina auxiliar para calcular las diez 
   secuencias mas similares al numero de secuencia dada. -}
buscar' :: String -> Int -> IO ()
buscar' dir cancion = do
   (eventos,file) <- loadMusicXmls dir
   if (cancion > 0) && (cancion <= length eventos) then
      buscar'' (eventos,file) cancion
      else
         putStrLn "Indice fuera de rango"
   
{- Calcula las diez secuencias mas similares al numero de secuencia dada. -}
buscar'' :: ([[Evento]], [String]) -> Int -> IO ()
buscar'' (eventos,file) cancion = do   
   let ordenados = sortBy (compare `on` snd) $ zip eventos file
       modelosOrd = map (\(s,n) -> (crearModelo s, n)) ordenados
       modelosEnum = zipWith (\x (s,n) -> (x,s,n)) [1..] modelosOrd 
       (antes,despues) = splitAt cancion modelosEnum
       (_,modPrin,_) = last antes
       (pos,models,nombres) = unzip3 $ (init antes) ++ despues
       distancias = map (distancia modPrin) models
       distanciasOrd = sortBy (compare `on` (\(_,_,x)-> x)) $ zip3 pos nombres distancias
       resultado = take 10 distanciasOrd
       resultStr = map (\(x,y,z) -> (show x, show y, show z)) resultado
       listStr = map (\(x,y,z) -> x ++ "\t" ++ y ++ "\t" ++ z) resultStr
   putStrLn $ unlines listStr
  
{- Calcula la distancia entre dos modelos. -}  
distancia :: Modelo -> Modelo -> Float
distancia (_,map1ord0,map1ord1) (_,map2ord0,map2ord1) =  sqrt (fromIntegral sumaTotal) where
   sumaTotal = (sumar map1ord0 map2ord0) + (sumar map1ord1 map2ord1)
 
{- Suma los valores de las claves de dos mapas asociativos. -}
sumar :: (Ord a) => Map.Map a Int -> Map.Map a Int -> Int
sumar mapa1 mapa2 = sum $ zipWith (\x y -> (x-y)^2) valores1 valores2 where
   claves = union (Map.keys mapa1) (Map.keys mapa2)
   valores1 = map (valor mapa1) claves
   valores2 = map (valor mapa2) claves
 
{- Si la clave dada existe en el mapa asociativo, devuelve el valor de esta.
   Si no, devuelve cero. -} 
valor :: (Ord a) => Map.Map a Int -> a -> Int
valor mapa clave
   | Map.member clave mapa = v
   | otherwise = 0
   where Just v = Map.lookup clave mapa  
  
tocar :: Int -> IO ()
tocar n = do
  seqfns <- loadMusicXmls directorio
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  if (n > 0) && (n <= length seqs) then
    putStrLn (filenames !! (n-1)) >>
    play (sequenceToMusic (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"
          
eventToNote :: Evento -> Music Note1
eventToNote e = note
  where
  d = (fromIntegral $ snd e) / 16
  p = Euterpea.pitch $ fst e
  note = Prim (Note d (p,[]))
  
sequenceToMusic :: [Evento] -> Music Note1
sequenceToMusic es = line $ map eventToNote es
