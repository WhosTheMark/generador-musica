module Main where

import Prelude hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import qualified Data.Map as Map
import System.Random 

type Modelo = (Int, Map.Map Evento Int, Map.Map (Evento,Evento) Int)
type DistribOrd1 = [((Evento,Evento), Float)]
type DistribOrd0 = [(Evento, Float)]
type ContextoInic = (DistribOrd0,DistribOrd1)

-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50

semilla :: StdGen
semilla = mkStdGen 3967

{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
{-componer :: IO ()
componer = componer' directorio

componer' :: String -> IO ()
componer' dir = do
  (seqs, filenames) <- loadMusicXmls dir
  -- let modelo = ...
  -- let composicion = ...
  putStrLn $ show composicion
  play $ sequenceToMusic composicion
  -}
  
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

convertir:: Modelo -> ContextoInic
convertir modelo = obtenerProb modelo where
   obtenerProb (cant, ord0, ord1) = (normalizar dist0,normalizar dist1) where
      dist0 = map (dividir cant) (Map.toList ord0)
      dist1 = map (dividir (cant-1)) (Map.toList ord1)
      dividir len (event, frec) = (event, (fromIntegral frec) / (fromIntegral len)) 
   
normalizar :: [(a,Float)] -> [(a,Float)]
normalizar xs = map normAux xs where
   total = sum $ map snd xs
   normAux (e,x) = (e,x / total)

-- getStdRandom (randomR (0.0,1.0))   

selectEvent :: [(a,Float)] -> Float -> a
selectEvent dist numero = fst (last (takeWhile (\(x,y) -> y < numero) rango)) where
   (event, prob) = unzip dist
   rango = zip event (tail $ scanl (+) 0 prob)
   
-- crearComposicion :: ContextoInic -> [Evento]
-- crearComposicion contexto@(dist0,dist1) = auxComposicion contexto [evento] gen where
--    (numero,gen) = randomR (0.0,1.0) semilla
--    evento = selectEvent dist0 numero   

--auxComposicion :: ContextoInic -> [Evento] -> StdGen -> [Evento]
--auxComposicion contexto@(dist0,dist1) eventos gen  


-- P(A/B) = ((B,A),probabilidad)

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
{-buscar :: Int -> IO ()
buscar = buscar' directorio
  
buscar' :: String -> Int -> IO ()
buscar' dir = do
  seqfns <- loadMusicXmls dir
  let seqfns_ordenados = unzip $ sortBy (compare `on` snd) $ zip seqfns
  -- ...
  -}
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
