import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (delete)
    
janela :: Display
janela = InWindow "Jogo" (800,600) (200,100)

type Posicao = (Int,Int)
data Estado = Estado {jogador :: Posicao,
                      tempo :: Float,
                      diamantes :: [Posicao],
                      paredes :: [Posicao]}

inicial :: Estado
inicial = Estado (2,1) 0 [(1,3),(3,3)] ([(0,x) | x <- [0..4]] ++ [(x,0) | x <- [0..4]] ++ [(4,x) | x <- [0..4]] ++ [(x,4) | x <- [0..4]] ++ [(2,2),(2,3)])

draw :: Estado -> Picture
draw (Estado (xj,yj) t ds ps) = pictures (map quadrado ps ++ map diamante ds ++ [jogador])
    where jogador = translate (40 * fromIntegral xj) (40 * fromIntegral yj) (color red (circleSolid 20))
          quadrado (x,y) = translate (40 * fromIntegral x) (40 * fromIntegral y) (color black (rectangleSolid 40 40))
          diamante (x,y) = translate (40 * fromIntegral x) (40 * fromIntegral y) (rotate 45 (color blue (rectangleSolid 20 20)))
                    
react :: Event -> Estado -> Estado
react (EventKey (SpecialKey k) Down _ _) s | new k (jogador s) `elem` paredes s = s
                                           | otherwise = s {jogador = new k (jogador s)}
    where new KeyUp (x,y) = (x,y+1)
          new KeyDown (x,y) = (x,y-1)
          new KeyLeft (x,y) = (x-1,y)
          new KeyRight (x,y) = (x+1,y)
          new _ (x,y) = (x,y)
react _ s = s

next :: Float -> Estado -> Estado
next t s | jogador s `elem` diamantes s = s {tempo = tempo s + t, diamantes = delete (jogador s) (diamantes s)}
         | otherwise = s {tempo = tempo s + t}
         
main :: IO ()
main = play janela white 25 inicial draw react next