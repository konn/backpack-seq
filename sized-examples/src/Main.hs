module Main where
import qualified Data.List.Sized   as SL
import qualified Data.Vector.Sized as SV

main :: IO ()
main = do
  print $ SV.fromList [1,2,3]
  case SV.fromList [1,2,3] of
    SV.SomeSized sz -> print $ SV.reverse sz
  print $ SL.fromList [1,2,3]
