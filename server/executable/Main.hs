-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

--import qualified App --AppPostgres

--import System.Environment

-- haskell/scotty
import ClassyPrelude
import qualified Lib
main :: IO ()
main = Lib.main
-- /haskell-scotty

--main :: IO ()
--main = 
--  App.run "sqlite.db" 
--main =AppPostgres.main