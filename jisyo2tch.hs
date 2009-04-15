import Control.Monad
import Database.TokyoCabinet.HDB
import List
import Char

skkDictPath = "/usr/share/skk/SKK-JISYO.L"
hdbFile = "skkjisyo.tch"


main = do
  hdb <- new
  contents <- readFile skkDictPath 
  open hdb hdbFile [OWRITER, OCREAT]
  mapM_ (uncurry $ put hdb) (map split2 (filter ((\x -> (';' /= x)).(!! 0)) 
                                        (lines contents)))
    where
      split2 line  = 
          case findIndex isSpace line of
            Just i -> (take i line, drop (i+1) line)
{-




          puts hdb [("foo", "hop"), ("bar", "step"), ("baz", "jump")] >>=
               err hdb . (all id)
          get_print hdb "foo"
          iterinit hdb
                   -- iter hdb >>= mapM_ (k -> putStr (k++ ":") >> get_print hdb k)
          close hdb >>= err hdb
    where
      puts :: HDB -> [(String, String)] -> IO [Bool]
      puts hdb = mapM (uncurry $ put hdb)
                 
      get_print :: HDB -> String -> IO ()
      get_print hdb key = get hdb key >>=
                          maybe (error "something goes wrong") putStrLn

      err :: HDB -> Bool -> IO ()
      err hdb = flip unless $ ecode hdb >>= error . show
-}
--      iter :: HDB -> IO [String]
--      iter hdb = iternext hdb >>=
--                 maybe (return []) (x -> return . (x:) =<< iter hdb)
