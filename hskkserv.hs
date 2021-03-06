{-
    Haskell SKK Server
    Copyright (C) 2009 TANIGUCHI, Takaki <takaki@asis.media-as.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where
import Control.Monad
import System.Environment
import System.IO
import System.Exit
import Data.Char
import Data.List
import Database.TokyoCabinet.HDB

version = "hskkd 0.0.20090415"

readUntilSpace s = do
  c <- getChar
  if isSpace c then
      if c == ' ' then
          return (s ++ [c])
      else
          readUntilSpace s
    else
        readUntilSpace (s ++ [c])

hskkd dict = do 
  line <- readUntilSpace ""
  case (if not $ null line then line !! 0 else 'X') of 
    '0' -> return ()
    '1' -> do 
      case (findIndex isSpace line) of
        Just i -> do
               let dict_key = tail (take i line)
               cand <- get dict dict_key
               case cand of
                 Just word -> do 
                           putStr $ "1" ++ word ++ "\n"
                 Nothing ->   putStr $ "4" ++ dict_key ++ " \n"
        Nothing -> do
               putStr $ "4" ++ (tail line) ++ " \n"
      hFlush stdout
      hskkd dict
    '2' -> 
        do putStr $ version  ++ " "
           hFlush stdout
           hskkd dict
    '3' -> 
        do putStr $ "novalue: "
           hFlush stdout
           hskkd dict
    otherwise -> exitWith (ExitFailure 1)

main :: IO()
main = do
  args <- getArgs
  hdb <- new
  open hdb (head args) [OREADER] >>= err hdb
  hskkd hdb
    where
      err :: HDB -> Bool -> IO ()
      err hdb = flip unless $ ecode hdb >>= error . show
