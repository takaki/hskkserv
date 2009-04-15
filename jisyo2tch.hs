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
