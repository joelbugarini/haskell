-- file: ch05/Main.hs

module Main where

import JSON
import PutJSON 
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
