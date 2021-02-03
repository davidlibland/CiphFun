module Cipher (Cipher, encode, decode) where

class Cipher config where
  encode :: config -> String -> [String]
  decode :: config -> [String] -> String
  