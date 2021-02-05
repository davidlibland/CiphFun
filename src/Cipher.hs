module Cipher (Cipher, encode, decode) where

type ErrorMsg = String  
  
-- A cipher includes an encoder and a decoder, parameterized by a common config (such as the code word).
class Cipher config where
  -- Encoders return chunked blocks of strings.
  encode :: config -> String -> Either ErrorMsg [String]
  -- Decoders accept chunked blocks of strings, and return a decoded message.
  decode :: config -> [String] -> Either ErrorMsg String
  