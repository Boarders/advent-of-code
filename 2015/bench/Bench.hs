{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Criterion.Main                             as C (bench, bgroup,
                                                                  defaultMain,
                                                                  env, nf, whnf, nfIO)
import qualified Data.ByteString                            as ByteString
import           Control.DeepSeq
import qualified Data.Primitive.ByteArray                   as ByteArray
import Common
import Day1


getInputs :: IO (ByteArray.ByteArray, ByteString.ByteString)
getInputs =
  do
    ba <- readFileBA "test"
    bs <- ByteString.readFile "test"
    pure (ba, bs)


main :: IO ()
main = do
  C.defaultMain $
    [ C.bench "readFileBA" $ C.nfIO (readFileBA "test")
    , C.bench "readFileBS" $ C.nfIO (ByteString.readFile "test")
    ]
  C.defaultMain . pure $
    C.env getInputs $ \ ~(bytes, bs) ->
      C.bgroup "bracket count"
        [ C.bench "BA" $ C.whnf countBracketsBA bytes
        , C.bench "BS" $ C.whnf countBrackets   bs
        ]
  
  
  


