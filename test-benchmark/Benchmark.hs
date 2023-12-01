module Main where

import Audiocate
  ( Command(Decode, Encode, EncodeStream, DecodeStream)
  , CommandReturnCode(CmdSuccess)
  )
import Command.Cmd (interpretCmd)
import Test.Tasty.Bench

runEncodeCmd inputFile outputFile secret seconds = do
  let encodeCmd = Encode secret seconds inputFile outputFile
  interpretCmd encodeCmd

runEncodeStreamCmd inputFile outputFile secret seconds = do
  let encodeCmd = EncodeStream secret seconds inputFile outputFile
  interpretCmd encodeCmd

runDecodeCmd inputFile secret seconds = do
  let decodeCmd = Decode secret seconds inputFile
  interpretCmd decodeCmd

runDecodeStreamCmd inputFile secret seconds = do
  let decodeCmd = DecodeStream secret seconds inputFile
  interpretCmd decodeCmd

main :: IO ()
main =
  defaultMain
    [ bgroup
        "EncodeCmd"
        [ bench "encode sample1" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample1.wav"
               "test/output/sample1_out.wav"
               "test123"
               5)
        , bench "encode sample2" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample2.wav"
               "test/output/sample2_out.wav"
               "test123"
               5)
        , bench "encode sample3" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample3.wav"
               "test/output/sample3_out.wav"
               "test123"
               5)
        , bench "encode sample4" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample4.wav"
               "test/output/sample4_out.wav"
               "test123"
               5)
        , bench "encode sample5" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample5.wav"
               "test/output/sample5_out.wav"
               "test123"
               5)
        , bench "encode sample6" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample6.wav"
               "test/output/sample6_out.wav"
               "test123"
               5)
        ]
    , bgroup
        "EncodeStreamCmd"
        [ bench "encodestream sample1" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample1.wav"
               "test/output/sample1_stream_out.wav"
               "test123"
               5)
        ,
         bench "encodestream sample2" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample2.wav"
               "test/output/sample2_stream_out.wav"
               "test123"
               5)
        ,
         bench "encodestream sample3" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample3.wav"
               "test/output/sample3_stream_out.wav"
               "test123"
               5)
        ]
    , bgroup
        "DecodeCmd"
        [ bench "decode sample1" $
          whnfIO (runDecodeCmd "test/output/sample1_out.wav" "test123" 5)
        , bench "decode sample2" $
          whnfIO (runDecodeCmd "test/output/sample2_out.wav" "test123" 5)
        , bench "decode sample3" $
          whnfIO (runDecodeCmd "test/output/sample3_out.wav" "test123" 5)
        , bench "decode sample4" $
          whnfIO (runDecodeCmd "test/output/sample4_out.wav" "test123" 5)
        , bench "decode sample5" $
          whnfIO (runDecodeCmd "test/output/sample5_out.wav" "test123" 5)
        , bench "decode sample6" $
          whnfIO (runDecodeCmd "test/output/sample6_out.wav" "test123" 5)
        ]
    , bgroup
        "DecodeStreamCmd"
        [ bench "decodestream sample1" $
          whnfIO (runDecodeStreamCmd "test/output/sample1_out.wav" "test123" 5)
        , bench "decodestream sample2" $
          whnfIO (runDecodeStreamCmd "test/output/sample2_out.wav" "test123" 5)
        , bench "decodestream sample3" $
          whnfIO (runDecodeStreamCmd "test/output/sample3_out.wav" "test123" 5)
        ]
    ]
