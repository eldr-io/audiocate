module Main where

import Audiocate
  ( Command(Decode, DecodeStream, Encode, EncodeStream)
  , CommandReturnCode(CmdSuccess)
  )
import Command.Cmd (interpretCmd)
import Test.Tasty.Bench
import GHC.IO.Encoding (BufferCodec(encode))

runEncodeCmd inputFile outputFile secret seconds = do
  let encodeCmd = Encode secret seconds inputFile outputFile
  interpretCmd encodeCmd False False

runEncodeStreamCmd inputFile outputFile secret seconds = do
  let encodeCmd = EncodeStream secret seconds inputFile outputFile
  interpretCmd encodeCmd False False

runDecodeCmd inputFile secret seconds = do
  let decodeCmd = Decode secret seconds inputFile
  interpretCmd decodeCmd False False

runDecodeStreamCmd inputFile secret seconds = do
  let decodeCmd = DecodeStream secret seconds inputFile
  interpretCmd decodeCmd False False

testSecret :: String
testSecret = "125@#1!12asde!3214[12345%¤¤21qassdf==:?213!3324124]"

main :: IO ()
main =
  defaultMain
    [ bgroup
        "EncodeCmd"
        [ bench "encode-sample1" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample1.wav"
               "test/output/sample1_out.wav"
               testSecret
               5)
        , bench "encode-sample2" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample2.wav"
               "test/output/sample2_out.wav"
               testSecret
               5)
        , bench "encode-sample3" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample3.wav"
               "test/output/sample3_out.wav"
               testSecret
               5)
        , bench "encode-sample4" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample4.wav"
               "test/output/sample4_out.wav"
               testSecret
               5)
        , bench "encode-sample5" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample5.wav"
               "test/output/sample5_out.wav"
               testSecret
               5)
        , bench "encode-sample6" $
          whnfIO
            (runEncodeCmd
               "test/corpus/sample6.wav"
               "test/output/sample6_out.wav"
               testSecret
               5)
        ]
    , bgroup
        "EncodeStreamCmd"
        [ bench "encodestream-sample1" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample1.wav"
               "test/output/sample1_stream_out.wav"
               testSecret
               5)
        , bench "encodestream-sample2" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample2.wav"
               "test/output/sample2_stream_out.wav"
               testSecret
               5)
        , bench "encodestream-sample3" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample3.wav"
               "test/output/sample3_stream_out.wav"
               testSecret
               5)
        , bench "encodestream-sample4" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample4.wav"
               "test/output/sample4_stream_out.wav"
               testSecret
               5)
        , bench "encodestream-sample5" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample5.wav"
               "test/output/sample5_stream_out.wav"
               testSecret
               5)
        , bench "encodestream-sample6" $
          whnfIO
            (runEncodeStreamCmd
               "test/corpus/sample6.wav"
               "test/output/sample6_stream_out.wav"
               testSecret
               5)
        ]
    , bgroup
        "DecodeCmd"
        [ bench "decode-sample1" $
          whnfIO (runDecodeCmd "test/output/sample1_out.wav" testSecret 5)
        , bench "decode-sample2" $
          whnfIO (runDecodeCmd "test/output/sample2_out.wav" testSecret 5)
        , bench "decode-sample3" $
          whnfIO (runDecodeCmd "test/output/sample3_out.wav" testSecret 5)
        , bench "decode-sample4" $
          whnfIO (runDecodeCmd "test/output/sample4_out.wav" testSecret 5)
        , bench "decode-sample5" $
          whnfIO (runDecodeCmd "test/output/sample5_out.wav" testSecret 5)
        , bench "decode-sample6" $
          whnfIO (runDecodeCmd "test/output/sample6_out.wav" testSecret 5)
        ]
    , bgroup
        "DecodeStreamCmd"
        [ bench "decodestream-sample1" $
          whnfIO (runDecodeStreamCmd "test/output/sample1_out.wav" testSecret 5)
        , bench "decodestream-sample2" $
          whnfIO (runDecodeStreamCmd "test/output/sample2_out.wav" testSecret 5)
        , bench "decodestream-sample3" $
          whnfIO (runDecodeStreamCmd "test/output/sample3_out.wav" testSecret 5)
        , bench "decodestream-sample4" $
          whnfIO (runDecodeStreamCmd "test/output/sample4_out.wav" testSecret 5)
        , bench "decodestream-sample5" $
          whnfIO (runDecodeStreamCmd "test/output/sample5_out.wav" testSecret 5)
        , bench "decodestream-sample6" $
          whnfIO (runDecodeStreamCmd "test/output/sample6_out.wav" testSecret 5)
        ]
    ]
