{-# LANGUAGE OverloadedStrings #-}
module Hook (attach) where
import Hach.Interpreter.IO
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.IORef
import System.Environment (getEnv)
import System.FilePath ((</>))

attach :: IOEnv -> IO IOEnv
attach env = do
  dir <- getEnv "HACH_PROBE_DIR"
  count <- newIORef (0 :: Int)
  mgr <- newManager tlsManagerSettings
    { managerModifyRequest = \req -> do
        n <- atomicModifyIORef' count (\i -> (i+1,i+1))
        case requestBody req of
          RequestBodyLBS body -> do
            BL.writeFile (dir </> show n <> "-original.json") body
            case eitherDecode body of
              Right (Object obj) | KM.lookup "model" obj == Just (String "openai/gpt-5.6-luna") -> do
                let wire = encode (Object (KM.insert "reasoning" (object ["effort" .= ("high" :: String)]) obj))
                BL.writeFile (dir </> show n <> "-wire.json") wire
                pure req {requestBody = RequestBodyLBS wire}
              _ -> fail "Probe refuses an unapproved model or malformed body"
          _ -> fail "Unexpected request body"
    , managerModifyResponse = \res -> do
        n <- readIORef count
        chunks <- brConsume (responseBody res)
        BL.writeFile (dir </> show n <> "-response.json") (BL.fromChunks chunks)
        remaining <- newIORef chunks
        let reader = atomicModifyIORef' remaining (\cs -> case cs of [] -> ([], BS.empty); c:rest -> (rest,c))
        pure res {responseBody = reader}
    }
  pure env {ioManager = mgr}
