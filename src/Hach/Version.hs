module Hach.Version (userAgent) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Version (showVersion)
import qualified Paths_hach as Paths

userAgent :: ByteString
userAgent = BS.pack ("hach/" <> showVersion Paths.version)
