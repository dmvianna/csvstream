{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.ByteString.Lazy (ByteString)

-- bytestring
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable as Foldable

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv.Streaming as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector

import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- data definition

data Item =
  Item
  { itemId :: Text -- I'm being conservative for starters
  , itemText :: Text -- No transformation yet
  }
  deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
    <$> m .: "AUSTRALIAN_APPL_NO"
    <*> m .: "ABSTRACT_TEXT"

-- file parsing

decodeItems
  :: ByteString
  -> Either String (Vector Item)
decodeItems =
  fmap snd . Cassava.decodeByName

-- decodeItemsFromFile
--   :: FilePath
--   -> IO (Either String (Vector Item))
-- decodeItemsFromFile filePath =
--   catchShowIO (ByteString.readFile filePath)
--   >>= return . either Left decodeItems

catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
  `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show


main :: IO ()
main = do
  putStrLn "Hello world"
