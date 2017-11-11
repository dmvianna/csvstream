{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable as F

-- cassava
import Data.Csv
  (
    FromNamedRecord(parseNamedRecord)
  , (.:)
  )
import Data.Csv.Streaming (decodeByName)
import Data.Csv.Streaming (Records)

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text


-- data definition

data Item =
  Item
  { itemId :: Integer -- I'm being conservative for starters
  , itemText :: Text -- No transformation yet
  }
  deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
    <$> m .: "AUSTRALIAN_APPL_NO"
    <*> fmap Text.decodeLatin1 (m .: "ABSTRACT_TEXT")

-- file parsing

decodeItems
  :: ByteString
  -> Either String (Records Item)
decodeItems =
  fmap snd . decodeByName

decodeItemsFromFile
  :: FilePath
  -> IO (Either String (Records Item))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
  >>= return . either Left decodeItems

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
  csvData <- decodeItemsFromFile "../data/IPGOD.IPGOD122B_PAT_ABSTRACTS.csv"
  let counted = fmap (F.foldr (\_ n -> n + 1)  0) csvData
  case counted of
    Left e -> putStrLn e
    Right n ->
      putStrLn $ "total rows: " ++ (show n)

