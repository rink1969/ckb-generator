{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Type where

import GHC.Generics
import Data.Aeson

type Path = String
type Hash = String
type Name = String
type Capacity = String
type Data = String
type Arg = String
type PrivateKey = String
type Index = String
type Since = String

data ContractInfo = ContractInfo
  { name :: Name
  , elf_path :: Path
  , code_hash :: Hash
  , tx_hash :: Hash
  , index :: Index
  } deriving (Generic, ToJSON, FromJSON, Show)

data CellOutPoint = CellOutPoint
  { tx_hash :: Hash
  , index :: Index
  } deriving (Generic, ToJSON, FromJSON, Show)

data OutPoint = OutPoint
  { block_hash :: Maybe Hash
  , cell :: Maybe CellOutPoint
  } deriving (Generic, ToJSON, FromJSON, Show)

data Input = Input
  { previous_output :: OutPoint
  , since :: Since
  } deriving (Generic, ToJSON, FromJSON, Show)

data Script = Script
  { code_hash :: Hash
  , args :: [Arg]
  } deriving (Generic, ToJSON, FromJSON, Show)

data Output = Output
  { _capacity :: Capacity
  , _data :: Data
  , _lock :: Script
  , _type :: Maybe Script
  } deriving (Generic, Show)

instance ToJSON Output where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = Prelude.drop 1 }

instance FromJSON Output where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = Prelude.drop 1 }

data Witness = Witness  { _data :: [Data]}  deriving (Generic, Show)
instance ToJSON Witness where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = Prelude.drop 1 }

instance FromJSON Witness where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = Prelude.drop 1 }

type Dep = OutPoint

data Transaction = Transaction
  { hash :: Hash
  , version :: Int
  , deps :: [Dep]
  , inputs :: [Input]
  , outputs :: [Output]
  } deriving (Generic, ToJSON, FromJSON, Show)

data TransactionWithWitnesses = TransactionWithWitnesses
  { tx :: Transaction
  , witnesses :: [Witness]
  } deriving (Generic, ToJSON, FromJSON, Show)


-- for operations
data RetGetLiveCellsByCapacity = RetGetLiveCellsByCapacity
  { getLiveCellsByCapacity_inputs :: [Input]
  , getLiveCellsByCapacity_capacity :: Capacity
  } deriving (Generic, ToJSON, FromJSON, Show)
