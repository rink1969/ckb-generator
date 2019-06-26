{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Type where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, fieldLabelModifier, parseJSON, toJSON, defaultOptions, genericParseJSON, genericToJSON)

type Path = String
type Hash = String
type Name = String
type Capacity = String
type Data = String
type Arg = String
type PrivateKey = String
type Index = String
type Since = String
type Status = String

data ContractInfo = ContractInfo
  { contract_info_name :: Name
  , contract_info_elf_path :: Path
  , contract_info_code_hash :: Hash
  , contract_info_tx_hash :: Hash
  , contract_info_index :: Index
  } deriving (Generic, Show)

instance ToJSON ContractInfo where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "contract_info_" }

instance FromJSON ContractInfo where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "contract_info_" }



data CellOutPoint = CellOutPoint
  { cell_outpoint_tx_hash :: Hash
  , cell_outpoint_index :: Index
  } deriving (Generic, Show)

instance ToJSON CellOutPoint where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_outpoint_" }

instance FromJSON CellOutPoint where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_outpoint_" }


data OutPoint = OutPoint
  { outpoint_block_hash :: Maybe Hash
  , outpoint_cell :: Maybe CellOutPoint
  } deriving (Generic, Show)

instance ToJSON OutPoint where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "outpoint_" }

instance FromJSON OutPoint where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "outpoint_" }


data Input = Input
  { input_previous_output :: OutPoint
  , input_since :: Since
  } deriving (Generic, Show)

instance ToJSON Input where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "input_" }

instance FromJSON Input where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "input_" }


data Script = Script
  { script_code_hash :: Hash
  , script_args :: [Arg]
  } deriving (Generic, Show)

instance ToJSON Script where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "script_" }

instance FromJSON Script where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "script_" }


data Output = Output
  { output_capacity :: Capacity
  , output_data :: Data
  , output_lock :: Script
  , output_type :: Maybe Script
  } deriving (Generic, Show)

instance ToJSON Output where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "output_" }

instance FromJSON Output where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "output_" }


data Witness = Witness  { witness_data :: [Data]}  deriving (Generic, Show)

instance ToJSON Witness where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "witness_" }

instance FromJSON Witness where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "witness_" }


type Dep = OutPoint

data Transaction = Transaction
  { transaction_hash :: Hash
  , transaction_version :: Int
  , transaction_deps :: [Dep]
  , transaction_inputs :: [Input]
  , transaction_outputs :: [Output]
  } deriving (Generic, Show)

instance ToJSON Transaction where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "transaction_" }

instance FromJSON Transaction where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "transaction_" }


data TransactionWithWitnesses = TransactionWithWitnesses
  { transaction_with_witnesses_tx :: Transaction
  , transaction_with_witnesses_witnesses :: [Witness]
  } deriving (Generic, Show)

instance ToJSON TransactionWithWitnesses where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "transaction_with_witnesses_" }

instance FromJSON TransactionWithWitnesses where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "transaction_with_witnesses_" }


-- for operations
data RetGetLiveCellsByCapacity = RetGetLiveCellsByCapacity
  { ret_getLiveCellsByCapacity_inputs :: [Input]
  , ret_getLiveCellsByCapacity_capacity :: Capacity
  } deriving (Generic, Show)

instance ToJSON RetGetLiveCellsByCapacity where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "ret_getLiveCellsByCapacity_" }

instance FromJSON RetGetLiveCellsByCapacity where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "ret_getLiveCellsByCapacity_" }

data CellWithStatus = CellWithStatus
  { cell_with_status_cell :: Output
  , cell_with_status_status :: Status
  } deriving (Generic, Show)

instance ToJSON CellWithStatus where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_with_status_" }

instance FromJSON CellWithStatus where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_with_status_" }