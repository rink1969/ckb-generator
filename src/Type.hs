{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Type where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, fieldLabelModifier, parseJSON, toJSON, defaultOptions, genericParseJSON, genericToJSON)
import Control.Lens (makeLenses)

type Path = String
type Hash = String
type Name = String
type Capacity = String
type Data = String
type Arg = String
type Key = String
type Index = String
type Since = String
type Status = String
type Version = String


-- data type from ckb

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
  { _transaction_hash :: Hash
  , _transaction_version :: Version
  , _transaction_deps :: [Dep]
  , _transaction_inputs :: [Input]
  , _transaction_outputs :: [Output]
  , _transaction_witnesses :: [Witness]
  } deriving (Generic, Show)

makeLenses ''Transaction

instance ToJSON Transaction where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "_transaction_" }

instance FromJSON Transaction where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "_transaction_" }


-- data type for operations
data UserInfo = UserInfo
  { userInfo_privkey :: Key
  , userInfo_pubkey :: Key
  , userInfo_blake160 :: Key
  , userInfo_address :: Key
  } deriving (Generic, Show)
instance ToJSON UserInfo where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "userInfo_" }

instance FromJSON UserInfo where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "userInfo_" }

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

data RetQueryLiveCells = RetQueryLiveCells
  { ret_queryLiveCells_inputs :: [Input]
  , ret_queryLiveCells_capacity :: Capacity
  } deriving (Generic, Show)

instance ToJSON RetQueryLiveCells where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "ret_queryLiveCells_" }

instance FromJSON RetQueryLiveCells where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "ret_queryLiveCells_" }

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


-- util function
fake_witness :: Int -> [Witness]
fake_witness n = take n $ repeat $ Witness []

mkInput :: Hash -> Index -> Since -> Input
mkInput hash index since = let
  cell_outpoint = CellOutPoint hash index
  outpoint = OutPoint Nothing (Just cell_outpoint)
  in Input outpoint since

mkDepFormContract :: ContractInfo -> Dep
mkDepFormContract info = let
  hash = contract_info_tx_hash info
  index = contract_info_index info
  cell_outpoint = CellOutPoint hash index
  in OutPoint Nothing (Just cell_outpoint)