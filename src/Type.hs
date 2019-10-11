{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Type where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, fieldLabelModifier, parseJSON, toJSON, defaultOptions, genericParseJSON, genericToJSON)
import Control.Lens (makeLenses, set)

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
type HashType = String
type DepType = String


-- data type from ckb

data OutPoint = OutPoint
  { outpoint_tx_hash :: Hash
  , outpoint_index :: Index
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
  , script_args :: Arg
  , script_hash_type :: String -- "data" or "type"
  } deriving (Generic, Show)

instance ToJSON Script where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "script_" }

instance FromJSON Script where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "script_" }


data Output = Output
  { _output_capacity :: Capacity
  , _output_lock :: Script
  , _output_type :: Maybe Script
  } deriving (Generic, Show)

makeLenses ''Output

instance ToJSON Output where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "_output_" }

instance FromJSON Output where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "_output_" }


data CellDep = CellDep
  { cell_dep_out_point :: OutPoint
  , cell_dep_dep_type :: String -- "code" or "dep_group"
  } deriving (Generic, Show)

instance ToJSON CellDep where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_dep_" }

instance FromJSON CellDep where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_dep_" }


data Transaction = Transaction
  { _transaction_hash :: Hash
  , _transaction_version :: Version
  , _transaction_cell_deps :: [CellDep]
  , _transaction_header_deps :: [Hash]
  , _transaction_inputs :: [Input]
  , _transaction_outputs :: [Output]
  , _transaction_outputs_data :: [Data]
  , _transaction_witnesses :: [Data]
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
  , contract_info_hash_type :: HashType
  , contract_info_tx_hash :: Hash
  , contract_info_index :: Index
  , contract_info_dep_type :: DepType
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

data CellWithStatusCellData = CellWithStatusCellData
  { cell_with_status_cell_data_content :: Data
  , cell_with_status_cell_data_hash :: Hash
  } deriving (Generic, Show)

instance ToJSON CellWithStatusCellData where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_with_status_cell_data_" }

instance FromJSON CellWithStatusCellData where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_with_status_cell_data_" }

data CellWithStatusCell = CellWithStatusCell
  { cell_with_status_cell_output :: Output
  , cell_with_status_cell_data :: CellWithStatusCellData
  } deriving (Generic, Show)

instance ToJSON CellWithStatusCell where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_with_status_cell_" }

instance FromJSON CellWithStatusCell where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_with_status_cell_" }

data CellWithStatus = CellWithStatus
  { cell_with_status_cell :: CellWithStatusCell
  , cell_with_status_status :: Status
  } deriving (Generic, Show)

instance ToJSON CellWithStatus where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop $ length "cell_with_status_" }

instance FromJSON CellWithStatus where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop $ length "cell_with_status_" }

data LockScriptCmd = LockScriptComplete
  | LockScriptNop
  | LockScriptUpdateCell
  | LockScriptBinaryVote
  | LockScriptMultiSigData
  | LockScriptSystemLock
  | LockScriptMultiSig
  deriving (Generic, Show)

data ResolvedTransaction = ResolvedTransaction
  { _resolved_transaction_tx :: Transaction
  , _resolved_transaction_deps :: [CellWithStatus]
  , _resolved_transaction_inputs :: [CellWithStatus]
  , _resolved_transaction_func :: LockScriptCmd
  } deriving (Generic, Show)

makeLenses ''ResolvedTransaction

data DappInfo = DappInfo
  { dapp_contract_info :: ContractInfo
  , dapp_lock_func :: Maybe (ResolvedTransaction -> ResolvedTransaction)
  }

-- util function
fake_witness :: Int -> [Data]
fake_witness n = take n $ repeat "0x"

mkInput :: Hash -> Index -> Since -> Input
mkInput hash index since = let
  outpoint = OutPoint hash index
  in Input outpoint since

mkDepFormContract :: ContractInfo -> CellDep
mkDepFormContract info = let
  hash = contract_info_tx_hash info
  index = contract_info_index info
  outpoint = OutPoint hash index
  dep_type = contract_info_dep_type info
  in CellDep outpoint dep_type

outPoint2Tuple :: OutPoint -> (Hash, Index)
outPoint2Tuple outpoint = let
  hash = outpoint_tx_hash outpoint
  index = outpoint_index outpoint
  in (hash, index)

cellDep2Tuple :: CellDep -> (Hash, Index)
cellDep2Tuple dep = let
  outpoint = cell_dep_out_point dep
  hash = outpoint_tx_hash outpoint
  index = outpoint_index outpoint
  in (hash, index)
{-
let wa = ["0x1a", "0x2a", "0x3a"]
let wb = ["0x1b", "0x2b", "0x3b"]
let wc = ["0x1c", "0x2c", "0x3c"]
let all_witness = [wa,wb,wc]
> mergeWitnesses all_witness
["0x1a1b1c", "0x2a2b2c, "0x3a3b3c"]
-}
mergeWitnesses :: [[Data]] -> [Data]
mergeWitnesses ([]:_) = []
mergeWitnesses all = ["0x" ++ (concat $ map (tail . tail) $ map head all)] <> mergeWitnesses (map tail all)

mergeTransactions :: [Transaction] -> Transaction
mergeTransactions txs = do
  let all_witnesses = map _transaction_witnesses txs
  let witness = mergeWitnesses all_witnesses
  let tx = head txs
  let stx = set transaction_witnesses witness tx
  stx

fetchOldOutputScript :: Transaction -> Script
fetchOldOutputScript tx = _output_lock old_output where
  old_output = (_transaction_outputs tx) !! 0
