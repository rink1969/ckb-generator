// Generated by Molecule 0.3.1

#include "molecule.h"

#define MOL_BoolOpt()                                                    MolOption
#define MOL_Byte32Opt()                                                  MolOption
#define MOL_BytesOpt()                                                   MolOption
#define MOL_Bool(idx)                                                    MolArray ,1,1,idx
#define MOL_Uint32(idx)                                                  MolArray ,4,1,idx
#define MOL_Uint64(idx)                                                  MolArray ,8,1,idx
#define MOL_Uint128(idx)                                                 MolArray ,16,1,idx
#define MOL_BeUint32(idx)                                                MolArray ,4,1,idx
#define MOL_BeUint64(idx)                                                MolArray ,8,1,idx
#define MOL_Byte32(idx)                                                  MolArray ,32,1,idx
#define MOL_Uint256(idx)                                                 MolArray ,32,1,idx
#define MOL_Bytes(idx)                                                   MolFixVec,1,idx
#define MOL_Uint32Vec(idx)                                               MolFixVec,4,idx
#define MOL_Uint64Vec(idx)                                               MolFixVec,8,idx
#define MOL_BytesVec(idx)                                                MolDynVec,idx
#define MOL_Byte32Vec(idx)                                               MolFixVec,32,idx
#define MOL_CellOutputOpt()                                              MolOption
#define MOL_ScriptOpt()                                                  MolOption
#define MOL_ProposalShortId(idx)                                         MolArray ,10,1,idx
#define MOL_ScriptHashType(idx)                                          MolArray ,1,1,idx
#define MOL_DepType(idx)                                                 MolArray ,1,1,idx
#define MOL_HeaderVec(idx)                                               MolFixVec,208,idx
#define MOL_UncleBlockVec(idx)                                           MolDynVec,idx
#define MOL_TransactionVec(idx)                                          MolDynVec,idx
#define MOL_ProposalShortIdVec(idx)                                      MolFixVec,10,idx
#define MOL_OutPointVec(idx)                                             MolFixVec,36,idx
#define MOL_CellDepVec(idx)                                              MolFixVec,37,idx
#define MOL_CellInputVec(idx)                                            MolFixVec,44,idx
#define MOL_CellOutputVec(idx)                                           MolDynVec,idx
#define MOL_Script_code_hash()                                           MolTable ,3,0
#define MOL_Script_hash_type()                                           MolTable ,3,1
#define MOL_Script_args()                                                MolTable ,3,2
#define MOL_OutPoint_tx_hash()                                           MolStruct,36,0,32
#define MOL_OutPoint_index()                                             MolStruct,36,32,4
#define MOL_CellInput_since()                                            MolStruct,44,0,8
#define MOL_CellInput_previous_output()                                  MolStruct,44,8,36
#define MOL_CellOutput_capacity()                                        MolTable ,3,0
#define MOL_CellOutput_lock()                                            MolTable ,3,1
#define MOL_CellOutput_type_()                                           MolTable ,3,2
#define MOL_CellDep_out_point()                                          MolStruct,37,0,36
#define MOL_CellDep_dep_type()                                           MolStruct,37,36,1
#define MOL_RawTransaction_version()                                     MolTable ,6,0
#define MOL_RawTransaction_cell_deps()                                   MolTable ,6,1
#define MOL_RawTransaction_header_deps()                                 MolTable ,6,2
#define MOL_RawTransaction_inputs()                                      MolTable ,6,3
#define MOL_RawTransaction_outputs()                                     MolTable ,6,4
#define MOL_RawTransaction_outputs_data()                                MolTable ,6,5
#define MOL_Transaction_raw()                                            MolTable ,2,0
#define MOL_Transaction_witnesses()                                      MolTable ,2,1
#define MOL_RawHeader_version()                                          MolStruct,192,0,4
#define MOL_RawHeader_compact_target()                                   MolStruct,192,4,4
#define MOL_RawHeader_timestamp()                                        MolStruct,192,8,8
#define MOL_RawHeader_number()                                           MolStruct,192,16,8
#define MOL_RawHeader_epoch()                                            MolStruct,192,24,8
#define MOL_RawHeader_parent_hash()                                      MolStruct,192,32,32
#define MOL_RawHeader_transactions_root()                                MolStruct,192,64,32
#define MOL_RawHeader_proposals_hash()                                   MolStruct,192,96,32
#define MOL_RawHeader_uncles_hash()                                      MolStruct,192,128,32
#define MOL_RawHeader_dao()                                              MolStruct,192,160,32
#define MOL_Header_raw()                                                 MolStruct,208,0,192
#define MOL_Header_nonce()                                               MolStruct,208,192,16
#define MOL_UncleBlock_header()                                          MolTable ,2,0
#define MOL_UncleBlock_proposals()                                       MolTable ,2,1
#define MOL_Block_header()                                               MolTable ,4,0
#define MOL_Block_uncles()                                               MolTable ,4,1
#define MOL_Block_transactions()                                         MolTable ,4,2
#define MOL_Block_proposals()                                            MolTable ,4,3
#define MOL_HeaderView_hash()                                            MolTable ,2,0
#define MOL_HeaderView_data()                                            MolTable ,2,1
#define MOL_UncleBlockVecView_hashes()                                   MolTable ,2,0
#define MOL_UncleBlockVecView_data()                                     MolTable ,2,1
#define MOL_TransactionView_hash()                                       MolTable ,3,0
#define MOL_TransactionView_witness_hash()                               MolTable ,3,1
#define MOL_TransactionView_data()                                       MolTable ,3,2
#define MOL_BlockExt_total_difficulty()                                  MolTable ,5,0
#define MOL_BlockExt_total_uncles_count()                                MolTable ,5,1
#define MOL_BlockExt_received_at()                                       MolTable ,5,2
#define MOL_BlockExt_txs_fees()                                          MolTable ,5,3
#define MOL_BlockExt_verified()                                          MolTable ,5,4
#define MOL_EpochExt_previous_epoch_hash_rate()                          MolTable ,8,0
#define MOL_EpochExt_last_block_hash_in_previous_epoch()                 MolTable ,8,1
#define MOL_EpochExt_compact_target()                                    MolTable ,8,2
#define MOL_EpochExt_number()                                            MolTable ,8,3
#define MOL_EpochExt_base_block_reward()                                 MolTable ,8,4
#define MOL_EpochExt_remainder_reward()                                  MolTable ,8,5
#define MOL_EpochExt_start_number()                                      MolTable ,8,6
#define MOL_EpochExt_length()                                            MolTable ,8,7
#define MOL_TransactionKey_block_hash()                                  MolStruct,36,0,32
#define MOL_TransactionKey_index()                                       MolStruct,36,32,4
#define MOL_TransactionInfo_block_number()                               MolTable ,3,0
#define MOL_TransactionInfo_block_epoch()                                MolTable ,3,1
#define MOL_TransactionInfo_key()                                        MolTable ,3,2
#define MOL_TransactionMeta_block_hash()                                 MolTable ,6,0
#define MOL_TransactionMeta_block_number()                               MolTable ,6,1
#define MOL_TransactionMeta_epoch_number()                               MolTable ,6,2
#define MOL_TransactionMeta_len()                                        MolTable ,6,3
#define MOL_TransactionMeta_bits()                                       MolTable ,6,4
#define MOL_TransactionMeta_cellbase()                                   MolTable ,6,5
#define MOL_TransactionPoint_tx_hash()                                   MolTable ,3,0
#define MOL_TransactionPoint_block_number()                              MolTable ,3,1
#define MOL_TransactionPoint_index()                                     MolTable ,3,2
#define MOL_TransactionPointOpt()                                        MolOption
#define MOL_LockHashCellOutput_lock_hash()                               MolTable ,3,0
#define MOL_LockHashCellOutput_block_number()                            MolTable ,3,1
#define MOL_LockHashCellOutput_cell_output()                             MolTable ,3,2
#define MOL_LockHashIndex_lock_hash()                                    MolStruct,76,0,32
#define MOL_LockHashIndex_block_number()                                 MolStruct,76,32,8
#define MOL_LockHashIndex_tx_hash()                                      MolStruct,76,40,32
#define MOL_LockHashIndex_index()                                        MolStruct,76,72,4
#define MOL_LockHashIndexState_block_number()                            MolTable ,2,0
#define MOL_LockHashIndexState_block_hash()                              MolTable ,2,1
#define MOL_RelayMessage()                                               MolUnion 
#define MOL_CompactBlock_header()                                        MolTable ,5,0
#define MOL_CompactBlock_short_ids()                                     MolTable ,5,1
#define MOL_CompactBlock_prefilled_transactions()                        MolTable ,5,2
#define MOL_CompactBlock_uncles()                                        MolTable ,5,3
#define MOL_CompactBlock_proposals()                                     MolTable ,5,4
#define MOL_RelayTransaction_cycles()                                    MolTable ,2,0
#define MOL_RelayTransaction_transaction()                               MolTable ,2,1
#define MOL_RelayTransactionVec(idx)                                     MolDynVec,idx
#define MOL_RelayTransactions_transactions()                             MolTable ,1,0
#define MOL_RelayTransactionHashes_tx_hashes()                           MolTable ,1,0
#define MOL_GetRelayTransactions_tx_hashes()                             MolTable ,1,0
#define MOL_GetBlockTransactions_block_hash()                            MolTable ,3,0
#define MOL_GetBlockTransactions_indexes()                               MolTable ,3,1
#define MOL_GetBlockTransactions_uncle_indexes()                         MolTable ,3,2
#define MOL_BlockTransactions_block_hash()                               MolTable ,3,0
#define MOL_BlockTransactions_transactions()                             MolTable ,3,1
#define MOL_BlockTransactions_uncles()                                   MolTable ,3,2
#define MOL_GetBlockProposal_block_hash()                                MolTable ,2,0
#define MOL_GetBlockProposal_proposals()                                 MolTable ,2,1
#define MOL_BlockProposal_transactions()                                 MolTable ,1,0
#define MOL_IndexTransaction_index()                                     MolTable ,2,0
#define MOL_IndexTransaction_transaction()                               MolTable ,2,1
#define MOL_IndexTransactionVec(idx)                                     MolDynVec,idx
#define MOL_SyncMessage()                                                MolUnion 
#define MOL_GetHeaders_hash_stop()                                       MolTable ,2,0
#define MOL_GetHeaders_block_locator_hashes()                            MolTable ,2,1
#define MOL_GetBlocks_block_hashes()                                     MolTable ,1,0
#define MOL_SendHeaders_headers()                                        MolTable ,1,0
#define MOL_SendBlock_block()                                            MolTable ,1,0
#define MOL_SetFilter_hash_seed()                                        MolTable ,3,0
#define MOL_SetFilter_filter()                                           MolTable ,3,1
#define MOL_SetFilter_num_hashes()                                       MolTable ,3,2
#define MOL_AddFilter_filter()                                           MolTable ,1,0
#define MOL_FilteredBlock_header()                                       MolTable ,3,0
#define MOL_FilteredBlock_transactions()                                 MolTable ,3,1
#define MOL_FilteredBlock_proof()                                        MolTable ,3,2
#define MOL_MerkleProof_indices()                                        MolTable ,2,0
#define MOL_MerkleProof_lemmas()                                         MolTable ,2,1
#define MOL_Time_timestamp()                                             MolTable ,1,0
#define MOL_RawAlert_notice_until()                                      MolTable ,7,0
#define MOL_RawAlert_id()                                                MolTable ,7,1
#define MOL_RawAlert_cancel()                                            MolTable ,7,2
#define MOL_RawAlert_priority()                                          MolTable ,7,3
#define MOL_RawAlert_message()                                           MolTable ,7,4
#define MOL_RawAlert_min_version()                                       MolTable ,7,5
#define MOL_RawAlert_max_version()                                       MolTable ,7,6
#define MOL_Alert_raw()                                                  MolTable ,2,0
#define MOL_Alert_signatures()                                           MolTable ,2,1
#define MOL_Identify_flag()                                              MolTable ,3,0
#define MOL_Identify_name()                                              MolTable ,3,1
#define MOL_Identify_client_version()                                    MolTable ,3,2
