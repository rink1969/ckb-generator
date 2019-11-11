#ifndef SCRIPT_H
#define SCRIPT_H

#include "blake2b.h"
#include "ckb_syscalls.h"
#include "common.h"
#include "protocol.h"
#include "secp256k1_helper.h"

#define BLAKE2B_BLOCK_SIZE 32
#define BLAKE160_SIZE 20
#define PUBKEY_SIZE 33
#define TEMP_SIZE 1024
#define RECID_INDEX 64
/* 32 KB */
#define WITNESS_SIZE 32768
#define SCRIPT_SIZE 32768
#define SIGNATURE_SIZE 65

/*
 * Arguments are listed in the following order:
 * 0. program name
 * 1 ~ n. pubkey blake160 hash, blake2b hash of pubkey first 20 bytes, used to
 * shield the real pubkey in lock script, the lock is considered as a multisig
 * lock when the number of pubkeys is more than 1.
 * n + 1. multisig threshold (optional), used to indicate the threshold of a
 * multisig lock, must ignore this arg when only one pubkey is used.
 *
 * Witness:
 * 0 ~ m signature, signatures used to present ownership, the number of signatures
 * must be equals to the threshold if the lock is a multisig lock, otherwise must
 * use only 1 signature.
 */
static int verify_sighash_all(const uint8_t* blake160, size_t n)
{
  int ret;
  size_t index = 0;
  uint64_t len = 0;
  unsigned char tx_hash[BLAKE2B_BLOCK_SIZE];
  unsigned char temp[TEMP_SIZE];
  unsigned char witness[WITNESS_SIZE];
  uint8_t secp_data[CKB_SECP256K1_DATA_SIZE];

  ckb_debug("Enter verify_sighash_all\n");
  secp256k1_context context;
  ret = ckb_secp256k1_custom_verify_only_initialize(&context, secp_data);
  if (ret != 0) {
    ckb_debug("error 63\n");
    return ret;
  }

  len = BLAKE2B_BLOCK_SIZE;
  ret = ckb_load_tx_hash(tx_hash, &len, 0);
  if (ret != CKB_SUCCESS) {
    ckb_debug("ERROR_SYSCALL 70\n");
    return ERROR_SYSCALL;
  }

  while (1) {
    len = 0;
    /*
     * Actually we don't need this syscall, we are just making it to grab all
     * input indices for current group, from which we can load the actual
     * witness data we need. `since` field is chosen here since it has a fixed
     * size of 8 bytes, which is both predictable, and also provides minimal
     * cycle consumption.
     */
    ret = ckb_load_input_by_field(NULL, &len, 0, index, CKB_SOURCE_GROUP_INPUT,
                                  CKB_INPUT_FIELD_SINCE);
    if (ret == CKB_INDEX_OUT_OF_BOUND) {
      ckb_debug("ok\n");
      return 0;
    }
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_SYSCALL 90\n");
      return ERROR_SYSCALL;
    }

    /* Now we load actual witness data using the same input index above. */
    len = WITNESS_SIZE;
    ret = ckb_load_witness(witness, &len, 0, index, CKB_SOURCE_GROUP_INPUT);
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_SYSCALL 98\n");
      return ERROR_SYSCALL;
    }
    if (len > WITNESS_SIZE) {
      ckb_debug("ERROR_WITNESS_SIZE 102\n");
      return ERROR_WITNESS_SIZE;
    }

      /* Recover pubkey */
      secp256k1_ecdsa_recoverable_signature signature;
      if (secp256k1_ecdsa_recoverable_signature_parse_compact(
              &context, &signature, &witness[n * SIGNATURE_SIZE], witness[n * SIGNATURE_SIZE + RECID_INDEX]) == 0) {
        ckb_debug("ERROR_SECP_PARSE_SIGNATURE 132\n");
        return ERROR_SECP_PARSE_SIGNATURE;
      }
      blake2b_state blake2b_ctx;
      blake2b_init(&blake2b_ctx, BLAKE2B_BLOCK_SIZE);
      blake2b_update(&blake2b_ctx, tx_hash, BLAKE2B_BLOCK_SIZE);
      // no data
      blake2b_final(&blake2b_ctx, temp, BLAKE2B_BLOCK_SIZE);

      secp256k1_pubkey pubkey;

      if (secp256k1_ecdsa_recover(&context, &pubkey, &signature, temp) != 1) {
        ckb_debug("ERROR_SECP_RECOVER_PUBKEY 144\n");
        return ERROR_SECP_RECOVER_PUBKEY;
      }

      /* Check pubkey hash */
      size_t pubkey_size = PUBKEY_SIZE;
      if (secp256k1_ec_pubkey_serialize(&context, temp, &pubkey_size, &pubkey,
                                        SECP256K1_EC_COMPRESSED) != 1) {
        ckb_debug("ERROR_SECP_SERIALIZE_PUBKEY 152\n");
        return ERROR_SECP_SERIALIZE_PUBKEY;
      }

      len = PUBKEY_SIZE;
      blake2b_init(&blake2b_ctx, BLAKE2B_BLOCK_SIZE);
      blake2b_update(&blake2b_ctx, temp, len);
      blake2b_final(&blake2b_ctx, temp, BLAKE2B_BLOCK_SIZE);

      if (memcmp(blake160, temp, BLAKE160_SIZE) != 0) {
        ckb_debug("ERROR_PUBKEY_BLAKE160_HASH 162\n");
        return ERROR_PUBKEY_BLAKE160_HASH;
      }
    index += 1;
  }
  ckb_debug("ERROR_ENCODING 167\n");
  return ERROR_ENCODING;
}
#endif
