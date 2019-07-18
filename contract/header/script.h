#ifndef SCRIPT_H
#define SCRIPT_H

#include "ckb_syscalls.h"
#include "blake2b.h"
#include "protocol_reader.h"

#undef ns
#define ns(x) FLATBUFFERS_WRAP_NAMESPACE(Ckb_Protocol, x)

#define ERROR_UNKNOWN -1
#define ERROR_WRONG_NUMBER_OF_ARGUMENTS -2
#define ERROR_PUBKEY_BLAKE160_HASH -3
#define ERROR_SYSCALL -4
#define ERROR_SECP_ABORT -5
#define ERROR_SECP_INITIALIZE -6
#define ERROR_SECP_RECOVER_PUBKEY -7
#define ERROR_SECP_PARSE_SIGNATURE -8
#define ERROR_SECP_SERIALIZE_PUBKEY -9
#define ERROR_BUFFER_NOT_ENOUGH -10
#define ERROR_ENCODING -11

#define BLAKE2B_BLOCK_SIZE 32
#define BLAKE160_SIZE 20
#define PUBKEY_SIZE 33
#define TEMP_SIZE 1024
#define RECID_INDEX 64
/* 32 KB */
#define WITNESS_SIZE 32768

/* Stripping as much unneeded bytes from secp256k1 as possible */
#define CUSTOM_ABORT 1
#define CUSTOM_PRINT_ERR 1

void custom_abort()
{
  syscall(SYS_exit, ERROR_SECP_ABORT, 0, 0, 0, 0, 0);
}

int custom_print_err(const char * arg, ...)
{
  (void) arg;
  return 0;
}

#include <secp256k1_static.h>
/*
 * We are including secp256k1 implementation directly so gcc can strip
 * unused functions. For some unknown reasons, if we link in libsecp256k1.a
 * directly, the final binary will include all functions rather than those used.
 */
#include <secp256k1.c>

static int extract_bytes(ns(Bytes_table_t) bytes, unsigned char *buffer, volatile size_t *s)
{
  flatbuffers_uint8_vec_t seq = ns(Bytes_seq(bytes));
  size_t len = flatbuffers_uint8_vec_len(seq);

  if (len > *s) {
    return ERROR_BUFFER_NOT_ENOUGH;
  }

  for (size_t i = 0; i < len; i++) {
    buffer[i] = flatbuffers_uint8_vec_at(seq, i);
  }
  *s = len;

  return CKB_SUCCESS;
}

static void blake2b_hash(char * in_buf, int len, char * out_buf)
{
  blake2b_state blake2b_ctx;
  blake2b_init(&blake2b_ctx, BLAKE2B_BLOCK_SIZE);
  blake2b_update(&blake2b_ctx, in_buf, len);
  blake2b_final(&blake2b_ctx, out_buf, BLAKE2B_BLOCK_SIZE);
}

/*
 * Arguments are listed in the following order:
 * 0. program name
 * 1. pubkey blake160 hash, blake2b hash of pubkey first 20 bytes, used to shield the real
 * pubkey in lock script.
 *
 * Witness:
 * 1. signature, signature used to present ownership
 * 2. signature size, in little endian 64 bit unsigned integer
 */
static int verify_sighash_all(char* blake160, size_t n)
{
  int ret;
  int recid;
  size_t index = 0;
  uint64_t signature_size = 0;
  volatile uint64_t len = 0;
  unsigned char tx_hash[BLAKE2B_BLOCK_SIZE];
  unsigned char temp[TEMP_SIZE];
  unsigned char witness[WITNESS_SIZE];
  ns(Witness_table_t) witness_table;
  ns(Bytes_vec_t) args;

  ckb_debug("Enter verify_sighash_all\n");

  secp256k1_context context;
  if (secp256k1_context_initialize(&context, SECP256K1_CONTEXT_VERIFY) == 0) {
    ckb_debug("ERROR_SECP_INITIALIZE 106\n");
    return ERROR_SECP_INITIALIZE;
  }

  len = BLAKE2B_BLOCK_SIZE;
  ret = ckb_load_tx_hash(tx_hash, &len, 0);
  if (ret != CKB_SUCCESS) {
    ckb_debug("ERROR_SYSCALL 113\n");
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
    ret = ckb_load_cell_by_field(NULL, &len, 0, index, CKB_SOURCE_GROUP_INPUT,
                                 CKB_INPUT_FIELD_SINCE);
    if (ret == CKB_INDEX_OUT_OF_BOUND) {
      return 0;
    }
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_SYSCALL 132\n");
      return ERROR_SYSCALL;
    }

    /* Now we load actual witness data using the same input index above. */
    len = WITNESS_SIZE;
    ret = ckb_load_witness(witness, &len, 0, index, CKB_SOURCE_GROUP_INPUT);
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_SYSCALL 140\n");
      return ERROR_SYSCALL;
    }

    if (!(witness_table = ns(Witness_as_root(witness)))) {
      ckb_debug("ERROR_ENCODING 145\n");
      return ERROR_ENCODING;
    }

    args = ns(Witness_data(witness_table));
    if (ns(Bytes_vec_len(args)) < 1) {
      ckb_debug("ERROR_WRONG_NUMBER_OF_ARGUMENTS 151\n");
      return ERROR_WRONG_NUMBER_OF_ARGUMENTS;
    }

    /* Check pubkey hash */
    len = TEMP_SIZE;
    ret = extract_bytes(ns(Bytes_vec_at(args, n)), temp, &len);
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_ENCODING 159\n");
      return ERROR_ENCODING;
    }

    /* The 65th byte is recid according to contract spec.*/
    recid = temp[RECID_INDEX];
    /* Recover pubkey */
    secp256k1_ecdsa_recoverable_signature signature;
    if (secp256k1_ecdsa_recoverable_signature_parse_compact(&context, &signature, temp, recid) == 0) {
      ckb_debug("ERROR_SECP_PARSE_SIGNATURE 168\n");
      return ERROR_SECP_PARSE_SIGNATURE;
    }
    blake2b_state blake2b_ctx;
    blake2b_init(&blake2b_ctx, BLAKE2B_BLOCK_SIZE);
    blake2b_update(&blake2b_ctx, tx_hash, BLAKE2B_BLOCK_SIZE);
    //no witness data
    blake2b_final(&blake2b_ctx, temp, BLAKE2B_BLOCK_SIZE);

    secp256k1_pubkey pubkey;

    if (secp256k1_ecdsa_recover(&context, &pubkey, &signature, temp) != 1) {
      ckb_debug("ERROR_SECP_RECOVER_PUBKEY 180\n");
      return ERROR_SECP_RECOVER_PUBKEY;
    }

    /* Check pubkey hash */
    size_t pubkey_size = PUBKEY_SIZE;
    if (secp256k1_ec_pubkey_serialize(&context, temp, &pubkey_size, &pubkey, SECP256K1_EC_COMPRESSED) != 1 ) {
      ckb_debug("ERROR_SECP_SERIALIZE_PUBKEY 187\n");
      return ERROR_SECP_SERIALIZE_PUBKEY;
    }

    len = PUBKEY_SIZE;
    blake2b_init(&blake2b_ctx, BLAKE2B_BLOCK_SIZE);
    blake2b_update(&blake2b_ctx, temp, len);
    blake2b_final(&blake2b_ctx, temp, BLAKE2B_BLOCK_SIZE);

    if (memcmp(blake160, temp, BLAKE160_SIZE) != 0) {
      ckb_debug("ERROR_PUBKEY_BLAKE160_HASH 197\n");
      return ERROR_PUBKEY_BLAKE160_HASH;
    }

    index += 1;
  }
  ckb_debug("ERROR_UNKNOWN 203\n");
  return ERROR_UNKNOWN;
}
#endif
