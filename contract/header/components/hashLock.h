#ifndef HASHLOCK_H
#define HASHLOCK_H

#include "script.h"

#define HASHLOCK_ERROR -6000

// error
#define HL_LOAD_DATA_ERROR HASHLOCK_ERROR + 1
#define HL_HASH_LEN_ERROR HASHLOCK_ERROR + 2
#define HL_LOCK_HASH_IMAGE_ERROR HASHLOCK_ERROR + 3

static int verify_hash_lock(char* argv[]) {
    char buf[TEMP_SIZE];
    int ret;
    // unlock
    // arg[1] is sender
    // arg[2] is receiver
    // arg[3] is lock hash  32bytes

    // only receiver can unlock
    ret = verify_sighash_all(argv[2], 0);
    if (ret != CKB_SUCCESS) {
        return ret;
    }

    {
        volatile uint64_t len = TEMP_SIZE;
        if (ckb_load_cell_by_field(buf, &len, 0, 0, CKB_SOURCE_OUTPUT, CKB_CELL_FIELD_DATA) != CKB_SUCCESS) {
          return HL_LOAD_DATA_ERROR;
        }
        if (len != 32) {
          return HL_HASH_LEN_ERROR;
        }
    }

    char hash[BLAKE2B_BLOCK_SIZE];
    blake2b_hash(buf, BLAKE2B_BLOCK_SIZE, hash);

    if (memcmp(hash, argv[3], BLAKE2B_BLOCK_SIZE) != 0) {
        return HL_LOCK_HASH_IMAGE_ERROR;
    }
    return 0;
}
#endif
