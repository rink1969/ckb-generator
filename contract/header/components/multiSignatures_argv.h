#ifndef MULTISIGNATURES_ARGV_H
#define MULTISIGNATURES_ARGV_H

#include "script.h"

static int msa_verify(int count, char* argv[]) {
    int ret;
    ckb_debug("Enter msa_verify\n");
    for (int i = 0; i < count; i++) {
        ret = verify_sighash_all(argv[i + 1], i);
        if (ret != CKB_SUCCESS) {
            ckb_debug("error 12\n");
            return ret;
        }
    }
    return 0;
}
#endif