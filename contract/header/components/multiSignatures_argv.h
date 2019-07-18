#ifndef MULTISIGNATURES_ARGV_H
#define MULTISIGNATURES_ARGV_H

#include "script.h"

static int msa_verify(int count, char* argv[]) {
    int ret;
    for (int i = 0; i < count; i++) {
        ret = verify_sighash_all(argv[i + 1], i);
        if (ret != CKB_SUCCESS) {
            return ret;
        }
    }
    return 0;
}
#endif