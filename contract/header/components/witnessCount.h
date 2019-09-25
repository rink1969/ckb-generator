#ifndef WITNESS_H
#define WITNESS_H

#include "script.h"

static int witness_count() {
    int ret;
    unsigned char witness[WITNESS_SIZE];
    mol_pos_t witness_pos;
    mol_read_res_t arg_res;

    ckb_debug("Enter witness_count\n");

    volatile uint64_t len = WITNESS_SIZE;
    ret = ckb_load_witness(witness, &len, 0, 0, CKB_SOURCE_GROUP_INPUT);
    if (ret != CKB_SUCCESS) {
      return ERROR_SYSCALL;
    }
    if (len > WITNESS_SIZE) {
      return ERROR_WITNESS_TOO_LONG;
    }

    witness_pos.ptr = (const uint8_t*)witness;
    witness_pos.size = len;

    arg_res = mol_cut(&witness_pos, MOL_Witness(0));
    if (arg_res.code != 0) {
      return ERROR_ENCODING;
    }

    ckb_debug("Leave witness_count\n");
    return arg_res.attr;
}
#endif