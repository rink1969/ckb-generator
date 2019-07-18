#ifndef WITNESS_H
#define WITNESS_H

#include "script.h"

static int witness_count() {
    int ret;
    unsigned char witness[WITNESS_SIZE];
    ns(Witness_table_t) witness_table;
    ns(Bytes_vec_t) args;

    ckb_debug("Enter witness_count\n");

    volatile uint64_t len = WITNESS_SIZE;
    ret = ckb_load_witness(witness, &len, 0, 0, CKB_SOURCE_GROUP_INPUT);
    if (ret != CKB_SUCCESS) {
      ckb_debug("ERROR_SYSCALL 17\n");
      return ERROR_SYSCALL;
    }

    if (!(witness_table = ns(Witness_as_root(witness)))) {
      ckb_debug("ERROR_ENCODING 22\n");
      return ERROR_ENCODING;
    }
    args = ns(Witness_data(witness_table));
    return ns(Bytes_vec_len(args));
}
#endif