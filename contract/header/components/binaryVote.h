#ifndef BINARYVOTE_H
#define BINARYVOTE_H

#include "script.h"

#define BINARYVOTE_ERROR -5000

// error
#define BV_LOAD_OUTPUT_DATA_ERROR BINARYVOTE_ERROR + 1
#define BV_SUMMARY_LEN_ERROR BINARYVOTE_ERROR + 2
#define BV_SUMMARY_ERROR BINARYVOTE_ERROR + 3

static int verify_binary_vote() {
    //check the summary
    int total = 0;
    int yes = 0;
    int i = 0;
    char buf[TEMP_SIZE];
    int ret;

    while (1) {
        volatile uint64_t len = TEMP_SIZE;
        ret = ckb_load_cell_by_field(buf, &len, 0, i, CKB_SOURCE_INPUT, CKB_CELL_FIELD_DATA);
        if (ret == CKB_INDEX_OUT_OF_BOUND) {
            break;
        }
        if (ret != CKB_SUCCESS) {
            return ret;
        }
        total++;
        if (len > 0) {
            yes++;
        }
        i++;
    }

    {
        volatile uint64_t len = TEMP_SIZE;
        if (ckb_load_cell_by_field(buf, &len, 0, 0, CKB_SOURCE_OUTPUT, CKB_CELL_FIELD_DATA) != CKB_SUCCESS) {
          return BV_LOAD_OUTPUT_DATA_ERROR;
        }
        if (len != 2) {
          return BV_SUMMARY_LEN_ERROR;
        }
    }
    int summary_total = buf[0];
    int summary_yes = buf[1];
    if ((summary_total != total) || (summary_yes != yes)) {
        return BV_SUMMARY_ERROR;
    }

    return 0;
}
#endif