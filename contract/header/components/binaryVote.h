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

    ckb_debug("Enter verify_binary_vote\n");

    while (1) {
        uint64_t len = TEMP_SIZE;
        ret = ckb_load_cell_data(buf, &len, 0, i, CKB_SOURCE_INPUT);
        if (ret == CKB_INDEX_OUT_OF_BOUND) {
            break;
        }
        if (ret != CKB_SUCCESS) {
            ckb_debug("error 30\n");
            return ret;
        }
        total++;
        if (len > 0) {
            yes++;
        }
        i++;
    }

    {
        uint64_t len = TEMP_SIZE;
        if (ckb_load_cell_data(buf, &len, 0, 0, CKB_SOURCE_OUTPUT) != CKB_SUCCESS) {
          ckb_debug("BV_LOAD_OUTPUT_DATA_ERROR 43\n");
          return BV_LOAD_OUTPUT_DATA_ERROR;
        }
        if (len != 2) {
          ckb_debug("BV_SUMMARY_LEN_ERROR 47\n");
          return BV_SUMMARY_LEN_ERROR;
        }
    }
    int summary_total = buf[0];
    int summary_yes = buf[1];
    if ((summary_total != total) || (summary_yes != yes)) {
        ckb_debug("BV_SUMMARY_ERROR 54\n");
        return BV_SUMMARY_ERROR;
    }

    return 0;
}
#endif
