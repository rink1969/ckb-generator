#ifndef MULTISIGNATURES_DATA_CONFIG_H
#define MULTISIGNATURES_DATA_CONFIG_H

#include "script.h"

#define MULTI_SIGNATURES_DATA_CONFIG_ERROR -4000

// error
#define MSDC_LOAD_DATA_ERROR MULTI_SIGNATURES_DATA_CONFIG_ERROR + 1
#define MSDC_INVALID_CONFIG_DATA MULTI_SIGNATURES_DATA_CONFIG_ERROR + 2
#define MSDC_TOO_MANY_SIGNERS MULTI_SIGNATURES_DATA_CONFIG_ERROR + 3
#define MSDS_VERIFICATION_FAILED MULTI_SIGNATURES_DATA_CONFIG_ERROR + 4

// multi-signatures config
#define MSDC_MAX_MULTI_SIGNERS 8
#define MSDC_TEMP_SIZE 1024
struct MSDC_Conifg {
    uint8_t total;
    uint8_t threshold;
    uint8_t blake160s[MSDC_MAX_MULTI_SIGNERS][BLAKE160_SIZE];
};
static struct MSDC_Conifg msdc_config;
static int msdc_parse_config() {
    char buf[MSDC_TEMP_SIZE];
    {
        volatile uint64_t len = MSDC_TEMP_SIZE;
        if (ckb_load_cell_by_field(buf, &len, 0, 1, CKB_SOURCE_DEP, CKB_CELL_FIELD_DATA) != CKB_SUCCESS) {
            return MSDC_LOAD_DATA_ERROR;
        }
        int count = buf[0];
        if (len != 2 + count * BLAKE160_SIZE) {
            return MSDC_INVALID_CONFIG_DATA;
        }
    }
    msdc_config.total = buf[0];
    msdc_config.threshold = buf[1];
    if (msdc_config.total > MSDC_MAX_MULTI_SIGNERS) {
        return MSDC_TOO_MANY_SIGNERS;
    }
    for (int i = 0; i < msdc_config.total; i++) {
        memcpy(msdc_config.blake160s[i], &buf[2 + i * BLAKE160_SIZE], BLAKE160_SIZE);
    }
    return 0;
}

static int msdc_verify() {
    int ok_count = 0;
    int ret;
    for (int i = 0; i < msdc_config.total; i++) {
        ret = verify_sighash_all((char *)msdc_config.blake160s[i], i);
        if (ret == CKB_SUCCESS) {
            ok_count += 1;
        }
    }

    if (ok_count < msdc_config.threshold) {
        return MSDS_VERIFICATION_FAILED;
    }
    return 0;
}

static int mdsc_run() {
    return msdc_parse_config() || msdc_verify();
}

static int mdsc_total() {
    return msdc_config.total;
}
#endif