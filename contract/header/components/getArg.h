#ifndef GETARG_H
#define GETARG_H

#include "script.h"

unsigned char g_script[SCRIPT_SIZE];

static const uint8_t* get_arg() {
  int ret;
  uint64_t len = 0;

  ckb_debug("Enter get_arg\n");

  /* Load args */
  len = SCRIPT_SIZE;
  ret = ckb_load_script(g_script, &len, 0);
  if (ret != CKB_SUCCESS) {
    ckb_debug("error 16\n");
    return NULL;
  }

  mol_seg_t script_seg;
  script_seg.ptr = (uint8_t *)g_script;
  script_seg.size = len;

  if (MolReader_Script_verify(&script_seg, false) != MOL_OK) {
    ckb_debug("error 27\n");
    return NULL;
  }

  mol_seg_t args_seg = MolReader_Script_get_args(&script_seg);
  mol_seg_t args_bytes_seg = MolReader_Bytes_raw_bytes(&args_seg);
  if (args_bytes_seg.size != BLAKE160_SIZE) {
    ckb_debug("error 34\n");
    return NULL;
  }

  return args_bytes_seg.ptr;
}
#endif