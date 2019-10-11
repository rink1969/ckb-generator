#ifndef GETARG_H
#define GETARG_H

#include "script.h"

unsigned char g_script[SCRIPT_SIZE];

static const uint8_t* get_arg() {
  int ret;
  volatile uint64_t len = 0;
  mol_pos_t script_pos;
  mol_read_res_t args_res;
  mol_read_res_t bytes_res;

  /* Load args */
  len = SCRIPT_SIZE;
  ret = ckb_load_script(g_script, &len, 0);
  if (ret != CKB_SUCCESS) {
    return NULL;
  }
  script_pos.ptr = (const uint8_t*)g_script;
  script_pos.size = len;
  args_res = mol_cut(&script_pos, MOL_Script_args());
  if (args_res.code != 0) {
    return NULL;
  }
  bytes_res = mol_cut_bytes(&args_res.pos);
  if (bytes_res.code != 0) {
    return NULL;
  } else if (bytes_res.pos.size == 0) {
    return NULL;
  }
  return bytes_res.pos.ptr;
}
#endif