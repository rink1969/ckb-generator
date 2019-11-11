TARGET := riscv64-unknown-elf
CC := $(TARGET)-gcc
LD := $(TARGET)-gcc
CFLAGS := -O3 -Ideps/molecule -I deps/secp256k1/src -I deps/secp256k1 -I header -Wall -Werror -Wno-nonnull-compare -Wno-unused-function
LDFLAGS := -Wl,-static -fdata-sections -ffunction-sections -Wl,--gc-sections -Wl,-s
SECP256K1_SRC := deps/secp256k1/src/ecmult_static_pre_context.h

all: vote always_success

always_success: src/always_success.c $(SECP256K1_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS) -o build/$@ $<

vote: src/vote.c $(SECP256K1_SRC)
	$(CC) $(CFLAGS) $(LDFLAGS) -o build/$@ $<


deps: $(SECP256K1_SRC)

$(SECP256K1_SRC):
	cd deps/secp256k1 && \
		./autogen.sh && \
		CC=$(CC) LD=$(LD) ./configure --with-bignum=no --enable-ecmult-static-precomputation --enable-endomorphism --enable-module-recovery --host=$(TARGET) && \
		make src/ecmult_static_pre_context.h src/ecmult_static_context.h

clean:
	cd deps/secp256k1 && make clean

.PHONY: clean deps
