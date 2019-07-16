TARGET := riscv64-unknown-elf
CC := $(TARGET)-gcc
LD := $(TARGET)-gcc
CFLAGS := -O2 -mcmodel=medlow -DSECP256K1_CUSTOM_FUNCS -I deps/flatcc/include -I deps/secp256k1/src -I deps/secp256k1 -I header -Wall -Werror -Wno-nonnull-compare
LDFLAGS := -Wl,-static -fdata-sections -ffunction-sections -Wl,--gc-sections -Wl,-s
SECP256K1_LIB := deps/secp256k1/.libs/libsecp256k1.a
FLATCC := deps/flatcc/bin/flatcc

all: vote htlc always_success

always_success: always_success.c header/protocol_reader.h $(SECP256K1_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o build/$@ $<

vote: vote.c header/protocol_reader.h $(SECP256K1_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o build/$@ $<

htlc: htlc.c header/protocol_reader.h $(SECP256K1_LIB)
	$(CC) $(CFLAGS) $(LDFLAGS) -o build/$@ $<

header/protocol_reader.h: header/protocol.fbs $(FLATCC)
	$(FLATCC) -c --reader -o header $<

$(FLATCC):
	cd deps/flatcc && scripts/initbuild.sh make && scripts/build.sh

$(SECP256K1_LIB):
	cd deps/secp256k1 && \
		./autogen.sh && \
		CC=$(CC) LD=$(LD) ./configure --with-bignum=no --enable-ecmult-static-precomputation --enable-endomorphism --enable-module-recovery --host=$(TARGET) && \
		make libsecp256k1.la

deps: header/protocol_reader.h $(SECP256K1_LIB)

clean:
	rm -rf vote htlc debug_cell
	cd deps/flatcc && scripts/cleanall.sh
	cd deps/secp256k1 && make clean

dist: clean all

.PHONY: all clean dist deps
