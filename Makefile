# Minimal gcc makefile for LPC824

# default Linux USB device name for upload
TTY ?= /dev/ttyUSB*

# use the arm cross compiler, not std gcc
TRGT = arm-none-eabi-
CC = $(TRGT)gcc
CXX = $(TRGT)g++
CP = $(TRGT)objcopy
DUMP = $(TRGT)objdump

# compiler and linker settings
COMMONFLAGS = -DCORE_M0PLUS -mcpu=cortex-m0plus -mthumb -I./ -Ilpc_chip_8xx/ -Os -ggdb
CFLAGS = $(COMMONFLAGS) -std=c11
CXXFLAGS = $(COMMONFLAGS) -std=c++14  -fno-rtti -fno-exceptions 
LDFLAGS = -Xlinker -print-memory-usage -Wl,--script=lpc824.ld -nostartfiles

OBJS= main.o sysinit.o cr_startup_lpc8xx.o printf.o lpc_chip_8xx/iap.o lpc_chip_8xx/adc_8xx.o lpc_chip_8xx/chip_8xx.o lpc_chip_8xx/clock_8xx.o lpc_chip_8xx/gpio_8xx.o lpc_chip_8xx/syscon_8xx.o lpc_chip_8xx/irc_8xx.o lpc_chip_8xx/sysinit_8xx.o lpc_chip_8xx/pmu_8xx.o lpc_chip_8xx/swm_8xx.o lpc_chip_8xx/spi_8xx.o lpc_chip_8xx/uart_8xx.o lpc_chip_8xx/ring_buffer.o  lpc_chip_8xx/sct_8xx.o lpc_chip_8xx/sct_pwm_8xx.o lpc_chip_8xx/iocon_8xx.o


#lpc_chip_8xx/acmp_8xx.o lpc_chip_8xx/chip_8xx.o lpc_chip_8xx/clock_8xx.o lpc_chip_8xx/crc_8xx.o lpc_chip_8xx/gpio_8xx.o lpc_chip_8xx/i2c_common_8xx.o lpc_chip_8xx/i2cm_8xx.o lpc_chip_8xx/i2cs_8xx.o lpc_chip_8xx/iap.o lpc_chip_8xx/irc_8xx.o lpc_chip_8xx/pinint_8xx.o lpc_chip_8xx/pmu_8xx.o  lpc_chip_8xx/spim_8xx.o lpc_chip_8xx/spis_8xx.o lpc_chip_8xx/stopwatch_8xx.o  lpc_chip_8xx/syscon_8xx.o lpc_chip_8xx/sysinit_8xx.o lpc_chip_8xx/wkt_8xx.o lpc_chip_8xx/wwdt_8xx.o

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# default target
upload: firmware.bin
	./lpc21isp/lpc21isp -control -donotstart -bin $< $(TTY) 115200 0

leddebug: leddebug.cpp
	c++ -o $@ $<

dump: firmware.elf
	$(DUMP) -d $< > firmware.s

firmware.elf: $(OBJS)
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $^

%.bin: %.elf
	$(CP) -O binary $< $@

clean:
	rm -f */*.o *.o *.elf *.bin *.s

# these target names don't represent real files
.PHONY: upload dump clean

