#include <stdint.h>
#include "chip.h"
#include "gpio_8xx.h"
#include "swm_8xx.h"
#include "spi_8xx.h"
#include "iap.h"

#include "cr_section_macros.h"
#include "printf.h"

#define printf simple_printf
#define sprintf simple_sprintf

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))
#define abs(a) ((a)<0?(-a):(a))
#define constrain(x,a,b) ((x)>(b)?(b):(((x)<(a)?(a):(x))))

static volatile uint32_t system_clock_ms = 0;

extern "C" {
	void SysTick_Handler(void) {
		system_clock_ms++;
	}
}

typedef struct rgb_color {
	uint8_t red, green, blue;
	rgb_color() {};
	rgb_color(uint8_t r, uint8_t g, uint8_t b) : red(r), green(g), blue(b) {};
} rgb_color;

static rgb_color hsvToRgb(uint16_t h, uint8_t s, uint8_t v) {
	uint8_t f = (h % 60) * 255 / 60;
	uint8_t p = (255 - s) * (uint16_t)v / 255;
	uint8_t q = (255 - f * (uint16_t)s / 255) * (uint16_t)v / 255;
	uint8_t t = (255 - (255 - f) * (uint16_t)s / 255) * (uint16_t)v / 255;
	uint8_t r = 0, g = 0, b = 0;
	switch ((h / 60) % 6) {
		case 0: r = v; g = t; b = p; break;
		case 1: r = q; g = v; b = p; break;
		case 2: r = p; g = v; b = t; break;
		case 3: r = p; g = q; b = v; break;
		case 4: r = t; g = p; b = v; break;
		case 5: r = v; g = p; b = q; break;
	}
	return rgb_color(r, g, b);
}

static const uint8_t gamma_curve[256] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x02, 
	0x02, 0x02, 0x03, 0x03, 0x03, 0x03, 0x03, 0x04, 0x04, 0x04, 0x04, 0x05, 0x05, 0x05, 0x05, 0x06, 
	0x06, 0x06, 0x07, 0x07, 0x07, 0x08, 0x08, 0x08, 0x09, 0x09, 0x09, 0x0a, 0x0a, 0x0a, 0x0b, 0x0b, 
	0x0c, 0x0c, 0x0d, 0x0d, 0x0d, 0x0e, 0x0e, 0x0f, 0x0f, 0x10, 0x10, 0x11, 0x11, 0x12, 0x12, 0x13, 
	0x13, 0x14, 0x15, 0x15, 0x16, 0x16, 0x17, 0x17, 0x18, 0x19, 0x19, 0x1a, 0x1b, 0x1b, 0x1c, 0x1d, 
	0x1d, 0x1e, 0x1f, 0x1f, 0x20, 0x21, 0x21, 0x22, 0x23, 0x24, 0x24, 0x25, 0x26, 0x27, 0x28, 0x28, 
	0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 
	0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 
	0x48, 0x49, 0x4a, 0x4b, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x54, 0x55, 0x56, 0x57, 0x58, 0x5a, 
	0x5b, 0x5c, 0x5d, 0x5f, 0x60, 0x61, 0x63, 0x64, 0x65, 0x67, 0x68, 0x69, 0x6b, 0x6c, 0x6d, 0x6f, 
	0x70, 0x72, 0x73, 0x75, 0x76, 0x77, 0x79, 0x7a, 0x7c, 0x7d, 0x7f, 0x80, 0x82, 0x83, 0x85, 0x87, 
	0x88, 0x8a, 0x8b, 0x8d, 0x8e, 0x90, 0x92, 0x93, 0x95, 0x97, 0x98, 0x9a, 0x9c, 0x9d, 0x9f, 0xa1, 
	0xa2, 0xa4, 0xa6, 0xa8, 0xa9, 0xab, 0xad, 0xaf, 0xb0, 0xb2, 0xb4, 0xb6, 0xb8, 0xba, 0xbb, 0xbd, 
	0xbf, 0xc1, 0xc3, 0xc5, 0xc7, 0xc9, 0xcb, 0xcd, 0xcf, 0xd1, 0xd3, 0xd5, 0xd7, 0xd9, 0xdb, 0xdd, 
	0xdf, 0xe1, 0xe3, 0xe5, 0xe7, 0xe9, 0xeb, 0xed, 0xef, 0xf1, 0xf4, 0xf6, 0xf8, 0xfa, 0xfc, 0xff
};

static const uint8_t sine_wave[256] = {
	0x80, 0x83, 0x86, 0x89, 0x8C, 0x90, 0x93, 0x96,
	0x99, 0x9C, 0x9F, 0xA2, 0xA5, 0xA8, 0xAB, 0xAE,
	0xB1, 0xB3, 0xB6, 0xB9, 0xBC, 0xBF, 0xC1, 0xC4,
	0xC7, 0xC9, 0xCC, 0xCE, 0xD1, 0xD3, 0xD5, 0xD8,
	0xDA, 0xDC, 0xDE, 0xE0, 0xE2, 0xE4, 0xE6, 0xE8,
	0xEA, 0xEB, 0xED, 0xEF, 0xF0, 0xF1, 0xF3, 0xF4,
	0xF5, 0xF6, 0xF8, 0xF9, 0xFA, 0xFA, 0xFB, 0xFC,
	0xFD, 0xFD, 0xFE, 0xFE, 0xFE, 0xFF, 0xFF, 0xFF,
	0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFE, 0xFE, 0xFD,
	0xFD, 0xFC, 0xFB, 0xFA, 0xFA, 0xF9, 0xF8, 0xF6,
	0xF5, 0xF4, 0xF3, 0xF1, 0xF0, 0xEF, 0xED, 0xEB,
	0xEA, 0xE8, 0xE6, 0xE4, 0xE2, 0xE0, 0xDE, 0xDC, 
	0xDA, 0xD8, 0xD5, 0xD3, 0xD1, 0xCE, 0xCC, 0xC9,
	0xC7, 0xC4, 0xC1, 0xBF, 0xBC, 0xB9, 0xB6, 0xB3,
	0xB1, 0xAE, 0xAB, 0xA8, 0xA5, 0xA2, 0x9F, 0x9C,
	0x99, 0x96, 0x93, 0x90, 0x8C, 0x89, 0x86, 0x83,
	0x80, 0x7D, 0x7A, 0x77, 0x74, 0x70, 0x6D, 0x6A,
	0x67, 0x64, 0x61, 0x5E, 0x5B, 0x58, 0x55, 0x52,
	0x4F, 0x4D, 0x4A, 0x47, 0x44, 0x41, 0x3F, 0x3C,
	0x39, 0x37, 0x34, 0x32, 0x2F, 0x2D, 0x2B, 0x28,
	0x26, 0x24, 0x22, 0x20, 0x1E, 0x1C, 0x1A, 0x18,
	0x16, 0x15, 0x13, 0x11, 0x10, 0x0F, 0x0D, 0x0C,
	0x0B, 0x0A, 0x08, 0x07, 0x06, 0x06, 0x05, 0x04,
	0x03, 0x03, 0x02, 0x02, 0x02, 0x01, 0x01, 0x01,
	0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x02, 0x03,
	0x03, 0x04, 0x05, 0x06, 0x06, 0x07, 0x08, 0x0A,
	0x0B, 0x0C, 0x0D, 0x0F, 0x10, 0x11, 0x13, 0x15,
	0x16, 0x18, 0x1A, 0x1C, 0x1E, 0x20, 0x22, 0x24,
	0x26, 0x28, 0x2B, 0x2D, 0x2F, 0x32, 0x34, 0x37,
	0x39, 0x3C, 0x3F, 0x41, 0x44, 0x47, 0x4A, 0x4D,
	0x4F, 0x52, 0x55, 0x58, 0x5B, 0x5E, 0x61, 0x64,
	0x67, 0x6A, 0x6D, 0x70, 0x74, 0x77, 0x7A, 0x7D
};

static uint32_t bird_colors[] = {
	0x404000,
	0x104020,
	0x005010,
	0x004040,
	0x083040,
	0x001050,
	0x300050,
	0x401040,
	0x501000,
	0x401818,
	0x400020,
	0x453000,
	0x452000,
	0x204000,
	0x201024,
	0x102014,
	0x201810,
	0x101820,
	0x303030,
	0x202020,
};

static uint32_t ring_colors[] = {
	0x404000,
	0x104020,
	0x005010,
	0x004040,
	0x083040,
	0x001050,
	0x300050,
	0x401040,
	0x501000,
	0x401818,
	0x400020,
	0x453000,
	0x402000,
	0x204000,
	0x201024,
	0x102014,
	0x201810,
	0x101820,
	0x303030,
	0x202020,
};

static void delay(uint32_t ms) {
	for (int32_t c = 0; c < 2200*ms; c++) {
		__asm volatile (
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			"nop\n\t"
			:::
		);
	}
}

class random {
public:

	random() {}

	#define rot(x,k) (((x)<<(k))|((x)>>(32-(k))))
	uint32_t get() {
		uint32_t e = a - rot(b, 27);
		a = b ^ rot(c, 17);
		b = c + d;
		c = d + e;
		d = e + a;
		return d;
	}

	void init(uint32_t seed) {
		uint32_t i;
		a = 0xf1ea5eed, b = c = d = seed;
		for (i=0; i<20; ++i) {
		    (void)get();
		}
	}

	uint32_t get(uint32_t lower, uint32_t upper) {
		return (get() % (upper-lower)) + lower;
	}

private:
	uint32_t a; 
	uint32_t b; 
	uint32_t c; 
	uint32_t d; 

} random;

static struct eeprom_settings {

#define START_ADDR_LAST_SECTOR  0x00007C00
#define SECTOR_SIZE             1024
#define IAP_LAST_SECTOR         31
#define IAP_NUM_BYTES_TO_WRITE  64

	eeprom_settings() {
		program_count = 0;
		program_curr = 0;
		program_change_count = 0;
	}

	void load() {
		uint32_t *src = ((uint32_t *)(START_ADDR_LAST_SECTOR));
		uint32_t *dst =  (uint32_t *)this;
		for (uint32_t x = 0; x < sizeof(eeprom_settings)/4; x++) {
			*dst++ = *src++;
		}
	}

	void save() {
		uint32_t *src = ((uint32_t *)(START_ADDR_LAST_SECTOR));
		uint32_t *dst =  (uint32_t *)this;
		for (uint32_t x = 0; x < sizeof(eeprom_settings)/4; x++) {
			if (*dst++ != *src++) {
				__disable_irq();
				Chip_IAP_PreSectorForReadWrite(IAP_LAST_SECTOR, IAP_LAST_SECTOR);
				Chip_IAP_EraseSector(IAP_LAST_SECTOR, IAP_LAST_SECTOR);
				Chip_IAP_PreSectorForReadWrite(IAP_LAST_SECTOR, IAP_LAST_SECTOR);
				Chip_IAP_CopyRamToFlash(START_ADDR_LAST_SECTOR, (uint32_t *)this, IAP_NUM_BYTES_TO_WRITE);
				__enable_irq();
				return;
			}
		}
	}

	uint32_t program_count;
	uint32_t program_curr;
	uint32_t program_change_count;
	uint32_t bird_color;
	uint32_t bird_color_index;
	uint32_t ring_color;
	uint32_t ring_color_index;
	uint32_t microphone_mode;

} eeprom_settings;

class spi;
static class leds {
public:

	static void set_ring(uint32_t index, uint32_t r, uint32_t g, uint32_t b) {
		led_data[frnt_ring_indecies[index]*3+1] = r;
		led_data[frnt_ring_indecies[index]*3+0] = g;
		led_data[frnt_ring_indecies[index]*3+2] = b;
		led_data[back_ring_indecies[index]*3+1] = r;
		led_data[back_ring_indecies[index]*3+0] = g;
		led_data[back_ring_indecies[index]*3+2] = b;
	}

	static void set_ring_synced(uint32_t index, uint32_t r, uint32_t g, uint32_t b) {
		led_data[frnt_ring_indecies[index]*3+1] = r;
		led_data[frnt_ring_indecies[index]*3+0] = g;
		led_data[frnt_ring_indecies[index]*3+2] = b;
		led_data[back_ring_indecies[(8-index)&7]*3+1] = r;
		led_data[back_ring_indecies[(8-index)&7]*3+0] = g;
		led_data[back_ring_indecies[(8-index)&7]*3+2] = b;
	}

	static void set_ring_all(uint32_t index, uint32_t r, uint32_t g, uint32_t b) {
		if(index < 8) { 
			led_data[frnt_ring_indecies[index]*3+1] = r;
			led_data[frnt_ring_indecies[index]*3+0] = g;
			led_data[frnt_ring_indecies[index]*3+2] = b;
		} else if (index < 16) {
			led_data[back_ring_indecies[index-8]*3+1] = r;
			led_data[back_ring_indecies[index-8]*3+0] = g;
			led_data[back_ring_indecies[index-8]*3+2] = b;
		}
	}

	static void set_bird(uint32_t index, uint32_t r, uint32_t g, uint32_t b) {
		led_data[frnt_bird_indecies[index]*3+1] = r;
		led_data[frnt_bird_indecies[index]*3+0] = g;
		led_data[frnt_bird_indecies[index]*3+2] = b;
		led_data[back_bird_indecies[index]*3+1] = r;
		led_data[back_bird_indecies[index]*3+0] = g;
		led_data[back_bird_indecies[index]*3+2] = b;
	}

private:
	friend class spi;

#define TOTAL_LEDS 			24
#define HALF_LEDS 			12
#define	RESET_LENGTH_BITS 	100

	static const uint8_t frnt_ring_indecies[];
	static const uint8_t back_ring_indecies[];
	static const uint8_t frnt_bird_indecies[];
	static const uint8_t back_bird_indecies[];

	static uint8_t led_data[TOTAL_LEDS*3];

} leds;

const uint8_t leds::frnt_ring_indecies[] = {0x14, 0x15, 0x16, 0x17, 0x10, 0x11, 0x12, 0x13};
const uint8_t leds::back_ring_indecies[] = {0x03, 0x04, 0x05, 0x06, 0x07, 0x00, 0x01, 0x02};
const uint8_t leds::frnt_bird_indecies[] = {0x0D, 0x0C, 0x0E, 0x0F};
const uint8_t leds::back_bird_indecies[] = {0x09, 0x0B, 0x0A, 0x08};

uint8_t leds::led_data[TOTAL_LEDS*3] = { 0x00 } ;

static class spi {
public:

	static void init() {
		Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0,  9);	
		Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0,  3);	

		Chip_SWM_MovablePinAssign(SWM_SPI0_MOSI_IO,  9);
		Chip_SWM_MovablePinAssign(SWM_SPI1_MOSI_IO,  3);

		Chip_SPI_Init(LPC_SPI0);
		Chip_SPI_ConfigureSPI(LPC_SPI0, SPI_MODE_MASTER |
							  			SPI_CLOCK_CPHA0_CPOL0 |	/* Set Clock polarity to 0 */
							  			SPI_CFG_MSB_FIRST_EN | /* Enable MSB first option */
							  			SPI_CFG_SPOL_LO); /* Chipselect is active low */
		Chip_SPI_EnableMasterMode(LPC_SPI0);
		LPC_SPI0->DIV = 9;
		Chip_SPI_Enable(LPC_SPI0);

		Chip_SPI_Init(LPC_SPI1);
		Chip_SPI_ConfigureSPI(LPC_SPI1, SPI_MODE_MASTER |
							  			SPI_CLOCK_CPHA0_CPOL0 |	/* Set Clock polarity to 0 */
							  			SPI_CFG_MSB_FIRST_EN | /* Enable MSB first option */
							  			SPI_CFG_SPOL_LO); /* Chipselect is active low */
		Chip_SPI_EnableMasterMode(LPC_SPI1);
		LPC_SPI1->DIV = 9;
		Chip_SPI_Enable(LPC_SPI1);
	}

	static void push_frame()  {
		convert();

		Chip_SPI_ClearStatus(LPC_SPI0, SPI_STAT_CLR_RXOV | SPI_STAT_CLR_TXUR | SPI_STAT_CLR_SSA | SPI_STAT_CLR_SSD);
		Chip_SPI_SetControlInfo(LPC_SPI0, 8, SPI_TXCTL_ASSERT_SSEL | SPI_TXCTL_EOF | SPI_TXCTL_RXIGNORE);
		Chip_SPI_ClearStatus(LPC_SPI1, SPI_STAT_CLR_RXOV | SPI_STAT_CLR_TXUR | SPI_STAT_CLR_SSA | SPI_STAT_CLR_SSD);
		Chip_SPI_SetControlInfo(LPC_SPI1, 8, SPI_TXCTL_ASSERT_SSEL | SPI_TXCTL_EOF | SPI_TXCTL_RXIGNORE);

		__disable_irq();
		for (uint32_t c = 0; c< sizeof(spi0_data); c++) {
			LPC_SPI0->TXDAT = spi0_data[c];
			LPC_SPI1->TXDAT = spi1_data[c];
			while (!(LPC_SPI0->STAT&SPI_STAT_TXRDY)) {}
			while (!(LPC_SPI1->STAT&SPI_STAT_TXRDY)) {}
		}
		__enable_irq();

		Chip_SPI_SendLastFrame_RxIgnore(LPC_SPI0,0,8);
		Chip_SPI_ClearStatus(LPC_SPI0, SPI_STAT_CLR_SSD);
		Chip_SPI_SendLastFrame_RxIgnore(LPC_SPI1,0,8);
		Chip_SPI_ClearStatus(LPC_SPI1, SPI_STAT_CLR_SSD);
	}

private:

	static void convert() {
		uint8_t *dst0 = &spi0_data[RESET_LENGTH_BITS];
		uint8_t *dst1 = &spi1_data[RESET_LENGTH_BITS];
		for (int32_t c = 0; c < HALF_LEDS*3; c++) {
			uint32_t p0 = leds::led_data[c];
			uint32_t p1 = leds::led_data[c+HALF_LEDS*3];
			for(int32_t d = 7; d >=0; d--) {
				if (p0&(1<<d)) {
					*dst0++ = 0b11110000;
				} else {
					*dst0++ = 0b11000000;
				}
				if (p1&(1<<d)) {
					*dst1++ = 0b11110000;
				} else {
					*dst1++ = 0b11000000;
				}
			}
		}
	}

#define SINGLE_BUFFER_SIZE	(HALF_LEDS*3*8+RESET_LENGTH_BITS)

	static uint8_t spi0_data[SINGLE_BUFFER_SIZE];
	static uint8_t spi1_data[SINGLE_BUFFER_SIZE];

} spi;

uint8_t spi::spi0_data[SINGLE_BUFFER_SIZE] = { 0x00 };
uint8_t spi::spi1_data[SINGLE_BUFFER_SIZE] = { 0x00 };

class adc {
public:
	static int init() {
		Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 15);
		Chip_GPIO_SetPinOutHigh(LPC_GPIO_PORT, 0, 15);

		Chip_SCTPWM_Init(LPC_SCT);
		Chip_SWM_MovablePinAssign(SWM_SCT_OUT0_O, 1);
		Chip_SCTPWM_SetOutPin(LPC_SCT, 1, 0);
		Chip_SCTPWM_SetRate(LPC_SCT, 1000000);
		Chip_SCTPWM_SetDutyCycle(LPC_SCT, 1, Chip_SCTPWM_PercentageToTicks(LPC_SCT, 50));
		Chip_SCTPWM_Start(LPC_SCT);

		Chip_GPIO_SetPinDIRInput(LPC_GPIO_PORT, 0, 14);
		Chip_IOCON_PinSetMode(LPC_IOCON, IOCON_PIO14, PIN_MODE_INACTIVE);

		Chip_ADC_Init(LPC_ADC, 0);
		Chip_ADC_StartCalibration(LPC_ADC);
		while(!Chip_ADC_IsCalibrationDone(LPC_ADC));

		Chip_ADC_DisableSequencer(LPC_ADC, ADC_SEQA_IDX);
		Chip_ADC_SetClockRate(LPC_ADC, 8192);
		Chip_SWM_EnableFixedPin(SWM_FIXED_ADC2);
		Chip_ADC_SetupSequencer(LPC_ADC, ADC_SEQA_IDX, ADC_SEQ_CTRL_CHANSEL(2));
		Chip_ADC_EnableSequencer(LPC_ADC, ADC_SEQA_IDX);
	}

	static void sample_min_max(uint32_t &min, uint32_t &max) {
		min = 0xFFF;
		max = 0x000;
		for (int32_t c = 0; c<16; c++) {
			Chip_ADC_StartSequencer(LPC_ADC, ADC_SEQA_IDX);
			uint32_t sample = 0;
			do {
				sample = Chip_ADC_GetDataReg(LPC_ADC, 2);
			} 
			while (!(sample & ADC_SEQ_GDAT_DATAVALID));

			uint32_t smp = ADC_DR_RESULT(sample);

			if (min > smp) min = smp;
			if (max < smp) max = smp;
		}
	}
} adc;

class uart {
public:
	static void init() {
		Chip_UART_Init(LPC_USART0);

		Chip_Clock_SetUSARTNBaseClockRate(Chip_Clock_GetMainClockRate(), false);

		Chip_GPIO_SetPinDIROutput(LPC_GPIO_PORT, 0, 4);
		Chip_SWM_MovablePinAssign(SWM_U0_TXD_O, 4);

		Chip_UART_ConfigData(LPC_USART0, UART_CFG_DATALEN_8 | UART_CFG_PARITY_NONE | UART_CFG_STOPLEN_1);
		Chip_UART_SetBaud(LPC_USART0, 115200);
		Chip_UART_Enable(LPC_USART0);
		Chip_UART_TXEnable(LPC_USART0);

		NVIC_SetPriority(UART0_IRQn, 1);
		NVIC_EnableIRQ(UART0_IRQn);
	}
} uart;

static void advance_mode(uint32_t mode) {
	switch(mode) {
		case	0:
				eeprom_settings.bird_color_index++; 
				eeprom_settings.bird_color_index %= 20; 
				eeprom_settings.bird_color = bird_colors[eeprom_settings.bird_color_index];
				break;
		case	1:
				eeprom_settings.ring_color_index++; 
				eeprom_settings.ring_color_index %= 20; 
				eeprom_settings.ring_color = ring_colors[eeprom_settings.ring_color_index];
				break;
		case	2:
				eeprom_settings.microphone_mode ++;
				eeprom_settings.microphone_mode %= 2;
				break;
	}
}

static void config_mode() {

	bool do_advance = false;

	uint32_t mode = 0;
	// mode 0 == set bird color
	// mode 1 == set ring color

	// reset to defaults of mode 0 and display now
	for (uint32_t d = 0; d < 8; d++) { leds::set_ring(d,0,0,0); }
	for (uint32_t d = 0; d < 4; d++) { leds::set_bird(d,0,0,0); }
	leds::set_ring(mode,0x80,0x00,0x00);
	for (uint32_t d = 0; d < 4; d++) {
		leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
				 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
				 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
	}					
	spi::push_frame();

	// Wait for button up as to not mess things up
	if (Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2)) {
		delay(100);
		for (;Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2);) { 
		}
	}

	uint32_t last_up_time = system_clock_ms;
	for (;;) {
		for (uint32_t d = 0; d < 8; d++) { leds::set_ring(d,0,0,0); }
		for (uint32_t d = 0; d < 4; d++) { leds::set_bird(d,0,0,0); }
		switch(mode) {
			case	0:
					for (uint32_t d = 0; d < 4; d++) {
						leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
								 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
								 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
					}					
					break;
			case	1:
					for (uint32_t d = 0; d < 8; d++) {
						leds::set_ring(d, gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
								 		  gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
								 		  gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]);
					}					
					break;
			case	2:
					for (uint32_t d = 0; d < 8; d++) {
						if (eeprom_settings.microphone_mode) {
							for (uint32_t d = 0; d < 4; d++) { leds::set_bird(d,0x40,0x40,0x40); }
						} else {
							for (uint32_t d = 0; d < 4; d++) { leds::set_bird(d,0,0,0); }
						}
					}					
					break;
		}
		leds::set_ring(mode,0x40,0x00,0x00);
		spi::push_frame();
		if (Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2)) {
			uint32_t d_time = system_clock_ms;
			delay(100);
			for (;Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2);) {
				uint32_t u_time = system_clock_ms;
				// long press (>2seconds) exits config mode
				if ((u_time - d_time) > 2000) {
					eeprom_settings.save();
					return;
				}
			}
			
			// select next config mode if we 'double tapped'.
			if ((system_clock_ms - last_up_time) < 500) {
				mode++;
				mode %= 3;
				do_advance = false;
			} else {
				do_advance = true;
			}	
			last_up_time = system_clock_ms;
		}
		// advance within a mode (i.e. select next color) if we did not detect double tap
		if (do_advance && (system_clock_ms - last_up_time) > 500) {
			advance_mode(mode);
			do_advance = false;
		}
	}
}

static bool test_button() {
	static uint32_t last_config_time = 0;
	// Don't take into account this button press if we just
	// came out of configuration
	if ((system_clock_ms - last_config_time) < 1000) {
		return false;
	}
	if (Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2)) {
		uint32_t d_time = system_clock_ms;
		delay(100);
		for (;Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2);) {
			uint32_t u_time = system_clock_ms;
			// long press > 2 seconds gets us into config mode
			if ((u_time - d_time) > 2000) {
				config_mode();
				last_config_time = system_clock_ms;
				return false;
			}
		}
		// advance program if we did not end up in config mode
		eeprom_settings.program_curr++;
		eeprom_settings.program_change_count++;
		if (eeprom_settings.program_curr >= eeprom_settings.program_count) {
			eeprom_settings.program_curr = 0;
		}
		printf("Setting program #%d\n", eeprom_settings.program_curr);
		eeprom_settings.save();
		return true;
	}
	return false;
}

static void microphone_flash() {
	if (!eeprom_settings.microphone_mode) {
		return;
	}

	uint32_t min;
	uint32_t max;
	adc::sample_min_max(min, max);

	if ( (max - min) > 0x100 ) {
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, 0x40, 0x40, 0x40);
		}
		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, 0x40, 0x40, 0x40);
		}
	}
}

static void color_ring() {
	for (; ;) {
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		microphone_flash();

		delay(5);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void fade_ring() {
	for (; ;) {
		rgb_color color;
		int32_t col = eeprom_settings.ring_color;
		color = rgb_color(max(((col>>16)&0xFF)-0x20,0), max(((col>> 8)&0xFF)-0x20,0), max(((col>> 0)&0xFF)-0x20,0));
		leds::set_ring(0, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		color = rgb_color(max(((col>>16)&0xFF)-0x1A,0), max(((col>> 8)&0xFF)-0x1A,0), max(((col>> 0)&0xFF)-0x1A,0));
		leds::set_ring(1, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		leds::set_ring(7, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		color = rgb_color(max(((col>>16)&0xFF)-0x18,0), max(((col>> 8)&0xFF)-0x18,0), max(((col>> 0)&0xFF)-0x18,0));
		leds::set_ring(2, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		leds::set_ring(6, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		color = rgb_color(max(((col>>16)&0xFF)-0x10,0), max(((col>> 8)&0xFF)-0x10,0), max(((col>> 0)&0xFF)-0x10,0));
		leds::set_ring(3, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		leds::set_ring(5, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);
		color = rgb_color(max(((col>>16)&0xFF)-0x00,0), max(((col>> 8)&0xFF)-0x00,0), max(((col>> 0)&0xFF)-0x00,0));
		leds::set_ring(4, gamma_curve[color.red],  gamma_curve[color.green], gamma_curve[color.blue]);

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		microphone_flash();

		delay(5);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void rgb_walker() {

	static uint8_t work_buffer[0x80] = { 0 };

	for (uint32_t c = 0; c < 0x80; c++) {
		work_buffer[c] = max(0,(sine_wave[c] - 0x80) - 0x20) ;
	}

	uint32_t walk = 0;
	uint32_t rgb_walk = 0;
	uint32_t flash = 0;
	for (;;) {

		rgb_color color = hsvToRgb(rgb_walk/3, 255, 255);
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color.red)&0xFF)) >> 8],
					 	      gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color.green)&0xFF)) >> 8],
					 		  gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color.blue)&0xFF)) >> 8]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		walk ++;
		walk &= 0x7F;

		rgb_walk ++;
		if (rgb_walk >= 360*3) {
			rgb_walk = 0;
		}

		delay(5);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}
static void rgb_glow() {
	uint32_t rgb_walk = 0;
	uint32_t walk = 0;
	int32_t switch_dir = 1;
	uint32_t switch_counter = 0;
	for (;;) {

		rgb_color color = hsvToRgb(rgb_walk, 255, 255);
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring_synced(d,
				gamma_curve[((color.red)&0xFF)/4],
				gamma_curve[((color.green)&0xFF)/4],
				gamma_curve[((color.blue)&0xFF)/4]
			);
		}
		
		rgb_walk ++;
		if (rgb_walk >= 360) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(50);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void rgb_tracer() {
	uint32_t rgb_walk = 0;
	uint32_t walk = 0;
	int32_t switch_dir = 1;
	uint32_t switch_counter = 0;
	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d,0,0,0);
		}

		rgb_color color = hsvToRgb(rgb_walk/3, 255, 255);
		leds::set_ring_synced(walk&0x7,
			gamma_curve[((color.red)&0xFF)/4],
			gamma_curve[((color.green)&0xFF)/4],
			gamma_curve[((color.blue)&0xFF)/4]
		);

		walk += switch_dir;

		rgb_walk += 7;
		if (rgb_walk >= 360*3) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		switch_counter ++;
		if (switch_counter > 64 && random.get(0,2)) {
			switch_dir *= -1;
			switch_counter = 0;
			walk += switch_dir;
			walk += switch_dir;
		}

		delay(50);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void light_tracer() {
	uint32_t walk = 0;

	rgb_color gradient[8];
	int32_t col = eeprom_settings.ring_color;
	gradient[7] = rgb_color(max(((col>>16)&0xFF)-0x40,0), max(((col>> 8)&0xFF)-0x40,0), max(((col>> 0)&0xFF)-0x40,0));
	gradient[6] = rgb_color(max(((col>>16)&0xFF)-0x40,0), max(((col>> 8)&0xFF)-0x40,0), max(((col>> 0)&0xFF)-0x40,0));
	gradient[5] = rgb_color(max(((col>>16)&0xFF)-0x30,0), max(((col>> 8)&0xFF)-0x30,0), max(((col>> 0)&0xFF)-0x30,0));
	gradient[4] = rgb_color(max(((col>>16)&0xFF)-0x18,0), max(((col>> 8)&0xFF)-0x18,0), max(((col>> 0)&0xFF)-0x18,0));
	gradient[3] = rgb_color(max(((col>>16)&0xFF)-0x00,0), max(((col>> 8)&0xFF)-0x00,0), max(((col>> 0)&0xFF)-0x00,0));
	gradient[2] = rgb_color(max((col>>16)&0xFF,0x10), max((col>> 8)&0xFF,0x00), max((col>> 0)&0xFF,0x20));
	gradient[1] = rgb_color(max((col>>16)&0xFF,0x30), max((col>> 8)&0xFF,0x30), max((col>> 0)&0xFF,0x30));
	gradient[0] = rgb_color(max((col>>16)&0xFF,0x40), max((col>> 8)&0xFF,0x40), max((col>> 0)&0xFF,0x40));

	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring((walk+d)&0x7,
				gamma_curve[((gradient[d].red)&0xFF)],
				gamma_curve[((gradient[d].green)&0xFF)],
				gamma_curve[((gradient[d].blue)&0xFF)]
			);
		}

		walk--;

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(100);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}


static void ring_tracer() {
	uint32_t walk = 0;
	int32_t switch_dir = 1;
	uint32_t switch_counter = 0;
	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d,0,0,0);
		}

		leds::set_ring_synced((walk+0)&0x7,
			gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]
		);
		leds::set_ring_synced((walk+1)&0x7,
			gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]
		);
		leds::set_ring_synced((walk+2)&0x7,
			gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]
		);

		walk += switch_dir;

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		switch_counter ++;
		if (switch_counter > 64 && random.get(0,2)) {
			switch_dir *= -1;
			switch_counter = 0;
			walk += switch_dir;
			walk += switch_dir;
		}

		delay(50);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void ring_bar() {
	uint32_t rgb_walk = 0;
	uint32_t walk = 0;
	int32_t switch_dir = 1;
	uint32_t switch_counter = 0;
	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d,0,0,0);
		}

		leds::set_ring_synced((walk+0)&0x7,
			gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]
		);
		leds::set_ring_synced((walk+4)&0x7,
			gamma_curve[(eeprom_settings.ring_color>>16)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 8)&0xFF],
			gamma_curve[(eeprom_settings.ring_color>> 0)&0xFF]
		);

		walk += switch_dir;

		rgb_walk += 7;
		if (rgb_walk >= 360*3) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		switch_counter ++;
		if (switch_counter > 64 && random.get(0,2)) {
			switch_dir *= -1;
			switch_counter = 0;
			walk += switch_dir;
			walk += switch_dir;
		}

		delay(50);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void rgb_vertical_wall() {
	uint32_t rgb_walk = 0;
	for (;;) {

		rgb_color color;
		color = hsvToRgb(((rgb_walk+  0)/3)%360, 255, 255);
		leds::set_ring_synced(0, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+ 30)/3)%360, 255, 255);
		leds::set_ring_synced(1, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(7, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+120)/3)%360, 255, 255);
		leds::set_ring_synced(2, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(6, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+210)/3)%360, 255, 255);
		leds::set_ring_synced(3, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(5, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+230)/3)%360, 255, 255);
		leds::set_ring_synced(4, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);

		rgb_walk += 7;
		if (rgb_walk >= 360*3) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(40);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void shine_vertical() {
	uint32_t rgb_walk = 0;
	rgb_color gradient[256];
	for (int32_t c = 0; c < 128; c++) {
		uint32_t r = max((eeprom_settings.ring_color>>16)&0xFF,c/2);
		uint32_t g = max((eeprom_settings.ring_color>> 8)&0xFF,c/2);
		uint32_t b = max((eeprom_settings.ring_color>> 0)&0xFF,c/2);
		gradient[c] = rgb_color(r, g, b);
	}
	for (int32_t c = 0; c < 128; c++) {
		uint32_t r = max((eeprom_settings.ring_color>>16)&0xFF,(128-c)/2);
		uint32_t g = max((eeprom_settings.ring_color>> 8)&0xFF,(128-c)/2);
		uint32_t b = max((eeprom_settings.ring_color>> 0)&0xFF,(128-c)/2);
		gradient[c+128] = rgb_color(r, g, b);
	}

	for (;;) {
		rgb_color color;
		color = gradient[((rgb_walk+ 0))%256];
		leds::set_ring_synced(0, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+10))%256];
		leds::set_ring_synced(1, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(7, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+40))%256];
		leds::set_ring_synced(2, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(6, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+70))%256];
		leds::set_ring_synced(3, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(5, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+80))%256];
		leds::set_ring_synced(4, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);

		rgb_walk += 7;
		if (rgb_walk >= 256) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(80);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void shine_horizontal() {
	int32_t rgb_walk = 0;
	int32_t switch_dir = 1;

	rgb_color gradient[256];
	for (int32_t c = 0; c < 128; c++) {
		uint32_t r = max((eeprom_settings.ring_color>>16)&0xFF,c/2);
		uint32_t g = max((eeprom_settings.ring_color>> 8)&0xFF,c/2);
		uint32_t b = max((eeprom_settings.ring_color>> 0)&0xFF,c/2);
		gradient[c] = rgb_color(r, g, b);
	}
	for (int32_t c = 0; c < 128; c++) {
		uint32_t r = max((eeprom_settings.ring_color>>16)&0xFF,(128-c)/2);
		uint32_t g = max((eeprom_settings.ring_color>> 8)&0xFF,(128-c)/2);
		uint32_t b = max((eeprom_settings.ring_color>> 0)&0xFF,(128-c)/2);
		gradient[c+128] = rgb_color(r, g, b);
	}

	for (;;) {
		rgb_color color;
		color = gradient[((rgb_walk+ 0))%256];
		leds::set_ring_synced(6, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+10))%256];
		leds::set_ring_synced(7, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(5, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+40))%256];
		leds::set_ring_synced(0, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(4, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+70))%256];
		leds::set_ring_synced(1, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		leds::set_ring_synced(3, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);
		color = gradient[((rgb_walk+80))%256];
		leds::set_ring_synced(2, gamma_curve[((color.red)&0xFF)], gamma_curve[((color.green)&0xFF)], gamma_curve[((color.blue)&0xFF)]);

		rgb_walk += 7*switch_dir;
		if (rgb_walk >= 256) {
			rgb_walk = 255;
			switch_dir *= -1;
		}
		if (rgb_walk < 0) {
			rgb_walk = 0;
			switch_dir *= -1;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(80);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void rgb_horizontal_wall() {
	uint32_t rgb_walk = 0;
	for (;;) {

		rgb_color color;
		color = hsvToRgb(((rgb_walk+  0)/3)%360, 255, 255);
		leds::set_ring_synced(6, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+ 30)/3)%360, 255, 255);
		leds::set_ring_synced(7, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(5, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+120)/3)%360, 255, 255);
		leds::set_ring_synced(0, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(4, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+210)/3)%360, 255, 255);
		leds::set_ring_synced(1, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		leds::set_ring_synced(3, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);
		color = hsvToRgb(((rgb_walk+230)/3)%360, 255, 255);
		leds::set_ring_synced(2, gamma_curve[((color.red)&0xFF)/4], gamma_curve[((color.green)&0xFF)/4], gamma_curve[((color.blue)&0xFF)/4]);

		rgb_walk += 7;
		if (rgb_walk >= 360*3) {
			rgb_walk = 0;
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(40);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void lightning() {
	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, 0,0,0);
		}

		int index = random.get(0,128);
		leds::set_ring_all(index,0x40,0x40,0x40);

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(10);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void sparkle() {
	for (;;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, 0,0,0);
		}

		int index = random.get(0,16);
		leds::set_ring_all(index,random.get(0x00,0x10),random.get(0x00,0x10),random.get(0,0x10));

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(eeprom_settings.bird_color>>16)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 8)&0xFF],
					 		  gamma_curve[(eeprom_settings.bird_color>> 0)&0xFF]);
		}

		delay(50);

		microphone_flash();

		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void heartbeat() {
	int32_t rgb_walk = 0;
	int32_t switch_dir = 1;
	for (; ;) {
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[(((eeprom_settings.bird_color>>16)&0xFF)*rgb_walk)/256],
					 		  gamma_curve[(((eeprom_settings.bird_color>> 8)&0xFF)*rgb_walk)/256],
					 		  gamma_curve[(((eeprom_settings.bird_color>> 0)&0xFF)*rgb_walk)/256]);
		}

		microphone_flash();

		rgb_walk += switch_dir;
		if (rgb_walk >= 256) {
			rgb_walk = 255;
			switch_dir *= -1;
		}
		if (rgb_walk < 0) {
			rgb_walk = 0;
			switch_dir *= -1;
		}

		delay(8);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void brilliance() {
	int32_t current_wait = 0;
	int32_t wait_time = 0;
	int32_t rgb_walk = 0;
	int32_t switch_dir = 1;
	for (; ;) {
		rgb_color gradient[256];
		for (int32_t c = 0; c < 112; c++) {
			uint32_t r = (eeprom_settings.bird_color>>16)&0xFF;
			uint32_t g = (eeprom_settings.bird_color>> 8)&0xFF;
			uint32_t b = (eeprom_settings.bird_color>> 0)&0xFF;
			gradient[c] = rgb_color(r, g, b);
		}
		for (int32_t c = 0; c < 16; c++) {
			uint32_t r = max((eeprom_settings.bird_color>>16)&0xFF,c*8);
			uint32_t g = max((eeprom_settings.bird_color>> 8)&0xFF,c*8);
			uint32_t b = max((eeprom_settings.bird_color>> 0)&0xFF,c*8);
			gradient[c+112] = rgb_color(r, g, b);
		}
		for (int32_t c = 0; c < 16; c++) {
			uint32_t r = max((eeprom_settings.bird_color>>16)&0xFF,(16-c)*8);
			uint32_t g = max((eeprom_settings.bird_color>> 8)&0xFF,(16-c)*8);
			uint32_t b = max((eeprom_settings.bird_color>> 0)&0xFF,(16-c)*8);
			gradient[c+128] = rgb_color(r, g, b);
		}
		for (int32_t c = 0; c < 112; c++) {
			uint32_t r = (eeprom_settings.bird_color>>16)&0xFF;
			uint32_t g = (eeprom_settings.bird_color>> 8)&0xFF;
			uint32_t b = (eeprom_settings.bird_color>> 0)&0xFF;
			gradient[c+144] = rgb_color(r, g, b);
		}


		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			rgb_color color = gradient[((rgb_walk+ 0))%256];
			leds::set_bird(d, gamma_curve[((color.red)&0xFF)], 
							  gamma_curve[((color.green)&0xFF)], 
							  gamma_curve[((color.blue)&0xFF)]);
		}

		microphone_flash();

		rgb_walk += switch_dir;
		if (rgb_walk >= 256) {
			current_wait++;
			if (current_wait > wait_time) {
				wait_time = random.get(0,2000);
				rgb_walk = 0;
				current_wait = 0;
			} else {
				rgb_walk = 255;
			}
		}

		delay(10);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void tingling() {
	#define NUM_TINGLES 16
	struct tingle {
		bool active;
		int32_t wait;
		int32_t index;
		int32_t progress;
		bool lightordark;
	} tingles[NUM_TINGLES] = {0};

	for (; ;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		for (int32_t c = 0 ; c < NUM_TINGLES; c++) {
			if (tingles[c].active == 0) {
				tingles[c].wait = random.get(0,25);
				for (;;) {
					bool done = true;
					tingles[c].index = random.get(0,16);
					for (int32_t d = 0 ; d < NUM_TINGLES; d++) {
						if( d != c && 
							tingles[c].active && 
							tingles[c].index == tingles[d].index) {
							done = false;
							break;
						}
					}
					if (done) {
						break;
					}
				}
				tingles[c].index = random.get(0,16);
				tingles[c].progress = 0;
				tingles[c].lightordark = random.get(0,2);
				tingles[c].active = 1;
			} else if (tingles[c].progress >= 16) {
				tingles[c].active = 0;
			} else if (tingles[c].wait > 0) {
				tingles[c].wait --;
			} else {
				int32_t r = 0,g = 0,b = 0;
				int32_t progress = tingles[c].progress;
				if (progress > 8) {
					progress -= 8;
					progress = 8 - progress;
				}
				if (tingles[c].lightordark) {
					r = max((eeprom_settings.ring_color>>16)&0xFF,progress*8);
					g = max((eeprom_settings.ring_color>> 8)&0xFF,progress*8);
					b = max((eeprom_settings.ring_color>> 0)&0xFF,progress*8);					
				} else {
					r = ((eeprom_settings.ring_color>>16)&0xFF)-progress*8;
					g = ((eeprom_settings.ring_color>> 8)&0xFF)-progress*8;
					b = ((eeprom_settings.ring_color>> 0)&0xFF)-progress*8;
					r = max(r,0);
					g = max(g,0);
					b = max(b,0);					
				}
				leds::set_ring_all(tingles[c].index, gamma_curve[r], 
								  				 gamma_curve[g], 
								  				 gamma_curve[b]);
				tingles[c].progress++;
			}
		}

		microphone_flash();

		delay(20);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}


static void twinkle() {
	#define NUM_TWINKLE 3
	struct tingle {
		bool active;
		int32_t wait;
		int32_t index;
		int32_t progress;
	} tingles[NUM_TWINKLE] = {0};

	for (; ;) {

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		for (int32_t c = 0 ; c < NUM_TWINKLE; c++) {
			if (tingles[c].active == 0) {
				tingles[c].wait = random.get(0,50);
				for (;;) {
					bool done = true;
					tingles[c].index = random.get(0,16);
					for (int32_t d = 0 ; d < NUM_TWINKLE; d++) {
						if( d != c && 
							tingles[c].active && 
							tingles[c].index == tingles[d].index) {
							done = false;
							break;
						}
					}
					if (done) {
						break;
					}
				}
				tingles[c].index = random.get(0,16);
				tingles[c].progress = 0;
				tingles[c].active = 1;
			} else if (tingles[c].progress >= 16) {
				tingles[c].active = 0;
			} else if (tingles[c].wait > 0) {
				tingles[c].wait --;
			} else {
				int32_t r = 0,g = 0,b = 0;
				int32_t progress = tingles[c].progress;
				if (progress > 8) {
					progress -= 8;
					progress = 8 - progress;
				}

				r = max((eeprom_settings.ring_color>>16)&0xFF,progress*16);
				g = max((eeprom_settings.ring_color>> 8)&0xFF,progress*16);
				b = max((eeprom_settings.ring_color>> 0)&0xFF,progress*16);					
				leds::set_ring_all(tingles[c].index, gamma_curve[r], 
								  				 gamma_curve[g], 
								  				 gamma_curve[b]);
				tingles[c].progress++;
			}
		}

		microphone_flash();

		delay(50);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void simple_change_ring() {

	int32_t color_index = -1;

	int32_t index = 0;

	int32_t r = random.get(0x00,0x40);
	int32_t g = random.get(0x00,0x40);
	int32_t b = random.get(0x00,0x40);
	int32_t cr = 0;
	int32_t cg = 0;
	int32_t cb = 0;
	int32_t nr = 0;
	int32_t ng = 0;
	int32_t nb = 0;

	for (; ;) {

		if (index >= 600) {
			if (index == 600) {
				cr = r;
				cg = g;
				cb = b;
				nr = random.get(0x00,0x40);
				ng = random.get(0x00,0x40);
				nb = random.get(0x00,0x40);
			}
			if (index >= 664) {
				index = 0;
			} else {
				int32_t lft = index-600;
				int32_t rgt = 64-lft;
				r = (nr*lft + cr*rgt) / 64;
				g = (ng*lft + cg*rgt) / 64;
				b = (nb*lft + cb*rgt) / 64;
				index++;
			}
		} else {
			index++;
		}

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[r],
					 		  gamma_curve[g],
					 		  gamma_curve[b]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		microphone_flash();

		delay(15);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void simple_change_bird() {

	int32_t color_index = -1;

	int32_t index = 0;

	int32_t r = random.get(0x00,0x40);
	int32_t g = random.get(0x00,0x40);
	int32_t b = random.get(0x00,0x40);
	int32_t cr = 0;
	int32_t cg = 0;
	int32_t cb = 0;
	int32_t nr = 0;
	int32_t ng = 0;
	int32_t nb = 0;

	for (; ;) {

		if (index >= 600) {
			if (index == 600) {
				cr = r;
				cg = g;
				cb = b;
				nr = random.get(0x00,0x40);
				ng = random.get(0x00,0x40);
				nb = random.get(0x00,0x40);
			}
			if (index >= 664) {
				index = 0;
			} else {
				int32_t lft = index-600;
				int32_t rgt = 64-lft;
				r = (nr*lft + cr*rgt) / 64;
				g = (ng*lft + cg*rgt) / 64;
				b = (nb*lft + cb*rgt) / 64;
				index++;
			}
		} else {
			index++;
		}

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[r],
					 		  gamma_curve[g],
					 		  gamma_curve[b]);
		}

		microphone_flash();

		delay(15);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void simple_random() {

	rgb_color colors[16];
	for (int32_t c = 0; c<16; c++) {
		colors[c].red = random.get(0x00,0x40);
		colors[c].green = random.get(0x00,0x40);
		colors[c].blue = random.get(0x00,0x40);
	}

	for (; ;) {

		uint32_t index = random.get(0x00,0x10);
		colors[index].red = random.get(0x00,0x40);
		colors[index].green = random.get(0x00,0x40);
		colors[index].blue = random.get(0x00,0x40);

		for (uint32_t d = 0; d < 16; d++) {
			leds::set_ring_all(d, gamma_curve[colors[d].red],
					 		      gamma_curve[colors[d].green],
					 		      gamma_curve[colors[d].blue]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		microphone_flash();

		delay(20);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void diagonal_wipe() {

	int32_t walk = 0;
	int32_t wait = random.get(60,1500);
	int32_t dir = random.get(0,2);

	for (; ;) {
		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[((eeprom_settings.ring_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.ring_color>> 0)&0xFF)]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		int32_t i0 = -1;
		int32_t i1 = -1;

		if (dir) {
			if (walk < 10) {
				i0 = 7;
				i1 = 7;
			} else if (walk < 20) {
				i0 = 0;
				i1 = 6;
			} else if (walk < 30) {
				i0 = 1;
				i1 = 5;
			} else if (walk < 40) {
				i0 = 2;
				i1 = 4;
			} else if (walk < 50) {
				i0 = 3;
				i1 = 3;
			} else {
				i0 = -1;
				i1 = -1;
			}	
		} else {
			if (walk < 10) {
				i0 = 1;
				i1 = 1;
			} else if (walk < 20) {
				i0 = 0;
				i1 = 2;
			} else if (walk < 30) {
				i0 = 7;
				i1 = 3;
			} else if (walk < 40) {
				i0 = 6;
				i1 = 4;
			} else if (walk < 50) {
				i0 = 5;
				i1 = 5;
			} else {
				i0 = -1;
				i1 = -1;
			}	
		}
		
		walk ++;
		if (walk > wait) {
			walk = 0;
			wait = random.get(60,1024);
			dir = random.get(0,2);
		}

		if (i0 >= 0) leds::set_ring_synced(i0, 0x40,0x40,0x40);
		if (i1 >= 0) leds::set_ring_synced(i1, 0x40,0x40,0x40);

		microphone_flash();

		delay(5);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void shimmer() {
	int32_t walk = 0;
	int32_t wait = random.get(16,64);
	int32_t dir = random.get(0,2);

	for (; ;) {

		rgb_color color = rgb_color(((eeprom_settings.ring_color>>16)&0xFF),
								  ((eeprom_settings.ring_color>> 8)&0xFF),
								  ((eeprom_settings.ring_color>> 0)&0xFF));

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[((eeprom_settings.bird_color>>16)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 8)&0xFF)],
					 		  gamma_curve[((eeprom_settings.bird_color>> 0)&0xFF)]);
		}

		int32_t r = color.red;
		int32_t g = color.green;
		int32_t b = color.blue;
		if (walk < 8) {
			r = max(int32_t(0),r - int32_t(walk));
			g = max(int32_t(0),g - int32_t(walk));
			b = max(int32_t(0),b - int32_t(walk));
		} else if (walk < 16) {
			r = max(int32_t(0),r - int32_t((8-(walk-8))));
			g = max(int32_t(0),g - int32_t((8-(walk-8))));
			b = max(int32_t(0),b - int32_t((8-(walk-8))));
		}

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[r],
					 		  gamma_curve[g],
					 		  gamma_curve[b]);
		}
		
		walk ++;
		if (walk > wait) {
			walk = 0;
			wait = random.get(16,64);

		}

		microphone_flash();

		delay(2);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}

static void red() {

	int32_t wait = 1200;

	int32_t index = 0;

	int32_t br = ((eeprom_settings.bird_color>>16)&0xFF);
	int32_t bg = ((eeprom_settings.bird_color>> 8)&0xFF);
	int32_t bb = ((eeprom_settings.bird_color>> 0)&0xFF);

	int32_t rr = ((eeprom_settings.ring_color>>16)&0xFF);
	int32_t rg = ((eeprom_settings.ring_color>> 8)&0xFF);
	int32_t rb = ((eeprom_settings.ring_color>> 0)&0xFF);

	int32_t b1r = br;
	int32_t b1g = bg;
	int32_t b1b = bb;

	int32_t r1r = rr;
	int32_t r1g = rg;
	int32_t r1b = rb;

	for (; ;) {

		if (index >= 0) {
			if (index >= wait) {
				wait = random.get(1200,10000);
				index = 0;
			} else if (index >= 0 && index < 64) {
				int32_t rgt = index-0;
				int32_t lft = 64-rgt;
				b1r = (br*lft + 0x40*rgt) / 64;
				b1g = (bg*lft + 0x00*rgt) / 64;
				b1b = (bb*lft + 0x10*rgt) / 64;
				r1r = (rr*lft + 0x40*rgt) / 64;
				r1g = (rg*lft + 0x00*rgt) / 64;
				r1b = (rb*lft + 0x10*rgt) / 64;
				index++;
			} else if (index >= 600 && index < 664) {
				int32_t lft = index-600;
				int32_t rgt = 64-lft;
				b1r = (br*lft + 0x40*rgt) / 64;
				b1g = (bg*lft + 0x00*rgt) / 64;
				b1b = (bb*lft + 0x10*rgt) / 64;
				r1r = (rr*lft + 0x40*rgt) / 64;
				r1g = (rg*lft + 0x00*rgt) / 64;
				r1b = (rb*lft + 0x10*rgt) / 64;
				index++;
			} else {
				index++;
			}
		} else {
			index++;
		}

		for (uint32_t d = 0; d < 8; d++) {
			leds::set_ring(d, gamma_curve[r1r],
					 		  gamma_curve[r1g],
					 		  gamma_curve[r1b]);
		}

		for (uint32_t d = 0; d < 4; d++) {
			leds::set_bird(d, gamma_curve[b1r],
					 		  gamma_curve[b1g],
					 		  gamma_curve[b1b]);
		}

		microphone_flash();

		delay(20);
		spi::push_frame();
		if (test_button()) {
			return;
		}
	}
}


int main () {
	Chip_Clock_SetupSystemPLL(3, 1);

	while (!Chip_Clock_IsSystemPLLLocked()) {}

	Chip_Clock_SetSysClockDiv(1);

	SystemCoreClockUpdate();

	Chip_GPIO_Init(LPC_GPIO_PORT);

	Chip_SWM_Init();

	// wait for caps to charge before we enable LEDs
	delay(500);

	/* Pin Assign 1 bit Configuration */
	/* RESET */    
	LPC_SWM->PINENABLE0 = 0xfffffeffUL;

	adc::init();

	uart::init();

	spi::init();

	Chip_SWM_Deinit();

	eeprom_settings.load();

	eeprom_settings.program_count = 24;

	if (eeprom_settings.bird_color == 0 ||
		eeprom_settings.bird_color_index > 16 ||
		eeprom_settings.ring_color == 0 ||
		eeprom_settings.ring_color_index > 16 ||
		eeprom_settings.microphone_mode > 1 ) {
	 	eeprom_settings.bird_color = 0x404000;
		eeprom_settings.bird_color_index = 0;
	 	eeprom_settings.ring_color = 0x083040;
		eeprom_settings.ring_color_index = 0;
		eeprom_settings.microphone_mode = 0;

		eeprom_settings.save();
	}

	uint32_t min;
	uint32_t max;
	adc::sample_min_max(min, max);
	random.init(min*max);

	SysTick_Config(SystemCoreClock / 1000);

	printf("== Duck Pond ==\n");
	printf("There are %d programs total\n", eeprom_settings.program_count);
	printf("Program has been changed %d times\n", eeprom_settings.program_change_count);
	printf("Setting program #%d\n", eeprom_settings.program_curr);

    while(1) {
		switch(eeprom_settings.program_curr) {
			case	0:
					color_ring();
					break;
			case	1:	
					fade_ring();
					break;
			case	2:
					rgb_walker();
					break;
			case	3:
					rgb_glow();
					break;
			case	4:
					rgb_tracer();
					break;
			case	5: 
					ring_tracer();
					break;
			case	6:
					light_tracer();
					break;
			case	7: 
					ring_bar();
					break;
			case	8:
					sparkle();
					break;
			case	9:
					lightning();
					break;
			case 	10:
					rgb_vertical_wall();
					break;
			case 	11:
					rgb_horizontal_wall();
					break;
			case	12:
					shine_vertical();
					break;
			case	13:
					shine_horizontal();
					break;	
			case 	14:
					heartbeat();
					break;
			case 	15:
					brilliance();
					break;
			case    16:
					tingling();
					break;
			case    17:
					twinkle();
					break;
			case	18:
					simple_change_ring();
					break;
			case	19:
					simple_change_bird();
					break;
			case	20:
					simple_random();
					break;
			case	21:
					diagonal_wipe();
					break;
			case	22:
					shimmer();
					break;
			case	23:
					red();
					break;
			default:
					color_ring();
					break;
		}
    }
}

