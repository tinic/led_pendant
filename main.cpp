#include <stdint.h>
#include "chip.h"
#include "gpio_8xx.h"
#include "swm_8xx.h"
#include "spi_8xx.h"
#include "iap.h"

#include "cr_section_macros.h"

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

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

static uint32_t program_count = 8;

static struct eeprom_settings {

#ifdef CHIP_LPC82X
	#define START_ADDR_LAST_SECTOR  0x00007C00
	#define SECTOR_SIZE             1024
	#define IAP_LAST_SECTOR         31
	#define IAP_NUM_BYTES_TO_WRITE  64
#endif  // #ifdef CHIP_LPC82X

	eeprom_settings() {
		program_curr = 0;
		program_change_count = 0;
	}

	void load() {
		uint32_t *src = ((uint32_t *)(START_ADDR_LAST_SECTOR));
		uint32_t *dst =  (uint32_t *)this;
		for (uint32_t x = 0; x < 16; x++) {
			*dst++ = *src++;
		}
		if (program_curr >= program_count) {
			program_curr = 0;
			program_change_count = 0;
		}
	}

	void save() {
		uint32_t *src = ((uint32_t *)(START_ADDR_LAST_SECTOR));
		uint32_t *dst =  (uint32_t *)this;
		for (uint32_t x = 0; x < 16; x++) {
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

	uint32_t program_curr;
	uint32_t program_change_count;
	uint32_t padding[14];

} eeprom_settings;


static const uint8_t frnt_ring_indecies[] = {0x14, 0x15, 0x16, 0x17, 0x10, 0x11, 0x12, 0x13};
static const uint8_t back_ring_indecies[] = {0x04, 0x05, 0x06, 0x07, 0x00, 0x01, 0x02, 0x03};
static const uint8_t frnt_bird_indecies[] = {0x0D, 0x0C, 0x0E, 0x0F};
static const uint8_t back_bird_indecies[] = {0x09, 0x0B, 0x0A, 0x08};

#define TOTAL_LEDS 			24
#define HALF_LEDS 			12
#define	RESET_LENGTH_BITS 	100
#define SINGLE_BUFFER_SIZE	(HALF_LEDS*3*8+RESET_LENGTH_BITS)

static uint8_t led_data[TOTAL_LEDS*3] = { 0x00 } ;
static uint8_t zero_data[TOTAL_LEDS*3] = { 0x00 } ;

static uint8_t spi0_data[SINGLE_BUFFER_SIZE] = { 0x00 };
static uint8_t spi1_data[SINGLE_BUFFER_SIZE] = { 0x00 };

static void convert_spi() {
	uint8_t *dst0 = &spi0_data[RESET_LENGTH_BITS];
	uint8_t *dst1 = &spi1_data[RESET_LENGTH_BITS];
	for (int32_t c = 0; c < HALF_LEDS*3; c++) {
		uint32_t p0 = led_data[c];
		uint32_t p1 = led_data[c+HALF_LEDS*3];
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

static void init_spi() {

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


static void push_spi_frame_gamma()  {
	convert_spi();

	Chip_SPI_ClearStatus(LPC_SPI0, SPI_STAT_CLR_RXOV | SPI_STAT_CLR_TXUR | SPI_STAT_CLR_SSA | SPI_STAT_CLR_SSD);
	Chip_SPI_SetControlInfo(LPC_SPI0, 8, SPI_TXCTL_ASSERT_SSEL | SPI_TXCTL_EOF | SPI_TXCTL_RXIGNORE);
	Chip_SPI_ClearStatus(LPC_SPI1, SPI_STAT_CLR_RXOV | SPI_STAT_CLR_TXUR | SPI_STAT_CLR_SSA | SPI_STAT_CLR_SSD);
	Chip_SPI_SetControlInfo(LPC_SPI1, 8, SPI_TXCTL_ASSERT_SSEL | SPI_TXCTL_EOF | SPI_TXCTL_RXIGNORE);

	for (uint32_t c = 0; c< sizeof(spi0_data); c++) {
		LPC_SPI0->TXDAT = spi0_data[c];
		LPC_SPI1->TXDAT = spi1_data[c];
		while (!(LPC_SPI0->STAT&SPI_STAT_TXRDY)) {}
		while (!(LPC_SPI1->STAT&SPI_STAT_TXRDY)) {}
	}

	Chip_SPI_SendLastFrame_RxIgnore(LPC_SPI0,0,8);
	Chip_SPI_ClearStatus(LPC_SPI0, SPI_STAT_CLR_SSD);
	Chip_SPI_SendLastFrame_RxIgnore(LPC_SPI1,0,8);
	Chip_SPI_ClearStatus(LPC_SPI1, SPI_STAT_CLR_SSD);
}

static uint32_t colorFrom(uint32_t r, uint32_t g, uint32_t b) {
	return (g<<16) | (r<<8) | b; 
}

static uint32_t rgb_cycle(uint8_t pos) {
	pos = 255 - pos;
	if(pos < 85) {
		return colorFrom(255 - pos * 3, 0, pos * 3);
	}
	if(pos < 170) {
		pos -= 85;
		return colorFrom(0, pos * 3, 255 - pos * 3);
	}
	pos -= 170;
	return colorFrom(pos * 3, 255 - pos * 3, 0);
}

static bool test_button() {
	if (Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2)) {
		delay(100);
		for (;Chip_GPIO_ReadPortBit(LPC_GPIO_PORT, 0, 2);) {
		}
		eeprom_settings.program_curr++;
		eeprom_settings.program_change_count++;
		if (eeprom_settings.program_curr >= program_count) {
			eeprom_settings.program_curr = 0;
		}
		eeprom_settings.save();
		return true;
	}
	return false;
}

static void simple_ring(uint32_t color) {
	for (; ;) {
		for (uint32_t d = 0; d < 8; d++) {
			led_data[frnt_ring_indecies[d]*3+0] = gamma_curve[((color>>16)&0xFF)];
			led_data[frnt_ring_indecies[d]*3+1] = gamma_curve[((color>> 8)&0xFF)];
			led_data[frnt_ring_indecies[d]*3+2] = gamma_curve[((color>> 0)&0xFF)];
			led_data[back_ring_indecies[d]*3+0] = gamma_curve[((color>>16)&0xFF)];
			led_data[back_ring_indecies[d]*3+1] = gamma_curve[((color>> 8)&0xFF)];
			led_data[back_ring_indecies[d]*3+2] = gamma_curve[((color>> 0)&0xFF)];
		}

		for (uint32_t d = 0; d < 4; d++) {
			led_data[frnt_bird_indecies[d]*3+0] = gamma_curve[((color>>16)&0xFF)];
			led_data[frnt_bird_indecies[d]*3+1] = gamma_curve[((color>> 8)&0xFF)];
			led_data[frnt_bird_indecies[d]*3+2] = gamma_curve[((color>> 0)&0xFF)];
			led_data[back_bird_indecies[d]*3+0] = gamma_curve[((color>>16)&0xFF)];
			led_data[back_bird_indecies[d]*3+1] = gamma_curve[((color>> 8)&0xFF)];
			led_data[back_bird_indecies[d]*3+2] = gamma_curve[((color>> 0)&0xFF)];
		}

		delay(5);

		push_spi_frame_gamma();

		if (test_button()) {
			return;
		}
	}
}


static void simple_walker() {

	static uint8_t work_buffer[0x80] = { 0 };

	for (uint32_t c = 0; c < 0x80; c++) {
		work_buffer[c] = max(0,(sine_wave[c] - 0x80) - 0x20) ;
	}

	uint32_t walk = 0;
	uint32_t rgb_walk = 0;
	uint32_t flash = 0;
	for (;;) {
		uint32_t color = rgb_cycle(rgb_walk/3);
		for (uint32_t d = 0; d < 8; d++) {
			led_data[frnt_ring_indecies[d]*3+0] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>>16)&0xFF)) >> 8];
			led_data[frnt_ring_indecies[d]*3+1] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>> 8)&0xFF)) >> 8];
			led_data[frnt_ring_indecies[d]*3+2] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>> 0)&0xFF)) >> 8];
			led_data[back_ring_indecies[d]*3+0] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>>16)&0xFF)) >> 8];
			led_data[back_ring_indecies[d]*3+1] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>> 8)&0xFF)) >> 8];
			led_data[back_ring_indecies[d]*3+2] = gamma_curve[(work_buffer[(((0x80/8)*d) + walk)&0x7F] * ((color>> 0)&0xFF)) >> 8];
		}

		for (uint32_t d = 0; d < 4; d++) {
			led_data[frnt_bird_indecies[d]*3+0] = gamma_curve[0x40];
			led_data[frnt_bird_indecies[d]*3+1] = gamma_curve[0x40];
			led_data[frnt_bird_indecies[d]*3+2] = gamma_curve[0x00];
			led_data[back_bird_indecies[d]*3+0] = gamma_curve[0x40];
			led_data[back_bird_indecies[d]*3+1] = gamma_curve[0x40];
			led_data[back_bird_indecies[d]*3+2] = gamma_curve[0x00];
		}

		if (flash) {
			for (uint32_t d = 0; d < 8; d++) {
				led_data[frnt_ring_indecies[d]*3+0] = 0x40;
				led_data[frnt_ring_indecies[d]*3+1] = 0x40;
				led_data[frnt_ring_indecies[d]*3+2] = 0x40;
				led_data[back_ring_indecies[d]*3+0] = 0x40;
				led_data[back_ring_indecies[d]*3+1] = 0x40;
				led_data[back_ring_indecies[d]*3+2] = 0x40;
			}
			for (uint32_t d = 0; d < 4; d++) {
				led_data[frnt_bird_indecies[d]*3+0] = 0x40;
				led_data[frnt_bird_indecies[d]*3+1] = 0x40;
				led_data[frnt_bird_indecies[d]*3+2] = 0x40;
				led_data[back_bird_indecies[d]*3+0] = 0x40;
				led_data[back_bird_indecies[d]*3+1] = 0x40;
				led_data[back_bird_indecies[d]*3+2] = 0x40;
			}
		}

		walk ++;
		walk &= 0x7F;

		rgb_walk ++;
		if (rgb_walk > 256*3) {
			rgb_walk = 0;
		}

		delay(5);

		push_spi_frame_gamma();

		if (test_button()) {
			return;
		}

  		static uint8_t ch[9] = { '0', '0', '0', '0', '0', '0', '0', '0', '\n' };
		static const uint8_t lk[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' }; 

		uint32_t min = 0xFFF;
		uint32_t max = 0;

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

		if ( (max - min) > 0x100 ) {
			flash = 1;
		} else {
			flash = 0;
		}
	}
}

int init_adc() {
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

void init_uart() {
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

int main () {
	Chip_Clock_SetupSystemPLL(3, 1);

	while (!Chip_Clock_IsSystemPLLLocked()) {}

	Chip_Clock_SetSysClockDiv(1);

	SystemCoreClockUpdate();

	Chip_GPIO_Init(LPC_GPIO_PORT);

	Chip_SWM_Init();

	// wait for caps to charge before we enable LEDs
	delay(500);

	eeprom_settings.load();

#ifdef CHIP_LPC82X
	/* Pin Assign 1 bit Configuration */
	/* RESET */    
	LPC_SWM->PINENABLE0 = 0xfffffeffUL;
#else //#ifdef CHIP_LPC82X
	/* Pin Assign 1 bit Configuration */
	/* RESET */
  	LPC_SWM->PINENABLE0 = 0xffffffbfUL;
#endif //#ifdef CHIP_LPC82X

	init_adc();

	init_uart();

	init_spi();

	Chip_SWM_Deinit();

    while(1) {
		switch(eeprom_settings.program_curr) {
			case	0:
					simple_walker();
					break;
			case	1:
					simple_ring(0x400000);
					break;
			case	2:
					simple_ring(0x004000);
					break;
			case	3:
					simple_ring(0x000040);
					break;
			case	4:
					simple_ring(0x404000);
					break;
			case	5:
					simple_ring(0x004040);
					break;
			case	6:
					simple_ring(0x400040);
					break;
			case	7:
					simple_ring(0x404040);
					break;
			default:
					break;
		}
    }
}

