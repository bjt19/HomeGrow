import time

# Import the ADS1x15 module.
import Adafruit_ADS1x15

# Create an ADS1115 ADC (16-bit) instance.
adc = Adafruit_ADS1x15.ADS1115()

GAIN = 1
def water_calibrate(val):
    
    if (val < 1000):
        return 1
    else:
        return 0

def tds_calibrate(val):

    volts = val*3.3*32768
    Tds_val = 0.5*(133.42*volts*volts*volts - 255.86*volts*volts+857.39*volts)
    return Tds_val

# Main loop.
def read_adc():
    values = [0]*4
    for i in range(4):
        # Read the specified ADC channel using the previously set gain value.
        values[0] =water_calibrate( adc.read_adc(0, gain=GAIN))
        values[1] = tds_calibrate( adc.read_adc(1, gain=GAIN))
        values[2] = adc.read_adc(2, gain=GAIN)
        values[3] = adc.read_adc(3, gain=GAIN)

    print('| {0:>6} | {1:>6} | {2:>6} | {3:>6} |'.format(*values))
    return values