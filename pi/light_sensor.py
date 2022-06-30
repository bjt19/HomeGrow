import time
import SI1145.SI1145 as SI1145

def read_light():
        sensor = SI1145.SI1145()
        IR = sensor.readIR()
        UV = sensor.readUV()
        uvIndex = UV / 100.0
        print ('UV Index:   ' + str(uvIndex))
        return uvIndex
