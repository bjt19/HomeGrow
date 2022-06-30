import paho.mqtt.client as mqtt
import time
import SI1145.SI1145 as SI1145
from adc_sensor import read_adc
from light_sensor import read_light
from gpiozero import LED

# GPIO Pins for pumps and LED lights
lights = 18
pump_nutrient=23
pump_water=17

led = LED(lights)
nuts = LED(pump_nutrient)
wats = LED(pump_water)

# Initial state of GPIO pins
led.off()
nuts.off()
wats.off()

broker = "test.mosquitto.org"
broker_port = 1883
encoding = 'utf-8'
adc_val = [0]*4

def on_connect(client, userdata, flags, rc):
    if rc == 0:
        print("Connected success")
    else:
        print(f"Connected fail with code {rc}")

def on_message(client, userdata, message):
    print("Received message:{} on topic {}".format(message.payload,message.topic))

    # pi will receive messages about optimal nutrients and light level
    if message.topic == "IC.AIHydro/Opt_Nutrients":
        global opt_nutrient
        opt_nutrient = message.payload
    elif message.topic == "IC.AIHydro/Opt_Light":
        global opt_light
        opt_light = message.payload


client = mqtt.Client()
client.on_connect = on_connect

try:
    client.connect("test.mosquitto.org", 1883, 60)
    client.subscribe("IC.AIHydro/Opt_Nutrients")
    client.subscribe("IC.AIHydro/Opt_Light")
    print("Connection success")
except:
    print("Connection Failed")

client.on_message = on_message
client.loop_start()

x=[60]*(86400//5)
t_lights=(86400//5*60)
light_stat=t_lights
light_reading = 0

while True:
        UV_val = read_light()
        adc_val = read_adc()

        if adc_val[0] < opt_nutrient:
            nuts.on()
            nutrient_status = 1
        else:
            nuts.off()
            nutrient_status = 0

        wat_interval = 1
        wats.off()
        water_status = 0
        if wat_interval%60 == 0:
            wats.on()
            water_status = 1
        wat_interval += 1

        loss=x.pop(0)
        light_stat -=loss
        x.append(light_reading)
        light_stat +- light_reading
        if light_stat>t_lights:
            led.on()
            light_status = 1
        else:
            led.off()
            light_status = 0

        client.publish("IC.AIHydro/Light", payload=UV_val,qos=0,retain=False)
        client.publish("IC.AIHydro/Light_Status", payload=light_status,qos=0,retain=False)
        client.publish("IC.AIHydro/Nutrients", payload=adc_val[0],qos=0,retain=False)
        client.publish("IC.AIHydro/Nutrients_Status", payload=nutrient_status,qos=0,retain=False)
        client.publish("IC.AIHydro/Water", payload=adc_val[1],qos=0,retain=False)
        client.publish("IC.AIHydro/Water_Status", payload=water_status,qos=0,retain=False)

        time.sleep(5)

