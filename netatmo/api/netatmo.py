#!/usr/bin/python3
# encoding=utf-8

# based on the lnetatmo sample

# python3 -m pip install lnetatmo
# python3 -m pip install influxdb

import os
import time
import lnetatmo
from influxdb import InfluxDBClient

# influxdb
client = InfluxDBClient(host=os.environ['INFLUX_HOST'], host=os.environ['INFLUX_PORT'])

vmetrics=os.environ['INFLUX_VICTORIAMETRICS']
if vmetrics == "":
    # don't create and switch databse when using victoria metrics
    client.create_database("netatmo")
    client.switch_database("netatmo")

interval = int(os.environ['INTERVAL'])

authorization = lnetatmo.ClientAuth()

while True:
    weather = lnetatmo.WeatherStationData(authorization)
    user = weather.user

    print("Station owner : ", user.mail)
    print("Data units    : ", user.unit)
    print(weather)
    print()

    # For each available module in the returned data that should not be older than one hour (3600 s) from now
    for module, moduleData in weather.lastData(exclude=3600).items() :    
        when = 0

        print(moduleData)

        # first, check the response for the date
        for sensor, value in moduleData.items() :
            if sensor == "When":
                when = value * 1000000000 # need nanoseconds

        # List key/values pair of sensor information (eg Humidity, Temperature, etc...)
        for sensor, value in moduleData.items() :
            if sensor == "When":
                continue

            if isinstance(value, int):
                value = float(value)

            # don't store when there is no rain etc.
            ## !! null values don't work well with grafana line graphs!!
            # if value == 0.0:
            #     continue
            
            influx_data = [{
            "measurement": sensor,
            "tags": {
                "module": module,
                },
            "fields": {"value": value},
            "time": when
            }]
            
            # write particular sensor data into influxdb
            print(influx_data)
            print(client.write_points(influx_data))
    time.sleep(interval)

# > INSERT Temperature,sensor=indoor  value=26.7 1465839830100400200
# > INSERT tempeTemperaturerature,sensor=outdoor value=20.0 1465839830100400200
# > SELECT * FROM Temperature
# name: Temperature
# time                value sensor
# ----                ------- ------
# 1465839830100400200 26.7    indoor
# 1465839830100400200 20      outdoor
#

# table-like,for query,for storing 
# weather,location=us-midwest temperature=82 1465839830100400200
#   |    -------------------- --------------  |
#   |             |             |             |
#   |             |             |             |
# +-----------+--------+-+---------+-+---------+
# |measurement|,tag_set| |field_set| |timestamp|
# +-----------+--------+-+---------+-+---------+


# OUTPUT SAMPLE :
#
# $ printAllLastData
#
#Station owner :  ph.larduinat@wanadoo.fr
#Data units    :  metric
#
#
#Office
#    AbsolutePressure : 988.7
#                 CO2 : 726
#       date_max_temp : 1400760301
#       date_min_temp : 1400736146
#            Humidity : 60
#            max_temp : 19.6
#            min_temp : 17.9
#               Noise : 46
#            Particle : 12768
#            Pressure : 988.7
#         Temperature : 19.6
#                When : 14:10:01
#Outdoor
#          battery_vp : 5200
#                 CO2 : 555
#       date_max_temp : 1400759951
#       date_min_temp : 1400732524
#            Humidity : 75
#            max_temp : 17.9
#            min_temp : 10.3
#           rf_status : 57
#         Temperature : 17.9
#                When : 14:09:25
#Greenhouse
#      date_min_temp : 1400732204
#            Humidity : 89
#            max_temp : 19.9
#            min_temp : 9.1
#           rf_status : 83
#         Temperature : 19.9
#                When : 14:09:12
