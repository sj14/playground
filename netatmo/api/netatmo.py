#!/usr/bin/python3
# encoding=utf-8

# based on the lnetatmo sample

# python3 -m pip install lnetatmo
# python3 -m pip install influxdb

import os
import time
import datetime
import lnetatmo
import psycopg2


# postgres
postgresClient = psycopg2.connect(
    host=os.environ["POSTGRES_HOST"],
    port=os.environ["POSTGRES_PORT"],
    database=os.environ["POSTGRES_DB"],
    user=os.environ["POSTGRES_USER"],
    password=os.environ["POSTGRES_PASSWORD"])

interval = int(os.environ['INTERVAL'])

authorization = lnetatmo.ClientAuth()

firstRun = True

def isfloat(num):
    try:
        float(num)
        return True
    except ValueError:
        return False


while True:
    weather = lnetatmo.WeatherStationData(authorization)
    user = weather.user

    print("Station owner : ", user.mail)
    print("Data units    : ", user.unit)
    print(weather)
    print()

    # For each available module in the returned data that should not be older than one hour (3600 s) from now
    for module, moduleData in weather.lastData(exclude=3600).items():    
        when = 0

        print(moduleData)

        # first, check the response for the date
        for sensor, value in moduleData.items() :
            if sensor == "When":
                when = value

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
            
            valueType = "REAL"
            if not isfloat(value):
                valueType = "VARCHAR(255)"

            with postgresClient as conn:
                with conn.cursor() as cur:
                    try:
                        if firstRun:
                            cur.execute('''
                            CREATE TABLE IF NOT EXISTS {0} (
                                time TIMESTAMP WITHOUT TIME ZONE NOT NULL,
                                module VARCHAR(255) NOT NULL,
                                value {1} NOT NULL,
                                PRIMARY KEY (time, module)
                            );
                            '''.format(sensor.lower(), valueType))

                        cur.execute("INSERT INTO {0} (time, module, value) VALUES('{1}', '{2}', '{3}');".format(sensor.lower(), datetime.datetime.utcfromtimestamp(int(when)).strftime('%Y-%m-%d %H:%M:%S'), module.lower(), value))
                    except Exception as e:
                        print("failed postgres: {0}".format(e))

    firstRun = False
    time.sleep(interval)

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
