#!/usr/bin/python3
# encoding=utf-8

# python3 -m pip install influxdb

import time
import csv
import sys
import os
from itertools import islice
from influxdb import InfluxDBClient


# influxdb
client = InfluxDBClient(host='ts-212', port=8086)
client.create_database("netatmo")
client.switch_database("netatmo")


def main():
    files = []
    for r, d, f in os.walk("./exports"):
        for file in f:
            if '.csv' in file:
                files.append(os.path.join(r, file))

    for f in files:
        if f.endswith("-INDOOR.csv"):
            import_indoor(f)
        elif f.endswith("-OUTDOOR.csv"):
            import_outdoor(f)
        elif f.endswith("-RAIN.csv"):
            import_rain(f)
        # else:
        #     # dont' delete any other files
        #     continue
        # # delete imported file
        # os.remove(f)


def mk_float(s):
    s = s.strip()
    return float(s) if s else 0.0


def import_indoor(filename):
    with open(filename, newline='') as csvfile:
        # spamreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        spamreader = csv.reader(csvfile, delimiter=';', quotechar='|')


        # row_index = 0
        # for row in spamreader:
        #     if row_index == 0 || rows_Index == 2:
        #         continue
            
        #     if row_index == 1:
                
            
        #     column_index = 0
        #     for column in row:
            # column_index++

        for row in islice(spamreader, 3, None):
            timestamp = int(row[0]) * 1000000000
                    
            influx_data = [
                {
                "measurement": "Temperature",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[2])},
                "time": timestamp
                },
                {
                "measurement": "Humidity",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[3])},
                "time": timestamp
                },
                {
                "measurement": "CO2",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[4])},
                "time": timestamp
                },
                {
                "measurement": "Noise",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[5])},
                "time": timestamp
                },
                {
                "measurement": "Pressure",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[6])},
                "time": timestamp
                }
            ]

            # write particular sensor data into influxdb
            print(influx_data)
            print(client.write_points(influx_data))

def import_rain(filename):
    with open(filename, newline='') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        for row in islice(spamreader, 3, None):
            print(', '.join(row))
            #print(row[0])
            
            timestamp = int(row[0]) * 1000000000
                    
            influx_data = [
                {
                "measurement": "Rain",
                "tags": {
                    "module": "Rain",
                    },
                "fields": {"value": mk_float(row[2])},
                "time": timestamp
                }
            ]

            # write particular sensor data into influxdb
            print(influx_data)
            print(client.write_points(influx_data))


def import_outdoor(filename):
    with open(filename, newline='') as csvfile:
        spamreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        for row in islice(spamreader, 3, None):
            print(', '.join(row))
            #print(row[0])
            
            timestamp = int(row[0]) * 1000000000
                    
            influx_data = [
                {
                "measurement": "Temperature",
                "tags": {
                    "module": "Outdoor",
                    },
                "fields": {"value": mk_float(row[2])},
                "time": timestamp
                },
                {
                "measurement": "Humidity",
                "tags": {
                    "module": "Outdoor",
                    },
                "fields": {"value": mk_float(row[3])},
                "time": timestamp
                },
            ]

            # write particular sensor data into influxdb
            print(influx_data)
            print(client.write_points(influx_data))

if __name__== "__main__":
  main()