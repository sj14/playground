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
client = InfluxDBClient(host=os.environ['INFLUX_HOST'], port=os.environ['INFLUX_PORT'])
client.create_database("netatmo")
client.switch_database("netatmo")


def main():
    files = []
    for r, d, f in os.walk("./exports"):
        for file in f:
            if '.csv' in file:
                files.append(os.path.join(r, file))

    for f in files:
        influx_data = []

        print("importing " + f)
        if f.endswith("-INDOOR.csv"):
            influx_data.extend(import_indoor(f))
        elif f.endswith("-OUTDOOR.csv"):
            influx_data.extend(import_outdoor(f))
        elif f.endswith("-RAIN.csv"):
            influx_data.extend(import_rain(f))
        else:
            # dont' delete any other files
            continue
        
        # import to influx db
        client.write_points(influx_data)

        # delete imported file
        os.remove(f)


def mk_float(s):
    s = s.strip()
    return float(s) if s else 0.0


def import_indoor(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        influx_data = []

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0]) * 1000000000
                    
            influx_data.extend([
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
                "measurement": "Pressure",
                "tags": {
                    "module": "Indoor",
                    },
                "fields": {"value": mk_float(row[6])},
                "time": timestamp
                }
            ])

            noise = mk_float(row[5])
            if noise != 0:
                influx_noise = {
                    "measurement": "Noise",
                    "tags": {
                        "module": "Indoor",
                        },
                    "fields": {"value": noise},
                    "time": timestamp
                    }
                
                influx_data.append(influx_noise)


        # write particular sensor data into influxdb
        #print(influx_data)
        return influx_data
        #client.write_points(influx_data)



def import_rain(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        influx_data = []

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0]) * 1000000000
                    
            influx_data.extend([
                {
                "measurement": "Rain",
                "tags": {
                    "module": "Rain",
                    },
                "fields": {"value": mk_float(row[2])},
                "time": timestamp
                }
            ])

        # write particular sensor data into influxdb
        #print(influx_data)
        #client.write_points(influx_data)
        return influx_data

def import_outdoor(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')
        influx_data = []

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0]) * 1000000000
                    
            influx_data.extend([
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
            ])

        # write particular sensor data into influxdb
        #print(influx_data)
        #client.write_points(influx_data)
        return influx_data

if __name__== "__main__":
  main()
