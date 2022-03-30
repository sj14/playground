#!/usr/bin/python3
# encoding=utf-8

# python3 -m pip install influxdb

import csv
import os
from itertools import islice
import psycopg2
import datetime

# postgres
postgresClient = psycopg2.connect(
    host=os.environ["POSTGRES_HOST"],
    port=os.environ["POSTGRES_PORT"],
    database=os.environ["POSTGRES_DB"],
    user=os.environ["POSTGRES_USER"],
    password=os.environ["POSTGRES_PASSWORD"])

def main():
    files = []
    for r, d, f in os.walk("./exports"):
        for file in f:
            if '.csv' in file:
                files.append(os.path.join(r, file))

    for f in files:

        print("importing " + f)
        if f.endswith("-INDOOR.csv"):
            import_indoor(f)
        elif f.endswith("-OUTDOOR.csv"):
            import_outdoor(f)
        elif f.endswith("-RAIN.csv"):
            import_rain(f)
        else:
            # dont' delete any other files
            continue
        
        # delete imported file
        os.remove(f)


def mk_float(s):
    s = s.strip()
    return float(s) if s else 0.0


def import_indoor(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0])

            with postgresClient as conn:
                with conn.cursor() as cur:
                    try:
                        cur.execute("INSERT INTO temperature (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "indoor", mk_float(row[2])))
                        cur.execute("INSERT INTO humidity (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "indoor", mk_float(row[3])))
                        cur.execute("INSERT INTO co2 (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "indoor", mk_float(row[4])))
                        cur.execute("INSERT INTO pressure (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "indoor", mk_float(row[6])))
                        noise = mk_float(row[5])
                        if noise != 0:
                            cur.execute("INSERT INTO noise (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "indoor", noise))
                    except Exception as e:
                        print("failed postgres: {0}".format(e))


def import_rain(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0])

            with postgresClient as conn:
                with conn.cursor() as cur:
                    try:
                        cur.execute("INSERT INTO rain (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "rain", mk_float(row[2])))
                    except Exception as e:
                        print("failed postgres: {0}".format(e))


def import_outdoor(filename):
    with open(filename, newline='') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';', quotechar='|')

        for row in islice(csvreader, 3, None):
            timestamp = int(row[0])

            with postgresClient as conn:
                with conn.cursor() as cur:
                    try:
                        cur.execute("INSERT INTO temperature (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "outdoor", mk_float(row[2])))
                        cur.execute("INSERT INTO humidity (time, module, value) VALUES('{0}', '{1}', '{2}');".format(datetime.datetime.utcfromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S'), "outdoor", mk_float(row[3])))
                    except Exception as e:
                        print("failed postgres: {0}".format(e))

if __name__== "__main__":
  main()
