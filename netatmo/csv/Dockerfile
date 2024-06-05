FROM python:3.12-slim

RUN apt update && apt install -y curl

COPY requirements.txt .

RUN pip3 install -r requirements.txt

COPY import_influxdb.py .
COPY getmeasurecsv.sh .
COPY run.sh .

CMD [ "bash", "run.sh" ]
