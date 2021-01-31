FROM python:3.9-slim

COPY import_influxdb.py .
COPY getmeasurecsv.sh .
COPY run.sh .
COPY requirements.txt .

RUN pip3 install -r requirements.txt
RUN apt update && apt install -y curl

CMD [ "bash", "run.sh" ]