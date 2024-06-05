FROM python:3.12-alpine

COPY requirements.txt .

RUN pip3 install -r requirements.txt

COPY netatmo.py .

ENV INFLUX_HOST="localhost"
ENV INTERVAL="300"

CMD [ "python3", "netatmo.py" ]
