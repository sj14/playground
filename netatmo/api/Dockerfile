FROM python:3.9-alpine

COPY netatmo.py .
COPY requirements.txt .

RUN pip3 install -r requirements.txt

ENV INFLUX_HOST="localhost"

CMD [ "python3", "netatmo.py" ]