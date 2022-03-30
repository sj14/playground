#!/usr/bin/env bash
set -euf -o pipefail



curl http://localhost:8428/api/v1/export/csv\?format=__name__,__timestamp__:unix_s,__value__\&match\=.*_value\&start=1609459200\&end=1640995200



curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=battery_vp_value > battery_vp_value.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=Rain_value > rain.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=CO2_value > co2.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=rf_status_value > rf_status.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=Pressure_value > pressure.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=AbsolutePressure_value > AbsolutePressure.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=Noise_value > noise.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=Humidity_value > humidity.csv
curl http://localhost:8428/api/v1/export/csv\?format\=__name__,__timestamp__:unix_s,module,__value__\&match\=Temperature_value > temperature.csv