-- https://docs.timescale.com/latest/using-timescaledb/reading-data#increase
-- increase with buckets
SELECT bucket, sum(b) as "bytes" FROM (
    SELECT time_bucket('15 minutes', time) AS bucket, 
    (
        CASE
        WHEN value >= lag(value) OVER w
            THEN value - lag(value) OVER w
        WHEN lag(value) OVER w IS NULL THEN NULL
        ELSE value -- TODO: handle integer overflow
        END
    ) AS b
    FROM metrics
    WHERE labels->>'device' = 'eth0' AND name='node_network_transmit_bytes_total' 
    WINDOW w AS (ORDER BY time)
    ORDER BY bucket DESC
) AS bucket
  WHERE bucket > NOW() - INTERVAL '75 min' -- 60 min + 15 min to start from the last completely processed bucket (see below)
    AND bucket < NOW() - INTERVAL '15 min' -- last 15 min is the bucket which is currently in process / not finished
GROUP BY bucket
ORDER BY bucket DESC
;
