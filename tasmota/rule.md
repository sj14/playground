Set coordinates

```text
longitude 6.52
latitude 51.58
```

Enable the light during the sunset and disable during sunlight

```text
Rule1
  ON Time#Initialized DO Backlog event checksunrise=%time%; event checksunset=%time% ENDON
  ON event#checksunset>%sunset% DO Power0 1; Power1 1 ENDON
  ON event#checksunrise<%sunrise% DO Power0 1; Power1 1 ENDON
  ON event#checksunset<%sunset% DO Power0 0; Power1 0 ENDON
  ON event#checksunrise>%sunrise% DO Power0 0; Power1 0 ENDON
```

```text
Rule1 ON
```
