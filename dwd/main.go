package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
)

func main() {
	data, err := request()
	if err != nil {
		log.Fatalln(err)
	}

	log.Printf("%+v\n", data)
}

type Stations map[string]Station

type Station struct {
	Forecast1 struct {
		StationID                    string      `json:"stationId"`
		Start                        int64       `json:"start"`
		TimeStep                     int         `json:"timeStep"`
		Temperature                  []int       `json:"temperature"`
		TemperatureStd               []int       `json:"temperatureStd"`
		WindSpeed                    interface{} `json:"windSpeed"`
		WindDirection                interface{} `json:"windDirection"`
		WindGust                     interface{} `json:"windGust"`
		Icon                         []int       `json:"icon"`
		PrecipitationTotal           []int       `json:"precipitationTotal"`
		PrecipitationProbablity      interface{} `json:"precipitationProbablity"`
		PrecipitationProbablityIndex interface{} `json:"precipitationProbablityIndex"`
	} `json:"forecast1"`
	Forecast2 struct {
		StationID                    string        `json:"stationId"`
		Start                        int64         `json:"start"`
		TimeStep                     int           `json:"timeStep"`
		Temperature                  []interface{} `json:"temperature"`
		TemperatureStd               []interface{} `json:"temperatureStd"`
		WindSpeed                    interface{}   `json:"windSpeed"`
		WindDirection                interface{}   `json:"windDirection"`
		WindGust                     interface{}   `json:"windGust"`
		Icon                         []int         `json:"icon"`
		PrecipitationTotal           []int         `json:"precipitationTotal"`
		PrecipitationProbablity      interface{}   `json:"precipitationProbablity"`
		PrecipitationProbablityIndex interface{}   `json:"precipitationProbablityIndex"`
	} `json:"forecast2"`
	ForecastStart interface{} `json:"forecastStart"`
	Days          []struct {
		StationID      interface{} `json:"stationId"`
		DayDate        string      `json:"dayDate"`
		TemperatureMin int         `json:"temperatureMin"`
		TemperatureMax int         `json:"temperatureMax"`
		Icon           int         `json:"icon"`
		Icon1          interface{} `json:"icon1"`
		Icon2          interface{} `json:"icon2"`
		Precipitation  int         `json:"precipitation"`
		WindSpeed      int         `json:"windSpeed"`
		WindGust       int         `json:"windGust"`
		WindDirection  int         `json:"windDirection"`
		Sunshine       int         `json:"sunshine"`
	} `json:"days"`
	Warnings           []interface{} `json:"warnings"`
	ThreeHourSummaries interface{}   `json:"threeHourSummaries"`
}

func request() (Stations, error) {
	// Generated by curl-to-Go: https://mholt.github.io/curl-to-go

	// curl -X 'GET' \
	//   'https://dwd.api.proxy.bund.dev/v30/stationOverviewExtended?stationIds=10865,G005' \
	//   -H 'accept: application/json'

	req, err := http.NewRequest("GET", "https://dwd.api.proxy.bund.dev/v30/stationOverviewExtended?stationIds=13670,H419", nil)
	if err != nil {
		return Stations{}, fmt.Errorf("failed creating request: %v", err)
	}
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return Stations{}, fmt.Errorf("failed sending request: %v", err)
	}
	defer func() {
		if err := resp.Body.Close(); err != nil {
			log.Printf("failed closing body: %v\n", err)
		}
	}()

	result := Stations{}

	// result.StationID = make(map[string]StationID)
	// result.StationSubID = make(map[string]SubID)

	respBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return Stations{}, fmt.Errorf("failed reading response: %v", err)
	}

	if err := json.Unmarshal(respBytes, &result); err != nil {
		return Stations{}, fmt.Errorf("failed unmarshalling response: %v", err)
	}

	return result, nil
}
