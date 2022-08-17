package main

import (
	"bytes"
	"encoding/xml"
	"log"
	"os"
	"os/exec"
	"sync"

	"github.com/prometheus/client_golang/prometheus"
)

func main() {
	cmd := exec.Command("/usr/local/bin/nmap", "-sn", "192.168.0.0/24", "-oX", "-", "--webxml")

	// Set the correct output device.
	cmd.Stderr = os.Stderr

	out := &bytes.Buffer{}
	cmd.Stdout = out

	var exporter Exporter

	// Execute the command and return the error.
	if err := cmd.Run(); err != nil {
		log.Println(err)
		exporter.failures.Inc()
	}

	var result Nmaprun

	log.Println(out.String())

	dec := xml.NewDecoder(out)
	// dec.Strict = false
	if err := dec.Decode(&result); err != nil {
		log.Println(err)
	}

	log.Println()
	log.Printf("%+v\n", result)

	for _, host := range result.Host {
		log.Println(host.Address.Addr)
	}
	log.Println(result.Runstats.Finished.Elapsed)
}

type Exporter struct {
	mutex    sync.RWMutex
	up       prometheus.Gauge
	runs     prometheus.Counter
	failures prometheus.Counter
}

// Collect fetches the stats from configured HAProxy location and delivers them
// as Prometheus metrics. It implements prometheus.Collector.
func (e *Exporter) Collect(ch chan<- prometheus.Metric) {
	e.mutex.Lock() // To protect metrics from concurrent collects.
	defer e.mutex.Unlock()

	// up := e.scrape(ch)

	// ch <- prometheus.MustNewConstMetric(haproxyUp, prometheus.GaugeValue, up)
	// ch <- e.totalScrapes
	// ch <- e.csvParseFailures
}

type Nmaprun struct {
	XMLName          xml.Name `xml:"nmaprun"`
	Text             string   `xml:",chardata"`
	Scanner          string   `xml:"scanner,attr"`
	Args             string   `xml:"args,attr"`
	Start            string   `xml:"start,attr"`
	Startstr         string   `xml:"startstr,attr"`
	Version          string   `xml:"version,attr"`
	Xmloutputversion string   `xml:"xmloutputversion,attr"`
	Scaninfo         struct {
		Text        string `xml:",chardata"`
		Type        string `xml:"type,attr"`
		Protocol    string `xml:"protocol,attr"`
		Numservices string `xml:"numservices,attr"`
		Services    string `xml:"services,attr"`
	} `xml:"scaninfo"`
	Verbose struct {
		Text  string `xml:",chardata"`
		Level string `xml:"level,attr"`
	} `xml:"verbose"`
	Debugging struct {
		Text  string `xml:",chardata"`
		Level string `xml:"level,attr"`
	} `xml:"debugging"`
	Hosthint []struct {
		Text   string `xml:",chardata"`
		Status struct {
			Text      string `xml:",chardata"`
			State     string `xml:"state,attr"`
			Reason    string `xml:"reason,attr"`
			ReasonTtl string `xml:"reason_ttl,attr"`
		} `xml:"status"`
		Address struct {
			Text     string `xml:",chardata"`
			Addr     string `xml:"addr,attr"`
			Addrtype string `xml:"addrtype,attr"`
		} `xml:"address"`
		Hostnames string `xml:"hostnames"`
	} `xml:"hosthint"`
	Host []struct {
		Text      string `xml:",chardata"`
		Starttime string `xml:"starttime,attr"`
		Endtime   string `xml:"endtime,attr"`
		Status    struct {
			Text      string `xml:",chardata"`
			State     string `xml:"state,attr"`
			Reason    string `xml:"reason,attr"`
			ReasonTtl string `xml:"reason_ttl,attr"`
		} `xml:"status"`
		Address struct {
			Text     string `xml:",chardata"`
			Addr     string `xml:"addr,attr"`
			Addrtype string `xml:"addrtype,attr"`
		} `xml:"address"`
		Hostnames string `xml:"hostnames"`
		Ports     struct {
			Text       string `xml:",chardata"`
			Extraports struct {
				Text         string `xml:",chardata"`
				State        string `xml:"state,attr"`
				Count        string `xml:"count,attr"`
				Extrareasons struct {
					Text   string `xml:",chardata"`
					Reason string `xml:"reason,attr"`
					Count  string `xml:"count,attr"`
				} `xml:"extrareasons"`
			} `xml:"extraports"`
			Port []struct {
				Text     string `xml:",chardata"`
				Protocol string `xml:"protocol,attr"`
				Portid   string `xml:"portid,attr"`
				State    struct {
					Text      string `xml:",chardata"`
					State     string `xml:"state,attr"`
					Reason    string `xml:"reason,attr"`
					ReasonTtl string `xml:"reason_ttl,attr"`
				} `xml:"state"`
				Service struct {
					Text   string `xml:",chardata"`
					Name   string `xml:"name,attr"`
					Method string `xml:"method,attr"`
					Conf   string `xml:"conf,attr"`
				} `xml:"service"`
			} `xml:"port"`
		} `xml:"ports"`
		Times struct {
			Text   string `xml:",chardata"`
			Srtt   string `xml:"srtt,attr"`
			Rttvar string `xml:"rttvar,attr"`
			To     string `xml:"to,attr"`
		} `xml:"times"`
	} `xml:"host"`
	Runstats struct {
		Text     string `xml:",chardata"`
		Finished struct {
			Text    string `xml:",chardata"`
			Time    string `xml:"time,attr"`
			Timestr string `xml:"timestr,attr"`
			Summary string `xml:"summary,attr"`
			Elapsed string `xml:"elapsed,attr"`
			Exit    string `xml:"exit,attr"`
		} `xml:"finished"`
		Hosts struct {
			Text  string `xml:",chardata"`
			Up    string `xml:"up,attr"`
			Down  string `xml:"down,attr"`
			Total string `xml:"total,attr"`
		} `xml:"hosts"`
	} `xml:"runstats"`
}
