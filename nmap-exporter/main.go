package main

import (
	"bytes"
	"encoding/xml"
	"fmt"
	"log"
	"os"
	"os/exec"
	"time"

	"github.com/sj14/playground/nmap-exporter/postgres"
	"golang.org/x/exp/slices"
)

func main() {
	time.Sleep(10 * time.Second) // pod networking needs a bit?

	dbURL, ok := os.LookupEnv("DB_URL")
	if !ok {
		log.Fatalln("DB_URL not set")
	}

	db := postgres.NewPostgresStore(dbURL)
	defer func() {
		if err := db.CloseConn(); err != nil {
			log.Printf("failed closing db connection: %v\n", err)
		}
	}()

	var (
		lastRun time.Time
		// lastInsert time.Time
		// lastResult Nmaprun
	)

	for {
		diff := time.Until(lastRun.Add(5 * time.Minute)) // wait 5 minutes since last run
		time.Sleep(diff)
		lastRun = time.Now()
		log.Println("start scanning")

		cmd := exec.Command("nmap", "-sn", "192.168.0.0/24", "-oX", "-", "--webxml")

		// Set the correct output device.
		cmd.Stderr = os.Stderr

		out := &bytes.Buffer{}
		cmd.Stdout = out

		// Execute the command and return the error.
		if err := cmd.Run(); err != nil {
			log.Println(err)
			continue
		}

		var nmapResult Nmaprun

		// log.Println(out.String())

		dec := xml.NewDecoder(out)
		// dec.Strict = false
		if err := dec.Decode(&nmapResult); err != nil {
			time.Sleep(1 * time.Minute)
			continue
		}

		log.Printf("hosts: %v\n", len(nmapResult.Hosts))
		log.Printf("took: %v\n", nmapResult.Runstats.Finished.Elapsed)

		// Skip writing to the database when nothing changed
		// but write at least once an hour.
		// if sameAddresses(lastResult.Hosts, nmapResult.Hosts) && lastInsert.Add(5*time.Minute).After(time.Now()) {
		// 	lastResult = nmapResult
		// 	log.Printf("no change")
		// 	continue
		// }

		now := time.Now()
		for _, host := range nmapResult.Hosts {
			fmt.Println(host.Address.Addr)
			err := db.StoreHost(host.Address.Addr, now)
			if err != nil {
				log.Println(err)
			}
		}
		// lastInsert = now
		// lastResult = nmapResult
	}
}

func sameAddresses(a, b Hosts) bool {
	if len(a) != len(b) {
		return false
	}

	var (
		addressesA []string
		addressesB []string
	)
	for _, hostsA := range a {
		addressesA = append(addressesA, hostsA.Address.Addr)
	}

	for _, hostsB := range b {
		addressesB = append(addressesB, hostsB.Address.Addr)
	}

	slices.Sort(addressesA)
	slices.Sort(addressesB)

	return slices.Compare(addressesA, addressesB) == 0
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
	Hosts    `xml:"host"`
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

type Hosts []struct {
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
}
