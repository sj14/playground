package main

import (
	"bytes"
	"encoding/xml"
	"log/slog"
	"os"
	"os/exec"
	"time"

	"github.com/sj14/playground/nmap-exporter/postgres"
)

func main() {
	time.Sleep(10 * time.Second) // pod networking needs a bit?

	dbURL, ok := os.LookupEnv("DB_URL")
	if !ok {
		slog.Error("DB_URL not set")
		os.Exit(1)
	}

	subnet := envOrDefault("SCAN_SUBNET", "192.168.0.0/24")

	interval, err := time.ParseDuration(envOrDefault("SCAN_INTERVAL", "5m"))
	if err != nil {
		slog.Error("invalid SCAN_INTERVAL", "error", err)
		os.Exit(1)
	}

	db, err := postgres.NewPostgresStore(dbURL)
	if err != nil {
		slog.Error("failed setting up db", "error", err)
		os.Exit(1)
	}
	defer func() {
		if err := db.CloseConn(); err != nil {
			slog.Error("failed closing db connection", "error", err)
		}
	}()

	var lastRun time.Time
	for {
		time.Sleep(time.Until(lastRun.Add(interval)))
		lastRun = time.Now()

		if err := scan(db, subnet); err != nil {
			slog.Error("scan failed", "error", err)
		}
	}
}

func envOrDefault(key, fallback string) string {
	if value, ok := os.LookupEnv(key); ok {
		return value
	}
	return fallback
}

func scan(db *postgres.PostgresStore, subnet string) error {
	slog.Info("start scanning", "subnet", subnet)

	cmd := exec.Command(
		"nmap",
		"--host-timeout",
		"10s",
		"-sn",
		subnet,
		"-oX",
		"-",
		"--webxml",
	)
	cmd.Stderr = os.Stderr

	out := &bytes.Buffer{}
	cmd.Stdout = out

	if err := cmd.Run(); err != nil {
		return err
	}

	var result Nmaprun
	if err := xml.NewDecoder(out).Decode(&result); err != nil {
		return err
	}

	slog.Info("scan finished", "hosts", len(result.Hosts), "took", result.Runstats.Finished.Elapsed)

	now := time.Now()
	for _, host := range result.Hosts {
		slog.Info("host up", "ip", host.Address.Addr)
		if err := db.StoreHost(host.Address.Addr, now); err != nil {
			slog.Error("failed storing host", "ip", host.Address.Addr, "error", err)
		}
	}
	return nil
}

// Nmaprun contains the parts of the nmap XML output we use.
type Nmaprun struct {
	Hosts []struct {
		Address struct {
			Addr string `xml:"addr,attr"`
		} `xml:"address"`
	} `xml:"host"`
	Runstats struct {
		Finished struct {
			Elapsed string `xml:"elapsed,attr"`
		} `xml:"finished"`
	} `xml:"runstats"`
}
