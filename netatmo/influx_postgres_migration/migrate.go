package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/jackc/pgx/v4/pgxpool"
	"github.com/remeh/sizedwaitgroup"
)

func main() {
	var (
		start = flag.Int("start", 0, "start")
		end   = flag.Int("end", 0, "end")
	)

	flag.Parse()

	// Read in connection string
	// scanner := bufio.NewScanner(os.Stdin)
	// log.Println("Enter a connection string: ")
	// scanner.Scan()
	connstring := os.ExpandEnv("postgresql://")

	// Connect to the "bank" database
	config, err := pgxpool.ParseConfig(connstring)
	if err != nil {
		log.Fatal("error configuring the database: ", err)
	}
	config.ConnConfig.Config.Database = "netatmo"

	conn, err := pgxpool.ConnectConfig(context.Background(), config)
	if err != nil {
		log.Fatal("error connecting to the database: ", err)
	}
	defer conn.Close()

	file, err := os.Open("data/absolutepressure_u.csv")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	log.Printf("start: %v\n", *start)
	log.Printf("end: %v\n", *end)

	swg := sizedwaitgroup.New(100)
	lineN := 0
	for scanner.Scan() {
		lineN = lineN + 1

		if *start != 0 && *start > lineN {
			continue
		}
		if *end != 0 && lineN >= *end {
			break
		}

		if lineN%1000 == 0 {
			log.Println(lineN)
		}

		row := strings.Split(scanner.Text(), ",")

		swg.Add()
		go func(conn *pgxpool.Pool) {
			defer swg.Done()
			timeInt, err := strconv.ParseInt(row[1], 10, 64)
			if err != nil {
				log.Fatalln(err)
			}
			t := time.Unix(timeInt, 0)
			if _, err := conn.Exec(context.Background(),
				fmt.Sprintf("INSERT INTO %s (time, module, value) VALUES ($1, $2, $3)", "absolutepressure"), t.UTC().Format("2006-01-02 15:04:05+07:00"), row[2], row[3]); err != nil {
			}
			if err != nil {
				log.Printf("%v | error: %v", lineN, err)
			}
		}(conn)
	}
	swg.Wait()

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
