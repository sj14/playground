package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/sj14/playground/go-crdt-db/crdt"
)

var counter = crdt.NewGCounter()

func inc(w http.ResponseWriter, r *http.Request) {
	counter.Inc()
	count(w, r)
}

func count(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(strconv.Itoa(counter.Count())))
}

func current(w http.ResponseWriter, r *http.Request) {
	b, err := json.Marshal(counter)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Write(b)
}

func merge() error {
	resp, err := http.Get(fmt.Sprintf("http://localhost:%s/current", otherAddr))
	if err != nil {
		return err
	}

	otherBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}

	var other = crdt.NewGCounter()

	err = json.Unmarshal(otherBody, other)
	if err != nil {
		return err
	}

	counter.Merge(other)
	return nil
}

func mergeHandler(w http.ResponseWriter, r *http.Request) {
	merge()
	current(w, r)
}

var otherAddr string

func main() {
	addr := flag.String("addr", ":8080", "")
	flag.StringVar(&otherAddr, "other", "8081", "")
	flag.Parse()

	http.HandleFunc("/inc", inc)
	http.HandleFunc("/count", count)
	http.HandleFunc("/current", current)
	http.HandleFunc("/merge", mergeHandler)

	go func() {
		for {
			time.Sleep(5 * time.Second)
			err := merge()
			if err != nil {
				log.Println(err)
			}
		}
	}()

	log.Fatal(http.ListenAndServe(*addr, nil))

	// log.Println(gcounter1.Contains("dummy-object"))
}
