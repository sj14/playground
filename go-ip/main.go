package main

import (
	"io"
	"log"
	"net"
	"net/http"
)

func main() {
	var srv ipServer

	go func() {
		listenIPv4, err := net.Listen("tcp4", ":8084")
		if err != nil {
			log.Fatalf("failed to listen IPv4 server: %v\n", err)
		}

		log.Println("listen IPv4")
		err = http.Serve(listenIPv4, srv)
		if err != nil {
			log.Fatalf("failed to serve IPv4 server: %v\n", err)
		}
	}()

	go func() {
		listenIPv6, err := net.Listen("tcp6", ":8086")
		if err != nil {
			log.Fatalf("failed to listen IPv6 server: %v\n", err)
		}

		log.Println("listen IPv6")
		err = http.Serve(listenIPv6, srv)
		if err != nil {
			log.Fatalf("failed to serve IPv6 server: %v\n", err)
		}
	}()

	log.Println("listen IPv4/IPv6")
	http.Handle("/", srv)
	if err := http.ListenAndServe(":8080", nil); err != nil {
		log.Fatalf("failed to start server: %v\n", err)
	}
}

type ipServer struct{}

func (s ipServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	host, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		io.WriteString(w, err.Error())
	}
	io.WriteString(w, host)
}
