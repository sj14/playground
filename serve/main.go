package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
)

func main() {
	var (
		addr = flag.String("addr", "0.0.0.0:6789", "listening address")
		dir  = flag.String("dir", ".", "the folder to serve")
		link = flag.String("link", "123456789", "the access link")
	)
	flag.Parse()

	*link = fmt.Sprintf("/%s/", *link)

	fs := http.FileServer(http.Dir(*dir))
	http.Handle(*link, http.StripPrefix(*link, fs))

	log.Printf("serving %s on %s%s", *dir, *addr, *link)
	log.Fatalln(http.ListenAndServe(*addr, nil))
}
