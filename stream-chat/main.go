package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"

	"github.com/gempir/go-twitch-irc/v2"
)

type controller struct {
	client *twitch.Client
}

func main() {
	twitchChannel := flag.String("twitch", "", "twitch channel name")

	ctrl := &controller{
		client: twitch.NewAnonymousClient(), // read only
	}

	ctrl.client.Join(*twitchChannel)

	go func() {
		log.Fatalf("twitch connection failed: %v\n", ctrl.client.Connect())
	}()

	hub := newHub()
	go hub.run()

	ctrl.client.OnPrivateMessage(func(message twitch.PrivateMessage) {
		msg := fmt.Sprintf("[T] %s: %s\n", message.User.DisplayName, message.Message)
		hub.broadcast <- []byte(msg)
	})

	http.HandleFunc("/", serveHome)
	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		serveWs(hub, w, r)
	})

	log.Println("listening...")
	err := http.ListenAndServe(":8080", nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
	}

}

func serveHome(w http.ResponseWriter, r *http.Request) {
	log.Println(r.URL)
	if r.URL.Path != "/" {
		http.Error(w, "Not found", http.StatusNotFound)
		return
	}
	if r.Method != "GET" {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	http.ServeFile(w, r, "home.html")
}

func (ctrl *controller) listMessages(w http.ResponseWriter, r *http.Request) {
	log.Println("in list messages")

}
