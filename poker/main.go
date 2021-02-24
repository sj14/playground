package main

import (
	"log"

	"github.com/sj14/playground/poker/logic"
)

func main() {
	deck := logic.GetDeck()

	for i, card := range deck {
		log.Printf("%v: color: %v rank: %v\n", i, card.Color, card.Value)
	}
}
