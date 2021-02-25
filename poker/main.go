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

	p1 := logic.Player{Name: "Jim", Stack: 100}
	p2 := logic.Player{Name: "Tom", Stack: 100}

	players := []logic.Player{p1, p2}
	round := logic.NewRound(players)
	round.Play()
}
