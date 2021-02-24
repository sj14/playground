package logic

import (
	"math/rand"
	"sort"
	"time"
)

type Card struct {
	Color
	Value byte
}

type Color byte

const (
	ColorCubs Color = iota
	ColorSpades
	ColorHearts
	ColorDiamonds
)

func GetDeck() []Card {
	var deck []Card

	for colorIdx := 0; colorIdx < 4; colorIdx++ {
		for rankIdx := byte(0); rankIdx < 13; rankIdx++ {
			deck = append(deck, Card{Color: Color(colorIdx), Value: rankIdx})
		}
	}

	return deck
}

func Shuffle(cards []Card) {
	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(cards), func(i, j int) { cards[i], cards[j] = cards[j], cards[i] })
}

type Rank byte

const (
	RankHighCard Rank = iota
	RankOnePair
	RankTwoPair
	RankThreeOfKind
	RankStraight
	RankFlush
	RankFullHouse
	RankFourOfKind
	RankStraightFlush
	RankRoyalFlush
)

type Winner byte

const (
	WinnerA Winner = iota
	WinnerB
	WinnerBoth
)

// cards is up to all 7 cards!
func Wins(cardsA, cardsB []Card) Winner {
	rankA, cardsA := GetRank(cardsA)
	rankB, cardsB := GetRank(cardsB)

	if rankA > rankB {
		return WinnerA
	}
	if rankB > rankA {
		return WinnerB
	}

	sortValueHighToLow(cardsA)
	sortValueHighToLow(cardsB)

	for i := range cardsA {
		if cardsA[i].Value > cardsB[i].Value {
			return WinnerA
		}
		if cardsA[i].Value < cardsB[i].Value {
			return WinnerB
		}
	}

	return WinnerBoth
}

// cards is up to all 7 cards!
func GetRank(cards []Card) (Rank, []Card) {
	sortValueHighToLow(cards)

	isFlush, flushCards := flush(cards)
	if isFlush {
		if isStraight, straightCards := straight(flushCards); isStraight {
			if !same(flushCards, straightCards) {
				panic("straigflush: straight cards not same as flush cards")
			}
			if flushCards[0].Value == 13 {
				return RankRoyalFlush, flushCards // #1
			}
			return RankStraightFlush, flushCards // #2
		}
	}

	// four of a kind

	// fullhouse

	if isFlush {
		return RankFlush, flushCards
	}

	if isStraight, straightCards := straight(flushCards); isStraight {
		return RankStraight, straightCards
	}

	// three of a kind

	// two pair

	// pair

	return RankHighCard, cards[0:1]
}

// ignore order
func same(cardsA, cardsB []Card) bool {
	if len(cardsA) != len(cardsB) {
		return false
	}

	sortColor(cardsA)
	sortColor(cardsB)
	sortValueHighToLow(cardsA)
	sortValueHighToLow(cardsB)

	for i := range cardsA {
		if cardsA[i].Color != cardsB[i].Color {
			return false
		}

		if cardsA[i].Value != cardsB[i].Value {
			return false
		}
	}

	return true
}

func sortColor(cards []Card) {
	sort.SliceStable(cards, func(i, j int) bool {
		return cards[i].Color < cards[j].Color
	})
}

// highest cards at the beginning (DESC)
func sortValueHighToLow(cards []Card) {
	sort.SliceStable(cards, func(i, j int) bool {
		return cards[i].Value > cards[j].Value
	})
}

// lowest cards at the beginning (ASC)
func sortValueLowToHigh(cards []Card) {
	sort.SliceStable(cards, func(i, j int) bool {
		return cards[i].Value < cards[j].Value
	})
}

// func high(cards []Card) []Card {
// 	sortValue(cards)
// 	return cards[:5]
// }

func straight(cards []Card) (bool, []Card) {
	sortValueHighToLow(cards)

	var result []Card
	for i, c := range cards {
		if i == 0 {
			result = append(result, c)
			continue
		}
		if c.Value == cards[i-1].Value-1 {
			result = append(result, c)
		}
		if len(result) == 5 {
			return true, result
		}
	}
	return false, nil
}

func flush(cards []Card) (bool, []Card) {
	for _, color := range []Color{ColorCubs, ColorDiamonds, ColorHearts, ColorSpades} {

		flushCards := getColor(color, cards)
		if len(flushCards) >= 5 {
			sortValueHighToLow(flushCards)
			return true, flushCards[:5] // 5 highest from the flush
		}
	}

	return false, nil
}

// returns the pairs and the remaining cards which could hold another pairs!
func pairs(cards []Card) ([]Card, []Card) {
	sortValueHighToLow(cards)
	var pairs []Card
	for i := range cards {
		if i == 0 {
			pairs = append(pairs, cards[i])
			continue
		}

		if cards[i].Value == pairs[0].Value {
			pairs = append(pairs, cards[i])
			continue
		}

		// no pair with the current card

		if len(pairs) >= 2 {
			// we had a pair before, return it and the remaining cards start with this one
			// (we could return nil remaining cards when i is the last remaining card...)
			return pairs, cards[i:]
		}

		// we didn't have a pair before. reset
		pairs = nil
		pairs = append(pairs, cards[i])
	}

	// loop finished, were the last cards a pair or not? (we don't have remaining cards)
	if len(pairs) < 2 {
		return []Card{}, nil
	}

	return pairs, nil
}

func getColor(color Color, cards []Card) []Card {
	var result []Card

	for _, c := range cards {
		if c.Color == color {
			result = append(result, c)
		}
	}
	return result
}
