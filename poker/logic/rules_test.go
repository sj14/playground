package logic

import (
	"reflect"
	"testing"
)

func TestFlush(t *testing.T) {
	tests := []struct {
		name       string
		cards      []Card
		isFlush    bool
		flushCards []Card
	}{
		{
			name:  "non-flush",
			cards: []Card{{Color: ColorHearts}, {Color: ColorHearts}, {Color: ColorDiamonds}, {Color: ColorHearts}, {Color: ColorCubs}, {Color: ColorSpades}, {Color: ColorHearts}},
		},
		{
			name:       "flush",
			cards:      []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 13}, {Color: ColorDiamonds}, {Color: ColorHearts, Value: 5}, {Color: ColorCubs}, {Color: ColorHearts, Value: 3}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			isFlush:    true,
			flushCards: []Card{{Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 12}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 5}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1 := flush(tt.cards)
			if got != tt.isFlush {
				t.Errorf("flush() got = %v, want %v", got, tt.isFlush)
			}
			if !reflect.DeepEqual(got1, tt.flushCards) {
				t.Errorf("flush() got1 = %v, want %v", got1, tt.flushCards)
			}
		})
	}
}

func TestStraight(t *testing.T) {
	tests := []struct {
		name          string
		cards         []Card
		isStraight    bool
		straightCards []Card
	}{
		{
			name:  "no straight",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorDiamonds}, {Color: ColorHearts, Value: 5}, {Color: ColorCubs}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
		},
		{
			name:          "straight",
			cards:         []Card{{Color: ColorHearts, Value: 10}, {Color: ColorHearts, Value: 13}, {Color: ColorDiamonds}, {Color: ColorHearts, Value: 9}, {Color: ColorCubs}, {Color: ColorHearts, Value: 3}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			isStraight:    true,
			straightCards: []Card{{Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 12}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 10}, {Color: ColorHearts, Value: 9}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1 := straight(tt.cards)
			if got != tt.isStraight {
				t.Errorf("straight() got = %v, want %v", got, tt.isStraight)
			}
			if !reflect.DeepEqual(got1, tt.straightCards) {
				t.Errorf("straight() got1 = %v, want %v", got1, tt.straightCards)
			}
		})
	}
}

func TestPairs(t *testing.T) {
	tests := []struct {
		name  string
		cards []Card
		pairs []Card
	}{
		{
			name:  "no pairs",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{},
		},
		{
			name:  "pairs",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}, {Color: ColorHearts, Value: 3}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}},
		},
		{
			name:  "pairs/remaining",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}, {Color: ColorHearts, Value: 3}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 7}},
			pairs: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPairs := pairs(tt.cards)
			// if got != tt.isPair {
			// 	t.Errorf("straight() got = %v, want %v", got, tt.isPair)
			// }
			if !reflect.DeepEqual(gotPairs, tt.pairs) {
				t.Errorf("pairs() got = %v, want %v", gotPairs, tt.pairs)
			}
		})
	}
}

func TestFourOfAkind(t *testing.T) {
	tests := []struct {
		name  string
		cards []Card
		pairs []Card
	}{
		{
			name:  "no four of a kind",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{},
		},
		{
			name:  "four of a kind",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}, {Color: ColorSpades, Value: 10}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}, {Color: ColorSpades, Value: 10}, {Color: ColorHearts, Value: 12}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPairs := xOfKind(tt.cards, 4)
			if !reflect.DeepEqual(gotPairs, tt.pairs) {
				t.Errorf("pairs() got = %v, want %v", gotPairs, tt.pairs)
			}

		})
	}
}

func TestTwoPairs(t *testing.T) {
	tests := []struct {
		name  string
		cards []Card
		pairs []Card
	}{
		{
			name:  "no two pairs",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			// pairs: []Card{},
		},
		{
			name:  "two pairs",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 9}, {Color: ColorSpades, Value: 11}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{{Color: ColorSpades, Value: 11}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorHearts, Value: 12}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPairs := twoPairs(tt.cards)
			if !reflect.DeepEqual(gotPairs, tt.pairs) {
				t.Errorf("pairs() got = %v, want %v", gotPairs, tt.pairs)
			}

		})
	}
}

func TestThreeOfAkind(t *testing.T) {
	tests := []struct {
		name  string
		cards []Card
		pairs []Card
	}{
		{
			name:  "no three of a kind",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{},
		},
		{
			name:  "three of a kind",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 9}, {Color: ColorSpades, Value: 10}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			pairs: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorSpades, Value: 10}, {Color: ColorHearts, Value: 12}, {Color: ColorHearts, Value: 11}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPairs := xOfKind(tt.cards, 3)
			if !reflect.DeepEqual(gotPairs, tt.pairs) {
				t.Errorf("pairs() got = %v, want %v", gotPairs, tt.pairs)
			}

		})
	}
}

func TestFullHouse(t *testing.T) {
	tests := []struct {
		name  string
		cards []Card
		want  []Card
	}{
		{
			name:  "no full house",
			cards: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			want:  []Card{},
		},
		{
			name:  "full house",
			cards: []Card{{Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}, {Color: ColorSpades, Value: 11}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			want:  []Card{{Color: ColorSpades, Value: 11}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 10}, {Color: ColorCubs, Value: 10}, {Color: ColorDiamonds, Value: 10}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := fullHouse(tt.cards)
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("pairs() got = %v, want %v", got, tt.want)
			}

		})
	}
}

func TestRemove(t *testing.T) {
	tests := []struct {
		name   string
		cards  []Card
		remove []Card
		result []Card
	}{
		{
			name:   "remove1",
			cards:  []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
			remove: []Card{{Color: ColorHearts, Value: 9}, {Color: ColorHearts, Value: 5}},
			result: []Card{{Color: ColorHearts, Value: 8}, {Color: ColorHearts, Value: 13}, {Color: ColorHearts, Value: 11}, {Color: ColorHearts, Value: 12}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := remove(tt.cards, tt.remove)
			if !reflect.DeepEqual(got, tt.result) {
				t.Errorf("pairs() got = %v, want %v", got, tt.result)
			}

		})
	}
}
