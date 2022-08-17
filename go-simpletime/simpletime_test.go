package simpletime

import (
	"testing"
	"time"
)

func TestParseFormat(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name   string
		format string
		want   string
	}{
		{
			name:   "year-month-day long",
			format: "YYYY-MM-DD",
			want:   "2022-08-09",
		},
		{
			name:   "month/day/year short",
			format: "M/D/YY",
			want:   "8/9/22",
		},
		{
			name:   "hour:minute:second long",
			format: "hh:mm:ss",
			want:   "10:15:45",
		},
		{
			name:   "second-minute short",
			format: "s-m",
			want:   "45-15",
		},
	}

	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			goFormat := ParseFormat(tt.format)
			fixedDate := time.Date(2022, 8, 9, 10, 15, 45, 20, time.UTC)

			if got := fixedDate.Format(goFormat); got != tt.want {
				t.Errorf("ParseFormat() got %q, want %q", got, tt.want)
			}
		})
	}
}
