package main

import (
	"testing"
)

func TestJulianDay(t *testing.T) {
	type args struct {
		year  int64
		month int64
		day   int64
	}
	tests := []struct {
		name string
		args args
		want float64
	}{
		{
			name: "1",
			args: args{
				year:  1582,
				month: 10,
				day:   15,
			},
			want: 2299160.5,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := julianDay(tt.args.year, tt.args.month, tt.args.day); got != tt.want {
				t.Errorf("julianDay() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRadToDegree(t *testing.T) {
	tests := []struct {
		name string
		rad  float64
		want float64
	}{
		{
			name: "1",
			rad:  0.5236,
			want: 30.0001856,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := radToDegree(tt.rad); got != tt.want {
				t.Errorf("radToDegree() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_degreeToRad(t *testing.T) {
	tests := []struct {
		name string
		deg  float64
		want float64
	}{
		{
			name: "1",
			deg:  30,
			want: 0.5235,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := degreeToRad(tt.deg); got != tt.want {
				t.Errorf("degreeToRad() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_gregorian(t *testing.T) {
	tests := []struct {
		name string
		jd   float64
		want string
	}{
		{
			name: "1",
			jd:   2299160.5,
			want: "1582-10-15",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := gregorian(tt.jd); got != tt.want {
				t.Errorf("gregorian() = %v, want %v", got, tt.want)
			}
		})
	}
}
