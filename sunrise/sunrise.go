package main

import (
	"fmt"
	"math"
)

func main() {
	// https://en.wikipedia.org/wiki/Sunrise_equation#Calculate_sunrise_and_sunset

	fmt.Printf("julian day: %v\n", julianDay(2019, 9, 11))

	n := julianDay(2019, 9, 9) - 2451545.0 + 0.0008
	// n := float64(jd.YMD2J(2019, 9, 11)) - float64(2451545.0) + float64(0.0008)

	lw := -6.5128741 // (longitude Alpen)
	latitude := 51.5767514

	// Mean solar noon
	j_star := n - (lw / 360)

	// Solar mean anomaly
	m := float64(int64(357.5291+0.98560028*j_star) % 360)

	// Equation of the center
	c := 1.9148*math.Sin(m) + 0.0200*math.Sin(2*m) + 0.0003*math.Sin(3*m)

	// Ecliptic longitude
	λ := float64(int64(m+c+180+102.9372) % 360)

	// Solar transit
	j_transit := 2451545.0 + j_star + 0.0053*math.Sin(m) - 0.0069*math.Sin(2*λ)

	// Declination of the Sun
	sin_d := math.Sin(λ) * math.Sin(23.44)

	// Hour angle
	cos_w := (math.Sin(-0.83) - math.Sin(latitude)*sin_d) / (math.Cos(latitude) * math.Cos(math.Asin(sin_d)))

	rise := j_transit - (math.Acos(cos_w) / 360)
	set := j_transit + (math.Acos(cos_w) / 360)

	fmt.Printf("rise: %v\nset: %v\n", (rise), (set))
}

func julianDay(year, month, day int64) float64 {
	if month == 1 || month == 2 {
		year--
		month += 12
	}

	// https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html
	//
	// A = Y/100
	// B = A/4
	// C = 2-A+B
	// E = 365.25x(Y+4716)
	// F = 30.6001x(M+1)
	// JD= C+D+E+F-1524.5

	a := int64(year / 100)
	b := int64(a / 4)
	c := float64(2 - a + b)
	e := float64(int64(365.25 * float64(year+4716)))
	f := float64(int64(30.6001 * float64(month+1)))
	jd := c + float64(day) + e + f - 1524.5
	return jd
}

func gregorian(jd float64) string {
	q := jd + 0.5
	z := int64(q)
	w := int64((float64(z) - 1867216.25) / 36524.25)
	x := int64(w / 4)
	a := z + 1 + w - x
	b := a + 1524
	c := int64((float64(b) - 122.1) / 365.25)
	d := int64(365.25 * float64(x))
	e := int64(float64(b-d) / 30.6001)
	f := int64(30.6001 * float64(e))
	day := b - d - f + (int64(q) - z) // TODO: check the rounding of q, doesnt make sense
	month := e - 1
	if month > 12 {
		month = e - 13
	}

	year := c - 4716
	if month == 1 || month == 2 {
		year = c - 4715
	}

	return fmt.Sprintf("%v-%v-%v\n", year, month, day)
}
