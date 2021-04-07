package main

import (
	"fmt"
	"math"
)

func main() {
	// https://en.wikipedia.org/wiki/Sunrise_equation#Calculate_sunrise_and_sunset

	n := julianDay(2019, 9, 9) - 2451545.0 + 0.0008
	// n := float64(jd.YMD2J(2019, 9, 11)) - float64(2451545.0) + float64(0.0008)

	lw := 6.5128741 // (longitude Alpen)
	latitude := 51.5767514

	// Mean solar noon
	j_star := n - (lw / 360)
	fmt.Printf("j_star: %v\n", j_star)

	// Solar mean anomaly
	m := math.Mod(357.5291+0.98560028*j_star, 360)
	fmt.Printf("m: %v\n", m)

	// Equation of the center
	c := 1.9148*Sin(m) + 0.0200*Sin(2*m) + 0.0003*Sin(3*m)
	fmt.Printf("c: %v\n", c)

	// Ecliptic longitude
	λ := math.Mod(m+c+180+102.9372, 360)
	fmt.Printf("λ: %v\n", λ)

	// Solar transit
	j_transit := 2451545.0 + j_star + 0.0053*Sin(m) - 0.0069*Sin(2*λ)
	fmt.Printf("j_transit: %v\n", j_transit)

	// Declination of the Sun
	sin_d := Sin(λ) * Sin(23.44)
	fmt.Printf("sin_d: %v\n", sin_d)

	// Hour angle
	cos_w := (Sin(-0.83) - (Sin(latitude) * sin_d)) / (Cos(latitude) * Cos(Asin(sin_d)))
	fmt.Printf("cos_w: %v\n", cos_w)

	wDiv360 := Acos(cos_w) / 360
	fmt.Printf("wDiv360: %v\n", wDiv360)

	rise := j_transit - wDiv360
	set := j_transit + wDiv360

	fmt.Printf("rise: %v\nset: %v\n", rise, set)
	fmt.Printf("rise: %v\nset: %v\n", gregorian(rise), gregorian(set))

}

func Sin(x float64) float64 {
	x = degreeToRad(x)
	return math.Sin(x)
}

func Asin(x float64) float64 {
	x = degreeToRad(x)
	return math.Asin(x)
}

func Cos(x float64) float64 {
	x = degreeToRad(x)
	return math.Cos(x)
}

func Acos(x float64) float64 {
	x = degreeToRad(x)
	return math.Acos(x)
}

func radToDegree(rad float64) float64 {
	return rad * 57.296
}

func degreeToRad(deg float64) float64 {
	return deg * 0.01745
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
	d := int64(365.25 * float64(c))
	e := int64(float64(b-d) / 30.6001)
	f := int64(30.6001 * float64(e))
	day := b - d - f + (int64(q) - z)
	month := e - 1
	if month > 12 {
		month = e - 13
	}

	year := c - 4716
	if month == 1 || month == 2 {
		year = c - 4715
	}

	return fmt.Sprintf("%v-%v-%v", year, month, day)
}
