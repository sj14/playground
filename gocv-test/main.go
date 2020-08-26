package main

import (
	"fmt"
	"os"

	"gocv.io/x/gocv"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("How to run:\n\tcapwindow [camera ID]")
		return
	}

	// parse args
	deviceID := os.Args[1]

	webcam, err := gocv.OpenVideoCapture(deviceID)
	if err != nil {
		fmt.Printf("Error opening video capture device: %v\n", deviceID)
		return
	}
	defer webcam.Close()

	window := gocv.NewWindow("Capture Window")
	defer window.Close()

	raw := gocv.NewMat()
	defer raw.Close()

	mod := gocv.NewMat()
	defer mod.Close()

	fin := gocv.NewMat()
	defer fin.Close()

	// sub := gocv.NewBackgroundSubtractorMOG2()
	// sub := gocv.NewBackgroundSubtractorMOG2WithParams(100, 3, false)
	// sub := gocv.NewBackgroundSubtractorKNN()
	sub := gocv.NewBackgroundSubtractorKNNWithParams(400, 10, false)
	defer sub.Close()

	fmt.Printf("Start reading device: %v\n", deviceID)
	for {
		if ok := webcam.Read(&raw); !ok {
			fmt.Printf("Device closed: %v\n", deviceID)
			return
		}
		if raw.Empty() {
			continue
		}

		sub.Apply(raw, &mod)
		// mod.CopyTo(&fin)

		// gocv.Subtract(mod, raw, &fin)
		// raw.CopyToWithMask(&fin, mod)
		// mod.CopyToWithMask(&fin, raw)

		gocv.CvtColor(mod, &mod, gocv.ColorGrayToBGR)
		gocv.BitwiseAnd(raw, mod, &fin)
		// gocv.BitwiseNot(mod, &fin)

		// gocv.BitwiseAndWithMask(raw, mod, &fin)

		window.IMShow(fin)
		if window.WaitKey(1) == 27 {
			break
		}
	}
}
