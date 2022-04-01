package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	input, err := readInput(true)
	if err != nil {
		log.Fatalln(err)
	}

	fmt.Printf("> %s\n", input)
}

// read program input from stdin or argument
func readInput(waitForStdin bool) (string, error) {
	flag.Parse()

	// from stdin/pipe
	if flag.NArg() == 0 {

		// check if it's piped or from empty stdin
		// https://stackoverflow.com/a/26567513
		if !waitForStdin {
			stat, err := os.Stdin.Stat()
			if err != nil {
				return "", fmt.Errorf("failed to get stdin stats: %v", err)
			}
			if (stat.Mode() & os.ModeCharDevice) != 0 {
				return "", nil // empty stdin
			}
		}

		// read the input from the pipe
		reader := bufio.NewReader(os.Stdin)
		input, err := reader.ReadString('\n')
		if err != nil {
			return "", fmt.Errorf("failed to read input: %v", err)
		}
		return strings.TrimSpace(input), nil
	}

	// from argument
	if flag.NArg() > 1 {
		return "", fmt.Errorf("takes at most one input")
	}

	return flag.Arg(0), nil
}
