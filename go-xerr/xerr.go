package xerr

import (
	"fmt"
	"log"
)

func Log(err error, msgf string, args ...any) {
	if err == nil {
		return
	}
	log.Printf("%s: %v\n", fmt.Sprintf(msgf, args...), err)
}

func Fatal(err error, msgf string, args ...any) {
	if err == nil {
		return
	}
	log.Fatalf("%s: %v\n", fmt.Sprintf(msgf, args...), err)
}

func Panic(err error, msgf string, args ...any) {
	if err == nil {
		return
	}
	panic(fmt.Sprintf("%s: %v\n", fmt.Sprintf(msgf, args...), err))
}
