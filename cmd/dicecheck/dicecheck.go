package main

import (
	"github.com/remko/dicewords"
	"log"
	"os"
)

func main() {
	log.SetFlags(0)
	if err := dicewords.Dicecheck(os.Args[1]); err != nil {
		log.Printf("%s", err)
		os.Exit(-1)
	}
}
