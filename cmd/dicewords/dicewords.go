package main

import (
	"flag"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"github.com/remko/dicewords"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	log.SetFlags(0)
	if err := doMain(); err != nil {
		log.Printf("%s", err)
		os.Exit(-1)
	}
}

func doMain() error {
	////////////////////////////////////////////////////////////////////////////////
	// Parse flags & weights
	////////////////////////////////////////////////////////////////////////////////

	formatFlag := flag.String("format", "dice", "format (one of: 'dice' (default), '8k')")
	composites := flag.Bool("composites", false, "allow composites")
	flag.Parse()

	format := dicewords.DiceFormat
	if *formatFlag == "8k" {
		format = dicewords.D8KFormat
	} else if *formatFlag != "dice" {
		return fmt.Errorf("invalid format")
	}

	weights := map[string]float64{}
	for _, arg := range flag.Args() {
		a := strings.SplitN(arg, "=", 2)
		if len(a) != 2 {
			return fmt.Errorf("Invalid weight: %s", arg)
		}
		weight, err := strconv.ParseFloat(a[1], 64)
		if err != nil {
			return err
		}
		weights[a[0]] = weight
	}

	return dicewords.Dicewords(weights, format, *composites)
}
