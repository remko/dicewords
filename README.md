# [DiceWords: Generate DiceWare word lists](https://el-tramo.be/dicewords)

## About

A small program to generate [DiceWare](http://world.std.com/~reinhold/diceware.html)
word lists.

Processes a file with a list of words, and strips out words containing non-ASCII
characters, composite words, ...
 

## Build

Run 

    cabal install

to build the `dicewords` executable and install it into `~/.cabal/bin`.


## Usage

To generate a DiceWare word list using the words in a text file, run

    dicewords path/to/words/file.txt -o dicewords.txt

To get a list of options:

    dicewords
