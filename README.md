# wordle

TUI version of the [Wordle](https://www.powerlanguage.co.uk/wordle/) word puzzle game written in Haskell

![wordle](https://user-images.githubusercontent.com/26609879/152110207-aeae6bc7-4770-4ee5-beb0-b325e370394e.gif)

## Features
- [x] Daily game mode (original Wordle words)
- [x] Infinite game mode
- [x] Custom word length
- [x] Custom max attempts
- [x] Custom target and guess dictionaries
- [x] Result sharing (emoji + ascii)
- [ ] Hard mode

## Install

### From source

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

```
git clone https://github.com/ivanjermakov/wordle.git
cd wordle
stack install
```

### From binary

Download the latest binary from the [Releases page](https://github.com/ivanjermakov/wordle/releases)

```
wget https://github.com/ivanjermakov/wordle/releases/latest/download/wordle
chmod +x wordle
sudo mv wordle /usr/bin/
```

## Usage

```
Usage:
  wordle [-idstgalh]

Options:
  -i, --infinite              Infinite game mode (Default)
  -d, --daily                 Daily game mode
  -s, --ascii                 Print results in ASCII
  -t, --target-dict=<file>    Path to the target word dictionary
  -g, --guess-dict=<file>     Path to the guess word dictionary
  -a, --attempts=<number>     Number of attempts allowed
  -l, --word-length=<number>  Target word length
  -h, --help                  Show this message and exit
```

## Credit

- Original game by Josh Wardle: [Wordle](https://www.powerlanguage.co.uk/wordle/)
- 84k words english dictionary: http://www.gwicks.net/dictionaries.htm
- 10k words english dictionary: https://www.mit.edu/~ecprice/wordlist.10000
