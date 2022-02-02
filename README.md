# wordle

TUI version of the Wordle word puzzle game written in Haskell

<html>
<head>
  <link rel="stylesheet" type="text/css" href="asciinema-player.css" />
</head>
<body>
  <div id="player"></div>
  <script src="asciinema-player.min.js"></script>
  <script>
    AsciinemaPlayer.create(
      '466033.cast',
      document.getElementById('player'),
      { cols: 96, rows: 31 }
    );
  </script>
</body>
</html>

# Install

## From source

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

```
git clone https://github.com/ivanjermakov/wordle.git
cd wordle
stack install
```

## From binary

TBD

# Credit

- Original game by Josh Wardle: [Wordle](https://www.powerlanguage.co.uk/wordle/)
- 84k words english dictionary: http://www.gwicks.net/dictionaries.htm
- 10k words english dictionary: https://www.mit.edu/~ecprice/wordlist.10000
