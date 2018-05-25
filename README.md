# Mastermind
An expert Mastermind player in Lisp that plays in tournaments. The system chooses a "secret code" and the program tries to guess that code. 

## Environment
The standard 4-6 version of the game uses a code with 4 pegs, each of which is one of 6 different colors (denoted here by letters). In a single game, player will get up to 100 guesses and 5 seconds to determine the secret code, whichever comes first. The system returns your guess with a scoring list: the number of exact pegs (correct color in the correct position) and the number of “almost” pegs (correct color in the wrong position). 

## Evaluation
Program will play a set of tournaments. Each tournament is a set of 100 rounds that pits the program against the same Secret-Code Selection Algorithm (henceforward, SCSA) for some fixed number of pegs and colors.
If the program wins a round by guessing the code precisely, you get 5 points. If the program makes an illegal guess (wrong length or illegal colors), the round ends and you lose 2 points. Winning in fewer guesses is better.

## Guessing strategies
Included are 3 baseline strategies that may not scale:

Baseline: #1
Exhaustively enumerate all possibilities. Guess each possibility in lexicographic order one at a time, paying no attention to the system’s responses. For example, if pegs = 4 and colors = 3, guess (A A A A), (A A A B), (A A A C), (A A B A), (A A B B), (A A B C), and so on. This method will take at most colors^(pegs) guesses.

Baseline: #2
Exhaustively enumerate all possibilities. Guess each possibility in lexicographic order unless it does not match some previous response. For example, for pegs = 4, if guess (A A A A) got (0,0) then you would never again on that round make any guess with an A in it.

Baseline: #3
Make your first (colors – 1) guesses monochromatic: "all A’s," "all B’s,"... for all but one of the colors. That will tell you how many pegs of each color are in the answer. Then you generate and test only answers consistent with that known color distribution.

SARSRAO: #4
Baseline #3 solution plus implementation of Rao's algorighm by T. Mahadeva Rao. 

## How to use the code
Define a game:
To set up a game, run the function Mastermind with the number of pegs, number of colors, and SCSA of your choice. 

For example: 
```
(Mastermind 7 5 'two-color-alternating)
``` 
builds a global variable *Mastermind* that describes a 7-peg, 5-color version that alternates exactly 2 colors.

Play round:
```
(play-round *Mastermind* 'SARS)
```
where SARS is the name of the tournament player.

Play tournament:
```
(play-tournament *Mastermind* 'SARS 'two-color-alternating 25)
```
