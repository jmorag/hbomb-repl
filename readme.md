# A repl for Keep Talking and Nobody Explodes

Input:

### Simple Wires

wires r/b/k/w/y

### Button

button [abort/a/detonate/d/hold/h] [blue/b/white/w/yellow/y/red/r]

### Who's on First

wof1 word (or blank)

wof2 word

### Memory

mem 1
What is the label?

### Morse Code

Unordered Morse

Letters may appear in any order in the word.

`morse .- .-..`

returns `[("halls",3.515),("leaks",3.542)]`

Ordered Morse

Letters must appear in sequential order in the word. For example, entering letters s, t, k:

`omorse ... - -.-`

returns `[("steak", 3.582)]`

### Complicated Wires

comp [red/r/blue/b/star/s/led/l] [...]

### Mazes

maze 1123 4466

### Passwords

pass hcw _ _ _ edlkjh

### Knobs

knobs 101100


Modules:
- [x] simple wires
- [x] button
- [] keypads
- [] simon says
- [x] who's on first
- [x] memory
- [x] morse code
- [x] complicated wires
- [] wire sequence
- [x] mazes
- [x] passwords?
- [x] knobs
