# Haskell-Forth-Interpreter
A Haskell interpreter for a subset of the Forth language, written using pure recursion with no loops. Reads Forth commands from an input file and writes stack results to an output file.

---

## Features

- Supports core Forth stack operations: `+`, `-`, `*`, `/`, `DUP`, `SWAP`, `DROP`, `OVER`, `ROT`
- Supports output commands: `.` (print top of stack) and `.S` (print entire stack)
- Handles invalid input gracefully with `(illegal,illegal)` response
- Purely recursive (no mutable state or loops)
- Input and output are line-based — each input line is treated as a separate Forth program

---

## File Overview

- `main.hs` — Contains the full interpreter logic and IO code
- `input_1.txt` — Sample input file (you create this)
- `myoutput_1.txt` — Output file written by the program

---

## How to Build and Run

```bash
ghc -o project_2 main.hs
./project_2 input_1.txt myoutput_1.txt
