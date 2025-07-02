<p align="center">
  <img src="imgs/typing-mode.png" width="600"/>
</p>

# Typpy

Terminal-based touch typing trainer with real-time feedback,
error tracking, and WPM stats — built in OCaml using Notty.

# Features

- Live typing interface
- Tracks common mistakes 
- WPM counter
- Execution time
- Summary table of top *k* most frequent mistakes
- Designed for minimal distraction and fast feedback

<p align="center">
  <img src="imgs/summary-mode.png" width="600"/>
</p>

# Installation (Linux)
### Opam dependencies
```
opam install dune base notty
```

```
git clone https://github.com/Ojkee/Typpy.git
cd Typpy
dune build
dune exec ./main.exe
```

# TODO
- [ ] Selection menu
- [ ] Refactor every window state to modules
- [ ] Save progression
- [ ] Generating words with common letter mistakes
- [ ] Per-character timing for advanced stats
