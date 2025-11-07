# 🎾 Elm Tennis 2026

Elm Tennis 2026 is a small Elm application that models and displays a single tennis match between two players. It focuses on accurate tennis scoring logic — games, sets, tiebreaks, and match completion — using Elm's strong type system and pure functions.

---

## ✅ Features

- **Two-player tennis match**
- **Accurate scoring logic**:
  - Love → 15 → 30 → 40 → Deuce → Advantage → Game
  - Win a set by 2 games (e.g. 6–4) or 7–5
  - Tiebreak at 6–6 (first to 7, win by 2)
- **Match results**:
  - Best-of-3 or Best-of-5 sets (configured at match start)
  - Automatically detects winner
- **UI elements**:
  - Shows set scores and current game score
  - Shows “Ad” and “40” correctly during advantage
  - Highlights set winners in the scoreboard
  - “Player 1 Scores”, “Player 2 Scores” buttons
  - “New Match” button when finished
- **Pure Elm — no ports, no JavaScript interop**

---

## 🗂 Project Structure

```
src/
  Main.elm        -- Model, update logic, and view
```

The entire application currently exists in a single file. View refactoring and module separation are planned for the future.

---

## ▶ How to Run

### Option 1: elm reactor (quickest)

```bash
elm reactor
```

Then open `http://localhost:8000/` and click `src/Main.elm`.

### Option 2: Compile to JS

```bash
elm make src/Main.elm --output=elm.js
open index.html   # or serve it statically / deploy
```
Then open `index.html` in your browser.

---

## 🎮 How Scoring Works

| Level   | Rules                                                                 |
|---------|-----------------------------------------------------------------------|
| Game    | Love → 15 → 30 → 40 → Deuce → Ad → Game                               |
| Set     | First to 6, must win by 2 (6–4, 7–5) or tiebreak at 6–6               |
| Match   | Best-of-3 sets (first to 2 sets wins the match)                      |
| Tiebreak| First to 7, must win by 2 (score shown like `7–6 (7–4)` internally)   |

---

## 🛠 Future Improvements

- Split logic and view into separate modules (e.g. `View/Scoreboard.elm`)
- Expose Best-of-3 or Best-of-5 configuration in the UI (currently only internal code setting)
- Save match progress to local storage
- Add styling or animations
- Unit tests for scoring functions

---

## 🧠 Purpose

This project is both a working tennis scorer and a learning exercise in Elm — emphasizing readability, pure functions, and refactoring over time.

---

## 📄 License

MIT License

Copyright (c) 2026 Bradley Mehder

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
