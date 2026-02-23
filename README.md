# 🎾 Elm Tennis 2026

A clean, focused tennis match scorekeeper built in Elm.

This app models real tennis scoring — including games, sets, tiebreaks, and match formats — with a small, readable architecture and no external dependencies.

It is intentionally minimal, both in UI and in structure.

---

## ✨ Features

- Real tennis point progression (Love → 15 → 30 → 40 → Deuce → Advantage)
- Game resolution
- Set resolution
- Tiebreak handling (with proper score display formatting)
- Best of 3 or Best of 5 match configuration
- Automatic match winner detection
- Bold display of set winners
- Tiebreak score shown in parentheses (e.g. `7–6 (3)`)
- Restart match functionality

---

## 🧠 Architecture

The application is structured around three clear layers:

### 1. Types

Domain modeling of:

- `Point`
- `Game`
- `SetResult`
- `Match`
- `SetsToWin`

All tennis rules are encoded explicitly in the type system.

---

### 2. Logic

Pure functions that transform match state:

- `updatePoint`
- `updateSet`
- `updateMatch`

Each transformation step is isolated and composable.

No side effects.
No commands.
No ports.

---

### 3. View

Declarative rendering of:

- Match in progress
- Match finished
- Scoreboard
- Set score formatting
- Tiebreak formatting

The view layer focuses only on presentation logic.

---

## 🛠 Development

### Install Elm

```bash
npm install -g elm
```

(Optional but recommended for development:)

```bash
npm install -g elm-live
```

---

### Run in Development Mode

If you have `elm-live` installed and a `dev` script in `package.json`:

```bash
npm run dev
```

This provides:

- Automatic rebuild
- Auto-refresh
- Fast feedback loop

---

### Manual Compile

```bash
elm make src/Main.elm --output=main.js
```

Then open `index.html` in your browser.

---

## 📂 Project Structure

```
src/
  Main.elm
  Types.elm
  Logic.elm
  View.elm

index.html
style.css
```

Each file has a single responsibility.

---

## 🎯 Design Philosophy

This project intentionally favors:

- Small, readable functions
- Clear domain modeling
- Minimal abstraction
- Explicit state transitions
- Straightforward view structure

It is designed to be understood quickly.

---

## 📜 License

MIT
