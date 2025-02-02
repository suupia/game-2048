# 2048 Game in Haskell

## Overview
This is a terminal-based implementation of the 2048 game written in Haskell. The project was created as part of the Haskell Advent Calendar 2024.
[[Haskell] 2048（ゲーム）を作る #Haskell - Qiita](https://qiita.com/suupia/items/4136a7d6a7513054c75b)

## Game Rules
- **Grid:** A 4×4 grid where numbers appear on tiles.
- **Movement:** Use `W`, `A`, `S`, `D` keys to move the tiles.
- **Merging:** Tiles with the same number merge into one when moved in the same direction.
- **New Tiles:** A new tile (2 or 4) appears after each move.
- **Winning:** Reach the tile `2048` to win, but you can keep playing for a higher score.
- **Game Over:** No valid moves left.

## Installation & Setup
Ensure you have `stack` installed:
```sh
brew install stack  # For macOS
```
Create the project:
```sh
stack new game2048
```

## Build & Run
Inside the `game2048` directory, run:
```sh
stack build
stack run
```

## Dependencies
Modify `package.yaml` to include:
```yaml
dependencies:
  - base >= 4.7 && < 5
  - mtl >= 2.3 && < 3
  - random
  - ansi-terminal
```

## Key Features
- Uses `StateT` for game state management.
- Implements merging logic to combine tiles.
- Colors tiles for better visibility.
- Detects game over state.

## Notes
- The game runs in the terminal.
- Use `Q` to quit at any time.
- If `Control.Monad.State` is not found, ensure `mtl` is installed.

## License
MIT License

## Author
Created by @suupia as part of the Haskell Advent Calendar 2024.
