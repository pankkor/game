## Not a Game (yet?)

That is actually not a game, rather a screensaver.

- Uses CoreGraphics private API to show a window.
- OpenGL to render instanced ~~sprites~~ particles.
- Dumb code, not optimized.
- No libc (-nostdlib).

https://github.com/user-attachments/assets/7478f311-aa52-4cdb-b3cc-6ad01327fff5

### Supported Platforms
- macOS AArch64 (clang)

### Build
```
./build.sh
```

### Run
```
./build/main
```
To exit press Ctrl-C.

### Files
- `build.sh` - simple build script.
- `main.c` - all the code.

#### Misc
- `.lvimrc` - vim local config. Ignore if you don't use vim.
- `compile_flags.txt` - list of compilation flags used by clangd and `build.sh`.
