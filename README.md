## Not a Game
That's totally not a game, rather a screensaver.

Uses CoreGraphics private API to show a window and OpenGL to render instanced
~~sprites~~ particles.

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
- `.lvimrc` - vim local config. Ignore if you don't use viml.
- `compile_flags.txt` - list of compilation flags used by clangd and `build.sh`.
