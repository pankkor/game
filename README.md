# A tiny collection of screensavers

- CoreGraphics private API to show a window.
- OpenGL to render.
- No libc (-nostdlib).
- Had no time for optimizations.
- To exit press Ctrl-C.

## Molecular Wind screensaver
`src/wind.c`

Render up to 1M instanced ~~sprites~~ particles that bounce around the edges,
and are blown by the wind. They change color depending on their speed.

- Sprite update is simulated on CPU without any optimization.
- Ofc compiler fails to autovectorize sprite update.

https://github.com/user-attachments/assets/dc945f6e-4408-4466-b5a7-13c9fad3a36e

## Keys and Keyholes screensaver
`src/keyhole.c`

Keys and Keyholes flying around and colliding.
Matching keys and keyholes dissapear.

- Simple collision of circles on CPU.

https://github.com/user-attachments/assets/eb9552e4-32e3-4b9c-8be6-1a401453ae10

### Supported Platforms
- macOS AArch64 (clang)

### Build all
```
./build.sh
```

### Run
Binaries are located in `build` directory.
```
./build/wind
```

### Files
- `src/*.c` - all screensavers, each screensaver is a separate `.c` file.
- `build.sh` - build script.

#### Misc
- `.clangd` - clangd config to suppress warnings
- `.lvimrc` - vim local config. Ignore if you don't use vim.
- `compile_flags.txt` - list of compilation flags used by clangd and `build.sh`.
