# A tiny collection of screensavers

- CoreGraphics private API to show a window.
- OpenGL to render.
- No libc (-nostdlib).
- Not optimizated.
- To exit press ESC or Ctrl-C.

### Supported Platforms
- macOS AArch64 (clang)

## Molecular Wind screensaver
`src/wind.c`

Render up to 1M instanced ~~sprites~~ particles that bounce around the edges,
and are blown by the wind. They change color depending on their speed.

- Particle update is simulated on CPU without any optimizations.
- Ofc compiler fails to autovectorize the update.

https://github.com/user-attachments/assets/cd8e88c2-d747-4fd0-91fe-a7a5d4a00b77

## Keys and Keyholes screensaver
`src/keyhole.c`

Keys and Keyholes flying around and colliding.
Matching keys and keyholes dissapear.

- Simple cirle elastic collision simulated on CPU.

https://github.com/user-attachments/assets/eb9552e4-32e3-4b9c-8be6-1a401453ae10

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
- `src/common.h` - common code for screensavers.
- `build.sh` - build script.
- `compile_flags.txt` - list of compilation flags used by clangd and `build.sh`.

#### Misc
- `.clangd` - clangd config.
- `.lvimrc` - vim local config.
