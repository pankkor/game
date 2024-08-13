// Keyhole peeker
// Screen turns black and you can only see your desktop through the flying
// holes.
//
// Platforms
//   macOS AArch64
// Build
//   ./build.sh
// Run
//   ./build/peeker

#include "common.h"

// --------------------------------------
// Config
// --------------------------------------
// Fixed simulation tick time, s.
#define SIM_TICK        (1.0f / 120.0f)
#define SPRITE_VEL_MAX  0.5f
#define SPRITE_SIZE     0.2f
#define SPRITE_SIZE_05  (SPRITE_SIZE * 0.5f)

enum {SPRITES_COUNT = 32};

enum sprite_state {
  SPRITE_STATE_MOVING   = 1 << 1,
  SPRITE_STATE_BLINKING = 1 << 2,
  SPRITE_STATE_SCALING  = 1 << 3,
};

enum game_state {
  GAME_STATE_FADE_OUT         = 1 << 1,
  GAME_STATE_SPRITES_FADE_IN  = 1 << 2,
};

struct sprites {
  ALIGNED(16) f32               pos       [3 * SPRITES_COUNT]; // update + draw
  ALIGNED(16) f32               vel       [2 * SPRITES_COUNT]; // update
  ALIGNED(16) f32               col       [4 * SPRITES_COUNT]; // update + draw
  ALIGNED(16) i32               tile      [2 * SPRITES_COUNT]; // update + draw
  ALIGNED(16) f32               scale     [2 * SPRITES_COUNT]; // update + draw
  ALIGNED(16) f32               scale_vel [2 * SPRITES_COUNT]; // update
  ALIGNED(16) enum sprite_state state     [1 * SPRITES_COUNT]; // update
};

struct sprites s_sprites;

// --------------------------------------
// GLSL
// --------------------------------------
static const char * const s_sprite_vert_src = "                                \
#version 410 core                                                              \
layout(location = 0) in vec4  v_vertuv;                                        \
layout(location = 1) in vec3  v_pos;                                           \
layout(location = 2) in vec4  v_col;                                           \
layout(location = 3) in vec2  v_scale;                                         \
layout(location = 4) in ivec2 v_tile;                                          \
                                                                               \
uniform float iaspect;                                                         \
uniform ivec2 tiles_count;                                                     \
                                                                               \
out vec4 f_col;                                                                \
out vec2 f_uv;                                                                 \
                                                                               \
void main(void) {                                                              \
  vec3 pos = vec3(v_vertuv.xy * v_scale.xy, 0.0) + v_pos;                      \
  pos.x *= iaspect;                                                            \
  gl_Position = vec4(pos, 1.0);                                                \
  f_col = v_col;                                                               \
  vec2 tile_off_uv = vec2(v_tile) / tiles_count;                               \
  f_uv = v_vertuv.zw + tile_off_uv;                                            \
}                                                                              \
";

static const char * const s_sprite_frag_src = "                                \
#version 410 core                                                              \
in vec4 f_col;                                                                 \
in vec2 f_uv;                                                                  \
                                                                               \
uniform sampler2D mask_tx;                                                     \
                                                                               \
out vec4 frag_col;                                                             \
                                                                               \
void main(void) {                                                              \
    float a = texture(mask_tx, f_uv).r;                                        \
    frag_col = vec4(f_col.rgb, 1.0 - a);                                       \
}                                                                              \
";

enum {
  TILE_W = 32,
  TILE_H = 32,
};

enum {
  TILES_X_COUNT = 1,
  TILES_Y_COUNT = 2,
};

enum {
  MASK_W = TILE_W * TILES_X_COUNT,
  MASH_H = TILE_H * TILES_Y_COUNT,
};

#define TILE_U (1.0f / TILES_X_COUNT)
#define TILE_V (1.0f / TILES_Y_COUNT)

// 32x32 R8 sprites:
// #0 keyhole u[0.0, 1.0), v[0.0, 0.5)
// #1 key     u[0.0, 1.0), v[0.5, 1.0)
static u8 s_mask_data[MASK_W * MASH_H] = {
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0x77, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x77, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x80, 0xCC, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0xCC, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x80, 0x80, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x80,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xEE, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xEE, 0xEE, 0xCC, 0x80,
0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x80, 0x80, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x80, 0x80, 0x80, 0xCC, 0xEE, 0xCC, 0x80, 0x80, 0x80, 0xCC, 0xEE, 0xEE, 0xCC, 0x80,
0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0xCC, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xEE, 0xCC, 0x80, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xCC, 0x80,
0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x80, 0xCC, 0xCC, 0xCC, 0x80, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xCC, 0x80,
0x00, 0x00, 0x80, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x00, 0x00, 0x80, 0xCC, 0xCC, 0xCC, 0xCC, 0x80,
0x00, 0x00, 0x00, 0x80, 0xCC, 0xCC, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xCC, 0xCC, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00,
0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

// --------------------------------------
// Entry point (aka main)
// --------------------------------------
void start(void) {
  // Init
  CGLError cgl_err;
  struct window w = init_window(0 /*is_full_screen*/);

  const GLubyte* version_cstr = glGetString(GL_VERSION);
  print_cstr(STDOUT, "OpenGL version: \n");
  print_cstr(STDOUT, (const char *)version_cstr);
  print_cstr(STDOUT, "\n\n");
  print_cstr(STDOUT, "<Press Ctrl-C to exit>\n");

  GLuint sprite_prog = create_gl_shader_program(
    s_sprite_vert_src,
    s_sprite_frag_src
  );

  f32 aspect  = w.rect[2] / w.rect[3];
  f32 iaspect = 1.0f / aspect;

  GLuint vao;
  GLuint vert_bo;
  GLuint pos_bo;
  GLuint col_bo;
  GLuint scale_bo;
  GLuint tile_bo;
  glGenVertexArrays(1, &vao);
  glGenBuffers(1, &vert_bo);
  glGenBuffers(1, &pos_bo);
  glGenBuffers(1, &col_bo);
  glGenBuffers(1, &scale_bo);
  glGenBuffers(1, &tile_bo);

  GLfloat sprite_verts[] = {
    -SPRITE_SIZE_05, -SPRITE_SIZE_05, 0.0f,   TILE_V,
     SPRITE_SIZE_05, -SPRITE_SIZE_05, TILE_U, TILE_V,
    -SPRITE_SIZE_05,  SPRITE_SIZE_05, 0.0f,   0.0f,
     SPRITE_SIZE_05,  SPRITE_SIZE_05, TILE_U, 0.0f,
  };

  glUseProgram(sprite_prog);
  glBindVertexArray(vao);

  // Vertices
  glBindBuffer(GL_ARRAY_BUFFER, vert_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(sprite_verts), sprite_verts,
      GL_STATIC_DRAW);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(0);

  GLuint mask_tx;
  glGenTextures(1, &mask_tx);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, mask_tx);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, MASK_W, MASH_H, 0,
      GL_RED, GL_UNSIGNED_BYTE, s_mask_data);
#if 1
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
#else
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
#endif
#if 1
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
#else
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
#endif

  glUniform1i(glGetUniformLocation(sprite_prog, "mask_tx"), 0);
  glUniform1f(glGetUniformLocation(sprite_prog, "iaspect"), iaspect);
  glUniform2i(glGetUniformLocation(sprite_prog, "tiles_count"),
      TILES_X_COUNT, TILES_Y_COUNT);

  glBindBuffer(GL_ARRAY_BUFFER, pos_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(s_sprites.pos), s_sprites.pos,
      GL_STATIC_DRAW);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, 0);
  glVertexAttribDivisor(1, 1);
  glEnableVertexAttribArray(1);

  glBindBuffer(GL_ARRAY_BUFFER, col_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(s_sprites.col), s_sprites.col,
      GL_STATIC_DRAW);
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glVertexAttribDivisor(2, 1);
  glEnableVertexAttribArray(2);

  glBindBuffer(GL_ARRAY_BUFFER, scale_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(s_sprites.scale), s_sprites.scale,
      GL_STATIC_DRAW);
  glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glVertexAttribDivisor(3, 1);
  glEnableVertexAttribArray(3);

  glBindBuffer(GL_ARRAY_BUFFER, tile_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(s_sprites.tile), s_sprites.tile,
      GL_STATIC_DRAW);
  glVertexAttribPointer(4, 2, GL_INT, GL_FALSE, 0, 0);
  glVertexAttribDivisor(4, 1);
  glEnableVertexAttribArray(4);

  // Logic
  // Scale bounds normalized to [-1;1] to match monitor aspect ratio
  f32 bounds[4] = {
    -1.0f * aspect + SPRITE_SIZE_05, 1.0f * aspect - SPRITE_SIZE_05,
    -1.0f          + SPRITE_SIZE_05, 1.0f          - SPRITE_SIZE_05,
  };

  struct xorshift64_state col_st  = {376586517380863};
  struct xorshift64_state vel_st  = {137382305742834};

  // keyholes
  i32 KEYHOLES_COUNT = SPRITES_COUNT * 0.5f;
  i32 KEYS_COUNT = SPRITES_COUNT - KEYHOLES_COUNT;

  for (i32 idx = 0; idx < KEYHOLES_COUNT; ++idx) {
    i32 i     = idx;
    f32 kcol  = xorshift64(&col_st) / (f32)U64_MAX;
    f32 kvel0 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kvel1 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 k     = (f32)(idx + 0.5f) / KEYHOLES_COUNT;

    // TODO
    s_sprites.pos[i * 3 + 0]        = lerpf32(k, bounds[0] + 0.3f, bounds[1] - 0.3f);
    s_sprites.pos[i * 3 + 1]        = 0.0f;
    // s_sprites.pos[i * 3 + 0]        = -0.8f;
    // s_sprites.pos[i * 3 + 1]        = -0.7f;
    s_sprites.pos[i * 3 + 2]        = (f32)i / SPRITES_COUNT;

    // TODO
    // s_sprites.vel[i * 2 + 0]        = 0.0f;
    // s_sprites.vel[i * 2 + 1]        = 0.5f;
    s_sprites.vel[i * 2 + 0]  = lerpf32(kvel0, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    s_sprites.vel[i * 2 + 1]  = lerpf32(kvel1, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);

    s_sprites.col[i * 4 + 0]        = 0.0f;
    s_sprites.col[i * 4 + 1]        = lerpf32(kcol, 0.4f, 1.0f);
    s_sprites.col[i * 4 + 2]        = 0.25f;
    s_sprites.col[i * 4 + 3]        = 0.2f;

    s_sprites.tile[i * 2 + 0]       = 0;
    s_sprites.tile[i * 2 + 1]       = 0;

    s_sprites.scale[i * 2 + 0]      = 1.0f;
    s_sprites.scale[i * 2 + 1]      = 1.0f;
    s_sprites.scale_vel[i * 2 + 0]  = 0.05f;
    s_sprites.scale_vel[i * 2 + 1]  = 0.05f;

    s_sprites.state[i * 1 + 0] = SPRITE_STATE_MOVING;
  }

  for (i32 idx = 0; idx < KEYS_COUNT; ++idx) {
    i32 i     = idx + KEYHOLES_COUNT;
    f32 kcol  = xorshift64(&col_st) / (f32)U64_MAX;
    f32 kvel0 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kvel1 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 k     = (f32)(idx + 0.5f) / KEYS_COUNT;

    s_sprites.pos[i * 3 + 0]        = bounds[0];
    s_sprites.pos[i * 3 + 1]        = lerpf32(k, bounds[2], bounds[3]);
    s_sprites.pos[i * 3 + 2]        = (f32)i / SPRITES_COUNT;

    // TODO
    // s_sprites.vel[i * 2 + 0]        = 0.5f;
    // s_sprites.vel[i * 2 + 1]        = 0.0f;
    s_sprites.vel[i * 2 + 0]  = lerpf32(kvel0, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    s_sprites.vel[i * 2 + 1]  = lerpf32(kvel1, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);

    s_sprites.col[i * 4 + 0]        = 0.0f;
    s_sprites.col[i * 4 + 1]        = 0.25f;
    s_sprites.col[i * 4 + 2]        = lerpf32(kcol, 1.0f, 0.4f);
    s_sprites.col[i * 4 + 3]        = 0.2f;

    s_sprites.tile[i * 2 + 0]       = 1;
    s_sprites.tile[i * 2 + 1]       = 1;

    s_sprites.scale[i * 2 + 0]      = 1.0f;
    s_sprites.scale[i * 2 + 1]      = 1.0f;
    s_sprites.scale_vel[i * 2 + 0]  = 0.05f;
    s_sprites.scale_vel[i * 2 + 1]  = 0.05f;

    s_sprites.state[i * 1 + 0] = SPRITE_STATE_MOVING;
  }
  f32 fade_out_a = 0.0f;

  // Game loop
  f32 cpu_timer_freq  = read_cpu_timer_freq();
  f32 icpu_timer_freq = 1.0f / cpu_timer_freq;
  u64 tsc             = read_cpu_timer();

  f32 loop_s          = 0.0f;
  u64 loop_count      = 0;
  f32 print_dt_tsc    = tsc + 5.0f * cpu_timer_freq;

  f32 sim_dt          = 0.0f;
  f32 sim_elapsed     = 0.0f;

  while (1) {
    // dt bookkeeping
    u64 new_tsc     = read_cpu_timer();
    f32 dt          = (new_tsc - tsc) * icpu_timer_freq;
    tsc             = new_tsc;
    loop_s          += dt;
    loop_count      += 1;

#if 1 // Print average tick time (print could block io)
    if (tsc > print_dt_tsc) {
      f32 avg_dt    = loop_s / loop_count;

      loop_s        = 0.0f;
      loop_count    = 0;
      print_dt_tsc  = tsc + 5.0f * cpu_timer_freq;

      print_cstr(STDOUT, "Average fps: ");
      print_i64(STDOUT, (u64)(1.0f / avg_dt));
      print_cstr(STDOUT, ", dt: ");
      print_i64(STDOUT, (u64)(avg_dt * 1e3));
      print_cstr(STDOUT, "ms (");
      print_i64(STDOUT, (u64)(avg_dt * 1e6));
      print_cstr(STDOUT, "us)\n");
    }
#else
    (void)loop_s;
    (void)loop_count;
    (void)print_dt_tsc;
#endif
    // Update
    // Accumulate passed time and run simulation with fixed `SIM_TICK` dt
    // Yeah, that's not great when we'r CPU bound.
    sim_dt              += dt;
    i32 sim_tick_count  = sim_dt / SIM_TICK;
    sim_dt              = sim_dt - sim_tick_count * SIM_TICK;

    for (i32 sim_tick = 0; sim_tick < sim_tick_count; ++sim_tick) {
      sim_elapsed += SIM_TICK;

      fade_out_a = clampf32(fade_out_a + 1.0f * SIM_TICK, 0.0f, 1.0f);

      for (i32 i = 0; i < SPRITES_COUNT; ++i) {
        enum sprite_state state = s_sprites.state[i * 1 + 0];
        f32 pos[2];
        f32 vel[2];
        f32 col[4];
        f32 scale[2];
        f32 scale_vel[2];
        i32 dmask[2];
        i32 smask[2];

        state           = s_sprites.state[i * 1 + 0];
        pos[0]          = s_sprites.pos[i * 3 + 0];
        pos[1]          = s_sprites.pos[i * 3 + 1];
        vel[0]          = s_sprites.vel[i * 2 + 0];
        vel[1]          = s_sprites.vel[i * 2 + 1];
        col[0]          = s_sprites.col[i * 4 + 0];
        col[1]          = s_sprites.col[i * 4 + 1];
        col[2]          = s_sprites.col[i * 4 + 2];
        col[3]          = s_sprites.col[i * 4 + 3];
        scale[0]        = s_sprites.scale[i * 2 + 0];
        scale[1]        = s_sprites.scale[i * 2 + 1];
        scale_vel[0]    = s_sprites.scale_vel[i * 2 + 0];
        scale_vel[1]    = s_sprites.scale_vel[i * 2 + 1];

        if (is_bit_set(state, SPRITE_STATE_MOVING)) {
          pos[0]        += vel[0] * SIM_TICK;
          pos[1]        += vel[1] * SIM_TICK;
          // Change direction on colliding with bounds
          dmask[0]      = pos[0] < bounds[0] || pos[0] > bounds[1];
          dmask[1]      = pos[1] < bounds[2] || pos[1] > bounds[3];
          vel[0]        *= (1 - (dmask[0] << 1));
          vel[1]        *= (1 - (dmask[1] << 1));
          pos[0]        = clampf32(pos[0], bounds[0], bounds[1]);
          pos[1]        = clampf32(pos[1], bounds[2], bounds[3]);
        }

        if (is_bit_set(state, SPRITE_STATE_SCALING)) {
          scale[0]      += scale_vel[0];
          scale[1]      += scale_vel[1];
          // Change direction of scale velocity
          smask[0]      = scale[0] < 0.0f || scale[0] > 50.0f;
          smask[1]      = scale[1] < 0.0f || scale[1] > 50.0f;
          scale_vel[0]  *= (1 - (smask[0] << 1));
          scale_vel[1]  *= (1 - (smask[1] << 1));
          scale[0]      = clampf32(scale[0], 0.0f, 50.0f);
          scale[1]      = clampf32(scale[1], 0.0f, 50.0f);
        }

        if (is_bit_set(state, SPRITE_STATE_BLINKING)) {
          col[0] = sinf32(sim_elapsed);
        }

        s_sprites.pos[i * 3 + 0]        = pos[0];
        s_sprites.pos[i * 3 + 1]        = pos[1];
        s_sprites.vel[i * 2 + 0]        = vel[0];
        s_sprites.vel[i * 2 + 1]        = vel[1];
        s_sprites.col[i * 4 + 0]        = col[0];
        s_sprites.col[i * 4 + 1]        = col[1];
        s_sprites.col[i * 4 + 2]        = col[2];
        s_sprites.col[i * 4 + 3]        = col[3];
        s_sprites.scale[i * 2 + 0]      = scale[0];
        s_sprites.scale[i * 2 + 1]      = scale[1];
        s_sprites.scale_vel[i * 2 + 0]  = scale_vel[0];
        s_sprites.scale_vel[i * 2 + 1]  = scale_vel[1];
      }

      // Circle collisions
      f32 radii   = SPRITE_SIZE_05 + SPRITE_SIZE_05;
      f32 radii2  = radii * radii;

      for (i32 i = 0; i < SPRITES_COUNT - 1; ++i) {
        f32 pos0[2];
        f32 vel0[2];
        pos0[0]   = s_sprites.pos[i * 3 + 0];
        pos0[1]   = s_sprites.pos[i * 3 + 1];
        vel0[0]   = s_sprites.vel[i * 2 + 0];
        vel0[1]   = s_sprites.vel[i * 2 + 1];

        for (i32 j = i + 1; j < SPRITES_COUNT; ++j) {
          f32 pos1[2];
          f32 vel1[2];
          f32 d[2];           // distance between centers
          f32 dunit[2];       // normalized d
          f32 depth;
          f32 dlen;
          f32 dlen2;
          f32 dotdvel0;
          f32 dotdvel1;
          f32 dvel[2];

          pos1[0]   = s_sprites.pos[j * 3 + 0];
          pos1[1]   = s_sprites.pos[j * 3 + 1];
          vel1[0]   = s_sprites.vel[j * 2 + 0];
          vel1[1]   = s_sprites.vel[j * 2 + 1];

          d[0]      = pos1[0] - pos0[0];
          d[1]      = pos1[1] - pos0[1];
          dlen2     = d[0] * d[0] + d[1] * d[1];

          if (dlen2 < radii2) {
// TODO remove
#if 0 // Blink red on collision
            s_sprites.col[i * 4 + 0] = 1.0f;
            s_sprites.col[j * 4 + 0] = 1.0f;
#endif

            // Depth and normal of collision intesection
            dlen     = sqrtf32(dlen2);
            depth    = radii - dlen;
            dunit[0] = d[0] / dlen;
            dunit[1] = d[1] / dlen;

            // Repulse
            pos0[0]  += dunit[0] * depth * -0.5;
            pos0[1]  += dunit[1] * depth * -0.5;
            pos1[0]  += dunit[0] * depth * 0.5;
            pos1[1]  += dunit[1] * depth * 0.5;

            // Ellastic collision
            dotdvel0  = dunit[0] * vel0[0] + dunit[1] * vel0[1];
            dotdvel1  = dunit[0] * vel1[0] + dunit[1] * vel1[1];

            dvel[0]   = dunit[0] * (dotdvel1 - dotdvel0);
            dvel[1]   = dunit[1] * (dotdvel1 - dotdvel0);

            vel0[0]   += dvel[0];
            vel0[1]   += dvel[1];
            vel1[0]   -= dvel[0];
            vel1[1]   -= dvel[1];
          }

          s_sprites.pos[i * 3 + 0]        = pos0[0];
          s_sprites.pos[i * 3 + 1]        = pos0[1];
          s_sprites.vel[i * 2 + 0]        = vel0[0];
          s_sprites.vel[i * 2 + 1]        = vel0[1];

          s_sprites.pos[j * 3 + 0]        = pos1[0];
          s_sprites.pos[j * 3 + 1]        = pos1[1];
          s_sprites.vel[j * 2 + 0]        = vel1[0];
          s_sprites.vel[j * 2 + 1]        = vel1[1];
        }
      }
    }

    // Draw
    glClearColor(0.0f, 0.0f, 0.0f, fade_out_a);
    glClear(GL_COLOR_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);

    // Draw sprites
    glBindBuffer(GL_ARRAY_BUFFER, pos_bo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(s_sprites.pos), s_sprites.pos);

    glBindBuffer(GL_ARRAY_BUFFER, col_bo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(s_sprites.col), s_sprites.col);

    glBindBuffer(GL_ARRAY_BUFFER, scale_bo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(s_sprites.scale),
        s_sprites.scale);

    glBindBuffer(GL_ARRAY_BUFFER, tile_bo);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(s_sprites.tile), s_sprites.tile);

    glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, SPRITES_COUNT);
    CHECK_GL_ERROR();

    cgl_err = CGLFlushDrawable(w.glctx); // swap and present
    WARN_IF(cgl_err, "CGLFlushDrawable() failed\n");
  }

  // Shutdown
  glDeleteShader(sprite_prog);

  glDeleteBuffers(1, &vert_bo);
  glDeleteBuffers(1, &pos_bo);
  glDeleteBuffers(1, &col_bo);
  glDeleteBuffers(1, &scale_bo);
  glDeleteBuffers(1, &tile_bo);
  glDeleteVertexArrays(1, &vao);

  shutdown_window(&w);

  exit(0);
}
