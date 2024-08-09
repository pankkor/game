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

struct sprites {
  ALIGNED(16) f32 pos[3 * SPRITES_COUNT];
  ALIGNED(16) f32 vel[2 * SPRITES_COUNT];
  ALIGNED(16) f32 col[4 * SPRITES_COUNT];
};

struct sprites s_sprites;

// --------------------------------------
// GLSL
// --------------------------------------
static const char * const s_sprite_vert_src = "                                \
#version 410 core                                                              \
layout(location = 0) in vec4 v_vertuv;                                         \
layout(location = 1) in vec3 v_pos;                                            \
layout(location = 2) in vec4 v_col;                                            \
                                                                               \
uniform float iaspect;                                                         \
                                                                               \
out vec4 f_col;                                                                \
out vec2 f_uv;                                                                 \
                                                                               \
void main(void) {                                                              \
  vec3 pos = vec3(v_vertuv.xy, 0.0) + v_pos;                                   \
  pos.x *= iaspect;                                                            \
  gl_Position = vec4(pos, 1.0);                                                \
  f_col = v_col;                                                               \
  f_uv = v_vertuv.zw;                                                          \
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
    frag_col = vec4(f_col.rgb, a);                                             \
}                                                                              \
";

enum {
  KEYHOLE_MASK_W = 32,
  KEYHOLE_MASK_H = 32,
};

// Keyhole texture 32x32 single channel
static u8 keyhole_mask_data[KEYHOLE_MASK_W * KEYHOLE_MASK_H] = {
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
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
  glGenVertexArrays(1, &vao);
  glGenBuffers(1, &vert_bo);
  glGenBuffers(1, &pos_bo);
  glGenBuffers(1, &col_bo);

  GLfloat sprite_verts[] = {
    -SPRITE_SIZE_05, -SPRITE_SIZE_05, 0.0f, 1.0f,
     SPRITE_SIZE_05, -SPRITE_SIZE_05, 1.0f, 1.0f,
    -SPRITE_SIZE_05,  SPRITE_SIZE_05, 0.0f, 0.0f,
     SPRITE_SIZE_05,  SPRITE_SIZE_05, 1.0f, 0.0f,
  };

  glUseProgram(sprite_prog);
  glBindVertexArray(vao);

  glUniform1f(glGetUniformLocation(sprite_prog, "iaspect"), iaspect);

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
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, KEYHOLE_MASK_W, KEYHOLE_MASK_H, 0,
      GL_RED, GL_UNSIGNED_BYTE, keyhole_mask_data);
    CHECK_GL_ERROR();
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  // glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  // glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glUniform1i(glGetUniformLocation(sprite_prog, "mask_tx"), 0);

  // Logic
  // Scale bounds normalized to [-1;1] to match monitor aspect ratio
  f32 bounds[4] = {
    -1.0f * aspect + SPRITE_SIZE_05, 1.0f * aspect - SPRITE_SIZE_05,
    -1.0f          + SPRITE_SIZE_05, 1.0f          - SPRITE_SIZE_05,
  };

  struct xorshift64_state pos_st  = {376586517380863};
  struct xorshift64_state vel_st  = {137382305742834};

  for (i32 i = 0; i < SPRITES_COUNT; ++i) {
    f32 vel[2];
    f32 kpos0 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kpos1 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kvel0 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kvel1 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kcol  = xorshift64(&vel_st) / (f32)U64_MAX;

    s_sprites.pos[i * 3 + 0]  = lerpf32(kpos0, bounds[0], bounds[1]);
    s_sprites.pos[i * 3 + 1]  = lerpf32(kpos1, bounds[2], bounds[3]);
    s_sprites.pos[i * 3 + 3]  = (f32)i / SPRITES_COUNT;

    vel[0]                    = lerpf32(kvel0, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    vel[1]                    = lerpf32(kvel1, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    s_sprites.vel[i * 2 + 0]  = vel[0];
    s_sprites.vel[i * 2 + 1]  = vel[1];

    s_sprites.col[i * 4 + 0]  = lerpf32(kcol, 0.0f, 1.0f);
    s_sprites.col[i * 4 + 1]  = lerpf32(kcol, 0.2f, 0.8f);
    s_sprites.col[i * 4 + 2]  = lerpf32(kcol, 1.0f, 0.0f);
    s_sprites.col[i * 4 + 3]  = 0.2f;
  }

  // Game loop
  f32 cpu_timer_freq  = read_cpu_timer_freq();
  f32 icpu_timer_freq = 1.0f / cpu_timer_freq;
  u64 tsc             = read_cpu_timer();

  f32 loop_s          = 0.0f;
  u64 loop_count      = 0;
  f32 print_dt_tsc    = tsc + 5.0f * cpu_timer_freq;

  f32 sim_dt          = 0.0f;

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
      for (i32 i = 0; i < SPRITES_COUNT; ++i) {
        f32 pos[2];
        f32 vel[2];
        i32 dmask[2];

        pos[0]      = s_sprites.pos[i * 3 + 0];
        pos[1]      = s_sprites.pos[i * 3 + 1];
        vel[0]      = s_sprites.vel[i * 2 + 0];
        vel[1]      = s_sprites.vel[i * 2 + 1];

        pos[0]      += vel[0] * SIM_TICK;
        pos[1]      += vel[1] * SIM_TICK;

        // Change direction on colliding with bounds
        dmask[0]    = pos[0] < bounds[0] || pos[0] > bounds[1];
        dmask[1]    = pos[1] < bounds[2] || pos[1] > bounds[3];
        vel[0]      *= (1 - (dmask[0] << 1));
        vel[1]      *= (1 - (dmask[1] << 1));

        pos[0]      = clampf32(pos[0], bounds[0], bounds[1]);
        pos[1]      = clampf32(pos[1], bounds[2], bounds[3]);

        s_sprites.pos[i * 3 + 0]  = pos[0];
        s_sprites.pos[i * 3 + 1]  = pos[1];
        s_sprites.vel[i * 2 + 0]  = vel[0];
        s_sprites.vel[i * 2 + 1]  = vel[1];
      }
    }

    // Draw
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);

    // Draw sprites
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

    glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, SPRITES_COUNT);
    CHECK_GL_ERROR();

    cgl_err = CGLFlushDrawable(w.glctx); // swap and present
    WARN_IF(cgl_err, "CGLFlushDrawable() failed\n");
  }

  // Shutdown
  glDeleteShader(sprite_prog);

  glDeleteBuffers(1, &vert_bo);
  glDeleteBuffers(1, &col_bo);
  glDeleteVertexArrays(1, &vao);

  shutdown_window(&w);

  exit(0);
}
