// Molecular Wind screensaver.
// From 1K to 1M particles bouncing around the edges, and blown by the wind.
//
// Platforms
//   macOS AArch64
// Build
//   ./build.sh
// Run
//   ./build/wind

#include "common.h"

// --------------------------------------
// Config
// --------------------------------------
// Fixed simulation tick time, s.
#define SIM_TICK        (1.0f / 120.0f)
// Wind acceleration, wind changes direction when reaches it's max velocity.
#define WIND_ACC_X      0.2f
#define WIND_ACC_Y      -0.33f
// Wind changes direction when reaches it's max velocity.
#define WIND_VEL_MAX    1.1f
// Initial Sprite velocities are in [-SPRITE_VEL_MAX, SPRITE_VEL_MAX] range.
#define SPRITE_VEL_MAX  0.5f

#define VEL_MAX         (SPRITE_VEL_MAX + WIND_VEL_MAX)
#define VEL_MAX2        (VEL_MAX * VEL_MAX)

#if 1 // Big and chunky sprites
#define SPRITE_SIZE     0.02f
enum {SPRITES_COUNT = 1024};
#else // Chaos (1M sprites is still ok)
#define SPRITE_SIZE     0.005f
enum {SPRITES_COUNT = 1 * 1024 * 1024};
#endif

#define SPRITE_SIZE_05  (SPRITE_SIZE * 0.5f)

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
layout(location = 0) in vec2 v_vert;                                           \
layout(location = 1) in vec3 v_pos;                                            \
layout(location = 2) in vec4 v_col;                                            \
                                                                               \
uniform float iaspect;                                                         \
                                                                               \
out vec4 f_col;                                                                \
out vec2 f_pos;                                                                \
                                                                               \
void main(void) {                                                              \
  vec3 pos = vec3(v_vert, 0.0f) + v_pos;                                       \
  pos.x *= iaspect;                                                            \
  gl_Position = vec4(pos, 1.0f);                                               \
  f_col = v_col;                                                               \
  f_pos = vec2(v_pos.x * iaspect, v_pos.y) * 0.5 + 0.5;                        \
}                                                                              \
";

static const char * const s_sprite_frag_src = "                                \
#version 410 core                                                              \
in vec4 f_col;                                                                 \
in vec2 f_pos;                                                                 \
                                                                               \
uniform vec2  resolution;                                                      \
uniform float iaspect;                                                         \
                                                                               \
out vec4 frag_col;                                                             \
                                                                               \
void main(void) {                                                              \
    vec2 uv = gl_FragCoord.xy / resolution.xy;                                 \
    vec2 p = f_pos - uv;                                                       \
    p.x /= iaspect;                                                            \
    float d = length(p) - 0.005;                                               \
    float a = smoothstep(0.001, -0.001, d);                                    \
    frag_col = vec4(f_col.rgb, a);                                             \
}                                                                              \
";

// --------------------------------------
// Entry point (aka main)
// --------------------------------------
void start(void) {
  // Init
  struct event_loop loop;
  struct window w;

  event_loop_init(&loop);
  window_init(&w, 0 /*is_full_screen*/);

  const GLubyte* version_cstr = glGetString(GL_VERSION);
  print_cstr(STDOUT, "OpenGL version: \n");
  print_cstr(STDOUT, (const char *)version_cstr);
  print_cstr(STDOUT, "\n\n");
  print_cstr(STDOUT, "<Press ESC or Ctrl-C to exit>\n");

  GLuint sprite_prog = create_gl_shader_program(
    s_sprite_vert_src,
    s_sprite_frag_src
  );

  f32 aspect  = w.rect[2] / w.rect[3];
  f32 iaspect = 1.0f / aspect;

  f32 clear_col[4]  = {0};

  GLuint vao;
  GLuint vert_bo;
  GLuint pos_bo;
  GLuint col_bo;
  glGenVertexArrays(1, &vao);
  glGenBuffers(1, &vert_bo);
  glGenBuffers(1, &pos_bo);
  glGenBuffers(1, &col_bo);

  GLfloat sprite_verts[] = {
    -SPRITE_SIZE_05, -SPRITE_SIZE_05,
     SPRITE_SIZE_05, -SPRITE_SIZE_05,
    -SPRITE_SIZE_05,  SPRITE_SIZE_05,
     SPRITE_SIZE_05,  SPRITE_SIZE_05
  };

  glUseProgram(sprite_prog);
  glBindVertexArray(vao);

  glUniform1f(glGetUniformLocation(sprite_prog, "iaspect"), iaspect);
  glUniform2f(glGetUniformLocation(sprite_prog, "resolution"),
      w.rect[2], w.rect[3]);

  // Vertices
  glBindBuffer(GL_ARRAY_BUFFER, vert_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(sprite_verts), sprite_verts,
      GL_STATIC_DRAW);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(0);

  // Logic
  // Scale bounds normalized to [-1;1] to match monitor aspect ratio
  // [left, right, down, up]
  f32 bounds[4] = {
    -1.0f * aspect, 1.0f * aspect,
    -1.0f, 1.0f,
  };

  struct xorshift64_state pos_st  = {376586517380863};
  struct xorshift64_state vel_st  = {137382305742834};

  for (i32 i = 0; i < SPRITES_COUNT; ++i) {
    f32 kpos0 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kpos1 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kvel0 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kvel1 = xorshift64(&vel_st) / (f32)U64_MAX;

    s_sprites.pos[i * 3 + 0]  = lerpf32(kpos0, bounds[0], bounds[1]);
    s_sprites.pos[i * 3 + 1]  = lerpf32(kpos1, bounds[2], bounds[3]);
    s_sprites.pos[i * 3 + 2]  = (f32)i / SPRITES_COUNT;

    s_sprites.vel[i * 2 + 0]  = lerpf32(kvel0, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    s_sprites.vel[i * 2 + 1]  = lerpf32(kvel1, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);

    // Rest of s_sprites.col is determined in during update
    s_sprites.col[i * 4 + 1]  = 0.1f;
    s_sprites.col[i * 4 + 3]  = 0.9f;
  }

  f32 ntemp = 0.0f;        // normalized "temperature" of the screen
  f32 total_kcol;

  f32 wind_vel[2] = {0};
  f32 wind_acc[2] = {WIND_ACC_X, WIND_ACC_Y};

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
      print_avg_dt_fps(loop_s / loop_count);

      loop_s        = 0.0f;
      loop_count    = 0;
      print_dt_tsc  = tsc + 5.0f * cpu_timer_freq;
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
      i32 wind_velmask[2] = {
        wind_vel[0] < -WIND_VEL_MAX || wind_vel[0] > WIND_VEL_MAX,
        wind_vel[1] < -WIND_VEL_MAX || wind_vel[1] > WIND_VEL_MAX
      };

      wind_vel[0]   = clampf32(wind_vel[0], -WIND_VEL_MAX, WIND_VEL_MAX);
      wind_vel[1]   = clampf32(wind_vel[1], -WIND_VEL_MAX, WIND_VEL_MAX);
      wind_acc[0]   *= (1 - (wind_velmask[0] << 1));
      wind_acc[1]   *= (1 - (wind_velmask[1] << 1));
      wind_vel[0]   += wind_acc[0] * SIM_TICK;
      wind_vel[1]   += wind_acc[1] * SIM_TICK;

      total_kcol    = 0.0f;
      for (i32 i = 0; i < SPRITES_COUNT; ++i) {
        f32 pos[2];
        f32 vel[2];
        i32 dmask[2];

        pos[0]      = s_sprites.pos[i * 3 + 0];
        pos[1]      = s_sprites.pos[i * 3 + 1];
        vel[0]      = s_sprites.vel[i * 2 + 0];
        vel[1]      = s_sprites.vel[i * 2 + 1];

        f32 kcol    = (vel[0] * vel[0] + vel[1] * vel[1]) / (VEL_MAX2);
        kcol        = clampf32(kcol, 0.0f, 1.0f);

        // Apply wind
        vel[0]      += wind_vel[0] * SIM_TICK;
        vel[1]      += wind_vel[1] * SIM_TICK;

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
        s_sprites.col[i * 4 + 0]  = lerpf32(kcol, 0.1f, 1.0f);
        s_sprites.col[i * 4 + 2]  = lerpf32(kcol, 1.0f, 0.1f);

        total_kcol  += kcol;
      }
    }

    // Draw
    // Update clear color based on the temperature
    ntemp         = total_kcol / SPRITES_COUNT;
    clear_col[0]  = lerpf32(ntemp, 0.1f, 0.3f);
    clear_col[1]  = 0.1f;
    clear_col[2]  = lerpf32(ntemp, 0.2f, 0.1f);
    clear_col[3]  = 0.25f;

    glClearColor(clear_col[0], clear_col[1], clear_col[2], clear_col[3]);
    glClear(GL_COLOR_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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

    window_flush(&w);

    // Step through event loop once, updating input events
    event_loop_step(&loop);

    // ESC to exit
    if (loop.keycodes.e[KC_ESC]) {
      break;
    }
  }
  print_avg_dt_fps(loop_s / loop_count);

  // Shutdown
  glDeleteShader(sprite_prog);

  glDeleteBuffers(1, &vert_bo);
  glDeleteBuffers(1, &col_bo);
  glDeleteVertexArrays(1, &vao);

  window_shutdown(&w);
  event_loop_shutdown(&loop);

  exit(0);
}
