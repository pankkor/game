// That is actually not a game, but a screensaver
// Platforms
//   macOS AArch64
// Build
//   ./build.sh
// Run
//   ./build/main

// --------------------------------------
// Types
// --------------------------------------
typedef signed char         i8;
typedef unsigned char       u8;
typedef short               i16;
typedef unsigned short      u16;
typedef int                 i32;
typedef unsigned int        u32;
typedef long long           i64;
typedef unsigned long long  u64;
typedef float               f32;
typedef double              f64;
typedef i32                 b32;

#define U64_MAX             -1UL

#define FORCE_INLINE        inline __attribute__((always_inline))
#define NO_RETURN           __attribute__((noreturn))

#define GL_SILENCE_DEPRECATION
#include <OpenGL/OpenGL.h>
#include <OpenGL/gl3.h>
#include <OpenGL/gl3ext.h>
#include <CoreGraphics/CoreGraphics.h>

// --------------------------------------
// Private CoreGraphics API
// --------------------------------------
typedef enum {
  kCGSOrderBelow = -1,
  kCGSOrderOut,       // hides the window
  kCGSOrderAbove,
  kCGSOrderIn         // shows the window
} CGSWindowOrderingMode;

typedef int CGSConnectionID;
typedef int CGSSurfaceID;
typedef CFTypeRef CGSRegionRef;

extern CGSConnectionID CGSMainConnectionID(void);

extern CGError CGSNewWindow(CGSConnectionID cid,
    CGWindowBackingType backingType, CGFloat left, CGFloat top,
    CGSRegionRef region, CGWindowID *outWID);

extern CGError CGSReleaseWindow(CGSConnectionID cid, CGWindowID wid);

extern CGContextRef CGWindowContextCreate(CGSConnectionID cid,
    CGWindowID wid, CFDictionaryRef options);

extern CGError CGSFlushWindow(CGSConnectionID cid, CGWindowID wid,
    CGSRegionRef flushRegion);

extern CGError CGSNewRegionWithRect(const CGRect *rect, CGSRegionRef *out);

extern CGError CGSSetWindowLevel(CGSConnectionID cid, CGWindowID wid,
    CGWindowLevel level);

extern CGError CGSOrderWindow(CGSConnectionID cid, CGWindowID wid,
    CGSWindowOrderingMode mode, CGWindowID relativeToWID);

extern CGError CGSSetWindowOpacity(CGSConnectionID cid, CGWindowID wid,
    bool isOpaque);

extern CGError CGSAddSurface(CGSConnectionID cid, CGWindowID wid,
    CGSSurfaceID *outSID);

extern CGLError CGLSetSurface(CGLContextObj glctx, CGSConnectionID cid,
    CGWindowID wid, CGSSurfaceID sid);

extern CGError CGSOrderSurface(CGSConnectionID cid, CGWindowID wid,
    CGSSurfaceID surface, CGSSurfaceID otherSurface, int place);

extern CGError CGSSetSurfaceBounds(CGSConnectionID cid, CGWindowID wid,
    CGSSurfaceID sid, CGRect bounds);

// --------------------------------------
// GLSL
// --------------------------------------
static const char * const s_sprite_vert_src = "                                \
#version 410 core                                                              \
layout(location = 0) in vec2 v_vert;                                           \
layout(location = 1) in vec3 v_pos;                                            \
layout(location = 2) in vec4 v_col;                                            \
layout(location = 3) in vec2 v_tex_coord;                                      \
                                                                               \
uniform float iaspect;                                                         \
                                                                               \
out vec4 f_col;                                                                \
out vec2 f_tex_coord;                                                          \
                                                                               \
void main()                                                                    \
{                                                                              \
  vec3 pos = vec3(v_vert, 0.0f) + v_pos;                                       \
  pos.x *= iaspect;                                                            \
  gl_Position = vec4(pos, 1.0f);                                               \
  f_col = v_col;                                                               \
  /*f_tex_coord = v_tex_coord;*/                                               \
}                                                                              \
";

static const char * const s_sprite_frag_src = "                                \
#version 410 core                                                              \
in vec4 f_col;                                                                 \
/*in vec2 f_tex_coord;*/                                                       \
                                                                               \
out vec4 frag_col;                                                             \
                                                                               \
/*uniform sampler2D sampler0;                                                  \
*/                                                                             \
void main()                                                                    \
{                                                                              \
    /* frag_col = texture(sample0, f_tex_coord);*/                             \
    frag_col = f_col;                                                          \
}                                                                              \
";

// --------------------------------------
// syscall
// --------------------------------------
i64 syscall1(i64 sys_num, i64 a0) {
  i64 ret;
  __asm__ volatile (
    "mov x16, %[sys_num]\n"   // syscall number
    "mov x0,  %[a0]\n"   // a0
    "svc      0x80\n"
    "mov %0,  x0\n"
    : "=r" (ret)
    : [sys_num] "r" (sys_num), [a0] "r" (a0)
    : "x16", "x0"
  );
  return ret;
}

i64 syscall2(i64 sys_num, i64 a0, i64 a1) {
  i64 ret;
  __asm__ volatile (
    "mov x16, %[sys_num]\n"
    "mov x0,  %[a0]\n"
    "mov x1,  %[a1]\n"
    "svc      0x80\n"
    "mov %0,  x0\n"
    : "=r" (ret)
    : [sys_num] "r" (sys_num), [a0] "r" (a0), [a1] "r" (a1)
    : "x16", "x0", "x1"
  );
  return ret;
}

i64 syscall3(i64 sys_num, i64 a0, i64 a1, i64 a2) {
  i64 ret;
  __asm__ volatile (
    "mov x16, %[sys_num]\n"
    "mov x0,  %[a0]\n"
    "mov x1,  %[a1]\n"
    "mov x2,  %[a2]\n"
    "svc      0x80\n"
    "mov %0,  x0\n"
    : "=r" (ret)
    : [sys_num] "r" (sys_num), [a0] "r" (a0), [a1] "r" (a1), [a2] "r" (a2)
    : "x16", "x0", "x1", "x2"
  );
  return ret;
}

// --------------------------------------
// Syscalls
// --------------------------------------
#define SYS_exit        1
#define SYS_write       4

NO_RETURN void exit(i32 ec) {
  syscall1(SYS_exit, ec);
  __builtin_unreachable();
}

// --------------------------------------
// Print
// --------------------------------------
#define STDIN           0
#define STDOUT          1
#define STDERR          2

i64 cstr_len(const char *cstr) {
  i64 ret = 0;
  if (cstr) {
    while (*cstr++) {
      ++ret;
    }
  }
  return ret;
}

void print_buf(i32 fd, const char *buf, i32 size) {
  syscall3(SYS_write, fd, (i64)buf, size);
}

void print_cstr(i32 fd, const char *cstr) {
  if (cstr) {
    i64 size = cstr_len(cstr);
    syscall3(SYS_write, fd, (i64)cstr, size);
  } else {
    syscall3(SYS_write, fd, (i64)"(null)", 6);
  }
}

void print_u64x(i32 fd, u64 v) {
  u8 buf[18];
  buf[0] = '0';
  buf[1] = 'x';

  for (i32 i = 0; i < 16; ++i) {
    u8 r = (v >> ((15 - i) << 2)) & 0xf;
    buf[i + 2] = r > 9 ? r - 10 + 'a' : r + '0';
  }

  syscall3(SYS_write, fd, (i64)buf, sizeof(buf) / sizeof(buf[0]));
}

void print_i64(i32 fd, i64 v) {
  u8 buf[21]; // sign (1 char) + 2^64(20 chars)

  i32 is_neg = 0;
  if (v < 0) {
    is_neg = 1;
    v = -v;
  }

  u8 *buf_end = buf + sizeof(buf) / sizeof(buf[0]);
  u8 *buf_cur = buf_end;
  do {
    i64 r = v % 10;
    *--buf_cur = r + '0';
    v = v / 10;
  } while (v);

  if (is_neg) {
    *--buf_cur = '-';
  }
  syscall3(SYS_write, fd, (i64)buf_cur, buf_end - buf_cur);
}

// --------------------------------------
// Expect/Assert
// --------------------------------------
FORCE_INLINE static void debugbreak(void) {
#if defined(_MSC_VER)
    __debugbreak();
#elif defined(__clang__)
    __builtin_debugtrap();
#else
    // gcc doesn't have __builtin_debugtrap equivalent
    // Beware:
    // __builtin_trap generates SIGILL and code after it will be optmized away.
    __builtin_trap();
#endif
}

#define STR1(s) # s
#define STR(s) STR1(s)

// EXPECT() behaves like Debug + Release assert
#define EXPECT(condition, msg) expect_msg(!!(condition), \
    __FILE__ ":" STR(__LINE__) ": Fatal: (" STR(condition) ") == 0\n"\
    msg)

FORCE_INLINE static void expect_msg(i32 condition, const char *msg) {
  if (!condition) {
    print_cstr(STDERR, msg);
    debugbreak();
  }
}

#define WARN_IF(condition, msg) warn_if_msg(!!(condition), \
    __FILE__ ":" STR(__LINE__) ": Warning: (" STR(condition) ") == 0\n"\
    msg)

FORCE_INLINE static void warn_if_msg(i32 condition, const char *msg) {
  if (condition) {
    print_cstr(STDERR, msg);
  }
}

// --------------------------------------
// Helpers
// --------------------------------------
FORCE_INLINE static f32 clampf32(f32 v, f32 lo, f32 hi) {
  return v > hi ? hi : v < lo ? lo : v;
}

FORCE_INLINE static f32 lerpf32(f32 k, f32 x, f32 y) {
  return (1.0f - k) * x + y * k;
}

#define CHECK_GL_ERROR()                                                       \
do {                                                                           \
  GLenum gl_err = glGetError();                                                \
  WARN_IF(gl_err, "");                                                         \
  if (gl_err) {                                                                \
    print_cstr(STDERR, "glGetError == ");                                      \
    print_u64x(STDERR, gl_err);                                                \
    print_cstr(STDERR, "\n");                                                  \
    exit(1);                                                                   \
  }                                                                            \
} while (0)

struct xorshift64_state {
  u64 a;
};

u64 static xorshift64(struct xorshift64_state *state) {
  uint64_t x = state->a;
  x ^= x << 7;
  x ^= x >> 9;
  state->a = x;
  return x;
}

FORCE_INLINE u64 read_cpu_timer_freq(void) {
  u64 val;
  __asm__ volatile("mrs %0, cntfrq_el0" : "=r" (val));
  return val;
}

FORCE_INLINE u64 read_cpu_timer(void) {
  u64 val;
  // use isb to avoid speculative read of cntvct_el0
  __asm__ volatile("isb;\n\tmrs %0, cntvct_el0" : "=r" (val) :: "memory");
  return val;
}

// --------------------------------------
// nostdlib stubs
// --------------------------------------

#if 1 // stack protector
u64 __stack_chk_guard = 0xDEADBEEF;

void __stack_chk_fail(void) {
    print_cstr(STDERR, "Stack smashed!\n");
    debugbreak();
}
#endif

// --------------------------------------
// Config
// --------------------------------------
// Fixed simulation tick time, s.
#define SIM_TICK        (1.0f / 240.0f)
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
#define SPRITE_SIZE     0.0005f
enum {SPRITES_COUNT = 1 * 1000 * 1000};
#endif

#define SPRITE_SIZE_05  (SPRITE_SIZE * 0.5f)

struct sprites {
  f32 pos[3 * SPRITES_COUNT];
  f32 vel[2 * SPRITES_COUNT];
  f32 col[4 * SPRITES_COUNT];
};

struct sprites s_sprites;

// --------------------------------------
// Entry point (aka main)
// --------------------------------------
void start(void) {
  // Create a window using Core Graphics private API
  CGError   cg_err;
  CGLError  cgl_err;

  CGDirectDisplayID did;
  CGWindowID        wid;
  CGSConnectionID   cid;

  CGRect            view_rect;
  CGRect            win_rect;

  cid = CGSMainConnectionID();
  EXPECT(cid, "Failed CGSMainConnectionID()");

  did = CGMainDisplayID();
  EXPECT(did, "Failed CGMainDisplayID()");

  win_rect  = CGDisplayBounds(did);
  view_rect = (CGRect){.size = win_rect.size};
#if 0 // Full screen
  cg_err = CGDisplayCapture(did);
  EXPECT(!cg_err, "Failed to capture display\n");

  wid = CGShieldingWindowID(did);
  EXPECT(wid, "Failed to get shielding window\n");
  win_rect = CGRectMake(800.0, 100.0, 800.0, 600.0);
#else // Over the screen
  CGSRegionRef      win_region;
  CGSRegionRef      view_region;

  cg_err = CGSNewRegionWithRect(&win_rect, &win_region);
  EXPECT(!cg_err, "Failed to create region\n");

  cg_err = CGSNewRegionWithRect(&win_rect, &view_region);
  EXPECT(!cg_err, "Failed to create region\n");

  cg_err = CGSNewWindow(cid, kCGBackingStoreBuffered, 0.0, 0.0, win_region, &wid);
  EXPECT(!cg_err, "Failed to create window\n");

  // clear windows surfeace
  CGContextRef cgctx = CGWindowContextCreate(cid, wid, 0);
  CGContextClearRect(cgctx, view_rect);
  CGContextRelease(cgctx);

  cg_err = CGSSetWindowLevel(cid, wid, kCGMaximumWindowLevel);
  EXPECT(!cg_err, "Failed to set window level\n");

  cg_err = CGSSetWindowOpacity(cid, wid, 0);
  EXPECT(!cg_err, "Failed to set window opacity\n");

  cg_err = CGSOrderWindow(cid, wid, kCGSOrderIn, 0); // make window appear
  EXPECT(!cg_err, "Failed to order window\n");
#endif
  // Create OpenGL context
  CGLPixelFormatAttribute attributes[] = {
    kCGLPFAOpenGLProfile, (CGLPixelFormatAttribute)kCGLOGLPVersion_GL4_Core,
    kCGLPFAColorSize, (CGLPixelFormatAttribute)24,
    kCGLPFADepthSize, (CGLPixelFormatAttribute)24,
    kCGLPFAStencilSize, (CGLPixelFormatAttribute)8,
    kCGLPFAAccelerated,
    kCGLPFADoubleBuffer,
    (CGLPixelFormatAttribute)0
  };

  CGLPixelFormatObj pixelFormat;
  GLint numPixelFormats;
  CGLChoosePixelFormat(attributes, &pixelFormat, &numPixelFormats);
  EXPECT(pixelFormat, "Failed to create OpenGL pixel format\n");

  CGLContextObj glctx;
  CGLCreateContext(pixelFormat, 0, &glctx);
  CGLDestroyPixelFormat(pixelFormat);
  EXPECT(glctx, "Failed to create OpenGL glctx\n");

  GLint vsync_enabled = 1;
  CGLSetParameter(glctx, kCGLCPSwapInterval, &vsync_enabled);

  GLint surface_opacity = 0;
  CGLSetParameter(glctx, kCGLCPSurfaceOpacity, &surface_opacity);

  CGSSurfaceID sid;
  cg_err = CGSAddSurface(cid, wid, &sid);
  EXPECT(!cg_err, "Failed to add surface\n");

  cgl_err = CGSSetSurfaceBounds(cid, wid, sid, view_rect);
  EXPECT(!cgl_err, "Failed to set surface bounds\n");

  cgl_err = CGSOrderSurface(cid, wid, sid, 1, 0);
  EXPECT(!cgl_err, "Failed to order surface bounds\n");

  cgl_err = CGLSetSurface(glctx, cid, wid, sid);
  EXPECT(!cgl_err, "Failed to set surface\n");

  GLint is_drawable = 0;
  cgl_err = CGLGetParameter(glctx, kCGLCPHasDrawable, &is_drawable);
  EXPECT(!cgl_err, "Failed to get is drawable parameter\n");

  CGLSetCurrentContext(glctx);

  const GLubyte* version_cstr = glGetString(GL_VERSION);
  print_cstr(STDOUT, "OpenGL version: \n");
  print_cstr(STDOUT, (const char *)version_cstr);
  print_cstr(STDOUT, "\n\n");

  // Init

  // Clear color
  f32 clear_col[4]  = {0};

  // Shaders
  GLuint sprite_prog = glCreateProgram();
  {
    GLint is_ok;
    GLchar info[1024];

    GLuint vert_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vert_shader, 1, &s_sprite_vert_src, 0);
    glCompileShader(vert_shader);

    glGetShaderiv(vert_shader, GL_COMPILE_STATUS, &is_ok);
    if (!is_ok) {
      glGetShaderInfoLog(vert_shader, sizeof(info), 0, info);
      print_cstr(STDOUT, "Vertex shader compile error:\n");
      print_cstr(STDOUT, info);
    }
    EXPECT(is_ok, "Failed to compile vertex shader");

    GLuint frag_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(frag_shader, 1, &s_sprite_frag_src, 0);
    glCompileShader(frag_shader);

    glGetShaderiv(frag_shader, GL_COMPILE_STATUS, &is_ok);
    if (!is_ok) {
      glGetShaderInfoLog(frag_shader, sizeof(info), 0, info);
      print_cstr(STDOUT, "Fragment shader compile error:\n");
      print_cstr(STDOUT, info);
    }
    EXPECT(is_ok, "Failed to compile fragment shader");

    sprite_prog = glCreateProgram();
    glAttachShader(sprite_prog, vert_shader);
    glAttachShader(sprite_prog, frag_shader);
    glLinkProgram(sprite_prog);

    glGetProgramiv(sprite_prog, GL_LINK_STATUS, &is_ok);
    if (!is_ok) {
      glGetProgramInfoLog(sprite_prog, sizeof(info), 0, info);
      print_cstr(STDOUT, "Program link error:\n");
      print_cstr(STDOUT, info);
    }
    EXPECT(is_ok, "Failed to link shader program");

    glDeleteShader(vert_shader);
    glDeleteShader(frag_shader);
  }

  f32 aspect  = view_rect.size.width / view_rect.size.height;
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
    -SPRITE_SIZE_05, -SPRITE_SIZE_05,
     SPRITE_SIZE_05, -SPRITE_SIZE_05,
    -SPRITE_SIZE_05,  SPRITE_SIZE_05,
     SPRITE_SIZE_05,  SPRITE_SIZE_05
  };

  glUseProgram(sprite_prog);
  glBindVertexArray(vao);

  glUniform1f(0, iaspect);

  // Vertices
  glBindBuffer(GL_ARRAY_BUFFER, vert_bo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(sprite_verts), sprite_verts,
      GL_STATIC_DRAW);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(0);

  // Logic
  // Scale bounds normalized to [-1;1] to match monitor aspect ratio
  f32 bounds[4] = {
    -1.0f * aspect, 1.0f * aspect,
    -1.0f, 1.0f,
  };

  struct xorshift64_state pos_st  = {376586517380863};
  struct xorshift64_state vel_st  = {137382305742834};

  for (i32 i = 0; i < SPRITES_COUNT; ++i) {
    f32 vel[2];
    f32 kpos0 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kpos1 = xorshift64(&pos_st) / (f32)U64_MAX;
    f32 kvel0 = xorshift64(&vel_st) / (f32)U64_MAX;
    f32 kvel1 = xorshift64(&vel_st) / (f32)U64_MAX;

    s_sprites.pos[i * 3 + 0]  = lerpf32(kpos0, bounds[0], bounds[1]);
    s_sprites.pos[i * 3 + 1]  = lerpf32(kpos1, bounds[2], bounds[3]);
    s_sprites.pos[i * 3 + 3]  = (f32)i / SPRITES_COUNT;

    vel[0]                    = lerpf32(kvel0, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    vel[1]                    = lerpf32(kvel1, -SPRITE_VEL_MAX, SPRITE_VEL_MAX);
    s_sprites.vel[i * 2 + 0]  = vel[0];
    s_sprites.vel[i * 2 + 1]  = vel[1];

    // Rest of s_sprites.col is determined in during update
    s_sprites.col[i * 4 + 1]  = 0.0f;
    s_sprites.col[i * 4 + 3]  = 0.8f;
  }

  print_cstr(STDOUT, "<Press Ctrl-C to exit>\n");

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
        s_sprites.col[i * 4 + 0]  = lerpf32(kcol, 0.0f, 1.0f);
        s_sprites.col[i * 4 + 2]  = lerpf32(kcol, 1.0f, 0.0f);

        total_kcol  += kcol;
      }
    }

    // Draw
    // Update clear color based on the temperature
    ntemp         = total_kcol / SPRITES_COUNT;
    clear_col[0]  = lerpf32(ntemp, 0.0f, 0.9f);
    clear_col[1]  = 0.0f;
    clear_col[2]  = lerpf32(ntemp, 0.9f, 0.0f);
    clear_col[3]  = 0.25f;

    glClearColor(clear_col[0], clear_col[1], clear_col[2], clear_col[3]);
    glClear(GL_COLOR_BUFFER_BIT);

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

    cgl_err = CGLFlushDrawable(glctx); // swap and present
    WARN_IF(cgl_err, "Failed CGLFlushDrawable ");
  }

  // Shutdown
  glDeleteShader(sprite_prog);
  glDeleteBuffers(1, &vert_bo);
  glDeleteBuffers(1, &col_bo);
  glDeleteVertexArrays(1, &vao);

  CGLDestroyContext(glctx);
  CGSReleaseWindow(cid, wid);

  if (did) {
    CGDisplayRelease(did);
  }

  exit(0);
}
