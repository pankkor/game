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
#define true                1
#define false               0

#define U64_MAX             -1UL

#define GL_SILENCE_DEPRECATION
#include <OpenGL/OpenGL.h>
#include <OpenGL/gl.h>
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
// Memory
// --------------------------------------
enum {BUMP_BUF_SIZE = 256 * 1024 * 1024};
static u8 s_bump_buf[BUMP_BUF_SIZE];
static i32 s_bump_offset = 0;

void *bump_alloc(i32 bytes) {
  u8 *buf = s_bump_buf + s_bump_offset;
  s_bump_offset += bytes;
  return buf;
}

void bump_dealloc(i32 bytes) {
  s_bump_offset -= bytes;
}

// static const char* vertex_shader_text =
//   "precision lowp float;"
//   "uniform mat4 uMVP;"
//   "attribute vec4 aPos;"
//   "attribute vec3 aCol;"
//   "varying vec3 vCol;"
//   "void main()"
//   "{"
//       "vCol = aCol;"
//       "gl_Position = uMVP * aPos;"
//   "}";
//
// static const char* fragment_shader_text =
//   "precision lowp float;"
//   "varying vec3 vCol;"
//   "void main()"
//   "{"
//       "gl_FragColor = vec4(vCol, 1.0);"
//   "}";
//
// typedef struct Vertex { float x, y, r, g, b; } Vertex;
// static GLuint program, vertex_buffer;
// static GLint uMVP_location, aPos_location, aCol_location;
//
// i32 init(void) {
  // glViewport(0, 0, 640, 480);

  // GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
  // glShaderSource(vertex_shader, 1, &vertex_shader_text, 0);
  // glCompileShader(vertex_shader);
  //
  // GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
  // glShaderSource(fragment_shader, 1, &fragment_shader_text, 0);
  // glCompileShader(fragment_shader);
  //
  // program = glCreateProgram();
  // glAttachShader(program, vertex_shader);
  // glAttachShader(program, fragment_shader);
  // glLinkProgram(program);
  //
  // uMVP_location = glGetUniformLocation(program, "uMVP");
  // aPos_location = glGetAttribLocation(program, "aPos");
  // aCol_location = glGetAttribLocation(program, "aCol");
  //
  // glGenBuffers(1, &vertex_buffer);
  // glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  //
  // glEnableVertexAttribArray(aPos_location);
  // glVertexAttribPointer(aPos_location, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);
  // glEnableVertexAttribArray(aCol_location);
  // glVertexAttribPointer(aCol_location, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)(sizeof(float) * 2));

  // return 0;
// }

// void draw(u32 dt) {
//   (void)dt;
//   // f32 f = ((dt % 1000) / 1000.0f);
//
//   glClearColor(0, 255, 255, 0);
//   glClear(GL_COLOR_BUFFER_BIT);

  // Vertex vertices[3] =
  // {
  //     { -0.6f, -0.4f, 1.f, 0.f, 0.f },
  //     {  0.6f, -0.4f, 0.f, 0.f, 1.f },
  //     {   0.f,  0.6f, 1.f, 1.f, 1.f },
  // };
  // // vertices[0].r = 0.5f + sinf(f * 3.14159f * 2.0f) * 0.5f;
  // // vertices[1].b = 0.5f + cosf(f * 3.14159f * 2.0f) * 0.5f;
  // vertices[0].r = 0.5f + f * 0.5f;
  // vertices[1].b = 0.5f + f * 0.5f;
  // glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  // glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
  //
  // GLfloat mvp[4*4] = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1 };
  // glUseProgram(program);
  // glUniformMatrix4fv(uMVP_location, 1, GL_FALSE, mvp);
  // glDrawArrays(GL_TRIANGLES, 0, 3);
// }

/*
#define GL_GLEXT_PROTOTYPES
#define EGL_EGLEXT_PROTOTYPES
#include <GL/gl.h>
#include <math.h>

// Functions defined in loader.js
void WAJS_SetupCanvas(int width, int height);
unsigned int WAJS_GetTime();

static const char* vertex_shader_text =
    "precision lowp float;"
    "uniform mat4 uMVP;"
    "attribute vec4 aPos;"
    "attribute vec3 aCol;"
    "varying vec3 vCol;"
    "void main()"
    "{"
        "vCol = aCol;"
        "gl_Position = uMVP * aPos;"
    "}";

static const char* fragment_shader_text =
    "precision lowp float;"
    "varying vec3 vCol;"
    "void main()"
    "{"
        "gl_FragColor = vec4(vCol, 1.0);"
    "}";

typedef struct Vertex { float x, y, r, g, b; } Vertex;
static GLuint program, vertex_buffer;
static GLint uMVP_location, aPos_location, aCol_location;

// This function is called at startup
int main(int argc, char *argv[])
{
    WAJS_SetupCanvas(640, 480);
    glViewport(0, 0, 640, 480);

    GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &vertex_shader_text, 0);
    glCompileShader(vertex_shader);

    GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_shader_text, 0);
    glCompileShader(fragment_shader);

    program = glCreateProgram();
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);

    uMVP_location = glGetUniformLocation(program, "uMVP");
    aPos_location = glGetAttribLocation(program, "aPos");
    aCol_location = glGetAttribLocation(program, "aCol");

    glGenBuffers(1, &vertex_buffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);

    glEnableVertexAttribArray(aPos_location);
    glVertexAttribPointer(aPos_location, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);
    glEnableVertexAttribArray(aCol_location);
    glVertexAttribPointer(aCol_location, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)(sizeof(float) * 2));

    return 0;
}

// This function is called by loader.js every frame
void WAFNDraw()
{
    float f = ((WAJS_GetTime() % 1000) / 1000.0f);

    glClear(GL_COLOR_BUFFER_BIT);

    Vertex vertices[3] =
    {
        { -0.6f, -0.4f, 1.f, 0.f, 0.f },
        {  0.6f, -0.4f, 0.f, 0.f, 1.f },
        {   0.f,  0.6f, 1.f, 1.f, 1.f },
    };
    vertices[0].r = 0.5f + sinf(f * 3.14159f * 2.0f) * 0.5f;
    vertices[1].b = 0.5f + cosf(f * 3.14159f * 2.0f) * 0.5f;
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    GLfloat mvp[4*4] = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1 };
    glUseProgram(program);
    glUniformMatrix4fv(uMVP_location, 1, GL_FALSE, mvp);
    glDrawArrays(GL_TRIANGLES, 0, 3);
}
*/

// --------------------------------------
// syscall
// --------------------------------------
i64 syscall1(i64 sys_num, i64 a0) {
  i64 ret;
  __asm__ volatile (
    "mov x16, %[sys_num]\n"   // syscall number
    "mov x0, %[a0]\n"   // a0
    "svc 0x80\n"
    "mov %0, x0\n"
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
    "mov x0, %[a0]\n"
    "mov x1, %[a1]\n"
    "svc 0x80\n"
    "mov %0, x0\n"
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
    "mov x0, %[a0]\n"
    "mov x1, %[a1]\n"
    "mov x2, %[a2]\n"
    "svc 0x80\n"
    "mov %0, x0\n"
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

__attribute__((noreturn)) void exit(i32 ec) {
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

#define STR1(s) # s
#define STR(s) STR1(s)

// Debug and Release assert
#define EXPECT(condition, msg) expect_msg(!!(condition), \
    __FILE__ ":" STR(__LINE__) ": (" STR(condition) ") expected to be true\n"\
    msg)

static inline void expect_msg(int condition, const char *msg) {
  if (!condition) {
    print_cstr(STDERR, msg);
#if defined(_MSC_VER)
    __debugbreak();
#elif defined(__clang__)
    __builtin_debugtrap();
#else
    // gcc doesn't have __builtin_debugtrap equivalent
    // __builtin_trap generates SIGILL and code after it will be optmized away.
    __builtin_trap();
#endif
  }
}

// --------------------------------------
// Entry point
// --------------------------------------
void start(void) {
  // Create a window using Core Graphics private API
  CGError err;
  CGLError glerr;

  CGRect rect = CGRectMake(100.0, 100.0, 800.0, 600.0);
  CGSRegionRef region = NULL;
  err = CGSNewRegionWithRect(&rect, &region);
  EXPECT(!err, "Failed to create region\n");

  CGWindowID wid = 0;
  CGSConnectionID cid = CGSMainConnectionID();
  err = CGSNewWindow(cid, kCGBackingStoreBuffered, 0.0, 0.0, region, &wid);
  EXPECT(!err, "Failed to create window\n");

  err = CGSSetWindowLevel(cid, wid, kCGMaximumWindowLevel);
  EXPECT(!err, "Failed to set window level\n");

  err = CGSOrderWindow(cid, wid, kCGSOrderIn, 0);
  EXPECT(!err, "Failed to order window\n");

  err = CGSSetWindowOpacity(cid, wid, 0);
  EXPECT(!err, "Failed to set window opacity\n");

  // Create OpenGL glctx
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

  GLint v_sync_enabled = 1;
  CGLSetParameter(glctx, kCGLCPSwapInterval, &v_sync_enabled);

  GLint surface_opacity = 0;
  CGLSetParameter(glctx, kCGLCPSurfaceOpacity, &surface_opacity);

  CGSSurfaceID sid;
  err = CGSAddSurface(cid, wid, &sid);
  EXPECT(!err, "Failed to add surface\n");

  glerr = CGSSetSurfaceBounds(cid, wid, sid, CGRectMake(0, 0, 800, 600));
  EXPECT(!glerr, "Failed to set surface bounds\n");

  glerr = CGSOrderSurface(cid, wid, sid, 1, 0);
  EXPECT(!glerr, "Failed to order surface bounds\n");

  glerr = CGLSetSurface(glctx, cid, wid, sid);
  EXPECT(!glerr, "Failed to set surface\n");

  GLint is_drawable = 0;
  glerr = CGLGetParameter(glctx, kCGLCPHasDrawable, &is_drawable);
  EXPECT(!glerr, "Failed to get is drawable parameter\n");

  CGLSetCurrentContext(glctx);

  const GLubyte* version_cstr = glGetString(GL_VERSION);
  print_cstr(STDOUT, "OpenGL version: \n");
  print_cstr(STDOUT, (const char *)version_cstr);
  print_cstr(STDOUT, "\n");

  while (1) {
    glViewport(0, 0, 800, 600);
    glClearColor(0.0f, 0.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glerr = glGetError();
    if (glerr) {
      print_cstr(STDERR, "glGetError(): ");
      print_i64 (STDERR, glerr);
      print_cstr(STDERR, "\n");
    }

    glerr = CGLFlushDrawable(glctx);
    if (glerr) {
      print_cstr(STDERR, "Failed CGLFlushDrawable ");
      print_i64 (STDERR, glerr);
      print_cstr(STDERR, "\n");
    }

    usleep(16000);
  }

  CGLDestroyContext(glctx);
  CGSReleaseWindow(cid, wid);

  exit(0);
}
