// Start of opengl.h.

#define OPENGL_SUCCEED(e) opengl_succeed(e, #e, __FILE__, __LINE__)
#define SHADER_SUCCEED(e) shader_succeed(e, #e, __FILE__, __LINE__)

typedef GLXContext (*glXCreateContextAttribsARBProc)(Display*,
                                                    GLXFBConfig,
                                                    GLXContext,
                                                    Bool,
                                                    const int*);
typedef Bool (*glXMakeContextCurrentARBProc)(Display*,
                                             GLXDrawable,
                                             GLXDrawable,
                                             GLXContext);

static glXCreateContextAttribsARBProc glXCreateContextAttribsARB = 0;
static glXMakeContextCurrentARBProc   glXMakeContextCurrentARB   = 0;

struct opengl_config {
  int debugging;
  int profiling;
  int logging;

  const char* dump_program_to;
  const char* load_program_from;
  const char* dump_binary_to;
  const char* load_binary_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_threshold;

  int default_group_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  const char **size_vars;
  size_t      *size_values;
  const char **size_classes;

};

static void opengl_config_init(struct opengl_config *cfg,
                               int         num_sizes,
                               const char *size_names[],
                               const char *size_vars[],
                               size_t     *size_values,
                               const char *size_classes[]) {
  cfg->debugging = 0;
  cfg->logging   = 0;
  cfg->profiling = 0;

  cfg->dump_program_to   = NULL;
  cfg->load_program_from = NULL;
  cfg->dump_binary_to    = NULL;
  cfg->load_binary_from  = NULL;

  cfg->default_group_size = 256;
  cfg->default_num_groups = 128;
  cfg->default_tile_size  = 32;
  cfg->default_threshold  = 32*1024;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed  = 0;

  cfg->num_sizes    = num_sizes;
  cfg->size_names   = size_names;
  cfg->size_vars    = size_vars;
  cfg->size_values  = size_values;
  cfg->size_classes = size_classes;

}

struct opengl_context {
  struct opengl_config cfg;

  struct free_list free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_local_memory;
  size_t max_block_size;

  size_t lockstep_width;

  GLuint program;

  Display*   dpy;
  GLXContext glctx;

};

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = (char*) malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

// Read a file into a NUL-terminated string; returns NULL on error.
static char* slurp_file(const char *filename, size_t *size) {
  char *s;
  FILE *f = fopen(filename, "rb"); // To avoid Windows messing with linebreaks.
  if (f == NULL) return NULL;
  fseek(f, 0, SEEK_END);
  size_t src_size = ftell(f);
  fseek(f, 0, SEEK_SET);
  s = (char*) malloc(src_size + 1);
  if (fread(s, 1, src_size, f) != src_size) {
    free(s);
    s = NULL;
  } else {
    s[src_size] = '\0';
  }
  fclose(f);

  if (size) {
    *size = src_size;
  }

  return s;
}

static const char* opengl_error_string(GLenum err) {
  switch(err) {
    case GL_NO_ERROR:                      return "GL_NO_ERROR";
    case GL_INVALID_ENUM:                  return "INVALID_ENUM";
    case GL_INVALID_VALUE:                 return "INVALID_VALUE";
    case GL_INVALID_OPERATION:             return "INVALID_OPERATION";
    case GL_INVALID_FRAMEBUFFER_OPERATION: return "INVALID_FRAMEBUFFER_OPERATION";
    case GL_OUT_OF_MEMORY:                 return "OUT_OF_MEMORY";
    case GL_STACK_UNDERFLOW:               return "GL_STACK_UNDERFLOW";
    case GL_STACK_OVERFLOW:                return "GL_STACK_OVERFLOW";
    default:                               return "UNSPECIFIED_ERROR";
  }
}

static void opengl_succeed(GLenum ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret != GL_NO_ERROR) {
    panic(-1, "%s:%d: OpenGL call:\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opengl_error_string(ret));
  }
}

static void shader_succeed(int ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret) {
    panic(-1, "%s:%d: Shader call:\n  %s\nfailed with error code %d\n",
          file, line, call, ret);
  }
}

static int shader_link_succeed(GLuint program) {
  GLint success;

  glGetProgramiv(program, GL_LINK_STATUS, &success);

  if (!success) {
    GLint maxLen;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLen);

    GLchar infoLog[maxLen];
    glGetProgramInfoLog(program, maxLen, &maxLen, infoLog);
    printf("PROGRAM_LINKING_ERROR:\n%s\n", infoLog);

    // The program is futile at this point.
    glDeleteProgram(program);

    return 1;
  }
  return 0;
}

static int shader_compile_succeed(GLuint shader) {
  GLint success;

  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

  if (!success) {
    GLint maxLen;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &maxLen);

    GLchar infoLog[maxLen];
    glGetShaderInfoLog(shader, maxLen, &maxLen, infoLog);
    printf("SHADER_COMPILATION_ERROR:\n%s\n", infoLog);

    glDeleteShader(shader); // Avoid leaking the shader.

    return 1;
  }
  return 0;
}

static void setup_size_opengl(struct opengl_context *ctx) {

   int max_local_memory;
   int max_num_groups;
   int max_group_size;

   size_t max_block_size;

   glGetIntegerv(GL_MAX_COMPUTE_SHARED_MEMORY_SIZE,
                 &max_local_memory);
   glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS,
                 &max_num_groups);
   glGetIntegeri_v(GL_MAX_COMPUTE_WORK_GROUP_SIZE, 0,
                   &max_group_size);
   glGetInteger64v(GL_MAX_SHADER_STORAGE_BLOCK_SIZE,
                   &max_block_size);
   OPENGL_SUCCEED(glGetError());

   ctx->program           = 0;
   ctx->max_threshold     = 0;
   ctx->max_local_memory  = max_local_memory;
   ctx->max_num_groups    = max_num_groups;
   ctx->max_group_size    = max_group_size;
   ctx->max_block_size    = max_block_size;

  // Go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
      max_value = 0;
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }
}

static void setup_opengl(struct opengl_context *ctx,
                         const char *extra_build_opts[]) {

  static int visual_attribs[] = { None };

  int context_attribs[] = {
    GLX_CONTEXT_MAJOR_VERSION_ARB, 4,
    GLX_CONTEXT_MINOR_VERSION_ARB, 5,
    None
  };

  Display*     dpy     = XOpenDisplay(0);
  int          fbcount = 0;
  GLXFBConfig* fbc     = NULL;
  GLXContext   glctx;
  GLXPbuffer   pbuf;

  /* open display */
  if ( !(dpy = XOpenDisplay(0)) ) {
    fprintf(stderr, "Failed to open display\n");
    exit(1);
  }

  /* get framebuffer configs, any is usable (might want to add proper attribs) */
  if ( !(fbc = glXChooseFBConfig(dpy, DefaultScreen(dpy), visual_attribs,
                                 &fbcount) ) ) {
    fprintf(stderr, "Failed to get FBConfig\n");
    exit(1);
  }

  /* get the required extensions */
  glXCreateContextAttribsARB = (glXCreateContextAttribsARBProc)glXGetProcAddressARB(
                                (const GLubyte *) "glXCreateContextAttribsARB");
  glXMakeContextCurrentARB = (glXMakeContextCurrentARBProc)glXGetProcAddressARB(
                                (const GLubyte *) "glXMakeContextCurrent");
  if ( !(glXCreateContextAttribsARB && glXMakeContextCurrentARB) ) {
    fprintf(stderr, "missing support for GLX_ARB_create_context\n");
    XFree(fbc);
    exit(1);
  }

  /* create a context using glXCreateContextAttribsARB */
  if ( !( glctx = glXCreateContextAttribsARB(dpy, fbc[0], 0, True,
                                             context_attribs)) ) {
    fprintf(stderr, "Failed to create opengl context\n");
    XFree(fbc);
    exit(1);
  }

  /* create temporary pbuffer */
  int pbuffer_attribs[] = {
    GLX_PBUFFER_WIDTH,  800,
    GLX_PBUFFER_HEIGHT, 600,
    None
  };
  pbuf = glXCreatePbuffer(dpy, fbc[0], pbuffer_attribs);

  XFree(fbc);
  XSync(dpy, False);

  /* try to make it the current context */
  if ( !glXMakeContextCurrent(dpy, pbuf, pbuf, glctx) ) {
    /* some drivers does not support context without default framebuffer,
    *  so fallback on using the default window.
    */
    if ( !glXMakeContextCurrent(dpy, DefaultRootWindow(dpy),
                                DefaultRootWindow(dpy), glctx) ) {
      fprintf(stderr, "failed to make current\n");
      exit(1);
    }
  }

  if ( !gladLoadGLLoader((GLADloadproc)glXGetProcAddress) ) {
      fprintf(stderr, "glad: Failed to initialize OpenGL context\n");
      exit(1);
    }


  ctx->dpy   = dpy;
  ctx->glctx = glctx;

  free_list_init(&ctx->free_list);
  setup_size_opengl(ctx);
  OPENGL_SUCCEED(glGetError());

}

static void setup_shader(struct opengl_context *ctx,
                         const char *srcs[]) {

  char *gl_src = NULL;

  if (ctx->cfg.load_binary_from == NULL) {
    size_t src_size = 0;

    // We might have to read OpenGL source from somewhere else (used for debugging).
    if (ctx->cfg.load_program_from != NULL) {
      gl_src = slurp_file(ctx->cfg.load_program_from, NULL);
      assert(gl_src != NULL);
    }
    else if (ctx->cfg.load_binary_from == NULL) {
      // Construct the OpenGL source concatenating all the fragments.
      for (const char **src = srcs; src && *src; src++) {
        src_size += strlen(*src);
      }

      gl_src = (char*)malloc(src_size + 1);

      size_t n, i;
      for (i = 0, n = 0; srcs && srcs[i]; i++) {
        strncpy(gl_src+n, srcs[i], src_size-n);
        n += strlen(srcs[i]);
      }
      gl_src[src_size] = 0;
  }

    if (ctx->cfg.dump_program_to != NULL) {
      if (ctx->cfg.debugging) {
        fprintf(stderr, "Dumping OpenGL source to %s...\n", ctx->cfg.dump_program_to);
      }
      FILE *f = fopen(ctx->cfg.dump_program_to, "w");
      assert(f != NULL);
      fputs(gl_src, f);
      fclose(f);
    }

    // We might use the attach/detach method, instead of deleting and
    // creating entire program objects.
    if(ctx->program) {
      glDeleteProgram(ctx->program);
    }

    ctx->program = glCreateProgram();

    // Create the compute shader object.
    GLuint computeShader = glCreateShader(GL_COMPUTE_SHADER);

    // Create and compile the compute shader.
    //TODO: delete these
    FILE *fp;
    fp = fopen("temp.glsl", "w");
    if (fp) {
      fputs(gl_src, fp);
    }
    fclose(fp);
    const char* src_ptr = gl_src;
    glShaderSource(computeShader, 1, &src_ptr, NULL);
    glCompileShader(computeShader);
    SHADER_SUCCEED(shader_compile_succeed(computeShader));

    // Attach and link the shader against to the compute program.
    glAttachShader(ctx->program, computeShader);
    glLinkProgram(ctx->program);
    SHADER_SUCCEED(shader_link_succeed(ctx->program));

    // Detach shader after a successful link.
    glDetachShader(ctx->program, computeShader);

    // Delete the compute shader object.
    glDeleteShader(computeShader);
    OPENGL_SUCCEED(glGetError());
  }
  else {
    if (ctx->cfg.debugging) {
      fprintf(stderr, "Loading OpenGL binary from %s...\n", ctx->cfg.load_binary_from);
    }
    size_t binary_size;
    GLenum binary_format;
    unsigned char *gl_bin =
      (unsigned char*)slurp_file(ctx->cfg.load_binary_from, &binary_size);
    assert(gl_bin != NULL);
    const unsigned char *binaries = gl_bin;
    glGetProgramBinary(ctx->program, binary_size, NULL,
                       &binary_format, &binaries);
    OPENGL_SUCCEED(glGetError());
    glProgramBinary(ctx->program, binary_format,
                    binaries, binary_size);
    OPENGL_SUCCEED(glGetError());

  }
}

static GLenum opengl_free_all(struct opengl_context *ctx) {
  GLuint mem;
  GLenum error;
  free_list_pack(&ctx->free_list);
    while (free_list_first(&ctx->free_list, &mem) == 0) {
      glDeleteBuffers(1, &mem);
      error = glGetError();
      if (error != GL_NO_ERROR) {
        return error;
      }
    }

  return GL_NO_ERROR;
}


static GLenum opengl_release_context(struct opengl_context *ctx) {

  glXMakeCurrent(ctx->dpy, None, NULL);
  glXDestroyContext(ctx->dpy, ctx->glctx);

  GLenum error;
  error = glGetError();
  if (error != GL_NO_ERROR) {
    return error;
  }

  return GL_NO_ERROR;
}

static GLenum opengl_release_all(struct opengl_context *ctx) {

  opengl_release_context(ctx);
  opengl_free_all(ctx);
  free(ctx);

  GLenum error;
  error = glGetError();
  if (error != GL_NO_ERROR) {
    return error;
  }

  return GL_NO_ERROR;
}

static GLenum opengl_alloc(struct opengl_context *ctx,
                           size_t      min_size,
                           const char *tag,
                           GLuint     *mem_out) {
  GLenum error;
  size_t size;

  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  if (free_list_find(&ctx->free_list, tag, &size, mem_out) == 0) {
    // Successfully found a free block.  Is it big enough?
    //
    // FIXME: See `opencl_alloc(...)` in `opencl.h`
    if (size >= min_size) {
      return GL_NO_ERROR;
    } else {
      glDeleteBuffers(size, mem_out);
      error = glGetError();
      if (error != GL_NO_ERROR) {
        return error;
      }
    }
  }

  if(min_size < 0 || min_size > ctx->max_block_size) {
    printf("Requested memory block size: %zu exceeds maximum block size: %zu\n",
            min_size, ctx->max_block_size);
    opengl_release_all(ctx);
    exit(1);
  }

  GLuint ssbo;

  glCreateBuffers(1, &ssbo);
  OPENGL_SUCCEED(glGetError());

  error = glGetError();
  if (error != GL_NO_ERROR) {
    return error;
  }

  glNamedBufferData(ssbo, min_size, NULL, GL_DYNAMIC_DRAW);
  OPENGL_SUCCEED(glGetError());

  *mem_out = ssbo;

  return 0;
}

static GLenum opengl_free(struct opengl_context *ctx,
                          GLuint      mem,
                          const char *tag) {
  size_t size;
  GLuint existing_mem;
  GLenum error;

  // If there is already a block with this tag, then remove it.
  if (free_list_find(&ctx->free_list, tag, &size, &existing_mem) == 0) {
    glDeleteBuffers(1, &existing_mem);
    error = glGetError();
    if (error != GL_NO_ERROR) {
      return error;
    }
  }

  glGetNamedBufferParameteri64v(mem, GL_BUFFER_SIZE, (GLint64*)&size);
  error = glGetError();

  if (error == GL_NO_ERROR) {
    free_list_insert(&ctx->free_list, size, mem, tag);
  }

  return error;
}

// End of opengl.h.
