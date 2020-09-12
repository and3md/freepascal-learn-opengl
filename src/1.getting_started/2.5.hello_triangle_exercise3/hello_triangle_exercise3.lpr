program hello_triangle_exercise3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, gl, GLext, glfw31;

const
  // settings
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
procedure processInput(window: pGLFWwindow); cdecl;
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
  begin
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  end;
end;

// glfw: whenever the window size changed (by OS or user resize) this callback function executes
// ---------------------------------------------------------------------------------------------
procedure framebuffer_size_callback(window: pGLFWwindow; width, height: Integer); cdecl;
begin
  // make sure the viewport matches the new window dimensions; note that width and
  // height will be significantly larger than specified on retina displays.
  glViewport(0, 0, width, height);
end;


procedure showError(error: GLFW_INT; description: PChar); cdecl;
begin
  Writeln(description);
end;

var
  window: pGLFWwindow;

  // set up vertex data
  // ------------------
  firstTriangle: array [0..8] of GLfloat = (
  -0.9, -0.5, 0.0,  // left
  -0.0, -0.5, 0.0,  // right
  -0.45, 0.5, 0.0  // top
  );

  secondTriangle: array [0..8] of GLfloat = (
  // second triangle
   0.0, -0.5, 0.0,  // left
   0.9, -0.5, 0.0,  // right
   0.45, 0.5, 0.0   // top
  );

  VBOs, VAOs: array[0..1] of GLuint;

  vertexShader: GLuint;
  vertexShaderSource: PGLchar;

  fragmentShaderOrange: GLuint;
  fragmentShaderYellow: GLuint;
  fragmentShader1Source: PGLchar;
  fragmentShader2Source: PGLchar;

  shaderProgramOrange: GLuint;
  shaderProgramYellow: GLuint;

  success: GLint;
  infoLog : array [0..511] of GLchar;

begin
  // glfw: initialize and configure
  // ------------------------------
  glfwInit;
  glfwSetErrorCallback(@showError);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  // glfw window creation
  // --------------------
  window := glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, 'LearnOpenGL', nil, nil);

  if window = nil then
  begin
    Writeln('Failed to create GLFW window');
    glfwTerminate;
    Exit;
  end;
  glfwMakeContextCurrent(window);
  glfwSetFramebufferSizeCallback(window, @framebuffer_size_callback);

  // GLext: load all OpenGL function pointers
  // ----------------------------------------
  if Load_GL_version_3_3_CORE = false then
  begin
    Writeln('OpenGL 3.3 is not supported!');
    glfwTerminate;
    Exit;
  end;

  // build and compile our shader program
  // ------------------------------------
  // we skipped compile log checks this time for readability (if you do encounter issues, add the compile-checks! see previous code samples)
  vertexShaderSource := '#version 330 core'                                              + #10
                      + 'layout (location = 0) in vec3 position;'                        + #10
                      + ' '                                                              + #10
                      + 'void main()'                                                    + #10
                      + '{'                                                              + #10
                      + '  gl_Position = vec4(position.x, position.y, position.z, 1.0);' + #10
                      + '}'                                                              + #10;
  // fragment shader
  fragmentShader1Source := '#version 330 core'                       + #10
                         + 'out vec4 color;'                         + #10
                         + ''                                        + #10
                         + 'void main()'                             + #10
                         + '{'                                       + #10
                         + '  color = vec4(1.0f, 0.5f, 0.2f, 1.0f);' + #10
                         + '}'                                       + #10;
  // fragment shader
  fragmentShader2Source := '#version 330 core'                       + #10
                         + 'out vec4 color;'                         + #10
                         + ''                                        + #10
                         + 'void main()'                             + #10
                         + '{'                                       + #10
                         + '  color = vec4(1.0f, 1.0f, 0.0f, 1.0f);' + #10
                         + '}'                                       + #10;


  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  fragmentShaderOrange := glCreateShader(GL_FRAGMENT_SHADER);
  fragmentShaderYellow := glCreateShader(GL_FRAGMENT_SHADER);
  shaderProgramOrange := glCreateProgram();
  shaderProgramYellow := glCreateProgram();
  glShaderSource(vertexShader, 1, @vertexShaderSource, nil);
  glCompileShader(vertexShader);
  glShaderSource(fragmentShaderOrange, 1, @fragmentShader1Source, nil);
  glCompileShader(fragmentShaderOrange);
  glShaderSource(fragmentShaderYellow, 1, @fragmentShader2Source, nil);
  glCompileShader(fragmentShaderYellow);
  // link the first program object
  glAttachShader(shaderProgramOrange, vertexShader);
  glAttachShader(shaderProgramOrange, fragmentShaderOrange);
  glLinkProgram(shaderProgramOrange);
  // then link the second program object using a different fragment shader (but same vertex shader)
  // this is perfectly allowed since the inputs and outputs of both the vertex and fragment shaders are equally matched.
  glAttachShader(shaderProgramYellow, vertexShader);
  glAttachShader(shaderProgramYellow, fragmentShaderYellow);
  glLinkProgram(shaderProgramYellow);

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShaderOrange);
  glDeleteShader(fragmentShaderYellow);

  // set up buffer(s) and configure vertex attributes
  // ------------------------------------------------
  // VBOs & VAOs
  glGenVertexArrays(2, VAOs); // we can also generate multiple VAOs or buffers at the same time
  glGenBuffers(2, VBOs);

  // first triangle setup
  // --------------------
  glBindVertexArray(VAOs[0]);

  glBindBuffer(GL_ARRAY_BUFFER, VBOs[0]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(firstTriangle), @firstTriangle, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  //glBindVertexArray(0);// no need to unbind at all as we directly bind a different VAOs the next few lines

  glBindVertexArray(VAOs[1]);

  glBindBuffer(GL_ARRAY_BUFFER, VBOs[1]);
  glBufferData(GL_ARRAY_BUFFER, sizeof(secondTriangle), @secondTriangle, GL_STATIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);
  // glBindVertexArray(0); // not really necessary as well, but beware of calls that could affect VAOs while this one is bound (like binding element buffer objects, or enabling/disabling vertex attributes)

  // uncomment this call to draw in wireframe polygons.
  // glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

  // render loop
  // -----------
  while glfwWindowShouldClose(window) = GLFW_FALSE do
  begin
    // input
    // -----
    processInput(window);

    // render
    // ------
    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    // now when we draw the triangle we first use the vertex and orange fragment shader from the first program
     glUseProgram(shaderProgramOrange);
     // draw the first triangle using the data from our first VAO
     glBindVertexArray(VAOs[0]);
     glDrawArrays(GL_TRIANGLES, 0, 3);	// this call should output an orange triangle
     // then we draw the second triangle using the data from the second VAO
     // when we draw the second triangle we want to use a different shader program so we switch to the shader program with our yellow fragment shader.
     glUseProgram(shaderProgramYellow);
     glBindVertexArray(VAOs[1]);
     glDrawArrays(GL_TRIANGLES, 0, 3);	// this call should output a yellow triangle

    // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
    // -------------------------------------------------------------------------------
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // optional: de-allocate all resources once they've outlived their purpose:
  // ------------------------------------------------------------------------
  glDeleteVertexArrays(2, VAOs);
  glDeleteBuffers(2, VBOs);
  glDeleteProgram(shaderProgramOrange);
  glDeleteProgram(shaderProgramYellow);

  // glfw: terminate, clearing all previously allocated GLFW resources.
  // ------------------------------------------------------------------
  glfwTerminate;

end.

