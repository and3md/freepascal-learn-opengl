program shaders_interpolation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, glfw31, gl, GLext;

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
  vertices: array [0..17] of GLfloat = (
    // positions      // colors
    -0.5, -0.5, 0.0,  1.0, 0.0, 0.0,
     0.5, -0.5, 0.0,  0.0, 1.0, 0.0,
     0.0,  0.5, 0.0,  0.0, 0.0, 1.0
  );

  VBO, VAO: GLuint;

  vertexShader: GLuint;
  vertexShaderSource: PGLchar;

  fragmentShader: GLuint;
  fragmentShaderSource: PGLchar;

  shaderProgram: GLuint;

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
  // vertex shader
  vertexShaderSource := '#version 330 core'                     + #10
                      + 'layout (location = 0) in vec3 aPos;'   + #10
                      + 'layout (location = 1) in vec3 aColor;' + #10
                      + 'out vec3 ourColor;'                    + #10
                      + 'void main()'                           + #10
                      + '{'                                     + #10
                      + '   gl_Position = vec4(aPos, 1.0);'     + #10
                      + '   ourColor = aColor;'                 + #10
                      + '}'                                     + #10;

  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertexShader, 1, @vertexShaderSource, nil);
  glCompileShader(vertexShader);

  // check for shader compile errors
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);

  if success <> GL_TRUE then
  begin
    glGetShaderInfoLog(vertexShader, 512, nil, @infoLog);
    Writeln('ERROR::SHADER::VERTEX::COMPILATION_FAILED');
    Writeln(infoLog);
  end;

  // fragment shader
  fragmentShaderSource := '#version 330 core'                    + #10
                        + 'out vec4 FragColor;'                  + #10
                        + 'in vec3 ourColor;'                    + #10
                        + 'void main()'                          + #10
                        + '{'                                    + #10
                        + '   FragColor = vec4(ourColor, 1.0f);' + #10
                        + '}'                                 + #10;

  fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragmentShader, 1, @fragmentShaderSource, nil);
  glCompileShader(fragmentShader);

  // check for shader compile errors
  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, @success);

  if success <> GL_TRUE then
  begin
    glGetShaderInfoLog(fragmentShader, 512, nil, @infoLog);
    Writeln('ERROR::SHADER::FRAGMENT::COMPILATION_FAILED');
    Writeln(infoLog);
  end;

  // link shaders
  shaderProgram := glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);

  // check for link errors
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, @success);

  if success <> GL_TRUE then
  begin
    glGetProgramInfoLog(shaderProgram, 512, nil, @infoLog);
    Writeln('ERROR::SHADER::PROGRAM::LINKING_FAILED');
    Writeln(infoLog);
  end;

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  // set up buffer(s) and configure vertex attributes
  // ------------------------------------------------
  // VBO & VAO
  glGenVertexArrays(1, @VAO);
  glGenBuffers(1, @VBO);
  // bind the Vertex Array Object first, then bind and set vertex buffer(s), and then configure vertex attributes(s).
  glBindVertexArray(VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices, GL_STATIC_DRAW);

  // position attribute
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  // color attribute
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat), PGLvoid(3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);


  // You can unbind the VAO afterwards so other VAO calls won't accidentally modify this VAO, but this rarely happens. Modifying other
  // VAOs requires a call to glBindVertexArray anyways so we generally don't unbind VAOs (nor VBOs) when it's not directly necessary.
  // glBindVertexArray(0);

  // as we only have a single shader, we could also just activate our shader once beforehand if we want to
  glUseProgram(shaderProgram);

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


    glBindVertexArray(VAO); // seeing as we only have a single VAO there's no need to bind it every time, but we'll do so to keep things a bit more organized
    glDrawArrays(GL_TRIANGLES, 0, 3);
    //glBindVertexArray(0); // no need to unbind it every time

    // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
    // -------------------------------------------------------------------------------
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // optional: de-allocate all resources once they've outlived their purpose:
  // ------------------------------------------------------------------------
  glDeleteVertexArrays(1, @VAO);
  glDeleteBuffers(1, @VBO);
  glDeleteProgram(shaderProgram);

  // glfw: terminate, clearing all previously allocated GLFW resources.
  // ------------------------------------------------------------------
  glfwTerminate;

end.

