program textures_exercise3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, glfw31, gl, GLext, shader_s, UMyFPImage;

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

  ourShader :TShader;

  // set up vertex data
  // ------------------
  vertices: array [0..31] of GLfloat = (
     // positions      // colors        // texture coords (note that we changed them to 'zoom in' on our texture image)
     0.5,  0.5, 0.0,   1.0, 0.0, 0.0,   0.55, 0.55, // top right
     0.5, -0.5, 0.0,   0.0, 1.0, 0.0,   0.55, 0.45, // bottom right
    -0.5, -0.5, 0.0,   0.0, 0.0, 1.0,   0.45, 0.45, // bottom left
    -0.5,  0.5, 0.0,   1.0, 1.0, 0.0,   0.45, 0.55  // top left
  );

  indices: array [0..5] of GLuint = (
    0, 1, 3, // first triangle
    1, 2, 3  // second triangle
  );

  VBO, VAO, EBO: GLuint;
  texture1, texture2: GLuint;
  ImageRGB: TMyRGB8BitImage;
  ImageRGBA: TMyRGBA8BitImage;

begin
  // glfw: initialize and configure
  // ------------------------------
  glfwInit;
  glfwSetErrorCallback(@showError);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  {$ifdef DARWIN}
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  {$endif}

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

  // build and compile our shader zprogram
  // -------------------------------------
  ourShader := TShader.Create('4.4.texture.vs', '4.4.texture.fs');

  // set up buffer(s) and configure vertex attributes
  // ------------------------------------------------
  glGenVertexArrays(1, @VAO);
  glGenBuffers(1, @VBO);
  glGenBuffers(1, @EBO);

  glBindVertexArray(VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices, GL_STATIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), @indices, GL_STATIC_DRAW);


  // position attribute
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  // color attribute
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  // texture coord attribute
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(6 * sizeof(GLfloat)));
  glEnableVertexAttribArray(2);

  // load and create a texture
  // -------------------------

  // texture 1
  // ---------
  glGenTextures(1, @texture1);
  glBindTexture(GL_TEXTURE_2D, texture1); // all upcoming GL_TEXTURE_2D operations now have effect on this texture object
  // set the texture wrapping parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); // note that we set the container wrapping method to GL_CLAMP_TO_EDGE
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  // set texture filtering parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); // set texture filtering to nearest neighbor to clearly see the texels/pixels
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  // load image, create texture and generate mipmaps
  ImageRGB := LoadJpegImage('../../../resources/textures/container.jpg', true);
  try
    if ImageRGB<> nil then
    begin
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, ImageRGB.Width, ImageRGB.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, ImageRGB.Data);
      glGenerateMipmap(GL_TEXTURE_2D);
    end;
  finally
    FreeAndNil(ImageRGB);
  end;

  // texture 2
  // ---------
  glGenTextures(1, @texture2);
  glBindTexture(GL_TEXTURE_2D, texture2); // all upcoming GL_TEXTURE_2D operations now have effect on this texture object
  // set the texture wrapping parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);	// set texture wrapping to GL_REPEAT (default wrapping method)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  // set texture filtering parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); // set texture filtering to nearest neighbor to clearly see the texels/pixels
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  // load image, create texture and generate mipmaps
  ImageRGBA := LoadPNGImage('../../../resources/textures/awesomeface.png', true);
  try
    if ImageRGBA<> nil then
    begin
      // note that the awesomeface.png has transparency and thus an alpha channel, so make sure to tell OpenGL the data type is of GL_RGBA
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ImageRGBA.Width, ImageRGBA.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, ImageRGBA.Data);
      glGenerateMipmap(GL_TEXTURE_2D);
    end;
  finally
    FreeAndNil(ImageRGBA);
  end;

  // tell opengl for each sampler to which texture unit it belongs to (only has to be done once)
  // -------------------------------------------------------------------------------------------

  ourShader.use(); // don't forget to activate/use the shader before setting uniforms!
  // either set it manually like so:
  glUniform1i(glGetUniformLocation(ourShader.ID, 'texture1'), 0);
  // or set it via the texture class
  ourShader.setInt('texture2', 1);

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

    // bind textures on corresponding texture units
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture1);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, texture2);

    // render container
    ourShader.use();
    glBindVertexArray(VAO); // seeing as we only have a single VAO there's no need to bind it every time, but we'll do so to keep things a bit more organized
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, nil);
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
  glDeleteBuffers(1, @EBO);

  FreeAndNil(ourShader);

  // glfw: terminate, clearing all previously allocated GLFW resources.
  // ------------------------------------------------------------------
  glfwTerminate;

end.

