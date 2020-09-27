program camera_exercise2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, glfw31, gl, GLext, shader_m, UMyFPImage, matrix, UMyMatrixExt, Math;

const
  // settings
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

var
  cameraPos, cameraFront, cameraUp: Tvector3_single;
  deltaTime: Single = 0.0;
  lastFrame: Single = 0.0;
  currentFrame: Single;

// Custom implementation of the LookAt function
function calculate_lookAt_matrix(const Position, Target, WorldUp: Tvector3_single): Tmatrix4_single;
var
  XAxis: Tvector3_single;
  YAxis: Tvector3_single;
  ZAxis: Tvector3_single;
  Translation: Tmatrix4_single;
  Rotation: Tmatrix4_single;
begin
  // 1. Position = known
  // 2. Calculate cameraDirection
  ZAxis := NormalizeVector3(Position - Target);
  // 3. Get positive right axis vector
  XAxis := NormalizeVector3(NormalizeVector3(WorldUp) >< ZAxis);
  // 4. Calculate camera up vector
  YAxis := ZAxis >< XAxis;

  // Create translation and rotation matrix
  // In glm we access elements as mat[col][row] due to column-major layout
  Translation.init_identity;
  Translation.data[3, 0] := -Position.data[0];
  Translation.data[3, 1] := -Position.data[1];
  Translation.data[3, 2] := -Position.data[2];
  Rotation.init_identity;
  Rotation.data[0, 0] := XAxis.data[0];
  Rotation.data[1, 0] := XAxis.data[1];
  Rotation.data[2, 0] := XAxis.data[2];

  Rotation.data[0, 1] := YAxis.data[0];
  Rotation.data[1, 1] := YAxis.data[1];
  Rotation.data[2, 1] := YAxis.data[2];

  Rotation.data[0, 2] := ZAxis.data[0];
  Rotation.data[1, 2] := ZAxis.data[1];
  Rotation.data[2, 2] := ZAxis.data[2];

  // Return lookAt matrix as combination of translation and rotation matrix
  Result := Translation * Rotation;
end;


// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
procedure processInput(window: pGLFWwindow); cdecl;
var
  cameraSpeed: Single;
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
  begin
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  end;

  cameraSpeed := 2.5 * deltaTime;
  if glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS then
    cameraPos := cameraPos + (cameraFront * cameraSpeed);
  if glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS then
    cameraPos := cameraPos - (cameraFront * cameraSpeed);
  if glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS then
    cameraPos := cameraPos - NormalizeVector3(cameraFront >< cameraUp) * cameraSpeed;
  if glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS then
    cameraPos := cameraPos + NormalizeVector3(cameraFront >< cameraUp) * cameraSpeed;
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
  vertices: array [0..179] of GLfloat = (
     // positions      // texture coords
     -0.5, -0.5, -0.5,  0.0, 0.0,
      0.5, -0.5, -0.5,  1.0, 0.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
     -0.5,  0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 0.0,

     -0.5, -0.5,  0.5,  0.0, 0.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 1.0,
      0.5,  0.5,  0.5,  1.0, 1.0,
     -0.5,  0.5,  0.5,  0.0, 1.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,

     -0.5,  0.5,  0.5,  1.0, 0.0,
     -0.5,  0.5, -0.5,  1.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,
     -0.5,  0.5,  0.5,  1.0, 0.0,

      0.5,  0.5,  0.5,  1.0, 0.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5,  0.5,  0.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 0.0,

     -0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5, -0.5,  1.0, 1.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,

     -0.5,  0.5, -0.5,  0.0, 1.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5,  0.5,  0.5,  1.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 0.0,
     -0.5,  0.5,  0.5,  0.0, 0.0,
     -0.5,  0.5, -0.5,  0.0, 1.0
  );

  cubePositions : array of Tvector3_single;

  VBO, VAO: GLuint;
  texture1, texture2: GLuint;
  ImageRGB: TMyRGB8BitImage;
  ImageRGBA: TMyRGBA8BitImage;

  model: Tmatrix4_single;
  view: Tmatrix4_single;
  projection: Tmatrix4_single;
  vec3: Tvector3_single;
  I: Integer;
  angle: Single;


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

  // configure global opengl state
  // -----------------------------
  glEnable(GL_DEPTH_TEST);

  // build and compile our shader zprogram
  // -------------------------------------
  ourShader := TShader.Create('7.2.camera.vs', '7.2.camera.fs');

  // set up buffer(s) and configure vertex attributes
  // ------------------------------------------------

  // world space positions of our cubes
  SetLength(cubePositions, 10);
  cubePositions[0].init(0.0, 0.0, 0.0);
  cubePositions[1].init(2.0, 5.0, -15.0);
  cubePositions[2].init(-1.5, -2.2, -2.5);
  cubePositions[3].init(-3.8, -2.0, -12.3);
  cubePositions[4].init(2.4, -0.4, -3.5);
  cubePositions[5].init(-1.7, 3.0, -7.5);
  cubePositions[6].init(1.3, -2.0, -2.5);
  cubePositions[7].init(1.5, 2.0, -2.5);
  cubePositions[8].init(1.5, 0.2, -1.5);
  cubePositions[9].init(-1.3, 1.0, -1.5);

  glGenVertexArrays(1, @VAO);
  glGenBuffers(1, @VBO);

  glBindVertexArray(VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices, GL_STATIC_DRAW);

  // position attribute
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  // color attribute
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(GLfloat), PGLvoid(3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  // load and create a texture
  // -------------------------

  // texture 1
  // ---------
  glGenTextures(1, @texture1);
  glBindTexture(GL_TEXTURE_2D, texture1); // all upcoming GL_TEXTURE_2D operations now have effect on this texture object
  // set the texture wrapping parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);	// set texture wrapping to GL_REPEAT (default wrapping method)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  // set texture filtering parameters
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

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
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // load image, create texture and generate mipmaps
  ImageRGBA := LoadPNGImage('../../../resources/textures/awesomeface.png', true);
  try
    if ImageRGBA<> nil then
    begin
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
  ourShader.setInt('texture1', 0);
  // or set it via the texture class
  ourShader.setInt('texture2', 1);

  projection.init_identity;
  projection := Perspective(DegToRad(45), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
  ourShader.setMat4('projection', projection);

  // camera start settings
  cameraPos.init(0.0, 0.0, 3.0);
  cameraFront.init(0.0, 0.0, -1.0);
  cameraUp.init(0.0, 1.0, 0.0);

  // render loop
  // -----------
  while glfwWindowShouldClose(window) = GLFW_FALSE do
  begin
    // per-frame time logic
    // --------------------
    currentFrame := glfwGetTime;
    deltaTime := currentFrame - lastFrame;
    lastFrame := currentFrame;

    // input
    // -----
    processInput(window);

    // render
    // ------
    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // also clear the depth buffer now!


    // bind textures on corresponding texture units
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture1);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, texture2);

    // activate shader
    ourShader.use();

    // camera/view transformation

    view := calculate_lookAt_matrix(cameraPos, cameraPos + cameraFront, cameraUp);
    ourShader.setMat4('view', view);

    // render boxes
    glBindVertexArray(VAO);
    for i := Low(cubePositions) to High(cubePositions) do
    begin
      model.init_identity;
      model := TranslateMatrix4(model, cubePositions[i]);

      angle := 20.0 * i;

      vec3.init(1.0, 0.3, 0.5);
      model := RotateMatrix4(model, DegToRad(angle), vec3);
      ourShader.setMat4('model', model);

      glDrawArrays(GL_TRIANGLES, 0, 36);
    end;

    // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
    // -------------------------------------------------------------------------------
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // optional: de-allocate all resources once they've outlived their purpose:
  // ------------------------------------------------------------------------
  glDeleteVertexArrays(1, @VAO);
  glDeleteBuffers(1, @VBO);

  FreeAndNil(ourShader);

  // glfw: terminate, clearing all previously allocated GLFW resources.
  // ------------------------------------------------------------------
  glfwTerminate;

end.

