program lighting_maps_exercise2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, glfw31, gl, GLext, shader_m, matrix,
  UMyMatrixExt, Math, camera_u, UMyFPImage, FPImage;

const
  // settings
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

var
  // camera
  Camera : TCamera = nil;
  lastX: Single = SCR_WIDTH / 2.0;
  lastY: Single = SCR_HEIGHT / 2.0;
  firstMouse: Boolean = true;

  // timing
  deltaTime: Single = 0.0;
  lastFrame: Single = 0.0;
  currentFrame: Single;

  // lighting
  lightPos: Tvector3_single;

// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
procedure processInput(window: pGLFWwindow); cdecl;
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
  begin
    glfwSetWindowShouldClose(window, GLFW_TRUE);
  end;

  if glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS then
    Camera.ProcessKeyboard(Camera_Movement.FORWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS then
    Camera.ProcessKeyboard(Camera_Movement.BACKWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS then
    Camera.ProcessKeyboard(Camera_Movement.LEFT, deltaTime);
  if glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS then
    Camera.ProcessKeyboard(Camera_Movement.RIGHT, deltaTime);
end;

// glfw: whenever the mouse moves, this callback is called
// -------------------------------------------------------
procedure mouse_callback(window: pGLFWwindow; xpos, ypos: double); cdecl;
var
  xoffset: Single;
  yoffset: Single;
begin
  if firstMouse then
  begin
    lastX := xpos;
    lastY := ypos;
    firstMouse := false;
  end;

  xoffset := xpos - lastX;
  yoffset := lastY - ypos;
  lastX := xpos;
  lastY := ypos;

  Camera.ProcessMouseMovement(xoffset, yoffset);

end;

// glfw: whenever the mouse scroll wheel scrolls, this callback is called
// ----------------------------------------------------------------------
procedure scroll_callback(window: pGLFWwindow; xoffset, yoffset: double); cdecl;
begin
  Camera.ProcessMouseScroll(yoffset);
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


function loadTexture(path: string): GLuint;
var
  textureID: GLuint;
  format: GLenum;

  Image : TFPCompactImgBase;
  data: PByte;
begin
  glGenTextures(1, @textureID);

  Image := LoadImage(path, true);

  if Image <> nil then
  begin
      if Image is TMyRGB8BitImage then
      begin
        format := GL_RGB;
        data := PByte(TMyRGB8BitImage(Image).Data);
      end;

      if Image is TMyRGBA8BitImage then
      begin
        format := GL_RGBA;
        data := PByte(TMyRGBA8BitImage(Image).Data);
      end;

      glBindTexture(GL_TEXTURE_2D, textureID);
      glTexImage2D(GL_TEXTURE_2D, 0, format, Image.Width, Image.Height, 0, format, GL_UNSIGNED_BYTE, data);
      glGenerateMipmap(GL_TEXTURE_2D);

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      FreeAndNil(Image);
  end
  else
     Writeln('Texture failed to load at path: ' + path);

  Result := textureID;
end;

var
  window: pGLFWwindow;

  lightingShader  :TShader = nil;
  lightCubeShader :TShader = nil;

  // set up vertex data
  // ------------------
  vertices: array [0..287] of GLfloat = (
     // positions      //normals          // texture cords
     -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  0.0,  0.0,
      0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  1.0,  0.0,
      0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  1.0,  1.0,
      0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  1.0,  1.0,
     -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  0.0,  1.0,
     -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  0.0,  0.0,

     -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,  0.0,  0.0,
      0.5, -0.5,  0.5,  0.0,  0.0,  1.0,  1.0,  0.0,
      0.5,  0.5,  0.5,  0.0,  0.0,  1.0,  1.0,  1.0,
      0.5,  0.5,  0.5,  0.0,  0.0,  1.0,  1.0,  1.0,
     -0.5,  0.5,  0.5,  0.0,  0.0,  1.0,  0.0,  1.0,
     -0.5, -0.5,  0.5,  0.0,  0.0,  1.0,  0.0,  0.0,

     -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,  1.0,  0.0,
     -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,  1.0,  1.0,
     -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,  0.0,  1.0,
     -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,  0.0,  1.0,
     -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,  0.0,  0.0,
     -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,  1.0,  0.0,

      0.5,  0.5,  0.5,  1.0,  0.0,  0.0,  1.0,  0.0,
      0.5,  0.5, -0.5,  1.0,  0.0,  0.0,  1.0,  1.0,
      0.5, -0.5, -0.5,  1.0,  0.0,  0.0,  0.0,  1.0,
      0.5, -0.5, -0.5,  1.0,  0.0,  0.0,  0.0,  1.0,
      0.5, -0.5,  0.5,  1.0,  0.0,  0.0,  0.0,  0.0,
      0.5,  0.5,  0.5,  1.0,  0.0,  0.0,  1.0,  0.0,

     -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  0.0,  1.0,
      0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  1.0,  1.0,
      0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  1.0,  0.0,
      0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  1.0,  0.0,
     -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  0.0,  0.0,
     -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  0.0,  1.0,

     -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  0.0,  1.0,
      0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  1.0,  1.0,
      0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  1.0,  0.0,
      0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  1.0,  0.0,
     -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  0.0,  0.0,
     -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  0.0,  1.0
  );

  VBO, cubeVAO, lightCubeVAO: GLuint;
  diffuseMap, specularMap: GLuint;

  model: Tmatrix4_single;
  view: Tmatrix4_single;
  projection: Tmatrix4_single;

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

  // Create Camera object
  Camera := TCamera.Create(Vector3(0.0, 0.0, 3.0));

  glfwSetFramebufferSizeCallback(window, @framebuffer_size_callback);
  glfwSetCursorPosCallback(window, @mouse_callback);
  glfwSetScrollCallback(window, @scroll_callback);

  // tell GLFW to capture our mouse
  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

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

  //light source position
  // --------------------
  lightPos.init(1.2, 1.0, 2.0);

  // build and compile our shader zprogram
  // -------------------------------------
  lightingShader := TShader.Create('4.2.lighting_maps.vs', '4.2.lighting_maps.fs');
  lightCubeShader := TShader.Create('4.2.light_cube.vs', '4.2.light_cube.fs');

  // first, configure the cube's VAO (and VBO)
  glGenVertexArrays(1, @cubeVAO);
  glGenBuffers(1, @VBO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), @vertices, GL_STATIC_DRAW);

  glBindVertexArray(cubeVAO);

  // position attribute
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  // normal attribute
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(3 * sizeof(GLfloat)));
  glEnableVertexAttribArray(1);

  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(6 * sizeof(GLfloat)));
  glEnableVertexAttribArray(2);


  // second, configure the light's VAO (VBO stays the same; the vertices are the same for the light object which is also a 3D cube)
  glGenVertexArrays(1, @lightCubeVAO);
  glBindVertexArray(lightCubeVAO);

  // we only need to bind to the VBO (to link it with glVertexAttribPointer), no need to fill it; the VBO's data already contains all we need (it's already bound, but we do it again for educational purposes)
  glBindBuffer(GL_ARRAY_BUFFER, VBO);

  // note that we update the lamp's position attribute's stride to reflect the updated buffer data
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), PGLvoid(0));
  glEnableVertexAttribArray(0);

  // load textures (we now use a utility function to keep the code more organized)
  // -----------------------------------------------------------------------------
  diffuseMap := loadTexture('../../../resources/textures/container2.png');
  specularMap := loadTexture('../../../resources/textures/container2_specular.png');

  lightingShader.use();
  lightingShader.setInt('material.diffuse', 0);
  lightingShader.setInt('material.specular', 1);

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
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // also clear the depth buffer now!

    // be sure to activate shader when setting uniforms/drawing objects
    lightingShader.use();
    lightingShader.setVec3('light.position', lightPos);
    lightingShader.setVec3('viewPos', Camera.Position);

    // light properties
    lightingShader.setVec3('light.ambient', 0.2, 0.2, 0.2);
    lightingShader.setVec3('light.diffuse', 0.5, 0.5, 0.5);
    lightingShader.setVec3('light.specular', 1.0, 1.0, 1.0);

    // material properties
    lightingShader.setFloat('material.shininess', 32.0);

    // view/projection transformations
    projection := Perspective(DegToRad(Camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
    view := Camera.GetViewMatrix;
    lightingShader.setMat4('projection', projection);
    lightingShader.setMat4('view', view);

    // world transformation
    model.init_identity;
    lightingShader.setMat4('model', model);

    // bind diffuse map
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, diffuseMap);
    // bind specular map
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, specularMap);

    // render the cube
    glBindVertexArray(cubeVAO);
    glDrawArrays(GL_TRIANGLES, 0, 36);

    // also draw the lamp object
    lightCubeShader.use();
    lightCubeShader.setMat4('projection', projection);
    lightCubeShader.setMat4('view', view);
    model.init_identity;
    model := TranslateMatrix4(model, lightPos);
    model := ScaleMatrix4(model, Vector3(0.2, 0.2, 0.2));
    lightCubeShader.setMat4('model', model);

    glBindVertexArray(lightCubeVAO);
    glDrawArrays(GL_TRIANGLES, 0, 36);


    // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
    // -------------------------------------------------------------------------------
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // optional: de-allocate all resources once they've outlived their purpose:
  // ------------------------------------------------------------------------
  glDeleteVertexArrays(1, @cubeVAO);
  glDeleteVertexArrays(1, @lightCubeVAO);
  glDeleteBuffers(1, @VBO);

  FreeAndNil(lightingShader);
  FreeAndNil(lightCubeShader);
  FreeAndNil(Camera);

  // glfw: terminate, clearing all previously allocated GLFW resources.
  // ------------------------------------------------------------------
  glfwTerminate;

end.

