unit shader_m;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, GLext, matrix;

type

  { TShader }

  TShader = class
    public
      ID: GLuint;

      constructor Create(const vertexPath, fragmentPath: string);
      procedure use();
      procedure setBool(const name: string; value: Boolean);
      procedure setInt(const name: string; value: GLint);
      procedure setFloat(const name: string; value: GLfloat);
      procedure setVec2(const name:string; const value: Tvector2_single);
      procedure setVec2(const name:string; x, y: Single);
      procedure setVec3(const name:string; const value: Tvector3_single);
      procedure setVec3(const name:string; x, y, z: Single);
      procedure setVec4(const name:string; const value: Tvector4_single);
      procedure setVec4(const name:string; x, y, z, w: Single);
      procedure setMat2(const name:string; const value: Tmatrix2_single);
      procedure setMat3(const name:string; const value: Tmatrix3_single);
      procedure setMat4(const name:string; const value: Tmatrix4_single);

    private
      procedure checkCompileErrors(shader: GLuint; type_: string);

  end;


implementation

{ TShader }

// constructor generates the shader on the fly
// ------------------------------------------------------------------------
constructor TShader.Create(const vertexPath, fragmentPath: string);
var
  vShaderFile: TStringList;
  fShaderFile: TStringList;
  vShaderCode: PGLchar;
  fShaderCode: PGLchar;
  vertexCode: String;
  fragmentCode: String;
  vertex: GLuint;
  fragment: GLuint;
begin
  vShaderFile := nil;
  fShaderFile := nil;
  try
    try
      vShaderFile := TStringList.Create;
      fShaderFile := TStringList.Create;

      vShaderFile.LoadFromFile(vertexPath);
      fShaderFile.LoadFromFile(fragmentPath);

      vertexCode := vShaderFile.Text;
      fragmentCode := fShaderFile.Text;

    except
      Writeln('ERROR::SHADER::FILE_NOT_SUCCESFULLY_READ');
    end;
  finally
    FreeAndNil(vShaderFile);
    FreeAndNil(fShaderFile);
  end;

  vShaderCode := PGLchar(vertexCode);
  fShaderCode := PGLchar(fragmentCode);

  // 2. compile shaders
  // vertex shader
  vertex := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertex, 1, @vShaderCode, nil);
  glCompileShader(vertex);
  checkCompileErrors(vertex, 'VERTEX');

  // fragment Shader
  fragment := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragment, 1, @fShaderCode, nil);
  glCompileShader(fragment);
  checkCompileErrors(fragment, 'FRAGMENT');

  // shader Program
  ID := glCreateProgram();
  glAttachShader(ID, vertex);
  glAttachShader(ID, fragment);
  glLinkProgram(ID);
  checkCompileErrors(ID, 'PROGRAM');
  // delete the shaders as they're linked into our program now and no longer necessary
  glDeleteShader(vertex);
  glDeleteShader(fragment);
end;

// activate the shader
// ------------------------------------------------------------------------
procedure TShader.use();
begin
  glUseProgram(ID);
end;

// utility uniform functions
// ------------------------------------------------------------------------
procedure TShader.setBool(const name: string; value: Boolean);
begin
  glUniform1i(glGetUniformLocation(ID, PGLchar(name)), GLInt(value));
end;
// ------------------------------------------------------------------------
procedure TShader.setInt(const name: string; value: GLint);
begin
  glUniform1i(glGetUniformLocation(ID, PGLchar(name)), value);
end;
// ------------------------------------------------------------------------
procedure TShader.setFloat(const name: string; value: GLfloat);
begin
  glUniform1f(glGetUniformLocation(ID, PGLchar(name)), value);
end;

procedure TShader.setVec2(const name: string; const value: Tvector2_single);
begin
  glUniform2fv(glGetUniformLocation(ID, PGLchar(name)), 1, @value.data);
end;

procedure TShader.setVec2(const name: string; x, y: Single);
begin
  glUniform2f(glGetUniformLocation(ID, PGLchar(name)), x, y);
end;

procedure TShader.setVec3(const name: string; const value: Tvector3_single);
begin
  glUniform3fv(glGetUniformLocation(ID, PGLchar(name)), 1, @value.data);
end;

procedure TShader.setVec3(const name: string; x, y, z: Single);
begin
  glUniform3f(glGetUniformLocation(ID, PGLchar(name)), x, y, z);
end;

procedure TShader.setVec4(const name: string; const value: Tvector4_single);
begin
  glUniform4fv(glGetUniformLocation(ID, PGLchar(name)), 1, @value.data);
end;

procedure TShader.setVec4(const name: string; x, y, z, w: Single);
begin
  glUniform4f(glGetUniformLocation(ID, PGLchar(name)), x, y, z, w);
end;

procedure TShader.setMat2(const name: string; const value: Tmatrix2_single);
begin
  glUniformMatrix2fv(glGetUniformLocation(ID, PGLchar(name)), 1, GL_FALSE, @value.data);
end;

procedure TShader.setMat3(const name: string; const value: Tmatrix3_single);
begin
  glUniformMatrix3fv(glGetUniformLocation(ID, PGLchar(name)), 1, GL_FALSE, @value.data);
end;

procedure TShader.setMat4(const name: string; const value: Tmatrix4_single);
begin
  glUniformMatrix4fv(glGetUniformLocation(ID, PGLchar(name)), 1, GL_FALSE, @value.data);
end;

// utility function for checking shader compilation/linking errors.
// ------------------------------------------------------------------------
procedure TShader.checkCompileErrors(shader: GLuint; type_: string);
var
  success: GLint;
  infoLog : array [0..1023] of GLchar;
begin
  if type_ <> 'PROGRAM' then
  begin
    glGetShaderiv(shader, GL_COMPILE_STATUS, @success);
    if success <> GL_TRUE then
    begin
      glGetShaderInfoLog(shader, 1024, nil, @infoLog); //in static array @infoLog = @infoLog[0]
      Writeln('ERROR::SHADER_COMPILATION_ERROR of type: ' + type_ + LineEnding + infoLog + LineEnding + '-- --------------------------------------------------- -- ');
    end;
  end
  else
    begin
      glGetProgramiv(shader, GL_LINK_STATUS, @success);
      if success <> GL_TRUE then
      begin
        glGetProgramInfoLog(shader, 1024, nil, @infoLog);
        Writeln('ERROR::PROGRAM_COMPILATION_ERROR of type: ' + type_ + LineEnding + infoLog + LineEnding + '-- --------------------------------------------------- -- ');
      end;
    end;
end;

end.

