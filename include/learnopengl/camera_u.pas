unit camera_u;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, GLext, matrix;

type

// Defines several possible options for camera movement. Used as abstraction to stay away from window-system specific input methods
Camera_Movement = (
  FORWARD,
  BACKWARD,
  LEFT,
  RIGHT
);

const
  DEFAULT_YAW         = -90.0;
  DEFAULT_PITCH       = 0.0;
  DEFAULT_SPEED       = 2.5;
  DEFAULT_SENSITIVITY = 0.1;
  DEFAULT_ZOOM        = 45.0;

type

// An abstract camera class that processes input and calculates the corresponding Euler Angles, Vectors and Matrices for use in OpenGL
  TCamera = class
    public
      // camera Attributes
      Position: Tvector3_single;
      Front: Tvector3_single;
      Up: Tvector3_single;
      Right: Tvector3_single;
      WorldUp: Tvector3_single;
      // euler Angles
      Yaw: Single;
      Pitch: Single;
      // camera options
      MovementSpeed: Single;
      MouseSensitivity: Single;
      Zoom: Single;

      // constructor with vectors
      constructor Create(const position_: Tvector3_single);
      constructor Create(const position_, up_: Tvector3_single; yaw_: Single = DEFAULT_YAW; pitch_: Single = DEFAULT_PITCH);
      // constructor with scalar values
      constructor Create(posX, posY, posZ, upX, upY, upZ, yaw_, pitch_: Single);

      // returns the view matrix calculated using Euler Angles and the LookAt Matrix
      function GetViewMatrix:Tmatrix4_single;

      // processes input received from any keyboard-like input system. Accepts input parameter in the form of camera defined ENUM (to abstract it from windowing systems)
      procedure ProcessKeyboard(direction: Camera_Movement; deltaTime: Single);

      // processes input received from a mouse input system. Expects the offset value in both the x and y direction.
      procedure ProcessMouseMovement(xoffset, yoffset: Single; constrainPitch: GLboolean = GL_TRUE);

      // processes input received from a mouse scroll-wheel event. Only requires input on the vertical wheel-axis
      procedure ProcessMouseScroll(yoffset: Single);

    private
      procedure updateCameraVectors;


  end;


implementation

uses UMyMatrixExt, math;
{ TCamera }

constructor TCamera.Create(const position_: Tvector3_single);
var
  upVec: Tvector3_single;
begin
  upVec.init(0.0, 1.0, 0.0);
  Create(position_, upVec);
end;

// constructor with vectors
constructor TCamera.Create(const position_, up_: Tvector3_single; yaw_: Single; pitch_: Single);
begin
  Position := position_;
  WorldUp := up_;
  Yaw := yaw_;
  Pitch := pitch_;
  Front.init(0.0, 0.0, -1.0);
  MovementSpeed := DEFAULT_SPEED;
  MouseSensitivity := DEFAULT_SENSITIVITY;
  Zoom := DEFAULT_ZOOM;
end;

// constructor with scalar values
constructor TCamera.Create(posX, posY, posZ, upX, upY, upZ, yaw_, pitch_: Single);
begin
  Position.init(posX, posY, posZ);
  WorldUp.init(upX, upY, upZ);
  Yaw := yaw_;
  Pitch := pitch_;
  Front.init(0.0, 0.0, -1.0);
  MovementSpeed := DEFAULT_SPEED;
  MouseSensitivity := DEFAULT_SENSITIVITY;
  Zoom := DEFAULT_ZOOM;
end;

// returns the view matrix calculated using Euler Angles and the LookAt Matrix
function TCamera.GetViewMatrix: Tmatrix4_single;
begin
  Result := LookAt(Position, Position + Front, WorldUp);
end;

// processes input received from any keyboard-like input system. Accepts input parameter in the form of camera defined ENUM (to abstract it from windowing systems)
procedure TCamera.ProcessKeyboard(direction: Camera_Movement; deltaTime: Single);
var
  velocity: Single;
begin
  velocity := MovementSpeed * deltaTime;

  if direction = Camera_Movement.FORWARD then
    Position := Position + Front * velocity;
  if direction = Camera_Movement.BACKWARD then
    Position := Position - Front * velocity;
  if direction = Camera_Movement.LEFT then
    Position := Position - Right * velocity;
  if direction = Camera_Movement.RIGHT then
    Position := Position + Right * velocity;
end;

// processes input received from a mouse input system. Expects the offset value in both the x and y direction.
procedure TCamera.ProcessMouseMovement(xoffset, yoffset: Single; constrainPitch: GLboolean);
begin
  xoffset := xoffset * MouseSensitivity;
  yoffset := yoffset * MouseSensitivity;

  Yaw := Yaw + xoffset;
  Pitch := Pitch + yoffset;

  // make sure that when pitch is out of bounds, screen doesn't get flipped
  if constrainPitch = GL_TRUE then
  begin
    if Pitch > 89.0 then
      Pitch := 89.0;

    if Pitch < -89.0 then
      Pitch := -89.0;
  end;

  // update Front, Right and Up Vectors using the updated Euler angles
  updateCameraVectors;
end;

// processes input received from a mouse scroll-wheel event. Only requires input on the vertical wheel-axis
procedure TCamera.ProcessMouseScroll(yoffset: Single);
begin
  Zoom := Zoom - yoffset;
  if Zoom < 1.0 then
    Zoom := 1.0;
  if Zoom > 45.0 then
    Zoom := 45.0;
end;

procedure TCamera.updateCameraVectors;
begin
  Front.data[0] := cos(DegToRad(Yaw)) * cos(DegToRad(Pitch));
  Front.data[1] := sin(DegToRad(Pitch));
  Front.data[2] := sin(DegToRad(Yaw)) * cos(DegToRad(Pitch));
  Front := NormalizeVector3(Front);

  Right := NormalizeVector3(Front >< WorldUp);
  Up := NormalizeVector3(Right >< Front);
end;


end.

