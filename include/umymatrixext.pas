unit UMyMatrixExt;

{$mode objfpc}{$H+}

{ Simple Extension to FPC matrix unit.
  Copyright (c) 2020 by Andrzej Kilijański (digitalkarabela.com)
  Public domain license.
  No warranty, use at your own risk.
}


interface

uses
  Classes, SysUtils, matrix, Math;

procedure PrintMatrix4(const Name: String; const M: Tmatrix4_single);
function TranslateMatrix4(const M: Tmatrix4_single; const V: Tvector3_single): Tmatrix4_single;
function RotateMatrix4(const M: Tmatrix4_single; Angle: Single; const Axis: Tvector3_single): Tmatrix4_single;
function ScaleMatrix4(const M: Tmatrix4_single; const V: Tvector3_single): Tmatrix4_single;
function NormalizeVector3(const V: Tvector3_single):Tvector3_single;
function Perspective(Fov, AspectRatio, NearDist, FarDist: Single): Tmatrix4_single;
function LookAt(const Position, Target, WorldUp: Tvector3_single): Tmatrix4_single;

implementation

procedure PrintMatrix4(const Name: String; const M: Tmatrix4_single);
var
  I, J: Integer;
begin
  WriteLn(Name, ' = [');
  for J := 0 to 3 do
  begin
    for I := 0 to 3 do
      Write(M.data[I, J]:8:2);
     WriteLn;
  end;
  WriteLn(']');
end;

function TranslateMatrix4(const M: Tmatrix4_single; const V: Tvector3_single): Tmatrix4_single;
var
  T: Tmatrix4_single;
begin
  T.init_identity;
  T.data[3, 0] := V.data[0];
  T.data[3, 1] := V.data[1];
  T.data[3, 2] := V.data[2];

  Result := T * M;
end;

function RotateMatrix4(const M: Tmatrix4_single; Angle: Single; const Axis: Tvector3_single): Tmatrix4_single;
var
  Rotate: Tmatrix4_single;
  NormalizedAxis: Tvector3_single;
  Cosinus, Sinus: Single;
begin
  Cosinus := Cos(Angle);
  Sinus := Sin(Angle);

  NormalizedAxis := NormalizeVector3(Axis);

  Rotate.init_identity;
  Rotate.data[0, 0] := Cosinus + NormalizedAxis.data[0] * NormalizedAxis.data[0] * (1 - Cosinus);
  Rotate.data[0, 1] := NormalizedAxis.data[1] * NormalizedAxis.data[0] * (1 - Cosinus) + NormalizedAxis.data[2] * Sinus;
  Rotate.data[0, 2] := NormalizedAxis.data[2] * NormalizedAxis.data[0] * (1 - Cosinus) - NormalizedAxis.data[1] * Sinus;

  Rotate.data[1, 0] := NormalizedAxis.data[0] * NormalizedAxis.data[1] * (1 - Cosinus) - NormalizedAxis.data[2] * Sinus;
  Rotate.data[1, 1] := Cosinus + NormalizedAxis.data[1] * NormalizedAxis.data[1] * (1 - Cosinus);
  Rotate.data[1, 2] := NormalizedAxis.data[2] * NormalizedAxis.data[1] * (1 - Cosinus) + NormalizedAxis.data[0] * Sinus;


  Rotate.data[2, 0] := NormalizedAxis.data[0] * NormalizedAxis.data[2] * (1 - Cosinus) + NormalizedAxis.data[1] * Sinus;
  Rotate.data[2, 1] := NormalizedAxis.data[1] * NormalizedAxis.data[2] * (1 - Cosinus) - NormalizedAxis.data[0] * Sinus;
  Rotate.data[2, 2] := Cosinus + NormalizedAxis.data[2] * NormalizedAxis.data[2] * (1 - Cosinus);

  Result := Rotate * M;
end;

function ScaleMatrix4(const M: Tmatrix4_single; const V: Tvector3_single): Tmatrix4_single;
var
  Scale : Tmatrix4_single;
begin
  Scale.init_zero;

  Scale.data[0, 0] := V.data[0];
  Scale.data[1, 1] := V.data[1];
  Scale.data[2, 2] := V.data[2];
  Scale.data[3, 3] := 1.0;

  Result := Scale * M;
end;

function NormalizeVector3(const V: Tvector3_single): Tvector3_single;
var
  Length: Single;
begin
  Length := V.length;

  if Length > 0 then
  begin
    Result.data[0] := V.data[0] / Length;
    Result.data[1] := V.data[1] / Length;
    Result.data[2] := V.data[2] / Length;
    Exit;
  end;

  Result.init_zero;
end;

{
Based on: https://gamedev.stackexchange.com/questions/120338/what-does-a-perspective-projection-matrix-look-like-in-opengl
}
function Perspective(Fov, AspectRatio, NearDist, FarDist: Single): Tmatrix4_single;
var
  FrustumDepth: Single;
begin
  FrustumDepth := FarDist - NearDist;

  Result.init_identity;

  if (AspectRatio = 0) or (FrustumDepth = 0) then
  begin
    Writeln('Perspective matrix - Bad attributes!');
    Exit;
  end;

  Result.data[1, 1] := 1 / tan(0.5 * Fov);
  Result.data[0, 0] := Result.data[1, 1] / AspectRatio;
  Result.data[2, 2] := (-(FarDist + NearDist)) / FrustumDepth;
  Result.data[2, 3] := -1.0;
  Result.data[3, 2] := ( -(2.0 * NearDist * FarDist)) / FrustumDepth;
  Result.data[3, 3] := 0.0;
end;

{ Algorithm from learnopengl.com tutorial }
function LookAt(const Position, Target, WorldUp: Tvector3_single): Tmatrix4_single;
var
  XAxis: Tvector3_single;
  YAxis: Tvector3_single;
  ZAxis: Tvector3_single;
  Translation: Tmatrix4_single;
  Rotation: Tmatrix4_single;
begin
  ZAxis := NormalizeVector3(Position - Target);
  XAxis := NormalizeVector3(NormalizeVector3(WorldUp) >< ZAxis);
  YAxis := ZAxis >< XAxis;

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

  Result := Translation * Rotation;
end;


end.

