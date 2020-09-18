unit UMyFPImage;

{$mode objfpc}{$H+}

{ Simple unit to load PNG and JPEG images with FP Image
  Copyright (c) 2020 by Andrzej Kilijański (digitalkarabela.com)
  Public domain license.
  No warranty, use at your own risk.
}

interface

uses
  Classes, SysUtils, FPImage, FPReadJPEG, FPReadPNG;

type

  // Simple FPImage code to load jpeg and png images
  // -------------------------------------------------------------------------
  TMyRGB8BitImage = class(TFPCompactImgRGB8Bit)
    public
      property Data :  PFPCompactImgRGB8BitValue read FData;
  end;

  TMyRGBA8BitImage = class(TFPCompactImgRGBA8Bit)
    public
      property Data :  PFPCompactImgRGBA8BitValue read FData;
  end;

  function LoadJpegImage(ImageFile: string; FlipVertical: Boolean): TMyRGB8BitImage;
  function LoadPNGImage(ImageFile: string; FlipVertical: Boolean): TMyRGBA8BitImage;


implementation

procedure FlipVert(Data:PByte; RowSize:Integer; ImageHeight: Integer);
var
  Y: Integer;
  SourceRow: PByte;
  DestRow: PByte;
  BuffArray: array of Byte;
begin
  SetLength(BuffArray, RowSize);
  SourceRow := PByte(Data); // first row
  DestRow := PByte(Data) + (ImageHeight - 1) * RowSize; // last row

  for Y := 0 to ImageHeight div 2 - 1 do
  begin
    { Copy DestRow to buffer }
    Move(DestRow^, BuffArray[0], RowSize);
    { Copy SourceRow to DestRow }
    Move(SourceRow^, DestRow^, RowSize);
    { Copy Buffer to Source }
    Move(BuffArray[0], SourceRow^, RowSize);
    { Update pointers }
    Inc(SourceRow, RowSize);
    Dec(DestRow, RowSize);
  end;
end;

function LoadJpegImage(ImageFile: string; FlipVertical: Boolean): TMyRGB8BitImage;
var
  MyImage: TMyRGB8BitImage;
  Reader: TFPReaderJPEG;

  // flip variables
  RowSize: Integer;
begin
  MyImage := nil;
  Reader := nil;
  try
    try
      MyImage := TMyRGB8BitImage.Create(0, 0);
      Reader := TFPReaderJPEG.Create;

      MyImage.LoadFromFile(ImageFile, Reader);

      if FlipVertical and (MyImage.Height > 1) then
      begin
        RowSize := MyImage.Width * 3; // RGB (three bytes)
        FlipVert(PByte(MyImage.FData), RowSize, MyImage.Height);
      end;

    except
      on E: Exception do
      begin
        FreeAndNil(MyImage);
        WriteLn('Failed to load texture: '+ E.Message);
      end;
    end;

  finally
    FreeAndNil(Reader);
  end;
  Result := MyImage;
end;

function LoadPNGImage(ImageFile: string; FlipVertical: Boolean): TMyRGBA8BitImage;
var
  MyImage: TMyRGBA8BitImage;
  Reader: TFPReaderPNG;

  // flip variables
  RowSize: Integer;
begin
  MyImage := nil;
  Reader := nil;
  try
    try
      MyImage := TMyRGBA8BitImage.Create(0, 0);
      Reader := TFPReaderPNG.Create;

      MyImage.LoadFromFile(ImageFile, Reader);

      if FlipVertical and (MyImage.Height > 1) then
      begin
        RowSize := MyImage.Width * 4; // RGBA (four bytes)
        FlipVert(PByte(MyImage.FData), RowSize, MyImage.Height);
      end;

    except
      on E: Exception do
      begin
        FreeAndNil(MyImage);
        WriteLn('Failed to load texture: '+ E.Message);
      end;
    end;

  finally
    FreeAndNil(Reader);
  end;
  Result := MyImage;
end;

end.

