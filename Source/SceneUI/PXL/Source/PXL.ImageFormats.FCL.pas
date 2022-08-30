unit PXL.ImageFormats.FCL;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef, PXL.Types, PXL.Surfaces, PXL.ImageFormats;

type
  TFCLImageFormatHandler = class(TCustomImageFormatHandler)
  protected
    procedure RegisterExtensions; override;
  public
    function LoadFromStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean; override;
    function SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
      const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean; override;
  end;

implementation

uses
  zstream, fpreadpng, fpreadbmp, fpreadjpeg, fpreadtiff, fpwritepng, fpwritebmp, fpwritejpeg, fpwritetiff, fpimage,
  SysUtils;

{$REGION 'Global Types and Constants'}

const
  MaxContextIndex = 4;

  InitialMemoryImageSize: TPoint2i = (X: 4; Y: 4);

  ContextImageReaders: array[0..MaxContextIndex - 1] of TFPCustomImageReaderClass = (
    TFPReaderBMP,
    TFPReaderPNG,
    TFPReaderJPEG,
    TFPReaderTIFF);

  ContextImageWriters: array[0..MaxContextIndex - 1] of TFPCustomImageWriterClass = (
    TFPWriterBMP,
    TFPWriterPNG,
    TFPWriterJPEG,
    TFPWriterTIFF);

{$ENDREGION}
{$REGION 'TFCLImageFormatHandler'}

procedure TFCLImageFormatHandler.RegisterExtensions;
begin
  RegisterExtension('.bmp', Pointer(PtrInt(0)));
  RegisterExtension('.png', Pointer(PtrInt(1)));
  RegisterExtension('.jpg', Pointer(PtrInt(2)));
  RegisterExtension('.jpeg', Pointer(PtrInt(2)));
  RegisterExtension('.tif', Pointer(PtrInt(3)));
  RegisterExtension('.tiff', Pointer(PtrInt(3)));
end;

function TFCLImageFormatHandler.LoadFromStream(const Context: Pointer; const Extension: StdString;
  const Stream: TStream; const DestSurface: TPixelSurface; const AlphaFormatRequest: TAlphaFormatRequest): Boolean;
var
  Image: TFPCustomImage;
  Reader: TFPCustomImageReader;
  SrcColor: TFPColor;
  DestPixel: PIntColor;
  I, J: Integer;
begin
  Image := TFPMemoryImage.Create(InitialMemoryImageSize.X, InitialMemoryImageSize.Y);
  try
    Reader := ContextImageReaders[PtrInt(Context)].Create;
    if Reader = nil then
      Exit(False);
    try
      try
        Image.LoadFromStream(Stream, Reader);
      except
        Exit(False);
      end;

      DestSurface.SetSize(Image.Width, Image.Height, TPixelFormat.A8R8G8B8);
      DestSurface.PremultipliedAlpha := False;

      for J := 0 to DestSurface.Height - 1 do
      begin
        DestPixel := DestSurface.Scanline[J];

        for I := 0 to DestSurface.Width - 1 do
        begin
          SrcColor := Image.Colors[I, J];

          PIntColorRec(DestPixel).Red := SrcColor.red shr 8;
          PIntColorRec(DestPixel).Green := SrcColor.green shr 8;
          PIntColorRec(DestPixel).Blue := SrcColor.blue shr 8;
          PIntColorRec(DestPixel).Alpha := SrcColor.alpha shr 8;

          Inc(DestPixel);
        end;
      end;
    finally
      Reader.Free;
    end;
  finally
    Image.Free;
  end;

  if AlphaFormatRequest = TAlphaFormatRequest.Premultiplied then
  begin
    DestSurface.PremultiplyAlpha;
    DestSurface.PremultipliedAlpha := True;
  end;

  Result := True;
end;

function TFCLImageFormatHandler.SaveToStream(const Context: Pointer; const Extension: StdString; const Stream: TStream;
  const SourceSurface: TPixelSurface; const Quality: Pointer): Boolean;
var
  Image: TFPCustomImage;
  Writer: TFPCustomImageWriter;
  SrcPixel: PIntColor;
  DestColor: TFPColor;
  I, J: Integer;
begin
  if SourceSurface.PremultipliedAlpha or (SourceSurface.PixelFormat <> TPixelFormat.A8R8G8B8) then
    Exit(False);

  Writer := ContextImageWriters[PtrInt(Context)].Create;
  if Writer = nil then
    Exit(False);
  try
    if Writer is TFPWriterPNG then
    begin
      TFPWriterPNG(Writer).WordSized := False;
      TFPWriterPNG(Writer).UseAlpha := SourceSurface.HasAlphaChannel;
      TFPWriterPNG(Writer).CompressionLevel := clmax;
    end
    else if Writer is TFPWriterJPEG then
      TFPWriterJPEG(Writer).CompressionQuality := SizeInt(Quality);

    Image := TFPMemoryImage.Create(SourceSurface.Width, SourceSurface.Height);
    try
      for J := 0 to SourceSurface.Height - 1 do
      begin
        SrcPixel := SourceSurface.Scanline[J];

        for I := 0 to SourceSurface.Width - 1 do
        begin
          DestColor.red := (Integer(PIntColorRec(SrcPixel).Red) * $FFFF) div 255;
          DestColor.green := (Integer(PIntColorRec(SrcPixel).Green) * $FFFF) div 255;
          DestColor.blue := (Integer(PIntColorRec(SrcPixel).Blue) * $FFFF) div 255;
          DestColor.alpha := (Integer(PIntColorRec(SrcPixel).Alpha) * $FFFF) div 255;

          Image.Colors[I, J] := DestColor;
          Inc(SrcPixel);
        end;
      end;

      try
        Image.SaveToStream(Stream, Writer);
      except
        Exit(False);
      end;
    finally
      Image.Free;
    end;
  finally
    Writer.Free;
  end;

  Result := True;
end;

{$ENDREGION}

end.
