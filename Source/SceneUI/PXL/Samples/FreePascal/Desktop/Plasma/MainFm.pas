unit MainFm;
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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, PXL.Types, PXL.Timing, PXL.ImageFormats, PXL.Devices,
  PXL.Canvas, PXL.SwapChains, PXL.Surfaces, PXL.Images, PXL.Fonts, PXL.Providers, PXL.Palettes;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineImages: TAtlasImages;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2i;

    SinLookup: array[0..1023] of Word;
    CosLookup: array[0..1023] of Word;
    PaletteLookup: array[0..1023] of TIntColor;
    ShiftX: Integer;
    ShiftY: Integer;
    PaletteIndex: Integer;

    PlasmaSurface: TPixelSurface;

    ImagePlasma: Integer;
    ImageScanline: Integer;

    FontTranceForm: Integer;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure InitPlasma;
    procedure InitPalette;
    function CreatePlasmaImage: Integer;
    procedure PreparePlasma(const ShiftX, ShiftY: Integer);
    procedure UpdatePlasmaImage;

    procedure RenderWindow;
    procedure RenderScene;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.lfm}

uses
  PXL.Classes, PXL.Providers.Auto, PXL.ImageFormats.Auto;

const
  PlasmaSize: TPoint2i = (X: 256; Y: 256);

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  DeviceProvider := CreateDefaultProvider(ImageFormatManager);
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2i(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize);

  if not EngineDevice.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Device.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Canvas.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImagePlasma := CreatePlasmaImage;
  if ImagePlasma = -1 then
  begin
    MessageDlg('Could not create Plasma image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  ImageScanline := EngineImages.AddFromFile(CrossFixFileName('..\..\..\Media\Scanline.png'));
  if ImageScanline = -1 then
  begin
    MessageDlg('Could not load Scanline image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontTranceForm := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\TranceForm.font'));
  if FontTranceForm = -1 then
  begin
    MessageDlg('Could not load TranceForm font.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;

  PlasmaSurface := TPixelSurface.Create;
  PlasmaSurface.SetSize(PlasmaSize, TPixelFormat.A8R8G8B8);

  InitPlasma;
  InitPalette;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  PlasmaSurface.Free;
  EngineTimer.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
  ImageFormatHandler.Free;
  ImageFormatManager.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  DisplaySize := Point2i(ClientWidth, ClientHeight);

  if (EngineDevice <> nil) and (EngineTimer <> nil) and EngineDevice.Initialized then
  begin
    EngineDevice.Resize(0, DisplaySize);
    EngineTimer.Reset;
  end;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  EngineTimer.NotifyTick;
  Done := False;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  PreparePlasma(ShiftX, ShiftY);
  UpdatePlasmaImage;

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(ShiftX);
  Dec(ShiftY);
  Inc(PaletteIndex);
end;

procedure TMainForm.InitPlasma;
var
  I: Integer;
begin
  for I := 0 to 1023 do
  begin
    SinLookup[I] := (Trunc(Sin(2.0 * Pi * I / 1024.0) * 512.0) + 512) and $3FF;
    CosLookup[I] := (Trunc(Cos(2.0 * Pi * I / 1024.0) * 512.0) + 512) and $3FF;
  end;

  ShiftX := 0;
  ShiftY := 0;
  PaletteIndex := 0;
end;

procedure TMainForm.InitPalette;
var
  Palette: TFloatPalette;
  I: Integer;
begin
  Palette:= TFloatPalette.Create;
  try
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Sine, 0.0);
    Palette.Add(FloatColor($FF7E00FF), TFloatNodeType.Sine, 0.1);
    Palette.Add(FloatColor($FFE87AFF), TFloatNodeType.Sine, 0.2);
    Palette.Add(FloatColor($FF7E00FF), TFloatNodeType.Sine, 0.3);
    Palette.Add(FloatColor($FFFFFFFF), TFloatNodeType.Sine, 0.4);
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Plain, 0.5);
    Palette.Add(FloatColor($FF0500A8), TFloatNodeType.Brake, 0.6);
    Palette.Add(FloatColor($FFBEFF39), TFloatNodeType.Accel, 0.7);
    Palette.Add(FloatColor($FFFFC939), TFloatNodeType.Brake, 0.8);
    Palette.Add(FloatColor($FFFFF58D), TFloatNodeType.Sine,  0.9);
    Palette.Add(FloatColor($FF000000), TFloatNodeType.Plain, 1.0);

    for I := 0 to 1023 do
      PaletteLookup[I] := Palette.Color[I / 1023.0].ToInt;
  finally
    Palette.Free;
  end;
end;

function TMainForm.CreatePlasmaImage: Integer;
var
  Image: TAtlasImage;
begin
  Image := TAtlasImage.Create(EngineDevice, False);
  Image.MipMapping  := False;
  Image.PixelFormat := TPixelFormat.A8R8G8B8;
  Image.DynamicImage:= True;

  if Image.InsertTexture(PlasmaSize.X, PlasmaSize.Y) = nil then
  begin
    Image.Free;
    Exit(-1);
  end;

  Result := EngineImages.Include(Image);
end;

procedure TMainForm.PreparePlasma(const ShiftX, ShiftY: Integer);
var
  I, J, Xadd, Cadd: Integer;
  DestPixel: PIntColor;
  Index: Integer;
begin
  for J := 0 to PlasmaSurface.Height - 1 do
  begin
    DestPixel := PlasmaSurface.Scanline[J];

    Xadd := SinLookup[((J shl 2) + ShiftX) and $3FF];
    Cadd := CosLookup[((J shl 2) + ShiftY) and $3FF];

    for I := 0 to PlasmaSurface.Width - 1 do
    begin
      Index := (SinLookup[((I shl 2) + Xadd) and $3FF] + Cadd + (PaletteIndex * 4)) and $3FF;
      if Index > 511 then
        Index := 1023 - Index;

      DestPixel^ := PaletteLookup[((Index div 4) + PaletteIndex) and $3FF];
      Inc(DestPixel);
    end;
  end;
end;

procedure TMainForm.UpdatePlasmaImage;
var
  PlasmaImage: TAtlasImage;
  LockSurface: TPixelSurface;
  I, J: Integer;
begin
  PlasmaImage := EngineImages[ImagePlasma];
  if (PlasmaImage = nil) or (PlasmaImage.TextureCount < 1) then
    Exit;

  if not PlasmaImage.Texture[0].Lock(LockSurface) then
    Exit;
  try
    LockSurface.CopyFrom(PlasmaSurface);
  finally
    LockSurface.Free;
  end;
end;

procedure TMainForm.RenderWindow;
begin
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], 0);

    if EngineCanvas.BeginScene then
    try
      RenderScene;
    finally
      EngineCanvas.EndScene;
    end;

    { Invoke "EngineProcess" event (60 times per second, independently of rendering speed) to do processing and calculations
      while GPU is busy rendering the scene. }
    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;

procedure TMainForm.RenderScene;
var
  J, I: Integer;
begin
  for j := 0 to DisplaySize.Y div PlasmaSize.Y do
    for i := 0 to DisplaySize.X div PlasmaSize.X do
    begin
      EngineCanvas.UseImage(EngineImages[ImagePlasma]);
      EngineCanvas.TexQuad(
        Quad(I * PlasmaSize.X, J * PlasmaSize.Y, PlasmaSize.X, PlasmaSize.Y),
        ColorRectWhite);
    end;

  for J := 0 to DisplaySize.Y div 64 do
    for I:= 0 to DisplaySize.X div 64 do
    begin
      EngineCanvas.UseImage(EngineImages[ImageScanline]);
      EngineCanvas.TexQuad(
        Quad(I * 64, J * 64, 64, 64),
        ColorRectWhite, TBlendingEffect.Multiply);
    end;

  EngineFonts[FontTranceForm].DrawText(
    Point2f(4.0, 4.0),
    'fps: ' + IntToStr(EngineTimer.FrameRate),
    ColorPair($FFD1FF46, $FF3EB243));
end;

end.

