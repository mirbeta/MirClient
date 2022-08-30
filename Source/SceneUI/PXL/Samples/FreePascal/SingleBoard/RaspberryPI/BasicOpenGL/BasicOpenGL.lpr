program BasicOpenGL;
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
{
  This example illustrates how to use OpenGL ES accelerated rendering on Raspberry PI using the same PXL code as on
  desktop. On Raspberry PI, this can be executed directly from console as it doesn't require X server to be running.

  Attention! Please follow these instructions before running the sample:

   1. After compiling and uploading this sample, change its attributes to executable. It is also required to execute
      this application with administrative privileges. Something like this:
        chmod +x BasicOpenGL
        sudo ./BasicOpenGL

   2. Remember to upload accompanying files "Lenna.png" and "Tahoma9b.font" to your device as well.

   3. Make sure you have display connected to HDMI port and can see Raspberry PI's output properly, even if it's just
      a console output.
}
uses
  Crt, SysUtils, PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas, PXL.SwapChains, PXL.Images, PXL.Fonts,
  PXL.Providers.GLES, PXL.Devices.GLES.RPi, PXL.ImageFormats, PXL.ImageFormats.Auto;

type
  TApplication = class
  private
    FImageFormatManager: TImageFormatManager;
    FImageFormatHandler: TCustomImageFormatHandler;

    FDeviceProvider: TGLESProvider;
    FEngineDevice: TGLESDevice;

    FEngineCanvas: TCustomCanvas;
    FEngineImages: TAtlasImages;
    FEngineFonts: TBitmapFonts;

    FEngineTimer: TMultimediaTimer;

    FDisplaySize: TPoint2i;
    FEngineTicks: Integer;

    FImageLenna: Integer;
    FFontTahoma: Integer;

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindow;
    procedure RenderScene;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TApplication.Create;
begin
  inherited;

  FImageFormatManager := TImageFormatManager.Create;
  FImageFormatHandler := CreateDefaultImageFormatHandler(FImageFormatManager);

  FDeviceProvider := TGLESProvider.Create(FImageFormatManager);

  FEngineDevice := FDeviceProvider.CreateDevice as TGLESDevice;
  if not FEngineDevice.Initialize then
    raise Exception.Create('Failed to initialize PXL Device.');

  FDisplaySize := FEngineDevice.DisplaySize;

  FEngineCanvas := FDeviceProvider.CreateCanvas(FEngineDevice);
  if not FEngineCanvas.Initialize then
    raise Exception.Create('Failed to initialize PXL Canvas.');

  FEngineImages := TAtlasImages.Create(FEngineDevice);

  FImageLenna := FEngineImages.AddFromFile('Lenna.png');
  if FImageLenna = -1 then
    raise Exception.Create('Could not load Lenna image.');

  FEngineFonts := TBitmapFonts.Create(FEngineDevice);
  FEngineFonts.Canvas := FEngineCanvas;

  FFontTahoma := FEngineFonts.AddFromBinaryFile('Tahoma9b.font');
  if FFontTahoma = -1 then
    raise Exception.Create('Could not load Tahoma font.');

  FEngineTimer := TMultimediaTimer.Create;
  FEngineTimer.OnTimer := EngineTiming;
  FEngineTimer.OnProcess := EngineProcess;
  FEngineTimer.MaxFPS := 4000;
end;

destructor TApplication.Destroy;
begin
  FEngineTimer.Free;
  FEngineFonts.Free;
  FEngineImages.Free;
  FEngineCanvas.Free;
  FEngineDevice.Free;
  FDeviceProvider.Free;
  FImageFormatHandler.Free;
  FImageFormatManager.Free;

  inherited;
end;

procedure TApplication.EngineTiming(const Sender: TObject);
begin
  RenderWindow;
end;

procedure TApplication.EngineProcess(const Sender: TObject);
begin
  Inc(FEngineTicks);
end;

procedure TApplication.RenderScene;
var
  J, I: Integer;
  Omega, Kappa: Single;
begin
  // Draw gray background.
  for J := 0 to FDisplaySize.Y div 40 do
    for I := 0 to FDisplaySize.X div 40 do
      FEngineCanvas.FillQuad(
        Quad(I * 40, J * 40, 40, 40),
        ColorRect($FF585858, $FF505050, $FF484848, $FF404040));

  for I := 0 to FDisplaySize.X div 40 do
    FEngineCanvas.Line(
      Point2f(I * 40.0, 0.0),
      Point2f(I * 40.0, FDisplaySize.Y),
      $FF555555);

  for J := 0 to FDisplaySize.Y div 40 do
    FEngineCanvas.Line(
      Point2f(0.0, J * 40.0),
      Point2f(FDisplaySize.X, J * 40.0),
      $FF555555);

  // Draw an animated hole.
  FEngineCanvas.QuadHole(
    Point2f(0.0, 0.0),
    FDisplaySize,
    Point2f(
      FDisplaySize.X * 0.5 + Cos(FEngineTicks * 0.0073) * FDisplaySize.X * 0.25,
      FDisplaySize.Y * 0.5 + Sin(FEngineTicks * 0.00312) * FDisplaySize.Y * 0.25),
    Point2f(80.0, 100.0),
    $20FFFFFF, $80955BFF, 16);

  // Draw the image of famous Lenna.
  FEngineCanvas.UseImage(FEngineImages[FImageLenna]);
  FEngineCanvas.TexQuad(TQuad.Rotated(
    TPoint2f(FDisplaySize) * 0.5,
    Point2f(300.0, 300.0),
    FEngineTicks * 0.01),
    IntColorAlpha(128));

  // Draw an animated Arc.
  Omega := FEngineTicks * 0.0274;
  Kappa := 1.25 * Pi + Sin(FEngineTicks * 0.01854) * 0.5 * Pi;

  FEngineCanvas.FillArc(
    Point2f(FDisplaySize.X * 0.1, FDisplaySize.Y * 0.9),
    Point2f(75.0, 50.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  // Draw an animated Ribbon.
  Omega := FEngineTicks * 0.02231;
  Kappa := 1.25 * Pi + Sin(FEngineTicks * 0.024751) * 0.5 * Pi;

  FEngineCanvas.FillRibbon(
    Point2f(FDisplaySize.X * 0.9, FDisplaySize.Y * 0.85),
    Point2f(25.0, 20.0),
    Point2f(70.0, 80.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  FEngineFonts[FFontTahoma].DrawText(
    Point2f(4.0, 4.0),
    'FPS: ' + IntToStr(FEngineTimer.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  FEngineFonts[FFontTahoma].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(FEngineDevice),
    ColorPair($FFE8FFAA, $FF12C312));

  FEngineFonts[FFontTahoma].DrawTextAligned(
    Point2f(FDisplaySize.X * 0.5, FDisplaySize.Y * 0.8),
    'This application is rendering continuously, press ESC key to exit.',
    ColorPair($FFF59CBA, $FFFF0000), TTextAlignment.Middle, TTextAlignment.Middle);
end;

procedure TApplication.RenderWindow;
begin
  FEngineDevice.Clear([TClearType.Color], 0);

  if FEngineCanvas.BeginScene then
  try
    RenderScene;
  finally
    FEngineCanvas.EndScene;
  end;

  FEngineTimer.Process;

  FEngineDevice.Flip;
end;

var
  Key: Char;
  Application: TApplication = nil;

begin
  Application := TApplication.Create;
  try
    while True do
    begin
      if KeyPressed then
      begin
        Key := ReadKey;
        if Key = #27 then
          Break;
      end;

      Application.FEngineTimer.NotifyTick;
    end;
  finally
    Application.Free;
  end;
end.

