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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, PXL.TypeDef, PXL.Types, PXL.Timing, PXL.Devices,
  PXL.ImageFormats, PXL.Canvas, PXL.SwapChains, PXL.Images, PXL.Fonts, PXL.Providers;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
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
    EngineTicks: Integer;

    ImageLenna: Integer;
    FontTahoma: Integer;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

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
  PXL.Classes, PXL.Providers.SRT, PXL.Devices.SRT, PXL.Surfaces.LCL, PXL.ImageFormats.FCL;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := TFCLImageFormatHandler.Create(ImageFormatManager);

  DeviceProvider := TSRTProvider.Create(ImageFormatManager);
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

  ImageLenna := EngineImages.AddFromFile(CrossFixFileName('..\..\..\Media\Lenna.png'));
  if ImageLenna = -1 then
  begin
    MessageDlg('Could not load Lenna image.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  { The first parameter is the image name. Since XML file name is not specified, it will be taken from image name,
    in this case being "Tahoma9b.xml". }
  FontTahoma := EngineFonts.AddFromXMLFile(CrossFixFileName('..\..\..\Media\Tahoma9b.png'));
  if FontTahoma = -1 then
  begin
    MessageDlg('Could not load Tahoma font.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;
  EngineTicks := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
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

procedure TMainForm.FormPaint(Sender: TObject);
begin
  RenderWindow;
end;

procedure TMainForm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  EngineTimer.NotifyTick;
  Done := False;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  Invalidate;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
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

    DrawSurfaceToCanvas(TSRTDevice(EngineDevice).Surface, Canvas, 0, 0);

    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;

procedure TMainForm.RenderScene;
var
  J, I: Integer;
  Omega, Kappa: VectorFloat;
begin
  // Draw gray background.
  for J := 0 to DisplaySize.Y div 40 do
    for I := 0 to DisplaySize.X div 40 do
      EngineCanvas.FillQuad(
        Quad(I * 40, J * 40, 40, 40),
        ColorRect($FF585858, $FF505050, $FF484848, $FF404040));

  for I := 0 to DisplaySize.X div 40 do
    EngineCanvas.Line(
      Point2f(I * 40.0, 0.0),
      Point2f(I * 40.0, DisplaySize.Y),
      $FF555555);

  for J := 0 to DisplaySize.Y div 40 do
    EngineCanvas.Line(
      Point2f(0.0, J * 40.0),
      Point2f(DisplaySize.X, J * 40.0),
      $FF555555);

  // Draw an animated hole.
  EngineCanvas.QuadHole(
    Point2f(0.0, 0.0),
    DisplaySize,
    Point2f(
      DisplaySize.X * 0.5 + Cos(EngineTicks * 0.0073) * DisplaySize.X * 0.25,
      DisplaySize.Y * 0.5 + Sin(EngineTicks * 0.00312) * DisplaySize.Y * 0.25),
    Point2f(80.0, 100.0),
    $20FFFFFF, $80955BFF, 16);

  // Draw the image of famous Lenna.
  EngineCanvas.UseImage(EngineImages[ImageLenna]);
  EngineCanvas.TexQuad(TQuad.Rotated(
//    TPoint2f(DisplaySize) * 0.5,  - FPC internal error
    Point2f(DisplaySize.X * 0.5, DisplaySize.Y * 0.5),
    Point2f(300.0, 300.0),
    EngineTicks * 0.01),
    IntColorAlpha(128));

  // Draw an animated Arc.
  Omega := EngineTicks * 0.0274;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01854) * 0.5 * Pi;

  EngineCanvas.FillArc(
    Point2f(DisplaySize.X * 0.1, DisplaySize.Y * 0.9),
    Point2f(75.0, 50.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  // Draw an animated Ribbon.
  Omega := EngineTicks * 0.02231;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.024751) * 0.5 * Pi;

  EngineCanvas.FillRibbon(
    Point2f(DisplaySize.X * 0.9, DisplaySize.Y * 0.85),
    Point2f(25.0, 20.0),
    Point2f(70.0, 80.0),
    Omega, Omega + Kappa, 32,
    ColorRect($FFFF0000, $FF00FF00, $FF0000FF, $FFFFFFFF));

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 4.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    ColorPair($FFE8FFAA, $FF12C312));
end;

end.

