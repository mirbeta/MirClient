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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, PXL.TypeDef, PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas, PXL.Images, PXL.Fonts,
  PXL.Providers, PXL.FMBridge;

type
  TMainForm = class(TForm)
    SysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FMBridge: TFMBridge;
    ScreenScale: Single;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomDevice;
    EngineCanvas: TCustomCanvas;
    EngineImages: TAtlasImages;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2i;
    EngineTicks: Integer;

    ImageLenna: Integer;
    FontTahoma: Integer;

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderScene;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.fmx}

uses
  System.IOUtils;

function GetMediaPath: StdString;
begin
{$IFDEF MSWINDOWS}
  Result := '..\..\..\..\..\Media\';
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/';
  {$ENDIF}
{$ENDIF}
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMBridge := TFMBridge.Create;

  DeviceProvider := FMBridge.CreateProvider;
  ScreenScale := FMBridge.ScreenScale;

  EngineDevice := DeviceProvider.CreateDevice;
  if (EngineDevice is TCustomStateDevice) and (not TCustomStateDevice(EngineDevice).Initialize) then
  begin
    MessageDlg('Failed to initialize PXL Device.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Canvas.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImageLenna := EngineImages.AddFromFile(GetMediaPath + 'Lenna.png');
  if ImageLenna = -1 then
  begin
    MessageDlg('Could not load Lenna image.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontTahoma := EngineFonts.AddFromXMLFile(GetMediaPath + 'Tahoma9b.png');
  if FontTahoma = -1 then
  begin
    MessageDlg('Could not load Tahoma font.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

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
  FMBridge.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  DisplaySize := Point2i(Round(Width * ScreenScale), Round(Height * ScreenScale));
end;

procedure TMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  // Make sure there is nothing in FM canvas cache before using PXL.
  Canvas.Flush;

  // Invoke PXL's multimedia timer, which will call "EngineTiming" to continue drawing on this form with PXL.
  EngineTimer.NotifyTick;
end;

procedure TMainForm.EngineTiming(const Sender: TObject);
begin
  if EngineCanvas.BeginScene then
  try
    RenderScene;

    // Invoke "EngineProcess" event to do processing while GPU is busy rendering the scene.
    EngineTimer.Process;
  finally
    EngineCanvas.EndScene;
  end;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
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
  EngineCanvas.UseImagePx(EngineImages[ImageLenna], Quad(0, 0, 512, 512));
  EngineCanvas.TexQuad(TQuad.Rotated(
    TPoint2f(DisplaySize) * 0.5,
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

  EngineCanvas.Flush;
end;

procedure TMainForm.SysTimerTimer(Sender: TObject);
begin
  MainForm.Invalidate;
end;

end.
