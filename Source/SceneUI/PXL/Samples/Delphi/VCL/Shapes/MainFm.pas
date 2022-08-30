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
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PXL.TypeDef, PXL.Types,
  PXL.Timing, PXL.Devices, PXL.Canvas, PXL.SwapChains, PXL.Fonts, PXL.Providers;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2i;
    EngineTicks: Integer;
    CacheStall: Integer;

    FontKristen: Integer;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindow;
    procedure RenderScene;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}

uses
  PXL.Classes, PXL.Providers.Auto;

procedure TMainForm.FormCreate(Sender: TObject);
const
  DefaultMultisamples = 8;
begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  DeviceProvider := CreateDefaultProvider;
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2i(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, DefaultMultisamples);

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

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontKristen := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\Kristen.font'));
  if FontKristen = -1 then
  begin
    MessageDlg('Could not load Kristen font.', mtError, [mbOk], 0);
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
  EngineCanvas.Free;
  EngineDevice.Free;
  DeviceProvider.Free;
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
  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.RenderWindow;
begin
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], $FF4E4433);

    if EngineCanvas.BeginScene then
    try
      RenderScene;
    finally
      EngineCanvas.EndScene;
    end;

    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;

procedure TMainForm.RenderScene;
var
 HexMtx: TMatrix3f;
 Omega, Kappa: Single;
 HoleAt, HoleSize: TPoint2f;
 I: Integer;
begin
  // Draw gradient lines.
  for I := 0 to DisplaySize.Y div 20 do
    EngineCanvas.Line(
      Point2f(0.0, 0.0), Point2f(DisplaySize.X, I * 20.0),
      ColorPair($FF837256, $FF4E4433));

    for I := 0 to DisplaySize.X div 20 do
      EngineCanvas.Line(
        Point2f(0.0, 0.0), Point2f(I * 20.0, DisplaySize.Y),
        ColorPair($FF837256, $FF4E4433));

  // Draw Hexagon.
  HexMtx :=
    // Make hexagon with dimensions of 50x50.
    TMatrix3f.Scale(Point2f(50.0, 50.0)) *
    // Rotate hexagon with time.
    TMatrix3f.Rotate(EngineTicks * 0.00371) *
    // Position hexagon at one quarter of screen.
    TMatrix3f.Translate(Point2f(DisplaySize.X * 0.25, DisplaySize.Y * 0.25));

  EngineCanvas.FillHexagon(HexMtx, $00FF0000, $FFFFD728, $00FF0000, $FFFFD728, $00FF0000, $FFFFD728);

  // Draw Arc.
  Omega := EngineTicks * 0.01892;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01241) * 0.5 * Pi;

  EngineCanvas.FillArc(
    Point2f(DisplaySize.X * 0.75, DisplaySize.Y * 0.25),
    Point2f(70.0, 50.0), Omega, Omega + Kappa, 64,
    ColorRect($FFA4E581, $FFFF9C00, $FF7728FF, $FFFFFFFF));

  // Draw small Ribbon.
  Omega := EngineTicks * 0.01134;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.014751) * 0.5 * Pi;

  EngineCanvas.FillRibbon(
    Point2f(DisplaySize.X * 0.25, DisplaySize.Y * 0.75),
    Point2f(25.0, 20.0), Point2f(45.0, 40.0), Omega, Omega + Kappa, 64,
    ColorRect($FFFF244F, $FFACFF0D, $FF2B98FF, $FF7B42FF));

  // Draw large Ribbon.
  Omega := EngineTicks * 0.01721;
  Kappa := 1.25 * Pi + Sin(EngineTicks * 0.01042) * 0.5 * Pi;

  EngineCanvas.FillRibbon(
    Point2f(DisplaySize.X * 0.25, DisplaySize.Y * 0.75),
    Point2f(50.0, 45.0), Point2f(70.0, 65.0),
    Omega, Omega + Kappa, 64,
    $FFFF244F, $FFACFF0D, $FF2B98FF, $FFA4E581, $FFFF9C00, $FF7728FF);

  // Draw hole with smooth internal border (using tape).
  HoleAt := Point2f(
   DisplaySize.X * 0.75 + Cos(EngineTicks * 0.00718) * DisplaySize.X * 0.15,
   DisplaySize.Y * 0.75 + Sin(EngineTicks * 0.00912) * DisplaySize.Y * 0.15);

  HoleSize := Point2f(40.0, 40.0);

  EngineCanvas.QuadHole(
    Point2f(DisplaySize.X * 0.5, DisplaySize.Y * 0.5),
    Point2f(DisplaySize.X * 0.5, DisplaySize.Y * 0.5),
    HoleAt, HoleSize,
    $004E4433, $FFE4DED5, 64);

  EngineCanvas.FillRibbon(
    HoleAt, HoleSize * 0.75, HoleSize, 0.0, 2.0 * Pi, 64,
    $004E4433, $004E4433, $004E4433, $FFE4DED5, $FFE4DED5, $FFE4DED5);

  // Draw information text.
  EngineFonts[FontKristen].DrawTextCentered(
    (Point2f(DisplaySize.X * 0.25, DisplaySize.Y * 0.25 + 70.0)).ToInt,
    'Hexagon',
    ColorPair($FFFFD25D, $FFFF0036));

  EngineFonts[FontKristen].DrawTextCentered(
    (Point2f(DisplaySize.X * 0.75, DisplaySize.Y * 0.25 + 70.0)).ToInt,
    'Arc',
    ColorPair($FFE5FF3B, $FF00FF00));

  EngineFonts[FontKristen].DrawTextCentered(
    (Point2f(DisplaySize.X * 0.25, DisplaySize.Y * 0.75 + 80.0)).ToInt,
    'Tapes',
    ColorPair($FFEAFAFF, $FF7B42FF));

  EngineFonts[FontKristen].DrawTextCentered(
    (Point2f(DisplaySize.X * 0.75, DisplaySize.Y * 0.75 + 80.0)).ToInt,
    'Hole + tape',
    ColorPair($FFFFF4B3, $FFA9824C));

  EngineFonts[FontKristen].DrawText(
    Point2f(4.0, 4.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate) + ', Cache Stall: ' + IntToStr(CacheStall),
    ColorPair($FFFFFF62, $FFFF8424), 1.0);

  EngineFonts[FontKristen].DrawText(
    Point2f(4.0, 34.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    ColorPair($FFE8FFAA, $FF12C312));

  EngineCanvas.Flush;
  CacheStall := EngineCanvas.CacheStall;
end;

end.
