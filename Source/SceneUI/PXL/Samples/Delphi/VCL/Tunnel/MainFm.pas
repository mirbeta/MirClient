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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, PXL.TypeDef, PXL.Types, PXL.Timing, PXL.Devices, PXL.ImageFormats, PXL.Canvas,
  PXL.SwapChains, PXL.Fonts, PXL.Providers, PXL.Bitmaps;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    ImageFormatManager: TImageFormatManager;
    ImageFormatHandler: TCustomImageFormatHandler;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    DisplaySize: TPoint2i;
    EngineTicks: Integer;

    FontTempusSans: Integer;

    BitmapMain: TBitmap;
    BitmapCopy: TBitmap;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure PrepareBitmaps;

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
  PXL.Classes, PXL.Providers.Auto, PXL.ImageFormats.Auto;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  ImageFormatManager := TImageFormatManager.Create;
  ImageFormatHandler := CreateDefaultImageFormatHandler(ImageFormatManager);

  DeviceProvider := CreateDefaultProvider(ImageFormatManager);
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2i(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, 0, True);

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

  FontTempusSans := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\TempusSans.font'));
  if FontTempusSans = -1 then
  begin
    MessageDlg('Could not load TempusSans font.', mtError, [mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;
  EngineTicks := 0;

  BitmapMain := TBitmap.Create(EngineDevice);
  BitmapMain.SetSize(512, 512);

  BitmapCopy := TBitmap.Create(EngineDevice);
  BitmapCopy.Storage := TBitmapStorage.Drawable;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BitmapCopy.Free;
  BitmapMain.Free;
  EngineTimer.Free;
  EngineFonts.Free;
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
  PrepareBitmaps;

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.PrepareBitmaps;
const
  SourceMapping: TQuad = (
    TopLeft    : (X:   0 + 4; Y: 0 + 4);
    TopRight   : (X: 511 - 1; Y: 0 + 3);
    BottomRight: (X: 511 - 3; Y: 511 - 4);
    BottomLeft : (X:   0 + 1; Y: 511 - 8));
var
  Theta, RibbonLength: Single;
begin
  if BitmapMain.Canvas.BeginScene then
  try
    // Copy previous scene, englarged and slightly rotated.
    BitmapMain.Canvas.UseImagePx(BitmapCopy, SourceMapping);
    BitmapMain.Canvas.TexQuad(Quad(0.0, 0.0, 512.0, 512.0), ColorRectWhite);

    // Darken the area slightly, to avoid color mess :)
    // Replace color parameter to $FFF0F0F0 to reduce the effect.
    BitmapMain.Canvas.FillRect(0, 0, 512, 512, $FFFCFCFC, TBlendingEffect.Multiply);

    // Add the "motion scene" to the working surface.
    Theta := (EngineTicks mod 200) * Pi / 100;
    RibbonLength := (1.0 + Sin(EngineTicks / 50.0)) * Pi * 2 / 3 + (Pi / 3);

    BitmapMain.Canvas.FillRibbon(
      Point2f(256, 256 - 32), Point2f(32.0, 48.0), Point2f(96.0, 64.0),
      Theta, Theta + RibbonLength, 64,
      $7F7E00FF, $7F75D3FF, $7FD1FF75, $7FFFC042, $7F00FF00, $7FFF0000);
  finally
    BitmapMain.Canvas.EndScene;
  end;

  // Copy newly created scene to auxiliary bitmap.
  BitmapCopy.CopyFrom(BitmapMain);
end;

procedure TMainForm.RenderWindow;
begin
  if EngineDevice.BeginScene then
  try
    EngineDevice.Clear([TClearType.Color], $FF404040);

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
begin
  // Simply draw the bitmap on the screen.
  EngineCanvas.UseImage(BitmapMain);
  EngineCanvas.TexQuad(
    Quad(
      (DisplaySize.X - BitmapMain.Width) * 0.5, (DisplaySize.Y - BitmapMain.Height) * 0.5,
      BitmapMain.Width, BitmapMain.Height), ColorRectWhite, TBlendingEffect.Add);

  // Display the information text.
  EngineFonts[FontTempusSans].DrawText(
    Point2f(4.0, 4.0),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate),
    ColorPair($FFEDF8FF, $FFA097FF));

  EngineFonts[FontTempusSans].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    ColorPair($FFE8FFAA, $FF12C312));
end;

end.
