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
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PXL.Types, PXL.Timing,
  PXL.Devices, PXL.Canvas, PXL.SwapChains, PXL.Fonts, PXL.Providers;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure PositionForms;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindows;
    procedure RenderPrimary;
    procedure RenderSecondary;
  public
    { Public declarations }
    PrimarySize: TPoint2i;
    SecondarySize: TPoint2i;

    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineCanvas: TCustomCanvas;
    EngineFonts: TBitmapFonts;
    EngineTimer: TMultimediaTimer;

    EngineTicks: Integer;

    FontBookAntiqua: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  PXL.Classes, PXL.Providers.Auto, SecondFm;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PositionForms;

  DeviceProvider := CreateDefaultProvider;
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  PrimarySize := Point2i(ClientWidth, ClientHeight);

  EngineDevice.SwapChains.Add(Handle, PrimarySize);

  if SecondForm <> nil then
    EngineDevice.SwapChains.Add(SecondForm.Handle, SecondarySize);

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

  FontBookAntiqua := EngineFonts.AddFromBinaryFile(CrossFixFileName('..\..\..\Media\BookAntiqua24.font'));
  if FontBookAntiqua = -1 then
  begin
    MessageDlg('Could not load Book Antiqua font.', mtError, [mbOk], 0);
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

procedure TMainForm.PositionForms;
begin
  if Screen.MonitorCount > 0 then
  begin
    BorderStyle := bsNone;

    Left := Screen.Monitors[0].Left;
    Top := Screen.Monitors[0].Top;

    Width := Screen.Monitors[0].Width;
    Height := Screen.Monitors[0].Height;
  end;

  // If more than one monitor is present, create the second form and place it on the second monitor.
  if (SecondForm = nil) and (Screen.MonitorCount > 1) then
  begin
    SecondForm := TSecondForm.Create(Self);
    SecondForm.Show;

    SecondForm.BorderStyle := bsNone;
    SecondForm.Left := Screen.Monitors[1].Left;
    SecondForm.Top := Screen.Monitors[1].Top;

    SecondForm.Width := Screen.Monitors[1].Width;
    SecondForm.Height := Screen.Monitors[1].Height;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  PrimarySize := Point2i(ClientWidth, ClientHeight);

  if (EngineDevice <> nil) and (EngineTimer <> nil) and EngineDevice.Initialized then
  begin
    EngineDevice.Resize(0, PrimarySize);
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
  RenderWindows;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  Inc(EngineTicks);
end;

procedure TMainForm.RenderWindows;
begin
  // Render on second window.
  if (SecondForm <> nil) and EngineDevice.BeginScene(1) then
  try
    EngineDevice.Clear([TClearType.Color], $FF404040);

    if EngineCanvas.BeginScene then
    try
      RenderSecondary;
    finally
      EngineCanvas.EndScene;
    end;
  finally
    EngineDevice.EndScene;
  end;

  // Render on first window.
  if EngineDevice.BeginScene(0) then
  try
    EngineDevice.Clear([TClearType.Color], $FF000040);

    if EngineCanvas.BeginScene then
    try
      RenderPrimary;
    finally
      EngineCanvas.EndScene;
    end;

    EngineTimer.Process;
  finally
    EngineDevice.EndScene;
  end;
end;

procedure TMainForm.RenderPrimary;
begin
  EngineFonts[FontBookAntiqua].DrawText(
    Point2f(4.0, 4.0),
    'This text should appear on first monitor.',
    ColorPair($FFE8F9FF, $FFAEE2FF));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2f(4.0, 44.0),
    'Frame Rate: ' + IntToStr(EngineTimer.FrameRate),
    ColorPair($FFEED1FF, $FFA1A0FF));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2f(4.0, 84.0),
    'Technology: ' + GetFullDeviceTechString(EngineDevice),
    ColorPair($FFE8FFAA, $FF12C312));
end;

procedure TMainForm.RenderSecondary;
begin
  EngineFonts[FontBookAntiqua].DrawText(
    Point2f(4.0, 4.0),
    'This text should appear on second monitor.',
    ColorPair($FFFFD27B, $FFFF0000));

  EngineFonts[FontBookAntiqua].DrawText(
    Point2f(4.0, 44.0),
    'FPS: ' + IntToStr(EngineTimer.FrameRate),
    ColorPair($FFE4FFA5, $FF00E000));
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
