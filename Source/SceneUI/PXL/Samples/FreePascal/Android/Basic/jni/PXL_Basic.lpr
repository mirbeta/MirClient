library PXL_Basic;
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
uses
  jni, SysUtils, Android.AppGlue, PXL.TypeDef, PXL.Logs, PXL.Types, PXL.Devices, PXL.Canvas, PXL.Images, PXL.Fonts,
  PXL.Providers, PXL.Devices.Android, PXL.ImageFormats, PXL.ImageFormats.FCL;

{$INCLUDE PXL.Config.inc}

var
  FImageFormatHandler: TCustomImageFormatHandler = nil;

  EngineCanvas: TCustomCanvas = nil;
  EngineImages: TAtlasImages = nil;
  EngineFonts: TBitmapFonts = nil;

  DisplaySize: TPoint2i = (X: 0; Y: 0);
  EngineTicks: Integer = 0;

  ImageLenna: Integer = -1;
  FontTahoma: Integer = -1;

procedure ApplicationCreate;
begin
  Application.PresentationAttributes := [TPresentationAttribute.KeepScreenOn, TPresentationAttribute.FullScreen];
end;

procedure ApplicationDestroy;
begin
end;

procedure CreateResources;
begin
  FImageFormatHandler := TFCLImageFormatHandler.Create(Application.ImageFormatManager);

  EngineCanvas := (Application.Provider as TGraphicsDeviceProvider).CreateCanvas(Application);
  if not EngineCanvas.Initialize then
    raise Exception.Create('Failed to initialize PXL Canvas.');

  EngineImages := TAtlasImages.Create(Application);

  ImageLenna := EngineImages.AddFromAsset('lenna.png');
  if ImageLenna = -1 then
    raise Exception.Create('Could not load Lenna image.');

  EngineFonts := TBitmapFonts.Create(Application);
  EngineFonts.Canvas := EngineCanvas;

  FontTahoma := EngineFonts.AddFromBinaryAsset('Tahoma9b.font');
  if FontTahoma = -1 then
    raise Exception.Create('Could not load Tahoma font.');
end;

procedure DestroyResources;
begin
  EngineFonts.Free;
  EngineImages.Free;
  EngineCanvas.Free;          
  FImageFormatHandler.Free;
end;

procedure DeviceChange;
begin
  DisplaySize := Application.ContentRect.Size;
end;

procedure PaintScreen;
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
    'FPS: ' + IntToStr(Application.Timer.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + GetFullDeviceTechString(Application),
    ColorPair($FFE8FFAA, $FF12C312));
end;

procedure ApplicationPaint;
begin
  Application.Clear([TClearType.Color], 0);

  if EngineCanvas.BeginScene then
  try
    PaintScreen;
  finally
    EngineCanvas.EndScene;
  end;
end;

procedure ApplicationProcess;
begin
  Inc(EngineTicks);
end;

// Android Native Activity export: do not edit.
exports
  ANativeActivity_onCreate;

begin
  // Note that this code is executed in a thread that is different than the main thread used by AppGlue.
  // It is recommended to keep this section as short as possible to avoid any threading conflicts.
{$IFDEF ANDROID_DEBUG}
  LogText('Library Load');
{$ENDIF}

  // Assign user's hooks that will be called by PXL application manager.
  HookApplicationCreate := ApplicationCreate;
  HookApplicationDestroy := ApplicationDestroy;
  HookApplicationPaint := ApplicationPaint;
  HookApplicationProcess := ApplicationProcess;

  HookApplicationCreateResources := CreateResources;
  HookApplicationDestroyResources := DestroyResources;
  HookApplicationDeviceChange := DeviceChange;

  // Default Application Entry: do not edit.
  android_main := DefaultApplicationEntry;
end.
