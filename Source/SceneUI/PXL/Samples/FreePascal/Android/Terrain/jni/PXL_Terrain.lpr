library PXL_Terrain;
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
  This example illustrates how to handle Android touch events, draw text and render isometric landscape according
  to scale of the actual device.
}
uses
  jni, SysUtils, Android.AppGlue, PXL.TypeDef, PXL.Logs, PXL.Types, PXL.Devices, PXL.Canvas, PXL.Images, PXL.Fonts,
  PXL.Providers, PXL.Devices.Android, PXL.ImageFormats, PXL.ImageFormats.FCL, Engine.Landscape, Engine.Scrolling;

{$INCLUDE PXL.Config.inc}

var
  ImageFormatHandler: TCustomImageFormatHandler = nil;

  EngineCanvas: TCustomCanvas = nil;
  EngineImages: TAtlasImages = nil;
  EngineFonts: TBitmapFonts = nil;

  DisplaySize: TPoint2i = (X: 0; Y: 0);

  FontSegoe: Integer = -1;
  ImageTerrain: Integer = -1;

  Landscape: TLandscape = nil;
  DragScroll: TDragScroll = nil;
  MapTileSize: TPoint2i;

procedure ApplicationCreate;
begin
  Application.PresentationAttributes := [TPresentationAttribute.KeepScreenOn, TPresentationAttribute.FullScreen];
end;

procedure ApplicationDestroy;
begin
end;

procedure CreateResources;
begin
  ImageFormatHandler := TFCLImageFormatHandler.Create(Application.ImageFormatManager);

  EngineCanvas := (Application.Provider as TGraphicsDeviceProvider).CreateCanvas(Application);
  if not EngineCanvas.Initialize then
    raise Exception.Create('Failed to initialize PXL Canvas.');

  EngineImages := TAtlasImages.Create(Application);

  ImageTerrain := EngineImages.AddFromAsset('terrain.png');
  if ImageTerrain = -1 then
    raise Exception.Create('Could not load Terrain image.');

  EngineFonts := TBitmapFonts.Create(Application);
  EngineFonts.Canvas := EngineCanvas;

  FontSegoe := EngineFonts.AddFromBinaryAsset('Segoe10.font');
  if FontSegoe = -1 then
    raise Exception.Create('Could not load Tahoma font.');

  Landscape := TLandscape.Create;
  DragScroll := TDragScroll.Create;

  DragScroll.DeviceScale := Application.DisplayScale;
  DragScroll.SetPosition(Point2f(MapTileSize.X * 20.0, MapTileSize.Y * 24.0));
end;

procedure DestroyResources;
begin
  DragScroll.Free;
  Landscape.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineCanvas.Free;          
  ImageFormatHandler.Free;
end;

procedure DeviceChange;
begin
  DisplaySize := Application.ContentRect.Size;

  MapTileSize.X := Round(64.0 * Application.DisplayScale);
  MapTileSize.Y := MapTileSize.X div 2;

  if DragScroll <> nil then
    DragScroll.DeviceScale := Application.DisplayScale;
end;

procedure DrawLandscape;
var
  I, J, DeltaX: Integer;
  ViewPos, IsoPos, DrawPos: TPoint2i;
  Heights: array[0..3] of Integer;
begin
  ViewPos := DragScroll.ViewPos;

  for J := -1 to (DisplaySize.Y div (MapTileSize.Y div 2)) + 20 do
  begin
    IsoPos.Y := (ViewPos.Y div (MapTileSize.Y div 2)) + J;
    DrawPos.Y := (IsoPos.Y * (MapTileSize.Y div 2)) - ViewPos.Y - (MapTileSize.Y div 2);
    DeltaX := ((IsoPos.Y mod 2) * (MapTileSize.X div 2)) - ViewPos.X - (MapTileSize.X div 2);

    for I := -1 to (DisplaySize.X div MapTileSize.X) + 2 do
    begin
      IsoPos.X := (ViewPos.X div MapTileSize.X) + I;
      DrawPos.X := (IsoPos.X * MapTileSize.X) + DeltaX;

      if (IsoPos.X >= 0) and
        (IsoPos.Y >= 0) and
        (IsoPos.X < TLandscape.MapWidth) and
        (IsoPos.Y < TLandscape.MapHeight) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[0].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[1].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[2].Light > 0) and
        (Landscape.Entries[IsoPos.X, IsoPos.Y].Corners[3].Light > 0) then
      begin
        // Premultiply the heights so they are in same scale as tile size.
        Heights[0] := Round(Landscape.GetTileHeightSafe(IsoPos, 0) * Application.DisplayScale);
        Heights[1] := Round(Landscape.GetTileHeightSafe(IsoPos, 1) * Application.DisplayScale);
        Heights[2] := Round(Landscape.GetTileHeightSafe(IsoPos, 2) * Application.DisplayScale);
        Heights[3] := Round(Landscape.GetTileHeightSafe(IsoPos, 3) * Application.DisplayScale);

        EngineCanvas.UseImage(EngineImages[ImageTerrain]);

        EngineCanvas.TexQuad(Quad(
          { Isometric Tile corner positions }
          Point2f(DrawPos.X, (DrawPos.Y + (MapTileSize.Y div 2)) - Heights[0]),
          Point2f(DrawPos.X + (MapTileSize.X div 2), DrawPos.Y - Heights[1]),
          Point2f(DrawPos.X + MapTileSize.X, (DrawPos.Y + (MapTileSize.Y div 2)) - Heights[3]),
          Point2f(DrawPos.X + (MapTileSize.X div 2), (DrawPos.Y + MapTileSize.Y) - Heights[2])),
          { Isometric Tile corner lights }
          ColorRect(
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 0)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 1)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 3)),
            IntColorGray(Landscape.GetTileLightSafe(IsoPos, 2))));
      end;
    end;
  end;
end;

procedure PaintScreen;
var
  DrawPos: TPoint2i;
  VertShift: VectorInt;
begin
  DrawLandscape;

  { Segoe UI font is pre-rendered at 2X scale, so this needs to be taken into account when scaling it. Ideally, it is
    better to have fonts pre-rendered at some pre-defined scales e.g. 1X, 1.5X, 2X and 3X, and then select the closest
    higher scale. For this sample, 2X should be sufficient. }
  EngineFonts[FontSegoe].Scale := Application.DisplayScale / 2.0;

  VertShift := Round(20.0 * Application.DisplayScale);
  DrawPos := (Point2f(8.0, 4.0) * Application.DisplayScale).ToInt;

  EngineFonts[FontSegoe].DrawText(
    DrawPos,
    'FPS: ' + IntToStr(Application.Timer.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  Inc(DrawPos.Y, VertShift);

  EngineFonts[FontSegoe].DrawText(
    DrawPos,
    'Technology: ' + GetFullDeviceTechString(Application),
    ColorPair($FFE8FFAA, $FF12C312));

  Inc(DrawPos.Y, VertShift);

  EngineFonts[FontSegoe].DrawText(
    DrawPos,
    'Display Scale: ' + FloatToStr(Application.DisplayScale),
    ColorPair($FFDAF5FF, $FF4E9DE5));

  EngineFonts[FontSegoe].DrawTextAligned(
    Point2f(DisplaySize.X * 0.5, DisplaySize.Y * 0.9),
    'Touch and drag to scroll the map.',
    ColorPair($FFFFFFFF, $FF808080), TTextAlignment.Middle, TTextAlignment.Final);
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
  DragScroll.Update;

  Landscape.AnimateHeights;
end;

procedure HandleTactileEvent(const EventType: TTactileEventType; const EventInfo: TTactileEventInfo);
begin
  case EventType of
    TTactileEventType.TouchDown:
      if EventInfo.PointerIndex = 0 then
        DragScroll.TouchDown(EventInfo.Positions[0]);

    TTactileEventType.TouchMove:
      DragScroll.TouchMove(EventInfo.Positions[0]);

    TTactileEventType.TouchUp:
      if EventInfo.PointerCount < 2 then
        DragScroll.TouchUp(EventInfo.Positions[0]);
  end;
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

  HookApplicationTactileEvent := HandleTactileEvent;

  // Default Application Entry: do not edit.
  android_main := DefaultApplicationEntry;
end.
