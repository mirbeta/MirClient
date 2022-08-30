program DisplayI2C;
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
  This example illustrates usage of I2C protocol and grayscale drawing on monochrome OLED display with SSD1306 driver.

  Make sure to connect everything as shown on the accompanying diagram and photo.

  Also, when uploading this sample to Raspberry PI, remember to upload "tahoma8.font" as well!
}
uses
  Crt, SysUtils, PXL.TypeDef, PXL.Types, PXL.Fonts, PXL.Boards.Types, PXL.Boards.RPi, PXL.Displays.Types,
  PXL.Displays.SSD1306;

const
  DisplayAddressI2C = $3C;
  PinRST = 7;

type
  TApplication = class
  private
    FSystemCore: TFastSystemCore;
    FGPIO: TFastGPIO;
    FDataPort: TCustomDataPort;
    FDisplay: TCustomDisplay;

    FDisplaySize: TPoint2i;

    FFontSystem: Integer;
    FFontTahoma: Integer;

    procedure LoadFonts;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;
  end;

constructor TApplication.Create;
begin
  FSystemCore := TFastSystemCore.Create;
  FGPIO := TFastGPIO.Create(FSystemCore);
  FDataPort := TFastI2C.Create(FGPIO);

  FDisplay := TDisplay.Create(TDisplay.OLED128x32, FGPIO, FDataPort, -1, PinRST, DisplayAddressI2C);

  FDisplaySize := (FDisplay as TDisplay).ScreenSize;

  FDisplay.Initialize;
  FDisplay.LogicalOrientation := TCustomDisplay.TOrientation.InverseLandscape;
  FDisplay.Clear;

  LoadFonts;
end;

destructor TApplication.Destroy;
begin
  FDataPort.Free;
  FGPIO.Free;
  FSystemCore.Free;

  inherited;
end;

procedure TApplication.LoadFonts;
const
  TahomaFontName: StdString = 'tahoma8.font';
begin
  FFontTahoma := FDisplay.Fonts.AddFromBinaryFile(TahomaFontName);
  if FFontTahoma = -1 then
    raise Exception.CreateFmt('Could not load %s.', [TahomaFontName]);

  FFontSystem := FDisplay.Fonts.AddSystemFont(TSystemFontImage.Font9x8);
  if FFontSystem = -1 then
    raise Exception.Create('Could not load system font.');
end;

procedure TApplication.Execute;
var
  Ticks: Integer = 0;
  Omega, Kappa: Single;
begin
  WriteLn('Showing animation on display, press any key to exit...');

  while not KeyPressed do
  begin
    Inc(Ticks);
    FDisplay.Clear;

    // Draw an animated ribbon with some sort of grayscale gradient.
    Omega := Ticks * 0.02231;
    Kappa := 1.25 * Pi + Sin(Ticks * 0.024751) * 0.5 * Pi;

    FDisplay.Canvas.FillRibbon(
      Point2f(FDisplaySize.X * 0.8, FDisplaySize.Y * 0.5),
      Point2f(7.0, 3.0),
      Point2f(14.0, 16.0),
      Omega, Omega + Kappa, 16,
      ColorRect($FF000000, $FF404040, $FFFFFFFF, $FF808080));

    // Draw some text.
    FDisplay.Fonts[FFontTahoma].DrawText(Point2f(0.0, 1.0),
      'Tahoma 8 font.', ColorPairWhite);

    FDisplay.Fonts[FFontSystem].DrawText(Point2f(0.0, 20.0),
      'System Font.', ColorPairWhite);

    // Send picture to the display.
    FDisplay.Present;
  end;

  ReadKey;
end;

var
  Application: TApplication = nil;

begin
  Application := TApplication.Create;
  try
    Application.Execute;
  finally
    Application.Free;
  end;
end.

