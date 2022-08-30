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

{$INCLUDE PXL.Config.inc}

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, PXL.TypeDef,
  PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas, PXL.SwapChains, PXL.Images, PXL.Fonts, PXL.Providers, PXL.Archives,
  Scene.Scores;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    DeviceProvider: TGraphicsDeviceProvider;

    EngineDevice: TCustomSwapChainDevice;
    EngineTimer: TMultimediaTimer;

    Keys: array[0..255] of Boolean;

    LogoAnimIndex: Integer;
    LogoAnimDelta: Integer;
    EngineTicks: Integer;
    Level: Integer;
    LevelDelay: Integer;
    GameOver: Boolean;
    StartingUp: Boolean;
    PlayerName: StdString;
    HighScores: THighScores;

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);

    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);

    procedure RenderWindow;
    procedure RenderScene;

    procedure ShowInitial;
    procedure ShowTitle(const Text: StdString; const Y, Size: Integer; const Color: TIntColor);
    procedure ControlShip;
    procedure NextLevel;
    function AsteroidCount: Integer;
    procedure RemoveBullets;
    procedure RemoveAsteroids;
    procedure ScorePlayer;
    procedure ShowHighScores;

    procedure InitBass;
    procedure DoneBass;
    procedure LoadSounds;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation
{$R *.dfm}

uses
  Math, PXL.Archives.Loading, Engine.Globals, Engine.Particles, Engine.Objects, Scene.Objects, bass, Sound.Globals,
  StartFm;

const
  StartDelay = 60;
  InitialPhase1 = 330;
  InitialPhase2 = 330;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Randomize;

  StartForm := TStartForm.Create(Self);
  if StartForm.ShowModal <> mrOk then
  begin
    StartForm.Free;
    Application.Terminate;
    Exit;
  end;

  PlayerName := StartForm.PlayerName;

  DeviceProvider := StartForm.CreateProvider;
  EngineDevice := DeviceProvider.CreateDevice as TCustomSwapChainDevice;

  DisplaySize := Point2i(ClientWidth, ClientHeight);
  EngineDevice.SwapChains.Add(Handle, DisplaySize, 0, StartForm.VSync);

  if not EngineDevice.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Device.', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  EngineCanvas := DeviceProvider.CreateCanvas(EngineDevice);
  if not EngineCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Canvas.', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  EngineArchive := TArchive.Create;
  EngineArchive.OpenMode := TArchive.TOpenMode.ReadOnly;

  if not EngineArchive.OpenFile('media.asvf') then
  begin
    MessageDlg('Failed to open media archive.', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  EngineImages := TAtlasImages.Create(EngineDevice);

  ImageBackground := LoadImageFromArchive('stars.Image', EngineImages, EngineArchive);
  ImageBandLogo := LoadImageFromArchive('cshine.Image', EngineImages, EngineArchive);
  ImageLogo := LoadImageFromArchive('logo.Image', EngineImages, EngineArchive);
  ImageCShineLogo := LoadImageFromArchive('cslogo.Image', EngineImages, EngineArchive);
  ImageShipArmor := LoadImageFromArchive('life.Image', EngineImages, EngineArchive);
  ImageShip := LoadImageFromArchive('ship.Image', EngineImages, EngineArchive);
  ImageRock := LoadImageFromArchive('rock.Image', EngineImages, EngineArchive);
  ImageTorpedo := LoadImageFromArchive('torpedo.Image', EngineImages, EngineArchive);
  ImageExplode := LoadImageFromArchive('explode.Image', EngineImages, EngineArchive);
  ImageCombust := LoadImageFromArchive('combust.Image', EngineImages, EngineArchive);

  EngineFonts := TBitmapFonts.Create(EngineDevice);
  EngineFonts.Canvas := EngineCanvas;

  FontArialBlack := LoadFontFromArchive('ArialBlack.xml', EngineFonts, EngineArchive);
  FontTimesRoman := LoadFontFromArchive('TimesNewRoman.xml', EngineFonts, EngineArchive);
  FontImpact := LoadFontFromArchive('Impact.xml', EngineFonts, EngineArchive);

  EngineTimer := TMultimediaTimer.Create;
  EngineTimer.OnTimer := EngineTiming;
  EngineTimer.OnProcess := EngineProcess;
  EngineTimer.MaxFPS := 4000;

  Application.OnIdle := ApplicationIdle;

  // Specify initial variables
  PEngine1 := TParticles.Create;
  OEngine1 := TBaseObjects.Create;

  OEngine1.Collide := True;
  OEngine1.CollideFreq := 4;
  OEngine1.CollideMethod := TCollideMethod.Distance;

  PEngine2 := TParticles.Create;

  HighScores := THighScores.Create;
  HighScores.LoadFromFile('highscores.dat');

  InitBass;

  ShipID := TShip.Create(OEngine1).ID;

  Level := 0;
  EngineTicks := 0;
  LogoAnimIndex := 0;
  LogoAnimDelta := 1;
  StartingUp := True;

  NextLevel;

  GameOver := True;
  LevelDelay := InitialPhase1 + InitialPhase2 + StartDelay;

  BASS_ChannelPlay(MusicModule, False);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DoneBass;

  if HighScores <> nil then
  begin
    HighScores.SaveToFile('highscores.dat');
    HighScores.Free;
  end;

  PEngine2.Clear;
  PEngine1.Clear;
  OEngine1.Clear;

  PEngine2.Free;
  OEngine1.Free;
  PEngine1.Free;

  EngineTimer.Free;
  EngineFonts.Free;
  EngineImages.Free;
  EngineArchive.Free;
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
  Caption := 'Hasteroids. FPS: ' + IntToStr(EngineTimer.FrameRate) + ', Tech: ' +
    GetFullDeviceTechString(EngineDevice);

  RenderWindow;
end;

procedure TMainForm.EngineProcess(const Sender: TObject);
begin
  ControlShip;

  // create a new star
  TStar.Create(PEngine1);

  PEngine1.Update;
  PEngine2.Update;

  if LevelDelay <= 0 then
  begin
    OEngine1.Update;
    if GameOver then
    begin
      // re-create ship
      OEngine1.Remove(ShipID);
      ShipID := TShip.Create(OEngine1).ID;

      RemoveAsteroids;
      Level := 0;

      if (Random(5) = 0) and (not StartingUp) then
      begin
        TText.Create(PEngine2, 'Try this one!!!', 2, 320, 280, 256, $FFFF0000).Velocity := Point2f(0.0, 0.5);
        TShip(OEngine1[ShipID]).Armour := 15;
        TShip(OEngine1[ShipID]).Life := 1;
        Level := 6;
      end;

      NextLevel;
      GameOver := False;
      StartingUp := False;
    end;
  end
  else
    Dec(LevelDelay);

  if AsteroidCount < 1 then
  begin
    RemoveAsteroids;
    NextLevel;

    if TShip(OEngine1[ShipID]).Armour < 10 then
    begin
      TShip(OEngine1[ShipID]).Armour := TShip(OEngine1[ShipID]).Armour + 1;
      ShowTitle('1 health bonus!', 300, 256, $FFFF7F3F);
    end
    else
    begin
      if TShip(OEngine1[ShipID]).Life < 5 then
      begin
        TShip(OEngine1[ShipID]).Life := TShip(OEngine1[ShipID]).Life + 1;
        TShip(OEngine1[ShipID]).Armour := 2;
        ShowTitle('1 life bonus!!', 300, 256, $FFFFFFFF);
      end
      else
      begin
        TShip(OEngine1[ShipID]).Score := TShip(OEngine1[ShipID]).Score + 10000;
        ShowTitle('10,000 bonus!!', 300, 256, $FFFFD000);
      end;
    end;
  end;

  if TShip(OEngine1[ShipID]).Armour < 1 then
  begin
    TShip(OEngine1[ShipID]).Armour := 7;
    TShip(OEngine1[ShipID]).Life := TShip(OEngine1[ShipID]).Life - 1;
    Level := Level - 1;
    RemoveAsteroids;
    TShip(OEngine1[ShipID]).Score := TShip(OEngine1[ShipID]).Score - 15;
    NextLevel;
  end;

  if (TShip(OEngine1[ShipID]).Life < 1) and (not GameOver) then
  begin
    ScorePlayer;
    GameOver := True;
    LevelDelay := 10 * 60;
  end;

  Inc(EngineTicks);
  if EngineTicks mod 8 = 0 then
    Inc(LogoAnimIndex, LogoAnimDelta);

  if LogoAnimIndex > 30 then
    LogoAnimDelta := -1;

  if LogoAnimIndex < 1 then
    LogoAnimDelta := 1;
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

      EngineTimer.Process;
    finally
      EngineDevice.EndScene;
    end;
end;

procedure TMainForm.RenderScene;
var
  I, J, Index, TextSize: Integer;
  Text: UniString;
begin
  // render background
  if not GameOver then
  begin
    Index := 0;

    for J := 0 to 1 do
      for I := 0 to 3 do
      begin
        EngineCanvas.UseImageRegion(EngineImages[ImageBackground], Index);
        EngineCanvas.TexQuad(Quad((I * 160.0), (J * 240.0), 160.0, 240.0), ColorRectWhite);
        Inc(Index);
      end;
  end;

  // render particles beneath objects
  PEngine1.Render(nil);

  if not GameOver then
  begin
    // render all game objects
    OEngine1.Render(nil);

    // render particles over objects
    PEngine2.Render(nil);

    // show player's armour
    for I := 0 to TShip(OEngine1[ShipID]).Armour - 1 do
    begin
      EngineCanvas.UseImage(EngineImages[ImageShipArmor]);
      EngineCanvas.TexQuad(Quad(640.0 - 32.0 - (I * 32.0), 4.0, 32.0, 32.0), ColorRectWhite);
    end;

    // show player's life
    for I := 0 to TShip(OEngine1[ShipID]).Life - 1 do
    begin
      EngineCanvas.UseImageRegion(EngineImages[ImageShip], 2);
      EngineCanvas.TexQuad(Quad((I * 32) + 8, 4, 32, 32), ColorRectWhite);
    end;
  end;

  if not StartingUp then
  begin
    EngineCanvas.UseImageRegion(EngineImages[ImageCShineLogo], LogoAnimIndex);
    EngineCanvas.TexQuad(Quad(4.0, 480.0 - 32.0, 128.0, 32.0), ColorRectWhite);
  end;

  if not GameOver then
  begin
    Text := 'Score: ' + IntToStr(TShip(OEngine1[ShipID]).Score);
    TextSize := EngineFonts[FontArialBlack].TextWidthInt(Text);

    EngineFonts[FontArialBlack].DrawText(Point2f(632.0 - TextSize, 460.0), Text, ColorPair($FFDFFF67, $FF5C966F));
  end;

  if GameOver and (not StartingUp) then
  begin
    for I := 0 to 3 do
    begin
      EngineCanvas.UseImageRegion(EngineImages[ImageLogo], I);
      EngineCanvas.TexQuad(Quad((I * 160.0), 0.0, 160.0, 240.0), ColorRectWhite);
    end;

    EngineCanvas.Line(Point2f(0.0, 224.0), Point2f(640.0, 224.0), ColorPair($1F1F1F, $5F5F5F));

    ShowHighScores;
  end;

  if StartingUp then
    ShowInitial;
end;

procedure TMainForm.ShowTitle(const Text: StdString; const Y, Size: Integer; const Color: TIntColor);
begin
  TText.Create(PEngine2, Text, 0, 320, Y, Size, Color);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <= 255 then
    Keys[Key] := True;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <= 255 then
    Keys[Key] := False;
end;

procedure TMainForm.ControlShip;
var
  Ship: TShip;
begin
  Ship := TShip(OEngine1[ShipID]);

  if Keys[VK_LEFT] then
    Ship.TurnLeft;

  if Keys[VK_RIGHT] then
    Ship.TurnRight;

  if Keys[VK_UP] then
    Ship.Accelerate;

  if Keys[VK_DOWN] then
    Ship.Brake;

  if Keys[VK_SPACE] and (LevelDelay <= 0) then
    if Ship.Shoot then
      PlaySample(EffectSamples[0], 80);
end;

procedure TMainForm.NextLevel;
const
  Colors: array[0..7] of TIntColor = ($FF00FF00, $FF007FFF, $FFFF0000, $FFFFE000, $FFFF00FF, $FFFF7F3F, $FF0000E0,
    $FFD0D0D0);
  Weapons: array[0..9] of Integer = (0, 0, 1, 1, 2, 3, 4, 5, 6, 7);
var
  Asteroid: TAsteroid;
  I, ColorIndex: Integer;
  Text: TText;
  PrevWeapon, NewWeapon: Integer;
begin
  RemoveBullets;
  Inc(Level);

  with OEngine1[ShipID] as TShip do
  begin
    Position := Point2f(480.0, 240.0);
    Velocity := ZeroPoint2f;
    PrevWeapon := WeaponIndex;

    if Level < 11 then
      WeaponIndex := Weapons[Level - 1]
    else
      WeaponIndex := 7;

    NewWeapon := WeaponIndex;
  end;

  for I := 1 to Max(Level div 2, 1) do
  begin
    Asteroid := TAsteroid.Create(OEngine1);
    Asteroid.Position := Point2f(160, 240);
    Asteroid.Size := 192 + (64 * Level);
    Asteroid.Score := 2 + ((Level * 2) div 3);
  end;

  ColorIndex := (Level - 1) mod 8;
  if Level > 8 then
    ColorIndex := Random(8);

  Text := TText.Create(PEngine2, 'Level ' + IntToStr(Level), 2, 320, 240, 256, $FFDBECA1);
  Text.MaxRange := Round(EngineTimer.Speed * 4);

  LevelDelay := Round(EngineTimer.Speed * 4);

  if (PrevWeapon <> NewWeapon) and (Level > 1) then
  begin
    Text := TText.Create(PEngine2, 'Weapons upgraded!', 0, 320, 260, 256, Colors[7 - ColorIndex]);
    Text.MaxRange := Round(EngineTimer.Speed * 4);
  end;

  if not StartingUp then
    PlaySample(EffectSamples[3], 100);
end;

function TMainForm.AsteroidCount: Integer;
var
  LNode: TBaseObject;
begin
  Result := 0;

  // acquire initial object
  LNode := OEngine1.ObjectNum[0];

  // loop through all objects in the list
  while LNode <> nil do
  begin
    if LNode is TAsteroid then
      Inc(Result);

    LNode := LNode.Next;
  end;
end;

procedure TMainForm.RemoveBullets;
var
  LNode: TBaseObject;
begin
  // acquire initial object
  LNode := OEngine1.ObjectNum[0];

  // loop through all objects in the list
  while LNode <> nil do
  begin
    if LNode is TBullet then
      LNode.Dying := True;

    LNode := LNode.Next;
  end;
end;

procedure TMainForm.RemoveAsteroids;
var
  LNode: TBaseObject;
begin
  // acquire initial object
  LNode := OEngine1.ObjectNum[0];

  // loop through all objects in the list
  while LNode <> nil do
  begin
    if LNode is TAsteroid then
      LNode.Dying := True;

    LNode := LNode.Next;
  end;
end;

procedure TMainForm.ShowInitial;
const
  LogoSize: TPoint2i = (X: 192; Y: 272);
var
  Alpha, Beta, I: Integer;
  DrawAt: TPoint2i;
begin
  // This method shows initial two logos
  if (LevelDelay <= InitialPhase1 + InitialPhase2) and (LevelDelay > InitialPhase2) then
  begin
    Beta := LevelDelay - InitialPhase2;
    Alpha := 255;

    if Beta >= InitialPhase1 * 2 / 3 then
      Alpha := Trunc(((InitialPhase1 - Beta) * 255) / (InitialPhase1 / 3));

    if Beta <= InitialPhase1 / 3 then
      Alpha := Trunc((Beta * 255) / (InitialPhase1 / 3));

    DrawAt.X := (DisplaySize.X - LogoSize.X) div 2;
    DrawAt.Y := (DisplaySize.Y - LogoSize.Y) div 2;

    EngineCanvas.UseImageRegion(EngineImages[ImageBandLogo], 0);
    EngineCanvas.TexQuad(Quad(DrawAt.X, DrawAt.Y, LogoSize.X, LogoSize.Y), IntColorAlpha(Alpha));

    EngineCanvas.FrameRect(FloatRect(DrawAt.X - 1, DrawAt.Y - 1, LogoSize.X + 2, LogoSize.Y + 2),
      IntColor($30707070, Alpha));
  end;

  if LevelDelay <= InitialPhase2 then
  begin
    Beta := LevelDelay;
    Alpha := 255;

    if Beta >= InitialPhase2 * 2 / 3 then
      Alpha := Trunc(((InitialPhase2 - Beta) * 255) / (InitialPhase2 / 3));

    if Beta <= InitialPhase2 / 3 then
      Alpha := Trunc((Beta * 255) / (InitialPhase2 / 3));

    for I := 0 to 3 do
    begin
      EngineCanvas.UseImageRegion(EngineImages[ImageLogo], I);
      EngineCanvas.TexQuad(Quad((I * 160), 240 - 112, 160.0, 224.0), IntColorAlpha(Alpha));
    end;

    EngineCanvas.Line(Point2f(0, 240 - 112 - 1), Point2f(640, 240 - 112 - 1),
      ColorPair(IntColor($40707070, Alpha)));

    EngineCanvas.Line(Point2f(0, 240 + 112), Point2f(640, 240 + 112),
      ColorPair(IntColor($40707070, Alpha)));
  end;
end;

procedure TMainForm.ScorePlayer;
var
  HighScore: THighScore;
begin
  HighScore := HighScores.Add;
  HighScore.Player := PlayerName;
  HighScore.Score := TShip(OEngine1[ShipID]).Score;

  HighScores.Sort;

  while HighScores.Count > 10 do
    HighScores.Delete(10);
end;

procedure TMainForm.ShowHighScores;
var
  LTextWidth, I, J: Integer;
  LText: UniString;
begin
  LTextWidth := 0;

  // determine total width of names
  for I := 0 to HighScores.Count - 1 do
  begin
    LText := IntToStr(I + 1);

    while Length(LText) < 2 do
      LText := ' ' + LText;

    LText := LText + '. ' + HighScores[I].Player;

    while Length(LText) > 24 do
      Delete(LText, Length(LText), 1);

    LTextWidth := Max(LTextWidth, EngineFonts[FontArialBlack].TextWidthInt(LText));
  end;

  EngineFonts[FontImpact].DrawTextCentered(Point2f(DisplaySize.X div 2, 228.0), 'High Scores:', ColorPairWhite);

  for I := 0 to HighScores.Count - 1 do
  begin
    J := (((9 - I) * 192) div 9) + 63;

    LText := IntToStr(I + 1);
    while Length(LText) < 2 do
      LText := ' ' + LText;

    LText := LText + '. ' + HighScores[I].Player;

    while Length(LText) > 24 do
      Delete(LText, Length(LText), 1);

    EngineFonts[FontArialBlack].DrawText(Point2f(200.0, 260 + (I * 20.0)), LText, ColorPair($FFFF7F3F), J / 255.0);

    EngineFonts[FontArialBlack].DrawText(Point2f(200 + LTextWidth + 20, 260 + (I * 20)), IntToStr(HighScores[I].Score),
      ColorPair($FFFFD000), J / 255.0);
  end;
end;

procedure TMainForm.InitBass;
var
  Stream: TMemoryStream;
begin
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    MessageDlg('Failed to initialize bass.dll!', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
	 end;

  Stream := TMemoryStream.Create;
  try
    if not EngineArchive.ReadStream('sten.it', Stream) then
    begin
      MessageDlg('Failed reading music from archive!', mtError, [mbOK], 0);
      DoneBass;
      Application.Terminate;
    end;

    Stream.Position := 0;

    MusicModule := BASS_MusicLoad(True, Stream.Memory, 0, Stream.Size, BASS_MUSIC_LOOP or BASS_MUSIC_RAMPS, 0);
  finally
    Stream.Free;
  end;

  if MusicModule = 0 then
  begin
    MessageDlg('Failed to load music module from media archive!', mtError, [mbOK], 0);
    DoneBass;
    Application.Terminate;
  end;

  LoadSounds;
end;

procedure TMainForm.DoneBass;
var
  I: Integer;
begin
  if MusicModule <> 0 then
  begin
    BASS_MusicFree(MusicModule);
    MusicModule := 0;
  end;

  for I := 0 to High(EffectSamples) do
    if EffectSamples[I] <> 0 then
    begin
      BASS_SampleFree(EffectSamples[I]);
      EffectSamples[I] := 0;
    end;

  BASS_Free;
end;

procedure TMainForm.LoadSounds;
const
  SoundKeys: array[0..3] of StdString = ('beam2.wav', 'crash1.wav', 'accel.wav', 'newlevel.wav');
var
  Stream: TMemoryStream;
  I: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    for I := 0 to 3 do
    begin
      Stream.Clear;

      if not EngineArchive.ReadStream(SoundKeys[I], Stream) then
      begin
        DoneBass;
        MessageDlg('Failed to load sample sounds!', mtError, [mbOK], 0);
        Application.Terminate;
        Exit;
      end;

      EffectSamples[I] := BASS_SampleLoad(True, Stream.Memory, 0, Stream.Size, 8, 0);
      if EffectSamples[I] = 0 then
      begin
        DoneBass;
        MessageDlg('Failed to load sample sounds!', mtError, [mbOK], 0);
        Application.Terminate;
        Exit;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

end.
