unit SoundUtil;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grobal2, ExtCtrls, HUtil32, GList, Bass;

const
  _PI                       = 3.1415927;

{$IF Var_UI = Var_176}
  bmg_intro = 'wav\Game over2.wav';  // 176
  bmg_select = 'wav\sellect-loop2.wav'; //选择角色   'wav\sellect-loop2.wav';
{$ELSE}
  bmg_intro = 'wav\log-in-long2.wav';  // 185
  bmg_select = 'wav\main_theme.wav'; //选择角色   'wav\sellect-loop2.wav';
{$IFEND}
  bmg_field = 'wav\Field2.wav';
  bmg_gameover = 'wav\game over2.wav';

  s_walk_ground_l = 1;
  s_walk_ground_r = 2;
  s_run_ground_l = 3;
  s_run_ground_r = 4;
  s_walk_stone_l = 5;
  s_walk_stone_r = 6;
  s_run_stone_l = 7;
  s_run_stone_r = 8;
  s_walk_lawn_l = 9;
  s_walk_lawn_r = 10;
  s_run_lawn_l = 11;
  s_run_lawn_r = 12;
  s_walk_rough_l = 13;
  s_walk_rough_r = 14;
  s_run_rough_l = 15;
  s_run_rough_r = 16;
  s_walk_wood_l = 17;
  s_walk_wood_r = 18;
  s_run_wood_l = 19;
  s_run_wood_r = 20;
  s_walk_cave_l = 21;
  s_walk_cave_r = 22;
  s_run_cave_l = 23;
  s_run_cave_r = 24;
  s_walk_room_l = 25;
  s_walk_room_r = 26;
  s_run_room_l = 27;
  s_run_room_r = 28;
  s_walk_water_l = 29;
  s_walk_water_r = 30;
  s_run_water_l = 31;
  s_run_water_r = 32;

  s_hit_short = 50;
  s_hit_wooden = 51;
  s_hit_sword = 52;
  s_hit_do = 53;
  s_hit_axe = 54;
  s_hit_club = 55;
  s_hit_long = 56;
  s_hit_fist = 57;

  s_struck_short = 60;
  s_struck_wooden = 61;
  s_struck_sword = 62;
  s_struck_do = 63;
  s_struck_axe = 64;
  s_struck_club = 65;

  s_struck_body_sword = 70;
  s_struck_body_axe = 71;
  s_struck_body_longstick = 72;
  s_struck_body_fist = 73;

  s_struck_armor_sword = 80;
  s_struck_armor_axe = 81;
  s_struck_armor_longstick = 82;
  s_struck_armor_fist = 83;

  //s_powerup_man         = 80;
  //s_powerup_woman       = 81;
  //s_die_man             = 82;
  //s_die_woman           = 83;
  //s_struck_man          = 84;
  //s_struck_woman        = 85;
  //s_firehit             = 86;

  //s_struck_magic        = 90;
  s_strike_stone = 91;
  s_drop_stonepiece = 92;

  s_rock_door_open = 100;
  s_intro_theme = 102;
  s_meltstone = 101;
  s_main_theme = 102;
  s_norm_button_click = 103;
  s_rock_button_click = 104;
  s_glass_button_click = 105;
  s_money = 106;
  s_eat_drug = 107;
  s_click_drug = 108;
  s_spacemove_out = 109;
  s_spacemove_in = 110;

  s_click_weapon = 111;
  s_click_armor = 112;
  s_click_ring = 113;
  s_click_armring = 114;
  s_click_necklace = 115;
  s_click_helmet = 116;
  s_click_grobes = 117;
  s_itmclick = 118;

  s_yedo_man = 130;
  s_yedo_woman = 131;
  s_longhit = 132;
  s_widehit = 133;
  s_rush_l = 134;
  s_rush_r = 135;
  s_firehit_ready = 136;
  s_firehit = 137;
  s_crshit = 140;
  s_twinhit = 141;
  s_twinhitReady = 142;

  s_man_struck = 138;
  s_wom_struck = 139;
  s_man_die = 144;
  s_wom_die = 145;

type
  IChannel = interface
    ['{32549D16-44B1-4912-A7FE-025BCFFFB950}']
    function GetHandle: HChannel;

    procedure SetPanning(const Pan: Integer);
    procedure SetVolume(const Volume: Integer);
    procedure SetPitch(const Pitch: Single);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    function IsPlaying: Boolean;
    function IsSliding: Boolean;
    function GetLength: Single;
    function GetPos: Single;
    procedure SetPos(const Seconds: Single);
    procedure SlideTo(const Time: Single; const Volume: Integer;
      const Pan: Integer = -101; const Pitch: Single = -1);

    property Handle: HChannel read GetHandle;
  end;

  IEffect = interface
    ['{526AD139-7C58-4692-AF7E-84206531CEC2}']
    function GetHandle: HSample;

    function Play: IChannel;
    function PlayEx(const Volume: Integer = 100; const Pan: Integer = 0;
      const Pitch: Single = 1.0; const Loop: Boolean = False): IChannel;

    property Handle: HSample read GetHandle;
  end;

  IMusic = interface
    ['{15A0ADA4-DF3D-4821-B06E-5F72208709EA}']
    function GetHandle: HMusic;

    function Play(const Loop: Boolean; const Volume: Integer = 100;
      const Order: Integer = -1; const Row: Integer = -1): IChannel;
    function GetAmplification: Integer;
    function GetLength: Integer;
    procedure SetPos(const Order, Row: Integer);
    function GetPos(out Order, Row: Integer): Boolean;
    procedure SetInstrVolume(const Instr, Volume: Integer);
    function GetInstrVolume(const Instr: Integer): Integer;
    procedure SetChannelVolume(const Channel, Volume: Integer);
    function GetChannelVolume(const Channel: Integer): Integer;

    property Handle: HMusic read GetHandle;
  end;

  IInternalChannel = interface(IChannel)
    ['{F7EFCB72-56E5-4FED-A89A-4B1D0AEE794D}']
    procedure SetHandle(const Value: HChannel);
  end;

  IResource = interface
    ['{BAA2A47B-87B1-4D26-A8EF-AE49E3B2BC6F}']
    function GetHandle: Pointer;
    function GetSize: Longword;

    property Handle: Pointer read GetHandle;
    property Size: Longword read GetSize;
  end;

type
  TChannel = class(TInterfacedObject, IChannel, IInternalChannel)
  private
    FHandle: HChannel;
  protected
    { IChannel }
    function GetHandle: HChannel;
    procedure SetPanning(const Pan: Integer);
    procedure SetVolume(const Volume: Integer);
    procedure SetPitch(const Pitch: Single);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    function IsPlaying: Boolean;
    function IsSliding: Boolean;
    function GetLength: Single;
    function GetPos: Single;
    procedure SetPos(const Seconds: Single);
    procedure SlideTo(const Time: Single; const Volume: Integer;
      const Pan: Integer = -101; const Pitch: Single = -1);

    { IInternalChannel }
    procedure SetHandle(const Value: HChannel);
  public
    constructor Create(const AHandle: HChannel);
    destructor Destroy; override;
  end;

  TEffect = class(TInterfacedObject, IEffect)
  private
    FHandle: HSample;
    FChannel: IInternalChannel;
  protected
    { IEffect }
    function GetHandle: HSample;
    function Play: IChannel;
    function PlayEx(const Volume: Integer = 100; const Pan: Integer = 0;
      const Pitch: Single = 1.0; const Loop: Boolean = False): IChannel;
  public
    constructor Create(const AHandle: HSample);
    destructor Destroy; override;
  end;

  TMusic = class(TChannel, IMusic, IChannel)
  private
    function MusicGetLength: Integer;
    procedure MusicSetPos(const Order, Row: Integer);
    function MusicGetPos(out Order, Row: Integer): Boolean;
  protected
    { IMusic }
    function Play(const Loop: Boolean; const Volume: Integer = 100;
      const Order: Integer = -1; const Row: Integer = -1): IChannel;
    function GetAmplification: Integer;
    function IMusic.GetLength = MusicGetLength;
    procedure IMusic.SetPos = MusicSetPos;
    function IMusic.GetPos = MusicGetPos;
    procedure SetInstrVolume(const Instr, Volume: Integer);
    function GetInstrVolume(const Instr: Integer): Integer;
    procedure SetChannelVolume(const Channel, Volume: Integer);
    function GetChannelVolume(const Channel: Integer): Integer;
  public
    destructor Destroy; override;
  end;

  TResource = class(TInterfacedObject, IResource)
  private
    FHandle: Pointer;
    FSize: Longword;
  protected
    { IResource }
    function GetHandle: Pointer;
    function GetSize: Longword;
  public
    constructor Create(const AHandle: Pointer; const ASize: Longword);
    destructor Destroy; override;
  end;

  TGameSound = record
    Name: string;
    Bkg: Boolean;
    Pan: Integer;
    Val: Integer;
    Snd: IEffect;
    Cnl: IChannel;
    //Msc: IMusic;
    New: Bool;
    Tick: Longword;
  end;
  pTGameSound = ^TGameSound;

  TOnAddDownloadSoundFile = procedure(const AFileName: String;
    Important: Boolean) of Object;

  TSoundMgr = class(TThread)
  private
    FWnd: hWnd;

    FSampleRate: Integer;
    FFXVolume: Integer;
    FMusVolume: Integer;

    FSilent: Boolean;
    FBass: THandle;
    FVolume: Integer;
    FPitch: Single;

    FList: TGList;
    FNewList: TGList;

    FCacheTick: Longword;
    FSndMemory: Integer;
    FSoundList: TStringlist;
    FBGMusicList: TStringlist;

    FSendRequestList: TStringlist;

    FBackGround: TGameSound;

    FAddDownloadSoundFile: TOnAddDownloadSoundFile;
    FAddDownLoadFile: TOnAddDownloadSoundFile;
    function CalsPan(Dir, Dis: Integer): Integer;
    function CalsDistance(Src, Chr: TPOINT): Integer;
    function CalsDirection(Src, Chr: TPOINT): Integer;
    function CalsVolume(Dis: Integer): Integer;
    function GainPanVolume(X, Y: Integer; out nValue: Integer): Integer;
    procedure AddDownloadSoundFile(const AFileName: String);
    procedure AddDownLoadFile(const AFileName: String);
  protected
    procedure Execute; override;
  public
    constructor Create(Wnd: hWnd);
    destructor Destroy; override;
    procedure SoundDone;
    function SoundInit: Boolean;
    procedure SetVolume(const Volume: Integer);
    procedure SetFXVolume(const Vol: Integer);
    procedure SetMusVolume(const Vol: Integer);

    procedure PlaySound(FileName: string; X: Integer = -1; Y: Integer = -1; Loop: Boolean = False); overload;
    procedure PlaySound(FileIndex: Integer; X: Integer = -1; Y: Integer = -1; Loop: Boolean = False); overload;
    procedure PlaySoundEx(FileIndex: Integer; X: Integer = -1; Y: Integer = -1; TX: Integer = -1; TY: Integer = -1; Loop: Boolean = False);
    procedure PlayBKGSound(FileName: string); overload;
    procedure PlayBKGSound(FileIndex: Integer); overload;
    procedure SilenceSound();
    procedure ItemClickSound(Std: TClientStdItem);
    procedure ItemUseSound(StdMode: Integer);

    function Effect_Load(const data: Pointer; const Size: Longword): IEffect; overload;
    function Effect_Load(const FileName: string): IEffect; overload;
    function Effect_Play(const Eff: IEffect): IChannel;
    function Effect_PlayEx(const Eff: IEffect; const Volume, Pan: Integer; const Pitch: Single; const Loop: Boolean): IChannel;

    function Resource_Load(const FileName: string; const Size: PLongword = nil): IResource;
    //procedure Test();
    procedure LoadSoundList(flname: string);
    procedure LoadBGMusicList(flname: string);

    property OnAddDownloadSoundFile: TOnAddDownloadSoundFile read FAddDownloadSoundFile write FAddDownloadSoundFile;
    property OnAddDownloadFile: TOnAddDownloadSoundFile read FAddDownLoadFile write FAddDownLoadFile;
  published
    property Volume: Integer read FVolume write FVolume;
    property Pitch: Single read FPitch write FPitch;
    property Silent: Boolean read FSilent write FSilent;
  end;

var
  g_SndMgr                  : TSoundMgr;

implementation

uses
  ClMain, MShare, Actor, DrawScrn;

{ TChannel }

constructor TChannel.Create(const AHandle: HChannel);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TChannel.Destroy;
begin
  FHandle := 0;
  inherited;
end;

function TChannel.GetHandle: HChannel;
begin
  Result := FHandle;
end;

function TChannel.GetLength: Single;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_ChannelBytes2Seconds(FHandle, BASS_ChannelGetLength(FHandle{$IF BASSVERSION = $204}, 0{$IFEND}))
  else
    Result := -1;
end;

function TChannel.GetPos: Single;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_ChannelBytes2Seconds(FHandle, BASS_ChannelGetPosition(FHandle{$IF BASSVERSION = $204}, 0{$IFEND}))
  else
    Result := -1;
end;

function TChannel.IsPlaying: Boolean;
begin
  if (GetBassDLLHandle <> 0) then
    Result := (BASS_ChannelIsActive(FHandle) = BASS_ACTIVE_PLAYING)
  else
    Result := False;
end;

function TChannel.IsSliding: Boolean;
begin
  if (GetBassDLLHandle <> 0) then
{$IF BASSVERSION = $204}
    Result := BASS_ChannelIsSliding(FHandle, 0)
{$ELSE}
    Result := (BASS_ChannelIsSliding(FHandle) <> 0)
{$IFEND}
  else
    Result := False;
end;

procedure TChannel.Pause;
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelPause(FHandle);
end;

procedure TChannel.Resume;
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelPlay(FHandle, False);
end;

procedure TChannel.SetHandle(const Value: HChannel);
begin
  FHandle := Value;
end;

procedure TChannel.SetPanning(const Pan: Integer);
begin
  if (GetBassDLLHandle <> 0) then begin
    BASS_ChannelSetAttributes(FHandle, -1, -1, Pan);
  end;
end;

procedure TChannel.SetPitch(const Pitch: Single);
var
  Info                      : BASS_CHANNELINFO;
begin
  if (GetBassDLLHandle <> 0) then begin
    BASS_ChannelGetInfo(FHandle, Info);
    BASS_ChannelSetAttributes(FHandle, Trunc(Pitch * Info.freq), -1, -101);
  end;
end;

procedure TChannel.SetPos(const Seconds: Single);
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelSetPosition(FHandle, BASS_ChannelSeconds2Bytes(FHandle, Seconds));
end;

procedure TChannel.SetVolume(const Volume: Integer);
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelSetAttributes(FHandle, -1, Volume, -101);
end;

procedure TChannel.SlideTo(const Time: Single; const Volume, Pan: Integer;
  const Pitch: Single);
var
  freq                      : Integer;
  Info                      : BASS_CHANNELINFO;
begin
  if (GetBassDLLHandle <> 0) then begin
    BASS_ChannelGetInfo(FHandle, Info);
    if (Pitch = -1) then
      freq := -1
    else
      freq := Trunc(Pitch * Info.freq);
    BASS_ChannelSlideAttributes(FHandle, freq, Volume, Pan, Trunc(Time * 1000));
  end;
end;

procedure TChannel.Stop;
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelStop(FHandle);
end;

{ TEffect }

constructor TEffect.Create(const AHandle: HSample);
begin
  inherited Create;
  FHandle := AHandle;
  FChannel := TChannel.Create(0);
end;

destructor TEffect.Destroy;
begin
  if (GetBassDLLHandle <> 0) then
    BASS_SampleFree(FHandle);
  FHandle := 0;
  inherited;
end;

function TEffect.GetHandle: HSample;
begin
  Result := FHandle;
end;

function TEffect.Play: IChannel;
begin
  if (GetBassDLLHandle <> 0) then begin
    FChannel.SetHandle(BASS_SampleGetChannel(FHandle, False));
    BASS_ChannelPlay(FChannel.Handle, True);
    Result := FChannel;
  end else
    Result := nil;
end;

function TEffect.PlayEx(const Volume, Pan: Integer; const Pitch: Single;
  const Loop: Boolean): IChannel;
var
  Info                      : BASS_SAMPLE;
  HC                        : HChannel;
begin
  if (GetBassDLLHandle <> 0) then begin
    BASS_SampleGetInfo(FHandle, Info);
    HC := BASS_SampleGetChannel(FHandle, False);
    FChannel.SetHandle(HC);
    BASS_ChannelSetAttributes(HC, Trunc(Pitch * Info.freq), Volume, Pan);
    Info.Flags := Info.Flags and (not BASS_SAMPLE_LOOP);
    if (Loop) then
      Info.Flags := Info.Flags or BASS_SAMPLE_LOOP;
    BASS_ChannelSetFlags(HC, Info.Flags);
    BASS_ChannelPlay(HC, True);
    Result := FChannel;
  end else
    Result := nil;
end;

{ TMusic }

destructor TMusic.Destroy;
begin
  if (GetBassDLLHandle <> 0) then
    BASS_MusicFree(FHandle);
  inherited;
end;

function TMusic.GetAmplification: Integer;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_MusicGetAttribute(FHandle, BASS_MUSIC_ATTRIB_AMPLIFY)
  else
    Result := -1;
end;

function TMusic.GetChannelVolume(const Channel: Integer): Integer;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_MusicGetAttribute(FHandle, BASS_MUSIC_ATTRIB_VOL_CHAN + Channel)
  else
    Result := -1;
end;

function TMusic.GetInstrVolume(const Instr: Integer): Integer;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_MusicGetAttribute(FHandle, BASS_MUSIC_ATTRIB_VOL_INST + Instr)
  else
    Result := -1;
end;

function TMusic.MusicGetLength: Integer;
begin
  if (GetBassDLLHandle <> 0) then
    Result := BASS_MusicGetOrders(FHandle)
  else
    Result := -1;
end;

function TMusic.MusicGetPos(out Order, Row: Integer): Boolean;
var
  Pos                       : Integer;
begin
  Result := False;
  if (GetBassDLLHandle <> 0) then begin
    Pos := BASS_MusicGetOrderPosition(FHandle);
    if (Pos <> -1) then begin
      Order := LOWORD(Pos);
      Row := HIWORD(Pos);
      Result := True;
    end;
  end;
end;

procedure TMusic.MusicSetPos(const Order, Row: Integer);
begin
  if (GetBassDLLHandle <> 0) then
    BASS_ChannelSetPosition(FHandle, MAKEMUSICPOS(Order, Row));
end;

function TMusic.Play(const Loop: Boolean; const Volume: Integer = 100;
  const Order: Integer = -1; const Row: Integer = -1): IChannel;
var
  Info                      : BASS_CHANNELINFO;
  Pos, O, R                 : Integer;
begin
  if (GetBassDLLHandle <> 0) then begin
    Pos := BASS_MusicGetOrderPosition(FHandle);
    if (Order = -1) then
      O := LOWORD(Pos)
    else
      O := Order;
    if (Row = -1) then
      R := HIWORD(Pos)
    else
      R := Row;
    BASS_ChannelSetPosition(FHandle, MAKEMUSICPOS(O, R));

    BASS_ChannelGetInfo(FHandle, Info);
    BASS_ChannelSetAttributes(FHandle, Info.freq, Volume, 0);

    Info.Flags := Info.Flags and (not BASS_SAMPLE_LOOP);
    if (Loop) then
      Info.Flags := Info.Flags or BASS_SAMPLE_LOOP;

    BASS_ChannelSetFlags(FHandle, Info.Flags);
    BASS_ChannelPlay(FHandle, False);
    Result := Self;
  end else
    Result := nil;
end;

procedure TMusic.SetChannelVolume(const Channel, Volume: Integer);
begin
  if (GetBassDLLHandle <> 0) then
    BASS_MusicSetAttribute(FHandle, BASS_MUSIC_ATTRIB_VOL_CHAN + Channel, Volume);
end;

procedure TMusic.SetInstrVolume(const Instr, Volume: Integer);
begin
  if (GetBassDLLHandle <> 0) then
    BASS_MusicSetAttribute(FHandle, BASS_MUSIC_ATTRIB_VOL_INST + Instr, Volume);
end;

{ TResource }

constructor TResource.Create(const AHandle: Pointer; const ASize: Longword);
begin
  inherited Create;
  FHandle := AHandle;
  FSize := ASize;
end;

destructor TResource.Destroy;
begin
  FreeMem(FHandle);
  inherited;
end;

function TResource.GetHandle: Pointer;
begin
  Result := FHandle;
end;

function TResource.GetSize: Longword;
begin
  Result := FSize;
end;

{TSoundMgr}

procedure TSoundMgr.AddDownLoadFile(const AFileName: String);
begin
  if Assigned(FAddDownLoadFile) then
    FAddDownLoadFile(AFileName, False);
end;

procedure TSoundMgr.AddDownloadSoundFile(const AFileName: String);
begin
  if Assigned(FAddDownloadSoundFile) then
    FAddDownloadSoundFile(AFileName, False);
end;

constructor TSoundMgr.Create(Wnd: hWnd);
begin
  FSilent := False;

  FWnd := Wnd;

  FSampleRate := 44100;
  FFXVolume := 100;
  FMusVolume := 100;

  FBass := 0;
  FVolume := 100;
  FPitch := 1.0;

  FSndMemory := 0;
  FCacheTick := GetTickCount();

  FList := TGList.Create;
  FNewList := TGList.Create;

  FSoundList := TStringlist.Create;
  FBGMusicList := TStringlist.Create;
//  LoadSoundList('.\wav\sound2.lst');
//  LoadBGMusicList('.\wav\BGList.lst');

  FSendRequestList := TStringlist.Create;
  FSendRequestList.CaseSensitive := False;
  FSendRequestList.Sorted := True;

  FillChar(FBackGround, SizeOf(FBackGround), 0);

  SoundInit();

  Sleep(10);

  inherited Create(False);
end;

destructor TSoundMgr.Destroy;
var
  i                         : Integer;
  pGsnd                     : pTGameSound;
begin
  for i := 0 to FList.count - 1 do begin
    pGsnd := pTGameSound(FList[i]);
    Dispose(pGsnd);
  end;
  FList.Free;
  FSoundList.Free;
  FBGMusicList.Free;
  FNewList.Free;
  FSendRequestList.Free;
  inherited Destroy;
end;

function TSoundMgr.CalsPan(Dir, Dis: Integer): Integer;
begin
  Result := Dir * Round(100.0 * (Sin(_PI * (Dis / 28.0))));
  if Result > 100 then
    Result := 100
  else if Result < -100 then
    Result := -100;
end;

function TSoundMgr.CalsDistance(Src, Chr: TPOINT): Integer;
begin
  Result := Round(Sqrt(Sqr(Src.X - Chr.X) + Sqr(Src.Y - Chr.Y)));
end;

function TSoundMgr.CalsDirection(Src, Chr: TPOINT): Integer;
begin
  Result := 0;
  if Src.X > Chr.X then
    Result := 1
  else if Src.X < Chr.X then
    Result := -1;
end;

function TSoundMgr.CalsVolume(Dis: Integer): Integer;
var
  l_Result                  : Integer;
begin
  if (Dis <> 0) then begin
    Result := Round((Cos(_PI * (Dis / 32.0))) * FVolume);
  end else
    Result := FVolume;
end;

function TSoundMgr.GainPanVolume(X, Y: Integer; out nValue: Integer): Integer;
var
  Mon, Chr                  : TPOINT;
  nv, Dis, Dir, lVolume     : Integer;
begin
  if (X <> -1) and (g_MySelf <> nil) then begin
    Mon.X := X;
    Mon.Y := Y;
    Chr.X := g_MySelf.m_nCurrX;
    Chr.Y := g_MySelf.m_nCurrY;
    Dis := CalsDistance(Mon, Chr);
    Dir := CalsDirection(Mon, Chr);
    nValue := CalsVolume(Dis);
    Result := CalsPan(Dir, Dis);
  end else begin
    nValue := FVolume;
    Result := 0;
  end;
end;

procedure TSoundMgr.SetVolume(const Volume: Integer);
var
  i                         : Integer;
  pGsnd                     : pTGameSound;
begin
  FList.Lock;
  try
    for i := 0 to FList.count - 1 do begin
      pGsnd := pTGameSound(FList[i]);
      if (pGsnd.Snd <> nil) then begin
        pGsnd.Cnl.SetVolume(Volume);
      end;
    end;
  finally
    FList.UnLock;
  end;

  FNewList.Lock;
  try
    for i := 0 to FNewList.count - 1 do begin
      pGsnd := pTGameSound(FNewList[i]);
      if (pGsnd.Snd <> nil) then begin
        pGsnd.Cnl.SetVolume(Volume);
      end;
    end;
  finally
    FNewList.UnLock;
  end;
end;

{procedure TSoundMgr.Test();
var
  i, ii                     : Integer;
begin
  if not g_Config.EffectSound or not FHGE.Active then Exit;
  for i := 0 to FSoundList.count - 1 do begin
    if FSoundList[i] = '' then Continue;
    if ComPareText(FSoundList[i], 'wav\game-over2.wav') = 0 then Continue;
    if ComPareText(FSoundList[i], 'wav\102.wav') = 0 then Continue;

    PlaySound(FSoundList[i]);
    Sleep(1);
    Application.ProcessMessages;
  end;
end;}

procedure TSoundMgr.PlaySound(FileIndex: Integer; X, Y: Integer; Loop: Boolean);
begin
  if FSilent or not frmMain.Active then Exit;

  if (FileIndex >= 0) and (FileIndex < FSoundList.count) then begin
    if FSoundList[FileIndex] <> '' then begin
      PlaySound(FSoundList[FileIndex], X, Y, Loop);
    end;
  end;
end;

procedure TSoundMgr.PlaySoundEx(FileIndex: Integer; X: Integer = -1; Y: Integer = -1; TX: Integer = -1; TY: Integer = -1; Loop: Boolean = False);
var
  bOk, Exits                : Boolean;
  Mon, Chr                  : TPOINT;
  pGsnd                     : pTGameSound;
  i, nv, Dis, Dir, nValue, nPan: Integer;
begin
  if FSilent or not frmMain.Active then Exit;

  bOk := False;
  if (FileIndex >= 0) and (FileIndex < FSoundList.count) then begin
    if FSoundList[FileIndex] <> '' then begin
      if (X <> -1) and (g_MySelf <> nil) then begin
        Mon.X := X;
        Mon.Y := Y;
        Chr.X := TX;
        Chr.Y := TY;
        Dis := CalsDistance(Mon, Chr);
        Dir := CalsDirection(Mon, Chr);
        nValue := CalsVolume(Dis);
        nPan := CalsPan(Dir, Dis);
      end else begin
        nValue := 100;
        nPan := 0;
      end;
      bOk := True;
    end;
  end;

  if not bOk then Exit;

  Exits := False;
  FList.Lock;
  try
    for i := 0 to FList.count - 1 do begin
      pGsnd := pTGameSound(FList[i]);
      if (pGsnd.Snd <> nil) and (CompareText(pGsnd.Name, FSoundList[FileIndex]) = 0) then begin
        pGsnd.Tick := GetTickCount;
        pGsnd.Pan := GainPanVolume(X, Y, nValue);
        pGsnd.Val := nValue;
        pGsnd.New := True;
        Exits := True;
        Exit;
      end;
    end;
  finally
    FList.UnLock;
  end;

  if Exits then Exit;

  nPan := GainPanVolume(X, Y, nValue);
  New(pGsnd);
  pGsnd.Pan := nPan;
  pGsnd.Val := nValue;
  pGsnd.Bkg := Loop;
  pGsnd.Snd := nil;
  pGsnd.Cnl := nil;
  pGsnd.New := False;
  pGsnd.Name := FSoundList[FileIndex];

  FNewList.Lock();
  try
    FNewList.Add(pGsnd);
  finally
    FNewList.UnLock;
  end;
end;

procedure TSoundMgr.PlaySound(FileName: string; X, Y: Integer; Loop: Boolean);
var
  i                         : Integer;
  Exits                     : Boolean;
  pGsnd                     : pTGameSound;
  nValue, nPan              : Integer;
begin
  if FSilent or not frmMain.Active then Exit;
  if FileExists(FileName) then begin
    //FBackGround
    if Loop and (X = -1) and (Y = -1) then begin
      FBackGround.Name := FileName;
      FBackGround.Bkg := True;
      FBackGround.Pan := GainPanVolume(X, Y, nValue);
      FBackGround.Val := nValue;
      InterlockedExchange(Integer(FBackGround.New), Integer(True));
      Exit;
    end;

    Exits := False;
    FList.Lock;
    try
      for i := 0 to FList.count - 1 do begin
        pGsnd := pTGameSound(FList[i]);
        if (pGsnd.Snd <> nil) and (CompareText(pGsnd.Name, FileName) = 0) then begin
          pGsnd.Tick := GetTickCount;
          pGsnd.Pan := GainPanVolume(X, Y, nValue);
          pGsnd.Val := nValue;
          pGsnd.New := True;
          Exits := True;
          Exit;
        end;
      end;
    finally
      FList.UnLock;
    end;

    if Exits then Exit;

    nPan := GainPanVolume(X, Y, nValue);
    New(pGsnd);
    pGsnd.Pan := nPan;
    pGsnd.Val := nValue;
    pGsnd.Bkg := Loop;
    pGsnd.Snd := nil;
    pGsnd.Cnl := nil;
    pGsnd.New := False;
    pGsnd.Name := FileName;
    FNewList.Lock;
    try
      FNewList.Add(pGsnd);
    finally
      FNewList.UnLock;
    end;
  end else
    AddDownloadSoundFile(FileName);
end;

procedure TSoundMgr.PlayBKGSound(FileIndex: Integer);
begin
  if (FileIndex >= 0) and (FileIndex < FSoundList.count) then begin
    if FSoundList[FileIndex] <> '' then begin
      PlayBKGSound(FSoundList[FileIndex]);
    end;
  end;
end;

procedure TSoundMgr.PlayBKGSound(FileName: string);
begin
  PlaySound(FileName, -1, -1, True);
end;

procedure TSoundMgr.SilenceSound();
begin
  if (FBass <> 0) then begin
    BASS_Stop;
    BASS_Start;
  end;
end;

procedure TSoundMgr.Execute;
var
  n, i, ii                  : Integer;
  dwRunTick                 : Longword;
  pGsnd                     : pTGameSound;
begin
  while True do begin
    if Terminated then
      Break;

    FNewList.Lock;
    try
      ii := 0;
      for i := FNewList.count - 1 downto 0 do begin
        if Terminated then Break;
        pGsnd := FNewList[i];
        pGsnd.Snd := nil;
        pGsnd.Cnl := nil;
        pGsnd.Snd := Effect_Load(pGsnd.Name);
        if (pGsnd.Snd <> nil) then begin
          pGsnd.New := False;
          pGsnd.Cnl := nil;
          pGsnd.Cnl := pGsnd.Snd.PlayEx(pGsnd.Val, pGsnd.Pan, FPitch, pGsnd.Bkg);
          if pGsnd.Cnl <> nil then begin
            pGsnd.Tick := GetTickCount;
            FList.Add(pGsnd);
          end else begin
            Dispose(pGsnd);
            pGsnd := nil;
          end;
        end else begin
          //123456
          if FSendRequestList.IndexOf(pGsnd.Name) < 0 then begin
            FSendRequestList.Add(pGsnd.Name);
//            PatchUnit.g_PatchClientManager.SendProcMsg(Self, pGsnd.Name, PM_WAV, 0);
          end;
          Dispose(pGsnd);
          pGsnd := nil;
        end;
        FNewList.Delete(i);
        Inc(ii);
        if ii > 10 then
          Break;                        //12345
      end;
    finally
      FNewList.UnLock;
    end;

    Sleep(0);

    FList.Lock;
    try
      for i := FList.count - 1 downto 0 do begin
        pGsnd := pTGameSound(FList[i]);
        if pGsnd.New then begin
          pGsnd.New := False;
          pGsnd.Tick := GetTickCount;
          pGsnd.Cnl := nil;
          pGsnd.Cnl := pGsnd.Snd.PlayEx(pGsnd.Val, pGsnd.Pan, FPitch, pGsnd.Bkg);
        end;
      end;
      if (FList.count > 0) and (GetTickCount - FCacheTick > 100) then begin
        FCacheTick := GetTickCount;
        n := 16;
        while n > 0 do begin
          if FList.count <= 0 then Break;
          pGsnd := pTGameSound(FList[0]);
          if (pGsnd.Cnl <> nil) and pGsnd.Cnl.IsPlaying then begin
            pGsnd.Tick := GetTickCount;
            FList.Move(0, FList.count - 1);
          end else begin
            if GetTickCount - pGsnd.Tick > 8 * 1000 then begin
              Dispose(pGsnd);
              pGsnd := nil;
              FList.Delete(0);
            end else begin
              FList.Move(0, FList.count - 1);
            end;
          end;
          Dec(n);
        end;
      end;
    finally
      FList.UnLock;
    end;
    if FBackGround.New then begin
      InterlockedExchange(Integer(FBackGround.New), Integer(False));
      FBackGround.Snd := nil;
      FBackGround.Cnl := nil;
      FBackGround.Snd := Effect_Load(FBackGround.Name);
      if (FBackGround.Snd <> nil) then begin
        //FBackGround.New := False;
        InterlockedExchange(Integer(FBackGround.New), Integer(False));
        FBackGround.Cnl := FBackGround.Snd.PlayEx(FBackGround.Val, FBackGround.Pan, FPitch, FBackGround.Bkg);
      end;
    end;
    Sleep(1);
  end;
end;

procedure TSoundMgr.LoadSoundList(flname: string);
var
  i, k, idx, n              : Integer;
  strlist                   : TStringlist;
  Str, data                 : string;
begin
  if FileExists(flname) then begin
    strlist := TStringlist.Create;
    strlist.LoadFromFile(flname);
    idx := 0;
    for i := 0 to strlist.count - 1 do begin
      Str := strlist[i];
      if Str <> '' then begin
        if Str[1] = ';' then Continue;
        Str := Trim(GetValidStr3(Str, data, [':', ' ', #9]));
        n := Str_ToInt(data, 0);
        if n > idx then begin
          for k := 0 to n - FSoundList.count - 1 do
            FSoundList.Add('');
          FSoundList.AddObject(Str, TObject(Integer(FileExists(Str))));
          idx := n;
        end;
      end;
    end;
    strlist.Free;
  end
  else
  begin
    AddDownLoadFile(flname);
  end;
end;

procedure TSoundMgr.LoadBGMusicList(flname: string);
var
  strlist                   : TStringlist;
  Str, sMapName, sFileName  : string;
  pFileName                 : ^string;
  i                         : Integer;
begin
  if FileExists(flname) then begin
    strlist := TStringlist.Create;
    strlist.LoadFromFile(flname);
    for i := 0 to strlist.count - 1 do begin
      Str := strlist[i];
      if (Str = '') or (Str[1] = ';') then Continue;
      Str := GetValidStr3(Str, sMapName, [':', ' ', #9]);
      Str := GetValidStr3(Str, sFileName, [':', ' ', #9]);
      sMapName := Trim(sMapName);
      sFileName := Trim(sFileName);

      if (sMapName <> '') and (sFileName <> '') then begin
        New(pFileName);
        pFileName^ := sFileName;
        FBGMusicList.AddObject(sMapName, TObject(pFileName));
      end;
    end;
    strlist.Free;
  end
  else
  begin
    AddDownLoadFile(flname);
  end;
end;

procedure TSoundMgr.SoundDone;
begin
  if (FBass <> 0) then begin
    BASS_Stop;
    BASS_Free;
    FinalizeBassDLL;
    FBass := 0;
  end;
end;

function TSoundMgr.SoundInit: Boolean;
begin
  if (FBass <> 0) then begin
    Result := True;
    Exit;
  end;

  Result := False;

  if (InitializeBassDLL) then
    FBass := GetBassDLLHandle
  else
    FBass := 0;

  if (FBass = 0) then begin
    DebugOutStr('Can''t load BASS.DLL');
    //MessageBox(0, 'Can''t load BASS.DLL', 'Err', MB_OK);
    Exit;
  end;

  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then begin
    DebugOutStr('Incorrect BASS.DLL version');
    //MessageBox(0, 'Incorrect BASS.DLL version', 'Err', MB_OK);
    Exit;
  end;

  FSilent := False;
  if (not BASS_Init(-1, FSampleRate, 0, FWnd, nil)) then begin
    //DebugOutStr('BASS Init failed, using no sound');
    BASS_Init(0, FSampleRate, 0, FWnd, nil);
    FSilent := True;
  end else begin
    //DebugOutStr(format('Sound Device: %s', [BASS_GetDeviceDescription(1)]));
    //DebugOutStr(format('Sample rate: %d', [FSampleRate]));
  end;

  SetFXVolume(FFXVolume);
  SetMusVolume(FMusVolume);

  Result := True;
end;

procedure TSoundMgr.SetFXVolume(const Vol: Integer);
begin
  if (FBass <> 0) then
    BASS_SetConfig(BASS_CONFIG_GVOL_SAMPLE, Vol);
end;

procedure TSoundMgr.SetMusVolume(const Vol: Integer);
begin
  if (FBass <> 0) then
    BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, Vol);
end;

function TSoundMgr.Effect_Load(const data: Pointer; const Size: Longword): IEffect;
var
  Length, Samples           : Longword;
  HS                        : HSample;
  HStrm                     : HStream;
  Info                      : BASS_CHANNELINFO;
  Buffer                    : Pointer;
begin
  if (FBass <> 0) then begin
    if (FSilent) then begin
      Result := nil;
      Exit;
    end;

    HS := BASS_SampleLoad(True, data, 0, Size, 4, BASS_SAMPLE_OVER_VOL);
    if (HS = 0) then begin
      HStrm := BASS_StreamCreateFile(True, data, 0, Size, BASS_STREAM_DECODE);
      if (HStrm <> 0) then begin
        Length := BASS_ChannelGetLength(HStrm);
        BASS_ChannelGetInfo(HStrm, &info);
        Samples := Length;
        if (Info.chans < 2) then
          Samples := Samples shr 1;
        if ((Info.Flags and BASS_SAMPLE_8BITS) = 0) then
          Samples := Samples shr 1;
        Buffer := BASS_SampleCreate(Samples, Info.freq, 2, 4, Info.Flags or BASS_SAMPLE_OVER_VOL);
        if (Buffer = nil) then begin
          BASS_StreamFree(HStrm);
          //PostError('Can''t create sound effect: Not enough memory');
        end else begin
          BASS_ChannelGetData(HStrm, Buffer, Length);
          HS := BASS_SampleCreateDone;
          BASS_StreamFree(HStrm);
          //if (HS = 0) then
          //  PostError('Can''t create sound effect');
        end;
      end;
    end;
    Result := TEffect.Create(HS);
  end else
    Result := nil;
end;

function TSoundMgr.Effect_Load(const FileName: string): IEffect;
var
  data                      : IResource;
  Size                      : Integer;
begin
  data := Resource_Load(FileName, @Size);
  if (data = nil) then begin
    Result := nil;
  end else begin
    Result := Effect_Load(data.Handle, Size);
    data := nil;
  end;
end;

function TSoundMgr.Effect_Play(const Eff: IEffect): IChannel;
begin
  Result := Eff.Play;
end;

function TSoundMgr.Effect_PlayEx(const Eff: IEffect; const Volume, Pan: Integer;
  const Pitch: Single; const Loop: Boolean): IChannel;
begin
  Result := Eff.PlayEx(Volume, Pan, Pitch, Loop);
end;

function TSoundMgr.Resource_Load(const FileName: string;
  const Size: PLongword): IResource;
const
  ResErr                    = 'Can''t load resource %d: %s';
var
  data                      : Pointer;
  Name, ZipName             : string;
  PZipName                  : array[0..MAX_PATH] of char;
  nSize                     : Integer;
  Done, i                   : Integer;
  F                         : THandle;
  BytesRead                 : Cardinal;
begin
  Result := nil;
  data := nil;
  if (FileName = '') then
    Exit;

  if (not (FileName[1] in ['\', '/', ':', '.'])) then begin
    // Load from pack
    Name := UpperCase(FileName);
    for i := 1 to Length(Name) do
      if (Name[i] = '/') then
        Name[i] := '\';
  end;

  // Load from file
  F := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
  if (F = INVALID_HANDLE_VALUE) then begin
    //PostError(Format(ResErr, [3, Filename]));
    Exit;
  end;

  Result := nil;
  nSize := GetFileSize(F, nil);
  try
    GetMem(data, nSize);
  except
    CloseHandle(F);
    Exit;
  end;

  if (not ReadFile(F, data^, nSize, BytesRead, nil)) then begin
    CloseHandle(F);
    FreeMem(data);
    Exit;
  end;

  Result := TResource.Create(data, BytesRead);
  CloseHandle(F);
  if Assigned(Size) then
    Size^ := BytesRead;
end;

procedure TSoundMgr.ItemClickSound(Std: TClientStdItem);
begin
  case Std.StdMode of
    0: PlaySound(s_click_drug);
    31: if Std.AniCount in [1..3] then PlaySound(s_click_drug) else PlaySound(s_itmclick);
    5, 6: PlaySound(s_click_weapon);
    10..13: PlaySound(s_click_armor);
    22, 23: PlaySound(s_click_ring);
    24, 26: begin
        if (Pos('手镯', Std.Name) > 0) or (Pos('手套', Std.Name) > 0) then
          PlaySound(s_click_grobes)
        else
          PlaySound(s_click_armring);
      end;
    19, 20, 21: PlaySound(s_click_necklace);
    15, 16: PlaySound(s_click_helmet);
  else
    PlaySound(s_itmclick);
  end;
end;

procedure TSoundMgr.ItemUseSound(StdMode: Integer);
begin
  case StdMode of
    0: PlaySound(s_click_drug);
    1, 2: PlaySound(s_eat_drug);
  else
    PlaySound(s_itmclick);
  end;
end;

end.

