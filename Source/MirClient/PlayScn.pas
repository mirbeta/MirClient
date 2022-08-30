//******************************************************************************
//*                                                                            *
//*                         AsphyreSphinx»æÍ¼ÐÞ¸Ä                              *
//*                         ×÷Õß£ºÅÎÅÎ QQ117594672                             *
//*                                                                            *
//******************************************************************************
unit PlayScn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, uCommon,
  PXL.Canvas, PXL.Textures, PXL.Types,  uGameEngine, IntroScn, Grobal2, HUtil32,
  HumanActor, Actor, HerbActor, AxeMon, SoundUtil, clEvent, WIL, cliUtil,
  StdCtrls, ClFunc, magiceff, ExtCtrls, MShare, MMSystem;

const
  LONGHEIGHT_IMAGE          = 35;
  FLASHBASE                 = 410;
  SOFFX                     = 0;
  SOFFY                     = 0;
  LMX                       = 30;
  LMY                       = 26;

  MAXLIGHT                  = 5;
  LightFiles                : array[0..MAXLIGHT] of string = (
    '.\Data\lig0a.dat',
    '.\Data\lig0b.dat',
    '.\Data\lig0c.dat',
    '.\Data\lig0d.dat',
    '.\Data\lig0e.dat',
    '.\Data\lig0f.dat'
    );

  LightMask0                : array[0..2, 0..2] of shortint = (
    (0, 1, 0),
    (1, 3, 1),
    (0, 1, 0)
    );
  LightMask1                : array[0..4, 0..4] of shortint = (
    (0, 1, 1, 1, 0),
    (1, 1, 3, 1, 1),
    (1, 3, 4, 3, 1),
    (1, 1, 3, 1, 1),
    (0, 1, 2, 1, 0)
    );
  LightMask2                : array[0..8, 0..8] of shortint = (
    (0, 0, 0, 1, 1, 1, 0, 0, 0),
    (0, 0, 1, 2, 3, 2, 1, 0, 0),
    (0, 1, 2, 3, 4, 3, 2, 1, 0),
    (1, 2, 3, 4, 4, 4, 3, 2, 1),
    (1, 3, 4, 4, 4, 4, 4, 3, 1),
    (1, 2, 3, 4, 4, 4, 3, 2, 1),
    (0, 1, 2, 3, 4, 3, 2, 1, 0),
    (0, 0, 1, 2, 3, 2, 1, 0, 0),
    (0, 0, 0, 1, 1, 1, 0, 0, 0)
    );
  LightMask3                : array[0..10, 0..10] of shortint = (
    (0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
    (0, 0, 0, 1, 2, 2, 2, 1, 0, 0, 0),
    (0, 0, 1, 2, 3, 3, 3, 2, 1, 0, 0),
    (0, 1, 2, 3, 4, 4, 4, 3, 2, 1, 0),
    (1, 2, 3, 4, 4, 4, 4, 4, 3, 2, 1),
    (2, 3, 4, 4, 4, 4, 4, 4, 4, 3, 2),
    (1, 2, 3, 4, 4, 4, 4, 4, 3, 2, 1),
    (0, 1, 2, 3, 4, 4, 4, 3, 2, 1, 0),
    (0, 0, 1, 2, 3, 3, 3, 2, 1, 0, 0),
    (0, 0, 0, 1, 2, 2, 2, 1, 0, 0, 0),
    (0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
    );

  LightMask4                : array[0..14, 0..14] of shortint = (
    (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 1, 1, 2, 2, 2, 1, 1, 0, 0, 0, 0),
    (0, 0, 0, 1, 1, 2, 3, 3, 3, 2, 1, 1, 0, 0, 0),
    (0, 0, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1, 1, 0, 0),
    (0, 1, 1, 2, 3, 4, 4, 4, 4, 4, 3, 2, 1, 1, 0),
    (1, 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 3, 2, 1, 1),
    (1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 2, 1),
    (1, 1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 3, 2, 1, 1),
    (0, 1, 1, 2, 3, 4, 4, 4, 4, 4, 3, 2, 1, 1, 0),
    (0, 0, 1, 1, 2, 3, 4, 4, 4, 3, 2, 1, 1, 0, 0),
    (0, 0, 0, 1, 1, 2, 3, 3, 3, 2, 1, 1, 0, 0, 0),
    (0, 0, 0, 0, 1, 1, 2, 2, 2, 1, 1, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0)
    );

  LightMask5                : array[0..16, 0..16] of shortint = (
    (0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 1, 2, 4, 4, 4, 2, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 1, 2, 4, 4, 4, 4, 4, 2, 1, 0, 0, 0, 0),
    (0, 0, 0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0, 0, 0),
    (0, 0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0, 0),
    (0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0),
    (1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1),
    (1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1),
    (1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1),
    (0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0),
    (0, 0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0, 0),
    (0, 0, 0, 1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 1, 0, 0, 0),
    (0, 0, 0, 0, 1, 2, 4, 4, 4, 4, 4, 2, 1, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 1, 2, 4, 4, 4, 2, 1, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
    );

type
  PShoftInt = ^shortint;
  TLightEffect = record
    Width: Integer;
    Height: Integer;
    PFog: PByte;
  end;
  TLightMapInfo = record
    ShiftX: Integer;
    ShiftY: Integer;
    light: Integer;
    bright: Integer;
  end;

  TProcMagic = record
    nTargetX: Integer;
    nTargetY: Integer;
    xTarget: TActor;
    xMagic: TClientMagic;
    fReacll: Boolean;
    fContinue: Boolean;
    fUnLockMagic: Boolean;
    dwTick: LongWord;
  end;
  pTProcMagic = ^TProcMagic;

  TShakeScreen = class
  private
  public
    boShake_X: Boolean;
    boShake_Y: Boolean;
    nShakeCnt_X: Integer;
    nShakeCnt_Y: Integer;
    nShakeLoopCnt_X: Integer;
    nShakeLoopCnt_Y: Integer;
    nShakeRange_X: Integer;
    nShakeRange_Y: Integer;
    dwShakeTime_X: LongWord;
    dwShakeTick_X: LongWord;
    dwShakeTime_Y: LongWord;
    dwShakeTick_Y: LongWord;
    constructor Create;
    function GetShakeRect(tick: LongWord): TIntRect;
    procedure SetScrShake_X(cnt: Integer);
    procedure SetScrShake_Y(cnt: Integer);
  end;

  TPlayScene = class(TScene)
//    m_boPlayChange: Boolean;
//    m_dwPlayChangeTick: LongWord;
    m_MapSurface: TCustomDrawableTexture;
    m_ObjSurface: TCustomDrawableTexture;
    m_LightSurface: TCustomDrawableTexture;
  private

//    m_ObjSurface: TAsphyreRenderTargetTexture;
    //m_FogScreen: array[0..MAPSURFACEHEIGHT, 0..MAPSURFACEWIDTH] of word;
    //m_FogScreen: array[0..MAPSURFACEHEIGHT, 0..MAPSURFACEWIDTH] of byte;
    //m_FogScreen: array of array of byte;
//    m_PFogScreen: PByte;
//    m_nFogWidth: Integer;
//    m_nFogHeight: Integer;
    m_Lights: array[0..MAXLIGHT] of TLightEffect;
    m_dwMoveTime: LongWord;
    m_dwPlayTime: LongWord;
    //m_nMoveStepCount: Integer;
    m_dwAniTime: LongWord;
    m_nAniCount: Integer;
    m_nDefXX: Integer;
    m_nDefYY: Integer;
    m_MainSoundTimer: TTimer;
    m_LightMap: array[0..LMX, 0..LMY] of TLightMapInfo;

    procedure LoadFog;
    procedure ClearLightMap;
    procedure AddLight(X, Y, ShiftX, ShiftY, light: Integer; nocheck: Boolean);
    procedure UpdateBright(X, Y, light: Integer);
    function CheckOverLight(X, Y, light: Integer): Boolean;
    procedure ApplyLightMap;
    procedure DrawLightEffect(lx, ly, bright: Integer);
    procedure SoundOnTimer(Sender: TObject);
    function CrashManEx(mx, my: Integer): Boolean;
    procedure ClearDropItemA();
  public
    //EdChat: TEdit;
    MemoLog: TMemo;
    m_ActorList: TList;
    m_EffectList: TList;
    m_FlyList: TList;
    m_dwBlinkTime: LongWord;
    m_boViewBlink: Boolean;
    ProcMagic: TProcMagic;
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure OpenScene; override;
    procedure CloseScene; override;
    procedure OpeningScene; override;
    procedure DrawMiniMap(Surface: TCustomCanvas);
    procedure PlayScene(MSurface: TCustomCanvas); override;
    procedure LightSurface(MSurface: TCustomCanvas);
    function ButchAnimal(X, Y: Integer): TActor;

    procedure DrawTileMap(Surface: TCustomCanvas);

    procedure EdChatKeyPress(Sender: TObject; var Key: Char);
    procedure EdChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EdChatOnChange(Sender: TObject);
    function FindActor(id: Integer): TActor; overload;
    function FindActor(sName: string): TActor; overload;
    function FindActorXY(X, Y: Integer): TActor;
    function IsValidActor(Actor: TActor): Boolean;
    function NewActor(chrid: Integer; cx, cy, cdir: Word; cfeature, cstate: Integer): TActor;
    procedure ActorDied(Actor: TObject);
    procedure SetActorDrawLevel(Actor: TObject; Level: Integer);
    procedure ClearActors;
    function DeleteActor(id: Integer; boDeath: Boolean = False): TActor;
    procedure DelActor(Actor: TObject);
    procedure SendMsg(ident, chrid, X, Y, cdir, Feature, State: Integer; Str: string; IPInfo: Integer = 0);
    procedure NewMagic(aowner: TActor; magid, Magnumb, cx, cy, tx, ty, TargetCode: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer; var boFly: Boolean; maglv: Integer = 0; Poison: Integer = 0);
    procedure DelMagic(magid: Integer);
    function NewFlyObject(aowner: TActor; cx, cy, tx, ty, TargetCode: Integer; Mtype: TMagicType): TMagicEff;
    procedure ScreenXYfromMCXY(cx, cy: Integer; var sX, sY: Integer);
    procedure CXYfromMouseXY(mx, my: Integer; var ccx, ccy: Integer);
    function GetCharacter(X, Y, wantsel: Integer; var nowsel: Integer; liveonly: Boolean): TActor;
    function GetAttackFocusCharacter(X, Y, wantsel: Integer; var nowsel: Integer; liveonly: Boolean): TActor;
    function IsSelectMyself(X, Y: Integer): Boolean;
    function GetDropItems(X, Y: Integer; var inames: string): pTDropItem;
    function GetXYDropItems(nX, nY: Integer): pTDropItem;
    procedure GetXYDropItemsList(nX, nY: Integer; var ItemList: TList);
    function CanRun(sX, sY, ex, ey: Integer): Boolean;
    function CanWalk(mx, my: Integer): Boolean;
    function CanWalkEx(mx, my: Integer): Boolean;
    function CrashMan(mx, my: Integer): Boolean;
    function CanFly(mx, my: Integer): Boolean;
    procedure CleanObjects;
    procedure DropItemsShow(Surface: TCustomCanvas);
    procedure InitSurface;
    procedure UnInitSurface;
    procedure ProcessObecjts;
  end;

var
  g_o                       : LongInt;
  g_w                       : LongInt;
  g_Bit                     : TBitmap;
  g_ProcDrawChrPos          : Integer = 0;
  g_ProcDrawChr             : LongInt;
  g_nEffectAction           : Integer = 0; //µØÍ¼ÌØÐ§¶¯×÷

implementation

uses
  AsphyreTextureFonts, ClMain, FState, MapUnit, EdCode;

constructor TShakeScreen.Create;
begin
  boShake_X := False;
  nShakeCnt_X := 0;
  boShake_Y := False;
  nShakeCnt_Y := 0;

  nShakeLoopCnt_X := 0;
  nShakeLoopCnt_Y := 0;

  dwShakeTime_X := 0;
  dwShakeTime_Y := 0;

  nShakeRange_X := 5;
  nShakeRange_Y := 5;
end;

procedure TShakeScreen.SetScrShake_X(cnt: Integer);
begin
  if boShake_X or not g_gcGeneral[10] then Exit;
  boShake_X := True;
  dwShakeTick_X := GetTickCount();
  nShakeCnt_X := 0;
  nShakeLoopCnt_X := cnt;

  nShakeRange_X := cnt;
end;

procedure TShakeScreen.SetScrShake_Y(cnt: Integer);
begin
  if boShake_Y or not g_gcGeneral[10] then Exit;
  boShake_Y := True;
  dwShakeTick_Y := GetTickCount();
  nShakeCnt_Y := 0;
  nShakeLoopCnt_Y := cnt;
  nShakeRange_Y := cnt;
end;

function TShakeScreen.GetShakeRect(tick: LongWord): TIntRect;
var
  i                         : Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := MAPSURFACEWIDTH - SOFFX * 2;
  Result.Bottom := MAPSURFACEHEIGHT;
  if boShake_X then begin
    if nShakeLoopCnt_X > 0 then begin
      if nShakeCnt_X < nShakeRange_X then begin
        if tick - dwShakeTick_X > dwShakeTime_X then begin
          dwShakeTick_X := tick;

          i := nShakeRange_X;

          Dec(i, nShakeCnt_X);
          Result.Left := i;

          Inc(i, MAPSURFACEWIDTH - SOFFX * 2);
          Result.Right := i;

          Inc(nShakeCnt_X);
        end;
      end else begin
        if nShakeRange_X > 1 then
          Dec(nShakeRange_X);
        nShakeCnt_X := 0;
        Dec(nShakeLoopCnt_X);
        if nShakeLoopCnt_X <= 0 then
          boShake_X := False;
      end;
    end;
  end;

  if boShake_Y then begin
    if nShakeLoopCnt_Y > 0 then begin
      if nShakeCnt_Y < nShakeRange_Y then begin
        if tick - dwShakeTick_Y > dwShakeTime_Y then begin
          dwShakeTick_Y := tick;

          i := nShakeRange_Y;

          Dec(i, nShakeCnt_Y);
          Result.Top := i;

          Inc(i, MAPSURFACEHEIGHT);
          Result.Bottom := i;

          Inc(nShakeCnt_Y);
        end;
      end else begin
        if nShakeRange_Y > 1 then
          Dec(nShakeRange_Y);
        nShakeCnt_Y := 0;
        Dec(nShakeLoopCnt_Y);
        if nShakeLoopCnt_Y <= 0 then
          boShake_Y := False;
      end;
    end;
  end;
end;

constructor TPlayScene.Create;
begin
  ProcMagic.nTargetX := -1;
  //SetLength(m_FogScreen, MAPSURFACEHEIGHT, MAPSURFACEWIDTH);
  m_MapSurface := nil;
  m_ObjSurface := nil;
  m_LightSurface := nil;
  m_ActorList := TList.Create;
  m_EffectList := TList.Create;
  m_FlyList := TList.Create;
  m_dwBlinkTime := GetTickCount;
  m_boViewBlink := False;

  MemoLog := TMemo.Create(frmMain.Owner);
  with MemoLog do begin
    Parent := frmMain;
    BorderStyle := bsNone;
    Visible := False;
    Ctl3D := True;
    Left := 0;
    Top := 250;
    Width := 300;
    Height := 150;
  end;

  m_dwMoveTime := GetTickCount;
  m_dwAniTime := GetTickCount;
  m_nAniCount := 0;
  //m_nMoveStepCount := 0;
  m_MainSoundTimer := TTimer.Create(frmMain.Owner);
  with m_MainSoundTimer do begin
    OnTimer := SoundOnTimer;
    Interval := 1;
    Enabled := False;
  end;
end;

destructor TPlayScene.Destroy;
begin
  m_ActorList.Free;
  m_EffectList.Free;
  m_FlyList.Free;
  inherited Destroy;
end;

procedure TPlayScene.SoundOnTimer(Sender: TObject);
begin
  g_SndMgr.PlaySound(s_main_theme);
  m_MainSoundTimer.Interval := 46 * 1000;
end;

procedure TPlayScene.EdChatKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    frmMain.SendSay(frmDlg.DEdChat.Text);
    if (frmDlg.DEdChat.Text <> '') and (g_SendSayList.indexof(frmDlg.DEdChat.Text) < 0) then
      g_SendSayList.Add(frmDlg.DEdChat.Text);
    frmDlg.DEdChat.Text := '';
    frmDlg.DEdChat.Visible := False;
    if not g_ChatStatusLarge then frmDlg.DBChat.Visible := False;
    Key := #0;
  end;
  if Key = #27 then begin
    frmDlg.DEdChat.Text := '';
    frmDlg.DEdChat.Visible := False;
    if not g_ChatStatusLarge then frmDlg.DBChat.Visible := False;
    Key := #0;
  end;
end;

procedure TPlayScene.EdChatOnChange(Sender: TObject);
begin
  //
end;

procedure TPlayScene.EdChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  frmMain.FormKeyDown(Sender, Key, Shift);

end;

procedure TPlayScene.InitSurface;
var
  i                         : Integer;
begin
  m_MapSurface := g_DeviceProvider.CreateDrawableTexture(g_GameDevice);
  m_MapSurface.MipMapping := False;
  m_MapSurface.Size := Point2i(SCREENWIDTH + UNITX * 10 + 30, SCREENHEIGHT + UNITY * 10 + 30);
  m_MapSurface.PixelFormat := TPixelFormat.A8R8G8B8;
  m_MapSurface.DepthStencil := TDepthStencil.Full;
  m_MapSurface.Multisamples := 1;
  m_MapSurface.Initialize;

  m_ObjSurface := g_DeviceProvider.CreateDrawableTexture(g_GameDevice);
  m_ObjSurface.MipMapping := False;
  m_ObjSurface.Size := Point2i(SCREENWIDTH + UNITX * 10 + 30, SCREENHEIGHT + UNITY * 10 + 30);
  m_ObjSurface.PixelFormat := TPixelFormat.A8R8G8B8;
  m_ObjSurface.DepthStencil := TDepthStencil.Full;
  m_ObjSurface.Multisamples := 1;
  m_ObjSurface.Initialize;

  m_LightSurface := g_DeviceProvider.CreateDrawableTexture(g_GameDevice);
  m_LightSurface.MipMapping := False;
  m_LightSurface.Size := Point2i(SCREENWIDTH + UNITX * 10 + 30, SCREENHEIGHT + UNITY * 10 + 30);
  m_LightSurface.PixelFormat := TPixelFormat.A8R8G8B8;
  m_LightSurface.DepthStencil := TDepthStencil.Full;
  m_LightSurface.Multisamples := 1;
  m_LightSurface.Initialize;

//  m_MapSurface := Factory.CreateRenderTargetTexture;
//  m_MapSurface.Format := apf_A8R8G8B8;
//  m_MapSurface.SetSize(MAPSURFACEWIDTH + UNITX * 7, MAPSURFACEHEIGHT + UNITY * 6, True);
//  m_ObjSurface := Factory.CreateRenderTargetTexture;
//  m_ObjSurface.Format := apf_A8R8G8B8;
//  m_ObjSurface.SetSize(MAPSURFACEWIDTH - SOFFX * 2, MAPSURFACEHEIGHT);
end;

procedure TPlayScene.UnInitSurface;
var
  i                         : Integer;
begin
  if Assigned(m_MapSurface) then FreeAndNil(m_MapSurface);
  if Assigned(m_ObjSurface) then FreeAndNil(m_ObjSurface);
  if Assigned(m_LightSurface) then FreeAndNil(m_LightSurface);
end;

procedure TPlayScene.Initialize;
var
  i                         : Integer;
begin
  InitSurface();
//  m_nFogWidth := MAPSURFACEWIDTH - SOFFX * 2;
//  m_nFogHeight := MAPSURFACEHEIGHT;
//  //m_PFogScreen := @m_FogScreen;
//  GetMem(m_PFogScreen, MAPSURFACEHEIGHT * MAPSURFACEWIDTH);
//  ZeroMemory(m_PFogScreen, MAPSURFACEHEIGHT * MAPSURFACEWIDTH);
//  g_boViewFog := False;
//  for i := 0 to MAXLIGHT do
//    m_Lights[i].PFog := nil;
//{$IF VIEWFOG}
//  LoadFog;                              //1001
//{$IFEND VIEWFOG}
end;

procedure TPlayScene.Finalize;
begin
  UnInitSurface();
end;

procedure TPlayScene.OpenScene;
begin
  frmDlg.ViewBottomBox(True);
end;

procedure TPlayScene.CloseScene;
begin
  g_SndMgr.SilenceSound;
  frmDlg.DEdChat.Visible := False;
  frmDlg.DBChat.Visible := False;
  frmDlg.ViewBottomBox(False);
end;

procedure TPlayScene.OpeningScene;
begin

end;

function IsMySlaveObject(atc: TActor): Boolean;
var
  i                         : Integer;
begin
  Result := False;
  if g_MySelf = nil then Exit;
  for i := 0 to g_MySelf.m_SlaveObject.Count - 1 do begin
    if atc = TActor(g_MySelf.m_SlaveObject[i]) then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TPlayScene.CleanObjects;
var
  i                         : Integer;
begin
  for i := m_ActorList.Count - 1 downto 0 do begin
    if (TActor(m_ActorList[i]) <> g_MySelf) and
      (TActor(m_ActorList[i]) <> g_MySelf.m_HeroObject) and not IsMySlaveObject(TActor(m_ActorList[i]))
      {(TActor(m_ActorList[i]) <> g_MySelf.m_SlaveObject)}then begin //BLUE
      TActor(m_ActorList[i]).Free;
      m_ActorList.Delete(i);
    end;
  end;
  g_TargetCret := nil;
  g_FocusCret := nil;
  g_MagicTarget := nil;
  for i := 0 to m_EffectList.Count - 1 do
    TMagicEff(m_EffectList[i]).Free;
  m_EffectList.Clear;
end;

procedure TPlayScene.DrawTileMap(Surface: TCustomCanvas);
var
  i, j, nY, nX, nImgNumber  : Integer;
  dsurface                  : TCustomLockableTexture;
  fReGetTileMap             : Boolean;
begin
  if g_MySelf = nil then exit;
  Map.m_OldClientRect := Map.m_ClientRect;

  //if not g_boDrawTileMap then
  //  Exit;
  with Map.m_ClientRect do begin
    nY := - UNITY * 2;
    for j := (Top - Map.m_nBlockTop - 1) to (Bottom - Map.m_nBlockTop + 1) do begin
      nX := AAX - UNITX;
      for i := (Left - Map.m_nBlockLeft - 2) to (Right - Map.m_nBlockLeft + 1) do begin
        if (i >= 0) and (i < LOGICALMAPUNIT * 3) and (j >= 0) and (j < LOGICALMAPUNIT * 3) then begin
          if (i mod 2 = 0) and (j mod 2 = 0) then begin
            nImgNumber := (Map.m_MArr[i, j].wBkImg and $7FFF);
            if nImgNumber > 0 then begin
              nImgNumber := nImgNumber - 1;
              dsurface := MShare.GetTiles(Map.m_MArr[i, j].btTiles, nImgNumber);
              {dsurface := nil;
              case Map.m_MArr[i, j].btTiles of
                0: dsurface := g_WTilesImages.Images[nImgNumber];
                1: dsurface := g_WTiles2Images.Images[nImgNumber]
              end;}
              if dsurface <> nil then begin
                Surface.Draw(nX, nY, dsurface.ClientRect, dsurface, False);
              end;
            end;
          end;
        end;
        Inc(nX, UNITX);
      end;
      Inc(nY, UNITY);
    end;
  end;

  //µØÍ¼ÖÐ¼ä²ã
  with Map.m_ClientRect do begin
    nY := - UNITY * 2;                   //1234
    for j := (Top - Map.m_nBlockTop - 1) to (Bottom - Map.m_nBlockTop + 1) do begin
      nX := AAX - UNITX;
      for i := (Left - Map.m_nBlockLeft - 2) to (Right - Map.m_nBlockLeft + 1) do begin
        if (i >= 0) and (i < LOGICALMAPUNIT * 3) and (j >= 0) and (j < LOGICALMAPUNIT * 3) then begin
          nImgNumber := Map.m_MArr[i, j].wMidImg;
          if nImgNumber > 0 then begin
            nImgNumber := nImgNumber - 1;
            dsurface := MShare.GetSmTiles(Map.m_MArr[i, j].btsmTiles, nImgNumber);
            {dsurface := nil;
            case Map.m_MArr[i, j].btsmTiles of
              0: dsurface := g_WSmTilesImages.Images[nImgNumber];
              1: dsurface := g_WSmTiles2Images.Images[nImgNumber]
            end;}
            if dsurface <> nil then
              Surface.Draw(nX, nY, dsurface.ClientRect, dsurface, True);
          end;
        end;
        Inc(nX, UNITX);
      end;
      Inc(nY, UNITY);
    end;
  end;

end;

procedure TPlayScene.LoadFog;
var
  i, fhandle, W, H, prevsize: Integer;
  cheat                     : Boolean;
begin
  prevsize := 0;
  cheat := False;
  for i := 0 to MAXLIGHT do begin
    if FileExists(LightFiles[i]) then begin
      fhandle := FileOpen(LightFiles[i], fmOpenRead or fmShareDenyNone);
      FileRead(fhandle, W, SizeOf(Integer));
      FileRead(fhandle, H, SizeOf(Integer));
      m_Lights[i].Width := W;
      m_Lights[i].Height := H;
      m_Lights[i].PFog := AllocMem(W * H + 8);
      if prevsize < W * H then begin
        FileRead(fhandle, m_Lights[i].PFog^, W * H);
      end else
        cheat := True;
      prevsize := W * H;
      FileClose(fhandle);
    end;
  end;
  if cheat then
    for i := 0 to MAXLIGHT do begin
      if m_Lights[i].PFog <> nil then
        FillChar(m_Lights[i].PFog^, m_Lights[i].Width * m_Lights[i].Height + 8, #0);
    end;
end;

procedure TPlayScene.ClearDropItemA;
var
  i                         : Integer;
  DropItem                  : pTDropItem;
begin
  for i := g_DropedItemList.Count - 1 downto 0 do begin
    DropItem := g_DropedItemList.Items[i];
    if DropItem = nil then begin
      g_DropedItemList.Delete(i);
      //Continue;
      Break;
    end;
    if (abs(DropItem.X - g_MySelf.m_nCurrX) > 20) or (abs(DropItem.Y - g_MySelf.m_nCurrY) > 20) then begin
      Dispose(DropItem);
      g_DropedItemList.Delete(i);
      Break;
    end;
  end;
end;

procedure TPlayScene.ClearLightMap;
var
  i, j                      : Integer;
begin                                   //1001
//{$IF VIEWFOG}
  FillChar(m_LightMap, (LMX + 1) * (LMY + 1) * SizeOf(TLightMapInfo), 0);
  for i := 0 to LMX do
    for j := 0 to LMY do
      m_LightMap[i, j].light := -1;
//{$IFEND VIEWFOG}
end;

procedure TPlayScene.UpdateBright(X, Y, light: Integer);
var
  i, j, r, lx, ly           : Integer;
  pmask                     : ^shortint;
begin                                   //1001
//{$IF VIEWFOG}
  pmask := nil;
  r := -1;
  case light of
    0: begin
        r := 2;
        pmask := @LightMask0;
      end;
    1: begin
        r := 4;
        pmask := @LightMask1;
      end;
    2: begin
        r := 8;
        pmask := @LightMask2;
      end;
    3: begin
        r := 10;
        pmask := @LightMask3;
      end;
    4: begin
        r := 14;
        pmask := @LightMask4;
      end;
    5: begin
        r := 16;
        pmask := @LightMask5;
      end;
  end;
  for i := 0 to r do
    for j := 0 to r do begin
      lx := X - (r div 2) + i;
      ly := Y - (r div 2) + j;
      if (lx in [0..LMX]) and (ly in [0..LMY]) then
        m_LightMap[lx, ly].bright := m_LightMap[lx, ly].bright + PShoftInt(Integer(pmask) + (i * (r + 1) + j) * SizeOf(shortint))^;
    end;
//{$IFEND VIEWFOG}
end;

function TPlayScene.CheckOverLight(X, Y, light: Integer): Boolean;
var
  i, j, r, mlight, lx, ly, Count, check: Integer;
  pmask                     : ^shortint;
begin                                   //1001
//{$IF VIEWFOG}
  pmask := nil;                         //jacky
  check := 0;                           //jacky
  r := -1;
  case light of
    0: begin
        r := 2;
        pmask := @LightMask0;
        check := 0;
      end;
    1: begin
        r := 4;
        pmask := @LightMask1;
        check := 4;
      end;
    2: begin
        r := 8;
        pmask := @LightMask2;
        check := 8;
      end;
    3: begin
        r := 10;
        pmask := @LightMask3;
        check := 18;
      end;
    4: begin
        r := 14;
        pmask := @LightMask4;
        check := 30;
      end;
    5: begin
        r := 16;
        pmask := @LightMask5;
        check := 40;
      end;
  end;
  Count := 0;
  for i := 0 to r do
    for j := 0 to r do begin
      lx := X - (r div 2) + i;
      ly := Y - (r div 2) + j;
      if (lx in [0..LMX]) and (ly in [0..LMY]) then begin
        mlight := PShoftInt(Integer(pmask) + (i * (r + 1) + j) * SizeOf(shortint))^;
        if m_LightMap[lx, ly].bright < mlight then begin
          Inc(Count, mlight - m_LightMap[lx, ly].bright);
          if Count >= check then begin
            Result := False;
            Exit;
          end;
        end;
      end;
    end;
  Result := True;
//{$IFEND VIEWFOG}
end;

procedure TPlayScene.AddLight(X, Y, ShiftX, ShiftY, light: Integer; nocheck: Boolean);
var
  lx, ly                    : Integer;
begin                                   //1001
//{$IF VIEWFOG}
  lx := X - g_MySelf.m_nRx + LMX div 2;
  ly := Y - g_MySelf.m_nRy + LMY div 2;
  if (lx >= 1) and (lx < LMX) and (ly >= 1) and (ly < LMY) then begin
    if m_LightMap[lx, ly].light < light then begin
      if not CheckOverLight(lx, ly, light) or nocheck then begin
        UpdateBright(lx, ly, light);
        m_LightMap[lx, ly].light := light;
        m_LightMap[lx, ly].ShiftX := ShiftX;
        m_LightMap[lx, ly].ShiftY := ShiftY;
      end;
    end;
  end;
//{$IFEND VIEWFOG}
end;

procedure TPlayScene.ApplyLightMap;
var
  i, j, light, defx, defy, lx, ly, lxx, lyy, lcount: Integer;
begin                                   //1001
//{$IF VIEWFOG}
  defx := -UNITX * 2 + AAX - g_MySelf.m_nShiftX;
  defy := -UNITY * 3 - g_MySelf.m_nShiftY;
  lcount := 0;
  for i := 1 to LMX - 1 do
    for j := 1 to LMY - 1 do begin
      light := m_LightMap[i, j].light;
      if light >= 0 then begin
        lx := (i + g_MySelf.m_nRx - LMX div 2);
        ly := (j + g_MySelf.m_nRy - LMY div 2);
        lxx := (lx - Map.m_ClientRect.Left) * UNITX + defx + m_LightMap[i, j].ShiftX;
        lyy := (ly - Map.m_ClientRect.Top) * UNITY + defy + m_LightMap[i, j].ShiftY;

//        FogCopy(m_Lights[light].PFog,
//          0,
//          0,
//          m_Lights[light].Width,
//          m_Lights[light].Height,
//          m_PFogScreen,
//          lxx - (m_Lights[light].Width - UNITX) div 2,
//          lyy - (m_Lights[light].Height - UNITY) div 2 - 5,
//          m_nFogWidth,
//          m_nFogHeight,
//          20);
        Inc(lcount);
      end;
    end;
//{$IFEND VIEWFOG}
end;

procedure TPlayScene.DrawLightEffect(lx, ly, bright: Integer);
begin                                   //1001
//{$IF VIEWFOG}
//  if (bright > 0) and (bright <= MAXLIGHT) then
//    FogCopy(m_Lights[bright].PFog,
//      0,
//      0,
//      m_Lights[bright].Width,
//      m_Lights[bright].Height,
//      m_PFogScreen,
//      lx - (m_Lights[bright].Width - UNITX) div 2,
//      ly - (m_Lights[bright].Height - UNITY) div 2,
//      m_nFogWidth,
//      m_nFogHeight,
//      15);
//{$IFEND VIEWFOG}
end;

const
  g_MapRect                 : TRect = (
    Left: 0;
    Top: 0;
    Right: MINIMAPSIZE;
    Bottom: MINIMAPSIZE
    );

procedure TPlayScene.DrawMiniMap(Surface: TCustomCanvas);
var
  D, dd                     : TCustomLockableTexture;
  v                         : Boolean;
  mx, my, nX, nY, i         : Integer;
  Actor                     : TActor;
  X, Y                      : Integer;
  btColor                   : byte;
  rx, ry, rrx, rry          : Real;
  S                         : string;
  tMapPath                  : array of TPoint;
  pMapDescInfo              : pTMapDescInfo;
begin
  if g_nMiniMapIndex < 0 then Exit;
  if g_nMiniMapIndex >= 300 then begin
    D := g_opui.Images[g_nMiniMapIndex + 1];   //ASP×¢ÊÍ
  end else
    D := g_WMMapImages.Images[g_nMiniMapIndex];
  if D = nil then begin
    g_DrawingMiniMap := False;
    Exit;
  end;
  if g_nViewMinMapLv = 1 then begin
    mx := (g_MySelf.m_nCurrX * 48) div 32;
    my := g_MySelf.m_nCurrY;
    g_MiniMapRC.Left := _MAX(0, mx - 60);
    g_MiniMapRC.Top := _MAX(0, my - 60);
    g_MiniMapRC.Right := _MIN(D.ClientRect.Right, g_MiniMapRC.Left + 120);
    g_MiniMapRC.Bottom := _MIN(D.ClientRect.Bottom, g_MiniMapRC.Top + 120);

    if g_DrawMiniBlend then begin
//      DrawBlend_Mix(Surface, (SCREENWIDTH - 120), 0, D, g_MiniMapRC.Left, g_MiniMapRC.Top, 120, 120, 0);    //ASP×¢ÊÍ
      Surface.DrawAlpha((SCREENWIDTH - 120), 0, g_MiniMapRC, D, 120)
    end else begin
      Surface.Draw(SCREENWIDTH - 120, 0, g_MiniMapRC, D, False); //ASP×¢ÊÍ
    end;

    g_DrawingMiniMap := True;

    if GetTickCount > m_dwBlinkTime + 300 then begin
      m_dwBlinkTime := GetTickCount;
      m_boViewBlink := not m_boViewBlink;
    end;
    if m_boViewBlink then begin
      mx := (SCREENWIDTH - 120) + (g_MySelf.m_nCurrX * 48) div 32 - g_MiniMapRC.Left;
      my := (g_MySelf.m_nCurrY * 32) div 32 - g_MiniMapRC.Top;
      Surface.FillRect(IntRectBDS(mx - 1, my - 1, 1 + 2, 1 + 2), GetRGB(255));
    end;

    for i := 0 to m_ActorList.Count - 1 do begin //opt
      Actor := TActor(m_ActorList[i]);
      if (Actor <> nil) and not Actor.m_boDeath and Actor.m_boVisible and Actor.m_boHoldPlace and (Actor <> g_MySelf) then begin
        if (abs(Actor.m_nCurrX - g_MySelf.m_nCurrX) <= 10) and (abs(Actor.m_nCurrY - g_MySelf.m_nCurrY) <= 10) then begin
          mx := (SCREENWIDTH - 120) + (Actor.m_nCurrX * 48) div 32 - g_MiniMapRC.Left;
          my := Actor.m_nCurrY - g_MiniMapRC.Top;
          case Actor.m_btRace of
            12, 24, 50: btColor := 250;
            54, 55, 81: btColor := 0;
            0: begin
                if Actor.m_btNameColor = 255 then
                  btColor := 251
                else
                  btColor := Actor.m_btNameColor;
                {if Actor.m_btIsHero <> 0 then
                  btColor := 147
                else
                  btColor := 251;}
              end;
          else
            btColor := 249;
          end;
          if Actor.m_btNameColor = 253 then btColor := 253;
          Surface.FillRect(IntRectBDS(mx - 1, my - 1, 1 + 2, 1 + 2), GetRGB(btColor));
        end;
      end;
    end;
    //123456
    for i := 0 to g_xCurMapDescList.Count - 1 do begin
      pMapDescInfo := pTMapDescInfo(g_xCurMapDescList.Objects[i]);
      mx := (SCREENWIDTH - 120) + (pMapDescInfo.nPointX * 48) div 32 - g_MiniMapRC.Left;
      my := pMapDescInfo.nPointY - g_MiniMapRC.Top;
      if (mx >= SCREENWIDTH - 120) and ((my >= 0) and (my <= 120)) then begin
        if g_DrawMiniBlend then
          Surface.BoldTextOut( mx, my, pMapDescInfo.nColor, FontBorderColor, pMapDescInfo.szPlaceName)
        else
          Surface.BoldTextOut( mx, my, pMapDescInfo.nColor, FontBorderColor, pMapDescInfo.szPlaceName)
      end;
    end;
    if g_ShowMiniMapXY then begin
      rx := g_MySelf.m_nCurrX + (g_nMouseX - (SCREENWIDTH - (g_MiniMapRC.Right - g_MiniMapRC.Left)) - ((g_MiniMapRC.Right - g_MiniMapRC.Left) div 2)) * 2 / 3;
      ry := g_MySelf.m_nCurrY + (g_nMouseY - (g_MiniMapRC.Bottom - g_MiniMapRC.Top) div 2);
      if (rx >= 0) and (ry >= 0) then begin
        S := Format('%s:%s', [IntToStr(Round(rx)), IntToStr(Round(ry))]);
        with Surface do begin
          try
            TextOut(SCREENWIDTH - FontManager.Default.TextWidth(S) - 02, g_MiniMapRC.Bottom - g_MiniMapRC.Top - 14, IntToStr(Round(rx)) + ':' + IntToStr(Round(ry)));  //ASP×¢ÊÍ
          finally
          end;
        end;
      end;
    end;
    if g_MapPath <> nil then
      if g_MoveStep <= High(g_MapPath) then
        for i := g_MoveStep to High(g_MapPath) do begin
          mx := (SCREENWIDTH - 120) + (g_MapPath[i].X * 48) div 32 - g_MiniMapRC.Left;
          my := g_MapPath[i].Y - g_MiniMapRC.Top;
          if (mx >= SCREENWIDTH - 120) and ((my >= 0) and (my <= 120)) then
          Surface.FillRect(IntRectBDS(mx, my, 1, 1), GetRGB(224));
        end;
  end else begin
    g_MiniMapRC.Left := SCREENWIDTH - MINIMAPSIZE;
    g_MiniMapRC.Top := 0;
    g_MiniMapRC.Right := SCREENWIDTH;
    g_MiniMapRC.Bottom := MINIMAPSIZE;
    Surface.StretchDraw(g_MiniMapRC,d.ClientRect, d,true);
//    if g_DrawMiniBlend then begin
//      g_DsMiniMapPixel.StretchDraw(g_MapRect, D.ClientRect, D, False);      //ASP×¢ÊÍ
//      DrawBlend_Mix(Surface, (SCREENWIDTH - MINIMAPSIZE), 0, g_DsMiniMapPixel, 0, 0, MINIMAPSIZE, MINIMAPSIZE, 0);
//    end else
//      Surface.StretchDraw(g_MiniMapRC, D.ClientRect, D, False);

    g_DrawingMiniMap := True;
    if GetTickCount > m_dwBlinkTime + 300 then begin
      m_dwBlinkTime := GetTickCount;
      m_boViewBlink := not m_boViewBlink;
    end;
    rx := D.Width / (MINIMAPSIZE * 1.5);
    ry := D.Height / MINIMAPSIZE;
    rrx := rx;
    rry := ry;
    if m_boViewBlink then begin
      mx := g_MiniMapRC.Left + Round(g_MySelf.m_nCurrX / rx);
      my := Round(g_MySelf.m_nCurrY / ry);
      Surface.FillRect(IntRectBDS(mx - 1, my - 1, 1 + 2, 1 + 2), GetRGB(255));
    end;

    if g_boOpenAutoPlay and (g_APMapPath <> nil) then begin
      nX := Round(12 / rx);
      nY := Round(12 / ry);
      SetLength(tMapPath, High(g_APMapPath) + 1);
      for i := 0 to High(g_APMapPath) do begin
        mx := Round(g_APMapPath[i].X / rx);
        my := Round(g_APMapPath[i].Y / ry);
        tMapPath[i] := Point(SCREENWIDTH - MINIMAPSIZE + mx, my);
        Surface.FillRect(IntRectBDS(SCREENWIDTH - 200 + mx - 1, my - 1, 1 + 1, 1 + 1), GetRGB(249));

//        with Surface.Canvas do begin         //ASP×¢ÊÍ
//          if i = g_APStep then
//            Pen.Color := clRed
//          else
//            Pen.Color := clLime;
//          Brush.Color := clBlack;
//          Brush.Style := bsClear;
//          RoundRect((SCREENWIDTH - 200 + mx + nX), (0 + my + nY), (SCREENWIDTH - 200 + mx - nX), (0 + my - nY), nX * 2, nY * 2);
//          Release;
//        end;
      end;

//      with Surface.Canvas do begin   //ASP×¢ÊÍ
//        Pen.Color := GetRGB(151);
//        Brush.Color := clBlack;
//        Brush.Style := bsClear;
//        Polygon(tMapPath);
//        Release;
//      end;

    end;

    if g_MapPath <> nil then begin
      if g_MoveStep <= High(g_MapPath) then
        for i := g_MoveStep to High(g_MapPath) do begin
          mx := SCREENWIDTH - 200 + Round(g_MapPath[i].X / rx);
          my := Round(g_MapPath[i].Y / ry);
          Surface.FillRect(IntRectBDS(mx, my, 1, 1), GetRGB(224));
        end;
    end;
    //123456
    for i := 0 to g_xCurMapDescList.Count - 1 do begin
      pMapDescInfo := pTMapDescInfo(g_xCurMapDescList.Objects[i]);
      mx := SCREENWIDTH - 200 + Round(pMapDescInfo.nPointX / rrx);
      my := Round(pMapDescInfo.nPointY / rry);
        if g_DrawMiniBlend then
          Surface.BoldTextOut( mx, my, pMapDescInfo.nColor, FontBorderColor, pMapDescInfo.szPlaceName)
        else
          Surface.BoldTextOut( mx, my, pMapDescInfo.nColor, FontBorderColor, pMapDescInfo.szPlaceName)
    end;
    if g_ShowMiniMapXY then begin
      rx := (g_nMouseX - g_MiniMapRC.Left) * (Map.m_MapHeader.wWidth / MINIMAPSIZE);
      ry := g_nMouseY * (Map.m_MapHeader.wHeight / MINIMAPSIZE);
      if (rx >= 0) and (ry >= 0) then begin
        S := Format('%s:%s', [IntToStr(Round(rx)), IntToStr(Round(ry))]);
        with Surface do begin
//          Windows.SetBkMode(Handle, TRANSPARENT);
          try

            TextOut(SCREENWIDTH - FontManager.Default.TextWidth(S) - 2, MINIMAPSIZE - 14, S);   //ASP×¢ÊÍ
          finally
//            Release;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPlayScene.ProcessObecjts;
var
  i, j, k, n, M, mmm, ix, iy, line, defx, defy, wunit, fridx, ani, anitick, ax, ay, idx: Integer;
  dsurface, D               : TCustomLockableTexture;
  b, blend, movetick        : Boolean;
  DropItem                  : pTDropItem;
  evn                       : TClEvent;
  Actor                     : TActor;
  meff                      : TMagicEff;
  ATexture                  : TCustomLockableTexture;
  ShowItem                  : pTShowItem;
  cc, nFColor, nBColor      : Integer;
//  Imgs                      : TDXImages;
  tick, dwCheckTime         : LongWord;
  boCheckTimeLimit          : Boolean;
  DrawRect                  : TRect;
  AList: TList;
begin
  if g_MySelf = nil then  Exit;

  cc := 0;
  tick := TimeGetTime;

  g_boDoFastFadeOut := False;

  movetick := False;
  if g_boSpeedRate then begin           //move speed
    if tick - m_dwMoveTime >= (95 - g_MoveSpeedRate div 2) then begin //move speed
      m_dwMoveTime := tick;
      movetick := True;
    end;
  end else begin
    if tick - m_dwMoveTime >= 95 then begin
      m_dwMoveTime := tick;
      movetick := True;
    end;
  end;

  try
    i := 0;
    while True do begin                 //DYNAMIC MODE
      if i >= m_ActorList.Count then Break;
      Actor := m_ActorList[i];
      if Actor.m_boDeath and g_gcGeneral[8] and not Actor.m_boItemExplore and (Actor.m_btRace <> 0) and Actor.IsIdle then begin
        Inc(i);
        Continue;
      end;

      if movetick then
        Actor.m_boLockEndFrame := False;

      if not Actor.m_boLockEndFrame then begin
        Actor.ProcMsg;
        if movetick then begin
          if Actor.Move() then begin
            Inc(i);
            Continue;
          end;
        end;
        Actor.Run;
        if Actor <> g_MySelf then
          Actor.ProcHurryMsg;
      end;
      if Actor = g_MySelf then
        Actor.ProcHurryMsg;

      //dogz....
      if Actor.m_nWaitForRecogId <> 0 then begin
        if Actor.IsIdle then begin
          DelChangeFace(Actor.m_nWaitForRecogId);
          NewActor(Actor.m_nWaitForRecogId, Actor.m_nCurrX, Actor.m_nCurrY, Actor.m_btDir, Actor.m_nWaitForFeature, Actor.m_nWaitForStatus);
          Actor.m_nWaitForRecogId := 0;
          Actor.m_boDelActor := True;
        end;
      end;
      if Actor.m_boDelActor then begin
        g_FreeActorList.Add(Actor);
        m_ActorList.Delete(i);
        if g_TargetCret = Actor then
          g_TargetCret := nil;
        if g_FocusCret = Actor then
          g_FocusCret := nil;
        if g_MagicTarget = Actor then
          g_MagicTarget := nil;
      end else
        Inc(i);
    end;
  except
    on E: Exception do DebugOutStr('101 ' + E.Message);
  end;
  (*
  try
//    i := 0;
//    while True do begin                 //DYNAMIC MODE
//      if i >= m_ActorList.Count then Break;
    for I := m_ActorList.Count - 1 downto 0 do begin
      Actor := m_ActorList[i];
      if Actor <> nil then
      begin
        if Actor.m_boDeath and g_gcGeneral[8] and not Actor.m_boItemExplore and (Actor.m_btRace <> 0) and Actor.IsIdle then begin
//          Inc(i);
          Continue;
        end;

        if movetick then
          Actor.m_boLockEndFrame := False;

        if not Actor.m_boLockEndFrame then begin
          Actor.ProcMsg;
          if movetick then begin
            if Actor.Move() then begin
  //            m_boPlayChange := m_boPlayChange;
//              Inc(i);
              Continue;
            end;
          end;
          Actor.Run;
          if Actor <> g_MySelf then
            Actor.ProcHurryMsg;
        end;
        if Actor = g_MySelf then
          Actor.ProcHurryMsg;

        //dogz....
        if Actor.m_nWaitForRecogId <> 0 then begin
          if Actor.IsIdle then begin
            DelChangeFace(Actor.m_nWaitForRecogId);
            NewActor(Actor.m_nWaitForRecogId, Actor.m_nCurrX, Actor.m_nCurrY, Actor.m_btDir, Actor.m_nWaitForFeature, Actor.m_nWaitForStatus);
            Actor.m_nWaitForRecogId := 0;
            Actor.m_boDelActor := True;
          end;
        end;
        if (GetTickCount - Actor.m_dwLastGetMessageTime > 2000) and ((ABS(Actor.m_nCurrX - g_MySelf.m_nCurrX) > 16) or (ABS(Actor.m_nCurrY - g_MySelf.m_nCurrY) > 16)) then
        begin
          Actor.m_boDelActor := True;
        end;
        if Actor.m_boDelActor then begin
          g_FreeActorList.Add(Actor);
          m_ActorList.Delete(i);
          if g_TargetCret = Actor then
            g_TargetCret := nil;
          if g_FocusCret = Actor then
            g_FocusCret := nil;
          if g_MagicTarget = Actor then
            g_MagicTarget := nil;
        end;
      end else
        m_ActorList.Delete(I);
    end;
  except
    on E: Exception do DebugOutStr('101 ' + E.Message);
  end;
//  m_boPlayChange := m_boPlayChange or (GetTickCount > m_dwPlayChangeTick);
  *)
  try                                   //STATIC MODE
    i := 0;
    while True do begin
      if i >= m_EffectList.Count then Break;
      meff := m_EffectList[i];
      if meff.m_boActive then begin
        if not meff.Run then begin
          meff.Free;                    //1003
          m_EffectList.Delete(i);
          Continue;
        end;
      end;
      Inc(i);
    end;

    i := 0;
    while True do begin
      if i >= m_FlyList.Count then Break;
      meff := m_FlyList[i];
      if meff.m_boActive then begin
        if not meff.Run then begin
          meff.Free;
          m_FlyList.Delete(i);
          Continue;
        end;
      end;
      Inc(i);
    end;

    EventMan.Execute;

    if (g_RareBoxWindow <> nil) then begin
      if g_RareBoxWindow.m_boActive then begin
        if not g_RareBoxWindow.Run then
          g_RareBoxWindow.m_boActive := False;
      end;
    end;
    //end;
  except
    DebugOutStr('102');
  end;

  try
    ClearDropItemA();

    for k := 0 to EventMan.EventList.Count - 1 do begin
      evn := TClEvent(EventMan.EventList[k]);
      //0808
      if (abs(evn.m_nX - g_MySelf.m_nCurrX) > 21) or (abs(evn.m_nY - g_MySelf.m_nCurrY) > 21) then begin
        evn.Free;
        EventMan.EventList.Delete(k);
        Break;
      end;
    end;
  except
    DebugOutStr('103');
  end;

  if g_MySelf = nil then  Exit;
  try
    with Map.m_ClientRect do begin
      Left := g_MySelf.m_nRx - g_TileMapOffSetX;
      Right := g_MySelf.m_nRx + g_TileMapOffSetX;
      Top := g_MySelf.m_nRy - g_TileMapOffSetY;
      Bottom := g_MySelf.m_nRy + g_TileMapOffSetY - 1;
    end;
    Map.UpdateMapPos(g_MySelf.m_nRx, g_MySelf.m_nRy);


    if g_boForceMapDraw then
    begin
      if g_boForceMapFileLoad then
        Map.LoadMap(Map.m_sCurrentMap, g_MySelf.m_nCurrX, g_MySelf.m_nCurrY);
      g_boForceMapDraw := False;
      g_boForceMapFileLoad := False;
    end
    else
    begin
      with Map do
        if (m_ClientRect.Left = m_OldClientRect.Left) and (m_ClientRect.Top = m_OldClientRect.Top) then
          Exit;
    end;
  except
  end;
end;

procedure TPlayScene.LightSurface(MSurface: TCustomCanvas);
var
  d: TCustomLockableTexture;
  i, j, defy, defx, lx, ly, lcount, light, lxx, lyy: Integer;
  Actor: TActor;
  DarkColor: Integer;
begin
  if (g_MySelf = nil) then exit;
  if DarkLevel = 1 then DarkColor := $0F0F0F
  else DarkColor := $555555;

  defx := -UNITX*2 + AAX + 14 - g_MySelf.m_nShiftX;
  defy := -UNITY*3 - g_MySelf.m_nShiftY;

  if g_boViewFog and (not g_MySelf.m_boDeath) then begin
    MSurface.FillRectAlpha(IntRectBDS(0, 0, SCREENWIDTH, SCREENHEIGHT), DarkColor, 255);
    lcount := 0;
    for i := 1 to LMX - 1 do begin
      for j := 1 to LMY - 1 do begin
        light := m_LightMap[i, j].light;
        if light >= 0 then begin
          lx := (i + g_MySelf.m_nRx - LMX div 2);
          ly := (j + g_MySelf.m_nRy - LMY div 2);
          lxx := (lx - Map.m_ClientRect.Left) * UNITX + defx + m_LightMap[i, j].shiftx;
          lyy := (ly - Map.m_ClientRect.Top) * UNITY + defy + m_LightMap[i, j].shifty;

          d := g_LightImages[light];
          if d <> nil then
            MSurface.Draw(lxx - (d.Width-UNITX) div 2, lyy - (d.Height-UNITY) div 2, d.ClientRect, d, IntColorWhite, TBlendingEffect.beSrcColorAdd);

          inc(lcount);
        end;
      end;
    end;
  end;
end;

procedure TPlayScene.PlayScene(MSurface: TCustomCanvas);
var
  i, j, k, n, M, mmm, ix, iy, line, defx, defy, wunit, fridx, ani, anitick, ax, ay, idx: Integer;
  dsurface, D ,dd           : TCustomLockableTexture;
  b, blend, movetick        : Boolean;
  DropItem                  : pTDropItem;
  evn                       : TClEvent;
  Actor                     : TActor;
  meff                      : TMagicEff;
  ATexture                  : TCustomLockableTexture;
  ShowItem                  : pTShowItem;
  cc, nFColor, nBColor      : Integer;
//  Imgs                      : TDXImages;
  tick, dwCheckTime         : LongWord;
  boCheckTimeLimit          : Boolean;
  DrawRect                  : TIntRect;
  infoMsg                   : string[255];
  Right                     : Integer;
  t, t2                     : DWORD;
  p                         : pTClientStdItem;
  tani, taniidx, tanioffset, nMidImage, nMidIndex: Integer;
//  OffsetY: Integer;
const
  bIMGBusy                  : Boolean = False;
  msgstr                    = 'ÇëÉÔºò¡­¡­¡­¡­';
begin
//  if not m_boPlayChange then Exit;
  if (g_MySelf = nil) then begin
    ATexture  :=  FontManager.Default.TextOut('ÕýÔÚÍË³öÓÎÏ·£¬ÇëÉÔºó...');
    if ATexture <> nil then
      MSurface.DrawBoldText((SCREENWIDTH - ATexture.Width) div 2, (SCREENHEIGHT - 280) div 2, ATexture, clWhite, FontBorderColor);
    Exit;
  end;

  if g_MySelf = nil then  Exit;

  tick := TimeGetTime;
  if tick - m_dwAniTime >= 50 then begin //»î¶¯ËØ²Ä
    m_dwAniTime := tick;
    Inc(m_nAniCount);
    if m_nAniCount > 100000 then m_nAniCount := 0;
  end;

//  if not g_ProcCanDraw then
//    Exit;

//  MSurface.Fill(0);                     //1234

  try
    if g_boForceNotViewFog or g_boNoDarkness then
      g_boViewFog := False;

    if (g_MySelf.m_boDeath) then begin
      g_boLastViewFog := g_boViewFog;
      g_boViewFog := False;
    end;

    if g_boViewFog then begin
//      ZeroMemory(m_PFogScreen, MAPSURFACEHEIGHT * MAPSURFACEWIDTH);
      ClearLightMap;
    end;

//    DrawTileMap(MSurface);
//  OffsetY := 1 - g_MySelf.m_nShiftY mod 2;
//  MSurface.Draw(0, 0, Bounds(UNITX * 4 + g_MySelf.m_nShiftX, UNITY * 3 + g_MySelf.m_nShiftY, MAPSURFACEWIDTH, MAPSURFACEHEIGHT), m_MapSurface, False);

    MSurface.Draw(0, 0,
      IntRectBDS(UNITX * 3 + g_MySelf.m_nShiftX,
      UNITY * 2 + g_MySelf.m_nShiftY,
      UNITX * 3 + g_MySelf.m_nShiftX + SCREENWIDTH,
      UNITY * 2 + g_MySelf.m_nShiftY + SCREENHEIGHT),
      m_MapSurface,
      False);
  except
    on E: Exception do DebugOutStr('104 ' + E.Message);
  end;

  defx := -UNITX * 2 - g_MySelf.m_nShiftX + AAX;
  defy := -UNITY * 2 - g_MySelf.m_nShiftY;
  m_nDefXX := defx;
  m_nDefYY := defy;

  try
    M := defy - UNITY;
    for j := (Map.m_ClientRect.Top - Map.m_nBlockTop) to (Map.m_ClientRect.Bottom - Map.m_nBlockTop + LONGHEIGHT_IMAGE) do begin
      if j < 0 then begin
        Inc(M, UNITY);
        Continue;
      end;
      n := defx - UNITX * 2;
      for i := (Map.m_ClientRect.Left - Map.m_nBlockLeft - 2) to (Map.m_ClientRect.Right - Map.m_nBlockLeft + 2) do begin
        if (i >= 0) and (i < LOGICALMAPUNIT * 3) and (j >= 0) and (j < LOGICALMAPUNIT * 3) then begin
          fridx := (Map.m_MArr[i, j].wFrImg) and $7FFF;
          if fridx > 0 then begin
            ani := Map.m_MArr[i, j].btAniFrame;
            wunit := Map.m_MArr[i, j].btArea;
            if (ani and $80) > 0 then begin
              blend := True;
              ani := ani and $7F;
            end;
            if ani > 0 then begin
              anitick := Map.m_MArr[i, j].btAniTick;
              fridx := fridx + (m_nAniCount mod (ani + (ani * anitick))) div (1 + anitick);
            end;
            if (Map.m_MArr[i, j].btDoorOffset and $80) > 0 then begin
              if (Map.m_MArr[i, j].btDoorIndex and $7F) > 0 then
                fridx := fridx + (Map.m_MArr[i, j].btDoorOffset and $7F);
            end;
            fridx := fridx - 1;
            dsurface := GetObjs(wunit, fridx);
            if dsurface <> nil then begin
              if (dsurface.Width = 48) and (dsurface.Height = 32) then begin
                mmm := M + UNITY - dsurface.Height;
                if (n + dsurface.Width > 0) and (n <= SCREENWIDTH) and (mmm + dsurface.Height > 0) and (mmm < MAPSURFACEHEIGHT) then begin
                  MSurface.Draw(n, mmm, dsurface.ClientRect, dsurface, True)
                end else if mmm < MAPSURFACEHEIGHT then
                  MSurface.Draw(n, mmm, dsurface.ClientRect, dsurface, True)
              end;
            end;
          end;
        end;
        Inc(n, UNITX);
      end;
      Inc(M, UNITY);
    end;
  except
    on E: Exception do DebugOutStr('105 ' + E.Message);
  end;

  try
    M := defy - UNITY;
    for j := (Map.m_ClientRect.Top - Map.m_nBlockTop) to (Map.m_ClientRect.Bottom - Map.m_nBlockTop + LONGHEIGHT_IMAGE) do begin
      if j < 0 then begin
        Inc(M, UNITY);
        Continue;
      end;
      n := defx - UNITX * 2;
      for i := (Map.m_ClientRect.Left - Map.m_nBlockLeft - 2) to (Map.m_ClientRect.Right - Map.m_nBlockLeft + 2) do begin
        if (i >= 0) and (i < LOGICALMAPUNIT * 3) and (j >= 0) and (j < LOGICALMAPUNIT * 3) then begin

        //20200726±ÈÆæË®ÁúÍ·¶¯Ì¬ ¿ªÊ¼
          //µØÍ¼ÌØÐ§
          nMidImage := Map.m_MArr[i, j].wMidImg;
          if nMidImage > 0 then begin
            if ((Map.m_MArr[i, j].btDoorIndex2 <> 0) and (g_nEffectAction = Map.m_MArr[i, j].btDoorIndex2)) or ((Map.m_MArr[i, j].btDoorOffset2 <> 0) and (g_nEffectAction >= Map.m_MArr[i, j].btDoorOffset2)) then begin
              g_nEffectAction := 0;
            end;
            if g_nEffectAction > 20 then
              g_nEffectAction := 0;

            nMidImage := nMidImage - 1;
            if Map.m_MArr[i, j].btDoorIndex2 > 0 then begin
              nMidImage := g_nEffectAction * LoByte(Map.m_MArr[i, j].wFrImg2) + nMidImage;
              nMidIndex := Map.m_MArr[i, j].btsmTiles;
              DSurface := g_WSmTilesArr[nMidIndex].Images[nMidImage];
              if DSurface <> nil then
                MSurface.Draw(n, m, DSurface.ClientRect, DSurface, True);
            end;
          end;

          //¶¯Ì¬µØÍ¼Ð§¹û
          taniidx := Map.m_MArr[i, j].wBkImg2;
          tani := Map.m_MArr[i, j].btDoorOffset2;

          blend := FALSE; //µØÍ¼ÂÔ¸Ä
          if (tani and $80) > 0 then begin
            blend := TRUE;
            tani := tani and $7F;
          end;

          if (taniidx > 0) and (tani > 0) then begin
            taniidx := taniidx - 1;
            tanioffset := Map.m_MArr[i, j].wAniFrame2 xor $2000;
            taniidx := taniidx + (tanioffset * (m_nAniCount mod tani));

            DSurface := g_WAniTilesArr.GetCachedImage(taniidx, ax, ay);
            if DSurface <> nil then begin
              if blend then begin
                mmm := m + ay - 68;
                if (n > 0) and (mmm + DSurface.Height > 0) and (n + DSurface.Width < SCREENWIDTH) and (mmm < SCREENHEIGHT) then begin
                  MSurface.DrawBlend( n + ax - 2, mmm, DSurface, 1);
                end
                else begin
                  if mmm < SCREENHEIGHT then begin
                    MSurface.DrawBlend( n + ax - 2, mmm, DSurface, 1);
                  end;
                end;
              end
              else if (DSurface.Width = 48) and (DSurface.Height = 32) then begin
                mmm := m + UNITY - DSurface.Height;
                if (n + DSurface.Width > 0) and (n <= SCREENWIDTH) and (mmm + DSurface.Height > 0) and
                  (mmm < SCREENHEIGHT) then begin
                  MSurface.Draw(n, mmm, DSurface.ClientRect, DSurface, TRUE)
                end
                else begin
                  if mmm < SCREENHEIGHT then begin
                    MSurface.Draw(n, mmm, DSurface.ClientRect, DSurface, TRUE)
                  end;
                end;
              end
              else begin
                mmm := m + UNITY - DSurface.Height;
                if (n + DSurface.Width > 0) and (n <= SCREENWIDTH) and (mmm + DSurface.Height > 0) and
                  (mmm < SCREENHEIGHT) then begin
                  MSurface.Draw(n, mmm, DSurface.ClientRect, DSurface, TRUE)
                end
                else begin
                  if mmm < SCREENHEIGHT then begin
                    MSurface.Draw(n, mmm, DSurface.ClientRect, DSurface, TRUE)
                  end;
                end;
              end;
            end;
          end;
          //20200726±ÈÆæË®ÁúÍ·¶¯Ì¬ ½áÊø


          fridx := (Map.m_MArr[i, j].wFrImg) and $7FFF;
          if fridx > 0 then begin
            blend := False;
            wunit := Map.m_MArr[i, j].btArea;
            ani := Map.m_MArr[i, j].btAniFrame;
            if (ani and $80) > 0 then begin
              blend := True;
              ani := ani and $7F;
            end;
            if ani > 0 then begin
              anitick := Map.m_MArr[i, j].btAniTick;
              fridx := fridx + (m_nAniCount mod (ani + (ani * anitick))) div (1 + anitick);
            end;
            if (Map.m_MArr[i, j].btDoorOffset and $80) > 0 then begin
              if (Map.m_MArr[i, j].btDoorIndex and $7F) > 0 then
                fridx := fridx + (Map.m_MArr[i, j].btDoorOffset and $7F);
            end;
            fridx := fridx - 1;
            if not blend then begin
              dsurface := GetObjs(wunit, fridx);
              if dsurface <> nil then begin
                if (dsurface.Width <> 48) or (dsurface.Height <> 32) then begin
                  mmm := M + UNITY - dsurface.Height;
                  if (n + dsurface.Width > 0) and (n <= SCREENWIDTH) and (mmm + dsurface.Height > 0) and (mmm < MAPSURFACEHEIGHT) then begin
                    MSurface.Draw(n, mmm, dsurface.ClientRect, dsurface, True)
                  end else begin
                    if mmm < MAPSURFACEHEIGHT then
                      MSurface.Draw(n, mmm, dsurface.ClientRect, dsurface, True)
                  end;
                end;
              end;
            end else begin
             //----------------ÏÔÊ¾µÆ¹âµÄµØ·½ 20120726 ÅÎÅÎÐÞ¸´µÆ¹â´íÎ»BUG
              dsurface := GetObjsEx(wunit, fridx, ax, ay);
              if dsurface <> nil then begin
                mmm := M + ay - 68;     //UNITY - DSurface.Height;
                if (n > 0) and (mmm + dsurface.Height > 0) and (n + dsurface.Width < SCREENWIDTH) and (mmm < MAPSURFACEHEIGHT) then begin
                  if (Map.m_sCurrentMap = 'n6') or (Map.m_sCurrentMap = 'N6') then //ÐÞÕýÐÂÄ§Áú³ÇµÆÖù»ð´íÎ»µÄÎÊÌâ
                    MSurface.DrawBlend( n + ax - 72, mmm - 140, DSurface, 1)
                  else if (DSurface.Width = 128) and (DSurface.Height = 128) then  //ÐÞÕý×æÂêµÆ»ðÆ«ÒÆ
                    MSurface.DrawBlend( n + ax - 3, mmm + Map.m_MArr[i, j].tempArr[0], DSurface, 1)
                  else
                    MSurface.DrawBlend( n + ax - 2, mmm, DSurface, 1);

                end else begin
                  if mmm < MAPSURFACEHEIGHT then
                    if (Map.m_sCurrentMap = 'n6') or (Map.m_sCurrentMap = 'N6') then //ÐÞÕýÐÂÄ§Áú³ÇµÆÖù»ð´íÎ»µÄÎÊÌâ
                      MSurface.DrawBlend(n + ax - 72, mmm - 140, DSurface, 1)
                    else if (DSurface.Width = 128) and (DSurface.Height = 128) then  //ÐÞÕý×æÂêµÆ»ðÆ«ÒÆ
                      MSurface.DrawBlend(n + ax - 3, mmm + Map.m_MArr[i, j].tempArr[0], DSurface, 1)
                    else
                      MSurface.DrawBlend(n + ax - 2, mmm, DSurface, 1);

                end;
              end;
            end;
          end;

        end;
        Inc(n, UNITX);
      end;

      if (j <= (Map.m_ClientRect.Bottom - Map.m_nBlockTop)) and (not g_boServerChanging) then begin

        for k := 0 to EventMan.EventList.Count - 1 do begin
          evn := TClEvent(EventMan.EventList[k]);
          if j = (evn.m_nY - Map.m_nBlockTop) then
            evn.DrawEvent(MSurface, (evn.m_nX - Map.m_ClientRect.Left) * UNITX + defx, M);
        end;

        if g_boDrawDropItem then begin  //
          for k := 0 to g_DropedItemList.Count - 1 do begin
            DropItem := pTDropItem(g_DropedItemList[k]);
            if DropItem <> nil then begin
              if j = (DropItem.Y - Map.m_nBlockTop) then begin

                D := frmMain.GetWDnItemImg(DropItem.looks);
                if D <> nil then begin
                  ix := (DropItem.X - Map.m_ClientRect.Left) * UNITX + defx + SOFFX;
                  iy := M;
                  MSurface.Draw(ix + HALFX - (D.Width div 2), iy + HALFY - (D.Height div 2), D.ClientRect, D, True);
                  if DropItem = g_FocusItem then
                    DrawEffect(ix + HALFX - (D.Width div 2), iy + HALFY - (D.Height div 2), MSurface, D, ceBright, True);
                end;
              end;
            end;
          end;
        end;

        //if g_ProcActorLimit > 0 then begin
        for k := 0 to m_ActorList.Count - 1 do begin
          Actor := m_ActorList[k];
          if (j = Actor.m_nRy - Map.m_nBlockTop - Actor.m_nDownDrawLevel) then begin
            ix := (Actor.m_nRx - Map.m_ClientRect.Left) * UNITX + defx;
            iy := M + (Actor.m_nDownDrawLevel * UNITY);
            Actor.m_nSayX := ix + Actor.m_nShiftX + 24;
            if Actor.m_boDeath then
              Actor.m_nSayY := Actor.m_nShiftY + iy - 12
            else
              Actor.m_nSayY := Actor.m_nShiftY + iy - 47;

            //if not g_boServerChanging and (g_MagicLockActor = Actor) and not Actor.m_boDeath then begin
            //  g_MagicLockActor.DrawFocus(m_ObjSurface);
            //end;

            Actor.DrawChr(MSurface, //win effect
              ix,
              iy,
              False, True, True);
          end;
        end;
        //end;
//
        for k := 0 to m_FlyList.Count - 1 do begin
          meff := TMagicEff(m_FlyList[k]);
          if j = (meff.ry - Map.m_nBlockTop) then
            meff.DrawEff(MSurface);
        end;
      end;
      Inc(M, UNITY);
    end;
  except
    on E: Exception do DebugOutStr(Format('106 %d ', [cc]) + E.Message);
  end;


  try
    for k := 0 to g_DropedItemList.Count - 1 do begin //1234
      DropItem := pTDropItem(g_DropedItemList[k]);
      if DropItem <> nil then begin
        if DropItem.Shine in [100..249] then
        begin
          if tick - DropItem.FlashStepTime > 200 then begin  //20200927µØÃæÌØÐ§
            DropItem.FlashTime := tick;
            DropItem.FlashStepTime := tick;
            Inc(DropItem.FlashStep);
            if DropItem.FlashStep > 19 then
              DropItem.FlashStep := 0;
          end;
          dsurface := frmMain.GetWDnItemImg(20000 + (DropItem.Shine+1 - 100) * 20-20 + DropItem.FlashStep, ax, ay);
          if dsurface = nil then begin
            DropItem.FlashStep := 0;
            dsurface := frmMain.GetWDnItemImg(20000 + (DropItem.Shine+1 - 100) * 20-20 + DropItem.FlashStep, ax, ay);
          end;

          if dsurface <> nil then
          begin
            ix := (DropItem.X - Map.m_ClientRect.Left) * UNITX + defx + SOFFX;
            iy := (DropItem.Y - Map.m_ClientRect.Top - 1) * UNITY +
              defy + SOFFY;

            MSurface.DrawBlend(ix + ax, iy + ay, dsurface, 1);
          end;

        end;

        if tick - DropItem.FlashTime > g_dwDropItemFlashTime then begin
          DropItem.FlashTime := tick;
          DropItem.BoFlash := True;
          DropItem.FlashStepTime := tick;
          DropItem.FlashStep := 0;
        end;
        ix := (DropItem.X - Map.m_ClientRect.Left) * UNITX + defx + SOFFX;
        iy := (DropItem.Y - Map.m_ClientRect.Top - 1) * UNITY + defy + SOFFY;
        if DropItem.BoFlash then begin
          if tick - DropItem.FlashStepTime >= 20 then begin
            DropItem.FlashStepTime := tick;
            Inc(DropItem.FlashStep);
          end;
          if (DropItem.FlashStep >= 0) and (DropItem.FlashStep < 10) then begin
            dsurface := g_WMainImages.GetCachedImage(FLASHBASE + DropItem.FlashStep, ax, ay);
            MSurface.DrawBlend(ix + ax, iy + ay, dsurface, 1);
          end else
            DropItem.BoFlash := False;
        end;
        //
      end;
    end;
  except
    DebugOutStr('107');
  end;

  if g_gcGeneral[4] then g_PlayScene.DropItemsShow(g_GameCanvas); //20200802

  try
    if g_boViewFog then begin
      M := defy - UNITY * 4;
      for j := (Map.m_ClientRect.Top - Map.m_nBlockTop - 4) to (Map.m_ClientRect.Bottom - Map.m_nBlockTop + LONGHEIGHT_IMAGE) do begin
        if j < 0 then begin
          Inc(M, UNITY);
          Continue;
        end;
        n := defx - UNITX * 5;
        for i := (Map.m_ClientRect.Left - Map.m_nBlockLeft - 5) to (Map.m_ClientRect.Right - Map.m_nBlockLeft + 5) do begin
          if (i >= 0) and (i < LOGICALMAPUNIT * 3) and (j >= 0) and (j < LOGICALMAPUNIT * 3) then begin
            idx := Map.m_MArr[i, j].btLight;
            if idx > 0 then begin
              AddLight(i + Map.m_nBlockLeft, j + Map.m_nBlockTop, 0, 0, idx, False);
            end;
          end;
          Inc(n, UNITX);
        end;
        Inc(M, UNITY);
      end;

      if m_ActorList.Count > 0 then begin
        for k := 0 to m_ActorList.Count - 1 do begin
          Actor := m_ActorList[k];
          if (Actor = g_MySelf) or (Actor.light > 0) then
            AddLight(Actor.m_nRx, Actor.m_nRy, Actor.m_nShiftX, Actor.m_nShiftY, Actor.light, Actor = g_MySelf);
        end;
      end else begin
        if g_MySelf <> nil then
          AddLight(g_MySelf.m_nRx, g_MySelf.m_nRy, g_MySelf.m_nShiftX, g_MySelf.m_nShiftY, g_MySelf.light, True);
      end;
    end;
    Inc(g_nEffectAction);
  except
    DebugOutStr('108');
  end;

  if not g_boServerChanging then begin
    try
      if not g_boCheckBadMapMode then
        if g_MySelf.m_nState and $00800000 = 0 then
          g_MySelf.DrawChr(MSurface, (g_MySelf.m_nRx - Map.m_ClientRect.Left) * UNITX + defx, (g_MySelf.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + defy, True, False)
        else
          g_MySelf.DrawChr_Transparent(MSurface, (g_MySelf.m_nRx - Map.m_ClientRect.Left) * UNITX + defx, (g_MySelf.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + defy, True, False);

      // 20201031 ÏÌÓãÈ¥³ý¹ÖÎïÖÐÂé±ÔÊ±ÔÚ×î¶¥²ãÎÞÍ¸Ã÷
      if (g_FocusCret <> nil) then
      begin
        if IsValidActor(g_FocusCret) and (g_FocusCret <> g_MySelf) then
        begin
          if g_FocusCret.m_nState and $04000000 = 0 then
            if (g_FocusCret.m_nState and $00800000 = 0) then
              g_FocusCret.DrawChr(MSurface,
                (g_FocusCret.m_nRx - Map.m_ClientRect.Left) * UNITX + defx,
                (g_FocusCret.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + defy,
                True, False);

        end;
      end;

      {if (g_MagicTarget <> nil) then begin
        if IsValidActor(g_MagicTarget) and (g_MagicTarget <> g_MySelf) then
          if g_MagicTarget.m_nState and $00800000 = 0 then
            g_MagicTarget.DrawChr(m_ObjSurface,
              (g_MagicTarget.m_nRx - Map.m_ClientRect.Left) * UNITX + defx,
              (g_MagicTarget.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + defy, True, False);
      end;}
      //Ä¿±ê¹â»·
      if (g_MagicLockActor <> nil) then begin
        if (g_MagicLockActor <> g_MySelf) and (g_MagicLockActor.m_sUserName <> '') and not g_MagicLockActor.m_boDeath and IsValidActor(g_MagicLockActor) then begin
          g_MagicLockActor.DrawFocus(MSurface);
        end;
      end;
    except
      DebugOutStr('109');
    end;
  end;

  // DrawBlend...
  try
    for k := 0 to m_ActorList.Count - 1 do begin
      Actor := m_ActorList[k];
      if Actor.m_btRace > 0 then
        Actor.DrawEff(MSurface,
          (Actor.m_nRx - Map.m_ClientRect.Left) * UNITX + defx,
          (Actor.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + defy);
    end;

    for k := 0 to m_EffectList.Count - 1 do begin
      meff := TMagicEff(m_EffectList[k]);
      meff.DrawEff(MSurface);
      if g_boViewFog then
        AddLight(meff.rx, meff.ry, 0, 0, meff.light, False);
    end;

    if (g_RareBoxWindow <> nil) and g_RareBoxWindow.m_boActive then
      g_RareBoxWindow.DrawEff(MSurface);

    if g_boViewFog then begin
      for k := 0 to EventMan.EventList.Count - 1 do begin
        evn := TClEvent(EventMan.EventList[k]);
        if evn.m_nLight > 0 then
          AddLight(evn.m_nX, evn.m_nY, 0, 0, evn.m_nLight, False);
      end;
    end;
  except
    DebugOutStr('110');
  end;


  if g_boViewFog and not g_boForceNotViewFog then begin
    MSurface.Draw(0, 0, m_LightSurface, IntColorWhite, TBlendingEffect.Multiply);
  end;

//  if g_MySelf.m_boDeath then
//    MSurface.FillRectAlpha(MSurface.ClipRect, $0000FF, 160);  //ÈËÎïËÀÍöºìÉ« 160Í¸Ã÷¶È

//  try
//    if g_boViewFog and not g_boForceNotViewFog then begin
//      ApplyLightMap;
//      DrawFog(m_ObjSurface, m_PFogScreen, m_nFogWidth);
//      //shake ...
//      DrawRect := g_ShakeScreen.GetShakeRect(tick);
//      //DrawRect
//      MSurface.Draw(SOFFX, SOFFY, DrawRect, m_ObjSurface, False);
//    end else begin
      if g_MySelf.m_boDeath then        //ÈËÎïËÀÍö£¬ÏÔÊ¾ºÚ°×»­Ãæ
        MSurface.Draw(0, 0, m_ObjSurface, TBlendingEffect.beGrayscale);
      //shake ...
      DrawRect := g_ShakeScreen.GetShakeRect(tick);
      MSurface.Draw(SOFFX, SOFFY, DrawRect, m_ObjSurface);
//    end;
//  except
//    DebugOutStr('111');
//  end;

end;

procedure TPlayScene.NewMagic(aowner: TActor; magid, Magnumb, cx, cy, tx, ty, TargetCode: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer; var boFly: Boolean; maglv: Integer; Poison: Integer);
var
  i, scx, scy, sctx, scty, effnum: Integer;
  meff                      : TMagicEff;
  target                    : TActor;
  wimg                      : TWMImages;
begin
  boFly := False;
  if not (magid in [MAGIC_SOULBALL_ATT3_1..MAGIC_SOULBALL_ATT3_5, 255]) then
    for i := 0 to m_EffectList.Count - 1 do
      if TMagicEff(m_EffectList[i]).ServerMagicId = magid then
        Exit;

  ScreenXYfromMCXY(cx, cy, scx, scy);
  ScreenXYfromMCXY(tx, ty, sctx, scty);

  if Magnumb > 0 then
    GetEffectBase(Magnumb - 1, 0, wimg, effnum)
  else
    effnum := -Magnumb;

  target := FindActor(TargetCode);
  meff := nil;
  case Mtype of
    mtReady {0}, mtFly, mtFlyAxe: begin
        meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
        meff.TargetActor := target;
        case Magnumb of
          39: begin
              meff.frame := 4;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          44: meff := nil;
          63: begin                     //ÊÉ»êÕÓÔó
              meff.MagExplosionBase := 780;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 25;
              meff.light := 3;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          100: begin
              meff.MagExplosionBase := 270;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 5;
              meff.light := 3;
              meff.ImgLib := g_WMagic5Images;
            end;
          101: begin
              meff.MagExplosionBase := 450;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 10;
              meff.light := 3;
              meff.ImgLib := g_WMagic5Images;
            end;
          121: begin
              //DScreen.AddChatBoardString(inttostr(Magnumb) + ' ' + IntToStr(TargetCode) + ' ' + IntToStr(g_MySelf.m_nRecogId), clWhite, clBlue);
              meff.MagExplosionBase := 0;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 8;
              meff.light := 3;
              meff.ImgLib := g_WMagic8Images2;
            end;
          {120,}122: begin
              //DScreen.AddChatBoardString(inttostr(Magnumb) + ' ' + IntToStr(TargetCode) + ' ' + IntToStr(g_MySelf.m_nRecogId), clWhite, clBlue);
              meff.MagExplosionBase := 860;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 20;
              meff.light := 3;
              meff.ImgLib := g_WMagic7Images2;
            end;

        end;
        boFly := True;
      end;
    mtExplosion:
      case Magnumb of
        04: begin
            case maglv div 4 of
              1: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  if Poison = 2 then
                    meff.MagExplosionBase := 830
                  else
                    meff.MagExplosionBase := 620;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 8;
                  meff.light := 2;
                  meff.ImgLib := g_WMagic7Images;
                end;
              2: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  if Poison = 2 then
                    meff.MagExplosionBase := 830 + 10
                  else
                    meff.MagExplosionBase := 620 + 10;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 8;
                  meff.light := 2;
                  meff.ImgLib := g_WMagic7Images;
                end;
              3: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  if Poison = 2 then
                    meff.MagExplosionBase := 830 + 20
                  else
                    meff.MagExplosionBase := 620 + 20;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 8;
                  meff.light := 2;
                  meff.ImgLib := g_WMagic7Images;
                end;
            else begin
                meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                meff.TargetActor := target;
                meff.NextFrameTime := 80;
              end;
            end;
          end;
        18: begin                       //ÓÕ»óÖ®¹â
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 1570;
            meff.TargetActor := target;
            meff.NextFrameTime := 80;
          end;
        21: begin                       //±¬ÁÑ»ðÑæ
            case maglv div 4 of
              1: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 350;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 11;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic7Images;
                end;
              2: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 380;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 11;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic7Images;
                end;
              3: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 410;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 14;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic7Images;
                end;
            else begin
                meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                meff.MagExplosionBase := 1660;
                meff.TargetActor := nil; //target;
                meff.NextFrameTime := 80;
                meff.ExplosionFrame := 20;
                meff.light := 3;
              end;
            end;
          end;
        26: begin                       //ÐÄÁéÆôÊ¾
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 3990;
            meff.TargetActor := target;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 10;
            meff.light := 2;
          end;
        27: begin                       //ÈºÌåÖÎÁÆÊõ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 1800;
            meff.TargetActor := nil;    //target;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 10;
            meff.light := 3;
          end;
        30: begin                       //Ê¥ÑÔÊõ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 3930;
            meff.TargetActor := target;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 16;
            meff.light := 3;
          end;
        31: begin                       //±ùÅØÏø
            case maglv div 4 of
              1: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 90;
                  meff.TargetActor := nil;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 18;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic8Images;
                end;
              2: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 110;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 18;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic8Images;
                end;
              3: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 130;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 80;
                  meff.ExplosionFrame := 18;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic8Images;
                end;
            else begin
                meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                meff.MagExplosionBase := 3850;
                meff.TargetActor := nil; //target;
                meff.NextFrameTime := 80;
                meff.ExplosionFrame := 20;
                meff.light := 3;
              end;
            end;
          end;
        40: begin                       // ¾»»¯Êõ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 620;
            meff.TargetActor := target;
            meff.NextFrameTime := 100;
            meff.ExplosionFrame := 20;
            meff.light := 3;
            if wimg <> nil then
              meff.ImgLib := wimg;
          end;
        45: begin                       //»ðÁúÆøÑæ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 920;
            meff.TargetActor := nil;    //target;
            meff.NextFrameTime := 120;
            meff.ExplosionFrame := 20;
            meff.light := 3;
            if wimg <> nil then
              meff.ImgLib := wimg;
          end;
        47: begin                       //ì«·çÆÆ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 1010;
            meff.TargetActor := nil;
            //meff.FixedEffect := False;
            meff.NextFrameTime := 120;
            meff.ExplosionFrame := 14;  //18
            meff.light := 3;
            meff.ImgLib := g_WMagic2Images;
            //PlaySoundName('wav\M6-3.wav');
            //PlaySoundName('wav\M6-3.wav');
          end;
        48: begin                       //ÑªÖä
            case maglv div 4 of
              1: begin
                  if target <> nil then begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 710;
                    meff.TargetActor := aowner;
                    meff.NextFrameTime := 85;
                    meff.ExplosionFrame := 14;
                    meff.light := 3;
                    boFly := True;
                    meff.ImgLib := wimg;
                    meff.TargetRx := tx;
                    meff.TargetRy := ty;
                    if meff.TargetActor <> nil then begin
                      meff.TargetRx := TActor(meff.TargetActor).m_nCurrX;
                      meff.TargetRy := TActor(meff.TargetActor).m_nCurrY;
                    end;
                    meff.ImgLib := g_WMagic9Images;
                    meff.MagOwner := aowner;
                    m_EffectList.Add(meff);
                  end;
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 690;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 70;
                  meff.ExplosionFrame := 20;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;
              2: begin
                  if target <> nil then begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 860;
                    meff.TargetActor := aowner;
                    meff.NextFrameTime := 85;
                    meff.ExplosionFrame := 14;
                    meff.light := 3;
                    boFly := True;
                    meff.ImgLib := wimg;
                    meff.TargetRx := tx;
                    meff.TargetRy := ty;
                    if meff.TargetActor <> nil then begin
                      meff.TargetRx := TActor(meff.TargetActor).m_nCurrX;
                      meff.TargetRy := TActor(meff.TargetActor).m_nCurrY;
                    end;
                    meff.ImgLib := g_WMagic9Images;
                    meff.MagOwner := aowner;
                    m_EffectList.Add(meff);
                  end;
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 840;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 70;
                  meff.ExplosionFrame := 20;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;
              3: begin
                  if target <> nil then begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 1010;
                    meff.TargetActor := aowner;
                    meff.NextFrameTime := 85;
                    meff.ExplosionFrame := 14;
                    meff.light := 3;
                    boFly := True;
                    meff.ImgLib := wimg;
                    meff.TargetRx := tx;
                    meff.TargetRy := ty;
                    if meff.TargetActor <> nil then begin
                      meff.TargetRx := TActor(meff.TargetActor).m_nCurrX;
                      meff.TargetRy := TActor(meff.TargetActor).m_nCurrY;
                    end;
                    meff.ImgLib := g_WMagic9Images;
                    meff.MagOwner := aowner;
                    m_EffectList.Add(meff);
                  end;
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 990;
                  meff.TargetActor := target;
                  meff.NextFrameTime := 70;
                  meff.ExplosionFrame := 20;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;

            else begin
                if target <> nil then begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 1090;
                  meff.TargetActor := aowner;
                  meff.NextFrameTime := 85;
                  meff.ExplosionFrame := 10;
                  meff.light := 3;
                  boFly := True;
                  meff.ImgLib := wimg;
                  meff.TargetRx := tx;
                  meff.TargetRy := ty;
                  if meff.TargetActor <> nil then begin
                    meff.TargetRx := TActor(meff.TargetActor).m_nCurrX;
                    meff.TargetRy := TActor(meff.TargetActor).m_nCurrY;
                  end;
                  meff.MagOwner := aowner;
                  m_EffectList.Add(meff);
                end;
                meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                meff.MagExplosionBase := 1060;
                meff.TargetActor := target;
                meff.NextFrameTime := 85;
                meff.ExplosionFrame := 20;
                meff.light := 3;
                if wimg <> nil then meff.ImgLib := wimg;
              end;
            end;
          end;
        49: begin                       //÷¼÷ÃÖä
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 1110;
            meff.TargetActor := nil;    //target;
            meff.NextFrameTime := 120;
            meff.ExplosionFrame := 10;
            meff.light := 3;
            if wimg <> nil then
              meff.ImgLib := wimg;
          end;
        51: begin                       //Á÷ÐÇ»ðÓê
            case maglv div 4 of
              1: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 530;
                  meff.TargetActor := nil;
                  meff.NextFrameTime := 60;
                  meff.ExplosionFrame := 30;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;
              2: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 560;
                  meff.TargetActor := nil;
                  meff.NextFrameTime := 60;
                  meff.ExplosionFrame := 30;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;
              3: begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 590;
                  meff.TargetActor := nil;
                  meff.NextFrameTime := 60;
                  meff.ExplosionFrame := 30;
                  meff.light := 3;
                  meff.ImgLib := g_WMagic9Images;
                end;
            else begin
                meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                meff.MagExplosionBase := 650;
                meff.TargetActor := nil;
                meff.NextFrameTime := 60;
                meff.ExplosionFrame := 30;
                meff.light := 3;
                if wimg <> nil then
                  meff.ImgLib := wimg;
              end;
            end;
          end;
        112: begin
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            if meff.Dir16 in [1..8] then
              meff.MagExplosionBase := 4020
            else
              meff.MagExplosionBase := 4030;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 10;
            meff.light := 3;
            meff.ImgLib := g_CboEffect;
          end;
        116: begin
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 2060 + (meff.Dir16 div 2) * 10;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 6 + 10;
            meff.light := 3;
            meff.ImgLib := g_WMagic8Images2;
            meff.m_nMagEffectNo := Magnumb;
            meff.MagOwner := aowner;
          end;
        117: begin
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 2200 + (meff.Dir16 div 2) * 20;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 13;
            meff.light := 3;
            meff.ImgLib := g_WMagic8Images2;
            meff.m_nMagEffectNo := Magnumb;
            meff.MagOwner := aowner;
          end;
        106: begin
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 3150;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 8;
            meff.light := 3;
            meff.ImgLib := g_CboEffect;
          end;
        125: begin                      //±ùËªÈºÓê
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 80;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 17;
            meff.light := 3;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx;
              meff.TargetRy := ty;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;

            ScreenXYfromMCXY(tx - 2, ty - 3, sctx, scty);
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 80;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 17;
            meff.light := 3;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx - 2;
              meff.TargetRy := ty - 3;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;

            ScreenXYfromMCXY(tx + 2, ty - 3, sctx, scty);
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 80;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 17;
            meff.light := 3;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx + 2;
              meff.TargetRy := ty - 3;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;

            ScreenXYfromMCXY(tx + 2, ty + 3, sctx, scty);
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 80;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 17;
            meff.light := 3;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx + 2;
              meff.TargetRy := ty + 3;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;

            ScreenXYfromMCXY(tx - 2, ty + 3, sctx, scty);
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 80;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 17;
            meff.light := 3;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx - 2;
              meff.TargetRy := ty + 3;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;
            Exit;
          end;
        127: begin                      //ËÀÍöÖ®ÑÛ
            meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
            meff.MagExplosionBase := 30;
            meff.TargetActor := nil;
            meff.NextFrameTime := 80;
            meff.ExplosionFrame := 21;
            meff.light := 4;
            meff.ImgLib := g_WMagic10Images;
            if (meff <> nil) then begin
              meff.TargetRx := tx;
              meff.TargetRy := ty;
              meff.MagOwner := aowner;
              m_EffectList.Add(meff);
            end;
            Exit;
          end;
      else begin                        //Ä¬ÈÏ
          meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
          meff.TargetActor := target;
          meff.NextFrameTime := 80;
        end;
      end;
    mtFireWind: begin
        meff := nil;
        {if Magnumb = 15 then begin

        end;}
      end;
    mtFireGun: meff := TFireGunEffect.Create(930, scx, scy, sctx, scty);
    mtThunder: begin
        case Magnumb of
          61: begin                     //ÅüÐÇÕ¶
              meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
              meff.MagExplosionBase := 495;
              meff.TargetActor := nil;
              meff.NextFrameTime := 90;
              meff.ExplosionFrame := 24;
              meff.light := 3;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          62: begin                     //À×öªÒ»»÷
              meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
              meff.MagExplosionBase := 390;
              meff.TargetActor := nil;
              meff.NextFrameTime := 90;
              meff.ExplosionFrame := 25;
              meff.light := 3;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          64: begin                     //Ä©ÈÕÉóÅÐ
              meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
              meff.MagExplosionBase := 230;
              meff.TargetActor := nil;
              meff.NextFrameTime := 90;
              meff.ExplosionFrame := 27;
              meff.light := 3;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          65: begin                     //»ðÁúÆøÑæ
              meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
              meff.MagExplosionBase := 560;
              meff.TargetActor := nil;
              meff.NextFrameTime := 90;
              meff.ExplosionFrame := 37;
              meff.light := 3;
              if wimg <> nil then meff.ImgLib := wimg;
            end;
          MAGIC_FOX_THUNDER: begin
              meff := TThuderEffectEx.Create(780, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 9;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 80;
            end;
          MAGIC_FOX_FIRE1: begin
              meff := TThuderEffectEx.Create(790, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 10;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 90;
            end;
          MAGIC_SOULBALL_ATT2: begin
              meff := TThuderEffectEx.Create(2120 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
            end;
          MAGIC_SOULBALL_ATT3_1: begin
              meff := TThuderEffectEx.Create(2160 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
              meff.light := 1;
            end;
          MAGIC_SOULBALL_ATT3_2: begin
              meff := TThuderEffectEx.Create(2180 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
              meff.light := 1;
            end;
          MAGIC_SOULBALL_ATT3_3: begin
              meff := TThuderEffectEx.Create(2200 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
              meff.light := 1;
            end;
          MAGIC_SOULBALL_ATT3_4: begin
              meff := TThuderEffectEx.Create(2220 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
              meff.light := 1;
            end;
          MAGIC_SOULBALL_ATT3_5: begin
              meff := TThuderEffectEx.Create(2240 + 1230, sctx, scty, nil, Magnumb);
              meff.ExplosionFrame := 20;
              meff.ImgLib := g_WMon33Img;
              meff.NextFrameTime := 100;
              meff.light := 1;
            end;
        else begin
            case maglv div 4 of
              1: begin
                  meff := TThuderEffect.Create(210, sctx, scty, nil);
                  meff.ExplosionFrame := 5;
                  meff.NextFrameTime := 80;
                  meff.ImgLib := g_WMagic7Images;
                end;
              2: begin
                  meff := TThuderEffect.Create(230, sctx, scty, nil);
                  meff.ExplosionFrame := 5;
                  meff.NextFrameTime := 80;
                  meff.ImgLib := g_WMagic7Images;
                end;
              3: begin
                  meff := TThuderEffect.Create(250, sctx, scty, nil);
                  meff.ExplosionFrame := 7;
                  meff.NextFrameTime := 80;
                  meff.ImgLib := g_WMagic7Images;
                end;
            else begin
                meff := TThuderEffect.Create(10, sctx, scty, nil);
                meff.ExplosionFrame := 6;
                meff.ImgLib := g_WMagic2Images;
              end;
            end;
          end;
        end;
      end;
    mtRedThunder: begin
        meff := TRedThunderEffect.Create(230, sctx, scty, nil);
        //meff.ExplosionFrame := 6;
      end;
    mtRedGroundThunder: begin
        meff := TRedGroundThunderEffect.Create(400, sctx, scty, nil);
        //meff.ExplosionFrame := 5;
      end;
    mtSpurt: begin
        meff := TSpurtEffect.Create(470, sctx, scty, nil);
        //meff.ExplosionFrame := 10;
      end;
    mtLava: meff := TLavaEffect.Create(440, sctx, scty, nil);
    mtLightingThunder: begin
        case maglv div 4 of
          1: meff := TLightingThunder.Create(1100, scx, scy, sctx, scty, target);
          2: meff := TLightingThunder.Create(1270, scx, scy, sctx, scty, target);
          3: meff := TLightingThunder.Create(1440, scx, scy, sctx, scty, target);
        else
          meff := TLightingThunder.Create(970, scx, scy, sctx, scty, target);
        end;
        if maglv > 3 then
          meff.ImgLib := g_WMagic7Images;
      end;
    mtExploBujauk: begin
        case Magnumb of
          MAGIC_FOX_FIRE2: begin
              meff := TExploBujaukEffect.Create(1160, scx, scy, sctx, scty, target, True, Magnumb);
              meff.MagExplosionBase := 1320;
              meff.ExplosionFrame := 10;
            end;
          MAGIC_FOX_CURSE: begin
              meff := TExploBujaukEffect.Create(1160, scx, scy, sctx, scty, target, True, Magnumb);
              meff.MagExplosionBase := 1330;
              meff.ExplosionFrame := 20;
            end;
          10: begin                     //Áé»ê»ð·û
              case maglv div 4 of
                1: begin
                    meff := TExploBujaukEffect.Create(600, scx, scy, sctx, scty, target, False, 10);
                    meff.frame := 6;
                    meff.EffectBase2 := 770;
                    meff.MagExplosionBase := 1620;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 6;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic8Images;
                  end;
                2: begin
                    meff := TExploBujaukEffect.Create(940, scx, scy, sctx, scty, target, False, 10);
                    meff.frame := 6;
                    meff.EffectBase2 := 1110;
                    meff.MagExplosionBase := 1630;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 6;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic8Images;
                  end;
                3: begin
                    meff := TExploBujaukEffect.Create(1280, scx, scy, sctx, scty, target, False, 10);
                    meff.frame := 6;
                    meff.EffectBase2 := 1450;
                    meff.MagExplosionBase := 1640;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 6;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic8Images;
                  end;
              else begin
                  meff := TExploBujaukEffect.Create(1160, scx, scy, sctx, scty, target);
                  meff.MagExplosionBase := 1360;
                end;
              end;
            end;
          17: begin
              meff := TExploBujaukEffect.Create(1160, scx, scy, sctx, scty, target);
              meff.MagExplosionBase := 1540;
            end;
          104: begin
              meff := TExploBujaukEffect.Create(2420, scx, scy, sctx, scty, target, True);
              meff.frame := 3;
              meff.MagExplosionBase := 2580;
              meff.TargetActor := target;
              meff.NextFrameTime := 80;
              meff.ExplosionFrame := 8;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
            end;
          105: begin
              meff := TExploBujaukEffect.Create(4230, scx, scy, sctx, scty, target, True, 105 {112});
              meff.frame := 4;
              meff.MagExplosionBase := 4240;
              meff.TargetActor := target;
              meff.NextFrameTime := 80;
              meff.ExplosionFrame := 7;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
            end;
          107: begin
              meff := TExploBujaukEffect.Create(2610, scx, scy, sctx, scty, target, True);
              meff.frame := 5;
              meff.MagExplosionBase := 2770;
              meff.TargetActor := target;
              meff.NextFrameTime := 80;
              meff.ExplosionFrame := 25;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
            end;
          108: begin
              meff := TExploBujaukEffect.Create(3580, scx, scy, sctx, scty, target, False, 108);
              meff.frame := 5;
              meff.EffectBase2 := 3660;
              meff.MagExplosionBase := 3740;
              meff.TargetActor := target;
              meff.NextFrameTime := 110;
              meff.ExplosionFrame := 10;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
              meff.Repetition := False;
            end;
          109: begin
              meff := TExploBujaukEffect.Create(2090, scx, scy, sctx, scty, target, True);
              meff.frame := 3;
              meff.MagExplosionBase := 2251;
              meff.TargetActor := target;
              meff.NextFrameTime := 60;
              meff.ExplosionFrame := 4;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
            end;
          110: begin
              meff := TExploBujaukEffect.Create(3400, scx, scy, sctx, scty, target, False, 110);
              meff.frame := 5;
              meff.EffectBase2 := 3240;
              meff.MagExplosionBase := 3560;
              meff.TargetActor := target;
              meff.NextFrameTime := 80;
              meff.ExplosionFrame := 6;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
            end;
          111: begin
              meff := TExploBujaukEffect.Create(2820, scx, scy, sctx, scty, target, True, 111);
              meff.frame := 5;
              meff.MagExplosionBase := 2980;
              meff.TargetActor := target;
              meff.NextFrameTime := 100;
              meff.ExplosionFrame := 6;
              meff.light := 3;
              meff.ImgLib := g_CboEffect;
              meff.Repetition := False;
            end;
          0075: begin
              meff := TExploBujaukEffect.Create(10, scx, scy, sctx, scty, target, True, 0075);
              meff.nStdX := g_MySelf.m_nRx;
              meff.nStdY := g_MySelf.m_nRy;
              meff.frame := 10;
              meff.TargetActor := nil;
              meff.NextFrameTime := 80;
              meff.light := 3;
              meff.ImgLib := g_WMagic5Images;
              meff.MagExplosionBase := 220;
              meff.ExplosionFrame := 10;
              meff.ExplosionImgLib := g_WMagic10Images;
              meff.Repetition := False;
            end;
        end;
        boFly := True;
      end;
    mtBujaukGroundEffect: begin
        case Magnumb of
          11: begin
              meff := TBujaukGroundEffect.Create(1160, Magnumb, scx, scy, sctx, scty, maglv);
              meff.ExplosionFrame := 16;
              if maglv > 3 then
                meff.ExplosionFrame := 20;
            end;
          12: begin
              meff := TBujaukGroundEffect.Create(1160, Magnumb, scx, scy, sctx, scty, maglv);
              meff.ExplosionFrame := 16;
              if maglv > 3 then
                meff.ExplosionFrame := 20;
            end;

          46: begin
              meff := TBujaukGroundEffect.Create(1160, Magnumb, scx, scy, sctx, scty);
              meff.ExplosionFrame := 24;
            end;
          74: begin                     //
              meff := TBujaukGroundEffect.Create(10, Magnumb, scx, scy, sctx, scty);
              meff.ExplosionFrame := 10;
              meff.NextFrameTime := 80;
              meff.ImgLib := g_WMagic5Images;
            end;
        end;
        boFly := True;
      end;
    mtKyulKai: meff := nil;             //TKyulKai.Create (1380, scx, scy, sctx, scty);
    mtFlyBug: ;
    mtGroundEffect: begin
        meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
        if meff <> nil then begin
          case Magnumb of
            32: begin
                meff.ImgLib := g_WMon21Img;
                meff.MagExplosionBase := 3580;
                meff.TargetActor := target;
                meff.light := 3;
                meff.NextFrameTime := 20;
              end;
            33: begin
                meff.ImgLib := g_WMon24Img;
                meff.MagExplosionBase := 3730;
                meff.TargetActor := target;
                meff.light := 3;
                meff.NextFrameTime := 120;
              end;
            37: begin
                meff.ImgLib := g_WMon22Img;
                meff.MagExplosionBase := 3520;
                meff.TargetActor := target;
                meff.light := 5;
                meff.NextFrameTime := 20;
              end;
            MAGIC_SIDESTONE_ATT1: begin
                meff.ImgLib := g_WMon33Img;
                meff.MagExplosionBase := 2670;
                meff.TargetActor := target;
                meff.light := 4;
                meff.ExplosionFrame := 10;
                meff.NextFrameTime := 150;
              end;
            MAGIC_SOULBALL_ATT1: begin
                meff.ImgLib := g_WMon33Img;
                meff.MagExplosionBase := 2140 + 1230;
                meff.TargetActor := target;
                meff.light := 5;
                meff.ExplosionFrame := 20;
              end;
          end;
        end;
      end;
    mtThuderEx: begin
        case Magnumb of
          34: begin                     //ÃðÌì»ð
              case maglv div 4 of
                1: begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 380;
                    meff.TargetActor := nil;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 9;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic9Images;
                  end;
                2: begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 390;
                    meff.TargetActor := nil; //target;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 9;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic9Images;
                  end;
                3: begin
                    meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                    meff.MagExplosionBase := 400;
                    meff.TargetActor := nil; //target;
                    meff.NextFrameTime := 100;
                    meff.ExplosionFrame := 9;
                    meff.light := 3;
                    meff.ImgLib := g_WMagic9Images;
                  end;
              else begin
                  meff := TMagicEff.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
                  meff.MagExplosionBase := 140;
                  meff.TargetActor := nil; //target;
                  meff.NextFrameTime := 100;
                  meff.ExplosionFrame := 20;
                  meff.light := 3;
                  if wimg <> nil then
                    meff.ImgLib := wimg;
                end;
              end;
            end;
        end;
      end;
    mtFireBall: begin
        meff := TFlyingBug.Create(magid, effnum, scx, scy, sctx, scty, Mtype, Recusion, anitime);
        meff.TargetActor := target;
        boFly := True;
      end;
    mtFlyBolt: begin

      end;
  end;

  if (meff <> nil) then begin
    meff.TargetRx := tx;
    meff.TargetRy := ty;
    if meff.TargetActor <> nil then begin
      meff.TargetRx := TActor(meff.TargetActor).m_nCurrX;
      meff.TargetRy := TActor(meff.TargetActor).m_nCurrY;
    end;
    meff.MagOwner := aowner;
    m_EffectList.Add(meff);
  end;
end;

procedure TPlayScene.DelMagic(magid: Integer);
var
  i                         : Integer;
begin
  for i := 0 to m_EffectList.Count - 1 do begin
    if TMagicEff(m_EffectList[i]).ServerMagicId = magid then begin
      TMagicEff(m_EffectList[i]).Free;
      m_EffectList.Delete(i);
      Break;
    end;
  end;
end;

function TPlayScene.NewFlyObject(aowner: TActor; cx, cy, tx, ty, TargetCode: Integer; Mtype: TMagicType): TMagicEff;
var
  i, scx, scy, sctx, scty   : Integer;
  meff                      : TMagicEff;
begin
  ScreenXYfromMCXY(cx, cy, scx, scy);
  ScreenXYfromMCXY(tx, ty, sctx, scty);
  case Mtype of
    mtFlyArrow: meff := TFlyingArrow.Create(1, 1, scx, scy, sctx, scty, Mtype, True, 0);
    mtFlyBug: meff := TFlyingFireBall.Create(1, 1, scx, scy, sctx, scty, Mtype, True, 0);
    mtFireBall: meff := TFlyingBug.Create(1, 1, scx, scy, sctx, scty, Mtype, True, 0);
  else meff := TFlyingAxe.Create(1, 1, scx, scy, sctx, scty, Mtype, True, 0);
  end;
  meff.TargetRx := tx;
  meff.TargetRy := ty;
  meff.TargetActor := FindActor(TargetCode);
  meff.MagOwner := aowner;
  m_FlyList.Add(meff);
  Result := meff;
end;

procedure TPlayScene.ScreenXYfromMCXY(cx, cy: Integer; var sX, sY: Integer);
begin
  if g_MySelf = nil then Exit;

  sX := (cx - g_MySelf.m_nRx) * UNITX - g_MySelf.m_nShiftX + SCREENWIDTH div 2;
  sY := (cy - g_MySelf.m_nRy) * UNITY - g_MySelf.m_nShiftY + ShiftYOffset;

end;

procedure TPlayScene.CXYfromMouseXY(mx, my: Integer; var ccx, ccy: Integer);
begin
  if g_MySelf = nil then Exit;

  ccx := Round((mx - SCREENWIDTH div 2 + g_MySelf.m_nShiftX) / UNITX) + g_MySelf.m_nRx;
  ccy := Round((my - ShiftYOffset + g_MySelf.m_nShiftY) / UNITY) + g_MySelf.m_nRy;

end;

function TPlayScene.GetCharacter(X, Y, wantsel: Integer; var nowsel: Integer; liveonly: Boolean): TActor;
var
  k, i, ccx, ccy, dx, dy    : Integer;
  a                         : TActor;
begin
  Result := nil;
  nowsel := -1;
  CXYfromMouseXY(X, Y, ccx, ccy);
  for k := ccy + 8 downto ccy - 1 do begin
    for i := m_ActorList.Count - 1 downto 0 do
      if TActor(m_ActorList[i]) <> g_MySelf then begin
        a := TActor(m_ActorList[i]);
        if (not liveonly or not a.m_boDeath) and (a.m_boHoldPlace) and (a.m_boVisible) then begin
          if a.m_nCurrY = k then begin
            dx := (a.m_nRx - Map.m_ClientRect.Left) * UNITX + m_nDefXX + a.m_nPx + a.m_nShiftX;
            dy := (a.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + m_nDefYY + a.m_nPy + a.m_nShiftY;
            if a.CheckSelect(X - dx, Y - dy) then begin
              Result := a;
              Inc(nowsel);
              if nowsel >= wantsel then
                Exit;
            end;
          end;
        end;
      end;
  end;
end;

//È¡µÃÊó±êËùÖ¸×ø±êµÄ½ÇÉ«

function TPlayScene.GetAttackFocusCharacter(X, Y, wantsel: Integer; var nowsel: Integer; liveonly: Boolean): TActor;
var
  k, i, ccx, ccy, dx, dy, centx, centy: Integer;
  a                         : TActor;
begin
  Result := GetCharacter(X, Y, wantsel, nowsel, liveonly);
  if Result = nil then begin
    nowsel := -1;
    CXYfromMouseXY(X, Y, ccx, ccy);
    for k := ccy + 8 downto ccy - 1 do begin
      for i := m_ActorList.Count - 1 downto 0 do
        if TActor(m_ActorList[i]) <> g_MySelf then begin
          a := TActor(m_ActorList[i]);
          if (not liveonly or not a.m_boDeath) and (a.m_boHoldPlace) and (a.m_boVisible) then begin
            if a.m_nCurrY = k then begin
              dx := (a.m_nRx - Map.m_ClientRect.Left) * UNITX + m_nDefXX + a.m_nPx + a.m_nShiftX;
              dy := (a.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + m_nDefYY + a.m_nPy + a.m_nShiftY;
              if a.CharWidth > 40 then
                centx := (a.CharWidth - 40) div 2
              else
                centx := 0;

              if a.CharHeight > 70 then
                centy := (a.CharHeight - 70) div 2
              else
                centy := 0;

              if (X - dx >= centx) and (X - dx <= a.CharWidth - centx) and (Y - dy >= centy) and (Y - dy <= a.CharHeight - centy) then begin
                Result := a;
                Inc(nowsel);
                if nowsel >= wantsel then
                  Exit;
              end;
            end;
          end;
        end;
    end;
  end;
end;

function TPlayScene.IsSelectMyself(X, Y: Integer): Boolean;
var
  k, i, ccx, ccy, dx, dy    : Integer;
begin
  Result := False;
  CXYfromMouseXY(X, Y, ccx, ccy);
  for k := ccy + 2 downto ccy - 1 do begin
    if g_MySelf.m_nCurrY = k then begin
      dx := (g_MySelf.m_nRx - Map.m_ClientRect.Left) * UNITX + m_nDefXX + g_MySelf.m_nPx + g_MySelf.m_nShiftX;
      dy := (g_MySelf.m_nRy - Map.m_ClientRect.Top - 1) * UNITY + m_nDefYY + g_MySelf.m_nPy + g_MySelf.m_nShiftY;
      if g_MySelf.CheckSelect(X - dx, Y - dy) then begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TPlayScene.GetDropItems(X, Y: Integer; var inames: string): pTDropItem; //È­¸éÁÂÇ¥·Î ¾ÆÀÌÅÛ
var
  k, i, ccx, ccy, ssx, ssy, dx, dy: Integer;
  DropItem                  : pTDropItem;
  S                         : TCustomLockableTexture;
  c                         : byte;
begin
  Result := nil;
  CXYfromMouseXY(X, Y, ccx, ccy);
  ScreenXYfromMCXY(ccx, ccy, ssx, ssy);
  dx := X - ssx;
  dy := Y - ssy;
  inames := '';
  for i := 0 to g_DropedItemList.Count - 1 do begin
    DropItem := pTDropItem(g_DropedItemList[i]);
    if (DropItem.X = ccx) and (DropItem.Y = ccy) then begin
      if Result = nil then Result := DropItem;
      inames := inames + DropItem.Name + '\';
    end;
  end;
end;

procedure TPlayScene.GetXYDropItemsList(nX, nY: Integer; var ItemList: TList);
var
  i                         : Integer;
  DropItem                  : pTDropItem;
begin
  for i := 0 to g_DropedItemList.Count - 1 do begin
    DropItem := g_DropedItemList[i];
    if (DropItem.X = nX) and (DropItem.Y = nY) then begin
      ItemList.Add(DropItem);
    end;
  end;
end;

function TPlayScene.GetXYDropItems(nX, nY: Integer): pTDropItem;
var
  i                         : Integer;
  DropItem                  : pTDropItem;
begin
  Result := nil;
  for i := 0 to g_DropedItemList.Count - 1 do begin
    DropItem := g_DropedItemList[i];
    if (DropItem.X = nX) and (DropItem.Y = nY) then begin
      Result := DropItem;
      //if not g_gcGeneral[7] or DropItem.boShowName then
      //  Break;
      if g_boPickUpAll or {not g_gcGeneral[7] or} DropItem.boPickUp then
        Break;
    end;
  end;
end;

function TPlayScene.CanRun(sX, sY, ex, ey: Integer): Boolean;
var
  ndir, rx, ry              : Integer;
begin
  ndir := GetNextDirection(sX, sY, ex, ey);
  rx := sX;
  ry := sY;
  GetNextPosXY(ndir, rx, ry);

  {if Map.CanMove(rx, ry) and Map.CanMove(ex, ey) then
    Result := True
  else begin
    Result := False;
  end;}

  if CanWalkEx(rx, ry) and CanWalkEx(ex, ey) then
    Result := True
  else
    Result := False;
end;

function TPlayScene.CanWalkEx(mx, my: Integer): Boolean;
begin
  Result := False;
  if Map.CanMove(mx, my) then
    Result := not CrashManEx(mx, my);
end;

function TPlayScene.CrashManEx(mx, my: Integer): Boolean;
var
  i                         : Integer;
  Actor                     : TActor;
begin
  Result := False;
  for i := 0 to m_ActorList.Count - 1 do begin
    Actor := TActor(m_ActorList[i]);
    if Actor = g_MySelf then Continue;
    if (Actor.m_boVisible) and (Actor.m_boHoldPlace) and (not Actor.m_boDeath) and (Actor.m_nCurrX = mx) and (Actor.m_nCurrY = my) then begin
      if (g_MySelf.m_nTagX = 0) and (g_MySelf.m_nTagY = 0) then begin
        if (Actor.m_btRace = RCC_USERHUMAN) and (g_boCanRunHuman or g_boCanRunSafeZone) then Continue;
        if (Actor.m_btRace = RCC_MERCHANT) and g_boCanRunNpc then Continue;
        if ((Actor.m_btRace > RCC_USERHUMAN) and (Actor.m_btRace <> RCC_MERCHANT)) and (g_boCanRunMon or g_boCanRunSafeZone) then Continue;
      end;
      Result := True;
      Break;
    end;
  end;
end;

function TPlayScene.CanWalk(mx, my: Integer): Boolean;
begin
  Result := False;
  if Map.CanMove(mx, my) then
    Result := not CrashMan(mx, my);
end;

function TPlayScene.CrashMan(mx, my: Integer): Boolean;
var
  i                         : Integer;
  a                         : TActor;
begin
  Result := False;
  for i := 0 to m_ActorList.Count - 1 do begin
    a := TActor(m_ActorList[i]);
    if a = g_MySelf then Continue;
    if (a.m_boVisible) and (a.m_boHoldPlace) and (not a.m_boDeath) and (a.m_nCurrX = mx) and (a.m_nCurrY = my) then begin
      Result := True;
      Break;
    end;
  end;
end;

{function TPlayScene.CrashManPath(mx, my: Integer): Boolean;
var
  i                         : Integer;
  a                         : TActor;
begin
  Result := False;
  for i := 0 to m_ActorList.count - 1 do begin
    a := TActor(m_ActorList[i]);
    if a = g_MySelf then Continue;
    if (a.m_boVisible) and (a.m_boHoldPlace) and (not a.m_boDeath) and (a.m_nCurrX = mx) and (a.m_nCurrY = my) then begin
      Result := True;
      Break;
    end;
  end;
end; }

function TPlayScene.CanFly(mx, my: Integer): Boolean;
begin
  Result := Map.CanFly(mx, my);
end;

{------------------------ Actor ------------------------}

function TPlayScene.FindActor(id: Integer): TActor;
var
  i                         : Integer;
begin
  Result := nil;
  if id = 0 then
    Exit;
  for i := 0 to m_ActorList.Count - 1 do begin
    if TActor(m_ActorList[i]).m_nRecogId = id then begin
      Result := TActor(m_ActorList[i]);
      Break;
    end;
  end;
end;

function TPlayScene.FindActor(sName: string): TActor;
var
  i                         : Integer;
  Actor                     : TActor;
begin
  Result := nil;
  for i := 0 to m_ActorList.Count - 1 do begin
    Actor := TActor(m_ActorList[i]);
    if CompareText(Actor.m_sUserName, sName) = 0 then begin
      Result := Actor;
      Break;
    end;
  end;
end;

function TPlayScene.FindActorXY(X, Y: Integer): TActor;
var
  i                         : Integer;
  a                         : TActor;
begin
  Result := nil;
  for i := 0 to m_ActorList.Count - 1 do begin
    a := TActor(m_ActorList[i]);
    if (a.m_nCurrX = X) and (a.m_nCurrY = Y) then begin
      Result := a;
      if not Result.m_boDeath and Result.m_boVisible and Result.m_boHoldPlace then
        Break;
    end;
  end;
end;

function TPlayScene.IsValidActor(Actor: TActor): Boolean;
var
  i                         : Integer;
begin
  Result := False;
  for i := 0 to m_ActorList.Count - 1 do begin
    if TActor(m_ActorList[i]) = Actor then begin
      Result := True;
      Break;
    end;
  end;
end;

function TPlayScene.NewActor(chrid: Integer; cx: Word; cy: Word; cdir: Word; cfeature: Integer; {race, hair, dress, weapon} cstate: Integer): TActor;
var
  i                         : Integer;
  Actor                     : TActor;
begin
  Result := nil;
  for i := 0 to m_ActorList.Count - 1 do
    if TActor(m_ActorList[i]).m_nRecogId = chrid then begin
      Result := TActor(m_ActorList[i]);
      //if is my hero then ???
      Exit;
    end;
  if IsChangingFace(chrid) then Exit;
  case RACEfeature(cfeature) of
    0: Actor := THumActor.Create;       //ÈËÎï
    9: Actor := TSoccerBall.Create;     //×ãÇò
    13: Actor := TKillingHerb.Create;   //Ê³ÈË»¨
    14: Actor := TSkeletonOma.Create;   //÷¼÷Ã
    15: Actor := TDualAxeOma.Create;    //ÖÀ¸«÷¼÷Ã
    16: Actor := TGasKuDeGi.Create;     //¶´Çù
    17: Actor := TCatMon.Create;        //¹³×¦Ã¨
    18: Actor := THuSuABi.Create;       //µ¾²ÝÈË
    19: Actor := TCatMon.Create;        //ÎÖÂêÕ½Ê¿
    20: Actor := TFireCowFaceMon.Create; //»ðÑæÎÖÂê
    21: Actor := TCowFaceKing.Create;   //ÎÖÂê½ÌÖ÷
    22: Actor := TDualAxeOma.Create;    //ºÚ°µÕ½Ê¿
    23: Actor := TWhiteSkeleton.Create; //±äÒì÷¼÷Ã
    24: Actor := TSuperiorGuard.Create; //´øµ¶ÎÀÊ¿
    25: Actor := TKingOfSculpureKingMon.Create;
    26: Actor := TKingOfSculpureKingMon.Create;
    27: Actor := TSnowMon.Create;
    28: Actor := TSnowMon.Create;
    29: Actor := TSnowMon.Create;

    30: Actor := TCatMon.Create;        //³¯°³Áþ
    31: Actor := TCatMon.Create;        //½ÇÓ¬
    32: Actor := TScorpionMon.Create;   //Ð«×Ó
    33: Actor := TCentipedeKingMon.Create; //´¥ÁúÉñ
    34: Actor := TBigHeartMon.Create;   //³àÔÂ¶ñÄ§
    35: Actor := TSpiderHouseMon.Create; //»ÃÓ°Ö©Öë
    36: Actor := TExplosionSpider.Create; //ÔÂÄ§Ö©Öë
    37: Actor := TFlyingSpider.Create;  //
    38: Actor := TSnowMon.Create;
    39: Actor := TSnowMon.Create;

    40: Actor := TZombiLighting.Create; //½©Ê¬1
    41: Actor := TZombiDigOut.Create;   //½©Ê¬2
    42: Actor := TZombiZilkin.Create;   //½©Ê¬3
    43: Actor := TBeeQueen.Create;      //½ÇÓ¬³²
    44: Actor := TSnowMon.Create;
    45: Actor := TArcherMon.Create;     //¹­¼ýÊÖ
    46: Actor := TSnowMon.Create;
    47: Actor := TSculptureMon.Create;  //×æÂêµñÏñ
    48: Actor := TSculptureMon.Create;  //
    49: Actor := TSculptureKingMon.Create; //×æÂê½ÌÖ÷

    50: Actor := TNpcActor.Create;
    51: Actor := TSnowMon.Create;
    52: Actor := TGasKuDeGi.Create;     //Ð¨¶ê
    53: Actor := TGasKuDeGi.Create;     //·à³æ
    54: Actor := TSmallElfMonster.Create; //ÉñÊÞ
    55: Actor := TWarriorElfMonster.Create; //ÉñÊÞ1
    56: Actor := TAngel.Create;
    57: Actor := TDualAxeOma.Create;    //1234
    58: Actor := TDualAxeOma.Create;    //1234

    60: Actor := TElectronicScolpionMon.Create;
    61: Actor := TBossPigMon.Create;
    62: Actor := TKingOfSculpureKingMon.Create;
    63: Actor := TSkeletonKingMon.Create;
    64: Actor := TGasKuDeGi.Create;
    65: Actor := TSamuraiMon.Create;
    66: Actor := TSkeletonSoldierMon.Create;
    67: Actor := TSkeletonSoldierMon.Create;
    68: Actor := TSkeletonSoldierMon.Create;
    69: Actor := TSkeletonArcherMon.Create;
    70: Actor := TBanyaGuardMon.Create;
    71: Actor := TBanyaGuardMon.Create;
    72: Actor := TBanyaGuardMon.Create;
    73: Actor := TPBOMA1Mon.Create;
    74: Actor := TCatMon.Create;
    75: Actor := TStoneMonster.Create;
    76: Actor := TSuperiorGuard.Create;
    77: Actor := TStoneMonster.Create;
    78: Actor := TBanyaGuardMon.Create;
    79: Actor := TPBOMA6Mon.Create;
    80: Actor := TMineMon.Create;
    81: Actor := TAngel.Create;
    83: Actor := TFireDragon.Create;
    84: Actor := TDragonStatue.Create;
    87: Actor := TDragonStatue.Create;

    90: Actor := TDragonBody.Create;    //Áú

    91: Actor := TWhiteSkeleton.Create; //±äÒì÷¼÷Ã
    92: Actor := TWhiteSkeleton.Create; //±äÒì÷¼÷Ã
    93: Actor := TWhiteSkeleton.Create; //±äÒì÷¼÷Ã

    94: Actor := TWarriorElfMonster.Create; //ÉñÊÞ1
    95: Actor := TWarriorElfMonster.Create; //ÉñÊÞ1

    98: Actor := TWallStructure.Create; //LeftWall
    99: Actor := TCastleDoor.Create;    //MainDoor
    101: Actor := TBanyaGuardMon.Create;
    102: Actor := TKhazardMon.Create;
    103: Actor := TFrostTiger.Create;
    104: Actor := TRedThunderZuma.Create;
    105: Actor := TCrystalSpider.Create;
    106: Actor := TYimoogi.Create;
    109: Actor := TBlackFox.Create;
    110: Actor := TGreenCrystalSpider.Create;
    111: Actor := TBanyaGuardMon.Create; // TSpiderKing.Create;

    113: Actor := TBanyaGuardMon.Create;
    114: Actor := TBanyaGuardMon.Create;
    115: Actor := TBanyaGuardMon.Create;

    117, 118, 119: Actor := TBanyaGuardMon.Create;
    120: Actor := TFireDragon.Create;
    121: Actor := TTiger.Create;
    122: Actor := TDragon.Create;
    123: Actor := TGhostShipMonster.Create;
  else
    Actor := TActor.Create;
  end;

  with Actor do begin
    m_nRecogId := chrid;
    m_nCurrX := cx;
    m_nCurrY := cy;
    m_nRx := m_nCurrX;
    m_nRy := m_nCurrY;
    m_btDir := cdir;
    m_nFeature := cfeature;
    if g_boOpenAutoPlay and g_gcAss[6] then m_btAFilter := g_APMobList.indexof(Actor.m_sUserName) >= 0;
    m_btRace := RACEfeature(cfeature);
    m_btHair := HAIRfeature(cfeature);
    m_btDress := DRESSfeature(cfeature);
    m_btWeapon := WEAPONfeature(cfeature);
    m_wAppearance := APPRfeature(cfeature);
    //if (m_btRace = 50) and (m_wAppearance in [54..48]) then
    //  m_boVisible := False;

    m_Action := nil;                    //GetMonAction(m_wAppearance);
    if m_btRace = 0 then begin
      m_btSex := m_btDress mod 2;
      //if m_btDress in [24..27] then m_btDress := 18 + m_btSex; //20200719ÒÂ·þ
    end else begin
      m_btSex := 0;
    end;
    m_nState := cstate;
    m_SayingArr[0] := '';
  end;

  if  (Actor.m_btRace = 54) or (Actor.m_btRace = 94) then begin  //20200719 Õ¾Á¢ÉñÊÞ±äÅ¿ÏÂÉñÊÞÃû×ÖÏûÊ§µÄBUG
    if Actor.m_sUserName = '' then  begin
    frmMain.SendQueryUserName(Actor.m_nRecogId,Actor.m_nCurrX,Actor.m_nCurrY);
    end;
  end;

  m_ActorList.Add(Actor);
  Result := Actor;
end;

procedure TPlayScene.ActorDied(Actor: TObject);
var
  i                         : Integer;
  flag                      : Boolean;
begin
  for i := 0 to m_ActorList.Count - 1 do
    if m_ActorList[i] = Actor then begin
      m_ActorList.Delete(i);
      Break;
    end;
  flag := False;
  for i := 0 to m_ActorList.Count - 1 do
    if not TActor(m_ActorList[i]).m_boDeath then begin
      m_ActorList.Insert(i, Actor);
      flag := True;
      Break;
    end;
  if not flag then m_ActorList.Add(Actor);
end;

procedure TPlayScene.SetActorDrawLevel(Actor: TObject; Level: Integer);
var
  i                         : Integer;
begin
  if Level = 0 then begin
    for i := 0 to m_ActorList.Count - 1 do
      if m_ActorList[i] = Actor then begin
        m_ActorList.Delete(i);
        m_ActorList.Insert(0, Actor);
        Break;
      end;
  end;
end;

procedure TPlayScene.ClearActors;
var
  i                         : Integer;
begin
  for i := 0 to g_FreeActorList.Count - 1 do
    TActor(g_FreeActorList[i]).Free;
  g_FreeActorList.Clear;

  for i := 0 to m_ActorList.Count - 1 do
    TActor(m_ActorList[i]).Free;
  m_ActorList.Clear;

  if g_MySelf <> nil then begin
    g_MySelf.m_HeroObject := nil;
    g_MySelf.m_SlaveObject.Clear;       // := nil;
    g_MySelf := nil;
  end;
  g_TargetCret := nil;
  g_FocusCret := nil;
  g_MagicTarget := nil;
  for i := 0 to m_EffectList.Count - 1 do
    TMagicEff(m_EffectList[i]).Free;
  m_EffectList.Clear;
  DScreen.ClearHint;
end;

function TPlayScene.DeleteActor(id: Integer; boDeath: Boolean = False): TActor;
var
  i                         : Integer;
begin
  Result := nil;
  i := 0;
  while True do begin
    if i >= m_ActorList.Count then Break;
    if TActor(m_ActorList[i]).m_nRecogId = id then begin
      if g_TargetCret = TActor(m_ActorList[i]) then g_TargetCret := nil;
      if g_FocusCret = TActor(m_ActorList[i]) then g_FocusCret := nil;
      if g_MagicTarget = TActor(m_ActorList[i]) then g_MagicTarget := nil;
      if (TActor(m_ActorList[i]) = g_MySelf.m_HeroObject) then begin
        //TActor(m_ActorList[i]).m_boNotShowHealth := True;
        if not boDeath then
          Break;
      end;
      if IsMySlaveObject(TActor(m_ActorList[i])) then begin
        if not boDeath then
          Break;
      end;
      {if (TActor(m_ActorList[i]) = g_MySelf.m_SlaveObject) then begin
        //TActor(m_ActorList[i]).m_boNotShowHealth := True;
        if not boDeath then
          Break;
      end;}
      TActor(m_ActorList[i]).m_dwDeleteTime := GetTickCount;
      g_FreeActorList.Add(m_ActorList[i]);
      m_ActorList.Delete(i);
    end else
      Inc(i);
  end;
end;

procedure TPlayScene.DelActor(Actor: TObject);
var
  i                         : Integer;
begin
  for i := 0 to m_ActorList.Count - 1 do
    if m_ActorList[i] = Actor then begin
      TActor(m_ActorList[i]).m_dwDeleteTime := GetTickCount;
      g_FreeActorList.Add(m_ActorList[i]);
      m_ActorList.Delete(i);
      Break;
    end;
end;

function TPlayScene.ButchAnimal(X, Y: Integer): TActor;
var
  i                         : Integer;
  a                         : TActor;
begin
  Result := nil;
  for i := 0 to m_ActorList.Count - 1 do begin
    a := TActor(m_ActorList[i]);
    if a.m_boDeath {and (a.m_btRace <> 0)} then begin
      if (abs(a.m_nCurrX - X) <= 1) and (abs(a.m_nCurrY - Y) <= 1) then begin
        Result := a;
        Break;
      end;
    end;
  end;
end;

procedure TPlayScene.SendMsg(ident, chrid, X, Y, cdir, Feature, State: Integer; Str: string; IPInfo: Integer);
var
  Actor                     : TActor;
  i, Row                    : Integer;
  nstr                      : string;
  meff                      : TMagicEff;
  mbw                       : TMessageBodyW;
begin
  case ident of
    SM_CHANGEMAP, SM_NEWMAP: begin
        ProcMagic.nTargetX := -1;
        EventMan.ClearEvents;
        g_PathBusy := True;
        try
          if frmMain.TimerAutoMove.Enabled then begin
            frmMain.TimerAutoMove.Enabled := False;
            SetLength(g_MapPath, 0);
            g_MapPath := nil;
            DScreen.AddChatBoardString('µØÍ¼Ìø×ª£¬Í£Ö¹×Ô¶¯ÒÆ¶¯', GetRGB(5), clWhite);
          end;

          if g_boOpenAutoPlay and frmMain.TimerAutoPlay.Enabled then begin
            frmMain.TimerAutoPlay.Enabled := False;
            g_gcAss[0] := False;
            SetLength(g_APMapPath, 0);
            SetLength(g_APMapPath2, 0);
            g_APStep := -1;
            g_APLastPoint.X := -1;
            DScreen.AddChatBoardString('[¹Ò»ú] µØÍ¼Ìø×ª£¬Í£Ö¹×Ô¶¯¹Ò»ú', clRed, clWhite);
          end;

          if (g_MySelf <> nil) then begin
            g_MySelf.m_nTagX := 0;
            g_MySelf.m_nTagY := 0;
          end;

          if Map.m_MapBuf <> nil then begin
            FreeMem(Map.m_MapBuf);
            Map.m_MapBuf := nil;
          end;
          if Length(Map.m_MapData) > 0 then begin
            SetLength(Map.m_MapData, 0);
            Map.m_MapData := nil;
          end;
        finally
          g_PathBusy := False;
        end;

        Map.LoadMap(Str, X, Y);
{$IF VIEWFOG}
        DarkLevel := cdir;
{$ELSE}
        DarkLevel := 0;
{$IFEND VIEWFOG}
        if g_boForceNotViewFog then
          DarkLevel := 0;
        if DarkLevel = 0 then
          g_boViewFog := False
        else
          g_boViewFog := True;

        if (ident = SM_NEWMAP) and (g_MySelf <> nil) then begin
          g_MySelf.m_nCurrX := X;
          g_MySelf.m_nCurrY := Y;
          g_MySelf.m_nRx := X;
          g_MySelf.m_nRy := Y;
          DelActor(g_MySelf);
        end;
        if frmDlg.DWGameConfig.Visible and (frmDlg.DWGameConfig.tag = 5) then begin
          g_nApMiniMap := True;
          frmMain.SendWantMiniMap;
        end;
        if g_boViewMiniMap then begin
          g_nMiniMapIndex := -1;
          frmMain.SendWantMiniMap;
        end;
        //if Str <> Map.m_sCurrentMap then
        //  g_nLastMapMusic := -1;
      end;
    SM_LOGON: begin
        Actor := FindActor(chrid);
        if Actor = nil then begin
          Actor := NewActor(chrid, X, Y, Lobyte(cdir), Feature, State);
          Actor.m_nChrLight := Hibyte(cdir);
          cdir := Lobyte(cdir);
          Actor.SendMsg(SM_TURN, X, Y, cdir, Feature, State, '', 0);
        end;
        if g_MySelf <> nil then begin
          if g_MySelf.m_HeroObject <> nil then begin
            //g_MySelf.m_HeroObject.Free;
            g_MySelf.m_HeroObject := nil;
          end;
          g_MySelf.m_SlaveObject.Clear; // := nil;
          //g_MySelf.Free;
          g_MySelf := nil;
        end;
        g_MySelf := THumActor(Actor);
      end;
    SM_HIDE: begin
        Actor := FindActor(chrid);
        if Actor <> nil then begin
          if Actor.m_boDelActionAfterFinished then Exit;
          if Actor.m_nWaitForRecogId <> 0 then Exit;
          if Actor = g_MySelf.m_HeroObject then begin
            if not Actor.m_boDeath then Exit;
            DeleteActor(chrid, True);
            Exit;
          end;
          if IsMySlaveObject(Actor) then begin
            //if (Actor = g_MySelf.m_SlaveObject) then begin
            if (cdir <> 0) or Actor.m_boDeath then
              DeleteActor(chrid, True);
            Exit;
          end;
        end;
        DeleteActor(chrid);
      end;
  else begin
      Actor := FindActor(chrid);
      if (ident = SM_TURN) or
        (ident = SM_RUN) or
        (ident = SM_HORSERUN) or
        (ident = SM_WALK) or
        (ident = SM_BACKSTEP) or
        (ident = SM_DEATH) or
        (ident = SM_SKELETON) or
        (ident = SM_DIGUP) or
        (ident = SM_ALIVE) then begin
        if Actor = nil then
          Actor := NewActor(chrid, X, Y, Lobyte(cdir), Feature, State);
        if Actor <> nil then begin
          if IPInfo <> 0 then begin
            //Actor.m_nIPower := LoWord(IPInfo);
            Actor.m_nIPowerLvl := HiWord(IPInfo);
          end;
          Actor.m_nChrLight := Hibyte(cdir);
          cdir := Lobyte(cdir);
          if ident = SM_SKELETON then begin
            Actor.m_boDeath := True;
            Actor.m_boSkeleton := True;
          end;
          if ident = SM_DEATH then begin
            if Hibyte(cdir) <> 0 then
              Actor.m_boItemExplore := True;
          end;
        end;
      end;
      if Actor = nil then Exit;
      case ident of
        SM_FEATURECHANGED: begin
            Actor.m_nFeature := Feature;
            Actor.m_nFeatureEx := State;
            if Str <> '' then begin
              DecodeBuffer(Str, @mbw, SizeOf(mbw));
              Actor.m_btTitleIndex := LoWord(mbw.param1);
            end else begin
              Actor.m_btTitleIndex := 0;
            end;
            Actor.FeatureChanged;
          end;
        SM_APPRCHANGED: begin

          end;
        SM_CHARSTATUSCHANGED: begin
            Actor.m_nState := Feature;
            Actor.m_nHitSpeed := State;
            if Str = '1' then begin
              meff := TCharEffect.Create(1110, 10, Actor);
              meff.NextFrameTime := 80;
              meff.ImgLib := g_WMagic2Images;
              g_PlayScene.m_EffectList.Add(meff);
              //PlaySoundName('wav\M1-2.wav');
            end;
          end;
      else begin
          if ident = SM_TURN then begin
            if Str <> '' then begin
              Actor.m_sUserName := Str;
              Actor.m_sUserNameOffSet := FontManager.Default.TextWidth(Actor.m_sUserName) div 2;

            end;
          end;
          Actor.SendMsg(ident, X, Y, cdir, Feature, State, '', 0);
        end;
      end;
    end;
  end;
end;

procedure TPlayScene.DropItemsShow(Surface: TCustomCanvas);
var
  i, k, mx, my, n           : Integer;
  D                         : pTDropItem;
  bcolor                    : TColor;
  dds                       : TCustomLockableTexture;
  S, Str                    : string;
begin
  if g_DropedItemList.Count <= 0 then Exit;
  for k := 0 to g_DropedItemList.Count - 1 do begin
    D := pTDropItem(g_DropedItemList[k]);

    if (D <> nil) then begin                       //ASP×¢ÊÍ
      ScreenXYfromMCXY(D.X, D.Y, mx, my);
      if D.boNonSuch then begin
        Surface.BoldTextOut(
          mx - ((Length(AnsiString(D.Name)) shr 1) * 6) + 6,
          my - 23,
          clRed,
          clBlack,
          D.Name);
      end else if (not g_gcGeneral[5] or D.boShowName) then begin
        ScreenXYfromMCXY(D.X, D.Y, mx, my);
        Surface.BoldTextOut(
          mx - ((Length(AnsiString(D.Name)) shr 1) * 6) + 6,
          my - 23,
          clSkyBlue,
          clBlack,
          D.Name);
      end;
    end;

  end;
end;

end.

