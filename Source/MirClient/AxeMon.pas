unit AxeMon;

interface

uses
  Windows, Messages, SysUtils, Classes, ExtCtrls, Graphics, Controls, Forms, Dialogs, uGameEngine,
  Grobal2, PXL.Canvas, PXL.Textures, cliUtil, ClFunc, magiceff, HumanActor, Actor, clEvent;

const
  DEATHEFFECTBASE           = 340;
  DEATHFIREEFFECTBASE       = 2860;
  AXEMONATTACKFRAME         = 6;
  KUDEGIGASBASE             = 1445;
  COWMONFIREBASE            = 1800;
  COWMONLIGHTBASE           = 1900;
  ZOMBILIGHTINGBASE         = 350;
  ZOMBIDIEBASE              = 340;
  ZOMBILIGHTINGEXPBASE      = 520;
  SCULPTUREFIREBASE         = 1680;
  MOTHPOISONGASBASE         = 3590;
  DUNGPOISONGASBASE         = 3590;
  WARRIORELFFIREBASE        = 820;
  SUPERIORGUARDBASE         = 760;

type
  TSkeletonOma = class(TActor)
  private
  protected
    EffectSurface: TCustomLockableTexture;
    ax: Integer;
    ay: Integer;
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

  TAngel = class(THumActor)
  private
  protected
    EffectSurface: TCustomLockableTexture;
    ax: Integer;
    ay: Integer;
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
    procedure SetSound; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
    //procedure DrawEff(dsurface: TDirectDrawSurface; dx, dy: Integer); override;
  end;

  TDualAxeOma = class(TSkeletonOma)
  private
  public
    procedure Run; override;
  end;

  TCatMon = class(TSkeletonOma)
  private
  public
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

  TArcherMon = class(TCatMon)           //Size: 0x25C Address: 0x00461A90
  public
    procedure Run; override;
  end;

  TScorpionMon = class(TCatMon)
  public
  end;

  THuSuABi = class(TSkeletonOma)
  public
    procedure LoadSurface; override;
  end;

  TZombiDigOut = class(TSkeletonOma)
  public
    procedure RunFrameAction(frame: Integer); override;
  end;

  TZombiZilkin = class(TSkeletonOma)
  public
  end;

  TWhiteSkeleton = class(TSkeletonOma)
  protected
    procedure CalcActorFrame; override;
  public
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

  TGasKuDeGi = class(TActor)            //Size 0x274
  protected
    AttackEffectSurface: TCustomLockableTexture;
    DieEffectSurface: TCustomLockableTexture;
    BoUseDieEffect: Boolean;            //0x258
    firedir: Integer;                   //0x25C
    fire16dir: Integer;                 //0c260
    ax: Integer;                        //0x264
    ay: Integer;                        //0x268
    bx: Integer;
    by: Integer;
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

  TFireCowFaceMon = class(TGasKuDeGi)
  public
    function light: Integer; override;
  end;

  TCowFaceKing = class(TGasKuDeGi)
  public
    function light: Integer; override;
  end;

  TZombiLighting = class(TGasKuDeGi)
  protected
  public
  end;

  TSuperiorGuard = class(TGasKuDeGi)
  protected
  public
  end;

  TExplosionSpider = class(TGasKuDeGi)
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TFlyingSpider = class(TSkeletonOma)   //Size: 0x25C Address: 0x00461F38
  protected
  public
    procedure CalcActorFrame; override;
  end;

  TSculptureMon = class(TSkeletonOma)
  private
    AttackEffectSurface: TCustomLockableTexture;
    ax, ay, firedir: Integer;
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
    procedure Run; override;
  end;

  TSculptureKingMon = class(TSculptureMon)
  public
  end;

  TSmallElfMonster = class(TSkeletonOma)
  public
  end;

  TWarriorElfMonster = class(TSkeletonOma)
  private
    oldframe: Integer;
  public
    procedure RunFrameAction(frame: Integer); override; //ÇÁ·¡ÀÓ¸¶´Ù µ¶Æ¯ÇÏ°Ô ÇØ¾ßÇÒÀÏ
  end;
  //´óòÚò¼
  TElectronicScolpionMon = class(TGasKuDeGi) //Size 0x274 0x3c
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TBossPigMon = class(TGasKuDeGi)       //0x3d
  protected
  public
    procedure LoadSurface; override;
  end;

  TKingOfSculpureKingMon = class(TGasKuDeGi) //0x3e
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TSkeletonKingMon = class(TGasKuDeGi)
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
  end;

  TSamuraiMon = class(TGasKuDeGi)       //0x41
  protected
  public
  end;
  TSkeletonSoldierMon = class(TGasKuDeGi) //0x42 0x43 0x44
  protected
  public
  end;

  TSkeletonArcherMon = class(TArcherMon)
    AttackEffectSurface: TCustomLockableTexture;
    m_boNowDeath: Boolean;
    n264: Integer;
    n268: Integer;
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

  TBanyaGuardMon = class(TSkeletonArcherMon)
    m_DrawEffect: TCustomLockableTexture;
  protected
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

  TStoneMonster = class(TSkeletonArcherMon) //Size: 0x270 0x4d 0x4b
    n26C: TCustomLockableTexture;
  protected
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;
  TPBOMA1Mon = class(TCatMon)           //0x49
  protected
  public
    procedure Run; override;
  end;
  TPBOMA6Mon = class(TCatMon)           //0x4f
  protected
  public
    procedure Run; override;
  end;

  TFireDragon = class(TSkeletonArcherMon) //0x53
    LightningTimer: TTimer;
    m_DrawEffect: TCustomLockableTexture;
  private
    procedure AttackEff;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
    procedure LightningTimerTimer(Sender: TObject);
  end;

  TDragonStatue = class(TSkeletonArcherMon) //Size: 0x270 0x54
    n26C: TCustomLockableTexture;
  protected
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

  TKhazardMon = class(TSkeletonOma)
  protected
  public
    procedure CalcActorFrame; override;
    constructor Create; override;
  end;
  TFrostTiger = class(TSkeletonOma)
    boActive: Boolean;
    boCasted: Boolean;
  protected
  public
    procedure Run; override;
    procedure CalcActorFrame; override;
    constructor Create; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
  end;

  TRedThunderZuma = class(TGasKuDeGi)
    boCasted: Boolean;
  protected
  public
    procedure Run; override;
    procedure CalcActorFrame; override;
    constructor Create; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
  end;

  TCrystalSpider = class(TGasKuDeGi)
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TYimoogi = class(TGasKuDeGi)
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TBlackFox = class(TGasKuDeGi)         //Size 0x274 0x3c
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TGreenCrystalSpider = class(TGasKuDeGi) //Size 0x274 0x3c
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TSpiderKing = class(TBanyaGuardMon)
    m_DrawEffect: TCustomLockableTexture;
  protected
  public
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
  end;

  TSnowMon = class(TActor)
  protected
    AttackEffectSurface: TCustomLockableTexture;
    AttackEffectSurface2: TCustomLockableTexture;
    DieEffectSurface: TCustomLockableTexture;
    ChrEffect: TCustomLockableTexture;
    BoUseDieEffect: Boolean;            //0x258
    firedir: Integer;                   //0x25C
    fire16dir: Integer;                 //0c260
    ax: Integer;                        //0x264
    ay: Integer;                        //0x268
    bx: Integer;
    by: Integer;
    m_bowChrEffect: Boolean;
  public
    constructor Create; override;
    procedure CalcActorFrame; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
    procedure Run; override;
    procedure SetSound; override;
    procedure RunSound; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

  TTiger = class(TActor)
  private
  protected
    EffectSurface: TCustomLockableTexture;
    ax: Integer;
    ay: Integer;
    firedir: Byte;
  public
    constructor Create; override;
    procedure Run; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

  TDragon = class(TActor)
  private
    procedure AttackEff;
  protected
    BodySurface: TCustomLockableTexture;
    EffectSurface: TCustomLockableTexture;
    ax, ax2: Integer;
    ay, ay2: Integer;
    firedir: Byte;
  public
    constructor Create; override;
    procedure Run; override;
    procedure CalcActorFrame; override;
    procedure LoadSurface; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

  TGhostShipMonster = class(TActor)
    FFireBall: Boolean;
    FLighting: Boolean;
  private
    procedure AttackEff;
  protected
    ShadowSurface: TCustomLockableTexture;
    EffectSurface: TCustomLockableTexture;
    ax, ax2: Integer;
    ay, ay2: Integer;
    firedir: Byte;
  public
    constructor Create; override;
    procedure Run; override;
    procedure CalcActorFrame; override;
    procedure SetSound; override;
    procedure RunSound; override;
    procedure RunActSound(frame: Integer); override;
    procedure RunFrameAction(frame: Integer); override;
    procedure LoadSurface; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
  end;

implementation

uses
  ClMain, SoundUtil, WIL, MShare;

procedure TWhiteSkeleton.CalcActorFrame;
var
  haircount                 : Integer;
begin
  inherited;
  m_boUseMagic := False;
  m_nCurrentFrame := -1;
  m_nHitEffectNumber := 0;
  m_nBodyOffset := GetOffset(m_wAppearance);
  m_Action := GetRaceByPM(m_btRace, m_wAppearance);
  if m_Action = nil then Exit;
  case m_nCurrentAction of
    SM_POWERHIT: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        if (m_nCurrentAction = SM_POWERHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 1;
          case m_btRace of
            91: Inc(m_nHitEffectNumber, 101);
            92: Inc(m_nHitEffectNumber, 201);
            93: Inc(m_nHitEffectNumber, 301);
          end;
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
  end;
end;

procedure TWhiteSkeleton.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  bCanDraw                  : Boolean;
  idx, ax, ay               : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  wimg                      : TWMImages;
begin
  if not (m_btDir in [0..7]) then Exit;

  inherited;

  //d := nil;
  {if GetTickCount - m_dwLoadSurfaceTime > g_dwLoadSurfaceTime then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
  //if m_sUserName = '' then Exit;  //1015
  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface,
      m_BodySurface,
      dx + m_nPx + m_nShiftX,
      dy + m_nPy + m_nShiftY,
      blend,
      ceff);
  end;}

  if m_boUseMagic and (m_CurMagic.EffectNumber > 0) then
    if m_nCurEffFrame in [0..m_nSpellFrame - 1] then begin
      GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, idx);
      idx := idx + m_nCurEffFrame;
      d := nil;
      if wimg <> nil then
        d := wimg.GetCachedImage(idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend(
          dx + ax + m_nShiftX,
          dy + ay + m_nShiftY,
          d, 1);
    end;

  if m_boHitEffect and (m_nHitEffectNumber > 0) then begin
    GetEffectBase(m_nHitEffectNumber - 1, 1, wimg, idx);
    if wimg <> nil then begin
      idx := idx + m_btDir * 10 + (m_nCurrentFrame - m_nStartFrame);
      d := wimg.GetCachedImage(idx, ax, ay);
    end;
    if d <> nil then dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, d, 1);
  end;
end;

constructor TSkeletonOma.Create;
begin
  inherited Create;
  EffectSurface := nil;
  m_boUseEffect := False;
end;

procedure TSkeletonOma.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_nCurrentFrame := -1;
  m_boReverseFrame := False;
  m_boUseEffect := False;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);

  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActStand.start
        else
          m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
        m_dwFrameTime := pm.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);
        if m_btRace in [118, 119] then begin
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
        end;
      end;
    SM_WALK, SM_BACKSTEP: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActWalk.start
        else
          m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        //WarMode := FALSE;
        m_nMoveStep := 1;
        if m_btRace in [118, 119] then begin
          if (Random(3000) mod 3) = 1 then begin
            if m_btRace = 118 then g_SndMgr.PlaySound(3451);
            if m_btRace = 119 then g_SndMgr.PlaySound(3461);
          end;
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
        end;
        if m_nCurrentAction = SM_WALK then
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else                            //sm_backstep
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_DIGUP: begin
        case m_btRace of
          23, 91..93: m_nStartFrame := pm.ActDeath.start;
        else
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        end;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_DIGDOWN: begin
        if m_btRace = 55 then begin
          m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
          m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
          m_dwFrameTime := pm.ActCritical.ftime;
          m_dwStartTime := GetTickCount;
          m_boReverseFrame := True;
          Shift(m_btDir, 0, 0, 1);
        end;
      end;
    SM_HIT, SM_FLYAXE, SM_LIGHTING: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        //WarMode := TRUE;
        m_dwWarModeTime := GetTickCount;
        if (m_btRace = 16) or (m_btRace = 54) then
          m_boUseEffect := True;
        Shift(m_btDir, 0, 0, 1);
        if m_btRace in [118, 119] then begin
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
        end;
      end;
    SM_STRUCK: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActStruck.start
        else
          m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime; //pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActDie.start
        else
          m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;   //
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActDie.start
        else
          m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        if m_btRace <> 22 then
          m_boUseEffect := True;
      end;
    SM_SKELETON: begin
        m_nStartFrame := pm.ActDeath.start;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_ALIVE: begin
        if m_btRace in [117] then
          m_nStartFrame := pm.ActDeath.start
        else
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

function TSkeletonOma.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //jacky
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  if m_boDeath then begin
    if m_wAppearance in [30..34, 151] then //¿ì¸é±ÍÀÎ °æ¿ì ½ÃÃ¼°¡ »ç¶÷À» µ¤´Â °ÍÀ» ¸·±â À§ÇØ
      m_nDownDrawLevel := 1;

    if m_boSkeleton then
      Result := pm.ActDeath.start
    else if m_btRace = 120 then begin
      Result := 417;
      m_boUseEffect := False;
    end else if m_btRace in [117..119] then begin
      Result := pm.ActDie.start + (pm.ActDie.frame - 1);
      m_boUseEffect := False;
    end else
      Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then
      cf := 0
    else if m_nCurrentDefFrame >= pm.ActStand.frame then
      cf := 0
    else
      cf := m_nCurrentDefFrame;

    if m_btRace = 120 then begin
      case m_nTempState of
        1: m_nStartFrame := 0;
        2: m_nStartFrame := 80;
        3: m_nStartFrame := 160;
        4: m_nStartFrame := 240;
        5: m_nStartFrame := 320;
      end;
      Result := m_nStartFrame + cf
    end
    else if m_btRace in [117..119] then
      Result := pm.ActStand.start + cf
    else
      Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;

    if m_btRace in [118, 119] then begin
      m_boUseEffect := True;
      m_dwEffectstarttime := GetTickCount;
      m_dwEffectframetime := pm.ActStand.ftime;
    end;

    if m_btRace = 118 then
      m_neffectframe := 2730 + m_nCurrentFrame
    else if m_btRace = 119 then
      m_neffectframe := 2840 + m_nCurrentFrame
    else if m_btRace = 120 then begin
      m_boUseEffect := True;
      m_dwEffectstarttime := GetTickCount;
      m_dwEffectframetime := pm.ActStand.ftime;
      m_neffectframe := 2940 + m_nCurrentFrame;
    end;
  end;
end;

procedure TSkeletonOma.LoadSurface;
begin
  if (m_btRace = 117) and m_boDeath then begin
    m_BodySurface := nil;
    Exit;
  end;

  inherited LoadSurface;

  case m_btRace of
    //¸ó½ºÅÍ
    14, 15, 17, 22, 53: begin
        if m_boUseEffect then
          EffectSurface := g_WMon3Img.GetCachedImage(DEATHEFFECTBASE + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
    23: begin
        if m_nCurrentAction = SM_DIGUP then begin
          m_BodySurface := nil;
          EffectSurface := g_WMon4Img.GetCachedImage(m_nBodyOffset + m_nCurrentFrame, ax, ay);
          m_boUseEffect := True;
        end else
          m_boUseEffect := False;
      end;
    91..93: begin
        if m_nCurrentAction = SM_DIGUP then begin
          m_BodySurface := nil;
          EffectSurface := g_WMagic7Images.GetCachedImage(m_nCurrentFrame, ax, ay);
          m_boUseEffect := True;
        end else
          m_boUseEffect := False;
      end;
  end;
  case m_wAppearance of
    703: if (m_nCurrentAction = SM_DIGUP) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(220 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end else if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1970 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
    705: if (m_nCurrentAction = SM_DIGUP) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(230 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end else if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1980 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
    707: if (m_nCurrentAction = SM_DIGUP) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(240 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end else if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1990 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;

    704: if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1970 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
    706: if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1980 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
    708: if (m_nCurrentAction = SM_NOWDEATH) then begin
        m_boUseEffect := True;
        EffectSurface := g_WMagic8Images.GetCachedImage(1990 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
  end;
end;

procedure TSkeletonOma.Run;
var
  prv                       : Integer;
  m_dwFrameTimetime         : LongWord;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  //»ç¿îµå È¿°ú
  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        //µ¿ÀÛÀÌ ³¡³².
        m_nCurrentAction := 0;          //µ¿ÀÛ ¿Ï·á
        m_boUseEffect := False;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

procedure TSkeletonOma.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  bCanDraw                  : Boolean;
begin
  if not (m_btDir in [0..7]) then Exit;

  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;                        //bodysurfaceµîÀÌ loadsurface¸¦ ´Ù½Ã ºÎ¸£Áö ¾Ê¾Æ ¸Þ¸ð¸®°¡ ÇÁ¸®µÇ´Â °ÍÀ» ¸·À½
  end;
  //if m_sUserName = '' then Exit;  //1015
  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface, m_BodySurface, dx + m_nPx + m_nShiftX, dy + m_nPy + m_nShiftY, blend, ceff);
  end;

  if m_boUseEffect then
    if EffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        EffectSurface, 1);
    end;
end;

procedure TDualAxeOma.Run;
var
  prv                       : Integer;
  m_dwFrameTimetime         : LongWord;
  meff                      : TFlyingAxe;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  //»ç¿îµå È¿°ú
  RunActSound(m_nCurrentFrame - m_nStartFrame);
  //ÇÁ·¡ÀÓ¸¶´Ù ÇØ¾ß ÇÒÀÏ
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        //µ¿ÀÛÀÌ ³¡³².
        m_nCurrentAction := 0;          //µ¿ÀÛ ¿Ï·á
        m_boUseEffect := False;
      end;
      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = AXEMONATTACKFRAME - 4) then begin
        //¸¶¹ý ¹ß»ç
        meff := TFlyingAxe(g_PlayScene.NewFlyObject(Self,
          m_nCurrX,
          m_nCurrY,
          m_nTargetX,
          m_nTargetY,
          m_nTargetRecog,
          mtFlyAxe));
        if meff <> nil then begin
          meff.ImgLib := g_WMon3Img;
          case m_btRace of
            15: meff.FlyImageBase := FLYOMAAXEBASE;
            22: meff.FlyImageBase := THORNBASE;
            57: begin
                meff.FlyImageBase := 3586;
                meff.ImgLib := g_WMon33Img;
              end;
            58: begin
                meff.FlyImageBase := 4016;
                meff.ImgLib := g_WMon33Img;
              end;
          end;
        end;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

procedure TWarriorElfMonster.RunFrameAction(frame: Integer);
var
  meff                      : TMapEffect;
  event                     : TClEvent;
begin
  {//case m_wAppearance of
  //  703..708: ;
  //else begin

      if m_nCurrentAction = SM_DIGUP then begin
        m_boUseEffect := True;
        EffectSurface := g_WMon18Img.GetCachedImage(810 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
      if m_nCurrentAction = SM_NOWDEATH then begin
        m_boUseEffect := True;
        EffectSurface := g_WMon18Img.GetCachedImage(900 + m_nCurrentFrame - m_nStartFrame, ax, ay);
      end;
  //  end;
  //end;}

  if m_nCurrentAction = SM_HIT then begin
    if (frame = 5) and (oldframe <> frame) then begin
      if m_wAppearance = 704 then begin
        meff := TMapEffect.Create(250 + 10 * m_btDir + 1, 6, m_nCurrX, m_nCurrY);
        meff.ImgLib := g_WMagic8Images;
        meff.NextFrameTime := 100;
        g_PlayScene.m_EffectList.Add(meff);
      end else if m_wAppearance = 706 then begin
        meff := TMapEffect.Create(330 + 10 * m_btDir + 1, 6, m_nCurrX, m_nCurrY);
        meff.ImgLib := g_WMagic8Images;
        meff.NextFrameTime := 100;
        g_PlayScene.m_EffectList.Add(meff);
      end else if m_wAppearance = 708 then begin
        meff := TMapEffect.Create(410 + 10 * m_btDir + 1, 6, m_nCurrX, m_nCurrY);
        meff.ImgLib := g_WMagic8Images;
        meff.NextFrameTime := 100;
        g_PlayScene.m_EffectList.Add(meff);
      end else begin
        meff := TMapEffect.Create(WARRIORELFFIREBASE + 10 * m_btDir + 1, 5, m_nCurrX, m_nCurrY);
        meff.ImgLib := g_WMon18Img;
        meff.NextFrameTime := 100;
        g_PlayScene.m_EffectList.Add(meff);
      end;
    end;
    oldframe := frame;
  end;
end;

procedure TCatMon.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  bCanDraw                  : Boolean;
begin
  if not (m_btDir in [0..7]) then Exit;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;                        //bodysurfaceµîÀÌ loadsurface¸¦ ´Ù½Ã ºÎ¸£Áö ¾Ê¾Æ ¸Þ¸ð¸®°¡ ÇÁ¸®µÇ´Â °ÍÀ» ¸·À½
  end;
  //if m_sUserName = '' then Exit;  //1015
  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface, m_BodySurface, dx + m_nPx + m_nShiftX, dy + m_nPy + m_nShiftY, blend, ceff);
  end;
end;

{============================= TArcherMon =============================}

procedure TArcherMon.Run;
var
  prv                       : Integer;
  m_dwFrameTimetime         : LongWord;
  meff                      : TFlyingAxe;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;
  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
      end;
      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = 4) then begin

        meff := TFlyingArrow(g_PlayScene.NewFlyObject(Self,
          m_nCurrX,
          m_nCurrY,
          m_nTargetX,
          m_nTargetY,
          m_nTargetRecog,
          mtFlyArrow));
        if meff <> nil then begin
          meff.ImgLib := g_WEffectImg;
          meff.NextFrameTime := 30;
          meff.FlyImageBase := ARCHERBASE2;
        end;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

{============================= TZombiDigOut =============================}

procedure TZombiDigOut.RunFrameAction(frame: Integer);
var
  clEvent                   : TClEvent;
begin
  if m_nCurrentAction = SM_DIGUP then begin
    if frame = 6 then begin
      clEvent := TClEvent.Create(m_nCurrentEvent, m_nCurrX, m_nCurrY, ET_DIGOUTZOMBI);
      clEvent.m_nDir := m_btDir;
      EventMan.AddEvent(clEvent);
    end;
  end;
end;

{============================== THuSuABi =============================}

procedure THuSuABi.LoadSurface;
begin
  inherited LoadSurface;
  if m_boUseEffect then
    EffectSurface := g_WMon3Img.GetCachedImage(DEATHFIREEFFECTBASE + m_nCurrentFrame - m_nStartFrame, ax, ay);
end;

{============================== TGasKuDeGi =============================}

constructor TGasKuDeGi.Create;
begin
  inherited Create;
  AttackEffectSurface := nil;
  DieEffectSurface := nil;
  m_boUseEffect := False;
  BoUseDieEffect := False;
end;

procedure TGasKuDeGi.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  Actor                     : TActor;
  haircount, scx, scy, stx, sty: Integer;
  meff                      : TCharEffect;
begin
  m_nCurrentFrame := -1;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
        m_dwFrameTime := pm.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_WALK: begin
        m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_WALK then
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else                            //sm_backstep
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_HIT, SM_LIGHTING: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;

        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        if m_btRace = 20 then m_nEffectEnd := m_nEndFrame + 1
        else m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;

        Actor := g_PlayScene.FindActor(m_nTargetRecog);
        if Actor <> nil then begin
          g_PlayScene.ScreenXYfromMCXY(m_nCurrX, m_nCurrY, scx, scy);
          g_PlayScene.ScreenXYfromMCXY(Actor.m_nCurrX, Actor.m_nCurrY, stx, sty);
          fire16dir := GetFlyDirection16(scx, scy, stx, sty);
        end else
          fire16dir := firedir * 2;
      end;
    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime; //pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;   //
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        if (m_btRace in [40, 65..69]) then
          BoUseDieEffect := True;
      end;
    SM_SKELETON: begin
        m_nStartFrame := pm.ActDeath.start;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

function TGasKuDeGi.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //jacky
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  if m_boDeath then begin
    if m_boSkeleton then
      Result := pm.ActDeath.start
    else Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then cf := 0
    else if m_nCurrentDefFrame >= pm.ActStand.frame then cf := 0
    else cf := m_nCurrentDefFrame;
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
  end;
end;

procedure TGasKuDeGi.LoadSurface;
begin
  inherited LoadSurface;
  case m_btRace of
    //¹¥»÷Ð§¹û
    16: {//¶´Çù} begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon3Img.GetCachedImage(
            KUDEGIGASBASE - 1 + (firedir * 10) + m_neffectframe - m_nEffectStart,
            ax, ay);
      end;
    20: {//»ðÑæÎÖÂê} begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon4Img.GetCachedImage(
            COWMONFIREBASE + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;
    21: {//ÎÖÂê½ÌÖ÷} begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon4Img.GetCachedImage(
            COWMONLIGHTBASE + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;
    24: begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMonImg.GetCachedImage(
            SUPERIORGUARDBASE + (m_btDir * 8) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;

    40: {//½©Ê¬1} begin
        if m_boUseEffect then begin
          AttackEffectSurface := g_WMon5Img.GetCachedImage(
            ZOMBILIGHTINGBASE + (fire16dir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
        end;
        if BoUseDieEffect then begin
          DieEffectSurface := g_WMon5Img.GetCachedImage(
            ZOMBIDIEBASE + m_nCurrentFrame - m_nStartFrame, //
            bx, by);
        end;
      end;
    52: {//Ð¨¶ê} begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon4Img.GetCachedImage(
            MOTHPOISONGASBASE + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;
    53: {//·à³æ} begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon3Img.GetCachedImage(
            DUNGPOISONGASBASE + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;
    64: begin
        if m_boUseEffect then begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            720 + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
        end;
      end;
    65: begin
        if BoUseDieEffect then begin
          DieEffectSurface := g_WMon20Img.GetCachedImage(
            350 + m_nCurrentFrame - m_nStartFrame, bx, by);
        end;
      end;
    66: begin
        if BoUseDieEffect then begin
          DieEffectSurface := g_WMon20Img.GetCachedImage(
            1600 + m_nCurrentFrame - m_nStartFrame, bx, by);
        end;
      end;
    67: begin
        if BoUseDieEffect then begin
          DieEffectSurface := g_WMon20Img.GetCachedImage(
            1160 + (m_btDir * 10) + m_nCurrentFrame - m_nStartFrame, bx, by);
        end;
      end;
    68: begin
        if BoUseDieEffect then begin
          DieEffectSurface := g_WMon20Img.GetCachedImage(
            1600 + m_nCurrentFrame - m_nStartFrame, bx, by);
        end;
      end;

  end;
end;

procedure TGasKuDeGi.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  //
  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  if m_boUseEffect then begin
    if m_boMsgMuch then m_dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        BoUseDieEffect := False;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

procedure TGasKuDeGi.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  bCanDraw                  : Boolean;
begin
  if not (m_btDir in [0..7]) then Exit;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface, m_BodySurface, dx + m_nPx + m_nShiftX, dy + m_nPy + m_nShiftY, blend, ceff);
  end;
end;

procedure TGasKuDeGi.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
begin
  if m_boUseEffect then begin
    if AttackEffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        AttackEffectSurface, 1);
    end;
  end;
  if BoUseDieEffect then
    if DieEffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + bx + m_nShiftX,
        dy + by + m_nShiftY,
        DieEffectSurface, 1);
    end;
end;

{-----------------------------------------------------------}

function TFireCowFaceMon.light: Integer;
var
  L                         : Integer;
begin
  L := m_nChrLight;
  if L < 2 then begin
    if m_boUseEffect then
      L := 2;
  end;
  Result := L;
end;

function TCowFaceKing.light: Integer;
var
  L                         : Integer;
begin
  L := m_nChrLight;
  if L < 2 then begin
    if m_boUseEffect then
      L := 2;
  end;
  Result := L;
end;

{-----------------------------------------------------------}

//procedure TZombiLighting.Run;

{-----------------------------------------------------------}

procedure TSculptureMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_nCurrentFrame := -1;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  m_boUseEffect := False;

  case m_nCurrentAction of
    SM_TURN: begin
        if (m_nState and STATE_STONE_MODE) <> 0 then begin
          if (m_btRace = 48) or (m_btRace = 49) then
            m_nStartFrame := pm.ActDeath.start // + Dir * (pm.ActDeath.frame + pm.ActDeath.skip)
          else
            m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
          m_nEndFrame := m_nStartFrame;
          m_dwFrameTime := pm.ActDeath.ftime;
          m_dwStartTime := GetTickCount;
          m_nDefFrameCount := pm.ActDeath.frame;
        end else begin
          m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
          m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
          m_dwFrameTime := pm.ActStand.ftime;
          m_dwStartTime := GetTickCount;
          m_nDefFrameCount := pm.ActStand.frame;
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_WALK, SM_BACKSTEP: begin
        m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        //WarMode := FALSE;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_WALK then
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else                            //sm_backstep
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_DIGUP: {//°È±â ¾øÀ½, SM_DIGUP, ¹æÇâ ¾øÀ½.} begin
        if (m_btRace = 48) or (m_btRace = 49) then begin
          m_nStartFrame := pm.ActDeath.start;
        end else begin
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        end;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        //WarMode := FALSE;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        if m_btRace = 49 then begin
          m_boUseEffect := True;
          firedir := m_btDir;
          m_neffectframe := 0;          //startframe;
          m_nEffectStart := 0;          //startframe;
          m_nEffectEnd := m_nEffectStart + 8;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime; //pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;   //
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

procedure TSculptureMon.LoadSurface;
begin
  inherited LoadSurface;
  case m_btRace of
    48, 49: begin
        if m_boUseEffect then
          AttackEffectSurface := g_WMon7Img.GetCachedImage(
            SCULPTUREFIREBASE + (firedir * 10) + m_neffectframe - m_nEffectStart, //
            ax, ay);
      end;
  end;
end;

function TSculptureMon.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //jacky
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  if m_boDeath then begin
    Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    if (m_nState and STATE_STONE_MODE) <> 0 then begin
      case m_btRace of
        47: Result := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        48, 49: Result := pm.ActDeath.start;
      end;
    end else begin
      m_nDefFrameCount := pm.ActStand.frame;
      if m_nCurrentDefFrame < 0 then cf := 0
      else if m_nCurrentDefFrame >= pm.ActStand.frame then cf := 0
      else cf := m_nCurrentDefFrame;
      Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
    end;
  end;
end;

procedure TSculptureMon.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
begin
  if m_boUseEffect then
    if AttackEffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        AttackEffectSurface, 1);
    end;
end;

procedure TSculptureMon.Run;
var
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;
  if m_boUseEffect then begin
    m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;
  inherited Run;
end;

{ TBanyaGuardMon }

procedure TBanyaGuardMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_HIT: begin
        if m_btRace in [117..119] then
          m_nStartFrame := pm.ActAttack.start
        else
          m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        if not (m_btRace in [27, 28, 111]) then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end;
        if m_btRace in [113, 114, 115] then
          m_boUseEffect := False
      end;
    SM_LIGHTING, SM_LIGHTING_1: begin
        if (m_btRace in [117..119]) then begin
          m_nStartFrame := pm.ActCritical.start
        end else
          m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        if (m_btRace in [71, 72, 111]) then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end else if (m_btRace in [27, 28]) then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          if m_btRace = 28 then m_nEffectEnd := m_nEndFrame - 4;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end else if (m_btRace in [113, 114]) then begin
          m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
          m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
          m_dwFrameTime := pm.ActAttack.ftime;
          if m_btRace = 113 then begin
            m_boUseEffect := True;
            m_neffectframe := 350 + m_btDir * 10;
            m_nEffectStart := m_neffectframe;
            m_nEffectEnd := m_neffectframe + 5;
            m_dwEffectstarttime := GetTickCount;
            m_dwEffectframetime := m_dwFrameTime;
          end;
        end else if (m_btRace = 117) and (m_nCurrentAction = SM_LIGHTING_1) then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame + 15;
          m_nEffectStart := m_neffectframe;
          m_nEffectEnd := m_neffectframe + 3;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
          g_SndMgr.PlaySound(1900);
        end else if m_btRace in [118, 119] then begin
          m_boUseEffect := True;
          m_boUseMagic := False;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
          if m_btRace = 118 then
            g_SndMgr.PlaySound(3452);
          m_neffectframe := 2750;
          m_nEffectStart := 2750;
          m_nEffectEnd := 2750 + 9;
          if m_btRace = 119 then begin
            g_SndMgr.PlaySound(3462);
            m_neffectframe := 2860;
            m_nEffectStart := 2860;
            m_nEffectEnd := 2860 + 9;
          end;
        end;
      end;
    SM_LIGHTING_2: begin
        if m_btRace = 115 then begin
          m_nStartFrame := 420 + m_btDir * 10;
          m_nEndFrame := m_nStartFrame + 9;
          m_dwFrameTime := pm.ActCritical.ftime;
          m_dwStartTime := GetTickCount;
          m_dwWarModeTime := GetTickCount;
          Shift(m_btDir, 0, 0, 1);

          m_boUseEffect := True;
          m_neffectframe := 0;
          m_nEffectStart := 0;
          m_nEffectEnd := 10;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
          g_SndMgr.PlaySound(3428);
        end;
      end;

  else inherited;
  end;
end;

constructor TBanyaGuardMon.Create;
begin
  inherited;
  m_DrawEffect := nil;
end;

procedure TBanyaGuardMon.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
begin
  inherited;
  if m_boUseEffect and (m_DrawEffect <> nil) then
    dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, m_DrawEffect, 1);
end;

procedure TBanyaGuardMon.LoadSurface;
begin
  if (m_btRace = 117) and m_boDeath then begin
    m_BodySurface := nil;
    Exit;
  end;

  inherited;
  if m_boNowDeath then begin
    case m_btRace of
      070: AttackEffectSurface := g_WMon21Img.GetCachedImage(2320 + m_nCurrentFrame - m_nStartFrame, n264, n268);
      071: AttackEffectSurface := g_WMon21Img.GetCachedImage(2870 + (m_btDir * 10) + m_nCurrentFrame - m_nStartFrame, n264, n268);
      078: AttackEffectSurface := g_WMon22Img.GetCachedImage(3120 + (m_btDir * 4) + m_nCurrentFrame - m_nStartFrame, n264, n268);
      111: AttackEffectSurface := g_WMon24Img.GetCachedImage(3710 + m_nCurrentFrame - m_nStartFrame, n264, n268);
      113, 114, 115: begin
          AttackEffectSurface := g_WMon33Img.GetCachedImage(340 + m_nCurrentFrame - m_nStartFrame, n264, n268);
          if (m_nCurrentFrame - m_nStartFrame) = 0 then
            g_SndMgr.PlaySound(10420);
        end;
      118: begin
          AttackEffectSurface := g_WMon33Img.GetCachedImage(2770 + m_nCurrentFrame - m_nStartFrame, n264, n268);
        end;
      119: begin
          AttackEffectSurface := g_WMon33Img.GetCachedImage(2880 + m_nCurrentFrame - m_nStartFrame, n264, n268);
        end;
    end;
  end else if m_boUseEffect then begin
    case m_btRace of
      70: if m_nCurrentAction = SM_HIT then
          m_DrawEffect := g_WMon21Img.GetCachedImage(2230 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      71: case m_nCurrentAction of
          SM_HIT: m_DrawEffect := g_WMon21Img.GetCachedImage(2780 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
          SM_FLYAXE..SM_LIGHTING: m_DrawEffect := g_WMon21Img.GetCachedImage(2960 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
        end;
      72: begin
          if m_nCurrentAction = SM_HIT then m_DrawEffect := g_WMon21Img.GetCachedImage(3490 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
          if m_nCurrentAction = SM_LIGHTING then m_DrawEffect := g_WMon21Img.GetCachedImage(3580 + m_neffectframe - m_nEffectStart, ax, ay);
        end;
      78: if m_nCurrentAction = SM_HIT then
          m_DrawEffect := g_WMon22Img.GetCachedImage(3440 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      111: if m_nCurrentAction = SM_LIGHTING then
          m_DrawEffect := g_WMon24Img.GetCachedImage(3720 + m_neffectframe - m_nEffectStart, ax, ay);
      027: if m_nCurrentAction = SM_LIGHTING then
          m_DrawEffect := g_WMon26Img.GetCachedImage(420 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      028: if m_nCurrentAction = SM_LIGHTING then
          m_DrawEffect := g_WMon26Img.GetCachedImage(930 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      113: if m_nCurrentAction = SM_LIGHTING then
          m_DrawEffect := g_WMon33Img.GetCachedImage(350 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      115: if m_nCurrentAction = SM_LIGHTING_2 then
          m_DrawEffect := g_WMon2Img.GetCachedImage(m_neffectframe - m_nEffectStart, ax, ay);
      117: if m_nCurrentAction = SM_LIGHTING_1 then
          m_DrawEffect := g_WMon33Img.GetCachedImage(2665 + m_neffectframe - m_nEffectStart, ax, ay);
      118: begin
          if not m_boNowDeath then begin
            m_dwEffectframetime := m_dwFrameTime;
            m_DrawEffect := g_WMon33Img.GetCachedImage(
              2730 + m_nCurrentFrame, ax, ay);
          end;
        end;
      119: begin
          if not m_boNowDeath then begin
            m_dwEffectframetime := m_dwFrameTime;
            m_DrawEffect := g_WMon33Img.GetCachedImage(
              2840 + m_nCurrentFrame, ax, ay);
          end;
        end;
    end;
  end;
end;

procedure TBanyaGuardMon.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
  bofly                     : Boolean;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;
  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  if m_boUseEffect then begin
    if m_boMsgMuch then m_dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
        m_boNowDeath := False;
      end;
      if m_nCurrentAction = SM_LIGHTING then begin
        if (m_btRace = 117) and (m_nCurrentFrame - m_nStartFrame = 1) then begin
          g_PlayScene.NewMagic(Self,
            MAGIC_SIDESTONE_ATT1,
            MAGIC_SIDESTONE_ATT1,
            m_nCurrX,
            m_nCurrY,
            m_nCurrX,
            m_nCurrY,
            m_nRecogId,
            mtGroundEffect,
            False,
            30,
            bofly);
          g_SndMgr.PlaySound(3462);
        end;
        if (m_nCurrentFrame - m_nStartFrame) = 4 then begin
          if (m_btRace = 111) then begin //RightGuard
            g_PlayScene.NewMagic(Self, 7, 33, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtGroundEffect, False, 30, bofly);
            g_SndMgr.PlaySound(2276, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 101) then begin //RightGuard
            g_PlayScene.NewMagic(Self, 1, 1, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtFly, True, 20, bofly);
            g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 70) {or (m_btRace = 81)} then begin
            g_PlayScene.NewMagic(Self, 7 {m_nMagicNum}, 9 {8}, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtThunder, False, 30, bofly);
            g_SndMgr.PlaySound(10112, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 71) then begin
            g_PlayScene.NewMagic(Self, {1} 11, {1} 32, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtFly, True, 30, bofly);
            g_SndMgr.PlaySound(10012, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 72) then begin
            g_PlayScene.NewMagic(Self, 11, 32, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtGroundEffect, False, 30, bofly);
            g_SndMgr.PlaySound(2276, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 78) then begin
            g_PlayScene.NewMagic(Self, 11, 37, m_nCurrX, m_nCurrY, m_nCurrX, m_nCurrY, m_nRecogId, mtGroundEffect, False, 30, bofly);
            g_SndMgr.PlaySound(2396, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 81) then begin
            g_PlayScene.NewMagic(Self, 7, 9, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtThunder, False, 30, bofly);
            g_SndMgr.PlaySound(10112, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 113) then begin
            g_SndMgr.PlaySound(3406, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 114) then begin
            g_PlayScene.NewMagic(Self,
              MAGIC_FOX_THUNDER,        //11,
              MAGIC_FOX_THUNDER,
              m_nCurrX,
              m_nCurrY,
              m_nTargetX,
              m_nTargetY,
              m_nTargetRecog,
              mtThunder,
              False,
              30,
              bofly);
            g_SndMgr.PlaySound(3416, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 115) then begin
            g_PlayScene.NewMagic(Self,
              MAGIC_FOX_FIRE2,
              MAGIC_FOX_FIRE2,
              m_nCurrX,
              m_nCurrY,
              m_nTargetX,
              m_nTargetY,
              m_nTargetRecog,
              mtExploBujauk,
              False,
              30,
              bofly);
            m_nMagicstartsound := 10130;
            m_nMagicFireSound := 10131;
            m_nMagicexplosionsound := 3426;
          end;
        end;
      end else if m_nCurrentAction = SM_LIGHTING_1 then begin
        if (m_nCurrentFrame - m_nStartFrame = 4) then begin
          if (m_btRace = 114) then begin
            g_PlayScene.NewMagic(Self,
              MAGIC_FOX_FIRE1,
              MAGIC_FOX_FIRE1,
              m_nCurrX,
              m_nCurrY,
              m_nTargetX,
              m_nTargetY,
              m_nTargetRecog,
              mtThunder,
              False,
              30,
              bofly);
            g_SndMgr.PlaySound(3417, m_nCurrX, m_nCurrY);
          end else if (m_btRace = 115) then begin
            g_PlayScene.NewMagic(Self,
              MAGIC_FOX_CURSE,
              MAGIC_FOX_CURSE,
              m_nCurrX,
              m_nCurrY,
              m_nTargetX,
              m_nTargetY,
              m_nTargetRecog,
              mtExploBujauk,
              False,
              30,
              bofly);
            m_nMagicstartsound := 10130;
            m_nMagicFireSound := 10131;
            m_nMagicexplosionsound := 3427;
          end;
        end;

      end;
      m_nCurrentDefFrame := 0;
      m_dwDefFrameTime := GetTickCount;
    end;
  end else begin
    if m_btRace in [118, 119] then begin
      if GetTickCount - m_dwDefFrameTime > 150 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end else if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

{ TElectronicScolpionMon }

procedure TElectronicScolpionMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TElectronicScolpionMon.LoadSurface;
begin
  inherited;
  if (m_btRace = 60) and m_boUseEffect and (m_nCurrentAction = SM_LIGHTING) then
    AttackEffectSurface := g_WMon19Img.GetCachedImage(430 + (firedir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
end;

{ TBossPigMon }

procedure TBossPigMon.LoadSurface;
begin
  inherited;
  if (m_btRace = 61) and m_boUseEffect then begin
    AttackEffectSurface := g_WMon19Img.GetCachedImage(
      860 + (firedir * 10) + m_neffectframe - m_nEffectStart,
      ax, ay);
  end;
end;

{ TKingOfSculpureKingMon }

procedure TKingOfSculpureKingMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        if m_btRace = 62 then begin
          m_boUseEffect := True;
          firedir := m_btDir;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        m_neffectframe := pm.ActDie.start;
        m_nEffectStart := pm.ActDie.start;
        m_nEffectEnd := pm.ActDie.start + pm.ActDie.frame - 1;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
        m_boUseEffect := True;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TKingOfSculpureKingMon.LoadSurface;
begin
  inherited;
  if m_boUseEffect then begin
    case m_nCurrentAction of
      SM_HIT: begin
          AttackEffectSurface := g_WMon19Img.GetCachedImage(1490 + (firedir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
        end;
      SM_LIGHTING: begin
          case m_btRace of
            25: AttackEffectSurface := g_WMon25Img.GetCachedImage(426 + (firedir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
            26: AttackEffectSurface := g_WMon25Img.GetCachedImage(932 + (firedir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
            62: AttackEffectSurface := g_WMon19Img.GetCachedImage(1380 + (firedir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
          end;
        end;
      SM_NOWDEATH: if (m_btRace = 62) then begin
          AttackEffectSurface := g_WMon19Img.GetCachedImage(1470 + m_neffectframe - m_nEffectStart, ax, ay);
        end;
    end;

  end;
end;

{ TSkeletonArcherMon }

procedure TSkeletonArcherMon.CalcActorFrame;
begin
  inherited;
  if (m_nCurrentAction = SM_NOWDEATH) and (m_btRace <> 72) then begin
    m_boNowDeath := True;
  end;
end;

procedure TSkeletonArcherMon.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
begin
  inherited;
  if m_boNowDeath and (AttackEffectSurface <> nil) then begin
    dsurface.DrawBlend(dx + n264 + m_nShiftX, dy + n268 + m_nShiftY, AttackEffectSurface, 1);
  end;
end;

procedure TSkeletonArcherMon.LoadSurface;
begin
  inherited;
  if m_boNowDeath then begin
    AttackEffectSurface := g_WMon20Img.GetCachedImage(1600 + m_neffectframe - m_nEffectStart, n264, n268);
  end;
end;

procedure TSkeletonArcherMon.Run;
var
  m_dwFrameTimetime         : LongWord;
begin

  if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
  else m_dwFrameTimetime := m_dwFrameTime;
  if m_nCurrentAction <> 0 then begin
    if (GetTickCount - m_dwStartTime) > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
      end else begin
        m_nCurrentAction := 0;
        m_boNowDeath := False;
      end;
    end;
  end;

  inherited;
end;

{ TFlyingSpider }

procedure TFlyingSpider.CalcActorFrame;
var
  Eff8                      : TNormalDrawEffect;
begin
  inherited;
  if m_nCurrentAction = SM_NOWDEATH then begin
    Eff8 := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMon12Img, 1420, 20, m_dwFrameTime, True);
    if Eff8 <> nil then begin
      Eff8.MagOwner := g_MySelf;
      g_PlayScene.m_EffectList.Add(Eff8);
    end;
  end;
end;

{ TExplosionSpider }

procedure TExplosionSpider.CalcActorFrame;
begin
  inherited;
  case m_nCurrentAction of
    SM_HIT: begin
        m_boUseEffect := False;
      end;
    SM_NOWDEATH: begin
        m_nEffectStart := m_nStartFrame;
        m_neffectframe := m_nStartFrame;
        m_dwEffectstarttime := GetTickCount();
        m_dwEffectframetime := m_dwFrameTime;
        m_nEffectEnd := m_nEndFrame;
        m_boUseEffect := True;
      end;
  end;
end;

procedure TExplosionSpider.LoadSurface;
begin
  inherited;
  if m_boUseEffect then
    AttackEffectSurface := g_WMon14Img.GetCachedImage(
      730 + m_neffectframe - m_nEffectStart,
      ax, ay);
end;

{ TSkeletonKingMon }

procedure TSkeletonKingMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  Actor                     : TActor;
  haircount, scx, scy, stx, sty: Integer;
  meff                      : TCharEffect;
begin
  m_nCurrentFrame := -1;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_BACKSTEP, SM_WALK: begin
        m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_neffectframe := pm.ActWalk.start;
        m_nEffectStart := pm.ActWalk.start;
        m_nEffectEnd := pm.ActWalk.start + pm.ActWalk.frame - 1;
        m_dwEffectstarttime := GetTickCount();
        m_dwEffectframetime := m_dwFrameTime;
        m_boUseEffect := True;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_WALK then
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount();
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_FLYAXE: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount();
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := 80 + pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount();
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
        m_neffectframe := pm.ActStruck.start;
        m_nEffectStart := pm.ActStruck.start;
        m_nEffectEnd := pm.ActStruck.start + pm.ActStruck.frame - 1;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
        m_boUseEffect := True;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        m_neffectframe := pm.ActDie.start;
        m_nEffectStart := pm.ActDie.start;
        m_nEffectEnd := pm.ActDie.start + pm.ActDie.frame - 1;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
        m_boUseEffect := True;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TSkeletonKingMon.LoadSurface;
begin
  inherited;
  if (m_btRace = 63) and m_boUseEffect then begin
    case m_nCurrentAction of
      SM_WALK: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3060 + (m_btDir * 10) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
      SM_HIT: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3140 + (firedir * 10) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
      SM_FLYAXE: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3300 + (firedir * 10) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
      SM_LIGHTING: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3220 + (firedir * 10) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
      SM_STRUCK: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3380 + (m_btDir * 2) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
      SM_NOWDEATH: begin
          AttackEffectSurface := g_WMon20Img.GetCachedImage(
            3400 + (m_btDir * 4) + m_neffectframe - m_nEffectStart,
            ax,
            ay);
        end;
    end;
  end;
end;

procedure TSkeletonKingMon.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
  meff                      : TFlyingFireBall;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  //
  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  if m_boUseEffect then begin
    if m_boMsgMuch then m_dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
        BoUseDieEffect := False;
      end;

      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = 4) then begin
        meff := TFlyingFireBall(g_PlayScene.NewFlyObject(Self,
          m_nCurrX,
          m_nCurrY,
          m_nTargetX,
          m_nTargetY,
          m_nTargetRecog,
          mtFlyBug));
        if meff <> nil then begin
          meff.ImgLib := g_WMon20Img;
          meff.NextFrameTime := 60;
          meff.FlyImageBase := 3573;
        end;
      end;
      m_nCurrentDefFrame := 0;
      m_dwDefFrameTime := GetTickCount;
    end;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

{ TStoneMonster }

procedure TStoneMonster.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_boUseMagic := False;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  m_btDir := 0;
  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := pm.ActStand.start;
        m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
        m_dwFrameTime := pm.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        if not m_boUseEffect then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := 300;
        end;
      end;
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start;
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        if not m_boUseEffect then begin
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nStartFrame + 25;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := 150;
        end;
      end;
    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start;
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        m_nStartFrame := pm.ActDie.start;
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start;
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        m_boNowDeath := True;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nStartFrame + 19;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 80;
      end;
  end;
end;

constructor TStoneMonster.Create;
begin
  inherited;
  n26C := nil;
  m_boUseEffect := False;
  m_boNowDeath := False;
end;

procedure TStoneMonster.DrawEff(dsurface: TCustomCanvas; dx,
  dy: Integer);
begin
  inherited;
  if m_boUseEffect and (n26C <> nil) then begin
    dsurface.DrawBlend(
      dx + ax + m_nShiftX,
      dy + ay + m_nShiftY,
      n26C, 1);
  end;
end;

procedure TStoneMonster.LoadSurface;
begin
  inherited;
  if m_boNowDeath then begin
    case m_btRace of
      75: begin
          AttackEffectSurface := g_WMon22Img.GetCachedImage(
            2530 + m_neffectframe - m_nEffectStart,
            n264, n268);
        end;
      77: begin
          AttackEffectSurface := g_WMon22Img.GetCachedImage(
            2660 + m_neffectframe - m_nEffectStart,
            n264, n268);
        end;
    end;
  end else begin
    if m_boUseEffect then
      case m_btRace of
        75: begin
            case m_nCurrentAction of
              SM_HIT: begin
                  n26C := g_WMon22Img.GetCachedImage(
                    2500 + m_neffectframe - m_nEffectStart,
                    ax, ay);
                end;
              SM_TURN: begin
                  n26C := g_WMon22Img.GetCachedImage(
                    2490 + m_neffectframe - m_nEffectStart,
                    ax, ay);
                end;
            end;
          end;
        77: begin
            case m_nCurrentAction of
              SM_HIT: begin
                  n26C := g_WMon22Img.GetCachedImage(
                    2630 + m_neffectframe - m_nEffectStart,
                    ax, ay);
                end;
              SM_TURN: begin
                  n26C := g_WMon22Img.GetCachedImage(
                    2620 + m_neffectframe - m_nEffectStart,
                    ax, ay);
                end;
            end;
          end;
      end;
  end;
end;

procedure TStoneMonster.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime   : LongWord;
  m_dwFrameTimetime         : LongWord;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;
  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  if m_boUseEffect or m_boNowDeath then begin
    if m_boMsgMuch then m_dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
        m_boNowDeath := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
      end;
      m_nCurrentDefFrame := 0;
      m_dwDefFrameTime := GetTickCount;
    end;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if (prv <> m_nCurrentFrame) or (prv <> m_neffectframe) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

{ TPBOMA6Mon }

procedure TPBOMA6Mon.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
  meff                      : TFlyingAxe;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
      end;
      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = 4) then begin
        meff := TFlyingAxe(g_PlayScene.NewFlyObject(Self,
          m_nCurrX,
          m_nCurrY,
          g_nTargetX,
          g_nTargetY,
          m_nTargetRecog,
          mtFlyBolt));
        if meff <> nil then begin
          meff.ImgLib := g_WMon22Img;
          meff.NextFrameTime := 50;
          meff.FlyImageBase := 1989;
        end;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

{ TDragonStatue }

procedure TDragonStatue.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_btDir := 0;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_DIGUP: begin
        Shift(0, 0, 0, 1);
        m_nStartFrame := 0;
        m_nEndFrame := 0;               //blue
        m_dwFrameTime := 100;
        m_dwStartTime := GetTickCount;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := 0;
        m_nEndFrame := 9;
        m_dwFrameTime := 100;
        m_dwStartTime := GetTickCount;
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 9;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 100;
      end;
  end;
end;

constructor TDragonStatue.Create;
begin
  inherited;
  n26C := nil;
end;

procedure TDragonStatue.DrawEff(dsurface: TCustomCanvas; dx,
  dy: Integer);
begin
  inherited;
  if m_boUseEffect and (EffectSurface <> nil) then begin
    dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, EffectSurface, 1);
  end;
end;

procedure TDragonStatue.LoadSurface;
var
  mimg                      : TWMImages;
begin
  mimg := g_WDragonImg;
  if mimg <> nil then
    m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) {+ m_nCurrentFrame}, m_nPx, m_nPy);
  if m_boUseEffect then begin
    case m_btRace of
      84..86: begin
          EffectSurface := mimg.GetCachedImage(310 + m_neffectframe, ax, ay);
        end;
      87..89: begin
          EffectSurface := mimg.GetCachedImage(330 + m_neffectframe, ax, ay);
        end;
    end;
  end;
end;

procedure TDragonStatue.Run;
var
  prv                       : Integer;
  dwEffectFrameTime, m_dwFrameTimetime: LongWord;
  bofly                     : Boolean;
begin
  m_btDir := 0;
  if (m_nCurrentAction = SM_WALK) or
    (m_nCurrentAction = SM_BACKSTEP) or
    (m_nCurrentAction = SM_RUN) or
    (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  if m_boUseEffect then begin
    if m_boMsgMuch then dwEffectFrameTime := Round(m_dwEffectframetime * 2 / 3)
    else dwEffectFrameTime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > dwEffectFrameTime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
        m_boNowDeath := False;
      end;
      if (m_nCurrentAction = SM_LIGHTING) and (m_nCurrentFrame = 4) then begin
        g_PlayScene.NewMagic(Self, {74} 90, {74} 90, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, 0, {mtThunder} mtExplosion, False, 30, bofly);
        g_SndMgr.PlaySound(8222, m_nCurrX, m_nCurrY);
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

{ TPBOMA1Mon }

procedure TPBOMA1Mon.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
  meff                      : TFlyingBug;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
      end;
      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = 4) then begin
        meff := TFlyingBug(g_PlayScene.NewFlyObject(Self,
          m_nCurrX,
          m_nCurrY,
          m_nTargetX,
          m_nTargetY,
          m_nTargetRecog,
          mtFireBall));
        if meff <> nil then begin
          meff.ImgLib := g_WMon22Img;
          meff.NextFrameTime := 50;
          meff.FlyImageBase := 350;
          meff.MagExplosionBase := 430;
        end;
      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

{ TFireDragon }

procedure TFireDragon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_btDir := 0;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);

        if m_btRace = 120 then begin
          case m_nTempState of
            1: m_nStartFrame := 0;
            2: m_nStartFrame := 80;
            3: m_nStartFrame := 160;
            4: m_nStartFrame := 240;
            5: m_nStartFrame := 320;
          end;
          m_boWarMode := True;
          m_dwFrameTime := 150;
          m_nEndFrame := m_nStartFrame + 19;
          m_dwStartTime := GetTickCount;
          m_nDefFrameCount := 20;
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := 150;
        end;
      end;
    SM_DIGUP: begin
        Shift(0, 0, 0, 1);
        m_nStartFrame := 0;
        m_nEndFrame := 9;
        m_dwFrameTime := 300;
        m_dwStartTime := GetTickCount;
      end;
    SM_LIGHTING, SM_LIGHTING_1..SM_LIGHTING_3: if m_btRace = 120 then begin
        m_nStartFrame := 0;
        m_nEndFrame := 19;
        m_dwFrameTime := 150;

        m_dwStartTime := GetTickCount;

        m_boUseEffect := True;
        m_neffectframe := 0;
        m_nEffectStart := 0;

        m_nEffectEnd := 19;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 150;

        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);

        if m_btRace = 120 then begin
          case m_nTempState of
            1: m_nStartFrame := 20;
            2: m_nStartFrame := 100;
            3: m_nStartFrame := 180;
            4: m_nStartFrame := 260;
            5: m_nStartFrame := 340;
          end;
          m_nEndFrame := m_nStartFrame + 9;
          m_dwFrameTime := 150;
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := 150;
        end;
      end;
    SM_HIT: if m_btRace <> 120 then begin
        m_nStartFrame := 0;
        m_nEndFrame := 19;
        m_dwFrameTime := 150;
        m_dwStartTime := GetTickCount;
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 19;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 150;
        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_STRUCK: if m_btRace <> 120 then begin
        m_nStartFrame := 0;
        m_nEndFrame := 9;
        m_dwFrameTime := 300;
        m_dwStartTime := GetTickCount;
      end;
    81..83: begin
        m_nStartFrame := 0;
        m_nEndFrame := 5;
        m_dwFrameTime := 150;
        m_dwStartTime := GetTickCount;
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 10;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 150;
        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_DEATH: if m_btRace <> 120 then begin
        m_nCurrentFrame := 0;
        m_nStartFrame := 80;
        m_nEndFrame := 81;
        m_boUseEffect := False;
        m_boDelActionAfterFinished := True;
      end;
    SM_NOWDEATH: begin
        if m_btRace = 120 then begin
          m_nStartFrame := pm.ActDie.start;
          m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
          m_dwFrameTime := pm.ActDie.ftime;
          m_dwStartTime := GetTickCount;

          m_boUseEffect := True;
          m_neffectframe := 420;
          m_nEffectStart := 420;
          m_dwFrameTime := 150;
          m_nEndFrame := m_nStartFrame + 17;
          m_dwStartTime := GetTickCount;
          m_boUseEffect := True;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := 150;
        end else begin
          m_nCurrentFrame := 0;
          m_nStartFrame := 80;
          m_nEndFrame := 81;
          m_nCurrentFrame := 0;
          m_boUseEffect := False;
          m_boDelActionAfterFinished := True;
        end;
      end;
  end;
  if m_btRace in [118, 119, 120] then m_boUseEffect := True;
end;

constructor TFireDragon.Create;
begin
  inherited;
  m_DrawEffect := nil;

  LightningTimer := TTimer.Create(nil);
  {if m_btRace = 83 then
    LightningTimer.Interval := 70
  else if m_btRace = 120 then}
  LightningTimer.Interval := 10;
  LightningTimer.Tag := 0;
  LightningTimer.OnTimer := LightningTimerTimer;
  LightningTimer.Enabled := False;
end;

destructor TFireDragon.Destroy;
begin
  if LightningTimer <> nil then
    LightningTimer.Free;
  inherited Destroy;
end;

procedure TFireDragon.LightningTimerTimer(Sender: TObject);
var
  tx, ty, n, kx, ky         : Integer;
  bofly                     : Boolean;
begin
  {if Race = 83 then begin
    if LightningTimer.Tag = 0 then begin
      LightningTimer.Tag := LightningTimer.Tag + 1;
      LightningTimer.Interval := 800;
      exit;
    end
    else LightningTimer.Interval := 70;
    tx := XX - 5;
    ty := YY + 3;

    Randomize;
    if LightningTimer.Tag = 0 then begin
      PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx - 3, ty + 3, 0, mtThunder, FALSE, 30, bofly);
      PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx - 3, ty - 3, 0, mtThunder, FALSE, 30, bofly);
    end;

    n := Random(4);
    kx := Random(7);
    ky := Random(5);
    case n of
      0: PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx + kx - 2, ty - ky + 1, 0, mtThunder, FALSE, 30, bofly);
      1: PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx - kx, ty + ky, 0, mtThunder, FALSE, 30, bofly);
      2: PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx - kx, ty - ky + 1, 0, mtThunder, FALSE, 30, bofly);
      3: PlayScene.NewMagic(self, SM_DRAGON_LIGHTING, SM_DRAGON_LIGHTING, XX, YY, tx + kx - 2, ty + ky, 0, mtThunder, FALSE, 30, bofly);
    end;

    if (LightningTimer.Tag mod 3) = 0 then g_SndMgr.PlaySound(8206);
    LightningTimer.Interval := LightningTimer.Interval + 15;
    LightningTimer.Tag := LightningTimer.Tag + 1;

    if LightningTimer.Tag > 7 then begin
      LightningTimer.Interval := 70;
      LightningTimer.Tag := 0;
      LightningTimer.Enabled := FALSE;
    end;
  end else}
  if m_btRace = 120 then begin
    if LightningTimer.Tag = 0 then begin
      LightningTimer.Tag := LightningTimer.Tag + 1;
      LightningTimer.Interval := 10;
      Exit;
    end;

    tx := g_MySelf.m_nCurrX;
    ty := g_MySelf.m_nCurrY;

    n := Random(4);
    kx := Random(7);
    ky := Random(5);

    if LightningTimer.Tag = 0 then begin
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_1, MAGIC_SOULBALL_ATT3_1, m_nCurrX, m_nCurrY, tx, ty, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_2, MAGIC_SOULBALL_ATT3_2, m_nCurrX, m_nCurrY, tx - 2, ty, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_3, MAGIC_SOULBALL_ATT3_3, m_nCurrX, m_nCurrY, tx, ty - 2, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_4, MAGIC_SOULBALL_ATT3_4, m_nCurrX, m_nCurrY, tx - kx, ty - ky, 0, mtThunder, False, 30, bofly);
      LightningTimer.Interval := 500;
    end
    else if LightningTimer.Tag = 2 then begin
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_1, MAGIC_SOULBALL_ATT3_1, m_nCurrX, m_nCurrY, tx - 2, ty - 2, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_2, MAGIC_SOULBALL_ATT3_2, m_nCurrX, m_nCurrY, tx + 2, ty - 2, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_3, MAGIC_SOULBALL_ATT3_3, m_nCurrX, m_nCurrY, tx + kx, ty, 0, mtThunder, False, 30, bofly);
      g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_4, MAGIC_SOULBALL_ATT3_4, m_nCurrX, m_nCurrY, tx - kx, ty, 0, mtThunder, False, 30, bofly);
    end;

    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_5, MAGIC_SOULBALL_ATT3_5, m_nCurrX, m_nCurrY, tx + kx, ty - ky, 0, mtThunder, False, 30, bofly);
    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_1, MAGIC_SOULBALL_ATT3_1, m_nCurrX, m_nCurrY, tx - kx - 2, ty + ky, 0, mtThunder, False, 30, bofly);
    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_2, MAGIC_SOULBALL_ATT3_2, m_nCurrX, m_nCurrY, tx - kx, ty - ky, 0, mtThunder, False, 30, bofly);
    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_3, MAGIC_SOULBALL_ATT3_3, m_nCurrX, m_nCurrY, tx + kx + 2, ty + ky, 0, mtThunder, False, 30, bofly);
    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_4, MAGIC_SOULBALL_ATT3_4, m_nCurrX, m_nCurrY, tx + kx, ty, 0, mtThunder, False, 30, bofly);
    g_PlayScene.NewMagic(Self, MAGIC_SOULBALL_ATT3_5, MAGIC_SOULBALL_ATT3_5, m_nCurrX, m_nCurrY, tx - kx, ty, 0, mtThunder, False, 30, bofly);

    LightningTimer.Interval := LightningTimer.Interval + 100;
    LightningTimer.Tag := LightningTimer.Tag + 1;

    if LightningTimer.Tag > 7 then begin
      LightningTimer.Interval := 10;
      LightningTimer.Tag := 0;
      LightningTimer.Enabled := False;
    end;
  end;
  {else if Race = 118 then begin         //Çö¹«Çö½Å
    if LightningTimer.Tag = 0 then begin
      LightningTimer.Tag := LightningTimer.Tag + 1;
      LightningTimer.Interval := 10;
      Exit;
    end;
    //       else LightningTimer.Interval := 500;

    tx := Myself.XX;
    ty := Myself.YY;

    //       Randomize;
    n := Random(4);
    kx := Random(7);
    ky := Random(5);

    if LightningTimer.Tag = 0 then begin
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx, ty, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx - 2, ty + 2, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx, ty + 3, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx - kx, ty - ky + 1, 0, mtThunder, False, 30, bofly);
      LightningTimer.Interval := 500;
    end
    else if LightningTimer.Tag = 2 then begin
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx - 2, ty + 3, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx + 2, ty + 2, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx + kx, ty, 0, mtThunder, False, 30, bofly);
      PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx - kx, ty + 1, 0, mtThunder, False, 30, bofly);
    end;

    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx + kx, ty - ky + 1, 0, mtThunder, False, 30, bofly);
    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx - kx - 2, ty + ky + 2, 0, mtThunder, False, 30, bofly);
    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx - kx, ty - ky + 3, 0, mtThunder, False, 30, bofly);
    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx + kx + 2, ty + ky + 1, 0, mtThunder, False, 30, bofly);
    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_2, MAGIC_KINGTURTLE_ATT2_2, XX, YY, tx + kx, ty + 2, 0, mtThunder, False, 30, bofly);
    PlayScene.NewMagic(Self, MAGIC_KINGTURTLE_ATT2_1, MAGIC_KINGTURTLE_ATT2_1, XX, YY, tx - kx, ty, 0, mtThunder, False, 30, bofly);

    if LightningTimer.Tag = 4 then g_SndMgr.PlaySound(10450);
    LightningTimer.Interval := LightningTimer.Interval + 200;
    LightningTimer.Tag := LightningTimer.Tag + 1;

    if LightningTimer.Tag > 7 then begin
      LightningTimer.Interval := 10;
      LightningTimer.Tag := 0;
      LightningTimer.Enabled := False;
    end;
  end;}
end;

procedure TFireDragon.AttackEff;
var
  n8, nc, n10, n14, n18     : Integer;
  bofly                     : Boolean;
  i, iCount                 : Integer;
begin
  if m_boDeath then Exit;
  n8 := m_nCurrX;
  nc := m_nCurrY;
  iCount := Random(4);
  for i := 0 to iCount do begin
    n10 := Random(4);
    n14 := Random(8);
    n18 := Random(8);
    case n10 of
      0: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      1: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18, 0, mtRedThunder, False, 30, bofly);
      2: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      3: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18, 0, mtRedThunder, False, 30, bofly);
    end;
    //PlaySound(8206);
    g_SndMgr.PlaySound(8301, m_nCurrX, m_nCurrY);
  end;
end;

procedure TFireDragon.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
begin
  if (m_btRace <> 120) and m_boDeath then Exit;
  inherited;
  if m_boUseEffect and (m_DrawEffect <> nil) then
    dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, m_DrawEffect, 1);
end;

procedure TFireDragon.LoadSurface;
var
  mimg                      : TWMImages;
begin
  mimg := g_WDragonImg;
  if m_btRace in [120] then begin
    m_boUseEffect := True;
    if m_boDeath then
      m_boUseEffect := False;
  end else begin
    if m_boDeath then begin
      m_BodySurface := nil;
      Exit;
    end;
    if mimg = nil then Exit;
  end;

  if (not m_boReverseFrame) then begin
    if m_btRace = 120 then
      m_BodySurface := g_WMon33Img.GetCachedImage(2900 + m_nCurrentFrame, m_nPx, m_nPy)
    else begin
      case m_nCurrentAction of
        SM_HIT: m_BodySurface := mimg.GetCachedImage(40 + m_nCurrentFrame, m_nPx, m_nPy);
        81: m_BodySurface := mimg.GetCachedImage(10 + m_nCurrentFrame, m_nPx, m_nPy);
        82: m_BodySurface := mimg.GetCachedImage(20 + m_nCurrentFrame, m_nPx, m_nPy);
        83: m_BodySurface := mimg.GetCachedImage(30 + m_nCurrentFrame, m_nPx, m_nPy);
      else
        m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nCurrentFrame, m_nPx, m_nPy);
      end;
    end;
  end else begin
    if m_btRace = 120 then
      m_BodySurface := g_WMon33Img.GetCachedImage(2900 + m_nCurrentFrame, m_nPx, m_nPy) //???
    else begin
      case m_nCurrentAction of
        SM_HIT: m_BodySurface := mimg.GetCachedImage(40 + m_nEndFrame - m_nCurrentFrame, ax, ay);
        81: m_BodySurface := mimg.GetCachedImage(10 + m_nEndFrame - m_nCurrentFrame, ax, ay);
        82: m_BodySurface := mimg.GetCachedImage(20 + m_nEndFrame - m_nCurrentFrame, ax, ay);
        83: m_BodySurface := mimg.GetCachedImage(30 + m_nEndFrame - m_nCurrentFrame, ax, ay);
      else
        m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nEndFrame - m_nCurrentFrame, m_nPx, m_nPy);
      end;
    end;
  end;
  if m_boUseEffect then begin
    if m_btRace = 120 then begin
      if ((2900 + m_nCurrentFrame + 20) > 2900 + 419) and ((2900 + m_nCurrentFrame + 20) < 2900 + 438) then begin
        m_DrawEffect := g_WMon33Img.GetCachedImage(2900 + m_nCurrentFrame + 20, ax, ay);
      end else
        m_DrawEffect := g_WMon33Img.GetCachedImage(2900 + m_nCurrentFrame + 40, ax, ay);
    end else begin
      case m_nCurrentAction of
        SM_HIT: m_DrawEffect := mimg.GetCachedImage(60 + m_neffectframe, ax, ay);
        81: m_DrawEffect := mimg.GetCachedImage(90 + m_neffectframe, ax, ay);
        82: m_DrawEffect := mimg.GetCachedImage(100 + m_neffectframe, ax, ay);
        83: m_DrawEffect := mimg.GetCachedImage(110 + m_neffectframe, ax, ay);
      end;
    end;
  end;
end;

procedure TFireDragon.Run;
var
  prv                       : Integer;
  m_dwEffectFrameTimetime, m_dwFrameTimetime: LongWord;
  bofly                     : Boolean;
begin
  if (m_btRace <> 120) and m_boDeath then begin
    m_BodySurface := nil;
    Exit;
  end;
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then m_boMsgMuch := True;
  if m_boRunSound then begin
    g_SndMgr.PlaySound(8201, m_nCurrX, m_nCurrY);
    m_boRunSound := False;
  end;

  if m_boUseEffect then begin
    if m_boMsgMuch then
      m_dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else
      m_dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        if m_btRace in [118, 119, 120] then begin
          if m_boDeath then
            m_boUseEffect := False
          else
            m_boUseEffect := True;
        end else
          m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then
      m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else
      m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
        m_boNowDeath := False;
      end;

      if (m_nCurrentAction = SM_HIT) then begin //and (m_nCurrentFrame = 4) then begin
        AttackEff;
        g_SndMgr.PlaySound(8202, m_nCurrX, m_nCurrY);
      end else if m_btRace = 120 then begin
        if (m_nCurrentAction = SM_LIGHTING) and (m_nCurrentFrame - m_nStartFrame = 1) then begin
          g_PlayScene.NewMagic(Self,
            MAGIC_SOULBALL_ATT1,
            MAGIC_SOULBALL_ATT1,
            m_nCurrX,
            m_nCurrY,
            m_nCurrX,
            m_nCurrY,
            m_nRecogId,                 //TargetRecog,
            mtGroundEffect,
            False,
            30,
            bofly);
          g_SndMgr.PlaySound(3476, m_nCurrX, m_nCurrY);
        end
        else if (m_nCurrentAction = SM_LIGHTING_1) and (m_nCurrentFrame - m_nStartFrame = 1) then begin
          g_PlayScene.NewMagic(Self,
            MAGIC_SOULBALL_ATT2,
            MAGIC_SOULBALL_ATT2,
            m_nCurrX,
            m_nCurrY,
            m_nTargetX,
            m_nTargetY,
            m_nTargetRecog,
            mtThunder,
            False,
            30,
            bofly);
          g_SndMgr.PlaySound(3477, m_nCurrX, m_nCurrY);
        end
        else if (m_nCurrentAction = SM_LIGHTING_2) and (m_nCurrentFrame - m_nStartFrame = 1) then begin
          if not LightningTimer.Enabled then begin
            LightningTimer.Enabled := True;
            g_SndMgr.PlaySound(3478, m_nCurrX, m_nCurrY);
          end;
        end
      end else if (m_nCurrentAction = 81) or (m_nCurrentAction = 82) or (m_nCurrentAction = 83) then begin
        if (m_nCurrentFrame - m_nStartFrame) = 4 then begin
          g_PlayScene.NewMagic(Self, m_nCurrentAction, m_nCurrentAction, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtFly, True, 30, bofly);
          g_SndMgr.PlaySound(8203, m_nCurrX, m_nCurrY);
        end;
      end;

    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if m_btRace = 120 then begin
      if GetTickCount - m_dwDefFrameTime > 150 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end else if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

constructor TKhazardMon.Create;
begin
  inherited;
end;

procedure TKhazardMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
  else begin
      inherited;
    end;
  end;
end;

constructor TFrostTiger.Create;
begin
  inherited;
  boActive := False;
  boCasted := False;
end;

procedure TFrostTiger.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        boCasted := True;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_DIGDOWN: begin
        m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        boActive := False;
      end;
    SM_DIGUP: boActive := True;
    SM_WALK: begin
        boActive := True;
        inherited;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TFrostTiger.Run;
var
  bofly                     : Boolean;
begin
  if (m_nCurrentAction = SM_LIGHTING) and (boCasted = True) then begin
    boCasted := False;
    g_PlayScene.NewMagic(Self, 1, 39, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtFly, False, 30, bofly);
    g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY);
  end;
  inherited;
end;

function TFrostTiger.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //Jacky
  if boActive = False then begin
    pm := GetRaceByPM(m_btRace, m_wAppearance);
    if pm = nil then Exit;
    if m_boDeath then begin
      inherited GetDefaultFrame(wmode);
      Exit;
    end;
    m_nDefFrameCount := pm.ActDeath.frame;
    if m_nCurrentDefFrame < 0 then cf := 0
    else if m_nCurrentDefFrame >= pm.ActDeath.frame then cf := 0
    else cf := m_nCurrentDefFrame;
    Result := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip) + cf;
  end else
    Result := inherited GetDefaultFrame(wmode);
end;

constructor TRedThunderZuma.Create;
begin
  inherited;
  boCasted := False;
end;

procedure TRedThunderZuma.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        if (m_nState and STATE_STONE_MODE) <> 0 then begin
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
          m_nEndFrame := m_nStartFrame;
          m_dwFrameTime := pm.ActDeath.ftime;
          m_dwStartTime := GetTickCount;
          m_nDefFrameCount := pm.ActDeath.frame;
        end else
          inherited;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        firedir := m_btDir;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 6;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 150;
        m_nCurEffFrame := 0;
        boCasted := True;
      end;
    SM_DIGUP: begin
        m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TRedThunderZuma.Run;
var
  bofly                     : Boolean;
begin
  if (m_nCurrentFrame - m_nStartFrame) = 2 then begin
    if (m_nCurrentAction = SM_LIGHTING) then begin
      if boCasted = True then begin
        boCasted := False;
        g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtRedThunder, False, 30, bofly);
        g_SndMgr.PlaySound(8301, m_nCurrX, m_nCurrY);
      end;
    end;
  end;
  inherited;
end;

function TRedThunderZuma.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //Jacky
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  {
  if boActive = FALSE then begin

    if pm = nil then exit;
    if m_boDeath then begin
      inherited GetDefaultFrame(wmode);
      exit;
    end;
    m_nDefFrameCount := 1;
    if m_nCurrentDefFrame < 0 then cf := 0
    else if m_nCurrentDefFrame >= 0 then cf := 0
    else cf := m_nCurrentDefFrame;
    Result := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip) + cf;
    }

  if (m_nState and STATE_STONE_MODE) <> 0 then begin
    Result := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
  end else begin
    Result := inherited GetDefaultFrame(wmode);
  end;
end;

procedure TRedThunderZuma.LoadSurface;
begin
  inherited;

  if (m_nState and STATE_STONE_MODE) <> 0 then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        AttackEffectSurface := g_WMon23Img.GetCachedImage(
          1200 + (m_btDir * 10) + m_neffectframe - m_nEffectStart,
          ax, ay);
      end;
    SM_WALK: begin
        m_boUseEffect := True;
        AttackEffectSurface := g_WMon23Img.GetCachedImage(
          1020 + (m_btDir * 10) + m_neffectframe - m_nEffectStart,
          ax, ay);
      end;
    SM_HIT: begin
        m_boUseEffect := True;
        AttackEffectSurface := g_WMon23Img.GetCachedImage(
          1100 + (firedir * 10) + m_neffectframe - m_nEffectStart,
          ax, ay);
      end;
  else begin
      m_boUseEffect := True;
      m_nEffectStart := 0;
      m_neffectframe := 0;
      m_nEffectEnd := 4;
      m_dwEffectstarttime := GetTickCount;
      m_dwEffectframetime := 150;
      AttackEffectSurface := g_WMon23Img.GetCachedImage(
        940 + (m_btDir * 10) + m_nCurrentDefFrame,
        ax, ay);
    end;
  end;
end;

procedure TCrystalSpider.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  //DScreen.AddChatBoardString(IntToStr(m_nCurrentAction), clYellow, clRed);
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 10;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 50;
        m_nCurEffFrame := 0;
      end;
    SM_HIT: begin
        inherited;
        m_boUseEffect := False;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TCrystalSpider.LoadSurface;
begin
  inherited;
  if m_boUseEffect and (m_nCurrentAction = SM_LIGHTING) then
    AttackEffectSurface := g_WMon23Img.GetCachedImage(2230 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
  {case m_nCurrentAction of
    SM_LIGHTING: begin
        AttackEffectSurface := g_WMon23Img.GetCachedImage(2230 + (m_btDir * 10) + m_nEffectFrame - m_nEffectStart, ax, ay);
      end;
  end;}
end;

procedure TYimoogi.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_FLYAXE: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 6;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := pm.ActCritical.ftime;
        m_nCurEffFrame := 0;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 2;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := pm.ActAttack.ftime;
        m_nCurEffFrame := 0;
      end;
    SM_HIT: begin
        inherited;
        m_boUseEffect := False;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start;
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        m_boUseEffect := True;
        m_neffectframe := 0;
        m_nEffectStart := 0;
        m_nEffectEnd := 10;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 100;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TYimoogi.LoadSurface;
var
  bofly                     : Boolean;
  meff                      : TMagicEff;
begin
  inherited;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        AttackEffectSurface := nil;
        if (m_neffectframe = 1) then begin
          meff := TObjectEffects.Create(Self, g_WMagic2Images, 1330, 10, 100, True);
          meff.ImgLib := g_WMagic2Images;
          g_PlayScene.m_EffectList.Add(meff);
        end;
      end;
    SM_FLYAXE: begin
        AttackEffectSurface := g_WMon23Img.GetCachedImage(
          2820 + (m_btDir * 10) + m_neffectframe - m_nEffectStart,
          ax, ay);
        if (m_neffectframe = 3) then begin
          g_PlayScene.NewMagic(Self, 93, 93, m_nCurrX, m_nCurrY, m_nCurrX, m_nCurrY, 0, mtExplosion, False, 30, bofly);
          //PlaySound(8222);
        end;
      end;
    SM_NOWDEATH: begin
        AttackEffectSurface := g_WMon23Img.GetCachedImage(
          2900 + m_neffectframe - m_nEffectStart,
          ax,
          ay);
      end;
  end;
end;

procedure TBlackFox.CalcActorFrame;
var
  pm                        : pTMonsterAction;
begin
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 10;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 50;
        m_nCurEffFrame := 0;
      end;
    SM_HIT: begin
        inherited;
        m_boUseEffect := False;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TBlackFox.LoadSurface;
begin
  inherited;
  if (m_btRace = 109) and m_boUseEffect and (m_nCurrentAction = SM_LIGHTING) then begin
    AttackEffectSurface := g_WMon24Img.GetCachedImage(
      352 + (m_btDir * 10) + m_neffectframe - m_nEffectStart,
      ax, ay);
  end;
end;

procedure TGreenCrystalSpider.CalcActorFrame;
var
  scx, scy, stx, sty        : Integer;
  pm                        : pTMonsterAction;
  Actor                     : TActor;
begin
  inherited;
  Exit;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_neffectframe := 0;
        m_nEffectEnd := 10;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := 50;
        m_nCurEffFrame := 0;
      end;
    SM_HIT: begin
        inherited;
        m_boUseEffect := False;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TGreenCrystalSpider.LoadSurface;
begin
  inherited;
  if (m_btRace = 110) and m_boUseEffect and (m_nCurrentAction = SM_LIGHTING) then begin
    AttackEffectSurface := g_WMon24Img.GetCachedImage(1100 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
  end;
end;

constructor TSnowMon.Create;
begin
  inherited Create;
  AttackEffectSurface := nil;
  AttackEffectSurface2 := nil;
  DieEffectSurface := nil;
  ChrEffect := nil;
  m_boUseEffect := False;
  BoUseDieEffect := False;
  m_bowChrEffect := False;
end;

procedure TSnowMon.SetSound;
var
  cx, cy, bidx, wunit, attackweapon: Integer;
  hiter                     : TActor;
begin
  inherited;
  if m_boUseMagic and (m_CurMagic.MagicSerial > 0) then begin
    m_nMagicstartsound := 10000 + m_CurMagic.MagicSerial * 10;
    m_nMagicFireSound := m_nMagicstartsound + 1;
    m_nMagicexplosionsound := m_nMagicstartsound + 2;
  end;
end;

procedure TSnowMon.RunSound;
begin
  m_boRunSound := True;
  SetSound;
  case m_nCurrentAction of
    SM_STRUCK: begin
        if (m_nStruckWeaponSound >= 0) then g_SndMgr.PlaySound(m_nStruckWeaponSound, m_nCurrX, m_nCurrY);
        if (m_nStruckSound >= 0) then g_SndMgr.PlaySound(m_nStruckSound, m_nCurrX, m_nCurrY);
        if (m_nScreamSound >= 0) then g_SndMgr.PlaySound(m_nScreamSound, m_nCurrX, m_nCurrY);
      end;
    SM_NOWDEATH: if (m_nDieSound >= 0) then begin
        g_SndMgr.PlaySound(m_nDieSound, m_nCurrX, m_nCurrY);
      end;
    SM_THROW, SM_HIT, SM_FLYAXE, SM_LIGHTING, SM_DIGDOWN: begin
        case m_wAppearance of
          250, 251, 255, 256: begin
              if m_nCurrentAction = SM_LIGHTING then begin
                if m_nDie2Sound >= 0 then
                  g_SndMgr.PlaySound(m_nDie2Sound, m_nCurrX, m_nCurrY);
              end else if m_nAttackSound >= 0 then
                g_SndMgr.PlaySound(m_nAttackSound, m_nCurrX, m_nCurrY);
            end;

        else begin
            if m_nAttackSound >= 0 then g_SndMgr.PlaySound(m_nAttackSound, m_nCurrX, m_nCurrY);
          end;
        end;
      end;
    SM_ALIVE, SM_DIGUP: begin
        g_SndMgr.SilenceSound;
        g_SndMgr.PlaySound(m_nAppearSound, m_nCurrX, m_nCurrY);
      end;
    SM_SPELL: g_SndMgr.PlaySound(m_nMagicstartsound, m_nCurrX, m_nCurrY);
  end;
end;

procedure TSnowMon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  Actor                     : TActor;
  haircount, scx, scy, stx, sty: Integer;
  meff                      : TCharEffect;
begin
  m_nCurrentFrame := -1;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
        m_dwFrameTime := pm.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_WALK: begin
        m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_WALK then
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else                            //sm_backstep
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_HIT: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;

        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);

        m_boUseEffect := True;
        firedir := m_btDir;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        if m_btRace = 20 then
          m_nEffectEnd := m_nEndFrame + 1
        else
          m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;

        {Actor := g_PlayScene.FindActor(m_nTargetRecog);
        if Actor <> nil then begin
          g_PlayScene.ScreenXYfromMCXY(m_nCurrX, m_nCurrY, scx, scy);
          g_PlayScene.ScreenXYfromMCXY(Actor.m_nCurrX, Actor.m_nCurrY, stx, sty);
          fire16dir := GetFlyDirection16(scx, scy, stx, sty);
        end else
          fire16dir := firedir * 2;}

        m_boUseEffect := False;
        if m_btRace = 51 then
          m_boUseEffect := True;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;

        //DScreen.AddChatBoardString(IntToStr(m_nMagicNum), clWhite, clBlack);
        if (m_nMagicNum = 2) and (m_btRace in [38, 39, 46]) then begin
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
          m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
          m_dwFrameTime := pm.ActDeath.ftime;
        end;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);

        m_boUseEffect := True;
        m_neffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_SPELL: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;

        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_nMagLight := 2;
        m_nSpellFrame := pm.ActCritical.frame;
        m_dwWaitMagicRequest := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;

    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime; //pm.ActStruck.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;   //
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        if (m_btRace in [40, 65..69]) then
          BoUseDieEffect := True;
        if m_btRace in [{38, 39,}51] then BoUseDieEffect := True;
      end;
    SM_SKELETON: begin
        m_nStartFrame := pm.ActDeath.start;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        if m_btRace = 39 then begin
          m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
          m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
          m_dwFrameTime := pm.ActDeath.ftime;
          m_dwStartTime := GetTickCount;
          m_dwWarModeTime := GetTickCount;
          Shift(m_btDir, 0, 0, 1);
          m_boUseEffect := True;
          m_neffectframe := m_nStartFrame;
          m_nEffectStart := m_nStartFrame;
          m_nEffectEnd := m_nEndFrame;
          m_dwEffectstarttime := GetTickCount;
          m_dwEffectframetime := m_dwFrameTime;
        end;
      end;
  end;
end;

function TSnowMon.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //jacky
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  if m_boDeath then begin
    if m_boSkeleton then
      Result := pm.ActDeath.start
    else
      Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then
      cf := 0
    else if m_nCurrentDefFrame >= pm.ActStand.frame then
      cf := 0
    else
      cf := m_nCurrentDefFrame;
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
  end;
end;

procedure TSnowMon.LoadSurface;
var
  nImgBase                  : Integer;
begin
  ChrEffect := nil;
  inherited LoadSurface;
  case m_btRace of
    27: if m_boUseEffect then begin
        AttackEffectSurface := g_WMon26Img.GetCachedImage(420 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      end;

    28: if m_boUseEffect then begin
        AttackEffectSurface := g_WMon26Img.GetCachedImage(930 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
      end;

    38: begin
        if m_boUseEffect then begin
          if m_nMagicNum = 2 then
            AttackEffectSurface := g_WMon26Img.GetCachedImage(2650 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay)
          else
            AttackEffectSurface := g_WMon26Img.GetCachedImage(2570 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
        end;
      end;

    39: begin
        ///ddddddddddddddddddddddddddddd
        if m_wAppearance = 267 then begin
          if m_boUseEffect then begin
            ChrEffect := g_WMon27Img.GetCachedImage(3040 + m_neffectframe, ax, ay);
          end else if not m_boReverseFrame then begin
            ChrEffect := g_WMon27Img.GetCachedImage(3040 + m_nCurrentFrame, ax, ay);
          end else begin
            ChrEffect := g_WMon27Img.GetCachedImage(3040 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
          end;

          if m_boUseEffect then begin
            AttackEffectSurface := g_WMon27Img.GetCachedImage(3470 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
            AttackEffectSurface2 := g_WMon27Img.GetCachedImage(3550 + m_neffectframe - m_nEffectStart, bx, by);
          end;
        end else if m_wAppearance = 268 then begin
          if m_boUseEffect then begin
            ChrEffect := g_WMon27Img.GetCachedImage(4070 + m_neffectframe, ax, ay);
          end else if not m_boReverseFrame then begin
            ChrEffect := g_WMon27Img.GetCachedImage(4070 + m_nCurrentFrame, ax, ay);
          end else begin
            ChrEffect := g_WMon27Img.GetCachedImage(4070 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
          end;

          if m_boUseEffect then begin
            AttackEffectSurface := g_WMon27Img.GetCachedImage(4500 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
            AttackEffectSurface2 := g_WMon27Img.GetCachedImage(4580 + m_neffectframe - m_nEffectStart, bx, by);
          end;
        end else begin
          if m_boUseEffect then begin
            ChrEffect := g_WMon26Img.GetCachedImage(3240 + m_neffectframe, ax, ay);
          end else if not m_boReverseFrame then begin
            ChrEffect := g_WMon26Img.GetCachedImage(3240 + m_nCurrentFrame, ax, ay);
          end else begin
            ChrEffect := g_WMon26Img.GetCachedImage(3240 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
          end;

          if m_boUseEffect then begin
            AttackEffectSurface := g_WMon26Img.GetCachedImage(3670 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
            AttackEffectSurface2 := g_WMon26Img.GetCachedImage(3750 + m_neffectframe - m_nEffectStart, bx, by);
          end;
        end;
      end;

    46: if m_boUseEffect and (m_nMagicNum = 2) then begin
        AttackEffectSurface := g_WMon27Img.GetCachedImage(1180 + m_neffectframe - m_nEffectStart, ax, ay);
      end;

    51: begin
        case m_nCurrentAction of
          SM_HIT: nImgBase := 1610;
          SM_LIGHTING: nImgBase := 1690;
        end;
        if m_boUseEffect then AttackEffectSurface := g_WMon27Img.GetCachedImage(nImgBase + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
        if BoUseDieEffect then DieEffectSurface := g_WMon27Img.GetCachedImage(1770 + (m_btDir * 10) + m_nCurrentFrame - m_nStartFrame, bx, by);
      end;
  end;
end;

procedure TSnowMon.Run;
var
  dwEffectFrameTimetime, dwFrameTimetime: LongWord;
begin
  inherited;
  if m_boUseEffect then begin
    if m_boMsgMuch then
      dwEffectFrameTimetime := Round(m_dwEffectframetime * 2 / 3)
    else
      dwEffectFrameTimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > dwEffectFrameTimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_neffectframe < m_nEffectEnd then begin
        Inc(m_neffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;
end;

procedure TSnowMon.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
begin
  inherited;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
  //if m_sUserName = '' then Exit;  //1015
  if ChrEffect <> nil then
    dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, ChrEffect, 1);
end;

procedure TSnowMon.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
begin
  if m_boUseEffect then begin
    if AttackEffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        AttackEffectSurface, 1);
    end;
    if AttackEffectSurface2 <> nil then begin
      dsurface.DrawBlend(
        dx + bx + m_nShiftX,
        dy + by + m_nShiftY,
        AttackEffectSurface2, 1);
    end;
  end;

  if BoUseDieEffect then
    if DieEffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + bx + m_nShiftX,
        dy + by + m_nShiftY,
        DieEffectSurface, 1);
    end;
end;

procedure TSpiderKing.CalcActorFrame;
var
  scx, scy, stx, sty        : Integer;
  pm                        : pTMonsterAction;
  Actor                     : TActor;
begin
  inherited;
  Exit;
  {m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  case m_nCurrentAction of
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_boUseEffect := True;
        m_nEffectStart := 0;
        m_nEffectFrame := 0;
        m_nEffectEnd := 10;
        m_dwEffectStartTime := GetTickCount;
        m_dwEffectFrameTime := 50;
        m_nCurEffFrame := 0;
      end;
    SM_HIT: begin
        inherited;
        m_boUseEffect := False;
      end;
  else begin
      inherited;
    end;
  end;}
end;

procedure TSpiderKing.LoadSurface;
begin
  inherited;
  if (m_btRace = 110) and m_boUseEffect and (m_nCurrentAction = SM_FLYAXE) then begin
    AttackEffectSurface := g_WMon24Img.GetCachedImage(1100 + (m_btDir * 10) + m_neffectframe - m_nEffectStart, ax, ay);
  end;
end;

constructor TAngel.Create;
begin
  inherited Create;
  EffectSurface := nil;
  m_boUseEffect := False;
end;

procedure TAngel.SetSound;
var
  cx, cy, bidx, wunit, attackweapon: Integer;
  hiter                     : TActor;
begin
  inherited;
  if m_boUseMagic and (m_CurMagic.MagicSerial > 0) then begin
    m_nMagicstartsound := 10000 + m_CurMagic.MagicSerial * 10;
    m_nMagicFireSound := m_nMagicstartsound + 1;
    m_nMagicexplosionsound := m_nMagicstartsound + 2;
  end;
end;

procedure TAngel.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_nCurrentFrame := -1;
  m_boReverseFrame := False;
  m_boUseEffect := False;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
        m_dwFrameTime := pm.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := pm.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_WALK: begin
        m_nStartFrame := pm.ActWalk.start + m_btDir * (pm.ActWalk.frame + pm.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + pm.ActWalk.frame - 1;
        m_dwFrameTime := pm.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := pm.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
      end;
    SM_DIGUP: begin
        if (m_wAppearance = 330) or (m_wAppearance = 336) then begin
          m_nStartFrame := 4;
          m_nEndFrame := m_nStartFrame + 6 - 1;
          m_dwFrameTime := 120;
          m_dwStartTime := GetTickCount;
          Shift(m_btDir, 0, 0, 1);
        end else begin
          m_nStartFrame := pm.ActDeath.start;
          m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
          m_dwFrameTime := pm.ActDeath.ftime;
          m_dwStartTime := GetTickCount;
          Shift(m_btDir, 0, 0, 1);
        end;
      end;
    SM_SPELL: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;

        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_nMagLight := 2;
        m_nSpellFrame := pm.ActCritical.frame;
        m_dwWaitMagicRequest := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_HIT, SM_FLYAXE, SM_LIGHTING: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        m_boUseEffect := True;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_STRUCK: begin
        m_nStartFrame := pm.ActStruck.start + m_btDir * (pm.ActStruck.frame + pm.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + pm.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime;
        m_dwStartTime := GetTickCount;
      end;
    SM_DEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDie.frame - 1;
        m_dwFrameTime := pm.ActDie.ftime;
        m_dwStartTime := GetTickCount;
        if m_btRace <> 22 then
          m_boUseEffect := True;
      end;
    SM_ALIVE: begin
        m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

function TAngel.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  //inherited GetDefaultFrame(wmode);
  Result := 0;
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  if m_boDeath then begin
    Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then
      cf := 0
    else if m_nCurrentDefFrame >= pm.ActStand.frame then
      cf := 0
    else
      cf := m_nCurrentDefFrame;
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
  end;
end;

procedure TAngel.LoadSurface;
var
  nBody, nEffect, nDigUp, nBodyOffset: Integer;
begin
  //inherited LoadSurface;
  if m_wAppearance = 330 then begin
    if m_boUseEffect then begin
      m_BodySurface := g_WMon34Img.GetCachedImage(20 + m_neffectframe, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(360 + m_neffectframe, ax, ay);
    end else if not m_boReverseFrame then begin
      m_BodySurface := g_WMon34Img.GetCachedImage(20 + m_nCurrentFrame, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(360 + m_nCurrentFrame, ax, ay);
    end else begin
      m_BodySurface := g_WMon34Img.GetCachedImage(20 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(360 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
    end;

    if m_nCurrentAction = SM_DIGUP then begin
      //g_Screen.AddChatBoardString(Format('%d', [m_nCurrentFrame]), GetRGB(5), clWhite);
      m_BodySurface := g_WMon34Img.GetCachedImage(m_nCurrentFrame, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(10 + m_nCurrentFrame, ax, ay);
      m_boUseEffect := True;
    end else
      m_boUseEffect := False;
    Exit;
  end;
  if m_wAppearance = 336 then begin
    nBodyOffset := GetOffset(m_wAppearance); //1840
    nBody := nBodyOffset;
    nDigUp := nBodyOffset - 10;
    nEffect := 360 + nBodyOffset - 20;
    if m_boUseEffect then begin
      m_BodySurface := g_WMon34Img.GetCachedImage(nBody + m_nEffectframe, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(nEffect + m_nEffectframe, ax, ay);
    end else if not m_boReverseFrame then begin
      m_BodySurface := g_WMon34Img.GetCachedImage(nBody + m_nCurrentFrame, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(nEffect + m_nCurrentFrame, ax, ay);
    end else begin
      m_BodySurface := g_WMon34Img.GetCachedImage(nBody + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(nEffect + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
    end;

    if m_nCurrentAction = SM_DIGUP then begin
      //g_Screen.AddChatBoardString(Format('%d', [FCurrentFrame]), GetRGB(5), clWhite);
      m_BodySurface := g_WMon34Img.GetCachedImage(m_nCurrentFrame, m_nPx, m_nPy);
      EffectSurface := g_WMon34Img.GetCachedImage(nDigUp + m_nCurrentFrame, ax, ay);
      m_boUseEffect := True;
    end else
      m_boUseEffect := False;
    Exit;
  end;

  if m_boUseEffect then begin
    m_BodySurface := g_WMon18Img.GetCachedImage(1280 + m_neffectframe, m_nPx, m_nPy);
    EffectSurface := g_WMon18Img.GetCachedImage(920 + m_neffectframe, ax, ay);
  end else if not m_boReverseFrame then begin
    m_BodySurface := g_WMon18Img.GetCachedImage(1280 + m_nCurrentFrame, m_nPx, m_nPy);
    EffectSurface := g_WMon18Img.GetCachedImage(920 + m_nCurrentFrame, ax, ay);
  end else begin
    m_BodySurface := g_WMon18Img.GetCachedImage(1280 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
    EffectSurface := g_WMon18Img.GetCachedImage(920 + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
  end;
  if m_nCurrentAction = SM_DIGUP then begin
    m_BodySurface := nil;
    EffectSurface := g_WMon18Img.GetCachedImage(920 + m_nCurrentFrame, ax, ay);
    m_boUseEffect := True;
  end else
    m_boUseEffect := False;
end;

procedure TAngel.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  wimg                      : TWMImages;
  bWin                      : Boolean;
  ShiftX, ShiftY            : Integer;
  ceff                      : TColorEffect;
  ax2, ay2                  : Integer;
begin
  if not (m_btDir in [0..7]) then Exit;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
  ShiftX := dx + m_nShiftX;
  ShiftY := dy + m_nShiftY;
  ceff := GetDrawEffectValue;
  if (m_wAppearance = 330) or (m_wAppearance = 336) then begin
    bWin := (m_btDir in [0..2, 6, 7]);
    if EffectSurface <> nil then begin
      if not bWin then
        dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, EffectSurface, 1);
    end;

    if m_BodySurface <> nil then
      DrawEffSurface(dsurface, m_BodySurface, ShiftX + m_nPx, ShiftY + m_nPy, blend, ceff);

    if EffectSurface <> nil then begin
      if bWin then
        dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, EffectSurface, 1);
    end;
  end else begin
    if EffectSurface <> nil then begin
      dsurface.DrawBlend(dx + ax + m_nShiftX, dy + ay + m_nShiftY, EffectSurface, 1);
    end;
    if m_BodySurface <> nil then
      DrawEffSurface(dsurface, m_BodySurface, ShiftX + m_nPx, ShiftY + m_nPy, blend, ceff);
  end;

  if m_boUseMagic and (m_CurMagic.EffectNumber > 0) then begin //sm_spell
    if m_nCurEffFrame in [0..m_nSpellFrame - 1] then begin
      wimg := nil;
      GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, idx);
      if wimg <> nil then begin
        idx := idx + m_nCurEffFrame;
        d := wimg.GetCachedImage(idx, ax2, ay2);
        if d <> nil then
          dsurface.DrawBlend(ShiftX + ax2, ShiftY + ay2, d, 1);
      end;
    end;
  end;

  //ÏÔÊ¾¹¥»÷Ð§¹û
  if m_boHitEffect then begin
    if (m_nHitEffectNumber > 0) then begin
      GetEffectBase(m_nHitEffectNumber - 1, 1, wimg, idx);
      if wimg <> nil then begin
        idx := idx + m_btDir * 10 + (m_nCurrentFrame - m_nStartFrame);
        d := wimg.GetCachedImage(idx, ax2, ay2);
        if d <> nil then
          dsurface.DrawBlend(ShiftX + ax2, ShiftY + ay2, d, 1);
      end;
    end;
  end;

end;

{TTiger}

constructor TTiger.Create;
begin
  inherited Create;
  EffectSurface := nil;
  m_boUseEffect := False;
end;

procedure TTiger.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_nCurrentFrame := -1;
  m_boReverseFrame := False;
  m_boUseEffect := False;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);

  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_HIT: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);

        m_boUseEffect := True;
        m_dwEffectstarttime := GetTickCount;

        firedir := m_btDir;
        m_nEffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
    SM_LIGHTING: begin
        m_nStartFrame := pm.ActCritical.start + m_btDir * (pm.ActCritical.frame + pm.ActCritical.skip);
        m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
        m_dwFrameTime := pm.ActCritical.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);

        m_boUseEffect := True;
        m_dwEffectstarttime := GetTickCount;

        firedir := m_btDir;
        m_nEffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TTiger.LoadSurface;
begin
  inherited LoadSurface;

  if m_boUseEffect then begin
    case m_nCurrentAction of
      SM_HIT: begin
          EffectSurface := g_WMon35Img.GetCachedImage(500 + (firedir * 10) + m_nEffectframe - m_nEffectStart, ax, ay);
        end;
      SM_LIGHTING: begin
          EffectSurface := g_WMon35Img.GetCachedImage(580 + (firedir * 10) + m_nEffectframe - m_nEffectStart, ax, ay);
        end;
    end;
  end;
end;

procedure TTiger.Run;
var
  m_dwEffectframetimetime, m_dwFrameTimetime: LongWord;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then Exit;
  if m_boUseEffect then begin
    m_dwEffectframetimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectframetimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_nEffectframe < m_nEffectEnd then begin
        Inc(m_nEffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;
  inherited Run;
end;

procedure TTiger.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  bCanDraw                  : Boolean;
begin
  inherited;

  if m_boUseEffect then begin
    if EffectSurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        EffectSurface, 1);
    end;
  end;
end;

{-------------------------------------------}

constructor TDragon.Create;
begin
  inherited Create;
  BodySurface := nil;
  EffectSurface := nil;
  m_boUseEffect := False;
end;

procedure TDragon.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_nCurrentFrame := -1;
  m_boReverseFrame := False;
  m_boUseEffect := False;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);

  if pm = nil then Exit;

  case m_nCurrentAction of
    SM_HIT: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_LIGHTING: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;

        m_boUseEffect := True;
        m_dwEffectstarttime := GetTickCount;

        firedir := m_btDir;
        m_nEffectframe := m_nStartFrame;
        m_nEffectStart := m_nStartFrame;
        m_nEffectEnd := m_nEndFrame;
        m_dwEffectstarttime := GetTickCount;
        m_dwEffectframetime := m_dwFrameTime;
      end;
  else begin
      inherited;
    end;
  end;
end;

procedure TDragon.LoadSurface;
begin
  inherited LoadSurface;

  if m_wAppearance = 342 then begin
    if (not m_boReverseFrame) then
      BodySurface := g_WMon35Img.GetCachedImage(340 + GetOffset(m_wAppearance) + m_nCurrentFrame, ax, ay)
    else
      BodySurface := g_WMon35Img.GetCachedImage(340 + GetOffset(m_wAppearance) + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
  end;
  {if m_boUseEffect then begin
    case m_nCurrentAction of
      SM_HIT: begin
          EffectSurface := g_WMon35Img.GetCachedImage(500 + (firedir * 10) + m_nEffectframe - m_nEffectStart, ax, ay);
        end;
      SM_LIGHTING: begin
          EffectSurface := g_WMon35Img.GetCachedImage(580 + (firedir * 10) + m_nEffectframe - m_nEffectStart, ax, ay);
        end;
    end;
  end;}
end;

procedure TDragon.AttackEff;
var
  n8, nc, n10, n14, n18     : Integer;
  bofly                     : Boolean;
  i, iCount                 : Integer;
  Effect                    : TMagicEff;
begin
  if m_boDeath then
    Exit;
  n8 := m_nCurrX;
  nc := m_nCurrY;
  iCount := Random(5) + 2;
  for i := 0 to iCount do begin
    n10 := Random(4);
    n14 := Random(10);
    n18 := Random(10);
    case n10 of
      0: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      1: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18, 0, mtRedThunder, False, 30, bofly);
      2: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      3: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18, 0, mtRedThunder, False, 30, bofly);
    end;
    g_SndMgr.PlaySound(8301, m_nCurrX, m_nCurrY);
  end;
  if (m_nCurrentFrame - m_nStartFrame) = 4 then begin
    Effect := THeroCharEffect.Create(g_WDragonImg, 60, 20, 100, Self);
    g_PlayScene.m_EffectList.Add(Effect);
  end;
end;

procedure TDragon.Run;
var
  prv, nDir                 : Integer;
  m_dwEffectframetimetime, m_dwFrameTimetime: LongWord;
  bofly                     : Boolean;
begin
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_BACKSTEP) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then
    Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= MSGMUCH then
    m_boMsgMuch := True;
  if m_boRunSound then begin
    g_SndMgr.PlaySound(8201, m_nCurrX, m_nCurrY);
    m_boRunSound := False;
  end;

  if m_boUseEffect then begin
    if m_boMsgMuch then
      m_dwEffectframetimetime := Round(m_dwEffectframetime * 2 / 3)
    else
      m_dwEffectframetimetime := m_dwEffectframetime;
    if GetTickCount - m_dwEffectstarttime > m_dwEffectframetimetime then begin
      m_dwEffectstarttime := GetTickCount;
      if m_nEffectframe < m_nEffectEnd then begin
        Inc(m_nEffectframe);
      end else begin
        m_boUseEffect := False;
      end;
    end;
  end;

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then
      m_dwFrameTimetime := Round(m_dwFrameTime * 2 / 3)
    else
      m_dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > m_dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        Inc(m_nCurrentFrame);
        m_dwStartTime := GetTickCount;
      end else begin
        m_nCurrentAction := 0;
        m_boUseEffect := False;
      end;

      if (m_nCurrentAction = SM_LIGHTING) then begin
        AttackEff;
        g_SndMgr.PlaySound(8202, m_nCurrX, m_nCurrY);
      end else if (m_nCurrentAction = SM_HIT) then begin
        if m_btDir <= 4 then
          nDir := 81
        else if m_btDir = 5 then
          nDir := 82
        else if m_btDir >= 6 then
          nDir := 83;

        if (m_nCurrentFrame - m_nStartFrame) = 4 then begin
          g_PlayScene.NewMagic(Self, nDir, nDir, m_nCurrX, m_nCurrY, m_nTargetX, m_nTargetY, m_nTargetRecog, mtFly, True, 30, bofly);
          g_SndMgr.PlaySound(8203, m_nCurrX, m_nCurrY);
        end;
      end;

    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else begin
    if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
      if GetTickCount - m_dwDefFrameTime > 500 then begin
        m_dwDefFrameTime := GetTickCount;
        Inc(m_nCurrentDefFrame);
        if m_nCurrentDefFrame >= m_nDefFrameCount then
          m_nCurrentDefFrame := 0;
      end;
      DefaultMotion;
    end;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

procedure TDragon.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  bCanDraw                  : Boolean;
begin
  inherited;

  if m_wAppearance = 342 then begin
    if BodySurface <> nil then begin
      dsurface.DrawBlend(
        dx + ax + m_nShiftX,
        dy + ay + m_nShiftY,
        BodySurface, 1);
    end;
  end;
  {if m_boUseEffect then begin
    if EffectSurface <> nil then begin
      DrawBlend(dsurface,
        dx + ax2 + m_nShiftX,
        dy + ay2 + m_nShiftY,
        EffectSurface, 1);
    end;
  end;}
end;

{-------------------------------------------}

constructor TGhostShipMonster.Create;
begin
  inherited Create;
  ShadowSurface := nil;
  EffectSurface := nil;
  m_boUseEffect := False;
  FFireBall := False;
  FLighting := False;
end;

procedure TGhostShipMonster.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_boUseMagic := False;
  m_boUseEffect := False;
  m_boHitEffect := False;
  m_boReverseFrame := False;

  m_nCurrentFrame := -1;
  m_nHitEffectNumber := 0;

  m_nBodyOffset := GetOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);

  if pm = nil then Exit;

  case m_nCurrentAction of
    {SM_TURN: begin

      end;}
    SM_HIT: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        if (m_wAppearance = 354) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 3;
          Inc(m_nHitEffectNumber, 101);
        end;
        if (m_wAppearance = 815) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 3;
          Inc(m_nHitEffectNumber, 301);
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_LIGHTING: begin
        case m_wAppearance of
          354, 356, 359, 813, 815: begin
              m_nStartFrame := m_Action.ActCritical.start + m_btDir * (m_Action.ActCritical.frame + m_Action.ActCritical.skip);
              m_nEndFrame := m_nStartFrame + m_Action.ActCritical.frame - 1;
              m_dwFrameTime := m_Action.ActCritical.ftime;
              m_dwStartTime := GetTickCount;
              m_dwWarModeTime := GetTickCount;
              if (m_wAppearance = 354) or (m_wAppearance = 815) then begin
                m_boSmiteWideHit2 := True;
              end;
            end;
        else begin
            inherited;
            Exit;
          end;
        end;
      end;
    SM_LIGHTING_1: begin
        case m_wAppearance of
          356, 813: begin
              m_nStartFrame := m_Action.ActCritical.start + m_btDir * (m_Action.ActCritical.frame + m_Action.ActCritical.skip);
              m_nEndFrame := m_nStartFrame + m_Action.ActCritical.frame - 1;
              m_dwFrameTime := m_Action.ActCritical.ftime;
              m_dwStartTime := GetTickCount;
              m_dwWarModeTime := GetTickCount;

              FLighting := True;
            end;
        end;
      end;
    SM_DIGUP: begin
        case m_wAppearance of
          351, 827: m_nStartFrame := pm.ActDeath.start + m_btDir * (pm.ActDeath.frame + pm.ActDeath.skip);
        else begin
            inherited;
            Exit;
          end;
        end;
        m_nEndFrame := m_nStartFrame + pm.ActDeath.frame - 1;
        m_dwFrameTime := pm.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_SPELL: begin
        if m_CurMagic.MagicSerial = 23 then begin
          m_nStartFrame := m_Action.ActCritical.start + m_btDir * (m_Action.ActCritical.frame + m_Action.ActCritical.skip);
          m_nEndFrame := m_nStartFrame + m_Action.ActCritical.frame - 1;
          m_dwFrameTime := m_Action.ActCritical.ftime;
          m_dwStartTime := GetTickCount;
          m_dwWarModeTime := GetTickCount;
        end else begin
          m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
          m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
          m_dwFrameTime := pm.ActAttack.ftime;
          m_dwStartTime := GetTickCount;
        end;

        m_nCurEffFrame := 0;
        m_boUseMagic := True;
        m_nMagLight := 2;
        m_nSpellFrame := pm.ActAttack.frame;
        m_dwWaitMagicRequest := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_FLYAXE: begin
        m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
        m_dwFrameTime := pm.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
  else begin
      inherited CalcActorFrame;
    end;
  end;
end;

procedure TGhostShipMonster.RunActSound(frame: Integer);
begin
  inherited;
  if m_boRunSound then begin
    case m_nCurrentAction of
      SM_HIT: begin
          if (m_wAppearance = 354) or (m_wAppearance = 815) then begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              g_SndMgr.PlaySound(s_widehit, m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;
        end;
      SM_LIGHTING: begin
          if (m_wAppearance = 354) or (m_wAppearance = 815) then begin
            if frame = 4 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              g_SndMgr.PlaySound('Wav\xsws_pbec.wav', m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;
        end;
    end;
  end;
end;

procedure TGhostShipMonster.RunFrameAction(frame: Integer);
var
  neff                      : TNormalDrawEffect;
  meff                      : TMapEffect;
  event                     : TClEvent;
  mfly                      : TFlyingAxe;
  HeroCharEffect            : TMagicEff;
begin
  if m_nCurrentAction = SM_LIGHTING then begin
    if (frame = 5) and m_boSmiteWideHit2 then begin
      m_boSmiteWideHit2 := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic2Images, 1391, 14, 75, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
      if (m_wAppearance = 354) then begin
        g_ShakeScreen.SetScrShake_X(4);
        g_ShakeScreen.SetScrShake_Y(3);
      end;
      if (m_wAppearance = 815) then begin
        g_ShakeScreen.SetScrShake_X(7);
        g_ShakeScreen.SetScrShake_Y(5);
      end;
    end;

  end;
end;

function TGhostShipMonster.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  if m_boDeath then begin
    if m_boSkeleton then
      Result := pm.ActDeath.start
    else
      Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then
      cf := 0
    else if m_nCurrentDefFrame >= m_nDefFrameCount then
      cf := 0
    else
      cf := m_nCurrentDefFrame;
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
  end;
end;

procedure TGhostShipMonster.LoadSurface;
var
  nShadowOffset             : Integer;
  mimg                      : TWMImages;
begin
  mimg := GetMonImg(m_wAppearance);
  if mimg <> nil then begin
    if not m_boReverseFrame then
      m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nCurrentFrame, m_nPx, m_nPy)
    else
      m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
  end;

  //g_Screen.AddChatBoardString(Format('%d', [400 + GetOffset(m_wAppearance) + m_nCurrentFrame]), GetRGB(5), clWhite);

  case m_wAppearance of
    351, 354, 356, 359, 813,
      815, 818, 819, 820, 821,
      822: nShadowOffset := 480;

    812: nShadowOffset := 320;
    825, 827: nShadowOffset := 560;
  else
    nShadowOffset := 400;
  end;

  if (not m_boReverseFrame) then
    ShadowSurface := g_WMon36Img.GetCachedImage(nShadowOffset + GetOffset(m_wAppearance) + m_nCurrentFrame, ax, ay)
  else
    ShadowSurface := g_WMon36Img.GetCachedImage(nShadowOffset + GetOffset(m_wAppearance) + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), ax, ay);
end;

procedure TGhostShipMonster.AttackEff;
var
  n8, nc, n10, n14, n18     : Integer;
  bofly                     : Boolean;
  i, iCount                 : Integer;
  Effect                    : TMagicEff;
begin
  if m_boDeath then
    Exit;
  n8 := m_nCurrX;
  nc := m_nCurrY;
  iCount := Random(5) + 2;
  for i := 0 to iCount do begin
    n10 := Random(4);
    n14 := Random(10);
    n18 := Random(10);
    case n10 of
      0: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      1: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18, 0, mtRedThunder, False, 30, bofly);
      2: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14, nc + n18 + 1, 0, mtRedThunder, False, 30, bofly);
      3: g_PlayScene.NewMagic(Self, 80, 80, m_nCurrX, m_nCurrY, n8 - n14 - 2, nc + n18, 0, mtRedThunder, False, 30, bofly);
    end;
    g_SndMgr.PlaySound(8301, m_nCurrX, m_nCurrY);
  end;
  if (m_nCurrentFrame - m_nStartFrame) = 4 then begin
    Effect := THeroCharEffect.Create(g_WDragonImg, 60, 20, 100, Self);
    g_PlayScene.m_EffectList.Add(Effect);
  end;
end;

procedure TGhostShipMonster.Run;
var
  prv, mx, my               : Integer;
  dwFrameTimetime           : LongWord;
  bofly                     : Boolean;
  meff                      : TFlyingAxe;
  meff2                     : TFlyingArrow;
begin
  if (m_nCurrentAction = SM_WALK) or
    (m_nCurrentAction = SM_BACKSTEP) or
    (m_nCurrentAction = SM_RUN) or
    (m_nCurrentAction = SM_HORSERUN) or
    (m_nCurrentAction = SM_RUSH) or
    (m_nCurrentAction = SM_RUSHEX) or
    (m_nCurrentAction = SM_RUSHKUNG) then Exit;

  m_boMsgMuch := False;
  if m_MsgList.count >= 2 then
    m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then
      dwFrameTimetime := Round(m_dwFrameTime * 2 / 3) //Round(m_dwFrameTime / 1.6)
    else
      dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        if m_boUseMagic then begin
          if (m_nCurEffFrame = m_nSpellFrame - 2) then begin
            if (m_CurMagic.ServerMagicCode >= 0) then begin
              Inc(m_nCurrentFrame);
              Inc(m_nCurEffFrame);
              m_dwStartTime := GetTickCount;
            end;
          end else begin
            if m_nCurrentFrame < m_nEndFrame - 1 then Inc(m_nCurrentFrame);
            Inc(m_nCurEffFrame);
            m_dwStartTime := GetTickCount;
          end;
        end else begin
          Inc(m_nCurrentFrame);
          m_dwStartTime := GetTickCount;
        end;

      end else begin
        if m_boDelActionAfterFinished then
          m_boDelActor := True;
        ActionEnded;
        m_nCurrentAction := 0;
        m_boUseMagic := False;
        m_boUseEffect := False;            //0605
        m_boHitEffect := False;
      end;

      if m_boUseMagic then begin
        if m_nCurEffFrame = m_nSpellFrame - 1 then begin
          if m_CurMagic.ServerMagicCode > 0 then begin
            with m_CurMagic do begin
              g_PlayScene.NewMagic(Self,
                ServerMagicCode,
                EffectNumber,
                m_nCurrX,
                m_nCurrY,
                targx,
                targy,
                target,
                EffectType,
                Recusion,
                anitime,
                bofly,
                magfirelv);
              if bofly then begin

                g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY)
              end else begin
                g_SndMgr.PlaySound(m_nMagicexplosionsound, targx, targy);
              end;
            end;
          end;
          m_CurMagic.ServerMagicCode := 0;
        end;
      end;
      if (m_nCurrentAction = SM_FLYAXE) and (m_nCurrentFrame - m_nStartFrame = AXEMONATTACKFRAME - 4) then begin
        if m_wAppearance = 812 then begin
          meff := TFlyingAxe(g_PlayScene.NewFlyObject(Self,
            m_nCurrX,
            m_nCurrY,
            m_nTargetX,
            m_nTargetY,
            m_nTargetRecog,
            mtFlyAxe));
          if meff <> nil then begin
            meff.ImgLib := g_WMon3Img;
            case m_wAppearance of
              812: meff.FlyImageBase := THORNBASE;
            end;
          end;
        end;
        if m_wAppearance = 826 then begin
          meff2 := TFlyingArrow(g_PlayScene.NewFlyObject(Self,
            m_nCurrX,
            m_nCurrY,
            m_nTargetX,
            m_nTargetY,
            m_nTargetRecog,
            mtFlyArrow));
          if meff2 <> nil then begin
            meff2.ImgLib := g_WEffectImg;
            meff2.NextFrameTime := 30;
            meff2.FlyImageBase := ARCHERBASE2;
          end;
        end;

      end;
    end;
    m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
    if GetTickCount - m_dwDefFrameTime > 500 then begin
      m_dwDefFrameTime := GetTickCount;
      Inc(m_nCurrentDefFrame);
      if m_nCurrentDefFrame >= m_nDefFrameCount then
        m_nCurrentDefFrame := 0;
    end;
    DefaultMotion;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

procedure TGhostShipMonster.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  wimg                      : TWMImages;
  idx                       : Integer;
  d                         : TCustomLockableTexture;
  tmpx, tmpy, ShiftX, ShiftY: Integer;
begin
  ShiftX := dx + m_nShiftX;
  ShiftY := dy + m_nShiftY;

  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

  if ShadowSurface <> nil then begin
    dsurface.DrawBlend(ShiftX + ax,
      ShiftY + ay,
      ShadowSurface, 0);
  end;

  inherited;

  if m_boHitEffect then begin
    if (m_nHitEffectNumber > 0) then begin
      GetEffectBase(m_nHitEffectNumber - 1, 1, wimg, idx);
      if wimg <> nil then begin
        idx := idx + m_btDir * 10 + (m_nCurrentFrame - m_nStartFrame);
        d := wimg.GetCachedImage(idx, tmpx, tmpy);
        if d <> nil then
          dsurface.DrawBlend(ShiftX + tmpx, ShiftY + tmpy, d, 1);
      end;
    end;
  end;
end;

procedure TGhostShipMonster.SetSound;
var
  cx, cy, bidx, wunit, attackweapon: Integer;
  hiter                     : TActor;
begin
  inherited;
  if m_boUseMagic and (m_CurMagic.MagicSerial > 0) then begin
    m_nMagicstartsound := 10000 + m_CurMagic.MagicSerial * 10;
    m_nMagicFireSound := m_nMagicstartsound + 1;
    m_nMagicexplosionsound := m_nMagicstartsound + 2;
  end;
end;

procedure TGhostShipMonster.RunSound;
begin
  m_boRunSound := True;
  SetSound;
  case m_nCurrentAction of
    SM_STRUCK: begin
        if (m_nStruckWeaponSound >= 0) then g_SndMgr.PlaySound(m_nStruckWeaponSound, m_nCurrX, m_nCurrY);
        if (m_nStruckSound >= 0) then g_SndMgr.PlaySound(m_nStruckSound, m_nCurrX, m_nCurrY);
        if (m_nScreamSound >= 0) then g_SndMgr.PlaySound(m_nScreamSound, m_nCurrX, m_nCurrY);
      end;
    SM_NOWDEATH: if (m_nDieSound >= 0) then begin
        g_SndMgr.PlaySound(m_nDieSound, m_nCurrX, m_nCurrY);
      end;
    SM_THROW, SM_HIT, SM_FLYAXE, SM_LIGHTING, SM_DIGDOWN: begin
        if m_nAttackSound >= 0 then g_SndMgr.PlaySound(m_nAttackSound, m_nCurrX, m_nCurrY);
      end;
    SM_ALIVE, SM_DIGUP: begin
        g_SndMgr.SilenceSound;
        g_SndMgr.PlaySound(m_nAppearSound, m_nCurrX, m_nCurrY);
      end;
    SM_SPELL: g_SndMgr.PlaySound(m_nMagicstartsound, m_nCurrX, m_nCurrY);
  end;
end;

end.

