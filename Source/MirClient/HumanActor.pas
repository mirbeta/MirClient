unit HumanActor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, uGameEngine,
  Grobal2, PXL.Canvas, PXL.Textures, cliUtil, magiceff, WIL, ClFunc, GList, Actor, StallSystem;

type
  THumanAction = packed record
    ActStand: TActionInfo;
    ActWalk: TActionInfo;
    ActRun: TActionInfo;
    ActRushLeft: TActionInfo;
    ActRushRight: TActionInfo;
    ActWarMode: TActionInfo;
    ActHit: TActionInfo;
    ActUnitHit: TActionInfo;
    ActHeavyHit: TActionInfo;
    ActBigHit: TActionInfo;
    ActFireHitReady: TActionInfo;
    ActSpell: TActionInfo;
    ActSitdown: TActionInfo;
    ActStruck: TActionInfo;
    ActDie: TActionInfo;

    ActRush2: TActionInfo;
    ActSmiteHit: TActionInfo;
    ActSmiteLongHit: TActionInfo;
    ActSmiteLongHit2: TActionInfo;
    ActSmiteLongHit3: TActionInfo;
    ActSmiteWideHit: TActionInfo;

    ActMagic_104: TActionInfo;
    ActMagic_105: TActionInfo;
    ActMagic_106: TActionInfo;
    ActMagic_107: TActionInfo;

    ActMagic_108: TActionInfo;
    ActMagic_109: TActionInfo;
    ActMagic_110: TActionInfo;
    ActMagic_111: TActionInfo;
    ActMagic_112: TActionInfo;

    ActMagic_113: TActionInfo;
    ActMagic_114: TActionInfo;
  end;
  pTHumanAction = ^THumanAction;

const
  HA                        : THumanAction = (
    ActStand: (start: 0; frame: 4; skip: 4; ftime: 200; usetick: 0);
    ActWalk: (start: 64; frame: 6; skip: 2; ftime: 90; usetick: 2);
    ActRun: (start: 128; frame: 6; skip: 2; ftime: 115; usetick: 3);
    ActRushLeft: (start: 128; frame: 3; skip: 5; ftime: 120; usetick: 3);
    ActRushRight: (start: 131; frame: 3; skip: 5; ftime: 120; usetick: 3);
    ActWarMode: (start: 192; frame: 1; skip: 0; ftime: 200; usetick: 0);
    ActHit: (start: 200; frame: 6; skip: 2; ftime: 85; usetick: 0);
    ActUnitHit: (start: 200; frame: 17; skip: 2; ftime: 85; usetick: 0);
    ActHeavyHit: (start: 264; frame: 6; skip: 2; ftime: 90; usetick: 0);
    ActBigHit: (start: 328; frame: 8; skip: 0; ftime: 70; usetick: 0);
    ActFireHitReady: (start: 192; frame: 1; skip: 0; ftime: 100; usetick: 0); //ActFireHitReady: (start: 192; frame: 6; skip: 4; ftime: 70; usetick: 0);
    ActSpell: (start: 392; frame: 6; skip: 2; ftime: 60; usetick: 0);
    ActSitdown: (start: 456; frame: 2; skip: 0; ftime: 300; usetick: 0);
    ActStruck: (start: 472; frame: 3; skip: 5; ftime: 70; usetick: 0);
    ActDie: (start: 536; frame: 4; skip: 4; ftime: 120; usetick: 0);

    ActRush2: (start: 080; frame: 8; skip: 2; ftime: 77; usetick: 3);

    ActSmiteHit: (start: 160; frame: 15; skip: 5; ftime: 56; usetick: 0);
    ActSmiteLongHit: (start: 1920; frame: 5; skip: 5; ftime: 45; usetick: 0);
    ActSmiteLongHit2: (start: 320; frame: 6; skip: 4; ftime: 80; usetick: 0);
    ActSmiteLongHit3: (start: 320; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActSmiteWideHit: (start: 560; frame: 10; skip: 0; ftime: 78; usetick: 0);

    ActMagic_104: (start: 640; frame: 6; skip: 4; ftime: 92; usetick: 0);
    ActMagic_105: (start: 880; frame: 10; skip: 0; ftime: 88; usetick: 0);
    ActMagic_106: (start: 800; frame: 8; skip: 2; ftime: 88; usetick: 0);
    ActMagic_107: (start: 1040; frame: 13; skip: 7; ftime: 72; usetick: 0);

    ActMagic_108: (start: 1200; frame: 6; skip: 4; ftime: 95; usetick: 0);
    ActMagic_109: (start: 1440; frame: 12; skip: 8; ftime: 78; usetick: 0);
    ActMagic_110: (start: 1600; frame: 12; skip: 8; ftime: 78; usetick: 0);
    ActMagic_111: (start: 1760; frame: 14; skip: 6; ftime: 65; usetick: 0);

    ActMagic_112: (start: 720; frame: 6; skip: 4; ftime: 95; usetick: 0);
    ActMagic_113: (start: 400; frame: 12; skip: 8; ftime: 70; usetick: 0);
    ActMagic_114: (start: 400; frame: 12; skip: 8; ftime: 85; usetick: 0)
    );

type
  THumActor = class(TActor)
  private
    m_boSSkill: Boolean;
    m_HairSurface: TCustomLockableTexture;
    m_WeaponSurface: TCustomLockableTexture;

    m_HumWinSurface: TCustomLockableTexture;
    m_boWeaponEffect: Boolean;
    m_nCurWeaponEffect: Integer;
    m_nCurBubbleStruck: Integer;
    m_dwWeaponpEffectTime: LongWord;
    m_boHideWeapon: Boolean;
  protected
    procedure CalcActorFrame; override;
    procedure DefaultMotion; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure CalcActorWinFrame;
  public
    m_StallMgr: TStallMgr;
    m_WeaponEffect: TCustomLockableTexture;
    m_nFrame: Integer;
    m_dwFrameTick: LongWord;
    m_dwFrameTime: LongWord;
    m_SlaveObject: TList;
    m_HeroObject: TActor;
    constructor Create; override;
    destructor Destroy; override;
    procedure Run; override;
    procedure RunFrameAction(frame: Integer); override;
    procedure ActionEnded(); override;
    procedure ReadyNextAction(); override;
    function light: Integer; override;
    procedure LoadSurface; override;
    procedure DoWeaponBreakEffect;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
    procedure DrawChr_Transparent(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean);
  end;

implementation

uses
  ClMain, SoundUtil, clEvent, MShare, HUtil32;

constructor THumActor.Create;
begin
  inherited Create;
  m_SlaveObject := TList.Create;
  m_HeroObject := nil;
  m_HairSurface := nil;
  m_WeaponSurface := nil;
  m_WeaponEffect := nil;
  m_HumWinSurface := nil;
  m_boWeaponEffect := False;
  m_boSSkill := False;
  m_dwFrameTime := 150;
  m_dwFrameTick := GetTickCount();
  m_nFrame := 0;
  m_nHumWinOffset := 0;
  m_nCboHumWinOffSet := 0;
  m_StallMgr := TStallMgr.Create;
end;

destructor THumActor.Destroy;
begin
  m_SlaveObject.Free;
  m_StallMgr.Free;
  inherited Destroy;
end;

procedure THumActor.CalcActorWinFrame;
begin
  if (m_btEffect = 50) then begin
    m_nCboHumWinOffSet := 352;
  end else if m_btEffect <> 0 then
    m_nCboHumWinOffSet := (m_btEffect - 1) * 2000;
end;

procedure THumActor.ActionEnded();
begin
  if g_SeriesSkillFire then begin
    if (g_MagicLockActor = nil) or g_MagicLockActor.m_boDeath then begin
      g_SeriesSkillFire := False;
      g_SeriesSkillStep := 0;
    end;
    if m_boUseMagic and (Self = g_MySelf) and (g_MagicLockActor <> nil) and (not g_MagicLockActor.m_boDeath) and (g_nCurrentMagic <= 3) then begin
      if (m_nCurrentFrame - m_nStartFrame) >= (m_nSpellFrame - 1) then begin
        if g_MagicArr[0][g_SeriesSkillArr[g_nCurrentMagic]] <> nil then begin
          frmMain.UseMagic(g_nMouseX, g_nMouseY, g_MagicArr[0][g_SeriesSkillArr[g_nCurrentMagic]], False, True);
        end;
        Inc(g_nCurrentMagic);
        if g_nCurrentMagic > _MIN(3, g_SeriesSkillStep) then begin
          g_SeriesSkillFire := False;
          g_SeriesSkillStep := 0;
        end;
      end;
    end;
  end;
end;

procedure THumActor.ReadyNextAction();
begin
  //if g_SeriesSkillFire then begin
  if m_boUseCboLib and m_boHitEffect and (Self = g_MySelf) and (g_nCurrentMagic2 < 4) then begin
    if (m_nCurrentFrame - m_nStartFrame) = 2 then begin
      if g_MagicArr[0][g_SeriesSkillArr[g_nCurrentMagic2]] <> nil then
        frmMain.UseMagic(g_nMouseX, g_nMouseY, g_MagicArr[0][g_SeriesSkillArr[g_nCurrentMagic2]], False, True);
      Inc(g_nCurrentMagic2);
      if g_nCurrentMagic2 > _MIN(4, g_SeriesSkillStep) then begin
        g_SeriesSkillFire := False;
        //  g_SeriesSkillStep := 0;
      end;
    end;
  end;
  //end;
end;

procedure THumActor.CalcActorFrame;
var
  nHairEx, haircount        : Integer;
  Effect                    : TMagicEff;
  boFly                     : Boolean;
begin
  m_boUseMagic := False;
  m_boNewMagic := False;
  m_boUseCboLib := False;
  m_boHitEffect := False;
  m_nHitEffectNumber := 0;
  m_nCurrentFrame := -1;
  m_btHair := HAIRfeature(m_nFeature);
  m_btDress := DRESSfeature(m_nFeature);
 // if m_btDress in [24..27] then m_btDress := 18 + m_btSex; 20200719
  m_btWeapon := WEAPONfeature(m_nFeature);
  m_btHorse := Horsefeature(m_nFeatureEx);
  //
  m_btWeaponEffect := m_btHorse;        // div 51;
  m_btHorse := 0;                       //m_btHorse mod 51;

  m_btEffect := Effectfeature(m_nFeatureEx);

  m_nBodyOffset := HUMANFRAME * (m_btDress);

  m_nCboHairOffset := -1;

  if m_btHair >= 10 then begin
    m_btHairEx := m_btHair div 10;
    m_btHair := m_btHair mod 10;
  end else
    m_btHairEx := 0;

  if m_btHairEx = 0 then begin          //头发兼容模式
    haircount := g_WHair2ImgImages.ImageCount div HUMANFRAME div 2;
    if m_btHair > haircount - 1 then
      m_btHair := haircount - 1;
    nHairEx := (m_btHair - m_btSex) shr 1 + 1;

    haircount := g_cbohair.ImageCount div 2000 div 2;
    if nHairEx > haircount then
      nHairEx := haircount;

    m_nCboHairOffset := 2000 * ((nHairEx - 1) * 2 + m_btSex);
  end else if m_btHairEx > 0 then begin
    haircount := g_WHair2ImgImages.ImageCount div HUMANFRAME div 2;
    if m_btHairEx > haircount then
      m_btHairEx := haircount;
    m_nHairOffsetEx := HUMANFRAME * ((m_btHairEx - 1) * 2 + m_btSex);

    //nHairEx := m_btHairEx + m_btSex + (m_btHairEx mod 4);
    nHairEx := (m_btHairEx - 1) * 2 + m_btSex;
    haircount := g_cbohair.ImageCount div 2000;
    if nHairEx > haircount then
      nHairEx := haircount;

    m_nCboHairOffset := 2000 * (nHairEx);
  end else
    m_nHairOffsetEx := -1;

  haircount := g_WHairImgImages.ImageCount div HUMANFRAME div 2;
  if m_btHair > haircount - 1 then
    m_btHair := haircount - 1;
  m_btHair := m_btHair * 2;
  if m_btHair > 1 then
    m_nHairOffset := HUMANFRAME * (m_btHair + m_btSex)
  else
    m_nHairOffset := -1;

  m_nWeaponOffset := HUMANFRAME * m_btWeapon;
  if (m_btEffect = 50) then begin
    m_nHumWinOffset := 352;
  end else if m_btEffect <> 0 then
    m_nHumWinOffset := (m_btEffect - 1) * HUMANFRAME;

  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := HA.ActStand.start + m_btDir * (HA.ActStand.frame + HA.ActStand.skip);
        m_nEndFrame := m_nStartFrame + HA.ActStand.frame - 1;
        m_dwFrameTime := HA.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := HA.ActStand.frame;
        Shift(m_btDir, 0, 0, m_nEndFrame - m_nStartFrame + 1);
        if m_fHideMode then begin
          m_fHideMode := False;
          m_dwSmoothMoveTime := 0;
          m_nCurrentAction := 0;
        end;
      end;
    SM_WALK, SM_BACKSTEP: begin
        m_nStartFrame := HA.ActWalk.start + m_btDir * (HA.ActWalk.frame + HA.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + HA.ActWalk.frame - 1;
        m_dwFrameTime := HA.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_BACKSTEP then
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_RUSH: begin
        if m_nRushDir = 0 then begin
          m_nRushDir := 1;
          m_nStartFrame := HA.ActRushLeft.start + m_btDir * (HA.ActRushLeft.frame + HA.ActRushLeft.skip);
          m_nEndFrame := m_nStartFrame + HA.ActRushLeft.frame - 1;
          m_dwFrameTime := HA.ActRushLeft.ftime;
          m_dwStartTime := GetTickCount;
          m_nMaxTick := HA.ActRushLeft.usetick;
          m_nCurTick := 0;
          m_nMoveStep := 1;
          Shift(m_btDir, 1, 0, m_nEndFrame - m_nStartFrame + 1);
        end else begin
          m_nRushDir := 0;
          m_nStartFrame := HA.ActRushRight.start + m_btDir * (HA.ActRushRight.frame + HA.ActRushRight.skip);
          m_nEndFrame := m_nStartFrame + HA.ActRushRight.frame - 1;
          m_dwFrameTime := HA.ActRushRight.ftime;
          m_dwStartTime := GetTickCount;
          m_nMaxTick := HA.ActRushRight.usetick;
          m_nCurTick := 0;
          m_nMoveStep := 1;
          Shift(m_btDir, 1, 0, m_nEndFrame - m_nStartFrame + 1);
        end;
      end;
    SM_RUSHEX: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActRush2.frame;
        m_nStartFrame := HA.ActRush2.start + m_btDir * (HA.ActRush2.frame + HA.ActRush2.skip);
        m_nEndFrame := m_nStartFrame + HA.ActRush2.frame - 1;
        m_dwFrameTime := HA.ActRush2.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActRush2.usetick;
        m_nCurTick := 0;
        m_nMoveStep := m_RushStep;
        Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;

        m_boHitEffect := True;
      end;
    SM_SMITEHIT: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActSmiteHit.frame;
        m_nStartFrame := HA.ActSmiteHit.start + m_btDir * (HA.ActSmiteHit.frame + HA.ActSmiteHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActSmiteHit.frame - 1;
        m_dwFrameTime := HA.ActSmiteHit.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActSmiteHit.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boSmiteHit := True;
        m_boHitEffect := True;

      end;
    SM_SMITELONGHIT: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActSmiteLongHit.frame;
        m_nStartFrame := HA.ActSmiteLongHit.start + m_btDir * (HA.ActSmiteLongHit.frame + HA.ActSmiteLongHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActSmiteLongHit.frame - 1;
        m_dwFrameTime := HA.ActSmiteLongHit.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActSmiteLongHit.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boSmiteLongHit := 1;
        m_boHitEffect := True;
      end;
    SM_SMITELONGHIT3: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActSmiteLongHit3.frame;
        m_nStartFrame := HA.ActSmiteLongHit3.start + m_btDir * (HA.ActSmiteLongHit3.frame + HA.ActSmiteLongHit3.skip);
        m_nEndFrame := m_nStartFrame + HA.ActSmiteLongHit3.frame - 1;
        m_dwFrameTime := HA.ActSmiteLongHit3.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActSmiteLongHit3.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boHitEffect := True;
      end;

    SM_SMITELONGHIT2: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActMagic_113.frame;
        m_nStartFrame := HA.ActMagic_113.start + m_btDir * (HA.ActMagic_113.frame + HA.ActMagic_113.skip);
        m_nEndFrame := m_nStartFrame + HA.ActMagic_113.frame - 1;
        m_dwFrameTime := HA.ActMagic_113.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActMagic_113.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boSmiteLongHit2 := True;
        m_boSmiteLongHitS2 := 2;
        m_boHitEffect := True;
      end;
    SM_SMITEWIDEHIT: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActSmiteWideHit.frame;
        m_nStartFrame := HA.ActSmiteWideHit.start + m_btDir * (HA.ActSmiteWideHit.frame + HA.ActSmiteWideHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActSmiteWideHit.frame - 1;
        m_dwFrameTime := HA.ActSmiteWideHit.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActSmiteWideHit.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boHitEffect := True;
      end;
    SM_SMITEWIDEHIT2: begin
        CalcActorWinFrame();
        m_nSpellFrame := HA.ActMagic_114.frame;
        m_nStartFrame := HA.ActMagic_114.start + m_btDir * (HA.ActMagic_114.frame + HA.ActMagic_114.skip);
        m_nEndFrame := m_nStartFrame + HA.ActMagic_114.frame - 1;
        m_dwFrameTime := HA.ActMagic_114.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActMagic_114.usetick;
        m_nCurTick := 0;
        Shift(m_btDir, 0, 0, 1);

        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boUseCboLib := True;
        m_boSmiteWideHit2 := True;
        m_boSmiteWideHitS2 := 9;
        m_boHitEffect := True;
      end;
    SM_RUSHKUNG: begin
        m_nStartFrame := HA.ActRun.start + m_btDir * (HA.ActRun.frame + HA.ActRun.skip);
        m_nEndFrame := m_nStartFrame + HA.ActRun.frame - 1;
        m_dwFrameTime := HA.ActRun.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActRun.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_SITDOWN: begin
        m_nStartFrame := HA.ActSitdown.start + m_btDir * (HA.ActSitdown.frame + HA.ActSitdown.skip);
        m_nEndFrame := m_nStartFrame + HA.ActSitdown.frame - 1;
        m_dwFrameTime := HA.ActSitdown.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_RUN: begin
        m_nStartFrame := HA.ActRun.start + m_btDir * (HA.ActRun.frame + HA.ActRun.skip);
        m_nEndFrame := m_nStartFrame + HA.ActRun.frame - 1;
        m_dwFrameTime := HA.ActRun.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActRun.usetick;
        m_nCurTick := 0;
        if m_nCurrentAction = SM_RUN then
          m_nMoveStep := 2
        else
          m_nMoveStep := 1;
        Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_HORSERUN: begin
        m_nStartFrame := HA.ActRun.start + m_btDir * (HA.ActRun.frame + HA.ActRun.skip);
        m_nEndFrame := m_nStartFrame + HA.ActRun.frame - 1;
        m_dwFrameTime := HA.ActRun.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := HA.ActRun.usetick;
        m_nCurTick := 0;
        if m_nCurrentAction = SM_HORSERUN then
          m_nMoveStep := 3
        else
          m_nMoveStep := 1;
        Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_THROW: begin
        m_nStartFrame := HA.ActHit.start + m_btDir * (HA.ActHit.frame + HA.ActHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActHit.frame - 1;
        m_dwFrameTime := HA.ActHit.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        m_boThrow := True;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_HIT, SM_POWERHIT, SM_LONGHIT, SM_WIDEHIT, SM_FIREHIT,
      SM_HERO_LONGHIT, SM_HERO_LONGHIT2, SM_SQUHIT, SM_CRSHIT, SM_TWNHIT: begin
        m_nStartFrame := HA.ActHit.start + m_btDir * (HA.ActHit.frame + HA.ActHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActHit.frame - 1;
        m_dwFrameTime := HA.ActHit.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        if (m_nCurrentAction = SM_POWERHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 1;
          case m_CurMagic.magfirelv div 4 of
            1: Inc(m_nHitEffectNumber, 101);
            2: Inc(m_nHitEffectNumber, 201);
            3: Inc(m_nHitEffectNumber, 301);
          end;
        end;
        if (m_nCurrentAction = SM_LONGHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 2;
          case m_CurMagic.magfirelv div 4 of
            1: Inc(m_nHitEffectNumber, 101);
            2: Inc(m_nHitEffectNumber, 201);
            3: Inc(m_nHitEffectNumber, 301);
          end;
        end;
        if (m_nCurrentAction = SM_WIDEHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 3;
          case m_CurMagic.magfirelv div 4 of
            1: Inc(m_nHitEffectNumber, 101);
            2: Inc(m_nHitEffectNumber, 201);
            3: Inc(m_nHitEffectNumber, 301);
          end;
        end;
        if (m_nCurrentAction = SM_FIREHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 4;
          //if m_CurMagic.magfirelv > MAXMAGICLV then m_nHitEffectNumber := 9;
          case m_CurMagic.magfirelv div 4 of
            1: Inc(m_nHitEffectNumber, 101);
            2: Inc(m_nHitEffectNumber, 201);
            3: Inc(m_nHitEffectNumber, 301);
          end;
        end;
        if (m_nCurrentAction = SM_SQUHIT) then begin
          Effect := THeroCharEffect.Create(g_WMagic2Images, m_btDir * 20 + 740, 15, 50, Self);
          if Effect <> nil then
            g_PlayScene.m_EffectList.Add(Effect);
        end;
        if (m_nCurrentAction = SM_CRSHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 5;
        end;
        if (m_nCurrentAction = SM_TWNHIT) then begin
          Effect := THeroCharEffect.Create(g_WMagic2Images, m_btDir * 20 + 226, 6, 72 {35}, Self);
          if Effect <> nil then
            g_PlayScene.m_EffectList.Add(Effect);
        end;
        if (m_nCurrentAction = SM_HERO_LONGHIT2) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 6;
          m_boHeroLongHit2 := True;
        end;
        if (m_nCurrentAction = SM_HERO_LONGHIT) then begin
          m_boHitEffect := True;
          m_nMagLight := 2;
          m_nHitEffectNumber := 8;
          m_boHeroLongHit := True;
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_WWJATTACK, SM_WSJATTACK, SM_WTJATTACK: begin
        m_nStartFrame := HA.ActHit.start + m_btDir * (HA.ActHit.frame + HA.ActHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActHit.frame - 1;
        m_dwFrameTime := HA.ActHit.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;

        if (m_nCurrentAction = SM_WWJATTACK) then begin
          Effect := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic4Images, m_btDir * 20 + 10, 20, 110, True, 10512);
          if Effect <> nil then begin
            Effect.MagOwner := Self;
            g_SndMgr.PlaySound(10510, m_nCurrX, m_nCurrY);
            g_PlayScene.m_EffectList.Add(Effect);
          end;
        end;

        if (m_nCurrentAction = SM_WSJATTACK) then begin //雷霆一击
          g_SndMgr.PlaySound(123, m_nCurrX, m_nCurrY);
          Effect := THeroCharEffect.Create(g_WMagic4Images, 290, 10, 80, Self);
          if Effect <> nil then g_PlayScene.m_EffectList.Add(Effect);
          m_boHitEffect := True;
          m_nMagLight := 3;
          m_nHitEffectNumber := 7;
          Effect := THeroCharEffect.Create(g_WMagic4Images, 420, 16, 75, Self);
          if Effect <> nil then g_PlayScene.m_EffectList.Add(Effect);
        end;

        if (m_nCurrentAction = SM_WTJATTACK) then begin //劈星斩
          g_SndMgr.PlaySound(124, m_nCurrX, m_nCurrY);
          Effect := THeroCharEffect.Create(g_WMagic4Images, 460, 10, 80, Self);
          if Effect <> nil then g_PlayScene.m_EffectList.Add(Effect);
        end;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_HEAVYHIT: begin
        m_nStartFrame := HA.ActHeavyHit.start + m_btDir * (HA.ActHeavyHit.frame + HA.ActHeavyHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActHeavyHit.frame - 1;
        m_dwFrameTime := HA.ActHeavyHit.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_BIGHIT, SM_PURSUEHIT: begin
        m_nStartFrame := HA.ActBigHit.start + m_btDir * (HA.ActBigHit.frame + HA.ActBigHit.skip);
        m_nEndFrame := m_nStartFrame + HA.ActBigHit.frame - 1;
        m_dwFrameTime := HA.ActBigHit.ftime;
        m_dwStartTime := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        if (m_nCurrentAction = SM_PURSUEHIT) then begin
          Effect := nil;
          case m_CurMagic.magfirelv div 4 of
            1: Effect := THeroCharEffect.Create(g_WMagic9Images, m_btDir * 10 + 00, 6, 125, Self);
            2: Effect := THeroCharEffect.Create(g_WMagic9Images, m_btDir * 10 + 90, 6, 125, Self);
            3: Effect := THeroCharEffect.Create(g_WMagic9Images, m_btDir * 10 + 180, 8, 110, Self);
          else
            Effect := THeroCharEffect.Create(g_WMagic6Images, m_btDir * 10 + 510, 9, 110, Self);
          end;
          //Effect := THeroCharEffect.Create(g_WMagic6Images, m_btDir * 10 + 510, 9, 115, Self);
          if Effect <> nil then
            g_PlayScene.m_EffectList.Add(Effect);
        end;
        Shift(m_btDir, 0, 0, 1);
      end;

    SM_SPELL: begin
        if m_CurMagic.EffectNumber in [104..114] then begin
          //DScreen.AddChatBoardString(format('EffectNumber=%d m_nEndFrame=%d', [m_CurMagic.EffectNumber, 1]), clWhite, clRed);
          CalcActorWinFrame();
          case m_CurMagic.EffectNumber of
            104: begin
                m_nSpellFrame := HA.ActMagic_104.frame;
                m_nStartFrame := HA.ActMagic_104.start + m_btDir * (HA.ActMagic_104.frame + HA.ActMagic_104.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_104.frame - 1;
                m_dwFrameTime := HA.ActMagic_104.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
              end;
            105: begin
                m_nSpellFrame := HA.ActMagic_105.frame;
                m_nStartFrame := HA.ActMagic_105.start + m_btDir * (HA.ActMagic_105.frame + HA.ActMagic_105.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_105.frame - 1;
                m_dwFrameTime := HA.ActMagic_105.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
                m_boNewMagic := True;
              end;
            106: begin
                m_nSpellFrame := HA.ActMagic_106.frame;
                m_nStartFrame := HA.ActMagic_106.start + m_btDir * (HA.ActMagic_106.frame + HA.ActMagic_106.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_106.frame - 1;
                m_dwFrameTime := HA.ActMagic_106.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
                m_boNewMagic := True;
              end;
            107: begin
                m_nSpellFrame := HA.ActMagic_107.frame;
                m_nStartFrame := HA.ActMagic_107.start + m_btDir * (HA.ActMagic_107.frame + HA.ActMagic_107.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_107.frame - 1;
                m_dwFrameTime := HA.ActMagic_107.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
                m_boNewMagic := True;
              end;

            108: begin
                m_nSpellFrame := HA.ActMagic_108.frame;
                m_nStartFrame := HA.ActMagic_108.start + m_btDir * (HA.ActMagic_108.frame + HA.ActMagic_108.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_108.frame - 1;
                m_dwFrameTime := HA.ActMagic_108.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
              end;
            109: begin
                m_nSpellFrame := HA.ActMagic_109.frame;
                m_nStartFrame := HA.ActMagic_109.start + m_btDir * (HA.ActMagic_109.frame + HA.ActMagic_109.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_109.frame - 1;
                m_dwFrameTime := HA.ActMagic_109.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
                m_boNewMagic := True;
              end;
            110: begin
                m_nSpellFrame := HA.ActMagic_110.frame;
                m_nStartFrame := HA.ActMagic_110.start + m_btDir * (HA.ActMagic_110.frame + HA.ActMagic_110.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_110.frame - 1;
                m_dwFrameTime := HA.ActMagic_110.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
                m_boNewMagic := True;
              end;
            111: begin
                m_nSpellFrame := HA.ActMagic_111.frame;
                m_nStartFrame := HA.ActMagic_111.start + m_btDir * (HA.ActMagic_111.frame + HA.ActMagic_111.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_111.frame - 1;
                m_dwFrameTime := HA.ActMagic_111.ftime;
                m_dwStartTime := GetTickCount;
                m_boNewMagic := True;
              end;
            112: begin
                m_nSpellFrame := HA.ActMagic_112.frame;
                m_nStartFrame := HA.ActMagic_112.start + m_btDir * (HA.ActMagic_112.frame + HA.ActMagic_112.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_112.frame - 1;
                m_dwFrameTime := HA.ActMagic_112.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
              end;
            113: begin
                m_nSpellFrame := HA.ActMagic_113.frame;
                m_nStartFrame := HA.ActMagic_113.start + m_btDir * (HA.ActMagic_113.frame + HA.ActMagic_113.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_113.frame - 1;
                m_dwFrameTime := HA.ActMagic_113.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
              end;
            114: begin
                m_nSpellFrame := HA.ActMagic_114.frame;
                m_nStartFrame := HA.ActMagic_114.start + m_btDir * (HA.ActMagic_114.frame + HA.ActMagic_114.skip);
                m_nEndFrame := m_nStartFrame + HA.ActMagic_114.frame - 1;
                m_dwFrameTime := HA.ActMagic_114.ftime;
                m_dwStartTime := GetTickCount;
                m_boSSkill := True;
              end;
          end;
          m_nCurEffFrame := 0;
          m_boUseMagic := True;
          m_boUseCboLib := True;
        end else begin
          m_nStartFrame := HA.ActSpell.start + m_btDir * (HA.ActSpell.frame + HA.ActSpell.skip);
          m_nEndFrame := m_nStartFrame + HA.ActSpell.frame - 1;
          m_dwFrameTime := HA.ActSpell.ftime;
          m_dwStartTime := GetTickCount;
          m_nCurEffFrame := 0;
          m_boUseMagic := True;
          //DScreen.AddChatBoardString(format('EffectNumber=%d m_nEndFrame=%d', [m_CurMagic.EffectNumber, 1]), clWhite, clRed);

          m_nSpellFrame := DEFSPELLFRAME;
          case m_CurMagic.EffectNumber of
            10: begin                   //灵魂火符
                m_nMagLight := 2;
                if m_CurMagic.spelllv > MAXMAGICLV then
                  m_nSpellFrame := 10;
              end;
            15: if m_CurMagic.spelllv > 3 then begin
                m_nMagLight := 2;
                m_nSpellFrame := 10;
              end;
            22: begin                   //地狱雷光
                m_nMagLight := 4;
                m_nSpellFrame := 10;
              end;
            26: begin                   //心灵启示
                m_nMagLight := 2;
                //m_nSpellFrame := 20;
                //m_dwFrameTime := m_dwFrameTime div 2;
                Effect := THeroCharEffect.Create(g_WMagicImages, 3960, 20, 65, Self);
                g_PlayScene.m_EffectList.Add(Effect);
              end;
            34: begin                   //灭天火
                m_nMagLight := 2;
                if m_CurMagic.spelllv > MAXMAGICLV then
                  m_nSpellFrame := 10;
              end;
            35: begin                   //无极真气
                m_nMagLight := 2;
                //m_nSpellFrame := 15;
                Effect := THeroCharEffect.Create(g_WMagic2Images, 160, 15, 65, Self);
                g_PlayScene.m_EffectList.Add(Effect);
              end;
            43: begin                   //狮子吼
                m_nMagLight := 3;
                //m_nSpellFrame := 20;
                Effect := THeroCharEffect.Create(g_WMagic2Images, 710, 20, 65, Self);
                g_PlayScene.m_EffectList.Add(Effect);
              end;
            121: begin
                m_nMagLight := 3;
                m_nSpellFrame := 10;
              end;
            120: begin
                m_nMagLight := 3;
                m_nSpellFrame := 12;
              end;
            122: begin
                m_nMagLight := 3;
                m_nSpellFrame := 8;
              end;
            116, 117: begin             //狮子吼
                m_nMagLight := 3;
                m_nSpellFrame := 10;
              end;
            124: begin
                m_nMagLight := 4;
                //FSpellFrame := 40;
                Effect := THeroCharEffect.Create(g_WMagic10Images, 110, 40, 65, Self);
                g_PlayScene.m_EffectList.Add(Effect);
              end;
            127: begin
                m_nMagLight := 3;
                //FSpellFrame := 18;
                Effect := THeroCharEffect.Create(g_WMagic10Images, 0, 18, 75, Self);
                g_PlayScene.m_EffectList.Add(Effect);
              end;
          else begin
              m_nMagLight := 2;
              m_nSpellFrame := DEFSPELLFRAME;
            end;
          end;
        end;

        m_dwWaitMagicRequest := GetTickCount;
        m_boWarMode := True;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    {SM_READYFIREHIT:
      begin
        startframe := HA.ActFireHitReady.start + dir * (HA.ActFireHitReady.frame + HA.ActFireHitReady.skip);
        m_nEndFrame := startframe + HA.ActFireHitReady.frame - 1;
        m_dwFrameTime := HA.ActFireHitReady.ftime;
        m_dwStartTime := GetTickCount;

        BoHitEffect := True;
        HitEffectNumber := 4;
        MagLight := 2;

        CurGlimmer := 0;
        MaxGlimmer := 6;

        WarMode := True;
        WarModeTime := GetTickCount;
        Shift(dir, 0, 0, 1);
      end;}
    SM_STRUCK: begin
        m_nStartFrame := HA.ActStruck.start + m_btDir * (HA.ActStruck.frame + HA.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + HA.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime;
        m_dwStartTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
        m_dwGenAnicountTime := GetTickCount;
        m_nCurBubbleStruck := 0;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := HA.ActDie.start + m_btDir * (HA.ActDie.frame + HA.ActDie.skip);
        m_nEndFrame := m_nStartFrame + HA.ActDie.frame - 1;
        m_dwFrameTime := HA.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

procedure THumActor.DefaultMotion;
var
  ndir, MaxIdx, frame       : Integer;
begin
  inherited DefaultMotion;
  if m_boUseCboLib then begin
    if (m_btEffect = 50) then begin
      if (m_nCurrentFrame <= 536) then begin
        if (GetTickCount - m_dwFrameTick) > 100 then begin
          if m_nFrame < 19 then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then
          m_HumWinSurface := g_WEffectImg.GetCachedImage(m_nCboHumWinOffSet + m_nFrame, m_nSpx, m_nSpy)
        else
          m_HumWinSurface := nil;
      end;
    end else if (m_btEffect <> 0) then begin
      //
      case m_nCurrentAction of
        SM_RUSHEX: begin
            frame := 8;
            ndir := 10;
            MaxIdx := 80;
          end;
        SM_SMITEHIT: begin
            frame := 15;
            ndir := 20;
            MaxIdx := 160;
          end;
        SM_SMITELONGHIT: if m_boSmiteLongHit = 2 then begin
            frame := 6;
            ndir := 10;
            MaxIdx := 320;
          end;
        SM_SMITELONGHIT3: begin
            frame := 6;
            ndir := 10;
            MaxIdx := 320;
          end;
        SM_SMITELONGHIT2, SM_SMITEWIDEHIT2: begin
            frame := 12;
            ndir := 20;
            MaxIdx := 400;
          end;
        SM_SMITEWIDEHIT: begin
            frame := 10;
            ndir := 10;
            MaxIdx := 560;
          end;
        SM_SPELL: begin
            case m_CurMagic.EffectNumber of
              104: begin
                  frame := 6;
                  ndir := 10;
                  MaxIdx := 640;
                end;
              112: begin
                  frame := 6;
                  ndir := 10;
                  MaxIdx := 720;
                end;
              106: begin
                  frame := 8;
                  ndir := 10;
                  MaxIdx := 800;
                end;
              107: begin
                  frame := 13;
                  ndir := 10;
                  MaxIdx := 1040;
                end;
              108: begin
                  frame := 6;
                  ndir := 10;
                  MaxIdx := 1200;
                end;
              109: begin
                  frame := 12;
                  ndir := 20;
                  MaxIdx := 1440;
                end;
              110: begin
                  frame := 12;
                  ndir := 20;
                  MaxIdx := 1600;
                end;
              111: begin
                  frame := 14;
                  ndir := 20;
                  MaxIdx := 1760;
                end;
              105 {112}: begin
                  frame := 10;
                  ndir := 10;
                  MaxIdx := 880;
                end;
            end;
          end;
      end;

      //dscreen.AddChatBoardString(inttostr(m_nCboHumWinOffSet), clBlue, clWhite);

      if m_nCurrentFrame < MaxIdx then begin
        if (GetTickCount - m_dwFrameTick) > HUMWINEFFECTTICK {200} then begin //Blue
          if m_nFrame < (frame - 1) then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then begin
          if m_nCboHumWinOffSet >= 68000 then begin
            case m_btEffect of
              41: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(24000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
              42: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(26000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
              43: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(40000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
              44: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(42000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
            else
              m_HumWinSurface := g_cboHumEffect3.GetCachedImage(m_nCboHumWinOffSet - 68000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
            end;
          end else if m_nCboHumWinOffSet >= 56000 then
            m_HumWinSurface := g_cboHumEffect2.GetCachedImage(m_nCboHumWinOffSet - 56000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy)
          else
            m_HumWinSurface := g_cboHumEffect.GetCachedImage(m_nCboHumWinOffSet + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
        end else m_HumWinSurface := nil;
      end else if not g_gcGeneral[6] then begin
        if m_nCboHumWinOffSet >= 68000 then begin
          case m_btEffect of
            41: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(24000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            42: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(26000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            43: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(40000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            44: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(42000 + m_nCurrentFrame, m_nSpx, m_nSpy);
          else
            m_HumWinSurface := g_cboHumEffect3.GetCachedImage(m_nCboHumWinOffSet - 68000 + m_nCurrentFrame, m_nSpx, m_nSpy);
          end;
        end else if m_nCboHumWinOffSet >= 56000 then
          m_HumWinSurface := g_cboHumEffect2.GetCachedImage(m_nCboHumWinOffSet - 56000 + m_nCurrentFrame, m_nSpx, m_nSpy)
        else
          m_HumWinSurface := g_cboHumEffect.GetCachedImage(m_nCboHumWinOffSet + m_nCurrentFrame, m_nSpx, m_nSpy);
      end else m_HumWinSurface := nil;
    end;
  end else begin
    if (m_btEffect = 50) then begin
      if (m_nCurrentFrame <= 536) then begin
        if (GetTickCount - m_dwFrameTick) > 100 then begin
          if m_nFrame < 19 then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then
          m_HumWinSurface := g_WEffectImg.GetCachedImage(m_nHumWinOffset + m_nFrame, m_nSpx, m_nSpy)
        else m_HumWinSurface := nil;
      end;
    end else if (m_btEffect <> 0) then begin
      if m_nCurrentFrame < 64 then begin
        if (GetTickCount - m_dwFrameTick) > HUMWINEFFECTTICK then begin //Blue
          if m_nFrame < 7 then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then begin
          if m_btEffect >= 35 then begin
            case m_btEffect of
              41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
            else
              m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
            end;
          end else if m_btEffect >= 20 then
            m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy)
          else
            m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
        end else m_HumWinSurface := nil;
      end else if not g_gcGeneral[6] then begin
        if m_btEffect >= 35 then begin
          case m_btEffect of
            41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + m_nCurrentFrame, m_nSpx, m_nSpy);
            42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + m_nCurrentFrame, m_nSpx, m_nSpy);
            43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + m_nCurrentFrame, m_nSpx, m_nSpy);
            45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + m_nCurrentFrame, m_nSpx, m_nSpy);
            46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + m_nCurrentFrame, m_nSpx, m_nSpy);
          else
            m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + m_nCurrentFrame, m_nSpx, m_nSpy);
          end;
        end else if m_btEffect >= 20 then
          m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + m_nCurrentFrame, m_nSpx, m_nSpy)
        else
          m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + m_nCurrentFrame, m_nSpx, m_nSpy);
      end else m_HumWinSurface := nil;
    end;
  end;
end;

function THumActor.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  if m_boDeath then
    Result := HA.ActDie.start + m_btDir * (HA.ActDie.frame + HA.ActDie.skip) + (HA.ActDie.frame - 1)
  else if wmode then begin
    Result := HA.ActWarMode.start + m_btDir * (HA.ActWarMode.frame + HA.ActWarMode.skip);
  end else begin
    m_nDefFrameCount := HA.ActStand.frame;
    if m_nCurrentDefFrame < 0 then
      cf := 0
    else if m_nCurrentDefFrame >= HA.ActStand.frame then
      cf := 0
    else
      cf := m_nCurrentDefFrame;
    Result := HA.ActStand.start + m_btDir * (HA.ActStand.frame + HA.ActStand.skip) + cf;
  end;
end;

procedure THumActor.RunFrameAction(frame: Integer);
var
  neff                      : TNormalDrawEffect;
  meff                      : TMapEffect;
  event                     : TClEvent;
  mfly                      : TFlyingAxe;
  HeroCharEffect            : TMagicEff;
begin
  m_boHideWeapon := False;
  if m_boSSkill then begin
    if (frame = 1) then begin
      m_boSSkill := False;
      //neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_cboEffect, 3990, 15, 60, True);
      HeroCharEffect := THeroCharEffect.Create(g_cboEffect, 3990, 15, 70, Self);
      if HeroCharEffect <> nil then g_PlayScene.m_EffectList.Add(HeroCharEffect);
    end;
  end else if m_nCurrentAction = SM_SMITELONGHIT then begin
    if (frame = 4) and (m_boSmiteLongHit = 2) then begin
      m_boSmiteLongHit := 0;
      g_SndMgr.PlaySound('Wav\cboZs3_start.wav');
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_cboEffect, m_btDir * 10 + 2003, 4, 115, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
      g_ShakeScreen.SetScrShake_X(2);
      g_ShakeScreen.SetScrShake_Y(2);
    end;
  end else if m_nCurrentAction = SM_SMITELONGHIT3 then begin
    if (frame = 4) then begin
      {PlaySoundName('Wav\cboZs3_start.wav');
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_cboEffect, m_btDir * 10 + 2003, 4, 115, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);}
      g_ShakeScreen.SetScrShake_X(3);
      g_ShakeScreen.SetScrShake_Y(3);
    end;
  end else if m_nCurrentAction = SM_SMITEWIDEHIT2 then begin
    if (frame = 8) and (m_boSmiteWideHitS2 > 0) then begin
      Dec(m_boSmiteWideHitS2);
      g_SndMgr.PlaySound('Wav\squarehit.wav');
    end;
    if (frame = 10) and m_boSmiteWideHit2 then begin
      m_boSmiteWideHit2 := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic2Images, 1391, 14, 75, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
      g_ShakeScreen.SetScrShake_X(4);
      g_ShakeScreen.SetScrShake_Y(3);
    end;
  end else if m_nCurrentAction = SM_SMITELONGHIT2 then begin
    if (frame = 8) and (m_boSmiteLongHitS2 > 0) then begin
      Dec(m_boSmiteLongHitS2);
      g_SndMgr.PlaySound('Wav\squarehit.wav');
    end;
    if (frame = 11) and m_boSmiteLongHit2 then begin
      m_boSmiteLongHit2 := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic5Images, m_btDir * 10 + 555, 5, 90, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
      g_ShakeScreen.SetScrShake_X(2);
      g_ShakeScreen.SetScrShake_Y(2);
    end;
  end else if m_nCurrentAction = SM_SMITEHIT then begin
    if (frame = 14) and (m_boSmiteHit) then begin
      m_boSmiteHit := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_cboEffect, m_btDir * 20 + 175, 2, 115, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
    end;
  end else if m_nCurrentAction = SM_HERO_LONGHIT2 then begin
    if (frame = 4) and (m_boHeroLongHit2) then begin
      m_boHeroLongHit2 := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic5Images, m_btDir * 10 + 555, 5, 85, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
    end;
  end else if m_nCurrentAction = SM_HERO_LONGHIT then begin
    if (frame = 4) and (m_boHeroLongHit) then begin
      m_boHeroLongHit := False;
      neff := TNormalDrawEffect.Create(m_nCurrX, m_nCurrY, g_WMagic5Images, m_btDir * 10 + 715, 5, 85, True);
      if neff <> nil then g_PlayScene.m_EffectList.Add(neff);
    end;
  end else if m_nCurrentAction = SM_HEAVYHIT then begin
    if (frame = 5) and (m_boDigFragment) then begin
      m_boDigFragment := False;
      meff := TMapEffect.Create(8 * m_btDir, 3, m_nCurrX, m_nCurrY);
      meff.ImgLib := g_WEffectImg;
      meff.NextFrameTime := 80;
      g_SndMgr.PlaySound(s_strike_stone, m_nCurrX, m_nCurrY);
      g_PlayScene.m_EffectList.Add(meff);
      event := EventMan.GetEvent(m_nCurrX, m_nCurrY, ET_PILESTONES);
      if event <> nil then
        event.m_nEventParam := event.m_nEventParam + 1;
    end;
  end else if m_nCurrentAction = SM_SITDOWN then begin
    if (frame = 5) and (m_boDigFragment) then begin
      m_boDigFragment := False;
      meff := TMapEffect.Create(8 * m_btDir, 3, m_nCurrX, m_nCurrY);
      meff.ImgLib := g_WEffectImg;
      meff.NextFrameTime := 80;
      g_SndMgr.PlaySound(s_strike_stone, m_nCurrX, m_nCurrY);
      g_PlayScene.m_EffectList.Add(meff);
      event := EventMan.GetEvent(m_nCurrX, m_nCurrY, ET_PILESTONES);
      if event <> nil then
        event.m_nEventParam := event.m_nEventParam + 1;
    end;
  end else if m_nCurrentAction = SM_THROW then begin
    if (frame = 3) and (m_boThrow) then begin
      m_boThrow := False;
      mfly := TFlyingAxe(g_PlayScene.NewFlyObject(Self,
        m_nCurrX,
        m_nCurrY,
        m_nTargetX,
        m_nTargetY,
        m_nTargetRecog,
        mtFlyAxe));
      if mfly <> nil then begin
        TFlyingAxe(mfly).ReadyFrame := 40;
        mfly.ImgLib := g_WMon3Img;
        mfly.FlyImageBase := FLYOMAAXEBASE;
      end;
    end;
    if frame >= 3 then
      m_boHideWeapon := True;
  end;
end;

procedure THumActor.DoWeaponBreakEffect;
begin
  m_boWeaponEffect := True;
  m_nCurWeaponEffect := 0;
end;

procedure THumActor.Run;
var
  i, off, prv, hprv         : Integer;
  dwFrameTimetime           : LongWord;
  boFly                     : Boolean;
  bss, sskill               : Boolean;
  mx, my                    : Integer;
  fAddNewMagic              : Boolean;

  function MagicTimeOut: Boolean;
  begin
    if (Self = g_MySelf) then begin
      Result := GetTickCount - m_dwWaitMagicRequest > 1800;
    end else begin
      Result := GetTickCount - m_dwWaitMagicRequest > 850 + Byte(bss) * 50;
    end;
    if not bss and Result then
      m_CurMagic.ServerMagicCode := 0;
  end;

begin

  if GetTickCount - m_dwGenAnicountTime > 120 then begin
    m_dwGenAnicountTime := GetTickCount;
    Inc(m_nGenAniCount);
    if m_nGenAniCount > 100000 then m_nGenAniCount := 0;
    Inc(m_nCurBubbleStruck);
  end;
  if m_boWeaponEffect then begin
    if GetTickCount - m_dwWeaponpEffectTime > 120 then begin
      m_dwWeaponpEffectTime := GetTickCount;
      Inc(m_nCurWeaponEffect);
      if m_nCurWeaponEffect >= MAXWPEFFECTFRAME then
        m_boWeaponEffect := False;
    end;
  end;

  if (m_nCurrentAction = SM_RUSHEX) then begin
    RunActSound(m_nCurrentFrame - m_nStartFrame);
    Exit;
  end;

  if (m_nCurrentAction in [5..7, 9, 11, 13, 39]) then Exit;

  m_boMsgMuch := (Self <> g_MySelf) and (m_MsgList.count >= 2);
  bss := m_CurMagic.EffectNumber in [105..107, 109..111];

  off := m_nCurrentFrame - m_nStartFrame;
  RunActSound(off);
  RunFrameAction(off);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    //m_nCurrentAction = SM_ALIVE
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if m_boMsgMuch then begin
      if m_boUseCboLib then begin
        if m_btIsHero = 1 then
          dwFrameTimetime := Round(m_dwFrameTime / 1.50)
        else
          dwFrameTimetime := Round(m_dwFrameTime / 1.55);
      end else
        dwFrameTimetime := Round(m_dwFrameTime / 1.7);
    end else if (Self <> g_MySelf) and (m_boUseMagic) then begin
      if m_boUseCboLib then begin
        if m_btIsHero = 1 then
          dwFrameTimetime := Round(m_dwFrameTime / 1.28)
        else
          dwFrameTimetime := Round(m_dwFrameTime / 1.32);
      end else
        dwFrameTimetime := Round(m_dwFrameTime / 1.38);
    end else
      dwFrameTimetime := m_dwFrameTime;

    if g_boSpeedRate then dwFrameTimetime := _Max(0, dwFrameTimetime - _MIN(10, g_MoveSpeedRate));

    if GetTickCount - m_dwStartTime > dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        if m_boUseMagic then begin      //魔法效果...
          if (m_nCurEffFrame = m_nSpellFrame - 2) or MagicTimeOut() then begin
            if (m_CurMagic.ServerMagicCode >= 0) or MagicTimeOut() then begin
              Inc(m_nCurrentFrame);
              Inc(m_nCurEffFrame);
              m_dwStartTime := GetTickCount;
            end;
          end else begin
            if m_nCurrentFrame < m_nEndFrame - 1 then
              Inc(m_nCurrentFrame);
            Inc(m_nCurEffFrame);
            m_dwStartTime := GetTickCount;
          end;
        end else begin
          Inc(m_nCurrentFrame);
          m_dwStartTime := GetTickCount;
        end;
        ReadyNextAction();
      end else begin
        if Self = g_MySelf then begin
          if frmMain.ServerAcceptNextAction then begin
            ActionEnded();
            m_nCurrentAction := 0;
            m_boUseMagic := False;
            m_boUseCboLib := False;
          end;
        end else begin
          ActionEnded();
          m_nCurrentAction := 0;
          m_boUseMagic := False;
          m_boUseCboLib := False;
        end;
        m_boHitEffect := False;

        if m_boSmiteLongHit = 1 then begin
          m_boSmiteLongHit := 2;
          CalcActorWinFrame();

          m_nStartFrame := HA.ActSmiteLongHit2.start + m_btDir * (HA.ActSmiteLongHit2.frame + HA.ActSmiteLongHit2.skip);
          m_nEndFrame := m_nStartFrame + HA.ActSmiteLongHit2.frame - 1;
          m_dwFrameTime := HA.ActSmiteLongHit2.ftime;
          m_dwStartTime := GetTickCount;
          m_nMaxTick := HA.ActSmiteLongHit2.usetick;
          m_nCurTick := 0;
          Shift(m_btDir, 0, 0, 1);

          m_boWarMode := True;
          m_dwWarModeTime := GetTickCount;
          m_boUseCboLib := True;

          m_boHitEffect := True;
          m_nCurrentAction := SM_SMITELONGHIT;
        end;

      end;
      if m_boUseMagic then begin
        //bss := m_CurMagic.EffectNumber in [105..107, 109..111];
        if bss and (m_CurMagic.ServerMagicCode > 0) then begin
          //i := m_nCurrentFrame - m_nStartFrame;
          case m_CurMagic.EffectNumber of
            105, 106: sskill := m_nCurEffFrame = 7;
            107, 109: sskill := m_nCurEffFrame = 9;
            110: sskill := m_nCurEffFrame in [6, 8, 10];
            111: sskill := m_nCurEffFrame = 10;
            //116, 117: sskill := m_nCurEffFrame = 6;
          end;
          if sskill then begin
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
                boFly,
                magfirelv,
                Poison);
              if boFly then begin
                g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY)
              end else begin
                //if m_CurMagic.EffectNumber <> 116 then
                g_SndMgr.PlaySound(m_nMagicExplosionSound, targx, targy);
              end;
            end;
            m_boNewMagic := False;
          end;
        end;

        fAddNewMagic := False;
        case m_CurMagic.EffectNumber of
          127: fAddNewMagic := m_nCurEffFrame = 7;
        else
          fAddNewMagic := m_nCurEffFrame = m_nSpellFrame - 1;
        end;

        if fAddNewMagic then begin
          if (m_CurMagic.ServerMagicCode > 0) and (not bss or m_boNewMagic) then begin
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
                boFly,
                magfirelv,
                Poison);
              if boFly then begin
                g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY)
              end else begin
                //DScreen.AddChatBoardString(inttostr(m_nMagicExplosionSound), GetRGB(219), clWhite);
                if m_CurMagic.EffectNumber <> 116 then
                  g_SndMgr.PlaySound(m_nMagicExplosionSound, targx, targy);
              end;

            end;
          end;
          if Self = g_MySelf then begin
            g_dwLatestSpellTick := GetTickCount;
          end;
          m_CurMagic.ServerMagicCode := 0;
        end;
      end;
    end;

    if m_btRace = 0 then
      m_nCurrentDefFrame := 0
    else
      m_nCurrentDefFrame := -10;
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

function THumActor.light: Integer;
var
  L                         : Integer;
begin
  L := m_nChrLight;
  if L < m_nMagLight then begin
    if m_boUseMagic or m_boHitEffect then
      L := m_nMagLight;
  end;
  Result := L;
end;

procedure THumActor.LoadSurface;
var
  ndir, MaxIdx, frame       : Integer;
label
  labWin, labHair;
begin
  //cbo ok
  //if (m_nCurrentAction = CM_SMITELONGHIT) then begin
  //  DScreen.AddChatBoardString(format('CurrentFrame=%d', [m_nCurrentFrame]), clWhite, clBlack);
  //end;
  if m_boUseCboLib and (m_nCurrentAction = SM_SMITELONGHIT) and (m_boSmiteLongHit = 1) then begin
    m_BodySurface := frmMain.GetWHumImg(m_btDress, m_btSex, GetDefaultFrame(True), m_nPx, m_nPy, False);
    if m_BodySurface = nil then begin
      m_BodySurface := frmMain.GetWHumImg(0, m_btSex, GetDefaultFrame(True), m_nPx, m_nPy, False);
    end;
  end else begin
    m_BodySurface := frmMain.GetWHumImg(m_btDress, m_btSex, m_nCurrentFrame, m_nPx, m_nPy, m_boUseCboLib);
    if m_BodySurface = nil then begin
      m_BodySurface := frmMain.GetWHumImg(0, m_btSex, m_nCurrentFrame, m_nPx, m_nPy, m_boUseCboLib);
    end;
  end;

  //m_HairSurface
  if g_UseItems[U_DRESS].s.Source <>  100  then begin //20201018增加衣服Source=100时候不显示发型外观
  if m_boUseCboLib and (m_nCurrentAction = SM_SMITELONGHIT) and (m_boSmiteLongHit = 1) then begin
    if m_nHairOffsetEx >= 0 then
      if g_gcGeneral[7] then  begin
         if m_nHairOffset >= 0 then m_HairSurface := g_WHairImgImages.GetCachedImage(m_nHairOffset + GetDefaultFrame(True), m_nHpx, m_nHpy)
         else m_HairSurface := nil;
      end else m_HairSurface := g_WHair2ImgImages.GetCachedImage(m_nHairOffsetEx + GetDefaultFrame(True), m_nHpx, m_nHpy)
    else if m_nHairOffset >= 0 then
      m_HairSurface := g_WHairImgImages.GetCachedImage(m_nHairOffset + GetDefaultFrame(True), m_nHpx, m_nHpy)
    else
      m_HairSurface := nil;

  end else begin
    if m_boUseCboLib then begin
      if m_nCboHairOffset >= 0 then
        m_HairSurface := g_cbohair.GetCachedImage(m_nCboHairOffset + m_nCurrentFrame, m_nHpx, m_nHpy)
      else
        m_HairSurface := nil;
    end else begin
      labHair:
      if m_nHairOffsetEx >= 0 then begin
        {if (m_btHairEx = 6) and (m_btSex <> 0) then begin
          m_HairSurface := g_WHair2ImgImages.GetCachedImage(m_nHairOffsetEx + m_nCurrentFrame, m_nHpx, m_nHpy);
          m_nHpx := m_nHpx + 24;
          m_nHpy := m_nHpy - 14;
        end else}
        if g_gcGeneral[7] then begin
          if m_nHairOffset >= 0 then m_HairSurface := g_WHairImgImages.GetCachedImage(m_nHairOffset + m_nCurrentFrame, m_nHpx, m_nHpy)
          else  m_HairSurface := nil;
        end else m_HairSurface := g_WHair2ImgImages.GetCachedImage(m_nHairOffsetEx + m_nCurrentFrame, m_nHpx, m_nHpy)
      end else if m_nHairOffset >= 0 then
        m_HairSurface := g_WHairImgImages.GetCachedImage(m_nHairOffset + m_nCurrentFrame, m_nHpx, m_nHpy)
      else
        m_HairSurface := nil;
    end;
  end;
  end else begin
      if g_gcGeneral[7] then begin
      if m_nHairOffset >= 0 then m_HairSurface := g_WHairImgImages.GetCachedImage(m_nHairOffset + m_nCurrentFrame, m_nHpx, m_nHpy)
      else  m_HairSurface := nil;
    end else
     m_HairSurface := nil
  end;

  //m_HumWinSurface
  if m_boUseCboLib and (m_nCurrentAction = SM_SMITELONGHIT) and (m_boSmiteLongHit = 1) then begin
    if (m_btEffect = 50) then begin
      if (GetDefaultFrame(True) <= 536) then begin
        if (GetTickCount - m_dwFrameTick) > 100 then begin
          if m_nFrame < 19 then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then
          m_HumWinSurface := g_WEffectImg.GetCachedImage(m_nHumWinOffset + m_nFrame, m_nSpx, m_nSpy)
        else m_HumWinSurface := nil;
      end;
    end else if (m_btEffect <> 0) then begin
      if GetDefaultFrame(True) < 64 then begin
        if (GetTickCount - m_dwFrameTick) > HUMWINEFFECTTICK {200} then begin //Blue
          if m_nFrame < 7 then
            Inc(m_nFrame)
          else
            m_nFrame := 0;
          m_dwFrameTick := GetTickCount();
        end;
        if not g_gcGeneral[6] then begin
          if m_btEffect >= 35 then begin
            case m_btEffect of
              41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
            else
              m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
            end;
          end else if m_btEffect >= 20 then
            m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy)
          else
            m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
        end else m_HumWinSurface := nil;
      end else if not g_gcGeneral[6] then begin
        if m_btEffect >= 35 then begin
          case m_btEffect of
            41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + m_nCurrentFrame, m_nSpx, m_nSpy);
            42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + m_nCurrentFrame, m_nSpx, m_nSpy);
            43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + m_nCurrentFrame, m_nSpx, m_nSpy);
            45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + m_nCurrentFrame, m_nSpx, m_nSpy);
            46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + m_nCurrentFrame, m_nSpx, m_nSpy);
          else
            m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + m_nCurrentFrame, m_nSpx, m_nSpy);
          end;
        end else if m_btEffect >= 20 then
          m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + m_nCurrentFrame, m_nSpx, m_nSpy)
        else
          m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + GetDefaultFrame(True), m_nSpx, m_nSpy);
      end else m_HumWinSurface := nil;
    end;
  end else begin
    if m_boUseCboLib then begin
      if (m_btEffect = 50) then begin
        if (m_nCurrentFrame <= 536) then begin
          if (GetTickCount - m_dwFrameTick) > 100 then begin
            if m_nFrame < 19 then
              Inc(m_nFrame)
            else
              m_nFrame := 0;
            m_dwFrameTick := GetTickCount();
          end;
          if not g_gcGeneral[6] then
            m_HumWinSurface := g_WEffectImg.GetCachedImage(m_nCboHumWinOffSet + m_nFrame, m_nSpx, m_nSpy)
          else m_HumWinSurface := nil;
        end;
      end else if (m_btEffect <> 0) then begin
        //
        case m_nCurrentAction of
          SM_RUSHEX: begin
              frame := 8;
              ndir := 10;
              MaxIdx := 80;
            end;
          SM_SMITEHIT: begin
              frame := 15;
              ndir := 20;
              MaxIdx := 160;
            end;
          SM_SMITELONGHIT: if m_boSmiteLongHit = 2 then begin
              frame := 6;
              ndir := 10;
              MaxIdx := 320;
            end;
          SM_SMITELONGHIT3: begin
              frame := 6;
              ndir := 10;
              MaxIdx := 320;
            end;
          SM_SMITELONGHIT2, SM_SMITEWIDEHIT2: begin
              frame := 12;
              ndir := 20;
              MaxIdx := 400;
            end;
          SM_SMITEWIDEHIT: begin
              frame := 10;
              ndir := 10;
              MaxIdx := 560;
            end;
          SM_SPELL: begin
              case m_CurMagic.EffectNumber of
                104: begin
                    frame := 6;
                    ndir := 10;
                    MaxIdx := 640;
                  end;
                112: begin
                    frame := 6;
                    ndir := 10;
                    MaxIdx := 720;
                  end;
                106: begin
                    frame := 8;
                    ndir := 10;
                    MaxIdx := 800;
                  end;
                107: begin
                    frame := 13;
                    ndir := 10;
                    MaxIdx := 1040;
                  end;
                108: begin
                    frame := 6;
                    ndir := 10;
                    MaxIdx := 1200;
                  end;
                109: begin
                    frame := 12;
                    ndir := 20;
                    MaxIdx := 1440;
                  end;
                110: begin
                    frame := 12;
                    ndir := 20;
                    MaxIdx := 1600;
                  end;
                111: begin
                    frame := 14;
                    ndir := 20;
                    MaxIdx := 1760;
                  end;
                105 {112}: begin
                    frame := 10;
                    ndir := 10;
                    MaxIdx := 880;
                  end;
              end;
            end;
        end;

        if m_nCurrentFrame < MaxIdx then begin
          if (GetTickCount - m_dwFrameTick) > HUMWINEFFECTTICK {200} then begin //Blue
            if m_nFrame < (frame - 1) then
              Inc(m_nFrame)
            else
              m_nFrame := 0;
            m_dwFrameTick := GetTickCount();
          end;
          if not g_gcGeneral[6] then begin
            if m_nCboHumWinOffSet >= 68000 then begin
              case m_btEffect of
                41: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(24000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
                42: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(26000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
                43: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(40000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
                44: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(42000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
                45: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(44000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
                46: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(46000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
              else
                m_HumWinSurface := g_cboHumEffect3.GetCachedImage(m_nCboHumWinOffSet - 68000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
              end;
            end else if m_nCboHumWinOffSet >= 56000 then
              m_HumWinSurface := g_cboHumEffect2.GetCachedImage(m_nCboHumWinOffSet - 56000 + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy)
            else
              m_HumWinSurface := g_cboHumEffect.GetCachedImage(m_nCboHumWinOffSet + (m_btDir * ndir) + m_nFrame, m_nSpx, m_nSpy);
          end else m_HumWinSurface := nil;
        end else if not g_gcGeneral[6] then begin
          if m_nCboHumWinOffSet >= 68000 then begin
            case m_btEffect of
              41: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(24000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              42: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(26000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              43: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(40000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              44: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(42000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              45: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(44000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              46: m_HumWinSurface := g_cboHumEffect3.GetCachedImage(46000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            else
              m_HumWinSurface := g_cboHumEffect3.GetCachedImage(m_nCboHumWinOffSet - 68000 + m_nCurrentFrame, m_nSpx, m_nSpy);
            end;
          end else if m_nCboHumWinOffSet >= 56000 then
            m_HumWinSurface := g_cboHumEffect2.GetCachedImage(m_nCboHumWinOffSet - 56000 + m_nCurrentFrame, m_nSpx, m_nSpy)
          else
            m_HumWinSurface := g_cboHumEffect.GetCachedImage(m_nCboHumWinOffSet + m_nCurrentFrame, m_nSpx, m_nSpy);
        end else m_HumWinSurface := nil;
      end;
    end else begin
      if (m_btEffect = 50) then begin
        if (m_nCurrentFrame <= 536) then begin
          if (GetTickCount - m_dwFrameTick) > 100 then begin
            if m_nFrame < 19 then
              Inc(m_nFrame)
            else
              m_nFrame := 0;
            m_dwFrameTick := GetTickCount();
          end;
          if not g_gcGeneral[6] then
            m_HumWinSurface := g_WEffectImg.GetCachedImage(m_nHumWinOffset + m_nFrame, m_nSpx, m_nSpy)
          else m_HumWinSurface := nil;
        end;
      end else if (m_btEffect <> 0) then begin
        if m_nCurrentFrame < 64 then begin
          if (GetTickCount - m_dwFrameTick) > HUMWINEFFECTTICK {200} then begin //Blue
            if m_nFrame < 7 then
              Inc(m_nFrame)
            else
              m_nFrame := 0;
            m_dwFrameTick := GetTickCount();
          end;
          if not g_gcGeneral[6] then begin
            if m_btEffect >= 35 then begin
              case m_btEffect of
                41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
                42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
                43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
                44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
                45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
                46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              else
                m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
              end;
            end else if m_btEffect >= 20 then
              m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy)
            else
              m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + (m_btDir * 8) + m_nFrame, m_nSpx, m_nSpy);
          end else m_HumWinSurface := nil;
        end else if not g_gcGeneral[6] then begin
          if m_btEffect >= 35 then begin
            case m_btEffect of
              41: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7200 + m_nCurrentFrame, m_nSpx, m_nSpy);
              42: m_HumWinSurface := g_WHumEffect3.GetCachedImage(7800 + m_nCurrentFrame, m_nSpx, m_nSpy);
              43: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12000 + m_nCurrentFrame, m_nSpx, m_nSpy);
              44: m_HumWinSurface := g_WHumEffect3.GetCachedImage(12600 + m_nCurrentFrame, m_nSpx, m_nSpy);
              45: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13200 + m_nCurrentFrame, m_nSpx, m_nSpy);
              46: m_HumWinSurface := g_WHumEffect3.GetCachedImage(13800 + m_nCurrentFrame, m_nSpx, m_nSpy);
            else
              m_HumWinSurface := g_WHumEffect3.GetCachedImage(m_nHumWinOffset - 20400 + m_nCurrentFrame, m_nSpx, m_nSpy);
            end;
          end else if m_btEffect >= 20 then
            m_HumWinSurface := g_WHumEffect2.GetCachedImage(m_nHumWinOffset - 12000 + m_nCurrentFrame, m_nSpx, m_nSpy)
          else
            m_HumWinSurface := g_WHumWingImages.GetCachedImage(m_nHumWinOffset + m_nCurrentFrame, m_nSpx, m_nSpy);
        end else m_HumWinSurface := nil;
      end;
    end;
  end;

  if g_UseItems[U_DRESS].s.Source <>  100  then begin //20201018增加衣服Source=100时候不显示原始武器外观
  if m_boUseCboLib and (m_nCurrentAction = SM_SMITELONGHIT) and (m_boSmiteLongHit = 1) then begin
    m_WeaponSurface := frmMain.GetWWeaponImg(m_btWeapon, m_btSex, GetDefaultFrame(True), m_nWpx, m_nWpy, False);
    if m_WeaponSurface = nil then
      m_WeaponSurface := frmMain.GetWWeaponImg(0, m_btSex, GetDefaultFrame(True), m_nWpx, m_nWpy, False);

    m_WeaponEffect := nil;
    if m_btWeaponEffect > 0 then begin
      //if (m_btWeaponEffect <> 2) or (m_btWeapon in [136, 137]) then
      m_WeaponEffect := frmMain.GetWWeaponEffectImg(Self, m_btWeaponEffect, m_btWeapon, m_btSex, GetDefaultFrame(True), m_nWpeX, m_nWpeY, m_boUseCboLib);
    end;
  end else begin
    m_WeaponSurface := frmMain.GetWWeaponImg(m_btWeapon, m_btSex, m_nCurrentFrame, m_nWpx, m_nWpy, m_boUseCboLib);
    if m_WeaponSurface = nil then
      m_WeaponSurface := frmMain.GetWWeaponImg(0, m_btSex, m_nCurrentFrame, m_nWpx, m_nWpy, m_boUseCboLib);

    m_WeaponEffect := nil;
    if m_btWeaponEffect > 0 then begin
      //DScreen.AddChatBoardString(Format('%d:%d', [m_btWeaponEffect, m_nCurEffFrame]), GetRGB(5), clWhite);
      //if (m_btWeaponEffect <> 2) or (m_btWeapon in [136, 137]) then
      m_WeaponEffect := frmMain.GetWWeaponEffectImg(Self, m_btWeaponEffect, m_btWeapon, m_btSex, m_nCurrentFrame, m_nWpeX, m_nWpeY, m_boUseCboLib);
    end;
  end;
  end else begin
   m_WeaponSurface := nil
  end;
end;

procedure THumActor.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  bWarMode, bWeapon         : Boolean;
  nOff, Idx, ax, ay         : Integer;
  Effect                    : TMagicEff;
  d, dd                     : TCustomLockableTexture;
  ceff                      : TColorEffect;
  wimg                      : TWMImages;
  ShiftX, ShiftY            : Integer;
begin
  d := nil;
  if not (m_btDir in [0..7]) then Exit;

  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
  //if m_sUserName = '' then Exit;  //1015

  if (Self <> g_MySelf) and (Self <> g_MySelf.m_HeroObject) and (m_sUserName = '') and (GetTickCount - m_dwSendQueryUserNameTime > 120 * 1000) then begin
    m_dwSendQueryUserNameTime := GetTickCount;
    frmMain.SendQueryUserName(m_nRecogId, m_nCurrX, m_nCurrY);
  end;

  if m_fHideMode then
    Exit;

  ceff := GetDrawEffectValue;

  if m_boUseCboLib and ((m_nState and $04000000 <> 0) or (m_nState and $02000000 <> 0)) then m_boUseCboLib := False;

  ShiftX := dx + m_nShiftX;
  ShiftY := dy + m_nShiftY;

  //Stall
  if DrawOnSale and m_StallMgr.OnSale then begin
    if not m_btDir in [1, 3, 5, 7] then m_btDir := 5;
    dd := nil;
    case m_btDir of
      1: begin
          case m_StallMgr.StallType of
            0: begin
                ax := -20;
                ay := -22;
                dd := g_opui.Images[g_StallLooks+10];
              end;
            1: begin
                ax := -6;
                ay := -40;
                dd := g_opui.Images[g_StallLooks+14];
              end;
            2: begin
                ax := -18;
                ay := -54;
               dd := g_opui.Images[g_StallLooks+18];
              end;
          end;
        end;
      3: begin
          case m_StallMgr.StallType of
            0: begin
                ax := -25;
                ay := -10;
                dd := g_opui.Images[g_StallLooks+7];
              end;
          end;
        end;
      5: begin
          case m_StallMgr.StallType of
            0: begin
                ax := -47;
                ay := -10;
                dd := g_opui.Images[g_StallLooks+9];
              end;
          end;
        end;
      7: begin
          case m_StallMgr.StallType of
            0: begin
                ax := -52;
                ay := -30;
                dd := g_opui.Images[g_StallLooks+8];
              end;
            1: begin
                ax := -46;
                ay := -44;
                dd := g_opui.Images[g_StallLooks+12];
              end;
            2: begin
                ax := -56;
                ay := -48;
                dd := g_opui.Images[g_StallLooks+16];
              end;
          end;
        end;
    end;
    if dd <> nil then begin
      dsurface.Draw(ShiftX + ax, ShiftY + ay, dd.ClientRect, dd, True);
    end;
  end;

  if (m_nCurrentFrame >= 0) and (m_nCurrentFrame <= 599) then
    m_nWpord := WORDER[m_btSex, m_nCurrentFrame];

  if (m_btEffect <> 0) and (m_HumWinSurface <> nil) then begin
    bWarMode := (m_btDir = 2) and m_boWarMode and IsIdle; //m_btEffect <> 0
    if g_MySelf = Self then begin
      if blend then begin
        if ((m_btDir in [3..5]) or bWarMode) and not boFlag then begin
          dsurface.DrawBlend(
            ShiftX + m_nSpx,
            ShiftY + m_nSpy,
            m_HumWinSurface,
            1);
        end else if ((m_btDir in [3..5]) or bWarMode) and boFlag then begin
          dsurface.DrawBlend(
            ShiftX + m_nSpx,
            ShiftY + m_nSpy,
            m_HumWinSurface,
            1);
        end;
      end;
    end else begin
      if ((g_FocusCret <> nil) or (g_MagicTarget <> nil)) and blend and ((m_btDir in [3..5]) or bWarMode) and not boFlag then begin
        dsurface.DrawBlend(
          ShiftX + m_nSpx,
          ShiftY + m_nSpy,
          m_HumWinSurface,
          1);
      end else if ((m_btDir in [3..5]) or bWarMode) and boFlag then begin
        dsurface.DrawBlend(
          ShiftX + m_nSpx,
          ShiftY + m_nSpy,
          m_HumWinSurface,
          1);
      end;
    end;
  end;

  //g_cboweapon  正反面
  bWeapon := (m_btWeapon >= 2) and (m_WeaponSurface <> nil) and (not m_boHideWeapon);
  if bWeapon and (m_nWpord = 0)  then begin
    DrawEffSurface(dsurface, m_WeaponSurface, ShiftX + m_nWpx, ShiftY + m_nWpy, blend, ceNone);
    //DrawWeaponGlimmer(dsurface, dx + m_nShiftX, dy + m_nShiftY);
    if (m_WeaponEffect <> nil) then
      dsurface.DrawBlend(
        ShiftX + m_nWpeX,
        ShiftY + m_nWpeY,
        m_WeaponEffect,
        1);
  end;

  //g_cbohum
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface, m_BodySurface, ShiftX + m_nPx, ShiftY + m_nPy, blend, ceff);
  end;

  //g_cbohair
  if m_HairSurface <> nil then
    DrawEffSurface(dsurface, m_HairSurface, ShiftX + m_nHpx, ShiftY + m_nHpy, blend, ceff);

  //g_cboweapon 正反面

  if (m_nWpord = 1) and bWeapon then begin
    DrawEffSurface(dsurface, m_WeaponSurface, ShiftX + m_nWpx, ShiftY + m_nWpy, blend, ceNone);
    //DrawWeaponGlimmer(dsurface, dx + m_nShiftX, dy + m_nShiftY);
    if (m_WeaponEffect <> nil) then
      dsurface.DrawBlend(
        ShiftX + m_nWpeX,
        ShiftY + m_nWpeY,
        m_WeaponEffect,
        1);
  end;

  //g_cboHumEffect
  if (m_btEffect = 50) then begin
    if (m_HumWinSurface <> nil) then
      dsurface.DrawBlend(
        ShiftX + m_nSpx,
        ShiftY + m_nSpy,
        m_HumWinSurface,
        1);
  end else if (m_btEffect <> 0) and (m_HumWinSurface <> nil) then begin
    if g_MySelf = Self then begin
      if blend then begin
        if not bWarMode and (m_btDir in [0..2, 6, 7]) and not boFlag then begin
          dsurface.DrawBlend(
            ShiftX + m_nSpx,
            ShiftY + m_nSpy,
            m_HumWinSurface,
            1);
        end else if not bWarMode and (m_btDir in [0..2, 6, 7]) and boFlag then begin
          dsurface.DrawBlend(
            ShiftX + m_nSpx,
            ShiftY + m_nSpy,
            m_HumWinSurface,
            1);
        end;
      end;
    end else begin
      if ((g_FocusCret <> nil) or (g_MagicTarget <> nil)) and not bWarMode and (m_btDir in [0..2, 6, 7]) and not boFlag then begin
        dsurface.DrawBlend(
          ShiftX + m_nSpx,
          ShiftY + m_nSpy,
          m_HumWinSurface,
          1);
      end else begin
        if not bWarMode and (m_btDir in [0..2, 6, 7]) and boFlag then begin
          dsurface.DrawBlend(
            ShiftX + m_nSpx,
            ShiftY + m_nSpy,
            m_HumWinSurface,
            1);
        end;
      end;
    end;
  end;

  if not m_boDeath and not m_boUseCboLib then begin
    if m_nState and $00100000 <> 0 then begin //显示魔法盾时效果
      if (m_nCurrentAction = SM_STRUCK) and (m_nCurBubbleStruck < 3) then
        Idx := MAGBUBBLESTRUCKBASE + m_nCurBubbleStruck
      else
        Idx := MAGBUBBLEBASE + (m_nGenAniCount mod 3);
      d := g_WMagicImages.GetCachedImage(Idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend( ShiftX + ax, ShiftY + ay, d, 1);
    end;
    if m_nState and $00080000 <> 0 then begin //显示4级魔法盾时效果
      if (m_nCurrentAction = SM_STRUCK) and (m_nCurBubbleStruck < 6) then
        Idx := 720
      else
        Idx := 730 + (m_nGenAniCount mod 2);
      d := g_WMagic6Images.GetCachedImage(Idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend( ShiftX + ax, ShiftY + ay, d, 1);
    end;

    //其他附身效果
    if m_nState and $00040000 <> 0 then begin
      Idx := 2040 + (m_nGenAniCount mod 8);
      d := g_WMagic8Images2.GetCachedImage(Idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend(ShiftX + ax, ShiftY + ay, d, 1);
    end;
    if m_nState and $00020000 <> 0 then begin //龙化
      Idx := 160 + (m_nGenAniCount mod 26);
      d := g_WMagic10Images.GetCachedImage(Idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend(ShiftX + ax, ShiftY + ay, d, 1);
    end;

  end;
  //end;

  if DrawOnSale and m_StallMgr.OnSale then begin
    if not m_btDir in [1, 3, 5, 7] then m_btDir := 5;
    dd := nil;
    case m_btDir of
      3: begin
          case m_StallMgr.StallType of
            1: begin
                ax := -8;
                ay := -24;
                dd := g_opui.Images[g_StallLooks+11];
              end;
            2: begin
                ax := -16;
                ay := -20;
                dd := g_opui.Images[g_StallLooks+15];
              end;
          end;
        end;
      5: begin
          case m_StallMgr.StallType of
            1: begin
                ax := -48;
                ay := -18;
                dd := g_opui.Images[g_StallLooks+13];
              end;
            2: begin
                ax := -50;
                ay := -18;
                dd := g_opui.Images[g_StallLooks+17];
              end;
          end;
        end;
    end;
    if dd <> nil then begin
      dsurface.Draw(ShiftX + ax, ShiftY + ay, dd.ClientRect, dd, True);
    end;
  end;

  if m_boUseMagic and (m_CurMagic.EffectNumber > 0) then begin //sm_spell
    if m_nCurEffFrame in [0..m_nSpellFrame - 1] then begin
      if m_boUseCboLib then begin
        GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, Idx);
        d := nil;
        if wimg <> nil then begin
          Idx := Idx + m_nCurEffFrame;
          d := wimg.GetCachedImage(Idx, ax, ay);
        end;
        if d <> nil then dsurface.DrawBlend( ShiftX + ax, ShiftY + ay, d, 1);
      end else begin
        if (m_CurMagic.EffectNumber in [29]) then begin
          if m_CurMagic.spelllv > MAXMAGICLV then
            GetEffectBase(m_CurMagic.EffectNumber - 1 + 500, 0, wimg, Idx)
          else
            GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, Idx);
        end else begin
          if (m_CurMagic.EffectNumber in [4, 8..12, 15, 20, 21, 28, 31, 34, 48, 51]) then begin
            case m_CurMagic.spelllv div 4 of
              1: begin
                  GetEffectBase(m_CurMagic.EffectNumber - 1 + 1001, 0, wimg, Idx);
                  if (m_CurMagic.EffectNumber = 4) and (m_CurMagic.Poison = 2) then
                    Inc(Idx, 210);
                end;
              2: begin
                  GetEffectBase(m_CurMagic.EffectNumber - 1 + 2001, 0, wimg, Idx);
                  if (m_CurMagic.EffectNumber = 4) and (m_CurMagic.Poison = 2) then
                    Inc(Idx, 210);
                end;
              3: begin
                  GetEffectBase(m_CurMagic.EffectNumber - 1 + 3001, 0, wimg, Idx);
                  if (m_CurMagic.EffectNumber = 4) and (m_CurMagic.Poison = 2) then
                    Inc(Idx, 210);
                end;
            else
              GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, Idx);
            end;
          end else begin
            //DScreen.AddChatBoardString(Format('%d:%d', [m_CurMagic.EffectNumber, m_nCurEffFrame]), GetRGB(5), clWhite);
            case m_CurMagic.EffectNumber of
              120: begin
                  wimg := g_Wui;
                  Idx := 1210;
                end;
              121: begin
                  wimg := g_WMagic8Images2;
                  Idx := 70;
                end;
              122: begin
                  wimg := g_WMagic7Images2;
                  Idx := 840;
                end;
            else begin
                GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, Idx);
              end;
            end;
          end;
        end;
        d := nil;
        if wimg <> nil then begin
          Idx := Idx + m_nCurEffFrame;
          d := wimg.GetCachedImage(Idx, ax, ay);
        end;
        if d <> nil then dsurface.DrawBlend(ShiftX + ax, ShiftY + ay, d, 1);
      end;
    end;
  end;

  //显示攻击效果
  if m_boHitEffect then begin
    if m_boUseCboLib then begin
      wimg := nil;
      nOff := 10;
      case m_nCurrentAction of
        SM_RUSHEX: begin
            wimg := g_cboEffect;
            Idx := 80;
            nOff := 10;
          end;
        SM_SMITEHIT: begin
            wimg := g_cboEffect;
            Idx := 160;
            nOff := 20;
            if m_nCurrentFrame = m_nEndFrame then begin
              g_NextSeriesSkill := True;
            end;
          end;
        SM_SMITELONGHIT: begin
            wimg := g_cboEffect;
            if m_boSmiteLongHit = 1 then
              Idx := 1920
            else
              Idx := 320;
            nOff := 10;
            if m_nCurrentFrame = m_nEndFrame then g_NextSeriesSkill := True;
          end;
        SM_SMITELONGHIT3: begin
            wimg := g_WMagic8Images2;
            Idx := 2380;
            nOff := 10;
          end;
        SM_SMITELONGHIT2, SM_SMITEWIDEHIT2: begin
            wimg := g_cboEffect;
            Idx := 400;
            nOff := 20;
            //if m_nCurrentFrame = m_nEndFrame then g_NextSeriesSkill := True;
          end;
        SM_SMITEWIDEHIT: begin
            wimg := g_cboEffect;
            nOff := 10;
            Idx := 560;
            if m_nCurrentFrame = m_nEndFrame then g_NextSeriesSkill := True;
          end;
      end;
      if wimg <> nil then begin
        Idx := Idx + m_btDir * nOff + (m_nCurrentFrame - m_nStartFrame);
        d := wimg.GetCachedImage(Idx, ax, ay);
      end;
    end else if (m_nHitEffectNumber > 0) then begin
      //
      GetEffectBase(m_nHitEffectNumber - 1, 1, wimg, Idx);
      if wimg <> nil then begin
        Idx := Idx + m_btDir * 10 + (m_nCurrentFrame - m_nStartFrame);
        d := wimg.GetCachedImage(Idx, ax, ay);
      end;
    end;
    if d <> nil then dsurface.DrawBlend(ShiftX + ax, ShiftY + ay, d, 1);
  end;

  //显示武器破碎效果
  if m_boWeaponEffect then begin
    Idx := WPEFFECTBASE + m_btDir * 10 + m_nCurWeaponEffect;
    d := g_WMagicImages.GetCachedImage(Idx, ax, ay);
    if d <> nil then dsurface.DrawBlend(ShiftX + ax, ShiftY + ay, d, 1);
  end;
end;

procedure THumActor.DrawChr_Transparent(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean);
var
  bWarMode                  : Boolean;
  nOff, Idx, ax, ay         : Integer;
  Effect                    : TMagicEff;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  wimg                      : TWMImages;
begin
  d := nil;
  if not (m_btDir in [0..7]) then Exit;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

  if m_fHideMode then
    Exit;

  ceff := GetDrawEffectValue;
  if m_btRace = 0 then begin
    if m_boUseCboLib and ((m_nState and $04000000 <> 0) or (m_nState and $02000000 <> 0)) then m_boUseCboLib := False;

    if (m_btEffect <> 0) and (m_HumWinSurface <> nil) then begin
      bWarMode := (m_btDir = 2) and m_boWarMode and IsIdle;
      if g_MySelf = Self then begin
        if blend then begin
          if ((m_btDir in [3..5]) or bWarMode) and not boFlag then begin
            dsurface.DrawBlend(
              dx + m_nSpx + m_nShiftX,
              dy + m_nSpy + m_nShiftY,
              m_HumWinSurface,
              1);
          end else if ((m_btDir in [3..5]) or bWarMode) and boFlag then begin
            dsurface.DrawBlend(
              dx + m_nSpx + m_nShiftX,
              dy + m_nSpy + m_nShiftY,
              m_HumWinSurface,
              1);
          end;
        end;
      end else begin
        if ((g_FocusCret <> nil) or (g_MagicTarget <> nil)) and blend and ((m_btDir in [3..5]) or bWarMode) and not boFlag then begin
          dsurface.DrawBlend(
            dx + m_nSpx + m_nShiftX,
            dy + m_nSpy + m_nShiftY,
            m_HumWinSurface,
            1);
        end else if ((m_btDir in [3..5]) or bWarMode) and boFlag then begin
          dsurface.DrawBlend(
            dx + m_nSpx + m_nShiftX,
            dy + m_nSpy + m_nShiftY,
            m_HumWinSurface,
            1);
        end;
      end;
    end;

    //g_cboHumEffect
    if (m_btEffect = 50) then begin
      if (m_HumWinSurface <> nil) then
        dsurface.DrawBlend(
          dx + m_nSpx + m_nShiftX,
          dy + m_nSpy + m_nShiftY,
          m_HumWinSurface,
          1);
    end else if (m_btEffect <> 0) and (m_HumWinSurface <> nil) then begin
      if g_MySelf = Self then begin
        if blend then begin
          if not bWarMode and (m_btDir in [0..2, 6, 7]) and not boFlag then begin
            dsurface.DrawBlend(
              dx + m_nSpx + m_nShiftX,
              dy + m_nSpy + m_nShiftY,
              m_HumWinSurface,
              1);
          end else if not bWarMode and (m_btDir in [0..2, 6, 7]) and boFlag then begin
            dsurface.DrawBlend(
              dx + m_nSpx + m_nShiftX,
              dy + m_nSpy + m_nShiftY,
              m_HumWinSurface,
              1);
          end;
        end;
      end else begin
        if ((g_FocusCret <> nil) or (g_MagicTarget <> nil)) and
          not bWarMode and (m_btDir in [0..2, 6, 7]) and not boFlag then begin
          dsurface.DrawBlend(
            dx + m_nSpx + m_nShiftX,
            dy + m_nSpy + m_nShiftY,
            m_HumWinSurface,
            1);
        end else begin
          if not bWarMode and (m_btDir in [0..2, 6, 7]) and boFlag then begin
            dsurface.DrawBlend(
              dx + m_nSpx + m_nShiftX,
              dy + m_nSpy + m_nShiftY,
              m_HumWinSurface,
              1);
          end;
        end;
      end;
    end;
  end;

end;

end.



