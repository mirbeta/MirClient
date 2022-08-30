unit magiceff;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, uGameEngine,
  Grobal2, PXL.Canvas, PXL.Textures, cliUtil, ClFunc, HUtil32, WIL;

const
  MG_READY                  = 10;
  MG_FLY                    = 6;
  MG_EXPLOSION              = 10;
  READYTIME                 = 120;
  EXPLOSIONTIME             = 100;
  FLYBASE                   = 10;
  EXPLOSIONBASE             = 170;
  MAXMAGIC                  = 10;
  FLYOMAAXEBASE             = 447;
  THORNBASE                 = 2967;
  ARCHERBASE                = 2607;
  ARCHERBASE2               = 272;
  FLYFORSEC                 = 500;
  FIREGUNFRAME              = 6;

  MAXEFFECT                 = 69;
  g_EffectBase              : array[0..MAXEFFECT - 1] of Integer = (
    0,                                  {1}
    200,                                {2}
    400,                                {3}
    600,                                {4}
    0,                                  {5}
    900,                                {6}
    920,                                {7}
    940,                                {8}
    20,                                 {9}
    940,                                {10}
    940,                                {11}
    940,                                {12}
    0,                                  {13}
    1380,                               {14}
    1500,                               {15}
    1520,                               {16}
    940,                                {17}
    1560,                               {18}
    1590,                               {19}
    1620,                               {20}
    1650,                               {21}
    1680,                               {22}
    0,                                  {23}
    0,                                  {24}
    0,                                  {25}
    3960,                               {26}
    1790,                               {27}
    0,                                  {28}
    3880,                               {29}
    3920,                               {30}
    3840,                               {31}
    0,                                  {32}
    40,                                 {33}
    130,                                {34}
    160,                                {35}
    190,                                {36}
    0,                                  {37}
    210,                                {38}
    400,                                {39}
    600,                                {40}
    1500,                               {41}
    650,                                {42}
    710,                                {43}
    740,                                {44}
    910,                                {45}
    940,                                {46}
    990,                                {47}
    1040,                               {48}
    1110,                               {49}
    0,                                  {50}
    940,                                {51}
    0,                                  {52}
    0,                                  {53}
    0,                                  {54}
    0,                                  {55}
    0,                                  {56}
    1040,                               {57}
    940,                                {58}
    0,                                  {59}
    0,                                  {60}
    440,                                {61}
    270,                                {62}
    610,                                {63}
    190,                                {64}
    540,                                {65}
    210,                                {66}
    840,                                {67}
    0,                                  {68}
    0                                   {69}
    );

  MAXHITEFFECT              = 15;
  HitEffectBase             : array[0..MAXHITEFFECT - 1] of Integer = (
    800,                                //1
    1410,                               //2
    1700,                               //3
    3480,                               //4
    40,                                 //5 3390
    470,                                //6
    310,                                //7
    630,                                //8
    0,                                  //9
    120,                                //10
    0,                                  //11
    0,                                  //12
    0,                                  //13
    0,                                  //14
    510                                 //15
    );
  HitEffectBaseEx           : array[0..MAXHITEFFECT - 1] of Integer = (
    800,                                //1
    1410,                               //2
    1700,                               //3
    3480,                               //4
    40,                                 //5 3390
    555,                                //6
    310,                                //7
    715,                                //8
    0,                                  //9
    120,                                //10
    0,                                  //11
    0,                                  //12
    0,                                  //13
    0,                                  //14
    510                                 //15
    );
  MAXMAGICTYPE              = 16;

type
  TMagicType = (mtReady, mtFly, mtExplosion,
    mtFlyAxe, mtFireWind, mtFireGun,
    mtLightingThunder, mtThunder, mtExploBujauk,
    mtBujaukGroundEffect, mtKyulKai, mtFlyArrow,
    mtFlyBug, mtGroundEffect, mtThuderEx,
    mtFireBall, mtFlyBolt, mtRedThunder, mtRedGroundThunder,
    mtLava, mtSpurt, mtFlyStick, mtFlyStick2
    );
    
  {TMagicType = (mtReady, mtFly, mtExplosion,
    mtFlyAxe, mtFireWind, mtFireGun,
    mtLightingThunder, mtThunder, mtExploBujauk,
    mtBujaukGroundEffect, mtKyulKai, mtFlyArrow,
    mt12, mt13, mtThuderEx,
    mt15, mt16, mtRedThunder, mtRedGroundThunder,
    mtLava, mtSpurt
    );}

  TUseMagicInfo = record
    ServerMagicCode: Integer;
    MagicSerial: Integer;
    target: Integer;
    EffectType: TMagicType;
    EffectNumber: Integer;
    targx: Integer;
    targy: Integer;
    Recusion: Boolean;
    anitime: Integer;
    spelllv: Integer;
    magfirelv: Integer;
    Poison: Integer;
  end;
  PTUseMagicInfo = ^TUseMagicInfo;

  TMagicEff = class
    m_nMagEffectNo: Integer;
    m_boActive: Boolean;
    ServerMagicId: Integer;
    MagOwner: TObject;
    TargetActor: TObject;
    ImgLib: TWMImages;
    ExplosionImgLib: TWMImages;
    EffectBase: Integer;
    EffectBase2: Integer;
    MagExplosionBase: Integer;
    px, py: Integer;
    rx, ry: Integer;
    Dir16, OldDir16: byte;
    targetx, targety: Integer;
    TargetRx, TargetRy: Integer;
    FlyX, FlyY, OldFlyX, OldFlyY: Integer;
    FlyXf, FlyYf: Real;
    Repetition: Boolean;
    FixedEffect: Boolean;
    MagicType: Integer;
    NextEffect: TMagicEff;
    ExplosionFrame: Integer;
    NextFrameTime: Integer;
    light: Integer;
    bt80: byte;
    start: Integer;
    curframe: Integer;
    frame: Integer;
    fFirstShift: Boolean;
    nStdX, nStdY: Integer;
  private
    m_dwFrameTime: LongWord;
    m_dwStartTime: LongWord;
    repeattime: LongWord;
    steptime: LongWord;
    fireX, fireY: Integer;
    firedisX, firedisY: Integer;
    newfiredisX, newfiredisY: Integer;
    FireMyselfX, FireMyselfY: Integer;
    prevdisx, prevdisy: Integer;
  protected
    procedure GetFlyXY(ms: Integer; var fx, fy: Integer);
  public
    constructor Create(id, effnum, sX, sY, tx, ty: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
    destructor Destroy; override;
    function Run: Boolean; virtual;
    function Shift: Boolean; virtual;
    procedure DrawEff(Surface: TCustomCanvas); virtual;
  end;

  TFlyingAxe = class(TMagicEff)
    FlyImageBase: Integer;
    ReadyFrame: Integer;
  public
    constructor Create(id, effnum, sX, sY, tx, ty: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TFlyingBug = class(TMagicEff)
    FlyImageBase: Integer;
    ReadyFrame: Integer;
  public
    constructor Create(id, effnum, sX, sY, tx, ty: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TFlyingArrow = class(TFlyingAxe)
  public
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TFlyingFireBall = class(TFlyingAxe)
  public
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TCharEffect = class(TMagicEff)
  public
    constructor Create(effbase, effframe: Integer; target: TObject);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TMapEffect = class(TMagicEff)
  public
    RepeatCount: Integer;
    constructor Create(effbase, effframe: Integer; X, Y: Integer);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TScrollHideEffect = class(TMapEffect)
  public
    constructor Create(effbase, effframe: Integer; X, Y: Integer; target: TObject);
    function Run: Boolean; override;
  end;

  TLightingEffect = class(TMagicEff)
  public
    constructor Create(effbase, effframe: Integer; X, Y: Integer);
    function Run: Boolean; override;
  end;

  TFireNode = record
    X: Integer;
    Y: Integer;
    firenumber: Integer;
  end;

  TFireGunEffect = class(TMagicEff)
  public
    OutofOil: Boolean;
    firetime: LongWord;
    FireNodes: array[0..FIREGUNFRAME - 1] of TFireNode;
    constructor Create(effbase, sX, sY, tx, ty: Integer);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TThuderEffect = class(TMagicEff)
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TThuderEffectEx = class(TMagicEff)
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject; magnum: Integer);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TLightingThunder = class(TMagicEff)
  public
    constructor Create(effbase, sX, sY, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TExploBujaukEffect = class(TMagicEff)
    bTransparent: Boolean;
  public
    constructor Create(effbase, sX, sY, tx, ty: Integer; target: TObject; bt: Boolean = False; MagEff: Integer = 0);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TBujaukGroundEffect = class(TMagicEff)
    mLevel: Integer;
  public
    MagicNumber: Integer;
    BoGroundEffect: Boolean;
    constructor Create(effbase, magicnumb, sX, sY, tx, ty: Integer; lv: Integer = 0);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TNormalDrawEffect = class(TMagicEff)
    FSoundIdx: Integer;
    boBlend: Boolean;
  public
    constructor Create(xx, yy: Integer; WMImage: TWMImages; effbase, nFrame: Integer; frmTime: LongWord; boFlag: Boolean; SoundIdx: Integer = 0);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  THeroCharEffect = class(TMagicEff)
  public
    constructor Create(WMImage: TWMImages; effbase, effframe: Integer; frmTime: LongWord; target: TObject);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  THumanEffects = class(TMagicEff)      //Size 0xCC
    boC8: Boolean;
  public
    constructor Create(xx, yy: Integer; WMImage: TWMImages; effbase, nX: Integer; frmTime: LongWord; boFlag: Boolean);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TObjectEffects = class(TMagicEff)
    ObjectID: TObject;
    boC8: Boolean;
  public
    constructor Create(ObjectiD2: TObject; WMImage: TWMImages; effbase, nX: Integer; frmTime: LongWord; boFlag: Boolean);
    function Run: Boolean; override;
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TRedThunderEffect = class(TMagicEff)
    n0: Integer;
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TRedGroundThunderEffect = class(TMagicEff)
    n0: Integer;
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TLavaEffect = class(TMagicEff)
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  TSpurtEffect = class(TMagicEff)
  public
    constructor Create(effbase, tx, ty: Integer; target: TObject);
    procedure DrawEff(Surface: TCustomCanvas); override;
  end;

  {TRareBoxWindow = class //(TMagicEff)
    m_boActive: Boolean;
    m_ImgLib: TWMImages;
    m_nLeft: Integer;
    m_nTop: Integer;
    m_nStepTime: Integer;
    m_nEffectBase: Integer;
    m_nStart: Integer;
    m_nCurframe: Integer;
    m_nFrame: Integer;
    m_nNextFrameTime: LongWord;
    m_Effect: TDirectDrawSurface;
  public
    constructor Create(xx, yy: Integer; WMImage: TWMImages; effbase, nFrame: Integer; frmTime: LongWord; boFlag: Boolean);
    function Run: Boolean; virtual;
    procedure DrawEff(Surface: TDirectDrawSurface); virtual;
  end;}

procedure GetEffectBase(mag, Mtype: Integer; var wimg: TWMImages; var idx: Integer);

implementation

uses
  ClMain, HumanActor, Actor, SoundUtil, MShare;

procedure GetEffectBase(mag, Mtype: Integer; var wimg: TWMImages; var idx: Integer); //取得魔法效果所在图库
var
  L, n                      : Integer;
begin
  wimg := nil;
  idx := 0;
  case Mtype of
    0: begin
        if mag > 1000 then begin
          wimg := g_WMagic7Images;
          L := mag div 1000;            //level
          n := mag mod 1000;            //magic
          case n of
            04: begin
                case L of
                  1: idx := 480;
                  2: idx := 540;
                  3: idx := 600;
                end;
              end;
            09: case L of
                1: idx := 180;
                2: idx := 190;
                3: idx := 200;
              end;
            10: begin                   //fire
                wimg := g_WMagic8Images;
                case L of
                  1: idx := 560;
                  2: idx := 570;
                  3: idx := 580;
                end;
              end;
            11: begin                   //幽灵盾
                wimg := g_WMagic8Images;
                case L of
                  1: idx := 500;
                  2: idx := 510;
                  3: idx := 520;
                end;
              end;
            12: begin                   //神圣战甲术
                wimg := g_WMagic8Images;
                case L of
                  1: idx := 530;
                  2: idx := 540;
                  3: idx := 550;
                end;
              end;
            15: case L of
                1: idx := 900;
                2: idx := 960;
                3: idx := 1020;
              end;
            20: case L of               //earth fire
                1: idx := 60;
                2: idx := 70;
                3: idx := 80;
              end;
            21: case L of
                1: idx := 260;
                2: idx := 310;
                3: idx := 340;
              end;
            28: begin                   //召唤神兽
                wimg := g_WMagic8Images;
                case L of
                  1: idx := 160;
                  2: idx := 180;
                  3: idx := 200;
                end;
              end;
            31: begin
                wimg := g_WMagic8Images;
                case L of               //ice cround
                  1: idx := 20;
                  2: idx := 50;
                  3: idx := 80;
                end;
              end;
            34: begin
                wimg := g_WMagic9Images;
                case L of               //灭天火
                  1: idx := 340;
                  2: idx := 350;
                  3: idx := 360;
                end;
              end;
            48: begin
                wimg := g_WMagic9Images;
                case L of               //噬血术
                  1: idx := 670;
                  2: idx := 820;
                  3: idx := 970;
                end;
              end;
            51: begin                   //流星火雨
                wimg := g_WMagic9Images;
                case L of
                  1: idx := 490;
                  2: idx := 500;
                  3: idx := 510;
                end;
              end;
          else
            wimg := nil;
          end;

        end else begin
          case mag of
            /////////////////////
            10 - 1 + 500: begin
                wimg := g_WMagic6Images;
                idx := 120;
              end;
            29 - 1 + 500: begin
                wimg := g_WMagic6Images;
                idx := 690;
              end;
            34 - 1 + 500: begin
                wimg := g_WMagic6Images;
                idx := 80;
              end;
            48 - 1 + 500: begin
                wimg := g_WMagic2Images;
                idx := 1130;
              end;

            /////////////////////
            60..68: begin
                wimg := g_WMagic4Images;
                if mag in [0..MAXEFFECT - 1] then
                  idx := g_EffectBase[mag];
              end;
            51 - 1: begin
                wimg := g_WMagic6Images;
                idx := 630;
              end;
            8, 27, 33, 35, 37..39, 41, 43, 44, 45..48: begin
                wimg := g_WMagic2Images;
                if mag in [0..MAXEFFECT - 1] then
                  idx := g_EffectBase[mag];
              end;
            35 - 1: begin
                wimg := nil;
              end;

            43 - 1: begin               //狮子吼
                wimg := nil;
              end;
            9: Exit;
            31: begin
                wimg := g_WMon21Img;
                if mag in [0..MAXEFFECT - 1] then
                  idx := g_EffectBase[mag];
              end;
            36: begin
                wimg := g_WMon22Img;
                if mag in [0..MAXEFFECT - 1] then
                  idx := g_EffectBase[mag];
              end;
            80..82: begin
                wimg := g_WDragonImg;
                if mag = 80 then begin
                  if g_MySelf.m_nCurrX >= 84 then begin
                    idx := 130;
                  end else begin
                    idx := 140;
                  end;
                end;
                if mag = 81 then begin
                  if (g_MySelf.m_nCurrX >= 78) and (g_MySelf.m_nCurrY >= 48) then begin
                    idx := 150;
                  end else begin
                    idx := 160;
                  end;
                end;
                if mag = 82 then begin
                  idx := 180;
                end;
              end;
            69: begin
                wimg := g_WDragonImg;
                idx := 400;
              end;
            70: begin
                wimg := g_WDragonImg;
                idx := 400;
              end;
            71: begin
                wimg := g_WDragonImg;
                idx := 400;
              end;
            73: begin
                wimg := g_WMagic5Images;
                idx := 90;
              end;
            89: begin
                wimg := g_WDragonImg;
                idx := 350;
              end;
            90: begin
                wimg := g_WDragonImg;
                idx := 440
              end;
            91: begin
                wimg := g_WDragonImg;
                idx := 470
              end;
            92: begin
                wimg := g_WMagic2Images;
                idx := 1250;
              end;
            99: begin
                wimg := g_WMagic5Images;
                idx := 100;
              end;
            100: begin
                wimg := g_WMagic5Images;
                idx := 280;
              end;

            103: begin
                wimg := g_cboEffect;
                idx := 640;
              end;
            111: begin
                wimg := g_cboEffect;
                idx := 720;
              end;
            105: begin
                wimg := g_cboEffect;
                idx := 800;
              end;
            106: begin
                wimg := g_cboEffect;
                idx := 1040;
              end;

            107: begin
                wimg := g_cboEffect;
                idx := 1200;
              end;
            108: begin
                wimg := g_cboEffect;
                idx := 1440;
              end;
            109: begin
                wimg := g_cboEffect;
                idx := 1600;
              end;
            110: begin
                wimg := g_cboEffect;
                idx := 1760;
              end;
            104: begin
                wimg := g_cboEffect;
                idx := 4210;
              end;
            115: begin
                wimg := g_WMagic8Images2;
                idx := 2040;
              end;
            116: begin
                wimg := g_WMagic8Images2;
                idx := 2180;
              end;
              
            {124 - 1: begin
                wimg := g_WMagic10Images;
                idx := 110;
              end;}
            125 - 1: begin
                wimg := g_WMagic10Images;
                idx := 60;
              end;
            126 - 1: begin
                wimg := g_WMagic10Images;
                idx := 200;
              end;
            {127 - 1: begin
                wimg := g_WMagic10Images;
                idx := 0;
              end;}
            128 - 1: begin
                wimg := g_WMagic10Images;
                idx := 330;
              end;
          else begin
              if (mag <> 25)    //心灵
                and (mag in [0..MAXEFFECT - 1]) then begin
                idx := g_EffectBase[mag];
                wimg := g_WMagicImages;
              end else
                wimg := nil;
            end;
          end;
        end;
      end;
    1: begin
        if mag > 100 then begin
          wimg := g_WMagic7Images;
          L := mag div 100;             //level
          n := mag mod 100;             //magic
          case n of
            1: case L of
                1: idx := 1600;
                2: idx := 1690;
                3: idx := 1780;
              end;
            2: case L of
                1: idx := 2140;
                2: idx := 2230;
                3: idx := 2320;
              end;
            3: case L of
                1: idx := 1870;
                2: idx := 1960;
                3: idx := 2050;
              end;
            4: begin                    //烈火
                wimg := g_WMagic8Images;
                case L of
                  1: idx := 1660;
                  2: idx := 1750;
                  3: idx := 1840;
                end;
              end;
          end;
        end else begin
          if mag = 6 then
            wimg := g_WMagic4Images
          else if mag in [4, 9] then
            wimg := g_WMagic2Images
          else if mag in [5, 7] then
            wimg := g_WMagic5Images
          else if mag in [8, 10..19] then
            wimg := g_WMagic6Images
          else
            wimg := g_WMagicImages;
          if mag in [0..MAXHITEFFECT - 1] then
            idx := HitEffectBase[mag];
        end;
      end;
    2: begin
        case mag of
          10 - 1: begin
              wimg := g_WMagic6Images;
              if mag in [0..MAXHITEFFECT - 1] then
                idx := HitEffectBase[mag];
            end;
        else
          wimg := g_WMagicImages;
        end;
      end;

  end;
end;

constructor TMagicEff.Create(id, effnum, sX, sY, tx, ty: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
var
  tax, tay                  : Integer;
begin
  ImgLib := g_WMagicImages;
  EffectBase := effnum;
  EffectBase2 := -1;
  case Mtype of
    mtFly, mtBujaukGroundEffect, mtExploBujauk: begin
        start := 0;
        frame := 6;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 10;
        if id = 74 then begin
          ImgLib := g_WMagic5Images;
          MagExplosionBase := 10;
          frame := 10;
        end;
        if id = 38 then
          frame := 10;
        if id = 39 then begin
          frame := 4;
          ExplosionFrame := 8;
        end;
        if id = 44 then
          ExplosionFrame := 16;
        if (id = 81) or (id = 82) or (id = 83) then begin //if (id - 81 - 3) < 0 then begin
          bt80 := 1;
          Repetition := True;
          if id = 81 then begin
            ImgLib := g_WDragonImg;
            if g_MySelf.m_nCurrX >= 84 then begin
              EffectBase := 130;
            end else begin
              EffectBase := 140;
            end;
          end;
          if id = 82 then begin
            ImgLib := g_WDragonImg;
            if (g_MySelf.m_nCurrX >= 78) and (g_MySelf.m_nCurrY >= 48) then begin
              EffectBase := 150;
            end else begin
              EffectBase := 160;
            end;
          end;
          if id = 83 then begin
            ImgLib := g_WDragonImg;
            EffectBase := 180;
          end;
          start := 0;
          frame := 10;
          MagExplosionBase := 200;
          ExplosionFrame := 10;
        end;
      end;
    mtFlyBug: begin
        start := 0;
        frame := 6;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 1;
      end;
    mtGroundEffect: begin
        start := 0;
        frame := 20;
        if id = MAGIC_SIDESTONE_ATT1 then
          frame := 10;
        curframe := start;
        FixedEffect := True;
        Repetition := False;
        ExplosionFrame := 20;
        if id = 7 then begin
          frame := 10;
          EffectBase := 3720;
          ExplosionFrame := 10;
          ImgLib := g_WMon24Img;
        end else
          ImgLib := g_WMon21Img;
      end;
    mtExplosion, mtThunder, mtLightingThunder, mtRedThunder, mtLava: begin
        start := 0;
        frame := -1;
        ExplosionFrame := 10;
        curframe := start;
        FixedEffect := True;
        Repetition := False;
        if id = 70 then begin
          bt80 := 3;
          case Random(3) of
            0: EffectBase := 400;
            1: EffectBase := 410;
            2: EffectBase := 420;
          end;
          light := 4;
          ExplosionFrame := 5;
          ImgLib := g_WDragonImg;
        end;
        if id = 71 then begin
          bt80 := 3;
          EffectBase := 440;
          ExplosionFrame := 20;
          ImgLib := g_WDragonImg;
        end;
        if id = 72 then begin
          bt80 := 3;
          light := 3;
          EffectBase := 470;
          ExplosionFrame := 10;
          ImgLib := g_WDragonImg;
        end;
        {if id = 73 then begin
          bt80 := 3;
          light := 5;
          ExplosionFrame := 20;
        end;
        if id = 74 then begin
          bt80 := 3;
          light := 4;
          ExplosionFrame := 35;
        end;}
        if id = 80 then begin
          bt80 := 2;
          case Random(6) of
            0: EffectBase := 230;
            1: EffectBase := 240;
            2: EffectBase := 250;
            3: EffectBase := 260;       //blue
            4: EffectBase := 270;
            5: EffectBase := 280;
          end;
          light := 4;
          ExplosionFrame := 5;
          frame := 5;
        end;
        if id = 90 then begin
          ImgLib := g_WDragonImg;
          EffectBase := 350;
          MagExplosionBase := 350;
          ExplosionFrame := 34;
          bt80 := 1;
        end;
        if id = 91 then begin
          bt80 := 2;
          frame := 20;
          EffectBase := 440;
        end;
        if id = 92 then begin
          bt80 := 2;
          frame := -1;
          ImgLib := g_WMagic2Images;
          EffectBase := 0;
          MagExplosionBase := 1250;
          ExplosionFrame := 14;
        end;
        if id = 93 then begin
          bt80 := 2;
          frame := -1;
          ImgLib := g_WMagic2Images;
          EffectBase := 0;
          MagExplosionBase := 1280;
          ExplosionFrame := 10;
        end;
      end;
    mtThuderEx: begin
        start := 0;
        frame := -1;
        curframe := start;
        FixedEffect := True;
        Repetition := False;
        ImgLib := g_WMagic2Images;
      end;
    mtFlyAxe: begin
        start := 0;
        frame := 3;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 3;
      end;
    mtFlyArrow: begin
        start := 0;
        frame := 1;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 1;
      end;
    mtFireBall: begin
        start := 0;
        frame := 6;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 2;
      end;
    mtFlyBolt: begin
        start := 0;
        frame := 1;
        curframe := start;
        FixedEffect := False;
        Repetition := Recusion;
        ExplosionFrame := 1;
      end;
  end;

  nStdX := 0;
  nStdY := 0;
  fFirstShift := True;
  
  ServerMagicId := id;
  m_nMagEffectNo := 0;
  //EffectBase := effnum;
  targetx := tx;
  targety := ty;
  if bt80 = 1 then g_SndMgr.PlaySound(8208, sX, sY);
  fireX := sX;
  fireY := sY;
  FlyX := sX;
  FlyY := sY;
  OldFlyX := sX;
  OldFlyY := sY;
  FlyXf := sX;
  FlyYf := sY;

  FireMyselfX := g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX;
  FireMyselfY := g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY;

  if bt80 = 0 then
    MagExplosionBase := EffectBase + EXPLOSIONBASE;

  light := 1;

  if fireX <> targetx then
    tax := abs(targetx - fireX)
  else
    tax := 1;
  if fireY <> targety then
    tay := abs(targety - fireY)
  else
    tay := 1;
  if abs(fireX - targetx) > abs(fireY - targety) then begin
    firedisX := Round((targetx - fireX) * (500 / tax));
    firedisY := Round((targety - fireY) * (500 / tax));
  end else begin
    firedisX := Round((targetx - fireX) * (500 / tay));
    firedisY := Round((targety - fireY) * (500 / tay));
  end;

  NextFrameTime := 50;
  m_dwFrameTime := GetTickCount;
  m_dwStartTime := GetTickCount;
  steptime := GetTickCount;
  repeattime := anitime;
  Dir16 := GetFlyDirection16(sX, sY, tx, ty);
  OldDir16 := Dir16;
  NextEffect := nil;
  m_boActive := True;
  prevdisx := 99999;
  prevdisy := 99999;
end;

destructor TMagicEff.Destroy;
begin
  inherited Destroy;
end;

function TMagicEff.Shift: Boolean;

  function OverThrough(olddir, newdir: Integer): Boolean;
  begin
    Result := False;
    if abs(olddir - newdir) >= 2 then begin
      Result := True;
      if ((olddir = 0) and (newdir = 15)) or ((olddir = 15) and (newdir = 0)) then
        Result := False;
    end;
  end;
var
  i, rrx, rry, ms, stepx, stepy, newstepx, newstepy, nn, mx, my: Integer;
  tax, tay, shx, shy, passdir16: Integer;
  boCrash                   : Boolean;
  stepxf, stepyf            : Real;
begin
  Result := True;
  
  if fFirstShift and (m_nMagEffectNo in [0075, 108, 111]) then begin
    fFirstShift := False;
    FireMyselfX := nStdX * UNITX;
    FireMyselfY := nStdY * UNITY;
  end;

  if Repetition then begin
    if GetTickCount - steptime > LongWord(NextFrameTime) then begin
      steptime := GetTickCount;
      Inc(curframe);
      if curframe > start + frame - 1 then
        curframe := start;
    end;
  end else if (frame > 0) and (GetTickCount - steptime > LongWord(NextFrameTime)) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start + frame - 1;
      Result := False;
    end;
  end;
  if (not FixedEffect) then begin
    boCrash := False;
    if TargetActor <> nil then begin
      ms := GetTickCount - m_dwFrameTime;
      m_dwFrameTime := GetTickCount;
      g_PlayScene.ScreenXYfromMCXY(TActor(TargetActor).m_nRx, TActor(TargetActor).m_nRy, targetx, targety);
      if (nStdX <> 0) and (nStdY <> 0) and (m_nMagEffectNo in [0075, 108, 111]) then begin
        shx := (nStdX * UNITX) - FireMyselfX;
        shy := (nStdY * UNITY) - FireMyselfY;
      end else begin
        shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
        shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;
      end;
      targetx := targetx + shx;
      targety := targety + shy;

      if FlyX <> targetx then
        tax := abs(targetx - FlyX)
      else
        tax := 1;
      if FlyY <> targety then
        tay := abs(targety - FlyY)
      else
        tay := 1;
      if abs(FlyX - targetx) > abs(FlyY - targety) then begin
        newfiredisX := Round((targetx - FlyX) * (500 / tax));
        newfiredisY := Round((targety - FlyY) * (500 / tax));
      end else begin
        newfiredisX := Round((targetx - FlyX) * (500 / tay));
        newfiredisY := Round((targety - FlyY) * (500 / tay));
      end;

      if firedisX < newfiredisX then firedisX := firedisX + _MAX(1, (newfiredisX - firedisX) div 10);
      if firedisX > newfiredisX then firedisX := firedisX - _MAX(1, (firedisX - newfiredisX) div 10);
      if firedisY < newfiredisY then firedisY := firedisY + _MAX(1, (newfiredisY - firedisY) div 10);
      if firedisY > newfiredisY then firedisY := firedisY - _MAX(1, (firedisY - newfiredisY) div 10);

      if m_nMagEffectNo in [0075, 108, 111] then begin
        stepxf := (firedisX / 500) * ms;
        stepyf := (firedisY / 500) * ms;
      end else begin
        stepxf := (firedisX / 700) * ms;
        stepyf := (firedisY / 700) * ms;
      end;
      FlyXf := FlyXf + stepxf;
      FlyYf := FlyYf + stepyf;
      FlyX := Round(FlyXf);
      FlyY := Round(FlyYf);

      OldFlyX := FlyX;
      OldFlyY := FlyY;

      passdir16 := GetFlyDirection16(FlyX, FlyY, targetx, targety);

      //DebugOutStr(IntToStr(prevdisx) + ' ' + IntToStr(prevdisy) + ' / ' + IntToStr(abs(targetx - FlyX)) + ' ' + IntToStr(abs(targety - FlyY)) + '   ' + IntToStr(firedisX) + '.' + IntToStr(firedisY) + ' ' + IntToStr(FlyX) + '.' + IntToStr(FlyY) + ' ' + IntToStr(targetx) + '.' + IntToStr(targety));

      if ((abs(targetx - FlyX) <= 15) and (abs(targety - FlyY) <= 15)) or OverThrough(OldDir16, passdir16) then begin
        boCrash := True;
      end else begin
        prevdisx := abs(targetx - FlyX);
        prevdisy := abs(targety - FlyY);
      end;
      OldDir16 := passdir16;

    end else begin
      if m_nMagEffectNo in [0075, 108, 111] then begin
        ms := GetTickCount - m_dwFrameTime;
        m_dwFrameTime := GetTickCount;
        if (nStdX <> 0) and (nStdY <> 0) then begin
          shx := (nStdX * UNITX) - FireMyselfX;
          shy := (nStdY * UNITY) - FireMyselfY;
        end else begin
          shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
          shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;
        end;
        targetx := targetx + shx;
        targety := targety + shy;

        if FlyX <> targetx then
          tax := abs(targetx - FlyX)
        else
          tax := 1;
        if FlyY <> targety then
          tay := abs(targety - FlyY)
        else
          tay := 1;
        if abs(FlyX - targetx) > abs(FlyY - targety) then begin
          newfiredisX := Round((targetx - FlyX) * (500 / tax));
          newfiredisY := Round((targety - FlyY) * (500 / tax));
        end else begin
          newfiredisX := Round((targetx - FlyX) * (500 / tay));
          newfiredisY := Round((targety - FlyY) * (500 / tay));
        end;

        if firedisX < newfiredisX then firedisX := firedisX + _MAX(1, (newfiredisX - firedisX) div 10);
        if firedisX > newfiredisX then firedisX := firedisX - _MAX(1, (firedisX - newfiredisX) div 10);
        if firedisY < newfiredisY then firedisY := firedisY + _MAX(1, (newfiredisY - firedisY) div 10);
        if firedisY > newfiredisY then firedisY := firedisY - _MAX(1, (firedisY - newfiredisY) div 10);

        stepxf := (firedisX / 500) * ms;
        stepyf := (firedisY / 500) * ms;
        FlyXf := FlyXf + stepxf;
        FlyYf := FlyYf + stepyf;
        FlyX := Round(FlyXf);
        FlyY := Round(FlyYf);

        OldFlyX := FlyX;
        OldFlyY := FlyY;

        passdir16 := GetFlyDirection16(FlyX, FlyY, targetx, targety);

        if ((abs(targetx - FlyX) <= 15) and (abs(targety - FlyY) <= 15)) or OverThrough(OldDir16, passdir16) then begin
          boCrash := True;
          nStdX := 0;
          nStdY := 0;
        end else begin
          prevdisx := abs(targetx - FlyX);
          prevdisy := abs(targety - FlyY);
        end;
        OldDir16 := passdir16;
      end else begin
        ms := GetTickCount - m_dwFrameTime;
        rrx := targetx - fireX;
        rry := targety - fireY;
        stepx := Round((firedisX / 900) * ms);
        stepy := Round((firedisY / 900) * ms);
        FlyX := fireX + stepx;
        FlyY := fireY + stepy;
      end;
    end;

    g_PlayScene.CXYfromMouseXY(FlyX, FlyY, rx, ry);

    if boCrash and ((TargetActor <> nil) or (m_nMagEffectNo in [0075, 108, 111])) then begin
      FixedEffect := True;
      start := 0;
      frame := ExplosionFrame;
      curframe := start;
      Repetition := False;
      g_PlayScene.CXYfromMouseXY(targetx, targety, mx, my);
      g_SndMgr.PlaySound(TActor(MagOwner).m_nMagicExplosionSound, mx, my);
    end;

  end;
  if FixedEffect then begin
    if frame = -1 then frame := ExplosionFrame;
    if TargetActor = nil then begin
      FlyX := targetx - ((g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX);
      FlyY := targety - ((g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY);
      g_PlayScene.CXYfromMouseXY(FlyX, FlyY, rx, ry);
    end else begin
      rx := TActor(TargetActor).m_nRx;
      ry := TActor(TargetActor).m_nRy;
      g_PlayScene.ScreenXYfromMCXY(rx, ry, FlyX, FlyY);
      FlyX := FlyX + TActor(TargetActor).m_nShiftX;
      FlyY := FlyY + TActor(TargetActor).m_nShiftY;
    end;
  end;
end;

procedure TMagicEff.GetFlyXY(ms: Integer; var fx, fy: Integer);
var
  rrx, rry, stepx, stepy    : Integer;
begin
  rrx := targetx - fireX;
  rry := targety - fireY;

  stepx := Round((firedisX / 900) * ms);
  stepy := Round((firedisY / 900) * ms);
  fx := fireX + stepx;
  fy := fireY + stepy;
end;

function TMagicEff.Run: Boolean;
begin
  Result := Shift();
  if Result then
    if GetTickCount - m_dwStartTime > 10000 then
      Result := False
    else
      Result := True;
end;

procedure TMagicEff.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d                         : TCustomLockableTexture;
  shx, shy, mx, my          : Integer;
begin
  if m_boActive and ((abs(FlyX - fireX) > 15) or (abs(FlyY - fireY) > 15) or FixedEffect) then begin
    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;
    if not FixedEffect then begin
      if (ServerMagicId = 81) or (ServerMagicId = 82) or (ServerMagicId = 83) {or (magnumber = 47)} then begin
        //DScreen.AddChatBoardString(ImgLib.FileName + ' ' + IntToStr(EffectBase), clWhite, clRed);
        img := EffectBase;
        d := ImgLib.GetCachedImage(img + curframe, px, py);
      end else begin
        img := EffectBase + FLYBASE + Dir16 * 10;

        if m_nMagEffectNo = 116 then begin
          if (MagOwner <> nil) and (curframe = 5) then begin
            g_PlayScene.CXYfromMouseXY(targetx, targety, mx, my);
            g_SndMgr.PlaySoundEx(TActor(MagOwner).m_nMagicExplosionSound, mx, my, TActor(MagOwner).m_nCurrX, TActor(MagOwner).m_nCurrY, False);
            g_ShakeScreen.SetScrShake_X(4);
            g_ShakeScreen.SetScrShake_Y(2);
          end;
          if curframe >= 6 then begin
            d := ImgLib.GetCachedImage(2150 + curframe - 6, px, py);
          end else
            d := ImgLib.GetCachedImage(img + curframe, px, py);
        end else begin
          d := ImgLib.GetCachedImage(img + curframe, px, py);
        end;
        if m_nMagEffectNo = 117 then begin
          if (curframe = 4) then begin
            g_ShakeScreen.SetScrShake_X(4);
            g_ShakeScreen.SetScrShake_Y(2);
          end;
        end;
      end;
      if d <> nil then begin
        Surface.DrawBlend(
          FlyX + px - UNITX div 2 - shx,
          FlyY + py - UNITY div 2 - shy,
          d, 1);
      end;
    end else begin
      img := MagExplosionBase + curframe;

      if m_nMagEffectNo = 116 then begin
        if (MagOwner <> nil) and (curframe = 5) then begin
          g_PlayScene.CXYfromMouseXY(targetx, targety, mx, my);
          g_SndMgr.PlaySoundEx(TActor(MagOwner).m_nMagicExplosionSound, mx, my, TActor(MagOwner).m_nCurrX, TActor(MagOwner).m_nCurrY, False);
          g_ShakeScreen.SetScrShake_X(4);
          g_ShakeScreen.SetScrShake_Y(2);
        end;

        if curframe >= 6 then begin
          d := ImgLib.GetCachedImage(2150 + curframe - 6, px, py);
        end else
          d := ImgLib.GetCachedImage(img, px, py);
      end else begin
        d := ImgLib.GetCachedImage(img, px, py);
      end;

      if m_nMagEffectNo = 117 then begin
        if (curframe = 4) then begin
          g_ShakeScreen.SetScrShake_X(4);
          g_ShakeScreen.SetScrShake_Y(2);
        end;
      end;

      if d <> nil then begin
        Surface.DrawBlend(
          FlyX + px - UNITX div 2,
          FlyY + py - UNITY div 2,
          d, 1);
      end;
    end;
  end;
end;

constructor TFlyingAxe.Create(id, effnum, sX, sY, tx, ty: Integer; Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
begin
  inherited Create(id, effnum, sX, sY, tx, ty, Mtype, Recusion, anitime);
  FlyImageBase := FLYOMAAXEBASE;
  ReadyFrame := 65;
end;

procedure TFlyingAxe.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d                         : TCustomLockableTexture;
  shx, shy                  : Integer;
begin
  if m_boActive and ((abs(FlyX - fireX) > ReadyFrame) or (abs(FlyY - fireY) > ReadyFrame)) then begin

    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

    if not FixedEffect then begin
      img := FlyImageBase + Dir16 * 10;
      d := ImgLib.GetCachedImage(img + curframe, px, py);
      if d <> nil then begin
        Surface.Draw(FlyX + px - UNITX div 2 - shx,
          FlyY + py - UNITY div 2 - shy,
          d.ClientRect, d, True);
      end;
    end;
  end;
end;

procedure TFlyingArrow.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d                         : TCustomLockableTexture;
  shx, shy                  : Integer;
begin
  if m_boActive and ((abs(FlyX - fireX) > 40) or (abs(FlyY - fireY) > 40)) then begin
    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

    if not FixedEffect then begin
      img := FlyImageBase + Dir16;
      d := ImgLib.GetCachedImage(img + curframe, px, py);
      if d <> nil then begin
        Surface.Draw(FlyX + px - UNITX div 2 - shx,
          FlyY + py - UNITY div 2 - shy - 46,
          d.ClientRect, d, True);
      end;
    end;
  end;
end;

constructor TCharEffect.Create(effbase, effframe: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    TActor(target).m_nCurrX, TActor(target).m_nCurrY,
    TActor(target).m_nCurrX, TActor(target).m_nCurrY,
    mtExplosion,
    False,
    0);
  TargetActor := target;
  frame := effframe;
  NextFrameTime := 60;
end;

function TCharEffect.Run: Boolean;
begin
  Result := True;
  if GetTickCount - steptime > LongWord(NextFrameTime) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start + frame - 1;
      Result := False;
    end;
  end;
end;

procedure TCharEffect.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
begin
  if TargetActor <> nil then begin
    rx := TActor(TargetActor).m_nRx;
    ry := TActor(TargetActor).m_nRy;
    g_PlayScene.ScreenXYfromMCXY(rx, ry, FlyX, FlyY);
    FlyX := FlyX + TActor(TargetActor).m_nShiftX;
    FlyY := FlyY + TActor(TargetActor).m_nShiftY;
    d := ImgLib.GetCachedImage(EffectBase + curframe, px, py);
    if d <> nil then begin
      Surface.DrawBlend(
        FlyX + px - UNITX div 2,
        FlyY + py - UNITY div 2,
        d, 1);
    end;
  end;
end;

constructor TMapEffect.Create(effbase, effframe: Integer; X, Y: Integer);
begin
  inherited Create(255, effbase,
    X, Y,
    X, Y,
    mtExplosion,
    False,
    0);
  TargetActor := nil;
  frame := effframe;
  NextFrameTime := 30;
  RepeatCount := 0;
end;

function TMapEffect.Run: Boolean;
begin
  Result := True;
  if GetTickCount - steptime > LongWord(NextFrameTime) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start + frame - 1;
      if RepeatCount > 0 then begin
        Dec(RepeatCount);
        curframe := start;
      end else
        Result := False;
    end;
  end;
end;

procedure TMapEffect.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
begin
  rx := targetx;
  ry := targety;
  g_PlayScene.ScreenXYfromMCXY(rx, ry, FlyX, FlyY);
  d := ImgLib.GetCachedImage(EffectBase + curframe, px, py);
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

constructor TScrollHideEffect.Create(effbase, effframe: Integer; X, Y: Integer; target: TObject);
begin
  inherited Create(effbase, effframe, X, Y);
  NextFrameTime := 50;
  //TargetCret := TActor(target);//在出现有人用随机之类时，将设置目标
end;

function TScrollHideEffect.Run: Boolean;
begin
  Result := inherited Run;
  if frame = 7 then
    if g_TargetCret <> nil then
      g_PlayScene.DeleteActor(g_TargetCret.m_nRecogId);
end;

{--------------------------------------------------------}

constructor TLightingEffect.Create(effbase, effframe: Integer; X, Y: Integer);
begin

end;

function TLightingEffect.Run: Boolean;
begin
  Result := False;                      //Jacky
end;

{--------------------------------------------------------}

constructor TFireGunEffect.Create(effbase, sX, sY, tx, ty: Integer);
begin
  inherited Create(255, effbase,
    sX, sY,
    tx, ty,                             //TActor(target).XX, TActor(target).m_nCurrY,
    mtFireGun,
    True,
    0);
  NextFrameTime := 50;
  FillChar(FireNodes, SizeOf(TFireNode) * FIREGUNFRAME, #0);
  OutofOil := False;
  firetime := GetTickCount;
end;

function TFireGunEffect.Run: Boolean;
var
  i, fx, fy                 : Integer;
  allgone                   : Boolean;
begin
  Result := True;
  if GetTickCount - steptime > LongWord(NextFrameTime) then begin
    Shift;
    steptime := GetTickCount;
    //if not FixedEffect then begin  //格钎俊 嘎瘤 臼疽栏搁
    if not OutofOil then begin
      if (abs(rx - TActor(MagOwner).m_nRx) >= 5) or (abs(ry - TActor(MagOwner).m_nRy) >= 5) or (GetTickCount - firetime > 800) then
        OutofOil := True;
      for i := FIREGUNFRAME - 2 downto 0 do begin
        FireNodes[i].firenumber := FireNodes[i].firenumber + 1;
        FireNodes[i + 1] := FireNodes[i];
      end;
      FireNodes[0].firenumber := 1;
      FireNodes[0].X := FlyX;
      FireNodes[0].Y := FlyY;
    end else begin
      allgone := True;
      for i := FIREGUNFRAME - 2 downto 0 do begin
        if FireNodes[i].firenumber <= FIREGUNFRAME then begin
          FireNodes[i].firenumber := FireNodes[i].firenumber + 1;
          FireNodes[i + 1] := FireNodes[i];
          allgone := False;
        end;
      end;
      if allgone then Result := False;
    end;
  end;
end;

procedure TFireGunEffect.DrawEff(Surface: TCustomCanvas);
var
  i, num, shx, shy, fireX, fireY, prx, pry, img: Integer;
  d                         : TCustomLockableTexture;
begin
  prx := -1;
  pry := -1;
  for i := 0 to FIREGUNFRAME - 1 do begin
    if (FireNodes[i].firenumber <= FIREGUNFRAME) and (FireNodes[i].firenumber > 0) then begin
      shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
      shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

      img := EffectBase + (FireNodes[i].firenumber - 1);
      d := ImgLib.GetCachedImage(img, px, py);
      if d <> nil then begin
        fireX := FireNodes[i].X + px - UNITX div 2 - shx;
        fireY := FireNodes[i].Y + py - UNITY div 2 - shy;
        if (fireX <> prx) or (fireY <> pry) then begin
          prx := fireX;
          pry := fireY;
          Surface.DrawBlend(fireX, fireY, d, 1);
        end;
      end;
    end;
  end;
end;

{--------------------------------------------------------}

constructor TThuderEffect.Create(effbase, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    tx, ty,
    tx, ty,                             //TActor(target).XX, TActor(target).m_nCurrY,
    mtThunder,
    False,
    0);
  TargetActor := target;

end;

procedure TThuderEffect.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + curframe, px, py);
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

{--------------------------------------------------------}

constructor TThuderEffectEx.Create(effbase, tx, ty: Integer; target: TObject; magnum: Integer);
begin
  inherited Create(magnum, effbase,
    tx, ty,
    tx, ty,
    mtThunder,
    False,
    0);
  TargetActor := target;
end;

procedure TThuderEffectEx.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + curframe, px, py);
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

{--------------------------------------------------------}

constructor TLightingThunder.Create(effbase, sX, sY, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    sX, sY,
    tx, ty,                             //TActor(target).XX, TActor(target).m_nCurrY,
    mtLightingThunder,
    False,
    0);
  TargetActor := target;
end;

procedure TLightingThunder.DrawEff(Surface: TCustomCanvas);
var
  img, sX, sY, px, py, shx, shy: Integer;
  d                         : TCustomLockableTexture;
begin
  img := EffectBase + Dir16 * 10;
  if curframe < 6 then begin

    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

    d := ImgLib.GetCachedImage(img + curframe, px, py);
    if d <> nil then begin
      g_PlayScene.ScreenXYfromMCXY(TActor(MagOwner).m_nRx,
        TActor(MagOwner).m_nRy,
        sX,
        sY);
      Surface.DrawBlend(
        sX + px - UNITX div 2,
        sY + py - UNITY div 2,
        d, 1);
    end;
  end;
  {if (curframe < 10) and (TargetActor <> nil) then begin
     d := ImgLib.GetCachedImage (EffectBase + 17*10 + curframe, px, py);
     if d <> nil then begin
        g_PlayScene.ScreenXYfromMCXY (TActor(TargetActor).RX,
                                    TActor(TargetActor).RY,
                                    sx,
                                    sy);
        DrawBlend (surface,
                   sx + px - UNITX div 2,
                   sy + py - UNITY div 2,
                   d, 1);
     end;
  end;}
end;

{--------------------------------------------------------}

constructor TExploBujaukEffect.Create(effbase, sX, sY, tx, ty: Integer; target: TObject; bt: Boolean; MagEff: Integer);
begin
  ExplosionImgLib := nil;
  inherited Create(255, effbase,
    sX, sY,
    tx, ty,
    mtExploBujauk,
    True,
    0);
  frame := 3;
  TargetActor := target;
  NextFrameTime := 50;
  bTransparent := bt;
  m_nMagEffectNo := MagEff;
end;

procedure TExploBujaukEffect.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d, bd                     : TCustomLockableTexture;
  shx, shy, x, y                  : Integer;
  meff                      : TMapEffect;
begin
  if m_boActive and ((abs(FlyX - fireX) > 30) or (abs(FlyY - fireY) > 30) or FixedEffect) then begin
    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;
    if not FixedEffect then begin
      case m_nMagEffectNo of
        10: begin
            img := EffectBase + Dir16 * 10;
            d := ImgLib.GetCachedImage(img + curframe, px, py);
            if d <> nil then begin
              if bTransparent then
                Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1)
              else
                Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
            end;
            if EffectBase2 >= 0 then begin
              img := EffectBase2 + Dir16 * 10;
              d := ImgLib.GetCachedImage(img + curframe, px, py);
              if d <> nil then Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1);
            end;
          end;
        0075: begin
            img := EffectBase + (Dir16 div 2) * 10;
            d := ImgLib.GetCachedImage(img + curframe, px, py);
            if d <> nil then begin
              X := FlyX + px - UNITX div 2 - shx;
              Y := FlyY + py - UNITY div 2 - shy;
              if bTransparent then
                Surface.DrawBlend(X, Y, d, 1)
              else
                Surface.Draw(X, Y, d.ClientRect, d, True);
            end;
          end;
        108: begin
            img := EffectBase + (Dir16 div 2) * 10;
            d := ImgLib.GetCachedImage(img + curframe, px, py);
            if d <> nil then begin
              if bTransparent then
                Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1)
              else
                Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
            end;
            if EffectBase2 >= 0 then begin
              img := EffectBase2 + (Dir16 div 2) * 10;
              d := ImgLib.GetCachedImage(img + curframe, px, py);
              if d <> nil then Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1);
            end;
          end;

        110: begin
            img := EffectBase + Dir16 * 10;
            d := ImgLib.GetCachedImage(img + curframe, px, py);
            if d <> nil then begin
              if bTransparent then
                Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1)
              else
                Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
            end;
            if EffectBase2 >= 0 then begin
              img := EffectBase2 + Dir16 * 10;
              d := ImgLib.GetCachedImage(img + curframe, px, py);
              if d <> nil then Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1);
            end;
          end;
        105 {112}: begin
            img := EffectBase;          // + Dir16 * 10;
            d := ImgLib.GetCachedImage(img + curframe, px, py);
            if d <> nil then begin
              if bTransparent then
                Surface.DrawBlend(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1)
              else
                Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
            end;
          end;
      else begin
          img := EffectBase + Dir16 * 10;
          d := ImgLib.GetCachedImage(img + curframe, px, py);
          if d <> nil then begin
            if bTransparent then
              Surface.DrawBlend( FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d, 1)
            else
              Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
          end;
        end;
      end;
    end else begin
      if m_nMagEffectNo = MAGIC_FOX_FIRE2 then begin
        NextFrameTime := 100;
        img := 1320 + curframe;
        d := g_WMon33Img.GetCachedImage(img, px, py);
        if d <> nil then begin
          X := FlyX + px - UNITX div 2;
          Y := FlyY + py - UNITY div 2;
          Surface.DrawBlend(X, Y, d, 1)
        end;
      end else if m_nMagEffectNo = MAGIC_FOX_CURSE then begin
        NextFrameTime := 100;
        img := 1330 + curframe;
        d := g_WMon33Img.GetCachedImage(img, px, py);
        if d <> nil then begin
          X := FlyX + px - UNITX div 2;
          Y := FlyY + py - UNITY div 2;
          Surface.DrawBlend(X, Y, d, 1)
        end;
      end else if m_nMagEffectNo = 108 then begin
        img := MagExplosionBase + curframe + (Dir16 div 2) * 10;
        d := ImgLib.GetCachedImage(img, px, py);
        if d <> nil then Surface.Draw(FlyX + px - UNITX div 2, FlyY + py - UNITY div 2, d.ClientRect, d, True);

        img := MagExplosionBase + (Dir16 div 2) * 10 + 80;
        d := ImgLib.GetCachedImage(img + curframe, px, py);
        if d <> nil then Surface.DrawBlend(FlyX + px - UNITX div 2, FlyY + py - UNITY div 2, d, 1)
      end else if m_nMagEffectNo = 105 {112} then begin
        img := MagExplosionBase;
        d := ImgLib.GetCachedImage(img + curframe, px, py);
        if d <> nil then Surface.DrawBlend(FlyX + px - UNITX div 2, FlyY + py - UNITY div 2, d, 1)
      end else begin
        img := MagExplosionBase + curframe;
        if ExplosionImgLib <> nil then
          d := ExplosionImgLib.GetCachedImage(img, px, py)
        else
          d := ImgLib.GetCachedImage(img, px, py);
        if d <> nil then begin
          Surface.DrawBlend(FlyX + px - UNITX div 2, FlyY + py - UNITY div 2, d, 1);
        end;
        if (m_nMagEffectNo = 0075) and (curframe = 1) then begin
          g_SndMgr.PlaySound('Wav\M39-2.wav');
          g_SndMgr.PlaySound('Wav\M39-2.wav');
        end;
      end;
    end;
  end;
end;

constructor TBujaukGroundEffect.Create(effbase, magicnumb, sX, sY, tx, ty: Integer; lv: Integer);
begin
  inherited Create(255, effbase,
    sX, sY,
    tx, ty,
    mtBujaukGroundEffect,
    True,
    0);

  mLevel := lv;
  frame := 3;
  MagicNumber := magicnumb;
  BoGroundEffect := False;

  NextFrameTime := 50;
end;

function TBujaukGroundEffect.Run: Boolean;
var
  mx, my                    : Integer;
begin
  Result := inherited Run;
  if not FixedEffect then begin
    if ((abs(targetx - FlyX) <= 15) and (abs(targety - FlyY) <= 15)) then begin
      FixedEffect := True;
      start := 0;
      frame := ExplosionFrame;
      curframe := start;
      Repetition := False;
      g_PlayScene.CXYfromMouseXY(targetx, targety, mx, my);
      g_SndMgr.PlaySound(TActor(MagOwner).m_nMagicExplosionSound, mx, my);
      Result := True;
    end else begin
      prevdisx := abs(targetx - FlyX);
      prevdisy := abs(targety - FlyY);
    end;
  end;
end;

procedure TBujaukGroundEffect.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d                         : TCustomLockableTexture;
  shx, shy                  : Integer;
  meff                      : TMapEffect;
begin
  if m_boActive and ((abs(FlyX - fireX) > 30) or (abs(FlyY - fireY) > 30) or FixedEffect) then begin

    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

    if not FixedEffect then begin
      if MagicNumber = 74 then begin
        img := EffectBase + (Dir16 div 2) * 10;
        d := ImgLib.GetCachedImage(img + curframe, px, py);
        if d <> nil then
          Surface.DrawBlend(
            FlyX + px - UNITX div 2 - shx,
            FlyY + py - UNITY div 2 - shy,
            d, 1);

      end else begin
        img := EffectBase + Dir16 * 10;
        d := ImgLib.GetCachedImage(img + curframe, px, py);
        if d <> nil then
          Surface.Draw(FlyX + px - UNITX div 2 - shx, FlyY + py - UNITY div 2 - shy, d.ClientRect, d, True);
      end;
    end else begin
      if MagicNumber = 46 then begin
        GetEffectBase(MagicNumber - 1, 0, ImgLib, img);
        img := img + 10 + curframe;
      end else if MagicNumber = 74 then begin
        GetEffectBase(MagicNumber - 1, 0, ImgLib, img);
        img := img + curframe;
      end else if MagicNumber = 11 then begin
        case mLevel div 4 of
          1: begin
              img := 2470 + curframe;
            end;
          2: begin
              img := 2490 + curframe;
            end;
          3: begin
              img := 2520 + curframe;
            end;
        else
          img := EffectBase + 160 + curframe;
        end;
      end else if MagicNumber = 12 then begin
        case mLevel div 4 of
          1: begin
              img := 2410 + curframe;
            end;
          2: begin
              img := 2430 + curframe;
            end;
          3: begin
              img := 2450 + curframe;
            end;
        else
          img := EffectBase + 180 + curframe;
        end;
      end;

      if mLevel > 3 then
        d := g_WMagic7Images.GetCachedImage(img, px, py)
      else
        d := ImgLib.GetCachedImage(img, px, py);

      if d <> nil then
        Surface.DrawBlend(FlyX + px - UNITX div 2, FlyY + py - UNITY div 2, d, 1);

    end;
  end;
end;

{ TNormalDrawEffect }

constructor TNormalDrawEffect.Create(xx, yy: Integer; WMImage: TWMImages; effbase, nFrame: Integer; frmTime: LongWord; boFlag: Boolean; SoundIdx: Integer = 0);
begin
  inherited Create(255, effbase, xx, yy, xx, yy, mtReady, True, 0);
  ImgLib := WMImage;
  EffectBase := effbase;
  start := 0;
  curframe := 0;
  frame := nFrame;
  NextFrameTime := frmTime;
  boBlend := boFlag;
  FSoundIdx := SoundIdx;
end;

procedure TNormalDrawEffect.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
  nRx, nRy, nPx, nPy        : Integer;
begin
  d := ImgLib.GetCachedImage(EffectBase + curframe, nPx, nPy);
  if d <> nil then begin
    g_PlayScene.ScreenXYfromMCXY(FlyX, FlyY, nRx, nRy);
    if boBlend then begin
      Surface.DrawBlend(nRx + nPx - UNITX div 2, nRy + nPy - UNITY div 2, d, 1);
    end else begin
      Surface.Draw(nRx + nPx - UNITX div 2, nRy + nPy - UNITY div 2, d.ClientRect, d, True);
    end;
  end;
end;

function TNormalDrawEffect.Run: Boolean;
begin
  Result := True;
  if m_boActive and (GetTickCount - steptime > LongWord(NextFrameTime)) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if (FSoundIdx > 0) and ((curframe - start) = 2) then begin
      //
      //DScreen.AddChatBoardString(IntToStr(FSoundIdx), clWhite, clRed);
      g_SndMgr.PlaySound(FSoundIdx, fireX, fireY);
      if FSoundIdx = 10512 then g_SndMgr.PlaySound(122, fireX, fireY);
      FSoundIdx := 0;
    end;
    if curframe > start + frame - 1 then begin
      curframe := start;
      Result := False;
    end;
  end;
end;

{ TFlyingBug }

constructor TFlyingBug.Create(id, effnum, sX, sY, tx, ty: Integer;
  Mtype: TMagicType; Recusion: Boolean; anitime: Integer);
begin
  inherited Create(id, effnum, sX, sY, tx, ty, Mtype, Recusion, anitime);
  FlyImageBase := FLYOMAAXEBASE;
  ReadyFrame := 65;
end;

procedure TFlyingBug.DrawEff(Surface: TCustomCanvas);
var
  img                       : Integer;
  d                         : TCustomLockableTexture;
  shx, shy                  : Integer;
begin
  if m_boActive and ((abs(FlyX - fireX) > ReadyFrame) or (abs(FlyY - fireY) > ReadyFrame)) then begin
    shx := (g_MySelf.m_nRx * UNITX + g_MySelf.m_nShiftX) - FireMyselfX;
    shy := (g_MySelf.m_nRy * UNITY + g_MySelf.m_nShiftY) - FireMyselfY;

    if not FixedEffect then begin
      img := FlyImageBase + (Dir16 div 2) * 10;
      d := ImgLib.GetCachedImage(img + curframe, px, py);
      if d <> nil then begin
        Surface.Draw(FlyX + px - UNITX div 2 - shx,
          FlyY + py - UNITY div 2 - shy,
          d.ClientRect, d, True);
      end;
    end else begin
      img := curframe + MagExplosionBase;
      d := ImgLib.GetCachedImage(img, px, py);
      if d <> nil then begin
        Surface.Draw(FlyX + px - UNITX div 2,
          FlyY + py - UNITY div 2,
          d.ClientRect, d, True);
      end;
    end;
  end;
end;

{ TFlyingFireBall }

procedure TFlyingFireBall.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
begin
  if m_boActive and ((abs(FlyX - fireX) > ReadyFrame) or (abs(FlyY - fireY) > ReadyFrame)) then begin
    d := ImgLib.GetCachedImage(FlyImageBase + (GetFlyDirection(FlyX, FlyY, targetx, targety) * 10) + curframe, px, py);
    if d <> nil then
      Surface.DrawBlend(
        FlyX + px - UNITX div 2,
        FlyY + py - UNITY div 2,
        d, 1);
  end;
end;

constructor THeroCharEffect.Create(WMImage: TWMImages; effbase, effframe: Integer; frmTime: LongWord; target: TObject);
begin
  inherited Create(255, effbase,
    TActor(target).m_nCurrX, TActor(target).m_nCurrY,
    TActor(target).m_nCurrX, TActor(target).m_nCurrY,
    mtExplosion,
    False,
    0);
  ImgLib := WMImage;
  TargetActor := target;
  frame := effframe;
  NextFrameTime := frmTime;
end;

function THeroCharEffect.Run: Boolean;
begin
  Result := True;
  if GetTickCount - steptime > LongWord(NextFrameTime) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start + frame - 1;
      Result := False;
    end;
  end;
end;

procedure THeroCharEffect.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
begin
  if TargetActor <> nil then begin
    rx := TActor(TargetActor).m_nRx;
    ry := TActor(TargetActor).m_nRy;
    g_PlayScene.ScreenXYfromMCXY(rx, ry, FlyX, FlyY);
    FlyX := FlyX + TActor(TargetActor).m_nShiftX;
    FlyY := FlyY + TActor(TargetActor).m_nShiftY;
    d := ImgLib.GetCachedImage(EffectBase + curframe, px, py);
    if d <> nil then begin
      Surface.DrawBlend(
        FlyX + px - UNITX div 2,
        FlyY + py - UNITY div 2,
        d, 1);
    end;
  end;
end;

{--------------------------------------------------------}

constructor TRedThunderEffect.Create(effbase, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    tx, ty,
    tx, ty,
    mtRedThunder,
    False,
    0);
  TargetActor := nil;
  frame := 5;
  n0 := Random(7);
end;

procedure TRedThunderEffect.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  ImgLib := g_WDragonImg;
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + (10 * n0) + curframe, px, py); //blue
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

constructor TRedGroundThunderEffect.Create(effbase, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    tx, ty,
    tx, ty,
    mtRedGroundThunder,
    False,
    0);
  TargetActor := nil;
  frame := 5;
  n0 := Random(3);
end;

procedure TRedGroundThunderEffect.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  ImgLib := g_WDragonImg;
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + (10 * n0) + curframe, px, py); //blue
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

constructor TLavaEffect.Create(effbase, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    tx, ty,
    tx, ty,
    mtLava,
    False,
    0);
  TargetActor := nil;
  frame := 20;
end;

procedure TLavaEffect.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  ImgLib := g_WDragonImg;
  //draw explosion
  {if curframe < 10 then begin
    img := 470;
    d := ImgLib.GetCachedImage(img + curframe, px, py);
    if d <> nil then begin
      DrawBlend(Surface,
        FlyX + px - UNITX div 2,
        FlyY + py - UNITY div 2,
        d, 1);
    end;
  end;}
  //draw the rest
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + curframe, px, py);
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

constructor TSpurtEffect.Create(effbase, tx, ty: Integer; target: TObject);
begin
  inherited Create(255, effbase,
    tx, ty,
    tx, ty,
    mtSpurt,
    False,
    0);
  TargetActor := nil;
  frame := 10;
end;

procedure TSpurtEffect.DrawEff(Surface: TCustomCanvas);
var
  img, px, py               : Integer;
  d                         : TCustomLockableTexture;
begin
  ImgLib := g_WDragonImg;
  img := EffectBase;
  d := ImgLib.GetCachedImage(img + curframe, px, py);
  if d <> nil then begin
    Surface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      d, 1);
  end;
end;

constructor THumanEffects.Create(xx, yy: Integer; WMImage: TWMImages; effbase, nX: Integer; frmTime: LongWord; boFlag: Boolean);
begin
  inherited Create(255, effbase, xx, yy, xx, yy, mtReady, True, 0);
  ImgLib := WMImage;
  EffectBase := effbase;
  start := 0;
  curframe := 0;
  frame := nX;
  NextFrameTime := frmTime;
  boC8 := boFlag;
end;

procedure THumanEffects.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
  rx, ry, nRx, nRy, nPx, nPy, shx, shy: Integer;
begin
  d := ImgLib.GetCachedImage(EffectBase + curframe, nPx, nPy);
  if d <> nil then begin
    rx := TActor(g_MySelf).m_nRx;
    ry := TActor(g_MySelf).m_nRy;
    g_PlayScene.ScreenXYfromMCXY(rx, ry, nRx, nRy);
    nRx := nRx + TActor(g_MySelf).m_nShiftX;
    nRy := nRy + TActor(g_MySelf).m_nShiftY;
    if boC8 then begin
      Surface.DrawBlend(
        nRx + nPx - UNITX div 2,
        nRy + nPy - UNITY div 2,
        d, 1);
    end else begin
      Surface.Draw(nRx + nPx - UNITX div 2, nRy + nPy - UNITY div 2, d.ClientRect, d, True);
    end;
  end;
end;

function THumanEffects.Run: Boolean;
begin
  Result := True;
  if m_boActive and (GetTickCount - steptime > LongWord(NextFrameTime)) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start;
      Result := False;
    end;
  end;
end;

{ TobjectEffect}

constructor TObjectEffects.Create(ObjectiD2: TObject; WMImage: TWMImages; effbase, nX: Integer; frmTime: LongWord; boFlag: Boolean);
begin
  inherited Create(255, effbase, 0, 0, 0, 0, mtReady, True, 0);
  ImgLib := WMImage;
  EffectBase := effbase;
  start := 0;
  curframe := 0;
  frame := nX;
  NextFrameTime := frmTime;
  boC8 := boFlag;
  ObjectID := ObjectiD2;
end;

procedure TObjectEffects.DrawEff(Surface: TCustomCanvas);
var
  d                         : TCustomLockableTexture;
  rx, ry, nRx, nRy, nPx, nPy, shx, shy: Integer;
begin
  d := ImgLib.GetCachedImage(EffectBase + curframe, nPx, nPy);
  if (d <> nil) and (ObjectID <> nil) then begin
    rx := TActor(ObjectID).m_nRx;
    ry := TActor(ObjectID).m_nRy;
    g_PlayScene.ScreenXYfromMCXY(rx, ry, nRx, nRy);
    nRx := nRx + TActor(ObjectID).m_nShiftX;
    nRy := nRy + TActor(ObjectID).m_nShiftY;
    if boC8 then begin
      Surface.DrawBlend(
        nRx + nPx - UNITX div 2,
        nRy + nPy - UNITY div 2,
        d, 1);
    end else begin
      Surface.Draw(nRx + nPx - UNITX div 2, nRy + nPy - UNITY div 2, d.ClientRect, d, True);
    end;
  end;
end;

function TObjectEffects.Run: Boolean;
begin
  Result := True;
  if m_boActive and (GetTickCount - steptime > LongWord(NextFrameTime)) then begin
    steptime := GetTickCount;
    Inc(curframe);
    if curframe > start + frame - 1 then begin
      curframe := start;
      Result := False;
    end;
  end;
end;

end.

