//******************************************************************************
//*                                                                            *
//*                         AsphyreSphinx绘图修改                              *
//*                         作者：盼盼 QQ117594672                             *
//*                                                                            *
//******************************************************************************
unit MShare;

interface

uses
  Windows, Classes, Dialogs, SysUtils, Forms, PXL.Canvas, PXL.Textures, PXL.Providers, pngimage,
  PXL.SwapChains, PXL.Types, PXL.Devices, DWinCtl, HumanActor, Actor, Grobal2, Wil, uEDcode,
  DIB, GList, IniFiles, HashList, Graphics, MirEffect, CnHashTable, uCommon, uGameEngine,SimpleMsgPack;

const
  MAXX = 52; //SCREENWIDTH div 20;
  MAXY = 40; //SCREENWIDTH div 20;
  LONGHEIGHT_IMAGE = 35;
  FLASHBASE = 410;
  SOFFX = 0;
  SOFFY = 0;
  HEALTHBAR_BLACK = 0;
  HEALTHBAR_RED = 1;
  BARWIDTH = 30;
  BARHEIGHT = 2;
  MAXSYSLINE = 8;
  BOTTOMBOARD = 1;
  AREASTATEICONBASE = 150;
  g_WinBottomRetry = 45;
  ResourceDir: String = '91Resource\';
  UserCfgDir: String = '91Resource\Users\';
  DEFSCREENWIDTH = 800;
  DEFSCREENHEIGHT = 600;
var
  g_UIInitialized: Boolean = False;
  g_Titles: THumTitles;
  g_ActiveTitle: THumTitle;
  g_hTitles: THumTitles;
  g_hActiveTitle: THumTitle;

  g_fWZLFirst: Byte = 7;

  g_boLogon: Boolean = False;
  g_fGoAttack: Boolean = False;

  g_dwReGetMapTitleTick: Longword = 0;
  g_QueryWinBottomTick: Longword;
  g_fGetRenderBottom: Boolean = False;

  g_nDragonRageStateIndex: Integer = 0;
  SCREENWIDTH: Integer = 800;
  SCREENHEIGHT: Integer = 600;
  g_VSync: Boolean = True;
  AAX: Integer = 26 + 14;
  LMX: Integer = 30;
  LMY: Integer = 26;
  VIEWWIDTH: Integer = 8;

  BOTTOMTOP: Integer = 800 - 251;
  WINRIGHT: Integer = 800 - 60;
  BOTTOMEDGE: Integer = 600 - 30;
  SURFACEMEMLEN: Integer = ((800 + 3) div 4) * 4 * 600;
  MAPSURFACEWIDTH: Integer = 800;
  MAPSURFACEHEIGHT: Integer = 600 - 150;
  BOXWIDTH: Integer = (800 div 2 - 214) * 2;

  g_SkidAD_Rect: TRect;
  g_SkidAD_Rect2: TRect;
  G_RC_LEVEL: TRect;
  G_RC_EXP: TRect;
  G_RC_WEIGTH: TRect;
  G_RC_SQUENGINER: TRect;
  G_RC_IMEMODE: TRect;

const
  {------------}
  NEWHINTSYS = True;

  NPC_CILCK_INVTIME = 500;
  MAXITEMBOX_WIDTH = 177;
  MAXMAGICLV = 3;
  RUNLOGINCODE = 1200205220;
  CLIENT_VERSION_NUMBER = 120020522;
  STDCLIENT = 0;
  RMCLIENT = 46;
  CLIENTTYPE = RMCLIENT;
  CUSTOMLIBFILE = 0;
  DEBUG = 0;
  LOGICALMAPUNIT = 30; //1108 40;
  HUMWINEFFECTTICK = 200;

  WINLEFT = 60;
  WINTOP = 60;
  MINIMAPSIZE = 200;

  MAPDIR = '.\Map\'; //地图文件所在目录
  CONFIGFILE = '.\Config\%s.ini';
{$IF CUSTOMLIBFILE = 1}
  EFFECTIMAGEDIR = 'Graphics\Effect\';
  MAINIMAGEFILE = 'Graphics\FrmMain\Main.data';
  MAINIMAGEFILE2 = 'Graphics\FrmMain\Main2.data';
  MAINIMAGEFILE3 = 'Graphics\FrmMain\Main3.data';
{$ELSE}
  MAINIMAGEFILE = 'Data\Prguse.data';
  MAINIMAGEFILE2 = 'Data\Prguse2.data';
  MAINIMAGEFILE3 = 'Data\Prguse3.data';
  MAINIMAGEFILE3_16 = 'Data\Prguse3_16.data';
  EFFECTIMAGEDIR = 'Data\';
{$IFEND}

  CHRSELIMAGEFILE = 'Data\ChrSel.data';
  MINMAPIMAGEFILE = 'Data\mmap.data';
  TITLESIMAGEFILE = 'Data\Tiles.data';
  TITLESIMAGEFILEX = 'Data\Tiles%d.data';

  SMLTITLESIMAGEFILE = 'Data\SmTiles.data';
  SMLTITLESIMAGEFILEX = 'Data\SmTiles%d.data';


  HUMWINGIMAGESFILE = 'Data\HumEffect.data';
  HUMWINGIMAGESFILE2 = 'Data\HumEffect2.data';
  HUMWINGIMAGESFILE3 = 'Data\HumEffect3.data';
  MAGICONIMAGESFILE = 'Data\MagIcon.data';
  MAGICON2IMAGESFILE = 'Data\MagIcon2.data';

  HUMIMGIMAGESFILE = 'Data\Hum.data';
  HUM2IMGIMAGESFILE = 'Data\Hum2.data';
  HUM3IMGIMAGESFILE = 'Data\Hum3.data';

  HAIRIMGIMAGESFILE = 'Data\Hair.data';
  HAIR2IMGIMAGESFILE = 'Data\Hair2.data';

  WEAPONIMAGESFILE = 'Data\Weapon.data';
  WEAPON2IMAGESFILE = 'Data\Weapon2.data';
  WEAPON3IMAGESFILE = 'Data\Weapon3.data';

  NPCIMAGESFILE = 'Data\Npc.data';
  NPC2IMAGESFILE = 'Data\Npc2.data';
  MAGICIMAGESFILE = 'Data\Magic.data';
  MAGIC2IMAGESFILE = 'Data\Magic2.data';
  MAGIC3IMAGESFILE = 'Data\Magic3.data';
  MAGIC4IMAGESFILE = 'Data\Magic4.data';

  MAGIC5IMAGESFILE = 'Data\Magic5.data';
  MAGIC6IMAGESFILE = 'Data\Magic6.data';
  MAGIC7IMAGESFILE = 'Data\magic7-16.data';
  MAGIC7IMAGESFILE2 = 'Data\magic7.data';
  MAGIC8IMAGESFILE = 'Data\magic8-16.data';
  MAGIC8IMAGESFILE2 = 'Data\magic8.data';
  MAGIC9IMAGESFILE = 'Data\magic9.data';
  MAGIC10IMAGESFILE = 'Data\magic10.data';
  STATEEFFECTFILE = 'Data\StateEffect.data';

  SHINEEFFECTFILE = 'Data\ShineEffect.data';
 // SHINEITEMFILE = 'Data\Shineitem.data'; //人物
  SHINEITEMSFILE = 'Data\ShineItems.data'; //背包
  SHINEDNITEMSFILE = 'Data\ShineDnItems.data'; //地上

  ANITILESIMAGEFILE = 'Data\AniTiles1.data';

  EVENTEFFECTIMAGESFILE = EFFECTIMAGEDIR + 'Event.data';

  BAGITEMIMAGESFILE = 'Data\Items.data';
  BAGITEMIMAGESFILE2 = 'Data\Items2.data';
  STATEITEMIMAGESFILE = 'Data\StateItem.data';
  STATEITEMIMAGESFILE2 = 'Data\StateItem2.data';
  DNITEMIMAGESFILE = 'Data\DnItems.data';
  DNITEMIMAGESFILE2 = 'Data\DnItems2.data';

  OBJECTIMAGEFILE = 'Data\Objects.data';
  OBJECTIMAGEFILE1 = 'Data\Objects%d.data';
  MONIMAGEFILE = 'Data\Mon%d.data';
  DRAGONIMAGEFILE = 'Data\Dragon.data';
  EFFECTIMAGEFILE = 'Data\Effect.data';

  cboEffectFile = 'Data\cboEffect.data';
  cbohairFile = 'Data\cbohair.data';
  cbohumFile = 'Data\cbohum.data';
  cbohum3File = 'Data\cbohum3.data';

  cboHumEffectFile = 'Data\cboHumEffect.data';
  cboHumEffect2File = 'Data\cboHumEffect2.data';
  cboHumEffect3File = 'Data\cboHumEffect3.data';
  cboweaponFile = 'Data\cboweapon.data';
  cboweapon3File = 'Data\cboweapon3.data';

  bInterfaceFile = 'Data\bInterface.data';

  MONIMAGEFILEEX = 'Data\%d.data';
  MONPMFILE = 'Data\%d.pm';

  //MAXX                      = SCREENWIDTH div 20;
  //MAXY                      = SCREENWIDTH div 20;

  DEFAULTCURSOR = 0; //系统默认光标
  IMAGECURSOR = 1; //图形光标
  USECURSOR = DEFAULTCURSOR; //使用什么类型的光标

  MAXBAGITEMCL = 52;
  MAXFONT = 8;
  ENEMYCOLOR = 69;
  HERO_MIIDX_OFFSET = 5000;
  SAVE_MIIDX_OFFSET = HERO_MIIDX_OFFSET + 500;
  STALL_MIIDX_OFFSET = HERO_MIIDX_OFFSET + 500 + 50;
  DETECT_MIIDX_OFFSET = HERO_MIIDX_OFFSET + 500 + 50 + 10 + 1;
  MSGMUCH = 2;

  g_sHumAttr: array[1..5] of string = ('金', '木', '水', '火', '土');
{$IFDEF UI_0508}
  g_DBStateStrArr: array[0..3] of string = ('装备', '状态', '属性', '技能');
  g_DBStateStrArrUS: array[0..2] of string = ('装备', '时装', '称号');
  g_DBStateStrArr2: array[0..3] of string = ('状态', '技能', '经络', '连击');
{$ELSE}
  g_DBStateStrArr: array[0..6] of string = ('装备', '时装', '状态', '属性', '称号', '技能', '其他');
  g_DBStateStrArrUS: array[0..2] of string = ('装备', '时装', '称号');
  g_DBStateStrArr2: array[0..4] of string = ('状态', '技能', '经络', '连击', '其他');
{$ENDIF}
{$IF SERIESSKILL}
  g_VenationLvStrArr: array[0..6] of string = ('经络未通', '经络已通', '一重经络', '二重经络', '三重经络', '四重经络', '五重经络');
{$IFEND SERIESSKILL}

  g_slegend: array[0..13] of string = (
    '', //0
    '传奇神剑', //1
    '传奇勋章', //2
    '传奇项链', //3
    '传奇之冠', //4
    '', //5
    '传奇护腕', //6
    '', //7
    '传奇之戒', //8
    '', //9
    '传奇腰带', //10
    '传奇之靴', //11
    '', //12
    '传奇面巾' //13
    );

  //config
  MAX_GC_GENERAL = 16;
  g_ptGeneral: array[0..MAX_GC_GENERAL] of TRect = (
    (Left: 35 + 000; Top: 70 + 23 * 0; Right: 35 + 000 + 72 + 18; Bottom: 70 + 23 * 0 + 16), //0
    (Left: 35 + 000; Top: 70 + 23 * 1; Right: 35 + 000 + 72 + 18; Bottom: 70 + 23 * 1 + 16), //1
    (Left: 35 + 000; Top: 70 + 23 * 2; Right: 35 + 000 + 78 + 18; Bottom: 70 + 23 * 2 + 16), //2
    (Left: 35 + 000; Top: 70 + 23 * 3; Right: 35 + 000 + 96; Bottom: 70 + 23 * 3 + 16), //3

    (Left: 35 + 120; Top: 70 + 23 * 0; Right: 35 + 120 + 72 + 30; Bottom: 70 + 23 * 0 + 16), //4
    (Left: 35 + 120; Top: 70 + 23 * 1; Right: 35 + 120 + 72; Bottom: 70 + 23 * 1 + 16), //5
    (Left: 35 + 120; Top: 70 + 23 * 2; Right: 35 + 120 + 72 + 18; Bottom: 70 + 23 * 2 + 16), //6
    (Left: 35 + 120; Top: 70 + 23 * 3; Right: 35 + 120 + 72; Bottom: 70 + 23 * 3 + 16), //7
    (Left: 35 + 120; Top: 70 + 23 * 4; Right: 35 + 120 + 72 + 18; Bottom: 70 + 23 * 4 + 16),

    (Left: 35 + 240; Top: 70 + 23 * 0; Right: 35 + 240 + 72; Bottom: 70 + 23 * 0 + 16),
    (Left: 35 + 240; Top: 70 + 23 * 1; Right: 35 + 240 + 72; Bottom: 70 + 23 * 1 + 16),
    (Left: 35 + 240; Top: 70 + 23 * 2; Right: 35 + 240 + 48; Bottom: 70 + 23 * 2 + 16),
    (Left: 35 + 240; Top: 70 + 23 * 3; Right: 35 + 240 + 72; Bottom: 70 + 23 * 3 + 16),
    (Left: 35 + 240; Top: 70 + 23 * 4; Right: 35 + 240 + 72; Bottom: 70 + 23 * 4 + 16),
    (Left: 35 + 240; Top: 70 + 23 * 5; Right: 35 + 240 + 72; Bottom: 70 + 23 * 5 + 16),

    (Left: 35 + 120; Top: 70 + 23 * 5; Right: 35 + 120 + 72; Bottom: 70 + 23 * 5 + 16),
    (Left: 35 + 000; Top: 70 + 23 * 5; Right: 35 + 000 + 96; Bottom: 70 + 23 * 5 + 16)
    );
  g_caGeneral: array[0..MAX_GC_GENERAL] of string = (
    '人名显示(Z)', //0
    '持久警告(X)', //1
    '免Shift键(C)', //2
    '显示经验过滤', //3
    '物品提示(ESC)', //4
    '显示过滤', //'显示过滤',                     //5
    '隐藏人物翅膀', //6
    '人物简装', //'拣取过滤',                     //7
    '隐藏尸体(V)', //8
    '自动修理', //9
    '屏幕震动', //10
    '音效',     //11
    '怪物简装',  //12
    '数字飘血',   //13
    '装备比较',   //14
    '怪物显名',    //15
    '显示称号'      //16
    );
  g_HintGeneral: array[0..MAX_GC_GENERAL] of string = (
    '钩选此项将全屏显示玩家名字', //0
    '钩选此项在装备持久低时进行提示', //1
    '钩选此项将不需要按Shift也能\攻击其他玩家', //2
    '钩选此项将隐藏聊天栏中低于\设置的经验值提示', //3
    '钩选此项将显示地面未过滤的\物品的名字', //4
    '钩选此项将过滤部分地面物品的显示', //5
    '钩选此项将不显示人物翅膀效果，可避免\电脑较差且翅膀显示过多而导致卡问题', //6
    '', //'钩选此项将不自动拣取未显示\在地面上的物品', //7
    '钩选此项将隐藏已死亡的怪物\尸体，避免资源效果，游戏更流畅', //8
    '钩选此项将自动修理身上装备\包裹需放置有一定持久的修复神水', //9
    '钩选此项将触发游戏中的屏幕震动效果',
    '钩选此项将启用游戏音效反之\则关闭游戏的音效',
    '',
    '钩选此项将触发开启伤害值显示',
    '是否开启装备比较功能',
    '钩选此项显示怪物名字',
    '钩选此项将显示人物头顶的称号'
    );
  g_gcGeneral: array[0..MAX_GC_GENERAL] of Boolean = (True, True, False, True, True, True, False, False, False, True, True, True, False, True, False, True, True);
  g_clGeneral: array[0..MAX_GC_GENERAL] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver);
  g_clGeneralDef: array[0..MAX_GC_GENERAL] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver);

  //====================================Protect====================================
  MAX_GC_PROTECT = 11;
  g_ptProtect: array[0..MAX_GC_PROTECT] of TRect = (
    (Left: 35 + 000; Top: 70 + 24 * 0; Right: 35 + 000 + 20; Bottom: 70 + 24 * 0 + 16), //0
    (Left: 35 + 000; Top: 70 + 24 * 1; Right: 35 + 000 + 20; Bottom: 70 + 24 * 1 + 16), //1
    (Left: 35 + 000; Top: 70 + 24 * 2; Right: 35 + 000 + 20; Bottom: 70 + 24 * 2 + 16), //2
    (Left: 35 + 000; Top: 70 + 24 * 3; Right: 35 + 000 + 20; Bottom: 70 + 24 * 3 + 16), //3
    (Left: 35 + 000; Top: 70 + 24 * 4; Right: 35 + 000 + 20; Bottom: 70 + 24 * 4 + 16), //4
    (Left: 35 + 000; Top: 70 + 24 * 5; Right: 35 + 000 + 20; Bottom: 70 + 24 * 5 + 16), //5
    (Left: 35 + 000; Top: 70 + 24 * 6; Right: 35 + 000 + 72; Bottom: 70 + 24 * 6 + 16), //6
    (Left: 35 + 180; Top: 70 + 24 * 0; Right: 35 + 180 + 20; Bottom: 70 + 24 * 0 + 16), //0
    (Left: 35 + 180; Top: 70 + 24 * 1; Right: 35 + 180 + 20; Bottom: 70 + 24 * 1 + 16), //1
    (Left: 35 + 180; Top: 70 + 24 * 3; Right: 35 + 180 + 20; Bottom: 70 + 24 * 3 + 16),
    (Left: 35 + 180; Top: 70 + 24 * 5; Right: 35 + 180 + 20; Bottom: 70 + 24 * 5 + 16),
    (Left: 35 + 180; Top: 70 + 24 * 6; Right: 35 + 180 + 20; Bottom: 70 + 24 * 6 + 16)
    );
  g_caProtect: array[0..MAX_GC_PROTECT] of string = (
    'HP               毫秒', //0
    'MP               毫秒', //1
    '', //2
    'HP               毫秒', //3
    '', //4
    'HP               毫秒', //5
    '卷轴类型', //6
    'HP               毫秒', //7
    'MP               毫秒', //8
    'HP               毫秒', //9
    'HP', //10
    'MP不足允许使用特殊药品'
    );
  g_sRenewBooks: array[0..MAX_GC_PROTECT] of string = (
    '随机传送卷', //shape = 2
    '地牢逃脱卷', //shape = 1
    '回城卷', //shape = 3
    '行会回城卷', //shape = 5
    '盟重传送石',
    '比奇传送石',
    '随机传送石',
    '',
    '',
    '',
    '',
    ''
    );
  g_gcProtect: array[0..MAX_GC_PROTECT] of Boolean = (False, False, False, False, False, False, False, True, True, True, False, True);
  g_gnProtectPercent: array[0..MAX_GC_PROTECT] of Integer = (10, 10, 10, 10, 10, 10, 0, 88, 88, 88, 20, 00);
  g_gnProtectTime: array[0..MAX_GC_PROTECT] of Integer = (4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 1000, 1000, 1000);
  g_clProtect: array[0..MAX_GC_PROTECT] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clLime);

  //====================================Tec====================================
  MAX_GC_TEC = 14;
  g_ptTec: array[0..MAX_GC_TEC] of TRect = (
    (Left: 35 + 000; Top: 70 + 20 * 0; Right: 35 + 000 + 72; Bottom: 70 + 20 * 0 + 16), //0
    (Left: 35 + 000; Top: 70 + 20 * 1; Right: 35 + 000 + 72; Bottom: 70 + 20 * 1 + 16), //1
    (Left: 35 + 000; Top: 70 + 20 * 2; Right: 35 + 000 + 72; Bottom: 70 + 20 * 2 + 16), //2
    (Left: 35 + 000; Top: 70 + 20 * 3; Right: 35 + 000 + 72; Bottom: 70 + 20 * 3 + 16), //3

    (Left: 35 + 120; Top: 70 + 24 * 0; Right: 35 + 120 + 72; Bottom: 70 + 24 * 0 + 16), //4
    (Left: 35 + 120; Top: 70 + 24 * 1; Right: 35 + 120 + 72; Bottom: 70 + 24 * 1 + 16), //5

    (Left: 35 + 240; Top: 70 + 24 * 0; Right: 35 + 240 + 72; Bottom: 70 + 24 * 0 + 16), //6

    (Left: 35 + 120; Top: 70 + 24 * 5; Right: 35 + 120 + 72; Bottom: 70 + 24 * 5 + 16), //7
    (Left: 35 + 120; Top: 70 + 24 * 6; Right: 35 + 120 + 20; Bottom: 70 + 24 * 6 + 16), //8

    (Left: 35 + 000; Top: 70 + 20 * 4; Right: 35 + 000 + 72; Bottom: 70 + 20 * 4 + 16), //9
    (Left: 35 + 000; Top: 70 + 20 * 5; Right: 35 + 000 + 72; Bottom: 70 + 20 * 5 + 16),
    (Left: 35 + 000; Top: 70 + 20 * 6; Right: 35 + 000 + 72; Bottom: 70 + 20 * 6 + 16),

    (Left: 35 + 240; Top: 70 + 24 * 5; Right: 35 + 240 + 72; Bottom: 70 + 24 * 5 + 16),
    (Left: 35 + 000; Top: 70 + 20 * 7; Right: 35 + 000 + 72; Bottom: 70 + 20 * 7 + 16),
    (Left: 35 + 120; Top: 70 + 24 * 2 + 12; Right: 35 + 120 + 72; Bottom: 70 + 24 * 2 + 16 + 12)
    );

  g_HintTec: array[0..MAX_GC_TEC] of string = (
    '钩选此项将开启刀刀刺杀', //0
    '钩选此项将开启智能半月', //1
    '钩选此项将自动凝聚烈火剑法', //2
    '钩选此项将自动凝聚逐日剑法', //3
    '钩选此项将自动开启魔法盾', //4
    '钩选此项英雄将自动开启魔法盾', //5
    '钩选此项道士将自动使用隐身术', //6
    '',
    '',
    '钩选此项将自动凝聚雷霆剑法', //7
    '钩选此项将自动进行隔位刺杀', //8
    '钩选此项将自动凝聚断空斩', //9
    '钩选此项英雄将不使用连击打怪\方便玩家之间进行PK',
    '钩选此项将自动凝聚开天斩',
    '钩选此项：施展魔法超过允许距离时，会自动跑近目标并释放魔法'
    );
  g_caTec: array[0..MAX_GC_TEC] of string = (
    '刀刀刺杀', //0
    '智能半月', //1
    '自动烈火', //2
    '逐日剑法', //3
    '自动开盾', //4
    '持续开盾(英雄)', //5
    '自动隐身', //6
    '时间间隔', //7
    '', //8
    '自动雷霆', //9
    '隔位刺杀',
    '自动断空斩',
    '英雄连击不打怪',
    '自动开天斩',
    '自动调节魔法距离');
  g_sMagics: array[0..MAX_GC_TEC] of string = (
    '火球术',
    '治愈术',
    '大火球',
    '施毒术',
    '攻杀剑术',
    '抗拒火环',
    '地狱火',
    '疾光电影',
    '雷电术',
    '雷电术',
    '雷电术',
    '雷电术',
    '雷电术',
    '开天斩',
    '开天斩');
  g_gnTecPracticeKey: Integer = 0;
  g_gcTec: array[0..MAX_GC_TEC] of Boolean = (True, True, True, True, True, True, False, False, False, False, False, False, False, True, False);
  g_gnTecTime: array[0..MAX_GC_TEC] of Integer = (0, 0, 0, 0, 0, 0, 0, 0, 4000, 0, 0, 0, 0, 0, 0);
  g_clTec: array[0..MAX_GC_TEC] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver);

  //====================================Assistant====================================
  MAX_GC_ASS = 6;
  g_ptAss: array[0..MAX_GC_ASS] of TRect = (
    (Left: 35 + 000; Top: 70 + 24 * 0; Right: 35 + 000 + 142; Bottom: 70 + 24 * 0 + 16), //0
    (Left: 35 + 000; Top: 70 + 24 * 1; Right: 35 + 000 + 72; Bottom: 70 + 24 * 1 + 16), //1
    (Left: 35 + 000; Top: 70 + 24 * 2; Right: 35 + 000 + 72; Bottom: 70 + 24 * 2 + 16), //2
    (Left: 35 + 000; Top: 70 + 24 * 3; Right: 35 + 000 + 72; Bottom: 70 + 24 * 3 + 16), //3
    (Left: 35 + 000; Top: 70 + 24 * 4; Right: 35 + 000 + 72; Bottom: 70 + 24 * 4 + 16), //4
    (Left: 35 + 000; Top: 70 + 24 * 5; Right: 35 + 000 + 120; Bottom: 70 + 24 * 5 + 16), //5
    (Left: 35 + 000; Top: 70 + 24 * 6; Right: 35 + 000 + 120; Bottom: 70 + 24 * 6 + 16)
    );
  g_HintAss: array[0..MAX_GC_ASS] of string = (
    '',
    '', //0
    '', //1
    '', //2
    '', //3
    '可以自己编辑要显示和拾取的物品，开启\此功能后，将替换掉 [物品] 选项卡的设置', //4
    '');
  g_caAss: array[0..MAX_GC_ASS] of string = (
    '开启挂机(Ctrl+Alt+X)',
    '红药用完回城', //0
    '蓝药用完回城', //1
    '符毒用完回城', //2
    '背包满时回城', //3
    '自定物品过滤(钩选编辑)', //4
    '自定打怪过滤(钩选编辑)');
  g_gcAss: array[0..MAX_GC_ASS] of Boolean = (False, False, False, False, False, False, False);
  g_clAss: array[0..MAX_GC_ASS] of TColor = (clLime, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver);

  //====================================HotKey====================================
  MAX_GC_HOTKEY = 8;
  g_ptHotkey: array[0..MAX_GC_HOTKEY] of TRect = (
    (Left: 25 + 000; Top: 43 + 20 * 0; Right: 35 + 000 + 99; Bottom: 43 + 20 * 0 + 16), //0
    (Left: 25 + 000; Top: 43 + 20 * 2; Right: 35 + 000 + 80; Bottom: 43 + 20 * 2 + 16), //2
    (Left: 25 + 000; Top: 43 + 20 * 3; Right: 35 + 000 + 80; Bottom: 43 + 20 * 3 + 16), //3
    (Left: 25 + 000; Top: 43 + 20 * 4; Right: 35 + 000 + 80; Bottom: 43 + 20 * 4 + 16), //4
    (Left: 25 + 000; Top: 43 + 20 * 5; Right: 35 + 000 + 80; Bottom: 43 + 20 * 5 + 16), //5
    (Left: 25 + 000; Top: 43 + 20 * 6; Right: 35 + 000 + 80; Bottom: 43 + 20 * 6 + 16), //6
    (Left: 25 + 000; Top: 43 + 20 * 7; Right: 35 + 000 + 80; Bottom: 43 + 20 * 7 + 16), //7
    (Left: 25 + 000; Top: 43 + 20 * 8; Right: 35 + 000 + 80; Bottom: 43 + 20 * 8 + 16),
    (Left: 25 + 000; Top: 43 + 20 * 9; Right: 35 + 000 + 80; Bottom: 43 + 20 * 9 + 16) //8
    );
  g_caHotkey: array[0..MAX_GC_HOTKEY] of string = (
    '启用自定快捷键',
    '召唤英雄', //0
    '英雄攻击目标', //1
    '使用合击技能', //2
    '英雄攻击模式', //3
    '英雄守护模式', //4
    '切换攻击模式', //5
    '切换小地图',
    '释放连击'
    );
  g_gcHotkey: array[0..MAX_GC_HOTKEY] of Boolean = (False, False, False, False, False, False, False, False, False);
  g_gnHotkey: array[0..MAX_GC_HOTKEY] of Integer = (0, 0, 0, 0, 0, 0, 0, 0, 0);
  g_clHotkey: array[0..MAX_GC_HOTKEY] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clLime);

  //====================================物品====================================
var
  g_BuildBotTex: Byte = 0;
  g_WinBottomType: Byte = 0;
//  g_Windowed: Boolean = False;
  g_pkeywords: PString = nil;

  g_boAutoPickUp: Boolean = True;
  g_boPickUpAll: Boolean = False;
  g_ptItems_Pos: Integer = -1;
  g_ptItems_Type: Integer = 0;
  g_ItemsFilter_All: TCnHashTableSmall;
  g_ItemsFilter_All_Def: TCnHashTableSmall;
  g_ItemsFilter_Dress: TStringList;
  g_ItemsFilter_Weapon: TStringList;
  g_ItemsFilter_Headgear: TStringList;
  g_ItemsFilter_Drug: TStringList;
  g_ItemsFilter_Other: TStringList;

  g_xMapDescList: TStringList;
  g_xCurMapDescList: TStringList;

const

  MAX_GC_ITEMS = 7;
  g_ptItemsA: TRect = (Left: 25 + 194; Top: 68 + 18 * 7 + 23; Right: 25 + 194 + 80; Bottom: 68 + 18 * 7 + 16 + 23);
  g_ptAutoPickUp: TRect = (Left: 25 + 267; Top: 68 + 18 * 7 + 23; Right: 25 + 267 + 80; Bottom: 68 + 18 * 7 + 16 + 23);

  g_ptItems0: array[0..MAX_GC_ITEMS] of TRect = (
    (Left: 25 + 000; Top: 68 + 18 * 0; Right: 25 + 000 + 120; Bottom: 68 + 18 * 0 + 16), //0
    (Left: 25 + 000; Top: 68 + 18 * 1; Right: 25 + 000 + 120; Bottom: 68 + 18 * 1 + 16), //1
    (Left: 25 + 000; Top: 68 + 18 * 2; Right: 25 + 000 + 120; Bottom: 68 + 18 * 2 + 16), //2
    (Left: 25 + 000; Top: 68 + 18 * 3; Right: 25 + 000 + 120; Bottom: 68 + 18 * 3 + 16), //3
    (Left: 25 + 000; Top: 68 + 18 * 4; Right: 25 + 000 + 120; Bottom: 68 + 18 * 4 + 16), //4
    (Left: 25 + 000; Top: 68 + 18 * 5; Right: 25 + 000 + 120; Bottom: 68 + 18 * 5 + 16), //5
    (Left: 25 + 000; Top: 68 + 18 * 6; Right: 25 + 000 + 120; Bottom: 68 + 18 * 6 + 16),
    (Left: 25 + 000; Top: 68 + 18 * 7; Right: 25 + 000 + 120; Bottom: 68 + 18 * 7 + 16)
    );
  g_ptItems1: array[0..MAX_GC_ITEMS] of TRect = (
    (Left: 25 + 160; Top: 68 + 18 * 0; Right: 25 + 160 + 16; Bottom: 68 + 18 * 0 + 16), //0
    (Left: 25 + 160; Top: 68 + 18 * 1; Right: 25 + 160 + 16; Bottom: 68 + 18 * 1 + 16), //1
    (Left: 25 + 160; Top: 68 + 18 * 2; Right: 25 + 160 + 16; Bottom: 68 + 18 * 2 + 16), //2
    (Left: 25 + 160; Top: 68 + 18 * 3; Right: 25 + 160 + 16; Bottom: 68 + 18 * 3 + 16), //3
    (Left: 25 + 160; Top: 68 + 18 * 4; Right: 25 + 160 + 16; Bottom: 68 + 18 * 4 + 16), //4
    (Left: 25 + 160; Top: 68 + 18 * 5; Right: 25 + 160 + 16; Bottom: 68 + 18 * 5 + 16), //5
    (Left: 25 + 160; Top: 68 + 18 * 6; Right: 25 + 160 + 16; Bottom: 68 + 18 * 6 + 16),
    (Left: 25 + 160; Top: 68 + 18 * 7; Right: 25 + 160 + 16; Bottom: 68 + 18 * 7 + 16)
    );
  g_ptItems2: array[0..MAX_GC_ITEMS] of TRect = (
    (Left: 25 + 220; Top: 68 + 18 * 0; Right: 25 + 220 + 16; Bottom: 68 + 18 * 0 + 16), //0
    (Left: 25 + 220; Top: 68 + 18 * 1; Right: 25 + 220 + 16; Bottom: 68 + 18 * 1 + 16), //1
    (Left: 25 + 220; Top: 68 + 18 * 2; Right: 25 + 220 + 16; Bottom: 68 + 18 * 2 + 16), //2
    (Left: 25 + 220; Top: 68 + 18 * 3; Right: 25 + 220 + 16; Bottom: 68 + 18 * 3 + 16), //3
    (Left: 25 + 220; Top: 68 + 18 * 4; Right: 25 + 220 + 16; Bottom: 68 + 18 * 4 + 16), //4
    (Left: 25 + 220; Top: 68 + 18 * 5; Right: 25 + 220 + 16; Bottom: 68 + 18 * 5 + 16), //5
    (Left: 25 + 220; Top: 68 + 18 * 6; Right: 25 + 220 + 16; Bottom: 68 + 18 * 6 + 16),
    (Left: 25 + 220; Top: 68 + 18 * 7; Right: 25 + 220 + 16; Bottom: 68 + 18 * 7 + 16)
    );
  g_ptItems3: array[0..MAX_GC_ITEMS] of TRect = (
    (Left: 25 + 280; Top: 68 + 18 * 0; Right: 25 + 280 + 16; Bottom: 68 + 18 * 0 + 16), //0
    (Left: 25 + 280; Top: 68 + 18 * 1; Right: 25 + 280 + 16; Bottom: 68 + 18 * 1 + 16), //1
    (Left: 25 + 280; Top: 68 + 18 * 2; Right: 25 + 280 + 16; Bottom: 68 + 18 * 2 + 16), //2
    (Left: 25 + 280; Top: 68 + 18 * 3; Right: 25 + 280 + 16; Bottom: 68 + 18 * 3 + 16), //3
    (Left: 25 + 280; Top: 68 + 18 * 4; Right: 25 + 280 + 16; Bottom: 68 + 18 * 4 + 16), //4
    (Left: 25 + 280; Top: 68 + 18 * 5; Right: 25 + 280 + 16; Bottom: 68 + 18 * 5 + 16), //5
    (Left: 25 + 280; Top: 68 + 18 * 6; Right: 25 + 280 + 16; Bottom: 68 + 18 * 6 + 16),
    (Left: 25 + 280; Top: 68 + 18 * 7; Right: 25 + 280 + 16; Bottom: 68 + 18 * 7 + 16)
    );

  g_caItems: array[0..MAX_GC_ITEMS] of pTCItemRule = (nil, nil, nil, nil, nil, nil, nil, nil);
  g_caItems2: array[0..MAX_GC_ITEMS] of pTCItemRule = (nil, nil, nil, nil, nil, nil, nil, nil);
  g_clItems: array[0..MAX_GC_ITEMS] of TColor = (clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver, clSilver);

  MAX_SERIESSKILL_POINT = 4;

  g_HitSpeedRate: Integer = 0;
  g_MagSpeedRate: Integer = 0;
  g_MoveSpeedRate: Integer = 0;

type
  TLoadMode = (lmUseWil, lmUseWzl, lmAutoWil);

  TMessageHandler = reference to procedure(AResult: Integer{TModalResult});
  TMessageDialogItem = record
    Text: String;
    Buttons: TMsgDlgButtons;
    Handler: TMessageHandler;
    Size: Integer;
  end;
  PTMessageDialogItem = ^TMessageDialogItem;

  IApplication = interface
    ['{501687F1-F936-4A5F-A235-34643269AF55}']
    procedure AddToChatBoardString(const Message: String; FColor, BColor: TColor);
    procedure LoadImage(const FileName: String; Index, Position: Integer);
//    procedure AddMessageDialog(const Text: String; Buttons: TMsgDlgButtons; Handler: TMessageHandler = nil; Size: Integer = 1);
    procedure Terminate;
    procedure DisConnect;
    function _CurPos: TPoint;
  end;

  TVaInfo = packed record
    cap: string;
    pt1: array[0..MAX_SERIESSKILL_POINT] of TRect;
    pt2: array[0..MAX_SERIESSKILL_POINT] of TRect;
    str1: array[0..MAX_SERIESSKILL_POINT] of string;
    Hint: array[0..MAX_SERIESSKILL_POINT] of string;
    //moved: array[0..MAX_SERIESSKILL_POINT] of Boolean;
    //downed: array[0..MAX_SERIESSKILL_POINT] of Boolean;
  end;

const
{$IF SERIESSKILL}
  g_VLastSender: TObject = nil;
  g_VMouseInfoTag: Integer = -1;
  g_VMouseInfo: string = '';

  g_hVLastSender: TObject = nil;
  g_hVMouseInfoTag: Integer = -1;
  g_hVMouseInfo: string = '';

  g_VLvHints: array[0..6] of string = (
    '未打通,不可修炼经络',
    '已打通,可修炼经络,一重\经络获得连击招式:%s',
    '一重,魔防下限+1,修炼得连\技%s,一级%s暴击率%d%s\暴击伤害%1.1n倍',
    '二重,防御魔防下限+1,二级\%s暴击率%d%s,\暴击伤害%1.1n倍',
    '三重,防御魔防下限+1,伤害\减免+2,三级%s暴击率%d%s\暴击伤害%1.1n倍',
    '四重,防御魔防下限+1,伤害\减免+5,四级%s暴击率%d%s\暴击伤害%1.1n倍',
    '五重,防御魔防下限+1,伤害\减免+9,五级%s暴击率%d%s\暴击伤害%1.1n倍'
    );

  g_VMouseInfo2: string = '';
  g_VLvHints2: string = '';

  g_hVMouseInfo2: string = '';
  g_hVLvHints2: string = '';

{$IFDEF UI_0508}
  g_VaInfos: array[0..3] of TVaInfo = (
    (
    pt1: (
    (Left: 35 + 038; Top: 30 + 000 + 50; Right: 35 + 038 + 23; Bottom: 30 + 000 + 50 + 23),
    (Left: 35 + 038; Top: 30 + 028 + 50; Right: 35 + 038 + 23; Bottom: 30 + 028 + 50 + 23),
    (Left: 37 + 038; Top: 30 + 053 + 50; Right: 37 + 038 + 23; Bottom: 30 + 053 + 50 + 23),
    (Left: 45 + 038; Top: 30 + 075 + 50; Right: 45 + 038 + 23; Bottom: 30 + 075 + 50 + 23),
    (Left: 50 + 038; Top: 30 + 057 + 50; Right: 50 + 038 + 23; Bottom: 30 + 057 + 50 + 23));
    pt2: (
    (Left: 35 + 047 + 038; Top: 30 + 005 + 50; Right: 35 + 047 + 28 + 038; Bottom: 30 + 005 + 12 + 50),
    (Left: 35 + 044 + 038; Top: 30 + 035 + 50; Right: 35 + 044 + 28 + 038; Bottom: 30 + 035 + 12 + 50),
    (Left: 37 - 034 + 038; Top: 30 + 058 + 50; Right: 37 - 034 + 28 + 038; Bottom: 30 + 058 + 12 + 50),
    (Left: 45 - 037 + 038; Top: 30 + 082 + 50; Right: 45 - 037 + 28 + 038; Bottom: 30 + 082 + 12 + 50),
    (Left: 50 + 038 + 038; Top: 30 + 063 + 50; Right: 50 + 038 + 28 + 038; Bottom: 30 + 063 + 12 + 50));
    str1: ('幽门', '通谷', '商曲', '四满', '横骨');
    Hint: ('获得防御上限+3', '获得魔防上限+3', '获得防御下限+3', '获得魔防下限+3', '获得防御上限+2');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 86; Top: 74; Right: 86 + 23; Bottom: 74 + 23),
    (Left: 85; Top: 113; Right: 35 + 038 + 23; Bottom: 30 + 028 + 50 + 23),
    (Left: 92; Top: 153; Right: 37 + 038 + 23; Bottom: 30 + 053 + 50 + 23),
    (Left: 99; Top: 192; Right: 45 + 038 + 23; Bottom: 30 + 075 + 50 + 23),
    (Left: 102; Top: 233; Right: 50 + 038 + 23; Bottom: 30 + 057 + 50 + 23));
    pt2: (
    (Left: 44; Top: 79; Right: 35 + 047 + 28 + 038; Bottom: 30 + 005 + 12 + 50),
    (Left: 43; Top: 118; Right: 35 + 044 + 28 + 038; Bottom: 30 + 035 + 12 + 50),
    (Left: 130; Top: 160; Right: 37 - 034 + 28 + 038; Bottom: 30 + 058 + 12 + 50),
    (Left: 62; Top: 198; Right: 45 - 037 + 28 + 038; Bottom: 30 + 082 + 12 + 50),
    (Left: 68; Top: 238; Right: 50 + 038 + 28 + 038; Bottom: 30 + 063 + 12 + 50));
    str1: ('睛明', '盘缺', '交信', '照海', '然谷');
    Hint: ('获得魔防上限+2', '获得防御下限+2', '获得魔防下限+2', '获得防御上限+1', '获得魔防上限+1');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 84; Top: 86; Right: 35 + 038 + 23; Bottom: 30 + 000 + 50 + 23),
    (Left: 78; Top: 117; Right: 35 + 038 + 23; Bottom: 30 + 028 + 50 + 23),
    (Left: 77; Top: 155; Right: 37 + 038 + 23; Bottom: 30 + 053 + 50 + 23),
    (Left: 73; Top: 188; Right: 45 + 038 + 23; Bottom: 30 + 075 + 50 + 23),
    (Left: 70; Top: 224; Right: 50 + 038 + 23; Bottom: 30 + 057 + 50 + 23));
    pt2: (
    (Left: 42; Top: 91; Right: 35 + 047 + 28 + 038; Bottom: 30 + 005 + 12 + 50),
    (Left: 124; Top: 123; Right: 35 + 044 + 28 + 038; Bottom: 30 + 035 + 12 + 50),
    (Left: 41; Top: 160; Right: 37 - 034 + 28 + 038; Bottom: 30 + 058 + 12 + 50),
    (Left: 112; Top: 194; Right: 45 - 037 + 28 + 038; Bottom: 30 + 082 + 12 + 50),
    (Left: 107; Top: 229; Right: 50 + 038 + 28 + 038; Bottom: 30 + 063 + 12 + 50));
    str1: ('廉泉', '期门', '府舍', '冲门', '筑宾');
    Hint: ('获得防御下限+1', '获得魔防下限+1', '', '', '');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 86; Top: 70; Right: 35 + 038 + 23; Bottom: 30 + 000 + 50 + 23),
    (Left: 86; Top: 89; Right: 35 + 038 + 23; Bottom: 30 + 028 + 50 + 23),
    (Left: 86; Top: 110; Right: 37 + 038 + 23; Bottom: 30 + 053 + 50 + 23),
    (Left: 86; Top: 134; Right: 45 + 038 + 23; Bottom: 30 + 075 + 50 + 23),
    (Left: 86; Top: 158; Right: 50 + 038 + 23; Bottom: 30 + 057 + 50 + 23));
    pt2: (
    (Left: 42; Top: 74; Right: 35 + 047 + 28 + 038; Bottom: 30 + 005 + 12 + 50),
    (Left: 132; Top: 95; Right: 35 + 044 + 28 + 038; Bottom: 30 + 035 + 12 + 50),
    (Left: 42; Top: 116; Right: 37 - 034 + 28 + 038; Bottom: 30 + 058 + 12 + 50),
    (Left: 132; Top: 141; Right: 45 - 037 + 28 + 038; Bottom: 30 + 082 + 12 + 50),
    (Left: 42; Top: 164; Right: 50 + 038 + 28 + 038; Bottom: 30 + 063 + 12 + 50));
    str1: ('承浆', '天突', '鸠尾', '气海', '曲骨');
    Hint: ('', '', '', '', '');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    )
    );
{$ELSE}
  g_VaInfos: array[0..3] of TVaInfo = (
    (
    pt1: (
    (Left: 35 + 038 + 52; Top: 30 + 000 + 50 + 113; Right: 35 + 038 + 52 + 23; Bottom: 30 + 000 + 50 + 113 + 23),
    (Left: 35 + 038 + 52; Top: 30 + 028 + 50 + 113; Right: 35 + 038 + 52 + 23; Bottom: 30 + 028 + 50 + 113 + 23),
    (Left: 37 + 038 + 52; Top: 30 + 053 + 50 + 113; Right: 37 + 038 + 52 + 23; Bottom: 30 + 053 + 50 + 113 + 23),
    (Left: 45 + 038 + 52; Top: 30 + 075 + 50 + 113; Right: 45 + 038 + 52 + 23; Bottom: 30 + 075 + 50 + 113 + 23),
    (Left: 50 + 038 + 52; Top: 30 + 057 + 50 + 113; Right: 50 + 038 + 52 + 23; Bottom: 30 + 057 + 50 + 113 + 23));
    pt2: (
    (Left: 35 + 047 + 52 + 038; Top: 30 + 005 + 113 + 50; Right: 35 + 047 + 28 + 52 + 038; Bottom: 30 + 005 + 12 + 113 + 50),
    (Left: 35 + 044 + 52 + 038; Top: 30 + 035 + 113 + 50; Right: 35 + 044 + 28 + 52 + 038; Bottom: 30 + 035 + 12 + 113 + 50),
    (Left: 37 - 034 + 52 + 038; Top: 30 + 058 + 113 + 50; Right: 37 - 034 + 28 + 52 + 038; Bottom: 30 + 058 + 12 + 113 + 50),
    (Left: 45 - 037 + 52 + 038; Top: 30 + 082 + 113 + 50; Right: 45 - 037 + 28 + 52 + 038; Bottom: 30 + 082 + 12 + 113 + 50),
    (Left: 50 + 038 + 52 + 038; Top: 30 + 063 + 113 + 50; Right: 50 + 038 + 28 + 52 + 038; Bottom: 30 + 063 + 12 + 113 + 50));
    str1: ('幽门', '通谷', '商曲', '四满', '横骨');
    Hint: ('获得防御上限+3', '获得魔防上限+3', '获得防御下限+3', '获得魔防下限+3', '获得防御上限+2');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 86 + 52; Top: 74 + 113; Right: 86 + 23 + 52; Bottom: 74 + 23 + 113),
    (Left: 85 + 52; Top: 113 + 113; Right: 35 + 038 + 23 + 52; Bottom: 30 + 028 + 50 + 23 + 113),
    (Left: 92 + 52; Top: 153 + 113; Right: 37 + 038 + 23 + 52; Bottom: 30 + 053 + 50 + 23 + 113),
    (Left: 99 + 52; Top: 192 + 113; Right: 45 + 038 + 23 + 52; Bottom: 30 + 075 + 50 + 23 + 113),
    (Left: 102 + 52; Top: 233 + 113; Right: 50 + 038 + 23 + 52; Bottom: 30 + 057 + 50 + 23 + 113));
    pt2: (
    (Left: 44 + 52; Top: 79 + 113; Right: 35 + 047 + 28 + 038 + 52; Bottom: 30 + 005 + 12 + 50 + 113),
    (Left: 43 + 52; Top: 118 + 113; Right: 35 + 044 + 28 + 038 + 52; Bottom: 30 + 035 + 12 + 50 + 113),
    (Left: 130 + 52; Top: 160 + 113; Right: 37 - 034 + 28 + 038 + 52; Bottom: 30 + 058 + 12 + 50 + 113),
    (Left: 62 + 52; Top: 198 + 113; Right: 45 - 037 + 28 + 038 + 52; Bottom: 30 + 082 + 12 + 50 + 113),
    (Left: 68 + 52; Top: 238 + 113; Right: 50 + 038 + 28 + 038 + 52; Bottom: 30 + 063 + 12 + 50 + 113));
    str1: ('睛明', '盘缺', '交信', '照海', '然谷');
    Hint: ('获得魔防上限+2', '获得防御下限+2', '获得魔防下限+2', '获得防御上限+1', '获得魔防上限+1');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 84 + 52; Top: 86 + 113; Right: 35 + 038 + 23 + 52; Bottom: 30 + 000 + 50 + 23 + 113),
    (Left: 78 + 52; Top: 117 + 113; Right: 35 + 038 + 23 + 52; Bottom: 30 + 028 + 50 + 23 + 113),
    (Left: 77 + 52; Top: 155 + 113; Right: 37 + 038 + 23 + 52; Bottom: 30 + 053 + 50 + 23 + 113),
    (Left: 73 + 52; Top: 188 + 113; Right: 45 + 038 + 23 + 52; Bottom: 30 + 075 + 50 + 23 + 113),
    (Left: 70 + 52; Top: 224 + 113; Right: 50 + 038 + 23 + 52; Bottom: 30 + 057 + 50 + 23 + 113));
    pt2: (
    (Left: 42 + 52; Top: 91 + 113; Right: 35 + 047 + 28 + 038 + 52; Bottom: 30 + 005 + 12 + 50 + 113),
    (Left: 124 + 52; Top: 123 + 113; Right: 35 + 044 + 28 + 038 + 52; Bottom: 30 + 035 + 12 + 50 + 113),
    (Left: 41 + 52; Top: 160 + 113; Right: 37 - 034 + 28 + 038 + 52; Bottom: 30 + 058 + 12 + 50 + 113),
    (Left: 112 + 52; Top: 194 + 113; Right: 45 - 037 + 28 + 038 + 52; Bottom: 30 + 082 + 12 + 50 + 113),
    (Left: 107 + 52; Top: 229 + 113; Right: 50 + 038 + 28 + 038 + 52; Bottom: 30 + 063 + 12 + 50 + 113));
    str1: ('廉泉', '期门', '府舍', '冲门', '筑宾');
    Hint: ('获得防御下限+1', '获得魔防下限+1', '', '', '');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    ),
    (
    pt1: (
    (Left: 86 + 52; Top: 70 + 113; Right: 35 + 038 + 23 + 52; Bottom: 30 + 000 + 50 + 23 + 113),
    (Left: 86 + 52; Top: 89 + 113; Right: 35 + 038 + 23 + 52; Bottom: 30 + 028 + 50 + 23 + 113),
    (Left: 86 + 52; Top: 110 + 113; Right: 37 + 038 + 23 + 52; Bottom: 30 + 053 + 50 + 23 + 113),
    (Left: 86 + 52; Top: 134 + 113; Right: 45 + 038 + 23 + 52; Bottom: 30 + 075 + 50 + 23 + 113),
    (Left: 86 + 52; Top: 158 + 113; Right: 50 + 038 + 23 + 52; Bottom: 30 + 057 + 50 + 23 + 113));
    pt2: (
    (Left: 42 + 52; Top: 74 + 113; Right: 35 + 047 + 28 + 038 + 52; Bottom: 30 + 005 + 12 + 50 + 113),
    (Left: 132 + 52; Top: 95 + 113; Right: 35 + 044 + 28 + 038 + 52; Bottom: 30 + 035 + 12 + 50 + 113),
    (Left: 42 + 52; Top: 116 + 113; Right: 37 - 034 + 28 + 038 + 52; Bottom: 30 + 058 + 12 + 50 + 113),
    (Left: 132 + 52; Top: 141 + 113; Right: 45 - 037 + 28 + 038 + 52; Bottom: 30 + 082 + 12 + 50 + 113),
    (Left: 42 + 52; Top: 164 + 113; Right: 50 + 038 + 28 + 038 + 52; Bottom: 30 + 063 + 12 + 50 + 113));
    str1: ('承浆', '天突', '鸠尾', '气海', '曲骨');
    Hint: ('', '', '', '', '');
    //moved: (False, False, False, False, False);
    //downed: (False, False, False, False, False);
    )
    );
{$ENDIF}

{$IFEND SERIESSKILL}

  g_boFlashMission: Boolean = False;
  g_boNewMission: Boolean = False;
  g_dwNewMission: Longword = 0;

  g_asSkillDesc: array[1..255] of string = (
    '凝聚自身魔力发射一枚火球\攻击目标',
    '释放精神之力恢复自己或者\他人的体力',
    '提高自身的攻击命中率',
    '通过与精神之力沟通，可以\提高战斗时的命中率',
    '凝聚自身魔力发射一枚大火\球攻击目标',
    '配合特殊药粉可以指定某个\目标中毒',
    '攻击时有机率造成大幅伤害',
    '将身边的人或者怪兽推开',
    '向前挥出一堵火焰墙，使法\术区域内的敌人受到伤害',
    '积蓄一道光电，使直线上所\有敌人受到伤害',
    '从空中召唤一道雷电攻击敌人', {11}
    '隔位施展剑气，使敌人受到\大幅伤害',
    '将精神之力附着在护身符上，\远程攻击目标',
    '使用护身符提高范围内友方\的魔法防御力',
    '使用护身符提高范围内友方\的防御力',
    '被限制在咒语中的怪兽不能\移动或攻击圈外敌人',
    '使用护身符从地狱深处召唤\骷髅，分担召唤者受到的伤害',
    '在自身周围释放精神之力使\怪物无法察觉你的存在',
    '通达大量释放精神之力，能\够隐藏范围内的人',
    '通过闪光电击使敌人瘫痪，\甚至可以使怪物成为忠实的仆人',
    '利用强大魔力打乱空间，从\而达到随机传送目的的法术', {21}
    '在地面上产生火焰，使踏入\的敌人受到伤害',
    '产生高热的火焰，使法术区\域内的敌人受到伤害',
    '能够呼唤出一股强力的雷光\风暴，伤害所有围在身边的敌人',
    '使用劲气可同时攻击环绕自\身周围的敌人',
    '召唤火精灵附在武器上，从\而造成强力的额外伤害',
    '用肩膀把敌人撞开，如果撞\到障碍物将会对自己造成伤害',
    '使用精神力查看目标体力',
    '恢复自己和周围所有玩家的\体力',
    '使用护身符召唤一只强大神\兽作为自己的随从',
    '使用自身魔力制造一个魔法\盾减少施法者受到的伤害', {31}
    '有机率一击杀死不死生物',
    '召唤强力的暴风雪，使法术\区域内的敌人受到伤害',
    '解除友方身上中的各种毒',
    '', {老狮子吼}
    '将法力凝结成冰攻击目标，\有一定几率使对方暂时石化', {火焰冰}
    '召唤强力的雷电，使法术\区域内的敌人受到伤害',
    '配合特殊药粉可以指定某个\区域内的目标中毒',
    '彻地钉，战士远程攻击技能',
    '使用劲气可同时攻击环绕\自身周围的敌人',
    '使用劲气如雷震般将自身\周围怪物暂时石化', {41}
    '使用劲气造成处于自身前\方大面积的敌人受伤',
    '召唤雷电灵附在武器上，\从而造成强力的额外伤害，\有一定几率使敌人麻痹',
    '产生巨大的魔力推力同时\给敌人造成一定的伤害',
    '吸取对方一定的MP，同时\产生巨大的魔力伤害',
    '',
    '召唤出可以熔化天日的火龙气焰',
    '一种内功的修炼，可以推\开周围的怪物而得以防身的作用',
    '解除友方身上中的各种中毒状态',
    '瞬间提升自己的精神力',
    '飓风破', {51}
    '诅咒术',
    '血咒',
    '骷髅咒',
    '',
    '剑气凝聚成形，瞬间化作\一道光影，突袭身前的敌人',
    '驱使护身符，伤害同时吸取\对方生命值',
    '召唤一阵猛烈的火雨，使\法术区域内的敌人受到伤害',
    '', {59}
    '将雷霆万钧之力凝于双刀之\尖，对敌人造成致命伤害',
    '将刀光幻化成缕缕虹光环绕\四周，对敌人造成致命伤害',
    '召唤天裂之力于利刃之中，\对敌人造成致命伤害',
    '召唤噬魂沼泽，对敌人造成\致命伤害',
    '炙热的龙焰与冻绝的冰息，\汇合成内敛之气，爆发于瞬间',
    '召唤出可以熔化天日的火龙气焰',
    '凝聚力量的顶点，幻化出一柄\巨剑，爆发出毁天灭地的威力',
    '', //67
    '', //68
    '', //69
    '将自己的属性召唤到身边', //70
    '远距离擒获怪物到自身前方', //71
    '使用法力自由移动到指定位置', //72
    '', //73
    '', //74
    '可以用来减低对方给予\你的伤害', //75
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '近身攻击，冲撞单体目标，\在迫使其后退的同时，造成伤害', //100
    '左右开弓，挥剑重砍。\近身攻击，对单体目标造成伤害', //101
    '跳起重击地面造成巨大伤害。\远程攻击，对三步内的\单体目标造成伤害', //102
    '横扫千军犹入无人之境。\范围攻击，以自身为中心，\对5*5范围内造成伤害', //103

    '犹如凤凰涅磐般的致命一击。\远程攻击，对单体目标造成伤害', //104
    '跃起后发出强烈的魔法气场。\远程攻击，对单体目标造成伤害', //105
    '蓄力重击，冻裂地面形成冰刺。\范围攻击，以目标为中心，\对5*5范围内造成持续伤害', //106
    '伤害非常恐怖的双龙出击。\远程攻击，对单体目标造成伤害', //107

    '放出圣兽对目标发起攻击。\远程攻击，对单体目标造成伤害', //108
    '双手运气，推出八卦掌攻击敌人。\远程攻击，对单体目标造成伤害', //109
    '让人难以招架的连环符。\远程攻击，对单体目标造成伤害', //110
    '万剑齐发，天地同归。\范围攻击，以目标为中心，\对5*5范围内造成伤害', //111
    '范围攻击，以目标为中心，\对3*3范围内造成持续伤害',
    '跳起重击地面造成巨大伤害\远程攻击，对四步内的\单体目标造成伤害',
    '跳起重击地面造成巨大伤害\远程攻击，对屏幕内的\目标造成巨大伤害',
    '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
    );

const
//  g_sGameConfigBackGround = '.\Data\ui\gcbkd.uib';
//  g_sGameConfigOk1 = '.\Data\ui\gcok1.uib';
//  g_sGameConfigOk2 = '.\Data\ui\gcok2.uib';
//  g_sGameConfigPage1 = '.\Data\ui\gcpage1.uib';
//  g_sGameConfigPage2 = '.\Data\ui\gcpage2.uib';
//  g_sGameConfigClose1 = '.\Data\ui\gcclose1.uib';
//  g_sGameConfigClose2 = '.\Data\ui\gcclose2.uib';
//  g_sGameConfigCheckBox1 = '.\Data\ui\gccheckbox1.uib';
//  g_sGameConfigCheckBox2 = '.\Data\ui\gccheckbox2.uib';

  g_sGameConfigBackGround = 40; //已改
//  g_sGameConfigOk1 = 2;
//  g_sGameConfigOk2 = 3;
  g_sGameConfigPage1 = 44; //已改
  g_sGameConfigPage2 = 45;
  g_sGameConfigClose1 = 46;//已改
  g_sGameConfigClose2 = 47;
  g_sGameConfigCheckBox1 = 48; //已改
  g_sGameConfigCheckBox2 = 49; //已改

  g_sBookPage = '.\Data\books\%d\%d.uib';
  g_sBookBtn = '.\Data\books\%d\%s.uib';
//  g_sMiniMap = '.\Data\minimap\%d.mmap';


  g_sStateWindowHero = 50; //已改
  g_sHeroStatusWindow = 51; //已改

  g_sDscStart0 = 60; //已改
  g_sDscStart1 = 61; //已改
  g_sGloryButton = 62;//已改

  g_sVigourbar1 = 64;//已改
  g_sVigourbar2 = 65;

  g_sBookBkgnd = 70;  //已改
  g_sBookCloseNormal = 71;//已改
  g_sBookCloseDown = 72; //已改
  g_sBookNextPageNormal = 73;
  g_sBookNextPageDown = 74;
  g_sBookPrevPageNormal = 75;
  g_sBookPrevPageDown = 76;

  g_WStall = 80; //已改
  g_WStallPrice = 81; //已改
  g_PStallPrice0 = 82; //已改
  g_PStallPrice1 = 83; //已改
  g_StallBot0 = 84;  //已改
  g_StallBot1 = 85;//已改
  g_StallLooks = 80; //已改

  g_affiche0 = '游戏音效已关闭！';
  g_affiche1 = '健康游戏公告';
  g_affiche2 = '抵制不良游戏 拒绝盗版游戏 注意自我保护 谨防受骗上当 适度游戏益脑';
  g_affiche3 = '沉迷游戏伤身 合理安排时间 享受健康生活 严厉打击赌博 营造和谐环境';

const
  WH_KEYBOARD_LL = 13;
  LLKHF_ALTDOWN = $20;

type
  TMirStartupInfo = record
    sServerName: String[30];
    sServeraddr: String[30];
    sServerKey: String[100];
    sUIPakKey: String[32];
    sResourceDir: String[50];
    nServerPort: Integer;
    boFullScreen: Boolean;
    boWaitVBlank: Boolean;
    bo3D: Boolean;
    boMini: Boolean;
    nScreenWidth: Integer;
    nScreenHegiht: Integer;
    nLocalMiniPort: Integer;
    btClientVer: Byte;
    sLogo: String[255];
    PassWordFileName:string[127];
  end;

  TMiniResRequest = packed record
    _Type: Byte; //0:图片 1:单文件
    Important: Boolean; //是否重要，无需队列等待，立即下载
    FileName: String[200];
    Index: Integer;
    FailCount:Word; //失败次数
    Data:Pointer; //用以挂接其他数据
  end;
  PTMiniResRequest = ^TMiniResRequest;

  TMiniResResponse = packed record
    Ident: Integer;
    FileName: String[200];
    Index: Integer;
    Position: Integer;
  end;
  PTMiniResResponse = ^TMiniResResponse;

  PFindNOde = ^TFindNode;
  TFindNode = record
    X, Y: Integer; //坐标
  end;

  PTree = ^Tree;
  Tree = record
    H: Integer;
    X, Y: Integer;
    Dir: Byte;
    Father: PTree;
  end;

  PLink = ^Link;
  Link = record
    Node: PTree;
    F: Integer;
    Next: PLink;
  end;

  TVirusSign = packed record
    Offset: Integer;
    //Length: LongWord;
    CodeSign: string;
  end;
  pTVirusSign = ^TVirusSign;

  tagKBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    Time: DWORD;
    dwExtraInfo: DWORD;
  end;
  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  TTimerCommand = (tcSoftClose, tcReSelConnect, tcFastQueryChr, tcQueryItemPrice {, tcGetToSelChar});
  TChrAction = (caWalk, caRun, caHorseRun, caHit, caSpell, caSitdown);
  TConnectionStep = (cnsIntro, cnsLogin, cnsSelChr, cnsReSelChr, cnsPlay);
  TAPPass = array[0..MAXX * 3, 0..MAXY * 3] of DWORD;
  PTAPPass = ^TAPPass;

//  TDXFont = packed record       //ASP修改
//    //boBold: Boolean;
//    clFront: Integer;
//    pszText: string;
//    dwLaststTick: Longword;
//    pSurface: TDirectDrawSurface;
//  end;
//  PTDXFont = ^TDXFont;

  TMovingItem = record
    Index: Integer;
    Item: TClientItem;
  end;
  pTMovingItem = ^TMovingItem;

  TCleintBox = packed record
    Index: Integer;
    Item: TClientItem;
  end;

  TItemType = (i_HPDurg, i_MPDurg, i_HPMPDurg, i_OtherDurg, i_Weapon, i_Dress, i_Helmet, i_Necklace, i_Armring, i_Ring, i_Belt, i_Boots, i_Charm, i_Book, i_PosionDurg, i_UseItem, i_Scroll, i_Stone, i_Gold, i_Other);
  TShowItem = record
    sItemName: string;
    ItemType: TItemType;
    boAutoPickup: Boolean;
    boShowName: Boolean;
    nFColor: Integer;
    nBColor: Integer;
  end;
  pTShowItem = ^TShowItem;
  TControlInfo = record
    Image: Integer;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Obj: TDControl;
  end;
  pTControlInfo = ^TControlInfo;
  TConfig = record
    DMsgDlg: TControlInfo;
    DMsgDlgOk: TControlInfo;
    DMsgDlgYes: TControlInfo;
    DMsgDlgCancel: TControlInfo;
    DMsgDlgNo: TControlInfo;
    DLogin: TControlInfo;
    DLoginNew: TControlInfo;
    DLoginOk: TControlInfo;
    DLoginChgPw: TControlInfo;
    DLoginClose: TControlInfo;
    DSelServerDlg: TControlInfo;
    DSSrvClose: TControlInfo;
    DSServer1: TControlInfo;
    DSServer2: TControlInfo;
    DSServer3: TControlInfo;
    DSServer4: TControlInfo;
    DSServer5: TControlInfo;
    DSServer6: TControlInfo;
    DNewAccount: TControlInfo;
    DNewAccountOk: TControlInfo;
    DNewAccountCancel: TControlInfo;
    DNewAccountClose: TControlInfo;
    DChgPw: TControlInfo;
    DChgpwOk: TControlInfo;
    DChgpwCancel: TControlInfo;
    DSelectChr: TControlInfo;
    DBottom: TControlInfo;
    DMyState: TControlInfo;
    DMyBag: TControlInfo;
    DMyMagic: TControlInfo;
    DOption: TControlInfo;
    DBotMiniMap: TControlInfo;
    DBotTrade: TControlInfo;
    DBotGuild: TControlInfo;
    DBotGroup: TControlInfo;
    DBotFriend: TControlInfo;
    DBotDare: TControlInfo;
    DBotLevelRank: TControlInfo;
    DBotPlusAbil: TControlInfo;
    //DBotMemo: TControlInfo;
    //DBotExit: TControlInfo;
    //DBotLogout: TControlInfo;
    DBelt1: TControlInfo;
    DBelt2: TControlInfo;
    DBelt3: TControlInfo;
    DBelt4: TControlInfo;
    DBelt5: TControlInfo;
    DBelt6: TControlInfo;
    DGold: TControlInfo;
    DRfineItem: TControlInfo;
    DCloseBag: TControlInfo;
    DMerchantDlg: TControlInfo;
    DMerchantDlgClose: TControlInfo;
    DConfigDlg: TControlInfo;
    DConfigDlgOK: TControlInfo;
    DConfigDlgClose: TControlInfo;
    DMenuDlg: TControlInfo;
    DMenuPrev: TControlInfo;
    DMenuNext: TControlInfo;
    DMenuBuy: TControlInfo;
    DMenuClose: TControlInfo;
    DSellDlg: TControlInfo;
    DSellDlgOk: TControlInfo;
    DSellDlgClose: TControlInfo;
    DSellDlgSpot: TControlInfo;
    DKeySelDlg: TControlInfo;
    DKsIcon: TControlInfo;
    DKsF1: TControlInfo;
    DKsF2: TControlInfo;
    DKsF3: TControlInfo;
    DKsF4: TControlInfo;
    DKsF5: TControlInfo;
    DKsF6: TControlInfo;
    DKsF7: TControlInfo;
    DKsF8: TControlInfo;
    DKsConF1: TControlInfo;
    DKsConF2: TControlInfo;
    DKsConF3: TControlInfo;
    DKsConF4: TControlInfo;
    DKsConF5: TControlInfo;
    DKsConF6: TControlInfo;
    DKsConF7: TControlInfo;
    DKsConF8: TControlInfo;
    DKsNone: TControlInfo;
    DKsOk: TControlInfo;
    DChgGamePwd: TControlInfo;
    DChgGamePwdClose: TControlInfo;
    DItemGrid: TControlInfo;
  end;

  TMapDescInfo = record
    szMapTitle: string;
    szPlaceName: string;
    nPointX: Integer;
    nPointY: Integer;
    nColor: TColor;
    nFullMap: Integer;
  end;
  pTMapDescInfo = ^TMapDescInfo;

//  TDXImgInf = record         //ASP修改
//    DImageObj: TDXImages;
//    dwImgTick: Longword;
//    PDImgList: TGList;
//  end;
//  PTDXImgInf = ^TDXImgInf;

  TItemShine = record
    idx: Integer;
    tick: Longword;
  end;

  TTempSeriesSkill = array[0..3] of Byte;

  TSeriesSkill = record
    wMagid: Byte;
    nStep: Byte;
    bSpell: Boolean;
  end;

  TTempSeriesSkillA = record
    pm: PTClientMagic;
    bo: Boolean;
  end;
  PTTempSeriesSkillA = ^TTempSeriesSkillA;

var
  ClientParamStr : string;
  g_Application: IApplication = nil;
  g_pWsockAddr: array[0..4] of Byte;
  g_dwImgThreadId: Longword = 0;
  g_hImagesThread: THandle = INVALID_HANDLE_VALUE;

  g_LogoHide: Boolean = False;
  g_nMagicRange: Integer = 8;
  g_TileMapOffSetX: Integer = 9;
  g_TileMapOffSetY: Integer = 9;

  g_btMyEnergy: Byte = 0;
  g_btMyLuck: Byte = 0;

  g_tiOKShow: TItemShine = (idx: 0; tick: 0);
  g_tiFailShow: TItemShine = (idx: 0; tick: 0);
  g_tiOKShow2: TItemShine = (idx: 0; tick: 0);
  g_tiFailShow2: TItemShine = (idx: 0; tick: 0);

  g_spOKShow2: TItemShine = (idx: 0; tick: 0);
  g_spFailShow2: TItemShine = (idx: 0; tick: 0);

  g_tiHintStr1: string = '';
  g_tiHintStr2: string = '';
  g_TIItems: array[0..1] of TMovingItem;

  g_spHintStr1: string = '';
  g_spHintStr2: string = '';
  g_spItems: array[0..1] of TMovingItem;

  g_SkidAD_Count: Integer = 0;
  g_SkidAD_Count2: Integer = 0;
  g_lastHeroSel: string;
  g_heros: THerosInfo;

  g_ReSelChr: Boolean = False;
  g_Logined: Boolean = False;
  g_ItemWear: Byte = 0;
  g_ShowSuite: Byte = 0;
  g_ShowSuite2: Byte = 0;
  g_ShowSuite3: Byte = 0;

  g_SuiteSpSkill: Byte = 0;

  g_SuiteIdx: Integer = -1;
  g_SuiteItemsList: TList;
  g_pshine: TSimpleMsgPack;
  g_TitlesList: TList;
  g_btSellType: Byte = 0;
  g_showgamegoldinfo: Boolean = False;
  SSE_AVAILABLE: Boolean = False;
  g_lWavMaxVol: Integer = 68; //-100;

  g_uDressEffectTick: Longword;
  g_sDressEffectTick: Longword;
  g_hDressEffectTick: Longword;

  g_uDressEffectIdx: Integer;
  g_sDressEffectIdx: Integer;
  g_hDressEffectIdx: Integer;

  g_uWeaponEffectTick: Longword;
  g_sWeaponEffectTick: Longword;
  g_hWeaponEffectTick: Longword;

  g_uWeaponEffectIdx: Integer;
  g_sWeaponEffectIdx: Integer;
  g_hWeaponEffectIdx: Integer;

  g_ChatStatusLarge: BOOL = False;
  g_ChatWindowLines: Integer = 12;

  g_LoadBeltConfig: BOOL = False;
  g_BeltMode: BOOL = True;
  g_BeltPositionX: Integer = 408;
  g_BeltPositionY: Integer = 487;

  g_dwActorLimit: Integer = 5;
  g_nProcActorIDx: Integer = 0;

  g_boPointFlash: Boolean = False;
  g_PointFlashTick: Longword;
  g_boHPointFlash: Boolean = False;
  g_HPointFlashTick: Longword;

  g_VenationInfos: TVenationInfos = (
    (Level: 0; Point: 0),
    (Level: 0; Point: 0),
    (Level: 0; Point: 0),
    (Level: 0; Point: 0)
    ); //经络信息
  g_hVenationInfos: TVenationInfos = (
    (Level: 0; Point: 0),
    (Level: 0; Point: 0),
    (Level: 0; Point: 0),
    (Level: 0; Point: 0)
    ); //经络信息

  g_NextSeriesSkill: Boolean = False;
  g_dwSeriesSkillReadyTick: Longword;
  g_nCurrentMagic: Integer = 888;
  g_nCurrentMagic2: Integer = 888;
  g_SendFireSerieSkillTick: Longword;
  g_IPointLessHintTick: Longword;
  g_MPLessHintTick: Longword;
  g_SeriesSkillStep: Integer = 0;
  g_SeriesSkillFire_100: Boolean = False;
  g_SeriesSkillFire: Boolean = False;
  g_SeriesSkillReady: Boolean = False;
  g_SeriesSkillReadyFlash: Boolean = False;
  //g_TempMagicArr            : array[0..3] of TTempSeriesSkillA;
  g_TempSeriesSkillArr: TTempSeriesSkill;
  g_HTempSeriesSkillArr: TTempSeriesSkill;
  g_SeriesSkillArr: array[0..3] of Byte; //TSeriesSkill;
  g_SeriesSkillSelList: TStringList;
  g_hSeriesSkillSelList: TStringList;

  g_dwAutoTecTick: Longword;
  g_dwAutoTecHeroTick: Longword;
  g_ProcOnIdleTick: Longword;

  g_dwProcMessagePacket: Longword;
  g_ProcNowTick: Longword;
  g_ProcCanFill: Boolean = True;

  g_ProcOnDrawTick: Longword;
  g_ProcOnDrawTick_Effect: Longword;
  g_ProcOnDrawTick_Effect2: Longword;

//  g_ProcCanDraw: Boolean;
//  FFrameRate: Integer;
//  FInterval: Cardinal;
//  FInterval2: Cardinal;
//  FNowFrameRate: Integer;
//  FOldTime: DWORD;
//  FOldTime2: DWORD;
  //g_ProcCanDraw_Effect      : Boolean;
  //g_ProcCanDraw_Effect2     : Boolean;

  g_dwImgMgrTick: Longword;
  g_nImgMgrIdx: Integer = 0;
  //ProcImagesCS              : TRTLCriticalSection;
  ProcMsgCS: TRTLCriticalSection;
  ThreadCS: TRTLCriticalSection;
  g_bIMGBusy: Boolean = False;
  g_DsMiniMapPixel: TCustomLockableTexture = nil;
  g_MainFrom: TForm;
  //g_dwCurrentTick           : PLongWord;
  g_dwThreadTick: PLongWord;
  g_rtime: Longword = 0;
  g_dwLastThreadTick: Longword = 0;

  g_boExchgPoison: Boolean = False;
  g_boCheckTakeOffPoison: Boolean;
  g_Angle: Integer = 0;
  g_ShowMiniMapXY: Boolean = False;
  g_DrawingMiniMap: Boolean = False;
  g_DrawMiniBlend: Boolean = False;
  g_MiniMapRC: TIntRect;
  g_boTakeOnPos: Boolean = True;
  g_boHeroTakeOnPos: Boolean = True;

  g_pRcHeader: pTRcHeader;
//  g_bLoginKey: PBoolean;
//  g_pbInitSock: PBoolean;
  g_pbRecallHero: PBoolean;
  g_RareBoxWindow: TRareBoxWindow;
  //g_dwCheckTick             : LongWord = 0;
  g_dwCheckCount: Integer = 0;
  g_nBookPath: Integer = 0;
  g_nBookPage: Integer = 0;
  g_HillMerchant: Integer = 0;
  g_sBookLabel: string = '';

  g_MaxExpFilter: Integer = 2000;
  g_boDrawLevelRank: Boolean = False;
  g_HeroLevelRanks: THeroLevelRanks;
  g_HumanLevelRanks: THumanLevelRanks;

  g_UnBindItems: array[0..12] of string = (
    '万年雪霜',
    '疗伤药',
    '强效太阳水',
    '强效金创药',
    '强效魔法药',
    '金创药(小量)',
    '魔法药(小量)',
    '金创药(中量)',
    '魔法药(中量)',
    '地牢逃脱卷',
    '随机传送卷',
    '回城卷',
    '行会回城卷');
  g_sLogoText: string = 'LegendSoft';
  g_sGoldName: string = '金币';
  g_sGameGoldName: string = '元宝';
  g_sGamePointName: string = '泡点';
  g_sWarriorName: string = '武士'; //职业名称
  g_sWizardName: string = '魔法师'; //职业名称
  g_sTaoistName: string = '道士'; //职业名称
  g_sUnKnowName: string = '未知';

  g_sMainParam1: string; //读取设置参数
  g_sMainParam2: string; //读取设置参数
  g_sMainParam3: string; //读取设置参数
  g_sMainParam4: string; //读取设置参数
  g_sMainParam5: string; //读取设置参数
  g_sMainParam6: string; //读取设置参数
  g_LoginKey: string = 'password';
  g_PakPassword: string;
  g_boQueryDynCode :Boolean = False;

//  g_DXDraw: TDXDraw;
//  g_DWinMan: TDWinManager;
  //g_DXSound                 : TDXSound;
  //g_Sound                   : TSoundEngine;

  g_wui: TWMImages;
  g_opui: TWMImages;
  g_WMainImages: TWMImages;
  g_WMain2Images: TWMImages;
  g_WMain3Images: TWMImages;
  g_WChrSelImages: TWMImages;

  g_WMonImg: TWMImages;
  g_WMon2Img: TWMImages;
  g_WMon3Img: TWMImages;
  g_WMon4Img: TWMImages;
  g_WMon5Img: TWMImages;
  g_WMon6Img: TWMImages;
  g_WMon7Img: TWMImages;
  g_WMon8Img: TWMImages;
  g_WMon9Img: TWMImages;
  g_WMon10Img: TWMImages;
  g_WMon11Img: TWMImages;
  g_WMon12Img: TWMImages;
  g_WMon13Img: TWMImages;
  g_WMon14Img: TWMImages;
  g_WMon15Img: TWMImages;
  g_WMon16Img: TWMImages;
  g_WMon17Img: TWMImages;
  g_WMon18Img: TWMImages;
  g_WMon19Img: TWMImages;
  g_WMon20Img: TWMImages;
  g_WMon21Img: TWMImages;
  g_WMon22Img: TWMImages;
  g_WMon23Img: TWMImages;
  g_WMon24Img: TWMImages;
  g_WMon25Img: TWMImages;
  g_WMon26Img: TWMImages;
  g_WMon27Img: TWMImages;
  g_WMon28Img: TWMImages;
  g_WMon29Img: TWMImages;
  g_WMon30Img: TWMImages;
  g_WMon31Img: TWMImages;
  g_WMon32Img: TWMImages;
  g_WMon33Img: TWMImages;
  g_WMon34Img: TWMImages;
  g_WMon35Img: TWMImages;
  g_WMon36Img: TWMImages;
  g_WMon37Img: TWMImages;
  g_WMon38Img: TWMImages;
  g_WMon39Img: TWMImages;
  g_WMon40Img: TWMImages;

  g_WEffectImg: TWMImages;
  g_WDragonImg: TWMImages;
  g_WSkeletonImg: TWMImages;

 // g_WBooksImages: TWMImages;
 // g_WMainUibImages: TWMImages;
 // g_WMiniMapImages: TWMImages;

  g_WMMapImages: TWMImages;
  //g_WTilesImages            : TWMImages;
  //g_WSmTilesImages          : TWMImages;
  //g_WTiles2Images           : TWMImages;
  //g_WSmTiles2Images         : TWMImages;
  g_WHumWingImages: TWMImages;
  g_WHumEffect2: TWMImages;
  g_WHumEffect3: TWMImages;
  g_WBagItemImages: TWMImages;
  g_WBagItemImages2: TWMImages;
  g_WStateItemImages: TWMImages;
  g_WStateItemImages2: TWMImages;
  g_WDnItemImages: TWMImages;
  g_WDnItemImages2: TWMImages;
  g_WHumImgImages: TWMImages;
  g_WHum2ImgImages: TWMImages;
  g_WHum3ImgImages: TWMImages;
  g_WHairImgImages: TWMImages;
  g_WHair2ImgImages: TWMImages;
  g_WWeaponImages: TWMImages;
  g_WWeapon2Images: TWMImages;
  g_WWeapon3Images: TWMImages;
  g_WMagIconImages: TWMImages;
  g_WMagIcon2Images: TWMImages;
  g_WNpcImgImages: TWMImages;
  g_WNpc2ImgImages: TWMImages;
  g_WMagicImages: TWMImages;
  g_WMagic2Images: TWMImages;
  g_WMagic3Images: TWMImages;
  g_WMagic4Images: TWMImages;
  g_WMagic5Images: TWMImages;
  g_WMagic6Images: TWMImages;
  g_WMagic7Images: TWMImages;
  g_WMagic7Images2: TWMImages;
  g_WMagic8Images: TWMImages;
  g_WMagic8Images2: TWMImages;
  g_WMagic9Images: TWMImages = nil;
  g_WMagic10Images: TWMImages = nil;
  g_StateEffect: TWMImages;
  g_ShineEffect: TWMImages;
  //g_Shineitem: TWMImages;
  g_ShineItems: TWMImages;
  g_ShineDnItems: TWMImages;
  //新加补丁
  g_WAniTilesArr: TWMImages;
{$IF CUSTOMLIBFILE = 1}
  g_WEventEffectImages: TWMImages;
{$IFEND}
  g_WObjectArr: array[0..255] of TWMImages;
  g_WTilesArr: array[0..255] of TWMImages;
  g_WSmTilesArr: array[0..255] of TWMImages;

  g_cboEffect: TWMImages;
  g_cbohair: TWMImages;
  g_cbohum: TWMImages;
  g_cbohum3: TWMImages;
  g_cboHumEffect: TWMImages;
  g_cboHumEffect2: TWMImages;
  g_cboHumEffect3: TWMImages;
  g_cboweapon: TWMImages;
  g_cboweapon3: TWMImages;

  g_PowerBlock: TPowerBlock = (
    $55, $8B, $EC, $83, $C4, $E8, $89, $55, $F8, $89, $45, $FC, $C7, $45, $EC, $E8,
    $03, $00, $00, $C7, $45, $E8, $64, $00, $00, $00, $DB, $45, $EC, $DB, $45, $E8,
    $DE, $F9, $DB, $45, $FC, $DE, $C9, $DD, $5D, $F0, $9B, $8B, $45, $F8, $8B, $00,
    $8B, $55, $F8, $89, $02, $DD, $45, $F0, $8B, $E5, $5D, $C3,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
    );
  g_PowerBlock1: TPowerBlock = (
    $55, $8B, $EC, $83, $C4, $E8, $89, $55, $F8, $89, $45, $FC, $C7, $45, $EC, $64,
    $00, $00, $00, $C7, $45, $E8, $64, $00, $00, $00, $DB, $45, $EC, $DB, $45, $E8,
    $DE, $F9, $DB, $45, $FC, $DE, $C9, $DD, $5D, $F0, $9B, $8B, $45, $F8, $8B, $00,
    $8B, $55, $F8, $89, $02, $DD, $45, $F0, $8B, $E5, $5D, $C3,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
    );
  //g_RegInfo                 : TRegInfo;
  g_sServerName: string; //服务器显示名称
  g_sServerMiniName: string; //服务器名称
  g_psServerAddr: PString;
  g_sSelChrAddr: string;
  g_nSelChrPort: Integer;
  g_sRunServerAddr: string;
  g_nRunServerPort: Integer;
  g_nTopDrawPos: Integer = 0;
  g_nLeftDrawPos: Integer = 0;

  g_boForceMapDraw: Boolean = False;
  g_boForceMapFileLoad: Boolean = False;

  g_boSendLogin: Boolean; //是否发送登录消息
  g_boServerConnected: Boolean;
  g_SoftClosed: Boolean;
  g_ChrAction: TChrAction;
  g_ConnectionStep: TConnectionStep;
  //g_boSound                 : Boolean;
  //g_boBGSound               : Boolean;

  g_MainFont: string = '宋体';
  g_FontArr: array[0..MAXFONT - 1] of string = (
    '宋体',
    '新宋体',
    '仿宋',
    '楷体',
    'Courier New',
    'Arial',
    'MS Sans Serif',
    'Microsoft Sans Serif'
    );

  g_OldTime: Longword;
  g_nCurFont: Integer = 0;
  g_sCurFontName: string = '宋体';

  g_boFullScreen: Boolean = False;
  //g_BitSperpelChanged       : Boolean = False;
  //g_ColorBits               : Integer;
  //g_BitSperpelCount         : Integer = 32;
  //g_nDrawCount              : Integer = 18;

  g_boDXDrawize: Boolean = False;

  g_DlgInitialize: Boolean;
//  g_ImgMixSurface: TCustomLockableTexture;
//  g_ImgLargeMixSurface: TCustomLockableTexture;
//  g_MiniMapSurface: TCustomLockableTexture;

  g_HeathBar_Red: TCustomLockableTexture;
  g_HeathBar_Green: TCustomLockableTexture;
  g_HeathBar_Blue: TCustomLockableTexture;
  g_HeathBar_Yellow: TCustomLockableTexture;
  g_HintSurface_Y: TCustomLockableTexture;
  g_HintSurface_B: TCustomLockableTexture;
  g_HintSurface_C: TCustomLockableTexture;
  g_HintSurface_Olive: TCustomLockableTexture;
  g_HintSurface_Red: TCustomLockableTexture;

  g_HintSurface_W: TCustomLockableTexture;
  g_BotSurface: TCustomLockableTexture;

  g_boFirstActive: Boolean = True;
  g_boFirstTime: Boolean = False;
  g_sMapTitle: string;
  g_nLastMapMusic: Integer = -1;

  g_SendSayList: TStringList;
  g_SendSayListIdx: Integer = 0;

  g_ServerList: TStringList;
  g_GroupMembers: THStringList;
  g_SoundList: TStringList;
  BGMusicList: TStringList;
  g_DxFontsMgrTick: Longword;
  g_MagicArr: array[0..2, 0..255] of PTClientMagic;

  g_MagicList: TList;
{$IF SERIESSKILL}
  g_MagicList2, g_hMagicList2: TList;
{$IFEND SERIESSKILL}
  g_IPMagicList: TList;

  g_HeroMagicList: TList;
  g_HeroIPMagicList: TList;
  g_ShopListArr: array[0..5] of TList;

  g_SaveItemList: TList;
  g_MenuItemList: TList;
  g_DropedItemList: TList;
  g_ChangeFaceReadyList: TList;
  g_FreeActorList: TList;

  g_PoisonIndex: Integer;
  g_nBonusPoint: Integer;
  g_nSaveBonusPoint: Integer;
  g_BonusTick: TNakedAbility;
  g_BonusAbil: TNakedAbility;
  g_NakedAbil: TNakedAbility;
  g_BonusAbilChg: TNakedAbility;

  g_sGuildName: string;
  g_sGuildRankName: string;

  g_dwLatestJoinAttackTick: Longword; //最后魔法攻击时间

  g_dwLastAttackTick: Longword; //最后攻击时间(包括物理攻击及魔法攻击)
  g_dwLastMoveTick: Longword; //最后移动时间
  g_dwLatestSpellTick: Longword; //最后魔法攻击时间
  g_dwLatestFireHitTick: Longword; //最后列火攻击时间
  g_dwLatestSLonHitTick: Longword; //最后列火攻击时间
  g_dwLatestTwinHitTick: Longword;
  g_dwLatestPursueHitTick: Longword;
  g_dwLatestRushHitTick: Longword;
  g_dwLatestSmiteHitTick: Longword;
  g_dwLatestSmiteLongHitTick: Longword;
  g_dwLatestSmiteLongHitTick2: Longword;
  g_dwLatestSmiteLongHitTick3: Longword;
  g_dwLatestSmiteWideHitTick: Longword;
  g_dwLatestSmiteWideHitTick2: Longword;
  g_dwLatestRushRushTick: Longword; //最后被推动时间
  //g_dwLatestStruckTick      : LongWord; //最后弯腰时间
  //g_dwLatestHitTick         : LongWord; //最后物理攻击时间(用来控制攻击状态不能退出游戏)
  //g_dwLatestMagicTick       : LongWord; //最后放魔法时间(用来控制攻击状态不能退出游戏)

  g_dwMagicDelayTime: Longword;
  g_dwMagicPKDelayTime: Longword;

  g_nMouseCurrX: Integer; //鼠标所在地图位置座标X
  g_nMouseCurrY: Integer; //鼠标所在地图位置座标Y
  g_nMouseX: Integer; //鼠标所在屏幕位置座标X
  g_nMouseY: Integer; //鼠标所在屏幕位置座标Y

  g_nTargetX: Integer; //目标座标
  g_nTargetY: Integer; //目标座标
  g_TargetCret: TActor;
  g_FocusCret: TActor;
  g_MagicTarget: TActor;

  g_APQueue: PLink;
  g_APPathList: TList;
  g_APPass: PTAPPass; //array[0..MAXX * 3, 0..MAXY * 3] of DWORD;
  g_APTagget: TActor;

  ///////////////////////////////
  g_APRunTick: Longword;
  g_APRunTick2: Longword;
  g_AutoPicupItem: pTDropItem = nil;
  g_nAPStatus: Integer;
  g_boAPAutoMove: Boolean;
  g_boLongHit: Boolean = False;
  g_sAPStr: string;
  g_boAPXPAttack: Integer = 0;

  m_dwSpellTick: Longword;
  m_dwRecallTick: Longword;
  m_dwDoubluSCTick: Longword;
  m_dwPoisonTick: Longword;
  m_btMagPassTh: Integer = 0;
  g_nTagCount: Integer = 0;
  m_dwTargetFocusTick: Longword;
  g_APPickUpList: THStringList;
  g_APMobList: THStringList;
  g_ItemDesc: THStringList;

  g_AttackInvTime: Integer = 900;
  g_AttackTarget: TActor;
  g_dwSearchEnemyTick: Longword;
  g_boAllowJointAttack: Boolean = False;

  //////////////////////////////////////////
  g_nAPReLogon: Byte = 0;
  g_nAPrlRecallHero: Boolean = False;
  g_nAPSendSelChr: Boolean = False;
  g_nAPSendNotice: Boolean = False;

  g_nAPReLogonWaitTick: Longword;
  g_nAPReLogonWaitTime: Integer = 10 * 1000;

  g_ApLastSelect: Integer = 0;
  g_nOverAPZone: Integer = 0;
  g_nOverAPZone2: Integer = 0;

  g_APGoBack: Boolean = False;
  g_APGoBack2: Boolean = False;

  g_APStep: Integer = -1;
  g_APStep2: Integer = -1;

  g_APMapPath: array of TPoint;
  g_APMapPath2: array of TPoint;

  g_APLastPoint: TPoint;
  g_APLastPoint2: TPoint;

  g_nApMiniMap: Boolean = False;
  g_dwBlinkTime: Longword;
  g_boViewBlink: Boolean;

  //g_boAttackSlow            : Boolean;  //腕力不够时慢动作攻击.
  //g_boAttackFast            : Byte = 0;
  //g_boMoveSlow              : Boolean;  //负重不够时慢动作跑
  //g_nMoveSlowLevel          : Integer;

  g_boMapMoving: Boolean;
  g_boMapMovingWait: Boolean;
  g_boCheckBadMapMode: Boolean;
  g_boCheckSpeedHackDisplay: Boolean;
  g_boViewMiniMap: Boolean;
  g_nViewMinMapLv: Integer;
  g_nMiniMapIndex: Integer;

  //NPC 相关
  g_nCurMerchant: Integer;
  g_nMDlgX: Integer;
  g_nMDlgY: Integer;
  g_nStallX: Integer;
  g_nStallY: Integer;

  g_dwChangeGroupModeTick: Longword;
  g_dwDealActionTick: Longword;
  g_dwQueryMsgTick: Longword;
  g_nDupSelection: Integer;

  g_boAllowGroup: Boolean;

  //人物信息相关
  g_nMySpeedPoint: Integer; //敏捷
  g_nMyHitPoint: Integer; //准确
  g_nMyAntiPoison: Integer; //魔法躲避
  g_nMyPoisonRecover: Integer; //中毒恢复
  g_nMyHealthRecover: Integer; //体力恢复
  g_nMySpellRecover: Integer; //魔法恢复
  g_nMyAntiMagic: Integer; //魔法躲避
  g_nMyHungryState: Integer; //饥饿状态
  g_nMyIPowerRecover: Integer; //中毒恢复
  g_nMyAddDamage: Integer;
  g_nMyDecDamage: Integer;
  //g_nMyGameDiamd            : Integer = 0;
  //g_nMyGameGird             : Integer = 0;
  //g_nMyGameGold             : Integer = 0;

  g_nHeroSpeedPoint: Integer; //敏捷
  g_nHeroHitPoint: Integer; //准确
  g_nHeroAntiPoison: Integer; //魔法躲避
  g_nHeroPoisonRecover: Integer; //中毒恢复
  g_nHeroHealthRecover: Integer; //体力恢复
  g_nHeroSpellRecover: Integer; //魔法恢复
  g_nHeroAntiMagic: Integer; //魔法躲避
  g_nHeroHungryState: Integer; //饥饿状态
  g_nHeroBagSize: Integer = 40;
  g_nHeroIPowerRecover: Integer;
  g_nHeroAddDamage: Integer;
  g_nHeroDecDamage: Integer;

  g_wAvailIDDay: Word;
  g_wAvailIDHour: Word;
  g_wAvailIPDay: Word;
  g_wAvailIPHour: Word;

  g_MySelf: THumActor;
  g_MyDrawActor: THumActor;

  g_sAttackMode: string = '';

  sAttackModeOfAll: string = '[全体攻击模式]';
  sAttackModeOfPeaceful: string = '[和平攻击模式]';
  sAttackModeOfDear: string = '[夫妻攻击模式]';
  sAttackModeOfMaster: string = '[师徒攻击模式]';
  sAttackModeOfGroup: string = '[编组攻击模式]';
  sAttackModeOfGuild: string = '[行会攻击模式]';
  sAttackModeOfRedWhite: string = '[善恶攻击模式]';

  g_RIWhere: Integer = 0;
  g_RefineItems: array[0..2] of TMovingItem;

  g_BuildAcusesStep: Integer = 0;
  g_BuildAcusesProc: Integer = 0;
  g_BuildAcusesProcTick: Longword;

  g_BuildAcusesSuc: Integer = -1;
  g_BuildAcusesSucFrame: Integer = 0;
  g_BuildAcusesSucFrameTick: Longword;

  g_BuildAcusesFrameTick: Longword;
  g_BuildAcusesFrame: Integer = 0;
  g_BuildAcusesRate: Integer = 0;
  g_BuildAcuses: array[0..7] of TMovingItem;
  g_BAFirstShape: Integer = -1;
  //g_BAFirstShape2           : Integer = -1;
  g_tui: array[0..13] of TClientItem;
  g_UseItems: array[0..U_FASHION] of TClientItem;
  g_HeroUseItems: array[0..U_FASHION] of TClientItem;
  UserState1: TUserStateInfo;

  g_detectItemShine: TItemShine;
  UserState1Shine: array[0..U_FASHION] of TItemShine;
  g_UseItemsShine: array[0..U_FASHION] of TItemShine;
  g_HeroUseItemsShine: array[0..U_FASHION] of TItemShine;

  g_ItemArr: array[0..MAXBAGITEMCL - 1] of TClientItem;
  g_HeroItemArr: array[0..MAXBAGITEMCL - 1] of TClientItem;

  g_ItemArrShine: array[0..MAXBAGITEMCL - 1] of TItemShine;
  g_HeroItemArrShine: array[0..MAXBAGITEMCL - 1] of TItemShine;
  g_StallItemArrShine: array[0..10 - 1] of TItemShine;
  g_uStallItemArrShine: array[0..10 - 1] of TItemShine;

  g_DealItemsShine: array[0..10 - 1] of TItemShine;
  g_DealRemoteItemsShine: array[0..20 - 1] of TItemShine;

  g_MovingItemShine: TItemShine;

  g_boBagLoaded: Boolean;
  //g_boHeroBagLoaded         : Boolean;
  g_boServerChanging: Boolean;

  //键盘相关
  g_ToolMenuHook: HHOOK;
  g_ToolMenuHookLL: HHOOK;
  g_nLastHookKey: Integer;
  g_dwLastHookKeyTime: Longword;

  g_nCaptureSerial: Integer; //抓图文件名序号
  //g_nSendCount              : Integer; //发送操作计数
  g_nReceiveCount: Integer; //接改操作状态计数
  g_nTestSendCount: Integer;
  g_nTestReceiveCount: Integer;
  g_nSpellCount: Integer; //使用魔法计数
  g_nSpellFailCount: Integer; //使用魔法失败计数
  g_nFireCount: Integer; //
  g_nDebugCount: Integer;
  g_nDebugCount1: Integer;
  g_nDebugCount2: Integer;

  //买卖相关
  g_SellDlgItem: TClientItem;
  g_TakeBackItemWait: TMovingItem;
  g_SellDlgItemSellWait: TMovingItem; //TClientItem;

  g_DetectItem: TClientItem;
  g_DetectItemMineID: Integer = 0;
  g_DealDlgItem: TClientItem;
  g_boQueryPrice: Boolean;
  g_dwQueryPriceTime: Longword;
  g_sSellPriceStr: string;

  //交易相关
  g_DealItems: array[0..9] of TClientItem;
  g_boYbDealing: Boolean = False;
  g_YbDealInfo: TClientPS;
  g_YbDealItems: array[0..9] of TClientItem;
  g_DealRemoteItems: array[0..19] of TClientItem;
  g_nDealGold: Integer;
  g_nDealRemoteGold: Integer;
  g_boDealEnd: Boolean;
  g_sDealWho: string;
  g_MouseItem: TClientItem;
  g_MouseStateItem: TClientItem;
  g_HeroMouseStateItem: TClientItem;
  g_MouseUserStateItem: TClientItem;
  g_HeroMouseItem: TClientItem;
  g_ClickShopItem: TShopItem;

  g_boItemMoving: Boolean;
  g_MovingItem: TMovingItem;
  g_OpenBoxItem: TMovingItem;
  g_WaitingUseItem: TMovingItem;
  g_WaitingStallItem: TMovingItem;
  g_WaitingDetectItem: TMovingItem;

  g_FocusItem, g_FocusItem2: pTDropItem;
  g_boOpenStallSystem: Boolean = True;
  g_boAutoLongAttack: Boolean = True;
  g_boAutoSay: Boolean = True;
  g_boMutiHero: Boolean = True;
  g_boSkill_114_MP: Boolean = False;
  g_boSkill_68_MP: Boolean = False;
  g_nDayBright: Integer;
  g_nAreaStateValue: Integer;
  g_boNoDarkness: Boolean;
  g_nRunReadyCount: Integer;

  g_boLastViewFog: Boolean = False;
//{$IF VIEWFOG}
  g_boViewFog: Boolean = True; //是否显示黑暗
  g_boForceNotViewFog: Boolean = False; //免蜡烛
//{$ELSE}
//  g_boViewFog: Boolean = False; //是否显示黑暗
//  g_boForceNotViewFog: Boolean = True; //免蜡烛
//{$IFEND VIEWFOG}

  g_EatingItem: TClientItem;
  g_dwEatTime: Longword; //timeout...
  g_dwHeroEatTime: Longword;

  g_dwDizzyDelayStart: Longword;
  g_dwDizzyDelayTime: Longword;

  g_boDoFadeOut: Boolean;
  g_boDoFadeIn: Boolean;
  g_nFadeIndex: Integer;
  g_boDoFastFadeOut: Boolean;

  g_boAutoDig, g_boAutoSit: Boolean; //自动锄矿
  g_boSelectMyself: Boolean; //鼠标是否指到自己

  //游戏速度检测相关变量
  g_dwFirstServerTime: Longword;
  g_dwFirstClientTime: Longword;
  //ServerTimeGap: int64;
  g_nTimeFakeDetectCount: Integer;
  //g_dwSHGetCount            : PLongWord;
  //g_dwSHGetTime             : LongWord;
  //g_dwSHTimerTime           : LongWord;
  //g_nSHFakeCount            : Integer;  //检查机器速度异常次数，如果超过4次则提示速度不稳定

  g_dwLatestClientTime2: Longword;
  g_dwFirstClientTimerTime: Longword; //timer 矫埃
  g_dwLatestClientTimerTime: Longword;
  g_dwFirstClientGetTime: Longword; //gettickcount 矫埃
  g_dwLatestClientGetTime: Longword;
  g_nTimeFakeDetectSum: Integer;
  g_nTimeFakeDetectTimer: Integer;

  g_dwLastestClientGetTime: Longword;

  //外挂功能变量开始
  g_dwDropItemFlashTime: Longword = 8 * 1000; //地面物品闪时间间隔
  g_nHitTime: Integer = 1400; //攻击间隔时间间隔  0820
  g_nItemSpeed: Integer = 60;
  g_dwSpellTime: Longword = 500; //魔法攻间隔时间

  g_boHero: Boolean = True;
  g_boOpenAutoPlay: Boolean = True;
  g_DeathColorEffect: TColorEffect = ceRed;
  g_boClientCanSet: Boolean = True;

  g_nEatIteminvTime: Integer = 200;
  g_boCanRunSafeZone: Boolean = True;
  g_boCanRunHuman: Boolean = True;
  g_boCanRunMon: Boolean = True;
  g_boCanRunNpc: Boolean = True;
  g_boCanRunAllInWarZone: Boolean = False;
  g_boCanStartRun: Boolean = True; //是否允许免助跑
  g_boParalyCanRun: Boolean = False; //麻痹是否可以跑
  g_boParalyCanWalk: Boolean = False; //麻痹是否可以走
  g_boParalyCanHit: Boolean = False; //麻痹是否可以攻击
  g_boParalyCanSpell: Boolean = False; //麻痹是否可以魔法

  g_boShowRedHPLable: Boolean = True; //显示血条
  g_boShowHPNumber: Boolean = True; //显示血量数字
  g_boShowJobLevel: Boolean = True; //显示职业等级
  g_boDuraAlert: Boolean = True; //物品持久警告
  g_boMagicLock: Boolean = True; //魔法锁定
  g_boSpeedRate: Boolean = False;
  g_boSpeedRateShow: Boolean = False;
  //g_boAutoPuckUpItem        : Boolean = False;

  g_boShowHumanInfo: Boolean = True;
  g_boShowMonsterInfo: Boolean = False;
  g_boShowNpcInfo: Boolean = False;
  //外挂功能变量结束
  g_boQuickPickup: Boolean = False;
  g_dwAutoPickupTick: Longword;
  g_dwAutoPickupTime: Longword = 100; //自动捡物品间隔
  //g_AutoPickupList          : TList;
  g_MagicLockActor: TActor;
  g_boNextTimePowerHit: Boolean;
  g_boCanLongHit: Boolean;
  g_boCanWideHit: Boolean;
  g_boCanCrsHit: Boolean;
  g_boCanStnHit: Boolean;
  g_boNextTimeFireHit: Boolean;
  g_boNextTimeTwinHit: Boolean;
  g_boNextTimePursueHit: Boolean;
  g_boNextTimeRushHit: Boolean;
  g_boNextTimeSmiteHit: Boolean;
  g_boNextTimeSmiteLongHit: Boolean;
  g_boNextTimeSmiteLongHit2: Boolean;
  g_boNextTimeSmiteLongHit3: Boolean;
  g_boNextTimeSmiteWideHit: Boolean;
  g_boNextTimeSmiteWideHit2: Boolean;

  g_boCanSLonHit: Boolean = False;
  g_boCanSquHit: Boolean;
  g_ShowItemList: THStringList;
  g_boDrawTileMap: Boolean = True;
  g_boDrawDropItem: Boolean = True;

  g_nTestX: Integer = 71;
  g_nTestY: Integer = 212;

  g_nSquHitPoint: Integer = 0;
  g_nMaxSquHitPoint: Integer = 0;

  g_boConfigLoaded: Boolean = False;

  g_dwCollectExpLv: Byte = 0;
  g_boCollectStateShine: Boolean = False;
  g_nCollectStateShine: Integer = 0;
  g_dwCollectStateShineTick: Longword;
  g_dwCollectStateShineTick2: Longword;

  g_dwCollectExp: Longword = 0;
  g_dwCollectExpMax: Longword = 1;
  g_boCollectExpShine: Boolean = False;
  g_boCollectExpShineCount: Integer = 0;
  g_dwCollectExpShineTick: Longword;

  g_dwCollectIpExp: Longword = 0;
  g_dwCollectIpExpMax: Longword = 1;

  //20200726开关集合
  g_Button_DetectBox: Boolean = False;
  g_Button_Mission: Boolean = False;
  g_Button_Shop: Boolean = False;
  g_Button_Friend: Boolean = False;
  g_Button_PlusAbil: Boolean = False;
  g_Button_baoguo: Boolean = True; //包裹
  g_ShowWinExpMode: Boolean = False;
  g_IgnoreStruck: Boolean = False;


  g_LightImages: array[0..5] of TCustomLockableTexture;

  DlgConf: TConfig = (
    DBottom: (Image: 1; Left: 0; Top: 0; Width: 0; Height: 0);
    DMyState: (Image: 8; Left: 643; Top: 62; Width: 0; Height: 0);
    DMyBag: (Image: 9; Left: 682; Top: 42; Width: 0; Height: 0);
    DMyMagic: (Image: 10; Left: 722; Top: 22; Width: 0; Height: 0);
    DOption: (Image: 11; Left: 764; Top: 12; Width: 0; Height: 0);

    DBotMiniMap: (Image: 130; Left: 209; Top: 104; Width: 0; Height: 0);
    DBotTrade: (Image: 132; Left: 209 + 30; Top: 104; Width: 0; Height: 0);
    DBotGuild: (Image: 134; Left: 209 + 30 * 2; Top: 104; Width: 0; Height: 0);
    DBotGroup: (Image: 128; Left: 209 + 30 * 3; Top: 104; Width: 0; Height: 0);
    DBotFriend: (Image: 34; Left: 209 + 30 * 4; Top: 104; Width: 0; Height: 0);
    DBotDare: (Image: 36; Left: 209 + 30 * 5; Top: 104; Width: 0; Height: 0);
    DBotLevelRank: (Image: 460; Left: 209 + 30 * 6; Top: 104; Width: 0; Height: 0);

    DBotPlusAbil: (Image: 140; Left: 209 + 30 * 8; Top: 104; Width: 0; Height: 0);
    //DBotMemo: (Image: 532; Left: SCREENWIDTH div 2 + (SCREENWIDTH div 2 - (400 - 353)) {753}; Top: 204; Width: 0; Height: 0);
    //DBotExit: (Image: 138; Left: SCREENWIDTH div 2 + (SCREENWIDTH div 2 - (400 - 160)) {560}; Top: 104; Width: 0; Height: 0);
    //DBotLogout: (Image: 136; Left: SCREENWIDTH div 2 + (SCREENWIDTH div 2 - (400 - 160)) - 30 {560}; Top: 104; Width: 0; Height: 0);
    DBelt1: (Image: 0; Left: 285; Top: 59; Width: 32; Height: 29);
    DBelt2: (Image: 0; Left: 328; Top: 59; Width: 32; Height: 29);
    DBelt3: (Image: 0; Left: 371; Top: 59; Width: 32; Height: 29);
    DBelt4: (Image: 0; Left: 415; Top: 59; Width: 32; Height: 29);
    DBelt5: (Image: 0; Left: 459; Top: 59; Width: 32; Height: 29);
    DBelt6: (Image: 0; Left: 503; Top: 59; Width: 32; Height: 29);
    DGold: (Image: 29; Left: 10; Top: 190; Width: 0; Height: 0);
    DRfineItem: (Image: 26; Left: 254; Top: 183; Width: 48; Height: 22);
    DCloseBag: (Image: 371; Left: 309; Top: 203; Width: 14; Height: 20);
    DMerchantDlg: (Image: 384; Left: 0; Top: 0; Width: 0; Height: 0);
    DMerchantDlgClose: (Image: 64; Left: 399; Top: 1; Width: 0; Height: 0);
    DConfigDlg: (Image: 204; Left: 0; Top: 0; Width: 0; Height: 0);
    DConfigDlgOK: (Image: 361; Left: 514; Top: 287; Width: 0; Height: 0);
    DConfigDlgClose: (Image: 64; Left: 584; Top: 6; Width: 0; Height: 0);
    DMenuDlg: (Image: 385; Left: 138; Top: 163; Width: 0; Height: 0);
    DMenuPrev: (Image: 388; Left: 43; Top: 175; Width: 0; Height: 0);
    DMenuNext: (Image: 387; Left: 90; Top: 175; Width: 0; Height: 0);
    DMenuBuy: (Image: 386; Left: 215; Top: 171; Width: 0; Height: 0);
    DMenuClose: (Image: 64; Left: 291; Top: 0; Width: 0; Height: 0);
    DSellDlg: (Image: 392; Left: 328; Top: 163; Width: 0; Height: 0);
    DSellDlgOk: (Image: 393; Left: 85; Top: 150; Width: 0; Height: 0);
    DSellDlgClose: (Image: 64; Left: 115; Top: 0; Width: 0; Height: 0);
    DSellDlgSpot: (Image: 0; Left: 27; Top: 67; Width: 0; Height: 0);
    DKeySelDlg: (Image: 620; Left: 0; Top: 0; Width: 0; Height: 0);
    DKsIcon: (Image: 0; Left: 51; Top: 31; Width: 0; Height: 0);
    DKsF1: (Image: 232; Left: 25; Top: 78; Width: 0; Height: 0);
    DKsF2: (Image: 234; Left: 57; Top: 78; Width: 0; Height: 0);
    DKsF3: (Image: 236; Left: 89; Top: 78; Width: 0; Height: 0);
    DKsF4: (Image: 238; Left: 121; Top: 78; Width: 0; Height: 0);
    DKsF5: (Image: 240; Left: 160; Top: 78; Width: 0; Height: 0);
    DKsF6: (Image: 242; Left: 192; Top: 78; Width: 0; Height: 0);
    DKsF7: (Image: 244; Left: 224; Top: 78; Width: 0; Height: 0);
    DKsF8: (Image: 246; Left: 256; Top: 78; Width: 0; Height: 0);
    DKsConF1: (Image: 626; Left: 25; Top: 120; Width: 0; Height: 0);
    DKsConF2: (Image: 628; Left: 57; Top: 120; Width: 0; Height: 0);
    DKsConF3: (Image: 630; Left: 89; Top: 120; Width: 0; Height: 0);
    DKsConF4: (Image: 632; Left: 121; Top: 120; Width: 0; Height: 0);
    DKsConF5: (Image: 633; Left: 160; Top: 120; Width: 0; Height: 0);
    DKsConF6: (Image: 634; Left: 192; Top: 120; Width: 0; Height: 0);
    DKsConF7: (Image: 638; Left: 224; Top: 120; Width: 0; Height: 0);
    DKsConF8: (Image: 640; Left: 256; Top: 120; Width: 0; Height: 0);
    DKsNone: (Image: 623; Left: 296; Top: 78; Width: 0; Height: 0);
    DKsOk: (Image: 621; Left: 296; Top: 120; Width: 0; Height: 0);
    DChgGamePwd: (Image: 621; Left: 296; Top: 120; Width: 0; Height: 0);
    DChgGamePwdClose: (Image: 64; Left: 312; Top: 1; Width: 0; Height: 0);
    DItemGrid: (Image: 0; Left: 29; Top: 41; Width: 286; Height: 162);
    );

const
  // Windows 2000/XP multimedia keys (adapted from winuser.h and renamed to avoid potential conflicts)
  // See also: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/winui/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
  _VK_BROWSER_BACK = $A6; // Browser Back key
  _VK_BROWSER_FORWARD = $A7; // Browser Forward key
  _VK_BROWSER_REFRESH = $A8; // Browser Refresh key
  _VK_BROWSER_STOP = $A9; // Browser Stop key
  _VK_BROWSER_SEARCH = $AA; // Browser Search key
  _VK_BROWSER_FAVORITES = $AB; // Browser Favorites key
  _VK_BROWSER_HOME = $AC; // Browser Start and Home key
  _VK_VOLUME_MUTE = $AD; // Volume Mute key
  _VK_VOLUME_DOWN = $AE; // Volume Down key
  _VK_VOLUME_UP = $AF; // Volume Up key
  _VK_MEDIA_NEXT_TRACK = $B0; // Next Track key
  _VK_MEDIA_PREV_TRACK = $B1; // Previous Track key
  _VK_MEDIA_STOP = $B2; // Stop Media key
  _VK_MEDIA_PLAY_PAUSE = $B3; // Play/Pause Media key
  _VK_LAUNCH_MAIL = $B4; // Start Mail key
  _VK_LAUNCH_MEDIA_SELECT = $B5; // Select Media key
  _VK_LAUNCH_APP1 = $B6; // Start Application 1 key
  _VK_LAUNCH_APP2 = $B7; // Start Application 2 key
  // Self-invented names for the extended keys
  NAME_VK_BROWSER_BACK = 'Browser Back';
  NAME_VK_BROWSER_FORWARD = 'Browser Forward';
  NAME_VK_BROWSER_REFRESH = 'Browser Refresh';
  NAME_VK_BROWSER_STOP = 'Browser Stop';
  NAME_VK_BROWSER_SEARCH = 'Browser Search';
  NAME_VK_BROWSER_FAVORITES = 'Browser Favorites';
  NAME_VK_BROWSER_HOME = 'Browser Start/Home';
  NAME_VK_VOLUME_MUTE = 'Volume Mute';
  NAME_VK_VOLUME_DOWN = 'Volume Down';
  NAME_VK_VOLUME_UP = 'Volume Up';
  NAME_VK_MEDIA_NEXT_TRACK = 'Next Track';
  NAME_VK_MEDIA_PREV_TRACK = 'Previous Track';
  NAME_VK_MEDIA_STOP = 'Stop Media';
  NAME_VK_MEDIA_PLAY_PAUSE = 'Play/Pause Media';
  NAME_VK_LAUNCH_MAIL = 'Start Mail';
  NAME_VK_LAUNCH_MEDIA_SELECT = 'Select Media';
  NAME_VK_LAUNCH_APP1 = 'Start Application 1';
  NAME_VK_LAUNCH_APP2 = 'Start Application 2';

procedure InitClientItems();
function GetTIHintString1(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
function GetTIHintString2(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
procedure AutoPutOntiBooks();
procedure AutoPutOntiCharms();

function GetSuiteAbil(idx, Shape: Integer; var sa: TtSuiteAbil): Boolean;

procedure AutoPutOntiSecretBooks();

function GetSpHintString1(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
function GetSpHintString2(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;

function GetEvaluationInfo(CurrMouseItem: TClientItem): string;
function GetSecretAbil(CurrMouseItem: TClientItem): Boolean;

procedure InitObj();
procedure LoadItemDesc();
procedure LoadItemFilter();
procedure LoadItemFilter2();
procedure SaveItemFilter();
function getSuiteHint(var idx: Integer; s: string; gender: Byte): pTClientSuiteItems;
function GetItemWhere(Item: TClientItem): Integer;
procedure LoadWMImagesLib();
procedure InitWMImagesLib({DDxDraw: TDXDraw; }bFirst: Boolean);
procedure InitWMImagesLibSecond;
procedure UnLoadWMImagesLib();
procedure LoadLightImages();
procedure UnLoadLightImages();
function GetObjs(nUnit, nIdx: Integer): TCustomLockableTexture;
function GetObjsEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture;

function GetTiles(nUnit, nIdx: Integer): TCustomLockableTexture;
function GetTilesEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture;
function GetSmTiles(nUnit, nIdx: Integer): TCustomLockableTexture;
function GetSmTilesEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture;


function GetMonImg(nAppr: Integer): TWMImages;
//function GetMonAction(nAppr: Integer): pTMonsterAction;
function GetJobName(nJob: Integer): string;
//procedure ClearShowItemList();
function GetItemType(ItemType: TItemType): string;
procedure LoadUserConfig(sUserName: string);
//procedure SaveUserConfig(sUserName: string);
function IsPersentHP(nMin, nMax: Integer): Boolean;
function IsPersentMP(nMin, nMax: Integer): Boolean;
function IsPersentSpc(nMin, nMax: Integer): Boolean;
function IsPersentBook(nMin, nMax: Integer): Boolean;

function IsPersentHPHero(nMin, nMax: Integer): Boolean;
function IsPersentMPHero(nMin, nMax: Integer): Boolean;
function IsPersentSpcHero(nMin, nMax: Integer): Boolean;

function GetHotKey(Modifiers, Key: Word): Cardinal;
procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
function HotKeyToText(HotKey: Cardinal; Localized: Boolean): string;

procedure CheckSpeedCount(Count: Integer = 2);
function GetSeriesSkillIcon(id: Integer): Integer;
procedure ResetSeriesSkillVar();
//function timeGetTime: DWORD; stdcall;
function GetTickCount: DWORD; //stdcall;

function IsDetectItem(idx: Integer): Boolean;
function IsBagItem(idx: Integer): Boolean;
function IsEquItem(idx: Integer): Boolean;
function IsStorageItem(idx: Integer): Boolean;
function IsStallItem(idx: Integer): Boolean;
function GetLevelColor(iLevel: Byte): Integer;

procedure ScreenChanged();
function ShiftYOffset(): Integer;
procedure InitScreenConfig();

function IsInMyRange(Act: TActor): Boolean;
function IsItemInMyRange(X, Y: Integer): Boolean;

function GetTitle(nItemIdx: Integer): pTClientStdItem;
//function GetGameImages(LoadMode: TLoadMode; const FileName: string): TWMImages;
function FillString(const Str: string; Len: Integer; FillStr: Char): string;
//123456
procedure LoadMapDesc();
procedure SetLoginGateKey(const Value: String);

const
  mmsyst = 'winmm.dll';
  kernel32 = 'kernel32.dll';
  HotKeyAtomPrefix = 'HotKeyManagerHotKey';
  ModName_Shift = 'Shift';
  ModName_Ctrl = 'Ctrl';
  ModName_Alt = 'Alt';
  ModName_Win = 'Win';
  VK2_SHIFT = 32;
  VK2_CONTROL = 64;
  VK2_ALT = 128;
  VK2_WIN = 256;

var
  EnglishKeyboardLayout: HKL;
  ShouldUnloadEnglishKeyboardLayout: Boolean;
  LocalModName_Shift: string = ModName_Shift;
  LocalModName_Ctrl: string = ModName_Ctrl;
  LocalModName_Alt: string = ModName_Alt;
  LocalModName_Win: string = ModName_Win;
  g_MirStartupInfo: TMirStartupInfo;

implementation

uses ClMain, FState, HUtil32,cliUtil, SoundUtil;


procedure SetLoginGateKey(const Value: String);
var
  ADValue: String;
begin
  ADValue := uEDcode.DecodeSource(Value);
  ConsoleDebug(Format('通信秘钥:%s',[ADValue]));
  //uLog.TLogger.AddLog('Password:' + ADValue + '$');
  if ADValue <> '' then
    g_LoginKey := ADValue;
end;

function FillString(const Str: string; Len: Integer; FillStr: Char): string;
begin
  Result:= StringOfChar(FillStr, Len - Length(Str)) + Str;
end;

procedure LoadMapDesc();
var
  i: Integer;
  szFileName, szLine: string;
  xsl: TStringList;
  szMapTitle, szPointX, szPointY, szPlaceName, szColor, szFullMap: string;
  nPointX, nPointY, nFullMap: Integer;
  nColor: TColor;
  pMapDescInfo: pTMapDescInfo;
begin
  szFileName := '.\data\MapDesc2.dat';
  if FileExists(szFileName) then begin
    xsl := TStringList.Create;
    xsl.LoadFromFile(szFileName);
    for i := 0 to xsl.Count - 1 do begin
      szLine := xsl[i];
      if (szLine = '') or (szLine[1] = ';') then
        Continue;
      szLine := GetValidStr3(szLine, szMapTitle, [',']);
      szLine := GetValidStr3(szLine, szPointX, [',']);
      szLine := GetValidStr3(szLine, szPointY, [',']);
      szLine := GetValidStr3(szLine, szPlaceName, [',']);
      szLine := GetValidStr3(szLine, szColor, [',']);
      szLine := GetValidStr3(szLine, szFullMap, [',']);
      nPointX := Str_ToInt(szPointX, -1);
      nPointY := Str_ToInt(szPointY, -1);
      nColor := StrToInt(szColor);
      nFullMap := Str_ToInt(szFullMap, -1);
      if (szPlaceName <> '') and (szMapTitle <> '') and (nPointX >= 0) and (nPointY >= 0) and (nFullMap >= 0) then begin
        New(pMapDescInfo);
        pMapDescInfo.szMapTitle := szMapTitle;
        pMapDescInfo.szPlaceName := szPlaceName;
        pMapDescInfo.nPointX := nPointX;
        pMapDescInfo.nPointY := nPointY;
        pMapDescInfo.nColor := nColor;
        pMapDescInfo.nFullMap := nFullMap;
        //DebugOutStr(format('%.8x', [pMapDescInfo.nColor]));
        g_xMapDescList.AddObject(szMapTitle, TObject(pMapDescInfo));
      end;
    end;
    xsl.Free;
  end;
end;

function GetTickCount: DWORD; // external kernel32 name 'GetTickCount';
var
  li: TLargeInteger;
begin
  //Result := g_tTime;
  //if g_tTime = 0 then
  Result := Windows.GetTickCount();
  //Result := timeGetTime;
end;

function IsDetectItem(idx: Integer): Boolean;
begin
  Result := idx = DETECT_MIIDX_OFFSET;
end;

function IsBagItem(idx: Integer): Boolean;
begin
  Result := idx in [6..MAXBAGITEM - 1];
end;

function IsEquItem(idx: Integer): Boolean;
var sel:Integer;
begin
    Result := False;
    if  idx < 0 then
    begin
      sel := -(idx+1);
      Result :=sel in [0..U_FASHION];
    end;

end;

function IsStorageItem(idx: Integer): Boolean;
begin
  Result := (idx >= SAVE_MIIDX_OFFSET) and (idx < SAVE_MIIDX_OFFSET + 46);
end;

function IsStallItem(idx: Integer): Boolean;
begin
  Result := (idx >= STALL_MIIDX_OFFSET) and (idx < STALL_MIIDX_OFFSET + 10);
end;

procedure ResetSeriesSkillVar();
begin
  g_nCurrentMagic := 888;
  g_nCurrentMagic2 := 888;
  g_SeriesSkillStep := 0;
  g_SeriesSkillFire := False;
  g_SeriesSkillFire_100 := False;
  g_SeriesSkillReady := False;
  g_NextSeriesSkill := False;
  FillChar(g_VenationInfos, SizeOf(g_VenationInfos), 0);
  FillChar(g_TempSeriesSkillArr, SizeOf(g_TempSeriesSkillArr), 0);
  FillChar(g_HTempSeriesSkillArr, SizeOf(g_HTempSeriesSkillArr), 0);
  FillChar(g_SeriesSkillArr, SizeOf(g_SeriesSkillArr), 0);
end;

function GetSeriesSkillIcon(id: Integer): Integer;
begin
  Result := -1;
  case id of
    100: Result := 950;
    101: Result := 952;
    102: Result := 956;
    103: Result := 954;

    104: Result := 942;
    105: Result := 946;
    106: Result := 940;
    107: Result := 944;

    108: Result := 934;
    109: Result := 936;
    110: Result := 932;
    111: Result := 930;
    112: Result := 944;
  end;
end;

procedure CheckSpeedCount(Count: Integer);
begin
  Inc(g_dwCheckCount);
  if g_dwCheckCount > Count then begin
    g_dwCheckCount := 0;
    //g_ModuleDetect.FCheckTick := 0;
  end;
end;

function IsExtendedKey(Key: Word): Boolean;
begin
  Result := ((Key >= _VK_BROWSER_BACK) and (Key <= _VK_LAUNCH_APP2));
end;

function GetHotKey(Modifiers, Key: Word): Cardinal;
var
  HK: Cardinal;
begin
  HK := 0;
  if (Modifiers and MOD_ALT) <> 0 then
    Inc(HK, VK2_ALT);
  if (Modifiers and MOD_CONTROL) <> 0 then
    Inc(HK, VK2_CONTROL);
  if (Modifiers and MOD_SHIFT) <> 0 then
    Inc(HK, VK2_SHIFT);
  if (Modifiers and MOD_WIN) <> 0 then
    Inc(HK, VK2_WIN);
  HK := HK shl 8;
  Inc(HK, Key);
  Result := HK;
end;

function HotKeyToText(HotKey: Cardinal; Localized: Boolean): string;

  function GetExtendedVKName(Key: Word): string;
  begin
    case Key of
      _VK_BROWSER_BACK: Result := NAME_VK_BROWSER_BACK;
      _VK_BROWSER_FORWARD: Result := NAME_VK_BROWSER_FORWARD;
      _VK_BROWSER_REFRESH: Result := NAME_VK_BROWSER_REFRESH;
      _VK_BROWSER_STOP: Result := NAME_VK_BROWSER_STOP;
      _VK_BROWSER_SEARCH: Result := NAME_VK_BROWSER_SEARCH;
      _VK_BROWSER_FAVORITES: Result := NAME_VK_BROWSER_FAVORITES;
      _VK_BROWSER_HOME: Result := NAME_VK_BROWSER_HOME;
      _VK_VOLUME_MUTE: Result := NAME_VK_VOLUME_MUTE;
      _VK_VOLUME_DOWN: Result := NAME_VK_VOLUME_DOWN;
      _VK_VOLUME_UP: Result := NAME_VK_VOLUME_UP;
      _VK_MEDIA_NEXT_TRACK: Result := NAME_VK_MEDIA_NEXT_TRACK;
      _VK_MEDIA_PREV_TRACK: Result := NAME_VK_MEDIA_PREV_TRACK;
      _VK_MEDIA_STOP: Result := NAME_VK_MEDIA_STOP;
      _VK_MEDIA_PLAY_PAUSE: Result := NAME_VK_MEDIA_PLAY_PAUSE;
      _VK_LAUNCH_MAIL: Result := NAME_VK_LAUNCH_MAIL;
      _VK_LAUNCH_MEDIA_SELECT: Result := NAME_VK_LAUNCH_MEDIA_SELECT;
      _VK_LAUNCH_APP1: Result := NAME_VK_LAUNCH_APP1;
      _VK_LAUNCH_APP2: Result := NAME_VK_LAUNCH_APP2;
    else
      Result := '';
    end;
  end;

  function GetModifierNames: string;
  var
    s: string;
  begin
    s := '';
    if Localized then begin
      if (HotKey and $4000) <> 0 then // scCtrl
        s := s + LocalModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then // scShift
        s := s + LocalModName_Shift + '+';
      if (HotKey and $8000) <> 0 then // scAlt
        s := s + LocalModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        s := s + LocalModName_Win + '+';
    end
    else begin
      if (HotKey and $4000) <> 0 then // scCtrl
        s := s + ModName_Ctrl + '+';
      if (HotKey and $2000) <> 0 then // scShift
        s := s + ModName_Shift + '+';
      if (HotKey and $8000) <> 0 then // scAlt
        s := s + ModName_Alt + '+';
      if (HotKey and $10000) <> 0 then
        s := s + ModName_Win + '+';
    end;
    Result := s;
  end;

  function GetVKName(Special: Boolean): string;
  var
    scanCode: Cardinal;
    KeyName: array[0..255] of Char;
    oldkl: HKL;
    Modifiers, Key: Word;
  begin
    Result := '';
    if Localized then {// Local language key names}  begin
      if Special then
        scanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16) or (1 shl 24)
      else
        scanCode := (MapVirtualKey(Byte(HotKey), 0) shl 16);
      if scanCode <> 0 then begin
        GetKeyNameText(scanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
      end;
    end
    else {// English key names}  begin
      if Special then
        scanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16) or (1 shl 24)
      else
        scanCode := (MapVirtualKeyEx(Byte(HotKey), 0, EnglishKeyboardLayout) shl 16);
      if scanCode <> 0 then begin
        oldkl := GetKeyboardLayout(0);
        if oldkl <> EnglishKeyboardLayout then
          ActivateKeyboardLayout(EnglishKeyboardLayout, 0); // Set English kbd. layout
        GetKeyNameText(scanCode, KeyName, SizeOf(KeyName));
        Result := KeyName;
        if oldkl <> EnglishKeyboardLayout then begin
          if ShouldUnloadEnglishKeyboardLayout then
            UnloadKeyboardLayout(EnglishKeyboardLayout); // Restore prev. kbd. layout
          ActivateKeyboardLayout(oldkl, 0);
        end;
      end;
    end;

    if Length(Result) <= 1 then begin
      // Try the internally defined names
      SeparateHotKey(HotKey, Modifiers, Key);
      if IsExtendedKey(Key) then
        Result := GetExtendedVKName(Key);
    end;
  end;

var
  KeyName: string;
begin
  case Byte(HotKey) of
    // PgUp, PgDn, End, Home, Left, Up, Right, Down, Ins, Del
    $21..$28, $2D, $2E: KeyName := GetVKName(True);
  else
    KeyName := GetVKName(False);
  end;
  Result := GetModifierNames + KeyName;
end;

procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
var
  Virtuals: Integer;
  v: Word;
  X: Word;
begin
  Key := Byte(HotKey);
  X := HotKey shr 8;
  Virtuals := X;
  v := 0;
  if (Virtuals and VK2_WIN) <> 0 then
    Inc(v, MOD_WIN);
  if (Virtuals and VK2_ALT) <> 0 then
    Inc(v, MOD_ALT);
  if (Virtuals and VK2_CONTROL) <> 0 then
    Inc(v, MOD_CONTROL);
  if (Virtuals and VK2_SHIFT) <> 0 then
    Inc(v, MOD_SHIFT);
  Modifiers := v;
end;

function IsPersentHP(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[0]) {or (nMax - nMin > 1500)};
end;

function IsPersentMP(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[1]) {or (nMax - nMin > 1500)};
end;

function IsPersentSpc(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[3]) {or (nMax - nMin > 6000)};
end;

function IsPersentBook(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[5]) {or (nMax - nMin > 6000)};
end;

function IsPersentHPHero(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[7]) {or (nMax - nMin > 1500)};
end;

function IsPersentMPHero(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[8]) {or (nMax - nMin > 1500)};
end;

function IsPersentSpcHero(nMin, nMax: Integer): Boolean;
begin
  Result := False;
  if nMax <> 0 then
    Result := (Round((nMin / nMax) * 100) < g_gnProtectPercent[9]) {or (nMax - nMin > 6000)};
end;

procedure LoadWMImagesLib();
var
  i: Integer;
begin
  g_wui :=CreateImages('Data\ui1.data', g_GameDevice);
  g_opui :=CreateImages('Data\NewopUI.data', g_GameDevice);
  g_WMainImages :=CreateImages(MAINIMAGEFILE, g_GameDevice);
  g_WMain2Images :=CreateImages(MAINIMAGEFILE2, g_GameDevice);
  g_WMain3Images :=CreateImages(MAINIMAGEFILE3, g_GameDevice);
  g_WChrSelImages :=CreateImages(CHRSELIMAGEFILE, g_GameDevice);
 // g_WBooksImages := CreateImages('Data\books.wzl', g_GameDevice);
 // g_WMainUibImages := CreateImages('Data\gui.data', g_GameDevice);
 // g_WMiniMapImages := CreateImages('Data\minimap.wzl', g_GameDevice);

  g_WMonImg :=CreateImages('Data\Mon1.data', g_GameDevice);
  g_WMon2Img :=CreateImages('Data\Mon2.data', g_GameDevice);
  g_WMon3Img :=CreateImages('Data\Mon3.data', g_GameDevice);
  g_WMon4Img :=CreateImages('Data\Mon4.data', g_GameDevice);
  g_WMon5Img :=CreateImages('Data\Mon5.data', g_GameDevice);
  g_WMon6Img :=CreateImages('Data\Mon6.data', g_GameDevice);
  g_WMon7Img :=CreateImages('Data\Mon7.data', g_GameDevice);
  g_WMon8Img :=CreateImages('Data\Mon8.data', g_GameDevice);
  g_WMon9Img :=CreateImages('Data\Mon9.data', g_GameDevice);
  g_WMon10Img :=CreateImages('Data\Mon10.data', g_GameDevice);
  g_WMon11Img :=CreateImages('Data\Mon11.data', g_GameDevice);
  g_WMon12Img :=CreateImages('Data\Mon12.data', g_GameDevice);
  g_WMon13Img :=CreateImages('Data\Mon13.data', g_GameDevice);
  g_WMon14Img :=CreateImages('Data\Mon14.data', g_GameDevice);
  g_WMon15Img :=CreateImages('Data\Mon15.data', g_GameDevice);
  g_WMon16Img :=CreateImages('Data\Mon16.data', g_GameDevice);
  g_WMon17Img :=CreateImages('Data\Mon17.data', g_GameDevice);
  g_WMon18Img :=CreateImages('Data\Mon18.data', g_GameDevice);
  g_WMon19Img :=CreateImages('Data\Mon19.data', g_GameDevice);
  g_WMon20Img :=CreateImages('Data\Mon20.data', g_GameDevice);
  g_WMon21Img :=CreateImages('Data\Mon21.data', g_GameDevice);
  g_WMon22Img :=CreateImages('Data\Mon22.data', g_GameDevice);
  g_WMon23Img :=CreateImages('Data\Mon23.data', g_GameDevice);
  g_WMon24Img :=CreateImages('Data\Mon24.data', g_GameDevice);
  g_WMon25Img :=CreateImages('Data\Mon25.data', g_GameDevice);
  g_WMon26Img :=CreateImages('Data\Mon26.data', g_GameDevice);
  g_WMon27Img :=CreateImages('Data\Mon27.data', g_GameDevice);
  g_WMon28Img :=CreateImages('Data\Mon28.data', g_GameDevice);
  g_WMon29Img :=CreateImages('Data\Mon29.data', g_GameDevice);
  g_WMon30Img :=CreateImages('Data\Mon30.data', g_GameDevice);
  g_WMon31Img :=CreateImages('Data\Mon31.data', g_GameDevice);
  g_WMon32Img :=CreateImages('Data\Mon32.data', g_GameDevice);
  g_WMon33Img :=CreateImages('Data\Mon33.data', g_GameDevice);
  g_WMon34Img :=CreateImages('Data\Mon34.data', g_GameDevice);
  g_WMon35Img :=CreateImages('Data\Mon35.data', g_GameDevice);
  g_WMon36Img :=CreateImages('Data\Mon36.data', g_GameDevice);
  g_WMon37Img :=CreateImages('Data\Mon37.data', g_GameDevice);
  g_WMon38Img :=CreateImages('Data\Mon38.data', g_GameDevice);
  g_WMon39Img :=CreateImages('Data\Mon39.data', g_GameDevice);
  g_WMon40Img :=CreateImages('Data\Mon40.data', g_GameDevice);

  g_WEffectImg :=CreateImages('Data\Effect.data', g_GameDevice);
  g_WDragonImg :=CreateImages('Data\Dragon.data', g_GameDevice);
  g_WSkeletonImg :=CreateImages('Data\Mon-kulou.data', g_GameDevice);

  g_cboEffect :=CreateImages(cboEffectFile, g_GameDevice);
    //g_cboEffect.m_Anti := True;
  g_cbohair :=CreateImages(cbohairFile, g_GameDevice);
  g_cbohum :=CreateImages(cbohumFile, g_GameDevice);
  g_cbohum3 :=CreateImages(cbohum3File, g_GameDevice);
  g_cboHumEffect :=CreateImages(cboHumEffectFile, g_GameDevice);
  g_cboHumEffect2 :=CreateImages(cboHumEffect2File, g_GameDevice);
  g_cboHumEffect3 :=CreateImages(cboHumEffect3File, g_GameDevice);

    //g_cboHumEffect.m_Anti := True;
  g_cboweapon :=CreateImages(cboweaponFile, g_GameDevice);
  g_cboweapon3 :=CreateImages(cboweapon3File, g_GameDevice);

  g_WMMapImages :=CreateImages(MINMAPIMAGEFILE, g_GameDevice);
    //InitWMImagesLibEx(g_WTilesImages := GetGameImagessFilePath + TITLESIMAGEFILE);
    //InitWMImagesLibEx(g_WSmTilesImages := GetGameImagessFilePath + SMLTITLESIMAGEFILE);
    //InitWMImagesLibEx(g_WTiles2Images := GetGameImagessFilePath + TITLESIMAGEFILE2);
    //InitWMImagesLibEx(g_WSmTiles2Images := GetGameImagessFilePath + SMLTITLESIMAGEFILE2);
  g_WHumWingImages :=CreateImages(HUMWINGIMAGESFILE, g_GameDevice);
  g_WHumEffect2 :=CreateImages(HUMWINGIMAGESFILE2, g_GameDevice);
  g_WHumEffect3 :=CreateImages(HUMWINGIMAGESFILE3, g_GameDevice);

  g_WBagItemImages :=CreateImages(BAGITEMIMAGESFILE, g_GameDevice);
    //if FileExists(BAGITEMIMAGESFILE2) then
  g_WBagItemImages2 :=CreateImages(BAGITEMIMAGESFILE2, g_GameDevice);
  g_WStateItemImages :=CreateImages(STATEITEMIMAGESFILE, g_GameDevice);
    //if FileExists(STATEITEMIMAGESFILE2) then
  g_WStateItemImages2 :=CreateImages(STATEITEMIMAGESFILE2, g_GameDevice);
  g_WDnItemImages :=CreateImages(DNITEMIMAGESFILE, g_GameDevice);
    //if FileExists(DNITEMIMAGESFILE2) then
  g_WDnItemImages2 :=CreateImages(DNITEMIMAGESFILE2, g_GameDevice);
  g_WHairImgImages :=CreateImages(HAIRIMGIMAGESFILE, g_GameDevice);
  g_WHair2ImgImages :=CreateImages(HAIR2IMGIMAGESFILE, g_GameDevice);

  g_WHumImgImages :=CreateImages(HUMIMGIMAGESFILE, g_GameDevice);
    //if FileExists(HUM2IMGIMAGESFILE) then
  g_WHum2ImgImages :=CreateImages(HUM2IMGIMAGESFILE, g_GameDevice);
    //if FileExists(HUM3IMGIMAGESFILE) then
  g_WHum3ImgImages :=CreateImages(HUM3IMGIMAGESFILE, g_GameDevice);

  g_WWeaponImages :=CreateImages(WEAPONIMAGESFILE, g_GameDevice);
    //if FileExists(WEAPON2IMAGESFILE) then
  g_WWeapon2Images :=CreateImages(WEAPON2IMAGESFILE, g_GameDevice);
    //if FileExists(WEAPON3IMAGESFILE) then
  g_WWeapon3Images :=CreateImages(WEAPON3IMAGESFILE, g_GameDevice);

  g_WMagIconImages :=CreateImages(MAGICONIMAGESFILE, g_GameDevice);
  g_WMagIcon2Images :=CreateImages(MAGICON2IMAGESFILE, g_GameDevice);

  g_WNpcImgImages :=CreateImages(NPCIMAGESFILE, g_GameDevice);

    //if FileExists(NPC2IMAGESFILE) then
  g_WNpc2ImgImages :=CreateImages(NPC2IMAGESFILE, g_GameDevice);

  g_WMagicImages :=CreateImages(MAGICIMAGESFILE, g_GameDevice);
    //g_WMagicImages.m_Anti := True;
  g_WMagic2Images :=CreateImages(MAGIC2IMAGESFILE, g_GameDevice);
    //g_WMagic2Images.m_Anti := True;
  g_WMagic3Images :=CreateImages(MAGIC3IMAGESFILE, g_GameDevice);
    //g_WMagic3Images.m_Anti := True;
  g_WMagic4Images :=CreateImages(MAGIC4IMAGESFILE, g_GameDevice);
    //g_WMagic4Images.m_Anti := True;
  g_WMagic5Images :=CreateImages(MAGIC5IMAGESFILE, g_GameDevice);
    //g_WMagic5Images.m_Anti := True;
  g_WMagic6Images :=CreateImages(MAGIC6IMAGESFILE, g_GameDevice);

  g_WMagic7Images :=CreateImages(MAGIC7IMAGESFILE, g_GameDevice);
  g_WMagic7Images2 :=CreateImages(MAGIC7IMAGESFILE2, g_GameDevice);

  g_WMagic8Images :=CreateImages(MAGIC8IMAGESFILE, g_GameDevice);
  g_WMagic8Images2 :=CreateImages(MAGIC8IMAGESFILE2, g_GameDevice);
  g_WMagic9Images :=CreateImages(MAGIC9IMAGESFILE, g_GameDevice);
  g_WMagic10Images :=CreateImages(MAGIC10IMAGESFILE, g_GameDevice);

  g_StateEffect :=CreateImages(STATEEFFECTFILE, g_GameDevice);
  g_ShineEffect :=CreateImages(SHINEEFFECTFILE, g_GameDevice);
  //g_Shineitem :=CreateImages(SHINEITEMFILE, g_GameDevice);
  g_ShineItems :=CreateImages(SHINEITEMSFILE, g_GameDevice);
  g_ShineDnItems :=CreateImages(SHINEDNITEMSFILE, g_GameDevice);
  //新加补丁读取
  g_WAniTilesArr :=CreateImages(ANITILESIMAGEFILE, g_GameDevice);

{$IF CUSTOMLIBFILE = 1}
  g_WEventEffectImages := CreateImages(EVENTEFFECTIMAGESFILE, g_GameDevice);
{$IFEND}
  FillChar(g_WObjectArr, SizeOf(g_WObjectArr), 0);
  FillChar(g_WTilesArr, SizeOf(g_WTilesArr), 0);
  FillChar(g_WSmTilesArr, SizeOf(g_WSmTilesArr), 0);


  //FillChar(g_WMonImagesArr, SizeOf(g_WMonImagesArr), 0);
end;

procedure InitWMImagesLibSecond;
var
  dwExitCode: DWORD;
begin
//  InitWMImagesLib(frmMain.DXDraw, False);
  //ExitThread(dwExitCode);
end;

//procedure InitWMImagesLibEx(WImages: TWMImages; DDxDraw: TDXDraw; FileName: string);
//begin
//  WImages.DXDraw := DDxDraw;
//  WImages.DDraw := DDxDraw.DDraw;
//  WImages.FileName := FileName;
//  WImages.LibType := ltUseCache;
//  WImages.Initialize;
//end;

procedure InitWMImagesLib(bFirst: Boolean);
var
  sFileName: string;
  i: Integer;
begin
  if bFirst then begin
    g_WMainImages.Initialize;
    g_WMain2Images.Initialize;
    g_WMain3Images.Initialize;
    g_WChrSelImages.Initialize;

   // g_WMainUibImages.Initialize;
   // g_WBooksImages.Initialize;
  //  g_WMiniMapImages.Initialize;

    g_opui.Initialize;
    g_wui.Initialize;
  end else begin
    g_WMonImg.Initialize;
    g_WMon2Img.Initialize;
    g_WMon3Img.Initialize;
    g_WMon4Img.Initialize;
    g_WMon5Img.Initialize;
    g_WMon6Img.Initialize;
    g_WMon7Img.Initialize;
    g_WMon8Img.Initialize;
    g_WMon9Img.Initialize;
    g_WMon10Img.Initialize;
    g_WMon11Img.Initialize;
    g_WMon12Img.Initialize;
    g_WMon13Img.Initialize;
    g_WMon14Img.Initialize;
    g_WMon15Img.Initialize;
    g_WMon16Img.Initialize;
    g_WMon16Img.Appr := 12;

    g_WMon17Img.Initialize;

    g_WMon18Img.Initialize;
    g_WMon18Img.Appr := 13;

    g_WMon19Img.Initialize;
    g_WMon19Img.Appr := 14;

    g_WMon20Img.Initialize;
    g_WMon20Img.Appr := 15;

    g_WMon21Img.Initialize;
    g_WMon21Img.Appr := 16;

    g_WMon22Img.Initialize;
    g_WMon22Img.Appr := 17;

    g_WMon23Img.Initialize;
    g_WMon23Img.Appr := 18;

    g_WMon24Img.Initialize;
    g_WMon24Img.Appr := 19;

    g_WMon25Img.Initialize;
    g_WMon25Img.Appr := 20;

    g_WMon26Img.Initialize;
    g_WMon26Img.Appr := 21;

    g_WMon27Img.Initialize;
    g_WMon27Img.Appr := 22;

    g_WMon28Img.Initialize;
    g_WMon29Img.Initialize;
    g_WMon30Img.Initialize;
    g_WMon31Img.Initialize;
    g_WMon32Img.Initialize;
    g_WMon33Img.Initialize;
    g_WMon34Img.Initialize;
    g_WMon35Img.Initialize;
    g_WMon36Img.Initialize;
    g_WMon37Img.Initialize;
    g_WMon38Img.Initialize;
    g_WMon39Img.Initialize;
    g_WMon40Img.Initialize;

    g_WEffectImg.Initialize;
    g_WEffectImg.Appr := 1;
    g_WDragonImg.Initialize;
    g_WSkeletonImg.Initialize;

    g_cboEffect.Initialize;
    g_cboEffect.Appr := 2;

    g_cbohair.Initialize;
    g_cbohum.Initialize;
    g_cbohum3.Initialize;

    g_cboHumEffect.Initialize;
    g_cboHumEffect.Appr := 3;
    g_cboHumEffect2.Initialize;
    g_cboHumEffect3.Initialize;

    g_cboweapon.Initialize;
    g_cboweapon3.Initialize;

    g_WMMapImages.Initialize;
    //g_WTilesImages.Initialize;
    //g_WSmTilesImages.Initialize;
    //g_WTiles2Images.Initialize;
    //g_WSmTiles2Images.Initialize;
    g_WHumWingImages.Initialize;
    g_WHumWingImages.Appr := 4;
    g_WHumEffect2.Initialize;
    g_WHumEffect2.Appr := 11;
    g_WHumEffect3.Initialize;

    g_WBagItemImages.Initialize;
    g_WStateItemImages.Initialize;
    g_WDnItemImages.Initialize;
    g_WBagItemImages2.Initialize;
    g_WStateItemImages2.Initialize;
    g_WDnItemImages2.Initialize;

    g_WHumImgImages.Initialize;
    g_WHum2ImgImages.Initialize;
    g_WHum3ImgImages.Initialize;
    g_WHairImgImages.Initialize;
    g_WHair2ImgImages.Initialize;
    g_WWeaponImages.Initialize;
    g_WWeapon2Images.Initialize;
    g_WWeapon3Images.Initialize;
    g_WMagIconImages.Initialize;
    g_WMagIcon2Images.Initialize;
    g_WNpcImgImages.Initialize;
    g_WNpc2ImgImages.Initialize;

    g_WMagicImages.Initialize;
    g_WMagicImages.Appr := 5;
    g_WMagic2Images.Initialize;
    g_WMagic2Images.Appr := 6;
    g_WMagic3Images.Initialize;
    g_WMagic3Images.Appr := 7;
    g_WMagic4Images.Initialize;
    g_WMagic4Images.Appr := 8;
    g_WMagic5Images.Initialize;
    g_WMagic5Images.Appr := 9;
    g_WMagic6Images.Initialize;
    g_WMagic6Images.Appr := 10;

    g_WMagic7Images.Initialize;
    g_WMagic7Images2.Initialize;
    g_WMagic8Images.Initialize;
    g_WMagic8Images2.Initialize;
    g_WMagic9Images.Initialize;
    g_WMagic10Images.Initialize;

    g_StateEffect.Initialize;
    g_StateEffect.Appr := 25;
    g_ShineEffect.Initialize;
    //g_Shineitem.Initialize;
    g_ShineItems.Initialize;
    g_ShineDnItems.Initialize;
    //新加补丁
    g_WAniTilesArr.Initialize;

{$IF CUSTOMLIBFILE = 1}
    g_WEventEffectImages.Initialize;
{$IFEND}
  end;
end;

procedure UnLoadWMImagesLib();
var
  i: Integer;
begin
  for i := Low(g_WTilesArr) to High(g_WTilesArr) do begin
    if g_WTilesArr[i] <> nil then begin
      g_WTilesArr[i].Finalize;
      g_WTilesArr[i].Free;
    end;
  end;

  for i := Low(g_WSmTilesArr) to High(g_WSmTilesArr) do begin
    if g_WSmTilesArr[i] <> nil then begin
      g_WSmTilesArr[i].Finalize;
      g_WSmTilesArr[i].Free;
    end;
  end;

  for i := Low(g_WObjectArr) to High(g_WObjectArr) do begin
    if g_WObjectArr[i] <> nil then begin
      g_WObjectArr[i].Finalize;
      g_WObjectArr[i].Free;
    end;
  end;
  {for i := Low(g_WMonImagesArr) to High(g_WMonImagesArr) do begin
    if g_WMonImagesArr[i] <> nil then begin
      g_WMonImagesArr[i].Finalize;
      g_WMonImagesArr[i].Free;
    end;
  end;}
  try
    g_WMainImages.Finalize;
    g_WMainImages.Free;
    g_WMain2Images.Finalize;
    g_WMain2Images.Free;
    g_WMain3Images.Finalize;
    g_WMain3Images.Free;
    g_WChrSelImages.Finalize;
    g_WChrSelImages.Free;

    g_opui.Finalize;
    g_opui.Free;
    g_wui.Finalize;
    g_wui.Free;
    g_WMonImg.Finalize;
    g_WMonImg.Free;
    g_WMon2Img.Finalize;
    g_WMon2Img.Free;
    g_WMon3Img.Finalize;
    g_WMon3Img.Free;
    g_WMon4Img.Finalize;
    g_WMon4Img.Free;
    g_WMon5Img.Finalize;
    g_WMon5Img.Free;
    g_WMon6Img.Finalize;
    g_WMon6Img.Free;
    g_WMon7Img.Finalize;
    g_WMon7Img.Free;
    g_WMon8Img.Finalize;
    g_WMon8Img.Free;
    g_WMon9Img.Finalize;
    g_WMon9Img.Free;
    g_WMon10Img.Finalize;
    g_WMon10Img.Free;
    g_WMon11Img.Finalize;
    g_WMon11Img.Free;
    g_WMon12Img.Finalize;
    g_WMon12Img.Free;
    g_WMon13Img.Finalize;
    g_WMon13Img.Free;
    g_WMon14Img.Finalize;
    g_WMon14Img.Free;
    g_WMon15Img.Finalize;
    g_WMon15Img.Free;
    g_WMon16Img.Finalize;
    g_WMon16Img.Free;
    g_WMon17Img.Finalize;
    g_WMon17Img.Free;
    g_WMon18Img.Finalize;
    g_WMon18Img.Free;
    g_WMon19Img.Finalize;
    g_WMon19Img.Free;
    g_WMon20Img.Finalize;
    g_WMon20Img.Free;
    g_WMon21Img.Finalize;
    g_WMon21Img.Free;
    g_WMon22Img.Finalize;
    g_WMon22Img.Free;
    g_WMon23Img.Finalize;
    g_WMon23Img.Free;
    g_WMon24Img.Finalize;
    g_WMon24Img.Free;
    g_WMon25Img.Finalize;
    g_WMon25Img.Free;
    g_WMon26Img.Finalize;
    g_WMon26Img.Free;
    g_WMon27Img.Finalize;
    g_WMon27Img.Free;
    g_WMon28Img.Finalize;
    g_WMon28Img.Free;
    g_WMon29Img.Finalize;
    g_WMon29Img.Free;
    g_WMon30Img.Finalize;
    g_WMon30Img.Free;

    g_WEffectImg.Finalize;
    g_WEffectImg.Free;
    g_WDragonImg.Finalize;
    g_WDragonImg.Free;
    g_WSkeletonImg.Finalize;
    g_WSkeletonImg.Free;

    g_cboEffect.Finalize;
    g_cbohair.Finalize;
    g_cbohum.Finalize;
    g_cbohum3.Finalize;
    g_cboHumEffect.Finalize;
    g_cboHumEffect2.Finalize;
    g_cboHumEffect3.Finalize;
    g_cboweapon.Finalize;
    g_cboweapon3.Finalize;
    g_cboEffect.Free;
    g_cbohair.Free;
    g_cbohum.Free;
    g_cbohum3.Free;
    g_cboHumEffect.Free;
    g_cboHumEffect2.Free;
    g_cboHumEffect3.Free;
    g_cboweapon.Free;
    g_cboweapon3.Free;

    g_WMMapImages.Finalize;
    g_WMMapImages.Free;
    //g_WTilesImages.Finalize;
    //g_WTilesImages.Free;
    //g_WSmTilesImages.Finalize;
    //g_WSmTilesImages.Free;
    //g_WTiles2Images.Finalize;
    //g_WTiles2Images.Free;
    //g_WSmTiles2Images.Finalize;
    //g_WSmTiles2Images.Free;
    g_WHumWingImages.Finalize;
    g_WHumWingImages.Free;

    g_WHumEffect2.Finalize;
    g_WHumEffect2.Free;
    g_WHumEffect3.Finalize;
    g_WHumEffect3.Free;

    g_WBagItemImages.Finalize;
    g_WBagItemImages.Free;
    g_WStateItemImages.Finalize;
    g_WStateItemImages.Free;
    g_WDnItemImages.Finalize;
    g_WDnItemImages.Free;

    g_WBagItemImages2.Finalize;
    g_WBagItemImages2.Free;
    g_WStateItemImages2.Finalize;
    g_WStateItemImages2.Free;
    g_WDnItemImages2.Finalize;
    g_WDnItemImages2.Free;

    g_WHumImgImages.Finalize;
    g_WHumImgImages.Free;

    if g_WHum2ImgImages <> nil then begin
      g_WHum2ImgImages.Finalize;
      g_WHum2ImgImages.Free;
    end;

    if g_WHum3ImgImages <> nil then begin
      g_WHum3ImgImages.Finalize;
      g_WHum3ImgImages.Free;
    end;

    g_WHairImgImages.Finalize;
    g_WHairImgImages.Free;

    g_WHair2ImgImages.Finalize;
    g_WHair2ImgImages.Free;

    g_WWeaponImages.Finalize;
    g_WWeaponImages.Free;

    if g_WWeapon2Images <> nil then begin
      g_WWeapon2Images.Finalize;
      g_WWeapon2Images.Free;
    end;

    if g_WWeapon3Images <> nil then begin
      g_WWeapon3Images.Finalize;
      g_WWeapon3Images.Free;
    end;

    g_WMagIconImages.Finalize;
    g_WMagIconImages.Free;
    g_WMagIcon2Images.Finalize;
    g_WMagIcon2Images.Free;

    g_WNpcImgImages.Finalize;
    g_WNpcImgImages.Free;

    g_WNpc2ImgImages.Finalize;
    g_WNpc2ImgImages.Free;

    g_WMagicImages.Finalize;
    g_WMagicImages.Free;
    g_WMagic2Images.Finalize;
    g_WMagic2Images.Free;
    g_WMagic3Images.Finalize;
    g_WMagic3Images.Free;
    g_WMagic4Images.Finalize;
    g_WMagic4Images.Free;
    g_WMagic5Images.Finalize;
    g_WMagic5Images.Free;
    g_WMagic6Images.Finalize;
    g_WMagic6Images.Free;
    g_WMagic7Images.Finalize;
    g_WMagic7Images.Free;
    g_WMagic7Images2.Finalize;
    g_WMagic7Images2.Free;
    g_WMagic8Images.Finalize;
    g_WMagic8Images.Free;
    g_WMagic8Images2.Finalize;
    g_WMagic8Images2.Free;
    g_WMagic9Images.Finalize;
    g_WMagic9Images.Free;
    g_WMagic10Images.Finalize;
    g_WMagic10Images.Free;

    g_StateEffect.Finalize;
    g_StateEffect.Free;
    g_ShineEffect.Finalize;
    g_ShineEffect.Free;
    //g_Shineitem.Finalize;
    //g_Shineitem.Free;
    g_ShineItems.Finalize;
    g_ShineItems.Free;
    g_ShineDnItems.Finalize;
    g_ShineDnItems.Free;
   // g_WBooksImages.Finalize;
   // g_WBooksImages.Free;
   // g_WMainUibImages.Finalize;
   // g_WMainUibImages.Free;
   // g_WMiniMapImages.Finalize;
  //  g_WMiniMapImages.Free;

     //新加补丁读取
    g_WAniTilesArr.Finalize;
    g_WAniTilesArr.Free;

{$IF CUSTOMLIBFILE = 1}
    g_WEventEffectImages.Finalize;
    g_WEventEffectImages.Free;
{$IFEND}
  except

  end;
end;

procedure LoadLightImages();
var
  I: integer;
  Png: TPngImage;
  AResource: TResourceStream;
begin
  for I := 0 to 5 do begin
    AResource := TResourceStream.Create(HInstance, 'PngImage'+IntToStr(I), RT_RCDATA);
    g_LightImages[I] := PXL.Providers.TGraphicsDeviceProvider(g_GameDevice.Provider).CreateLockableTexture(g_GameDevice, False);

    Png := TPngImage.Create;
    try
      Png.LoadFromStream(AResource);
      g_LightImages[I].LoadFromPng(Png);
    finally
      Png.Free;
      AResource.Free;
    end;
  end;
end;

procedure UnLoadLightImages();
var
  I: integer;
begin
  for I := 0 to 5 do begin
    if g_LightImages[I] <> nil then begin
      try
        g_LightImages[I].Free;
      finally
        g_LightImages[I] := nil;
      end;
    end;
  end;
end;

function GetObjs(nUnit, nIdx: Integer): TCustomLockableTexture; //取地图图库
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WObjectArr)..High(g_WObjectArr)]) then nUnit := 0;
  if g_WObjectArr[nUnit] = nil then begin
    if nUnit = 0 then
      sFileName := OBJECTIMAGEFILE
    else
      sFileName := Format(OBJECTIMAGEFILE1, [nUnit + 1]);

    g_WObjectArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WObjectArr[nUnit].Initialize;
  end;
  Result := g_WObjectArr[nUnit].Images[nIdx];
end;

function GetObjsEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture; //取地图图库
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WObjectArr)..High(g_WObjectArr)]) then nUnit := 0;
  if g_WObjectArr[nUnit] = nil then begin

    if nUnit = 0 then sFileName := OBJECTIMAGEFILE
    else sFileName := Format(OBJECTIMAGEFILE1, [nUnit + 1]);

    g_WObjectArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WObjectArr[nUnit].Initialize;
  end;
  Result := g_WObjectArr[nUnit].GetCachedImage(nIdx, px, py);
end;

function GetTiles(nUnit, nIdx: Integer): TCustomLockableTexture;
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WTilesArr)..High(g_WTilesArr)]) then
    nUnit := 0;

  if g_WTilesArr[nUnit] = nil then begin
    if nUnit = 0 then
      sFileName := TITLESIMAGEFILE
    else
      sFileName := Format(TITLESIMAGEFILEX, [nUnit + 1]);

    g_WTilesArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WTilesArr[nUnit].Initialize;
  end;
  Result := g_WTilesArr[nUnit].Images[nIdx];
end;

function GetTilesEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture;
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WTilesArr)..High(g_WTilesArr)]) then
    nUnit := 0;

  if g_WTilesArr[nUnit] = nil then begin
    if nUnit = 0 then
      sFileName := TITLESIMAGEFILE
    else
      sFileName := Format(TITLESIMAGEFILEX, [nUnit + 1]);

    g_WTilesArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WTilesArr[nUnit].Initialize;
  end;
  Result := g_WTilesArr[nUnit].GetCachedImage(nIdx, px, py);
end;

function GetSmTiles(nUnit, nIdx: Integer): TCustomLockableTexture;
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WSmTilesArr)..High(g_WSmTilesArr)]) then
    nUnit := 0;

  if g_WSmTilesArr[nUnit] = nil then begin
    if nUnit = 0 then
      sFileName := SMLTITLESIMAGEFILE
    else
      sFileName := Format(SMLTITLESIMAGEFILEX, [nUnit + 1]);

    g_WSmTilesArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WSmTilesArr[nUnit].Initialize;
  end;
  Result := g_WSmTilesArr[nUnit].Images[nIdx];
end;

function GetSmTilesEx(nUnit, nIdx: Integer; var px, py: Integer): TCustomLockableTexture;
var
  sFileName: string;
begin
  Result := nil;
  if not (nUnit in [Low(g_WSmTilesArr)..High(g_WSmTilesArr)]) then
    nUnit := 0;

  if g_WSmTilesArr[nUnit] = nil then begin
    if nUnit = 0 then
      sFileName := SMLTITLESIMAGEFILE
    else
      sFileName := Format(SMLTITLESIMAGEFILEX, [nUnit + 1]);
    g_WSmTilesArr[nUnit] := CreateImages(sFileName, g_GameDevice);
    g_WSmTilesArr[nUnit].Initialize;
  end;
  Result := g_WSmTilesArr[nUnit].GetCachedImage(nIdx, px, py);
end;

function GetMonImg(nAppr: Integer): TWMImages;
var
  WMImage: TWMImages;
begin
  Result := g_WMonImg;
  if nAppr < 1000 then begin
    case (nAppr div 10) of
      0: Result := g_WMonImg;
      1: Result := g_WMon2Img;
      2: Result := g_WMon3Img;
      3: Result := g_WMon4Img;
      4: Result := g_WMon5Img;
      5: Result := g_WMon6Img;
      6: Result := g_WMon7Img;
      7: Result := g_WMon8Img;
      8: Result := g_WMon9Img;
      9: Result := g_WMon10Img;
      10: Result := g_WMon11Img;
      11: Result := g_WMon12Img;
      12: Result := g_WMon13Img;
      13: Result := g_WMon14Img;
      14: Result := g_WMon15Img;
      15: Result := g_WMon16Img;
      16: Result := g_WMon17Img;
      17: Result := g_WMon18Img;
      18: Result := g_WMon19Img;
      19: Result := g_WMon20Img;
      20: Result := g_WMon21Img;
      21: Result := g_WMon22Img;
      22: Result := g_WMon23Img;
      23: Result := g_WMon24Img;
      24: Result := g_WMon25Img;
      25: Result := g_WMon26Img;
      26: Result := g_WMon27Img;
      27: Result := g_WMon28Img;
      28: Result := g_WMon29Img;
      29: Result := g_WMon30Img;
      30: Result := g_WMon31Img;
      31: Result := g_WMon32Img;
      32: Result := g_WMon33Img;
      33: Result := g_WMon34Img;
      34: Result := g_WMon35Img;
      35: Result := g_WMon36Img;
      36: Result := g_WMon37Img;
      37: Result := g_WMon38Img;
      38: Result := g_WMon39Img;
      39: Result := g_WMon40Img;

      70: begin
          case (nAppr mod 100) of
            0..2: Result := g_WSkeletonImg;
          else
            Result := g_WMon28Img;
          end;
        end;
      80: Result := g_WDragonImg;
      81: Result := g_WMon36Img;
      82: Result := g_WMon36Img;
      90: begin
          case nAppr of
            904: Result := g_WMon34Img;
            905: Result := g_WMon34Img;
            906: Result := g_WMon34Img;
          else
            Result := g_WEffectImg;
          end;
        end;
    end;
  end; // else Result := GetMonImgEx(nAppr);
end;

function GetMonAction(nAppr: Integer): pTMonsterAction;
var
  FileStream: TFileStream;
  sFileName: string;
  MonsterAction: TMonsterAction;
begin
  Result := nil;
  sFileName := Format(MONPMFILE, [nAppr]);
  if FileExists(sFileName) then begin
    FileStream := TFileStream.Create(sFileName, fmOpenRead or fmShareDenyNone);
    FileStream.Read(MonsterAction, SizeOf(MonsterAction));
    New(Result);
    Result^ := MonsterAction;
    FileStream.Free;
  end;
end;

//取得职业名称
//0 武士
//1 魔法师
//2 道士

function GetJobName(nJob: Integer): string;
begin
  Result := '';
  case nJob of
    0: Result := g_sWarriorName;
    1: Result := g_sWizardName;
    2: Result := g_sTaoistName;
  else begin
      Result := g_sUnKnowName;
    end;
  end;
end;

function GetItemType(ItemType: TItemType): string;
begin
  case ItemType of //
    i_HPDurg: Result := '金创药';
    i_MPDurg: Result := '魔法药';
    i_HPMPDurg: Result := '高级药';
    i_OtherDurg: Result := '其它药品';
    i_Weapon: Result := '武器';
    i_Dress: Result := '衣服';
    i_Helmet: Result := '头盔';
    i_Necklace: Result := '项链';
    i_Armring: Result := '手镯';
    i_Ring: Result := '戒指';
    i_Belt: Result := '腰带';
    i_Boots: Result := '鞋子';
    i_Charm: Result := '宝石';
    i_Book: Result := '技能书';
    i_PosionDurg: Result := '毒药';
    i_UseItem: Result := '消耗品';
    i_Scroll: Result := '卷类';
    i_Stone: Result := '矿石';
    i_Gold: Result := '金币';
    i_Other: Result := '其它';
  end;
end;

function GetItemShowFilter(sItemName: string): Boolean;
var
  i: Integer;
begin
  Result := g_ShowItemList.IndexOf(sItemName) > -1;
end;

procedure LoadUserConfig(sUserName: string);
var
  ShowItem: pTShowItem;
  ini, ini2: TIniFile;
  sFileName: string;
  Strings: TStringList;
  i, no: Integer;
  sItemName, sLineText: string;
  sType, sPickup, sShow, sFColor, sBColor: string;
  sn, so: string;
begin
  sFileName := '.\Config\' + g_sServerName + '.' + sUserName + '.Set';

  ini := TIniFile.Create(sFileName);
  //base
  g_gcGeneral[0] := ini.ReadBool('Basic', 'ShowActorName', g_gcGeneral[0]);
  g_gcGeneral[1] := ini.ReadBool('Basic', 'DuraWarning', g_gcGeneral[1]);
  g_gcGeneral[2] := ini.ReadBool('Basic', 'AutoAttack', g_gcGeneral[2]);
  g_gcGeneral[3] := ini.ReadBool('Basic', 'ShowExpFilter', g_gcGeneral[3]);
  g_MaxExpFilter := ini.ReadInteger('Basic', 'ShowExpFilterMax', g_MaxExpFilter);
  g_gcGeneral[4] := ini.ReadBool('Basic', 'ShowDropItems', g_gcGeneral[4]);
  g_gcGeneral[5] := ini.ReadBool('Basic', 'ShowDropItemsFilter', g_gcGeneral[5]);
  g_gcGeneral[6] := ini.ReadBool('Basic', 'ShowHumanWing', g_gcGeneral[6]);

  g_boAutoPickUp := ini.ReadBool('Basic', 'AutoPickUp', g_boAutoPickUp);
  g_gcGeneral[7] := ini.ReadBool('Basic', 'AutoPickUpFilter', g_gcGeneral[7]);
  g_boPickUpAll := ini.ReadBool('Basic', 'PickUpAllItem', g_boPickUpAll);

  g_gcGeneral[8] := ini.ReadBool('Basic', 'HideDeathBody', g_gcGeneral[8]);
  g_gcGeneral[9] := ini.ReadBool('Basic', 'AutoFixItem', g_gcGeneral[9]);
  g_gcGeneral[10] := ini.ReadBool('Basic', 'ShakeScreen', g_gcGeneral[10]);
  g_gcGeneral[13] := ini.ReadBool('Basic', 'StruckShow', g_gcGeneral[13]);
  g_gcGeneral[15] := ini.ReadBool('Basic', 'HideStruck', g_gcGeneral[15]);

  g_gcGeneral[14] := ini.ReadBool('Basic', 'CompareItems', g_gcGeneral[14]);

  //g_gcGeneral[12] := ini.ReadBool('Basic', 'DayLight', g_gcGeneral[12]);

  ini2 := TIniFile.Create('.\lscfg.ini');
  g_gcGeneral[11] := ini2.ReadBool('Setup', 'EffectSound', g_gcGeneral[11]);
  g_SndMgr.Silent := not g_gcGeneral[11];

  g_gcGeneral[12] := ini2.ReadBool('Setup', 'EffectBKGSound', g_gcGeneral[12]);
  //g_boBGSound := g_boSound;
  g_lWavMaxVol := ini2.ReadInteger('Setup', 'EffectSoundLevel', g_lWavMaxVol);
  g_SndMgr.Volume := Round(g_lWavMaxVol / 68 * 100);
  ini2.Free;

  g_HitSpeedRate := ini.ReadInteger('Basic', 'HitSpeedRate', g_HitSpeedRate);
  g_MagSpeedRate := ini.ReadInteger('Basic', 'MagSpeedRate', g_MagSpeedRate);
  g_MoveSpeedRate := ini.ReadInteger('Basic', 'MoveSpeedRate', g_MoveSpeedRate);

  //Protect
  g_gcProtect[0] := ini.ReadBool('Protect', 'RenewHPIsAuto', g_gcProtect[0]);
  g_gcProtect[1] := ini.ReadBool('Protect', 'RenewMPIsAuto', g_gcProtect[1]);
  g_gcProtect[3] := ini.ReadBool('Protect', 'RenewSpecialIsAuto', g_gcProtect[3]);
  g_gcProtect[5] := ini.ReadBool('Protect', 'RenewBookIsAuto', g_gcProtect[5]);
  g_gcProtect[7] := ini.ReadBool('Protect', 'HeroRenewHPIsAuto', g_gcProtect[7]);
  g_gcProtect[8] := ini.ReadBool('Protect', 'HeroRenewMPIsAuto', g_gcProtect[8]);
  g_gcProtect[9] := ini.ReadBool('Protect', 'HeroRenewSpecialIsAuto', g_gcProtect[9]);
  g_gcProtect[10] := ini.ReadBool('Protect', 'HeroSidestep', g_gcProtect[10]);
  g_gcProtect[11] := ini.ReadBool('Protect', 'RenewSpecialIsAuto_MP', g_gcProtect[11]);

  g_gnProtectTime[0] := ini.ReadInteger('Protect', 'RenewHPTime', g_gnProtectTime[0]);
  g_gnProtectTime[1] := ini.ReadInteger('Protect', 'RenewMPTime', g_gnProtectTime[1]);
  g_gnProtectTime[3] := ini.ReadInteger('Protect', 'RenewSpecialTime', g_gnProtectTime[3]);
  g_gnProtectTime[5] := ini.ReadInteger('Protect', 'RenewBookTime', g_gnProtectTime[5]);
  g_gnProtectTime[7] := ini.ReadInteger('Protect', 'HeroRenewHPTime', g_gnProtectTime[7]);
  g_gnProtectTime[8] := ini.ReadInteger('Protect', 'HeroRenewMPTime', g_gnProtectTime[8]);
  g_gnProtectTime[9] := ini.ReadInteger('Protect', 'HeroRenewSpecialTime', g_gnProtectTime[9]);
  g_gnProtectPercent[0] := ini.ReadInteger('Protect', 'RenewHPPercent', g_gnProtectPercent[0]);
  g_gnProtectPercent[1] := ini.ReadInteger('Protect', 'RenewMPPercent', g_gnProtectPercent[1]);
  g_gnProtectPercent[3] := ini.ReadInteger('Protect', 'RenewSpecialPercent', g_gnProtectPercent[3]);
  g_gnProtectPercent[7] := ini.ReadInteger('Protect', 'HeroRenewHPPercent', g_gnProtectPercent[7]);
  g_gnProtectPercent[8] := ini.ReadInteger('Protect', 'HeroRenewMPPercent', g_gnProtectPercent[8]);
  g_gnProtectPercent[9] := ini.ReadInteger('Protect', 'HeroRenewSpecialPercent', g_gnProtectPercent[9]);
  g_gnProtectPercent[10] := ini.ReadInteger('Protect', 'HeroPerSidestep', g_gnProtectPercent[10]);
  g_gnProtectPercent[5] := ini.ReadInteger('Protect', 'RenewBookPercent', g_gnProtectPercent[5]);
  g_gnProtectPercent[6] := ini.ReadInteger('Protect', 'RenewBookNowBookIndex', g_gnProtectPercent[6]);
  frmMain.SendClientMessage(CM_HEROSIDESTEP, MakeLong(Integer(g_gcProtect[10]), g_gnProtectPercent[10]), 0, 0, 0);

  //
  g_gcTec[0] := ini.ReadBool('Tec', 'SmartLongHit', g_gcTec[0]);
  g_gcTec[10] := ini.ReadBool('Tec', 'SmartLongHit2', g_gcTec[10]);
  g_gcTec[11] := ini.ReadBool('Tec', 'SmartSLongHit', g_gcTec[11]);
  g_gcTec[1] := ini.ReadBool('Tec', 'SmartWideHit', g_gcTec[1]);
  g_gcTec[2] := ini.ReadBool('Tec', 'SmartFireHit', g_gcTec[2]);
  g_gcTec[3] := ini.ReadBool('Tec', 'SmartPureHit', g_gcTec[3]);
  g_gcTec[4] := ini.ReadBool('Tec', 'SmartShield', g_gcTec[4]);
  g_gcTec[5] := ini.ReadBool('Tec', 'SmartShieldHero', g_gcTec[5]);
  g_gcTec[6] := ini.ReadBool('Tec', 'SmartTransparence', g_gcTec[6]);
  g_gcTec[9] := ini.ReadBool('Tec', 'SmartThunderHit', g_gcTec[9]);

  g_gcTec[7] := ini.ReadBool('AutoPractice', 'PracticeIsAuto', g_gcTec[7]);
  g_gnTecTime[8] := ini.ReadInteger('AutoPractice', 'PracticeTime', g_gnTecTime[8]);
  g_gnTecPracticeKey := ini.ReadInteger('AutoPractice', 'PracticeKey', g_gnTecPracticeKey);

  g_gcTec[12] := ini.ReadBool('Tec', 'HeroSeriesSkillFilter', g_gcTec[12]);
  g_gcTec[13] := ini.ReadBool('Tec', 'SLongHit', g_gcTec[13]);
  g_gcTec[14] := ini.ReadBool('Tec', 'SmartGoMagic', g_gcTec[14]);
  frmMain.SendClientMessage(CM_HEROSERIESSKILLCONFIG, MakeLong(Integer(g_gcTec[12]), 0), 0, 0, 0);

  //
  g_gcHotkey[0] := ini.ReadBool('Hotkey', 'UseHotkey', g_gcHotkey[0]);
  FrmDlg.DEHeroCallHero.SetOfHotKey(ini.ReadInteger('Hotkey', 'HeroCallHero', 0));
  FrmDlg.DEHeroSetAttackState.SetOfHotKey(ini.ReadInteger('Hotkey', 'HeroSetAttackState', 0));
  FrmDlg.DEHeroSetGuard.SetOfHotKey(ini.ReadInteger('Hotkey', 'HeroSetGuard', 0));
  FrmDlg.DEHeroSetTarget.SetOfHotKey(ini.ReadInteger('Hotkey', 'HeroSetTarget', 0));
  FrmDlg.DEHeroUnionHit.SetOfHotKey(ini.ReadInteger('Hotkey', 'HeroUnionHit', 0));
  FrmDlg.DESwitchAttackMode.SetOfHotKey(ini.ReadInteger('Hotkey', 'SwitchAttackMode', 0));
  FrmDlg.DESwitchMiniMap.SetOfHotKey(ini.ReadInteger('Hotkey', 'SwitchMiniMap', 0));
  FrmDlg.DxEditSSkill.SetOfHotKey(ini.ReadInteger('Hotkey', 'SerieSkill', 0));

  g_ShowItemList.LoadFromFile('.\Data\Filter.dat');

  //============================================================================
  //g_gcAss[0] := ini.ReadBool('Ass', '0', g_gcAss[0]);
  g_gcAss[1] := ini.ReadBool('Ass', '1', g_gcAss[1]);
  g_gcAss[2] := ini.ReadBool('Ass', '2', g_gcAss[2]);
  g_gcAss[3] := ini.ReadBool('Ass', '3', g_gcAss[3]);
  g_gcAss[4] := ini.ReadBool('Ass', '4', g_gcAss[4]);
  g_gcAss[5] := ini.ReadBool('Ass', '5', g_gcAss[5]);
  g_gcAss[6] := ini.ReadBool('Ass', '6', g_gcAss[6]);

  g_APPickUpList.Clear;
  g_APMobList.Clear;

  Strings := TStringList.Create;
  if g_gcAss[5] then begin
    sFileName := '.\Config\' + g_sServerName + '.' + sUserName + '.ItemFilterEx.txt';
    if FileExists(sFileName) then
      Strings.LoadFromFile(sFileName)
    else
      Strings.SaveToFile(sFileName);

    for i := 0 to Strings.Count - 1 do begin
      if (Strings[i] = '') or (Strings[i][1] = ';') then Continue;
      so := GetValidStr3(Strings[i], sn, [',', ' ', #9]);
      no := Integer(so <> '');
      g_APPickUpList.AddObject(sn, TObject(no));
    end;
  end;

  if g_gcAss[6] then begin
    sFileName := '.\Config\' + g_sServerName + '.' + sUserName + '.MonFilter.txt';
    if FileExists(sFileName) then
      Strings.LoadFromFile(sFileName)
    else
      Strings.SaveToFile(sFileName);

    for i := 0 to Strings.Count - 1 do begin
      if (Strings[i] = '') or (Strings[i][1] = ';') then Continue;
      g_APMobList.Add(Strings[i] {, nil});
    end;
  end;
  Strings.Free;

  ini.Free;

end;

procedure LoadItemDesc();
const
  fItemDesc = '.\data\ItemDesc.dat';
var
  i: Integer;
  Name, desc: string;
  ps: PString;
  temp: TStringList;
begin
  //g_ItemDesc
  if FileExists(fItemDesc) then begin
    temp := TStringList.Create;
    temp.LoadFromFile(fItemDesc);
    for i := 0 to temp.Count - 1 do begin
      if temp[i] = '' then Continue;
      desc := GetValidStr3(temp[i], Name, ['=']);
      desc := StringReplace(desc, '\', '', [rfReplaceAll]);
      New(ps);
      ps^ := desc;
      if (Name <> '') and (desc <> '') then begin
        //g_ItemDesc.Put(name, TObject(ps));
        g_ItemDesc.AddObject(Name, TObject(ps));
      end;
    end;
    temp.Free;
  end;
end;

procedure InitObj();
begin
  //frmMain.DXDraw.AutoInitialize := True;
  DlgConf.DMsgDlg.Obj := FrmDlg.DMsgDlg;
  DlgConf.DMsgDlgOk.Obj := FrmDlg.DMsgDlgOk;
  DlgConf.DMsgDlgYes.Obj := FrmDlg.DMsgDlgYes;
  DlgConf.DMsgDlgCancel.Obj := FrmDlg.DMsgDlgCancel;
  DlgConf.DMsgDlgNo.Obj := FrmDlg.DMsgDlgNo;
  DlgConf.DLogin.Obj := FrmDlg.DLogin;
  DlgConf.DLoginNew.Obj := FrmDlg.DLoginNew;
  DlgConf.DLoginOk.Obj := FrmDlg.DLoginOk;
  DlgConf.DLoginChgPw.Obj := FrmDlg.DLoginChgPw;
  DlgConf.DLoginClose.Obj := FrmDlg.DLoginClose;
  DlgConf.DSelServerDlg.Obj := FrmDlg.DSelServerDlg;
  DlgConf.DSSrvClose.Obj := FrmDlg.DSSrvClose;
  DlgConf.DSServer1.Obj := FrmDlg.DSServer1;
  DlgConf.DSServer2.Obj := FrmDlg.DSServer2;
  DlgConf.DSServer3.Obj := FrmDlg.DSServer3;
  DlgConf.DSServer4.Obj := FrmDlg.DSServer4;
  DlgConf.DSServer5.Obj := FrmDlg.DSServer5;
  DlgConf.DSServer6.Obj := FrmDlg.DSServer6;
  DlgConf.DNewAccount.Obj := FrmDlg.DNewAccount;
  DlgConf.DNewAccountOk.Obj := FrmDlg.DNewAccountOk;
  DlgConf.DNewAccountCancel.Obj := FrmDlg.DNewAccountCancel;
  DlgConf.DNewAccountClose.Obj := FrmDlg.DNewAccountClose;
  DlgConf.DChgPw.Obj := FrmDlg.DChgPw;
  DlgConf.DChgpwOk.Obj := FrmDlg.DChgpwOk;
  DlgConf.DChgpwCancel.Obj := FrmDlg.DChgpwCancel;
  DlgConf.DSelectChr.Obj := FrmDlg.DSelectChr;
  DlgConf.DBottom.Obj := FrmDlg.DBottom;
  DlgConf.DMyState.Obj := FrmDlg.DMyState;
  DlgConf.DMyBag.Obj := FrmDlg.DMyBag;
  DlgConf.DMyMagic.Obj := FrmDlg.DMyMagic;
  DlgConf.DOption.Obj := FrmDlg.DOption;
  DlgConf.DBotMiniMap.Obj := FrmDlg.DBotMiniMap;
  DlgConf.DBotTrade.Obj := FrmDlg.DBotTrade;
  DlgConf.DBotGuild.Obj := FrmDlg.DBotGuild;
  DlgConf.DBotGroup.Obj := FrmDlg.DBotGroup;
  DlgConf.DBotPlusAbil.Obj := FrmDlg.DBotPlusAbil;
  DlgConf.DBotFriend.Obj := FrmDlg.DBotFriend;
  DlgConf.DBotDare.Obj := FrmDlg.DBotDare;
  DlgConf.DBotLevelRank.Obj := FrmDlg.DBotLevelRank;

  //DlgConf.DBotMemo.Obj := FrmDlg.DBotMemo;
  //DlgConf.DBotExit.Obj := FrmDlg.DBotExit;
  //DlgConf.DBotLogout.Obj := FrmDlg.DBotLogout;
  DlgConf.DBelt1.Obj := FrmDlg.DBelt1;
  DlgConf.DBelt2.Obj := FrmDlg.DBelt2;
  DlgConf.DBelt3.Obj := FrmDlg.DBelt3;
  DlgConf.DBelt4.Obj := FrmDlg.DBelt4;
  DlgConf.DBelt5.Obj := FrmDlg.DBelt5;
  DlgConf.DBelt6.Obj := FrmDlg.DBelt6;
  DlgConf.DGold.Obj := FrmDlg.DGold;
  DlgConf.DRfineItem.Obj := FrmDlg.DRefineItem;
  DlgConf.DCloseBag.Obj := FrmDlg.DCloseBag;
  DlgConf.DMerchantDlg.Obj := FrmDlg.DMerchantDlg;
  DlgConf.DMerchantDlgClose.Obj := FrmDlg.DMerchantDlgClose;
  //DlgConf.DConfigDlg.Obj := FrmDlg.DConfigDlg;
  //DlgConf.DConfigDlgOK.Obj := FrmDlg.DConfigDlgOK;
  //DlgConf.DConfigDlgClose.Obj := FrmDlg.DConfigDlgClose;
  DlgConf.DMenuDlg.Obj := FrmDlg.DMenuDlg;
  DlgConf.DMenuPrev.Obj := FrmDlg.DMenuPrev;
  DlgConf.DMenuNext.Obj := FrmDlg.DMenuNext;
  DlgConf.DMenuBuy.Obj := FrmDlg.DMenuBuy;
  DlgConf.DMenuClose.Obj := FrmDlg.DMenuClose;
  DlgConf.DSellDlg.Obj := FrmDlg.DSellDlg;
  DlgConf.DSellDlgOk.Obj := FrmDlg.DSellDlgOk;
  DlgConf.DSellDlgClose.Obj := FrmDlg.DSellDlgClose;
  DlgConf.DSellDlgSpot.Obj := FrmDlg.DSellDlgSpot;
  DlgConf.DKeySelDlg.Obj := FrmDlg.DKeySelDlg;
  DlgConf.DKsIcon.Obj := FrmDlg.DKsIcon;
  DlgConf.DKsF1.Obj := FrmDlg.DKsF1;
  DlgConf.DKsF2.Obj := FrmDlg.DKsF2;
  DlgConf.DKsF3.Obj := FrmDlg.DKsF3;
  DlgConf.DKsF4.Obj := FrmDlg.DKsF4;
  DlgConf.DKsF5.Obj := FrmDlg.DKsF5;
  DlgConf.DKsF6.Obj := FrmDlg.DKsF6;
  DlgConf.DKsF7.Obj := FrmDlg.DKsF7;
  DlgConf.DKsF8.Obj := FrmDlg.DKsF8;
  DlgConf.DKsConF1.Obj := FrmDlg.DKsConF1;
  DlgConf.DKsConF2.Obj := FrmDlg.DKsConF2;
  DlgConf.DKsConF3.Obj := FrmDlg.DKsConF3;
  DlgConf.DKsConF4.Obj := FrmDlg.DKsConF4;
  DlgConf.DKsConF5.Obj := FrmDlg.DKsConF5;
  DlgConf.DKsConF6.Obj := FrmDlg.DKsConF6;
  DlgConf.DKsConF7.Obj := FrmDlg.DKsConF7;
  DlgConf.DKsConF8.Obj := FrmDlg.DKsConF8;
  DlgConf.DKsNone.Obj := FrmDlg.DKsNone;
  DlgConf.DKsOk.Obj := FrmDlg.DKsOk;
  DlgConf.DItemGrid.Obj := FrmDlg.DItemGrid;
end;

function GetLevelColor(iLevel: Byte): Integer;
begin
  case iLevel of
    0: Result := $00FFFFFF;
    1: Result := $004AD663;
    2: Result := $00E9A000;
    3: Result := $00FF35B1;
    4: Result := $000061EB;
    5: Result := $005CF4FF;
    15: Result := clGray;
  else
    Result := $005CF4FF;
  end;
end;

procedure LoadItemFilter();
var
  i, n: Integer;
  s, s0, s1, s2, s3, s4, fn: string;
  ls: TStringList;
  p, p2: pTCItemRule;
begin
  fn := '.\Data\lsDefaultItemFilter.txt';
  if FileExists(fn) then begin
    ls := TStringList.Create;
    ls.LoadFromFile(fn);
    for i := 0 to ls.Count - 1 do begin
      s := ls[i];
      if s = '' then Continue;

      s := GetValidStr3(s, s0, [',']);
      s := GetValidStr3(s, s1, [',']);
      s := GetValidStr3(s, s2, [',']);
      s := GetValidStr3(s, s3, [',']);
      s := GetValidStr3(s, s4, [',']);
      New(p);
      p.Name := s0;
      p.rare := s2 = '1';
      p.pick := s3 = '1';
      p.show := s4 = '1';
      g_ItemsFilter_All.Put(s0, TObject(p));

      New(p2);
      p2^ := p^;
      g_ItemsFilter_All_Def.Put(s0, TObject(p2));

      n := StrToInt(s1);
      case n of
        0: g_ItemsFilter_Dress.AddObject(s0, TObject(p));
        1: g_ItemsFilter_Weapon.AddObject(s0, TObject(p));
        2: g_ItemsFilter_Headgear.AddObject(s0, TObject(p));
        3: g_ItemsFilter_Drug.AddObject(s0, TObject(p));
      else
        g_ItemsFilter_Other.AddObject(s0, TObject(p)); //服装
      end;

    end;

    ls.Free;
  end;
end;

procedure LoadItemFilter2();
var
  i, n: Integer;
  s, s0, s1, s2, s3, s4, fn: string;
  ls: TStringList;
  p, p2: pTCItemRule;
  b2, b3, b4: Boolean;
begin
  fn := '.\Config\' + g_sServerName + '.' + frmMain.m_sCharName + '.ItemFilter.txt';
  //DScreen.AddChatBoardString(fn, clWhite, clBlue);
  if FileExists(fn) then begin
    //DScreen.AddChatBoardString('1', clWhite, clBlue);
    ls := TStringList.Create;
    ls.LoadFromFile(fn);
    for i := 0 to ls.Count - 1 do begin
      s := ls[i];
      if s = '' then Continue;

      s := GetValidStr3(s, s0, [',']);
      s := GetValidStr3(s, s2, [',']);
      s := GetValidStr3(s, s3, [',']);
      s := GetValidStr3(s, s4, [',']);

      p := pTCItemRule(g_ItemsFilter_All_Def.GetValues(s0));
      if p <> nil then begin
        //DScreen.AddChatBoardString('2', clWhite, clBlue);
        b2 := s2 = '1';
        b3 := s3 = '1';
        b4 := s4 = '1';
        if (b2 <> p.rare) or (b3 <> p.pick) or (b4 <> p.show) then begin
          //DScreen.AddChatBoardString('3', clWhite, clBlue);
          p2 := pTCItemRule(g_ItemsFilter_All.GetValues(s0));
          if p2 <> nil then begin
            //DScreen.AddChatBoardString('4', clWhite, clBlue);
            p2.rare := b2;
            p2.pick := b3;
            p2.show := b4;
          end;
        end;
      end;
    end;

    ls.Free;
  end;
end;

procedure SaveItemFilter(); //退出增量保存
var
  i, n: Integer;
  s, s0, s1, s2, s3, s4, fn: string;
  ls: TStringList;
  p, p2: pTCItemRule;
begin
  //Savebagsdat(, @g_ItemArr);
  fn := '.\Config\' + g_sServerName + '.' + frmMain.m_sCharName + '.ItemFilter.txt';
  ls := TStringList.Create;
  for i := 0 to g_ItemsFilter_All.Count - 1 do begin
    p := pTCItemRule(g_ItemsFilter_All.GetValues(g_ItemsFilter_All.Keys[i]));
    p2 := pTCItemRule(g_ItemsFilter_All_Def.GetValues(g_ItemsFilter_All_Def.Keys[i]));
    if p.Name = p2.Name then begin
      if (p.rare <> p2.rare) or
        (p.pick <> p2.pick) or
        (p.show <> p2.show) then begin

        ls.Add(Format('%s,%d,%d,%d', [
          p.Name,
            Byte(p.rare),
            Byte(p.pick),
            Byte(p.show)
            ]));

      end;
    end;
  end;
  if ls.Count > 0 then
    ls.SaveToFile(fn);
  ls.Free;
end;

function getSuiteHint(var idx: Integer; s: string; gender: Byte): pTClientSuiteItems;
var
  i: Integer;
  p: pTClientSuiteItems;
begin
  Result := nil;
  if (idx > 12) or (idx < 0) then Exit;
  for i := 0 to g_SuiteItemsList.Count - 1 do begin
    p := g_SuiteItemsList[i];
    if {((p.asSuiteName[0] = '') or (gender = p.gender)) and }(CompareText(s, p.asSuiteName[idx]) = 0) then begin  //20200720修复不是本性别衣服无法显示套装属性
      Result := p;
      Break;
    end;
  end;
  idx := -1;
end;

function GetItemWhere(Item: TClientItem): Integer;
begin
  Result := -1;
  if Item.s.Name = '' then Exit;
  case Item.s.StdMode of
    10, 11: begin
        Result := U_DRESS;
      end;
    5, 6: begin
        Result := U_WEAPON;
      end;
    30: begin
        Result := U_RIGHTHAND;
      end;
    19, 20, 21: begin
        Result := U_NECKLACE;
      end;
    15: begin
        Result := U_HELMET;
      end;
    16: begin

      end;
    24, 26: begin
        Result := U_ARMRINGL;
      end;
    22, 23: begin
        Result := U_RINGL;
      end;
    25: begin
        Result := U_BUJUK;
      end;
    27: begin
        Result := U_BELT;
      end;
    28: begin
        Result := U_BOOTS;
      end;
    7, 29: begin
        Result := U_CHARM;
      end;
  end;
end;

const
  g_levelstring: array[1..8] of string = ('一', '二', '三', '四', '五', '六', '七', '八');

function GetEvaBaseAbil(Val: TEvaAbil): string;
begin
  Result := '';
  if Val.btValue = 0 then Exit;
  case Val.btType of
    01: Result := Format(' 攻击上限 +%d', [Val.btValue]);
    02: Result := Format(' 魔法上限 +%d', [Val.btValue]);
    03: Result := Format(' 道术上限 +%d', [Val.btValue]);
    04: Result := Format(' 物防上限 +%d', [Val.btValue]);
    05: Result := Format(' 魔防上限 +%d', [Val.btValue]);

    06: Result := Format('< 准确     +%d|c=clSkyBlue>', [Val.btValue]);
    07: Result := Format('< 敏捷     +%d|c=clSkyBlue>', [Val.btValue]);
    08: Result := Format('< 魔法躲避 +%d|c=clSkyBlue>', [Val.btValue * 10]);
    09: Result := Format('< 幸运     +%d|c=clSkyBlue>', [Val.btValue]);
    10: Result := Format('< 诅咒     +%d|c=clSkyBlue>', [Val.btValue]);
    11: Result := Format('< 攻击速度 +%d|c=clSkyBlue>', [Val.btValue]);
    12: Result := Format('< 神圣     +%d|c=clSkyBlue>', [Val.btValue]);
    13: Result := Format('< 魔法回复 +%d|c=clSkyBlue>', [Val.btValue * 10]);
    14: Result := Format('< 体力回复 +%d|c=clSkyBlue>', [Val.btValue * 10]);

    //
    15: Result := Format('< 击杀爆率 +%d|c=clSkyBlue>', [Val.btValue]);
    16: Result := Format('< 防爆     +%d|c=clSkyBlue>', [Val.btValue]);
    17: Result := Format('< 吸血上限 +%d|c=clSkyBlue>', [Val.btValue]);

    18: Result := Format('< 内力恢复 +%d|c=clSkyBlue>', [Val.btValue]);
    19: Result := Format('< 内力上限 +%d|c=clSkyBlue>', [Val.btValue]);

    20: Result := Format('< 内功伤害 +%d|c=clSkyBlue>', [Val.btValue]);
    21: Result := Format('< 内功减免 +%d|c=clSkyBlue>', [Val.btValue]);
    22: Result := Format('< 内伤等级 +%d|c=clSkyBlue>', [Val.btValue]);

    23: Result := Format('< 暴击威力 +%d|c=clSkyBlue>', [Val.btValue * 2]);
    24: Result := Format('< 合击威力 +%d|c=clSkyBlue>', [Val.btValue]) + '<%|c=clSkyBlue>';

    25: Result := Format('< 麻痹抗性 +%d|c=clSkyBlue>', [Val.btValue]) + '<%|c=clSkyBlue>';

    26: Result := Format('< 强身等级 +%d|c=clSkyBlue>', [Val.btValue]);
    27: Result := Format('< 聚魔等级 +%d|c=clSkyBlue>', [Val.btValue]);
    28: Result := Format('< 主属性   +%d|c=clWhite>', [Val.btValue]);

    29: Result := Format('< 毒物躲避 +%d|c=clSkyBlue>', [Val.btValue]);
    30: Result := Format('< 中毒恢复 +%d|c=clSkyBlue>', [Val.btValue]);

    //11: Result := Format(' 增加血量 +%d', [val.btValue]);
    //12: Result := Format(' 增加魔法 +%d', [val.btValue]);
  end;
end;

function GetEvaMysteryAbil(Val, val2: Byte; var cnt: Byte; Std: TClientStdItem): string;
var
  SpSkill: Byte;
begin
  cnt := 0;
  Result := '';
  Val := Val and $7F;
  if Val and $01 <> 0 then begin
    Result := Result + '< 八卦护身神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $02 <> 0 then begin
    Result := Result + '< 战意麻痹神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $04 <> 0 then begin
    Result := Result + '< 重生神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $08 <> 0 then begin
    Result := Result + '< 探测神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $10 <> 0 then begin
    Result := Result + '< 传送神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $20 <> 0 then begin
    Result := Result + '< 麻痹神技|c=$004AD663>\';
    Inc(cnt);
  end;
  if Val and $40 <> 0 then begin
    Result := Result + '< 魔道麻痹神技|c=$004AD663>\';
    Inc(cnt);
  end;

  {
      0: Result := $00FFFFFF;
      1: Result := $004AD663;
      2: Result := $00E9A000;
      3: Result := $00FF35B1;
      4: Result := $000061EB;
      5: Result := $005CF4FF;
  }
  if Std.StdMode in [5, 6] then begin
    if val2 and $01 <> 0 then begin
      Result := Result + '< 五岳独尊特技|c=$00E9A000>\';
      Inc(cnt);
    end;
    if val2 and $02 <> 0 then begin
      Result := Result + '< 召唤巨魔特技|c=$00E9A000>\';
      Inc(cnt);
    end;
    if val2 and $04 <> 0 then begin
      Result := Result + '< 神龙附体特技|c=$00E9A000>\';
      Inc(cnt);
    end;
    if val2 and $08 <> 0 then begin
      Result := Result + '< 倚天劈地特技|c=$00E9A000>\';
      Inc(cnt);
    end;
  end else begin
    SpSkill := 0;
    case g_ShowSuite3 of
      1: if (g_UseItems[1].s.Name <> '') and (g_UseItems[1].s.Eva.SpSkill <> 0) then
          SpSkill := g_UseItems[1].s.Eva.SpSkill;
      2: if (g_HeroUseItems[1].s.Name <> '') and (g_HeroUseItems[1].s.Eva.SpSkill <> 0) then
          SpSkill := g_UseItems[1].s.Eva.SpSkill;
      3: if (UserState1.UseItems[1].s.Name <> '') and (UserState1.UseItems[1].s.Eva.SpSkill <> 0) then
          SpSkill := g_UseItems[1].s.Eva.SpSkill;
    end;
    g_ShowSuite3 := 0;

    if (val2 and $01 <> 0) then begin
      if (SpSkill and $01 <> 0) then
        Result := Result + '< 五岳独尊Lv+1|c=$00E9A000>\'
      else
        Result := Result + '< 五岳独尊Lv+1 (未修炼)|c=clGray>\';
      Inc(cnt);
    end;

    if (val2 and $2 <> 0) then begin
      if (SpSkill and $2 <> 0) or (g_SuiteSpSkill > 0) then
        Result := Result + '< 召唤巨魔Lv+1|c=$00E9A000>\'
      else
        Result := Result + '< 召唤巨魔Lv+1 (未修炼)|c=clGray>\';
      Inc(cnt);
    end;

    if (val2 and $4 <> 0) then begin
      if (SpSkill and $4 <> 0) then
        Result := Result + '< 神龙附体Lv+1|c=$00E9A000>\'
      else
        Result := Result + '< 神龙附体Lv+1 (未修炼)|c=clGray>\';
      Inc(cnt);
    end;

    if (val2 and $8 <> 0) then begin
      if (SpSkill and $8 <> 0) then
        Result := Result + '< 倚天劈地Lv+1|c=$00E9A000>\'
      else
        Result := Result + '< 倚天劈地Lv+1 (未修炼)|c=clGray>\';
      Inc(cnt);
    end;

    g_SuiteSpSkill := 0;
  end;
end;

function GetSecretAbil(CurrMouseItem: TClientItem): Boolean;
var
  i, start: Integer;
  adv, cnt: Byte;
  s, ss: string;
begin
  Result := False;
  if not (CurrMouseItem.s.StdMode in [5, 6, 10..13, 15..24, 26..30]) then Exit;

  if CurrMouseItem.s.Eva.AdvAbilMax > 0 then begin
    cnt := 0;
    for i := CurrMouseItem.s.Eva.BaseMax to 7 do begin
      if CurrMouseItem.s.Eva.Abil[i].btValue = 0 then
        Break;
      s := GetEvaBaseAbil(CurrMouseItem.s.Eva.Abil[i]);
      if s <> '' then begin
        Inc(cnt);
      end;
    end;

    adv := 0;
    if (CurrMouseItem.s.Eva.AdvAbil <> 0) or (CurrMouseItem.s.Eva.SpSkill <> 0) then
      GetEvaMysteryAbil(CurrMouseItem.s.Eva.AdvAbil, CurrMouseItem.s.Eva.SpSkill, adv, CurrMouseItem.s);

    cnt := cnt + adv;
    start := Integer(CurrMouseItem.s.Eva.AdvAbilMax - cnt);
    if start > 0 then
      Result := True;
  end;
end;

function GetEvaluationInfo(CurrMouseItem: TClientItem): string;
var
  i, start: Integer;
  adv, cnt: Byte;
  s, ss: string;
begin
  Result := '';

  if (CurrMouseItem.s.Eva.EvaTimesMax <> 0) and (CurrMouseItem.s.Eva.EvaTimes = 0) then begin
    Result := Result + '<可鉴定|c=clyellow>\';
  end else begin
    if CurrMouseItem.s.Eva.EvaTimes in [1..8] then begin
      if CurrMouseItem.s.Eva.EvaTimes < CurrMouseItem.s.Eva.EvaTimesMax then
        Result := Result + Format('<%s鉴(仍可鉴定)|c=clyellow>\', [g_levelstring[CurrMouseItem.s.Eva.EvaTimes]])
      else
        Result := Result + Format('<%s鉴|c=clyellow>\', [g_levelstring[CurrMouseItem.s.Eva.EvaTimes]]);
    end;
  end;

  start := 0;
  case CurrMouseItem.s.Eva.Quality of
    001..050: start := 1;
    051..100: start := 2;
    101..150: start := 3;
    151..200: start := 4;
    201..255: start := 5;
  end;
  ss := '';
  if start > 0 then begin
    for i := 0 to start - 1 do
      ss := ss + '<u~|I=1345>';
  end;
  if (ss <> '') then
    Result := Result + ' ' + ss + '\';

  //基础属性
  ss := '';
  if CurrMouseItem.s.Eva.BaseMax in [1..8] then begin
    for i := 0 to CurrMouseItem.s.Eva.BaseMax - 1 do begin
      s := GetEvaBaseAbil(CurrMouseItem.s.Eva.Abil[i]);
      if s <> '' then
        ss := ss + s + '\';
    end;
  end;
  if ss <> '' then
    Result := Result + ' \<附加基础属性|c=clYellow>\' + ss;

  //神秘属性
  ss := '';
  if CurrMouseItem.s.Eva.AdvAbilMax > 0 then begin
    cnt := 0;
    for i := CurrMouseItem.s.Eva.BaseMax to 7 do begin
      if CurrMouseItem.s.Eva.Abil[i].btValue = 0 then
        Break;
      s := GetEvaBaseAbil(CurrMouseItem.s.Eva.Abil[i]);
      if s <> '' then begin
        ss := ss + s + '\';
        Inc(cnt);
      end;
    end;

    adv := 0;
    if (CurrMouseItem.s.Eva.AdvAbil <> 0) or (CurrMouseItem.s.Eva.SpSkill <> 0) then begin
      ss := ss + GetEvaMysteryAbil(CurrMouseItem.s.Eva.AdvAbil, CurrMouseItem.s.Eva.SpSkill, adv, CurrMouseItem.s);
    end;

    cnt := cnt + adv;

    start := Integer(CurrMouseItem.s.Eva.AdvAbilMax - cnt);
    if start > 0 then
      for i := 0 to start - 1 do
        ss := ss + '< 神秘属性(待解读)|c=clRed>\';
  end;
  if ss <> '' then
    Result := Result + ' \<附加神秘属性|c=clYellow>\' + ss;

  //灵媒
  ss := '';
  if CurrMouseItem.s.Eva.SpiritMax > 0 then begin
    if CurrMouseItem.s.Eva.Spirit > 0 then
      ss := ss + Format('<宝物灵媒|c=clLime> <品质%d|c=clYellow> <灵气%d/%d|c=clYellow>\', [CurrMouseItem.s.Eva.SpiritQ, CurrMouseItem.s.Eva.Spirit, CurrMouseItem.s.Eva.SpiritMax])
    else
      ss := ss + Format('<宝物灵媒|c=clLime> <品质%d|c=clYellow> <灵气0/%d|c=clRed>\', [CurrMouseItem.s.Eva.SpiritQ, CurrMouseItem.s.Eva.SpiritMax]);
  end;
  if ss <> '' then
    Result := Result + '-\' + ss;

end;

procedure InitClientItems();
begin
  FillChar(g_MagicArr, SizeOf(g_MagicArr), 0);
  FillChar(g_TakeBackItemWait, SizeOf(g_TakeBackItemWait), 0);

  FillChar(g_UseItems, SizeOf(TClientItem) * 14, #0);
  FillChar(g_ItemArr, SizeOf(TClientItem) * MAXBAGITEMCL, #0);
  FillChar(g_HeroUseItems, SizeOf(TClientItem) * 14, #0);
  FillChar(g_HeroItemArr, SizeOf(TClientItem) * MAXBAGITEMCL, #0);
  FillChar(g_RefineItems, SizeOf(TMovingItem) * 3, #0);
  FillChar(g_BuildAcuses, SizeOf(g_BuildAcuses), #0);
  FillChar(g_DetectItem, SizeOf(g_DetectItem), #0);
  FillChar(g_TIItems, SizeOf(g_TIItems), #0);
  FillChar(g_spItems, SizeOf(g_spItems), #0);

  FillChar(g_ItemArr, SizeOf(TClientItem) * MAXBAGITEMCL, #0);
  FillChar(g_HeroItemArr, SizeOf(TClientItem) * MAXBAGITEMCL, #0);

  FillChar(g_DealItems, SizeOf(TClientItem) * 10, #0);
  FillChar(g_YbDealItems, SizeOf(TClientItem) * 10, #0);
  FillChar(g_DealRemoteItems, SizeOf(TClientItem) * 20, #0);
end;

function GetTIHintString1(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
begin
  Result := 0;
  g_tiHintStr1 := '';
  case idx of
    0: begin
        g_tiHintStr1 := '我收藏天下的奇珍异宝，走南闯北几十年了，各种神器见过不少，把你要鉴定的装备放在桌子上吧！';
        FrmDlg.DBTIbtn1.OnbtnState := tdisable;
        FrmDlg.DBTIbtn1.Caption := '普通鉴定';
        FrmDlg.DBTIbtn2.OnbtnState := tdisable;
        FrmDlg.DBTIbtn2.Caption := '高级鉴定';
      end;
    1: begin
        if (ci = nil) or (ci.s.Name = '') then Exit;
        if ci.s.Eva.EvaTimesMax = 0 then begin
          g_tiHintStr1 := '标志了不可鉴定的物品我是鉴定不了的，你换一个吧。';
          FrmDlg.DBTIbtn1.OnbtnState := tdisable;
          FrmDlg.DBTIbtn1.Caption := '普通鉴定';
          FrmDlg.DBTIbtn2.OnbtnState := tdisable;
          FrmDlg.DBTIbtn2.Caption := '高级鉴定';
          Exit;
        end;
        if ci.s.Eva.EvaTimes < ci.s.Eva.EvaTimesMax then begin
          if FrmDlg.DWTI.tag = 1 then begin
            case ci.s.Eva.EvaTimes of
              0: begin
                  g_tiHintStr1 := '第一次鉴定我需要一个一级鉴定卷轴，你快去收集一个吧！';
                  FrmDlg.DBTIbtn1.OnbtnState := tnor;
                  FrmDlg.DBTIbtn1.Caption := '普通一鉴';
                  FrmDlg.DBTIbtn2.OnbtnState := tnor;
                  FrmDlg.DBTIbtn2.Caption := '高级一鉴';
                end;
              1: begin
                  g_tiHintStr1 := '第二次鉴定我需要一个二级鉴定卷轴，你快去收集一个吧！';
                  FrmDlg.DBTIbtn1.OnbtnState := tnor;
                  FrmDlg.DBTIbtn1.Caption := '普通二鉴';
                  FrmDlg.DBTIbtn2.OnbtnState := tnor;
                  FrmDlg.DBTIbtn2.Caption := '高级二鉴';
                end;
              2: begin
                  g_tiHintStr1 := '第三次鉴定我需要一个三级鉴定卷轴，你快去收集一个吧！';
                  FrmDlg.DBTIbtn1.OnbtnState := tnor;
                  FrmDlg.DBTIbtn1.Caption := '普通三鉴';
                  FrmDlg.DBTIbtn2.OnbtnState := tnor;
                  FrmDlg.DBTIbtn2.Caption := '高级三鉴';
                end;
            else begin
                g_tiHintStr1 := '我需要一个三级鉴定卷轴来鉴定你这个装备。';
                FrmDlg.DBTIbtn1.OnbtnState := tnor;
                FrmDlg.DBTIbtn1.Caption := '普通三鉴';
                FrmDlg.DBTIbtn2.OnbtnState := tnor;
                FrmDlg.DBTIbtn2.Caption := '高级三鉴';
              end;
            end;
          end else if FrmDlg.DWTI.tag = 2 then begin
            FrmDlg.DBTIbtn1.OnbtnState := tnor;
            FrmDlg.DBTIbtn1.Caption := '更换';
          end;

          Result := ci.s.Eva.EvaTimes;
        end else begin
          g_tiHintStr1 := Format('你的这件%s已经不能再鉴定了。', [ci.s.Name]);
          FrmDlg.DBTIbtn1.OnbtnState := tdisable;
          FrmDlg.DBTIbtn1.Caption := '普通鉴定';
          FrmDlg.DBTIbtn2.OnbtnState := tdisable;
          FrmDlg.DBTIbtn2.Caption := '高级鉴定';
        end;
      end;

    2: g_tiHintStr1 := Format('借助卷轴的力量，我已经帮你发现了你这%s的潜能。', [iname]);
    3: g_tiHintStr1 := Format('借助卷轴的力量，我已经帮你发现了你这%s的神秘潜能。', [iname]);
    4: g_tiHintStr1 := Format('这%s虽然没能发现更大的潜能，但是他拥有感应其他宝物存在的特殊能力。', [iname]);
    5: g_tiHintStr1 := Format('我并没能从你的这个%s上发现更多的潜能。你不要沮丧，我会给你额外的补偿。', [iname]);
    6: g_tiHintStr1 := Format('我并没能从你的这个%s上发现更多的潜能。', [iname]);
    7: g_tiHintStr1 := Format('我并没能从你的这个%s上发现更多的潜能。你的宝物已经不可鉴定。', [iname]);

    8: g_tiHintStr1 := '你缺少宝物或者卷轴。';
    9: g_tiHintStr1 := Format('恭喜你的宝物被鉴定为主宰装备，你获得了%s。', [iname]);
    10: g_tiHintStr1 := '待鉴物品错误或不存在！';
    11: g_tiHintStr1 := Format('你的这件%s不可以鉴定！', [iname]);
    12: begin
        FrmDlg.DBTIbtn1.OnbtnState := tdisable;
        FrmDlg.DBTIbtn2.OnbtnState := tdisable;
        g_tiHintStr1 := Format('以我目前的能力，%s只能先鉴定到这里了。', [iname]);
      end;

    30: g_tiHintStr1 := '鉴定卷轴错误或不存在！';
    31: g_tiHintStr1 := Format('我需要一个%s级鉴定卷轴，你的卷轴不符合要求！', [iname]);

    32: g_tiHintStr1 := Format('高级鉴定失败，你的%s消失了！', [iname]);
    33: g_tiHintStr1 := Format('服务器没有%s的数据，高级鉴定失败！', [iname]);
  end;
end;

function GetTIHintString2(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
begin
  Result := 0;
  g_tiHintStr1 := '';
  case idx of
    0: begin
        g_tiHintStr1 := '如果你不喜欢已经鉴定过了的宝物，你可以把他给我，我平素最爱收藏各种宝物，我会给你一个一模一样的没鉴定过的装备作为补偿。';
        FrmDlg.DBTIbtn1.OnbtnState := tdisable;
      end;
    1: begin
        g_tiHintStr1 := Format('这个%s，看上去不错，我这里正好有没有鉴定过的各种%s你可以挑一把，要换的话，你要给我一个幸运符。', [ci.s.Name, ci.s.Name]);
        FrmDlg.DBTIbtn1.OnbtnState := tnor;
      end;
    2: g_tiHintStr1 := Format('我已经给了你一把没鉴定过的%s，跟你原来的%s没鉴定过之前是一模一样的！', [iname, iname]);
    3: g_tiHintStr1 := '缺少宝物或材料。';
    4: g_tiHintStr1 := Format('你的这件%s并没有鉴定过。', [iname]);
    5: g_tiHintStr1 := '材料不符合，请放入幸运符。';
    6: g_tiHintStr1 := '该物品框只能放鉴定过的宝物，你的东西不符合，我已经将它放回你的包裹了。';
    7: g_tiHintStr1 := '该物品框只能放幸运符，你的东西不符合，我已经将它放回你的包裹了。';
    8: g_tiHintStr1 := '宝物更换失败。';
  end;
end;

function GetSpHintString1(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
begin
  Result := 0;
  g_spHintStr1 := '';
  case idx of
    0: g_spHintStr1 := '你可以跟别人购买神秘卷轴，也可以自己制作神秘卷轴来解读宝物的神秘属性。';
    1: g_spHintStr1 :=
      '这次解读不幸失败，解读幸运值、神秘卷轴' +
        '的等级和熟练度过低可能导致解读失败，不' +
        '要失望，再接再厉吧。';
    2: g_spHintStr1 := '找不到鉴定物品或卷轴';
    3: begin
        FrmDlg.DBSP.OnbtnState := tdisable;
        g_spHintStr1 := '没有可鉴定的神秘属性';
      end;
    4: g_spHintStr1 := '装备不符合神秘解读要求';
    5: g_spHintStr1 := '卷轴类型不符合';
    6: g_spHintStr1 := '卷轴等级不符合';
    7: g_spHintStr1 := '借助神秘卷轴的帮助，已经帮你解读出了一个神秘属性';

    10: g_spHintStr1 := '神秘卷轴制作成功。';
    11: g_spHintStr1 :=
      '这次制作不幸的失败了，可能是因为你的神' +
        '秘解读技能等级还不够高，或者是你制作的' +
        '卷轴等级太高了';
    12: g_spHintStr1 := '找不到羊皮卷。';
    13: g_spHintStr1 := '请放入羊皮卷。';
    14: g_spHintStr1 := '精力值不够。';
    15: g_spHintStr1 := '没有解读技能，制作失败。';

  end;
end;

function GetSpHintString2(idx: Integer; ci: PTClientItem = nil; iname: string = ''): Byte;
begin
  Result := 0;
  g_spHintStr1 := '';
  case idx of
    0: begin
        g_spHintStr1 := '你可以把你对鉴宝的心得还有你的鉴定经验写在神秘卷轴上，这样的话，就可以帮助更多人解读神秘属性。';
      end;
  end;
end;

procedure AutoPutOntiBooks();
var
  i: Integer;
  cu: TClientItem;
begin
  if FrmDlg.DWTI.Visible and (FrmDlg.DWTI.tag = 1) and
    (g_TIItems[0].Item.s.Name <> '') and
    (g_TIItems[0].Item.s.Eva.EvaTimesMax > 0) and
    (g_TIItems[0].Item.s.Eva.EvaTimes < g_TIItems[0].Item.s.Eva.EvaTimesMax) and
    ((g_TIItems[1].Item.s.Name = '') or
    (g_TIItems[1].Item.s.StdMode <> 56) or
    not (g_TIItems[1].Item.s.Shape in [1..3]) or
    (g_TIItems[1].Item.s.Shape <> g_TIItems[0].Item.s.Eva.EvaTimes + 1)) then begin
    for i := MAXBAGITEMCL - 1 downto 6 do begin
      if (g_ItemArr[i].s.Name <> '') and (g_ItemArr[i].s.StdMode = 56) and (g_ItemArr[i].s.Shape = g_TIItems[0].Item.s.Eva.EvaTimes + 1) then begin
        if g_TIItems[1].Item.s.Name <> '' then begin
          cu := g_TIItems[1].Item;
          g_TIItems[1].Item := g_ItemArr[i];
          g_TIItems[1].Index := i;
          g_ItemArr[i] := cu;
        end else begin
          g_TIItems[1].Item := g_ItemArr[i];
          g_TIItems[1].Index := i;
          g_ItemArr[i].s.Name := '';
        end;
        Break;
      end;
    end;
  end;
end;

procedure AutoPutOntiSecretBooks();
var
  i: Integer;
  cu: TClientItem;
begin
  if FrmDlg.DWSP.Visible and (FrmDlg.DWSP.tag = 1) and
    (g_spItems[0].Item.s.Name <> '') and
    (g_spItems[0].Item.s.Eva.EvaTimesMax > 0) and
    ((g_spItems[1].Item.s.Name = '') or
    (g_spItems[1].Item.s.StdMode <> 56) or
    (g_spItems[1].Item.s.Shape <> 0)) then begin
    for i := MAXBAGITEMCL - 1 downto 6 do begin
      if (g_ItemArr[i].s.Name <> '') and (g_ItemArr[i].s.StdMode = 56) and (g_ItemArr[i].s.Shape = 0) then begin
        if g_spItems[1].Item.s.Name <> '' then begin
          cu := g_spItems[1].Item;
          g_spItems[1].Item := g_ItemArr[i];
          g_spItems[1].Index := i;
          g_ItemArr[i] := cu;
        end else begin
          g_spItems[1].Item := g_ItemArr[i];
          g_spItems[1].Index := i;
          g_ItemArr[i].s.Name := '';
        end;
        Break;
      end;
    end;
  end;
end;

procedure AutoPutOntiCharms();
var
  i, n: Integer;
  cu: TClientItem;
begin
  n := 0;
  if FrmDlg.DWTI.Visible and (FrmDlg.DWTI.tag = 2) and
    (g_TIItems[0].Item.s.Name <> '') and
    (g_TIItems[0].Item.s.Eva.EvaTimesMax > 0) and
    (g_TIItems[0].Item.s.Eva.EvaTimes > 0) and
    ((g_TIItems[1].Item.s.Name = '') or (g_TIItems[1].Item.s.StdMode <> 41) or (g_TIItems[1].Item.s.Shape <> 30)) then begin
    n := 1;
    for i := MAXBAGITEMCL - 1 downto 6 do begin
      n := 2;
      if (g_ItemArr[i].s.Name <> '') and (g_ItemArr[i].s.StdMode = 41) and (g_ItemArr[i].s.Shape = 30) then begin
        n := 3;
        if g_TIItems[1].Item.s.Name <> '' then begin
          n := 4;
          cu := g_TIItems[1].Item;
          g_TIItems[1].Item := g_ItemArr[i];
          g_TIItems[1].Index := i;
          g_ItemArr[i] := cu;
        end else begin
          n := 5;
          g_TIItems[1].Item := g_ItemArr[i];
          g_TIItems[1].Index := i;
          g_ItemArr[i].s.Name := '';
        end;
        Break;
      end;
    end;
  end;
  //DScreen.AddChatBoardString(inttostr(n), clWhite, GetRGB($FC));
end;

function GetSuiteAbil(idx, Shape: Integer; var sa: TtSuiteAbil): Boolean;
var
  i: Integer;
begin
  FillChar(sa, SizeOf(sa), 0);
  case idx of
    1: begin
        for i := Low(TtSuiteAbil) to High(TtSuiteAbil) do begin
          if (g_UseItems[i].s.Name <> '') and ((g_UseItems[i].s.Shape = Shape) or (g_UseItems[i].s.AniCount = Shape)) then
            sa[i] := 1;
        end;
      end;
    2: for i := Low(TtSuiteAbil) to High(TtSuiteAbil) do begin
        if (g_HeroUseItems[i].s.Name <> '') and ((g_HeroUseItems[i].s.Shape = Shape) or (g_HeroUseItems[i].s.AniCount = Shape)) then
          sa[i] := 1;
      end;
    3: for i := Low(TtSuiteAbil) to High(TtSuiteAbil) do begin
        if (UserState1.UseItems[i].s.Name <> '') and ((UserState1.UseItems[i].s.Shape = Shape) or (UserState1.UseItems[i].s.AniCount = Shape)) then
          sa[i] := 1;
      end;
  end;
end;

procedure ScreenChanged();
begin
  g_TileMapOffSetX := Round(SCREENWIDTH div 2 / UNITX) + 1;
  g_TileMapOffSetY := Round(SCREENHEIGHT div 2 / UNITY);
  AAX := (SCREENWIDTH - UNITX) div 2 mod UNITX;

  LMX := Round(SCREENWIDTH / UNITX * 2) + 4; //30
  LMY := Round((SCREENHEIGHT - 150) / UNITX * 2); //26;
end;

function ShiftYOffset(): Integer;
begin
  Result := Round((SCREENHEIGHT - 150) / UNITY / 2) * UNITY - HALFY;
end;

procedure InitScreenConfig();
var
  i: Integer;
  ini: TIniFile;
begin
//  ini := TIniFile.Create('.\lscfg.ini');
//  SCREENWIDTH := ini.ReadInteger('Setup', 'SCREENWIDTH', 800);
//  SCREENHEIGHT := ini.ReadInteger('Setup', 'SCREENHEIGHT', 600);
//  g_boFullScreen := ini.ReadBool('Setup', 'FullScreen', False);
////  g_Windowed := ini.ReadBool('Setup', 'Windowed', True);
//  g_VSync := ini.ReadBool('Setup', 'VSync', True);
//  ini.Free;
  SCREENWIDTH := g_MirStartupInfo.nScreenWidth;
  SCREENHEIGHT := g_MirStartupInfo.nScreenHegiht;
  g_boFullScreen := g_MirStartupInfo.boFullScreen;
  g_VSync := g_MirStartupInfo.boWaitVBlank;

  BOTTOMTOP := SCREENHEIGHT - 251;
  WINRIGHT := SCREENWIDTH - 60;
  BOTTOMEDGE := SCREENHEIGHT - 30;
  SURFACEMEMLEN := ((SCREENWIDTH + 3) div 4) * 4 * SCREENHEIGHT;
  MAPSURFACEWIDTH := SCREENWIDTH;
  MAPSURFACEHEIGHT := SCREENHEIGHT - 150;
  BOXWIDTH := (SCREENWIDTH div 2 - 214) * 2;

  with g_SkidAD_Rect do begin
    Left := 5;
    Top := 6;
    Right := SCREENWIDTH - 5;
    Bottom := 6 + 20;
  end;

  with g_SkidAD_Rect2 do begin
    Left := 183;
    Top := 6;
    Right := SCREENWIDTH - 208;
    Bottom := 6 + 20;
  end;

  with G_RC_LEVEL do begin
    Left := SCREENWIDTH - 800 + 664;
    Top := 144;
    Right := SCREENWIDTH - 800 + 664 + 34;
    Bottom := 144 + 14;
  end;
  with G_RC_EXP do begin
    Left := SCREENWIDTH - 800 + 666;
    Top := 178;
    Right := SCREENWIDTH - 800 + 666 + 72;
    Bottom := 178 + 14;
  end;
  with G_RC_WEIGTH do begin
    Left := SCREENWIDTH - 800 + 666;
    Top := 212;
    Right := SCREENWIDTH - 800 + 666 + 72;
    Bottom := 212 + 14;
  end;
  with G_RC_SQUENGINER do begin
    Left := 78;
    Top := 90;
    Right := Left + 16;
    Bottom := Top + 95;
  end;
  with G_RC_IMEMODE do begin
    Left := SCREENWIDTH - 270 - 65;
    Top := 105;
    Right := Left + 60;
    Bottom := Top + 9;
  end;
end;

function IsInMyRange(Act: TActor): Boolean;
begin
  Result := False;
  if (Act = nil) or (g_MySelf = nil) then
    Exit;
  if (abs(Act.m_nCurrX - g_MySelf.m_nCurrX) <= (g_TileMapOffSetX - 2)) and (abs(Act.m_nCurrY - g_MySelf.m_nCurrY) <= (g_TileMapOffSetY - 1)) then
    Result := True;
end;

function IsItemInMyRange(X, Y: Integer): Boolean;
begin
  Result := False;
  if (g_MySelf = nil) then
    Exit;
  if (abs(X - g_MySelf.m_nCurrX) <= _MIN(24, g_TileMapOffSetX + 9)) and (abs(Y - g_MySelf.m_nCurrY) <= _MIN(24, (g_TileMapOffSetY + 10))) then
    Result := True;
end;

function GetTitle(nItemIdx: Integer): pTClientStdItem;
begin
  Result := nil;
  Dec(nItemIdx);
  if (nItemIdx >= 0) and (g_TitlesList.Count > nItemIdx) then begin
    if pTStdItem(g_TitlesList.Items[nItemIdx]).Name <> '' then
      Result := g_TitlesList.Items[nItemIdx];
  end;
end;

initialization
  FillChar(g_WTilesArr, SizeOf(g_WTilesArr), 0);
  FillChar(g_WSmTilesArr, SizeOf(g_WSmTilesArr), 0);
  FillChar(g_WObjectArr, SizeOf(g_WObjectArr), 0);
  New(g_APPass);
  New(g_dwThreadTick);
  g_dwThreadTick^ := 0;
  New(g_pRcHeader);
  New(g_pbRecallHero);
  g_pbRecallHero^ := False;

  //InitializeCriticalSection(ProcImagesCS);
  InitializeCriticalSection(ProcMsgCS);
  InitializeCriticalSection(ThreadCS);
  g_APPickUpList := THStringList.Create;
  g_APMobList := THStringList.Create;

  g_ItemsFilter_All := TCnHashTableSmall.Create;
  g_ItemsFilter_All_Def := TCnHashTableSmall.Create;
  g_ItemsFilter_Dress := TStringList.Create;
  g_ItemsFilter_Weapon := TStringList.Create;
  g_ItemsFilter_Headgear := TStringList.Create;
  g_ItemsFilter_Drug := TStringList.Create;
  g_ItemsFilter_Other := TStringList.Create;
  //g_VCLUnZip1 := TVCLUnZip.Create(nil);
  g_SuiteItemsList := TList.Create;
  g_pshine:= TSimpleMsgPack.Create;
  g_TitlesList := TList.Create;
  g_xMapDescList := TStringList.Create;
  g_xCurMapDescList := TStringList.Create;

finalization
  Dispose(g_APPass);
  //DeleteCriticalSection(ProcImagesCS);
  DeleteCriticalSection(ProcMsgCS);
  DeleteCriticalSection(ThreadCS);
  g_APPickUpList.Free;
  g_APMobList.Free;
  g_ItemsFilter_All.Free;
  g_ItemsFilter_All_Def.Free;
  g_ItemsFilter_Dress.Free;
  g_ItemsFilter_Weapon.Free;
  g_ItemsFilter_Headgear.Free;
  g_ItemsFilter_Drug.Free;
  g_ItemsFilter_Other.Free;
  //g_VCLUnZip1.Free;
  g_SuiteItemsList.Free;
  g_pshine.Free;

  g_xMapDescList.Free;
  g_xCurMapDescList.Free;

end.




