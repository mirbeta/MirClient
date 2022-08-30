unit Share;

interface

uses Dialogs, Grobal2;

{$J+}
const
  SCREENWIDTH: Integer  = 1280;
  SCREENHEIGHT: Integer = 800;
  MAPSURFACEWIDTH: Integer  = 1280;
  MAPSURFACEHEIGHT: Integer = 800 - 155;
  WINRIGHT: Integer   = 1280 - 60;
  CHATBOXWIDTH: Integer = 1280 -447; // 41 聊天框文字宽度
  TIME_MOVEOBJECTS: Integer = 90;
  MAXX= 52;
  MAXY= 40;

  ResourceDir: String = 'Resource\';
  UserCfgDir: String = 'Resource\Users\';

  MapLeftUnit: Integer=0;
  MapTopUnit: Integer=0;
  MapRightUnit: Integer=0;
  MapBottomUnit: Integer=0;
  PixeXOffset: Integer = 0;
  PixeYOffset: Integer = 0;
  PixeXToMapXOffset: Integer=0;
  PixeYToMapYOffset: Integer=0;
  MapXToPixeXOffset: Integer=0;
  MapYToPixeYOffset: Integer=0;


  MAPDIR         = 'Map\'; // 地图文件所在目录

  MAINIMAGEFILE       = 'Data\Prguse.wil';
  MAINIMAGEFILE2      = 'Data\Prguse2.wil';
  MAINIMAGEFILE3      = 'Data\Prguse3.wil';
  EFFECTIMAGEDIR      = 'Data\';

  CHRSELIMAGEFILE    = 'Data\ChrSel.wil';
  MINMAPIMAGEFILE    = 'Data\mmap.wil';
  TITLESIMAGEFILE    = 'Data\Tiles.wil';
  SMLTITLESIMAGEFILE = 'Data\SmTiles.wil';
  UI1IMAGESFILE      = 'Data\Ui1.wil';
  UI2IMAGESFILE      = 'Data\Ui2.wil';
  UI3IMAGESFILE      = 'Data\Ui3.wil';
  MAGICONIMAGESFILE  = 'Data\MagIcon.wil';
  MAGICON2IMAGESFILE = 'Data\MagIcon2.wil';
  HAIRIMGIMAGESFILE  = 'Data\Hair.wil';
  HAIR2IMGIMAGESFILE = 'Data\Hair2.wil';
  NPCIMAGESFILE      = 'Data\Npc.wil';
  NPCIMAGESFILE2     = 'Data\Npc2.wil';
  ResNPCImageFile    = '%s\Data\Npc.wil';
  MAGICIMAGESFILE    = 'Data\Magic.wil';
  MAGIC2IMAGESFILE   = 'Data\Magic2.wil';
  MAGIC3IMAGESFILE   = 'Data\Magic3.wil';
  MAGIC4IMAGESFILE   = 'Data\Magic4.wil'; // 2007.10.28
  MAGIC5IMAGESFILE   = 'Data\Magic5.wil'; // 2007.11.29
  MAGIC6IMAGESFILE   = 'Data\Magic6.wil'; // 2007.11.29
  MAGIC7IMAGESFILE   = 'Data\Magic7.wil'; // 20100317
  MAGIC716IMAGESFILE   = 'Data\Magic7-16.wil'; // 20100317
  MAGIC8IMAGESFILE   = 'Data\Magic8.wil'; // 20100317
  MAGIC816IMAGESFILE   = 'Data\Magic8-16.wil'; // 20100317
  MAGIC9IMAGESFILE   = 'Data\Magic9.wil'; // 20100317
  MAGIC10IMAGESFILE   = 'Data\Magic10.wil'; // 20100317

  BAGITEMIMAGESFILE   = 'Data\Items.wil';
  STATEITEMIMAGESFILE = 'Data\StateItem.wil';
  DNITEMIMAGESFILE    = 'Data\DnItems.wil';

  OBJECTIMAGEFILE  = 'Data\Objects.wil';
  OBJECTIMAGEFILE1 = 'Data\Objects%d.wil';
  MONIMAGEFILE     = 'Data\Mon%d.wil';
  MONIMAGESFILE     = 'Data\%s.wil';
  ResMonImagFile   = '%sData\Mon%d.wil';
  DRAGONIMAGEFILE  = 'Data\Dragon.wil';
  EFFECTIMAGEFILE  = 'Data\Effect.wil';

  WEAPON2WISFILE    = 'Data\Weapon2.wis';
  MAINWISFILE       = 'Data\Prguse.wis';
  CBOHUMWISFILE     = 'Data\cboHum.wis';
  CBOHUMWIS3FILE     = 'Data\cboHum3.wis';
  CBOWINGWISFILE    = 'Data\cboEffect.wis';
  CBOHUMWINGWISFILE = 'Data\cboHumEffect.wis';
  CBOHUMWINGWIS2FILE = 'Data\cboHumEffect2.wis';
  CBOHUMWINGWIS3FILE = 'Data\cboHumEffect3.wis';
  CBOHAIRWISFILE    = 'Data\cboHair.wis';
  CBOWEAPONWISFILE  = 'Data\cboWeapon.wis';
  CBOWEAPONWIS3FILE  = 'Data\cboWeapon3.wis';
  {
    MAXX = 40;
    MAXY = 40;
  }

  DEFAULTCURSOR = 0; // 系统默认光标
  IMAGECURSOR   = 1; // 图形光标

  USECURSOR = DEFAULTCURSOR; // 使用什么类型的光标
  ENEMYCOLOR   = 69;

type
  TSceneLevel = (slNormal, slHigh);
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

  TMessageHandler = reference to procedure(AResult: Integer{TModalResult});
  TMessageDialogItem = record
    Text: String;
    Buttons: TMsgDlgButtons;
    Handler: TMessageHandler;
    Size: Integer;
  end;
  PTMessageDialogItem = ^TMessageDialogItem;

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

var
  g_MirStartupInfo: TMirStartupInfo;

procedure BuildMapOffsetUnit;
procedure ChangeClientSpeed(Value: Byte);

implementation

uses Math;

procedure BuildMapOffsetUnit;
begin
  MapLeftUnit :=  Ceil((SCREENWIDTH-UNITX) / 2 / UNITX);
  MapTopUnit  :=  Ceil((SCREENHEIGHT-UNITY) / 2 / UNITY);
  MapRightUnit := MapLeftUnit;
  MapBottomUnit := MapTopUnit;
  MapLeftUnit := MapLeftUnit + 1;
  MapTopUnit := MapTopUnit + 1;
  case SCREENWIDTH of
    800..1000:  MapRightUnit := MapRightUnit + 2;
  end;
  PixeXToMapXOffset := MapLeftUnit * UNITX;
  PixeYToMapYOffset := MapTopUnit * UNITY;
  MapXToPixeXOffset := PixeXToMapXOffset + 24 - 66;
  MapYToPixeYOffset := PixeYToMapYOffset - 16 - 64;
end;

procedure ChangeClientSpeed(Value: Byte);
begin
  if Value in [5 .. 15] then
    TIME_MOVEOBJECTS := 100 - (Value - 10) * 5;
end;

end.
