unit Grobal2;

interface

uses
  Windows, Classes, D7ScktComp;

resourcestring
  g_sSelToDBSKey            = 'LoadMsgFilterFile';

const
  //USEWHLIST                 = False;

  MapManagerHList           = True;
  VER_PATHMAP               = True;
  SETBACKDOOR               = False;

  SM_QUERYDYNCODE           = 41501;
  SM_SENDSERVERINFO         = 41502;
  SM_CMDRETINFO             = 41503;

  //N_SVR                     = 3;        //Monster Search (Master <> nil) ViewRange
  //MAPCELLFREEOBJTIME        = 60 * 1000;
  MapNameLen                = 16;
  ActorNameLen              = 14;
  GuildNameLen              = 20;

  SUIT_RATE_MAXHP           = 0;
  SUIT_RATE_MAXMP           = 1;
  SUIT_RATE_AC              = 2;
  SUIT_RATE_MAC             = 3;
  SUIT_RATE_DC              = 4;
  SUIT_RATE_MC              = 5;
  SUIT_RATE_SC              = 6;
  SUIT_RATE_HITPOINT        = 7;
  SUIT_RATE_SPEED           = 8;
  SUIT_RATE_ANTIMAG         = 9;
  SUIT_RATE_ANTIPOISON      = 10;
  SUIT_RATE_POISONRECOVER   = 11;
  SUIT_RATE_HPRECOVER       = 12;
  SUIT_RATE_MPRECOVER       = 13;

  VERDEMO                   = 0;
  VERFREE                   = 1;
  VERSTD                    = 2;
  VEROEM                    = 3;
  VERPRO                    = 4;
  VERENT                    = 5;
  SoftVersion               = VERPRO;

  DEMOCLIENT                = 0;
  DR_UP                     = 0;
  DR_UPRIGHT                = 1;
  DR_RIGHT                  = 2;
  DR_DOWNRIGHT              = 3;
  DR_DOWN                   = 4;
  DR_DOWNLEFT               = 5;
  DR_LEFT                   = 6;
  DR_UPLEFT                 = 7;

  U_DRESS                   = 0;
  U_WEAPON                  = 1;
  U_RIGHTHAND               = 2;
  U_NECKLACE                = 3;
  U_HELMET                  = 4;
  U_ARMRINGL                = 5;
  U_ARMRINGR                = 6;
  U_RINGL                   = 7;
  U_RINGR                   = 8;
  U_BUJUK                   = 9;
  U_BELT                    = 10;
  U_BOOTS                   = 11;
  U_CHARM                   = 12;
  U_HELMETEX                = 13;

  UNITX                     = 48;
  UNITY                     = 32;
  HALFX                     = 24;
  HALFY                     = 16;

  DEFBLOCKSIZE              = 16;
  BUFFERSIZE                = 16 * 1024;
  DATA_BUFSIZE              = 08 * 1024;
  MAXLEVEL                  = 500;
  GROUPMAX                  = 11;
  BAGGOLD                   = 5000000;
  BODYLUCKUNIT              = 10;
  MAX_STATUS_ATTRIBUTE      = 12;

  sGameLogMsg               = '%d'#9'%s'#9'%d'#9'%d'#9'%s'#9'%s'#9'%d'#9'%s'#9'%s';
  GAMELOGNUMBERBASE         = 100;
  GAMELOGBUYITEM            = GAMELOGNUMBERBASE + 1;

  POISON_DECHEALTH          = 0;
  POISON_DAMAGEARMOR        = 1;
  POISON_SHOCKED            = 2;        //BLUE
  POISON_DONTMOVE           = 3;        //YELLOW
  POISON_FREEZE             = 4;        //PURPLE
  POISON_STONE              = 5;
  POISON_PURPLE             = 6;
  //POISON_SHOCKED            = 7;

  STATE_STONE_MODE          = 7;
  STATE_TRANSPARENT         = 8;
  STATE_DEFENCEUP           = 9;
  STATE_MAGDEFENCEUP        = 10;
  STATE_BUBBLEDEFENCEUP     = 11;
  STATE_OPENHEATH           = 12;

  USERMODE_PLAYGAME         = 1;
  USERMODE_LOGIN            = 2;
  USERMODE_LOGOFF           = 3;
  USERMODE_NOTICE           = 4;

  RUNGATEMAX                = 20;
  // For Game Gate
  GM_OPEN                   = 1;
  GM_CLOSE                  = 2;
  GM_CHECKSERVER            = 3;        // Send check signal to Server
  GM_CHECKCLIENT            = 4;        // Send check signal to Client
  GM_DATA                   = 5;
  GM_SERVERUSERINDEX        = 6;
  GM_RECEIVE_OK             = 7;
  GM_TEST                   = 20;

  OS_MOVINGOBJECT           = 1;
  OS_ITEMOBJECT             = 2;
  OS_EVENTOBJECT            = 3;
  OS_GATEOBJECT             = 4;
  OS_SWITCHOBJECT           = 5;
  OS_MAPEVENT               = 6;
  OS_DOOR                   = 7;
  OS_ROON                   = 8;

  RC_PLAYOBJECT             = 1;
  RC_HERO                   = 60;
  RC_TRAINER                = 55;
  RC_MONSTER                = 80;
  RC_ANIMAL                 = 50;
  RC_NPC                    = 10;
  RC_PEACENPC               = 15;
  RC_GUARD                  = 11;
  RC_ARCHERGUARD            = 112;
  RC_MISSION                = 108;
  RC_MISSIONARCHER          = 109;

  RCC_USERHUMAN             = 0;
  RCC_GUARD                 = 11;
  RCC_ARCHERGUARD           = 112;
  RCC_MERCHANT              = 50;

  SM_OPENSESSION            = 100;
  SM_CLOSESESSION           = 101;
  CM_CLOSESESSION           = 102;

  SM_CERTIFICATION_SUCCESS  = 1;

  SM_PURSUEHIT              = 2;
  SM_SQUHIT                 = 3;
  SM_FIREHITREADY           = 4;
  SM_HORSERUN               = 5;
  SM_RUSH                   = 6;
  SM_RUSHKUNG               = 7;
  SM_FIREHIT                = 8;
  SM_BACKSTEP               = 9;
  SM_TURN                   = 10;
  SM_WALK                   = 11;
  SM_SITDOWN                = 12;
  SM_RUN                    = 13;
  SM_HIT                    = 14;
  SM_HEAVYHIT               = 15;
  SM_BIGHIT                 = 16;
  SM_SPELL                  = 17;
  SM_POWERHIT               = 18;
  SM_LONGHIT                = 19;
  SM_DIGUP                  = 20;
  SM_DIGDOWN                = 21;
  SM_FLYAXE                 = 22;
  SM_LIGHTING               = 23;
  SM_WIDEHIT                = 24;
  SM_HERO_LONGHIT           = 25;
  SM_HERO_LONGHIT2          = 26;

  SM_ALIVE                  = 27;
  SM_MOVEFAIL               = 28;
  SM_HIDE                   = 29;
  SM_DISAPPEAR              = 30;
  SM_STRUCK                 = 31;
  SM_DEATH                  = 32;
  SM_SKELETON               = 33;
  SM_NOWDEATH               = 34;

  SM_CRSHIT                 = 35;
  SM_TWNHIT                 = 36;
  SM_42                     = 37;
  SM_43                     = 38;

  SM_HEAR                   = 40;
  SM_FEATURECHANGED         = 41;
  SM_USERNAME               = 42;
  SM_WINEXP                 = 44;
  SM_LEVELUP                = 45;
  SM_DAYCHANGING            = 46;
  SM_LOGON                  = 50;
  SM_NEWMAP                 = 51;
  SM_ABILITY                = 52;
  SM_HEALTHSPELLCHANGED     = 53;
  SM_MAPDESCRIPTION         = 54;
  SM_GAMEGOLDNAME           = 55;

  SM_WWJATTACK              = 60;
  SM_WSJATTACK              = 61;
  SM_WTJATTACK              = 62;

  SM_81                     = 81;
  SM_82                     = 82;
  SM_83                     = 83;

  SM_SYSMESSAGE             = 100;
  SM_GROUPMESSAGE           = 101;
  SM_CRY                    = 102;
  SM_WHISPER                = 103;
  SM_GUILDMESSAGE           = 104;
  SM_INPOWERINFO            = 105;
  SM_INTERNALPOWER          = 106;
  SM_HEROINTERNALPOWER      = 107;
  SM_INTERNALPOWER2         = 108;
  SM_HEROINTERNALPOWER2     = 109;

  SM_SPELL2                 = 117;

  SM_ADDITEM                = 200;
  SM_BAGITEMS               = 201;
  SM_DELITEM                = 202;
  SM_UPDATEITEM             = 203;
  SM_ADDMAGIC               = 210;
  SM_SENDMYMAGIC            = 211;
  SM_DELMAGIC               = 212;
  SM_HERODELMAGIC           = 213;

  SM_NEEDPASSWORD           = 500;
  SM_CERTIFICATION_FAIL     = 501;
  SM_ID_NOTFOUND            = 502;
  SM_PASSWD_FAIL            = 503;
  SM_NEWID_SUCCESS          = 504;
  SM_NEWID_FAIL             = 505;
  SM_CHGPASSWD_SUCCESS      = 506;
  SM_CHGPASSWD_FAIL         = 507;
  SM_GETBACKPASSWD_SUCCESS  = 508;
  SM_GETBACKPASSWD_FAIL     = 509;

  SM_CHANGENAME             = 517;
  SM_QUERYDELCHR            = 518;
  SM_GETBACKDELCHR          = 519;
  SM_QUERYCHR               = 520;
  SM_NEWCHR_SUCCESS         = 521;
  SM_NEWCHR_FAIL            = 522;
  SM_DELCHR_SUCCESS         = 523;
  SM_DELCHR_FAIL            = 524;
  SM_STARTPLAY              = 525;
  SM_STARTFAIL              = 526;
  SM_QUERYCHR_FAIL          = 527;
  SM_OUTOFCONNECTION        = 528;
  SM_PASSOK_SELECTSERVER    = 529;
  SM_SELECTSERVER_OK        = 530;
  SM_NEEDUPDATE_ACCOUNT     = 531;
  SM_UPDATEID_SUCCESS       = 532;
  SM_UPDATEID_FAIL          = 533;
  SM_SETTARGETXY            = 534;
  SM_REFDIAMOND             = 535;
  SM_QUERYYBSELL_SELL       = 536;
  SM_QUERYYBSELL_DEAL       = 537;
  SM_OPENDEAL_FAIL          = 538;

  SM_CANCELYBSELL_FAIL      = 540;
  SM_AFFIRMYBDEA_FAIL       = 541;

  SM_POST_FAIL1             = 543;
  SM_POST_FAIL2             = 544;
  SM_ATTACKMODE             = 545;
  SM_FIREWORKS              = 554;

  SM_DROPITEM_SUCCESS       = 600;
  SM_DROPITEM_FAIL          = 601;

  SM_ITEMSHOW               = 610;
  SM_ITEMHIDE               = 611;
  SM_OPENDOOR_OK            = 612;
  SM_OPENDOOR_LOCK          = 613;
  SM_CLOSEDOOR              = 614;
  SM_TAKEON_OK              = 615;
  SM_TAKEON_FAIL            = 616;
  SM_TAKEOFF_OK             = 619;
  SM_TAKEOFF_FAIL           = 620;
  SM_SENDUSEITEMS           = 621;
  SM_WEIGHTCHANGED          = 622;
  SM_QUERYITEMDLG           = 623;
  SM_ITEMDLGSELECT          = 624;

  SM_CLEAROBJECTS           = 633;
  SM_CHANGEMAP              = 634;
  SM_EAT_OK                 = 635;
  SM_EAT_FAIL               = 636;
  SM_BUTCH                  = 637;
  SM_MAGICFIRE              = 638;
  SM_MAGICFIRE_FAIL         = 639;
  SM_MAGIC_LVEXP            = 640;
  SM_DURACHANGE             = 642;
  SM_MERCHANTSAY            = 643;
  SM_MERCHANTDLGCLOSE       = 644;
  SM_SENDGOODSLIST          = 645;
  SM_SENDUSERSELL           = 646;
  SM_SENDBUYPRICE           = 647;
  SM_USERSELLITEM_OK        = 648;
  SM_USERSELLITEM_FAIL      = 649;
  SM_BUYITEM_SUCCESS        = 650;
  SM_BUYITEM_FAIL           = 651;
  SM_SENDDETAILGOODSLIST    = 652;
  SM_GOLDCHANGED            = 653;
  SM_CHANGELIGHT            = 654;
  SM_LAMPCHANGEDURA         = 655;
  SM_CHANGENAMECOLOR        = 656;
  SM_CHARSTATUSCHANGED      = 657;
  SM_SENDNOTICE             = 658;
  SM_GROUPMODECHANGED       = 659;
  SM_CREATEGROUP_OK         = 660;
  SM_CREATEGROUP_FAIL       = 661;
  SM_GROUPADDMEM_OK         = 662;
  SM_GROUPDELMEM_OK         = 663;
  SM_GROUPADDMEM_FAIL       = 664;
  SM_GROUPDELMEM_FAIL       = 665;
  SM_GROUPCANCEL            = 666;
  SM_GROUPMEMBERS           = 667;
  SM_SENDUSERREPAIR         = 668;
  SM_USERREPAIRITEM_OK      = 669;
  SM_USERREPAIRITEM_FAIL    = 670;
  SM_SENDREPAIRCOST         = 671;
  SM_QUERYREFINEITEM        = 672;
  SM_DEALMENU               = 673;
  SM_DEALTRY_FAIL           = 674;
  SM_DEALADDITEM_OK         = 675;
  SM_DEALADDITEM_FAIL       = 676;
  SM_DEALDELITEM_OK         = 677;
  SM_DEALDELITEM_FAIL       = 678;
  SM_DEALCANCEL             = 681;
  SM_DEALREMOTEADDITEM      = 682;
  SM_DEALREMOTEDELITEM      = 683;
  SM_DEALCHGGOLD_OK         = 684;
  SM_DEALCHGGOLD_FAIL       = 685;
  SM_DEALREMOTECHGGOLD      = 686;
  SM_DEALSUCCESS            = 687;
  SM_SENDUSERSTORAGEITEM    = 700;
  SM_STORAGE_OK             = 701;
  SM_STORAGE_FULL           = 702;
  SM_STORAGE_FAIL           = 703;
  SM_SAVEITEMLIST           = 704;
  SM_TAKEBACKSTORAGEITEM_OK = 705;
  SM_TAKEBACKSTORAGEITEM_FAIL = 706;
  SM_TAKEBACKSTORAGEITEM_FULLBAG = 707;
  SM_AREASTATE              = 708;
  SM_DELITEMS               = 709;
  SM_READMINIMAP_OK         = 710;
  SM_READMINIMAP_FAIL       = 711;
  SM_SENDUSERMAKEDRUGITEMLIST = 712;
  SM_MAKEDRUG_SUCCESS       = 713;
  SM_HEROLAMPCHANGEDURA     = 714;

  SM_MONHIT                 = 716;

  SM_MAKEDRUG_FAIL          = 65036;

  SM_CHANGEGUILDNAME        = 750;
  SM_SENDUSERSTATE          = 751;
  SM_SUBABILITY             = 752;
  SM_OPENGUILDDLG           = 753;
  SM_OPENGUILDDLG_FAIL      = 754;
  SM_SENDGUILDMEMBERLIST    = 756;
  SM_GUILDADDMEMBER_OK      = 757;
  SM_GUILDADDMEMBER_FAIL    = 758;
  SM_GUILDDELMEMBER_OK      = 759;
  SM_GUILDDELMEMBER_FAIL    = 760;
  SM_GUILDRANKUPDATE_FAIL   = 761;
  SM_BUILDGUILD_OK          = 762;
  SM_BUILDGUILD_FAIL        = 763;
  SM_DONATE_OK              = 764;
  SM_DONATE_FAIL            = 765;
  SM_MYSTATUS               = 766;
  SM_MENU_OK                = 767;
  SM_GUILDMAKEALLY_OK       = 768;
  SM_GUILDMAKEALLY_FAIL     = 769;
  SM_GUILDBREAKALLY_OK      = 770;
  SM_GUILDBREAKALLY_FAIL    = 771;
  SM_DLGMSG                 = 772;
  SM_CREATEHERO_INFO        = 773;

  SM_SPACEMOVE_HIDE         = 800;
  SM_SPACEMOVE_SHOW         = 801;
  SM_RECONNECT              = 802;
  SM_GHOST                  = 803;
  SM_SHOWEVENT              = 804;
  SM_HIDEEVENT              = 805;
  SM_SPACEMOVE_HIDE2        = 806;
  SM_SPACEMOVE_SHOW2        = 807;
  SM_TIMECHECK_MSG          = 810;
  SM_ADJUST_BONUS           = 811;
  SM_SPECOFFERITEM          = 812;
  SM_PRESENDITEMFAIL        = 813;
  SM_HEROEXCHGBAGITEM_FAIL  = 814;
  SM_OFFERITEM              = 815;
  SM_BUGITEMFAIL            = 816;
  SM_ADDITEMTOHEROBAG       = 817;
  SM_ADDITEMTOHEROBAG1      = 818;
  SM_GETITEMFROMHEROBAG     = 819;
  SM_GETITEMFROMHEROBAG1    = 820;

  // Maket system ---------------------
  SM_MARKET_LIST            = 828;
  SM_MARKET_RESULT          = 829;

  SM_HEROLOGOUT             = 896;
  SM_HEROLOGIN              = 897;
  SM_HERONAME               = 898;
  SM_HEROSTATE              = 899;
  SM_HEROABILITY            = 900;
  SM_HEROSUBABILITY         = 901;
  SM_HEROBAGITEMS           = 902;
  SM_HEROUSEITEMS           = 903;
  SM_HEROMYMAGICS           = 904;
  SM_HEROADDITEM            = 905;
  SM_HERODELITEM            = 906;
  SM_HEROTAKEON_OK          = 907;
  SM_HEROTAKEON_FAIL        = 908;
  SM_HEROTAKEOFF_OK         = 909;
  SM_HEROTAKEOFF_FAIL       = 910;
  SM_HEROEAT_OK             = 911;
  SM_HEROEAT_FAIL           = 912;
  SM_HEROADDMAGIC           = 913;
  SM_HEROLEVELUP            = 914;
  SM_HEROWINEXP             = 915;
  SM_HEROMAGIC_LVEXP        = 916;
  SM_HERODELITEMS           = 917;
  SM_HEROSTATEDISPEAR       = 918;
  SM_HERODURACHANGE         = 919;
  SM_HERODROPITEM_SUCCESS   = 920;
  SM_HERODROPITEM_FAIL      = 921;
  SM_HEROUPDATEITEM         = 922;
  SM_HEROPOWERUP            = 923;
  SM_HEROSPELL              = 924;

  SM_OPENBOX                = 950;
  SM_OPENBOX_FAIL           = 951;
  SM_SELETEBOXFLASH         = 952;
  SM_CLOSEBOX               = 953;

  SM_SQUAREPOWERUP          = 958;

  CM_OPENBOX                = 1080;
  CM_SELETEBOXFLASH         = 1081;
  CM_GETBOXITEM             = 1082;

  SM_OPENHEALTH             = 1100;
  SM_CLOSEHEALTH            = 1101;
  SM_BREAKWEAPON            = 1102;
  SM_INSTANCEHEALGUAGE      = 1103;
  SM_CHANGEFACE             = 1104;
  SM_PASSWORD               = 1105;
  SM_VERSION_FAIL           = 1106;
  SM_STRUCKEFFECT           = 1107;
  SM_LEVELRANK              = 1108;
  //1109 ×°±¸°ó¶¨
  SM_ITEMUPDATE             = 1500;
  SM_MONSTERSAY             = 1501;

  SM_PLAYDICE               = 1200;

  SM_ITEMLOCK               = 2811;
  SM_BOOK                   = 2812;
  SM_MSG001                 = 2813;
  SM_SHELLEXECUTE           = 2814;

  SM_HEROLOYALTY            = 45000;
  SM_WINIPEXP               = 45001;
  SM_HEROWINIPEXP           = 45002;

  SM_REMOTEMSG              = 60004;
  SM_TEST                   = 65037;
  SM_THROW                  = 65069;

  RM_DELITEMS               = 9000;
  RM_TURN                   = 10001;
  RM_WALK                   = 10002;
  RM_RUN                    = 10003;
  RM_HIT                    = 10004;
  RM_HEAVYHIT               = 10005;
  RM_BIGHIT                 = 10006;
  RM_SPELL                  = 10007;
  RM_SPELL2                 = 10008;
  RM_POWERHIT               = 10009;
  RM_MOVEFAIL               = 10010;
  RM_LONGHIT                = 10011;
  RM_WIDEHIT                = 10012;
  RM_PUSH                   = 10013;
  RM_FIREHIT                = 10014;
  RM_RUSH                   = 10015;
  RM_HERO_LONGHIT           = 10016;
  RM_HERO_LONGHIT2          = 10017;
  RM_MONHIT                 = 10018;
  RM_HEROSPELL              = 10019;
  RM_STRUCK                 = 10020;
  RM_DEATH                  = 10021;
  RM_DISAPPEAR              = 10022;
  RM_FIREHITREADY           = 10023;
  RM_SKELETON               = 10024;
  RM_MAGSTRUCK              = 10025;
  RM_MAGHEALING             = 10026;
  RM_STRUCK_MAG             = 10027;
  RM_MAGSTRUCK_MINE         = 10028;
  RM_INSTANCEHEALGUAGE      = 10029;
  RM_HEAR                   = 10030;
  RM_WHISPER                = 10031;
  RM_CRY                    = 10032;
  RM_RIDE                   = 10033;
  RM_SENDUSERREPAIR         = 10034;
  RM_SENDUSERSREPAIR        = 10035;
  RM_CRSHIT                 = 10036;
  RM_RUSHKUNG               = 10037;
  RM_SENDREGINFO            = 10038;
  RM_PURSUEHIT              = 10039;
  RM_INPOWERINFO            = 10040;

  RM_USERNAME               = 10043;
  RM_WINEXP                 = 10044;
  RM_LEVELUP                = 10045;
  RM_CHANGENAMECOLOR        = 10046;
  RM_REMOTEMSG              = 10047;
  RM_SELLOFF                = 10048;
  RM_BUYOFF                 = 10049;
  RM_LOGON                  = 10050;
  RM_ABILITY                = 10051;
  RM_HEALTHSPELLCHANGED     = 10052;
  RM_DAYCHANGING            = 10053;
  RM_HORSERUN               = 10054;
  RM_GETSELLITEMSLIST       = 10055;
  RM_HEROWINEXP             = 10056;
  RM_HEROLEVELUP            = 10057;
  RM_HEROUSERNAME           = 10058;
  RM_JOINTATTACK            = 10059;
  RM_STRUCKEFFECTEX         = 10060;
  RM_81                     = 10061;
  RM_82                     = 10062;
  RM_83                     = 10063;
  RM_HEROWINIPEXP           = 10064;
  RM_WINIPEXP               = 10065;
  RM_INTERNALPOWER          = 10066;

  RM_SYSMESSAGE             = 10100;
  RM_STRUCKEFFECT           = 10101;
  RM_GROUPMESSAGE           = 10102;
  RM_SYSMESSAGE2            = 10103;
  RM_GUILDMESSAGE           = 10104;
  RM_SYSMESSAGE3            = 10105;
  RM_ITEMSHOW               = 10110;
  RM_ITEMHIDE               = 10111;
  RM_DOOROPEN               = 10112;
  RM_DOORCLOSE              = 10113;
  RM_SENDUSEITEMS           = 10114;
  RM_WEIGHTCHANGED          = 10115;
  RM_FEATURECHANGED         = 10116;
  RM_CLEAROBJECTS           = 10117;
  RM_CHANGEMAP              = 10118;
  RM_BUTCH                  = 10119;
  RM_MAGICFIRE              = 10120;
  RM_SENDMYMAGIC            = 10122;
  RM_MAGIC_LVEXP            = 10123;
  RM_DURACHANGE             = 10125;
  RM_MERCHANTSAY            = 10126;
  RM_MERCHANTDLGCLOSE       = 10127;
  RM_SENDGOODSLIST          = 10128;
  RM_SENDUSERSELL           = 10129;
  RM_SENDBUYPRICE           = 10130;
  RM_USERSELLITEM_OK        = 10131;
  RM_USERSELLITEM_FAIL      = 10132;
  RM_BUYITEM_SUCCESS        = 10133;
  RM_BUYITEM_FAIL           = 10134;
  RM_SENDDETAILGOODSLIST    = 10135;
  RM_GOLDCHANGED            = 10136;
  RM_CHANGELIGHT            = 10137;
  RM_LAMPCHANGEDURA         = 10138;
  RM_CHARSTATUSCHANGED      = 10139;
  RM_GROUPCANCEL            = 10140;
  RM_SENDREPAIRCOST         = 10142;
  RM_USERREPAIRITEM_OK      = 10143;
  RM_USERREPAIRITEM_FAIL    = 10144;
  RM_USERSTORAGEITEM        = 10146;
  RM_USERGETBACKITEM        = 10147;
  RM_SENDDELITEMLIST        = 10148;
  RM_USERMAKEDRUGITEMLIST   = 10149;
  RM_MAKEDRUG_SUCCESS       = 10150;
  RM_MAKEDRUG_FAIL          = 10151;
  RM_GETSALELIST            = 10152;
  RM_ALIVE                  = 10153;
  RM_DELAYMAGIC             = 10154;
  RM_MAPRANDOMMOVE          = 10155;
  RM_MAGICFIREFAIL          = 10156;
  RM_HEROLOGOUT             = 10157;
  RM_HEROLOGIN              = 10158;
  RM_HEROSTATE              = 10159;
  RM_HERONAME               = 10160;
  RM_HERODISPEAR            = 10161;
  RM_HEROMAGIC_LVEXP        = 10162;
  RM_FIREWORKS              = 10163;
  RM_DRAGONDURACHANGE       = 10164;
  RM_DELAYINCHEALTHSPELL    = 10165;
  RM_DELAYMAGICEX           = 10166;

  RM_DIGUP                  = 10200;
  RM_DIGDOWN                = 10201;
  RM_FLYAXE                 = 10202;
  RM_LIGHTING               = 10204;

  RM_POISON                 = 10300;
  RM_CHANGEGUILDNAME        = 10301;
  RM_SUBABILITY             = 10302;
  RM_BUILDGUILD_OK          = 10303;
  RM_BUILDGUILD_FAIL        = 10304;
  RM_DONATE_OK              = 10305;
  RM_DONATE_FAIL            = 10306;

  RM_TRANSPARENT            = 10308;
  RM_MENU_OK                = 10309;

  RM_ADJUST_BONUS           = 10400;
  RM_10401                  = 10401;
  RM_PLAYDICE               = 10500;

  RM_GAMEGOLDCHANGED        = 10666;
  RM_MYSTATUS               = 10777;
  RM_SPACEMOVE_FIRE2        = 10330;
  RM_SPACEMOVE_FIRE         = 10331;
  RM_DELAYPUSHED            = 10555;
  RM_DELAYPUSHED1           = 10556;
  RM_10414                  = 10414;

  RM_RECONNECTION           = 11332;
  RM_SPACEMOVE_SHOW         = 11331;
  RM_SPACEMOVE_SHOW2        = 10332;
  RM_HIDEEVENT              = 10333;
  RM_SHOWEVENT              = 10334;
  RM_ZEN_BEE                = 10337;

  RM_OPENHEALTH             = 10410;
  RM_CLOSEHEALTH            = 10411;
  RM_DOOPENHEALTH           = 10412;
  RM_BREAKWEAPON            = 10413;
  RM_CHANGEFACE             = 10415;
  RM_PASSWORD               = 10416;

  RM_ITEMUPDATE             = 11000;
  RM_MONSTERSAY             = 11001;
  RM_MAKESLAVE              = 11002;

  RM_COUNTERITEMCHANGE      = 11011;
  RM_USERSELLCOUNTITEM_OK   = 11012;
  RM_USERSELLCOUNTITEM_FAIL = 11013;
  RM_SENDUSERMAKEFOODLIST   = 11014;
  RM_MARKET_LIST            = 11015;
  RM_MARKET_RESULT          = 11016;

  RM_PASSWORDSTATUS         = 20001;
  SM_PASSWORDSTATUS         = 20001;

  SM_ACTION_MIN             = SM_PURSUEHIT;
  SM_ACTION_MAX             = SM_HERO_LONGHIT2;
  SM_ACTION2_MIN            = 35;
  SM_ACTION2_MAX            = 100;

  RUNGATECODE               = $AA55AA55 + $00450045;

  SS_LOGINCOST              = 3333;
  SS_OPENSESSION            = 1000;
  SS_CLOSESESSION           = 1001;
  SS_SOFTOUTSESSION         = 1002;
  SS_SERVERINFO             = 1003;
  SS_KEEPALIVE              = 1004;
  SS_110                    = 1005;
  SS_KICKUSER               = 1006;
  SS_SERVERLOAD             = 1007;
  UNKNOWMSG                 = 1008;
  SS_CLOSESESSIONA          = 1009;

  SLAVEMAXLEVEL             = 8;
  sSTRING_GOLDNAME          = '½ð±Ò';
  PN_GETRGB                 = 'GetRgb';
  PN_GAMEDATALOG            = 'GAMEDATALOG';
  PN_SENDBROADCASTMSG       = 'SENDBROADCASTMSG';
  sENCYPTSCRIPTFLAG         = 'ENCYPTSCRIPTFLAG';

  LA_UNDEAD                 = 1;
  SKILL_FIREBALL            = 1;
  SKILL_HEALLING            = 2;
  SKILL_ONESWORD            = 3;
  SKILL_ILKWANG             = 4;
  SKILL_FIREBALL2           = 5;
  SKILL_AMYOUNSUL           = 6;
  SKILL_YEDO                = 7;
  SKILL_FIREWIND            = 8;
  SKILL_FIRE                = 9;
  SKILL_SHOOTLIGHTEN        = 10;
  SKILL_LIGHTENING          = 11;
  SKILL_ERGUM               = 12;
  SKILL_FIRECHARM           = 13;
  SKILL_HANGMAJINBUB        = 14;
  SKILL_DEJIWONHO           = 15;
  SKILL_HOLYSHIELD          = 16;
  SKILL_SKELLETON           = 17;
  SKILL_CLOAK               = 18;
  SKILL_BIGCLOAK            = 19;
  SKILL_TAMMING             = 20;
  SKILL_SPACEMOVE           = 21;
  SKILL_EARTHFIRE           = 22;
  SKILL_FIREBOOM            = 23;
  SKILL_LIGHTFLOWER         = 24;
  SKILL_BANWOL              = 25;
  SKILL_FIRESWORD           = 26;
  SKILL_MOOTEBO             = 27;
  SKILL_SHOWHP              = 28;
  SKILL_BIGHEALLING         = 29;
  SKILL_SINSU               = 30;
  SKILL_SHIELD              = 31;
  SKILL_KILLUNDEAD          = 32;
  SKILL_SNOWWIND            = 33;
  SKILL_UNAMYOUNSUL         = 34;
  SKILL_WINDTEBO            = 35;
  SKILL_MABE                = 36;
  SKILL_GROUPLIGHTENING     = 37;
  SKILL_GROUPAMYOUNSUL      = 38;
  SKILL_GROUPDEDING         = 39;
  SKILL_40                  = 40;
  SKILL_41                  = 41;
  SKILL_42                  = 42;
  SKILL_43                  = 43;
  SKILL_44                  = 44;
  SKILL_45                  = 45;
  SKILL_46                  = 46;
  SKILL_47                  = 47;
  SKILL_48                  = 48;
  SKILL_49                  = 49;
  SKILL_50                  = 50;
  SKILL_51                  = 51;
  SKILL_52                  = 52;
  SKILL_53                  = 53;
  SKILL_54                  = 54;
  SKILL_55                  = 55;
  SKILL_56                  = 56;
  SKILL_57                  = 57;
  SKILL_58                  = 58;
  SKILL_59                  = 59;

  ET_DIGOUTZOMBI            = 1;
  ET_PILESTONES             = 3;
  ET_HOLYCURTAIN            = 4;
  ET_FIRE                   = 5;
  ET_SCULPEICE              = 6;

  DB_LOADHUMANRCD           = 100;
  DB_SAVEHUMANRCD           = 101;
  DB_NEWHERO                = 102;
  DB_SAVEHUMANRCDEX         = 103;
  DB_GETREMOVEDIP           = 104;
  DB_LOADRANKDATA           = 105;

  DB_MAKEITEMRCD            = 150;
  DB_ITEMTHROW              = 151;
  DB_MAKEITEMRCD2           = 152;
  ET_STONEMINE              = 1;

  DBR_LOADHUMANRCD          = 1200;
  DBR_LOADHUMANRCD2         = 1201;
  DBR_NEWHERO               = 1203;
  DBR_GETREMOVEDIP          = 1204;
  DBR_LOADRANKDATA          = 1205;

  DBR_MAKEITEMRCD           = 1300;
  DBR_MAKEITEMRCD2          = 1301;
  DBR_SAVEHUMANRCD          = 1400;
  DBR_FAIL                  = 2000;

  DBR_LOADRCDFLAG           = 8;
  DBR_SAVERCDFLAG           = 5;
  DBR_NEWHEROFLAG           = 415;

  CLIENTVERNO               = 10;

  CM_QUERYUSERNAME          = 80;
  CM_QUERYBAGITEMS          = 81;
  CM_QUERYUSERSTATE         = 82;
  CM_QUERYHEROBAGITEMS      = 83;

  CM_QUERYCHR               = 100;
  CM_NEWCHR                 = 101;
  CM_DELCHR                 = 102;
  CM_SELCHR                 = 103;
  CM_SELECTSERVER           = 104;
  CM_QUERYDELCHR            = 105;
  CM_GETBACKDELCHR          = 106;

  CM_FASTLOOP               = 140;
  CM_DBSTEST                = 141;
  CM_DBSQUESTINFO           = 142;
  CM_REMOVEDCMD             = 143;

  CM_DROPITEM               = 1000;
  CM_PICKUP                 = 1001;
  CM_OPENDOOR               = 1002;
  CM_TAKEONITEM             = 1003;
  CM_TAKEOFFITEM            = 1004;
  CM_HEROMAGICKEYCHANGE     = 1005;
  CM_EAT                    = 1006;
  CM_BUTCH                  = 1007;
  CM_MAGICKEYCHANGE         = 1008;
  CM_SOFTCLOSE              = 1009;
  CM_CLICKNPC               = 1010;
  CM_MERCHANTDLGSELECT      = 1011;
  CM_MERCHANTQUERYSELLPRICE = 1012;
  CM_USERSELLITEM           = 1013;
  CM_USERBUYITEM            = 1014;
  CM_USERGETDETAILITEM      = 1015;
  CM_DROPGOLD               = 1016;
  CM_LOGINNOTICEOK          = 1018;
  CM_GROUPMODE              = 1019;
  CM_CREATEGROUP            = 1020;
  CM_ADDGROUPMEMBER         = 1021;
  CM_DELGROUPMEMBER         = 1022;
  CM_USERREPAIRITEM         = 1023;
  CM_MERCHANTQUERYREPAIRCOST = 1024;
  CM_DEALTRY                = 1025;
  CM_DEALADDITEM            = 1026;
  CM_DEALDELITEM            = 1027;
  CM_DEALCANCEL             = 1028;
  CM_DEALCHGGOLD            = 1029;
  CM_DEALEND                = 1030;
  CM_USERSTORAGEITEM        = 1031;
  CM_USERTAKEBACKSTORAGEITEM = 1032;
  CM_WANTMINIMAP            = 1033;
  CM_USERMAKEDRUGITEM       = 1034;
  CM_OPENGUILDDLG           = 1035;
  CM_GUILDHOME              = 1036;
  CM_GUILDMEMBERLIST        = 1037;
  CM_GUILDADDMEMBER         = 1038;
  CM_GUILDDELMEMBER         = 1039;
  CM_GUILDUPDATENOTICE      = 1040;
  CM_GUILDUPDATERANKINFO    = 1041;
  CM_POSTSELL               = 1042;
  CM_ADJUST_BONUS           = 1043;
  CM_GUILDALLY              = 1044;
  CM_GUILDBREAKALLY         = 1045;
  CM_GETSHOPITEM            = 1046;
  CM_ITEMDLGSELECT          = 1047;
  CM_BUYSHOPITEM            = 1048;
  CM_SHOPPRESEND            = 1049;
  CM_RECALLHERO             = 1050;
  CM_UNRECALLHERO           = 1051;

  CM_CANCELYBDEAL           = 1056;
  CM_CANCELYBSELL           = 1057;
  CM_AFFIRMYBDEAL           = 1058;
  CM_LEVELRANK              = 1060;
  CM_REFINEITEM             = 1061;     //´ãÁ¶
  CM_REFINEITEM_FAIL        = 1062;

  CM_PLAYERADDITEMTOHERO    = 1100;
  CM_HEROADDITEMTOPLAYER    = 1101;
  CM_HEROTAKEONITEM         = 1102;
  CM_HEROTAKEOFFITEM        = 1103;
  CM_HEROEAT                = 1104;
  CM_HEROSETTARGET          = 1105;
  CM_HERODROPITEM           = 1106;
  CM_HERORECALLSLAVE        = 1107;
  CM_HERORJOINTATTACK       = 1108;

  CM_SPEEDHACKUSER          = 10430;
  CM_HEROSIDESTEP           = 10431;

  CM_PROTOCOL               = 2000;
  CM_IDPASSWORD             = 2001;
  CM_ADDNEWUSER             = 2002;
  CM_CHANGEPASSWORD         = 2003;
  CM_UPDATEUSER             = 2004;
  CM_GETBACKPASSWORD        = 2010;

  CM_PURSUEHIT              = 3002;
  CM_SQUHIT                 = 3003;
  CM_THROW                  = 3005;
  CM_HORSERUN               = 3009;
  CM_TURN                   = 3010;
  CM_WALK                   = 3011;
  CM_SITDOWN                = 3012;
  CM_RUN                    = 3013;

  CM_HIT                    = 3014;
  CM_HEAVYHIT               = 3015;
  CM_BIGHIT                 = 3016;
  CM_SPELL                  = 3017;
  CM_POWERHIT               = 3018;
  CM_LONGHIT                = 3019;

  CM_WIDEHIT                = 3024;
  CM_FIREHIT                = 3025;
  CM_40HIT                  = 3026;
  CM_41HIT                  = 3027;
  CM_43HIT                  = 3028;
  CM_42HIT                  = 3029;
  CM_SAY                    = 3030;

  CM_CRSHIT                 = 3035;
  CM_TWNHIT                 = 3036;

  CM_SPEEDHACKMSG           = 3500;
  CM_QUERYDYNCODE           = 3501;

  CM_GETSALELIST            = 8001;
  CM_BUYSALEITEM            = 8002;
  SM_GETSALELIST            = 9001;

  RM_TWNHIT                 = 41;
  RM_SUQHIT                 = 42;
  RM_43                     = 43;
  RM_60                     = 60;
  RM_61                     = 61;
  RM_62                     = 62;

  CM_PASSWORD               = 33333;
  CM_CHGPASSWORD            = 44444;
  CM_SETPASSWORD            = 55555;
  CM_QUERYUSERSET           = 49999;
  CM_POWERBLOCK             = 49998;
  CM_GETREGINFO             = 49997;
  CM_1017                   = 1017;
  LOG_GAMEGOLD              = 2000;
  LOG_GAMEPOINT             = 2000;
  MAXBAGITEM                = 46;
  vNone                     = 0;
  vInteger                  = 1;
  vString                   = 2;
  nInternet                 = 3;
  sSTATUS_FAIL              = '+FL';
  sSTATUS_GOOD              = '+GD';
  //sSTATUS_FAIL              = '+FAIL/';
  //sSTATUS_GOOD              = '+GOOD/';

  SM_SERVERCONFIG           = 20002;
  SM_RUNHUMAN               = 20003;
  SM_SellOff                = 20005;
  RM_SELLOFFERROR           = 20006;
  SM_SELLOFFERROR           = 20007;
  SM_BuyOff                 = 20008;
  SM_BUYITEM                = 20009;

  CM_SELLOFF                = 4004;
  CM_BUYSELLOFFITEM         = 4005;
  CM_GETSALEDETAILITEM      = 4006;
  TiShenItemIndex           = 80;

  CM_USERBASE               = 8000;
  SM_USERBASE               = 9000;
  RM_USERBASE               = 61000;
  SM_GETID_FAIL             = 61001;

  ISM_PASSWDSUCCESS         = 100;      //ÆÐ½º¿öµå Åë°ú, Certification+ID
  ISM_CANCELADMISSION       = 101;      //Certification ½ÂÀÎÃë¼Ò..
  ISM_USERCLOSED            = 102;      //»ç¿ëÀÚ Á¢¼Ó ²÷À½
  ISM_USERCOUNT             = 103;      //ÀÌ ¼­¹öÀÇ »ç¿ëÀÚ ¼ö
  ISM_TOTALUSERCOUNT        = 104;
  ISM_SHIFTVENTURESERVER    = 110;
  ISM_ACCOUNTEXPIRED        = 111;
  ISM_GAMETIMEOFTIMECARDUSER = 112;
  ISM_USAGEINFORMATION      = 113;
  ISM_FUNC_USEROPEN         = 114;
  ISM_FUNC_USERCLOSE        = 115;
  ISM_CHECKTIMEACCOUNT      = 116;
  ISM_REQUEST_PUBLICKEY     = 117;
  ISM_SEND_PUBLICKEY        = 118;
  ISM_PREMIUMCHECK          = 119;
  ISM_EVENTCHECK            = 120;

  {-------Inter Server Msg-------}
  ISM_USERSERVERCHANGE      = 200;
  ISM_USERLOGON             = 201;
  ISM_USERLOGOUT            = 202;
  ISM_WHISPER               = 203;
  ISM_SYSOPMSG              = 204;
  ISM_ADDGUILD              = 205;
  ISM_DELGUILD              = 206;
  ISM_RELOADGUILD           = 207;
  ISM_GUILDMSG              = 208;
  ISM_CHATPROHIBITION       = 209;
  ISM_CHATPROHIBITIONCANCEL = 210;
  ISM_CHANGECASTLEOWNER     = 211;
  ISM_RELOADCASTLEINFO      = 212;
  ISM_RELOADADMIN           = 213;

  // Friend System -------------
  ISM_FRIEND_INFO           = 214;
  ISM_FRIEND_DELETE         = 215;
  ISM_FRIEND_OPEN           = 216;
  ISM_FRIEND_CLOSE          = 217;
  ISM_FRIEND_RESULT         = 218;

  //Tag System ----------------
  ISM_TAG_SEND              = 219;
  ISM_TAG_RESULT            = 220;

  //User System --------------
  ISM_USER_INFO             = 221;
  ISM_CHANGESERVERRECIEVEOK = 222;
  ISM_RELOADCHATLOG         = 223;
  ISM_MARKETOPEN            = 224;
  ISM_MARKETCLOSE           = 225;

  //relationship --------------
  ISM_LM_DELETE             = 226;

  ISM_RELOADMAKEITEMLIST    = 227;

  ISM_GUILDMEMBER_RECALL    = 228;
  ISM_RELOADGUILDAGIT       = 229;

  ISM_LM_WHISPER            = 230;
  ISM_GMWHISPER             = 231;
  ISM_LM_LOGIN              = 232;
  ISM_LM_LOGOUT             = 233;
  ISM_REQUEST_RECALL        = 234;
  ISM_RECALL                = 235;
  ISM_LM_LOGIN_REPLY        = 236;
  ISM_LM_KILLED_MSG         = 237;
  ISM_REQUEST_LOVERRECALL   = 238;

  ISM_STANDARDTICKREQ       = 239;
  ISM_STANDARDTICK          = 240;
  ISM_GUILDWAR              = 241;

  //DB_LOADHUMANRCD           = 100;
  //DB_SAVEHUMANRCD           = 101;
  DB_SAVEANDCHANGE          = 102;
  DB_IDPASSWD               = 103;
  DB_NEWUSERID              = 104;
  DB_CHANGEPASSWD           = 105;
  DB_QUERYCHR               = 106;
  DB_NEWCHR                 = 107;
  DB_GETOTHERNAMES          = 108;
  DB_ISVALIDUSER            = 111;
  DB_DELCHR                 = 112;
  DB_ISVALIDUSERWITHID      = 113;
  DB_CONNECTIONOPEN         = 114;
  DB_CONNECTIONCLOSE        = 115;
  DB_SAVELOGO               = 116;
  DB_GETACCOUNT             = 117;
  DB_SAVESPECFEE            = 118;
  DB_SAVELOGO2              = 119;
  DB_GETSERVER              = 120;
  DB_CHANGESERVER           = 121;
  DB_LOGINCLOSEUSER         = 122;
  DB_RUNCLOSEUSER           = 123;
  DB_UPDATEUSERINFO         = 124;
  // Friend System -------------
  DB_FRIEND_LIST            = 125;      // Ä£±¸ ¸®½ºÆ® ¿ä±¸
  DB_FRIEND_ADD             = 126;      // Ä£±¸ Ãß°¡
  DB_FRIEND_DELETE          = 127;      // Ä£±¸ »èÁ¦
  DB_FRIEND_OWNLIST         = 128;      // Ä£±¸·Î µî·ÏÇÑ »ç¶÷ ¸®½ºÆ® ¿ä±¸
  DB_FRIEND_EDIT            = 129;      // Ä£±¸ ¼³¸í ¼öÁ¤
  // Tag System ----------------
  DB_TAG_ADD                = 130;      // ÂÊÁö Ãß°¡
  DB_TAG_DELETE             = 131;      // ÂÊÁö »èÁ¦
  DB_TAG_DELETEALL          = 132;      // ÂÊÁö ÀüºÎ »èÁ¦ ( °¡´ÉÇÑ°Í¸¸ )
  DB_TAG_LIST               = 133;      // ÂÊÁö ¸®½ºÆ® Ãß°¡
  DB_TAG_SETINFO            = 134;      // ÃËÁö »óÅÂ º¯°æ
  DB_TAG_REJECT_ADD         = 135;      // °ÅºÎÀÚ Ãß°¡
  DB_TAG_REJECT_DELETE      = 136;      // °ÅºÎÀÚ »èÁ¦
  DB_TAG_REJECT_LIST        = 137;      // °ÅºÎÀÚ ¸®½ºÆ® ¿äÃ»
  DB_TAG_NOTREADCOUNT       = 138;      // ÀÐÁö¾ÊÀº ÂÊÁö °³¼ö ¿äÃ»
  // RelationShip --------------
  DB_LM_LIST                = 139;      // °ü°èÀÚ ¸®½ºÆ® ¿ä±¸
  DB_LM_ADD                 = 140;      // °ü°èÀÚ Ãß°¡
  DB_LM_EDIT                = 141;      // °ü°èÀÚ ¼³Á¤ º¯°æ
  DB_LM_DELETE              = 142;      // °ü°èÀÚ »èÁ¦

  //DBR_LOADHUMANRCD          = 1100;
  //DBR_SAVEHUMANRCD          = 1101;
  DBR_IDPASSWD              = 1103;
  DBR_NEWUSERID             = 1104;
  DBR_CHANGEPASSWD          = 1105;
  DBR_QUERYCHR              = 1106;
  DBR_NEWCHR                = 1107;
  DBR_GETOTHERNAMES         = 1108;
  DBR_ISVALIDUSER           = 1111;
  DBR_DELCHR                = 1112;
  DBR_ISVALIDUSERWITHID     = 1113;
  DBR_GETACCOUNT            = 1117;
  DBR_GETSERVER             = 1200;
  DBR_CHANGESERVER          = 1201;
  DBR_UPDATEUSERINFO        = 1202;
  // Friend System ---------------
  DBR_FRIEND_LIST           = 1203;
  DBR_FRIEND_WONLIST        = 1204;
  DBR_FRIEND_RESULT         = 1205;
  // Tag System ------------------
  DBR_TAG_LIST              = 1206;
  DBR_TAG_REJECT_LIST       = 1207;
  DBR_TAG_NOTREADCOUNT      = 1208;
  DBR_TAG_RESULT            = 1209;
  // RelationShip ---------------
  DBR_LM_LIST               = 1210;
  DBR_LM_RESULT             = 1211;

  //DBR_FAIL                  = 2000;
  DBR_NONE                  = 2000;

  // Frined System---------------
  CM_FRIEND_ADD             = 1046;
  CM_FRIEND_DELETE          = 1047;
  CM_FRIEND_EDIT            = 1048;
  CM_FRIEND_LIST            = 1049;
  // Tag System -----------------
  CM_TAG_ADD                = 1050;
  CM_TAG_DELETE             = 1051;
  CM_TAG_SETINFO            = 1052;
  CM_TAG_LIST               = 1053;
  CM_TAG_NOTREADCOUNT       = 1054;
  CM_TAG_REJECT_LIST        = 1055;
  CM_TAG_REJECT_ADD         = 1056;
  CM_TAG_REJECT_DELETE      = 1057;
  // Relationship ---------------
  CM_LM_OPTION              = 1058;
  CM_LM_REQUEST             = 1059;
  CM_LM_Add                 = 1060;
  CM_LM_EDIT                = 1061;
  CM_LM_DELETE              = 1062;
  // UpgradeItem ----------------
  CM_UPGRADEITEM            = 1063;
  CM_DROPCOUNTITEM          = 1064;
  CM_USERMAKEITEMSEL        = 1065;
  CM_USERMAKEITEM           = 1066;
  CM_ITEMSUMCOUNT           = 1067;

  // À§Å¹ÆÇ¸Å -------------------
  CM_MARKET_LIST            = 1068;
  CM_MARKET_SELL            = 1069;
  CM_MARKET_BUY             = 1070;
  CM_MARKET_CANCEL          = 1071;
  CM_MARKET_GETPAY          = 1072;
  CM_MARKET_CLOSE           = 1073;

  CM_GUILDAGITLIST          = 1074;
  CM_GUILDAGIT_TAG_ADD      = 1075;

  CM_GABOARD_LIST           = 1076;     // Àå¿ø°Ô½ÃÆÇ ¸®½ºÆ®
  CM_GABOARD_ADD            = 1077;     // Àå¿ø°Ô½ÃÆÇ ±Û¾²±â
  CM_GABOARD_READ           = 1078;     // Àå¿ø°Ô½ÃÆÇ ±ÛÀÐ±â
  CM_GABOARD_EDIT           = 1079;     // Àå¿ø°Ô½ÃÆÇ ±Û¼öÁ¤
  CM_GABOARD_DEL            = 1080;     // Àå¿ø°Ô½ÃÆÇ ±Û»èÁ¦
  CM_GABOARD_NOTICE_CHECK   = 1081;     // Àå¿ø°Ô½ÃÆÇ °øÁö»çÇ× ¾²±â Ã¼Å©

  CM_TAG_ADD_DOUBLE         = 1082;     // µÎ¸í µ¿½Ã ÂÊÁö Ãß°¡

  // Àå¿ø²Ù¹Ì±â -------------------
  CM_DECOITEM_BUY           = 1083;     // Àå¿ø²Ù¹Ì±â ¾ÆÀÌÅÛ ±¸ÀÔ

  //±×·ì °á¼º È®ÀÎ
  CM_CREATEGROUPREQ_OK      = 1084;     //±×·ì °á¼º È®ÀÎ
  CM_CREATEGROUPREQ_FAIL    = 1085;     //±×·ì °á¼º È®ÀÎ

  CM_ADDGROUPMEMBERREQ_OK   = 1086;     //±×·ì °á¼º È®ÀÎ
  CM_ADDGROUPMEMBERREQ_FAIL = 1087;     //±×·ì °á¼º È®ÀÎ

  // Relationship (cont.)---------------
  CM_LM_DELETE_REQ_OK       = 1088;     // °ü°è ÆÄ±â OK
  CM_LM_DELETE_REQ_FAIL     = 1089;     // °ü°è ÆÄ±â FAIL

  CM_CLIENT_CHECKTIME       = 1100;
  CM_CANCLOSE               = 1101;

  ////////////////////////////////////////////////////
  USERMARKET_TYPE_ALL       = 0;
  USERMARKET_TYPE_WEAPON    = 1;
  USERMARKET_TYPE_NECKLACE  = 2;
  USERMARKET_TYPE_RING      = 3;
  USERMARKET_TYPE_BRACELET  = 4;
  USERMARKET_TYPE_CHARM     = 5;
  USERMARKET_TYPE_HELMET    = 6;
  USERMARKET_TYPE_BELT      = 7;
  USERMARKET_TYPE_SHOES     = 8;
  USERMARKET_TYPE_ARMOR     = 9;
  USERMARKET_TYPE_DRINK     = 10;
  USERMARKET_TYPE_JEWEL     = 11;
  USERMARKET_TYPE_BOOK      = 12;
  USERMARKET_TYPE_MINERAL   = 13;
  USERMARKET_TYPE_QUEST     = 14;
  USERMARKET_TYPE_ETC       = 15;
  USERMARKET_TYPE_ITEMNAME  = 16;

  USERMARKET_TYPE_SET       = 100;
  USERMARKET_TYPE_MINE      = 200;
  USERMARKET_TYPE_OTHER     = 300;

  USERMARKET_MODE_NULL      = 0;
  USERMARKET_MODE_BUY       = 1;
  USERMARKET_MODE_INQUIRY   = 2;
  USERMARKET_MODE_SELL      = 3;

  MARKET_CHECKTYPE_SELLOK   = 1;
  MARKET_CHECKTYPE_SELLFAIL = 2;
  MARKET_CHECKTYPE_BUYOK    = 3;
  MARKET_CHECKTYPE_BUYFAIL  = 4;
  MARKET_CHECKTYPE_CANCELOK = 5;
  MARKET_CHECKTYPE_CANCELFAIL = 6;
  MARKET_CHECKTYPE_GETPAYOK = 7;
  MARKET_CHECKTYPE_GETPAYFAIL = 8;

  MARKET_DBSELLTYPE_SELL    = 1;
  MARKET_DBSELLTYPE_BUY     = 2;
  //MARKET_DBSELLTYPE_CANCEL  = 3;
  //MARKET_DBSELLTYPE_GETPAY  = 4;
  MARKET_DBSELLTYPE_READYSELL = 11;     //ready to sell...
  MARKET_DBSELLTYPE_READYBUY = 12;      //ready to buy ...
  MARKET_DBSELLTYPE_READYCANCEL = 13;   //ready to CANCEL
  MARKET_DBSELLTYPE_READYGETPAY = 14;
  MARKET_DBSELLTYPE_DELETE  = 20;

  UMRESULT_SUCCESS          = 0;
  UMResult_Fail             = 1;
  UMResult_ReadFail         = 2;
  UMResult_WriteFail        = 3;
  UMResult_ReadyToSell      = 4;
  UMResult_OverSellCount    = 5;
  UMResult_LessMoney        = 6;
  UMResult_LessLevel        = 7;
  UMResult_MaxBagItemCount  = 8;
  UMResult_NoItem           = 9;
  UMResult_DontSell         = 10;
  UMResult_DontBuy          = 11;
  UMResult_DontGetMoney     = 12;
  UMResult_MarketNotReady   = 13;
  UMResult_LessTrustMoney   = 14;
  UMResult_MaxTrustMoney    = 15;
  UMResult_CancelFail       = 16;
  UMResult_OverMoney        = 17;
  UMResult_SellOK           = 18;
  UMResult_BuyOK            = 19;
  UMResult_CancelOK         = 20;
  UMResult_GetPayOK         = 21;

  MAKET_ITEMCOUNT_PER_PAGE  = 10;
  MAKET_MAX_PAGE            = 15;
  MAKET_MAX_ITEM_COUNT      = MAKET_ITEMCOUNT_PER_PAGE * MAKET_MAX_PAGE;

  MAKET_STATE_EMPTY         = 0;
  MAKET_STATE_LOADING       = 1;
  MAKET_STATE_LOADED        = 2;

  MARKET_ALLOW_LEVEL        = 10;
  MARKET_COMMISION          = 10;
  MARKET_MAX_SELL_COUNT     = 15;

  MARKET_CHARGE_MONEY       = 1000;
  MARKET_MAX_TRUST_MONEY    = 100000000;
  MAX_MARKETPRICE           = 100000000;

type
  TPlugInfo = record
    DllName: string;
    sDesc: string;
    Module: THandle;
  end;
  pTPlugInfo = ^TPlugInfo;

  TDefaultMessage = record
    Recog: Integer;                     //0x34
    Ident: Word;                        //0x30
    Param: Word;                        //0x2E
    Tag: Word;                          //0x2C
    Series: Word;                       //0x2A
  end;
  pTDefaultMessage = ^TDefaultMessage;

  TSrvNetInfo = record
    sIPaddr: string[15];
    nPort: Integer;
  end;
  pTSrvNetInfo = ^TSrvNetInfo;

  TSuiteIndex = array[0..255] of Byte;
  TSuitSubRate = array[0..29] of Word;
  TSuiteNames = array[0..12] of string[14];
  //TSuiteUnit = array[0..12] of Byte;

  TSuiteItems = record
    //nSuiteIndex: Integer;
    nNeedCount: Integer;
    sDesc: string[100];
    asSuiteName: TSuiteNames;
    aSuitSubRate: TSuitSubRate;
  end;
  pTSuiteItems = ^TSuiteItems;

  //default
  TClientStdItem = record
    Name: string[14];                   //0X00
    StdMode: Byte;                      //0X0F
    Shape: Byte;                        //0X10
    Weight: Byte;                       //0X11
    AniCount: Byte;                     //0X12
    Source: ShortInt;                   //0X13
    Reserved: Byte;                     //0X14
    NeedIdentify: Byte;                 //0X15
    looks: Word;                        //0X16
    DuraMax: Word;                      //0X18
    AC: Integer;                        //0X1A
    MAC: Integer;                       //0X1E
    DC: Integer;                        //0X22
    MC: Integer;                        //0X26
    SC: Integer;                        //0X2A
    Need: Integer;                      //0X2E
    NeedLevel: Integer;                 //0X32
    Price: Integer;                     //0X36

    UniqueItem: Byte;
    Overlap: Byte;
    ItemType: Byte;
    ItemSet: Word;
    Reference: string[60];
  end;
  pTClientStdItem = ^TClientStdItem;

  TStdItem = record
    Name: string[14];                   //0X00
    StdMode: Byte;                      //0X0F
    Shape: Byte;                        //0X10
    Weight: Byte;                       //0X11
    AniCount: Byte;                     //0X12
    Source: ShortInt;                   //0X13
    Reserved: Byte;                     //0X14
    NeedIdentify: Byte;                 //0X15
    looks: Word;                        //0X16
    DuraMax: Word;                      //0X18
    AC: Integer;                        //0X1A
    MAC: Integer;                       //0X1E
    DC: Integer;                        //0X22
    MC: Integer;                        //0X26
    SC: Integer;                        //0X2A
    Need: Integer;                      //0X2E
    NeedLevel: Integer;                 //0X32
    Price: Integer;                     //0X36

    UniqueItem: Byte;
    Overlap: Byte;
    ItemType: Byte;
    ItemSet: Word;
    Reference: string[60];

    boHeroPickup: Boolean;
    btRefSuiteCount: Byte;
    aSuiteWhere: TSuiteIndex;
    aSuiteIndex: TSuiteIndex;
    nGetRate: Integer;
  end;
  pTStdItem = ^TStdItem;

  TOStdItem = packed record
    Name: string[14];
    StdMode: Byte;                      //0X0F
    Shape: Byte;                        //0X10
    Weight: Byte;                       //0X11
    AniCount: Byte;                     //0X12
    Source: ShortInt;                   //0X13
    Reserved: Byte;                     //0X14
    NeedIdentify: Byte;                 //0X15
    looks: Word;                        //0X16
    DuraMax: Word;                      //0X18
    AC: Word;                           //0X1A
    MAC: Word;                          //0X1C
    DC: Word;                           //0X1E
    MC: Word;                           //0X20
    SC: Word;                           //0X22
    Need: Byte;                         //0X24
    NeedLevel: Byte;                    //0X25
    w26: Word;                          //0X26
    Price: Integer;                     //0X28
  end;
  pTOStdItem = ^TOStdItem;

  TMonInfo = record
    //sName: string[14];
    btRace: Byte;
    btRaceImg: Byte;
    wAppr: Word;
    wLevel: Word;
    wCoolEye: Word;
    dwExp: DWORD;
    wMP: Word;
    wHP: Word;
    wWalkSpeed: Word;
    wWalkStep: Word;
    wWalkWait: Word;
    wAttackSpeed: Word;
    wAC: Word;
    wMAC: Word;
    wDC: Word;
    wMaxDC: Word;
    wMC: Word;
    wSC: Word;
    wSpeed: Word;
    wHitPoint: Word;
    btLifeAttrib: Byte;
    boUndead: Boolean;
    ItemList: TList;
    btExplore: Byte;
    wInLevel: Word;
    dwIPExp: DWORD;
  end;
  pTMonInfo = ^TMonInfo;

  {TMagicInfo = record
    wMagicId: Word;
    sMagicName: string[12];
    btEffectType: byte;
    btEffect: byte;
    wSpell: Word;
    wPower: Word;
    wMaxPower: Word;
    btJob: byte;
    btDefSpell: byte;
    btDefPower: byte;
    btDefMaxPower: byte;
    TrainLevel: array[0..3] of byte;
    MaxTrain: array[0..3] of Integer;
    btTrainLv: byte;
    nDelayTime: Integer;
    sDescr: string;
  end;
  pTMagicInfo = ^TMagicInfo;}

  TMinMap = record
    sName: string[16];
    nID: Integer;
  end;
  pTMinMap = ^TMinMap;

  TMapRoute = record
    sSMapNO: string;
    nDMapX: Integer;
    nSMapY: Integer;
    sDMapNO: string;
    nSMapX: Integer;
    nDMapY: Integer;
  end;
  pTMapRoute = ^TMapRoute;

  TUnbindInfo = record
    nUnbindCode: Integer;
    sItemName: string[14];
  end;
  pTUnbindInfo = ^TUnbindInfo;

  TQuestDiaryInfo = record
    QDDinfoList: TList;
  end;
  pTQuestDiaryInfo = ^TQuestDiaryInfo;

  TAdminInfo = record
    nLv: Integer;
    sChrName: string[14];
    sIPaddr: string[15];
  end;
  pTAdminInfo = ^TAdminInfo;

  TAbility = packed record
    Level: Word;
    AC: Integer;
    MAC: Integer;
    DC: Integer;
    MC: Integer;
    SC: Integer;
    HP: Word;
    MP: Word;
    MaxHP: Word;
    MaxMP: Word;
    Exp: DWORD;
    MaxExp: DWORD;
    Weight: Word;
    MaxWeight: Word;
    WearWeight: Word;
    MaxWearWeight: Word;
    HandWeight: Word;
    MaxHandWeight: Word;
  end;
  pTAbility = ^TAbility;

  TOAbility = packed record
    Level: Word;
    AC: Word;
    MAC: Word;
    DC: Word;
    MC: Word;
    SC: Word;
    HP: Word;
    MP: Word;
    MaxHP: Word;
    MaxMP: Word;
    Diamond: Word;
    Gird: Word;
    Exp: LongWord;
    MaxExp: LongWord;
    Weight: Word;
    MaxWeight: Word;
    WearWeight: Byte;
    MaxWearWeight: Byte;
    HandWeight: Byte;
    MaxHandWeight: Byte;
  end;

  TWAbility = record
    dwExp: LongWord;
    wHP: Word;
    wMP: Word;
    wMaxHP: Word;
    wMaxMP: Word;
  end;

  TMerchantInfo = record
    sScript: string[14];
    sMapName: string[14];
    nX: Integer;
    nY: Integer;
    sNPCName: string[40];
    nFace: Integer;
    nBody: Integer;
    boCastle: Boolean;
  end;
  pTMerchantInfo = ^TMerchantInfo;

  TSocketBuff = record
    Buffer: PChar;                      //0x24
    nLen: Integer;                      //0x28
  end;
  pTSocketBuff = ^TSocketBuff;

  {TSendBuff = record
    nLen: Integer;
    Buffer: array[0..DATA_BUFSIZE - 1] of Char;
  end;
  pTSendBuff = ^TSendBuff;}

  TUserItem = record
    MakeIndex: Integer;
    wIndex: Word;
    Dura: Word;
    DuraMax: Word;
    btValue: array[0..25] of Byte;
  end;
  pTUserItem = ^TUserItem;

  TMonItemInfo = record
    SelPoint: Integer;
    MaxPoint: Integer;
    ItemName: string;
    Count: Integer;
  end;
  pTMonItemInfo = ^TMonItemInfo;

  {TMonsterInfo = record
    Name: string;
    ItemList: TList;
  end;
  PTMonsterInfo = ^TMonsterInfo;}

  TMapItem = record
    Name: string;
    looks: Word;
    AniCount: Byte;
    Reserved: Byte;
    Count: Integer;
    dwCanPickUpTick: DWORD;
    UserItem: TUserItem;
    OfBaseObject: TObject;
    DropBaseObject: TObject;
    boHeroPickup: Boolean;
  end;
  pTMapItem = ^TMapItem;

  {THumanRcd = record
    sUserID: string[16];
    sCharName: string[20];
    btJob: Byte;
    btGender: Byte;
    btLevel: Byte;
    btHair: Byte;
    sMapName: string[15];
    btAttackMode: Byte;
    btIsAdmin: Byte;
    nX: Integer;
    nY: Integer;
    nGold: Integer;
    dwExp: LongWord;
  end;
  pTHumanRcd = ^THumanRcd;}

  TGlobaSessionInfo = packed record
    sAccount: string[10];
    sIPaddr: string[15];
    nSessionID: Integer;
    dwAddTick: LongWord;
    dAddDate: TDateTime;
    boLoadRcd: Boolean;
    boStartPlay: Boolean;
  end;
  pTGlobaSessionInfo = ^TGlobaSessionInfo;

  TObjectFeature = record
    btGender: Byte;
    btWear: Byte;
    btHair: Byte;
    btWeapon: Byte;
  end;
  pTObjectFeature = ^TObjectFeature;

  TStatusInfo = record
    nStatus: Integer;                   //0x60
    dwStatusTime: LongWord;             //0x1E8
    sm218: SmallInt;                    //0x218
    dwTime220: LongWord;                //0x220
  end;

  {TMsgHeader = packed record
    dwCode: LongWord;                   //0x00
    nSocket: Integer;                   //0x04
    wGSocketIdx: Word;                  //0x08
    wIdent: Word;                       //0x0A
    wUserListIndex: Word;               //0x0C
    nLength: Integer;                   //0x10
  end;
  pTMsgHeader = ^TMsgHeader;}

  TMsgHeader = packed record
    dwCode: LongWord;                   //0x00
    nSocket: Integer;                   //0x04
    wGSocketIdx: Word;                  //0x08
    wIdent: Word;                       //0x0A
    wUserListIndex: Integer;            //0x0C
    //wTemp: Word; //0x0E
    nLength: Integer;                   //0x10
  end;
  pTMsgHeader = ^TMsgHeader;

  TMapFlag = record
    boSAFE: Boolean;
    boDARK: Boolean;
    boFIGHTZone: Boolean;
    boFIGHT2Zone: Boolean;
    boFIGHT3Zone: Boolean;
    boDAY: Boolean;
    boQUIZ: Boolean;
    boNORECONNECT: Boolean;
    boMUSIC: Boolean;
    boEXPRATE: Boolean;
    boPKWINLEVEL: Boolean;
    boPKWINEXP: Boolean;
    boPKLOSTLEVEL: Boolean;
    boPKLOSTEXP: Boolean;
    boDECHP: Boolean;
    boINCHP: Boolean;
    boDECGAMEGOLD: Boolean;
    boINCGAMEGOLD: Boolean;
    boINCGAMEPOINT: Boolean;
    boDECGAMEPOINT: Boolean;
    boRUNHUMAN: Boolean;
    boRUNMON: Boolean;
    boNEEDHOLE: Boolean;
    boNORECALL: Boolean;
    boNOGUILDRECALL: Boolean;
    boNODEARRECALL: Boolean;
    boNOMASTERRECALL: Boolean;
    boNORANDOMMOVE: Boolean;
    boNODRUG: Boolean;
    boMINE: Boolean;
    boNOPOSITIONMOVE: Boolean;
    boNoManNoMon: Boolean;

    nL: Integer;
    nNEEDSETONFlag: Integer;
    nNeedONOFF: Integer;
    nMUSICID: Integer;
    nPKWINLEVEL: Integer;
    nEXPRATE: Integer;
    nPKWINEXP: Integer;
    nPKLOSTLEVEL: Integer;
    nPKLOSTEXP: Integer;
    nDECHPPOINT: Integer;
    nDECHPTIME: Integer;
    nINCHPPOINT: Integer;
    nINCHPTIME: Integer;
    nDECGAMEGOLD: Integer;
    nDECGAMEGOLDTIME: Integer;
    nDECGAMEPOINT: Integer;
    nDECGAMEPOINTTIME: Integer;
    nINCGAMEGOLD: Integer;
    nINCGAMEGOLDTIME: Integer;
    nINCGAMEPOINT: Integer;
    nINCGAMEPOINTTIME: Integer;
    sReConnectMap: string[16];
    boKILLFUNC: Boolean;
    nKILLFUNCNO: Integer;
    boCANRIDE: Boolean;
    boCANBAT: Boolean;
    boNotAllowUseItems: Boolean;
    sNotAllowUseItems: string;
    boNotAllowUseMag: Boolean;
    sNotAllowUseMag: string;
    boNoTagMapInfo: Boolean;
    boNoRecallHero: Boolean;
    boNOTHROWITEM: Boolean;
    boNODEAL: Boolean;
    nThunder: Integer;
    nGreatThunder: Integer;
    nLava: Integer;
    nSpurt: Integer;
    nGuildTerritory: Integer;
  end;
  pTMapFlag = ^TMapFlag;

  TNakedAbility = record
    DC: Word;
    MC: Word;
    SC: Word;
    AC: Word;
    MAC: Word;
    HP: Word;
    MP: Word;
    HIT: Word;
    Speed: Word;
    X2: Word;
  end;
  pTNakedAbility = ^TNakedAbility;

  TAddAbility = record
    btWeaponStrong: Byte;
    btHoly: Byte;
    wHitPoint: Word;
    wSpeedPoint: Word;
    wAntiPoison: Word;
    wPoisonRecover: Word;
    wHealthRecover: Word;
    wSpellRecover: Word;
    wAntiMagic: Word;
    btLuck: Byte;
    btUnLuck: Byte;
    nHitSpeed: Integer;
    wHP: Integer;
    wMP: Integer;
    wAC: Integer;
    wMAC: Integer;
    wDC: Integer;
    wMC: Integer;
    wSC: Integer;
  end;

  TStatusTime = array[0..11] of Word;
  TQuestUnit = array[0..12] of Byte;
  TQuestFlag = array[0..100] of Byte;
  THumanUseItems = array[0..13] of TUserItem;
  pTHumanUseItems = ^THumanUseItems;

  TMonUseMagic = (t_Norm, t_MobSlave, t_Defence_m, t_Defence_m_m, t_Defence_s, t_Defence_s_m, t_Attack, t_AddHP_M, t_AddHP_S, t_Poison_0, t_Poison_1, t_Poison_2, t_Poison_3, t_SCDouble, t_UNAMYOUNSUL, t_UNAMYOUNSULSELF);
  TMsgColor = (c_Red, c_Green, c_Blue, c_White, c_Purple, c_Cust);
  TMsgType = (t_Mon, t_Hint, t_GM, t_System, t_Notice, t_Cust, t_Castle, t_Say);
  TMonStatus = (s_KillHuman, s_UnderFire, s_Die, s_MonGen);
  TItemType = (t_aMake, t_dMake, t_dTakeOff, t_aSellOff, t_dSell, t_dDeal, t_dDrop, t_dStorage, t_pOnLogon, t_dUpgrade, t_dRepair, t_sDrop, t_ADrop, t_dCustomName, t_dHeroUse, t_dps, t_nsc);

  TProcessMessage = record
    wIdent: Word;                       //0x00
    wParam: Word;                       //0x02
    nParam1: Integer;                   //0x04
    nParam2: Integer;                   //0x08
    nParam3: Integer;                   //0x0C
    dwDeliveryTime: DWORD;              //0x10
    BaseObject: TObject;                //0x14
    boLateDelivery: Boolean;            //0x18
    nMagicID: Word;
    sMsg: string;                       //0x1C
  end;
  pTProcessMessage = ^TProcessMessage;

  TSendMessage = record
    wIdent: Word;                       //0x00
    wParam: Word;                       //0x02
    nParam1: Integer;                   //0x04
    nParam2: Integer;                   //0x08
    nParam3: Integer;                   //0x0C
    dwDeliveryTime: DWORD;              //0x10
    BaseObject: TObject;                //0x14
    boLateDelivery: Boolean;            //0x18
    nMagicID: Word;
    Buff: PChar;                        //0x18
  end;
  pTSendMessage = ^TSendMessage;

  TSessInfo = record
    nSessionID: Integer;
    nPayMent: Integer;
    nPayMode: Integer;
    nSessionStatus: Integer;
    dwStartTick: LongWord;
    dwActiveTick: LongWord;
    nRefCount: Integer;
    nSocket: Integer;
    nGateIdx: Integer;
    nGSocketIdx: Integer;
    dwNewUserTick: DWORD;
    nSoftVersionDate: Integer;
    sAccount: string[10];
    sIPaddr: string[15];
  end;

  TQuestInfo = record
    wFlag: Word;
    btValue: Byte;
    nRandRage: Integer;
  end;
  pTSessInfo = ^TSessInfo;

  TScript = record
    boQuest: Boolean;
    nQuest: Integer;
    QuestInfo: array[0..9] of TQuestInfo;
    RecordList: TList;
  end;
  pTScript = ^TScript;

  TPowerBlock = array[0..99] of Byte;
  pTPowerBlock = ^TPowerBlock;

  TSlaveInfo = record
    sSalveName: string[14];
    btSalveLevel: Byte;
    btSlaveExpLevel: Byte;
    dwRoyaltySec: DWORD;
    nKillCount: Integer;
    nHP: Integer;
    nMP: Integer;
  end;
  pTSlaveInfo = ^TSlaveInfo;

  TMagic = record
    wMagicId: Word;
    sMagicName: string[12];
    btEffectType: Byte;
    btEffect: Byte;
    wSpell: Word;
    wPower: Word;
    TrainLevel: array[0..3] of Byte;
    MaxTrain: array[0..3] of Integer;
    btTrainLv: Byte;
    btJob: Byte;
    dwDelayTime: DWORD;
    btDefSpell: Byte;
    btDefPower: Byte;
    wMaxPower: Word;
    btDefMaxPower: Byte;
    sDescr: string[14];
    btClass: Byte;
  end;
  pTMagic = ^TMagic;

  TUserMagic = record
    MagicInfo: pTMagic;
    btLevel: Byte;
    btKey: Byte;
    wMagIdx: Integer;
    nTranPoint: Integer;
  end;
  pTUserMagic = ^TUserMagic;

  TMagicArr = array[0..2, 0..255] of pTUserMagic;
  PTMagicArr = ^TMagicArr;
  THumItems = THumanUseItems;           //array[0..8] of TUserItem;
  pTHumItems = ^THumItems;
  //THumAddItems = array[9..12] of TUserItem;
  //pTHumAddItems = ^THumAddItems;
  TBagItems = array[0..45] of TUserItem;
  pTBagItems = ^TBagItems;

  TMagicRcd = packed record
    wMagIdx: Byte;                      //Word;      InternalPower
    btClass: Byte;
    btLevel: Byte;
    btKey: Byte;
    nTranPoint: Integer;
  end;
  pTHumMagicInfo = ^TMagicRcd;
  THumMagic = array[0..59] of TMagicRcd;
  pTHumMagic = ^THumMagic;

  TStorageItems = array[0..45] of TUserItem;
  pTStorageItems = ^TStorageItems;

  TStorageItemsEx = array[0..64] of TUserItem;
  pTStorageItemsEx = ^TStorageItemsEx;

  TCustomData = packed record
    StorageItems: TStorageItemsEx;
  end;

  {THumAbil = packed record
    Level: byte; //µÈ¼¶
    unknown1: byte; //±£Áô
    AC: Word;
    MAC: Word;
    MinAttack: byte; // ×îÐ¡¹¥»÷Á¦
    MaxAttack: byte; // ×î´ó¹¥»÷Á¦
    MinMagic: byte; // ×îÐ¡Ä§·¨
    MAXMAGIC: byte; // ×î´óÄ§·¨
    MinDao: byte; // ×îÐ¡µÀÊõ
    MaxDao: byte; // ×î´óµÀÊõ
    HP: Word; // ÉúÃüÖµ
    MP: Word; // Ä§·¨Öµ
    MaxHP: Word; // ×î´óÉúÃüÖµ
    NowMaxMagic: Word; // ×î´óÄ§·¨Öµ
    unknown2: array[0..3] of byte; // ²»Çå³þ
    Exp: Longword; // ¾­ÑéÖµ
    MaxExp: Longword; // Éý¼¶ËùÐè¾­ÑéÖµ
    unknown3: array[0..31] of byte; // ²»Çå³þ
  end;}

  THumData = packed record
    sChrName: string[14];               //0x00
    sCurMap: string[16];                //0x0F
    wCurX: Word;                        //0X20
    wCurY: Word;                        //0X22
    btDir: Byte;                        //0X24
    btHair: Byte;                       //0X25
    btSex: Byte;                        //0X26
    btJob: Byte;                        //0X27
    nGold: Integer;                     //0X28
    Abil: TOAbility;                    //0X2C
    wStatusTimeArr: TStatusTime;        //0x54
    sHomeMap: string[17];               //0X6C
    wHomeX: Word;                       //0x7E
    wHomeY: Word;                       //0X80
    sDearName: string[14];              //0x82
    sMasterName: string[14];            //0X91
    boMaster: Boolean;                  //0XA0
    btCreditPoint: Byte;                //0XA1
    btInPowerLevel: Byte;               //ipower
    btReconnection2: Byte;              //.......................
    sStoragePwd: string[7];             //0XA4
    btReLevel: Byte;                    //0XAC
    boLockLogon: Boolean;               //0XAD
    wInPowerPoint: Word;                //ipower
    BonusAbil: TNakedAbility;           //0XB0
    nBonusPoint: Integer;               //0XC4
    nGameGold: Integer;                 //0XC8
    nGamePoint: Integer;                //0XCC
    nPayMentPoint: Integer;             //0XD0
    nHungerStatus: Integer;             //0XD4
    nPKPOINT: Integer;                  //0XD8
    btAllowGroup: Byte;                 //0xDC
    btClPkPoint: Byte;                  //0XDD
    btAttatckMode: Byte;                //0XDE
    btIncHealth: Byte;                  //0XDF
    btIncSpell: Byte;                   //0XE0
    btIncHealing: Byte;                 //0xE1
    btFightZoneDieCount: Byte;          //0xE2
    sAccount: string[10];               //0XE3
    btNewHuman: Byte;                   //0XEE
    dwInPowerExp: LongWord;             //ipower
    sMarkerMapName: string[12];         //.......................
    btAttribute: Byte;                  //ÎåÐÐÊôÐÔ
    boAllowGuildReCall: Boolean;        //0XF6
    boAllowGroupReCall: Boolean;        //0x102
    nKillMonExpRate: Integer;           //0x103
    dwKillMonExpRateTime: LongWord;     //0x107
    sHeroName: string[14];              //0x82
    sHeroMasterName: string[14];        //0x82
    btOptnYBDeal: Byte;
    wGroupRcallTime: Word;              //0xF8
    dBodyLuck: Double;                  //0xFA
    sMarkerMap: string[16];
    wMarkerX: Word;
    wMarkerY: Word;
    QuestUnitOpen: TQuestUnit;          //0x149 - 13
    QuestUnit: TQuestUnit;              //0x156 - 13
    QuestFlag: TQuestFlag;              //0x163
    HumItems: THumItems;                //0x1C8
    BagItems: TBagItems;                //0x2BC
    Magic: THumMagic;                   //0x70C
    StorageItems: TStorageItems;        //0x7AC
    sTemp: string[255];
    //HumAddItems: THumAddItems;          //0xBFC  //C18
  end;
  pTHumData = ^THumData;

  THumDataBack = packed record
    sChrName: string[14];               //0x00
    sCurMap: string[16];                //0x0F
    wCurX: Word;                        //0X20
    wCurY: Word;                        //0X22
    btDir: Byte;                        //0X24
    btHair: Byte;                       //0X25
    btSex: Byte;                        //0X26
    btJob: Byte;                        //0X27
    nGold: Integer;                     //0X28
    Abil: TOAbility;                    //0X2C
    wStatusTimeArr: TStatusTime;        //0x54
    sHomeMap: string[17];               //0X6C
    wHomeX: Word;                       //0x7E
    wHomeY: Word;                       //0X80
    sDearName: string[14];              //0x82
    sMasterName: string[14];            //0X91
    boMaster: Boolean;                  //0XA0
    btCreditPoint: Byte;                //0XA1
    btInPowerLevel: Byte;               //ipower
    btReconnection2: Byte;              //.......................
    sStoragePwd: string[7];             //0XA4
    btReLevel: Byte;                    //0XAC
    boLockLogon: Boolean;               //0XAD
    wInPowerPoint: Word;                //ipower
    BonusAbil: TNakedAbility;           //0XB0
    nBonusPoint: Integer;               //0XC4
    nGameGold: Integer;                 //0XC8
    nGamePoint: Integer;                //0XCC
    nPayMentPoint: Integer;             //0XD0
    nHungerStatus: Integer;             //0XD4
    nPKPOINT: Integer;                  //0XD8
    btAllowGroup: Byte;                 //0xDC
    btClPkPoint: Byte;                  //0XDD
    btAttatckMode: Byte;                //0XDE
    btIncHealth: Byte;                  //0XDF
    btIncSpell: Byte;                   //0XE0
    btIncHealing: Byte;                 //0xE1
    btFightZoneDieCount: Byte;          //0xE2
    sAccount: string[10];               //0XE3
    btNewHuman: Byte;                   //0XEE
    dwInPowerExp: LongWord;             //ipower
    sMarkerMapName: string[12];         //.......................
    btAttribute: Byte;                  //ÎåÐÐÊôÐÔ
    boAllowGuildReCall: Boolean;        //0XF6
    boAllowGroupReCall: Boolean;        //0x102
    nKillMonExpRate: Integer;           //0x103
    dwKillMonExpRateTime: LongWord;     //0x107
    sHeroName: string[14];              //0x82
    sHeroMasterName: string[14];        //0x82
    btOptnYBDeal: Byte;                 //
    wGroupRcallTime: Word;              //0xF8
    dBodyLuck: Double;                  //0xFA
    sMarkerMap: string[16];             //
    wMarkerX: Word;                     //
    wMarkerY: Word;                     //
    QuestUnitOpen: TQuestUnit;          //0x149
    QuestUnit: TQuestUnit;              //0x156
    QuestFlag: TQuestFlag;              //0x163

    HumItems: THumItems;                //0x1C8   - 0..13
    BagItems: TBagItems;                //0x2BC
    Magic: THumMagic;                   //0x70C   - 0..59
    StorageItems: TStorageItems;        //0x7AC
    //HumAddItems: THumAddItems;          //0xBFC
  end;
  pTHumDataBack = ^THumDataBack;

  THumHeader = packed record
    boDeleted: Boolean;
    aByte: array[0..2] of Byte;
    dCreateDate: TDateTime;
    sName: string[15];
  end;

  THumDataInfo = packed record
    Header: THumHeader;
    Data: THumData;
  end;
  pTHumDataInfo = ^THumDataInfo;

  TRecordHeader = packed record
    boDeleted: Boolean;
    b: array[0..2] of Byte;
    dCreateDate: TDateTime;
    sName: string[15];
  end;

  THumInfo = packed record
    Header: TRecordHeader;
    sChrName: string[14];
    sAccount: string[10];
    boDeleted: Boolean;
    boSelected: Boolean;
    dModDate: TDateTime;
    btCount: Byte;
    unknown2: array[0..6] of Byte;
  end;

  TIDHeader = packed record
    unknown1: array[0..2] of Byte;
    boDeleted: Boolean;
    CreateDate: TDateTime;              //×¢²áÈÕÆÚ
    UpdateDate: TDateTime;              //×îºóµÇÂ½ÈÕÆÚ
    sAccount: string[11];               //ÓÃ»§µÇÂ½id
  end;
  TUserEntry = packed record
    sAccount: string[10];
    sPassWord: string[10];
    sUserName: string[20];
    sSSNo: string[14];
    sPhone: string[14];
    sQuiz: string[20];
    sAnswer: string[12];
    sEMail: string[40];
  end;
  TUserEntryAdd = packed record
    sQuiz2: string[20];
    sAnswer2: string[12];
    sBirthDay: string[10];
    sMobilePhone: string[13];
    sMemo: string[20];
    sMemo2: string[20];
  end;
  TUserEntryA = packed record
    sAccount: string[10];
    sPassWord: string[10];
    sUserName: string[20];
    sSSNo: string[14];
    sPhone: string[14];
    sQuiz: string[20];
    sAnswer: string[12];
    sEMail: string[40];
    sQuiz2: string[20];
    sAnswer2: string[12];
    sBirthDay: string[10];
    sMobilePhone: string[13];
    sMemo: string[20];
    sMemo2: string[20];
  end;
  TAccountDBRecord = packed record
    Header: TIDHeader;
    UserEntry: TUserEntry;
    UserEntryAdd: TUserEntryAdd;
    b: array[0..6] of Byte;
    nErrorCount: Integer;
    dwActionTick: DWORD;
    b1: array[0..31] of Byte;
  end;

  TGameCmd = record
    sCmd: string[25];
    nPermissionMin: Integer;
    nPermissionMax: Integer;
  end;
  pTGameCmd = ^TGameCmd;

  TLoadDBInfo = record
    btClinetFlag: Byte;
    nGateIdx: Integer;
    nSocket: Integer;
    nSessionID: Integer;
    nSoftVersionDate: Integer;
    nPayMent: Integer;
    nPayMode: Integer;
    nGSocketIdx: Integer;
    nReLoadCount: Integer;
    dwNewUserTick: LongWord;
    PlayObject: TObject;
    mPlayObject: TObject;
    sAccount: string[10];
    sCharName: string[14];
    sIPaddr: string[15];
    btQueryMsg: Byte;
    nLRType: Integer;
    nLRPage: Integer;
  end;
  pTLoadDBInfo = ^TLoadDBInfo;

  {TGoldChangeInfo = packed record
    sGameMasterName: string;
    sGetGoldUser: string;
    nGold: Integer;
  end;
  pTGoldChangeInfo = ^TGoldChangeInfo;}

  TSwitchDataInfo = packed record
    dwWaitTime: LongWord;
    sChrName: string[14];
    sMAP: string[16];
    wX: Word;
    wY: Word;
    wClitntType: Word;
    nClientTick: Integer;
    nClientVerNO: Integer;
    nCode: Integer;
    boRecallHero: Boolean;
    boBanShout: Boolean;
    boHearWhisper: Boolean;
    boBanGuildChat: Boolean;
    boAdminMode: Boolean;
    boObMode: Boolean;
    BlockWhisperArr: array[0..19] of string[14];
    SlaveArr: array[0..5] of TSlaveInfo;
    StatusValue: array[0..5] of Word;
    StatusTimeOut: array[0..5] of LongWord;
    Abil: TAbility;
    TempArr: array[1..2048] of Char;
  end;
  pTSwitchDataInfo = ^TSwitchDataInfo;

  TUserOpenInfo = record
    sChrName: string[14];
    LoadUser: TLoadDBInfo;
    HumanRcd: THumDataInfo;
  end;
  pTUserOpenInfo = ^TUserOpenInfo;

  TIPaddr = packed record
    a, b, c, d: Byte;
    Port: Integer;
    sIPaddr: string[15];
    dIPaddr: string[15];
  end;
  PTIPAddr = ^TIPaddr;

  TClassProc = procedure(Sender: TObject);

  TProc = record
    sProcName: string;
    nProcAddr: Pointer;
  end;
  TProcArray = array[0..100] of TProc;

  TMyObject = record
    sObjcName: string;
    Obj: TObject;
  end;
  TObjectArray = array[0..100] of TMyObject;

  {TCheckCode = packed recordend;}

  TClientConf = record
    boClientCanSet: Boolean;
    boRUNHUMAN: Boolean;
    boRUNMON: Boolean;
    boRunNpc: Boolean;
    boWarRunAll: Boolean;
    btDieColor: Integer;
    wSpellTime: Integer;
    wHitIime: Integer;
    wItemFlashTime: Integer {5 * 1000};
    btItemSpeed: Integer;               {60}
    boCanStartRun: Boolean;
    boParalyCanRun: Boolean;
    boParalyCanWalk: Boolean;
    boParalyCanHit: Boolean;
    boParalyCanSpell: Boolean;
    boForbidDoubleHit: Boolean;         //½ûÖ¹Ë«±¶(¶à±¶)¹¥»÷ Add By Blue
    boForbidDoubleSpell: Boolean;       //½ûÖ¹Ë«±¶(¶à±¶)Ä§·¨ Add By Blue
    boForbidDoubleRun: Boolean;         //½ûÖ¹Ë«±¶(¶à±¶)ÅÜ²½ Add By Blue
    boShowRedHPLable: Boolean;
    boShowHPNumber: Boolean;
    boShowJobLevel: Boolean;
    boDuraAlert: Boolean;
    boMagicLock: Boolean;
    boAutoPuckUpItem: Boolean;
  end;

  TRecallMigic = record
    nHumLevel: Integer;
    nCount: Integer;
    nLevel: Integer;
    sMonName: string[16];
  end;

  TLimitLevelExp = packed record
    nHumLevel: Integer;
    nEXPRATE: Integer;
  end;
  TLevelNeedExp = array[1..500] of LongWord;

  TIPLelelInfo = record
    nPower: Integer;
    dwPExp: LongWord;
    dwHExp: LongWord;
  end;
  TIPLvlNeedList = array[1..100] of TIPLelelInfo;

  CommandType = record
    sCmd: string[25];
    nPermissionMin: Integer;
    nPermissionMax: Integer;
  end;

  TGameCommand = record
    Data: CommandType;
    PRVMSG: CommandType;
    ALLOWMSG: CommandType;
    LETSHOUT: CommandType;
    LETTRADE: CommandType;
    LETGUILD: CommandType;
    ENDGUILD: CommandType;
    BANGUILDCHAT: CommandType;
    AUTHALLY: CommandType;
    AUTH: CommandType;
    AUTHCANCEL: CommandType;
    DIARY: CommandType;
    USERMOVE: CommandType;
    SEARCHING: CommandType;
    ALLOWGROUPCALL: CommandType;
    GROUPRECALLL: CommandType;
    ALLOWGUILDRECALL: CommandType;
    GUILDRECALLL: CommandType;
    UNLOCKSTORAGE: CommandType;
    UnLock: CommandType;
    Lock: CommandType;
    PASSWORDLOCK: CommandType;
    SETPASSWORD: CommandType;
    CHGPASSWORD: CommandType;
    CLRPASSWORD: CommandType;
    UNPASSWORD: CommandType;
    MEMBERFUNCTION: CommandType;
    MEMBERFUNCTIONEX: CommandType;
    DEAR: CommandType;
    ALLOWDEARRCALL: CommandType;
    DEARRECALL: CommandType;
    Master: CommandType;
    ALLOWMASTERRECALL: CommandType;
    MASTERECALL: CommandType;
    ATTACKMODE: CommandType;
    REST: CommandType;
    TAKEONHORSE: CommandType;
    TAKEOFHORSE: CommandType;
    HUMANLOCAL: CommandType;
    Move: CommandType;
    POSITIONMOVE: CommandType;
    SignMove: CommandType;
    INFO: CommandType;
    MOBLEVEL: CommandType;
    MOBCOUNT: CommandType;
    HUMANCOUNT: CommandType;
    Map: CommandType;
    KICK: CommandType;
    TING: CommandType;
    SUPERTING: CommandType;
    MAPMOVE: CommandType;
    SHUTUP: CommandType;
    RELEASESHUTUP: CommandType;
    SHUTUPLIST: CommandType;
    GAMEMASTER: CommandType;
    OBSERVER: CommandType;
    SUEPRMAN: CommandType;
    Level: CommandType;
    SABUKWALLGOLD: CommandType;
    RECALL: CommandType;
    REGOTO: CommandType;
    SHOWFLAG: CommandType;
    SHOWOPEN: CommandType;
    SHOWUNIT: CommandType;
    Attack: CommandType;
    MOB: CommandType;
    MOBNPC: CommandType;
    DELNPC: CommandType;
    NPCSCRIPT: CommandType;
    RECALLMOB: CommandType;
    LUCKYPOINT: CommandType;
    LOTTERYTICKET: CommandType;
    RELOADGUILD: CommandType;
    RELOADLINENOTICE: CommandType;
    RELOADABUSE: CommandType;
    BACKSTEP: CommandType;
    BALL: CommandType;
    FREEPENALTY: CommandType;
    PKPOINT: CommandType;
    IncPkPoint: CommandType;
    CHANGELUCK: CommandType;
    HUNGER: CommandType;
    HAIR: CommandType;
    TRAINING: CommandType;
    DELETESKILL: CommandType;
    CHANGEJOB: CommandType;
    CHANGEGENDER: CommandType;
    NameColor: CommandType;
    Mission: CommandType;
    MobPlace: CommandType;
    TRANSPARECY: CommandType;
    DELETEITEM: CommandType;
    LEVEL0: CommandType;
    CLEARMISSION: CommandType;
    SETFLAG: CommandType;
    SETOPEN: CommandType;
    SETUNIT: CommandType;
    RECONNECTION: CommandType;
    DISABLEFILTER: CommandType;
    CHGUSERFULL: CommandType;
    CHGZENFASTSTEP: CommandType;
    CONTESTPOINT: CommandType;
    STARTCONTEST: CommandType;
    ENDCONTEST: CommandType;
    ANNOUNCEMENT: CommandType;
    OXQUIZROOM: CommandType;
    GSA: CommandType;
    CHANGEITEMNAME: CommandType;
    DISABLESENDMSG: CommandType;
    ENABLESENDMSG: CommandType;
    DISABLESENDMSGLIST: CommandType;
    KILL: CommandType;
    MAKE: CommandType;
    SMAKE: CommandType;
    BonusPoint: CommandType;
    DELBONUSPOINT: CommandType;
    RESTBONUSPOINT: CommandType;
    FIREBURN: CommandType;
    TESTFIRE: CommandType;
    TESTSTATUS: CommandType;
    DELGOLD: CommandType;
    ADDGOLD: CommandType;
    DELGAMEGOLD: CommandType;
    ADDGAMEGOLD: CommandType;
    GAMEGOLD: CommandType;
    GAMEPOINT: CommandType;
    CREDITPOINT: CommandType;
    TESTGOLDCHANGE: CommandType;
    REFINEWEAPON: CommandType;
    RELOADADMIN: CommandType;
    ReLoadNpc: CommandType;
    RELOADMANAGE: CommandType;
    RELOADROBOTMANAGE: CommandType;
    RELOADROBOT: CommandType;
    RELOADMONITEMS: CommandType;
    RELOADDIARY: CommandType;
    RELOADITEMDB: CommandType;
    RELOADMAGICDB: CommandType;
    RELOADMONSTERDB: CommandType;
    RELOADMINMAP: CommandType;
    ReAlive: CommandType;
    ADJUESTLEVEL: CommandType;
    ADJUESTEXP: CommandType;
    AddGuild: CommandType;
    DelGuild: CommandType;
    CHANGESABUKLORD: CommandType;
    FORCEDWALLCONQUESTWAR: CommandType;
    ADDTOITEMEVENT: CommandType;
    ADDTOITEMEVENTASPIECES: CommandType;
    ItemEventList: CommandType;
    STARTINGGIFTNO: CommandType;
    DELETEALLITEMEVENT: CommandType;
    STARTITEMEVENT: CommandType;
    ITEMEVENTTERM: CommandType;
    ADJUESTTESTLEVEL: CommandType;
    TRAININGSKILL: CommandType;
    OPDELETESKILL: CommandType;
    CHANGEWEAPONDURA: CommandType;
    RELOADGUILDALL: CommandType;
    WHO: CommandType;
    TOTAL: CommandType;
    TESTGA: CommandType;
    MAPINFO: CommandType;
    SBKDOOR: CommandType;
    CHANGEDEARNAME: CommandType;
    CHANGEMASTERNAME: CommandType;
    STARTQUEST: CommandType;
    SETPERMISSION: CommandType;
    CLEARMON: CommandType;
    RENEWLEVEL: CommandType;
    DENYIPLOGON: CommandType;
    DENYACCOUNTLOGON: CommandType;
    DENYCHARNAMELOGON: CommandType;
    DELDENYIPLOGON: CommandType;
    DELDENYACCOUNTLOGON: CommandType;
    DELDENYCHARNAMELOGON: CommandType;
    SHOWDENYIPLOGON: CommandType;
    SHOWDENYACCOUNTLOGON: CommandType;
    SHOWDENYCHARNAMELOGON: CommandType;
    VIEWWHISPER: CommandType;
    SPIRIT: CommandType;
    SPIRITSTOP: CommandType;
    SetMapMode: CommandType;
    SHOWMAPMODE: CommandType;
    TESTSERVERCONFIG: CommandType;
    SERVERSTATUS: CommandType;
    TESTGETBAGITEM: CommandType;
    CLEARBAG: CommandType;
    SHOWUSEITEMINFO: CommandType;
    BINDUSEITEM: CommandType;
    MOBFIREBURN: CommandType;
    TESTSPEEDMODE: CommandType;
    LOCKLOGON: CommandType;
    UNLOCKLOGON: CommandType;
    RemoteMusic: CommandType;
    RemoteMsg: CommandType;
    INITSABUK: CommandType;
    TAKEUSERITEM: CommandType;
  end;

  TMonDrop = record
    sItemName: string[20];
    nDropCount: Integer;
    nNoDropCount: Integer;
    nCountLimit: Integer;
    ClearTime: LongWord;
    Time: LongWord;
  end;
  pTMonDrop = ^TMonDrop;

  TMonSayMsg = record
    State: TMonStatus;
    Color: TMsgColor;
    nRate: Integer;
    sSayMsg: string;
  end;
  pTMonSayMsg = ^TMonSayMsg;

  TMsgProc = procedure(Msg: PChar; nMsgLen: Integer; nMode: Integer); stdcall;
  TIPLocal = procedure(IPAddr: PChar; var IP: array of Char; IPSize: Integer); stdcall;
  //TIPLocal = procedure(IPAddr, IP: PChar; IPSize: Integer); stdcall;
  TLocalIP = procedure(var IP: array of Char; IPSize: Integer); stdcall;
  TFindProc = function(ProcName: PChar; nNameLen: Integer): Pointer; stdcall;
  TSetProc = function(ProcAddr: Pointer; ProcName: PChar; nNameLen: Integer): Boolean; stdcall;
  TFindObj = function(ObjName: PChar; nNameLen: Integer): TObject; stdcall;
  TPlugInit = function(AppHandle: HWnd; MsgProc: TMsgProc; FindProc: TFindProc; SetProc: TSetProc; FindObj: TFindObj): PChar; stdcall;
  TDeCryptString = procedure(src, Dest: PChar; nSrc: Integer; var nDest: Integer); stdcall;
  //TDeCryptString = function(Src: PChar): PChar; stdcall;
  TPlugConfig = procedure(); stdcall;

  //TGetLicense = function(var nDay: Integer; var nM2Crc: Integer): Integer; stdcall;
  TGetNextDirection = function(sX, sY, dx, dy: Integer): Byte; stdcall;
  TGetExVersionNO = function(nVersionDate: Integer; var nOldVerstionDate: Integer): Integer; stdcall;
  TGetGoldShape = function(nGold: Integer): Word; stdcall;
  TStrType = (t_sWellCome, t_sTitleName, t_sProductName, t_sVersion, t_sUpDateTime, t_sProgram, t_sWebSite, t_sBbsSite, t_sHash, t_sChar, t_sContact, t_sAdvertisement {, t_mSendWhisperMsg});
  TGetResourceString = function(StrType: TStrType): PChar; stdcall;
  TGetValNameNo = function(sText: string): Integer; stdcall;
  TCheckUserItems = function(nIdx: Integer; StdItem: pTStdItem): Boolean; stdcall;
  TGetItemNumber = function(): Integer; stdcall;
  TGetItemNumberEx = function(): Integer; stdcall;
  TFilterShowName = function(sName: string): string; stdcall;
  TCheckGuildName = function(sName: string): Boolean; stdcall;

  TDoorStatus = record
    boOpened: Boolean;
    nRefCount: Integer;
    dwOpenTick: DWORD;
  end;
  pTDoorStatus = ^TDoorStatus;

  TDoorInfo = record
    nX, nY: Integer;
    nPoint: Integer;
    Status: pTDoorStatus;
  end;
  pTDoorInfo = ^TDoorInfo;

  TMapQuestInfo = record
    nFlag: Integer;
    nValue: Integer;
    sMonName: string[15];
    sItemName: string[15];
    boGrouped: Boolean;
    NPC: TObject;
  end;
  pTMapQuestInfo = ^TMapQuestInfo;

  TDynamicVar = record
    sName: string[15];
    VarType: TVarType;
    nInternet: Integer;
    sString: string;
  end;
  pTDynamicVar = ^TDynamicVar;

  TItemName = record
    nMakeIndex: LongWord;
    nItemIndex: Integer;
    sItemName: string[15];
  end;
  pTItemName = ^TItemName;

  TLoadHuman = record
    sAccount: string[12];
    sChrName: string[14];
    sUserAddr: string[15];
    nSessionID: Integer;
  end;

  {TNewHero = packed record
    sAccount: string[12];
    sChrName: string[14];
    sUserAddr: string[15];
    btHair: Byte;
    btJob: Byte;
    btSex: Byte;
    nSessionID: Integer;
  end;}

  TOClientItem = packed record
    s: TOStdItem;
    MakeIndex: Integer;
    Dura: Word;
    DuraMax: Word;
  end;
  pTOClientItem = ^TOClientItem;

  TClientItem = record
    s: TClientStdItem;
    MakeIndex: Integer;
    Dura: Word;
    DuraMax: Word;
  end;
  PTClientItem = ^TClientItem;

  TCharDesc = packed record
    Feature: Integer;
    Status: Integer;
    StatusEx: Integer;
  end;

  TCharDescEx = packed record
    nFeature: Integer;
    nStatus: Integer;
    nFeatureEx: Integer;
  end;

  TMessageBodyW = record
    param1: Word;
    param2: Word;
    Tag1: Word;
    Tag2: Word;
  end;

  TMessageBodyWL = record
    lParam1: Integer;
    lParam2: Integer;
    lTag1: Integer;
    lTag2: Integer;
  end;

  TShortMessage = record
    Ident: Word;
    wMsg: Word;
  end;

  TUserStateInfo = record
    Feature: Integer;
    UserName: string[15];
    NameColor: Integer;
    GuildName: string[14];
    GuildRankName: string[15];
    btGender: Byte;
    btHumAttr: Byte;
    btResver1: Byte;
    btResver2: Byte;
    UseItems: array[0..13] of TClientItem;
  end;
  pTUserStateInfo = ^TUserStateInfo;

  TOUserStateInfo = packed record
    Feature: Integer;
    UserName: string[15];
    NameColor: Integer;
    GuildName: string[14];
    GuildRankName: string[16];
    UseItems: array[0..13] of TOClientItem;
  end;

  TClientMagic = record
    Key: Char;
    Level: Byte;
    CurTrain: Integer;
    Def: TMagic;
  end;
  PTClientMagic = ^TClientMagic;

  {TRegInfo = packed record
    sKey: string;
    sServerName: string;
    sRegSrvIP: string;
    nRegPort: Integer;
    //sGateIPaddr: string[20];
  end;}

  TGateObj = record
    DEnvir: TObject;
    nDMapX: Integer;
    nDMapY: Integer;
    boFlag: Boolean;
  end;
  pTGateObj = ^TGateObj;

  TSellOff = packed record
    sCharName: string[14];              //00
    SellTime: TDateTime;                //0F
    Price: LongWord;                    //17
    Tax: Integer;                       //1B
    item: TUserItem;                    //1F
    No2: Integer;                       //Î´Öª
  end;
  pSellOff = ^TSellOff;

  TItemLimit = record
    sItemName: string[14];
    nItemInedx: Integer;
    boAllowMake: Boolean;
    boDisableMake: Boolean;
    boDisableTakeOff: Boolean;
    boAllowSellOff: Boolean;
    boDisableSell: Boolean;
    boDisableDeal: Boolean;
    boDisableDrop: Boolean;
    boDisableStorage: Boolean;
    boDispearOnLogon: Boolean;
    boDisableUpgrade: Boolean;
    boDisableRepair: Boolean;
    boDropWithoutFail: Boolean;
    boAbleDropInSafeZone: Boolean;
    boDisCustomName: Boolean;
    boDisallowHeroUse: Boolean;
    boDisallowPSell: Boolean;
    boNoScatter: Boolean;
  end;
  pTItemLimit = ^TItemLimit;

  TShortString = packed record
    btLen: Byte;
    Strings: array[0..49] of Char;
  end;
  pTShortString = ^TShortString;

  TSaleItem = packed record
    sStdItem: TClientStdItem;
    sItemDesc: array[0..50] of Char;
  end;
  pTSaleItem = ^TSaleItem;

  TShopItem = packed record             //0x98
    sItemName: string[14];              //0x00
    btClass: Byte;                      //0X0F
    wLooks: Word;                       //0X10 //Items.wil
    wPrice: Word;                       //0X12
    wShape1: Word;                      //0X14
    wShape2: Word;                      //0X16
    sExplain: string[127];
  end;
  pTShopItem = ^TShopItem;

  TIPCData = record
    SendString: string[100];
    SendInteger: Integer;
  end;
  PIPCData = ^TIPCData;

  TServerInfo = record
    nSckHandle: Integer;
    sStr: string;
    Socket: TCustomWinSocket;
  end;
  pTServerInfo = ^TServerInfo;

  THumSession = record
    sChrName: string[14];
    nIndex: Integer;
    Socket: TCustomWinSocket;
    bo24: Boolean;
    bo2C: Boolean;
    dwTick30: LongWord;
  end;
  pTHumSession = ^THumSession;

  TClientGT = packed record
    number: Byte;
    GuildName: string[GuildNameLen];
    GuildMasters: array[0..1] of string[ActorNameLen];
    SalePrice: Integer;
    Status: Byte;                       //0=normal, 1= onsale, 2= sold, pending activation
  end;
  pTClientGT = ^TClientGT;

  TDecoItem = packed record
    Appr: Word;
    Name: string[20];
    Price: Integer;
    Location: Byte;
  end;
  pTDecoItem = ^TDecoItem;

  TMakerMap = packed record
    sCharName: string[14];
    sMapName: string[16];
    wMapX: Word;
    wMapY: Word;
  end;
  pTMakerMap = ^TMakerMap;
  TMakerMapInfo = array[1..6] of TMakerMap;

  TPSInfo = packed record
    sStr: string[15];
    nInt: Integer;
  end;
  TCPostSell = array[0..10] of TPSInfo;

  TPostSell = packed record
    sCharName: string[14];
    sTargName: string[14];
    dPostTime: TDateTime;
    nPostPrice: Integer;
    aPostItems: array[0..8] of TUserItem;
    nPostStone: Integer;
  end;
  pTPostSell = ^TPostSell;

  TClientPS = packed record
    sCharName: string[14];
    sTargName: string[14];
    nPostPrice: Integer;
    sPostTime: string[20];
    sPostState: string[4];
    aPostItems: array[0..9] of TClientItem;
  end;
  PTClientPS = ^TClientPS;

  TEGoldRcd = packed record
    boDealFlag: Boolean;
    PostTime: TDateTime;
    nDealGold: Integer;
    sTargName: string[14];
  end;
  TPostGold = packed record
    sCharName: string[14];
    EGold: array[0..4] of TEGoldRcd;
  end;
  pTPostGold = ^TPostGold;

  TMsgHeaderNew = packed record         //0X0C
    dwRunCode: LongWord;                //0X04    $FF44FF44
    btIdent: Byte;                      //0X04
    btFalgs: Byte;                      //0X05
    wLength: Word;                      //0X06
    nSendTickCount: Integer;            //0X08
  end;

  TOpenBoxItem = packed record
    nBoxItemMIdx: Integer;
    sBoxItemName: string[15];
    nKeyItemMIdx: Integer;
    sKeyItemName: string[15];
  end;

  TBoxItem = packed record              //0X18
    sName: string[14];                  //0X00
    btRate: Byte;                       //0X0F
    nLooks: Integer;                    //0X10
    nNumber: Integer;                   //0X14
  end;
  pTBoxItem = ^TBoxItem;
  TBoxItems = array[1..9] of TBoxItem;
  pTBoxItems = ^TBoxItems;

  TScriptTimer = packed record
    bActive: Boolean;
    dwExecTick: LongWord;
    dwTnterval: LongWord;
  end;
  TScriptTimers = array[0..9] of TScriptTimer;

  TColorEffect = (ceNone, ceGrayScale, ceBright, ceRed, ceGreen, ceBlue, ceYellow, ceFuchsia {, ceDark});

  TChrMsg = record
    Ident: Integer;
    x: Integer;
    y: Integer;
    dir: Integer;
    State: Integer;
    Feature: Integer;
    saying: string;
    Sound: Integer;
  end;
  pTChrMsg = ^TChrMsg;

  TDropItem = record
    x: Integer;
    y: Integer;
    ID: Integer;
    looks: Integer;
    Name: string;
    FlashTime: DWORD;
    FlashStepTime: DWORD;
    FlashStep: Integer;
    BoFlash: Boolean;
    boShowName: Boolean;
  end;
  pTDropItem = ^TDropItem;

  TUserCharacterInfo = record
    Name: string[19];
    job: Byte;
    HAIR: Byte;
    Level: Word;
    sex: Byte;
  end;

  TClientGoods = record
    Name: string;
    SubMenu: Integer;
    Price: Integer;
    Stock: Integer;
    Grade: Integer;
  end;
  PTClientGoods = ^TClientGoods;

  TRouteInfo = record
    nServerIdx: Integer;
    nGateCount: Integer;
    sSelGateIP: string[15];
    sGameGateIP: array[0..7] of string[15];
    nGameGatePort: array[0..7] of Integer;
  end;
  pTRouteInfo = ^TRouteInfo;

{$IF VER_PATHMAP}
  TPath = array of TPoint;
  pTPath = ^TPath;
{$IFEND}

  TRandomAddValue = packed record
    nAddValueRate: Integer;
    abAddValueRate: array[0..14] of Byte;
    abAddValueMaxLimit: array[0..14] of Byte;
  end;

  TRefineItem = packed record
    sUpItemName: string[15];
    nSucessRate: Integer;
    nRevertRate: Integer;
    bDelCrystal: Boolean;
    RamAddValue: TRandomAddValue;
  end;
  pTRefineItem = ^TRefineItem;

  TClientRefineItem = packed record
    nMakeIndex: Integer;
    sItemName: string[15];
  end;
  TClientRefineItems = array[0..2] of TClientRefineItem;

  THeroLevelRank = packed record        //34
    sMasterName: string[14];
    sHeroName: string[14];
    nLevel: Word;
    nIndex: Word;
  end;
  pTHeroLevelRank = ^THeroLevelRank;
  THeroLevelRanks = array[0..9] of THeroLevelRank;
  pTHeroLevelRanks = ^THeroLevelRanks;

  THumanLevelRank = packed record       //24
    sCharName: string[15];
    nLevel: Integer;
    nIndex: Integer;
  end;
  pTHumanLevelRank = ^THumanLevelRank;
  THumanLevelRanks = array[0..9] of THumanLevelRank;
  pTHumanLevelRanks = ^THumanLevelRanks;

  TRegInfo = packed record
    sKey: string;
    sServerName: string;
    sRegSrvIP: string;
    nRegPort: Integer;
    //sGateIPaddr: string[20];
  end;

  TRcHeader = packed record
    sCompany: string[050];              // = 'ÈÈÑª´«Ææ';
    sFileName: string[050];             // = 'ÈÈÑª´«ÆæµÇÂ½Æ÷';
    sWebLink: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41359108DC0D5C25228D808D71F2E82078';
    sWebSite: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A4167BEFB55C9F9AD2F';
    sBbsSite: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A4167BEFB55C9F9AD2F';

    sListSite: string[255];
    GameList: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41555B8ECAE49841B72C28D608615BB0E306F0C168FEC29FAECAFD4FDDAAE82DD8';
    GameList2: string[255];
    sSiteUrl: string[255];              // = '55B19C2CB50DA4D16F8E345A00496A41555B8ECAE49841B74F398006846553C8AA3E4F7A4F6BDEAD';
    sPathAdress: array[1..3] of string[198];
    nPicSize: Integer;
  end;
  pTRcHeader = ^TRcHeader;

  TedHeader = packed record
    //sWebSite: string[255];
    nStrLen: Integer;
    nCetLen: Integer;
    nLoginToolCrc: Integer;
  end;

  THero = record
    sCharName: string[15];
    nPort: Integer;
  end;
  pTHero = ^THero;

  TDelChar = record
    sCharName: string[14];
    nLevel: Integer;
    btJob: Byte;
    btSex: Byte;
  end;
  pTDelChar = ^TDelChar;

  //----------------------------------------------------------
  TMarketItem = record
    item: TClientItem;
    UpgCount: Integer;
    Index: Integer;
    SellPrice: Integer;
    SellWho: string[20];
    SellDate: string[10];               //(0312311210 = 2003-12-31 12:10 )
    SellState: Word
  end;
  PTMarketItem = ^TMarketItem;

  TMarketLoad = record
    UserItem: TUserItem;                // DB ÀúÀå¿ë
    Index: Integer;                     // DB ÀÎµ¦½º
    MarketType: Integer;                // ºÐ¸®µÈ ¾ÆÀÌÅÛ Á¾·ù
    SetType: Integer;                   // ¼ÂÆ® ¾ÆÀÌÅÛ Á¾·ù
    SellCount: Integer;
    SellPrice: Integer;                 // ÆÇ¸Å °¡°Ý
    ItemName: string[30];               // ¾ÆÀÌÅÛÀÌ¸§
    MarketName: string[30];             // ÆÇ¸ÅÀÚ¸í
    SellWho: string[20];                // ÆÇ¸ÅÀÚ
    SellDate: string[10];               // ÆÇ¸Å³¯Â¥(0312311210 = 2003-12-31 12:10 )
    SellState: Word;                    // 1 = ÆÇ¸ÅÁß , 2 = ÆÇ¸Å¿Ï·á
    IsOK: Integer;                      // °á°ú°ª
  end;
  PTMarketLoad = ^TMarketLoad;

  //------------------------------------------------------------
  TSearchSellItem = record
    MarketName: string[25];             // ¼­¹öÀÌ¸§_NPC  ÀÌ¸§ÀÌ »ç¿ëµÊ
    WHO: string[25];                    // ¾ÆÀÌÅÛ ÆÇ¸ÅÀÚ °Ë»ö½Ã »ç¿ë ,
    ItemName: string[25];               // ¾ÆÀÌÅÛ ÀÌ¸§ °Ë»ö½Ã »ç¿ë
    MakeIndex: Integer;                 // ¾ÆÀÌÅÛÀÇ À¯´ÏÅ© ¹øÈ£
    ItemType: Integer;                  // ¾ÆÀÌÅ× Á¾·ù °Ë»ö½Ã »ç¿ë
    ItemSet: Integer;                   // ¾ÆÀÌÅÛ ¼ÂÆ® Á¶È¸½Ã »ç¿ë
    sellindex: Integer;                 // ÆÇ¸Å ÀÎµ¦½º ¾ÆÀÌÅÛ »ì¶§ , Ãë¼Ò , ±Ý¾×È¸¼öµî¿¡ »ç¿ë
    CheckType: Integer;                 // DB ÀÇ Ã¼Å©Å¸ÀÔ
    IsOK: Integer;                      // °á°ú°ª
    UserMode: Integer;                  // 1= ¾ÆÀÌÅÛ »ç±â  , 2= ÀÚ½ÅÀÇ ¾ÆÀÌÅÛ °Ë»ö
    pList: TList;                       // À§Å¹¾ÆÀÌÅÛÀÇ ¸®½ºÆ®
  end;
  PTSearchSellItem = ^TSearchSellItem;

  //------------------------------------------------------------
  TMarKetReqInfo = record
    UserName: string[30];
    MarketName: string[30];
    SearchWho: string[30];
    SearchItem: string[30];
    ItemType: Integer;
    ItemSet: Integer;
    UserMode: Integer;
  end;

  TCenterMsg = record
    s: string;
    fc: Integer;
    bc: Integer;
    dwSec: Integer;
    dwNow: LongWord;
    dwCloseTick: LongWord;
  end;
  PTCenterMsg = ^TCenterMsg;

  TDelayCallNPC = record
    nDelayCall: Integer;
    dwDelayCallTick: LongWord;
    boDelayCall: Boolean;
    DelayCallNPC: Integer;
    sDelayCallLabel: string;
  end;
  PTDelayCallNPC = ^TDelayCallNPC;

  TShortStr = string[255];
  PTShortStr = ^TShortStr;

function APPRfeature(cfeature: Integer): Word;
function RACEfeature(cfeature: Integer): Byte;
function HAIRfeature(cfeature: Integer): Byte;
function DRESSfeature(cfeature: Integer): Byte;
function WEAPONfeature(cfeature: Integer): Byte;
function Horsefeature(cfeature: Integer): Byte;
function Effectfeature(cfeature: Integer): Byte;
function MakeHumanFeature(btRaceImg, btDress, btWeapon, btHair: Byte): Integer;
function MakeMonsterFeature(btRaceImg, btWeapon: Byte; wAppr: Word): Integer;

const
  g_dwIPNeedExps            : TIPLvlNeedList = (
    {01}(nPower: 011; dwPExp: 0069470; dwHExp: 0091080),
    {02}(nPower: 014; dwPExp: 0112690; dwHExp: 0149440),
    {03}(nPower: 019; dwPExp: 0186190; dwHExp: 0238680),
    {04}(nPower: 026; dwPExp: 0291170; dwHExp: 0360000), //     1¼¶Å­Ö®¹¥É±  1¼¶Å­Ö®»ðÇò
    {05}(nPower: 035; dwPExp: 0428830; dwHExp: 0514600),
    {06}(nPower: 046; dwPExp: 0600370; dwHExp: 0703680),
    {07}(nPower: 059; dwPExp: 0806990; dwHExp: 0928440),
    {08}(nPower: 074; dwPExp: 1049890; dwHExp: 1190080), //     2¼¶Å­Ö®¹¥É±
    {09}(nPower: 091; dwPExp: 1330270; dwHExp: 1489800), //                  1¼¶Å­Ö®µØÓü»ð
    {10}(nPower: 110; dwPExp: 1649330; dwHExp: 1828800), //
    {11}(nPower: 131; dwPExp: 2008270; dwHExp: 2208280), //                  2¼¶Å­Ö®»ðÇò
    {12}(nPower: 154; dwPExp: 2408290; dwHExp: 2629440), //                                             1¼¶¾²Ö®»ðÇò
    {13}(nPower: 179; dwPExp: 2850590; dwHExp: 3093480), //     1¼¶Å­Ö®°ëÔÂ
    {14}(nPower: 206; dwPExp: 3336370; dwHExp: 3601600), //                  1¼¶Å­Ö®±¬ÁÑ»ðÑæ            1¼¶¾²Ö®¹¥É±
    {15}(nPower: 235; dwPExp: 3866830; dwHExp: 4155000), //                  2¼¶Å­Ö®µØÓü»ð
    {16}(nPower: 266; dwPExp: 4443170; dwHExp: 4754880), //                  3¼¶Å­Ö®»ðÇò                1¼¶¾²Ö®µØÓü»ð
    {17}(nPower: 299; dwPExp: 5066590; dwHExp: 5402440), //     3¼¶Å­Ö®¹¥É±
    {18}(nPower: 334; dwPExp: 5738290; dwHExp: 6098880), //                                             2¼¶¾²Ö®¹¥É±
    {19}(nPower: 371; dwPExp: 6459470; dwHExp: 6845400), //
    {20}(nPower: 410; dwPExp: 7231330; dwHExp: 7643200), //                  2¼¶Å­Ö®±¬ÁÑ»ðÑæ            2¼¶¾²Ö®»ðÇò
    {21}(nPower: 451; dwPExp: 8055070; dwHExp: 8493480), //     2¼¶Å­Ö®°ëÔÂ                 1¼¶Å­Ö®»ð·û
    {22}(nPower: 494; dwPExp: 8931890; dwHExp: 9397440), //                  3¼¶Å­Ö®µØÓü»ð              1¼¶¾²Ö®°ëÔÂ
    {23}(nPower: 539; dwPExp: 9862990; dwHExp: 10356280), //
    {24}(nPower: 586; dwPExp: 10849570; dwHExp: 11371200), //                 3¼¶Å­Ö®±¬ÁÑ»ðÑæ            2¼¶¾²Ö®µØÓü»ð
    {25}(nPower: 635; dwPExp: 11892830; dwHExp: 12443400), //
    {26}(nPower: 686; dwPExp: 12993970; dwHExp: 13574080), //                 1¼¶Å­Ö®À×µçÊõ              3¼¶¾²Ö®»ðÇò
    {27}(nPower: 739; dwPExp: 14154190; dwHExp: 14764440), //
    {28}(nPower: 794; dwPExp: 15374690; dwHExp: 16015680), //                 1¼¶Å­Ö®´ó»ðÇò              1¼¶¾²Ö®´ó»ðÇò
    {29}(nPower: 851; dwPExp: 16656670; dwHExp: 17329000), //                                            1¼¶¾²Ö®±¬ÁÑ»ðÑæ
    {30}(nPower: 910; dwPExp: 18001330; dwHExp: 18705600), //                                            3¼¶¾²Ö®¹¥É±
    {31}(nPower: 971; dwPExp: 19409870; dwHExp: 20146680), //     3¼¶Å­Ö®°ëÔÂ
    {32}(nPower: 1034; dwPExp: 20883490; dwHExp: 21653440), //                  1¼¶Å­Ö®»ðÇ½               1¼¶¾²Ö®À×µç
    {33}(nPower: 1099; dwPExp: 22423390; dwHExp: 23227080), //                                            3¼¶¾²Ö®µØÓü»ð
    {34}(nPower: 1166; dwPExp: 24030770; dwHExp: 24868800), //                  2¼¶Å­Ö®´ó»ðÇò             2¼¶¾²Ö®´ó»ðÇò
    {35}(nPower: 1235; dwPExp: 25706830; dwHExp: 26579800), //                                            1¼¶¾²Ö®»ð·û
    {36}(nPower: 1306; dwPExp: 27452770; dwHExp: 28361280), //                                            2¼¶¾²Ö®°ëÔÂ
    {37}(nPower: 1379; dwPExp: 29269790; dwHExp: 30214440), //                  2¼¶Å­Ö®À×µçÊõ
    {38}(nPower: 1454; dwPExp: 31159090; dwHExp: 32140480), //    1¼¶Å­Ö®ÄÚ¹¦½£·¨
    {39}(nPower: 1531; dwPExp: 33121870; dwHExp: 34140600), //                                            1¼¶¾²Ö®»ðÇ½
    {40}(nPower: 1610; dwPExp: 35159330; dwHExp: 36216000), //                  3¼¶Å­Ö®´ó»ðÇò             2¼¶¾²Ö®±¬ÁÑ»ðÑæ 1¼¶¾²Ö®ÄÚ¹¦½£·¨
    {41}(nPower: 1691; dwPExp: 37272670; dwHExp: 38367880), //                                            2¼¶¾²Ö®À×µç
    {42}(nPower: 1774; dwPExp: 39463090; dwHExp: 40597440), //                                            1¼¶¾²Ö®¼²¹âµçÓ°
    {43}(nPower: 1859; dwPExp: 41731790; dwHExp: 42905880), //                  3¼¶Å­Ö®À×µçÊõ             3¼¶¾²Ö®´ó»ðÇò
    {44}(nPower: 1946; dwPExp: 44079970; dwHExp: 45294400), //                                            2¼¶¾²Ö®»ð·û
    {45}(nPower: 2035; dwPExp: 46508830; dwHExp: 47764200), //
    {46}(nPower: 2126; dwPExp: 49019570; dwHExp: 50316480), //                  2¼¶Å­Ö®»ðÇ½
    {47}(nPower: 2219; dwPExp: 51613390; dwHExp: 52952440), //                                2¼¶Å­Ö®»ð·û 3¼¶¾²Ö®°ëÔÂ
    {48}(nPower: 2314; dwPExp: 54291490; dwHExp: 55673280), //                                            2¼¶¾²Ö®»ðÇ½
    {49}(nPower: 2411; dwPExp: 57055070; dwHExp: 58480200), //                  1¼¶Å­Ö®¼²¹âµçÓ°
    {50}(nPower: 2510; dwPExp: 59905330; dwHExp: 61374400), //    2¼¶Å­Ö®ÄÚ¹¦½£·¨                         3¼¶¾²Ö®À×µç
    {51}(nPower: 2611; dwPExp: 62843470; dwHExp: 64357080), //                                            1¼¶¾²Ö®µØÓüÀ×¹â
    {52}(nPower: 2714; dwPExp: 65870690; dwHExp: 67429440), //                  3¼¶Å­Ö®»ðÇ½               2¼¶¾²Ö®ÄÚ¹¦½£·¨
    {53}(nPower: 2819; dwPExp: 68988190; dwHExp: 70592680), //                                            3¼¶¾²Ö®±¬ÁÑ»ðÑæ
    {54}(nPower: 2926; dwPExp: 72197170; dwHExp: 73848000), //
    {55}(nPower: 3035; dwPExp: 75498830; dwHExp: 77196600), //                  1¼¶Å­Ö®µØÓüÀ×¹â
    {56}(nPower: 3146; dwPExp: 78894370; dwHExp: 80639680), //
    {57}(nPower: 3259; dwPExp: 82384990; dwHExp: 84178440), //                                            2¼¶¾²Ö®¼²¹âµçÓ°
    {58}(nPower: 3374; dwPExp: 85971890; dwHExp: 87814080), //                  2¼¶Å­Ö®¼²¹âµçÓ°
    {59}(nPower: 3491; dwPExp: 89656270; dwHExp: 91547800), //                                            3¼¶¾²Ö®»ðÇ½
    {60}(nPower: 3610; dwPExp: 93439330; dwHExp: 95380800), //                                 3¼¶Å­Ö®»ð·û
    {61}(nPower: 3731; dwPExp: 97322270; dwHExp: 99314280), //                  1¼¶Å­Ö®º®±ùÕÆ             1¼¶¾²Ö®º®±ùÕÆ
    {62}(nPower: 3854; dwPExp: 101306290; dwHExp: 103349440), //
    {63}(nPower: 3979; dwPExp: 105392590; dwHExp: 107487480), //   3¼¶Å­Ö®ÄÚ¹¦½£·¨                         3¼¶¾²Ö®»ð·û
    {64}(nPower: 4106; dwPExp: 109582370; dwHExp: 111729600), //                 3¼¶Å­Ö®¼²¹âµçÓ°
    {65}(nPower: 4235; dwPExp: 113876830; dwHExp: 116077000), //                                           3¼¶¾²Ö®ÄÚ¹¦½£·¨ 3¼¶¾²Ö®¼²¹âµçÓ°
    {66}(nPower: 4366; dwPExp: 118277170; dwHExp: 120530880), //
    {67}(nPower: 4499; dwPExp: 122784590; dwHExp: 125092440), //                 2¼¶Å­Ö®µØÓüÀ×¹â
    {68}(nPower: 4634; dwPExp: 127400290; dwHExp: 129762880), //                                           2¼¶¾²Ö®µØÓüÀ×¹â
    {69}(nPower: 4771; dwPExp: 132125470; dwHExp: 134543400), //                                           1¼¶¾²Ö®ÁÒ»ð
    {70}(nPower: 4910; dwPExp: 136961330; dwHExp: 139435200), //   1¼¶Å­Ö®ÁÒ»ð   1¼¶Å­Ö®±ùÅØÏø   1¼¶ÊÉÑªÊõ
    {71}(nPower: 5051; dwPExp: 141909070; dwHExp: 144439480), //                 1¼¶Å­Ö®ÃðÌì»ð             1¼¶¾²Ö®±ùÅØÏø
    {72}(nPower: 5194; dwPExp: 146969890; dwHExp: 149557440), //                 2¼¶Å­Ö®º®±ùÕÆ
    {73}(nPower: 5339; dwPExp: 152144990; dwHExp: 154790280), //                 2¼¶Å­Ö®±ùÅØÏø             3¼¶¾²Ö®µØÓüÀ×¹â
    {74}(nPower: 5486; dwPExp: 157435570; dwHExp: 160139200), //   1¼¶Å­Ö®ÖðÈÕ   1¼¶Å­Ö®Á÷ÐÇ»ðÓê
    {75}(nPower: 5635; dwPExp: 162842830; dwHExp: 165605400), //                 3¼¶Å­Ö®º®±ùÕÆ             1¼¶¾²Ö®ÃðÌì»ð
    {76}(nPower: 5786; dwPExp: 168367970; dwHExp: 171190080), //                 3¼¶Å­Ö®µØÓüÀ×¹â           2¼¶¾²Ö®º®±ùÕÆ
    {77}(nPower: 5939; dwPExp: 174012190; dwHExp: 176894440), //                 3¼¶Å­Ö®±ùÅØÏø             2¼¶¾²Ö®±ùÅØÏø
    {78}(nPower: 6094; dwPExp: 179776690; dwHExp: 182719680), //   2¼¶Å­Ö®ÁÒ»ð   2¼¶Å­Ö®ÃðÌì»ð
    {79}(nPower: 6251; dwPExp: 185662670; dwHExp: 188667000), //                                           1¼¶¾²Ö®ÊÉÑª 1¼¶¾²Ö®ÖðÈÕ
    {80}(nPower: 6410; dwPExp: 191671330; dwHExp: 194737600), //                                           2¼¶¾²Ö®ÁÒ»ð 1¼¶¾²Ö®Á÷ÐÇ»ðÓê
    {81}(nPower: 6571; dwPExp: 197803870; dwHExp: 200932680), //                                           3¼¶¾²Ö®º®±ùÕÆ
    {82}(nPower: 6734; dwPExp: 204061490; dwHExp: 207253440), //   2¼¶Å­Ö®ÖðÈÕ   2¼¶Å­Ö®Á÷ÐÇ»ðÓê  2¼¶Å­Ö®ÊÉÑª
    {83}(nPower: 6899; dwPExp: 210445390; dwHExp: 213701080), //                                           3¼¶¾²Ö®±ùÅØÏø 2¼¶¾²Ö®Á÷ÐÇ»ðÓê
    {84}(nPower: 7066; dwPExp: 216956770; dwHExp: 220276800), //                                           2¼¶¾²Ö®ÊÉÑª
    {85}(nPower: 7235; dwPExp: 223596830; dwHExp: 226981800), //                                           2¼¶¾²Ö®ÃðÌì»ð 2¼¶¾²Ö®ÖðÈÕ
    {86}(nPower: 7406; dwPExp: 230366770; dwHExp: 233817280), //   3¼¶Å­Ö®ÁÒ»ð   3¼¶Å­Ö®ÃðÌì»ð
    {87}(nPower: 7579; dwPExp: 237267790; dwHExp: 240784440), //                                           3¼¶¾²Ö®ÁÒ»ð
    {88}(nPower: 7754; dwPExp: 244301090; dwHExp: 247884480), //                                           3¼¶¾²Ö®ÃðÌì»ð 3¼¶¾²Ö®ÖðÈÕ
    {89}(nPower: 7931; dwPExp: 251467870; dwHExp: 255118600), //                                           3¼¶¾²Ö®ÊÉÑª   3¼¶¾²Ö®Á÷ÐÇ»ðÓê
    {90}(nPower: 8110; dwPExp: 258769330; dwHExp: 262488000), //   3¼¶Å­Ö®ÖðÈÕ   3¼¶Å­Ö®Á÷ÐÇ»ðÓê  3¼¶Å­Ö®ÊÉÑª
    {91}(nPower: 8291; dwPExp: 266206670; dwHExp: 269993880), //
    {92}(nPower: 8474; dwPExp: 273781090; dwHExp: 277637440), //
    {93}(nPower: 8659; dwPExp: 281493790; dwHExp: 285419880), //
    {94}(nPower: 8846; dwPExp: 289345970; dwHExp: 293342400), //
    {95}(nPower: 9035; dwPExp: 297338830; dwHExp: 301406200), //
    {96}(nPower: 9226; dwPExp: 305473570; dwHExp: 309612480), //
    {97}(nPower: 9419; dwPExp: 313751390; dwHExp: 317962440), //
    {98}(nPower: 9614; dwPExp: 322173490; dwHExp: 326457280), //
    {99}(nPower: 9811; dwPExp: 330741070; dwHExp: 335098200), //
    {100}(nPower: 10010; dwPExp: 339455330; dwHExp: 343886400) //
    );

implementation

function WEAPONfeature(cfeature: Integer): Byte;
begin
  Result := HiByte(cfeature);
end;

function DRESSfeature(cfeature: Integer): Byte;
begin
  Result := HiByte(HiWord(cfeature));
end;

function APPRfeature(cfeature: Integer): Word;
begin
  Result := HiWord(cfeature);
end;

function HAIRfeature(cfeature: Integer): Byte;
begin
  Result := HiWord(cfeature);
end;

function RACEfeature(cfeature: Integer): Byte;
begin
  Result := cfeature;
end;

function Horsefeature(cfeature: Integer): Byte;
begin
  Result := LoByte(LoWord(cfeature));
end;

function Effectfeature(cfeature: Integer): Byte;
begin
  Result := HiByte(LoWord(cfeature));
end;

function MakeHumanFeature(btRaceImg, btDress, btWeapon, btHair: Byte): Integer;
begin
  Result := MakeLong(MakeWord(btRaceImg, btWeapon), MakeWord(btHair, btDress));
end;

function MakeMonsterFeature(btRaceImg, btWeapon: Byte; wAppr: Word): Integer;
begin
  Result := MakeLong(MakeWord(btRaceImg, btWeapon), wAppr);
end;

end.

