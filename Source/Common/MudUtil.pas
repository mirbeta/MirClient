unit MudUtil;

interface

uses
  Windows, Messages, SysUtils, Classes, Grobal2;

type
  TQuickID = record
    sAccount: string[20];
    sChrName: string[30];
    nIndex: Integer;
    nSelectID: Integer;
  end;
  pTQuickID = ^TQuickID;

  pTConfig = ^TConfig;

  TThreadInfo = record
    nRunFlag: Integer;                  //0x00
    boActived: BOOL;                    //0x04
    dwRunTick: LongWord;                //0x08
    Config: pTConfig;                   //0x0C
    boTerminaled: BOOL;                 //0x10
    hThreadHandle: THandle;             //0x14
    dwThreadID: LongWord;               //0x18
    nRunTime: LongWord;
    nMaxRunTime: LongWord;
  end;
  pTThreadInfo = ^TThreadInfo;

  TThreadInfos = array[0..1] of TThreadInfo;
  pTThreadInfos = ^TThreadInfos;

  TConfig = record
    nConfigSize: Integer;
    sServerName: string;
    sServerIPaddr: string;
    sWebSite: string;
    sBbsSite: string;
    sClientDownload: string;
    sQQ: string;
    sPhone: string;
    sBankAccount0: string;
    sBankAccount1: string;
    sBankAccount2: string;
    sBankAccount3: string;
    sBankAccount4: string;
    sBankAccount5: string;
    sBankAccount6: string;
    sBankAccount7: string;
    sBankAccount8: string;
    sBankAccount9: string;
    nServerNumber: Integer;
    boVentureServer: Boolean;
    boTestServer: Boolean;
    boServiceMode: Boolean;
    boNonPKServer: Boolean;
    nTestLevel: Integer;
    nTestGold: Integer;
    nTestUserLimit: Integer;
    nSendBlock: Integer;
    nCheckBlock: Integer;
    nAvailableBlock: Integer;
    nGateLoad: Integer;
    nUserFull: Integer;
    nZenFastStep: Integer;
    sGateAddr: string;
    nGatePort: Integer;
    sDBAddr: string;
    nDBPort: Integer;
    sIDSAddr: string;
    nIDSPort: Integer;
    sMsgSrvAddr: string;
    nMsgSrvPort: Integer;
    sLogServerAddr: string;
    nLogServerPort: Integer;
    boDiscountForNightTime: Boolean;
    nHalfFeeStart: Integer;
    nHalfFeeEnd: Integer;
    boViewHackMessage: Boolean;
    boViewAdmissionFailure: Boolean;
    sBaseDir: string;
    sGuildDir: string;
    sGuildFile: string;
    sVentureDir: string;
    sConLogDir: string;
    sCastleDir: string;
    sCastleFile: string;
    sEnvirDir: string;
    sMapDir: string;
    sNoticeDir: string;
    sLogDir: string;
    sPlugDir: string;
    sUserDataDir: string;
    sClientFile1: string;
    sClientFile2: string;
    sClientFile3: string;

    sClothsMan: string;
    sClothsWoman: string;
    sWoodenSword: string;
    sCandle: string;
    sBasicDrug: string;
    sGoldStone: string;
    sSilverStone: string;
    sSteelStone: string;
    sCopperStone: string;
    sBlackStone: string;
    sZuma: array[0..3] of string;
    sBee: string;
    sSpider: string;
    sWomaHorn: string;
    sZumaPiece: string;
    sGameGoldName: string;
    sGamePointName: string;
    sPayMentPointName: string;
    sBloodMonSlave: array[1..3] of string;
    DBSocket: Integer;
    nHealthFillTime: Integer;
    nSpellFillTime: Integer;
    nMonUpLvNeedKillBase: Integer;
    nMonUpLvRate: Integer;
    MonUpLvNeedKillCount: array[0..14] of Integer;
    SlaveColor: array[0..14] of byte;
    dwNeedExps: TLevelNeedExp;
    WideAttack: array[0..2] of byte;
    CrsAttack: array[0..6] of byte;
    SpitMap: array[0..7, 0..4, 0..4] of byte;
    sHomeMap: string;
    nHomeX: Integer;
    nHomeY: Integer;
    sRedHomeMap: string;
    nRedHomeX: Integer;
    nRedHomeY: Integer;
    sRedDieHomeMap: string;
    nRedDieHomeX: Integer;
    nRedDieHomeY: Integer;
    dwDecPkPointTime: Integer;
    nDecPkPointCount: Integer;
    dwPKFlagTime: Integer;
    nKillHumanAddPKPoint: Integer;
    nKillHumanDecLuckPoint: Integer;
    dwDecLightItemDrugTime: Integer;
    nSafeZoneSize: Integer;
    nStartPointSize: Integer;
    nNonGuildWarZoneSize: Integer;
    boSafeZoneAureole: Boolean;
    dwHumanGetMsgTime: Integer;
    nGroupMembersMax: Integer;
    sFireBallSkill: string;
    sHealSkill: string;
    ReNewNameColor: array[0..9] of byte;
    dwReNewNameColorTime: Integer;
    boReNewChangeColor: Boolean;
    btReNewChangeColorLevel: byte;
    boReNewLevelClearExp: Boolean;
    BonusAbilofWarr: TNakedAbility;
    BonusAbilofWizard: TNakedAbility;
    BonusAbilofTaos: TNakedAbility;
    NakedAbilofWarr: TNakedAbility;
    NakedAbilofWizard: TNakedAbility;
    NakedAbilofTaos: TNakedAbility;
    nUpgradeWeaponMaxPoint: Integer;
    nUpgradeWeaponPrice: Integer;
    dwUPgradeWeaponGetBackTime: Integer;
    nClearExpireUpgradeWeaponDays: Integer;
    nUpgradeWeaponDCRate: Integer;
    nUpgradeWeaponDCTwoPointRate: Integer;
    nUpgradeWeaponDCThreePointRate: Integer;
    nUpgradeWeaponSCRate: Integer;
    nUpgradeWeaponSCTwoPointRate: Integer;
    nUpgradeWeaponSCThreePointRate: Integer;
    nUpgradeWeaponMCRate: Integer;
    nUpgradeWeaponMCTwoPointRate: Integer;
    nUpgradeWeaponMCThreePointRate: Integer;
    dwProcessMonstersTime: Integer;
    dwRegenMonstersTime: Integer;
    nMonGenRate: Integer;
    nProcessMonRandRate: Integer;
    nProcessMonLimitCount: Integer;
    nSoftVersionDate: Integer;
    boCanOldClientLogon: Boolean;
    dwConsoleShowUserCountTime: Integer;
    dwShowLineNoticeTime: Integer;
    nLineNoticeColor: Integer;
    nStartCastleWarDays: Integer;
    nStartCastlewarTime: Integer;
    dwShowCastleWarEndMsgTime: Integer;
    dwCastleWarTime: LongWord;
    dwGetCastleTime: Integer;
    dwGuildWarTime: Integer;
    nBuildGuildPrice: Integer;
    nGuildWarPrice: Integer;
    nMakeDurgPrice: Integer;
    nHumanMaxGold: Integer;
    nHumanTryModeMaxGold: Integer;
    nTryModeLevel: Integer;
    boTryModeUseStorage: Boolean;
    nCanShoutMsgLevel: Integer;
    boShowMakeItemMsg: Boolean;
    boShutRedMsgShowGMName: Boolean;
    nSayMsgMaxLen: Integer;
    dwSayMsgTime: Integer;
    nSayMsgCount: Integer;
    dwDisableSayMsgTime: Integer;
    nSayRedMsgMaxLen: Integer;
    boShowGuildName: Boolean;
    boShowRankLevelName: Boolean;
    boMonSayMsg: Boolean;
    nStartPermission: Integer;
    boKillHumanWinLevel: Boolean;
    boKilledLostLevel: Boolean;
    boKillHumanWinExp: Boolean;
    boKilledLostExp: Boolean;
    nKillHumanWinLevel: Integer;
    nKilledLostLevel: Integer;
    nKillHumanWinExp: Integer;
    nKillHumanLostExp: Integer;
    nHumanLevelDiffer: Integer;
    nMonsterPowerRate: Integer;
    nItemsPowerRate: Integer;
    nItemsACPowerRate: Integer;
    boSendOnlineCount: Boolean;
    nSendOnlineCountRate: Integer;
    dwSendOnlineTime: Integer;
    dwSaveHumanRcdTime: Integer;
    dwHumanFreeDelayTime: Integer;
    dwMakeGhostTime: Integer;
    dwClearDropOnFloorItemTime: Integer;
    dwFloorItemCanPickUpTime: Integer;
    boPasswordLockSystem: Boolean;      //是否启用密码保护系统
    boLockDealAction: Boolean;          //是否锁定交易操作
    boLockDropAction: Boolean;          //是否锁定扔物品操作
    boLockGetBackItemAction: Boolean;   //是否锁定取仓库操作
    boLockHumanLogin: Boolean;          //是否锁定走操作
    boLockWalkAction: Boolean;          //是否锁定走操作
    boLockRunAction: Boolean;           //是否锁定跑操作
    boLockHitAction: Boolean;           //是否锁定攻击操作
    boLockSpellAction: Boolean;         //是否锁定魔法操作
    boLockSendMsgAction: Boolean;       //是否锁定发信息操作
    boLockUserItemAction: Boolean;      //是否锁定使用物品操作
    boLockInObModeAction: Boolean;      //锁定时进入隐身状态
    nPasswordErrorCountLock: Integer;   //输入密码错误超过 指定次数则锁定密码
    boPasswordErrorKick: Boolean;       //输入密码错误超过限制则踢下线
    boLockRecallAction: Boolean;
    nSendRefMsgRange: Integer;
    boDecLampDura: Boolean;
    boHungerSystem: Boolean;
    boHungerDecHP: Boolean;
    boHungerDecPower: Boolean;
    boDiableHumanRun: Boolean;
    boRUNHUMAN: Boolean;
    boRUNMON: Boolean;
    boRunNpc: Boolean;
    boRunGuard: Boolean;
    boWarDisHumRun: Boolean;
    boGMRunAll: Boolean;
    boSafeZoneRunAll: Boolean;
    dwTryDealTime: Integer;
    dwDealOKTime: Integer;
    boCanNotGetBackDeal: Boolean;
    boDisableDeal: Boolean;
    nMasterOKLevel: Integer;
    nMasterOKCreditPoint: Integer;
    nMasterOKBonusPoint: Integer;
    boPKLevelProtect: Boolean;
    nPKProtectLevel: Integer;
    nRedPKProtectLevel: Integer;
    nItemPowerRate: Integer;
    nItemExpRate: Integer;
    nItemAcRate: Integer;
    nItemMacRate: Integer;
    nScriptGotoCountLimit: Integer;
    btHearMsgFColor: byte;              //前景
    btHearMsgBColor: byte;              //背景
    btWhisperMsgFColor: byte;           //前景
    btWhisperMsgBColor: byte;           //背景
    btGMWhisperMsgFColor: byte;         //前景
    btGMWhisperMsgBColor: byte;         //背景
    btCryMsgFColor: byte;               //前景
    btCryMsgBColor: byte;               //背景
    btGreenMsgFColor: byte;             //前景
    btGreenMsgBColor: byte;             //背景
    btBlueMsgFColor: byte;              //前景
    btBlueMsgBColor: byte;              //背景
    btRedMsgFColor: byte;               //前景
    btRedMsgBColor: byte;               //背景
    btGuildMsgFColor: byte;             //前景
    btGuildMsgBColor: byte;             //背景
    btGroupMsgFColor: byte;             //前景
    btGroupMsgBColor: byte;             //背景
    btCustMsgFColor: byte;              //前景
    btCustMsgBColor: byte;              //背景
    btPurpleMsgFColor: byte;
    btPurpleMsgBColor: byte;
    nMonRandomAddValue: Integer;
    nMakeRandomAddValue: Integer;
    nWeaponDCAddValueMaxLimit: Integer;
    nWeaponDCAddValueRate: Integer;
    nWeaponMCAddValueMaxLimit: Integer;
    nWeaponMCAddValueRate: Integer;
    nWeaponSCAddValueMaxLimit: Integer;
    nWeaponSCAddValueRate: Integer;
    nDressDCAddRate: Integer;
    nDressDCAddValueMaxLimit: Integer;
    nDressDCAddValueRate: Integer;
    nDressMCAddRate: Integer;
    nDressMCAddValueMaxLimit: Integer;
    nDressMCAddValueRate: Integer;
    nDressSCAddRate: Integer;
    nDressSCAddValueMaxLimit: Integer;
    nDressSCAddValueRate: Integer;
    nNeckLace202124DCAddRate: Integer;
    nNeckLace202124DCAddValueMaxLimit: Integer;
    nNeckLace202124DCAddValueRate: Integer;
    nNeckLace202124MCAddRate: Integer;
    nNeckLace202124MCAddValueMaxLimit: Integer;
    nNeckLace202124MCAddValueRate: Integer;
    nNeckLace202124SCAddRate: Integer;
    nNeckLace202124SCAddValueMaxLimit: Integer;
    nNeckLace202124SCAddValueRate: Integer;
    nNeckLace19DCAddRate: Integer;
    nNeckLace19DCAddValueMaxLimit: Integer;
    nNeckLace19DCAddValueRate: Integer;
    nNeckLace19MCAddRate: Integer;
    nNeckLace19MCAddValueMaxLimit: Integer;
    nNeckLace19MCAddValueRate: Integer;
    nNeckLace19SCAddRate: Integer;
    nNeckLace19SCAddValueMaxLimit: Integer;
    nNeckLace19SCAddValueRate: Integer;
    nArmRing26DCAddRate: Integer;
    nArmRing26DCAddValueMaxLimit: Integer;
    nArmRing26DCAddValueRate: Integer;
    nArmRing26MCAddRate: Integer;
    nArmRing26MCAddValueMaxLimit: Integer;
    nArmRing26MCAddValueRate: Integer;
    nArmRing26SCAddRate: Integer;
    nArmRing26SCAddValueMaxLimit: Integer;
    nArmRing26SCAddValueRate: Integer;
    nRing22DCAddRate: Integer;
    nRing22DCAddValueMaxLimit: Integer;
    nRing22DCAddValueRate: Integer;
    nRing22MCAddRate: Integer;
    nRing22MCAddValueMaxLimit: Integer;
    nRing22MCAddValueRate: Integer;
    nRing22SCAddRate: Integer;
    nRing22SCAddValueMaxLimit: Integer;
    nRing22SCAddValueRate: Integer;
    nRing23DCAddRate: Integer;
    nRing23DCAddValueMaxLimit: Integer;
    nRing23DCAddValueRate: Integer;
    nRing23MCAddRate: Integer;
    nRing23MCAddValueMaxLimit: Integer;
    nRing23MCAddValueRate: Integer;
    nRing23SCAddRate: Integer;
    nRing23SCAddValueMaxLimit: Integer;
    nRing23SCAddValueRate: Integer;
    nHelMetDCAddRate: Integer;
    nHelMetDCAddValueMaxLimit: Integer;
    nHelMetDCAddValueRate: Integer;
    nHelMetMCAddRate: Integer;
    nHelMetMCAddValueMaxLimit: Integer;
    nHelMetMCAddValueRate: Integer;
    nHelMetSCAddRate: Integer;
    nHelMetSCAddValueMaxLimit: Integer;
    nHelMetSCAddValueRate: Integer;
    nUnknowHelMetACAddRate: Integer;
    nUnknowHelMetACAddValueMaxLimit: Integer;
    nUnknowHelMetMACAddRate: Integer;
    nUnknowHelMetMACAddValueMaxLimit: Integer;
    nUnknowHelMetDCAddRate: Integer;
    nUnknowHelMetDCAddValueMaxLimit: Integer;
    nUnknowHelMetMCAddRate: Integer;
    nUnknowHelMetMCAddValueMaxLimit: Integer;
    nUnknowHelMetSCAddRate: Integer;
    nUnknowHelMetSCAddValueMaxLimit: Integer;
    nUnknowRingACAddRate: Integer;
    nUnknowRingACAddValueMaxLimit: Integer;
    nUnknowRingMACAddRate: Integer;
    nUnknowRingMACAddValueMaxLimit: Integer;
    nUnknowRingDCAddRate: Integer;
    nUnknowRingDCAddValueMaxLimit: Integer;
    nUnknowRingMCAddRate: Integer;
    nUnknowRingMCAddValueMaxLimit: Integer;
    nUnknowRingSCAddRate: Integer;
    nUnknowRingSCAddValueMaxLimit: Integer;
    nUnknowNecklaceACAddRate: Integer;
    nUnknowNecklaceACAddValueMaxLimit: Integer;
    nUnknowNecklaceMACAddRate: Integer;
    nUnknowNecklaceMACAddValueMaxLimit: Integer;
    nUnknowNecklaceDCAddRate: Integer;
    nUnknowNecklaceDCAddValueMaxLimit: Integer;
    nUnknowNecklaceMCAddRate: Integer;
    nUnknowNecklaceMCAddValueMaxLimit: Integer;
    nUnknowNecklaceSCAddRate: Integer;
    nUnknowNecklaceSCAddValueMaxLimit: Integer;
    nMonOneDropGoldCount: Integer;
    nMakeMineHitRate: Integer;          //挖矿命中率
    nMakeMineRate: Integer;             //挖矿率
    nStoneTypeRate: Integer;
    nStoneTypeRateMin: Integer;
    nGoldStoneMin: Integer;
    nGoldStoneMax: Integer;
    nSilverStoneMin: Integer;
    nSilverStoneMax: Integer;
    nSteelStoneMin: Integer;
    nSteelStoneMax: Integer;
    nBlackStoneMin: Integer;
    nBlackStoneMax: Integer;
    nStoneMinDura: Integer;
    nStoneGeneralDuraRate: Integer;
    nStoneAddDuraRate: Integer;
    nStoneAddDuraMax: Integer;
    nWinLottery6Min: Integer;
    nWinLottery6Max: Integer;
    nWinLottery5Min: Integer;
    nWinLottery5Max: Integer;
    nWinLottery4Min: Integer;
    nWinLottery4Max: Integer;
    nWinLottery3Min: Integer;
    nWinLottery3Max: Integer;
    nWinLottery2Min: Integer;
    nWinLottery2Max: Integer;
    nWinLottery1Min: Integer;
    nWinLottery1Max: Integer;           //16180 + 1820;
    nWinLottery1Gold: Integer;
    nWinLottery2Gold: Integer;
    nWinLottery3Gold: Integer;
    nWinLottery4Gold: Integer;
    nWinLottery5Gold: Integer;
    nWinLottery6Gold: Integer;
    nWinLotteryRate: Integer;
    nWinLotteryCount: Integer;
    nNoWinLotteryCount: Integer;
    nWinLotteryLevel1: Integer;
    nWinLotteryLevel2: Integer;
    nWinLotteryLevel3: Integer;
    nWinLotteryLevel4: Integer;
    nWinLotteryLevel5: Integer;
    nWinLotteryLevel6: Integer;
    GlobalVal: array[0..99] of Integer;
    GlobaDyTval: array[0..99] of string;

    nItemNumber: Integer;
    nItemNumberEx: Integer;
    nGuildRecallTime: Integer;
    nGroupRecallTime: Integer;
    boControlDropItem: Boolean;
    boInSafeDisableDrop: Boolean;
    nCanDropGold: Integer;
    nCanDropPrice: Integer;
    boSendCustemMsg: Boolean;
    boSubkMasterSendMsg: Boolean;
    nSuperRepairPriceRate: Integer;
    nRepairItemDecDura: Integer;
    boDieScatterBag: Boolean;
    nDieScatterBagRate: Integer;
    boDieRedScatterBagAll: Boolean;
    nDieDropUseItemRate: Integer;
    nDieRedDropUseItemRate: Integer;
    nMonDieDropUseItemRate: Integer;
    boDieDropGold: Boolean;
    boKillByHumanDropUseItem: Boolean;
    boKillByMonstDropUseItem: Boolean;
    boKickExpireHuman: Boolean;
    nGuildRankNameLen: Integer;
    nGuildMemberMaxLimit: Integer;
    nGuildNameLen: Integer;
    nAttackPosionRate: Integer;
    nAttackPosionTime: Integer;
    boAutoClearEctype: Boolean;
    nAutoClearEctypeTick: Integer;
    dwRevivalTime: Integer;
    boUserMoveCanDupObj: Boolean;
    boUserMoveCanOnItem: Boolean;
    dwUserMoveTime: Integer;
    dwPKDieLostExpRate: Integer;
    nPKDieLostLevelRate: Integer;
    btPKFlagNameColor: byte;
    btPKLevel1NameColor: byte;
    btPKLevel2NameColor: byte;
    btAllyAndGuildNameColor: byte;
    btWarGuildNameColor: byte;
    btInFreePKAreaNameColor: byte;
    boSpiritMutiny: Boolean;
    dwSpiritMutinyTime: Integer;
    nSpiritPowerRate: Integer;
    boMasterDieMutiny: Boolean;
    nMasterDieMutinyRate: Integer;
    nMasterDieMutinyPower: Integer;
    nMasterDieMutinySpeed: Integer;
    boBBMonAutoChangeColor: Boolean;
    dwBBMonAutoChangeColorTime: Integer;
    boOldClientShowHiLevel: Boolean;
    boShowScriptActionMsg: Boolean;
    nRunSocketDieLoopLimit: Integer;
    boThreadRun: Boolean;
    boShowExceptionMsg: Boolean;
    boShowPreFixMsg: Boolean;
    nMagicAttackRage: Integer;          //魔法锁定范围
    sBody: string;
    nBodyCount: Integer;
    boAllowBodyMakeSlave: Boolean;
    boMoveMakeSlave: Boolean; //道士心灵召唤  Development 2019-01-12 添加
    sBoneFamm: string;
    nBoneFammCount: Integer;
    BoneFammArray: array[0..9] of TRecallMigic;
    sDogz: string;
    nDogzCount: Integer;
    DogzArray: array[0..9] of TRecallMigic;
    nAmyOunsulPoint: Integer;
    boDisableInSafeZoneFireCross: Boolean;
    boGroupMbAttackPlayObject: Boolean;
    boGroupMbAttackBaoBao: Boolean;
    dwPosionDecHealthTime: Integer;
    nPosionDamagarmor: Integer;         //中红毒着持久及减防量（实际大小为 12 / 10）
    boLimitSwordLong: Boolean;
    nSwordLongPowerRate: Integer;
    nFireBoomRage: Integer;
    nSnowWindRange: Integer;
    nElecBlizzardRange: Integer;
    nMagTurnUndeadLevel: Integer;       //圣言怪物等级限制
    nMagTammingLevel: Integer;          //诱惑之光怪物等级限制
    nMagTammingTargetLevel: Integer;    //诱惑怪物相差等级机率，此数字越小机率越大；
    nMagTammingHPRate: Integer;
    nMagTammingCount: Integer;
    nMabMabeHitRandRate: Integer;
    nMabMabeHitMinLvLimit: Integer;
    nMabMabeHitSucessRate: Integer;
    nMabMabeHitMabeTimeRate: Integer;
    sCASTLENAME: string;
    sCastleHomeMap: string;
    nCastleHomeX: Integer;
    nCastleHomeY: Integer;
    nCastleWarRangeX: Integer;
    nCastleWarRangeY: Integer;
    nCastleTaxRate: Integer;
    boGetAllNpcTax: Boolean;
    nHireGuardPrice: Integer;
    nHireArcherPrice: Integer;
    nCastleGoldMax: Integer;
    nCastleOneDayGold: Integer;
    nRepairDoorPrice: Integer;
    nRepairWallPrice: Integer;
    nCastleMemberPriceRate: Integer;
    nMaxHitMsgCount: Integer;
    nMaxSpellMsgCount: Integer;
    nMaxRunMsgCount: Integer;
    nMaxWalkMsgCount: Integer;
    nMaxTurnMsgCount: Integer;
    nMaxSitDonwMsgCount: Integer;
    nMaxDigUpMsgCount: Integer;
    boSpellSendUpdateMsg: Boolean;
    boActionSendActionMsg: Boolean;
    boKickOverSpeed: Boolean;
    btSpeedControlMode: Integer;
    nOverSpeedKickCount: Integer;
    dwDropOverSpeed: Integer;
    dwHitIntervalTime: Integer;         //攻击间隔
    dwMagicHitIntervalTime: Integer;    //魔法间隔
    dwRunIntervalTime: Integer;         //跑步间隔
    dwWalkIntervalTime: Integer;        //走路间隔
    dwTurnIntervalTime: Integer;        //换方向间隔
    boControlActionInterval: Boolean;
    boControlWalkHit: Boolean;
    boControlRunLongHit: Boolean;
    boControlRunHit: Boolean;
    boControlRunMagic: Boolean;
    dwActionIntervalTime: Integer;      //组合操作间隔
    dwRunLongHitIntervalTime: Integer;  //跑位刺杀间隔
    dwRunHitIntervalTime: Integer;      //跑位攻击间隔
    dwWalkHitIntervalTime: Integer;     //走位攻击间隔
    dwRunMagicIntervalTime: Integer;    //跑位魔法间隔
    boDisableStruck: Boolean;           //不显示人物弯腰动作
    boDisableSelfStruck: Boolean;       //自己不显示人物弯腰动作
    boHeroDisableStruck: Boolean;
    dwStruckTime: Integer;              //人物弯腰停留时间
    dwKillMonExpMultiple: Integer;      //杀怪经验倍数
{$IF SoftVersion = VERENT}
    dwRequestVersion: Integer;
{$ELSE}
    dwRequestVersion: Integer;
{$IFEND}
    boHighLevelKillMonFixExp: Boolean;
    boHighLevelGroupFixExp: Boolean;
    boHighLevelLimitExp: Boolean;
    nLimitLevel: Integer;
    nKillMonExpDiv: Integer;
    boAddUserItemNewValue: Boolean;
    sLineNoticePreFix: string;
    sSysMsgPreFix: string;
    sGuildMsgPreFix: string;
    sGroupMsgPreFix: string;
    sHintMsgPreFix: string;
    sGMRedMsgpreFix: string;
    sMonSayMsgpreFix: string;
    sCustMsgpreFix: string;
    sCastleMsgpreFix: string;
    sGuildNotice: string;
    sGuildWar: string;
    sGuildAll: string;
    sGuildMember: string;
    sGuildMemberRank: string;
    sGuildChief: string;
    boKickAllUser: Boolean;
    boTestSpeedMode: Boolean;
    boSaveRcdNow: Boolean;              //立即存盘
    ClientConf: TClientConf;
    boCheckHookTool: Boolean;
    nCheckHookToolTimes: Integer;
    nWeaponMakeUnLuckRate: Integer;
    nWeaponMakeLuckPoint1: Integer;
    nWeaponMakeLuckPoint2: Integer;
    nWeaponMakeLuckPoint3: Integer;
    nWeaponMakeLuckPoint2Rate: Integer;
    nWeaponMakeLuckPoint3Rate: Integer;
    boCheckUserItemPlace: Boolean;
{$IF DEMOCLIENT = 1}
    nClientKey: Integer;
{$ELSE}
    nClientKey: Integer;
{$IFEND}
    nLevelValueOfTaosHP: Integer;
    nLevelValueOfTaosHPRate: Double;
    nLevelValueOfTaosMP: Integer;
    nLevelValueOfWizardHP: Integer;
    nLevelValueOfWizardHPRate: Double;
    nLevelValueOfWarrHP: Integer;
    nLevelValueOfWarrHPRate: Double;
    nProcessMonsterInterval: Integer;
    boIDSocketConnected: Boolean;
    boDBSocketConnected: Boolean;
    UserIDSection: TRTLCriticalSection;
    sIDSocketRecvText: string;
    IDSocket: Integer;
    nIDSocketRecvIncLen: LongWord;
    nIDSocketRecvMaxLen: LongWord;
    nIDSocketRecvCount: LongWord;
    nIDReceiveMaxTime: LongWord;
    IDSocketWSAErrCode: Integer;
    nIDSocketWSAErrCode: Integer;
    nIDSocketErrorCount: Integer;
    nDBSocketRecvIncLen: Integer;
    nDBSocketRecvMaxLen: Integer;
    sDBSocketRecvText: string;
    boDBSocketWorking: Boolean;
    nDBSocketRecvCount: Integer;
    nDBReceiveMaxTime: Integer;
    nDBSocketWSAErrCode: Integer;
    nDBSocketErrorCount: Integer;
    nLoadDBErrorCount: Integer;
    nLoadDBCount: Integer;
    nSaveDBCount: Integer;
    nDBQueryID: Integer;
    nClientFile1_CRC: Integer;
    nClientFile2_CRC: Integer;
    nClientFile3_CRC: Integer;
    boMoveCanDupObj: Boolean;
    boDropGoldToPlayBag: Boolean;
    boDropGoldToPlayBagSlave: Boolean;
    dwSendToClientTickCount_Cancel: DWORD;
    nCheckLicenseFail: Integer;
    nProcessTick: LongWord;
    nProcesstime: LongWord;
    nSendWhisperPlayCount: Integer;
    nServerFile_CRCA: Integer;
    dwSendWhisperTime: DWORD;
    GlobaDyMval: array[0..99] of Integer;

    DBSOcketThread: TThreadInfo;
    IDSocketThread: TThreadInfo;
    UserEngineThread: TThreadInfos;

    SellCount: Integer;
    SellTax: byte;
    nDBSocketSendLen: Integer;
    boNoDropItemOfGameGold: Boolean;
    nNoDropItemGamegold: Integer;
    nNoScatterBagGamegold: Integer;

    dwMagNailTick: Integer;
    nFireHitPowerRate: Integer;
    boNoDoubleFireHit: Boolean;
    boDisableDoubleAttack: Boolean;
    nDoubleAttackCheck: Integer;
    boSafeZonePush: Boolean;
    nTest: Integer;
    nHeroNextHitTime_Warr: Integer;
    nHeroNextHitTime_Wizard: Integer;
    nHeroNextHitTime_Taos: Integer;
    nHeroWalkSpeed_Warr: Integer;
    nHeroWalkSpeed_Wizard: Integer;
    nHeroWalkSpeed_Taos: Integer;
    sHeroName: string;
    sHeroSlaveName: string;
    nHeroFireSwordTime: Integer;
    nMagNailPowerRate: Integer;
    nHeroMainSkill: Integer;
    boHeroDoMotaebo: Boolean;
    boSaveKillMonExpRate: Boolean;
    boEnableMapEvent: Boolean;
    boPShowMasterName: Boolean;
    nScatterRange: Integer;
    nMagIceBallRange: Integer;
    nEarthFirePowerRate: Integer;
    boMagCapturePlayer: Boolean;
    nHPStoneStartRate: Integer;
    nMPStoneStartRate: Integer;
    nHPStoneIntervalTime: LongWord;
    nMPStoneIntervalTime: LongWord;
    nHPStoneAddRate: Integer;
    nMPStoneAddRate: Integer;
    nHPStoneDecDura: Integer;
    nMPStoneDecDura: Integer;
    LimitLevelExp: array[0..19] of TLimitLevelExp;
    DieDropGold: array[0..19] of TLimitLevelExp;
    boFireBurnEventOff: Boolean;
    boExtendStorage: Boolean;
    boUseCustomData: Boolean;
    nForceOffLineMsg: Integer;
    sHintColor: string;
    sMemoLogFontColor: string;
    sMemoLogColor: string;
    nHeroNameColor: byte;
    boAllowHeroPickUpItem: Boolean;
    boTaosHeroAutoChangePoison: Boolean;
    boHeroActiveAttack: Boolean;
    boGetFullRateExp: Boolean;
    nHeroGetExpRate: Integer;
    nHeroGainExpRate: Integer;
    nRecallHeroIntervalTime: Integer;
    nTestHeroType: Integer;
    boCalcHeroHitSpeed: Boolean;
    nHeroHitSpeedMax: Integer;
    boAllowJointAttack: Boolean;
    nSkillWWPowerRate: Integer;
    nSkillTWPowerRate: Integer;
    nSkillZWPowerRate: Integer;
    nSkillTTPowerRate: Integer;
    nSkillZTPowerRate: Integer;
    nSkillZZPowerRate: Integer;
    nEnergyStepUpRate: Integer;
    boHumanAttribute: Boolean;
    nGroupAttribHPMPRate: Integer;
    nGroupAttribPowerRate: Integer;
    boHeroHomicideAddPKPoint: Boolean;
    boHeroLockTarget: Boolean;
    nRegStatus_Null: Integer;
    boNoButchItemSubGird: Boolean;
    nButchItemNeedGird: Integer;
    boOnlyHeroClientLogon: Boolean;
    boShowShieldEffect: Boolean;
    boAutoOpenShield: Boolean;
    nCordialAddHPMPMax: Integer;
    nHeroLevelExpRate: Integer;
    nShopItemBind: Integer;
    boMagCanHitTarget: Boolean;
    boHeroNeedAmulet: Boolean;
    boOpenLevelRankSystem: Boolean;
    nPursueHitPowerRate: Integer;
    boNoDoublePursueHit: Boolean;
    dwCloneSelfTime: Integer;
    nEatItemTime: Integer;
    nGatherExpRate: Integer;
    HGlobalVal: array[0..99] of Integer;
    nHeroMaxHealthRate: Integer;
    boHeroMaxHealthType: Boolean;
    nInternalPowerRate: Integer;
    nInternalPowerSkillRate: Integer;
    nMagicShootingStarPowerRate: Integer;

    nWarrCmpInvTime: Integer;
    nWizaCmpInvTime: Integer;
    nTaosCmpInvTime: Integer;

    nShadowExpriesTime: Integer;
    boLimitSquAttack: Boolean;

    boIgnoreTagDefence: Boolean;
    boIgnoreTagDefence2: Boolean;
    nMagBubbleDefenceRate: Integer;
    boClientAutoPlay: Boolean;

    nSeriesSkillReleaseInvTime: Integer;
    nSmiteWideHitSkillInvTime: Integer;
    nPowerRateOfSeriesSkill_100: Integer;
    nPowerRateOfSeriesSkill_101: Integer;
    nPowerRateOfSeriesSkill_102: Integer;
    nPowerRateOfSeriesSkill_103: Integer;
    nPowerRateOfSeriesSkill_104: Integer;
    nPowerRateOfSeriesSkill_105: Integer;
    nPowerRateOfSeriesSkill_106: Integer;
    nPowerRateOfSeriesSkill_107: Integer;
    nPowerRateOfSeriesSkill_108: Integer;
    nPowerRateOfSeriesSkill_109: Integer;
    nPowerRateOfSeriesSkill_110: Integer;
    nPowerRateOfSeriesSkill_111: Integer;
    nPowerRateOfSeriesSkill_114: Integer;

    nMaxHealth: LongWord;
    nBoneFammDcEx: Integer;
    nDogzDcEx: Integer;
    nAngelDcEx: Integer;
    nMagSuckHpRate: Integer;
    nMagSuckHpPowerRate: Integer;
    nMagTwinPowerRate: Integer;
    nMagSquPowerRate: Integer;
    SmiteLongHit2PowerRate: Integer;
    boTDBeffect: Boolean;

    boSpeedCtrl: Boolean;
    boSpeedHackCheck: Boolean;
    nSSFreezeRate: Integer;

    boStallSystem: Boolean;
    boClientNoFog: Boolean;
    boSpiritMutinyDie: Boolean;
    boMedalItemLight: Boolean;
    boNullAttackOnSale: Boolean;
    nDoubleScRate: Integer;
    boRecallHeroCtrl: Boolean;
    nEffectBonuPointLevel: Integer;
    btMaxPowerLuck: Byte;
    SetShopNeedLevel: Integer;
    EffectHeroDropRate: Boolean;
    ClientAotoLongAttack: Boolean;
    LargeMagicRange: Boolean;
    nInternalPowerRate2: Integer;

    DeathWalking: Boolean;
    btSellType: byte;

    boDieDropUseItemRateSingle: Boolean;
    nDieRedDropUseItemRateSingle: Integer;
    aDieDropUseItemRate: array[0..High(THumanUseItems)] of Integer;
    boHeroSystem: Boolean;

    boBindNoScatter: Boolean;
    boBindNoMelt: Boolean;
    boBindNoUse: Boolean;
    boBindNoSell: Boolean;
    boBindNoPickUp: Boolean;

    SuperSkillInvTime: Integer;
    ssPowerRate_115: Integer;
    PushedPauseTime: Integer;
    nSuperSkill68InvTime: Integer;
    IceMonLiveTime: Integer;
    Skill77Time: Integer;
    Skill77Inv: Integer;
    Skill77PowerRate: Integer;
    SkillMedusaEyeEffectTimeMax: Integer;

    ItemSuiteDamageTypes: Integer;
    DoubleScInvTime: Integer;
    ClientAutoSay: Boolean;
    cbMutiHero: Boolean;
    boViewWhisper: Boolean;

    boBindPickUp: Boolean;
    boBindTakeOn: Boolean;
    nFireBurnHoldTime: Integer;
    nSquareHitPowerRate: Integer;
    nHeroMaxHealthRate1: Integer;
    nHeroMaxHealthRate2: Integer;

    nNeckLace19LuckAddRate: Integer;
    nNeckLace19LuckAddValueMaxLimit: Integer;
    nNeckLace19LuckAddValueRate: Integer;
    boAddValEx: Boolean;
    cbSmiteDamegeShow: Boolean;
    dwMasterRoyaltyRate: Integer;
    boHeroAutoLockTarget: Boolean;

    boHeroHitCmp: Boolean;
    boHeroEvade: Boolean;
    boHeroRecalcWalkTick: Boolean;

    boSkill_114_MP : Boolean;
    boSkill_68_MP : Boolean;
    ///
    tiOpenSystem: Boolean;
    tiPutAbilOnce: Boolean;
    nDetectItemRate: Integer;
    nMakeItemButchRate: Integer;        //挖宝命中率
    nMakeItemRate: Integer;             //挖宝率

    tiSpiritAddRate: Integer;
    tiSpiritAddValueRate: Integer;

    tiSecretPropertyAddRate: Integer;
    tiSecretPropertyAddValueMaxLimit: Integer;
    tiSecretPropertyAddValueRate: Integer;

    tiSucessRate: Integer;
    tiSucessRateEx: Integer;
    tiExchangeItemRate: Integer;
    spSecretPropertySucessRate: Integer;
    spMakeBookSucessRate: Integer;
    spEnergyAddTime: Integer;
    tiSpSkillAddHPMax: Boolean;
    tiHPSkillAddHPMax: Boolean;
    tiMPSkillAddMPMax: Boolean;
    tiAddHealthPlus_0: Word;
    tiAddHealthPlus_1: Word;
    tiAddHealthPlus_2: Word;
    tiAddSpellPlus_0: Word;
    tiAddSpellPlus_1: Word;
    tiAddSpellPlus_2: Word;

    tiWeaponDCAddRate: Integer;
    tiWeaponDCAddValueMaxLimit: Integer;
    tiWeaponDCAddValueRate: Integer;
    tiWeaponMCAddRate: Integer;
    tiWeaponMCAddValueMaxLimit: Integer;
    tiWeaponMCAddValueRate: Integer;
    tiWeaponSCAddRate: Integer;
    tiWeaponSCAddValueMaxLimit: Integer;
    tiWeaponSCAddValueRate: Integer;
    tiWeaponLuckAddRate: Integer;
    tiWeaponLuckAddValueMaxLimit: Integer;
    tiWeaponLuckAddValueRate: Integer;
    tiWeaponCurseAddRate: Integer;
    tiWeaponCurseAddValueMaxLimit: Integer;
    tiWeaponCurseAddValueRate: Integer;
    tiWeaponHitAddRate: Integer;
    tiWeaponHitAddValueMaxLimit: Integer;
    tiWeaponHitAddValueRate: Integer;
    tiWeaponHitSpdAddRate: Integer;
    tiWeaponHitSpdAddValueMaxLimit: Integer;
    tiWeaponHitSpdAddValueRate: Integer;
    tiWeaponHolyAddRate: Integer;
    tiWeaponHolyAddValueMaxLimit: Integer;
    tiWeaponHolyAddValueRate: Integer;

    tiWearingDCAddRate: Integer;
    tiWearingDCAddValueMaxLimit: Integer;
    tiWearingDCAddValueRate: Integer;
    tiWearingMCAddRate: Integer;
    tiWearingMCAddValueMaxLimit: Integer;
    tiWearingMCAddValueRate: Integer;
    tiWearingSCAddRate: Integer;
    tiWearingSCAddValueMaxLimit: Integer;
    tiWearingSCAddValueRate: Integer;
    tiWearingACAddRate: Integer;
    tiWearingACAddValueMaxLimit: Integer;
    tiWearingACAddValueRate: Integer;
    tiWearingMACAddRate: Integer;
    tiWearingMACAddValueMaxLimit: Integer;
    tiWearingMACAddValueRate: Integer;

    tiWearingHitAddRate: Integer;
    tiWearingHitAddValueMaxLimit: Integer;
    tiWearingHitAddValueRate: Integer;
    tiWearingSpeedAddRate: Integer;
    tiWearingSpeedAddValueMaxLimit: Integer;
    tiWearingSpeedAddValueRate: Integer;
    tiWearingLuckAddRate: Integer;
    tiWearingLuckAddValueMaxLimit: Integer;
    tiWearingLuckAddValueRate: Integer;
    tiWearingAntiMagicAddRate: Integer;
    tiWearingAntiMagicAddValueMaxLimit: Integer;
    tiWearingAntiMagicAddValueRate: Integer;
    tiWearingHealthRecoverAddRate: Integer;
    tiWearingHealthRecoverAddValueMaxLimit: Integer;
    tiWearingHealthRecoverAddValueRate: Integer;
    tiWearingSpellRecoverAddRate: Integer;
    tiWearingSpellRecoverAddValueMaxLimit: Integer;
    tiWearingSpellRecoverAddValueRate: Integer;


    
    tiAbilTagDropAddRate: Integer;
    tiAbilTagDropAddValueMaxLimit: Integer;
    tiAbilTagDropAddValueRate: Integer;
    tiAbilPreDropAddRate: Integer;
    tiAbilPreDropAddValueMaxLimit: Integer;
    tiAbilPreDropAddValueRate: Integer;
    tiAbilSuckAddRate: Integer;
    tiAbilSuckAddValueMaxLimit: Integer;
    tiAbilSuckAddValueRate: Integer;
    tiAbilIpRecoverAddRate: Integer;
    tiAbilIpRecoverAddValueMaxLimit: Integer;
    tiAbilIpRecoverAddValueRate: Integer;
    tiAbilIpExAddRate: Integer;
    tiAbilIpExAddValueMaxLimit: Integer;
    tiAbilIpExAddValueRate: Integer;
    tiAbilIpDamAddRate: Integer;
    tiAbilIpDamAddValueMaxLimit: Integer;
    tiAbilIpDamAddValueRate: Integer;
    tiAbilIpReduceAddRate: Integer;
    tiAbilIpReduceAddValueMaxLimit: Integer;
    tiAbilIpReduceAddValueRate: Integer;
    tiAbilIpDecAddRate: Integer;
    tiAbilIpDecAddValueMaxLimit: Integer;
    tiAbilIpDecAddValueRate: Integer;
    tiAbilBangAddRate: Integer;
    tiAbilBangAddValueMaxLimit: Integer;
    tiAbilBangAddValueRate: Integer;
    tiAbilGangUpAddRate: Integer;
    tiAbilGangUpAddValueMaxLimit: Integer;
    tiAbilGangUpAddValueRate: Integer;
    tiAbilPalsyReduceAddRate: Integer;
    tiAbilPalsyReduceAddValueMaxLimit: Integer;
    tiAbilPalsyReduceAddValueRate: Integer;
    tiAbilHPExAddRate: Integer;
    tiAbilHPExAddValueMaxLimit: Integer;
    tiAbilHPExAddValueRate: Integer;
    tiAbilMPExAddRate: Integer;
    tiAbilMPExAddValueMaxLimit: Integer;
    tiAbilMPExAddValueRate: Integer;
    tiAbilCCAddRate: Integer;
    tiAbilCCAddValueMaxLimit: Integer;
    tiAbilCCAddValueRate: Integer;
    tiAbilPoisonReduceAddRate: Integer;
    tiAbilPoisonReduceAddValueMaxLimit: Integer;
    tiAbilPoisonReduceAddValueRate: Integer;
    tiAbilPoisonRecoverAddRate: Integer;
    tiAbilPoisonRecoverAddValueMaxLimit: Integer;
    tiAbilPoisonRecoverAddValueRate: Integer;


    tiSpecialSkills1AddRate: Integer;
    tiSpecialSkills2AddRate: Integer;
    tiSpecialSkills3AddRate: Integer;
    tiSpecialSkills4AddRate: Integer;
    tiSpecialSkills5AddRate: Integer;
    tiSpecialSkills6AddRate: Integer;
    tiSpecialSkills7AddRate: Integer;

    tiSpMagicAddAtFirst: Boolean;
    tiSpMagicAddMaxLevel: Integer;
    tiSpMagicAddRate1: Integer;
    tiSpMagicAddRate2: Integer;
    tiSpMagicAddRate3: Integer;
    tiSpMagicAddRate4: Integer;
    tiSpMagicAddRate5: Integer;
    tiSpMagicAddRate6: Integer;
    tiSpMagicAddRate7: Integer;
    tiSpMagicAddRate8: Integer;

    tiGift_weapon: string;
    tiGift_dress_m: string;
    tiGift_dress_w: string;
    tiGift_medal: string;
    tiGift_necklace: string;
    tiGift_helmet: string;
    tiGift_helmetex: string;
    tiGift_mask: string;
    tiGift_armring: string;
    tiGift_ring: string;
    tiGift_belt: string;
    tiGift_boots: string;

    sSnowMobName1: string;
    sSnowMobName2: string;
    sSnowMobName3: string;

    fPosMoveAttackOnItem: Boolean;
    fPosMoveAttackParalysisPlayer: Boolean;
    nPosMoveAttackPowerRate: Integer;
    nPosMoveAttackInterval: Integer;

    fMagicIceRainParalysisPlayer: Boolean;
    nMagicIceRainPowerRate: Integer;
    nMagicIceRainInterval: Integer;

    fMagicDeadEyeParalysisPlayer: Boolean;
    nMagicDeadEyePowerRate: Integer;
    nMagicDeadEyeInterval: Integer;
    nMagicDragonRageInterval: Integer;
    nMagicDragonRageDuration: Integer;
    nMagicDragonRageDamageAdd: Integer;

    boPresendItem: Boolean;
    boSearchHumanOutSafeZone: Boolean;

    nHealingRate: Integer;
    fDieDeductionExp: Boolean;

    fProcClientHWID: Boolean;
    nMaxClientCount: Integer;

    boHeroHomicideAddMasterPkPoint: Boolean;
  end;
  //pTConfig = ^TConfig;

  TGList = class(TList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  TGStringList = class(TStringList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  TQuickList = class(TStringList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortString(nMIN, nMax: Integer);
    function GetIndex(sName: string): Integer;
    function AddRecord(sName: string; nIndex: Integer): Boolean;
    procedure Lock;
    procedure UnLock;
  end;

  TQuickIDList = class(TStringList)
  public
    procedure AddRecord(sAccount, sChrName: string; nIndex: Integer);
    procedure DelRecord(nIndex: Integer; sChrName: string);
    function GetChrList(sAccount: string; var ChrNameList: TList): Integer;
  end;

implementation

{ TGList }

constructor TGList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TGList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TGList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TGStringList }

constructor TGStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TGStringList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TGStringList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGStringList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TQuickList }

constructor TQuickList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TQuickList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TQuickList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TQuickList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

function TQuickList.GetIndex(sName: string): Integer;
var
  nLow, nHigh, nMed, nCompareVal: Integer;
begin
  Result := -1;
  if Self.count <> 0 then begin
    if Self.Sorted then begin
      if Self.count = 1 then begin
        if CompareStr(sName, Self.Strings[0]) = 0 then
          Result := 0;
      end else begin
        nLow := 0;
        nHigh := Self.count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            if CompareStr(sName, Self.Strings[nHigh]) = 0 then
              Result := nHigh;
            if CompareStr(sName, Self.Strings[nLow]) = 0 then
              Result := nLow;
            Break;
          end else begin
            nCompareVal := CompareStr(sName, Self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := nMed;
            Break;
          end;
        end;
      end;
    end else begin
      if Self.count = 1 then begin
        if CompareText(sName, Self.Strings[0]) = 0 then
          Result := 0;
      end else begin
        nLow := 0;
        nHigh := Self.count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            if CompareText(sName, Self.Strings[nHigh]) = 0 then
              Result := nHigh;
            if CompareText(sName, Self.Strings[nLow]) = 0 then
              Result := nLow;
            Break;
          end else begin
            nCompareVal := CompareText(sName, Self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := nMed;
            Break;
          end;
        end;
      end;
    end;
  end;

end;

procedure TQuickList.SortString(nMIN, nMax: Integer);
var
  ntMin, ntMax              : Integer;
  s18                       : string;
begin
  if Self.count > 0 then
    while (True) do begin
      ntMin := nMIN;
      ntMax := nMax;
      s18 := Self.Strings[(nMIN + nMax) shr 1];
      while (True) do begin
        while (CompareText(Self.Strings[ntMin], s18) < 0) do
          Inc(ntMin);
        while (CompareText(Self.Strings[ntMax], s18) > 0) do
          Dec(ntMax);
        if ntMin <= ntMax then begin
          Self.Exchange(ntMin, ntMax);
          Inc(ntMin);
          Dec(ntMax);
        end;
        if ntMin > ntMax then
          Break
      end;
      if nMIN < ntMax then
        SortString(nMIN, ntMax);
      nMIN := ntMin;
      if ntMin >= nMax then
        Break;
    end;
end;

function TQuickList.AddRecord(sName: string; nIndex: Integer): Boolean;
var
  nLow, nHigh, nMed, nCompareVal: Integer;
begin
  Result := True;
  if Self.count = 0 then
    Self.AddObject(sName, TObject(nIndex))
  else begin
    if Self.Sorted then begin
      if Self.count = 1 then begin
        nMed := CompareStr(sName, Self.Strings[0]);
        if nMed > 0 then
          Self.AddObject(sName, TObject(nIndex))
        else begin
          if nMed < 0 then
            Self.InsertObject(0, sName, TObject(nIndex));
        end;
      end else begin
        nLow := 0;
        nHigh := Self.count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            nMed := CompareStr(sName, Self.Strings[nHigh]);
            if nMed > 0 then begin
              Self.InsertObject(nHigh + 1, sName, TObject(nIndex));
              Break;
            end else begin
              nMed := CompareStr(sName, Self.Strings[nLow]);
              if nMed > 0 then begin
                Self.InsertObject(nLow + 1, sName, TObject(nIndex));
                Break;
              end else begin
                if nMed < 0 then begin
                  Self.InsertObject(nLow, sName, TObject(nIndex));
                  Break;
                end else begin
                  Result := False;
                  Break;
                end;
              end;
            end;
          end else begin
            nCompareVal := CompareStr(sName, Self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := False;
            Break;
          end;
        end;
      end;
    end else begin
      if Self.count = 1 then begin
        nMed := CompareText(sName, Self.Strings[0]);
        if nMed > 0 then
          Self.AddObject(sName, TObject(nIndex))
        else begin
          if nMed < 0 then
            Self.InsertObject(0, sName, TObject(nIndex));
        end;
      end else begin
        nLow := 0;
        nHigh := Self.count - 1;
        nMed := (nHigh - nLow) div 2 + nLow;
        while (True) do begin
          if (nHigh - nLow) = 1 then begin
            nMed := CompareText(sName, Self.Strings[nHigh]);
            if nMed > 0 then begin
              Self.InsertObject(nHigh + 1, sName, TObject(nIndex));
              Break;
            end
            else begin
              nMed := CompareText(sName, Self.Strings[nLow]);
              if nMed > 0 then begin
                Self.InsertObject(nLow + 1, sName, TObject(nIndex));
                Break;
              end
              else begin
                if nMed < 0 then begin
                  Self.InsertObject(nLow, sName, TObject(nIndex));
                  Break;
                end
                else begin
                  Result := False;
                  Break;
                end;
              end;
            end;
          end else begin
            nCompareVal := CompareText(sName, Self.Strings[nMed]);
            if nCompareVal > 0 then begin
              nLow := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            if nCompareVal < 0 then begin
              nHigh := nMed;
              nMed := (nHigh - nLow) div 2 + nLow;
              Continue;
            end;
            Result := False;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

{ TQuickIDList }

procedure TQuickIDList.AddRecord(sAccount, sChrName: string; nIndex: Integer);
var
  QuickID                   : pTQuickID;
  ChrList                   : TList;
  nLow, nHigh, nMed, n1C, n20: Integer;
begin
  New(QuickID);
  QuickID.sAccount := sAccount;
  QuickID.sChrName := sChrName;
  QuickID.nIndex := nIndex;
  QuickID.nSelectID := 0;
  if count = 0 then begin
    ChrList := TList.Create;
    ChrList.Add(QuickID);
    AddObject(sAccount, ChrList);
  end else begin
    if count = 1 then begin
      nMed := CompareStr(sAccount, Self.Strings[0]);
      if nMed > 0 then begin
        ChrList := TList.Create;
        ChrList.Add(QuickID);
        AddObject(sAccount, ChrList);
      end else begin
        if nMed < 0 then begin
          ChrList := TList.Create;
          ChrList.Add(QuickID);
          InsertObject(0, sAccount, ChrList);
        end else begin
          ChrList := TList(Self.Objects[0]);
          ChrList.Add(QuickID);
        end;
      end;
    end else begin
      nLow := 0;
      nHigh := Self.count - 1;
      nMed := (nHigh - nLow) div 2 + nLow;
      while (True) do begin
        if (nHigh - nLow) = 1 then begin
          n20 := CompareStr(sAccount, Self.Strings[nHigh]);
          if n20 > 0 then begin
            ChrList := TList.Create;
            ChrList.Add(QuickID);
            InsertObject(nHigh + 1, sAccount, ChrList);
            Break;
          end else begin
            if CompareStr(sAccount, Self.Strings[nHigh]) = 0 then begin
              ChrList := TList(Self.Objects[nHigh]);
              ChrList.Add(QuickID);
              Break;
            end else begin
              n20 := CompareStr(sAccount, Self.Strings[nLow]);
              if n20 > 0 then begin
                ChrList := TList.Create;
                ChrList.Add(QuickID);
                InsertObject(nLow + 1, sAccount, ChrList);
                Break;
              end else begin
                if n20 < 0 then begin
                  ChrList := TList.Create;
                  ChrList.Add(QuickID);
                  InsertObject(nLow, sAccount, ChrList);
                  Break;
                end else begin
                  ChrList := TList(Self.Objects[n20]);
                  ChrList.Add(QuickID);
                  Break;
                end;
              end;
            end;
          end;
        end else begin
          n1C := CompareStr(sAccount, Self.Strings[nMed]);
          if n1C > 0 then begin
            nLow := nMed;
            nMed := (nHigh - nLow) div 2 + nLow;
            Continue;
          end;
          if n1C < 0 then begin
            nHigh := nMed;
            nMed := (nHigh - nLow) div 2 + nLow;
            Continue;
          end;
          ChrList := TList(Self.Objects[nMed]);
          ChrList.Add(QuickID);
          Break;
        end;
      end;
    end;
  end;
end;

procedure TQuickIDList.DelRecord(nIndex: Integer; sChrName: string);
var
  QuickID                   : pTQuickID;
  ChrList                   : TList;
  i                         : Integer;
begin
  if (Self.count - 1) < nIndex then
    Exit;
  ChrList := TList(Self.Objects[nIndex]);
  for i := 0 to ChrList.count - 1 do begin
    QuickID := ChrList.Items[i];
    if QuickID.sChrName = sChrName then begin
      Dispose(QuickID);
      ChrList.Delete(i);
      Break;
    end;
  end;
  if ChrList.count <= 0 then begin
    ChrList.Free;
    Self.Delete(nIndex);
  end;

end;

function TQuickIDList.GetChrList(sAccount: string; var ChrNameList: TList): Integer;
var
  nHigh, nLow, nMed, n20, n24: Integer;
begin
  Result := -1;
  if Self.count = 0 then
    Exit;
  if Self.count = 1 then begin
    if CompareStr(sAccount, Self.Strings[0]) = 0 then begin
      ChrNameList := TList(Self.Objects[0]);
      Result := 0;
    end;
  end else begin
    nLow := 0;
    nHigh := Self.count - 1;
    nMed := (nHigh - nLow) div 2 + nLow;
    n24 := -1;
    while (True) do begin
      if (nHigh - nLow) = 1 then begin
        if CompareStr(sAccount, Self.Strings[nHigh]) = 0 then
          n24 := nHigh;
        if CompareStr(sAccount, Self.Strings[nLow]) = 0 then
          n24 := nLow;
        Break;
      end else begin
        n20 := CompareStr(sAccount, Self.Strings[nMed]);
        if n20 > 0 then begin
          nLow := nMed;
          nMed := (nHigh - nLow) div 2 + nLow;
          Continue;
        end;
        if n20 < 0 then begin
          nHigh := nMed;
          nMed := (nHigh - nLow) div 2 + nLow;
          Continue;
        end;
        n24 := nMed;
        Break;
      end;
    end;
    if n24 <> -1 then
      ChrNameList := TList(Self.Objects[n24]);
    Result := n24;
  end;
end;

end.

