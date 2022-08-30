unit SDK;

interface

const
  SG_FORMHANDLE             = 1000;
  SG_STARTNOW               = 1001;
  SG_STARTOK                = 1002;
  SG_USERACCOUNT            = 1003;
  SG_USERACCOUNTNOTFOUND    = 1004;
  SG_USERACCOUNTCHANGESTATUS = 1005;
  SG_CHECKCODEADDR          = 1006;
  GS_QUIT                   = 2000;
  GS_USERACCOUNT            = 2001;
  GS_CHANGEACCOUNTINFO      = 2002;
  WM_SENDPROCMSG            = $0401;

type
  TProgamType = (
    MsgData,
    tDBServer = 0,
    tLoginSrv = 1,
    tLogServer = 2,
    tWolServer = 3,
    tM2Server = 3,
    tLoginGate = 4,
    tLoginGate1 = 5,
    tSelGate = 6,
    tSelGate1 = 7,
    tRunGate = 8,
    tRunGate1 = 9,
    tRunGate2 = 10,
    tRunGate3 = 11,
    tRunGate4 = 12,
    tRunGate5 = 13,
    tRunGate6 = 14,
    tRunGate7 = 15);

  TCheckCode = record
    dwThread0: Integer;
    sThread0: Integer;
  end;

  {TDLL_Config = record
    nCheckLicenseFail: Integer;
    dwRequestVersion: Integer;
    nProcessTick: LongWord;
    nProcesstime: LongWord;
  end;
  pTDLL_Config = ^TDLL_Config;}

implementation

end.
