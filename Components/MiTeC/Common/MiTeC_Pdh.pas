{*******************************************************}
{                 MiTeC Common Routines                 }
{                Performance Data Helper                }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Pdh;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils,
     {$ELSE}
     Windows, SysUtils,
     {$ENDIF}
     MiTeC_Windows;

const
  PdhLib = 'pdh.dll';

  MAX_COUNTER_PATH        = 256;   // Maximum counter path length
  PDH_MAX_COUNTER_NAME    = 1024;  // Maximum counter name length.
  PDH_MAX_INSTANCE_NAME   = 1024;  // Maximum counter instance name length.
  PDH_MAX_COUNTER_PATH    = 2048;  // Maximum full counter path length.
  PDH_MAX_DATASOURCE_PATH = 1024;  // MAximum full counter log name length.

  PDH_MORE_DATA                    = DWORD($800007D2);
  PDH_CSTATUS_VALID_DATA           = DWORD($00000000);
  PDH_CSTATUS_NEW_DATA             = DWORD($00000001);
  PDH_CSTATUS_INVALID_DATA         = DWORD($C0000BBA);
  PDH_CSTATUS_NO_OBJECT            = DWORD($C0000BB8);
  PDH_CSTATUS_NO_COUNTER           = DWORD($C0000BB9);

  PDH_CF_INCLUDEINSTANCEINDEX    = 1 shl 0;
  PDH_CF_SINGLECOUNTERPERADD     = 1 shl 1;
  PDH_CF_SINGLECOUNTERPERDIALOG  = 1 shl 2;
  PDH_CF_LOCALCOUNTERSONLY       = 1 shl 3;
  PDH_CF_WILDCARDINSTANCES       = 1 shl 4;
  PDH_CF_HIDEDETAILBOX           = 1 shl 5;
  PDH_CF_INITIALIZEPATH          = 1 shl 6;
  PDH_CF_DISABLEMACHINESELECTION = 1 shl 7;
  PDH_CF_INCLUDECOSTLYOBJECTS    = 1 shl 8;
  PDH_CF_RESERVED                = DWORD($FFFFFE00);

type
  PDH_STATUS = DWORD;
  PDH_HQUERY = THandle;
  PDH_HCOUNTER = THandle;

  PPDH_RAW_COUNTER = ^PDH_RAW_COUNTER;
  _PDH_RAW_COUNTER = record
    CStatus: DWORD;
    TimeStamp: FILETIME;
    FirstValue: LONGLONG;
    SecondValue: LONGLONG;
    MultiCount: DWORD;
  end;
  PDH_RAW_COUNTER = _PDH_RAW_COUNTER;
  TPdhRawCounter = PDH_RAW_COUNTER;
  PPdhRawCounter = PPDH_RAW_COUNTER;

  PPDH_RAW_COUNTER_ITEM_A = ^PDH_RAW_COUNTER_ITEM_A;
  _PDH_RAW_COUNTER_ITEM_A = record
    szName: LPSTR;
    RawValue: PDH_RAW_COUNTER;
  end;
  PDH_RAW_COUNTER_ITEM_A = _PDH_RAW_COUNTER_ITEM_A;
  TPdhRawCounterItemA = PDH_RAW_COUNTER_ITEM_A;
  PPdhRawCounterItemA = PPDH_RAW_COUNTER_ITEM_A;

  PPDH_RAW_COUNTER_ITEM_W = ^PDH_RAW_COUNTER_ITEM_W;
  _PDH_RAW_COUNTER_ITEM_W = record
    szName: LPWSTR;
    RawValue: PDH_RAW_COUNTER;
  end;
  PDH_RAW_COUNTER_ITEM_W = _PDH_RAW_COUNTER_ITEM_W;
  TPdhRawCounterItemW = PDH_RAW_COUNTER_ITEM_W;
  PPdhRawCounterItemW = PPDH_RAW_COUNTER_ITEM_W;

  {$IFDEF UNICODE}
  PPdhRawCounterItem = PPdhRawCounterItemW;
  PDH_RAW_COUNTER_ITEM = _PDH_RAW_COUNTER_ITEM_W;
  PPDH_RAW_COUNTER_ITEM = PPDH_RAW_COUNTER_ITEM_W;
  TPdhRawCounterItem = _PDH_RAW_COUNTER_ITEM_W;
  {$ELSE}
  PPdhRawCounterItem = PPdhRawCounterItemA;
  PDH_RAW_COUNTER_ITEM = _PDH_RAW_COUNTER_ITEM_A;
  PPDH_RAW_COUNTER_ITEM = PPDH_RAW_COUNTER_ITEM_A;
  TPdhRawCounterItem = _PDH_RAW_COUNTER_ITEM_A;
  {$ENDIF}

  PPDH_FMT_COUNTERVALUE = ^PDH_FMT_COUNTERVALUE;
  _PDH_FMT_COUNTERVALUE = record
    CStatus: DWORD;
    case Longint of
      1: (longValue: LONG);
      2: (doubleValue: Double);
      3: (largeValue: LONGLONG);
      4: (AnsiStringValue: LPSTR);
      5: (WideStringValue: LPCWSTR);
  end;
  PDH_FMT_COUNTERVALUE = _PDH_FMT_COUNTERVALUE;
  TPdhFmtCounterValue = PDH_FMT_COUNTERVALUE;
  PPdhFmtCounterValue = PPDH_FMT_COUNTERVALUE;

  PPDH_FMT_COUNTERVALUE_ITEM_A = ^PDH_FMT_COUNTERVALUE_ITEM_A;
  _PDH_FMT_COUNTERVALUE_ITEM_A = record
    szName: LPSTR;
    FmtValue: PDH_FMT_COUNTERVALUE;
  end;
  PDH_FMT_COUNTERVALUE_ITEM_A = _PDH_FMT_COUNTERVALUE_ITEM_A;
  TPdhFmtCounterValueItemA = PDH_FMT_COUNTERVALUE_ITEM_A;
  PPdhFmtCounterValueItemA = PPDH_FMT_COUNTERVALUE_ITEM_A;

  PPDH_FMT_COUNTERVALUE_ITEM_W = ^PDH_FMT_COUNTERVALUE_ITEM_W;
  _PDH_FMT_COUNTERVALUE_ITEM_W = record
    szName: LPWSTR;
    FmtValue: PDH_FMT_COUNTERVALUE;
  end;
  PDH_FMT_COUNTERVALUE_ITEM_W = _PDH_FMT_COUNTERVALUE_ITEM_W;
  TPdhFmtCounterValueItemW = PDH_FMT_COUNTERVALUE_ITEM_W;
  PPdhFmtCounterValueItemW = PPDH_FMT_COUNTERVALUE_ITEM_W;

  {$IFDEF UNICODE}
  PPdhFmtCounterValueItem = PPdhFmtCounterValueItemW;
  PDH_FMT_COUNTERVALUE_ITEM = _PDH_FMT_COUNTERVALUE_ITEM_W;
  PPDH_FMT_COUNTERVALUE_ITEM = PPDH_FMT_COUNTERVALUE_ITEM_W;
  TPdhFmtCounterValueItem = _PDH_FMT_COUNTERVALUE_ITEM_W;
  {$ELSE}
  PPdhFmtCounterValueItem = PPdhFmtCounterValueItemA;
  PDH_FMT_COUNTERVALUE_ITEM = _PDH_FMT_COUNTERVALUE_ITEM_A;
  PPDH_FMT_COUNTERVALUE_ITEM = PPDH_FMT_COUNTERVALUE_ITEM_A;
  TPdhFmtCounterValueItem = _PDH_FMT_COUNTERVALUE_ITEM_A;
  {$ENDIF}

  PPDH_DATA_ITEM_PATH_ELEMENTS_A = ^PDH_DATA_ITEM_PATH_ELEMENTS_A;
  _PDH_DATA_ITEM_PATH_ELEMENTS_A = record
    szMachineName: LPSTR;
    ObjectGUID: TGUID;
    dwItemId: DWORD;
    szInstanceName: LPSTR;
  end;
  PDH_DATA_ITEM_PATH_ELEMENTS_A = _PDH_DATA_ITEM_PATH_ELEMENTS_A;
  TPdhDataItemPathElementsA = PDH_DATA_ITEM_PATH_ELEMENTS_A;
  PPdhDataItemPathElementsA = PPDH_DATA_ITEM_PATH_ELEMENTS_A;

  PPDH_DATA_ITEM_PATH_ELEMENTS_W = ^PDH_DATA_ITEM_PATH_ELEMENTS_W;
  _PDH_DATA_ITEM_PATH_ELEMENTS_W = record
    szMachineName: LPWSTR;
    ObjectGUID: TGUID;
    dwItemId: DWORD;
    szInstanceName: LPWSTR;
  end;
  PDH_DATA_ITEM_PATH_ELEMENTS_W = _PDH_DATA_ITEM_PATH_ELEMENTS_W;
  TPdhDataItemPathElementsW = PDH_DATA_ITEM_PATH_ELEMENTS_W;
  PPdhDataItemPathElementsW = PPDH_DATA_ITEM_PATH_ELEMENTS_W;

  {$IFDEF UNICODE}
  PPdhDataItemPathElements = PPdhDataItemPathElementsW;
  PDH_DATA_ITEM_PATH_ELEMENTS = _PDH_DATA_ITEM_PATH_ELEMENTS_W;
  PPDH_DATA_ITEM_PATH_ELEMENTS = PPDH_DATA_ITEM_PATH_ELEMENTS_W;
  TPdhDataItemPathElements = _PDH_DATA_ITEM_PATH_ELEMENTS_W;
  {$ELSE}
  PPdhDataItemPathElements = PPdhDataItemPathElementsA;
  PDH_DATA_ITEM_PATH_ELEMENTS = _PDH_DATA_ITEM_PATH_ELEMENTS_A;
  PPDH_DATA_ITEM_PATH_ELEMENTS = PPDH_DATA_ITEM_PATH_ELEMENTS_A;
  TPdhDataItemPathElements = _PDH_DATA_ITEM_PATH_ELEMENTS_A;
  {$ENDIF}

  PPDH_COUNTER_PATH_ELEMENTS_A = ^PDH_COUNTER_PATH_ELEMENTS_A;
  _PDH_COUNTER_PATH_ELEMENTS_A = record
    szMachineName: LPSTR;
    szObjectName: LPSTR;
    szInstanceName: LPSTR;
    szParentInstance: LPSTR;
    dwInstanceIndex: DWORD;
    szCounterName: LPSTR;
  end;
  PDH_COUNTER_PATH_ELEMENTS_A = _PDH_COUNTER_PATH_ELEMENTS_A;
  TPdhCounterPathElementsA = PDH_COUNTER_PATH_ELEMENTS_A;
  PPdhCounterPathElementsA = PPDH_COUNTER_PATH_ELEMENTS_A;

  PPDH_COUNTER_PATH_ELEMENTS_W = ^PDH_COUNTER_PATH_ELEMENTS_W;
  _PDH_COUNTER_PATH_ELEMENTS_W = record
    szMachineName: LPWSTR;
    szObjectName: LPWSTR;
    szInstanceName: LPWSTR;
    szParentInstance: LPWSTR;
    dwInstanceIndex: DWORD;
    szCounterName: LPWSTR;
  end;
  PDH_COUNTER_PATH_ELEMENTS_W = _PDH_COUNTER_PATH_ELEMENTS_W;
  TPdhCounterPathElementsW = PDH_COUNTER_PATH_ELEMENTS_W;
  PPdhCounterPathElementsW = PPDH_COUNTER_PATH_ELEMENTS_W;

  {$IFDEF UNICODE}
  PPdhCounterPathElements = PPdhCounterPathElementsW;
  PDH_COUNTER_PATH_ELEMENTS = _PDH_COUNTER_PATH_ELEMENTS_W;
  PPDH_COUNTER_PATH_ELEMENTS = PPDH_COUNTER_PATH_ELEMENTS_W;
  TPdhCounterPathElements = _PDH_COUNTER_PATH_ELEMENTS_W;
  {$ELSE}
  PPdhCounterPathElements = PPdhCounterPathElementsA;
  PDH_COUNTER_PATH_ELEMENTS = _PDH_COUNTER_PATH_ELEMENTS_A;
  PPDH_COUNTER_PATH_ELEMENTS = PPDH_COUNTER_PATH_ELEMENTS_A;
  TPdhCounterPathElements = _PDH_COUNTER_PATH_ELEMENTS_A;
  {$ENDIF}

  PPDH_COUNTER_INFO_A = ^PDH_COUNTER_INFO_A;
  _PDH_COUNTER_INFO_A = record
    dwLength: DWORD;
    dwType: DWORD;
    CVersion: DWORD;
    CStatus: DWORD;
    lScale: LONG;
    lDefaultScale: LONG;
    dwUserData: DWORD_PTR;
    dwQueryUserData: DWORD_PTR;
    szFullPath: LPSTR;
    Union: record
      case Longint of
        1: (DataItemPath: PDH_DATA_ITEM_PATH_ELEMENTS_A);
        2: (CounterPath: PDH_COUNTER_PATH_ELEMENTS_A);
        3: (szMachineName: LPSTR;
            szObjectName: LPSTR;
            szInstanceName: LPSTR;
            szParentInstance: LPSTR;
            dwInstanceIndex: DWORD;
            szCounterName: LPSTR);
    end;
    szExplainText: LPSTR;
    DataBuffer: array [0..0] of DWORD;
  end;
  PDH_COUNTER_INFO_A = _PDH_COUNTER_INFO_A;
  TPdhCounterInfoA = PDH_COUNTER_INFO_A;
  PPdhCounterInfoA = PPDH_COUNTER_INFO_A;

  PPDH_COUNTER_INFO_W = ^PDH_COUNTER_INFO_W;
  _PDH_COUNTER_INFO_W = record
    dwLength: DWORD;
    dwType: DWORD;
    CVersion: DWORD;
    CStatus: DWORD;
    lScale: LONG;
    lDefaultScale: LONG;
    dwUserData: DWORD_PTR;
    dwQueryUserData: DWORD_PTR;
    szFullPath: LPWSTR;
    Union: record
      case Longint of
        1: (DataItemPath: PDH_DATA_ITEM_PATH_ELEMENTS_W);
        2: (CounterPath: PDH_COUNTER_PATH_ELEMENTS_W);
        3: (szMachineName: LPWSTR;
            szObjectName: LPWSTR;
            szInstanceName: LPWSTR;
            szParentInstance: LPWSTR;
            dwInstanceIndex: DWORD;
            szCounterName: LPWSTR);
    end;
    szExplainText: LPWSTR;
    DataBuffer: array [0..0] of DWORD;
  end;
  PDH_COUNTER_INFO_W = _PDH_COUNTER_INFO_W;
  TPdhCounterInfoW = PDH_COUNTER_INFO_W;
  PPdhCounterInfoW = PPDH_COUNTER_INFO_W;

  {$IFDEF UNICODE}
  PPdhCounterInfo = PPdhCounterInfoW;
  PDH_COUNTER_INFO = _PDH_COUNTER_INFO_W;
  PPDH_COUNTER_INFO = PPDH_COUNTER_INFO_W;
  TPdhCounterInfo = _PDH_COUNTER_INFO_W;
  {$ELSE}
  PPdhCounterInfo = PPdhCounterInfoA;
  PDH_COUNTER_INFO = _PDH_COUNTER_INFO_A;
  PPDH_COUNTER_INFO = PPDH_COUNTER_INFO_A;
  TPdhCounterInfo = _PDH_COUNTER_INFO_A;
  {$ENDIF}

  CounterPathCallBack = function(dwArg: DWORD_PTR): PDH_STATUS; stdcall;

  PPDH_BROWSE_DLG_CONFIG_A = ^_BrowseDlgConfig_A;
  _BrowseDlgConfig_A = record
    dwConfigFlags: DWORD;
    hWndOwner: HWND;
    szDataSource: LPSTR;
    szReturnPathBuffer: LPSTR;
    cchReturnPathLength: DWORD;
    pCallBack: CounterPathCallBack;
    dwCallBackArg: DWORD_PTR;
    CallBackStatus: PDH_STATUS;
    dwDefaultDetailLevel: DWORD;
    szDialogBoxCaption: LPSTR;
  end;
  PDH_BROWSE_DLG_CONFIG_A = _BrowseDlgConfig_A;
  TPdhBrowseDlgConfigA = PDH_BROWSE_DLG_CONFIG_A;
  PPdhBrowseDlgConfigA = PPDH_BROWSE_DLG_CONFIG_A;

  PPDH_BROWSE_DLG_CONFIG_W = ^_BrowseDlgConfig_W;
  _BrowseDlgConfig_W = record
    dwConfigFlags: DWORD;
    hWndOwner: HWND;
    szDataSource: LPWSTR;
    szReturnPathBuffer: LPWSTR;
    cchReturnPathLength: DWORD;
    pCallBack: CounterPathCallBack;
    dwCallBackArg: DWORD_PTR;
    CallBackStatus: PDH_STATUS;
    dwDefaultDetailLevel: DWORD;
    szDialogBoxCaption: LPWSTR;
  end;
  PDH_BROWSE_DLG_CONFIG_W = _BrowseDlgConfig_W;
  TPdhBrowseDlgConfigW = PDH_BROWSE_DLG_CONFIG_W;
  PPdhBrowseDlgConfigW = PPDH_BROWSE_DLG_CONFIG_W;

  {$IFDEF UNICODE}
  PPdhBrowseDlgConfig = PPdhBrowseDlgConfigW;
  PDH_BROWSE_DLG_CONFIG = PDH_BROWSE_DLG_CONFIG_W;
  PPDH_BROWSE_DLG_CONFIG = PPDH_BROWSE_DLG_CONFIG_W;
  TPdhBrowseDlgConfig = TPdhBrowseDlgConfigW;
  {$ELSE}
  PPdhBrowseDlgConfig = PPdhBrowseDlgConfigA;
  PDH_BROWSE_DLG_CONFIG = PDH_BROWSE_DLG_CONFIG_A;
  PPDH_BROWSE_DLG_CONFIG = PPDH_BROWSE_DLG_CONFIG_A;
  TPdhBrowseDlgConfig = TPdhBrowseDlgConfigA;
  {$ENDIF}

  TPdhOpenQueryA = function (szDataSource: LPCSTR; dwUserData: DWORD_PTR; var phQuery: PDH_HQUERY): PDH_STATUS; stdcall;
  TPdhOpenQueryW = function (szDataSource: LPCWSTR; dwUserData: DWORD_PTR; var phQuery: PDH_HQUERY): PDH_STATUS; stdcall;


  TPdhExpandWildCardPathA = function (szDataSource, szWildCardPath: LPCSTR; mszExpandedPathList: LPTSTR; var pcchPathListLength: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;
  TPdhExpandWildCardPathW = function (szDataSource, szWildCardPath: LPCWSTR; mszExpandedPathList: LPTSTR; var pcchPathListLength: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;

  TPdhAddCounterA = function (hQuery: PDH_HQUERY; szFullCounterPath: LPCSTR; dwUserData: DWORD_PTR; var phCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;
  TPdhAddCounterW = function (hQuery: PDH_HQUERY; szFullCounterPath: LPCWSTR; dwUserData: DWORD_PTR; var phCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;

  TPdhAddEnglishCounterA = function (hQuery: PDH_HQUERY; szFullCounterPath: LPCSTR; dwUserData: DWORD_PTR; var phCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;
  TPdhAddEnglishCounterW = function (hQuery: PDH_HQUERY; szFullCounterPath: LPCWSTR; dwUserData: DWORD_PTR; var phCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;

  TPdhRemoveCounter = function (hCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;

  TPdhCollectQueryData = function (hQuery: PDH_HQUERY): PDH_STATUS; stdcall;

  TPdhCloseQuery = function (hQuery: PDH_HQUERY): PDH_STATUS; stdcall;

  TPdhGetRawCounterValue = function (hCounter: PDH_HCOUNTER; lpdwType: LPDWORD; var pValue: PDH_RAW_COUNTER): PDH_STATUS; stdcall;

  TPdhGetRawCounterArrayA = function(hCounter: PDH_HCOUNTER; var lpdwBufferSize, lpdwItemCount: DWORD; var ItemBuffer: PDH_RAW_COUNTER_ITEM_A): PDH_STATUS; stdcall;
  TPdhGetRawCounterArrayW = function (hCounter: PDH_HCOUNTER; var lpdwBufferSize, lpdwItemCount: DWORD; var ItemBuffer: PDH_RAW_COUNTER_ITEM_W): PDH_STATUS; stdcall;

  TPdhGetFormattedCounterValue = function (hCounter: PDH_HCOUNTER; dwFormat: DWORD; lpdwType: LPDWORD; var pValue: PDH_FMT_COUNTERVALUE): PDH_STATUS; stdcall;

  TPdhGetFormattedCounterArrayA = function (hCounter: PDH_HCOUNTER; dwFormat: DWORD; var lpdwBufferSize, lpdwItemCount: DWORD; var ItemBuffer: PDH_FMT_COUNTERVALUE_ITEM_A): PDH_STATUS; stdcall;
  TPdhGetFormattedCounterArrayW = function (hCounter: PDH_HCOUNTER; dwFormat: DWORD; var lpdwBufferSize, lpdwItemCount: DWORD; var ItemBuffer: PDH_FMT_COUNTERVALUE_ITEM_W): PDH_STATUS; stdcall;

  TPdhGetCounterInfoA = function (hCounter: PDH_HCOUNTER; bRetrieveExplainText: Boolean; var pdwBufferSize: DWORD; lpBuffer: PPDH_COUNTER_INFO_A): PDH_STATUS; stdcall;
  TPdhGetCounterInfoW = function (hCounter: PDH_HCOUNTER; bRetrieveExplainText: Boolean; var pdwBufferSize: DWORD; lpBuffer: PPDH_COUNTER_INFO_W): PDH_STATUS; stdcall;

  TPdhBrowseCountersA = function (const pBrowseDlgData: PDH_BROWSE_DLG_CONFIG_A): PDH_STATUS; stdcall;
  TPdhBrowseCountersW = function (const pBrowseDlgData: PDH_BROWSE_DLG_CONFIG_W): PDH_STATUS; stdcall;

  TPdhMakeCounterPathA = function (pCounterPathElements: PPDH_COUNTER_PATH_ELEMENTS_A; szFullPathBuffer: LPSTR; var pcchBufferSize: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;
  TPdhMakeCounterPathW = function (pCounterPathElements: PPDH_COUNTER_PATH_ELEMENTS_W; szFullPathBuffer: LPWSTR; var pcchBufferSize: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;

  TPdhConnectMachineA = function (szMachineName: LPCSTR): PDH_STATUS; stdcall;
  TPdhConnectMachineW = function (szMachineName: LPCWSTR): PDH_STATUS; stdcall;

  TPdhEnumMachinesA = function (szDataSource: LPCSTR; mszMachineList: LPTSTR; pcchBufferSize: LPDWORD): PDH_STATUS; stdcall;
  TPdhEnumMachinesW = function (szDataSource: LPCWSTR; mszMachineList: LPTSTR; pcchBufferSize: LPDWORD): PDH_STATUS; stdcall;

  TPdhEnumObjectsA = function (szDataSource, szMachineName: LPCSTR; mszObjectList: LPTSTR; var pcchBufferSize: DWORD; dwDetailLevel: DWORD; bRefresh: BOOL): PDH_STATUS; stdcall;
  TPdhEnumObjectsW = function (szDataSource, szMachineName: LPCWSTR; mszObjectList: LPTSTR; var pcchBufferSize: DWORD; dwDetailLevel: DWORD; bRefresh: BOOL): PDH_STATUS; stdcall;

  TPdhEnumObjectItemsA = function (szDataSource, szMachineName, szObjectName: LPCSTR; mszCounterList: LPTSTR; var pcchCounterListLength: DWORD; mszInstanceList: LPTSTR;
                                   var pcchInstanceListLength: DWORD; dwDetailLevel, dwFlags: DWORD): PDH_STATUS; stdcall;
  TPdhEnumObjectItemsW = function (szDataSource, szMachineName, szObjectName: LPCWSTR; mszCounterList: LPTSTR; var pcchCounterListLength: DWORD; mszInstanceList: LPTSTR;
                                   var pcchInstanceListLength: DWORD; dwDetailLevel, dwFlags: DWORD): PDH_STATUS; stdcall;

  TPdhGetDefaultPerfObjectA = function (szDataSource, szMachineName: LPCSTR; szDefaultObjectName: LPSTR; var pcchBufferSize: DWORD): PDH_STATUS; stdcall;
  TPdhGetDefaultPerfObjectW = function (szDataSource, szMachineName: LPCWSTR; szDefaultObjectName: LPWSTR; var pcchBufferSize: DWORD): PDH_STATUS; stdcall;

  TPdhGetDefaultPerfCounterA = function (szDataSource, szMachineName, szObjectName: LPCSTR; szDefaultCounterName: LPSTR; var pcchBufferSize: DWORD): PDH_STATUS; stdcall;
  TPdhGetDefaultPerfCounterW = function (szDataSource, szMachineName, szObjectName: LPCWSTR; szDefaultCounterName: LPWSTR; var pcchBufferSize: DWORD): PDH_STATUS; stdcall;

  TPdhLookupPerfIndexByNameA = function (szMachineName, szNameBuffer: PAnsiChar; var pdwIndex: DWORD): PDH_STATUS; stdcall;
  TPdhLookupPerfIndexByNameW = function (szMachineName, szNameBuffer: PWideChar; var pdwIndex: DWORD): PDH_STATUS; stdcall;

  TPdhParseCounterPathA = function (szFullPathBuffer: PAnsiChar; pCounterPathElements: PPdhCounterPathElementsA; var pdwBufferSize: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;
  TPdhParseCounterPathW = function (szFullPathBuffer: PWideChar; pCounterPathElements: PPdhCounterPathElementsW; var pdwBufferSize: DWORD; dwFlags: DWORD): PDH_STATUS; stdcall;

  {$IFDEF UNICODE}
  TPdhExpandWildCardPath = TPdhExpandWildCardPathW;
  TPdhOpenQuery = TPdhOpenQueryW;
  TPdhAddCounter = TPdhAddCounterW;
  TPdhAddEnglishCounter = TPdhAddEnglishCounterW;
  TPdhGetRawCounterArray = TPdhGetRawCounterArrayW;
  TPdhGetFormattedCounterArray = TPdhGetFormattedCounterArrayW;
  TPdhGetCounterInfo = TPdhGetCounterInfoW;
  TPdhBrowseCounters = TPdhBrowseCountersW;
  TPdhMakeCounterPath = TPdhMakeCounterPathW;
  TPdhConnectMachine = TPdhConnectMachineW;
  TPdhEnumMachines = TPdhEnumMachinesW;
  TPdhEnumObjects = TPdhEnumObjectsW;
  TPdhEnumObjectItems = TPdhEnumObjectItemsW;
  TPdhGetDefaultPerfObject = TPdhGetDefaultPerfObjectW;
  TPdhGetDefaultPerfCounter = TPdhGetDefaultPerfCounterW;
  TPdhLookupPerfIndexByName = TPdhLookupPerfIndexByNameW;
  TPdhParseCounterPath = TPdhParseCounterPathW;
  {$ELSE}
  TPdhOpenQuery = TPdhOpenQueryA;
  TPdhExpandWildCardPath = TPdhExpandWildCardPathA;
  TPdhAddCounter = TPdhAddCounterA;
  TPdhAddEnglishCounter = TPdhAddEnglishCounterA;
  TPdhGetRawCounterArray = TPdhGetRawCounterArrayA;
  TPdhGetFormattedCounterArray = TPdhGetFormattedCounterArrayA;
  TPdhGetCounterInfo = TPdhGetCounterInfoA;
  TPdhBrowseCounters = TPdhBrowseCountersA;
  TPdhMakeCounterPath = TPdhMakeCounterPathA;
  TPdhConnectMachine = TPdhConnectMachineA;
  TPdhEnumMachines = TPdhEnumMachinesA;
  TPdhEnumObjects = TPdhEnumObjectsA;
  TPdhEnumObjectItems = TPdhEnumObjectItemsA;
  TPdhGetDefaultPerfObject = TPdhGetDefaultPerfObjectA;
  TPdhGetDefaultPerfCounter = TPdhGetDefaultPerfCounterA;
  TPdhLookupPerfIndexByName = TPdhLookupPerfIndexByNameA;
  TPdhParseCounterPath = TPdhParseCounterPathA;
  {$ENDIF}

const
  PDH_FMT_RAW      = DWORD($00000010);
  PDH_FMT_ANSI     = DWORD($00000020);
  PDH_FMT_UNICODE  = DWORD($00000040);
  PDH_FMT_LONG     = DWORD($00000100);
  PDH_FMT_DOUBLE   = DWORD($00000200);
  PDH_FMT_LARGE    = DWORD($00000400);
  PDH_FMT_NOSCALE  = DWORD($00001000);
  PDH_FMT_1000     = DWORD($00002000);
  PDH_FMT_NODATA   = DWORD($00004000);
  PDH_FMT_NOCAP100 = DWORD($00008000);

  PERF_DETAIL_COSTLY   = DWORD($00010000);
  PERF_DETAIL_STANDARD = DWORD($0000FFFF);

var
  PdhOpenQuery: TPdhOpenQuery = nil;
  PdhExpandWildCardPath: TPdhExpandWildCardPath = nil;
  PdhAddCounter: TPdhAddCounter = nil;
  PdhAddEnglishCounter: TPdhAddEnglishCounter = nil;
  PdhRemoveCounter: TPdhRemoveCounter = nil;
  PdhCollectQueryData: TPdhCollectQueryData = nil;
  PdhCloseQuery: TPdhCloseQuery = nil;
  PdhGetRawCounterValue: TPdhGetRawCounterValue = nil;
  PdhGetRawCounterArray: TPdhGetRawCounterArray = nil;
  PdhGetFormattedCounterValue: TPdhGetFormattedCounterValue = nil;
  PdhGetFormattedCounterArray: TPdhGetFormattedCounterArray = nil;
  PdhGetCounterInfo: TPdhGetCounterInfo = nil;
  PdhBrowseCounters: TPdhBrowseCounters = nil;
  PdhMakeCounterPath: TPdhMakeCounterPath = nil;
  PdhConnectMachine: TPdhConnectMachine = nil;
  PdhEnumMachines: TPdhEnumMachines = nil;
  PdhEnumObjects: TPdhEnumObjects = nil;
  PdhEnumObjectItems: TPdhEnumObjectItems = nil;
  PdhGetDefaultPerfObject: TPdhGetDefaultPerfObject = nil;
  PdhGetDefaultPerfCounter: TPdhGetDefaultPerfCounter = nil;
  PdhLookupPerfIndexByName: TPdhLookupPerfIndexByName = nil;
  PdhParseCounterPath: TPdhParseCounterPath  = nil;

function GetPdhErrorMessage(AValue: Cardinal): string;
function AddCounter(AQueryHandle: PDH_HQUERY; AName: string; var ACounterHandle: PDH_HCOUNTER): Boolean;
function GetCounterValue(const Aname: string): Double;

implementation

var
  PdhHandle: THandle;

function MAKELANGID(PrimaryLang, SubLang: Word): Word;
begin
  Result:=(SubLang shl 10) or PrimaryLang;
end;

function InitPdh: Boolean;
const
  {$IFDEF UNICODE}
  s = 'W';
  {$ELSE}
  s = 'A';
  {$ENDIF}
begin
  PdhHandle:=GetModuleHandle(PdhLib);
  if PdhHandle=0 then
    PdhHandle:=LoadLibrary(PdhLib);
  if PdhHandle<>0 then begin
    @PdhOpenQuery:=GetProcAddress(PdhHandle,'PdhOpenQuery'+s);
    @PdhExpandWildCardPath:=GetProcAddress(PdhHandle,'PdhExpandWildCardPath'+s);
    @PdhAddCounter:=GetProcAddress(PdhHandle,'PdhAddCounter'+s);
    @PdhAddEnglishCounter:=GetProcAddress(PdhHandle,'PdhAddEnglishCounter'+s);
    if not Assigned(PdhAddEnglishCounter) then
      PdhAddEnglishCounter:=PdhAddCounter;
    @PdhRemoveCounter:=GetProcAddress(PdhHandle,'PdhRemoveCounter');
    @PdhCollectQueryData:=GetProcAddress(PdhHandle,'PdhCollectQueryData');
    @PdhCloseQuery:=GetProcAddress(PdhHandle,'PdhCloseQuery');
    @PdhGetRawCounterValue:=GetProcAddress(PdhHandle,'PdhGetRawCounterValue');
    @PdhGetRawCounterArray:=GetProcAddress(PdhHandle,'PdhGetRawCounterArray'+s);
    @PdhGetFormattedCounterValue:=GetProcAddress(PdhHandle,'PdhGetFormattedCounterValue');
    @PdhGetFormattedCounterArray:=GetProcAddress(PdhHandle,'PdhGetFormattedCounterArray'+s);
    @PdhGetCounterInfo:=GetProcAddress(PdhHandle,'PdhGetCounterInfo'+s);
    @PdhBrowseCounters:=GetProcAddress(PdhHandle,'PdhBrowseCounters'+s);
    @PdhMakeCounterPath:=GetProcAddress(PdhHandle,'PdhMakeCounterPath'+s);
    @PdhConnectMachine:=GetProcAddress(PdhHandle,'PdhConnectMachine'+s);
    @PdhEnumMachines:=GetProcAddress(PdhHandle,'PdhEnumMachines'+s);
    @PdhEnumObjects:=GetProcAddress(PdhHandle,'PdhEnumObjects'+s);
    @PdhEnumObjectItems:=GetProcAddress(PdhHandle,'PdhEnumObjectItems'+s);
    @PdhGetDefaultPerfObject:=GetProcAddress(PdhHandle,'PdhGetDefaultPerfObject'+s);
    @PdhGetDefaultPerfCounter:=GetProcAddress(PdhHandle,'PdhGetDefaultPerfCounter'+s);
    @PdhLookupPerfIndexByName:=GetProcAddress(PdhHandle,'PdhLookupPerfIndexByName'+s);
    @PdhParseCounterPath:=GetProcAddress(PdhHandle,'PdhParseCounterPath'+s);
  end;
  Result:=(PdhHandle<>0) and Assigned(PdhOpenQuery) and Assigned(PdhEnumObjectItems);
end;

function GetPdhErrorMessage(AValue: Cardinal): string;
var
  lpMsgBuf: PChar;
  LangID,c: Cardinal;
begin
  Result:='';
  lpMsgBuf:=StrAlloc(4096);
  LangID:=MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT);
  c:=FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
                  Pointer(PdhHandle),AValue,LangID,lpMsgBuf,4096,nil);
  if c>0 then
    Result:=Trim(string(lpMsgBuf));
  StrDispose(lpMsgBuf);
end;

function AddCounter(AQueryHandle: PDH_HQUERY; AName: string; var ACounterHandle: PDH_HCOUNTER): Boolean;
begin
  if Assigned(PdhAddEnglishCounter) then
    Result:=(PdhAddEnglishCounter(AQueryHandle,PChar(AName),0,ACounterHandle)=ERROR_SUCCESS)
  else
    Result:=(PdhAddCounter(AQueryHandle,PChar(AName),0,ACounterHandle)=ERROR_SUCCESS);
end;

function GetCounterValue(const Aname: string): Double;
var
  pdhq: PDH_HQUERY;
  phc: PDH_HCOUNTER;
  ct: Cardinal;
  cv: TPdhFmtCounterValue;
begin
  Result:=0;
  if not Assigned(PdhOpenQuery) then
    Exit;
  PdhOpenQuery(nil,0,pdhq);
  if pdhq<>0 then begin
    if AddCounter(pdhq,AName,phc) then
      if PdhCollectQueryData(pdhq)=ERROR_SUCCESS then
        if (PdhGetFormattedCounterValue(phc,PDH_FMT_DOUBLE,@ct,cv)=ERROR_SUCCESS) and (cv.CStatus<>PDH_CSTATUS_INVALID_DATA) then
          Result:=cv.doubleValue;

    PdhCloseQuery(pdhq);
  end;
end;

initialization
  InitPdh;
end.

