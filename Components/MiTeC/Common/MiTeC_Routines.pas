{*******************************************************}
{               MiTeC Common Routines                   }
{                  Common routines                      }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Routines;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Win.Registry, VCL.Graphics, System.Classes, System.SysUtils,
     System.Variants, System.Win.ComObj, VCL.Controls,
     {$ELSE}
     Windows, Classes, SysUtils, Graphics, Variants, Registry, ComObj, Controls,
     {$IFDEF FPC}JwaTlHelp32, jwaPSAPI{$ELSE}TlHelp32{$ENDIF},
     {$ENDIF}
     MiTeC_Windows, MiTeC_PsAPI;

type
  TVersionInfo = record
    FileName,
    FileVersion,
    ProductVersion,
    FileVersionText,
    ProductVersionText,
    ProductName,
    CompanyName,
    Description,
    Comments,
    Copyright,
    Trademarks,
    InternalName,
    OriginalFilename: string;
    Major,
    Minor,
    Release,
    Build: Cardinal;
    ProductMajor,
    ProductMinor,
    ProductRelease,
    ProductBuild: Cardinal;
    SpecialBuild: string;
    PreReleaseBuild: Boolean;
    DebugBuild: Boolean;
    PrivateBuild: Boolean;
    BuildTimestamp: string;

    function GetCompany: string;
    function GetFullDesc: string;
  end;

  TNtProductType = (ptUnknown, ptWorkStation, ptServer, ptAdvancedServer, ptDataCenter, ptWeb);


  TNTSuite = (suSmallBusiness, suEnterprise, suBackOffice, suCommunications,
              suTerminal, suSmallBusinessRestricted, suEmbeddedNT, suDataCenter,
              suSingleUserTS,suPersonal,suBlade,suEmbeddedRestricted, suStorageServer,
              suComputeCluster, suHomeServer);

  TNTSuites = set of TNTSuite;

  TTerminateStatus = (tsError, tsClose, tsTerminate);

  TOSVersion = (osUnknown, os2K, osXP, os2K3, osVista,
                os2K8, osSeven, os2K8R2, osWin8, os2K12, osBlue, os2K12R2,
                osWin10, os2K16, os2K19);

  TMediaType = (dtUnknown, dtNotExists, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRAMDisk);

  TFileFlag = (fsCaseIsPreserved, fsCaseSensitive, fsUnicodeStoredOnDisk,
               fsPersistentAcls, fsFileCompression, fsVolumeIsCompressed,
               fsLongFileNames,
               fsEncryptedFileSystemSupport, fsObjectIDsSupport, fsReparsePointsSupport,
               fsSparseFilesSupport, fsDiskQuotasSupport);
  TFileFlags = set of TFileFlag;

  TDiskInfo = record
    Sign: string;
    MediaType: TMediaType;
    FileFlags: TFileFlags;
    SectorsPerCluster,
    BytesPerSector,
    FreeClusters,
    TotalClusters,
    Serial: Cardinal;
    Capacity,
    FreeSpace: Int64;
    VolumeLabel,
    SerialNumber,
    FileSystem: string;
  end;

  PWindowInfo = ^TWindowInfo;
  TWindowInfo = record
    ClassName,
    Text :String;
    Handle: THandle;
    Process,
    Thread :Cardinal;
    ParentWin,
    WndProc,
    Instance,
    ID,
    UserData,
    Style,
    ExStyle :integer;
    Rect,
    ClientRect :TRect;
    Atom,
    ClassBytes,
    WinBytes,
    ClassWndProc,
    ClassInstance,
    Background,
    Cursor,
    Icon,
    ClassStyle :integer;
    Styles,
    ExStyles,
    ClassStyles :TStringList;
    Visible,
    Enabled :boolean;
    WindowAffinity: Cardinal;
    _Flag: Boolean;
  end;

  TFileInfo = record
    Name: string;
    FileType: string;
    Size :UInt64;
    Created,
    Accessed,
    Modified :TDateTime;
    Attributes :Cardinal;
    BinaryType: string;
    IconHandle: THandle;
  end;

  TPrivilegeInfo = record
    Name,
    DisplayName: String;
    Flags: Cardinal;
  end;
  TPrivilegeList = array of TPrivilegeInfo;

  TTokenGroupInfo = record
    SID,
    Domain,
    Name: String;
    Flags: Cardinal;
  end;
  TTokenGroupList = array of TTokenGroupInfo;

  TDriveQueryData = record
    DiskLabel: String;
    DiskDosQuery: String;
    DosQueryLen: Integer;
  end;

  TSessionType = (stLocal,stVMWare,stVPC,stVBOX,stQEMU,stTerminal,stCitrix);
  TSessionTypes = set of TSessionType;

  TDigitalProductID = array[0..14] of Byte;

  TRegTimeZoneInfo = packed record
    Bias: Longint;
    StandardBias: Longint;
    DaylightBias: Longint;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

  TDynamicArrayCompare = function (AField: Integer; AIndex1, AIndex2: Integer; ADescending: Boolean): Integer;
  TDynamicArraySwap = procedure (AIndex1, AIndex2: Integer);

  TDynamicArrayCompareMethod = function (AField: Integer; AIndex1, AIndex2: Integer; ADescending: Boolean): Integer of object;
  TDynamicArraySwapMethod = procedure (AIndex1, AIndex2: Integer) of object;

  TFileEntry = record
    FileName: string;
    CreateTime,
    LastWrite,
    LastAccess: TDateTime;
    Size: Int64;
    Attr: Integer;
  end;
  TFileList = array of TFileEntry;

const
  flsFilename = 1;
  flsSize = 2;
  flsCreateTime = 3;
  flsLastWrite = 4;
  flsLastAccess = 5;

  cSessionType: array[TSessionType] of string = ('Local',
                                                 'VMWare',
                                                 'VirtualPC',
                                                 'VirtualBox',
                                                 'QEMU',
                                                 'Terminal',
                                                 'Citrix');

type
  TCardinalArray = array of Cardinal;

procedure DynamicArrayQuickSort(AField, ALowBound, AHighBound: integer; ACompare: TDynamicArrayCompare; ASwap: TDynamicArraySwap; ADescending: Boolean = False); overload;
procedure DynamicArrayQuickSort(AField, ALowBound, AHighBound: integer; ACompare: TDynamicArrayCompareMethod; ASwap: TDynamicArraySwapMethod; ADescending: Boolean = False); overload;

function MatchesMaskEx(const Filename, Mask: string): Boolean;
function BuildFileList(const Path: string; const Mask: string; Attr: Integer; const List: TStrings; Recursive: boolean = False; NoDuplicates: Boolean = False): Int64; overload;
function BuildFileList(const Path: string; const Mask: string; Attr: Integer; var List: TFileList; Recursive: boolean = False; NoDuplicates: Boolean = False): Int64; overload;
procedure FileListSort(var AList: TFileList; AField: Integer = flsFilename; ADescending: Boolean = False);
function DeleteDirectory(Path: String): Boolean;
function DeleteFiles(FileMask: string): Integer;
function DeleteFilesEx(const FileMasks: array of string): integer; overload;
function DeleteFilesEx(AFileMasks: TStrings): integer; overload;

function ExpandEnvVars(ASource: string; Extended: Boolean = True): string; overload;
function ExpandEnvVars(AEnvironment: TStrings; ASource: string): string; overload;
function GetEnvVarValue(Name: string): string;
function GetErrorMessage(ErrorCode: integer): string;
function GetUser :string;
function GetWindowsLiveID: string;

function GetMachine :string;
function GetOSName(var Product,Edition: string): string; overload;
function GetOSName(ABuild: Cardinal): string; overload;
function GetOSBuild: string;
function GetWinPEVersion: string;
procedure UpdateWinVersion;
function IsGenuine: SL_GENUINE_STATE;
function GetBuildLab: string;
function GetProductName(var AInstaType: string): string;
function GetTrueWindowsVersion: string;
function GetTrueWindowsName: string;
function GetFileVerInfo(const AFilename :string; out AData: TVersionInfo): Boolean;
function GetFileVersion(const fn: string): string;
function GetFileCopyright(const fn: string): string;
function GetFileProduct(const fn: string): string;
function GetFileDesc(const fn: string): string;
function GetFileOwner(AFilename: string): string;
procedure GetEnvironment(EnvList :tstrings);
function GetWinDir :string;
function GetSysDir :string;
function GetTempDir :string;
function GetWinSysDir: string;
procedure GetRecycleBin(AList: TStrings);
function GetSpecialFolder(Handle: Hwnd; nFolder: Integer): string;
function GetKnownFolderPath(AKnownFolderID: TGUID): string;
function GetSpecialFolderEx(AUser: string; ACSIDL: integer): string;
procedure GetProfileList(AList: TStrings; AExpand: Boolean = True);
function GetProfilePath(AUser: string = ''): string;
function GetFolderDateTime(const strFolder: String): TDateTime;
function GetMemoryLoad: Cardinal;

function GetWinText(AHandle: THandle; AClassNameIfEmpty: Boolean = False): string;
function GetWindowInfo(wh: hwnd; AStyles: Boolean = False): TWindowInfo;
function FindWindowByTitle(AHandle: THandle; const ClassName,WindowTitle: string): Hwnd;
function ForceForegroundWindow(AHandle: THandle): Boolean;
function GetUniqueFilename(Prefix: string; Unique: Cardinal = 0; Temp: Boolean = False): string;
function KillProcess(ProcessID: Cardinal; Timeout: Integer = MAXINT): TTerminateStatus;
function ProcessExists(const APID: Cardinal; var AThreadCount,APriority: integer): Boolean;
function GetChildProcesses(const APID: Cardinal; var AChildProcs: TCardinalArray): Boolean;
function IsProcessActive(APID: integer): Boolean;
function IsProcessResponsible(APID: Cardinal): Boolean;
function GetFontRes: Cardinal;
function CreateDOSProcessRedirected(const CommandLine, InputFile, OutputFile,ErrMsg :string): Boolean;
function CreateDOSProcessToStrings(CommandLine: string; AOutput: TStrings): Boolean;
function CreateDOSProcess(CommandLine: string): Boolean;
function GetCmdOutput(const ACommandLine: string): string;
function CreateProc(FileName: string; CommandLine: string = ''; ShowMode: Integer = -1; AWaitms: Cardinal = INFINITE; const ADir: string = ''): boolean;
function CreateProcEx(FileName: string; var AExitCode: Cardinal; CommandLine: string = ''; ShowMode: Integer = -1; AWaitms: Cardinal = INFINITE): Boolean;
function FindProcess(const AName: string; ASession: Cardinal = Cardinal(-1)): Integer;
function GetProccessInstanceCount(const AName: string): integer;
function FileExistsEx(const FileName: string): Boolean;
//function FileTimeToDateTimeStr(FileTime: TFileTime): string;
//function FiletimeToDateTime(FT: FILETIME): TDateTime;
procedure GetFileInfo(const AFilename: string; var AFileInfo: TFileInfo; AConvertToLocalTime: Boolean = False);
function GetFileIconCount(const Filename: string): Integer;
function GetFileIcon(const Filename: string; IconIndex: Word = 0): HICON;
function GetFileSize(const AFilename: string): int64;
function GetFileTimes(const AFilename: string; out ACreated,AModified,AAccessed: TDateTime; ConvertTimeToLocal: Boolean = False): int64;
function HasAttr(const AFileName: string; AAttr: Word): Boolean;
function GetBinType(const AFilename :string) :string;
function ExtractUNCFilename(ASource :string) :string;
function DequoteStr(Source: string; Quote: Char = '"'): string;
function ExtractFilenameFromStr(Source: string): string;
function ExtractName(const AFilename: string): string;
function FileCopy(const AFileName, ADestName: string): Boolean;
function FileMove(const AFileName, ADestName: string): boolean;
function FileNameMove(const AFileName, ADestName: string): Integer;
function FileNameCopy(const AFileName,AExtSpec, ADestName: string): Integer;
procedure SaveToFile(AFilename,AText: string; AOverwrite: Boolean = False);
function GetMediaTypeStr(MT: TMediaType): string;
function GetMediaPresent(const Value: string) :Boolean;
function GetMediaIcon(Value: string) :THandle;
function GetDiskInfo(Value: string): TDiskInfo;
function GetAvailDisks : string;
procedure GetCDs(cds :tstrings);
function OpenMailSlot(Const Server, Slot : String): THandle;
function SendToMailSlot(Const Server, Slot, Mail : String) : Boolean;
function SendToWinpopup(Server, Reciever, Sender, Msg : String) : Boolean;
function IsBitOn(Value: UInt64; Bit: Byte): Boolean;
function SetBit(const Value: UInt64; const Bit: byte): UInt64;
function ClearBit(const Value: UInt64; const Bit: Byte): UInt64;
function GetBitsFromDWORD(const aval: Cardinal; const afrom,ato: byte): Integer;
function CountSetBits(bitMask: {$IFDEF FPC}Cardinal{$ELSE}ULONG_PTR{$ENDIF}): DWORD;
procedure RunAtStartup(AKey: HKEY; Flag: Boolean; Name,Cmdline: string);
function CheckRunAtStartup(Akey: HKEY; Name,CmdLine: string): Boolean;
function ExtractImageName(ACmdLine: string; AExtendedVarExpand: boolean = True): string;

function WinExecAndWait32(FileName,Parameters: String; Visibility: integer): Cardinal;
procedure ScanNetResources(AList: TStrings);
function NetResourceConnect(const AResource, AUser, APassword: string): Integer;
function NetResourceDisconnect(const AResource: string): Boolean;
function GetLogicalDisks(OnlyWithMedia: Boolean = False): string;
function GetRemovableDisks: string;

function AppIsResponding(AHandle: THandle): Boolean;
function EnablePrivilege(Privilege: string): Boolean;
function DisablePrivileges: Boolean;
function DisablePrivilege(Privilege: string): Boolean;
function ConvertSIDToString(ASID: Pointer): string;
function ConvertStringToSID(ASID: string): PSID;
function GetSIDFromAccount(AMachine, AName: string): string;
function GetAccountFromSID(ASID: PSID; ASystemName: string = ''): string;
function GetUserObjectSID(AObj: Cardinal): string;
function IsAdmin: Boolean;
function GetProcessFilename(APID: Cardinal): string;
function GetModFilename(APID: Cardinal; const AName: string): string;
function GetProcessUserName(hProcess :THandle; var UserName, DomainName :string) :Boolean;
function GetProcessUserSID(hProcess :THandle): string;
function GetProcessUserNameFromPID(PID :Cardinal; var UserName, DomainName :string) :Boolean;
function GetProcessUserNameEx(PID :Cardinal; var UserName, DomainName :string) :Boolean;
function GetProcessPrivileges(Processhandle: THandle; var AList: TPrivilegeList; var AElevation: Cardinal): boolean;
function IsPrivilegeEnabled(const Privilege: string): Boolean;
function GetProcessGroups(Processhandle: THandle; var AList: TTokenGroupList): Boolean;
function GetProcessWorkingSet(AHandle: THandle): Int64;
function GetProcessPIDWorkingSet(APID: Cardinal): Int64;
function GetProcessMemoryCounters(AHandle: THandle): TProcessMemoryCountersEx;
function GetProcessWindow(const APID: Cardinal): THandle;
function GetWindowCount(APID: Cardinal; AOnlyVisible: boolean = True): Cardinal;
function GetProcessHandle(APID: Cardinal; AFlags: Cardinal = 0): THandle;
function GetThreadHandle(AID: Cardinal; AFlags: Cardinal = 0): THandle;
function IsProcess64bit(APID: Cardinal): Boolean;
function GetParentProcess(APID: Cardinal): Cardinal;

function DecodeProductKey(PID: TDigitalProductID): string;
function ReadDPID(AReg: TRegistry; AValueName: string): string;

function AssignHotkey(Handle: HWND; HotKey: TShortCut; KeyIdx: Word): Boolean;
function ValidHotkey(Handle: HWND; HotKey: TShortCut; KeyIdx: Word): Boolean;
procedure ClearKeyBoardBuffer;

function GetLastFilename(AFilename: string): string;

procedure MultiWideStrFromBuf(Buffer: array of Byte; Len: Integer; var List: TStringList);
procedure MultiStrFromBuf(Buffer: array of Byte; Len: Integer; var List: TStringList);
function ReadValueAsString(AReg: TRegistry; const Value: string): string;

function VarToFloat(Source: Variant): Double;
function VarToInt(Source: Variant): Integer;
function VarToInt64(Source: Variant): Int64;
function VarToBool(Source: Variant): boolean;
function VarToDT(Source: Variant): Tdatetime;
function IntToBin(AValue: Int64; ANumBits: word = 64): string;
function BinToInt(AValue: String): Int64;
function IntToRoman(AValue: int64): string;
function DatetimeToVar(ADT: TDateTime): Variant;

function GetObjectFullName(Sender: TObject): string;
{$IFNDEF FPC}
function ConvertAddr(Address: Pointer): Pointer; assembler;
procedure ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
{$ENDIF}
function CorrectFilename(fn: string; subst: Char = #32): string;

procedure StartTimer;
function StopTimer: comp;

function SwapEndian(const Value : LongWord) : Longword; overload;
function SwapEndian(const Value: Int64): Int64; overload;
function UNIX32ToDatetime(ADate: Cardinal): TDateTime;
function Complement(Value: Cardinal): Cardinal;
function NumberInSet(const AValue: Integer; const ASet: array of Integer): Boolean;

function IsVirtualMachine(ASignature: string): Boolean;

{$IFNDEF FPC}
function _IsVMWARE: Boolean;
function _IsVPC: Boolean;
{$ENDIF}
function IsRemoteSession: boolean;
function IsVMWARE: Boolean;
function IsVPC: Boolean;
function IsVBOX: Boolean;
function IsQEMU: Boolean;
function IsCitrix: Boolean;
function IsPW: Boolean;
function IsWinPE: Boolean;
function GetSession: TSessionTypes;
function GetSessionStr(ASession: TSessionTypes): string;
function SessionTypesAsInt(A: TSessionTypes): Cardinal;
function IntAsSessionTypes(A: Cardinal): TSessionTypes;

function IsUAC: Boolean;
function IsUACEnabled: Boolean;
function IsElevated: Boolean;
function IsVirtualized: Boolean;

procedure SaveResource(AName,AFilename: string);
procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);

procedure GetDebugPrivs;

function IsEqualTZ(ATZ1, ATZ2: TTimeZoneInformation): Boolean;
function GetTimeZone(out ATZ: TTimeZoneInformation): string;
function GetTZDaylightSavingInfoForYear(TZ: TTimeZoneInformation; year: Word; var DaylightDate, StandardDate: TDateTime; var DaylightBias, StandardBias: longint): Boolean;

function GetDeviceHandle(AName: string): THandle;

procedure ScanNetwork(lpnr: PNETRESOURCE; AList: TStrings);

function GetTrayWndHeight: Cardinal;

function GetLocaleLangId: Integer;
function GetLocaleProp(LCType: Cardinal): string;

function Join(const LoWord, HiWord:word):Integer;

function GetWinDirFromBoot(ADisk: string; var OS: string): string;
procedure GetUsersFromDisk(ADisk: string; AList: TStrings);

procedure CreateDosDevicesTable;
function GetVolumeNameForVolumeMountPoint (lpszVolumeMountPoint: PChar; lpszVolumeName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
function GetVolumeNameForVolumeMountPointString(Name: string): string;
function GetVolumePathName(lpszFilename: PChar; lpszVolumePathName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
procedure CreateMountPointTable(ATable: TStrings);
function VolumeNameToDeviceName(const VolName: String): String;

function KernelNameToFilename(AName: string): string;
procedure GetSpecialAccounts(AList: TStrings);

function CreateProcWithLogon(UserName,Password,CmdLine: {$IFDEF UNICODE}WideString{$ELSE}AnsiString{$ENDIF}):Boolean;
function CreateProcessAsUserInSession(UserName,Domain,Password: string; Session: Cardinal; CmdLine: string): Cardinal;
function FormatOSName(const AName: string): string;
procedure GetServerInfo(var Comment: string; var Flags,LicensedUsers,UsersPerLicense: Cardinal);

function IsFileLocked(AFilename: string; AReadOnly: boolean = True): Boolean;

function RunAsAdmin(hWnd: HWND; AFilename, AParams: string): Boolean;

function LoadResourceString(const DllName: String; ResourceId: Cardinal): string;
procedure LoadResourceStrings(const DllName: String; ACount: Integer; Strings: TStringList);

function WindowsExit(AMode: Cardinal): Boolean;

procedure FixLocale; overload;
procedure FixLocale(var AFS: TFormatSettings); overload;
function HtmlToColor(AHTML:string; ADefault: TColor): TColor;

procedure ResetMemory(out P; Size: Longint);

{$IFDEF RAD6PLUS}
procedure SaveBytesToStream(ABytes: TBytes; AStream: TStream);
procedure SaveBytesToFile(ABytes: TBytes; AFilename: string);
procedure SaveStringToFile(AString: string; AFilename: string); overload;
procedure SaveStringToFile(AString: ansistring; AFilename: string); overload;
{$ENDIF}


procedure GetCPUTopology(var APkgCnt,ACoreCnt,AThrCnt: Byte);

function GetTaskManager: string;

function WinControlExists(AControl: TWinControl): Boolean;

function GeneratePassword(ALength: Integer; AUpper, ALower, ANumbers, ASymbols: Boolean): string;

function GetActiveOleObject(const ClassName: string): IDispatch;
function CreateOleObject(const ClassName: string): IDispatch;
function GetObjectIntf(const AClassName: string): OLEVariant;

function GetMonitorPixelsPerInch(AMonitor: THandle): Integer;

function GDIProcessHandleQuota: integer;
function USERProcessHandleQuota: Integer;

function GetDelphi: string;

function CalcEntropy(AData: PByte; ASize: int64): Double; overload;
function CalcEntropy(AData: TBytes): Double; overload;
function CalcEntropy(const AFilename: string): Double; overload;

function FileChecksum(const AFilename: string): Cardinal;

function FindInStream(const ASubStr: String; const AStream: TMemoryStream; APosition: Int64 = 0): Integer; overload;
function FindInStream(const APattern: TBytes; const AStream: TStream; APosition: Int64 = 0): Int64; overload;
procedure DeleteFromStream(AStream: TStream; AStart, ALength: Int64);
function FindInFile(const APattern: TBytes; const AFilename: string): Int64;
procedure SplitFile(const AFilename: string; AStartOffsets: array of Int64; AFilenames: array of string);
procedure MergeFiles(AFilenames: array of string; const AFilename: string);

const
  ClsName: array[0..6] of char = 'Window'#0;
  DescValue = 'DriverDesc';

  StartStat = 'PerfStats\StartStat';
  StatData = 'PerfStats\StatData';
  StopStat = 'PerfStats\StopStat';

var
  SystemInfo: TSystemInfo;
  OSVIX :TOSVersionInfoEx;
  OSVI :TOSVersionInfo;
  ModuleName, OSName, OSEdition, ClassKey, ServicePack, FormOSName, BuildLab, OSBuild: string;
  Is2K,IsXP,Is2K3,IsVista,Is2K8,IsSeven,Is64,IsWin8,Is2K12,IsBlue,IsWin10: Boolean;
  WindowsUser, WindowsUserSID, MachineName, ProfilePath, ProductName, InstallationType,
  TrueWindowsVersion, TrueWindowsName, WindowsLiveID: string;
  CompatibilityMode: Boolean;
  OS: TOSVersion;
  Memory: Int64;
  EXEVersionInfo, ModuleInfo: TVersionInfo;
  InstalledSuites: TNTSuites;
  InstallDate: TDateTime;
  ProductType: TNTProductType;
  IsServer: Bool;
  LangId: Integer;
  DosDevices: array [0..25] of TDriveQueryData;
  Session: TSessionTypes;

implementation

uses {$IFDEF RAD9PLUS}
     System.StrUtils, System.DateUtils, WinAPI.ShellAPI, WinAPI.Messages, System.INIFiles,
     WinAPI.ShlObj, WinAPI.ActiveX, System.Masks, WinAPI.TlHelp32, System.RTLConsts,
     System.Math, Vcl.GraphUtil,
     {$ELSE}
     StrUtils, DateUtils,
     ShellAPI, Messages, INIFiles, ShlObj, ActiveX, RTLConsts, Math, Masks, GraphUtil,
     {$ENDIF}
     MiTeC_Datetime, MiTeC_StrUtils, MiTeC_NetAPI32, MiTeC_RegUtils, MiTeC_Mappings;

var
  MS: TMemoryStatus;
  MSEX: TMemoryStatusEx;
  InternalTimer: comp;
  _FileList: TFileList;
  irwh: HWND;

const
  wpSlot = 'messngr';

procedure DynamicArrayQuickSort(AField, ALowBound, AHighBound: integer; ACompare: TDynamicArrayCompare; ASwap: TDynamicArraySwap; ADescending: Boolean = False);

  procedure _QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while ACompare(AField,Lo,Mid,ADescending)<0 do
          Inc(Lo);
        while ACompare(AField,Hi,Mid,ADescending)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            ASwap(Lo,Hi);
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        _QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  if (AHighBound>ALowBound) then
    _QuickSort(ALowBound,AHighBound);
end;

procedure DynamicArrayQuickSort(AField, ALowBound, AHighBound: integer; ACompare: TDynamicArrayCompareMethod; ASwap: TDynamicArraySwapMethod; ADescending: Boolean);

  procedure _QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while ACompare(AField,Lo,Mid,ADescending)<0 do
          Inc(Lo);
        while ACompare(AField,Hi,Mid,ADescending)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            ASwap(Lo,Hi);
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        _QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  if (AHighBound>ALowBound) then
    _QuickSort(ALowBound,AHighBound);
end;

function MatchesMaskEx(const Filename, Mask: string): Boolean;
var
  sl: TStringList;
  i: Integer;
begin
  Result:=False;
  sl:=TStringList.Create;
  try
    {$IFDEF BDS3PLUS}
    sl.Delimiter:=';';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=Mask;
    {$ELSE}
    SetDelimitedText(Mask,';',sl);
    {$ENDIF}
    for i:=0 to sl.Count-1 do
      Result:=Result or MatchesMask(FileName,sl[i])
  finally
    sl.Free;
  end;
end;

function BuildFileList(const Path: string; const Mask: string; Attr: Integer; const List: TStrings; Recursive: Boolean; NoDuplicates: Boolean): int64;
var
  SearchRec: TSearchRec;
  R,idx,i: Integer;
  p,s: string;
begin
  Result:=0;
  if not Assigned(List) then
    Exit;
  p:=IncludeTrailingPathDelimiter(Path);
  R:=FindFirst(p+'*.*',faAnyfile,SearchRec);
  try
    while (R=0) do begin
      if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
        s:=SearchRec.Name;
        if Pos('.',s)=0 then
          s:=s+'.';
        if ((SearchRec.Attr and faDirectory=0) or (Attr=faDirectory)) and (SearchRec.Attr and Attr>0) and MatchesMaskEx(s,Mask) then begin
          idx:=-1;
          if NoDuplicates then
            for i:=0 to List.Count-1 do
              if SameText(ExtractFileName(List[i]),SearchRec.Name) then begin
                idx:=i;
                Break;
              end;
          if (idx=-1) then begin
            List.Add(p+SearchRec.Name);
            inc(Result,SearchRec.Size);
          end;
        end;

        if Recursive and (SearchRec.Attr and faDirectory=faDirectory) then
          Result:=Result+BuildFileList(p+SearchRec.Name,Mask,Attr,List,Recursive,NoDuplicates);
      end;
      R:=FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

function BuildFileList(const Path: string; const Mask: string; Attr: Integer; var List: TFileList; Recursive: boolean = False; NoDuplicates: Boolean = False): Int64;
var
  SearchRec: TSearchRec;
  i,R,idx,t: Integer;
  p,s: string;
  LocalFileTime: TFileTime;
  e: TFileEntry;
begin
  Result:=0;
  p:=IncludeTrailingPathDelimiter(Path);
  R:=FindFirst(p+'*.*',faAnyfile,SearchRec);
  try
    while (R=0) do begin
      if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
        s:=SearchRec.Name;
        if Pos('.',s)=0 then
          s:=s+'.';
        if ((SearchRec.Attr and faDirectory=0) or (Attr=faDirectory)) and (SearchRec.Attr and Attr>0) and MatchesMaskEx(s,Mask) then begin
          idx:=-1;
          if NoDuplicates then
            for i:=0 to High(List) do
              if SameText(ExtractFileName(List[i].FileName),SearchRec.Name) then begin
                idx:=i;
                Break;
              end;
          if (not NoDuplicates or (idx=-1)) then begin
            e.FileName:=p+SearchRec.Name;
            e.Size:=SearchRec.Size;
            e.Attr:=SearchRec.Attr;
            FileTimeToLocalFileTime(SearchRec.FindData.ftCreationTime,LocalFileTime);
            FileTimeToDosDateTime(LocalFileTime,LongRec(t).Hi,LongRec(t).Lo);
            e.CreateTime:=FileDateToDateTime(t);
            FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime,LocalFileTime);
            FileTimeToDosDateTime(LocalFileTime,LongRec(t).Hi,LongRec(t).Lo);
            e.LastWrite:=FileDateToDateTime(t);
            FileTimeToLocalFileTime(SearchRec.FindData.ftLastAccessTime,LocalFileTime);
            FileTimeToDosDateTime(LocalFileTime,LongRec(t).Hi,LongRec(t).Lo);
            e.LastAccess:=FileDateToDateTime(t);
            SetLength(List,Length(List)+1);
            List[High(List)]:=e;
          end;
          inc(Result,SearchRec.Size);
        end;

        if Recursive and (SearchRec.Attr and faDirectory=faDirectory) then
          Result:=Result+BuildFileList(p+SearchRec.Name,Mask,Attr,List,Recursive,NoDuplicates);
      end;
      R:=FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

function FileListCompare(AField: Integer; AIndex1, AIndex2: Integer; ADescending: Boolean): integer;
begin
  Result:=0;
  case AField of
    flsFilename: Result:=CompareText(_FileList[AIndex1].FileName,_FileList[AIndex2].FileName);
    flsSize: Result:=CompareValue(_FileList[AIndex1].Size,_FileList[AIndex2].Size);
    flsCreateTime: Result:=CompareDateTime(_FileList[AIndex1].CreateTime,_FileList[AIndex2].CreateTime);
    flsLastWrite: Result:=CompareDateTime(_FileList[AIndex1].LastWrite,_FileList[AIndex2].LastWrite);
    flsLastAccess: Result:=CompareDateTime(_FileList[AIndex1].LastAccess,_FileList[AIndex2].LastAccess);
  end;
  if ADescending then
    Result:=-Result;
end;

procedure FileListSwap(AIndex1, AIndex2: integer);
var
  r: TFileEntry;
begin
  r:=_FileList[AIndex1];
  _FileList[AIndex1]:=_FileList[AIndex2];
  _FileList[AIndex2]:=r;
end;

procedure FileListSort(var AList: TFileList; AField: Integer = flsFilename; ADescending: Boolean = False);
begin
  _FileList:=AList;
  DynamicArrayQuickSort(AField,0,High(AList),@FileListCompare,@FileListSwap,ADescending);
end;

function DeleteDirectory(Path: String): Boolean;
var
  Files: TStringList;
  LPath: string; // writable copy of Path
  FileName: string;
  I: Integer;
  PartialResult: Boolean;
  Attr: Cardinal;
begin
  Result:=True;
  Files:=TStringList.Create;
  try
    LPath:=ExcludeTrailingPathDelimiter(Path);
    BuildFileList(LPath,'*.*',faAnyFile,Files);
    for I:=0 to Files.Count-1 do begin
      FileName:=Files[i];
      PartialResult:=True;
      // If the current file is itself a directory then recursively delete it
      Attr:=GetFileAttributes(PChar(FileName));
      if (Attr <> Cardinal(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
        PartialResult:=DeleteDirectory(FileName)
      else begin
        {if Assigned(Progress) then
          PartialResult:=Progress(FileName, Attr);}
        if PartialResult then begin
          // Set attributes to normal in case it's a readonly file
          PartialResult:=SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_NORMAL);
          if PartialResult then
            PartialResult:=DeleteFile(FileName);
        end;
      end;
      if not PartialResult then begin
        Result:=False;
        {if AbortOnFailure then
          Break;}
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
  if Result then begin
    // Finally remove the directory itself
    Result:=SetFileAttributes(PChar(LPath), FILE_ATTRIBUTE_NORMAL);
    if Result then begin
      {$IOCHECKS OFF}
      RmDir(LPath);
      {$IFDEF IOCHECKS_ON}
      {$IOCHECKS ON}
      {$ENDIF IOCHECKS_ON}
      Result:=IOResult = 0;
    end;
  end;
end;

function DeleteFiles(FileMask: string): integer;
var
  SearchRec: TSearchRec;
begin
  Result:=0;
  try
    if FindFirst(ExpandFileName(FileMask),faAnyFile,SearchRec)=0 then
      repeat
{$WARNINGS OFF}
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
          (SearchRec.Attr and faVolumeID <> faVolumeID) and
          (SearchRec.Attr and faDirectory <> faDirectory) then
        begin
          if DeleteFile(ExtractFilePath(FileMask)+SearchRec.Name) then
            Inc(Result);
        end;
{$WARNINGS ON}
      until FindNext(SearchRec)<>0;
  finally
    FindClose(SearchRec);
  end;
end;

function DeleteFilesEx(const FileMasks: array of string): integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=Low(FileMasks) to High(FileMasks) do
    Result:=Result+DeleteFiles(FileMasks[I]);
end;

function DeleteFilesEx(AFileMasks: TStrings): integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=0 to AFileMasks.Count-1 do
    Result:=Result+DeleteFiles(AFileMasks[I]);
end;

function GetErrorMessage(ErrorCode: integer): string;
const
  BUFFER_SIZE = 1024;
var
  lpMsgBuf: PChar;
  LangID: Cardinal;
begin
  lpMsgBuf:=StrAlloc(BUFFER_SIZE);
  LangID:=$409;//GetUserDefaultLangID;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
                nil,ErrorCode,LangID,lpMsgBuf,BUFFER_SIZE,nil);
  Result:=lpMsgBuf;
  StrDispose(lpMsgBuf);
end;

function GetOSName(var Product,Edition: string): string;
var
  d: Cardinal;
begin
  Is64:=False;
  Result:='';
  Product:='';
  Edition:='';
  InstallDate:=0;

  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion',False) then begin
        if ValueExists('InstallDate') then begin
          d:=ReadInteger('InstallDate');
          InstallDate:=Int(Encodedate(1970,1,1));
          InstallDate:=((InstallDate*SecsPerDay)+d)/SecsPerDay;
        end;
        CloseKey;
      end;
    finally
      Free;
    end;

  d:=0;
  if Assigned(GetProductInfo) then
    GetProductInfo(OSVIX.dwMajorVersion,OSVIX.dwMinorVersion,OSVIX.wServicePackMajor,OSVIX.wServicePackMinor,d);

  case d of
    PRODUCT_BUSINESS                         : Edition:='Business';
    PRODUCT_BUSINESS_N                       : Edition:='Business N';
    PRODUCT_CLUSTER_SERVER                   : Edition:='HPC Edition';
    PRODUCT_DATACENTER_SERVER                : Edition:='Datacenter Full';
    PRODUCT_DATACENTER_SERVER_CORE           : Edition:='Datacenter Core';
    PRODUCT_DATACENTER_SERVER_CORE_V         : Edition:='Datacenter without Hyper-V Core';
    PRODUCT_DATACENTER_SERVER_V              : Edition:='Datacenter without Hyper-V Full';
    PRODUCT_ENTERPRISE                       : Edition:='Enterprise';
    PRODUCT_ENTERPRISE_E                     : Edition:='Enterprise E';
    PRODUCT_ENTERPRISE_N                     : Edition:='Enterprise N';
    PRODUCT_ENTERPRISE_SERVER                : Edition:='Enterprise Full';
    PRODUCT_ENTERPRISE_SERVER_CORE           : Edition:='Enterprise Core';
    PRODUCT_ENTERPRISE_SERVER_CORE_V         : Edition:='Enterprise without Hyper-V Core';
    PRODUCT_ENTERPRISE_SERVER_IA64           : Edition:='Enterprise for Itanium-based Systems';
    PRODUCT_ENTERPRISE_SERVER_V              : Edition:='Enterprise without Hyper-V Full';
    PRODUCT_HOME_BASIC                       : Edition:='Home Basic';
    PRODUCT_HOME_BASIC_E                     : Edition:='Home Basic E';
    PRODUCT_HOME_BASIC_N                     : Edition:='Home Basic N';
    PRODUCT_HOME_PREMIUM                     : Edition:='Home Premium';
    PRODUCT_HOME_PREMIUM_E                   : Edition:='Home Premium E';
    PRODUCT_HOME_PREMIUM_N                   : Edition:='Home Premium N';
    PRODUCT_HYPERV                           : Edition:='Microsoft Hyper-V Server';
    PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT : Edition:='Windows Essential Business Server Management Server';
    PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING  : Edition:='Windows Essential Business Server Messaging Server';
    PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY   : Edition:='Windows Essential Business Server Security Server';
    PRODUCT_PROFESSIONAL                     : Edition:='Professional';
    PRODUCT_PROFESSIONAL_E                   : Edition:='Professional E';
    PRODUCT_PROFESSIONAL_N                   : Edition:='Professional N';
    PRODUCT_SERVER_FOR_SMALLBUSINESS         : Edition:='Essential Solutions';
    PRODUCT_SERVER_FOR_SMALLBUSINESS_V       : Edition:='without Hyper-V for Windows Essential Server Solutions';
    PRODUCT_SERVER_FOUNDATION                : Edition:='Foundation';
    PRODUCT_SMALLBUSINESS_SERVER             : Edition:='Small Business';
    PRODUCT_STANDARD_SERVER                  : Edition:='Standard Full';
    PRODUCT_STANDARD_SERVER_CORE             : Edition:='Standard Core';
    PRODUCT_STANDARD_SERVER_CORE_V           : Edition:='Standard without Hyper-V Core';
    PRODUCT_STANDARD_SERVER_V                : Edition:='Standard without Hyper-V Full';
    PRODUCT_STARTER                          : Edition:='Starter';
    PRODUCT_STARTER_E                        : Edition:='Starter E';
    PRODUCT_STARTER_N                        : Edition:='Starter N';
    PRODUCT_STORAGE_ENTERPRISE_SERVER        : Edition:='Storage Server Enterprise';
    PRODUCT_STORAGE_EXPRESS_SERVER           : Edition:='Storage Server Express';
    PRODUCT_STORAGE_STANDARD_SERVER          : Edition:='Storage Server Standard';
    PRODUCT_STORAGE_WORKGROUP_SERVER         : Edition:='Storage Server Workgroup';
    PRODUCT_UNDEFINED                        : Edition:='An unknown product';
    PRODUCT_ULTIMATE                         : Edition:='Ultimate';
    PRODUCT_ULTIMATE_E                       : Edition:='Ultimate E';
    PRODUCT_ULTIMATE_N                       : Edition:='Ultimate N';
    PRODUCT_WEB_SERVER                       : Edition:='Web Server Full';
    PRODUCT_WEB_SERVER_CORE                  : Edition:='Web Server Core';
    PRODUCT_CORE                             : Edition:='Home';
    PRODUCT_CORE_N                           : Edition:='Home N';
    PRODUCT_CORE_COUNTRYSPECIFIC             : Edition:='Home China';
    PRODUCT_CORE_SINGLELANGUAGE              : Edition:='Home Single Language';
    PRODUCT_MOBILE_CORE                      : Edition:='Mobile';
    PRODUCT_MOBILE_ENTERPRISE                : Edition:='Mobile Enterprise';
    PRODUCT_EDUCATION                        : Edition:='Education';
    PRODUCT_EDUCATION_N                      : Edition:='Education N';
  end;

  if Edition='' then begin
    if OSVIX.wSuiteMask and VER_SUITE_EMBEDDEDNT<>0 then
      Edition:='Embedded'
    else if OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE<>0 then
      Edition:='Enterprise';
  end;

  {case OSVIX.dwPlatformId of
    VER_PLATFORM_WIN32_NT: begin}
      if OSVIX.dwMajorVersion=10 then begin
        if OSVIX.wProductType = VER_NT_WORKSTATION then begin
          Product:='Windows 10';
          OS:=osWin10;
        end else if OSVIX.dwBuildNumber<17663 then begin
          Product:='Windows Server 2016';
          OS:=os2K16;
        end else begin
          Product:='Windows Server 2019';
          OS:=os2K19;
        end;
        if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64 then begin
          if Edition<>'' then
            Edition:=Edition+' ';
          Edition:=Edition+'x64';
          Is64:=True;
        end;
      end else if OSVIX.dwMajorVersion=6 then begin
        case OSVIX.dwMinorVersion of
          0: if (OSVIX.wProductType = VER_NT_WORKSTATION) or (d in [PRODUCT_BUSINESS,PRODUCT_BUSINESS_N]) then begin
               Product:='Windows Vista';
               OS:=osVista;
             end else begin
               Product:='Windows Server 2008';
               OS:=os2K8;
             end;
          1: if OSVIX.wProductType = VER_NT_WORKSTATION then begin
               Product:='Windows 7';
               OS:=osSeven;
             end else begin
               Product:='Windows Server 2008 R2';
               OS:=os2K8R2;
             end;
          2: if OSVIX.wProductType = VER_NT_WORKSTATION then begin
               Product:='Windows 8';
               OS:=osWin8;
             end else begin
               Product:='Windows Server 2012';
               OS:=os2K12;
             end;
          3: if OSVIX.wProductType = VER_NT_WORKSTATION then begin
               Product:='Windows 8.1';
               OS:=osBlue;
             end else begin
               Product:='Windows Server 2012 R2';
               OS:=os2K12R2;
             end;
        end;

        if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64 then begin
          if Edition<>'' then
            Edition:=Edition+' ';
          Edition:=Edition+'x64';
          Is64:=True;
        end;
      end else begin
        Edition:='';
        if (OSVIX.dwMajorVersion=5) and (OSVIX.dwMinorVersion=2) then begin
          if (OSVIX.wProductType=VER_NT_WORKSTATION) and
             (SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64) then begin
            Product:='Windows XP';
            Edition:='Professional x64';
            OS:=osXP;
            Is64:=True;
          end else
          if (GetSystemMetrics(SM_SERVERR2)>0) then begin
            Product:='Windows Server 2003 R2';
            OS:=os2K3;
          end else
          if (OSVIX.wSuiteMask and VER_SUITE_STORAGE_SERVER)=VER_SUITE_STORAGE_SERVER then begin
            Product:='Windows Storage Server 2003';
            OS:=os2K3;
          end else
          if (OSVIX.wSuiteMask and VER_SUITE_WH_SERVER)=VER_SUITE_WH_SERVER then begin
            Product:='Windows Home Server';
            OS:=os2K3;
          end else begin
            Product:='Windows Server 2003';
            OS:=os2K3;
          end;


          if OSVIX.wProductType <> VER_NT_WORKSTATION then begin
            if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_IA64 then begin
              if (OSVIX.wSuiteMask and VER_SUITE_DATACENTER)=VER_SUITE_DATACENTER then
                Edition:='Datacenter for Itanium-based Systems'
              else
              if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
                Edition:='Enterprise for Itanium-based Systems';
            end else
              if SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then begin
                Is64:=True;
                if (OSVIX.wSuiteMask and VER_SUITE_DATACENTER)=VER_SUITE_DATACENTER then
                  Edition:='Datacenter x64'
                else
                if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
                  Edition:='Enterprise x64'
                else
                  Edition:='Standard x64';
              end else begin
                if (OSVIX.wSuiteMask and VER_SUITE_COMPUTE_SERVER)=VER_SUITE_COMPUTE_SERVER then
                  Edition:='Compute Cluster'
                else
                if (OSVIX.wSuiteMask and VER_SUITE_DATACENTER)=VER_SUITE_DATACENTER then
                  Edition:='Datacenter'
                else
                if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
                  Edition:='Enterprise'
                else
                if (OSVIX.wSuiteMask and VER_SUITE_BLADE)=VER_SUITE_BLADE then
                  Edition:='Web'
                else
                  Edition:='Standard';
             end;
           end;
        end else if (OSVIX.dwMajorVersion=5) and (OSVIX.dwMinorVersion=1) then begin
          Product:='Windows XP';
          OS:=osXP;
          if (OSVIX.wSuiteMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then
            Edition:='Home'
          else
            Edition:='Professional';
          if (GetSystemMetrics(SM_STARTER)>0) then
            Edition:=Edition+' Starter'
          else if (GetSystemMetrics(SM_TABLETPC)>0) then
            Edition:=Edition+' Tablet PC'
          else if (GetSystemMetrics(SM_MEDIACENTER)>0) then
            Edition:=Edition+' Media Center';
        end else if (OSVIX.dwMajorVersion=5) and (OSVIX.dwMinorVersion=0) then begin
          Product:='Windows 2000';
          OS:=os2K;
          if OSVIX.wProductType=VER_NT_WORKSTATION then
            Edition:='Professional'
          else begin
            if (OSVIX.wSuiteMask and VER_SUITE_DATACENTER)=VER_SUITE_DATACENTER then
              Edition:='Datacenter Server'
            else
            if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
              Edition:='Advanced Server'
            else
              Edition:='Server';
          end;
        end;{ else if OSVIX.dwMajorVersion=4 then begin
            Product:='Windows NT 4';
            OS:=osNT4;
            if OSVIX.wProductType=VER_NT_WORKSTATION then
              Edition:='Workstation'
            else begin
              if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
                Edition:='Server 4.0, Enterprise'
              else
                Edition:='Server 4.0';
            end;

            s:='';
            with OpenRegistryReadOnly do
              try
                Rootkey:=HKEY_LOCAL_MACHINE;
                if OpenKey('SYSTEM\CurrentControlSet\Control\ProductOptions',False) then begin
                  if ValueExists('ProductType') then
                    s:=ReadString('ProductType');
                  CloseKey;
                  if SameText(s,'WINNT') then
                       Edition:='Workstation'
                   else
                   if SameText(s,'LANMANNT') then
                     Edition:='Server'
                   else
                   if SameText(s,'SERVERNT') then
                     Edition:='Advanced Server';
                 end;

                 if OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009',False) then begin
                   if ValueExists('Installed') then begin
                     case GetdataType('Installed') of
                       rdString: k:=StrToIntDef(ReadString('Installed'),0);
                       rdInteger: k:=ReadInteger('Installed');
                       else k:=0;
                     end;
                     if (k=1) and SameText(OSVIX.szCSDVersion,'Service Pack 6') then
                       OSVIX.szCSDVersion:='Service Pack 6a';
                   end;
                 end;
               finally
                 Free;
               end;
             end;
           end;
        end;
    VER_PLATFORM_WIN32_WINDOWS: begin
      if (OSVIX.dwMajorVersion=4) and (OSVIX.dwMinorVersion=0) then begin
        Product:='Windows 95';
        OS:=os95;
        case OSVIX.szCSDVersion[1] of
          'A': Edition:='OSR 1';
          'B': Edition:='OSR 2';
          'C': Edition:='OSR 2.5';
        end;
      end else
      if (OSVIX.dwMajorVersion=4) and (OSVIX.dwMinorVersion=10) then begin
        Product:='Windows 98';
        OS:=os98;
        if (OSVIX.szCSDVersion[1]='A') or (OSVIX.szCSDVersion[1]='B') then
          Edition:='SE';
      end else
      if (OSVIX.dwMajorVersion=4) and (OSVIX.dwMinorVersion=90) then begin
        Product:='Windows Millennium Edition';
        OS:=osME;
      end;
    end;}
  end;

  case OSVIX.wProductType of
    VER_NT_WORKSTATION       : ProductType:=ptWorkStation;
    VER_NT_DOMAIN_CONTROLLER : ProductType:=ptAdvancedServer;
    VER_NT_SERVER            : ProductType:=ptServer;
  end;

  if (OSVIX.dwMajorVersion>=5) and (OSVIX.wProductType=VER_NT_DOMAIN_CONTROLLER) then
    ProductType:=ptServer;

  if OSVIX.wSuiteMask and VER_SUITE_SMALLBUSINESS<>0 then
    InstalledSuites:=InstalledSuites+[suSmallBusiness];

  if OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE<>0 then begin
    InstalledSuites:=InstalledSuites+[suEnterprise];
    ProductType:=ptAdvancedServer;
  end;

  if OSVIX.wSuiteMask and VER_SUITE_BACKOFFICE<>0 then
    InstalledSuites:=InstalledSuites+[suBackOffice];

  if OSVIX.wSuiteMask and VER_SUITE_COMMUNICATIONS<>0 then
    InstalledSuites:=InstalledSuites+[suCommunications];

  if OSVIX.wSuiteMask and VER_SUITE_TERMINAL<>0 then
    InstalledSuites:=InstalledSuites+[suTerminal];

  if OSVIX.wSuiteMask and VER_SUITE_SMALLBUSINESS_RESTRICTED<>0 then
    InstalledSuites:=InstalledSuites+[suSmallBusinessRestricted];

  if OSVIX.wSuiteMask and VER_SUITE_EMBEDDEDNT<>0 then
    InstalledSuites:=InstalledSuites+[suEmbeddedNT];

  if OSVIX.wSuiteMask and VER_SUITE_DATACENTER<>0 then begin
    InstalledSuites:=InstalledSuites+[suDataCenter];
    ProductType:=ptDataCenter;
  end;

  if OSVIX.wSuiteMask and VER_SUITE_SINGLEUSERTS<>0 then
    InstalledSuites:=InstalledSuites+[suSingleUserTS];

  if OSVIX.wSuiteMask and VER_SUITE_PERSONAL<>0 then
    InstalledSuites:=InstalledSuites+[suPersonal];

  if OSVIX.wSuiteMask and VER_SUITE_BLADE<>0 then begin
    InstalledSuites:=InstalledSuites+[suBlade];
    ProductType:=ptWeb;
  end;

  if OSVIX.wSuiteMask and VER_SUITE_EMBEDDED_RESTRICTED<>0 then
    InstalledSuites:=InstalledSuites+[suEmbeddedRestricted];

  Edition:=Trim(Edition);

  Is2K:=OS=os2K;
  IsXP:=OS=osXP;
  Is2K3:=OS=os2K3;
  IsVista:=OS=osVista;
  IsSeven:=OS=osSeven;
  IsWin8:=OS=osWin8;
  Is2K12:=OS=os2K12;
  IsBlue:=OS=osBlue;
  IsWin10:=OS=osWin10;

  if IsWinPE then
    Product:='WinPE '+GetWinPEVersion;

  Result:=Trim(Format('%s %s',[Product,Edition]));
end;

function GetOSName(ABuild: Cardinal): string;
begin
  Result:='';
  if ABuild=0 then
    Result:=''
  else if ABuild<500 then
    Result:=Format('Non-Windows (%d)',[ABuild])
  else if ABuild=528 then
    Result:='Windows NT 3.1'
  else if ABuild=807 then
    Result:='Windows NT 3.5'
  else if ABuild=1357 then
    Result:='Windows NT 3.51'
  else if ABuild=1381 then
    Result:='Windows NT 4'
  else if ABuild<2505 then
    Result:='Windows 2000'
  else if ABuild=2600 then
    Result:='Windows XP'
  else if ABuild<5048 then
    Result:='Windows Server 2003'
  else if (ABuild=6000) then
    Result:='Windows Vista'
  else if ABuild=6001 then
    Result:='Windows Vista SP 1 / Windows Server 2008'
  else if ABuild=6002 then
    Result:='Windows Vista SP 2'
  else if ABuild<7600 then
    Result:='Windows Server 2008'
  else if ABuild=7600 then
    Result:='Windows 7'
  else if (ABuild=7601) then
    Result:='Windows 7 SP 1'
  else if (ABuild=8400) then
    Result:='Windows Home Server 2011'
  else if (ABuild=9200) then
    Result:='Windows 8 / Windows Server 2012'
  else if ABuild=9600 then
    Result:='Windows 8.1'
  else if ABuild<14393 then
    Result:='Windows 10'
  else if ABuild>=14393 then
    Result:='Windows 10 / Windows Server 2016'
  else if ABuild>=17763 then
    Result:='Windows 10 / Windows Server 2019';
end;

function GetOSBuild: string;
const
  rvUBR = 'UBR';
begin
  Result:=IntToStr(OSVIX.dwBuildNumber);
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion',False) then begin
        if ValueExists(rvUBR) then
          Result:=Result+'.'+IntToStr(ReadInteger(rvUBR));
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function IsGenuine: SL_GENUINE_STATE;
var
  pAppId: SLID;
  pGenuineState: SL_GENUINE_STATE;
  Status: HRESULT;
begin
  Result:=SL_GENUINE_STATE(-1);
  if (Win32MajorVersion>=6) and Assigned(SLIsGenuineLocal) then begin
    pAppId:=StringToGUID('{55C92734-D682-4D71-983E-D6EC3F16059F}');
    Status:=SLIsGenuineLocal(pAppId,pGenuineState,nil);
    if Succeeded(Status) then
      Result:=pGenuineState;
  end;
end;

function GetBuildLab: string;
const
  rvBuildLab = 'BuildLab';
  rvBuildLabEx = 'BuildLabEx';
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion',False) then begin
        if ValueExists(rvBuildLab) then
          Result:=ReadString(rvBuildLab);
        if ValueExists(rvBuildLabEx) then
          Result:=ReadString(rvBuildLabEx);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetProductName(var AInstaType: string): string;
const
  rvInstallationType = 'InstallationType';
  rvProductName = 'ProductName';
begin
  Result:='';
  AInstaType:='';
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion',False) then begin
        if ValueExists(rvInstallationType) then
          AInstaType:=ReadString(rvInstallationType);
        if ValueExists(rvProductName) then
          Result:=ReadString(rvProductName);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetTrueWindowsVersion: string;
var
  vi: TVersionInfo;
begin
  Result:='';
  GetFileVerInfo(FileSearch('kernel32.dll',GetWinSysDir),vi);
  Result:=vi.ProductVersion;
end;

function GetTrueWindowsName: string;
var
  vi: TVersionInfo;
begin
  if IsWinPE then begin
    Result:='WinPE '+GetWinPEVersion;
    Exit;
  end;
  GetFileVerInfo(FileSearch('kernel32.dll',GetWinSysDir),vi);
  case vi.ProductMajor of
    10: Result:='Windows 10';
    6: case vi.ProductMinor of
      0: if OSVIX.wProductType in [0,VER_NT_WORKSTATION] then
           Result:='Windows Vista'
         else
           Result:='Windows Server 2008';
      1: if OSVIX.wProductType in [0,VER_NT_WORKSTATION] then
           Result:='Windows 7'
         else
           Result:='Windows Server 2008 R2';
      2: if OSVIX.wProductType in [0,VER_NT_WORKSTATION] then
           Result:='Windows 8'
         else
           Result:='Windows Server 2012';
      3: if OSVIX.wProductType in [0,VER_NT_WORKSTATION] then
           Result:='Windows 8.1'
         else
           Result:='Windows Server 2012 R2';  //?
    end;
    5: case vi.ProductMinor of
      0: begin
         Result:='Windows 2000';
         if OSVIX.wProductType in [0,VER_NT_WORKSTATION] then
            Result:=Result+' Professional'
          else if (OSVIX.wSuiteMask and VER_SUITE_DATACENTER)=VER_SUITE_DATACENTER then
              Result:=Result+' Datacenter Server'
            else if (OSVIX.wSuiteMask and VER_SUITE_ENTERPRISE)=VER_SUITE_ENTERPRISE then
              Result:=Result+' Advanced Server'
            else
              Result:=Result+' Server';
      end;
      1: begin
         Result:='Windows XP';
         if (OSVIX.wSuiteMask and VER_SUITE_PERSONAL)=VER_SUITE_PERSONAL then
            Result:=Result+' Home'
          else
            Result:=Result+' Professional';
          if (GetSystemMetrics(SM_STARTER)>0) then
            Result:=Result+' Starter'
          else if (GetSystemMetrics(SM_TABLETPC)>0) then
            Result:=Result+' Tablet PC'
          else if (GetSystemMetrics(SM_MEDIACENTER)>0) then
            Result:=Result+' Media Center';
      end;
      2: begin
         if (OSVIX.wProductType in [0,VER_NT_WORKSTATION]) and (SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64) then
           Result:='Windows XP Professional'
         else if (GetSystemMetrics(SM_SERVERR2)>0) then
           Result:='Windows Server 2003 R2'
         else if (OSVIX.wSuiteMask and VER_SUITE_STORAGE_SERVER)=VER_SUITE_STORAGE_SERVER then
           Result:='Windows Storage Server 2003'
         else if (OSVIX.wSuiteMask and VER_SUITE_WH_SERVER)=VER_SUITE_WH_SERVER then
           Result:='Windows Home Server'
         else
           Result:='Windows Server 2003';
      end;
    end;
  end;

  if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64 then
    Result:=Result+' x64';
end;

function GetFileVerInfo(const AFilename :string; out AData: TVersionInfo): Boolean;
var
  t: string;
  pd: PDWORD;
  pc: PChar;
  Handle: Cardinal;
  Len,Size: Cardinal;
  buf: PChar;
  FixedFileInfo :PVSFixedFileInfo;
begin
  Result:=False;
  AData.FileName:=AFilename;
  Size:=GetFileVersionInfoSize(PChar(AFilename),Handle);
  if Size>0 then begin
    buf:=Allocmem(Size);
    try
      if GetFileVersionInfo(PChar(AFilename),Handle,Size,buf) then begin
        if VerQueryValue(buf,'\',Pointer(FixedFileInfo),Len) then begin
          AData.Major:=hiword(FixedFileInfo^.dwfileversionms);
          AData.Minor:=loword(FixedFileInfo^.dwfileversionms);
          AData.Release:=hiword(FixedFileInfo^.dwfileversionls);
          AData.Build:=loword(FixedFileInfo^.dwfileversionls);

          AData.ProductMajor:=hiword(FixedFileInfo^.dwProductVersionMS);
          AData.ProductMinor:=loword(FixedFileInfo^.dwProductVersionMS);
          AData.ProductRelease:=hiword(FixedFileInfo^.dwProductVersionLS);
          AData.ProductBuild:=loword(FixedFileInfo^.dwProductVersionLS);
          AData.PreReleaseBuild:=FixedFileInfo^.dwFileFlags and VS_FF_PRERELEASE=VS_FF_PRERELEASE;
          AData.DebugBuild:=FixedFileInfo^.dwFileFlags and VS_FF_DEBUG=VS_FF_DEBUG;
          AData.PrivateBuild:=FixedFileInfo^.dwFileFlags and VS_FF_PRIVATEBUILD=VS_FF_PRIVATEBUILD;

          if VerQueryValue(buf,PChar('\VarFileInfo\Translation'),Pointer(pd),Len) then
            t:=IntToHex(MakeLong(HiWord(Longint(pd^)),LoWord(Longint(pd^))), 8)
          else
            t:='040904E4';

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\ProductVersion'),Pointer(pc),Len) then
            AData.ProductVersionText:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\FileVersion'),Pointer(pc),Len) then
            AData.FileVersionText:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\FileDescription'),Pointer(pc),Len) then
            AData.description:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\LegalCopyright'),Pointer(pc),Len) then
            AData.Copyright:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\LegalTrademarks'),Pointer(pc),Len) then
            AData.TradeMarks:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\Comments'),Pointer(pc),Len) then
            AData.Comments:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\SpecialBuild'),Pointer(pc),Len) then
            AData.SpecialBuild:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\ProductName'),Pointer(pc),Len) then
            AData.ProductName:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\CompanyName'),Pointer(pc),Len) then
            AData.CompanyName:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\InternalName'),Pointer(pc),Len) then
            AData.InternalName:=string(pc);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\OriginalFileName'),Pointer(pc),Len) then
            AData.OriginalFilename:=string(pc);

          AData.ProductVersion:=Format('%u.%u.%u',[AData.ProductMajor,AData.ProductMinor,AData.ProductRelease]);
          AData.FileVersion:=Format('%u.%u.%u.%u',[AData.Major,AData.Minor,AData.Release,AData.Build]);

          if VerQueryValue(buf,PChar('\StringFileInfo\'+t+'\BuildTimestamp'),Pointer(pc),Len) then
            AData.BuildTimestamp:=string(pc);

          if AData.PreReleaseBuild then begin
            if AData.Build>0 then
              AData.FileVersion:=Format('%s BETA %d',[AData.FileVersion,AData.Build])
            else
              AData.FileVersion:=Format('%s BETA',[AData.FileVersion])
          end;

          Result:=True;
        end;
      end;
    finally
      FreeMem(buf);
    end;
  end;
end;


function GetFileVersion(const fn: string): string;
var
  VIR: TVersionInfo;
begin
  if GetFileVerInfo(fn,VIR) then
    Result:=VIR.FileVersion
  else
    Result:='';
end;

function GetFileCopyright(const fn: string): string;
var
  VIR: TVersionInfo;
begin
  if GetFileVerInfo(fn,VIR) then
    Result:=VIR.Copyright
  else
    Result:='';
end;

function GetFileProduct(const fn: string): string;
var
  VIR: TVersionInfo;
begin
  if GetFileVerInfo(fn,VIR) then
    Result:=VIR.ProductName
  else
    Result:='';
end;

function GetFileDesc(const fn: string): string;
var
  VIR: TVersionInfo;
begin
  if GetFileVerInfo(fn,VIR) then
    Result:=VIR.Description
  else
    Result:='';
end;

function GetFileOwner(AFilename: string): string;
{var
  SD: TSecurityDescriptor;
  sdLen, nsdLen : DWORD;}
begin
  {sdLen:=4096;
  ReallocMem (SD.fpSD, sdLen);
  try
    if not GetFileSecurity (PAnsiChar (AFileName), OWNER_SECURITY_INFORMATION, SD.fpSD, sdLen, nsdLen) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        RaiseLastOSError;

    if nsdLen > sdLen then
    begin
      Reallocmem (SD.fpsd, nsdLen);
      Win32Check (GetFileSecurity (PAnsiChar (FileName), securityInfo, SD.fpSD, nsdLen, nsdLen))
    end
    else
      ReallocMem (SD.fpSD, nsdLen)
  except
    ReallocMem (SD.fpSD, 0);
    raise
  end;

  if Assigned (SD.fpSD) then
  begin
    SD.fSDLen:=nsdlen;
    SD.fInfoFlags:=securityInfo
  end
  else
    SD.fInfoFlags:=0;}
end;

function GetMachine :string;
var
  n :Cardinal;
  buf :PChar;
const
  rkMachine = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName';
    rvMachine = 'ComputerName';
begin
  n:=255;
  buf:=stralloc(n);
  GetComputerName(buf,n);
  result:=buf;
  strdispose(buf);
  {with OpenRegistryReadOnly do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkMachine,False) then begin
      if ValueExists(rvMachine) then
        result:=ReadString(rvMachine);
      closekey;
    end;
    free;
  end;}
end;

function GetUser :string;
var
  n :Cardinal;
  buf :PChar;
begin
  n:=255;
  buf:=stralloc(n);
  if GetUserName(buf,n) then
    result:=strpas(buf)
  else
    Result:='';
  strdispose(buf);
end;

function GetWindowsLiveID: string;
var
  sl: TStringList;
begin
  Result:='';
  sl:=TStringList.Create;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKeyReadOnly('\SOFTWARE\Microsoft\IdentityCRL\UserExtendedProperties') then begin
        GetKeyNames(sl);
        if sl.Count>0 then
          Result:=sl[0];
      end;
    finally
      Free;
      sl.Free;
    end;
end;

procedure GetEnvironment(EnvList :tstrings);
var
  i :Cardinal;
  b :PChar;
  s :string;
begin
  EnvList.Clear;
  b:=GetEnvironmentStrings;
  i:=0;
  s:='';
  while (b[i]<>#0) or (b[i-1]<>#0) do begin
    if b[i]<>#0 then
      s:=s+b[i]
    else begin
      if s='' then
        break;
      if Pos('=',s)<>1 then
        EnvList.Add(Trim(s));
      s:='';
    end;
    inc(i);
  end;
  FreeEnvironmentStrings(b);
end;

function GetWinSysDir: string;
var
  n: integer;
  p: PChar;
begin
  n:=MAX_PATH;
  p:=stralloc(n);
  getwindowsdirectory(p,n);
  result:=string(p)+';';
  getsystemdirectory(p,n);
  Result:=Result+string(p)+';';
  StrDispose(p);
end;

procedure GetRecycleBin(AList: TStrings);

procedure ScanPath(APath: string);

function IsRecycleBin(APath: string): boolean;
var
  fn: string;
begin
  Result:=false;
  fn:=IncludeTrailingPathDelimiter(Apath)+'desktop.ini';
  if not FileExists(fn) then
    Exit;
  with TINIFile.Create(fn) do
    try
      Result:=SameText(ReadString('.ShellClassInfo','CLSID',''),'{645FF040-5081-101B-9F08-00AA002F954E}');
    finally
      Free;
    end;
end;

var
  SR: TSearchRec;
begin
  APath:=IncludeTrailingPathDelimiter(APath);
  try
    if FindFirst(APath+'*.*',faDirectory or faSysFile or faHidden,SR)=0 then
      repeat
        if ((SR.Attr and (faDirectory or faSysFile or faHidden))>=faDirectory or faSysFile or faHidden) and not SameText(SR.Name,'.') and not SameText(SR.Name,'..') then begin
          if IsRecycleBin(APath+SR.Name) then
            AList.Add(APath+SR.Name)
          else
            ScanPath(APath+SR.Name);
        end;
      until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end;
end;

var
  ad: string;
  i: Integer;
begin
  AList.Clear;
  ad:=GetAvailDisks;
  for i:=1 to Length(ad) do
    if GetMediaPresent(Copy(ad,i,1)+':') then
      ScanPath(Copy(ad,i,1)+':');
end;

function GetSpecialFolder(Handle: Hwnd; nFolder: Integer): string;
var
  PIDL: PItemIDList;
  Path: {$IFDEF RAD6PLUS}PWideChar;{$ELSE}LPSTR;{$ENDIF}
  Malloc: IMalloc;
begin
  Result:='';
  Path:=StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, nFolder, PIDL);

  if SHGetPathFromIDList(PIDL, Path) then
    Result:=Path;

  StrDispose(Path);

  if Succeeded(SHGetMalloc(Malloc)) then
    Malloc.Free( PIDL );
end;

function GetKnownFolderPath(AKnownFolderID: TGUID): string;
var
  p: PWideChar;
  r: integer;
begin
  Result:='';
  {$IFNDEF RAD7PLUS}
  if not Assigned(SHGetKnownFolderPath) then
    Exit;
  {$ENDIF}
  r:=SHGetKnownFolderPath(AKnownFolderID,0,0,p);
  if r=S_OK then begin
    Result:={$IFNDEF UNICODE}WideToAnsi{$ENDIF}(p);
    CoTaskmemFree(p);
  end;
end;

function GetSpecialFolderEx(AUser: string; ACSIDL: integer): string;
var
  s,sp,pp,cp: string;
begin
  if AUser='' then
    AUser:=WindowsUser;
  if not SameText(AUser,WindowsUser) then begin
    pp:=IncludeTrailingPathDelimiter(GetProfilePath);
    sp:=GetSpecialFolder(0,ACSIDL);
    if PosText('\'+AUser+'\',sp)>0 then
      Result:=sp
    else
      if PosText('LocalService',sp)>0 then
        Result:=StringReplace(sp,'LocalService',ExtractFilename(ExcludeTrailingPathDelimiter(GetProfilePath(AUser))),[rfIgnorecase])
      else if PosText(pp,sp)>0 then begin
        s:=StringReplace(sp,pp,'',[rfIgnorecase]);
        Result:=IncludeTrailingPathDelimiter(GetProfilePath(AUser))+s;
      end else begin
        pp:=IncludeTrailingPathDelimiter(GetProfilePath(AUser));
        cp:=ExtractFilePath(ExcludeTrailingPathDelimiter(GetSpecialFolder(0,CSIDL_APPDATA)));
        Result:=StringReplace(sp,cp,pp,[rfIgnorecase]);
      end;
  end else
    Result:=GetSpecialFolder(0,ACSIDL);
  Result:=IncludeTrailingPathDelimiter(Result);
end;

procedure GetProfileList(AList: TStrings; AExpand: Boolean = True);
const
  rkPF = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\';
  rvPIP = 'ProfileImagePath';
var
  s,p: string;
  i: Integer;
  sl: TStringList;
  sid: Pointer;
begin
  AList.Clear;
  sl:=TStringList.create;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkPF,False)then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkPF+sl[i],False) then begin
            if ValueExists(rvPIP) then begin
              sid:=ConvertStringToSID(sl[i]);
              s:=GetAccountFromSID(sid);
              p:=ReadString(rvPIP);
              if s='' then
                s:=sl[i];
              if AExpand then
                p:=ExpandEnvVars(p,False);
              AList.Add(Format('%s=%s',[Uppercase(s),p]));
            end;
            CloseKey;
          end;
      end;
    finally
      Free;
      sl.Free;
    end;
end;

function GetProfilePath(AUser: string = ''): string;
const
  rkPF = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\%s';
  rvPIP = 'ProfileImagePath';
var
  s: string;
begin
  Result:='';
  if AUser='' then
    AUser:=WindowsUser;
  if Win32Platform=VER_PLATFORM_WIN32_NT then begin
    with OpenRegistryReadOnly do
      try
        RootKey:=HKEY_LOCAL_MACHINE;
        s:=Format(rkPF,[GetSIDFromAccount('',AUser)]);
        if OpenKey(s,False)then begin
          if ValueExists(rvPIP) then
            Result:=ExpandEnvVars(ReadString(rvPIP),False);
          CloseKey;
        end;
      finally
        Free;
      end;
  end;
  if Result='' then begin
    s:=GetSpecialFolder(GetDesktopWindow,CSIDL_DESKTOP);
    s:=ReverseString(s);
    Result:=ReverseString(Copy(s,Pos('\',s)+1,255));
    Result:=StringReplace(Result,'\'+WindowsUser,'\'+AUser,[rfIgnorecase]);
  end;
end;

function GetFolderDateTime(const strFolder: String): TDateTime;
var
  sr: TSearchRec;
begin
  if FindFirst(strFolder, faDirectory, sr) = 0 then
  try
    {$IFNDEF RAD5PLUS}
    Result:=FileDateToDateTime(sr.Time) ;
    {$ELSE}
    Result:=FileTimeToDatetime(sr.FindData.ftLastWriteTime);
    {$ENDIF}
  finally
    FindClose(sr);
  end
  else
  begin
    Result:=0;
  end;
end;

function GetMemoryLoad: Cardinal;
begin
  if Assigned(GlobalMemoryStatusEx_) then begin
    ResetMemory(MSEX,SizeOf(MSEX));
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Result:=MSEX.dwMemoryLoad;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Result:=MS.dwMemoryLoad;
  end;
end;


function GetWinText(AHandle: THandle; AClassNameIfEmpty: Boolean = False): string;
var
  len: {$IFDEF NATIVEINT}PDWORD_PTR{$ELSE}Cardinal{$ENDIF};
  cn: array[0..255] of char;
begin
  Result:='';
  {$IFDEF RAD9PLUS}new(len);{$ENDIF}
  if SendMessageTimeout(AHandle,WM_GETTEXTLENGTH,0,0,SMTO_ABORTIFHUNG,1000,len)=0 then
    Exit;
  if len{$IFDEF NATIVEINT}^{$ENDIF}<1 then
    Exit;
  SetLength(Result,len{$IFDEF NATIVEINT}^{$ENDIF});
  SendMessageTimeout(AHandle,WM_GETTEXT,len{$IFDEF NATIVEINT}^{$ENDIF}+1,Longint(PChar(Result)),SMTO_ABORTIFHUNG,1000,len);
  {$IFDEF NATIVEINT}dispose(len);{$ENDIF}

  if AClassNameIfEmpty and (Result='') then begin
    GetClassname(AHandle,cn,SizeOf(cn));
    Result:=cn;
  end;
end;

function GetWindowInfo(wh: hwnd; AStyles: Boolean = False): TWindowInfo;
var
  cn{,wn} :array[0..255] of Char;
  n,wpid,tid :Cardinal;
begin
  n:=256;
  tid:=GetWindowThreadProcessId(wh,@wpid);
  getclassname(wh,cn,n);
  //getwindowtext(wh,wn,n);
  Result.ClassName:=string(cn);
  Result.Text:=GetWinText(wh);//string(wn);
  Result.Handle:=wh;
  Result.Process:=wpid;
  Result.Thread:=tid;
  Result.Enabled:=IsWindowEnabled(wh);
  {$IFDEF RAD7PLUS}
  Result.ParentWin:=getwindowlongptr(wh,GWLP_HWNDPARENT);
  Result.WndProc:=getwindowlongptr(wh,GWLP_WNDPROC);
  Result.Instance:=getwindowlongptr(wh,GWLP_HINSTANCE);
  Result.ID:=getwindowlongptr(wh,GWLP_ID);
  Result.UserData:=getwindowlongptr(wh,GWLP_USERDATA);
  {$ELSE}
  Result.ParentWin:=getwindowlong(wh,GWL_HWNDPARENT);
  Result.WndProc:=getwindowlong(wh,GWL_WNDPROC);
  Result.Instance:=getwindowlong(wh,GWL_HINSTANCE);
  Result.ID:=getwindowlong(wh,GWL_ID);
  Result.UserData:=getwindowlong(wh,GWL_USERDATA);
  {$ENDIF}
  Result.Style:=getwindowlong(wh,GWL_STYLE);
  Result.ExStyle:=getwindowlong(wh,GWL_EXSTYLE);
  getwindowrect(wh,Result.Rect);
  getclientrect(wh,Result.ClientRect);
  Result.Atom:=getclasslong(wh,GCW_ATOM);
  Result.ClassBytes:=getclasslong(wh,GCL_CBCLSEXTRA);
  Result.WinBytes:=getclasslong(wh,GCL_CBWNDEXTRA);
  Result.ClassWndProc:=getclasslong(wh,GCL_WNDPROC);
  Result.ClassInstance:=getclasslong(wh,GCL_HMODULE);
  Result.Background:=getclasslong(wh,GCL_HBRBACKGROUND);
  Result.Cursor:=getclasslong(wh,GCL_HCURSOR);
  Result.Icon:=getclasslong(wh,GCL_HICON);
  Result.ClassStyle:=getclasslong(wh,GCL_STYLE);
  Result.Visible:=IsWindowVisible(wh);
  {$IFDEF RAD7PLUS}
  try GetWindowDisplayAffinity(wh,Result.WindowAffinity) except end;
  {$ENDIF}

  if AStyles then begin
    Result.Styles:=TStringList.Create;
    if not(Result.Style and WS_BORDER=0) then
      Result.Styles.add('WS_BORDER');
    if not(Result.Style and WS_CHILD=0) then
      Result.Styles.add('WS_CHILD');
    if not(Result.Style and WS_CLIPCHILDREN=0) then
      Result.Styles.add('WS_CLIPCHILDREN');
    if not(Result.Style and WS_CLIPSIBLINGS=0) then
      Result.Styles.add('WS_CLIPSIBLINGS');
    if not(Result.Style and WS_DISABLED=0) then
      Result.Styles.add('WS_DISABLED');
    if not(Result.Style and WS_DLGFRAME=0) then
      Result.Styles.add('WS_DLGFRAME');
    if not(Result.Style and WS_THICKFRAME=0) then
      Result.Styles.add('WS_THICKFRAME');
    if not(Result.Style and WS_GROUP=0) then
      Result.Styles.add('WS_GROUP');
    if not(Result.Style and WS_HSCROLL=0) then
      Result.Styles.add('WS_HSCROLL');
    if not(Result.Style and WS_MAXIMIZE=0) then
      Result.Styles.add('WS_MAXIMIZE');
    if not(Result.Style and WS_MAXIMIZEBOX=0) then
      Result.Styles.add('WS_MAXIMIZEBOX');
    if not(Result.Style and WS_MINIMIZE=0) then
      Result.Styles.add('WS_MINIMIZE');
    if not(Result.Style and WS_MINIMIZEBOX=0) then
      Result.Styles.add('WS_MINIMIZEBOX');
    if not(Result.Style and WS_SIZEBOX=0) then
      Result.Styles.add('WS_SIZEBOX');
    if not(Result.Style and WS_ICONIC=0) then
      Result.Styles.add('WS_ICONIC');
    if not(Result.Style and WS_TILED=0) then
      Result.Styles.add('WS_TILED');
    if not(Result.Style and WS_OVERLAPPED=0) then
      Result.Styles.add('WS_OVERLAPPED');
    if not(Result.Style and WS_POPUP=0) then
      Result.Styles.add('WS_POPUP');
    if not(Result.Style and WS_SYSMENU=0) then
      Result.Styles.add('WS_SYSMENU');
    if not(Result.Style and WS_TABSTOP=0) then
      Result.Styles.add('WS_TABSTOP');
    if not(Result.Style and WS_THICKFRAME=0) then
      Result.Styles.add('WS_THICKFRAME');
    if not(Result.Style and WS_VISIBLE=0) then
      Result.Styles.add('WS_VISIBLE');
    if not(Result.Style and WS_VSCROLL=0) then
      Result.Styles.add('WS_VSCROLL');
    if not(Result.Style and WS_TABSTOP=0) then
      Result.Styles.add('WS_TABSTOP');
    if not(Result.Style and WS_GROUP=0) then
      Result.Styles.add('WS_GROUP');


    Result.ExStyles:=TStringList.Create;
    if Result.ExStyle>0 then begin
      if not(Result.ExStyle and WS_EX_ACCEPTFILES=0) then
        Result.ExStyles.add('WS_EX_ACCEPTFILES');
      if not(Result.ExStyle and WS_EX_DLGMODALFRAME=0) then
        Result.ExStyles.add('WS_EX_DLGMODALFRAME');
      if not(Result.ExStyle and WS_EX_NOPARENTNOTIFY=0) then
        Result.ExStyles.add('WS_EX_NOPARENTNOTIFY');
      if not(Result.ExStyle and WS_EX_TOPMOST=0) then
        Result.ExStyles.add('WS_EX_TOPMOST');
      if not(Result.ExStyle and WS_EX_TRANSPARENT=0) then
        Result.ExStyles.add('WS_EX_TRANSPARENT');
      if not(Result.ExStyle and WS_EX_MDICHILD=0) then
        Result.ExStyles.add('WS_EX_MDICHILD');
      if not(Result.ExStyle and WS_EX_TOOLWINDOW=0) then
        Result.ExStyles.add('WS_EX_TOOLWINDOW');
      if not(Result.ExStyle and WS_EX_WINDOWEDGE=0) then
        Result.ExStyles.add('WS_EX_WINDOWEDGE');
      if not(Result.ExStyle and WS_EX_CLIENTEDGE =0) then
        Result.ExStyles.add('WS_EX_CLIENTEDGE');
      if not(Result.ExStyle and WS_EX_CONTEXTHELP=0) then
        Result.ExStyles.add('WS_EX_CONTEXTHELP');
      if not(Result.ExStyle and WS_EX_RIGHT=0) then
        Result.ExStyles.add('WS_EX_RIGHT')
      else
        Result.ExStyles.add('WS_EX_LEFT');
      if not(Result.ExStyle and WS_EX_RTLREADING=0) then
        Result.ExStyles.add('WS_EX_RTLREADING')
      else
        Result.ExStyles.add('WS_EX_LTRREADING');
      if not(Result.ExStyle and WS_EX_LEFTSCROLLBAR=0) then
        Result.ExStyles.add('WS_EX_LEFTSCROLLBAR')
      else
        Result.ExStyles.add('WS_EX_RIGHTSCROLLBAR');
      if not(Result.ExStyle and WS_EX_CONTROLPARENT=0) then
        Result.ExStyles.add('WS_EX_CONTROLPARENT');
      if not(Result.ExStyle and WS_EX_STATICEDGE =0) then
        Result.ExStyles.add('WS_EX_STATICEDGE');
      if not(Result.ExStyle and WS_EX_APPWINDOW=0) then
        Result.ExStyles.add('WS_EX_APPWINDOW');
      if not(Result.ExStyle and WS_EX_LAYERED=0) then
        Result.ExStyles.add('WS_EX_LAYERED');
      if not(Result.ExStyle and WS_EX_NOINHERITLAYOUT=0) then
        Result.ExStyles.add('WS_EX_NOINHERITLAYOUT');
      if not(Result.ExStyle and WS_EX_LAYOUTRTL=0) then
        Result.ExStyles.add('WS_EX_LAYOUTRTL');
      if not(Result.ExStyle and WS_EX_COMPOSITED=0) then
        Result.ExStyles.add('WS_EX_COMPOSITED');
      if not(Result.ExStyle and WS_EX_NOACTIVATE=0) then
        Result.ExStyles.add('WS_EX_NOACTIVATE');
    end;

    Result.ClassStyles:=TStringList.Create;
    if not(Result.ClassStyle and CS_BYTEALIGNCLIENT=0) then
      Result.ClassStyles.add('CS_BYTEALIGNCLIENT');
    if not(Result.ClassStyle and CS_VREDRAW=0) then
      Result.ClassStyles.add('CS_VREDRAW');
    if not(Result.ClassStyle and CS_HREDRAW=0) then
      Result.ClassStyles.add('CS_HREDRAW');
    if not(Result.ClassStyle and CS_KEYCVTWINDOW=0) then
      Result.ClassStyles.add('CS_KEYCVTWINDOW');
    if not(Result.ClassStyle and CS_DBLCLKS=0) then
      Result.ClassStyles.add('CS_DBLCLKS');
    if not(Result.ClassStyle and CS_OWNDC=0) then
      Result.ClassStyles.add('CS_OWNDC');
    if not(Result.ClassStyle and CS_CLASSDC=0) then
      Result.ClassStyles.add('CS_CLASSDC');
    if not(Result.ClassStyle and CS_PARENTDC=0) then
      Result.ClassStyles.add('CS_PARENTDC');
    if not(Result.ClassStyle and CS_NOKEYCVT=0) then
      Result.ClassStyles.add('CS_NOKEYCVT');
    if not(Result.ClassStyle and CS_NOCLOSE=0) then
      Result.ClassStyles.add('CS_NOCLOSE');
    if not(Result.ClassStyle and CS_SAVEBITS=0) then
      Result.ClassStyles.add('CS_SAVEBITS');
    if not(Result.ClassStyle and CS_BYTEALIGNWINDOW=0) then
      Result.ClassStyles.add('CS_BYTEALIGNWINDOW');
    if not(Result.ClassStyle and CS_GLOBALCLASS=0) then
      Result.ClassStyles.add('CS_GLOBALCLASS');
    if not(Result.ClassStyle and CS_IME=0) then
      Result.ClassStyles.add('CS_IME');
    if not(Result.ClassStyle and CS_DROPSHADOW=0) then
      Result.ClassStyles.add('CS_DROPSHADOW');
  end;
end;

function FindWindowByTitle(AHandle: THandle; const ClassName,WindowTitle: string): Hwnd;
var
  NextHandle: Hwnd;
  NextTitle,Buffer: array[0..260] of char;
begin
  Result:=0;
  NextHandle:=GetWindow(AHandle,GW_HWNDFIRST);
  while NextHandle>0 do  begin
    GetWindowText(NextHandle, NextTitle,255);
    GetClassName(NextHandle,Buffer,255);
    if SameText(Classname,Buffer) and (Pos(WindowTitle,NextTitle)<>0) then begin
      Result:=NextHandle;
      Break;
    end else
      NextHandle:=GetWindow(NextHandle,GW_HWNDNEXT);
  end;
end;

function ForceForegroundWindow(AHandle: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(AHandle) then
    ShowWindow(AHandle,SW_RESTORE);
  if GetForegroundWindow=AHandle then
    Result:=True
  else begin
    if (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>4) then begin
      Result:=False;
      ForegroundThreadID:=GetWindowThreadProcessID(GetForegroundWindow,nil);
      ThisThreadID:=GetWindowThreadPRocessId(AHandle,nil);
      if AttachThreadInput(ThisThreadID,ForegroundThreadID,True) then begin
        BringWindowToTop(AHandle);
        SetForegroundWindow(AHandle);
        AttachThreadInput(ThisThreadID,ForegroundThreadID,False);
        Result:=(GetForegroundWindow=AHandle);
      end;
      if not Result then begin
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT,0,@timeout,0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT,0,TObject(0),SPIF_SENDCHANGE);
        BringWindowToTop(AHandle);
        SetForegroundWindow(AHandle);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT,0,TObject(timeout),SPIF_SENDCHANGE);
      end;
    end;
    Result:=(GetForegroundWindow=AHandle);
  end;
end;

function GetMediaTypeStr(MT: TMediaType): string;
begin
  case MT of
    dtUnknown     :result:='<unknown>';
    dtNotExists   :result:='<not exists>';
    dtRemovable   :result:='Removable';
    dtFixed       :result:='Fixed';
    dtRemote      :result:='Remote';
    dtCDROM       :result:='CDROM';
    dtRAMDisk     :result:='RAM';
  end;
end;

function GetMediaPresent(const Value: string) : Boolean;
var
  Root :string;
  em,a,b,c,d :Cardinal;
begin
  Root:=Value+'\';
  em:=SetErrorMode(SEM_FailCriticalErrors or SEM_NoOpenFileErrorBox);
  try
    try
      result:=GetDiskFreeSpace(PChar(Root),a,b,c,d);
    except
      result:=False;
    end;
  finally
    SetErrorMode(em);
  end;
end;


function GetMediaIcon(Value: string) : THandle;
var
  em: Cardinal;
  i :Word;
begin
  em:=SetErrorMode(SEM_FailCriticalErrors or SEM_NoOpenFileErrorBox);
  try
    try
      Result:=ExtractAssociatedIcon(HInstance,PChar(string(Value)+'\'),{$IFDEF FPC}@{$ENDIF}i);
    except
      Result:=0;
    end;
  finally
    SetErrorMode(em);
  end;
end;


function GetDiskInfo(Value: string): TDiskInfo;
var
  BPS,TC,FC,SPC :integer;
  T,F :TLargeInteger;
  TF :PLargeInteger;
  bufRoot, bufVolumeLabel, bufFileSystem :PChar;
  MCL,Size,Flags :Cardinal;
  s :string;
begin
  with Result do begin
    Sign:=Value;
    Size:=255;
    bufRoot:=StrAlloc(Size);
    bufVolumeLabel:=StrAlloc(Size);
    bufFileSystem:=StrAlloc(Size);
    strpcopy(bufRoot,IncludeTrailingPathDelimiter(Value));
    try
      FileFlags:=[];
      case GetDriveType(bufRoot) of
        DRIVE_UNKNOWN     :MediaType:=dtUnknown;
        DRIVE_NO_ROOT_DIR :MediaType:=dtNotExists;
        DRIVE_REMOVABLE   :MediaType:=dtRemovable;
        DRIVE_FIXED       :MediaType:=dtFixed;
        DRIVE_REMOTE      :MediaType:=dtRemote;
        DRIVE_CDROM       :MediaType:=dtCDROM;
        DRIVE_RAMDISK     :MediaType:=dtRAMDisk;
      end;
      if GetMediaPresent(Value) then begin
        GetDiskFreeSpace(bufRoot,SectorsPerCluster,BytesPerSector,FreeClusters,TotalClusters);
        try
          new(TF);
          GetDiskFreeSpaceEx(bufRoot,F,T,TF);
          Capacity:=T;
          FreeSpace:=F;
          dispose(TF);
        except
          BPS:=BytesPerSector;
          TC:=TotalClusters;
          FC:=FreeClusters;
          SPC:=SectorsPerCluster;
          Capacity:=TC*SPC*BPS;
          FreeSpace:=FC*SPC*BPS;
        end;
        if GetVolumeInformation(bufRoot,bufVolumeLabel,Size,@Serial,MCL,Flags,bufFileSystem,Size) then begin;
          VolumeLabel:=string(bufVolumeLabel);
          FileSystem:=string(bufFileSystem);
          s:=IntToHex(Serial,8);
          SerialNumber:=copy(s,1,4)+'-'+copy(s,5,4);
          if Flags and FS_CASE_SENSITIVE=FS_CASE_SENSITIVE then
            FileFlags:=FileFlags+[fsCaseSensitive];
          if Flags and FS_CASE_IS_PRESERVED=FS_CASE_IS_PRESERVED then
            FileFlags:=FileFlags+[fsCaseIsPreserved];
          if Flags and FS_UNICODE_STORED_ON_DISK=FS_UNICODE_STORED_ON_DISK then
            FileFlags:=FileFlags+[fsUnicodeStoredOnDisk];
          if Flags and FS_PERSISTENT_ACLS=FS_PERSISTENT_ACLS then
            FileFlags:=FileFlags+[fsPersistentAcls];
          if Flags and FS_VOL_IS_COMPRESSED=FS_VOL_IS_COMPRESSED then
            FileFlags:=FileFlags+[fsVolumeIsCompressed];
          if Flags and FS_FILE_COMPRESSION=FS_FILE_COMPRESSION then
            FileFlags:=FileFlags+[fsFileCompression];
          if MCL=255 then
            FileFlags:=FileFlags+[fsLongFileNames];
          if Flags and FILE_SUPPORTS_ENCRYPTION=FILE_SUPPORTS_ENCRYPTION then
            FileFlags:=FileFlags+[fsEncryptedFileSystemSupport];
          if Flags and FILE_SUPPORTS_OBJECT_IDS=FILE_SUPPORTS_OBJECT_IDS then
            FileFlags:=FileFlags+[fsObjectIDsSupport];
          if Flags and FILE_SUPPORTS_REPARSE_POINTS=FILE_SUPPORTS_REPARSE_POINTS then
            FileFlags:=FileFlags+[fsReparsePointsSupport];
          if Flags and FILE_SUPPORTS_SPARSE_FILES=FILE_SUPPORTS_SPARSE_FILES then
            FileFlags:=FileFlags+[fsSparseFilesSupport];
          if Flags and FILE_VOLUME_QUOTAS=FILE_VOLUME_QUOTAS then
            FileFlags:=FileFlags+[fsDiskQuotasSupport];
        end;
      end else begin
        SectorsPerCluster:=0;
        BytesPerSector:=0;
        FreeClusters:=0;
        TotalClusters:=0;
        Capacity:=0;
        FreeSpace:=0;
        VolumeLabel:='';
        SerialNumber:='';
        FileSystem:='';
        Serial:=0;
      end;
    finally
      StrDispose(bufVolumeLabel);
      StrDispose(bufFileSystem);
      StrDispose(bufRoot);
    end;
  end;
end;

function GetWinDir :string;
var
  n :Cardinal;
  p :PChar;
begin
  n:=MAX_PATH;
  p:=StrAlloc(n);
  try
    GetWindowsDirectory(p,n);
    Result:=string(p);
    if Result<>'' then
      Result:=IncludeTrailingPathDelimiter(Result);
  finally
    StrDispose(p);
  end;
end;

function GetTempDir :string;
var
  n :Cardinal;
  p :PChar;
begin
  n:=MAX_PATH;
  p:=StrAlloc(n);
  try
    GetTempPath(n,p);
    Result:=string(p);
    if Result<>'' then
      Result:=IncludeTrailingPathDelimiter(Result);
  finally
    StrDispose(p);
  end;
end;

function GetSysDir :string;
var
  n :Cardinal;
  p :PChar;
begin
  n:=MAX_PATH;
  p:=StrAlloc(n);
  try
    GetSystemDirectory(p,n);
    Result:=string(p);
    if Result<>'' then
      Result:=IncludeTrailingPathDelimiter(Result);
  finally
    StrDispose(p);
  end;
end;

function ExpandEnvVars(AEnvironment: TStrings; ASource: string): string; overload;
var
  i,p: integer;
  s: string;
begin
  for i:=0 to AEnvironment.Count-1 do begin
    if Trim(AEnvironment.Names[i])<>'' then begin
      s:='%'+AEnvironment.Names[i]+'%';
      p:=Pos(s,ASource);
      if p>0 then
        ASource:=Copy(ASource,1,p-1)+AEnvironment.Values[AEnvironment.names[i]]+Copy(ASource,p+Length(s),1024)
      else begin
        s:='\'+AEnvironment.Names[i];
        p:=Pos(s,ASource);
        if p>0 then
          ASource:=Copy(ASource,1,p-1)+AEnvironment.Values[AEnvironment.names[i]]+Copy(ASource,p+Length(s),1024);
      end;
    end;
  end;
  Result:=ASource;
end;

function GetEnvVarValue(Name: string): string;
var
  sl: TStrings;
begin
  sl:=TStringList.Create;
  GetEnvironment(sl);
  Result:=sl.Values[Name];
  sl.Free;
end;

function ExpandEnvVars(ASource: string; Extended: boolean = True): string; overload;
var
  i,p: integer;
  sl: TStrings;
  s,a: string;
begin
  a:=UpperCase(ASource);
  sl:=TStringList.Create;
  GetEnvironment(sl);
  for i:=0 to sl.Count-1 do begin
    if Trim(sl.Names[i])<>'' then begin
      s:='%'+UpperCase(sl.Names[i])+'%';
      p:=Pos(s,a);
      if p>0 then begin
        ASource:=Copy(ASource,1,p-1)+sl.Values[sl.names[i]]+Copy(ASource,p+Length(s),1024);
        if not FileExists(ASource) and (Pos(' (x86)',ASource)>0) then
          ASource:=StringReplace(ASource,' (x86)','',[rfIgnorecase]);
      end else if Extended then begin
        s:='\'+UpperCase(sl.Names[i]);
        p:=Pos(s,a);
        if p>0 then begin
          ASource:=Copy(ASource,1,p-1)+sl.Values[sl.names[i]]+Copy(ASource,p+Length(s),1024);
          if not FileExistsEx(ASource) and (Pos(' (x86)',ASource)>0) then
            ASource:=StringReplace(ASource,' (x86)','',[rfIgnorecase]);
        end;
      end;
    end;
  end;
  Result:=ASource;
  sl.Free;
end;

function GetAvailDisks: string;
var
  i,n :integer;
  buf :PChar;
  di: TDiskInfo;
begin
  buf:=stralloc(255);
  n:=GetLogicalDriveStrings(255,buf);
  result:='';
  for i:=0 to n do
    if buf[i]<>#0 then begin
      if (ord(buf[i]) in [$41..$5a]) or (ord(buf[i]) in [$61..$7a]) then
        result:=result+upcase(buf[i]);
    end else
      if buf[i+1]=#0 then
        break;
  strdispose(buf);
end;

procedure GetCDs(cds :tstrings);
var
  i :integer;
  root :PChar;
  s :string;
begin
  root:=stralloc(255);
  s:=getavaildisks;
  cds.clear;
  for i:=1 to length(s) do begin
    strpcopy(root,copy(s,i,1)+':\');
    if getdrivetype(root)=drive_cdrom then
      cds.add(copy(root,1,length(root)-1));
  end;
  strdispose(root);
end;

function kpEnumWindowsProc(Wnd: HWND; ProcessID: {$IFDEF FPC}LParam{$ELSE}Cardinal{$ENDIF}): {$IFDEF FPC}LongBool{$ELSE}Boolean{$ENDIF}; stdcall;
var
  PID: Cardinal;
begin
  GetWindowThreadProcessId(Wnd, @PID);
  if ProcessID=PID then
    PostMessage(Wnd,WM_CLOSE,0,0);
  Result:=True;
end;

function KillProcess(ProcessID: Cardinal; Timeout: Integer = MAXINT): TTerminateStatus;
var
  ProcessHandle: THandle;
begin
  Result:=tsError;
  if ProcessID<>GetCurrentProcessId then begin
    ProcessHandle:=OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, ProcessID);
    try
      if (ProcessHandle<>0) and (ProcessHandle<>INVALID_HANDLE_VALUE) then begin
        if Timeout>=0 then begin
          EnumWindows(@kpEnumWindowsProc,LPARAM(ProcessID));
          if WaitForSingleObject(ProcessHandle,Timeout)=WAIT_OBJECT_0 then
            Result:=tsClose
          else
            if TerminateProcess(ProcessHandle,0) then
              Result:=tsTerminate;
        end else
          if TerminateProcess(ProcessHandle,0) then
            Result:=tsTerminate;
      end;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function ProcessExists(const APID: Cardinal; var AThreadCount,APriority: integer): Boolean;
var
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  Continue: BOOL;
begin
  AThreadCount:=0;
  APriority:=0;
  Result:=False;
  SnapshotHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (SnapshotHandle=INVALID_HANDLE_VALUE) then
    Exit;
  try
    ProcessEntry32.dwSize:=SizeOf(ProcessEntry32);
    Continue:=Process32First(SnapshotHandle, ProcessEntry32);
    while Continue do begin
      if (ProcessEntry32.th32ProcessID=APID) then begin
        AThreadCount:=ProcessEntry32.cntThreads;
        APriority:=ProcessEntry32.pcPriClassBase;
        Result:=True;
        Exit;
      end;
      Continue:=Process32Next(SnapshotHandle,ProcessEntry32);
    end;
  finally
    CloseHandle(SnapshotHandle);
  end;
end;

function GetChildProcesses(const APID: Cardinal; var AChildProcs: TCardinalArray): Boolean;
var
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  Continue: BOOL;
begin
  Finalize(AChildProcs);
  Result:=False;
  SnapshotHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (SnapshotHandle=INVALID_HANDLE_VALUE) then
    Exit;
  try
    ProcessEntry32.dwSize:=SizeOf(ProcessEntry32);
    Continue:=Process32First(SnapshotHandle, ProcessEntry32);
    while Continue do begin
      if (ProcessEntry32.th32ParentProcessID=APID) then begin
        SetLength(AChildProcs,Length(AChildProcs)+1);
        AChildProcs[High(AChildProcs)]:=ProcessEntry32.th32ProcessID;
      end;
      Continue:=Process32Next(SnapshotHandle,ProcessEntry32);
    end;
    Result:=True;
  finally
    CloseHandle(SnapshotHandle);
  end;
end;

function GetParentProcess(APID: Cardinal): Cardinal;
var
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  Continue: BOOL;
begin
  Result:=0;
  SnapshotHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (SnapshotHandle=INVALID_HANDLE_VALUE) then
    Exit;
  try
    ProcessEntry32.dwSize:=SizeOf(ProcessEntry32);
    Continue:=Process32First(SnapshotHandle, ProcessEntry32);
    while Continue do begin
      if (ProcessEntry32.th32ProcessID=APID) then begin
        Result:=ProcessEntry32.th32ParentProcessID;
        Exit;
      end;
      Continue:=Process32Next(SnapshotHandle,ProcessEntry32);
    end;
  finally
    CloseHandle(SnapshotHandle);
  end;
end;

function IsProcessActive(APID: integer): Boolean;
var
  ph: THandle;
begin
  ph:=OpenProcess(PROCESS_QUERY_INFORMATION,False,APID);
  Result:=(ph<>0) and (ph<>INVALID_HANDLE_VALUE);
  CloseHandle(ph);
end;

function irEnumWindowsProc(Wnd: HWND; APID: {$IFDEF FPC}LParam{$ELSE}Cardinal{$ENDIF}): {$IFDEF FPC}LongBool{$ELSE}Boolean{$ENDIF}; stdcall;
var
  PID: Cardinal;
begin
  GetWindowThreadProcessId(Wnd,PID);
  if (Cardinal(APID)=PID) then begin
    irwh:=Wnd;
    Result:=True;
  end else
    Result:=irwh=0;
end;

function IsProcessResponsible(APID: Cardinal): Boolean;
{$if not defined(RAD9PLUS) and not defined(FPC)}
type
  PDWORD_PTR = ^ULONG_PTR;
{$ifend}
var
  r: PDWORD_PTR;
begin
  irwh:=0;
  EnumWindows(@irEnumWindowsProc,APID);
  if irwh>0 then begin
    new(r);
    Result:=SendMessageTimeout(irwh,WM_NULL,0,0,SMTO_ABORTIFHUNG and SMTO_BLOCK,1000,{$if defined(RAD9PLUS) or defined(FPC)}r{$ELSE}r^{$ifend})>0;
    dispose(r);
  end else
    Result:=True;
end;

procedure GetFileInfo(const AFilename: string; var AFileInfo: TFileInfo; AConvertToLocalTime: Boolean = False);
var
  em: Cardinal;
  FI :TBYHANDLEFILEINFORMATION;
  shinfo :TSHFileInfo;
  h :THandle;
  ii :word;
  q :array [0..MAX_PATH - 1] of char;
begin
  h:=FileOpen(AFilename,fmOpenRead or fmShareDenyNone);
  if h<>Cardinal(-1) then
    with AFileInfo do begin
      ii:=0;
      strpcopy(q,AFilename);
      em:=SetErrorMode(SEM_FailCriticalErrors or SEM_NoOpenFileErrorBox);
      try
        if extracticon(hinstance,q,word(-1))>0 then
          iconhandle:=extracticon(hinstance,PChar(AFilename),ii)
        else
          iconhandle:=ExtractAssociatedIcon(hInstance,q,{$IFDEF FPC}@{$ENDIF}ii);
      finally
        SetErrorMode(em);
      end;
      if ShGetFileInfo(q,0,ShInfo,SizeOf(ShInfo),SHGFI_TYPENAME)<>0 then
        FileType:=ShInfo.szTypeName
      else
        FileType:='';
      GetFileInformationByHandle(h,FI);
      FileClose(h);
      Size:=FI.nFileSizelow+256*FI.nFileSizehigh;
      Attributes:=FI.dwFileAttributes;
      Created:=FileTimeToDateTime(FI.ftCreationTime,AConvertToLocalTime);
      Accessed:=FileTimeToDateTime(FI.ftLastAccessTime,AConvertToLocalTime);
      Modified:=FileTimeToDateTime(FI.ftLastWriteTime,AConvertToLocalTime);
      BinaryType:=GetBinType(Afilename);
    end;
end;

function GetFileIconCount(const Filename: string): Integer;
begin
  Result:=ExtractIcon(HInstance,PChar(Filename),Cardinal(-1));
end;

function GetFileIcon(const Filename: string; IconIndex: Word = 0): HICON;
var
  s: string;
  FileInfo : SHFILEINFO;
begin
  s:=FileName;
  if ExtractIcon(hInstance,PChar(s),Word(-1))>0 then
    Result:=ExtractIcon(hInstance,PChar(s),IconIndex)
  else begin
    SHGetFileInfo(PChar(s),
                  FILE_ATTRIBUTE_NORMAL,
                  FileInfo,
                  SizeOf(FileInfo),
                  SHGFI_ICON or SHGFI_LARGEICON or
                  SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES
                 );
    Result:=FileInfo.hIcon;
  end;
end;

function GetFileSize(const AFilename: string): int64;
var
  SRec: TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, SRec) <> 0 then
    Result:=-1
  else begin
    Int64Rec(Result).Lo:=SRec.FindData.nFileSizeLow;
    Int64Rec(Result).Hi:=SRec.FindData.nFileSizeHigh;
    FindClose(SRec);
  end;
end;

function GetFileTimes(const AFilename: string; out ACreated,AModified,AAccessed: TDateTime; ConvertTimeToLocal: Boolean = False): int64;
var
  SRec: TSearchRec;
begin
  if FindFirst(AFileName, faAnyFile, SRec) <> 0 then
    Result:=-1
  else begin
    Int64Rec(Result).Lo:=SRec.FindData.nFileSizeLow;
    Int64Rec(Result).Hi:=SRec.FindData.nFileSizeHigh;
    ACreated:=FileTimeToDatetime(SRec.FindData.ftCreationTime,ConvertTimeToLocal);
    AModified:=FileTimeToDatetime(SRec.FindData.ftLastWriteTime,ConvertTimeToLocal);
    AAccessed:=FileTimeToDatetime(SRec.FindData.ftLastAccessTime,ConvertTimeToLocal);
    FindClose(SRec);
  end;
end;

function HasAttr(const AFileName: string; AAttr: Word): Boolean;
begin
  Result:=(FileGetAttr(AFileName) and AAttr)=AAttr;
end;

function GetBinType(const AFilename :string) : string;
var
  BinaryType: Cardinal;
  fi :TSHFileInfo;
const
  IMAGE_DOS_SIGNATURE    = $5A4D; // MZ
  IMAGE_OS2_SIGNATURE    = $454E; // NE
  IMAGE_VXD_SIGNATURE    = $454C; // LE
  IMAGE_NT_SIGNATURE     = $0000; // PE
  IMAGE_32_SIGNATURE     = $4550;
begin
  binarytype:=SHGetFileInfo(PChar(AFilename),0,fi,sizeof(fi),SHGFI_EXETYPE);
  result:='';
  if binarytype<>0 then
    case loword(binarytype) of
      IMAGE_DOS_SIGNATURE: result:='DOS Executable';
      IMAGE_VXD_SIGNATURE: result:='Virtual Device Driver';
      IMAGE_OS2_SIGNATURE,IMAGE_NT_SIGNATURE, IMAGE_32_SIGNATURE:
      case hiword(binarytype) of
        $400: result:='Win32 Executable';
        $30A,$300: result:='Win16 Executable';
        $0 :result:='Win32 Console Executable';
      end;
    end;
  if Result='' then
    if GetBinaryType(PChar(AFilename),Binarytype) then
      case BinaryType of
        SCS_32BIT_BINARY: result:= 'Win32 Executable';
        SCS_DOS_BINARY  : result:= 'DOS Executable';
        SCS_WOW_BINARY  : result:= 'Win16 Executable';
        SCS_PIF_BINARY  : result:= 'PIF File';
        SCS_POSIX_BINARY: result:= 'POSIX Executable';
        SCS_OS216_BINARY: result:= 'OS/2 16 bit Executable'
      end;
end;

function ExtractUNCFilename(ASource :string) : string;
var
  p,l :integer;
begin
  p:=pos(':',ASource);
  if p>0 then begin
    l:=Length(ASource);
    result:=Copy(ASource,p-1,l-p+2);
  end else
    result:=ASource;
end;

function DequoteStr(Source: string; Quote: Char = '"'): string;
begin
  Result:=Source;
  if Length(Source)>1 then
    if (Source[1]=Quote) and (Source[Length(Source)]=Quote) then
      Result:=Copy(Source,2,Length(Source)-2);
end;

function ExtractFilenameFromStr(Source: string): string;
var
  s: string;
begin
  s:=DequoteStr(ExpandEnvVars(Source));
  while not FileExists(s) and (Length(s)>0) do begin
    Delete(s,Length(s),1);
    s:=DequoteStr(s);
  end;
  if Length(s)<>0 then
    Result:=s
  else
    Result:='';
end;

function ExtractName(const AFilename: string): string;
begin
  Result:=ExtractFileName(AFilename);
  if SameText(AFilename,'.') or SameText(AFilename,'..') then
    Exit;
  Result:=ChangeFileExt(Result,'');
end;

function FileCopy(const AFileName, ADestName: string): Boolean;
var
  CopyBuffer: Pointer;
  BytesCopied: Longint;
  Source, Dest: Integer;
  Destination: TFileName;
const
  ChunkSize: Longint = 8192;
begin
  Result:=False;
  Destination:=ExpandFileName(ADestName);
{  if HasAttr(Destination, faDirectory) then
    Destination:=UniPath(Destination,true) + ExtractFileName(AFileName);}
  GetMem(CopyBuffer, ChunkSize);
  try
    Source:=FileOpen(AFileName, fmShareDenyNone);
    if not(Source<0) then
      try
        Dest:=FileCreate(Destination);
        if not(Dest<0) then
          try
            repeat
              BytesCopied:=FileRead(Source, CopyBuffer^, ChunkSize);
              if BytesCopied>0 then
                 FileWrite(Dest, CopyBuffer^, BytesCopied);
             until BytesCopied<ChunkSize;
             Result:=True;
          finally
            FileClose(Dest);
          end;
        finally
          FileClose(Source);
        end;
  finally
    FreeMem(CopyBuffer, ChunkSize);
  end;
end;

function FileMove(const AFileName, ADestName: string): boolean;
var
  Destination: string;
begin
  Result:=True;
  Destination:=ExpandFileName(ADestName);
  if not RenameFile(AFileName, Destination) then begin
    if HasAttr(AFileName, faReadOnly) then begin
      Result:=False;
      Exit;
    end;
    Result:=FileCopy(AFileName, Destination);
    if Result then
      DeleteFile(AFilename);
  end;
end;

function FileNameMove(const AFileName, ADestName: string): Integer;
var
  SR: TSearchRec;
  s,t: string;
begin
  Result:=0;
  s:=ExtractFilePath(AFileName);
  t:=ExtractFilePath(ADestName);
  if FindFirst(ChangeFileExt(AFileName,'.*'),faArchive,SR)=0 then begin
    if FileMove(s+SR.Name,t+SR.Name) then
      Inc(Result);
    while FindNext(SR)=0 do begin
      if FileMove(s+SR.Name,t+SR.Name) then
        Inc(Result);
    end;
  end;
  FindClose(SR);
end;

function FileNameCopy(const AFileName,AExtSpec, ADestName: string): Integer;
var
  SR: TSearchRec;
  s,t: string;
begin
  Result:=0;
  s:=ExtractFilePath(AFileName);
  t:=ExtractFilePath(ADestName);
  if FindFirst(ChangeFileExt(AFileName,AExtSpec),faArchive,SR)=0 then begin
    if FileCopy(s+SR.Name,t+SR.Name) then
      Inc(Result);
    while FindNext(SR)=0 do
      if FileCopy(s+SR.Name,t+SR.Name) then
        Inc(Result);
  end;
  FindClose(SR);
end;

procedure SaveToFile(AFilename,AText: string; AOverwrite: Boolean = False);
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    if FileExists(AFilename) and not AOverwrite then
      sl.LoadFromFile(AFilename);
    sl.Add(AText);
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

function IsBitOn(Value: UInt64; Bit: Byte): Boolean;
begin
  Result:=(Value and (1 shl Bit))<>0;
end;

function SetBit(const Value: UInt64; const Bit: byte): UInt64;
begin
  Result:=Value or (1 shl Bit);
end;

function ClearBit(const Value: UInt64; const Bit: Byte): UInt64;
begin
  Result:=Value and not (1 shl Bit);
end;

function GetBitsFromDWORD(const aval: Cardinal; const afrom,ato: byte): Integer;
var
  mask: Integer;
begin
  mask:=(1 shl (ato+1))-1;
  if (ato=31) then
    Result:=aval shr afrom
  else
    Result:=(aval and mask) shr afrom;
end;

function CountSetBits(bitMask: {$IFDEF FPC}Cardinal{$ELSE}ULONG_PTR{$ENDIF}): DWORD;
var
  LSHIFT,i: DWORD;
  bitTest: {$IFDEF FPC}Cardinal{$ELSE}ULONG_PTR{$ENDIF};
begin
  Result:=0;
  LSHIFT:=sizeof(bitMask)*8-1;
  bitTest:={$IFDEF FPC}Cardinal{$ELSE}ULONG_PTR{$ENDIF}(1 shl LSHIFT);
  for i:=0 to LSHIFT do begin
    if (bitMask and bitTest)>0 then
      Inc(Result);
    bitTest:=bitTest div 2;
  end;
end;

function CreateDOSProcessRedirected(const CommandLine, InputFile, OutputFile,ErrMsg :string): Boolean;
const
  ROUTINE_ID = '[function: CreateDOSProcessRedirected]';
var
  pCommandLine: array[0..MAX_PATH] of char;
  pInputFile,
  pOutPutFile: array[0..MAX_PATH] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecAtrrs: TSecurityAttributes;
  hAppProcess,
  hAppThread,
  hInputFile,
  hOutputFile: THandle;
begin
  Result:=False;
  if (InputFile<>'') and (not FileExists(InputFile)) then
    raise Exception.CreateFmt(ROUTINE_ID + #10 + #10 +
       'Input file * %s *' + #10 +
       'does not exist' + #10 + #10 +
       ErrMsg, [InputFile]);
  hAppProcess:=0;
  hAppThread:=0;
  hInputFile:=0;
  hOutputFile:=0;
  try
    StrPCopy(pCommandLine, CommandLine);
    StrPCopy(pInputFile, InputFile);
    StrPCopy(pOutPutFile, OutputFile);
    { prepare SecAtrrs structure for the CreateFile calls.  This SecAttrs
      structure is needed in this case because we want the returned handle to
      be inherited by child process. This is true when running under WinNT.
      As for Win95, the parameter is ignored. }
    ResetMemory(SecAtrrs,SizeOf(SecAtrrs));
    SecAtrrs.nLength:=SizeOf(SecAtrrs);
    SecAtrrs.lpSecurityDescriptor:=nil;
    SecAtrrs.bInheritHandle:=TRUE;
    if InputFile<>'' then begin
      hInputFile:=CreateFile(
         pInputFile,                          { pointer to name of the file }
         GENERIC_READ or GENERIC_WRITE,       { access (read-write) mode }
         FILE_SHARE_READ or FILE_SHARE_WRITE, { share mode }
         @SecAtrrs,                           { pointer to security attributes }
         OPEN_ALWAYS,                         { how to create }
         FILE_ATTRIBUTE_NORMAL
         or FILE_FLAG_WRITE_THROUGH,          { file attributes }
         0);                                 { handle to file with attrs to copy }
      if hInputFile = INVALID_HANDLE_VALUE then
        raise Exception.CreateFmt(ROUTINE_ID + #10 +  #10 +
           'WinApi function CreateFile returned an invalid handle value' + #10 +
           'for the input file * %s *' + #10 + #10 +
            ErrMsg, [InputFile]);
    end else
      hInputFile:=0;

    hOutputFile:=CreateFile(
       pOutPutFile,                         { pointer to name of the file }
       GENERIC_READ or GENERIC_WRITE,       { access (read-write) mode }
       FILE_SHARE_READ or FILE_SHARE_WRITE, { share mode }
       @SecAtrrs,                           { pointer to security attributes }
       CREATE_ALWAYS,                       { how to create }
       FILE_ATTRIBUTE_NORMAL
       or FILE_FLAG_WRITE_THROUGH,          { file attributes }
       0 );                                 { handle to file with attrs to copy }
    if hOutputFile=INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt(ROUTINE_ID + #10 +  #10 +
         'WinApi function CreateFile returned an invalid handle value'  + #10 +
         'for the output file * %s *' + #10 + #10 +
         ErrMsg, [OutputFile]);

    ResetMemory(StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb:=SizeOf(StartupInfo);
    StartupInfo.dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow:=SW_HIDE;
    StartupInfo.hStdOutput:=hOutputFile;
    StartupInfo.hStdInput:=hInputFile;

    Result:=CreateProcess(
       NIL,                           { pointer to name of executable module }
       pCommandLine,                  { pointer to command line string }
       NIL,                           { pointer to process security attributes }
       NIL,                           { pointer to thread security attributes }
       TRUE,                          { handle inheritance flag }
       HIGH_PRIORITY_CLASS,           { creation flags }
       NIL,                           { pointer to new environment block }
       NIL,                           { pointer to current directory name }
       StartupInfo,                   { pointer to STARTUPINFO }
       ProcessInfo);                  { pointer to PROCESS_INF }

    if Result then begin
      WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
      hAppProcess:=ProcessInfo.hProcess;
      hAppThread:=ProcessInfo.hThread;
    end else
      raise Exception.Create(ROUTINE_ID + #10 +  #10 +
         'Function failure'  + #10 +  #10 + ErrMsg);
  finally
    if hOutputFile <> 0 then
      CloseHandle(hOutputFile);
    if hInputFile <> 0 then
      CloseHandle(hInputFile);
    if hAppThread <> 0 then
      CloseHandle(hAppThread);
    if hAppProcess <> 0 then
      CloseHandle(hAppProcess);
  end;
end;

function CreateDOSProcessToStrings(CommandLine: string; AOutput: TStrings): Boolean;
const
   ReadBuffer = 1024;
var
  Security: TSecurityAttributes;
  orh, iwh, OutputRead, OutputWrite, InputRead, InputWrite, ErrorWrite: THandle;
  start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PAnsichar;
  BytesRead, AppRunning: Cardinal;
begin
  Result:=False;
  with Security do begin
    nlength:=SizeOf(TSecurityAttributes) ;
    binherithandle:=True;
    lpsecuritydescriptor:=nil;
  end;

  if CreatePipe(orh,OutputWrite,@Security,0) then begin
    DuplicateHandle(GetCurrentProcess,OutputWrite,GetCurrentProcess,@ErrorWrite,0,True,DUPLICATE_SAME_ACCESS);
    if CreatePipe(InputRead,iwh,@Security,0) then begin
      DuplicateHandle(GetCurrentProcess,orh,GetCurrentProcess,@OutputRead,0,False,DUPLICATE_SAME_ACCESS);
      DuplicateHandle(GetCurrentProcess,iwh,GetCurrentProcess,@InputWrite,0,False,DUPLICATE_SAME_ACCESS);
      CloseHandle(orh);
      CloseHandle(iwh);

      Buffer:=AllocMem(ReadBuffer+1);
      try
        ResetMemory(Start,Sizeof(Start));
        start.cb:=SizeOf(start);
        start.hStdOutput:=OutputWrite;
        start.hStdInput:=InputRead;
        start.hStdError:=ErrorWrite;
        start.dwFlags:=STARTF_USESTDHANDLES+STARTF_USESHOWWINDOW;
        start.wShowWindow:=SW_HIDE;

        Result:=CreateProcess(nil,PChar('cmd.exe /c '+CommandLine),@Security,@Security,True,NORMAL_PRIORITY_CLASS,nil,nil,start,ProcessInfo);
        CloseHandle(OutputWrite);
        CloseHandle(InputRead);
        CloseHandle(ErrorWrite);
        try
          if Result then begin
            repeat
              AppRunning:=WaitForSingleObject(ProcessInfo.hProcess,1000);
              repeat
                BytesRead:=0;
                ReadFile(OutputRead,Buffer[0],ReadBuffer,BytesRead,nil);
                Buffer[BytesRead]:= #0;
                OemToAnsi(Buffer,Buffer);
                AOutput.Text:=AOutput.Text+string(Buffer);
              until (BytesRead=0) or (AppRunning<>WAIT_TIMEOUT);
              AppRunning:=WaitForSingleObject(ProcessInfo.hProcess,1000);
            until (AppRunning<>WAIT_TIMEOUT);
          end;
        finally
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
        end;
      finally
        CloseHandle(OutputRead);
        CloseHandle(InputWrite);
        FreeMem(Buffer);
      end;
    end;
  end;
end;

function CreateDOSProcess(CommandLine: string): Boolean;
var
  Security : TSecurityAttributes;
  start : TStartUpInfo;
  ProcessInfo : TProcessInformation;
  Apprunning : DWord;
begin
  with Security do begin
    nlength:=SizeOf(TSecurityAttributes);
    binherithandle:=true;
    lpsecuritydescriptor:=nil;
  end;
  ResetMemory(Start,Sizeof(Start));
  start.cb:=SizeOf(start) ;
  start.dwFlags:=STARTF_USESTDHANDLES+STARTF_USESHOWWINDOW;
  start.wShowWindow:=SW_HIDE;
  Result:=CreateProcess(nil,PChar(CommandLine),@Security,@Security,True,NORMAL_PRIORITY_CLASS,nil,nil,start,ProcessInfo);
  try
    if Result then
      repeat
        Apprunning:=WaitForSingleObject(ProcessInfo.hProcess,100);
        //Application.ProcessMessages;
      until (Apprunning<>WAIT_TIMEOUT);
 finally
   CloseHandle(ProcessInfo.hProcess);
   CloseHandle(ProcessInfo.hThread);
 end;
end;

function GetCmdOutput(const ACommandLine: string): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  ok: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  fn: string;
begin
  Result:='';
  fn:=GetUniqueFilename('PM',0,True);
  DeleteFile(fn);
  fn:=ChangeFileExt(fn,'.bat');
  with TStringList.Create do
    try
      Add('@echo off');
      Add(Format('cd "%s"',[GetCurrentDir]));
      Add(ACommandLine);
      SaveToFile(fn);
    finally
      Free;
    end;
  try
    ResetMemory(PI,sizeof(PI));
    with SA do begin
      nLength:=SizeOf(SA);
      bInheritHandle:=True;
      lpSecurityDescriptor:=nil;
    end;
    CreatePipe(StdOutPipeRead,StdOutPipeWrite,@SA,0);
    try
      with SI do begin
        FillChar(SI,SizeOf(SI),0);
        cb:=SizeOf(SI);
        dwFlags:=STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow:=SW_HIDE;
        hStdInput:=GetStdHandle(STD_INPUT_HANDLE);
        hStdOutput:=StdOutPipeWrite;
        hStdError:=StdOutPipeWrite;
      end;
      ok:=CreateProcess(nil,PChar('cmd.exe /c '+fn),nil,nil,True,0,nil,nil,SI,PI);
      CloseHandle(StdOutPipeWrite);
      if ok then
        try
          repeat
            ok:=ReadFile(StdOutPipeRead,Buffer,255,BytesRead,nil);
            if BytesRead>0 then begin
              Buffer[BytesRead]:=#0;
              Result:=Result+Buffer;
            end;
          until not ok or (BytesRead=0);
          WaitForSingleObject(PI.hProcess,INFINITE);
        finally
          CloseHandle(PI.hThread);
          CloseHandle(PI.hProcess);
        end;
    finally
      CloseHandle(StdOutPipeRead);
    end;
  finally
    DeleteFile(fn);
  end;
end;

function CreateProc(FileName: string; CommandLine: string = ''; ShowMode: Integer = -1; AWaitms: Cardinal = INFINITE; const ADir: string = ''): Boolean;
var
  s: string;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  s:=ADir;
  if s='' then
    s:=ExtractFilePath(Filename);
  if SameText(Filename,s) then
    s:='';
  ResetMemory(StartInfo,SizeOf(TStartupInfo));
  ResetMemory(ProcInfo,SizeOf(TProcessInformation));
  StartInfo.cb:=SizeOf(TStartupInfo);
  if ShowMode>=0 then begin
    StartInfo.wShowWindow:=ShowMode;
    StartInfo.dwFlags:=STARTF_USESHOWWINDOW;
  end;
  Result:=CreateProcess(nil,PChar(Trim(Filename+' '+CommandLine)),nil,nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, PChar(s), StartInfo, ProcInfo);
  if Result then
    WaitForSingleObject(ProcInfo.hProcess,AWaitms);
  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

function CreateProcEx(FileName: string; var AExitCode: Cardinal; CommandLine: string = ''; ShowMode: Integer = -1; AWaitms: Cardinal = INFINITE): Boolean;
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  ResetMemory(StartInfo,SizeOf(TStartupInfo));
  ResetMemory(ProcInfo,SizeOf(TProcessInformation));
  StartInfo.cb:=SizeOf(TStartupInfo);
  if ShowMode>=0 then begin
    StartInfo.wShowWindow:=ShowMode;
    StartInfo.dwFlags:=STARTF_USESHOWWINDOW;
  end;
  Result:=CreateProcess(nil,PChar(Trim(Filename+' '+CommandLine)),nil,nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, PChar(ExtractFilePath(Filename)), StartInfo, ProcInfo);
  if Result then
    WaitForSingleObject(ProcInfo.hProcess,AWaitms);
  GetExitCodeProcess(ProcInfo.hProcess,AExitCode);
  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

function FindProcess(const AName: string; ASession: Cardinal = Cardinal(-1)): Integer;
var
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
  sid: Cardinal;
begin
  if not Assigned(ProcessIdToSessionId) then
    ASession:=Cardinal(-1);
  Result:=-1;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        sid:=Cardinal(-1);
        if ASession<>Cardinal(-1) then
          ProcessIdToSessionId(pe32.th32ProcessID,sid);
        if SameText(AName,pe32.szExeFile) and ((ASession=Cardinal(-1)) or (ASession=sid)) then begin
          Result:=pe32.th32ProcessID;
          Break;
        end;
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;

function GetProccessInstanceCount(const AName: string): integer;
var
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
begin
  Result:=0;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        if SameText(AName,pe32.szExeFile) then
          inc(Result);
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;

function OpenMailSlot(Const Server, Slot : String): THandle;
var
  FullSlot : String;
begin
  FullSlot:='\\'+Server+'\mailslot\'+Slot;
  Result:=CreateFile(
    PChar(FullSlot),
    GENERIC_WRITE,
    FILE_SHARE_READ,
    NIL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0                    );
end;

function SendToMailSlot(Const Server, Slot, Mail : String) : Boolean;
var
  hToSlot : THandle;
  BytesWritten : Cardinal;
begin
  Result:=False;
  hToSlot:=OpenMailSlot(Server,Slot);
  If hToSlot = INVALID_HANDLE_VALUE Then
    Exit;
  try
    BytesWritten:=0;
    if (NOT WriteFile(hToSlot,
                      Pointer(Mail)^,
                      Length(Mail),
                      BytesWritten,
                      NIL))         OR
        (INTEGER(BytesWritten) <> Length(Mail)) Then
      Exit;
    Result:=True;
  finally
    CloseHandle(hToSlot);
  end;
end;

function SendToWinpopup(Server, Reciever, Sender, Msg : String) : Boolean;
var
  szserver,szsender,szreciever,szmsg :pansichar;
begin
  szserver:=allocmem(256);
  szsender:=allocmem(256);
  szreciever:=allocmem(256);
  szmsg:=allocmem(256);
  {$IFDEF RAD6PLUS}
  CharToOEM(PWideChar(Server),szServer);
  CharToOEM(PWideChar(Sender),szSender);
  CharToOEM(PWideChar(Reciever),szReciever);
  CharToOEM(PWideChar(Msg),szMsg);
  {$ELSE}
  CharToOEM(PAnsiChar(Server),szServer);
  CharToOEM(PAnsiChar(Sender),szSender);
  CharToOEM(PAnsiChar(Reciever),szReciever);
  CharToOEM(PAnsiChar(Msg),szMsg);
  {$ENDIF}
  Result:=SendToMailSlot(Server, string(wpslot), string(szSender)+#0+string(szReciever)+#0+string(szMsg));
  freemem(szserver);
  freemem(szsender);
  freemem(szreciever);
  freemem(szmsg);
end;

function GetFontRes: Cardinal;
var
  tm: TTextMetric;
  hwnd,hdc: THandle;
  MapMode: Cardinal;
begin
  Result:=0;
  hwnd:=GetDesktopWindow;
  hdc:=GetWindowDC(hwnd);
  if hdc>0 then begin
    MapMode:=SetMapMode(hdc,MM_TEXT);
    GetTextMetrics(hdc,tm);
    SetMapMode(hdc,MapMode);
    ReleaseDC(hwnd,hdc);
    Result:=tm.tmHeight;
  end;
end;

procedure RunAtStartup(AKey: HKEY; Flag: Boolean; Name,Cmdline: string);
begin
  with TRegistry.Create do begin
    RootKey:=AKey;

    if OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',False) then begin
      if Flag then
        WriteString(Name,CmdLine)
      else
        DeleteValue(Name);
      CloseKey;
    end;
    Free;
  end;
end;

function CheckRunAtStartup(Akey: HKEY; Name,CmdLine: string): Boolean;
begin
  Result:=False;
  with OpenRegistryReadOnly do begin
    RootKey:=AKey;
    if OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',False) then begin
      if ValueExists(Name) then
        Result:=UpperCase(ReadString(Name))=UpperCase(CmdLine);
      CloseKey;
    end;
    Free;
  end;
end;

function ExtractImageName(ACmdLine: string; AExtendedVarExpand: boolean = True): string;
var
  sl: TStringList;
  i: Integer;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    sl.QuoteChar:='"';
    sl.DelimitedText:=ExpandEnvVars(ACmdLine,AExtendedVarExpand);
    if sl.Count=0 then
      Exit;
    Result:=sl[0];
    i:=1;
    while not FileExists(Result) and (i<sl.Count) do begin
      Result:=Result+' '+sl[i];
      Inc(i);
    end;
  finally
    sl.Free;
  end;
end;

function GetUniqueFilename(Prefix: string; Unique: Cardinal = 0; Temp: Boolean = False): string;
var
  p: PChar;
begin
  p:=StrAlloc(MAX_PATH+1);
  GetTempFileName(PChar(GetTempDir),PChar(Prefix),Unique,p);
  if Temp then
    Result:=p
  else
    Result:=ExtractFilename(p);
  StrDispose(p);
end;

function FileExistsEx(const FileName: string): Boolean;
var
  shinfo :TSHFileInfo;
  p: Pointer;
begin
  if FileName='' then
    Result:=False
  else begin
    if Assigned(Wow64DisableWow64FsRedirection) then
       Wow64DisableWow64FsRedirection(p);
    try
      Result:=ShGetFileInfo(PChar(FileName),0,ShInfo,SizeOf(ShInfo),SHGFI_TYPENAME)<>0;
    finally
      if Assigned(Wow64RevertWow64FsRedirection) then
        Wow64RevertWow64FsRedirection(p);
    end;
  end;
end;

function WinExecAndWait32(FileName,Parameters: String; Visibility: integer): Cardinal;
var
  zParams,zAppName: array[0..512] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName+' '+Parameters);
  StrPCopy(zParams, Parameters);
  ResetMemory(StartupInfo, Sizeof(StartupInfo));
  StartupInfo.cb:=Sizeof(StartupInfo);
  StartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow:=Visibility;
  if not CreateProcess(nil, zAppName, nil, nil, false, CREATE_NEW_CONSOLE or
                                          NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    Result:=Cardinal(-1)
  else begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess );
    CloseHandle(ProcessInfo.hThread );
  end;
end;

{function FileTimeToDateTimeStr(FileTime: TFileTime): string;
var
  LocFTime: TFileTime;
  SysFTime: TSystemTime;
  DateStr: string;
  TimeStr: string;
  FDateTimeStr: string;
  Dt, Tm: TDateTime;
begin
  FileTimeToLocalFileTime(FileTime, LocFTime);
  FileTimeToSystemTime(LocFTime, SysFTime);
  try
    with SysFTime do begin
      Dt:=EncodeDate(wYear, wMonth, wDay);
      DateStr:=DateToStr(Dt);
      Tm:=EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
      Timestr:=TimeToStr(Tm);
      FDateTimeStr:=DateStr + '   ' + TimeStr;
    end;
    Result:=DateTimeToStr(StrToDateTime(FDateTimeStr));
  except
    Result:='';
  end;
end;

function FiletimeToDateTime(FT: FILETIME): TDateTime;
var
  st: SYSTEMTIME;
  dt1,dt2: TDateTime;
begin
  FileTimeToSystemTime(FT,st);
  try
    dt1:=EncodeTime(st.whour,st.wminute,st.wsecond,st.wMilliseconds);
  except
    dt1:=0;
  end;
  try
    dt2:=EncodeDate(st.wyear,st.wmonth,st.wday);
  except
    dt2:=0;
  end;
  Result:=dt1+dt2;
end;}

function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;
var
  h: THandle;
  ptg: PTokenGroups;
  n: Cardinal;
  psidAdmins: PSID;
  i: Integer;
  ok: BOOL;
begin
  Result:=False;
  ok:=OpenThreadToken(GetCurrentThread,TOKEN_QUERY,True,h);
  if not ok then begin
    if GetLastError = ERROR_NO_TOKEN then
      ok:=OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,h);
  end;
  n:=0;
  if ok then begin
    try
      GetTokenInformation(h,TokenGroups,nil,0,n);
      GetMem(ptg,n);
      ok:=GetTokenInformation(h,TokenGroups,ptg,n,n);
    finally
      CloseHandle(h);
    end;
    try
      if ok then begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY,2,SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_ADMINS,0,0,0,0,0,0,psidAdmins);
        try
          {$IFOPT R+} {$R-} {$DEFINE NoRangeCheck} {$ENDIF}
          for i:=0 to ptg^.GroupCount-1 do
            if EqualSid(psidAdmins,ptg^.Groups[i].Sid) then begin
              Result:=True;
              Break;
            end;
          {$IFDEF NoRangeCheck} {$R+} {$UNDEF NoRangeCheck} {$ENDIF}
        finally
          FreeSid(psidAdmins);
        end;
      end;
    finally
      FreeMem(ptg);
    end;
  end;
end;

function GetProcessPrivileges(Processhandle: THandle; var AList: TPrivilegeList; var AElevation: Cardinal): boolean;
var
  hToken: THandle;
  pTokenInfo: PTokenPrivileges;
  n: Cardinal;
  i: Integer;
  PrivName,
  DispName: array[0..255] of Char;
  NameSize: Cardinal;
  DisplSize: Cardinal;
  LangId: Cardinal;
  priv: PLUIDAndAttributes;
begin
  Result:=False;
  SetLength(AList,0);
  if OpenProcessToken(ProcessHandle,TOKEN_QUERY,hToken) then
    try
      n:=0;
      GetTokenInformation(hToken,TokenPrivileges,nil,n,n);
      GetMem(pTokenInfo,n);
      try
        if GetTokenInformation(hToken,TokenPrivileges,pTokenInfo,n,n) then begin
          priv:=PLUIDAndAttributes(PAnsiChar(pTokenInfo)+SizeOf(Cardinal));
          for i:=0 to pTokenInfo^.PrivilegeCount-1 do begin
            NameSize:=255;
            LookupPrivilegeName(nil,priv^.Luid,@PrivName,Namesize);
            DisplSize:=255;
            LookupPrivilegeDisplayName(nil,@PrivName,@DispName,DisplSize,LangId);
            SetLength(AList,Length(AList)+1);
            with AList[High(AList)] do begin
              Name:=string(PrivName);
              DisplayName:=string(DispName);
              Flags:=priv^.Attributes;
            end;
            priv:=PLUIDAndAttributes(PAnsiChar(priv)+SizeOf(TLUIDAndAttributes));
          end;
        end;
      finally
        FreeMem(pTokenInfo);
      end;
      n:=0;
      GetTokenInformation(hToken,TTokenInformationClass(18){TokenElevationType},@AElevation,SizeOf(AElevation),n);
    finally
      CloseHandle(hToken);
    end;
end;

function IsPrivilegeEnabled(const Privilege: string): Boolean;
var
  Token: THandle;
  TokenPriv: TPrivilegeSet;
  Res: LongBool;
  HaveToken: Boolean;
begin
  Result:=True;
  Token := 0;
  HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, False, Token);
  if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
    HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
  if HaveToken then
  begin
    TokenPriv.PrivilegeCount := 1;
    TokenPriv.Control := 0;
    LookupPrivilegeValue(nil, PChar(Privilege), TokenPriv.Privilege[0].Luid);
    Res := False;
    Result := PrivilegeCheck(Token, TokenPriv, Res) and Res;
    CloseHandle(Token);
  end;
end;

function GetProcessGroups(Processhandle: THandle; var AList: TTokenGroupList): Boolean;
var
  hToken: THandle;
  pTokenInfo: PTokenGroups;
  i: Integer;
  pName,
  pDomain: array[0..255] of Char;
  n: Cardinal;
  SIDType: {$IFDEF FPC}SID_NAME_USE{$ELSE}Cardinal{$ENDIF};
  group: PSIDAndAttributes;
begin
  Result:=False;
  SetLength(AList,0);
  if OpenProcessToken(ProcessHandle,TOKEN_QUERY,hToken) then
  try
    n:=0;
    GetTokenInformation(hToken,TokenGroups,nil,n,n);
    GetMem(pTokenInfo,n);
    try
      if GetTokenInformation(hToken,TokenGroups,pTokenInfo,n,n) then begin
        group:=PSIDAndAttributes(PAnsiChar(pTokenInfo)+SizeOf(NativeUInt));
        for i:=0 to pTokenInfo^.GroupCount-1 do begin
          ResetMemory(pname,SizeOf(pName));
          ResetMemory(pDomain,SizeOf(pDomain));
          n:=255;
          LookupAccountSID(nil,group^.Sid,PChar(@pName),n,PChar(@pDomain),n,SIDType);
          SetLength(AList,Length(AList)+1);
          with AList[High(AList)] do begin
            Name:=string(pName);
            Domain:=string(pDomain);
            SID:=ConvertSIDToString(group^.Sid);
            Flags:=group^.Attributes;
          end;
          group:=PSIDAndAttributes(PAnsiChar(group)+SizeOf(TSIDAndAttributes));
        end;
      end;
    finally
      FreeMem(pTokenInfo);
    end;
  finally
    CloseHandle(hToken);
  end;
end;

function GetProcessFilename(APID: Cardinal): string;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  ps,ph: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
  Buf: array[0..MAX_PATH] of char;
  n: {$IFDEF NATIVEINT}NativeUint{$ELSE}Cardinal{$ENDIF};
begin
  Result:='';
  if APID<5 then begin
    Result:='System';
    Exit;
  end;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        if pe32.th32ProcessID=APID then begin
          Result:=pe32.szExeFile;
          ph:=OpenProcess(PROCESS_ALL_ACCESS,False,pe32.th32ProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_OPERATION,False,pe32.th32ProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_INFORMATION,False,pe32.th32ProcessID);
          if ph=0 then
            ph:=OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,False,pe32.th32ProcessID);
          if ph>0 then begin
            ResetMemory(Buf,SizeOf(Buf));
            if Assigned(QueryFullProcessImageName) then begin
              n:=SizeOf(Buf);
              if QueryFullProcessImageName(ph,0,@Buf,@n)>0 then
                Result:=Buf;
            end else if GetModuleFileNameEx(ph,0,@Buf,SizeOf(Buf))>0 then begin
              Result:=Buf;
              SetLength(Result,StrLen(PChar(Result)));
              Result:=StringReplace(Result,'\??\','',[]);
            end;
            CloseHandle(ph);
          end;
          Break;
        end;
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;

function GetModFilename(APID: Cardinal; const AName: string): string;
const
  TH32CS_SNAPMODULE32   = $00000010;
var
  ms: THandle;
  me32: TMODULEENTRY32;
  ok: Boolean;
begin
  Result:=AName;
  ms:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32,APID);
  if (ms<>INVALID_HANDLE_VALUE) then
    try
      me32.dwSize:=sizeof(TMODULEENTRY32);
      ok:=Module32First(ms,me32);
      while ok do begin
        if SameText(string(me32.szModule),Aname) then begin
          Result:=string(me32.szExePath);
          if not FileExists(Result) then
            Result:=StringReplace(Result,'\??\','',[rfIgnoreCase]);
          if not FileExists(Result) then
            Result:=ExpandEnvVars(Result);
          if not FileExists(Result) then
            Result:=ExpandFilename(FileSearch(Result,GetWinSysDir));
          Break;
        end;
        ok:=Module32Next(ms,me32);
      end;
    finally
      CloseHandle(ms);
    end;
end;

function GetProcessUserSID(hProcess :THandle): string;
var
  hToken :THandle;
  pTokenInfo: PSIDAndAttributes;
  n: Cardinal;
begin
  Result:='';
  if OpenProcessToken(hProcess,TOKEN_QUERY,hToken) then
    try
      n:=0;
      GetTokenInformation(hToken,TokenUser,nil,n,n);
      GetMem(pTokenInfo,n);
      try
        if GetTokenInformation(hToken,TokenUser,pTokenInfo,n,n) then
          Result:=ConvertSIDToString(pTokenInfo^.Sid);
      finally
        Freemem(pTokenInfo);
      end;
    finally
      CloseHandle(hToken);
    end;
end;

function GetProcessUserName(hProcess :THandle; var UserName, DomainName :string) :boolean;
var
  hToken :THandle;
  pTokenInfo: PSIDAndAttributes;
  pName, pDomain :array[0..255] of Char;
  SIDType: {$IFDEF FPC}SID_NAME_USE{$ELSE}Cardinal{$ENDIF};
  n: Cardinal;
begin
  Result:=False;
  UserName:='';
  DomainName:='';
  if OpenProcessToken(hProcess,TOKEN_QUERY,hToken) then
    try
      n:=0;
      GetTokenInformation(hToken,TokenUser,nil,n,n);
      GetMem(pTokenInfo,n);
      try
        if GetTokenInformation(hToken,TokenUser,pTokenInfo,n,n) then begin
          n:=255;
          LookupAccountSID(nil,pTokenInfo^.Sid,PChar(@pName),n,PChar(@pDomain),n,SIDType);
          if string(pName)='' then
            UserName:=ConvertSIDToString(pTokenInfo^.Sid)
          else begin
            UserName:=string(pName);
            DomainName:=string(pDomain);
          end;
          Result:=True;
        end;
      finally
        Freemem(pTokenInfo);
      end;
    finally
      CloseHandle(hToken);
    end;
end;

function GetProcessUserNameFromPID(PID :Cardinal; var UserName, DomainName :string) : Boolean;
var
  ph :THandle;
begin
  ph:=OpenProcess(PROCESS_ALL_ACCESS,True,PID);
  try
    Result:=GetProcessUserName(ph,Username,DomainName);
  finally
    CloseHandle(ph);
  end;
end;

function GetProcessUserNameEx(PID :Cardinal; var UserName, DomainName :string) : Boolean;
var
  ph :THandle;
  psd: PSecurityDescriptor;
  psidOwner: PSID;
  pName, pDomain :array[0..255] of Char;
  SIDType: {$IFDEF FPC}SID_NAME_USE{$ELSE}Cardinal{$ENDIF};
  j,n: Cardinal;
  b: LongBool;
begin
  Result:=False;
  psd:=nil;
  j:=OWNER_SECURITY_INFORMATION;
  ph:=OpenProcess(PROCESS_ALL_ACCESS,True,PID);
  try
    GetUserObjectSecurity(ph,j,psd,0,n);
    psd:=Allocmem(n);
    try
      if not GetUserObjectSecurity(ph,j,psd,n,n) then
        Exit;
      GetSecurityDescriptorOwner(psd,psidOwner,{$IFDEF FPC}@{$ENDIF}b);
      if not IsValidSid(psidOwner) then
        Exit;
      j:=0;
      repeat
        LookupAccountSID(nil,psidOwner,PChar(@pName),n,PChar(@pDomain),n,SIDType);
        n:=GetLastError;
        Inc(j);
      until (string(pName)<>'') or (n=ERROR_NONE_MAPPED) or (j>10);
      if string(pName)='' then
        UserName:=ConvertSIDToString(psidOwner)
      else begin
        UserName:=string(pName);
        DomainName:=string(pDomain);
      end;
      Result:=True;
    finally
      Freemem(psd);
    end;
  finally
    CloseHandle(ph);
  end;
end;

function GetProcessWorkingSet(AHandle: THandle): Int64;
var
  pmc: TProcessMemoryCounters;
  cb: Cardinal;
begin
  Result:=0;
  cb:=SizeOf(pmc);
  {$IFNDEF RAD9PLUS}
  if not Assigned(GetProcessMemoryInfo) then
    Exit;
  {$ENDIF}
  if GetProcessMemoryInfo(AHandle,@pmc,cb) then
    Result:=pmc.WorkingSetSize;
end;

function GetProcessPIDWorkingSet(APID: Cardinal): Int64;
var
  h: THandle;
begin
  Result:=0;
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      Result:=GetProcessWorkingSet(h);
    finally
      CloseHandle(h);
    end;
end;

function GetProcessMemoryCounters(AHandle: THandle): TProcessMemoryCountersEx;
var
  cb: Cardinal;
begin
  cb:=SizeOf(Result);
  ResetMemory(Result,cb);
  GetProcessMemoryInfo(AHandle,@Result,cb);
end;

type
  TmwEnumInfo = record
    ProcessID: DWORD;
    HWND: THandle;
  end;

function mwEnumWindowsProc(HWND: THandle; var EI: TmwEnumInfo): Bool; stdcall;
var
  PID: DWORD;
  s: string;
  l: Integer;
  cn :array[0..255] of Char;
begin
  Result:=True;
  GetWindowThreadProcessID(HWND, @PID);
  if (PID<>EI.ProcessID) or not IsWindowVisible(HWND) then
    Exit;
  {if GetWindow(HWND, GW_OWNER)<>0 then
    Exit;}
  GetClassName(HWND,cn,255);
  l:=GetWindowTextLength(HWND);
  if l>0 then begin
    SetLength(s,l);
    GetWindowText(HWND, PChar(s), Length(s)+1);
  end;
  {$IFDEF RAD7PLUS}
  if (GetWindowLongPtr(HWND, GWL_STYLE) and WS_EX_APPWINDOW<>0)
     or (GetWindowLongPtr(HWND, GWL_STYLE) and WS_EX_CONTROLPARENT<>0)
     or (SameText(cn,'TApplication') and (s<>'')) then begin
    EI.HWND:=HWND;
    Result:=False;
  end;
  {$ELSE}
  if (GetWindowLong(HWND, GWL_STYLE) and WS_EX_APPWINDOW<>0)
     or (GetWindowLong(HWND, GWL_STYLE) and WS_EX_CONTROLPARENT<>0)
     or (SameText(cn,'TApplication') and (s<>'')) then begin
    EI.HWND:=HWND;
    Result:=False;
  end;
  {$ENDIF}
end;

function GetProcessWindow(const APID: Cardinal): THandle;
var
  EI: TmwEnumInfo;
begin
  EI.ProcessID:=APID;
  EI.HWND:=0;
  EnumWindows(@mwEnumWindowsProc,{$IFDEF FPC}LParam{$ELSE}Integer{$ENDIF}(@EI));
  Result:=EI.HWND;
end;

type
  TwcEnumInfo = record
    ProcessID: Cardinal;
    Count: Cardinal;
    OnlyVisible: boolean;
  end;

function wcEnumWindowsProc(HWND: THandle; var EI: TwcEnumInfo): Bool; stdcall;
var
  PID: DWORD;
begin
  Result:=True;
  GetWindowThreadProcessID(HWND,@PID);
  if (PID=EI.ProcessID) and (not EI.OnlyVisible or IsWindowVisible(HWND)) then
    Inc(EI.Count);
end;

function GetWindowCount(APID: Cardinal; AOnlyVisible: boolean = True): Cardinal;
var
  EI: TwcEnumInfo;
begin
  EI.ProcessID:=APID;
  EI.Count:=0;
  EI.OnlyVisible:=AOnlyVisible;
  EnumWindows(@wcEnumWindowsProc,LPARAM(@EI));
  Result:=EI.Count;
end;

function GetProcessHandle(APID: Cardinal; AFlags: Cardinal = 0): THandle;
begin
  Result:=OpenProcess(PROCESS_ALL_ACCESS or AFlags,False,APID);
  if (Result=0) or (Result=INVALID_HANDLE_VALUE) then
    Result:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_OPERATION or AFlags,False,APID);
  if (Result=0) or (Result=INVALID_HANDLE_VALUE) then
    Result:=OpenProcess(PROCESS_QUERY_INFORMATION or AFlags,False,APID);
  if (Result=0) or (Result=INVALID_HANDLE_VALUE) then
    Result:=OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or AFlags,False,APID);
end;

function GetThreadHandle(AID: Cardinal; AFlags: Cardinal = 0): THandle;
begin
  Result:=OpenThread(THREAD_ALL_ACCESS or AFlags,False,AID);
  if (Result=0) or (Result=INVALID_HANDLE_VALUE) then
    Result:=OpenThread(THREAD_QUERY_INFORMATION or AFlags,False,AID);
  if (Result=0) or (Result=INVALID_HANDLE_VALUE) then
    Result:=OpenThread(THREAD_QUERY_LIMITED_INFORMATION or AFlags,False,AID);
end;

function IsProcess64bit(APID: Cardinal): Boolean;
var
  ph: THandle;
begin
  Result:=False;
  ph:=GetProcessHandle(APID);
  try
    if Assigned(IsWow64Process) then begin
      if IsWow64Process(ph,IsWow64) then
        Result:=not IsWow64;
    end;
  finally
    CloseHandle(ph);
  end;
end;

function EnablePrivilege(Privilege: string): Boolean;
var
  {$IFDEF FPC}ptp,{$ENDIF}tp: TTOKENPRIVILEGES;
  th: THandle;
  n: Cardinal;
begin
  n:=0;
  tp.PrivilegeCount:=1;
  tp.Privileges[0].Luid:=0;
  tp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
  if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES,th) then begin
    if LookupPrivilegeValue(nil,PChar(Privilege),tp.Privileges[0].Luid) then
      AdjustTokenPrivileges(th,False,tp,sizeof(TTOKENPRIVILEGES),{$IFDEF FPC}ptp{$ELSE}nil{$ENDIF},n);
    CloseHandle(th);
  end;
  Result:=GetLastError=ERROR_SUCCESS;
end;

function DisablePrivileges: Boolean;
var
  {$IFDEF FPC}ptp,{$ENDIF}tp: TOKEN_PRIVILEGES;
  th: THandle;
  n: Cardinal;
begin
  n:=0;
  tp.PrivilegeCount:=1;
  tp.Privileges[0].Luid:=0;
  tp.Privileges[0].Attributes:=0;
  if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES,th) then begin
    AdjustTokenPrivileges(th,True,tp,sizeof(TOKEN_PRIVILEGES),{$IFDEF FPC}ptp{$ELSE}nil{$ENDIF},n);
    CloseHandle(th);
  end;
  Result:=GetLastError=ERROR_SUCCESS;
end;

function DisablePrivilege(Privilege: string): Boolean;
var
  {$IFDEF FPC}ptp,{$ENDIF}tp: TOKEN_PRIVILEGES;
  th: THandle;
  n: Cardinal;
begin
  n:=0;
  tp.PrivilegeCount:=1;
  tp.Privileges[0].Luid:=0;
  tp.Privileges[0].Attributes:=0;
  if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES,th) then begin
    if LookupPrivilegeValue(nil,PChar(Privilege),tp.Privileges[0].Luid) then
      AdjustTokenPrivileges(th,True,tp,sizeof(TOKEN_PRIVILEGES),{$IFDEF FPC}ptp{$ELSE}nil{$ENDIF},n);
    CloseHandle(th);
  end;
  Result:=GetLastError=ERROR_SUCCESS;
end;

function AppIsResponding(AHandle: THandle): Boolean;
const
  TIMEOUT = 50;
var
  Res: {$IFDEF NATIVEINT}PDWORD_PTR{$ELSE}Cardinal{$ENDIF};
begin
  {$IFDEF NATIVEINT}new(res);{$ENDIF}
  if AHandle<>0 then
    Result:=SendMessageTimeout(AHandle,WM_NULL,0,0,SMTO_NORMAL or SMTO_ABORTIFHUNG,TIMEOUT,Res)<>0
  else
    Result:=False;
  {$IFDEF NATIVEINT}dispose(res);{$ENDIF}
end;

function ConvertSIDToString(ASID: Pointer): string;
var
  i: integer;
  SIDAuth: PSIDIdentifierAuthority;
  SIDSubAuth: Cardinal;
  SIDSubAuthCount: Byte;
begin
  Result:='';
  if not IsValidSID(ASID) then
    Exit;
  Result:='S-1-';
  SIDAuth:=GetSidIdentifierAuthority(ASID);
  SIDSubAuthCount:=GetSidSubAuthorityCount(ASID)^;
  for i:=0 to 5 do
    if SIDAuth^.Value[i]<>0 then
      Result:=Result+IntToStr(SIDAuth^.Value[i]);
  for i:=0 to SIDSubAuthCount-1 do begin
    SIDSubAuth:=GetSidSubAuthority(ASID,i)^;
    Result:=Result+'-'+IntToStr(SIDSubAuth);
  end;
end;

function ConvertStringToSID(ASID: string): PSID;
var
  sa: TSIDIdentifierAuthority;
  s: string;
  i,p: integer;
  sub: array[0..7] of Cardinal;
  d: Cardinal;
begin
  ResetMemory(sa,SizeOf(sa));
  ResetMemory(sub,SizeOf(sub));
  s:=ASID+'-';
  p:=Pos('-',s);
  Delete(s,1,p);
  p:=Pos('-',s);
  Delete(s,1,p);
  p:=Pos('-',s);
  d:=StrToIntDef(Copy(s,1,p-1),0);
  Delete(s,1,p);
  sa.Value[5]:=Byte(d) and $000000FF;
  sa.Value[4]:=(Byte(d) and $0000FF00) shr 8;
  sa.Value[3]:=(Byte(d) and $00FF0000) shr 16;
  sa.Value[2]:=(Byte(d) and $FF000000) shr 24;
  i:=0;
  p:=Pos('-',s);
  while p>0 do begin
    sub[i]:=StrToIntDef(Copy(s,1,p-1),0);
    Delete(s,1,p);
    Inc(i);
    p:=Pos('-',s);
  end;
  if not AllocateAndInitializeSid(sa,i,sub[0],sub[1],sub[2],sub[3],sub[4],sub[5],sub[6],sub[7],Result) then
    Result:=nil;
end;

function GetSIDFromAccount(AMachine, AName: string): string;
var
  SID: Pointer;
  szDomain: PChar;
  cbDomain, cbSID: Cardinal;
  NameUse: {$IFDEF FPC}SID_NAME_USE{$ELSE}Cardinal{$ENDIF};
begin
  Result:='';
  cbDomain:=0;
  cbSID:=0;
  szDomain:=nil;
  SID:=nil;
  LookupAccountName(PChar(AMachine),PChar(AName),SID,cbSID,szDomain,cbDomain,NameUse);
  szDomain:=StrAlloc(cbDomain);
  SID:=AllocMem(cbSID);
  if LookupAccountName(PChar(AMachine),PChar(AName),SID,cbSID,szDomain,cbDomain,NameUse) then
    Result:=ConvertSIDToString(SID);
  StrDispose(szDomain);
  Freemem(SID);
end;

function GetAccountFromSID(ASID: PSID; ASystemName: string = ''): string;
var
  pName,
  pDomain: array[0..255] of Char;
  j,n: Cardinal;
  SIDType: {$IFDEF FPC}SID_NAME_USE{$ELSE}Cardinal{$ENDIF};
begin
  Result:='';
  ResetMemory(pname,SizeOf(pName));
  ResetMemory(pDomain,SizeOf(pDomain));
  j:=0;
  repeat
    LookupAccountSID(PChar(ASystemName),ASID,PChar(@pName),n,PChar(@pDomain),n,SIDType);
    n:=GetLastError;
    Inc(j);
  until (string(pName)<>'') or (n=ERROR_NONE_MAPPED) or (j>10);
  Result:=pName;
end;

function GetUserObjectSID(AObj: Cardinal): string;
var
  c,sz,n: Cardinal;
  buf: Pointer;
  ec: Integer;
begin
  sz:=0;
  Result:='';
  c:=OWNER_SECURITY_INFORMATION; // UOI_USER_SID;
  GetUserObjectSecurity(AObj,c,buf,sz,n);
  ec:=GetLastError;
  if (ec=ERROR_INSUFFICIENT_BUFFER) or (ec=0) then begin
    if n>sz then begin
      sz:=n;
      ReallocMem(buf,n);
      GetUserObjectsecurity(AObj,c,buf,sz,n);
      ec:=GetLastError;
    end;
  end;
  if ec=0 then
    Result:=ConvertSIDToString(buf); //GetAccountFromSID(PSID(buf));
end;


const
  PIDDigits: array[0..23] of AnsiChar = 'BCDFGHJKMPQRTVWXY2346789';

function DecodeProductKey(PID: TDigitalProductID): string;
var
  i,n: integer;
  hi,low,Value: Cardinal;
begin
  Result:='';
  for i:=29 downto 1 do begin
    if (i) mod 6 = 0 then
      Result:='-'+Result
    else begin
      hi:=0;
      for n:=14 downto 0 do begin
        low:=PID[n];
        value:=hi shl 8;
        value:=value or low;
        PID[n]:=value div 24;
        hi:=value mod 24;
      end;
      Result:=string(PIDDigits[value mod 24])+Result;
    end;
  end;
end;

function ReadDPID(AReg: TRegistry; AValueName: string): string;
var
  i,n,s,fl: Integer;
  p: array[0..2047] of Byte;
  PID: TDigitalProductID;
  w8: Cardinal;
  r: string;
begin
  with AReg do begin
    n:=GetDataSize(AValueName);
    if n>SizeOf(p) then
      Exit;
    ReadBinarydata(AValueName,p,n);
    if n>164 then
      s:=$328
    else
      s:=$34;
    w8:=(p[s+14] shr 3) and 1;
    p[s+14]:=(p[s+14] and $F7) or ((w8 and 2) shl 2);
    Move(p[s],PID,15);
    r:=DecodeProductKey(PID);
    if w8>0 then begin
      n:=Length(r);
      fl:=1;
      for i:=1 to n do
        if SameText(r[1],string(PIDDigits[i])) then begin
          fl:=i;
          break;
        end;
      r:=StringReplace(r,'-','',[rfReplaceAll,rfIgnoreCase]);
      Delete(r,1,1);
      if fl=1 then
        r:='N'+r
      else
        r:=Copy(r,1,fl)+'N'+Copy(r,fl+1,255);
      n:=Length(r);
      Result:='';
      for i:=n downto 1 do begin
        if (i mod 5 = 0) and (Result<>'') then
          Result:='-'+Result;
        Result:=r[i]+Result;
      end;
    end else
      Result:=r;
  end;
end;

procedure SeparateHotKey(HotKey: Word; var Modifiers, Key: Word);
const
  VK2_SHIFT   =  32;
  VK2_CONTROL =  64;
  VK2_ALT     = 128;
var
  Virtuals: Integer;
  V: Word;
  x: Byte;
begin
  Key:=Byte(HotKey);

  x:=HotKey shr 8;
  V:=0;
  Virtuals:=x;

  if Virtuals >= VK2_ALT then
  begin
    Virtuals:=Virtuals - VK2_ALT;
    V:=V + MOD_ALT;
  end;

  if Virtuals >= VK2_CONTROL then
  begin
    Virtuals:=Virtuals - VK2_CONTROL;
    V:=V + MOD_CONTROL;
  end;

  if Virtuals >= VK2_SHIFT then
  begin
    V:=V + MOD_SHIFT;
  end;
  Modifiers:=V;
end;


function AssignHotkey(Handle: HWND; HotKey: TShortCut; KeyIdx: Word): Boolean;
var
  Modifiers, Key: Word;
begin
  UnregisterHotkey(Handle,KeyIdx);
  SeparateHotKey(HotKey,Modifiers,Key);
  Result:=RegisterHotkey(Handle,KeyIdx,Modifiers,Key);
end;


function ValidHotkey(Handle: HWND; HotKey: TShortCut; KeyIdx: Word): Boolean;
var
  V1, V2: Word;
begin
  SeparateHotKey(HotKey, V1, V2);
  Result:=RegisterHotkey(Handle, KeyIdx, V1, V2);
  UnregisterHotkey(Handle, KeyIdx);
end;

procedure ClearKeyBoardBuffer;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE or PM_NOYIELD) do;
end;


function GetLastFilename(AFilename: string): string;
var
  i: Integer;
  s,e: string;
  fi: TSearchrec;
begin
  i:=0;
  e:=ExtractFileExt(AFilename);
  s:=ExtractFilename(AFilename);
  s:=Copy(s,1,Pos('.',s)-1);
  while FindFirst(ExtractFilePath(AFilename)+Format('%s%2.2d%s',[s,i,e]),faArchive,fi)=0 do
    Inc(i);
  Result:=ExtractFilePath(AFilename)+Format('%s%2.2d%s',[s,i,e]);
  FindClose(fi);
end;

function VarToFloat(Source: Variant): Double;
begin
  try
    Result:=Source;
  except
    Result:=0;
  end;
end;

function VarToInt(Source: Variant): Integer;
begin
  try
    Result:=Source;
  except
    Result:=0;
  end;
end;

function VarToInt64(Source: Variant): Int64;
begin
  try
    Result:=Source;
  except
    Result:=0;
  end;
end;

function VarToBool(Source: Variant): boolean;
begin
  try
    Result:=VarAsType(Source, varBoolean);
  except
    Result:=False;
  end;
end;

function VarToDT(Source: Variant): Tdatetime;
begin
  try
    Result:=VarToDatetime(Source);
  except
    Result:=0;
  end;
end;

function IntToBin(AValue: Int64; ANumBits: word = 64): string;
begin
  Result:='';
  case ANumBits of
    32 :AValue:=dword(AValue);
    16 :AValue:=Word(AValue);
     8 :AValue:=Byte(AValue);
  end;
  while AValue<>0 do begin
    Result:=char(48+(AValue and 1))+Result;
    AValue:=AValue shr 1;
  end;

  if Result='' then
    Result:='0';
end;

function BinToInt(AValue: String): Int64;

  function Pow(i, k: Integer): Integer;
  var
    j, Count: Integer;
  begin
    if k>0 then j:=2
      else j:=1;
    for Count:=1 to k-1 do
      j:=j*2;
    Result:=j;
  end;

var
  l,i: Integer;
begin
  l:=Length(AValue);
  Result:=0;
  for i:=1 to l do
    if (AValue[i]='0') or (AValue[i]='1') then
      Result:=Result+Pow(2,l-i)*StrToInt(AValue[i])
    else begin
      Result:=0;
      Break;
    end;
end;

function IntToRoman(AValue: int64): string;
const
  Arabics: Array[1..13] of Integer = (1,4,5,9,10,40,50,90,100,400,500,900,1000);
  Romans: Array[1..13] of String = ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M');
var
  i: Integer;
begin
  for i:= 13 downto 1 do
    while (AValue >= Arabics[i]) do begin
      AValue:=AValue-Arabics[i];
      Result:=Result+Romans[i];
    end;
end;

function RomanToInt(const AValue: string): int64;
const
  Romans = 'IVXLCDMvxlcdm?!#' ;
  Arabics: array [0..8] of integer = (0,1,10,100,1000,10000,100000,1000000,10000000);
  OneFive : array [boolean] of byte = (1,5);
var
  newValue,oldValue: integer;
  i,p : byte;
begin
  Result:=0;
  oldValue:=0;
  for i:=Length(AValue) downto 1 do begin
    p:=Succ(Pos(AValue[i],Romans));
    newValue:=OneFive[Odd(p)]*Arabics[p div 2];
    if newValue=0 then begin
      Result:=-1;
      Exit;
    end;
    if newValue<oldValue then
      newValue:=-newValue;
      Inc(Result,newValue);
      oldValue:=newValue
    end;
end;

function DatetimeToVar(ADT: TDateTime): Variant;
begin
  if ADT=0 then
    Result:=null
  else
    Result:=ADT;
end;

procedure MultiWideStrFromBuf(Buffer: array of Byte; Len: Integer; var List: TStringList);
var
  s: string;
  l: integer;
begin
  List.Clear;
  s:=WideCharToString(PWideChar(@Buffer));
  List.Add(s);
  l:=Length(s)*2+2;
  while (l<Len) and (s<>'') do begin
    Move(Buffer[Length(s)*2+2],Buffer,Len-Length(s)*2+2);
    s:=WideCharToString(PWideChar(@Buffer));
    l:=l+Length(s)*2+2;
    if s<>'' then
      List.Add(s);
  end;
end;

procedure MultiStrFromBuf(Buffer: array of Byte; Len: Integer; var List: TStringList);
var
  s: string;
  l: Integer;
begin
  List.Clear;
  s:=string(PAnsiChar(@Buffer));
  List.Add(s);
  l:=Length(s)*2+2;
  while (l<Len) and (s<>'') do begin
    Move(Buffer[Length(s)*2+2],Buffer,Len-Length(s)*2+2);
    s:=string(PAnsiChar(@Buffer));
    l:=l+Length(s)*2+2;
    if s<>'' then
      List.Add(s);
  end;
end;

function ReadValueAsString(AReg: TRegistry; const Value: string): string;
var
  Data: array[0..1024] of Char;
  vi: TRegDataInfo;
begin
 Result:='';
 with AReg do
   try
     if ValueExists(Value) then begin
       GetDataInfo(Value,vi);
       case vi.RegData of
         rdString: Result:=ReadString(Value);
         rdExpandString: begin
           Result:=ExpandEnvVars(ReadString(Value));
         end;
         rdInteger: Result:=IntToStr(ReadInteger(Value));
         rdBinary: begin
           Result:='';
           if vi.DataSize>-1 then begin
             ReadBinaryData(Value,Data,sizeof(Data));
             Result:=PChar(@Data);
           end;
         end;
       end;
     end;
   finally
   end;
end;

function GetObjectFullName(Sender: TObject): string;
var
  s: string;
begin
  Result:='';
  while Sender<>nil do begin
    s:='';
    if Sender is TComponent then
      s:=TComponent(Sender).Name;
    s:=Format('(%s: %s).',[s,Sender.ClassName]);
    if Sender is TComponent then
      Sender:=TComponent(Sender).Owner
    else
      Sender:=nil;
    Result:=s+Result;
  end;
  if Result<>'' then
    SetLength(Result,Length(Result)-1);
end;

{$IFNDEF FPC}
function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX
        JE      @@1
        SUB     EAX, $1000
@@1:
end;

procedure ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
var
  Info :TMemoryBasicInformation;
  Temp,ModName :array[0..MAX_PATH] of Char;
begin
  VirtualQuery(ExceptAddr,Info,SizeOf(Info));
  if (Info.State<>MEM_COMMIT) or ({$IFDEF RAD9PLUS}WinApi.{$ENDIF}Windows.GetModuleFilename(THandle(Info.AllocationBase),Temp,SizeOf(Temp))=0) then begin
    {$IFDEF RAD9PLUS}WinApi.{$ENDIF}Windows.GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    LogicalAddress:=ConvertAddr(LogicalAddress);
  end else
    {$IFDEF WIN64}NativeInt{$ELSE}integer{$ENDIF}(LogicalAddress):={$IFDEF WIN64}NativeInt{$ELSE}integer{$ENDIF}(LogicalAddress)-Integer(Info.AllocationBase);
  StrLCopy(ModName,AnsiStrRScan(Temp,'\')+1,SizeOf(ModName)-1);
  ModuleName:=StrPas(ModName);
end;
{$ENDIF}

function CorrectFilename(fn: string; subst: Char = #32): string;
var
  i,l: Cardinal;
begin
  Result:=fn;
  l:=Length(Result);
  for i:=1 to l do
    if {$IFDEF UNICODE}
       CharInSet(Result[i],[#33, #34, #37, #39, #42, #46, #47, #58, #62, #63, #92, '|'])
       {$ELSE}
       Result[i] in [#33, #34, #37, #39, #42, #46, #47, #58, #62, #63, #92, '|']
       {$ENDIF} then
      Result[i]:=subst;
end;

procedure StartTimer;
begin
  InternalTimer:=GetTickCount64;
end;

function StopTimer: comp;
begin
  Result:=GetTickCount64-InternalTimer;
end;

function SwapEndian(const Value: Longword): LongWord;
begin
  {$IFDEF FPC}
  Result:=swap(Value);
  {$ELSE}
  Result:=swap(Value shr 16) or (longword(swap(Value and $FFFF)) shl 16);
  {$ENDIF}
end;

function SwapEndian(const Value: Int64): Int64;
asm
{$IFDEF CPUX64}
  mov    rax, rcx
  bswap  rax
{$ELSE}
  mov    edx, [ebp+$08]
  mov    eax, [ebp+$0c]
  bswap  edx
  bswap  eax
{$ENDIF}
end;

function UNIX32ToDatetime(ADate: Cardinal): TDateTime;
begin
  Result:=Int(Encodedate(1970,1,1));
  Result:=((Result*SecsPerDay)+ADate)/SecsPerDay;
end;

function Complement(Value: Cardinal): Cardinal;
begin
   Result:=not(Value)+1;
end;

function NumberInSet(const AValue: Integer; const ASet: array of Integer): boolean;
{$IFDEF BDS3PLUS}
var
  v: Integer;
begin
  Result:=False;
  for v in ASet do
    if AValue=v then begin
      Result:=True;
      Break;
    end;
end;
{$ELSE}
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to High(ASet) do
    if AValue=ASet[i] then begin
      Result:=True;
      Break;
    end;
end;
{$ENDIF}

function GetLogicalDisks(OnlyWithMedia: Boolean = False): string;
var
  i,n :integer;
  buf :PChar;
  em: Cardinal;
begin
  buf:=stralloc(255);
  em:=SetErrorMode(SEM_FailCriticalErrors or SEM_NoOpenFileErrorBox);
  try
    n:=GetLogicalDriveStrings(255,buf);
  finally
    SetErrorMode(em);
  end;
  Result:='';
  for i:=0 to n do
    if buf[i]<>#0 then begin
      if (ord(buf[i]) in [$41..$5a]) or (ord(buf[i]) in [$61..$7a]) then begin
        if not OnlyWithMedia or GetMediaPresent(buf[i]+':') then
          Result:=Result+upcase(buf[i])
      end;
    end else
      if buf[i+1]=#0 then
        break;
  strdispose(buf);
end;

function GetRemovableDisks: string;
var
  s: string;
  i: Integer;
begin
  Result:='';
  s:=GetLogicalDisks;
  for i:=1 to Length(s) do
    if GetDriveType(PChar(Copy(s,i,1)+':'))=DRIVE_REMOVABLE then
      Result:=Result+s[i];
end;

function NetResourceConnect(const AResource, AUser, APassword: string): Integer;
var
  n : NETRESOURCE;
begin
  Result:=0;
  if Pos('\\',AResource)<>1 then
    Exit;
  n.dwScope:=RESOURCE_GLOBALNET;
  n.dwType:=RESOURCETYPE_DISK;
  n.dwDisplayType:=RESOURCEDISPLAYTYPE_GENERIC;
  n.dwUsage:=RESOURCEUSAGE_CONNECTABLE;
  n.lpLocalName:='';
  n.lpRemoteName:=PChar(ExcludeTrailingPathDelimiter(AResource));
  n.lpComment:='';
  n.lpProvider:='';
  Result:=WNetAddConnection2(n,PChar(APassword),PChar(AUser),0);
end;

function NetResourceDisconnect(const AResource: string): Boolean;
var
  e: Cardinal;
begin
  Result:=True;
  if Pos('\\',AResource)<>1 then
    Exit;
  e:=WNetCancelConnection2(PChar(ExcludeTrailingPathDelimiter(AResource)),CONNECT_UPDATE_PROFILE,True);
  Result:=e=NO_ERROR;
end;

procedure ScanNetResources(AList: TStrings);
var
  dwResult: Cardinal;
  dwResultEnum: Cardinal;
  hEnum: THandle;
  cbBuffer: Cardinal;
  cEntries: Cardinal;
  lpnrLocal: PNETRESOURCE;
  PtrResource: PNetResource;
  i: Cardinal;
  p: PChar;
begin
  lpnrLocal:=nil;
  AList.Clear;
  cbBuffer:=16384;
  cEntries:=Cardinal(-1);
  dwResult:=WNetOpenEnum(RESOURCE_CONNECTED,
                         RESOURCETYPE_DISK,
                         RESOURCEUSAGE_CONNECTABLE,
                         nil,
                         hEnum);
  if dwResult<>NO_ERROR then
    Exit;
  try
  repeat
    GetMem(lpnrLocal, cbBuffer);
    dwResultEnum:=WNetEnumResource(hEnum,cEntries,lpnrLocal,cbBuffer);
    if dwResultEnum=NO_ERROR then begin
      for i:=0 to cEntries-1 do begin
        PtrResource:=PNETRESOURCE(PAnsiChar(lpnrLocal)+i*SizeOf(lpnrLocal^));
        if PtrResource^.dwDisplayType=RESOURCEDISPLAYTYPE_SHARE then begin
          p:=PtrResource^.lpRemoteName;
          AList.Add(p);
        end;
      end;
    end else if dwResultEnum<>ERROR_NO_MORE_ITEMS then
      Break;
  until dwResultEnum = ERROR_NO_MORE_ITEMS;
  finally
    if Assigned(lpnrLocal) then
      FreeMem(lpnrLocal);
    WNetCloseEnum(hEnum);
  end;
end;

{$IFNDEF FPC}
function _IsVMWARE;
begin
{$IFDEF WIN64}
  Result:=False;
{$ELSE}
  Result:=True;
  try
    asm

      push   edx
      push   ecx
      push   ebx

      mov    eax, 'VMXh'
      mov    ebx, 0 // any value but not the MAGIC VALUE
      mov    ecx, 10 // get VMWare version
      mov    edx, 'VX' // port number

      in     eax, dx // read port
                     // on return EAX returns the VERSION
      cmp    ebx, 'VMXh' // is it a reply from VMWare?
      setz   [Result] // set return value

      pop    ebx
      pop    ecx
      pop    edx
    end;
  except
    Result:=False;
  end;
{$ENDIF}
end;

function _IsVPC: Boolean;
begin
  Result:=False;
{$IFDEF WIN32}
  try
    asm
      mov eax, 1
      db 0fh
      aas
      pop es
      or eax, edi
      inc ebp
      cld
      dd 0ffffffffh
    end;
  except
    Result:=False;
  end;
{$ENDIF}
end;
{$ENDIF}

function IsVirtualMachine(ASignature: string): Boolean;
var
  i,c: Integer;
begin
  Result:=False;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\Disk\Enum') then begin
        if ValueExists('Count') then begin
          c:=ReadInteger('Count');
          for i:=0 to c-1 do
            if ValueExists(IntToStr(i)) then
              if PosText(ASignature,ReadString(IntToStr(i)))>0 then begin
                Result:=True;
                Break;
              end;
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function IsRemoteSession: boolean;
begin
  Result:=GetSystemMetrics(SM_REMOTESESSION)>0;
end;

function IsVPC: Boolean;
begin
  Result:=IsVirtualMachine('VIRTUAL_DISK');
end;

function IsVMWare: Boolean;
begin
  Result:=IsVirtualMachine('VMWARE');   //VMWARE_VIRTUAL
end;

function IsVBOX: Boolean;
begin
  Result:=IsVirtualMachine('VBOX');
end;

function IsQEMU: Boolean;
begin
  Result:=IsVirtualMachine('QEMU');
end;

function IsCitrix: Boolean;
begin
  Result:=False;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon') then begin
        if ValueExists('GinaDLL') then
          Result:=Pos('CTXGINA',Uppercase(ReadString('GinaDLL')))>0;
        CloseKey;
      end;
      if not Result then begin
        Rootkey:=HKEY_CURRENT_USER;
        Result:=KeyExists('Software\Citrix\GoToAssist\ConnectionInfo\LastGood\0000');
      end;
    finally
      Free;
    end;
end;

function GetSession: TSessionTypes;
begin
  Result:=[];
  if IsVMWARE then
    Result:=Result+[stVMWare]
  else if IsVBOX then
    Result:=Result+[stVBOX]
  else if IsVPC then
    Result:=Result+[stVPC]
  else if IsQEMU then
    Result:=Result+[stQEMU];

  if IsCitrix then
    Result:=Result+[stCitrix];
  if IsRemoteSession then
    Result:=Result+[stTerminal]
  else
    Result:=Result+[stLocal];
end;

function GetSessionStr(ASession: TSessionTypes): string;
begin
  Result:='';
  if stLocal in ASession then
    Result:=Result+cSessionType[stLocal]+'+';
  if stTerminal in ASession then
    Result:=Result+cSessionType[stTerminal]+'+';
  if stCitrix in ASession then
    Result:=Result+cSessionType[stCitrix]+'+';
  if stVMWare in ASession then
    Result:=Result+cSessionType[stVMWare]+'+';
  if stVPC in ASession then
    Result:=Result+cSessionType[stVPC]+'+';
  if stVBOX in ASession then
    Result:=Result+cSessionType[stVBOX]+'+';
  if stQEMU in ASession then
    Result:=Result+cSessionType[stQEMU]+'+';
  SetLength(Result,Length(Result)-1);
end;

function SessionTypesAsInt(A: TSessionTypes): Cardinal;
var
  i: TSessionType;
begin
  Result:=0;
  for i:=Low(TSessionType) to High(TSessionType) do
    if i in A then
      Result:=Result or (1 shl Integer(i));
end;

function IntAsSessionTypes(A: Cardinal): TSessionTypes;
var
  i: TSessionType;
begin
  Result:=[];
  for i:=Low(TSessionType) to High(TSessionType) do
    if (A and (1 shl Integer(i)))<>0 then
      Result:=Result+[i];
end;

function IsPW: Boolean;
begin
  Result:=IsVirtualMachine('VIRTUAL HDD');
end;

function IsUAC: Boolean;
const
  rkPolicies = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System';
    rvUAC = 'EnableLUA';
begin
  Result:=False;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkPolicies,False) then begin
        if ValueExists(rvUAC) then
          Result:=ReadInteger(rvUAC)=1;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function IsWinPE: Boolean;
const
  rkWinPE = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion\WinPE';
begin
  Result:=False;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkWinPE,False) then begin
        Result:=ValueExists('Version');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetWinPEVersion: string;
const
  rkWinPE = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion\WinPE';
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkWinPE,False) then begin
        Result:=ReadString('Version');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure UpdateWinVersion;
type
  pfnRtlGetVersion = function(var {$IFDEF RAD6PLUS}RTL_OSVERSIONINFOEXW{$ELSE}TOSVersionInfoEx{$ENDIF}): Longint; stdcall;
var
  Buffer: Pointer;
  ver: {$IFDEF RAD6PLUS}RTL_OSVERSIONINFOEXW{$ELSE}TOSVersionInfoEx{$ENDIF};
  RtlGetVersion: pfnRtlGetVersion;
begin
  Buffer:=nil;
  RtlGetVersion:=pfnRtlGetVersion(GetProcAddress(GetModuleHandle('ntdll.dll'), 'RtlGetVersion'));
  if Assigned(RtlGetVersion) then begin
    ResetMemory(ver,SizeOf(ver));
    ver.dwOSVersionInfoSize:=SizeOf(ver);
    if RtlGetVersion(ver)=0 then begin
      OSVIX.dwMajorVersion:=ver.dwMajorVersion;
      OSVIX.dwMinorVersion:=ver.dwMinorVersion;
      OSVIX.dwBuildNumber:=ver.dwBuildNumber;
      OSVIX.dwPlatformId:=ver.dwPlatformId;
      StrCopy(OSVIX.szCSDVersion,ver.szCSDVersion);
    end;
  end else
    if NetServerGetInfo(nil,101,Buffer)=NO_ERROR then
      try
        OSVIX.dwMajorVersion:=PServerInfo101(Buffer)^.sv101_version_major;
        OSVIX.dwMinorVersion:=PServerInfo101(Buffer)^.sv101_version_minor;
      finally
        NetApiBufferFree(Buffer);
      end;
end;

function IsUACEnabled: Boolean;
var
  hToken: THandle;
  tet: TOKEN_ELEVATION_TYPE;
  dwSize: DWORD;
begin
  OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,hToken);
  try
    GetTokenInformation(hToken,{TokenElevationType}TTokenInformationClass(18),@tet,SizeOf(tet)*4,dwSize);
  finally
    CloseHandle(hToken);
  end;
  Result:=tet<>TokenElevationTypeDefault;
end;

function IsElevated: Boolean;
var
  hToken: THandle;
  Elevation: DWord;
  dwSize: DWORD;
begin
  OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,hToken);
  try
    dwSize:=0;
    GetTokenInformation(hToken,TTokenInformationClass(20{TokenElevation}),@Elevation,SizeOf(Elevation),dwSize);
  finally
    CloseHandle(hToken);
  end;
  Result:=Elevation<>0;
end;

function IsVirtualized: Boolean;
var
  hToken: THandle;
  n,dwSize: DWORD;
begin
  Result:=False;
  OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,hToken);
  try
    GetTokenInformation(hToken,TTokenInformationClass({TokenVirtualizationAllowed}23),@n,SizeOf(n),dwSize);
    if n<>0 then begin
      GetTokenInformation(hToken,TTokenInformationClass({TokenVirtualizationEnabled}24),@n,SizeOf(n),dwSize);
      Result:=n<>0;
    end;
  finally
    CloseHandle(hToken);
  end;
end;

procedure GetDebugPrivs;
var
  hToken: THandle;
  {$IFDEF FPC}ptp,{$ENDIF}tkp: TTokenPrivileges;
  retval: dword;
begin
  if (OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,hToken)) then begin
    LookupPrivilegeValue(nil,SE_DEBUG_NAME,tkp.Privileges[0].Luid);
    tkp.PrivilegeCount:=1;
    tkp.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    AdjustTokenPrivileges(hToken,False,tkp,0,{$IFDEF FPC}ptp{$ELSE}nil{$ENDIF},retval);
  end;
end;

function IsEqualTZ(ATZ1, ATZ2: TTimeZoneInformation): boolean;
begin
  Result:=(ATZ1.Bias=ATZ2.Bias) and
          (ATZ1.StandardBias=ATZ2.StandardBias) and
          //(ATZ1.DaylightBias=ATZ2.DaylightBias) and
          (CompareSysTime(ATZ1.StandardDate,ATZ2.StandardDate)=0) and
          //(CompareSysTime(ATZ1.DaylightDate,ATZ2.DaylightDate)=0) and
          (WideCharToString(ATZ1.StandardName)=WideCharToString(ATZ2.StandardName));
          //and (WideCharToString(ATZ1.DaylightName)=WideCharToString(ATZ2.DaylightName));
end;


function GetTimeZone(out ATZ: TTimeZoneInformation): string;
var
  TZKey: string;
  RTZ: TRegTimeZoneInfo;
  RegTZ: TTimeZoneInformation;
  i: Word;
  sl: TStringList;
  s: string;
const
  rkTZKN = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\TimeZoneInformation';
  rvTZKN = 'TimeZoneKeyName';
  rkTimeZones = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones';
  rvTimeZone = 'StandardName';
begin
  GetTimeZoneInformation(ATZ);
  Result:=ATZ.StandardName;
  s:=Result;
  sl:=TStringList.Create;
  with OpenRegistryReadOnly do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkTZKN,False) then begin
      if ValueExists(rvTZKN) then
        s:=ReadString(rvTZKN);
      CloseKey;
    end;
    TZKey:=rkTimeZones;
    if OpenKey(TZKey,False) then begin
      GetKeyNames(sl);
      CloseKey;
      for i:=0 to sl.Count-1 do
        if OpenKey(TZKey+'\'+sl[i],False) then begin
          if (GetDataSize('TZI')=SizeOf(RTZ)) then begin
            ReadBinaryData('TZI',RTZ,SizeOf(RTZ));
            StringToWideChar(ReadString('Std'),PWideChar(@RegTZ.StandardName),SizeOf(RegTZ.StandardName) div SizeOf(WideChar));
            StringToWideChar(ReadString('Dlt'),PWideChar(@RegTZ.DaylightName),SizeOf(RegTZ.DaylightName) div SizeOf(WideChar));
            RegTZ.Bias:=RTZ.Bias;
            RegTZ.StandardBias:=RTZ.StandardBias;
            RegTZ.DaylightBias:=RTZ.DaylightBias;
            RegTZ.StandardDate:=RTZ.StandardDate;
            RegTZ.DaylightDate:=RTZ.DaylightDate;
            if IsEqualTZ(ATZ,RegTZ) and AnsiSameText(s,sl[i]) then begin
              Result:=ReadString('Display');
              ATZ.StandardName:=RegTZ.StandardName;
              ATZ.DaylightName:=RegTZ.DaylightName;
              Break;
            end;
          end;
          CloseKey;
        end;
    end;
    Free;
  end;
  sl.Free;
end;

function GetTZDaylightSavingInfoForYear(TZ: TTimeZoneInformation; year: Word; var DaylightDate, StandardDate: TDateTime; var DaylightBias, StandardBias: longint): boolean;
begin
  Result:=false;
  try
    if (TZ.DaylightDate.wMonth <> 0) and
       (TZ.StandardDate.wMonth <> 0) then begin
      DaylightDate:=DSTDate2Date(TZ.DaylightDate,year);
      StandardDate:=DSTDate2Date(TZ.StandardDate,year);
      DaylightBias:=TZ.Bias+TZ.DaylightBias;
      StandardBias:=TZ.Bias+TZ.StandardBias;
      Result:=true;
    end;
  except
  end;
end;

procedure SaveResource(AName,AFilename: string);
var
  rs: TResourceStream;
  rcd: TStringList;
begin
  rs:=TResourceStream.Create(hinstance,AName,RT_RCDATA);
  rcd:=TStringList.Create;
  try
    rcd.LoadFromStream(rs);
    rcd.SaveToFile(AFilename);
  finally
    rs.Free;
    rcd.Free;
  end;
end;

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then
  begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        raise Exception.CreateFmt('Library %s not found.',[ModuleName]);
    end;
    P:=GetProcAddress(ModuleHandle, PChar(ProcName));
    if not Assigned(P) then
      raise Exception.CreateFmt('Procedure %s in library %s not found.', [ProcName,ModuleName]);
  end;
end;

function GetDeviceHandle(AName: string): THandle;
var
  errorMode: Cardinal;
begin
  errorMode:=SetErrorMode(SEM_FailCriticalErrors);
  try
    Result:=CreateFile(PChar(AName),
                     GENERIC_READ or GENERIC_WRITE,
                     FILE_SHARE_READ or FILE_SHARE_WRITE,
                     nil,
                     OPEN_EXISTING,
                     0,//FILE_ATTRIBUTE_NORMAL,
                     0);
    if Result=INVALID_HANDLE_VALUE then
      Result:=CreateFile(PChar(AName),
                     0,
                     FILE_SHARE_READ or FILE_SHARE_WRITE,
                     nil,
                     OPEN_EXISTING,
                     0,
                     0);
  finally
    SetErrorMode(errorMode);
  end;
end;

procedure ScanNetwork(lpnr: PNETRESOURCE; AList: TStrings);
var
  dwResult: Cardinal;
  dwResultEnum: Cardinal;
  hEnum: THandle;
  cbBuffer: Cardinal;             // 16K is a good size
  cEntries: Cardinal;             // enumerate all possible entries
  lpnrLocal: PNETRESOURCE;      // pointer to enumerated structures
  PtrResource: PNetResource;
  i: Cardinal;
  p: PChar;
  s: string;
begin
  lpnrLocal:=nil;
  AList.Clear;
  cbBuffer:=16384;
  cEntries:=Cardinal(-1);
  dwResult:=WNetOpenEnum(RESOURCE_GLOBALNET,
                         RESOURCETYPE_ANY,
                         RESOURCEUSAGE_ALL, // enumerate all resources
                         lpnr,                    // NULL first time this function is called
                         hEnum);                  // handle to resource
  if dwResult<>NO_ERROR then
    Exit;
  try
  repeat
    // Allocate memory for NETRESOURCE structures.
    GetMem(lpnrLocal, cbBuffer);
    dwResultEnum:=WNetEnumResource(hEnum,       // resource handle
                                   cEntries,    // defined locally as 0xFFFFFFFF
                                   lpnrLocal,   // LPNETRESOURCE
                                   cbBuffer);   // buffer size
    if dwResultEnum=NO_ERROR then begin
      for i:=0 to cEntries-1 do begin
        PtrResource:=PNETRESOURCE(PAnsiChar(lpnrLocal)+i*SizeOf(lpnrLocal^));
        if PtrResource^.dwDisplayType=RESOURCEDISPLAYTYPE_SERVER then begin
          p:=PtrResource^.lpRemoteName;
          if (p[0]='\') and (p[1]='\') then
            p:=p+2;
          AList.Add(p);
        end;
        s:=string(PtrResource^.lpRemoteName);
        if (RESOURCEUSAGE_CONTAINER=(PtrResource^.dwUsage and RESOURCEUSAGE_CONTAINER)) and
           (PtrResource^.dwDisplayType<>RESOURCEDISPLAYTYPE_SERVER) and
           ((Pos('Microsoft',s)>0) or (PtrResource^.dwDisplayType=RESOURCEDISPLAYTYPE_DOMAIN)) then
         ScanNetwork(PtrResource,AList);
      end;
    end else if dwResultEnum<>ERROR_NO_MORE_ITEMS then
      Break;
  until dwResultEnum = ERROR_NO_MORE_ITEMS;
  finally
    if Assigned(lpnrLocal) then
      FreeMem(lpnrLocal);
    WNetCloseEnum(hEnum);
  end;
end;

function GetTrayWndHeight: Cardinal;
var
  h: THandle;
  R: TRect;
begin
  Result:=0;
  h:=FindWindow('Shell_TrayWnd',nil);
  if h=0 then
    Exit;
  GetWindowRect(h,R);
  Result:=R.Bottom-R.Top;
end;

function GetLocaleLangId: Integer;
var
  Buffer: PChar;
  BufLen: Integer;
begin
  BufLen:=255;
  GetMem(Buffer,BufLen);
  try
    GetLocaleInfo(LOCALE_USER_DEFAULT,LOCALE_ILANGUAGE,Buffer,BufLen);
    Result:=StrToInt('$'+string(Buffer));
  finally
    FreeMem(Buffer);
  end;
end;

function GetLocaleProp(LCType: Cardinal): string;
var
  Buffer: PChar;
  BufLen: Integer;
begin
  BufLen:=255;
  GetMem(Buffer,BufLen);
  try
    GetLocaleInfo(LOCALE_USER_DEFAULT,LCType,Buffer,BufLen);
    Result:=Buffer;
  finally
    FreeMem(Buffer,BufLen);
  end;
end;

function Join(const LoWord, HiWord:word):Integer;
begin
  Result:=LoWord+65536*HiWord;
end;

function GetWinDirFromBoot(ADisk: string; var OS: string): string;
var
  s: string;
  p: Integer;
begin
  OS:='';
  Result:='';
  with TIniFile.Create(IncludeTrailingPathDelimiter(ADisk)+'boot.ini') do
    try
      s:=Trim(ReadString('boot loader','default',''));
      if s='' then
        Exit;
      Result:=Trim(Copy(s,Pos('\',s)+1,255));
      s:=Trim(ReadString('operating systems',s,''));
      p:=Pos('/',s);
      if p>0 then
        OS:=Trim(Copy(s,1,p-1))
      else
        OS:=Trim(s);
    finally
      Free;
    end;
end;

procedure GetUsersFromDisk(ADisk: string; AList: TStrings);
var
  SR: TSearchRec;
  d: string;
const
  cXPDAS = '%s\Documents and Settings\';
  cVistaDAS = '%s\Users\';
begin
  AList.Clear;
  d:=Format(cXPDAS,[ADisk]);
  if FindFirst(d+'*.*',faDirectory,SR)<>0 then
    d:=Format(cVistaDAS,[ADisk]);
  if FindFirst(d+'*.*',faDirectory,SR)=0 then begin
    if not SameText('.',SR.Name) and not SameText('..',SR.Name) and not SameText('PUBLIC',SR.Name) then
      AList.Add(Uppercase(SR.Name));
    while FindNext(SR)=0 do
      if not SameText('.',SR.Name) and not SameText('..',SR.Name) and not SameText('PUBLIC',SR.Name) then
        AList.Add(Uppercase(SR.Name));
  end;
end;

procedure CreateDosDevicesTable;
var
  i: Integer;
begin
  for i:=0 to 25 do begin
    DosDevices[i].DiskLabel:=Chr(i+Ord('a'))+':';
    SetLength(DosDevices[i].DiskDosQuery,MAXCHAR);
    ResetMemory(DosDevices[i].DiskDosQuery[1],MAXCHAR);
    QueryDosDevice(PChar(DosDevices[i].DiskLabel),@DosDevices[i].DiskDosQuery[1],MAXCHAR);
    DosDevices[i].DosQueryLen:=Length(PChar(DosDevices[i].DiskDosQuery));
    SetLength(DosDevices[i].DiskDosQuery,DosDevices[i].DosQueryLen);
  end;
end;

function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: PChar; lpszVolumeName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
type
  TGetVolumeNameForVolumeMountPoint = function(lpszVolumeMountPoint: PChar; lpszVolumeName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
var
  _GetVolumeNameForVolumeMountPoint: TGetVolumeNameForVolumeMountPoint;
begin
  Result:=False;
  {$IFDEF UNICODE}
  _GetVolumeNameForVolumeMountPoint:=TGetVolumeNameForVolumeMountPoint(GetProcAddress(GetModuleHandle('kernel32.dll'),'GetVolumeNameForVolumeMountPointW'));
  {$ELSE}
  _GetVolumeNameForVolumeMountPoint:=TGetVolumeNameForVolumeMountPoint(GetProcAddress(GetModuleHandle('kernel32.dll'),'GetVolumeNameForVolumeMountPointA'));
  {$ENDIF}
  if Assigned(_GetVolumeNameForVolumeMountPoint) then
    Result:=_GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint,lpszVolumeName,cchBufferLength);
end;

function GetVolumeNameForVolumeMountPointString(Name: string): string;
var
  Volume: array [0..MAX_PATH] of Char;
begin
  Name:=IncludeTrailingPathDelimiter(Name);
  ResetMemory(Volume[0], SizeOf(Volume));
  GetVolumeNameForVolumeMountPoint(PChar(Name), @Volume[0], SizeOf(Volume));
  Result:=Volume;
end;

function GetVolumePathName(lpszFilename: PChar; lpszVolumePathName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
type
  TGetVolumePathName = function(lpszFilename: PChar; lpszVolumePathName: PChar; cchBufferLength: Cardinal): BOOL; stdcall;
var
  _GetVolumePathName: TGetVolumePathName;
begin
  Result:=False;
  {$IFDEF UNICODE}
  _GetVolumePathName:=TGetVolumePathName(GetProcAddress(GetModuleHandle('kernel32.dll'),'GetVolumePathNameW'));
  {$ELSE}
  _GetVolumePathName:=TGetVolumePathName(GetProcAddress(GetModuleHandle('kernel32.dll'),'GetVolumePathNameA'));
  {$ENDIF}
  if Assigned(_GetVolumePathName) then
    Result:=_GetVolumePathName(lpszFilename,lpszVolumePathName,cchBufferLength);
end;

procedure CreateMountPointTable(ATable: TStrings);
var
  i: Integer;
  n: Cardinal;
  s: string;
  v: array[0..MAX_PATH] of char;
begin
  ATable.Clear;
  s:=GetLogicalDisks;
  n:=SizeOf(v);
  for i:=1 to Length(s) do begin
    ResetMemory(v,SizeOf(v));
    if GetVolumeNameForVolumeMountPoint(PChar(Copy(s,i,1)+':\'),@v,n) then
      ATable.Add(Format('%s=%s',[s[i],v]));
  end;
end;

function VolumeNameToDeviceName(const VolName: String): String;
var
  s: String;
  TargetPath: Array[0..MAX_PATH] of WideChar;
begin
  Result:='';
  s:=ExcludeTrailingPathDelimiter(Copy(VolName,5,Length(VolName)-4));
  if QueryDosDeviceW(PWideChar(WideString(s)), TargetPath, MAX_PATH)<>0 then
    Result:=TargetPath;
end;

function KernelNameToFilename(AName: string): string;
var
  i: Integer;
  n: Cardinal;
  s,TmpFileName: string;
begin
  s:='';
  Result:=AName;
  i:=0;
  while i<=25 do begin
    if DosDevices[i].DosQueryLen>0 then
      if SameText(Copy(AName,1,DosDevices[i].DosQueryLen),DosDevices[i].DiskDosQuery) then begin
        s:=DosDevices[i].DiskLabel;
        Delete(AName,1,DosDevices[i].DosQueryLen);
        Break;
      end;
    Inc(i);
  end;
  SetLength(TmpFileName,MAX_PATH);
  n:=GetLongPathName(PChar(s+AName),@TmpFileName[1],MAX_PATH);
  SetLength(TmpFileName,n);
  if Length(Trim(TmpFilename))>2 then
    Result:=Trim(TmpFilename)
  else if (s<>'') and (Pos('~',AName)=0) then
    Result:=s+AName;
end;

procedure GetSpecialAccounts(AList: TStrings);
const
  rk = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\SpecialAccounts\UserList';
var
  i: Integer;
begin
  AList.Clear;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rk,False) then begin
        GetValueNames(AList);
        for i:=0 to AList.Count-1 do
          AList[i]:=Format('%s=%d',[AList[i],ReadInteger(AList[i])]);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function CreateProcWithLogon(UserName,Password,CmdLine: {$IFDEF UNICODE}WideString{$ELSE}AnsiString{$ENDIF}): Boolean;
var
  StartupInfo: PStartupInfo;
  ProcessInfo: PProcessInformation;
begin
  Result:=False;
  if not Assigned(CreateProcessWithLogon) then
    Exit;
  ProcessInfo:=nil;//ProcessInfo:=AllocMem(SizeOf(TProcessInformation));
  StartupInfo:=AllocMem(SizeOf(TStartupInfo));
  //StartupInfo.cb:=SizeOf(TStartupInfo);
  try
    {$IFDEF UNICODE}
    Result:=CreateProcessWithLogon(PWChar(UserName),'',PWChar(Password),
                 LOGON_WITH_PROFILE,nil,PWChar(CmdLine),CREATE_DEFAULT_ERROR_MODE,
                 nil,nil,StartupInfo,ProcessInfo);
    {$ELSE}
    Result:=CreateProcessWithLogon(PAnsiChar(UserName),'',PAnsiChar(Password),
                 LOGON_WITH_PROFILE,nil,PAnsiChar(CmdLine),CREATE_DEFAULT_ERROR_MODE,
                 nil,nil,StartupInfo,ProcessInfo);
    {$ENDIF}
    CloseHandle(ProcessInfo^.hProcess);
    CloseHandle(ProcessInfo^.hThread);
  finally
    FreeMem(StartupInfo);
    //FreeMem(ProcessInfo);
  end;

  if not Result then
    Result:=WinExec(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(CmdLine)),SW_SHOWDEFAULT)>31;
end;

function CreateProcessAsUserInSession(UserName,Domain,Password: string; Session: Cardinal; CmdLine: string): Cardinal;
var
  th: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  Result:=0;
  if not LogonUser(PChar(Username),PChar(Domain),PChar(Password),LOGON32_LOGON_INTERACTIVE,LOGON32_PROVIDER_DEFAULT,th) then
    Exit;
  if SetTokenInformation(th,TTokenInformationClass(12),@Session,SizeOf(Session)) then
    try
      ResetMemory(si,sizeof(STARTUPINFO));
      si.cb:=sizeof(STARTUPINFO);
      si.lpDesktop:=PChar('winsta0\default');
      if CreateProcessAsUser(th,nil,PChar(CmdLine),nil,nil,False,
                            NORMAL_PRIORITY_CLASS or CREATE_NEW_CONSOLE,nil,nil,{$IFDEF FPC}@{$ENDIF}si,{$IFDEF FPC}@{$ENDIF}pi)
         and (pi.hProcess<>INVALID_HANDLE_VALUE) then
        Result:=pi.dwProcessId;
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
  finally
    CloseHandle(th);
  end;
end;

function FormatOSName(const AName: string): string;
begin
  Result:=AName;
  Result:=StringReplace(Result,'Service Pack ','SP',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Standard','Std',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Professional','Pro',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Enterprise','Ent',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Edition','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(R)','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'NULL','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'®','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'(TM)',' ',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Microsoft','',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'Seven','7',[rfReplaceAll,rfIgnoreCase]);
  Result:=Trim(StripSpaces(Result));
end;

procedure GetServerInfo(var Comment: string; var Flags,LicensedUsers,UsersPerLicense: Cardinal);
var
 server: Pointer;
 ec: cardinal;
begin
  LicensedUsers:=0;
  UsersPerLicense:=0;
  Comment:='';
  {if not InitNetAPI then
    Exit;}
  try
    ec:=NetServerGetInfo('\\.',102,server);
    if (ec<>NERR_Success) then
      Exit;
    with PSERVER_INFO_102(server)^ do begin
      LicensedUsers:=sv102_users;
      UsersPerLicense:=sv102_Licenses;
      Comment:=sv102_comment;
      Flags:=sv102_type;
    end;
  finally
    NetApiBufferFree(server);
  end;
end;

function IsFileLocked(AFilename: string; AReadOnly: boolean = True): Boolean;
var
  F: TFileStream;
begin
  try
    if AReadOnly then
      F:=TFileStream.Create(AFilename,fmOpenRead or fmShareDenyNone)
    else
      F:=TFileStream.Create(AFilename,fmOpenReadWrite or fmShareDenyNone);
    try
      Result:=False;
    finally
      F.Free;
    end;
  except
    Result:=True;
  end;
end;

function RunAsAdmin(hWnd: HWND; AFilename, AParams: string): Boolean;
var
  sei: TShellExecuteInfo;
begin
  ResetMemory(sei,SizeOf(sei));
  sei.cbSize:=SizeOf(TShellExecuteInfo);
  sei.Wnd:=hwnd;
  sei.fMask:=SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI {$IFDEF UNICODE}or SEE_MASK_UNICODE{$ENDIF};
  sei.lpVerb:=PChar('runas');
  sei.lpFile:=PChar(AFilename);
  if AParams<>'' then
    sei.lpParameters:=PChar(AParams);
  sei.nShow:=SW_SHOWNORMAL;
  Result:={$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(@sei);
end;

function LoadResourceString(const DllName: String; ResourceId: Cardinal): String;
var
  hDLL: THandle;
  l: integer;
  Buffer: array[0..1023] of Char;
  f: Boolean;
begin
  Result:='';
  hDLL:=GetModuleHandle(PChar(DllName));
  f:=hDll=0;
  if hDLL=0 then
    hDLL:=LoadLibraryEx(PChar(DllName),0,LOAD_LIBRARY_AS_DATAFILE);
  if hDLL=0 then
    Exit;
  try
    l:=LoadString(hDll,ResourceId,Buffer,Length(Buffer));
    if l>0 then
      SetString(Result,Buffer,l);
  finally
    if f then
      FreeLibrary(hDLL);
  end;
end;

procedure LoadResourceStrings(const DllName: String; ACount: Integer; Strings: TStringList);
var
  hDLL: THandle;
  Buffer: array[0..1023] of Char;
  i,l: Integer;
  s: string;
  f: Boolean;
begin
  if ACount=0 then
    ACount:=255;
  Strings.Clear;
  hDLL:=GetModuleHandle(PChar(DllName));
  f:=hDll=0;
  if hDLL=0 then
    hDLL:=LoadLibraryEx(PChar(DllName),0,LOAD_LIBRARY_AS_DATAFILE);
  if hDLL=0 then
    Exit;
  try
    for i:=0 to ACount-1 do begin
      l:=LoadString(hDll,i,Buffer,Length(Buffer));
      if l>0 then begin
        SetString(s,Buffer,l);
        Strings.Add(s);
      end;
    end;
  finally
    if f then
      FreeLibrary(hDLL);
  end;
end;

function WindowsExit(AMode: Cardinal): Boolean;
//EWX_POWEROFF,EWX_REBOOT,EWX_LOGOFF
var
  th: THandle;
  tp1,tp2: TTokenPrivileges;
  n,c: cardinal;
  r: Boolean;
const
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    r:=OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,th);
    if r then begin
      r:=LookupPrivilegeValue(nil,SE_SHUTDOWN_NAME,tp1.Privileges[0].Luid);
      tp1.PrivilegeCount:=1;
      tp1.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
      n:=SizeOf(tp1) ;
      c:=0;
      if r then
        AdjustTokenPrivileges(th,False,tp1,n,tp2,c);
    end;
  end;
  Result:=ExitWindowsEx(AMode,0);
end;

{$IFDEF FPC}
function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
var
  Buffer: array[0..1] of Char;
begin
  if GetLocaleInfo(Locale, LocaleType, Buffer, 2) > 0 then
    Result := Buffer[0] else
    Result := Default;
end;
{$ENDIF}

procedure FixLocale;
begin
  SetThreadLocale(GetUserDefaultLCID);
  GetFormatSettings;
  if (GetThreadLocale=1051) then begin
    {$IFDEF FS}FormatSettings.{$ENDIF}DateSeparator:=GetLocaleChar(GetThreadLocale,LOCALE_SDATE,#0);
    if {$IFDEF FS}FormatSettings.{$ENDIF}DateSeparator=#0 then begin
      try {$IFDEF FS}FormatSettings.{$ENDIF}DateSeparator:='.' except end;
      {$IFDEF FS}FormatSettings.{$ENDIF}ShortDateFormat:='d.M.yyyy';
    end;
  end;
  if Pos('.',{$IFDEF FS}FormatSettings.{$ENDIF}ShortDateFormat)>0 then
    try {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DateSeparator:='.' except end;
end;

procedure FixLocale(var AFS: TFormatSettings);
begin
  if (GetThreadLocale=1051) then begin
    AFS.DateSeparator:=GetLocaleChar(GetThreadLocale,LOCALE_SDATE,#0);
    if AFS.DateSeparator=#0 then begin
      try AFS.DateSeparator:='.' except end;
      AFS.ShortDateFormat:='d.M.yyyy';
    end;
  end;
  if Pos('.',AFS.ShortDateFormat)>0 then
    try AFS.DateSeparator:='.' except end;
end;

function HtmlToColor(AHTML:string; ADefault: TColor): TColor;
begin
  Result:=ADefault;
  if Copy(AHTML,1,1)='#' then begin
    AHTML:='$'+Copy(AHTML,6,2)+Copy(AHTML,4,2)+Copy(AHTML,2,2);
    try Result:=StringToColor(AHTML) except end;
  end;
end;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size>0 then begin
    Byte(P):=0;
    FillChar(P,Size,0);
  end;
end;

{$IFDEF RAD6PLUS}
procedure SaveBytesToStream(ABytes: TBytes; AStream: TStream);
var
  i: Integer;
begin
  for i:=0 to High(ABytes) do
    AStream.Write(ABytes[i],1);
  AStream.Position:=0;
end;

procedure SaveBytesToFile(ABytes: TBytes; AFilename: string);
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    SaveBytesToStream(ABytes,ms);
    ms.SaveToFile(AFilename);
  finally
    ms.Free;
  end;
end;

procedure SaveStringToFile(AString: ansistring; AFilename: string);
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(AString);
  try
    ss.SaveToFile(AFilename);
  finally
    ss.Free;
  end;
end;

procedure SaveStringToFile(AString: string; AFilename: string);
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(AString);
  try
    ss.SaveToFile(AFilename);
  finally
    ss.Free;
  end;
end;
{$ENDIF}

function GetTaskManager: string;
begin
  Result:='taskmgr.exe';
  with TRegistry.Create do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Image File Execution Options\taskmgr.exe') then begin
        if ValueExists('Debugger') then
          Result:=DequoteStr(ReadString('Debugger'));
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure GetCPUTopology(var APkgCnt,ACoreCnt,AThrCnt: Byte);
var
  i,l: Integer;
  n: DWORD;
  Buffer: array of TSystemLogicalProcessorInformation;
begin
  APkgCnt:=0;
  ACoreCnt:=0;
  AThrCnt:=0;
  if not Assigned(GetLogicalProcessorInformation) then
    Exit;
  n:=0;
  if not GetLogicalProcessorInformation(nil,n) then begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
      SetLength(Buffer,n div SizeOf(TSystemLogicalProcessorInformation)+1);
      if not GetLogicalProcessorInformation(@Buffer[0],n) then
        Exit;
    end else
      Exit;
  end;

  l:=High(Buffer);
  for i:=0 to l-1 do
    if Buffer[i].ProcessorMask>0 then
    case Buffer[i].Relationship of
      RelationProcessorCore: begin
        Inc(ACoreCnt);
        Inc(AThrCnt,CountSetBits(Buffer[i].ProcessorMask));
      end;
      RelationProcessorPackage: Inc(APkgCnt);
    end;
end;

function WinControlExists(AControl: TWinControl): Boolean;
begin
  try
    Result:=Assigned(AControl) and AControl.HandleAllocated and IsWindow(AControl.Handle);
  except;
    Result:=False;
  end;
end;

function GeneratePassword(ALength: Integer; AUpper, ALower, ANumbers, ASymbols: Boolean): string;
const
  cULetters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  cLLetters = 'abcdefghijklmnopqrstuvwxyz';
  cNumbers  = '012345678901234567890123456789012345678901234567890123';
  cSymbols  = '@#$%^&*()+-/=!_?.,<>{}@#$%^&*()+-/=!_?.,<>{}@#$%^&*()+';
var
  sGenChars: string;
  wGenChar, wPrevChar: Char;
  iCharsLen, iRnd: Integer;
begin
  sGenChars:='';

  if (not AUpper) and (not ALower) and (not ANumbers) and (not ASymbols) then begin
    AUpper:=True;
    ANumbers:=True;
  end;
  if ALength<10 then
    ALength:=10;
  if AUpper then
    sGenChars:=sGenChars+cULetters;
  if ANumbers then begin
    if not AUpper and ALower then
      sGenChars:=sGenChars+Copy(cNumbers,1,Length(cULetters))
    else
      sGenChars:=sGenChars+cNumbers;
  end;
  if ASymbols then begin
    if not AUpper and ALower then
      sGenChars:=sGenChars+Copy(cSymbols,1,Length(cULetters))
    else
      sGenChars:=sGenChars+cSymbols;
  end;
  if ALower then
    sGenChars:=sGenChars+cLLetters;

  Result:='';
  Randomize;
  iCharsLen:=Length(sGenChars);
  wPrevChar:=#0;
  while Length(Result)<ALength do begin
    iRnd:=Random(iCharsLen)+1;
    wGenChar:=sGenChars[iRnd];
    if wGenChar<>wPrevChar then begin

      Result:=Result+wGenChar;
      wPrevChar:=wGenChar;
    end;
  end;
end;

function GetActiveOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
  Unknown: IUnknown;
  HR: HRESULT;
begin
  ClassID:=ProgIDToClassID(ClassName);
  HR:=GetActiveObject(ClassID, nil, Unknown);
  if Succeeded(HR) then
    HR:=Unknown.QueryInterface(IDispatch, Result);
  if not Succeeded(HR) then
    Result:=nil;
end;

function CreateOleObject(const ClassName: string): IDispatch;
var
  ClassID: TCLSID;
  HR: HRESULT;
begin
  ClassID := ProgIDToClassID(ClassName);
  HR:=CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IDispatch, Result);
  if not Succeeded(HR) then
    Result:=nil;
end;

function GetObjectIntf(const AClassName: string): OLEVariant;
var
  idisp: IDispatch;
begin
  Result:=null;
  try
    idisp:=GetActiveOLEObject(AClassname);
    if not Assigned(idisp) then
      idisp:=CreateOLEObject(AClassname);
    if Assigned(idisp) then
      Result:=idisp;
  except
  end;
end;

function GetMonitorPixelsPerInch(AMonitor: THandle): Integer;
var
  Ydpi: Cardinal;
  Xdpi: Cardinal;
  DC: THandle;
begin
  if CheckWin32Version(6,3) and Assigned(GetDpiForMonitor) then begin
    if GetDpiForMonitor(AMonitor,MDT_EFFECTIVE_DPI,Ydpi,Xdpi)=S_OK then
      Result:=Ydpi
    else
      Result:=0;
  end else begin
    DC:=GetDC(0);
    Result:={$IFDEF RAD9PLUS}WinApi.{$ENDIF}Windows.GetDeviceCaps(DC,LOGPIXELSY);
    ReleaseDC(0,DC);
  end;
end;

function GDIProcessHandleQuota: integer;
begin
  Result:=10000;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows',False) then begin
        if ValueExists('GDIProcessHandleQuota') then
          Result:=ReadInteger('GDIProcessHandleQuota');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function USERProcessHandleQuota: integer;
begin
  Result:=10000;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows',False) then begin
        if ValueExists('USERProcessHandleQuota') then
          Result:=ReadInteger('USERProcessHandleQuota');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetDelphi: string;
begin
  Result:='?';
  {$IFDEF VER320}Result:='Embarcadero Delphi 10.2 Tokyo';{$ENDIF}
  {$IFDEF VER310}Result:='Embarcadero Delphi 10.1 Berlin';{$ENDIF}
  {$IFDEF VER300}Result:='Embarcadero Delphi 10 Seattle';{$ENDIF}
  {$IFDEF VER290}Result:='Embarcadero Delphi XE8'{$ENDIF}
  {$IFDEF VER280}Result:='Embarcadero Delphi XE7';{$ENDIF}
  {$IFDEF VER270}Result:='Embarcadero Delphi XE6';{$ENDIF}
  {$IFDEF VER260}Result:='Embarcadero Delphi XE5';{$ENDIF}
  {$IFDEF VER250}Result:='Embarcadero Delphi XE4';{$ENDIF}
  {$IFDEF VER240}Result:='Embarcadero Delphi XE3';{$ENDIF}
  {$IFDEF VER230}Result:='Embarcadero Delphi XE2';{$ENDIF}
  {$IFDEF VER220}Result:='Embarcadero Delphi XE';{$ENDIF}
  {$IFDEF VER210}Result:='Embarcadero Delphi 2010';{$ENDIF}
  {$IFDEF VER200}Result:='Embarcadero Delphi 2009';{$ENDIF}
  {$IFDEF VER190}Result:='CodeGear Delphi 2007';{$ENDIF}
  {$IFDEF VER185}Result:='CodeGear Delphi 2007';{$ENDIF}
  {$IFDEF VER180}Result:='Borland Delphi 2006';{$ENDIF}
  {$IFDEF VER170}Result:='Borland Delphi 2005';{$ENDIF}
  {$IFDEF VER150}Result:='Borland Delphi 7';{$ENDIF}
end;

function CalcEntropy(AData: PByte; ASize: int64): Double; overload;
var
  a: array[0..255] of int64;
  i: int64;
  j: integer;
  p: double;
begin
  Result:=0;
  ResetMemory(a,sizeof(a));
  i:=0;
  while i<ASize do begin
    Inc(a[AData^]);
    Inc(AData);
    Inc(i);
  end;
  for j:=0 to High(a) do
    if a[j]>0 then begin
      p:=a[j]/ASize;
      Result:=Result-p*log2(p);
    end;
end;

function CalcEntropy(AData: TBytes): Double;
begin
  Result:=CalcEntropy(@AData[0],Length(AData));
end;

function CalcEntropy(const AFilename: string): Double;
var
  mf: TMappedFile;
begin
  Result:=-1;
  mf:=TMappedFile.Create(AFilename);
  try
    if Assigned(mf.Content) then
      Result:=CalcEntropy(mf.Content,mf.Size);
  finally
    mf.Free;
  end;
end;

function FileChecksum(const AFilename: string): Cardinal;
var
  mf: TMappedFile;
  hs: Cardinal;
  p: PByte;
begin
  Result:=0;
  if not Assigned(CheckSumMappedFile) or not FileExists(AFilename) then
    Exit;
  mf:=TMappedFile.Create(AFilename);
  try
    p:=mf.Content;
    CheckSumMappedFile(p,mf.Size,hs,Result);
  finally
    mf.Free;
  end;
end;

function FindInStream(const ASubStr: String; const AStream: TMemoryStream; APosition: Int64 = 0): Integer;
var
  SubLen,SrcLen,l,j,i,k: Integer;
  s: AnsiString;
  c: AnsiChar;
  p: PAnsiChar;
begin
  s:=WideToAnsi(ASubstr);
  SrcLen:=AStream.Size;
  SubLen:=Length(s);
  Result:=-1;
  if (SubLen<=0) or (SrcLen<=0) or (SrcLen<SubLen) then
    Exit;
  k:=Max(1,APosition);
  p:=AStream.Memory;
  l:=SrcLen-SubLen+1;
  c:=s[1];
  for i:=k to l do begin
    if p[i-1]=c then begin
      Result:=i-1;
      for j:=1 to SubLen-1 do begin
        if p[i+j-1]<>s[1+j] then begin
          Result:=-1;
          Break;
        end;
      end;
      if Result<>-1 then
        Exit;
    end;
  end;
end;

function FindInStream(const APattern: TBytes; const AStream: TStream; APosition: Int64 = 0): Int64;
var
  buf: TBytes;
  i,l: integer;
  ok: boolean;
begin
  Result:=-1;
  l:=Length(APattern);
  SetLength(buf,l);
  if APosition>-1 then
    AStream.Position:=APosition;
  while AStream.Position<AStream.Size-l do begin
    AStream.Read(buf[0],l);
    ok:=True;
    for i:=0 to l-1 do
      if buf[i]<>APattern[i] then begin
        ok:=False;
        Break;
      end;
    if ok then begin
      Result:=AStream.Position-l;
      Break;
    end;
    AStream.Seek(-l+1,soCurrent);
  end;
end;

procedure DeleteFromStream(AStream: TStream; AStart, ALength: Int64);
var
  Buffer: Pointer;
  BufferSize: Integer;
  BytesToRead: Int64;
  BytesRemaining: Int64;
  SourcePos, DestPos: Int64;
begin
  SourcePos:=AStart+ALength;
  DestPos:=AStart;
  BytesRemaining:=AStream.Size-SourcePos;
  BufferSize:=Min(BytesRemaining,1024*1024*16);//no bigger than 16MB
  GetMem(Buffer,BufferSize);
  try
    while BytesRemaining>0 do begin
      BytesToRead:=Min(BufferSize,BytesRemaining);
      AStream.Position:=SourcePos;
      AStream.ReadBuffer(Buffer^,BytesToRead);
      AStream.Position:=DestPos;
      AStream.WriteBuffer(Buffer^,BytesToRead);
      inc(SourcePos,BytesToRead);
      inc(DestPos,BytesToRead);
      dec(BytesRemaining,BytesToRead);
    end;
    AStream.Size:=DestPos;
  finally
    FreeMem(Buffer);
  end;
end;

function FindInFile(const APattern: TBytes; const AFilename: string): Int64;
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(AFilename,fmOpenRead or fmShareDenyWrite);
  try
    Result:=FindInStream(APattern,fs);
  finally
    fs.Free;
  end;
end;

procedure SplitFile(const AFilename: string; AStartOffsets: array of Int64; AFilenames: array of string);
var
  fs,nfs: TFileStream;
  i: integer;
  c: int64;
begin
  fs:=TFileStream.Create(AFilename,fmOpenRead or fmShareDenyWrite);
  try
    for i:=0 to High(AStartOffsets) do begin
      fs.Seek(AStartOffsets[i],soFromBeginning);
      nfs:=TFileStream.Create(AFilenames[i],fmCreate or fmShareDenyWrite);
      try
        if i<High(AStartOffsets) then
          c:=AStartOffsets[i+1]
        else
          c:=fs.Size;
        c:=c-AStartOffsets[i];
        nfs.CopyFrom(fs,c);
      finally
        nfs.Free;
      end;
    end;
  finally
    fs.Free;
  end;
end;

procedure MergeFiles(AFilenames: array of string; const AFilename: string);
var
  fs,nfs: TFileStream;
  i: integer;
begin
  fs:=TFileStream.Create(AFilename,fmCreate or fmShareDenyWrite);
  try
    for i:=0 to High(AFilenames) do begin
      nfs:=TFileStream.Create(AFilenames[i],fmOpenRead or fmShareDenyWrite);
      try
        fs.CopyFrom(nfs,nfs.Size);
      finally
        nfs.Free;
      end;
    end;
  finally
    fs.Free;
  end;
end;

var
  p: array[0..MAX_PATH] of char;


{ TVersionInfo }

function TVersionInfo.GetCompany: string;
begin
  if (Trim(CompanyName)<>'') and (PosText(CompanyName,Copyright)=0) then begin
   if (Trim(Copyright)<>'') then
     Result:=Trim(CompanyName)+' - '+Trim(Copyright)
   else
     Result:=Trim(CompanyName);
  end else
    Result:=Trim(Copyright);
end;

function TVersionInfo.GetFullDesc: string;
begin
  Result:=Trim(Description);
  if Result='' then
    Result:=Trim(ProductName);
  if Result='' then
    Result:=ChangeFileExt(ExtractFileName(FileName),'');
  Result:=Trim(Result+' '+FileVersion);
  if (Trim(ProductName)<>'') and (Trim(Description)<>'') and not SameText(ProductName,Description) then
    Result:=Trim(Result)+' - '+Trim(ProductName);
end;

initialization
  FixLocale;
  if IsWow64 then
    GetNativeSystemInfo(SystemInfo)
  else
    GetSystemInfo(SystemInfo);

  Session:=GetSession;

  ResetMemory(OSVI,SizeOf(OSVI));
  OSVI.dwOSVersionInfoSize:=SizeOf(OSVI);
  GetVersionEx(OSVI);
  if OSVI.dwMajorVersion>=5 then begin
    ResetMemory(OSVIX,SizeOf(OSVIX));
    OSVIX.dwOSVersionInfoSize:=SizeOf(OSVIX);
    GetVersionEx(OSVIX);
  end else begin
    OSVIX.dwMajorVersion:=OSVI.dwMajorVersion;
    OSVIX.dwMinorVersion:=OSVI.dwMinorVersion;
    OSVIX.dwBuildNumber:=OSVI.dwBuildNumber;
    OSVIX.dwPlatformId:=OSVI.dwPlatformId;
    StrCopy(OSVIX.szCSDVersion,OSVI.szCSDVersion);
  end;
  UpdateWinVersion;
  IsServer:=OSVIX.wProductType<>VER_NT_WORKSTATION;

  MachineName:=Uppercase(GetMachine);

  GetOSName(OSName,OSEdition);

  WindowsUser:=GetUser;
  WindowsUserSID:=GetSIDFromACcount(MachineName,WindowsUser);

  WindowsLiveID:=GetWindowsLiveID;

  ProfilePath:=GetProfilePath;

  BuildLab:=GetBuildLab;
  OSBuild:=GetOSBuild;

  ClassKey:='SYSTEM\CurrentControlSet\Control\Class';

  ResetMemory(MSEX,SizeOf(MSEX));
  if Assigned(GlobalMemoryStatusEx_) then begin
    MSEX.dwLength:=SizeOf(MSEX);
    GlobalMemoryStatusEx_(@MSEX);
    Memory:=MSEX.ullTotalPhys;
  end else begin
    ResetMemory(MS,SizeOf(MS));
    MS.dwLength:=SizeOf(MS);
    GlobalMemoryStatus(MS);
    Memory:=MS.dwTotalPhys;
    MSEX.dwMemoryLoad:=MS.dwMemoryLoad;
    MSEX.ullTotalPhys:=MS.dwTotalPhys;
    MSEX.ullAvailPhys:=MS.dwAvailPhys;
    MSEX.ullTotalPageFile:=MS.dwTotalPageFile;
    MSEX.ullAvailPageFile:=MS.dwAvailPageFile;
    MSEX.ullTotalVirtual:=MS.dwTotalVirtual;
    MSEX.ullAvailVirtual:=MS.dwAvailVirtual;
  end;

  if IsLibrary then begin
    {$IFDEF RAD9PLUS}WinApi.{$ENDIF}Windows.GetModuleFileName(hInstance,@p,MAX_PATH);
    ModuleName:=p;
  end else
    ModuleName:=ParamStr(0);

  GetFileVerInfo(ModuleName,ModuleInfo);
  ExeVersionInfo:=ModuleInfo;

  LangId:=GetLocaleLangId;

  CreateDosDevicesTable;

  if OSVIX.wServicePackMajor>0 then
    ServicePack:=Format('%d.%d',[OSVIX.wServicePackMajor,OSVIX.wServicePackMinor]);

  FormOSName:=FormatOSName(OSName+' '+OSEdition);
  if ServicePack<>'' then
    FormOSName:=FormOSName+' SP '+ServicePack;

  ProductName:=GetProductName(InstallationType);

  TrueWindowsVersion:=GetTrueWindowsVersion;
  TrueWindowsName:=GetTrueWindowsName;

  CompatibilityMode:=not SameText(TrueWindowsVersion,Format('%d.%d.%d',[OSVIX.dwMajorVersion,OSVIX.dwMinorVersion,OSVIX.dwBuildNumber]));
end.

