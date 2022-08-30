{*******************************************************}
{       MiTeC System Information Component Suite        }
{                 OS Detection Part                     }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_OS;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_SysUtils, MiTeC_Windows, MiTeC_Routines;

const
  StorageFolderName = 'OS';
  Internet_StorageFolderName = 'Internet';
  Locale_StorageFolderName = 'LocaleInfo';
  TimeZone_StorageFolderName = 'TimeZone';
  strm_Update = 'Update_%d';
  Updates_StorageFolderName = 'Updates';

  {$IFDEF FPC}
  LOCALE_SINTLSYMBOL              = $00000015;   { intl monetary symbol }
  LOCALE_ICENTURY                 = $00000024;   { century format specifier (short date) }
  {$ENDIF}

type
  TMiTeC_WEI = class(TMiTeC_Component)
  private
    FSystemScore: single;
    FCPUScore: single;
    FDiskScore: single;
    FMemoryScore: single;
    FGraphicsScore: single;
    FGamingScore: single;
  public
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    property SystemScore: single read FSystemScore;
    property GraphicsScore: single read FGraphicsScore;
    property MemoryScore: single read FMemoryScore;
    property DiskScore: single read FDiskScore;
    property CPUScore: single read FCPUScore;
    property GamingScore: single read FGamingScore;
  end;

  TMiTeC_TimeZone = class(TMiTeC_Component)
  private
    FStdBias: integer;
    FDayBias: integer;
    FBias: integer;
    FDisp: string;
    FStd: string;
    FDayStart: TDatetime;
    FStdStart: TDatetime;
    FDay: string;
    FMap: string;
  public
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property MapID: string read FMap;
  published
    property DisplayName: string read FDisp stored False;
    property StandardName: string read FStd stored False;
    property DaylightName: string read FDay stored False;
    property DaylightStart: TDatetime read FDayStart stored False;
    property StandardStart: TDatetime read FStdStart stored False;
    property Bias: integer read FBias stored False;
    property DaylightBias: integer read FDayBias stored False;
    property StandardBias: integer read FStdBias stored False;
  end;

  TMiTeC_Internet = class(TMiTeC_Component)
  private
    FBrowser: string;
    FProxy: TStrings;
    FMailClient: string;
    FCType: TConnectionType;
    FMailClientName: string;
    FBrowserName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function GetConnTypeStr(ACType: TConnectionType): string;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property DefaultBrowserName: string read FBrowserName stored False;
    property DefaultBrowser: string read FBrowser stored False;
    property DefaultMailClientName: string read FMailClientName stored False;
    property DefaultMailClient: string read FMailClient stored False;
    property ConnectionType: TConnectionType read FCType stored False;
    property ProxyServer: TStrings read FProxy stored False;
  end;

  TMeasureSystem = (Metric, US);

  TPositiveCurrencyMode = (Prefix_No_Separation, Suffix_No_Separation,
                           Prefix_One_Char_Separation, Suffix_One_Char_Separation);

  TDateOrder = (MDY, DMY, YMD);

  TTimeFormat = (H12, H24);

  TYearFormat = (TwoDigit, FourDigit);

const
  SNegativeCurrencyMode: array[0..9] of string =
                         ('($1.1)',
                          '-$1.1',
                          '$-1.1',
                          '$1.1-',
                          '(1.1$)',
                          '-1.1$',
                          '1.1-$',
                          '1.1$-',
                          '-1.1 $',
                          '-$ 1.1');

type
  TMiTeC_LocaleInfo = Class(TMiTeC_Component)
  private
    FLang,
    FEngLang,
    FAbbrLang,
    FCountry,
    FFCountry,
    FAbbrCtry,
    FList,
    FDecimal,
    FDigit,
    FCurrency,
    FIntlSymbol,
    FMonDecSep,
    FMonThoSep,
    FCurrdigit,
    FNCurrMode,
    FDate,
    FTime,
    FTimeFormat,
    FShortDate: string;
    FMeasure: TMeasureSystem;
    FPCurrMode: TPositiveCurrencyMode;
    FShortDateOrdr,
    FLongDateOrdr: TDateOrder;
    FTimeFormatSpec: TTimeFormat;
    FYearFormat: TYearFormat;
    FLongDate: string;
    FLocaleID: Cardinal;
    FDecThoSep: string;
  public
   constructor Create(AOwner: TComponent); override;
   procedure Clear; override;
   procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
   procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
   function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
   property LocaleID: Cardinal read FLocaleID write FLocaleID;
   property FullLocalizeLanguage: string read Flang stored false;
   property FullLanguageEnglishName: string read FEngLang stored false;
   property AbbreviateLanguageName: string read FAbbrLang stored false;
   property CountryCode: string read FCountry stored false;
   property FullCountryCode: string read FFCountry stored false;
   property AbbreviateCountryCode: string read FAbbrCtry stored false;
   property ListSeparator: string read FList stored false;
   property MeasurementSystem: TMeasureSystem read FMeasure stored false;
   property DecimalSeparator: string read FDecimal stored false;
   property NumberOfDecimalDigits: string read FDigit stored false;
   property LocalMonetarySymbol: string read FCurrency stored false;
   property InternationalMonetarySymbol: string read FIntlSymbol stored false;
   property CurrencyDecimalSeparator: string read FMonDecSep stored false;
   property CurrencyThousandSeparator: string read FMonThoSep stored false;
   property CurrencyDecimalDigits: string read FCurrDigit stored false;
   property PositiveCurrencyMode: TPositiveCurrencyMode read FPCurrMode stored false;
   property NegativeCurrencyMode: string read FNCurrMode stored false;
   property DateSeparator: string read FDate stored false;
   property TimeSeparator: string read FTime stored false;
   property TimeFormat: string read FTimeFormat stored false;
   property ShortDateFormat: string read FShortDate stored false;
   property LongDateFormat: string read FLongDate stored False;
   property ShortDateOrder: TDateOrder read FShortDateOrdr stored false;
   property LongDateOrder: TDateOrder read FLongDateOrdr stored false;
   property TimeFormatSpecifier: TTimeFormat read FTimeFormatSpec stored false;
   property YearFormat: TYearFormat read FYearFormat stored false;
   property DecimalThousandSeparator: string read FDecThoSep stored false;
  end;

  TNTProductID = array[0..14] of Byte;

  TUpdateRecord = record
    ID,
    InstallDate,
    InstalledBy,
    Description,
    SPInEffect,
    Comment: string;
  end;

  TUpdates = array of TUpdateRecord;

  TMiTeC_OperatingSystem = class(TMiTeC_Component)
  private
    FBuildNumber: integer;
    FMajorVersion: integer;
    FMinorVersion: integer;
    FCSD: string;
    FRegUser: string;
    FProductID: string;
    FRegOrg: string;
    FEnv: TStrings;
    FDirs: TStrings;
    FTZ: TMiTeC_TimeZone;
    FDVD: string;
    FInternet: TMiTeC_Internet;
    FLocale: TMiTeC_LocaleInfo;
    FSysLangDefID: string;
    FProductKey: string;
    FInstallDate: TDateTime;
    FOS: TOSVersion;
    FOSName: string;
    FBuildLab: string;
    FEdition: string;
    FVersion: string;
    FMGUID: string;
    FFlags: Cardinal;
    FUsers: Cardinal;
    FLicenses: Cardinal;
    FComment: string;
    FTM: string;
    FCM: Boolean;
    FTV,FPN: string;
    FIT: string;
    FLiveID: string;
    FSPMinorVer: Word;
    FSPMajorVer: Word;
    FProduct: TNTProductType;
    FSuites: TNTSuites;
    FNOLU, FNOU: integer;
    FGW: Boolean;
    FPI: Cardinal;
    FDUH: Boolean;
    FUR: TUpdates;
    FUAC: boolean;
    FV: boolean;
    FPC: TStrings;
    FNoWMI: Boolean;
    FRelID: string;
    FBits: byte;
    FOSBuild: string;
    FWEI: TMiTeC_WEI;
  protected
    procedure SetHeaderReader(const Value: THeaderReader); override;
    procedure SetHeaderWriter(const Value: THeaderWriter); override;
    function GetUpdateCount: Cardinal;
    function GetUpdateRec(Index: Cardinal): TUpdateRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    procedure GetInstalledSuitesStr(var sl: TStringList);
    property Updates[Index: Cardinal]: TUpdateRecord read GetUpdateRec;
  published
    property MajorVersion: integer read FMajorVersion stored false;
    property MinorVersion: integer read FMinorVersion stored false;
    property BuildNumber: integer read FBuildNumber stored false;
    property ReleaseId: string read FRelID stored False;
    property OSName: string read FOSName stored False;
    property OSEdition: string read FEdition stored False;
    property Version: string read FVersion stored False;
    property Bits: byte read  FBits write FBits stored False;
    property OS: TOSVersion read FOS stored False;
    property CSD: string read FCSD stored false;
    property ProductID: string read FProductID stored false;
    property ProductKey: string read FProductKey stored False;
    property RegisteredUser: string read FRegUser stored false;
    property RegisteredOrg: string read FRegOrg stored false;
    property TimeZone: TMiTeC_TimeZone read FTZ stored false;
    property Environment: TStrings read FEnv stored false;
    property Folders: TStrings read FDirs stored False;
    property DVDRegion: string read FDVD stored False;
    property Internet: TMiTeC_Internet read FInternet stored False;
    property LocaleInfo: TMiTeC_LocaleInfo read FLocale stored False;
    property LanguageID: string read FSysLangDefID stored False;
    property InstallDate: TDateTime read FInstallDate stored False;
    property BuildLab: string read FBuildLab stored False;
    property OSBuild: string read FOSBuild stored False;
    property MachineGUID: string read FMGUID stored False;
    property Flags: Cardinal read FFlags stored False;
    property LicensedUsers: Cardinal read FUsers stored False;
    property UsersPerLicense: Cardinal read FLicenses stored False;
    property Comment: string read FComment write FComment;
    property TaskManager: string read FTM stored False;
    property CompatibilityMode: Boolean read FCM stored False;
    property ProductName: string read FPN stored False;
    property TrueWindowsVersion: string read FTV stored False;
    property InstallationType: string read FIT stored False;
    property LiveID: string read FLiveID stored False;
    property ProductType: TNTProductType read FProduct stored False;
    property InstalledSuites: TNTSuites read FSuites stored False;
    property ServicePackMajorVersion: Word read FSPMajorVer stored False;
    property ServicePackMinorVersion: Word read FSPMinorVer stored False;
    property NumberOfLicensedUsers: integer read FNOLU stored False;
    property NumberOfUsers: integer read FNOU stored False;
    property GenuineWindows: Boolean read FGW stored False;
    property ProductInfo: Cardinal read FPI stored False;
    property UpdateCount: Cardinal read GetUpdateCount;
    property UserAccountControl: boolean read FUAC stored False;
    property Virtualization: boolean read FV stored False;
    property ParentalControls: TStrings read FPC stored False;
    property DetectUpdatesAndHotfixes: Boolean read FDUH write FDUH;
    property DisableWMI: Boolean read FNoWMI write FNoWMI;
    property WindowsExperienceIndex: TMiTeC_WEI read FWEI stored false;
  end;

function GetVersionEx(lpVersionInformation: POSVersionInfoEx): BOOL; stdcall;

function NTSuitesAsInt(A: TNTSuites): Cardinal;
function IntAsNTSuites(A: Cardinal): TNTSuites;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Math, WinAPI.ShlObj, System.StrUtils,
     {$ELSE}
     Registry, StrUtils, Math, ShlObj,
     {$ENDIF}
     MiTeC_WMI, MiTeC_Datetime, MiTeC_RegUtils,
     MiTeC_WINSATLib_TLB,
     {$IFDEF FPC}MiTeC_FPC_WbemScripting_TLB{$ELSE}MiTeC_WbemScripting_TLB{$ENDIF};

{$IFDEF UNICODE}
function GetVersionEx; external kernel32 name 'GetVersionExW';
{$ELSE}
function GetVersionEx; external kernel32 name 'GetVersionExA';
{$ENDIF}

function NTSuitesAsInt;
var
  i: TNTSuite;
begin
  Result:=0;
  for i:=Low(TNTSuite) to High(TNTSuite) do
    if i in A then
      Result:=Result or (1 shl Integer(i));
end;

function IntAsNTSuites;
var
  i: TNTSuite;
begin
  Result:=[];
  for i:=Low(TNTSuite) to High(TNTSuite) do
    if (A and (1 shl Integer(i)))<>0 then
      Result:=Result+[i];
end;

{ TMiTeC_TimeZone }

procedure TMiTeC_TimeZone.Clear;
begin

end;

function TMiTeC_TimeZone.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(TimeZone_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
      try
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FDisp:=ReadStrProperty(sl,'DisplayName');
              Self.FStd:=ReadStrProperty(sl,'StandardName');
              Self.FDay:=ReadStrProperty(sl,'DaylightName');
              Self.FStdBias:=ReadIntProperty(sl,'StandardBias');
              Self.FBias:=ReadIntProperty(sl,'Bias');
              Self.FDayBias:=ReadIntProperty(sl,'DaylightBias');
              Self.FDayStart:=ReadDTProperty(sl,'DaylightStart');
              Self.FStdStart:=ReadDTProperty(sl,'StandardStart');
              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;
      finally
        Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_TimeZone.RefreshData;
var
  RTZ: TRegTimeZoneInfo;
  HomeTZ, RegTZ: TTimeZoneInformation;
  y,m,d,i: Word;
  sl: TStringList;
  s: string;
const
  rkTZKN = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\TimeZoneInformation';
  rvTZKN = 'TimeZoneKeyName';
  rkTimeZones = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones';
  rvTimeZone = 'StandardName';
begin
  inherited;
  Clear;
  GetTimeZoneInformation(HomeTZ);
  FDisp:=HomeTZ.StandardName;
  s:=HomeTZ.StandardName;
  sl:=TStringList.Create;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkTZKN,False) then begin
        if ValueExists(rvTZKN) then
          s:=ReadString(rvTZKN);
        CloseKey;
      end;
      if OpenKey(rkTimeZones,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkTimeZones+'\'+sl[i],False) then begin
            if GetDataSize('TZI')=SizeOf(RTZ) then begin
              ReadBinaryData('TZI',RTZ,SizeOf(RTZ));
              StringToWideChar(ReadString('Std'),PWideChar(@RegTZ.StandardName),SizeOf(RegTZ.StandardName) div SizeOf(WideChar));
              StringToWideChar(ReadString('Dlt'),PWideChar(@RegTZ.DaylightName),SizeOf(RegTZ.DaylightName) div SizeOf(WideChar));
              RegTZ.Bias:=RTZ.Bias;
              RegTZ.StandardBias:=RTZ.StandardBias;
              RegTZ.DaylightBias:=RTZ.DaylightBias;
              RegTZ.StandardDate:=RTZ.StandardDate;
              RegTZ.DaylightDate:=RTZ.DaylightDate;
              if IsEqualTZ(HomeTZ,RegTZ) or AnsiSameText(s,sl[i]) then begin
                FDisp:=ReadString('Display');
                HomeTZ.StandardName:=RegTZ.StandardName;
                HomeTZ.DaylightName:=RegTZ.DaylightName;
                try
                  FMap:=ReadString('MapID');
                except
                  FMap:='';
                end;
                Break;
              end;
            end;
            CloseKey;
          end;
      end;
  finally
    Free;
    sl.Free;
  end;
  FBias:=HomeTZ.Bias;
  FStd:=HomeTZ.StandardName;
  FDay:=HomeTZ.DaylightName;
  DecodeDate(Now,y,m,d);
  GetTZDaylightSavingInfoForYear(HomeTZ,y,FDayStart,FStdStart,FDayBias,FStdBias);
  SetDataAvail(True);
end;

procedure TMiTeC_TimeZone.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(TimeZone_StorageFolderName);
    Sub:=SS.OpenSubStorage(TimeZone_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'DisplayName',Self.DisplayName);
        WriteStrProperty(sl,'StandardName',Self.StandardName);
        WriteStrProperty(sl,'DaylightName',Self.DaylightName);
        WriteIntProperty(sl,'Bias',Self.Bias);
        WriteIntProperty(sl,'StandardBias',Self.StandardBias);
        WriteIntProperty(sl,'DaylightBias',Self.DaylightBias);
        WriteDtProperty(sl,'StandardStart',Self.StandardStart);
        WriteDtProperty(sl,'DaylightStart',Self.DaylightStart);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

{ TMiTeC_OperatingSystem }

procedure TMiTeC_OperatingSystem.Clear;
begin
  FDirs.Clear;
  TimeZone.Clear;
  Internet.Clear;
  LocaleInfo.Clear;
  FPC.Clear;
  Finalize(FUR);
  FWEI.Clear;
end;

constructor TMiTeC_OperatingSystem.Create;
begin
  inherited Create(AOwner);
  FEnv:=TStringList.Create;
  FDirs:=TStringList.Create;
  FPC:=TStringList.Create;
  FTZ:=TMiTeC_TimeZone.Create(Self);
  FTZ.Name:='TimeZone';
  FInternet:=TMiTeC_Internet.Create(Self);
  FInternet.Name:='Internet';
  FLocale:=TMiTeC_LocaleInfo.Create(Self);
  FLocale.Name:='LocaleInfo';
  FWEI:=TMiTeC_WEI.Create(Self);
  FWEI.Name:='WindowsExperienceIndex';
  Finalize(FUR);
  FNoWMI:=False;
end;

destructor TMiTeC_OperatingSystem.Destroy;
begin
  FEnv.Free;
  FDirs.Free;
  FTZ.Free;
  FInternet.Free;
  FLocale.Free;
  FPC.Free;
  Finalize(FUR);
  FWEI.Free;
  inherited;
end;


function TMiTeC_OperatingSystem.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub, Sub1: TStructuredStorage;
  strm,strm1: TStorageStream;
  sl: TStringList;
  n,i: integer;
  nts: TNTSuite;
const
  NTSpec_StorageFolderName = 'NTSpecific';
  VistaSpec_StorageFolderName = 'VistaSpecific';

function ReadUpdateFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub1.OpenStream(Format(strm_Update,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            SetLength(Self.FUR,Length(Self.FUR)+1);
            with Self.FUR[High(Self.FUR)] do begin
              ID:=ReadStrProperty(sl,'ID');
              InstallDate:=ReadStrProperty(sl,'InstallDate');
              InstalledBy:=ReadStrProperty(sl,'InstalledBy');
              Description:=ReadStrProperty(sl,'Description');
              SPInEffect:=ReadStrProperty(sl,'SPInEffect');
              Comment:=ReadStrProperty(sl,'Comment');
            end;
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

begin
  inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B+}
  Clear;
  Result:=Self.Internet.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and Self.LocaleInfo.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and Self.TimeZone.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and Self.WindowsExperienceIndex.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B-}
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
      try
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FOS:=TOSVersion(ReadIntProperty(sl,'OS'));
              Self.FOSName:=ReadStrProperty(sl,'Name');
              Self.FEdition:=ReadStrProperty(sl,'Edition');
              Self.FVersion:=ReadStrProperty(sl,'Version');
              Self.FCSD:=ReadStrProperty(sl,'CSD');
              Self.FMajorVersion:=ReadIntProperty(sl,'MajorVersion');
              Self.FMinorVersion:=ReadIntProperty(sl,'MinorVersion');
              Self.FBuildNumber:=ReadIntProperty(sl,'BuildNumber');
              Self.FRelID:=ReadStrProperty(sl,'ReleaseID');
              Self.FFlags:=ReadIntProperty(sl,'Flags');
              Self.FInstallDate:=ReadDtProperty(sl,'InstallDate');
              Self.FProductID:=ReadStrProperty(sl,'ProductID');
              Self.FProductKey:=ReadStrProperty(sl,'ProductKey');
              Self.FRegUser:=ReadStrProperty(sl,'RegisteredUser');
              Self.FRegOrg:=ReadStrProperty(sl,'RegisteredOrganization');
              Self.FEnv.CommaText:=ReadStrProperty(sl,'Environment');
              Self.FDirs.CommaText:=ReadStrProperty(sl,'Folders');
              Self.FSysLangDefID:=ReadStrProperty(sl,'LanguageID');
              Self.FDVD:=ReadStrProperty(sl,'DVDRegion');
              Self.FBuildLab:=ReadStrProperty(sl,'BuildLab');
              Self.FOSBuild:=ReadStrProperty(sl,'OSBuild');
              Self.FMGUID:=ReadStrProperty(sl,'MachineGUID');
              Self.FTM:=ReadStrProperty(sl,'TaskManager');
              Self.FPN:=ReadStrProperty(sl,'ProductName');
              Self.FIT:=ReadStrProperty(sl,'InstallationType');
              Self.FTV:=ReadStrProperty(sl,'TrueWindowsVersion');
              Self.FCM:=not SameText(Self.FTV,Format('%d.%d.%d',[Self.FMajorVersion,Self.FMinorVersion,Self.FBuildNumber]));
              Self.FLiveID:=ReadStrProperty(sl,'LiveID');

              if Self.StorageInfo.SIFVersion=1 then begin
                try
                  Sub1:=SS.OpenSubStorage(NTSpec_StorageFolderName,STG_READ_INSTORAGE,False);
               except
                  Sub1:=nil;
               end;
               if Sub1<>nil then
                 try
                   strm1:=Sub1.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
                   if strm1<>nil then
                     try
                       sl.Clear;
                       LoadFromEncodedStream(strm1,sl,ACodeStream);
                     finally
                       strm1.Free;
                     end;
                 finally
                   Sub1.Free;
                 end;
              end;
              Self.FProduct:=TNTProductType(ReadIntProperty(sl,'ProductType'));
              Self.FSPMajorVer:=ReadIntProperty(sl,'ServicePackMajorVersion');
              Self.FSPMinorVer:=ReadIntProperty(sl,'ServicePackMinorVersion');
              Self.FNOLU:=ReadIntProperty(sl,'NumberOfLicensedUsers');
              Self.FNOU:=ReadIntProperty(sl,'NumberOfUsers');
              Self.FGW:=ReadIntProperty(sl,'Genuine')=1;
              n:=ReadIntProperty(sl,'InstalledSuites');
              Self.FSuites:=[];
              for nts:=Low(TNTSuite) to High(TNTSuite) do
                if n and Round(Power(2,Integer(nts)))<>0 then
                  Self.FSuites:=Self.FSuites+[nts];
              Self.FPI:=ReadIntProperty(sl,'ProductInfo');

              try
                if Self.StorageInfo.SIFVersion=1 then
                  Sub1:=SS.OpenSubStorage(NTSpec_StorageFolderName,STG_READ_INSTORAGE,False)
                else
                  Sub1:=SS.OpenSubStorage(Updates_StorageFolderName,STG_READ_INSTORAGE,False);
              except
                Sub1:=nil;
              end;

              try
                i:=0;
                while ReadUpdateFromStream(i) do
                  Inc(i);
              finally
                if Sub1<>nil then
                  Sub1.Free;
              end;

              if Self.StorageInfo.SIFVersion=1 then begin
                try
                  Sub1:=SS.OpenSubStorage(VistaSpec_StorageFolderName,STG_READ_INSTORAGE,False);
               except
                  Sub1:=nil;
               end;
               if Sub1<>nil then
                 try
                   strm1:=Sub1.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
                   if strm1<>nil then
                     try
                       sl.Clear;
                       LoadFromEncodedStream(strm1,sl,ACodeStream);
                     finally
                       strm1.Free;
                     end;
                 finally
                   Sub1.Free;
                 end;
              end;
              Self.FUAC:=ReadIntProperty(sl,'UserAccountControl')=1;
              Self.FV:=ReadIntProperty(sl,'Virtualization')=1;
              Self.FPC.CommaText:=ReadStrProperty(sl,'ParentalControls');

              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;

      finally
        Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_OperatingSystem.RefreshData;
var
  p: PAnsiChar;
  wp: PChar;
  n: Cardinal;
  WinH: HWND;
  s: string;
  ft: TFiletime;
  reg: TRegistry;
  VersionInfo: TOSVersionInfoEx;
  OS :TOSVersionInfo;
  sl,kl,vl: TStringList;
  i,j,k: Integer;
  wmi: TInstances;
  ur: TUpdateRecord;
  dt: TDateTime;
  wmiServices: ISWbemServices;
const
  rkOSInfoNT = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  //rkSP6a = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009';

  rvInstalled = 'Installed';
  rvVersionNameNT = 'CurrentType';
  rvRegOrg = 'RegisteredOrganization';
  rvRegOwn = 'RegisteredOwner';
  rvProductID = 'ProductID';
  rvProductName = 'ProductName';
  rvProductKey = 'ProductKey';
  rvDVD = 'DVD_Region';
  rvInstalldate = 'FirstInstallDateTime';
  rvBuildLab = 'BuildLab';
  rvBuildLabEx = 'BuildLabEx';
  rvUBR = 'UBR';
  rvInstallationType = 'InstallationType';

  rkPIDNT = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
    rvDPIDNT = 'DigitalProductID';
    rvInstalldateNT = 'InstallDate';

  rkProductTypeNT = {HKEY_LOCAL_MACHINE}'\System\CurrentControlSet\Control\ProductOptions';
    rvProductType = 'ProductType';

  rkHotFixes = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows NT\CurrentVersion\HotFix\';
    rvDesc = 'Fix Description';

  rkUpdates = '\SOFTWARE\Microsoft\Updates\';

  rkPolicies = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System';
    rvUAC = 'EnableLUA';
    rvVirt = 'EnableVirtualization';
  rkPC = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion\Parental Controls\Users\';
    rvPCO = 'Parental Controls On';

  SecsPerDay = 24*60*60;
begin
  inherited;

  FDirs.Clear;
  TimeZone.RefreshData;
  Internet.RefreshData;
  LocaleInfo.RefreshData;
  FWEI.RefreshData;

  GetServerInfo(FComment,FFlags,FUsers,FLicenses);

  FMajorVersion:=OSVIX.dwMajorVersion;
  FMinorVersion:=OSVIX.dwMinorVersion;
  FBuildNumber:=word(OSVIX.dwBuildNumber);
  FCSD:=OSVIX.szCSDVersion;
  FOS:=MiTeC_Routines.OS;
  FOSName:=MiTeC_Routines.OSName;
  FEdition:=MiTeC_Routines.OSEdition;

  FBits:=32;
  if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64 then
    FBits:=64;

  FRegUser:='';
  FRegOrg:='';
  FProductID:='';
  FPN:='';
  FRelId:='';
  FOSBuild:=IntToStr(OSVIX.dwBuildNumber);

  sl:=TStringList.Create;
  kl:=TStringList.Create;
  vl:=TStringList.Create;
  try
  reg:=OpenRegistryReadOnly;
  with reg do
    try
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkOSInfoNT,False) then begin
      if ValueExists(rvRegOrg) then
        FRegOrg:=ReadString(rvRegOrg);
      if ValueExists(rvRegOwn) then
        FRegUser:=ReadString(rvRegOwn);
      if ValueExists(rvVersionNameNT) then
        FVersion:=ReadString(rvVersionNameNT);
      if ValueExists(rvProductID) then
        FProductID:=ReadString(rvProductID);
      if ValueExists(rvProductKey) then
        FProductKey:=ReadString(rvProductKey);
      if ValueExists(rvBuildLab) then
        FBuildLab:=ReadString(rvBuildLab);
      if ValueExists(rvBuildLabEx) then
        FBuildLab:=ReadString(rvBuildLabEx);
      if ValueExists(rvUBR) then
        FOSBuild:=FOSBuild+'.'+IntToStr(ReadInteger(rvUBR));
      if ValueExists(rvProductName) then
        FPN:=ReadString(rvProductName);
      if ValueExists(rvInstallationType) then
        FIT:=ReadString(rvInstallationType);
      if ValueExists(rvInstallDate) then begin
        try
          n:=ReadInteger(rvInstallDate);
        except
          n:=GetDataSize(rvInstallDate);
          p:=AllocMem(n);
          try
            ReadBinarydata(rvInstallDate,p^,n);
            Move(p[0],n,n);
          finally
            FreeMem(p);
          end;
        end;
        DosDateTimeToFileTime(HiWord(n),LoWord(n),ft);
        FInstallDate:=FileTimeTodateTime(ft,ConvertTimeToLocal);
      end;
      if ValueExists(rvDVD) then
        FDVD:=ReadRegistryValueAsString(RootKey,CurrentPath,rvDVD);

      FTV:=GetTrueWindowsVersion;
      FCM:=not SameText(FTV,Format('%d.%d.%d',[FMajorVersion,FMinorVersion,FBuildNumber]));

      FDirs.Values['CommonFiles']:=ReadString('CommonFilesDir');
      FDirs.Values['ProgramFiles']:=ReadString('ProgramFilesDir');
      FDirs.Values['Device']:=ReadString('DevicePath');
      FDirs.Values['OtherDevice']:=ReadString('OtherDevicePath');
      FDirs.Values['Media']:=ReadString('MediaPath');
      FDirs.Values['Config']:=ReadString('ConfigPath');
      FDirs.Values['Wallpaper']:=ReadString('WallPaperDir');
      CloseKey;

      if OpenKey(rkPIDNT,False) then begin
        FProductKey:=ReadDPID(reg,rvDPIDNT);
        n:=ReadInteger(rvInstallDateNT);
        FInstallDate:=Int(Encodedate(1970,1,1));
        FInstallDate:=((FInstallDate*SecsPerDay)+n)/SecsPerDay;
        FRelId:=ReadString('ReleaseId');
        CloseKey;
      end;
    end;

    if FProduct=ptUnknown then begin
      if OpenKey(rkProductTypeNT,False) then begin
        s:=ReadString(rvProductType);
        if s='WinNT' then
          FProduct:=ptWorkStation
        else
          if s='ServerNT' then
            FProduct:=ptServer
          else
            if s='LanmanNT' then
              FProduct:=ptAdvancedServer;
        CloseKey;
      end;
    end;

    if OpenKey(rkPolicies,False) then begin
      if ValueExists(rvUAC) then
        FUAC:=ReadInteger(rvUAC)=1;
      if ValueExists(rvVirt) then
        FV:=ReadInteger(rvVirt)=1;
      CloseKey;
    end;
    if OpenKey(rkPC,False) then begin
      GetKeyNames(sl);
      GetAccountList(vl);
      CloseKey;
      for i:=0 to sl.Count-1 do begin
        if OpenKey(IncludeTrailingPathDelimiter(rkPC)+sl[i],False) then begin
          if ValueExists(rvPCO) and (ReadInteger(rvPCO)=1) then begin
            s:=GetAccountFromSID(ConvertStringToSID(sl[i]));
            if s='' then
              s:=vl.Values[sl[i]];
            if s='' then
              s:=sl[i];
            FPC.Add(s);
          end;
          CloseKey;
        end;
      end;
    end;
    sl.Clear;
    vl.Clear;

    if FDUH then begin
      if OpenKey(rkUpdates,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkUpdates+sl[i],False) then begin
            GetKeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do
              if OpenKey(rkUpdates+sl[i]+'\'+kl[j],False) then begin
                GetKeyNames(vl);
                CloseKey;
                for k:=0 to vl.Count-1 do
                  if OpenKey(rkUpdates+sl[i]+'\'+kl[j]+'\'+vl[k],False) then begin
                    Finalize(ur);
                    Zeromemory(@ur,SizeOf(ur));
                    ur.ID:=vl[k];
                    ur.SPInEffect:=sl[i]+' '+kl[j];
                    try ur.Description:=ReadString('Description') except end;
                    try ur.InstallDate:=ReadString('InstalledDate') except end;
                    try ur.InstalledBy:=ReadString('InstalledBy') except end;
                    try ur.Comment:=ReadString('Type') except end;

                    SetLength(FUR,Length(FUR)+1);
                    FUR[High(FUR)]:=ur;

                    CloseKey;
                  end;
              end;
          end;
      end;

      sl.Clear;
      if OpenKey(rkHotFixes,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rkHotFixes+sl[i],False) then begin
            if ValueExists(rvInstalled) then begin
              case GetDataType(rvInstalled) of
                rdString: k:=StrToIntDef(ReadString(rvInstalled),0);
                rdInteger: k:=ReadInteger(rvInstalled);
                else k:=0;;
              end;
              if k=1 then begin
                Finalize(ur);
                Zeromemory(@ur,SizeOf(ur));
                ur.ID:=sl[i];
                try ur.Description:=ReadString(rvDesc) except end;

                k:=-1;
                for j:=0 to High(FUR) do
                  if SameText(FUR[j].ID,ur.ID) then begin
                    k:=j;
                    Break;
                  end;
                if k=-1 then begin
                  SetLength(FUR,Length(FUR)+1);
                  FUR[High(FUR)]:=ur;
                end;

              end;
            end;
            CloseKey;
          end;
      end;
    end;

  finally
    Free;
  end;

  if FMajorVersion<5 then begin
    n:=MAX_PATH;
    wp:=StrAlloc(n);

    GetWindowsDirectory(wp,n);
    FDirs.Values['Windows']:=wp;

    GetSystemDirectory(wp,n);
    FDirs.Values['System']:=wp;

    GetTempPath(n,wp);
    FDirs.Values['Temp']:=wp;

    StrDispose(wp);
  end;

  if not SameText(LoggedUser,GetUser) then begin
    FDirs.Values['AppData']:=GetSpecialFolderRegEx('APPDATA',SessionID);
    FDirs.Values['CommonDesktopDir']:=GetSpecialFolderRegEx('COMMON DESKTOP DIRECTORY',SessionID);
    FDirs.Values['CommonAltStartUp']:=GetSpecialFolderRegEx('COMMON ALTSTARTUP',SessionID);
    FDirs.Values['RecycleBin']:=GetSpecialFolderRegEx('BITBUCKET',SessionID);
    FDirs.Values['CDBurnArea']:=GetSpecialFolderRegEx('CD BURNING',SessionID);
    FDirs.Values['CommonAppData']:=GetSpecialFolderRegEx('COMMON APPDATA',SessionID);
    FDirs.Values['CommonPrograms']:=GetSpecialFolderRegEx('COMMON PROGRAMS',SessionID);
    FDirs.Values['CommonStartMenu']:=GetSpecialFolderRegEx('COMMON START MENU',SessionID);
    FDirs.Values['CommonStartup']:=GetSpecialFolderRegEx('COMMON STARTUP',SessionID);
    FDirs.Values['CommonFavorites']:=GetSpecialFolderRegEx('COMMON FAVORITES',SessionID);
    FDirs.Values['CommonMusic']:=GetSpecialFolderRegEx('COMMONMUSIC',SessionID);
    FDirs.Values['CommonPictures']:=GetSpecialFolderRegEx('COMMONPICTURES',SessionID);
    FDirs.Values['CommonVideo']:=GetSpecialFolderRegEx('COMMONVIDEO',SessionID);
    FDirs.Values['CommonDocuments']:=GetSpecialFolderRegEx('COMMON DOCUMENTS',SessionID);
    FDirs.Values['CommonDesktop']:=GetSpecialFolderRegEx('COMMON DESKTOP',SessionID);
    FDirs.Values['Cookies']:=GetSpecialFolderRegEx('COOKIES',SessionID);
    FDirs.Values['Controls']:=GetSpecialFolderRegEx('CONTROLS',SessionID);
    FDirs.Values['Desktop']:=GetSpecialFolderRegEx('DESKTOP',SessionID);
    FDirs.Values['DesktopDir']:=GetSpecialFolderRegEx('DESKTOP DIRECTORY',SessionID);
    FDirs.Values['Favorites']:=GetSpecialFolderRegEx('FAVORITES',SessionID);
    FDirs.Values['Drives']:=GetSpecialFolderRegEx('DRIVES',SessionID);
    FDirs.Values['Fonts']:=GetSpecialFolderRegEx('FONTS',SessionID);
    FDirs.Values['History']:=GetSpecialFolderRegEx('HISTORY',SessionID);
    FDirs.Values['Internet']:=GetSpecialFolderRegEx('INTERNET',SessionID);
    FDirs.Values['InternetCache']:=GetSpecialFolderRegEx('CACHE',SessionID);
    FDirs.Values['LocalAppData']:=GetSpecialFolderRegEx('LOCAL APPDATA',SessionID);
    FDirs.Values['MyMusic']:=GetSpecialFolderRegEx('MY MUSIC',SessionID);
    FDirs.Values['MyPictures']:=GetSpecialFolderRegEx('MY PICTURES',SessionID);
    FDirs.Values['MyVideo']:=GetSpecialFolderRegEx('MY VIDEO',SessionID);
    FDirs.Values['NetWork']:=GetSpecialFolderRegEx('NETWORK',SessionID);
    FDirs.Values['NetHood']:=GetSpecialFolderRegEx('NETHOOD',SessionID);
    FDirs.Values['MyDocuments']:=GetSpecialFolderRegEx('PERSONAL',SessionID);
    FDirs.Values['Personal']:=GetSpecialFolderRegEx('PERSONAL',SessionID);
    FDirs.Values['PrintHood']:=GetSpecialFolderRegEx('PRINTHOOD',SessionID);
    FDirs.Values['Printers']:=GetSpecialFolderRegEx('PRINTERS',SessionID);
    FDirs.Values['Programs']:=GetSpecialFolderRegEx('PROGRAMS',SessionID);
    FDirs.Values['Recent']:=GetSpecialFolderRegEx('RECENT',SessionID);
    FDirs.Values['SendTo']:=GetSpecialFolderRegEx('SENDTO',SessionID);
    FDirs.Values['StartMenu']:=GetSpecialFolderRegEx('START MENU',SessionID);
    FDirs.Values['StartUp']:=GetSpecialFolderRegEx('STARTUP',SessionID);
    FDirs.Values['Templates']:=GetSpecialFolderRegEx('TEMPLATES',SessionID);
  end else begin
    WinH:=GetDesktopWindow;
    FDirs.Values['AdminTools']:=GetSpecialFolder(WinH,CSIDL_ADMINTOOLS);
    FDirs.Values['AltStartup']:=GetSpecialFolder(WinH,CSIDL_ALTSTARTUP);
    FDirs.Values['AppData']:=GetSpecialFolder(WinH,CSIDL_APPDATA);
    FDirs.Values['CDBurnArea']:=GetSpecialFolder(WinH,CSIDL_CDBURN_AREA);
    FDirs.Values['CommonAdminTools']:=GetSpecialFolder(WinH,CSIDL_COMMON_ADMINTOOLS);
    FDirs.Values['CommonDesktopDir']:=GetSpecialFolder(WinH,CSIDL_COMMON_DESKTOPDIRECTORY);
    FDirs.Values['CommonAltStartUp']:=GetSpecialFolder(WinH,CSIDL_COMMON_ALTSTARTUP);
    FDirs.Values['CommonAppData']:=GetSpecialFolder(WinH,CSIDL_COMMON_APPDATA);
    FDirs.Values['CommonDocuments']:=GetSpecialFolder(WinH,CSIDL_COMMON_DOCUMENTS);
    FDirs.Values['CommonFavorites']:=GetSpecialFolder(WinH,CSIDL_COMMON_FAVORITES);
    FDirs.Values['CommonMusic']:=GetSpecialFolder(WinH,CSIDL_COMMON_MUSIC);
    FDirs.Values['CommonPictures']:=GetSpecialFolder(WinH,CSIDL_COMMON_PICTURES);
    FDirs.Values['CommonStartMenu']:=GetSpecialFolder(WinH,CSIDL_COMMON_STARTMENU);
    FDirs.Values['CommonStartup']:=GetSpecialFolder(WinH,CSIDL_COMMON_STARTUP);
    FDirs.Values['CommonTemplates']:=GetSpecialFolder(WinH,CSIDL_COMMON_TEMPLATES);
    FDirs.Values['CommonVideo']:=GetSpecialFolder(WinH,CSIDL_COMMON_VIDEO);
    FDirs.Values['Cookies']:=GetSpecialFolder(WinH,CSIDL_COOKIES);
    FDirs.Values['Controls']:=GetSpecialFolder(WinH,CSIDL_CONTROLS);
    FDirs.Values['Desktop']:=GetSpecialFolder(WinH,CSIDL_DESKTOP);
    FDirs.Values['DesktopDir']:=GetSpecialFolder(WinH,CSIDL_DESKTOPDIRECTORY);
    FDirs.Values['Favorites']:=GetSpecialFolder(WinH,CSIDL_FAVORITES);
    FDirs.Values['Drives']:=GetSpecialFolder(WinH,CSIDL_DRIVES);
    FDirs.Values['Fonts']:=GetSpecialFolder(WinH,CSIDL_FONTS);
    FDirs.Values['History']:=GetSpecialFolder(WinH,CSIDL_HISTORY);
    FDirs.Values['Internet']:=GetSpecialFolder(WinH,CSIDL_INTERNET);
    FDirs.Values['InternetCache']:=GetSpecialFolder(WinH,CSIDL_INTERNET_CACHE);
    FDirs.Values['LocalAppData']:=GetSpecialFolder(WinH,CSIDL_LOCAL_APPDATA);
    FDirs.Values['NetWork']:=GetSpecialFolder(WinH,CSIDL_NETWORK);
    FDirs.Values['NetHood']:=GetSpecialFolder(WinH,CSIDL_NETHOOD);
    FDirs.Values['MyDocuments']:=GetSpecialFolder(WinH,CSIDL_PERSONAL);
    FDirs.Values['MyMusic']:=GetSpecialFolder(WinH,CSIDL_MYMUSIC);
    FDirs.Values['MyPictures']:=GetSpecialFolder(WinH,CSIDL_MYPICTURES);
    FDirs.Values['MyVideo']:=GetSpecialFolder(WinH,CSIDL_MYVIDEO);
    FDirs.Values['Personal']:=GetSpecialFolder(WinH,CSIDL_PERSONAL);
    FDirs.Values['PrintHood']:=GetSpecialFolder(WinH,CSIDL_PRINTHOOD);
    FDirs.Values['Printers']:=GetSpecialFolder(WinH,CSIDL_PRINTERS);
    FDirs.Values['Programs']:=GetSpecialFolder(WinH,CSIDL_PROGRAMS);
    FDirs.Values['Profile']:=GetSpecialFolder(WinH,CSIDL_PROFILE);
    FDirs.Values['Profiles']:=GetSpecialFolder(WinH,CSIDL_PROFILES);
    FDirs.Values['ProgramFiles']:=GetSpecialFolder(WinH,CSIDL_PROGRAM_FILES);
    FDirs.Values['ProgramFilesCommon']:=GetSpecialFolder(WinH,CSIDL_PROGRAM_FILES_COMMON);
    FDirs.Values['RecycleBin']:=GetSpecialFolder(WinH,CSIDL_BITBUCKET);
    FDirs.Values['Recent']:=GetSpecialFolder(WinH,CSIDL_RECENT);
    FDirs.Values['SendTo']:=GetSpecialFolder(WinH,CSIDL_SENDTO);
    FDirs.Values['StartMenu']:=GetSpecialFolder(WinH,CSIDL_STARTMENU);
    FDirs.Values['StartUp']:=GetSpecialFolder(WinH,CSIDL_STARTUP);
    FDirs.Values['System']:=GetSpecialFolder(WinH,CSIDL_SYSTEM);
    FDirs.Values['Windows']:=GetSpecialFolder(WinH,CSIDL_WINDOWS);
    FDirs.Values['Templates']:=GetSpecialFolder(WinH,CSIDL_TEMPLATES);
    if FMajorVersion>5 then begin
      FDirs.Values['Programs64']:=GetKnownFolderPath(FOLDERID_ProgramFilesX64);
      FDirs.Values['Programs32']:=GetKnownFolderPath(FOLDERID_ProgramFilesX86);
    end;
  end;
  if FMajorVersion<5 then begin
    s:=ReverseString(FDirs.Values['Desktop']);
    s:=ReverseString(Copy(s,Pos('\',s)+1,255));
    FDirs.Values['Profile']:=s;
  end;
  FEnv.Clear;
  GetEnvironment(FEnv);
  FSysLangDefID:=Format('$%.4x',[GetSystemDefaultLangID]);

  FMGUID:=ReadRegistryString(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Cryptography','MachineGUID');

  FTM:=GetTaskManager;

  FLiveID:=GetWindowsLiveID;

  FillChar(OS,SizeOf(OS),0);
  OS.dwOSVersionInfoSize:=SizeOf(OS);
  {$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.GetVersionEx(OS);
  if (OS.dwPlatformId=VER_PLATFORM_WIN32_NT) and (OS.dwMajorVersion>=5) then begin
    FillChar(VersionInfo,SizeOf(VersionInfo),0);
    VersionInfo.dwOSVersionInfoSize:=SizeOf(VersionInfo);
    if GetVersionEx(@VersionInfo) then begin
      if Assigned(GetProductInfo) then
        GetProductInfo(VersionInfo.dwMajorVersion,VersionInfo.dwMinorVersion,VersionInfo.wServicePackMajor,VersionInfo.wServicePackMinor,FPI);
      case VersionInfo.wProductType of
        VER_NT_WORKSTATION: FProduct:=ptWorkStation;
        VER_NT_DOMAIN_CONTROLLER: FProduct:=ptAdvancedServer;
        VER_NT_SERVER: FProduct:=ptServer;
      end;
      if (VersionInfo.dwMajorVersion>=5) and (VersionInfo.wProductType=VER_NT_DOMAIN_CONTROLLER) then
        FProduct:=ptServer;
      FSuites:=[];
      if VersionInfo.wSuiteMask and VER_SUITE_SMALLBUSINESS<>0 then
        FSuites:=FSuites+[suSmallBusiness];
      if VersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE<>0 then begin
        FSuites:=FSuites+[suEnterprise];
        FProduct:=ptAdvancedServer;
      end;
      if VersionInfo.wSuiteMask and VER_SUITE_BACKOFFICE<>0 then
        FSuites:=FSuites+[suBackOffice];
      if VersionInfo.wSuiteMask and VER_SUITE_COMMUNICATIONS<>0 then
        FSuites:=FSuites+[suCommunications];
      if VersionInfo.wSuiteMask and VER_SUITE_TERMINAL<>0 then
        FSuites:=FSuites+[suTerminal];
      if VersionInfo.wSuiteMask and VER_SUITE_SMALLBUSINESS_RESTRICTED<>0 then
        FSuites:=FSuites+[suSmallBusinessRestricted];
      if VersionInfo.wSuiteMask and VER_SUITE_EMBEDDEDNT<>0 then
        FSuites:=FSuites+[suEmbeddedNT];
      if VersionInfo.wSuiteMask and VER_SUITE_DATACENTER<>0 then begin
        FSuites:=FSuites+[suDataCenter];
        FProduct:=ptDataCenter;
      end;
      if VersionInfo.wSuiteMask and VER_SUITE_SINGLEUSERTS<>0 then
        FSuites:=FSuites+[suSingleUserTS];
      if VersionInfo.wSuiteMask and VER_SUITE_PERSONAL<>0 then
        FSuites:=FSuites+[suPersonal];
      if VersionInfo.wSuiteMask and VER_SUITE_BLADE<>0 then begin
        FSuites:=FSuites+[suBlade];
        FProduct:=ptWeb;
      end;
      if VersionInfo.wSuiteMask and VER_SUITE_EMBEDDED_RESTRICTED<>0 then
        FSuites:=FSuites+[suEmbeddedRestricted];
      if VersionInfo.wSuiteMask and VER_SUITE_STORAGE_SERVER<>0 then
        FSuites:=FSuites+[suStorageServer];
      if VersionInfo.wSuiteMask and VER_SUITE_COMPUTE_SERVER<>0 then
        FSuites:=FSuites+[suComputeCluster];
      if VersionInfo.wSuiteMask and VER_SUITE_WH_SERVER<>0 then
        FSuites:=FSuites+[suHomeServer];

      FSPMajorVer:=VersionInfo.wServicePackMajor;
      FSPMinorVer:=VersionInfo.wServicePackMinor;
    end;
  end;

  try
    FGW:=IsGenuine=SL_GEN_STATE_IS_GENUINE;
  except
    FGW:=False;
  end;

  if not FNoWMI then
  try
    if Length(FUR)=0 then begin
      if FDUH then begin
        if WMIConnect('','','',Rootnamespace,wmiServices) then begin
          WMICommand(wmiServices,'Win32_QuickFixEngineering',wmi);

          for i:=0 to High(wmi) do begin
            Finalize(ur);
            Zeromemory(@ur,SizeOf(ur));
            for j:=0 to High(wmi[i]) do begin
              if SameText(wmi[i][j].Name,'HotFixID') then
                ur.ID:=wmi[i][j].Value;
              if SameText(wmi[i][j].Name,'InstalledOn') and not SameText(wmi[i][j].Value,'null') then begin
                if Pos('/',wmi[i][j].Value)>0 then
                  ur.InstallDate:=wmi[i][j].Value
                else begin
                  ft.dwHighDateTime:=StrToIntDef('$'+Copy(wmi[i][j].Value,1,8),0);
                  ft.dwLowDateTime:=StrToIntDef('$'+Copy(wmi[i][j].Value,5,8),0);
                  dt:=FileTimeToDateTime(ft);
                  if ConvertTimeToLocal then
                    dt:=UTCToSystemTime(dt);
                  ur.InstallDate:=FormatDatetime('mm"/"dd"/"yyyy',dt);
                end;
              end;
              if SameText(wmi[i][j].Name,'InstallDate') and not SameText(wmi[i][j].Value,'null') then
                ur.InstallDate:=wmi[i][j].Value;
              if SameText(wmi[i][j].Name,'InstalledBy') and not SameText(wmi[i][j].Value,'null') then
                ur.InstalledBy:=wmi[i][j].Value;
              if SameText(wmi[i][j].Name,'Description') and not SameText(wmi[i][j].Value,'null') then
                ur.Description:=wmi[i][j].Value;
             if SameText(wmi[i][j].Name,'ServicePackInEffect') and not SameText(wmi[i][j].Value,'null') then
                 ur.SPInEffect:=wmi[i][j].Value;
              if SameText(wmi[i][j].Name,'FixComments') and not SameText(wmi[i][j].Value,'null') then
                ur.Comment:=wmi[i][j].Value;
            end;
            if Pos('File',ur.ID)=0 then begin
              SetLength(FUR,Length(FUR)+1);
              FUR[High(FUR)]:=ur;
            end;
          end;
          Finalize(wmi);
          WMIDisconnect(wmiServices);
        end;
      end;
    end;
    if WMIConnect('','','',Rootnamespace,wmiServices) then begin
      WMICommand(wmiServices,'Win32_OperatingSystem',wmi);
      FNOLU:=0;
      FNOU:=0;
      FNOLU:=StrToIntDef(GetInstancePropertyValue(wmi,'NumberOfLicensedUsers'),0);
      FNOU:=StrToIntDef(GetInstancePropertyValue(wmi,'NumberOfUsers'),0);
      Finalize(wmi);
      WMIDisconnect(wmiServices);
    end;

    if not FGW then
      if WMIConnect('','','',Rootnamespace,wmiServices) then begin
        try
          WMICommand(wmiServices,'Win32_WindowsProductActivation',wmi);
          FGW:=StrToIntDef(GetInstancePropertyValue(wmi,'ActivationRequired'),1)=0;
        except
          FGW:=False;
        end;
        Finalize(wmi);
        WMIDisconnect(wmiServices);
      end;
  finally
    WMIDisconnect(wmiServices);
  end;
  finally
    sl.Free;
    vl.Free;
    kl.Free;
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_OperatingSystem.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  n,i: integer;
  nts: TNTSuite;

procedure WriteUpdateToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'ID',Self.Updates[AIndex].ID);
    WriteStrProperty(sl,'InstallDate',Self.Updates[AIndex].InstallDate);
    WriteStrProperty(sl,'InstalledBy',Self.Updates[AIndex].InstalledBy);
    WriteStrProperty(sl,'Description',Self.Updates[AIndex].Description);
    WriteStrProperty(sl,'SPInEffect',Self.Updates[AIndex].SPInEffect);
    WriteStrProperty(sl,'Comment',Self.Updates[AIndex].Comment);
    strm:=Sub.OpenStream(Format(strm_Update,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Self.Internet.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Self.LocaleInfo.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Self.TimeZone.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  Self.WindowsExperienceIndex.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'OS',integer(Self.OS));
        WriteStrProperty(sl,'Name',Self.OSName);
        WriteStrProperty(sl,'Edition',Self.OSEdition);
        WriteStrProperty(sl,'Version',Self.Version);
        WriteStrProperty(sl,'CSD',Self.CSD);
        WriteIntProperty(sl,'MajorVersion',Self.MajorVersion);
        WriteIntProperty(sl,'MinorVersion',Self.MinorVersion);
        WriteIntProperty(sl,'BuildNumber',Self.BuildNumber);
        WriteStrProperty(sl,'ReleaseID',Self.ReleaseId);
        WriteIntProperty(sl,'Flags',integer(Self.Flags));
        WriteDtProperty(sl,'InstallDate',Self.InstallDate);
        WriteStrProperty(sl,'ProductID',Self.ProductID);
        WriteStrProperty(sl,'ProductKey',Self.ProductKey);
        WriteStrProperty(sl,'RegisteredUser',Self.RegisteredUser);
        WriteStrProperty(sl,'RegisteredOrganization',Self.RegisteredOrg);
        WriteStrProperty(sl,'Environment',Self.Environment.CommaText);
        WriteStrProperty(sl,'Folders',Self.Folders.CommaText);
        WriteStrProperty(sl,'LanguageID',Self.LanguageID);
        WriteStrProperty(sl,'DVDRegion',Self.DVDRegion);
        WriteStrProperty(sl,'BuildLab',Self.BuildLab);
        WriteStrProperty(sl,'OSBuild',Self.OSBuild);
        WriteStrProperty(sl,'MachineGUID',Self.MachineGUID);
        WriteStrProperty(sl,'TaskManager',Self.TaskManager);
        WriteStrProperty(sl,'TrueWindowsVersion',Self.TrueWindowsVersion);
        WriteStrProperty(sl,'InstallationType',Self.InstallationType);
        WriteStrProperty(sl,'ProductName',Self.ProductName);
        WriteStrProperty(sl,'LiveID',Self.LiveID);

        WriteIntProperty(sl,'ProductType',integer(Self.ProductType));
        n:=0;
        for nts:=Low(TNTSuite) to High(TNTSuite) do
          if nts in Self.InstalledSuites then
            n:=n+Round(Power(2,Integer(nts)));
        WriteIntProperty(sl,'InstalledSuites',n);
        WriteIntProperty(sl,'ServicePackMajorVersion',Self.ServicePackMajorVersion);
        WriteIntProperty(sl,'ServicePackMinorVersion',Self.ServicePackMinorVersion);
        WriteIntProperty(sl,'NumberOfLicensedUsers',Self.NumberOfLicensedUsers);
        WriteIntProperty(sl,'NumberOfUsers',Self.NumberOfUsers);
        WriteIntProperty(sl,'Genuine',Integer(Self.GenuineWindows));
        WriteIntProperty(sl,'ProductInfo',Self.ProductInfo);

        WriteIntProperty(sl,'UserAccountControl',integer(Self.UserAccountControl));
        WriteIntProperty(sl,'Virtualization',integer(Self.Virtualization));
        WriteStrProperty(sl,'ParentalControls',Self.ParentalControls.CommaText);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;

    SS.DeleteElement(Updates_StorageFolderName);
    Sub:=SS.OpenSubStorage(Updates_StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to Self.UpdateCount-1 do
        WriteUpdateToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_OperatingSystem.SetHeaderReader(const Value: THeaderReader);
begin
  inherited;
  FTZ.OnReadHeader:=Value;
  FInternet.OnReadHeader:=Value;
  FLocale.OnReadHeader:=Value;
end;

procedure TMiTeC_OperatingSystem.SetHeaderWriter(const Value: THeaderWriter);
begin
  inherited;
  FTZ.OnWriteHeader:=Value;
  FInternet.OnWriteHeader:=Value;
  FLocale.OnWriteHeader:=Value;
end;

function TMiTeC_OperatingSystem.GetUpdateCount: Cardinal;
begin
  Result:=Length(FUR);
end;

function TMiTeC_OperatingSystem.GetUpdateRec(Index: Cardinal): TUpdateRecord;
begin
  try
    Result:=FUR[Index];
  except
  end;
end;

procedure TMiTeC_OperatingSystem.GetInstalledSuitesStr(var sl: TStringList);
begin
  with sl do begin
    Add(Format('Microsoft Small Business Server=%d',
               [integer(suSmallBusiness in InstalledSuites)]));
    Add(Format('Enterprise Edition/Advanced Server=%d',
               [integer(suEnterprise in InstalledSuites)]));
    Add(Format('Microsoft BackOffice Components=%d',
               [integer(suBackOffice in InstalledSuites)]));
    Add(Format('Communications=%d',
               [integer(suCommunications in InstalledSuites)]));
    Add(Format('Microsoft Small Business Server with the restrictive client license in force=%d',
               [integer(suSmallBusinessRestricted in InstalledSuites)]));
    Add(Format('Terminal Services=%d',
               [integer(suTerminal in InstalledSuites)]));
    Add(Format('Embedded NT=%d',
               [integer(suEmbeddedNT in InstalledSuites)]));
    Add(Format('Datacenter Edition/Datacenter Server=%d',
               [integer(suDataCenter in InstalledSuites)]));
    Add(Format('Web Edition=%d',
               [integer(suBlade in InstalledSuites)]));
    Add(Format('Storage Server=%d',
               [integer(suStorageServer in InstalledSuites)]));
    Add(Format('Compute Cluster Edition=%d',
               [integer(suComputeCluster in InstalledSuites)]));
    Add(Format('Home Server=%d',
               [integer(suHomeServer in InstalledSuites)]));
  end;
end;

procedure TMiTeC_Internet.Clear;
begin
  FProxy.Clear;
end;

constructor TMiTeC_Internet.Create;
begin
  inherited Create(AOwner);
  FProxy:=TStringList.Create;
end;

destructor TMiTeC_Internet.Destroy;
begin
  FProxy.Destroy;
  inherited;
end;

function TMiTeC_Internet.GetConnTypeStr(ACType: TConnectionType): string;
begin
  case ACType of
    ctNone: Result:='None';
    ctDialup: Result:='Dialup';
    ctLAN: Result:='LAN';
  end;
end;

function TMiTeC_Internet.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(Internet_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
      try
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FBrowser:=ReadStrProperty(sl,'DefaultBrowser');
              Self.FBrowserName:=ReadStrProperty(sl,'DefaultBrowserName');
              Self.FProxy.CommaText:=ReadStrProperty(sl,'ProxyServer');
              Self.FMailClient:=ReadStrProperty(sl,'DefaultMailClient');
              Self.FMailClientName:=ReadStrProperty(sl,'DefaultMailClientName');
              Self.FCType:=TConnectionType(ReadIntProperty(sl,'ConnectionType'));
              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;
      finally
        Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Internet.RefreshData;
const
  rkBrowser = 'StartMenuInternet';
  rkMail = 'Mail';
var
  i: Integer;
begin
  inherited;
  Clear;
  GetURLAssoc('http',FBrowserName,FBrowser,SessionID);
  if FBrowserName='' then
    GetClient(rkBrowser,FBrowserName,FBrowser,SessionID);
  GetURLAssoc('mailto',FMailClientName,FMailClient,SessionID);
  if FMailClientName='' then
    GetClient(rkMail,FMailClientName,FMailClient,SessionID);

  FCType:=GetConnectionType;
  FProxy.CommaText:=StringReplace(GetProxyserver(SessionID),';',',',[rfIgnorecase,rfReplaceAll]);
  for i:=0 to FProxy.Count-1 do
    if Pos('=',FProxy[i])=0 then
      FProxy[i]:=Format('%d=',[i])+FProxy[i];

  SetDataAvail(True);
end;


procedure TMiTeC_Internet.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Internet_StorageFolderName);
    Sub:=SS.OpenSubStorage(Internet_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'DefaultBrowser',Self.DefaultBrowser);
        WriteStrProperty(sl,'DefaultMailClient',Self.DefaultMailClient);
        WriteStrProperty(sl,'DefaultBrowserName',Self.DefaultBrowserName);
        WriteStrProperty(sl,'DefaultMailClientName',Self.DefaultMailClientName);
        WriteStrProperty(sl,'ProxyServer',Self.ProxyServer.CommaText);
        WriteIntProperty(sl,'ConnectionType',integer(Self.ConnectionType));

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

{ TMiTeC_LocaleInfo }


procedure TMiTeC_LocaleInfo.Clear;
begin

end;

constructor TMiTeC_LocaleInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocaleID:=LOCALE_USER_DEFAULT;
end;

function TMiTeC_LocaleInfo.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(Locale_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
      try
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FLocaleID:=ReadIntProperty(sl,'LocaleID');
              Self.FLang:=ReadStrProperty(sl,'FullLocalizeLanguage');
              Self.FEngLang:=ReadStrProperty(sl,'FullLanguageEnglishName');
              Self.FAbbrLang:=ReadStrProperty(sl,'AbbreviateLanguageName');
              Self.FCountry:=ReadStrProperty(sl,'CountryCode');
              Self.FFCountry:=ReadStrProperty(sl,'FullCountryCode');
              Self.FAbbrCtry:=ReadStrProperty(sl,'AbbreviateCountyCode');
              Self.FList:=ReadStrProperty(sl,'ListSeparator');
              Self.FDecimal:=ReadStrProperty(sl,'DecimalSeparator');
              Self.FDigit:=ReadStrProperty(sl,'NumberOfDecimalDigits');
              Self.FCurrency:=ReadStrProperty(sl,'LocalMonetarySymbol');
              Self.FIntlSymbol:=ReadStrProperty(sl,'InternationalMonetarySymbol');
              Self.FMonDecSep:=ReadStrProperty(sl,'CurrencyDecimalSeparator');
              Self.FMonThoSep:=ReadStrProperty(sl,'CurrencyThousandSeparator');
              Self.FCurrdigit:=ReadStrProperty(sl,'CurrencyDecimalDigits');
              Self.FNCurrMode:=ReadStrProperty(sl,'NegativeCurrencyMode');
              Self.FDate:=ReadStrProperty(sl,'DateSeparator');
              Self.FTime:=ReadStrProperty(sl,'TimeSeparator');
              Self.FTimeFormat:=ReadStrProperty(sl,'TimeFormat');
              Self.FShortDate:=ReadStrProperty(sl,'ShortDateFormat');
              Self.FLongDate:=ReadStrProperty(sl,'LongDateFormat');
              Self.FDecThoSep:=ReadStrProperty(sl,'DecimalThousandSeparator');
              Self.FMeasure:=TMeasureSystem(ReadIntProperty(sl,'MeasurementSystem'));
              Self.FPCurrMode:=TPositiveCurrencyMode(ReadIntProperty(sl,'PositiveCurrencyMode'));
              Self.FShortDateOrdr:=TDateOrder(ReadIntProperty(sl,'ShortDateOrder'));
              Self.FLongDateOrdr:=TDateOrder(ReadIntProperty(sl,'LongDateOrder'));
              Self.FTimeFormatSpec:=TTimeFormat(ReadIntProperty(sl,'TimeFormatSpecifier'));
              Self.FYearFormat:=TYearFormat(ReadIntProperty(sl,'YearFormat'));
              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;
        finally
          Sub.Free;
        end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_LocaleInfo.RefreshData;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Clear;
  try
    inherited;

    GetLocaleInfo(LocaleID,LOCALE_SLANGUAGE,@Buffer,sizeof(Buffer));
    FLang:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SENGLANGUAGE,@Buffer,sizeof(Buffer));
    FEngLang:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SABBREVLANGNAME,@Buffer,sizeof(Buffer));
    FAbbrLang:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_ICOUNTRY,@Buffer,sizeof(Buffer));
    FCountry:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SCOUNTRY,@Buffer,sizeof(Buffer));
    FFCountry:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SABBREVCTRYNAME,@Buffer,sizeof(Buffer));
    FAbbrCtry:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SLIST,@Buffer,sizeof(Buffer));
    FList:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_IMEASURE,@Buffer,sizeof(Buffer));
    FMeasure:=TMeasureSystem(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_SDECIMAL,@Buffer,sizeof(Buffer));
    FDecimal:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_IDIGITS,@Buffer,sizeof(Buffer));
    FDigit:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SCURRENCY,@Buffer,sizeof(Buffer));
    FCurrency:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SINTLSYMBOL,@Buffer,sizeof(Buffer));
    FIntlSymbol:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SMONDECIMALSEP,@Buffer,sizeof(Buffer));
    FMonDecSep:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SMONTHOUSANDSEP,@Buffer,sizeof(Buffer));
    FMonThoSep:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_ICURRDIGITS,@Buffer,sizeof(Buffer));
    FCurrdigit:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_ICURRENCY,@Buffer,sizeof(Buffer));
    FPCurrMode:=TPositiveCurrencyMode(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_INEGCURR,@Buffer,sizeof(Buffer));
    FNCurrMode:=StringReplace(SNegativeCurrencyMode[StrToInt(Buffer[0])],'$',FCurrency,[rfIgnoreCase]);
    GetLocaleInfo(LocaleID,LOCALE_SDATE,@Buffer,sizeof(Buffer));
    FDate:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_STIME,@Buffer,sizeof(Buffer));
    FTime:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_STIMEFORMAT,@Buffer,sizeof(Buffer));
    FTimeFormat:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SSHORTDATE,@Buffer,sizeof(Buffer));
    FShortDate:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_SLONGDATE,@Buffer,sizeof(Buffer));
    FLongDate:=Buffer;
    GetLocaleInfo(LocaleID,LOCALE_IDATE,@Buffer,sizeof(Buffer));
    FShortDateOrdr:=TDateOrder(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_ILDATE,@Buffer,sizeof(Buffer));
    FLongDateOrdr:=TDateOrder(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_ITIME,@Buffer,sizeof(Buffer));
    FTimeFormatSpec:=TTimeFormat(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_ICENTURY,@Buffer,sizeof(Buffer));
    FYearFormat:=TYearFormat(StrToInt(Buffer[0]));
    GetLocaleInfo(LocaleID,LOCALE_STHOUSAND,@Buffer,sizeof(Buffer));
    FDecThoSep:=Buffer;


  finally
  end;
  SetDataAvail(True);
end;

procedure TMiTeC_LocaleInfo.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(Locale_StorageFolderName);
    Sub:=SS.OpenSubStorage(Locale_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'LocaleID',Self.LocaleID);
        WriteStrProperty(sl,'FullLocalizeLanguage',Self.FullLocalizeLanguage);
        WriteStrProperty(sl,'FullLanguageEnglishName',Self.FullLanguageEnglishName);
        WriteStrProperty(sl,'AbbreviateLanguageName',Self.AbbreviateLanguageName);
        WriteStrProperty(sl,'CountryCode',Self.CountryCode);
        WriteStrProperty(sl,'FullCountryCode',Self.FullCountryCode);
        WriteStrProperty(sl,'AbbreviateCountyCode',Self.AbbreviateCountryCode);
        WriteStrProperty(sl,'ListSeparator',Self.ListSeparator);
        WriteStrProperty(sl,'DecimalSeparator',Self.DecimalSeparator);
        WriteStrProperty(sl,'NumberOfDecimalDigits',Self.NumberOfDecimalDigits);
        WriteStrProperty(sl,'LocalMonetarySymbol',Self.LocalMonetarySymbol);
        WriteStrProperty(sl,'InternationalMonetarySymbol',Self.InternationalMonetarySymbol);
        WriteStrProperty(sl,'CurrencyDecimalSeparator',Self.CurrencyDecimalSeparator);
        WriteStrProperty(sl,'CurrencyThousandSeparator',Self.CurrencyThousandSeparator);
        WriteStrProperty(sl,'CurrencyDecimalDigits',Self.CurrencyDecimalDigits);
        WriteStrProperty(sl,'NegativeCurrencyMode',Self.NegativeCurrencyMode);
        WriteStrProperty(sl,'DateSeparator',Self.DateSeparator);
        WriteStrProperty(sl,'TimeSeparator',Self.TimeSeparator);
        WriteStrProperty(sl,'TimeFormat',Self.TimeFormat);
        WriteStrProperty(sl,'ShortDateFormat',Self.ShortDateFormat);
        WriteStrProperty(sl,'LongDateFormat',Self.LongDateFormat);
        WriteStrProperty(sl,'DecimalThousandSeparator',Self.DecimalThousandSeparator);
        WriteIntProperty(sl,'MeasurementSystem',integer(Self.MeasurementSystem));
        WriteIntProperty(sl,'PositiveCurrencyMode',integer(Self.PositiveCurrencyMode));
        WriteIntProperty(sl,'ShortDateOrder',integer(Self.ShortDateOrder));
        WriteIntProperty(sl,'LongDateOrder',integer(Self.LongDateOrder));
        WriteIntProperty(sl,'TimeFormatSpecifier',integer(Self.TimeFormatSpecifier));
        WriteIntProperty(sl,'YearFormat',integer(Self.YearFormat));

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

{ TMiTeC_WEI }

procedure TMiTeC_WEI.Clear;
begin
  FSystemScore:=0;
  FCPUScore:=0;
  FDiskScore:=0;
  FMemoryScore:=0;
  FGraphicsScore:=0;
  FGamingScore:=0;
end;

function TMiTeC_WEI.LoadFromStorage(const AFilename: string;
  var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure): boolean;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(TimeZone_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
      try
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FSystemScore:=ReadDblProperty(sl,'SystemScore');
              Self.FCPUScore:=ReadDblProperty(sl,'CpuScore');
              Self.FDiskScore:=ReadDblProperty(sl,'DiskScore');
              Self.FMemoryScore:=ReadDblProperty(sl,'MemoryScore');
              Self.FGraphicsScore:=ReadDblProperty(sl,'GraphicsScore');
              Self.FGamingScore:=ReadDblProperty(sl,'GamingScore');
              Result:=True;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;
      finally
        Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_WEI.RefreshData(AScanObjects: TScanObjects);
var
  WinSAT: IQueryRecentWinSATAssessment;
  info: IProvideWinSATAssessmentInfo;
  AllWinSAT: IQueryAllWinSATAssessments;
  nodes: IXMLDOMNodeList;
  n: IXMLDOMNode;
  fs: TFormatSettings;
begin
  fs.DecimalSeparator:='.';
  Clear;
  try
    CoInitialize(nil);
    WinSAT:=CoCQueryWinSAT.Create;
    FSystemScore:=WinSAT.Info.SystemRating;
    info:=WinSAT.Info.GetAssessmentInfo(WINSAT_ASSESSMENT_GRAPHICS);
    FGraphicsScore:=info.Score;
    info:=WinSAT.Info.GetAssessmentInfo(WINSAT_ASSESSMENT_D3D);
    FGamingScore:=info.Score;
    info:=WinSAT.Info.GetAssessmentInfo(WINSAT_ASSESSMENT_CPU);
    FCPUScore:=info.Score;
    info:=WinSAT.Info.GetAssessmentInfo(WINSAT_ASSESSMENT_MEMORY);
    FMemoryScore:=info.Score;
    info:=WinSAT.Info.GetAssessmentInfo(WINSAT_ASSESSMENT_DISK);
    FDiskScore:=info.Score;

    if SameValue(FSystemScore,0) then begin
      AllWinSAT:=CoCQueryAllWinSAT.Create;
      nodes:=AllWinSAT.AllXML['WinsatAssessments/WinSAT/WinSPR',''];
      if Assigned(nodes) then begin
        n:=nodes.nextnode;
        if Assigned(n) then begin
          FSystemScore:=StrToFloatDef(n.selectSingleNode('SystemScore').text,0,fs);
          FMemoryScore:=StrToFloatDef(n.selectSingleNode('MemoryScore').text,0,fs);
          FGraphicsScore:=StrToFloatDef(n.selectSingleNode('GraphicsScore').text,0,fs);
          FGamingScore:=StrToFloatDef(n.selectSingleNode('GamingScore').text,0,fs);
          FDiskScore:=StrToFloatDef(n.selectSingleNode('DiskScore').text,0,fs);
          FCPUScore:=StrToFloatDef(n.selectSingleNode('CpuScore').text,0,fs);
        end;
      end;
    end;
  except
  end;
end;

procedure TMiTeC_WEI.SaveToStorage(const AFilename: string;
  var AWriteHeader: Boolean; AFormat: integer; const AComment: string;
  ACodeStream: TCodeStreamProcedure);
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(TimeZone_StorageFolderName);
    Sub:=SS.OpenSubStorage(TimeZone_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteDblProperty(sl,'SystemScore',Self.SystemScore);
        WriteDblProperty(sl,'CpuScore',Self.CPUScore);
        WriteDblProperty(sl,'MemoryScore',Self.MemoryScore);
        WriteDblProperty(sl,'GraphicsScore',Self.GraphicsScore);
        WriteDblProperty(sl,'GamingScore',Self.GamingScore);
        WriteDblProperty(sl,'DiskScore',Self.DiskScore);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.


