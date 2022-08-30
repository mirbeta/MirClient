{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Engines Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Engines;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, Variants, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MiTeC_WnASPI32, MSI_Common, MSI_Defs;

const
  StorageFolderName = 'Engines';
  ASPI32_StorageFolderName = 'ASPI32';
  DirectX_StorageFolderName = 'DirectX';

type
  TMiTeC_DirectX = class(TMiTeC_Component)
  private
    FVersion: string;
    FDirect3D: TStrings;
    FDirectPlay: TStrings;
    FDirectMusic: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    property Version: string read FVersion stored false;
    property Direct3D: TStrings read FDirect3D stored false;
    property DirectPlay: TStrings read FDirectPlay stored false;
    property DirectMusic: TStrings read FDirectMusic stored false;
  end;

  TMiTeC_ASPI32 = class(TMiTeC_Component)
  private
    FASPI: string;
    FASPIConfig: TASPIConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    function GetTypeStr(AType: Integer): string;
    property Configuration: TASPIConfig read FASPIConfig;
  published
    property ASPI :string read FASPI stored False;
  end;

  TMiTeC_Engines = class(TMiTeC_Component)
  private
    FBDE: string;
    FODBC: string;
    FDAO: string;
    FADO: string;
    FASPI32: TMiTeC_ASPI32;
    FDirectX: TMiTeC_DirectX;
    FNET: string;
    FOpenGL: string;
    FIE: string;
    FMSI: string;
    FQT: string;
  protected
    procedure SetHeaderReader(const Value: THeaderReader); override;
    procedure SetHeaderWriter(const Value: THeaderWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published

    property ODBC: string read FODBC stored false;
    property BDE: string read FBDE stored false;
    property DAO: string read FDAO stored False;
    property ADO: string read FADO stored False;
    property NET: string read FNET stored False;
    property OpenGL: string read FOpenGL stored False;
    property IE: string read FIE stored False;
    property MSI: string read FMSI stored False;
    property QT: string read FQT stored False;

    property DirectX: TMiTeC_DirectX read FDirectX stored False;
    property ASPI32: TMiTeC_ASPI32 read FASPI32 stored False;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_RegUtils;

{ TMiTeC_DirectX }

procedure TMiTeC_DirectX.Clear;
begin
  FDirect3D.Clear;
  FDirectPlay.Clear;
  FDirectMusic.Clear;
end;

constructor TMiTeC_DirectX.Create;
begin
  inherited Create(AOwner);
  FDirect3D:=TStringlist.Create;
  FDirectPlay:=TStringlist.Create;
  FDirectMusic:=TStringlist.Create;
end;

destructor TMiTeC_DirectX.Destroy;
begin
  FDirect3D.Free;
  FDirectPlay.Free;
  FDirectMusic.Free;
  inherited;
end;

function TMiTeC_DirectX.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(DirectX_StorageFolderName,STG_READ_INSTORAGE,False);
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
            Self.FVersion:=ReadStrProperty(sl,'Version');
            Self.FDirect3D.CommaText:=ReadStrProperty(sl,'Direct3D');
            Self.FDirectPlay.CommaText:=ReadStrProperty(sl,'DirectPlay');
            Self.FDirectMusic.CommaText:=ReadStrProperty(sl,'DirectMusic');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_DirectX.RefreshData;

procedure AddIfNotExists(AList: TStrings; const S: string);
var
  i: Longint;
begin
  for i:=0 to AList.Count-1 do
    if SameText(S,AList.Strings[i]) then
      Exit;
  AList.Add(S);
end;

var
  bdata :PChar;
  sl :TStringList;
  i,k :Integer;
  X64: Integer;
  Reg : TRegistry;
const
  rkDirectX = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\DirectX';
    rvDXVersionNT = 'InstalledVersion';
    rvDXVersion95 = 'Version';
  rkDirect3D = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Direct3D\Drivers';
  rkDirectPlay = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\DirectPlay\Services';
  rkDirectMusic = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\DirectMusic\SoftwareSynths';
    rvDesc = 'Description';
begin
  inherited;

  Clear;

  if IsWow64 then
    X64:=1
  else
    X64:=0;

  for k:=0 to X64 do begin
    if k=0 then
      Reg:=TRegistry.Create(KEY_READ)
    else
      Reg:=TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
    with Reg do begin
      rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkDirectX,False) then begin
        bdata:=stralloc(255);
        if ValueExists(rvDXVersion95) then
          FVersion:=ReadString(rvDXVersion95);
        if FVersion='' then
          if ValueExists(rvDXVersionNT) then
            try
              readbinarydata(rvDXVersionNT,bdata^,4);
              FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
            except
              try
                readbinarydata(rvDXVersionNT,bdata^,8);
                FVersion:=inttostr(lo(integer(bdata^)))+'.'+inttostr(hi(integer(bdata^)));
              except
              end;
            end;
        closekey;
        strdispose(bdata);
      end;

      sl:=tstringlist.create;
      if OpenKey(rkDirect3D,False) then begin
        getkeynames(sl);
        closekey;
        for i:=0 to sl.count-1 do
          if OpenKey(rkDirect3D+'\'+sl[i],False) then begin
            if ValueExists(rvDesc) then
              AddIfNotExists(FDirect3D,ReadString(rvDesc));
            closekey;
          end;
      end;
      if OpenKey(rkDirectPlay,False) then begin
        getkeynames(sl);
        closekey;
        for i:=0 to sl.count-1 do
          if OpenKey(rkDirectPlay+'\'+sl[i],False) then begin
            if ValueExists(rvDesc) then
              AddIfNotExists(FDirectPlay,ReadString(rvDesc));
            closekey;
          end;
      end;
      if OpenKey(rkDirectMusic,False) then begin
        getkeynames(sl);
        closekey;
        for i:=0 to sl.count-1 do
          if OpenKey(rkDirectMusic+'\'+sl[i],False) then begin
            if ValueExists(rvDesc) then
              AddIfNotExists(FDirectMusic,ReadString(rvDesc));
            closekey;
          end;
      end;
      sl.free;
      free;
    end;
  end;

  SetDataAvail(True);
end;


procedure TMiTeC_DirectX.SaveToStorage;
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
    SS.DeleteElement(DirectX_StorageFolderName);
    Sub:=SS.OpenSubStorage(DirectX_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'Version',Self.Version);
        WriteStrProperty(sl,'Direct3D',Self.Direct3D.CommaText);
        WriteStrProperty(sl,'DirectPlay',Self.DirectPlay.CommaText);
        WriteStrProperty(sl,'DirectMusic',Self.DirectMusic.CommaText);

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

{ TMiTeC_ASPI32 }

procedure TMiTeC_ASPI32.Clear;
begin
  FASPI:='';
end;

constructor TMiTeC_ASPI32.Create;
begin
  inherited Create(AOwner);
  FASPIConfig.Host:=TStringList.Create;
  FASPIConfig.LUN:=TStringList.Create;
  FASPIConfig.ID:=TStringList.Create;
  FASPIConfig.Vendor:=TStringList.Create;
  FASPIConfig.Model:=TStringList.Create;
  FASPIConfig.Typ:=TStringList.Create;
  FASPIConfig.Status:=TStringList.Create;
  FASPIConfig.Revision:=TStringList.Create;
  FASPIConfig.Extra:=TStringList.Create;
end;

destructor TMiTeC_ASPI32.Destroy;
begin
  FASPIConfig.Host.Free;
  FASPIConfig.LUN.Free;
  FASPIConfig.ID.Free;
  FASPIConfig.Vendor.Free;
  FASPIConfig.Model.Free;
  FASPIConfig.Typ.Free;
  FASPIConfig.Status.Free;
  FASPIConfig.Revision.Free;
  FASPIConfig.Extra.Free;
  inherited;
end;

function TMiTeC_ASPI32.GetTypeStr(AType: Integer): string;
begin
  case AType of
    0: Result:='Disk';
    1: Result:='Tape';
    2: Result:='Printer';
    3: Result:='Processor';
    4: Result:='Optical Disk';
    5: Result:='CD/DVD';
    6: Result:='Scanner';
    7: Result:='Optical Disk';
    8: Result:='Medium Changer';
    9: Result:='Communications';
    10: Result:='Graphics';
    11: Result:='Graphics';
    12: Result:='Storage Array';
    13: Result:='Enclosure';
    14: Result:='Simplified Disk';
    15: Result:='Optical Card Reader';
  end;
end;

function TMiTeC_ASPI32.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(ASPI32_StorageFolderName,STG_READ_INSTORAGE,False);
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
                Self.FASPI:=ReadStrProperty(sl,'ASPI');
                Self.FASPIConfig.AdapterCount:=ReadIntProperty(sl,'AdapterCount');
                Self.FASPIConfig.Host.CommaText:=ReadStrProperty(sl,'Host');
                Self.FASPIConfig.LUN.CommaText:=ReadStrProperty(sl,'LUN');
                Self.FASPIConfig.ID.CommaText:=ReadStrProperty(sl,'ID');
                Self.FASPIConfig.Vendor.CommaText:=ReadStrProperty(sl,'Vendor');
                Self.FASPIConfig.Model.CommaText:=ReadStrProperty(sl,'Model');
                Self.FASPIConfig.Typ.CommaText:=ReadStrProperty(sl,'Typ');
                Self.FASPIConfig.Status.CommaText:=ReadStrProperty(sl,'Status');
                Self.FASPIConfig.Extra.CommaText:=ReadStrProperty(sl,'Extra');
                Self.FASPIConfig.Revision.CommaText:=ReadStrProperty(sl,'Revision');
                Result:=True;
                SetDataAvail(True);
              finally
                sl.Free;
              end;
            finally
              strm.Free;
            end;
      finally
        if Sub<>nil then
          Sub.Free;
      end;
  finally
    SS.Free;
  end;
end;
    
procedure TMiTeC_ASPI32.RefreshData;
var
  s: string;
  VI: TVersionInfo;
begin
  inherited;
  Clear;
  InitASPI;
  try
  s:=GetWinSysDir;
  if HIBYTE(LOWORD(ExecuteASPI32Test(FASPIConfig)))=SS_COMP then
    if FileSearch(ASPI_DLL,s)='' then
      FASPI:=''
    else begin
      GetFileVerInfo(ASPI_DLL,VI);
      FASPI:=VI.FileVersion;
    end;
  finally
    FreeASPI;
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_ASPI32.SaveToStorage;
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
    SS.DeleteElement(ASPI32_StorageFolderName);
    Sub:=SS.OpenSubStorage(ASPI32_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'ASPI',Self.ASPI);
        WriteIntProperty(sl,'AdapterCount',Self.Configuration.AdapterCount);
        WriteStrProperty(sl,'Host',Self.Configuration.Host.CommaText);
        WriteStrProperty(sl,'LUN',Self.Configuration.LUN.CommaText);
        WriteStrProperty(sl,'ID',Self.Configuration.ID.CommaText);
        WriteStrProperty(sl,'Vendor',Self.Configuration.Vendor.CommaText);
        WriteStrProperty(sl,'Model',Self.Configuration.Model.CommaText);
        WriteStrProperty(sl,'Typ',Self.Configuration.Typ.CommaText);
        WriteStrProperty(sl,'Status',Self.Configuration.Status.CommaText);
        WriteStrProperty(sl,'Extra',Self.Configuration.Extra.CommaText);
        WriteStrProperty(sl,'Revision',Self.Configuration.Revision.CommaText);

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

{ TMiTeC_Engines }

procedure TMiTeC_Engines.Clear;
begin
  DirectX.Clear;
  ASPI32.Clear;
  FBDE:='';
  FODBC:='';
  FDAO:='';
  FADO:='';
  FNET:='';
  FOpenGL:='';
  FIE:='';
  FMSI:='';
  FQT:='';
end;

constructor TMiTeC_Engines.Create;
begin
  inherited Create(AOwner);
  FDirectX:=TMiTeC_DirectX.Create(Self);
  FDirectX.Name:='DirectX';
  FASPI32:=TMiTeC_ASPI32.Create(Self);
  FASPI32.Name:='ASPI32';
end;

destructor TMiTeC_Engines.Destroy;
begin
  FDirectX.Free;
  FASPI32.Free;
  inherited;
end;

function TMiTeC_Engines.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

  strm: TStorageStream;
  sl: TStringList;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B+}
  Result:=Result and ASPI32.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and DirectX.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
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
            Self.FBDE:=ReadStrProperty(sl,'BDE');
            Self.FODBC:=ReadStrProperty(sl,'ODBC');
            Self.FDAO:=ReadStrProperty(sl,'DAO');
            Self.FADO:=ReadStrProperty(sl,'ADO');
            Self.FNET:=ReadStrProperty(sl,'NET');
            Self.FOpenGL:=ReadStrProperty(sl,'OpenGL');
            Self.FIE:=ReadStrProperty(sl,'IE');
            Self.FMSI:=ReadStrProperty(sl,'MSI');
            Self.FQT:=ReadStrProperty(sl,'QT');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Engines.RefreshData;
var
  s :string;
  //OLEObj: OLEVariant;
  VI: TVersionInfo;
  sl: TStringList;
  i,j,k,m,X64: Integer;
  Reg : TRegistry;
const
  rkBDESettings = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Borland\Database Engine';
    rvBDEDLLPath = 'DLLPATH';
    fnBDEDLL = 'IDAPI32.DLL';
  rkODBCSettings = {HKEY_LOCAL_MACHINE}'\SOFTWARE\ODBC\ODBCINST.INI\ODBC Core\FileList';
    rvODBCCoreDLL = 'ODBC32.DLL';
  rkNET = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\.NETFramework';
    rvIR = 'InstallRoot';
  rkSharedDLLs = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion\SharedDLLs';

  fnSystem = 'mscorlib.tlb';
  fnOpenGL = 'opengl32.dll';
  fnIE = 'iexplore.exe';

  rkIE = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Internet Explorer';
    rvVersion = 'Version';
    rvSvcVersion = 'svcVersion';
    rvsvcUpdateVersion = 'svcUpdateVersion';
  rkIES = {HKEY_LOCAL_MACHINE\}'SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings';
    rvSP = 'MinorVersion';
  rkCLSID = 'CLSID';

  { OLE object table class string }
  daoEngine36 = 'DAO.DBEngine.36';
  daoEngine35 = 'DAO.DBEngine.35';
  daoEngine30 = 'DAO.DBEngine';

  daoEngines: array[0..2] of string = (daoEngine36, daoEngine35, daoEngine30);

  adoEngine = 'adodb.connection';
  adoEngine28 = 'adodb.connection.2.8';

  adoEngines: array[0..1] of string = (adoEngine28, adoEngine);

  msiEngine = 'WindowsInstaller.Installer';

  qtEngine4 = 'QuickTime.QuickTime.4';
  qtEngine = 'QuickTime.QuickTime';

  qtEngines: array[0..1] of string = (qtEngine4, qtEngine);

  {function GetOLEObject(ProgID: string): OLEVariant;
  var
    idisp: IDispatch;
    ClassID: TCLSID;
    Unknown: IUnknown;
    HR: HRESULT;
  begin
    Finalize(Result);
    if CoInitialize(nil) in [S_OK, S_FALSE] then
    try
      HR:=CLSIDFromProgID(PWideChar(WideString(ProgID)),ClassID);
      if Succeeded(HR) then begin
        HR:=GetActiveObject(ClassID,nil,Unknown);
        if Succeeded(HR) then
          HR:=Unknown.QueryInterface(IDispatch,idisp);
        if not Succeeded(HR) then
          HR:=CoCreateInstance(ClassID,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IDispatch,idisp);
        if not Succeeded(HR) then
          Finalize(Result)
        else
          Result:=idisp;
      end;
    finally
      try
        //CoUninitialize;
      except
      end;
    end;
  end;}

  function ExpandPath(Use6432: Boolean; const APath: string ): string;
  begin
    Result:=APath;
    if Use6432 then begin
      Result:=StringReplace(Result,'%CommonProgramFiles%','%CommonProgramW6432%',[rfReplaceAll,rfIgnoreCase]);
      Result:=StringReplace(Result,'%ProgramFiles%','%ProgramW6432%',[rfReplaceAll,rfIgnoreCase]);
    end;
    Result:=ExpandEnvVars(Result,False);
  end;

begin
  inherited;

  Clear;

  DirectX.RefreshData;
  ASPI32.RefreshData;

  if IsWow64 then
    X64:=1
  else
    X64:=0;

  sl:=TStringList.Create;

  for k:=0 to X64 do begin
    if k=0 then
      Reg:=TRegistry.Create(KEY_READ)
    else
      Reg:=OpenRegistryReadOnly;


    with Reg do begin
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkBDESettings,False) then begin
        if ValueExists(rvBDEDLLPath) then begin
          s:=ReadString(rvBDEDLLPath);
          if FileExists(s+'\'+fnBDEDLL) then begin
            GetFileVerInfo(s+'\'+fnBDEDLL,VI);
            FBDE:=VI.FileVersion;
            if FBDE='' then
              FBDE:=Format('%d.%d.%d.%d',[VI.Major,VI.Minor,VI.Release,VI.Build]);
          end;
        end;
        CloseKey;
      end;


      if OpenKey(rkODBCSettings,False) then begin
        if ValueExists(rvODBCCoreDLL) then begin
          s:=ReadString(rvODBCCoreDLL);
          GetFileVerInfo(s,VI);
          FODBC:=VI.FileVersion
        end;
        closekey;
      end else begin
        s:=FileSearch(rvODBCCoreDLL,GetSysDir);
        GetFileVerInfo(s,VI);
        FODBC:=VI.FileVersion;
      end;

      sl.Clear;
      if OpenKey('SOFTWARE\Microsoft\NET Framework Setup\NDP',False) then begin
        GetKeyNames(sl);
        closekey;
      end;

      for i:=0 to sl.Count-1 do begin
        if OpenKey('SOFTWARE\Microsoft\NET Framework Setup\NDP\' + sl[i],False) then begin
          if ValueExists('Version') then begin
            s:=ReadString('Version');
            if FNET<s then
              FNET:=s;
          end;
          CloseKey;
        end;
        if OpenKey('SOFTWARE\Microsoft\NET Framework Setup\NDP\'+sl[i]+'\Full',False) then begin
          if ValueExists('Version') then begin
            s:=ReadString('Version');
            if FNET<s then
              FNET:=s;
          end;
          CloseKey;
        end;
      end;

  {
      if OpenKey(rkNET,False) then begin
        if ValueExists(rvIR) then begin
          CloseKey;
          if OpenKey(rkSharedDLLs,False) then begin
            GetValueNames(sl);
            for i:=0 to sl.Count-1 do
              if Pos(fnSystem,Lowercase(sl[i]))>0 then begin
                GetFileVerInfo(sl[i],VI);
                if FNET<VI.Version then
                  FNET:=VI.Version;
              end;
          end;
        end;
        closekey;
      end;
  }

      if OpenKey(rkIE,False) then begin
        if ValueExists(rvVersion) then begin
          FIE:=ReadString(rvVersion);
          if ValueExists(rvSvcVersion) then
            FIE:=ReadString(rvSvcVersion);
          s:='';
          if ValueExists(rvsvcUpdateVersion) then
            s:=ReadString(rvsvcUpdateVersion);
          CloseKey;
          if (s='') and OpenKey(rkIES,False) then begin
            if ValueExists(rvSP) then begin
              sl.CommaText:=StringReplace(ReadString(rvSP),';',',',[rfReplaceAll,rfIgnoreCase]);
              i:=0;
              while i<sl.Count do
                if Trim(sl[i])='' then
                  sl.Delete(i)
                else
                  Inc(i);
              FIE:=Format('%s (%s)',[FIE,sl.CommaText]);
            end;
          end else
            FIE:=Format('%s (%s)',[FIE,s]);
        end;
        closekey;
      end;
      if FIE='' then
        FIE:=ReadRegistryString(HKEY_LOCAL_MACHINE,'SOFTWARE\Microsoft\Internet Explorer','Version');

      Rootkey:=HKEY_CLASSES_ROOT;


      if OpenKey(rkCLSID,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          for m:=0 to 1 do begin
            if ((m=0) and OpenKey(Format('CLSID\%s\VersionIndependentProgID',[sl[i]]),False)) or
               ((m=1) and OpenKey(Format('CLSID\%s\ProgId',[sl[i]]),False)) then begin
              s:=ReadString('');
              CloseKey;
              for j:=0 to 2 do
                if SameText(s,daoEngines[j]) then begin
                  if OpenKey(Format('CLSID\%s\InprocServer32',[sl[i]]),False) then begin
                    s:=ReadString('');
                    CloseKey;

                    s:=ExpandPath(k=1,s);

                    if FileExists(s) then
                      FDAO:=GetFileVersion(s);
                  end;
                end;

              for j:=0 to 1 do
                if SameText(s,adoEngines[j]) then begin
                  if OpenKey(Format('CLSID\%s\InprocServer32',[sl[i]]),False) then begin
                    s:=ReadString('');
                    CloseKey;

                    s:=ExpandPath(k=1,s);

                    if FileExists(s) then
                      FADO:=GetFileVersion(s);
                  end;
                end;

              if SameText(s,msiEngine) then begin
                if OpenKey(Format('CLSID\%s\InprocServer32',[sl[i]]),False) then begin
                  s:=ReadString('');
                  CloseKey;

                  s:=ExpandPath(k=1,s);

                  if FileExists(s) then
                    FMSI:=GetFileVersion(s);
                end;
              end;
              for j:=0 to 1 do
                if SameText(s,qtEngines[j]) then begin
                  if OpenKey(Format('CLSID\%s\InprocServer32',[sl[i]]),False) then begin
                    s:=ReadString('');
                    CloseKey;

                    s:=ExpandPath(k=1,s);

                    if FileExists(s) then
                      FQT:=GetFileVersion(s);
                  end;
                end;
            end;

            if (FDAO<>'') and
               (FADO<>'') and
               (FMSI<>'') and
               (FQT<>'') then
              Break;
          end;
      end;

      Free;
    end;
  end;

  sl.Free;

  s:=FileSearch(fnOpenGL,GetSysDir);
  GetFileVerInfo(s,VI);
  FOpenGL:=VI.FileVersion;

  {OLEObj:=GetOLEObject(daoEngine36);
  if TVarData(OLEObj).VType<>varDispatch then
    OLEObj:=GetOLEObject(daoEngine35);
  if TVarData(OLEObj).VType<>varDispatch then
    OLEObj:=GetOLEObject(daoEngine30);
  if TVarData(OLEObj).VType=varDispatch then
    FDAO:=OLEObj.Version;
  Finalize(OLEObj);

  OLEObj:=GetOLEObject(adoEngine);
  if TVarData(OLEObj).VType=varDispatch then
    FADO:=OLEObj.Version;
  Finalize(OLEObj);}

  SetDataAvail(True);
end;



procedure TMiTeC_Engines.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  ASPI32.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  DirectX.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
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
        WriteStrProperty(sl,'BDE',Self.BDE);
        WriteStrProperty(sl,'ODBC',Self.ODBC);
        WriteStrProperty(sl,'DAO',Self.DAO);
        WriteStrProperty(sl,'ADO',Self.ADO);
        WriteStrProperty(sl,'NET',Self.NET);
        WriteStrProperty(sl,'OpenGL',Self.OpenGL);
        WriteStrProperty(sl,'IE',Self.IE);
        WriteStrProperty(sl,'MSI',Self.MSI);
        WriteStrProperty(sl,'QT',Self.QT);

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

procedure TMiTeC_Engines.SetHeaderReader(const Value: THeaderReader);
begin
  inherited;
  FDirectX.OnReadHeader:=Value;
  FASPI32.OnReadHeader:=Value;
end;

procedure TMiTeC_Engines.SetHeaderWriter(const Value: THeaderWriter);
begin
  inherited;
  FDirectX.OnWriteHeader:=Value;
  FASPI32.OnWriteHeader:=Value;
end;

end.
