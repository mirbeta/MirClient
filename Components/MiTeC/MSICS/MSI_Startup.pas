{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Startup Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Startup;
                        
interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs, MiTeC_Routines;

const
  StorageFolderName = 'Autoruns';

  ParamDelimiter = '=';

type
  TAutorunLocation = (arlRegistry, arlFolder, arlBHO, arlFile, arlTaskScheduler);

  TAutorun = record
    Location: TAutorunLocation;
    Path,
    Name,
    ImageName,
    CmdLine: string;
    Size: int64;
    VersionInfo: TVersionInfo;
    _ImageIndex: integer;
  end;
  PAutorun = ^TAutorun;

  TAutoRuns = array of TAutorun;

  TMiTeC_Startup = class(TMiTeC_Component)
  private
    sl,kl: TStringList;
    FList: TAutoruns;
    function GetCount: Cardinal;
    function GetAutorun(AIndex: Cardinal): TAutorun;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property Records[AIndex: Cardinal]: TAutorun read GetAutorun;
  published
    property Count: Cardinal read GetCount stored False;
  end;


implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, WinAPI.ShlObj, System.INIFiles, System.Variants,
     {$ELSE}
     Registry, ShlObj, INIFiles, Variants,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_SysUtils, MiTeC_Shell, MiTeC_TaskScheduler_TLB;

{ TMiTeC_Startup }

procedure TMiTeC_Startup.Clear;
begin
  Finalize(FList);
end;

constructor TMiTeC_Startup.Create;
begin
  inherited Create(AOwner);
  sl:=TStringList.Create;
  kl:=TStringList.Create;
end;

destructor TMiTeC_Startup.Destroy;
begin
  Finalize(FList);
  sl.Free;
  kl.Free;
  inherited;
end;

function TMiTeC_Startup.GetAutorun(AIndex: Cardinal): TAutorun;
begin
  Result:=FList[AIndex];
end;

function TMiTeC_Startup.GetCount: Cardinal;
begin
  Result:=Length(FList);
end;

procedure TMiTeC_Startup.RefreshData;
var
  VI: TVersionInfo;

procedure Add(APath,AEntryName,ACmdLine: string; ALoc: TAutorunLocation = arlRegistry; AImageName: string = '');
var
  d: TAutorun;
begin
  Resetmemory(d,SizeOf(d));
  d.Path:=APath;
  d.Location:=ALoc;
  d.Name:=AEntryName;
  d.CmdLine:=ACmdLine;
  if AImageName='' then
    d.ImageName:=ExtractImageName(ACmdLine,False)
  else
    d.ImageName:=AImageName;

  if (d.ImageName<>'') then begin
    GetFileVerInfo(d.ImageName,d.VersionInfo);
    d.Size:=GetFileSize(d.ImageName);
    if d.Size<0 then
      d.Size:=0;
  end;
  SetLength(FList,Length(FList)+1);
  FList[High(FList)]:=d;
end;

procedure ScanINI;
var
  i: Integer;
  f: string;
begin
  sl.Clear;
  with TINIFile.Create(IncludeTrailingPathDelimiter(GetWinDir)+'SYSTEM.INI') do begin
    ReadSection('boot',sl);
    for i:=0 to sl.Count-1 do
      if SameText(sl[i],'run') or
         SameText(sl[i],'load') or
         SameText(sl[i],'scrnsave.exe') or
         SameText(sl[i],'shell') then begin
       f:=TrimAll(ReadString('boot',sl[i],''));
       if f<>'' then
         Add('SYSTEM.INI',sl[i],f,arlFile);
      end;
    Free;
  end;

  sl.Clear;
  with TINIFile.Create(IncludeTrailingPathDelimiter(GetWinDir)+'WIN.INI') do begin
    ReadSection('windows',sl);
    for i:=0 to sl.Count-1 do
      if SameText(sl[i],'run') or
         SameText(sl[i],'load') then begin
        f:=TrimAll(ReadString('windows',sl[i],''));
        if f<>'' then
          Add('WIN.INI',sl[i],f,arlFile);
      end;
    Free;
  end;
end;

procedure ScanRegistry(AHK: HKEY; ARootPath, APrefix: string);
const
  rk_Run = {Software}'\Microsoft\Windows\CurrentVersion\Run';
  rk_Once = {Software}'\Microsoft\Windows\CurrentVersion\RunOnce';
  rk_OnceEx = {Software}'\Microsoft\Windows\CurrentVersion\RunOnceEx';
  rk_Services = {Software}'\Microsoft\Windows\CurrentVersion\RunServices';
  rk_ServicesOnce = {Software}'\Microsoft\Windows\CurrentVersion\RunServicesOnce';

  rk_WinLogon = {Software}'\Microsoft\Windows NT\CurrentVersion\Winlogon';
    rv_UserInit = 'UserInit';
    rv_Shell = 'Shell';
    rv_GINA = 'GinaDLL';

  rk_WinLogonNotify = {Software}'\Microsoft\Windows NT\CurrentVersion\Winlogon\Notify';
    rvDLLName = 'DLLName';

  rk_WindowsNT = {Software}'\Microsoft\Windows NT\CurrentVersion\Windows';
    rv_Load = 'Load';

  rk_SessionManager = {\SYSTEM}'\CurrentControlSet\Control\Session Manager';
    rv_BootExecute = 'BootExecute';

  rk_AppInit_DLLs = {SOFTWARE}'\Microsoft\Windows NT\CurrentVersion\Windows';
   rv_AppInit_DLLs = 'AppInit_DLLs';

  rk_BHO = {SOFTWARE}'\Microsoft\Windows\CurrentVersion\Explorer\Browser Helper Objects';
  rk_CLSID = {SOFTWARE}'\Classes\CLSID';
    rkInprocSrv = 'InprocServer32';
var
  i,j: integer;
  s,f,rk: string;
  data: PChar;
  rdt: TRegDataInfo;
  reg: TRegistry;
begin
  data:=nil;
  if OS>=osXP then
    reg:=TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    reg:=TRegistry.Create;
  with reg do
    try
      RootKey:=AHK;

      sl.Clear;
      rk:=ARootPath+rk_BHO;
      if OpenKeyReadOnly(rk) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if OpenKey(ARootPath+rk_CLSID+'\'+sl[i],False) then begin
            if (GetDataType('')=rdString) then
              s:=ReadString('');
            if s='' then
              s:=sl[i];
            CloseKey;
            if OpenKey(ARootPath+rk_CLSID+'\'+sl[i]+'\'+rkInprocSrv,False) then
              if (GetDataType('')=rdString) then begin
                f:=ReadString('');
                CloseKey;
                Add('Browser Helper Objects',s,f,arlBHO);
              end;
          end;
        end;
      end;

      sl.Clear;
      rk:=ARootPath+rk_Run;
      if OpenKeyReadOnly(rk) then begin
        GetValueNames(sl);
        for i:=0 to sl.Count-1 do
          if (Trim(sl[i])<>'') and (GetDataType(sl[i]) in [rdString,rdExpandString]) then
            Add(APrefix+'\'+rk,sl[i],ReadString(sl[i]));
        CloseKey;
      end;
      sl.Clear;
      rk:=ARootPath+rk_Once;
      if OpenKeyReadOnly(rk) then begin
        GetValueNames(sl);
        for i:=0 to sl.Count-1 do
          if (Trim(sl[i])<>'') and (GetDataType(sl[i]) in [rdString,rdExpandString]) then
            Add(APrefix+'\'+rk,sl[i],ReadString(sl[i]));
        CloseKey;
      end;
      rk:=ARootPath+rk_OnceEx;
      sl.Clear;
      if OpenKeyReadOnly(rk) then begin
        GetKeyNames(kl);
        CloseKey;
        for j:=0 to kl.Count-1 do
          if not SameText(kl[j],'Depend') and OpenKeyReadOnly(rk+'\'+kl[j]) then begin
            GetValueNames(sl);
            for i:=0 to sl.Count-1 do
              if (Trim(sl[i])<>'') and (GetDataType(sl[i]) in [rdString,rdExpandString]) then begin
                s:=ReadString(sl[i]);
                s:=StringReplace(s,'||','',[rfIgnoreCase]);
                s:=StringReplace(s,'|',' ',[rfIgnoreCase]);
                Add(APrefix+'\'+rk,sl[i],s);
              end;
            CloseKey;
          end;
      end;
      rk:=ARootPath+rk_Services;
      sl.Clear;
      if OpenKeyReadOnly(rk) then begin
        GetValueNames(sl);
        for i:=0 to sl.Count-1 do
          if (Trim(sl[i])<>'') and (GetDataType(sl[i]) in [rdString,rdExpandString]) then
            Add(APrefix+'\'+rk,sl[i],ReadString(sl[i]));
        CloseKey;
      end;
      sl.Clear;
      rk:=ARootPath+rk_ServicesOnce;
      if OpenKeyReadOnly(rk) then begin
        GetValueNames(sl);
        for i:=0 to sl.Count-1 do
          if (Trim(sl[i])<>'') and (GetDataType(sl[i]) in [rdString,rdExpandString]) then
            Add(APrefix+'\'+rk,sl[i],ReadString(sl[i]));
        CloseKey;
      end;
      sl.Clear;
      rk:=ARootPath+rk_WinLogon;
      if OpenKeyReadOnly(rk) then begin
        if ValueExists(rv_UserInit) and (GetDataType(rv_UserInit)=rdString) then begin
          s:=ReadString(rv_UserInit);
          Add(APrefix+'\'+rk,rv_UserInit,s);
        end;
        if ValueExists(rv_Shell) and (GetDataType(rv_Shell)=rdString) then begin
          s:=ReadString(rv_Shell);
          Add(APrefix+'\'+rk,rv_Shell,s);
        end;
        if ValueExists(rv_GINA) and (GetDataType(rv_GINA)=rdString) then begin
          s:=IncludeTrailingPathDelimiter(GetSysDir)+ReadString(rv_GINA);
          Add(APrefix+'\'+rk,rv_GINA,s);
        end;
        CloseKey;
      end;
      sl.Clear;
      rk:=ARootPath+rk_WinLogonNotify;
      if OpenKeyReadOnly(rk) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKey(rk+'\'+sl[i],False) then begin
            if ValueExists(rvDLLName) then begin
              s:=ReadValueAsString(reg,rvDLLName);
              if Pos('\',s)=0 then
                s:=IncludeTrailingPathDelimiter(GetSysDir)+s;
              Add(APrefix+'\'+rk,sl[i],s);
            end;
            CloseKey;
          end;
      end;
      sl.Clear;
      rk:=ARootPath+rk_AppInit_DLLs;
      if OpenKeyReadOnly(rk) then begin
        if ValueExists(rv_AppInit_DLLs) then begin
          s:=Trim(ReadValueAsString(reg,rv_AppInit_DLLs));
          if s>'' then
            Add(APrefix+'\'+rk,rv_AppInit_DLLs,s);
        end;
        CloseKey;
      end;
      sl.Clear;
      rk:=ARootPath+rk_SessionManager;
      if OpenKeyReadOnly(rk) then begin
        if ValueExists(rv_BootExecute) then begin
          GetDataInfo(rv_BootExecute,rdt);
          if rdt.RegData in [rdUnknown,rdBinary] then begin
            try
              Data:=StrAlloc(rdt.DataSize);
              ReadBinaryData(rv_BootExecute,Data^,rdt.DataSize);
              s:=string(data);
            finally
              if Assigned(Data) then
                StrDispose(Data);
            end;
          end else
            if GetDataType(rv_BootExecute)=rdString then
              s:=ReadString(rv_BootExecute);
          if not Empty(s) then
            Add(APrefix+'\'+rk,rv_BootExecute,s);
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure ScanFolder(APath: string);
var
  fi: TSearchRec;
  lr: TShellLinkRecord;
begin
  if FindFirst(APath+'\*.*',faArchive+faHidden+faSysFile+faReadOnly,fi)=0 then begin
    if SameText('.lnk',ExtractFileExt(fi.Name)) then begin
      ResolveLink(APath+'\'+fi.Name,lr);
      Add(APath,fi.Name,lr.Target+' '+lr.Arguments,arlFolder,lr.Target);
    end else if (PosText(ExtractFileExt(fi.Name),'.ini')=0) then
      Add(APath,fi.Name,fi.Name,arlFolder,APath+'\'+fi.Name);
    while FindNext(fi)=0 do begin
      if SameText('.lnk',ExtractFileExt(fi.Name)) then begin
        ResolveLink(APath+'\'+fi.Name,lr);
        Add(APath,fi.Name,lr.Target+' '+lr.Arguments,arlFolder,lr.Target);
      end else if (PosText(ExtractFileExt(fi.Name),'.ini')=0) then
        Add(APath,fi.Name,fi.Name,arlFolder,APath+'\'+fi.Name);
    end;
    FindClose(fi);
  end;
end;

var
  tslist: TTSTasks;
  i: Integer;
begin
  Clear;
  GetTaskList(tslist);
  CoInitialize(nil);
  try
    ScanRegistry(HKEY_LOCAL_MACHINE,'System','HKLM');
    ScanRegistry(HKEY_LOCAL_MACHINE,'Software','HKLM');
    ScanRegistry(HKEY_LOCAL_MACHINE,'Software\Wow6432Node','HKLM');
    ScanRegistry(HKEY_CURRENT_USER,'Software','HKCU');
    ScanFolder(GetSpecialFolder(GetDesktopWindow,CSIDL_COMMON_STARTUP));
    ScanFolder(GetSpecialFolder(GetDesktopWindow,CSIDL_STARTUP));
    ScanINI;
    for i:=0 to High(tslist) do
      if tslist[i].Enabled and (tslist[i].ImagePath<>'') and (tslist[i].Logon or tslist[i].Boot) then begin
        tslist[i].ImagePath:=ExpandEnvVars(tslist[i].ImagePath);
        Add(ExtractFilePath(tslist[i].Path),tslist[i].Name,tslist[i].ImagePath+' '+tslist[i].Args,arlTaskScheduler,tslist[i].ImagePath);
      end;
    SetDataAvail(True);
  finally
    CoUninitialize;
  end;
end;

function TMiTeC_Startup.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  r: TAutorun;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl.Clear;
      LoadFromEncodedStream(strm,sl,ACodeStream);
      r.Name:=ReadStrProperty(sl,'Name');
      r.Path:=ReadStrProperty(sl,'Path');
      r.Location:=TAutoRunLocation(ReadIntProperty(sl,'Location'));
      r.ImageName:=ReadStrProperty(sl,'ImageName');
      r.CmdLine:=ReadStrProperty(sl,'CmdLine');
      r.VersionInfo.Description:=ReadStrProperty(sl,'Desc');
      r.VersionInfo.CompanyName:=ReadStrProperty(sl,'Company');
      r.VersionInfo.FileVersion:=ReadStrProperty(sl,'Version');
      r.VersionInfo.Copyright:=ReadStrProperty(sl,'Copyright');
      r.Size:=ReadIntProperty(sl,'Size');
      SetLength(FList,Length(FList)+1);
      FList[High(FList)]:=r;
      Result:=True;
    finally
      strm.Free;
    end;
end;

var
  i: Integer;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
      try
        i:=0;
        while ReadFromStream(i) do
          Inc(i);
        Result:=(i>0);
        SetDataAvail(True);
      finally
        Sub.Free;
      end;
    except
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Startup.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
begin
  sl.Clear;
  with FList[AIndex] do begin
    WriteStrProperty(sl,'Name',Name);
    WriteIntProperty(sl,'Location',Integer(Location));
    WriteStrProperty(sl,'Path',Path);
    WriteStrProperty(sl,'ImageName',ImageName);
    WriteStrProperty(sl,'CmdLine',CmdLine);
    WriteStrProperty(sl,'Desc',VersionInfo.Description);
    WriteStrProperty(sl,'Company',VersionInfo.CompanyName);
    WriteStrProperty(sl,'Version',VersionInfo.FileVersion);
    WriteStrProperty(sl,'Copyright',VersionInfo.Copyright);
    WriteIntProperty(sl,'Size',Size);
    strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  end;
end;

var
  i: Integer;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement('Startup');
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to High(FList) do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.
