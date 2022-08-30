{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Software Detection Part                  }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Software;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs;

const
  StorageFolderName = 'Software';

type
  TInstallRecord = record
    Name,
    Version,
    Company,
    Uninstall,
    InstallSource: string;
    InstallDate: TDateTime;
    Legal: Boolean;
    NoRemove: boolean;
    NoRepair: boolean;
    SecurityUpdate: boolean;
    SystemComponent: boolean;
    HelpLink,
    AboutLink,
    InfoLink,
    UpdateLink: string;
  end;

  TInstallData = array of TInstallRecord;

  TMiTeC_Software = class(TMiTeC_Component)
  private
    FData: TInstallData;
    FAppPaths: TStrings;
    FSC: Boolean;
    FSU: boolean;
    function GetIC: Cardinal;
    function GetIR(Index: Cardinal): TInstallRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    procedure SetLegal(Index: Cardinal; ALegal: boolean);
    property InstallEntry[Index: Cardinal]: TInstallRecord read GetIR;
    property AppPaths: TStrings read FAppPaths;
  published
    property SystemComponents: Boolean read FSC write FSC;
    property SecurityUpdates: boolean read FSU write FSU;
    property Count: Cardinal read GetIC;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.DateUtils,
     {$ELSE}
     Registry, DateUtils,
     {$ENDIF}
     MiTeC_Routines, MiTeC_Datetime;

{ TMiTeC_Software }

procedure TMiTeC_Software.Clear;
begin
  Finalize(FData);
  FAppPaths.Clear;
end;

constructor TMiTeC_Software.Create;
begin
  inherited Create(AOwner);
  FAppPaths:=TStringList.Create;
  FSC:=False;
  FSU:=False;
end;

destructor TMiTeC_Software.Destroy;
begin
  FAppPaths.Free;
  Finalize(FData);
  inherited;
end;

function TMiTeC_Software.GetIC: Cardinal;
begin
  Result:=Length(FData);
end;

procedure TMiTeC_Software.RefreshData;

function GetKeyDate(AKey: HKEY): TDateTime;
var
  ft: FILETIME;
begin
  Result:=0;
  if RegQueryInfoKey(AKey,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,@ft)=ERROR_SUCCESS then
    Result:=FileTimeToDateTime(ft);
end;

const
  rkRoot: array[0..1] of string = ('SOFTWARE','SOFTWARE\Wow6432Node');

  rk0 = {HKEY_LOCAL_MACHINE}'\%s\Microsoft\Windows\CurrentVersion\Uninstall';
  rk1 = {HKEY_LOCAL_MACHINE}'\%s\Microsoft\Windows\CurrentVersion\Installer\UserData';
  rk2 = {HKEY_LOCAL_MACHINE}'\%s\Microsoft\Active Setup\Installed Components';
  rvDN = 'DisplayName';
  rvDV = 'DisplayVersion';
  rvUS = 'UninstallString';
  rvDate = 'InstallDate';
  rvCompany = 'Publisher';
  rvSource = 'InstallSource';
  rvKFN = 'KeyFileName';
  rvVer = 'Version';
  rvHelpLink = 'HelpLink';
  rvAboutLink = 'URLInfoAbout';
  rvInfoLink = 'MoreInfoURL';
  rvUpdateLink = 'URLUpdateInfo';

  rkAP = '\%s\Microsoft\Windows\CurrentVersion\App Paths\';
var
  i,j: integer;
  sl,kl: TStringList;
  r: TInstallRecord;
  rk,s,v: string;
  x{$IFDEF WIN32},x64{$ENDIF}: Integer;
  reg: TRegistry;

procedure AddEntry(AEntry: TInstallRecord);
var
  i,idx: integer;
begin
  idx:=-1;
  for i:=0 to High(FData) do
    if SameText(FData[i].Name,AEntry.Name) then begin
      idx:=i;
      Break;
    end;
  if idx=-1 then begin
    SetLength(FData,Length(FData)+1);
    idx:=High(FData);
  end;
  if (FData[idx].Name='') or (AEntry.Installdate<>0) then
    FData[idx]:=AEntry;
end;

function GetInstallDate: TDateTime;
var
  s: string;
begin
  Result:=0;
  try
    s:=reg.ReadString(rvDate);
  except
    s:=''
  end;
  if s<>'' then
    try Result:=EncodeDate(StrToIntDef(Copy(s,1,4),0),StrToIntDef(Copy(s,5,2),0),StrToIntDef(Copy(s,7,2),0)) except end;
  if Result=0 then
    Result:=GetKeyDate(reg.CurrentKey);
end;

begin
  inherited;
  Clear;
  sl:=TStringList.Create;
  kl:=TStringList.Create;
  try
    {$IFDEF WIN64}
    for x:=0 to 1 do begin
      Reg:=TRegistry.Create(KEY_READ);
      rk:=Format(rk0,[rkRoot[x]]);
    {$ELSE}
    if not IsWow64 then
      x64:=0
    else
      x64:=1;
    for x:=0 to X64 do begin
      rk:=Format(rk0,[rkRoot[0]]);
      if x=0 then
        Reg:=TRegistry.Create(KEY_READ)
      else
        Reg:=TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
    {$ENDIF}
      with reg do
        try
          RootKey:=HKEY_LOCAL_MACHINE;
          if OpenKey(rk,False) then begin
            GetKeyNames(sl);
            CloseKey;
            for i:=0 to sl.Count-1 do
              if OpenKey(rk+'\'+sl[i],False) then begin
                ResetMemory(r,sizeof(r));
                if ValueExists('NoRemove') then
                  try r.NoRemove:=ReadInteger('NoRemove')=1 except end;
                if ValueExists('NoRepair') then
                  try r.NoRepair:=ReadInteger('NoRepair')=1 except end;
                if ValueExists('SystemComponent') then
                  try r.SystemComponent:=ReadInteger('SystemComponent')=1 except end;
                if ValueExists('ReleaseType') then
                  try
                    r.SecurityUpdate:=SameText(ReadString('ReleaseType'),'Security Update') or
                                      SameText(ReadString('ReleaseType'),'ServicePack') or
                                      (Pos('http://support.microsoft.com/kb/',ReadString('HelpLink'))=1)
                  except end;

                if ValueExists(rvDN) and (FSC or not r.SystemComponent) and (FSU or not r.SecurityUpdate) then begin
                  Finalize(r);
                  r.Name:=ReadString(rvDN);
                  if ValueExists(rvDV) then
                    try r.Version:=ReadString(rvDV) except r.Version:='' end;
                  if ValueExists(rvUS) then
                    r.Uninstall:=ReadString(rvUS);
                  if ValueExists(rvCompany) then
                    try r.Company:=ReadString(rvCompany) except r.Company:='' end;
                  if ValueExists(rvSource) then
                    r.InstallSource:=ReadString(rvSource);
                  r.InstallDate:=GetInstallDate;
                  if ValueExists(rvHelpLink) then
                    r.HelpLink:=ReadString(rvHelpLink);
                  if ValueExists(rvAboutLink) then
                    r.AboutLink:=ReadString(rvAboutLink);
                  if ValueExists(rvInfoLink) then
                    r.InfoLink:=ReadString(rvInfoLink);
                  if ValueExists(rvUpdateLink) then
                    r.UpdateLink:=ReadString(rvUpdateLink);

                  if r.Name='' then
                    Continue;

                  AddEntry(r);
                end;
                CloseKey;
              end;
          end;

          if FSC then begin
            {$IFDEF WIN64}
            rk:=Format(rk1,[rkRoot[x]]);
            {$ELSE}
            rk:=Format(rk1,[rkRoot[0]]);
            {$ENDIF}
            if OpenKey(rk,False) then begin
               sl.Clear;
               GetKeyNames(sl);
               CloseKey;
               for i:=0 to sl.Count-1 do
                 if OpenKey(Format('%s\%s\Products',[rk,sl[i]]),False) then begin
                   kl.Clear;
                   GetKeyNames(kl);
                   CloseKey;
                   for j:=0 to kl.Count-1 do
                     if OpenKey(Format('%s\%s\Products\%s\InstallProperties',[rk,sl[i],kl[j]]),False) then begin
                       ResetMemory(r,sizeof(r));
                       if ValueExists('NoRemove') then
                         try r.NoRemove:=ReadInteger('NoRemove')=1 except end;
                       if ValueExists('NoRepair') then
                         try r.NoRepair:=ReadInteger('NoRepair')=1 except end;
                       if ValueExists('SystemComponent') then
                         try r.SystemComponent:=ReadInteger('SystemComponent')=1 except end;
                       if ValueExists('ReleaseType') then
                         try
                    r.SecurityUpdate:=SameText(ReadString('ReleaseType'),'Security Update') or
                                      SameText(ReadString('ReleaseType'),'ServicePack') or
                                      (Pos('http://support.microsoft.com/kb/',ReadString('HelpLink'))=1)
                  except end;

                       if ValueExists(rvDN) and (FSC or not r.SystemComponent) and (FSU or not r.SecurityUpdate) then begin
                         Finalize(r);
                         r.Name:=ReadString(rvDN);
                         if ValueExists(rvDV) then
                           try r.Version:=ReadString(rvDV); except r.Version:='' end;
                         if ValueExists(rvUS) then
                           r.Uninstall:=ReadString(rvUS);
                         if ValueExists(rvCompany) then
                           r.Company:=ReadString(rvCompany);
                         if ValueExists(rvSource) then
                           r.InstallSource:=ReadString(rvSource);
                         r.InstallDate:=GetInstallDate;
                         if ValueExists(rvHelpLink) then
                           r.HelpLink:=ReadString(rvHelpLink);
                         if ValueExists(rvAboutLink) then
                           r.AboutLink:=ReadString(rvAboutLink);
                         if ValueExists(rvInfoLink) then
                           r.InfoLink:=ReadString(rvInfoLink);
                         if ValueExists(rvUpdateLink) then
                           r.UpdateLink:=ReadString(rvUpdateLink);

                         if r.Name='' then
                           Continue;

                         AddEntry(r);
                       end;
                       CloseKey;
                     end;
                 end;
             end;
          end;

          {$IFDEF WIN64}
          rk:=Format(rk2,[rkRoot[x]]);
          {$ELSE}
          rk:=Format(rk2,[rkRoot[0]]);
          {$ENDIF}
          if OpenKey(rk,False) and FSC then begin
            sl.Clear;
            GetKeyNames(sl);
            CloseKey;
            for i:=0 to sl.Count-1 do
              if OpenKey(Format('%s\%s',[rk,sl[i]]),False) then begin
                if ValueExists('') then begin
                  ResetMemory(r,sizeof(r));
                  r.SystemComponent:=True;
                  r.Name:=ReadString('');
                  if ValueExists(rvVer) then
                    try r.Version:=ReadString(rvVer) except r.Version:='' end;
                  if ValueExists(rvKFN) then
                    r.Uninstall:=ReadString(rvKFN);

                  if r.Name='' then
                    Continue;

                   AddEntry(r);
                 end;
                 CloseKey;
               end;
           end;

           {$IFDEF WIN64}
           rk:=Format(rk0,[rkRoot[x]]);
           {$ELSE}
           rk:=Format(rk0,[rkRoot[0]]);
           {$ENDIF}
           if (Win32Platform=VER_PLATFORM_WIN32_NT) and (SID<>'') then begin
             RootKey:=HKEY_USERS;
             s:=SID+rk;
           end else begin
             RootKey:=HKEY_CURRENT_USER;
             s:=rk;
           end;

           if OpenKey(s,False) then begin
             sl.Clear;
             GetKeyNames(sl);
             CloseKey;
             for i:=0 to sl.Count-1 do
               if OpenKey(s+'\'+sl[i],False) then begin
                 ResetMemory(r,sizeof(r));
                if ValueExists('NoRemove') then
                  try r.NoRemove:=ReadInteger('NoRemove')=1 except end;
                if ValueExists('NoRepair') then
                  try r.NoRepair:=ReadInteger('NoRepair')=1 except end;
                if ValueExists('SystemComponent') then
                  try r.SystemComponent:=ReadInteger('SystemComponent')=1 except end;
                if ValueExists('ReleaseType') then
                  try
                    r.SecurityUpdate:=SameText(ReadString('ReleaseType'),'Security Update') or
                                      SameText(ReadString('ReleaseType'),'ServicePack') or
                                      (Pos('http://support.microsoft.com/kb/',ReadString('HelpLink'))=1)
                  except end;

                if ValueExists(rvDN) and (FSC or not r.SystemComponent) and (FSU or not r.SecurityUpdate) then begin
                   Finalize(r);
                   r.Name:=ReadString(rvDN);
                   if ValueExists(rvDV) then
                     try r.Version:=ReadString(rvDV); except r.Version:='' end;
                   if ValueExists(rvUS) then
                     r.Uninstall:=ReadString(rvUS);
                   if ValueExists(rvCompany) then
                     r.Company:=ReadString(rvCompany);
                   if ValueExists(rvSource) then
                     r.InstallSource:=ReadString(rvSource);
                   r.InstallDate:=GetInstallDate;
                   if ValueExists(rvHelpLink) then
                     r.HelpLink:=ReadString(rvHelpLink);
                   if ValueExists(rvAboutLink) then
                     r.AboutLink:=ReadString(rvAboutLink);
                   if ValueExists(rvInfoLink) then
                     r.InfoLink:=ReadString(rvInfoLink);
                   if ValueExists(rvUpdateLink) then
                     r.UpdateLink:=ReadString(rvUpdateLink);

                   if r.Name='' then
                     Continue;

                   AddEntry(r);
                 end;
                 CloseKey;
               end;
           end;

           if FSC then begin
             {$IFDEF WIN64}
             rk:=Format(rk1,[rkRoot[x]]);
             {$ELSE}
             rk:=Format(rk1,[rkRoot[0]]);
             {$ENDIF}
             RootKey:=HKEY_USERS;
             s:=SID+rk;

             if OpenKey(s,False) then begin
               sl.Clear;
               GetKeyNames(sl);
               CloseKey;
               for i:=0 to sl.Count-1 do
                 if OpenKey(Format('%s\%s\Products',[s,sl[i]]),False) then begin
                   kl.Clear;
                   GetKeyNames(kl);
                   CloseKey;
                   for j:=0 to kl.Count-1 do
                     if OpenKey(Format('%s\%s\Products\%s\InstallProperties',[s,sl[i],kl[j]]),False) then begin
                       ResetMemory(r,sizeof(r));
                       r.SystemComponent:=True;
                       if ValueExists('NoRemove') then
                         try r.NoRemove:=ReadInteger('NoRemove')=1 except end;
                       if ValueExists('NoRepair') then
                         try r.NoRepair:=ReadInteger('NoRepair')=1 except end;
                       if ValueExists('SystemComponent') then
                         try r.SystemComponent:=ReadInteger('SystemComponent')=1 except end;
                       if ValueExists('ReleaseType') then
                         try
                    r.SecurityUpdate:=SameText(ReadString('ReleaseType'),'Security Update') or
                                      SameText(ReadString('ReleaseType'),'ServicePack') or
                                      (Pos('http://support.microsoft.com/kb/',ReadString('HelpLink'))=1)
                  except end;

                       if ValueExists(rvDN) and (FSC or not r.SystemComponent) and (FSU or not r.SecurityUpdate) then begin
                         Finalize(r);
                         r.Name:=ReadString(rvDN);
                         if ValueExists(rvDV) then
                           try r.Version:=ReadString(rvDV); except r.Version:='' end;
                         if ValueExists(rvUS) then
                           r.Uninstall:=ReadString(rvUS);
                         if ValueExists(rvCompany) then
                           r.Company:=ReadString(rvCompany);
                         if ValueExists(rvSource) then
                           r.InstallSource:=ReadString(rvSource);
                         r.InstallDate:=GetInstallDate;
                         if ValueExists(rvHelpLink) then
                           r.HelpLink:=ReadString(rvHelpLink);
                         if ValueExists(rvAboutLink) then
                           r.AboutLink:=ReadString(rvAboutLink);
                         if ValueExists(rvInfoLink) then
                           r.InfoLink:=ReadString(rvInfoLink);
                         if ValueExists(rvUpdateLink) then
                           r.UpdateLink:=ReadString(rvUpdateLink);

                         if r.Name='' then
                           Continue;

                         AddEntry(r);
                       end;
                       CloseKey;
                     end;
                 end;
             end;
           end;

          RootKey:=HKEY_LOCAL_MACHINE;
          {$IFDEF WIN64}
          rk:=Format(rkAP,[rkRoot[x]]);
          {$ELSE}
          rk:=Format(rkAP,[rkRoot[0]]);
          {$ENDIF}
          if OpenKey(rk,False) then begin
            kl.Clear;
            GetKeyNames(kl);
            CloseKey;
            for i:=0 to kl.Count-1 do
              if OpenKey(rk+kl[i],False) then begin
                s:=DequoteStr(ReadString(''));
                if (s='') or Sametext(s,'blank') then
                  s:=kl[i];
                s:=Trim(ExpandEnvVars(s,False));
                v:=Trim(ExpandEnvVars(ReadString('Path'),False));
                s:=Format('%s=%s',[s,v]);
                if FAppPaths.IndexOf(s)=-1 then
                  FAppPaths.Add(s);
                CloseKey;
              end;
          end;
        finally
          Free;
        end;
    end;
  finally
    sl.Free;
    kl.Free;
  end;
  SetDataAvail(True);
end;

function TMiTeC_Software.GetIR(Index: Cardinal): TInstallRecord;
begin
  try
    Result:=FData[Index];
  except
    Finalize(Result);
  end;
end;

function TMiTeC_Software.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;
  strm: TStorageStream;

function ReadFromStream(AIndex: Cardinal): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  s: string;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(FData,Length(FData)+1);
        with FData[High(FData)] do begin
          Name:=ReadStrProperty(sl,'Name');
          Version:=ReadStrProperty(sl,'Version');
          Company:=ReadStrProperty(sl,'Company');
          Uninstall:=ReadStrProperty(sl,'Uninstall');
          InstallSource:=ReadStrProperty(sl,'InstallSource');
          InstallDate:=ReadDtProperty(sl,'InstallDate');
          if YearOf(Installdate)>3000 then begin
            s:=ReadStrProperty(sl,'InstallDate');
            try InstallDate:=EncodeDate(StrToIntDef(Copy(s,1,4),0),StrToIntDef(Copy(s,5,2),0),StrToIntDef(Copy(s,8,2),0)) except InstallDate:=0 end;
          end;
          NoRemove:=ReadIntProperty(sl,'NoRemove')=1;
          NoRepair:=ReadIntProperty(sl,'NoRepair')=1;
          SystemComponent:=ReadIntProperty(sl,'SystemComponent')=1;
          SecurityUpdate:=ReadIntProperty(sl,'SecurityUpdate')=1;
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
    except
      Exit;
    end;
    try
      i:=0;
      while ReadFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);

      try strm:=Sub.OpenStream('AppPaths',STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then begin
        LoadFromEncodedStream(strm,FAppPaths,ACodeStream);
        strm.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Software.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;

procedure WriteToStream(AIndex: Cardinal);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Name',Self.InstallEntry[AIndex].Name);
    WriteStrProperty(sl,'Version',Self.InstallEntry[AIndex].Version);
    WriteStrProperty(sl,'Company',Self.InstallEntry[AIndex].Company);
    WriteStrProperty(sl,'Uninstall',Self.InstallEntry[AIndex].Uninstall);
    WriteStrProperty(sl,'InstallSource',Self.InstallEntry[AIndex].InstallSource);
    WriteDtProperty(sl,'InstallDate',Self.InstallEntry[AIndex].InstallDate);
    WriteIntProperty(sl,'NoRemove',integer(Self.InstallEntry[AIndex].NoRemove));
    WriteIntProperty(sl,'NoRepair',integer(Self.InstallEntry[AIndex].NoRepair));
    WriteIntProperty(sl,'SystemComponent',integer(Self.InstallEntry[AIndex].SystemComponent));
    WriteIntProperty(sl,'SecurityUpdate',integer(Self.InstallEntry[AIndex].SecurityUpdate));
    strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
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
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      for i:=0 to Self.Count-1 do
        WriteToStream(i);
      strm:=Sub.OpenStream('AppPaths',STG_OPEN,True);
      SaveToEncodedStream(FAppPaths,strm,ACodeStream);
      strm.Free;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Software.SetLegal(Index: Cardinal; ALegal: boolean);
begin
  FData[Index].Legal:=ALegal;
end;

end.



