{*******************************************************}
{                MiTeC Common Routines                  }
{                 Application Paths                     }
{                                                       }
{          Copyright (c) 2009-2018 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Apps;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

function GetThunderbirdDataPath(AUser: string = ''): string;
procedure GetOEDataPath(AList: TStrings; AUser: string = '');
function GetWinMailDataPath(AUser: string = ''): string;
function GetWLMDataPath(AUser: string = ''): string;
procedure GetTBDataPath(AList: TStrings; AUser: string = '');

function GetIEHistoryPath(AUser: string = ''): string;
function GetFirefoxHistoryPath(AUser: string = ''): string;
function GetChromeHistoryPath(AUser: string = ''): string;
function GetSafariHistoryPath(AUser: string = ''): string;
function GetOperaHistoryPath(AUser: string = ''): string;

function GetSkypeChatPath(AUser: string = ''): string;
function GetICQChatPath(AUser: string = ''): string;
function GetMSNChatPath(AUser: string = ''): string;
function GetYahooChatPath(AUser: string = ''): string;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.INIFiles, System.StrUtils,
     {$else}
     Registry, INIFiles, StrUtils,
     {$ENDIF}
     MiTeC_Windows, MiTeC_Routines, MiTeC_StrUtils, MiTeC_PrefsJs;

function GetThunderbirdDataPath;

function GetValue(ASource: TStrings; AName: string): string;
var
  i,p: Integer;
begin
  Result:='';
  for i:=0 to ASource.Count-1 do
    if PosText(AName,ASource[i])=1 then begin
      Result:=Trim(Copy(ASource[i],Length(AName)+1,MAX_PATH));
      p:=PosEx('"',Result,2);
      Delete(Result,p+1,MAX_PATH);
      Result:=StringReplace(DequoteStr(Result),'\\','\',[rfReplaceAll,rfIgnoreCase]);
      Break;
    end;
end;

const
  tfp = '%sThunderbird\';
  pref_defacc = 'user_pref("mail.accountmanager.defaultaccount",';
  pref_accsrv ='user_pref("mail.account.%s.server",';
  pref_srvdir = 'user_pref("mail.server.%s.directory",';
// user_pref("mail.accountmanager.defaultaccount", "account2");
// user_pref("mail.account.account2.server", "server1");
// user_pref("mail.server.server1.directory", "C:\\.EMailBox");
var
  pf,fn,s: string;
  i: Integer;
  sl: TStringList;
begin
  Result:='';
  if AUser='' then
    AUser:=GetUser;
  pf:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  fn:=Format(tfp+'profiles.ini',[pf]);
  if not FileExists(fn) then
    Exit;

  with TINIFile.Create(fn) do
    try
      i:=0;
      s:=StringReplace(ReadString(Format('Profile%d',[i]),'Path',''),'/','\',[rfReplaceAll,rfIgnorecase]);
      fn:=Format(tfp,[pf])+IncludeTrailingPathDelimiter(s)+'prefs.js';
      if not FileExists(fn) then
        Exit;
    finally
      Free;
    end;

  sl:=TStringList.Create;
  try
    sl.LoadFromFile(fn);
    s:=GetValue(sl,pref_defacc);
    if s<>'' then
      s:=GetValue(sl,Format(pref_accsrv,[s]));
    if s<>'' then
      Result:=GetValue(sl,Format(pref_srvdir,[s]));
  finally
    sl.Free;
  end;
end;

procedure GetOEDataPath;
const
  rkOE = 'Identities';
  rvName = 'Username';
  rkStorePath = 'Identities\%s\Software\Microsoft\Outlook Express\5.0';
  rvStorePath = 'Store Root';
  rvIO = 'Identity Ordinal';
var
  i,j: Integer;
  p,s: string;
  sl: TStringList;
  rk: string;
begin
  AList.Clear;
  if AUser='' then
    AUser:=GetUser;
  sl:=TStringList.Create;
  with TRegistry.Create do
   try
     RootKey:=HKEY_USERS;
     rk:=GetSIDFromAccount('',AUser)+'\';
     if OpenKeyReadOnly(rk+rkOE) then begin
       GetKeyNames(sl);
       CloseKey;
       for i:=0 to sl.Count-1 do
         if OpenKeyReadOnly(Format('%s\%s',[rk+rkOE,sl[i]])) then begin
           try j:=ReadInteger(rvIO) except j:=AList.Count+1 end;
           try s:=ReadString(rvName) except s:='' end;
           if s='' then
             s:=IntToStr(j);
           CloseKey;
           if OpenKeyReadOnly(Format(rk+rkStorePath,[sl[i]])) then begin
             try p:=ReadString(rvStorePath); except p:='' end;
             p:=StringReplace(p,'%UserProfile%',GetProfilePath(AUser),[rfIgnoreCase]);
             if p<>'' then
               AList.Add(Format('%s=%s',[s,IncludeTrailingPathDelimiter(p)]));
             CloseKey;
           end;
         end;
     end else begin
       p:=IncludeTrailingPathdelimiter(GetSpecialFolderEx(AUser,CSIDL_LOCAL_APPDATA))+'Identities\';
       BuildFileList(p,'*.dbx',faAnyFile,sl,True);
       for i:=0 to sl.Count-1 do
         if AList.IndexOf(ExtractFilePath(sl[i]))=-1 then
           AList.Add(Format('%d=%s',[AList.Count+1,ExtractFilePath(sl[i])]));
     end;
  finally
    Free;
    sl.Free;
  end;
end;

function GetWinMailDataPath;
const
  rkWinMail = 'Windows Mail';
  rvStore = 'Store Root';
  rvDMA = 'Default Mail Account';
var
  rk: string;
begin
  Result:='';
  if AUser='' then
    AUser:=GetUser;
  with TRegistry.Create do
    try
      RootKey:=HKEY_USERS;
      rk:=GetSIDFromAccount('',AUser)+'\Software\Microsoft\';
      if OpenKeyReadOnly(rk+rkWinMail) then begin
        Result:=ReadString(rvStore);
        Result:=IncludeTrailingPathDelimiter(StringReplace(Result,'%UserProfile%',GetProfilePath(AUser),[rfIgnoreCase]))+'Local Folders';
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetWLMDataPath;
const
  rkWinMail = 'Windows Live Mail';
  rvStore = 'Store Root';
  rvDMA = 'Default Mail Account';
var
  rk: string;
  s: string;
  i: Integer;
  sl: TStringList;
begin
  Result:='';
  if AUser='' then
    AUser:=GetUser;
  with TRegistry.Create do
    try
      RootKey:=HKEY_USERS;
      rk:=GetSIDFromAccount('',AUser)+'\Software\Microsoft\';
      if OpenKeyReadOnly(rk+rkWinMail) then begin
        Result:=ReadString(rvStore);
        Result:=IncludeTrailingPathDelimiter(StringReplace(Result,'%UserProfile%',GetProfilePath(AUser),[rfIgnoreCase]));
        s:=ReadString(rvDMA);
        CloseKey;
      end;
    finally
      Free;
    end;                                                              
  sl:=TStringList.Create;
  try
    if Result<>'' then
      BuildFileList(Result,'*.oeaccount',faAnyFile,sl,True);
    for i:=0 to sl.Count-1 do
      if SameText(ExtractFilename(sl[i]),s) then begin
        Result:=ExtractFilePath(sl[i]);
        Break;
      end;
  finally
    sl.Free;
  end;
end;

procedure GetTBDataPath;
const
  mfp = '%sThunderbird\';
  pfn1 = 'prefs.js.init';
  pfn2 = 'prefs.js';
var
  tpf,pf,fn,s: string;
  i,j: Integer;
  sl: TStringList;
begin
  AList.Clear;
  if AUser='' then
    AUser:=WindowsUser;


  pf:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  fn:=Format(mfp+'profiles.ini',[pf]);

  if not FileExists(fn) then
    Exit;

  sl:=TStringList.Create;
  try
    with TINIFile.Create(fn) do
      try
        i:=0;
        repeat
          s:=StringReplace(ReadString(Format('Profile%d',[i]),'Path',''),'/','\',[rfReplaceAll,rfIgnorecase]);
          if Pos(':',s)>0 then
            fn:=IncludeTrailingPathDelimiter(s)+pfn1
          else
            fn:=Format(mfp,[pf])+IncludeTrailingPathDelimiter(s)+pfn1;

          if FileExists(fn) then
            sl.Add(fn);

          if Pos(':',s)>0 then
            fn:=IncludeTrailingPathDelimiter(s)+pfn2
          else
            fn:=Format(mfp,[pf])+IncludeTrailingPathDelimiter(s)+pfn2;

          if FileExists(fn) then
            sl.Add(fn);

          if sl.Count>0 then
            Break;

          Inc(i);
        until s='';
      finally
        Free;
      end;

    for i:=0 to sl.Count-1 do
      with TPrefsJS.Create do
        try
          FileName:=sl[i];
          tpf:=IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
          s:=StringReplace(UserPrefsByName['mail.root.none'].Value,'\\','\',[rfReplaceAll,rfIgnoreCase]);
          if s<>'' then
            s:=IncludeTrailingPathDelimiter(s);
          if s<>'' then
            for j:=1 to 254 do begin
              s:=ExcludeTrailingPathDelimiter(StringReplace(UserPrefsByName[Format('mail.server.server%d.directory-rel',[j])].Value,'\\','\',[rfReplaceAll,rfIgnoreCase]));
              s:=StringReplace(s,'/','\',[rfReplaceAll,rfIgnoreCase]);
              if s<>'' then begin
                s:=StringReplace(s,'[ProfD]',tpf,[rfIgnoreCase]);
                s:=IncludeTrailingPathDelimiter(s);
              end;
              if DirectoryExists(s) and (UserPrefsByName[Format('mail.server.server%d.deferred_to_account',[j])].Value='') then
                AList.Add(UserPrefsByName[Format('mail.server.server%d.name',[j])].Value+'='+s);
            end;
        finally
          Free;
        end;
  finally
    sl.Free;
  end;
end;

function GetIEHistoryPath;
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_HISTORY))+'History.IE5\';
end;

function GetFirefoxHistoryPath(AUser: string = ''): string;
const
  mfp = '%sMozilla\Firefox\';
var
  pf,fn,s: string;
  i: Integer;
begin
  Result:='';
  if AUser='' then
    AUser:=GetUser;
  pf:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  fn:=Format(mfp+'profiles.ini',[pf]);

  if not FileExists(fn) then
    Exit;

  with TINIFile.Create(fn) do
    try
      i:=0;
      repeat
        s:=StringReplace(ReadString(Format('Profile%d',[i]),'Path',''),'/','\',[rfReplaceAll,rfIgnorecase]);
        fn:=Format(mfp,[pf])+IncludeTrailingPathDelimiter(s)+'places.sqlite';
        if FileExists(fn) then begin
          Result:=ExtractFilePath(fn);
          Break;
        end;
        Inc(i);
      until s='';
    finally
      Free;
    end;
end;

function GetChromeHistoryPath(AUser: string = ''): string;
const
  cfn = '%sGoogle\Chrome\User Data\Default\History';
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_LOCAL_APPDATA));
  Result:=Format(cfn,[Result]);
end;

function GetSafariHistoryPath(AUser: string = ''): string;
const
  cfn = '%s\Apple Computer\Safari\History.plist';
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  Result:=Format(cfn,[Result]);
end;

function GetOperaHistoryPath(AUser: string = ''): string;
const
  cfn = '%s\Opera\Opera\Profile\Global.dat';
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  Result:=Format(cfn,[Result]);
end;

function GetSkypeChatPath(AUser: string = ''): string;
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  Result:=IncludeTrailingPathDelimiter(Result)+'Skype';
end;

function GetICQChatPath(AUser: string = ''): string;
begin
  if AUser='' then
    AUser:=GetUser;
  Result:=IncludeTrailingPathDelimiter(GetSpecialFolderEx(AUser,CSIDL_APPDATA));
  Result:=IncludeTrailingPathDelimiter(Result)+'ICQ';
end;

function GetMSNChatPath(AUser: string = ''): string;
const
  rkMSN = '\Software\Microsoft\MSNMessenger\PerPassportSettings\';
var
  rk: string;
  sl: TStringList;
  i: Integer;
  s: string;
  ok: Boolean;
begin
  Result:='';
  if AUser='' then
    AUser:=GetUser;

  sl:=TStringList.Create;
  with TRegistry.Create do
    try
      RootKey:=HKEY_USERS;
      rk:=GetSIDFromAccount('',AUser)+rkMSN;
      if not OpenKeyReadOnly(rk) then begin
        RootKey:=HKEY_CURRENT_USER;
        rk:=rkMSN;
        ok:=OpenKeyReadOnly(rk);
      end else
        ok:=True;
      if ok then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do
          if OpenKeyReadOnly(rk+sl[i]) then begin
            s:=ReadString('MessageLogPath');
            if (s<>'') then begin
              Result:=IncludeTrailingPathDelimiter(s);
              Break;
            end;
            CloseKey;
          end;
      end;
  finally
    Free;
    sl.Free;
  end;
end;

function GetYahooChatPath(AUser: string = ''): string;
begin
  if AUser='' then
    AUser:=GetUser;
  with TRegistry.Create do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('SOFTWARE\Yahoo\Essentials') then begin
        Result:=IncludeTrailingPathDelimiter(ReadString('MainDir'))+'Messenger\Profiles\';
        CloseKey;
      end;
    finally
      Free;
    end;
end;


end.
