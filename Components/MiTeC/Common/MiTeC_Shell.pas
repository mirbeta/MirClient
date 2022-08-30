{*******************************************************}
{               MiTeC Common Routines                   }
{                  Shell routines                       }
{                                                       }
{          Copyright (c) 1997-2015 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Shell;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Win.Registry, System.SysUtils;
     {$ELSE}
     Windows, Registry,SysUtils;
     {$ENDIF}

type
  TNewEntryType = (etNullFile, etFileName, etCommand);

  TShellLinkRecord = record
    Name,
    Target,
    Arguments,
    WorkingDirectory,
    IconFile: string;
    IconIndex,
    ShowCmd: Integer;
  end;

procedure RegisterFileType(Extension, Typename, Description, Icon, EXEName :string);
procedure UnRegisterFileType(Extension, Typename: string);
function GetShellExtension(Extension,Typename: string): string;
procedure AddCMAction(RegistryKey, ActionName, MenuCaption, Action: string);
procedure RemoveCMAction(RegistryKey, ActionName: string);
procedure AddCMNew(Extension, Params: string; EntryType: TNewEntryType);
procedure RemoveCMNew(Extension: string);
//procedure CreateLinkToFile(FileName: string; LinkPath: string; LinkCaption: string);
procedure CreateShortCut(const LinkName: string; ARecord: TShellLinkRecord);
function ResolveLink(const LinkFile: TFileName; var ARecord: TShellLinkRecord): HRESULT;
function CheckShellCM(const ASection, AName: string): boolean;
procedure AddShellCM(const ASection, AName, AEXEName: string);
procedure RemoveShellCM(const ASection, AName: string);
procedure ShowShellContextPopup(const AFilename: string; X,Y: integer; AHandle: HWND);

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.ComObj, WinAPI.ShlObj, WinAPI.ActiveX;
     {$ELSE}
     ComObj, ShlObj, ActiveX;
     {$ENDIF}

function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;
var
  l: integer;
  f: Cardinal;
begin
  f:=MB_PRECOMPOSED;
  if codepage=CP_UTF8 then
    f:=0;
  if s = '' then
    Result := ''
  else begin
    l:=MultiByteToWideChar(codePage,f,PAnsiChar(@s[1]),-1,nil,0);
    SetLength(Result,l-1);
    if l>1 then
      MultiByteToWideChar(CodePage,f,PAnsiChar(@s[1]),-1,PWideChar(@Result[1]),l-1);
  end;
end;

procedure RegisterFileType(Extension, Typename, Description, Icon, EXEName :string);
var
  p: integer;
begin
  p:=pos('.',Extension);
  while p>0 do begin
    Delete(Extension,p,1);
    p:=pos('.',Extension);
  end;
  if (Extension='') or (EXEName='') then
    Exit;
  Extension:='.'+Extension;

  with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey(Format('\Software\Classes\%s',[Extension]), true) then begin
        WriteString('',Typename);
        CloseKey;
      end;
      if OpenKey(Format('\Software\Classes\%s',[Typename]), true) then begin
        WriteString('',Description);
        CloseKey;
      end;
      if OpenKey(Format('\Software\Classes\%s\DefaultIcon',[Typename]), true) then begin
        WriteString('',Icon);
        CloseKey;
      end;
      if OpenKey(Format('\Software\Classes\%s\shell\open\command',[Typename]), true) then begin
        WriteString('',Format('%s "%%1"',[ExeName]));
        CloseKey;
      end;
    finally
      Free;
    end;
  SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,nil,nil);

  {with TRegINIFile.Create('') do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if RegistryKey='' then
        RegistryKey:=Copy(Extension,2,MaxInt)+'_auto_file';
      WriteString(Extension,'',RegistryKey);
      WriteString(RegistryKey,'',Description);
      if Icon<>'' then
        WriteString(RegistryKey+'\DefaultIcon','',Icon);
      WriteString(RegistryKey+'\shell\open\command','',EXEName+' "%1"');
    finally
      Free;
    end;}
end;

procedure UnRegisterFileType(Extension, Typename: string);
var
  s: string;
  p: integer;
begin
  p:=pos('.',Extension);
  while p>0 do begin
    Delete(Extension,p,1);
    p:=pos('.',Extension);
  end;
  if (Extension='') or (Typename='') then
    Exit;
  Extension:='.'+Extension;
  with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKey(Format('\Software\Classes\%s',[Extension]),False) then begin
        s:=ReadString('');
        if SameText(s,Typename) then
          WriteString('','');
        CloseKey;
      end;
      DeleteKey(Format('\Software\Classes\%s\shell\open\command',[Typename]));
      DeleteKey(Format('\Software\Classes\%s\shell\open',[Typename]));
      DeleteKey(Format('\Software\Classes\%s\shell',[Typename]));
      DeleteKey(Format('\Software\Classes\%s\DefaultIcon',[Typename]));
      DeleteKey(Format('\Software\Classes\%s',[Typename]));


      {RootKey:=HKEY_CLASSES_ROOT;
      if OpenKey(Extension,False) then begin
        s:=ReadString('');
        CloseKey;
        DeleteKey(Extension);
        DeleteKey(s);
      end;}
    finally
      Free;
    end;
  SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,nil,nil);
end;

function GetShellExtension(Extension,Typename: string): string;
var
  i: Integer;
  p: integer;
begin
  p:=pos('.',Extension);
  while p>0 do begin
    Delete(Extension,p,1);
    p:=pos('.',Extension);
  end;
  if (Extension='') or (Typename='') then
    Exit;
  Extension:='.'+Extension;
  Result:='';
  with TRegistry.Create do
    try
      RootKey:=HKEY_CURRENT_USER;
      if OpenKeyReadOnly(Format('\Software\Classes\%s\shell\open\command',[Typename])) then begin
        Result:=ReadString('');
        CloseKey;
      end;
      {RootKey:=HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly(Extension) then begin
        s:=ReadString('');
        CloseKey;
        if OpenKeyReadOnly(s+'\shell\open\command') then begin
          Result:=ReadString('');
          CloseKey;
        end;
      end;}
    finally
      Free;
    end;
  i:=Pos('"',Result);
  if i=1 then begin
    Result:=Copy(Result,i+1,Length(Result)-1);
    i:=Pos('"',Result);
    Result:=Copy(Result,1,i-1);
  end else
    Result:=Trim(Copy(Result,1,i-1));
end;

procedure AddCMAction;
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if ActionName='' then
        ActionName:=MenuCaption;
      if Copy(RegistryKey,1,1)='.' then
        RegistryKey:=Copy(RegistryKey,2,MaxInt)+'_auto_file';
      if Copy(RegistryKey,Length(RegistryKey),1)<>'\' then
        RegistryKey:=RegistryKey+'\';
      if Copy(ActionName,Length(ActionName),1)<>'\' then
        ActionName:=ActionName+'\';
      if OpenKey(RegistryKey+'Shell\'+ActionName,True) then begin
        WriteString('',MenuCaption);
        CloseKey;
      end;
      if OpenKey(RegistryKey+'Shell\'+ActionName+'Command\',True) then
        WriteString('',ActionName);
    finally
      Free;
    end;
end;

procedure RemoveCMAction;
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if Copy(RegistryKey,1,1)='.' then
        RegistryKey:=Copy(RegistryKey,2,MaxInt)+'_auto_file';
      if Copy(RegistryKey,Length(RegistryKey),1)<>'\' then
        RegistryKey:=RegistryKey+'\';
      if OpenKey('\'+RegistryKey+'shell\',True) then begin
        if KeyExists(ActionName) then
          DeleteKey(ActionName);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure AddCMNew;
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if KeyExists(Extension) then begin
        if OpenKey(Extension+'\ShellNew',True) then
          case EntryType of
            etNullFile: WriteString('NullFile', '');
            etFileName: WriteString('FileName',Params);
            etCommand: WriteString('Command',Params);
          end;
          CloseKey;
        end;
    finally
      Free;
    end;
end;

procedure RemoveCMNew;
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if KeyExists(Extension) then
        if OpenKey(Extension,True) then begin
          if KeyExists('ShellNew') then
            DeleteKey('ShellNew');
          CloseKey;
        end;
    finally
      Free;
    end;
end;

const
  LinkExt = '.lnk';
  IID_IPersistFile: TGUID = (
    D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

{procedure CreateLinkToFile(FileName: string; LinkPath: string; LinkCaption: string);
begin
  FileName := ExpandFileName(FileName);
  if LinkPath[Length(LinkPath)] <> '\' then LinkPath := LinkPath + '\';
  LinkPath := ExtractFilePath(ExpandFileName(LinkPath));
  CreateShortCut(FileName, LinkPath + LinkCaption + LinkExt);
end;}

function ResolveLink(const LinkFile: TFileName; var ARecord: TShellLinkRecord): HRESULT;
var
  wfd: TWIN32FINDDATA;
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
begin
  ZeroMemory(@ARecord,SizeOf(ARecord));
  IObject:=CreateComObject(CLSID_ShellLink);
  ISLink:=IObject as IShellLink;
  IPFile:=IObject as IPersistFile;
  Result:=IPFile.Load(PWideChar({$IFNDEF UNICODE}ANSIToWide{$ENDIF}(LinkFile)),STGM_READ);
  if Succeeded(Result) then begin
    Result:=ISLink.Resolve(0,SLR_NO_UI);
    if Succeeded(Result) then begin
      SetLength(ARecord.Target,MAX_PATH);
      SetLength(ARecord.Arguments,255);
      SetLength(ARecord.WorkingDirectory,MAX_PATH);
      SetLength(ARecord.IconFile,MAX_PATH);
      SetLength(ARecord.Name,255);
      Result:=ISLink.GetPath(PChar(ARecord.Target),MAX_PATH,wfd,SLGP_UNCPRIORITY);
      if Succeeded(Result) then begin
        SetLength(ARecord.Target,Length(PChar(ARecord.Target)));
        // issue in 32bit code on 64bit os
        if not FileExists(ARecord.Target) and (Pos(' (x86)',ARecord.Target)>0) then
          ARecord.Target:=StringReplace(ARecord.Target,' (x86)','',[rfIgnorecase]);
      end;
      Result:=ISLink.GetArguments(PChar(ARecord.Arguments),255);
      if Succeeded(Result) then
        SetLength(ARecord.Arguments,Length(PChar(ARecord.Arguments)));
      Result:=ISLink.GetWorkingDirectory(PChar(ARecord.WorkingDirectory),255);
      if Succeeded(Result) then
        SetLength(ARecord.WorkingDirectory,Length(PChar(ARecord.WorkingDirectory)));
      Result:=ISLink.GetIconLocation(PChar(ARecord.IconFile),255,ARecord.IconIndex);
      if Succeeded(Result) then
        SetLength(ARecord.IconFile,Length(PChar(ARecord.IconFile)));
      Result:=ISLink.GetDescription(PChar(ARecord.Name),255);
      if Succeeded(Result) then
        SetLength(ARecord.Name,Length(PChar(ARecord.Name)));
      Result:=ISLink.GetShowCmd(ARecord.ShowCmd);
    end;
  end;
end;

procedure CreateShortCut(const LinkName: string; ARecord: TShellLinkRecord);
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
begin
  IObject:=CreateComObject(CLSID_ShellLink);
  ISLink:=IObject as IShellLink;
  IPFile:=IObject as IPersistFile;

  ISLink.SetPath(PChar(ARecord.Target));
  ISLink.SetArguments(PChar(ARecord.Arguments));
  ISLink.SetWorkingDirectory(PChar(ARecord.WorkingDirectory));
  ISLink.SetIconLocation(PChar(ARecord.IconFile),ARecord.IconIndex);
  ISLink.SetDescription(PChar(ARecord.Name));
  ISLink.SetShowCmd(ARecord.ShowCmd);
  IPFile.Save(PWideChar({$IFNDEF UNICODE}ANSIToWide{$ENDIF}(LinkName)),True);
end;

function CheckShellCM(const ASection, AName: string): boolean;
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      Result:=KeyExists(Format('%s\shell\%s',[ASection,AName]));
    finally
      Free;
    end;
end;

procedure AddShellCM(const ASection, AName, AEXEName: string);
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if OpenKey(Format('%s\shell\%s',[ASection,AName]),True) then begin
        WriteString('',AName+'...');
        WriteString('Icon','"'+AEXEName+'",0');
        CloseKey;
      end;
      if OpenKey(Format('%s\shell\%s\command',[ASection,AName]),True) then begin
        WriteString('','"'+AEXEName+'" "%1"');
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure RemoveShellCM(const ASection, AName: string);
begin
  with TRegistry.Create do
    try
      RootKey:=HKEY_CLASSES_ROOT;
      if KeyExists(Format('%s\shell\%s',[ASection,AName])) then
        DeleteKey(Format('%s\shell\%s',[ASection,AName]));
    finally
      Free;
    end;
end;

procedure ShowShellContextPopup(const AFilename: string; X,Y: integer; AHandle: HWND);
var
  Root: IShellFolder;
  ShellParentFolder: IShellFolder;
  chEaten,dwAttributes: ULONG;
  FilePIDL,ParentFolderPIDL: PItemIDList;
  CM: IContextMenu;
  Menu: HMenu;
  Command: LongBool;
  ICM2: IContextMenu2;
  ICI: TCMInvokeCommandInfo;
  ICmd: integer;
  P: TPoint;
begin
  OleCheck(SHGetDesktopFolder(Root));
  OleCheck(Root.ParseDisplayName(AHandle,nil,PWideChar(WideString(ExtractFilePath(AFilename))),chEaten,ParentFolderPIDL,dwAttributes));
  OleCheck(Root.BindToObject(ParentFolderPIDL,nil,IShellFolder,ShellParentFolder));
  OleCheck(ShellParentFolder.ParseDisplayName(AHandle,nil,PWideChar(WideString(ExtractFileName(AFilename))),chEaten,FilePIDL,dwAttributes));
  ShellParentFolder.GetUIObjectOf(AHandle,1,FilePIDL,IID_IContextMenu,nil,CM);
  if not Assigned(CM) then
    Exit;

  P.X:=X;
  P.Y:=Y;

  ClientToScreen(AHandle,P);
  Menu:=CreatePopupMenu;
  try
    CM.QueryContextMenu(Menu,0,1,$7FFF,CMF_EXPLORE or CMF_CANRENAME);
    CM.QueryInterface(IID_IContextMenu2,ICM2);
    try
      Command:=TrackPopupMenu(Menu,TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON or TPM_RETURNCMD,p.X,p.Y,0,AHandle,nil);
    finally
      ICM2:=nil;
    end;

    if Command then begin
      ICmd:=LongInt(Command)-1;
      FillChar(ICI,SizeOf(ICI),#0);
      with ICI do begin
        cbSize:=SizeOf(ICI);
        hWND:=0;
        lpVerb:=MakeIntResourceA(ICmd);
        nShow:=SW_SHOWNORMAL;
      end;
      CM.InvokeCommand(ICI);
    end;
  finally
    DestroyMenu(Menu);
  end;
end;

initialization
  OleInitialize(nil);
finalization
  OleUninitialize;
end.



