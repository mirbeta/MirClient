{*******************************************************}
{               MiTeC Common Routines                   }
{                  Dialog routines                      }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MiTeC_Dialogs;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Classes, System.SysUtils, WinAPI.ShlObj, VCL.Forms,
     Vcl.StdCtrls, Vcl.Graphics, Vcl.Controls;
     {$ELSE}
     Windows, Classes, SysUtils, ShlObj, Forms, StdCtrls, Graphics, Controls;
     {$ENDIF}

type
  TOpenFileNameEx = packed record
     // Size of the structure in bytes.
    lStructSize: Cardinal;
     // Handle that is the parent of the dialog.
    hWndOwner: HWND;
     // Application instance handle.
    hInstance: HINST;
     // String containing filter information.
    lpstrFilter: PChar;
     // Will hold the filter chosen by the user.
    lpstrCustomFilter: PChar;
     // Size of lpstrCustomFilter, in bytes.
    nMaxCustFilter: Cardinal;
     // Index of the filter to be shown.
    nFilterIndex: Integer;
     // File name to start with (and retrieve).
    lpstrFile: PChar;
     // Size of lpstrFile, in bytes.
    nMaxFile: Cardinal;
     // File name without path will be returned.
    lpstrFileTitle: PChar;
     // Size of lpstrFileTitle, in bytes.
    nMaxFileTitle: Cardinal;
     // Starting directory.
    lpstrInitialDir: PChar;
     // Title of the open dialog.
    lpstrTitle: PChar;
     // Controls user selection options.
    Flags: Cardinal;
     // Offset of file name in filepath=lpstrFile.
    nFileOffset: Word;
     // Offset of extension in filepath=lpstrFile.
    nFileExtension: Word;
     // Default extension if no extension typed.
    lpstrDefExt: PChar;
     // Custom data to be passed to hook.
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: Cardinal; wParam: WPARAM;
      lParam: LPARAM): Cardinal stdcall;  // Hook.
     // Template dialog, if applicable.
    lpTemplateName: PChar;
     // Extended structure starts here.
    pvReserved: Pointer;   // Reserved, use nil.
    dwReserved: Cardinal;     // Reserved, use 0.
    FlagsEx: Cardinal;        // Extended Flags.
  end;

  TControlApplet = (cplAll, cplAppWiz, cplTimeDate, cplDisplay, cplMultimedia,
                    cplNetwork, cplIntl, cplSystem, cplHwWiz);

  TFormatMode = (fmFull, fmQuick);

function YesNo(Text :string; ATitle: string = ''; AAppRestore: Boolean = False) :boolean;
function YesNoCancel(Text :string; ATitle: string = ''; AAppRestore: Boolean = False) :integer;
procedure Warn(Text :string; ATitle: string = ''; AAppRestore: Boolean = False);
function WarnYesNo(Text :string; ATitle: string = ''; AAppRestore: Boolean = False) :Boolean;
function WarnOKCancel(Text :string; ATitle: string = ''; AAppRestore: Boolean = False) :Boolean;
procedure Error(Text :string; ATitle: string = ''; AAppRestore: Boolean = False);
procedure ErrorAbort(Text :string; ATitle: string = ''; AAppRestore: Boolean = False);
procedure Debug(Text :string; ATitle: string = ''; AAppRestore: Boolean = False);
procedure Info(Text :string; ATitle: string = ''; AAppRestore: Boolean = False);

{$IFNDEF BCB}

function GetFileOpenDlg(AHandle: THandle;
                        var FileName: string;
                        AFilter: string;
                        ATitle: string = ''): Boolean;
function GetFileSaveDlg(AHandle: THandle;
                        var ADir: string;
                        var FileName: string;
                        AFilter: string;
                        ATitle: string = ''): Boolean;
function ConcatFilters(const Filters: array of string): string;
function BrowseFolderDlg(Handle: HWND; var FolderName: string; Caption: string): Boolean;
function ComputerNameDlg(Handle: HWND; var ComputerName: string; Caption: string): Boolean;
function RunDlg(Handle, IconHandle: HWND; Caption, Description: string): Integer;
function FormatDlg(Handle: HWND; FormatType: TFormatMode; DriveChar: char): integer;
function FindFilesDlg: Boolean;
function FindComputerDlg: Boolean;
procedure ShutdownDlg(Wnd: THandle; Kind: Integer);
procedure ControlApplet(AHandle: THandle; Applet: TControlApplet = cplAll); overload;
procedure ControlApplet(AHandle: THandle; Applet: string = ''); overload;
function ShellPropDlg(const Handle: HWND; const FileName: string): Boolean;

procedure FreePIDL(PIDL: PItemIDList); stdcall;
function SHFormatDrive(wnd: HWND; drive : Cardinal; fmtID : Cardinal; options : Cardinal): Cardinal; stdcall;
procedure SHShutDownDialog(Wnd: THandle; Kind: Integer); stdcall;
function SHRunDialog(Wnd: THandle; Unknown1: Integer; Unknown2: Pointer; szTitle: PChar; szPrompt: PChar; uiFlages: Integer): DWORD; stdcall;
function SHChangeIcon(wnd : HWND; szFileName : PChar; reserved : integer; var lpIconIndex : integer) : Cardinal; stdcall;
function SHFindFiles(Root: PItemIDList; SavedSearchFile: PItemIDList): LongBool; stdcall;
function SHFindComputer(Reserved1: PItemIDList; Reserved2: PItemIDList): LongBool; stdcall;
function SHObjectProperties(Owner: HWND; Flags: Cardinal; ObjectName: Pointer; InitialTabName: Pointer): LongBool; stdcall;
function SHNetConnectionDialog(Owner: HWND; ResourceName: Pointer; ResourceType: Cardinal): Cardinal; stdcall;
function SHStartNetConnectionDialog(Owner: HWND; ResourceName: PWideChar; ResourceType: Cardinal): Cardinal; stdcall;
function SHOutOfMemoryMessageBox(Owner: HWND; Caption: Pointer; Style: Cardinal): Integer; stdcall;
procedure SHHandleDiskFull(Owner: HWND; uDrive: Cardinal); stdcall;
function  GetOpenFileNameEx(var OpenFile: TOpenFilenameEx): Bool; stdcall;
function  GetSaveFileNameEx(var SaveFile: TOpenFileNameEx): bool; stdcall;
procedure NewLinkHere(HWND : THandle; HInstance : THandle; CmdLine : PChar; cmdShow : integer); stdcall;

{$ENDIF}

{$IFDEF UNICODE}
function InputIntegerQuery(const ACaption, APrompt: string; var Value: Integer): Boolean;
function InputRealQuery(const ACaption, APrompt: string; var Value: Single; ADecimalPlaces: Byte = 2): Boolean;
function InputQueryEx(const ACaption, APrompt: string; AMaxLength: Integer; ACharCase: TEditCharCase; APasswordChar: Char; var Value: string): Boolean;
{$ENDIF}

var
  LastDir: string;
  SelectedFilename: string;

  MessageTitle_Confirmation: string = 'Confirmation';
  MessageTitle_Warning: string = 'Warning';
  MessageTitle_Information: string = 'Information';
  MessageTitle_Error: string = 'Error';
  MessageTitle_Debug: string = 'Debug message';

  MsgBoxLevel: Integer;

const  
  SHFMT_ID_DEFAULT = $FFFF;
  SHFMT_OPT_FULL   = $0001;
  SHFMT_OPT_SYSONLY= $0002;
 // Special return values. PLEASE NOTE that these are Cardinal values.
  SHFMT_ERROR = $FFFFFFFF;  // Error on last format
 // drive may be formatable
  SHFMT_CANCEL = $FFFFFFFE;  // Last format wascanceled
  SHFMT_NOFORMAT = $FFFFFFFD;  // Drive is not formatable

  LB_FILETYPES_ID = 1089; // "File types:" label
  LB_FILENAME_ID = 1090;  // "File name:" label
  LB_DRIVES_ID = 1091;    // "Look in:" label


implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ShellAPI, WinAPI.CommDlg, Vcl.Consts,
     {$ELSE}
     ShellAPI, CommDlg, {$IFDEF FPC}{$ELSE}Consts,{$ENDIF}
     {$ENDIF}
     MiTeC_StrUtils;


const
  MAXSIZE = 10240;
  Shell32 = 'shell32.dll';

var
  ofn: TOpenFilename;
  buffer: array [0..MAXSIZE - 1] of Char;

function GetActiveWindowHandle: THandle;
begin
  {$IFDEF RAD5PLUS}
  Result:=Application.ActiveFormHandle;
  {$ELSE}
  Result:=0;
  {$ENDIF}
  if Result=0 then
    try Result:=Application.MainForm.Handle except Result:=0 end;
  {$IFNDEF FPC}
  if Result=0 then
    Result:=Application.Handle;
  {$ENDIF}
end;

procedure GetProcedureAddress(var P: Pointer; const ModuleName: string; ProcName: PAnsiChar);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        Exit;
    end;
    P:=GetProcAddress(ModuleHandle,PChar(ProcName));
    if not Assigned(P) then
      Exit;
  end;
end;

function YesNo(Text :string; ATitle: string = ''; AAppRestore: Boolean = False) : boolean;
begin
  //Result:=MessageDlg(Text,mtConfirmation,[mbYes,mbNo],0)=mrYes;
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Confirmation;
  Inc(MsgBoxLevel);
  Result:=MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_YESNO or MB_ICONQUESTION)=IDYES;
  Dec(MsgBoxLevel);
end;

function YesNoCancel;
begin
  //Result:=MessageDlg(Text,mtConfirmation,[mbYes,mbNo,mbCancel],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Confirmation;
  Result:=MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_YESNOCANCEL or MB_ICONQUESTION);
end;

procedure Warn;
begin
  //MessageDlg(Text,mtWarning,[mbOK],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Warning;
  Inc(MsgBoxLevel);
  MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_OK or MB_ICONWARNING);
  Dec(MsgBoxLevel);
end;

function WarnYesNo;
begin
  //Result:=MessageDlg(Text,mtWarning,[mbYes,mbNo],0)=mrYes;
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Warning;
  Inc(MsgBoxLevel);
  Result:=MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_YESNO or MB_ICONWARNING)=IDYES;
  Dec(MsgBoxLevel);
end;

function WarnOKCancel;
begin
  //Result:=MessageDlg(Text,mtWarning,[mbYes,mbNo,mbCancel],0)=mrOK;
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Warning;
  Inc(MsgBoxLevel);
  Result:=MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_OKCANCEL or MB_ICONWARNING or MB_TASKMODAL)=IDOK;
  Dec(MsgBoxLevel);
end;

procedure Error;
begin
  //MessageDlg(Text,mtError,[mbOK],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Error;
  Inc(MsgBoxLevel);
  MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_OK or MB_ICONERROR);
  Dec(MsgBoxLevel);
end;

procedure ErrorAbort;
begin
  //MessageDlg(Text,mtError,[mbOK],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Error;
  Inc(MsgBoxLevel);
  MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_OK or MB_ICONERROR);
  Dec(MsgBoxLevel);
  Abort;
end;

procedure Debug;
begin
  //MessageDlg(Text,mtInformation,[mbOK],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Debug;
  Inc(MsgBoxLevel);
  MessageBox(GetActiveWindowHandle,PChar(Text),PChar(ATitle),MB_OK or MB_ICONINFORMATION);
  Dec(MsgBoxLevel);
end;

procedure Info;
begin
  //MessageDlg(Text,mtInformation,[mbOK],0);
  if AAppRestore then
    Application.Restore;
  if ATitle='' then
    ATitle:=MessageTitle_Information;
  Inc(MsgBoxLevel);
  MessageBox(GetActiveWindowHandle,PChar(Text),PChar(Atitle),MB_OK or MB_ICONINFORMATION);
  Dec(MsgBoxLevel);
end;

{$IFNDEF BCB}
function GetFileOpenDlg;
var
  e: Cardinal;
  ADir: string;
begin
  if ADir='' then
    ADir:=ExtractFilePath(ParamStr(0));
  StrPCopy(PChar(@buffer),FileName);
  ofn.lStructSize:=SizeOf(TOpenFilename);
  ofn.hWndOwner:=AHandle;
  ofn.hInstance:=HInstance;
  ofn.lpstrFilter:=PChar(AFilter);
  ofn.lpstrFile:=buffer;
  ofn.nMaxFile:=MAXSIZE;
  ofn.lpstrTitle:=PChar(ATitle);
  ofn.lpstrInitialDir:=PChar(ADir);
  ofn.Flags:=OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or
             OFN_LONGNAMES or OFN_EXPLORER or OFN_HIDEREADONLY;
  Result:=GetOpenFileName({$IFDEF FPC}@{$ENDIF}ofn);
  e:=CommDlgExtendedError;
  if e=1 then begin
    ofn.Flags:=OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST or
               OFN_LONGNAMES or OFN_HIDEREADONLY;
    Result:=GetOpenFileName({$IFDEF FPC}@{$ENDIF}ofn);
    e:=CommDlgExtendedError;
  end;
  if e>0 then
    raise Exception.Create(Format('GetFileOpenDlg.CommDlgExtendedError = %x',[e]));
  Filename:=buffer;
end;

function GetFileSaveDlg;
var
 e: Cardinal;
begin
  if ADir='' then
    ADir:=ExtractFilePath(ParamStr(0));
  StrPCopy(PChar(@buffer),FileName);
  ofn.lStructSize:=SizeOf(TOpenFilename);
  ofn.hWndOwner:=AHandle;
  ofn.hInstance:=HInstance;
  ofn.lpstrFilter:=PChar(AFilter);
  ofn.lpstrFile:=buffer;
  ofn.nMaxFile:=MAXSIZE;
  ofn.lpstrTitle:=PChar(ATitle);
  ofn.lpstrInitialDir:=PChar(ADir);
  ofn.Flags:=OFN_PATHMUSTEXIST or OFN_OVERWRITEPROMPT or
               OFN_LONGNAMES or OFN_EXPLORER or OFN_HIDEREADONLY;
  Result:=GetSaveFileName({$IFDEF FPC}@{$ENDIF}ofn);
  e:=CommDlgExtendedError;
  if e=1 then begin
    ofn.Flags:=OFN_PATHMUSTEXIST or OFN_OVERWRITEPROMPT or
               OFN_LONGNAMES or OFN_HIDEREADONLY;
    Result:=GetSaveFileName({$IFDEF FPC}@{$ENDIF}ofn);
    e:=CommDlgExtendedError;
  end;
  if e>0 then
    raise Exception.Create(Format('GetFileSaveDlg.CommDlgExtendedError = %x',[e]));
  Filename:=buffer;
  ADir:=ExtractFilepath(FileName);
end;

function ConcatFilters;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to High(Filters) do
    Result:=Result+Copy(Filters[i],1,Length(Filters[i])-1);
end;

function  SHFormatDrive;           external shell32 name 'SHFormatDrive';
function  GetOpenFileNameEx;       external 'comdlg32.dll' name 'GetOpenFileNameA';
function  GetSaveFileNameEx;       external 'comdlg32.dll' name 'GetSaveFileNameA';
procedure NewLinkHere(HWND : THandle; HInstance : THandle; CmdLine : PChar; cmdShow : integer); stdcall; external 'appwiz.cpl' name 'NewLinkHereA';

type
  TFreePIDL = procedure (PIDL: PItemIDList); stdcall;
var
  _FreePIDL: TFreePIDL;

procedure FreePIDL; //                external 'shell32.dll' index 155;
begin
  GetProcedureAddress(Pointer(@_FreePIDL), shell32, PAnsiChar(155));
  if Assigned(_FreePIDL) then
    _FreePIDL(PIDL);
end;

type
  TSHShutDownDialog = procedure(Wnd: THandle); stdcall;
  TSHShutDownDialog6 = procedure(Wnd: THandle; Kind: Integer); stdcall;
var
  _SHShutDownDialog: TSHShutDownDialog;
  _SHShutDownDialog6: TSHShutDownDialog6;

procedure SHShutDownDialog; //       external 'shell32.dll' index 60;
begin
  if Win32MajorVersion>=6 then begin
    GetProcedureAddress(Pointer(@_SHShutDownDialog6), shell32, PAnsiChar(60));
    if Assigned(_SHShutDownDialog6) then
      _SHShutDownDialog6(Wnd,Kind);
  end else begin
    GetProcedureAddress(Pointer(@_SHShutDownDialog), shell32, PAnsiChar(60));
    if Assigned(_SHShutDownDialog) then
      _SHShutDownDialog(Wnd);
  end;
end;

type
  TSHRunDialog = function(Wnd: THandle; Unknown1: Integer; Unknown2: Pointer; szTitle: PChar; szPrompt: PChar; uiFlages: Integer): DWORD; stdcall;
var
  _SHRunDialog: TSHRunDialog;

function  SHRunDialog; //            external 'shell32.dll' index 61;
begin
  GetProcedureAddress(Pointer(@_SHRunDialog), shell32, PAnsiChar(61));
  if Assigned(_SHRunDialog) then
    Result:=_SHRunDialog(Wnd,Unknown1,Unknown2,szTitle,szPrompt,uiFlages)
  else
    Result:=0;
end;

type
  TSHChangeIcon = function(wnd : HWND; szFileName : PChar; reserved : integer; var lpIconIndex : integer) : Cardinal; stdcall;
var
  _SHChangeIcon: TSHChangeIcon;

function  SHChangeIcon; //            external 'shell32.dll' index 62;
begin
  GetProcedureAddress(Pointer(@_SHChangeIcon), shell32, PAnsiChar(61));
  if Assigned(_SHChangeIcon) then
    Result:=_SHChangeIcon(Wnd,szFileName,reserved,lpIconIndex)
  else
    Result:=0;
end;

type
  TSHFindFiles = function (Root: PItemIDList; SavedSearchFile: PItemIDList): LongBool; stdcall;
var
  _SHFindFiles: TSHFindFiles;

function  SHFindFiles; //            external 'shell32.dll' index 90;
begin
  GetProcedureAddress(Pointer(@_SHFindFiles), shell32, PAnsiChar(90));
  if Assigned(_SHFindFiles) then
    Result:=_SHFindFiles(Root,SavedSearchFile)
  else
    Result:=False;
end;

type
  TSHFindComputer = function (Reserved1: PItemIDList; Reserved2: PItemIDList): LongBool; stdcall;
var
  _SHFindComputer: TSHFindComputer;

function  SHFindComputer; //         external 'shell32.dll' index 91;
begin
  GetProcedureAddress(Pointer(@_SHFindComputer), shell32, PAnsiChar(91));
  if Assigned(_SHFindComputer) then
    Result:=_SHFindComputer(Reserved1,Reserved2)
  else
    Result:=False;
end;

type
  TSHObjectProperties = function (Owner: HWND; Flags: Cardinal; ObjectName: Pointer; InitialTabName: Pointer): LongBool; stdcall;
var
  _SHObjectProperties: TSHObjectProperties;

function  SHObjectProperties; //     external 'shell32.dll' index 178;
begin
  GetProcedureAddress(Pointer(@_SHObjectProperties), shell32, PAnsiChar(178));
  if Assigned(_SHObjectProperties) then
    Result:=_SHObjectProperties(Owner,Flags,ObjectName,InitialTabName)
  else
    Result:=False;
end;

type
  TSHNetConnectionDialog = function (Owner: HWND; ResourceName: Pointer; ResourceType: Cardinal): Cardinal; stdcall;
var
  _SHNetConnectionDialog: TSHNetConnectionDialog;

function  SHNetConnectionDialog; //  external 'shell32.dll' index 160;
begin
  GetProcedureAddress(Pointer(@_SHNetConnectionDialog), shell32, PAnsiChar(160));
  if Assigned(_SHNetConnectionDialog) then
    Result:=_SHNetConnectionDialog(Owner,ResourceName,ResourceType)
  else
    Result:=0;
end;

type
  TSHOutOfMemoryMessageBox = function (Owner: HWND; Caption: Pointer; Style: Cardinal): Integer; stdcall;
var
  _SHOutOfMemoryMessageBox: TSHOutOfMemoryMessageBox;

function  SHOutOfMemoryMessageBox; // external 'shell32.dll' index 126;
begin
  GetProcedureAddress(Pointer(@_SHOutOfMemoryMessageBox), shell32, PAnsiChar(126));
  if Assigned(_SHOutOfMemoryMessageBox) then
    Result:=_SHOutOfMemoryMessageBox(Owner,Caption,Style)
  else
    Result:=0;
end;

type
  TSHHandleDiskFull = procedure (Owner: HWND; uDrive: Cardinal); stdcall;
var
  _SHHandleDiskFull: TSHHandleDiskFull;

procedure SHHandleDiskFull; //       external 'shell32.dll' index 185;
begin
  GetProcedureAddress(Pointer(@_SHHandleDiskFull), shell32, PAnsiChar(185));
  if Assigned(_SHHandleDiskFull) then
    SHHandleDiskFull(Owner,uDrive)
end;

type
  TSHStartNetConnectionDialog = function (Owner: HWND; ResourceName: PWideChar; ResourceType: Cardinal): Cardinal; stdcall;
var
  _SHStartNetConnectionDialog: TSHStartNetConnectionDialog;

function  SHStartNetConnectionDialog; // external 'shell32.dll' index 215 ;
begin
  GetProcedureAddress(Pointer(@_SHStartNetConnectionDialog), shell32, PAnsiChar(215));
  if Assigned(_SHStartNetConnectionDialog) then
    Result:=_SHStartNetConnectionDialog(Owner,ResourceName,ResourceType)
  else
    Result:=0;
end;

function BrowseFolderDlg(Handle: HWND; var FolderName: string; Caption: string): boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  ItemSelected : PItemIDList;
  NameBuffer: array[0..MAX_PATH] of Char;
//  WindowList: Pointer;
begin
  StrPCopy(NameBuffer,FolderName);
  itemIDList:=nil;
  FillChar(BrowseInfo,SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner:=Handle;
  BrowseInfo.pidlRoot:=ItemIDList;
  BrowseInfo.pszDisplayName:=NameBuffer;
  BrowseInfo.lpszTitle:=PChar(Caption);
  BrowseInfo.ulFlags:=BIF_RETURNONLYFSDIRS;
  //WindowList:=DisableTaskWindows(0);
  try
    ItemSelected:=SHBrowseForFolder(BrowseInfo);
    Result:=ItemSelected<>nil;
  finally
    //EnableTaskWindows(WindowList);
  end;

  if Result then begin
    SHGetPathFromIDList(ItemSelected,NameBuffer);
    FolderName:=NameBuffer;
   end;
  Freepidl(BrowseInfo.pidlRoot);
end;

function ComputerNameDlg(Handle: HWND; var ComputerName: string; Caption: string): boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  NameBuffer: array[0..MAX_PATH] of Char;
//  WindowList: Pointer;
begin
  Result:=False;
  if Failed(SHGetSpecialFolderLocation(Handle,CSIDL_NETWORK,ItemIDList)) then
     Exit;
  FillChar(BrowseInfo,SizeOf(BrowseInfo), 0);
  BrowseInfo.hwndOwner:=Handle;
  BrowseInfo.pidlRoot:=ItemIDList;
  BrowseInfo.pszDisplayName:=NameBuffer;
  BrowseInfo.lpszTitle:=PChar(Caption);
  BrowseInfo.ulFlags:=BIF_BROWSEFORCOMPUTER;
  //WindowList:=DisableTaskWindows(0);
  try
    Result:=SHBrowseForFolder(BrowseInfo)<>nil;
  finally
    //EnableTaskWindows(WindowList);
    FreePidl(BrowseInfo.pidlRoot);
  end;
  if Result then
    ComputerName:=NameBuffer;
end;

procedure ControlApplet(AHandle: THandle; Applet: TControlApplet = cplAll); overload;
var
  s: string;
begin
  s:='';
  case Applet of
    cplAppWiz: s:='appwiz.cpl';
    cplTimeDate: s:='timedate.cpl';
    cplDisplay: s:='desk.cpl';
    cplMultimedia: s:='mmsys.cpl';
    cplNetwork: s:='ncpa.cpl';
    cplIntl: s:='intl.cpl';
    cplSystem: s:='sysdm.cpl';
    cplHWWiz: s:='hdwwiz.cpl';
  end;
  ShellExecute(AHandle,'open','rundll32.exe',PChar('shell32.dll,Control_RunDLL '+s),nil,SW_NORMAL);
end;

procedure ControlApplet(AHandle: THandle; Applet: string = ''); overload;
begin
  if Win32MajorVersion>=6 then
    WinExec(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(Format('control.exe /name "%s"',[Applet]))),SW_SHOWDEFAULT)
  else
    ShellExecute(AHandle,'open','rundll32.exe',PChar('shell32.dll,Control_RunDLL '+ExtractShortPathName(Applet)),nil,SW_NORMAL);
end;

function ShellPropDlg(const Handle: HWND; const FileName: string): Boolean;
var
  Info: TShellExecuteInfo;
begin
  FillChar(Info,SizeOf(Info),#0);
  with Info do begin
    cbSize:=SizeOf(Info);
    lpFile:=PChar(FileName);
    nShow:=SW_SHOW;
    fMask:=SEE_MASK_INVOKEIDLIST;
    Wnd:=Handle;
    lpVerb:=PChar('properties');
  end;
  Result:={$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(@Info);
end;

function RunDlg;
var
  CaptionBuffer: Pointer;
  DescriptionBuffer: Pointer;
begin
  CaptionBuffer:= nil;
  DescriptionBuffer:= nil;
  if (Caption<>'') then
    GetMem(CaptionBuffer,(Length(Caption)+1)*SizeOf(WideChar));
 if (Description<>'') then
   GetMem(DescriptionBuffer,(Length(Description)+1)*SizeOf(WideChar));
 if Assigned(CaptionBuffer) then
   StringToWideChar(Caption,PWideChar(CaptionBuffer),(Length(Caption)+1));
 if Assigned(DescriptionBuffer) then
   StringToWideChar(Description,PWideChar(DescriptionBuffer),(Length(Description)+1));
 Result:=SHRunDialog(Handle,IconHandle,nil,CaptionBuffer,DescriptionBuffer,0)
end;

function FormatDlg;
var
  options: Cardinal;
  Drive: Cardinal;
begin
  if {$IFDEF UNICODE}CharInSet(DriveChar,['a'..'z']){$ELSE}(DriveChar in ['a'..'z']){$ENDIF} then
    DriveChar:=Char(ord(DriveChar)-$20);
  if (not ({$IFDEF UNICODE}CharInSet(DriveChar,['A'..'Z']){$ELSE}DriveChar in ['A'..'Z']{$ENDIF}))then
   raise Exception.Create('Wrong drive char');
  Drive:=Ord(DriveChar)-ord('A');
  case FormatType of
    fmFull: options:=0;
    fmQuick: options:=SHFMT_OPT_FULL;
    else options:=0;
  end;
  Result:=SHFormatDrive(Handle,Drive,SHFMT_ID_DEFAULT,Options);
end;

function FindFilesDlg: Boolean;
begin
  Result:=SHFindFiles(nil,nil);
end;

function FindComputerDlg: Boolean;
begin
  Result:=SHFindComputer(nil,nil);
end;

procedure ShutdownDlg;
begin
  SHShutDownDialog(Wnd,Kind);
end;

{$IFDEF UNICODE}
function GetAveCharSize(Canvas: TCanvas): TPoint;
{$IF DEFINED(CLR)}
var
  I: Integer;
  Buffer: string;
  Size: TSize;
begin
  SetLength(Buffer, 52);
  for I:=0 to 25 do Buffer[I + 1]:=Chr(I + Ord('A'));
  for I:=0 to 25 do Buffer[I + 27]:=Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, Size);
  Result.X:=Size.cx div 52;
  Result.Y:=Size.cy;
end;
{$ELSE}
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I:=0 to 25 do Buffer[I]:=Chr(I + Ord('A'));
  for I:=0 to 25 do Buffer[I + 26]:=Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X:=Result.X div 52;
end;
{$IFEND}

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function InputIntegerQuery(const ACaption, APrompt: string; var Value: Integer): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:=False;
  Form:=TForm.Create(Application);
  with Form do
    try
      Canvas.Font:=Font;
      DialogUnits:=GetAveCharSize(Canvas);
      BorderStyle:=bsDialog;
      Caption:=ACaption;
      ClientWidth:=MulDiv(180, DialogUnits.X, 4);
      PopupMode:=pmAuto;
      Position:=poScreenCenter;
      Prompt:=TLabel.Create(Form);
      with Prompt do begin
        Parent:=Form;
        Caption:=APrompt;
        Left:=MulDiv(8, DialogUnits.X, 4);
        Top:=MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth:=MulDiv(164, DialogUnits.X, 4);
        WordWrap:=True;
      end;
      Edit:=TEdit.Create(Form);
      with Edit do begin
        Parent:=Form;
        Left:=Prompt.Left;
        Top:=Prompt.Top + Prompt.Height + 5;
        Width:=MulDiv(164, DialogUnits.X, 4);
        NumbersOnly:=True;
        Text:=IntToStr(Value);
        SelectAll;
      end;
      ButtonTop:=Edit.Top + Edit.Height + 15;
      ButtonWidth:=MulDiv(50, DialogUnits.X, 4);
      ButtonHeight:=MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgOK;
        ModalResult:=mrOk;
        Default:=True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgCancel;
        ModalResult:=mrCancel;
        Cancel:=True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight:=Top + Height + 13;
      end;
      if ShowModal = mrOk then begin
        Value:=StrToInt(Edit.Text);
        Result:=True;
      end;
    finally
      Form.Free;
    end;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function InputRealQuery(const ACaption, APrompt: string; var Value: Single; ADecimalPlaces: Byte = 2): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:=False;
  Form:=TForm.Create(Application);
  with Form do
    try
      Canvas.Font:=Font;
      DialogUnits:=GetAveCharSize(Canvas);
      BorderStyle:=bsDialog;
      Caption:=ACaption;
      ClientWidth:=MulDiv(180, DialogUnits.X, 4);
      PopupMode:=pmAuto;
      Position:=poScreenCenter;
      Prompt:=TLabel.Create(Form);
      with Prompt do begin
        Parent:=Form;
        Caption:=APrompt;
        Left:=MulDiv(8, DialogUnits.X, 4);
        Top:=MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth:=MulDiv(164, DialogUnits.X, 4);
        WordWrap:=True;
      end;
      Edit:=TEdit.Create(Form);
      with Edit do begin
        Parent:=Form;
        Left:=Prompt.Left;
        Top:=Prompt.Top + Prompt.Height + 5;
        Width:=MulDiv(164, DialogUnits.X, 4);
        Text:=Format('%1.*f',[ADecimalPlaces,Value]);
        SelectAll;
      end;
      ButtonTop:=Edit.Top + Edit.Height + 15;
      ButtonWidth:=MulDiv(50, DialogUnits.X, 4);
      ButtonHeight:=MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgOK;
        ModalResult:=mrOk;
        Default:=True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgCancel;
        ModalResult:=mrCancel;
        Cancel:=True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight:=Top + Height + 13;
      end;
      if ShowModal = mrOk then begin
        Value:=StrToFloat(Edit.Text);
        Result:=True;
      end;
    finally
      Form.Free;
    end;
end;

//[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.SafeSubWindows)]
function InputQueryEx(const ACaption, APrompt: string; AMaxLength: Integer; ACharCase: TEditCharCase; APasswordChar: Char; var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result:=False;
  Form:=TForm.Create(Application);
  with Form do
    try
      Canvas.Font:=Font;
      DialogUnits:=GetAveCharSize(Canvas);
      BorderStyle:=bsDialog;
      Caption:=ACaption;
      ClientWidth:=MulDiv(180, DialogUnits.X, 4);
      PopupMode:=pmAuto;
      Position:=poScreenCenter;
      Prompt:=TLabel.Create(Form);
      with Prompt do begin
        Parent:=Form;
        Caption:=APrompt;
        Left:=MulDiv(8, DialogUnits.X, 4);
        Top:=MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth:=MulDiv(164, DialogUnits.X, 4);
        WordWrap:=True;
      end;
      Edit:=TEdit.Create(Form);
      with Edit do begin
        Parent:=Form;
        Left:=Prompt.Left;
        Top:=Prompt.Top + Prompt.Height + 5;
        Width:=MulDiv(164, DialogUnits.X, 4);
        MaxLength:=AMaxLength;
        CharCase:=ACharCase;
        Text:=Value;
        SelectAll;
        PasswordChar:=APasswordChar;
      end;
      ButtonTop:=Edit.Top + Edit.Height + 15;
      ButtonWidth:=MulDiv(50, DialogUnits.X, 4);
      ButtonHeight:=MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgOK;
        ModalResult:=mrOk;
        Default:=True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do begin
        Parent:=Form;
        Caption:=SMsgDlgCancel;
        ModalResult:=mrCancel;
        Cancel:=True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight:=Top + Height + 13;
      end;
      if ShowModal = mrOk then begin
        Value:=Edit.Text;
        Result:=True;
      end;
    finally
      Form.Free;
    end;
end;
{$ENDIF}

{$ENDIF}
initialization
  MsgBoxLevel:=0;
end.














