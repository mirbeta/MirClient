{********************************************************************}
{ SHELLDLG components                                                }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2010                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit ushldlg;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, shellapi, forms;

const
  SHFD_CAPACITY_DEFAULT = 0;    // default drive capacity
  SHFD_CAPACITY_360 = 3;        // 360KB, applies to 5.25" drives only
  SHFD_CAPACITY_720 = 5;        // 720KB, applies to 3.5" drives only

  SHFD_FORMAT_FULL_NT = 0;      // full format
  SHFD_FORMAT_QUICK_NT = 1;     // quick format

  SHFD_FORMAT_QUICK_95 = 0;     // quick format
  SHFD_FORMAT_FULL_95 = 1;      // full format
  SHFD_FORMAT_SYSONLY_95 = 2;   // copies system files only

type
 TFormatType = (fmQuick,fmFull, fmSysOnly);
 TFormatCapacity = (fcDefault,fc360k,fc720k);

 {$IFDEF DELPHIXE2_LVL}
 [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
 {$ENDIF}
 TRunDialog = class(TComponent)
 private
   FTitle:string;
   FPrompt:string;
   FShowLast:boolean;
 public
   function Execute:integer;
 published
   property Title:string read FTitle write FTitle;
   property Prompt:string read FPrompt write FPrompt;
   property ShowLastPrompt:boolean read FShowLast write FShowLast;
 end;

 {$IFDEF DELPHIXE2_LVL}
 [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
 {$ENDIF}
 TShutDownDialog = class(TComponent)
 private
   FDefaultShutDown:integer;
 public
   function Execute:integer;
 published
   property DefaultShutDown:integer read FDefaultShutDown write FDefaultShutDown;
 end;

 {$IFDEF DELPHIXE2_LVL}
 [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
 {$ENDIF}
 TChangeIconDialog = class(TComponent)
 private
   Ffilename:string;
   FIdx:integer;
   function GetIconHandle:thandle;
 public
   function Execute:integer;
   property IconHandle:tHandle read GetIconHandle;
 published
   property FileName:string read Ffilename write Ffilename;
   property IconIndex:integer read FIdx write FIdx;
 end;

 {$IFDEF DELPHIXE2_LVL}
 [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
 {$ENDIF}
 TFormatDialog = class(TComponent)
 private
   FDrive:integer;
   FCapacity:TFormatCapacity;
   FType:TFormatType;
 public
   function Execute:integer;
 published
   property Drive:integer read FDrive write FDrive;
   property Capacity:TFormatCapacity read FCapacity write FCapacity;
   property FormatType:TFormatType read FType write FType;
 end;

 TPropertyDialog = class(TComponent)
 private
   FFileName:string;
 public
   function Execute:integer;
 published
   property FileName:string read FFileName write FFileName;
 end;


implementation

{$WARNINGS OFF}

function SHShutDownDialog(YourGuess:integer):longint; stdcall; external 'shell32.dll' index 60;
function SHRunDialog(hOwner:thandle;Unknown1:integer;Unknown2:pointer;szTitle,szPrompt:pwidechar;uiFlags:integer):integer; stdcall; external 'shell32.dll' index 61;
function SHFormatDrive(hOwner:thandle;iDrive,iCapacity,iFormatType:integer):integer; stdcall; external 'shell32.dll' name 'SHFormatDrive';
function SHChangeIconDialog(hOwner:thandle;szFileName:pchar;reserved:integer;var lpIconIndex:integer):integer; stdcall; external 'shell32.dll' index 62;
(*
function SHShutDownDialog; external 'shell32.dll' index 60;
function SHRunDialog; external 'shell32.dll' index 61;
function SHChangeIconDialog; external 'shell32.dll' index 62;
function SHFormatDrive; external 'shell32.dll' name 'SHFormatDrive';
*)
{$WARNINGS ON}

function TRunDialog.Execute;
const
  ShowL:array[boolean] of integer = (2,0);
var
  szWTitle,szWPrompt:array[0..255] of widechar;
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) then
  begin
    StringToWideChar(FTitle,szWTitle,sizeof(szWTitle));
    StringToWideChar(FPrompt,szWPrompt,sizeof(szWPrompt));
  end
  else
  begin
    strpcopy(pchar(@szWTitle),FTitle);
    strpcopy(pchar(@szWPrompt),FPrompt);
  end;
  result:=SHRunDialog((owner as twincontrol).handle,0,nil,szWTitle,szWPrompt,ShowL[FShowLast]);
end;

function TShutDownDialog.Execute;
begin
 result:=SHShutDownDialog(FDefaultShutdown);
end;

function TChangeIconDialog.Execute;
var
 szFilenameW:array[0..259] of widechar;
 szFileName:array[0..259] of char;
 verinfo:tosversioninfo;

begin
 verinfo.dwOSVersionInfoSize:=sizeof(tosversioninfo);
 getversionex(verinfo);

 if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) then
  begin
   StringToWideChar(fFilename,szFileNameW,sizeof(szFileNameW));
   result:=SHChangeIconDialog((owner as twincontrol).handle,pchar(@szFileNameW),0,FIdx);
   if (result<>0) then
     begin
      WideCharToString(szFileNameW);
      result:=idOk;
     end;
  end
 else
  begin
   strpcopy(szFileName,fFileName);
   result:=SHChangeIconDialog((owner as twincontrol).handle,szFileName,0,FIdx);
   if (result<>0) then
    begin
     fFileName:=strpas(szFileName);
     result:=idOk;
   end;
  end;
end;

function TChangeIconDialog.GetIconHandle;
begin
 result:=extracticon(application.handle,pchar(fFilename),fidx);
end;

function TFormatDialog.Execute;
const
 Capac:array[TFormatCapacity] of integer = (0,3,5);
 Type95:array[TFormatType] of integer = (SHFD_FORMAT_QUICK_95,SHFD_FORMAT_FULL_95,SHFD_FORMAT_SYSONLY_95);
 TypeNT:array[TFormatType] of integer = (SHFD_FORMAT_FULL_NT,SHFD_FORMAT_QUICK_NT,SHFD_FORMAT_QUICK_NT);
var
 osv:TOSVersionInfo;

begin
 osv.dwOSVersionInfoSize :=Sizeof(osv);
 GetVersionEx(osv);
 if (osv.dwPlatformId = VER_PLATFORM_WIN32_NT) then
   result:=SHFormatDrive((owner as twincontrol).handle,FDrive,Capac[FCapacity],TypeNT[FType])
 else
   result:=SHFormatDrive((owner as twincontrol).handle,FDrive,Capac[FCapacity],Type95[FType]);
end;



{ TPropertyDialog }

function TPropertyDialog.Execute: integer;
var
  sei:ShellExecuteInfo;
begin
  Result := 0;
  with sei do
  begin
    fillchar(sei,sizeof(sei),0);
    cbSize := sizeof(sei);
    fMask := SEE_MASK_NOCLOSEPROCESS Or SEE_MASK_INVOKEIDLIST Or SEE_MASK_FLAG_NO_UI;
    wnd := (owner as twincontrol).handle;
    lpVerb := 'properties';
    lpFile := pchar(FFilename);
  end;
  if  ShellExecuteEX(@SEI) then SysErrorMessage(GetLastError);
end;

end.
