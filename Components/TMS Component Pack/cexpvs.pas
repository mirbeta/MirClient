{***************************************************************************}
{ XPTheme interface                                                         }
{ for Delphi & C++Builder                                                   }
{ version 1.0                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2006                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit CEXPVS;

interface

uses
  Windows;

const
//---------------------------------------------------------------------------------------
//   "ComboBox" Parts & States
//---------------------------------------------------------------------------------------

  CP_DROPDOWNBUTTON = 1;
  {$EXTERNALSYM CP_DROPDOWNBUTTON}

  CBXS_NORMAL = 1;
  {$EXTERNALSYM CBXS_NORMAL}

  CBXS_HOT = 2;
  {$EXTERNALSYM CBXS_HOT}

  CBXS_PRESSED = 3;
  {$EXTERNALSYM CBXS_PRESSED}

  CBXS_DISABLED = 4;
  {$EXTERNALSYM CBXS_DISABLED}

type
  HTHEME = THandle;
  {$EXTERNALSYM HTHEME}

var
  OpenThemeData: function(hwnd: THandle; pszClassList: PWideChar): HTheme cdecl stdcall;
  {$EXTERNALSYM OpenThemeData}

  CloseThemeData: function(hTheme: HTHEME): THandle cdecl stdcall;
  {$EXTERNALSYM CloseThemeData}

  DrawThemeBackground: function(hTheme: HTHEME;
                                hdc: HDC;
                                iPartId: Integer;
                                iStateId: Integer;
                                const pRect: PRECT;
                                const pClipRect: PRECT): THandle cdecl stdcall;
  {$EXTERNALSYM DrawThemeBackground}

  IsThemeActive: function: BOOL cdecl stdcall;
  {$EXTERNALSYM IsThemeActive}

implementation

var
  DLLLoaded: Boolean = False;
  DLLHandle: THandle;

procedure UnLoadDLL;
begin
  if DLLLoaded then
  begin
    FreeLibrary(DLLHandle);
    DLLLoaded := false;
  end;
end;

procedure LoadDLL;
begin
  if DLLLoaded then Exit;

  DLLHandle := LoadLibrary('UXTHEME.DLL');
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;

    @OpenThemeData := GetProcAddress(DLLHandle,'OpenThemeData');
    Assert(@OpenThemeData <> nil);

    @CloseThemeData := GetProcAddress(DLLHandle,'CloseThemeData');
    Assert(@CloseThemeData <> nil);

    @DrawThemeBackground := GetProcAddress(DLLHandle,'DrawThemeBackground');
    Assert(@DrawThemeBackground <> nil);

    @IsThemeActive := GetProcAddress(DLLHandle,'IsThemeActive');
    Assert(@IsThemeActive <> nil);
  end
  else
  begin
    DLLLoaded := False;
    { Error: UXTHEME.DLL could not be loaded !! }
  end;

end;

initialization
  LoadDLL;

finalization
  UnLoadDLL;

end.
