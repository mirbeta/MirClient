{***************************************************************************}
{ TAdvGlowButton component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2008 - 2012                                        }
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

unit DBAdvGlowNavigatorDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, DBAdvGlowNavigator, ExtDlgs, AdvStyleIF
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type

  TDBAdvGlowNavigatorEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;


implementation

uses
  SysUtils;


{ TDBAdvGlowNavigatorEditor }

procedure TDBAdvGlowNavigatorEditor.ExecuteVerb(Index: Integer);
begin
  inherited;

  if (Component is TDBAdvGlowNavigator) then
  begin
     if (Index in [0..7]) or (Index in [9..15]) then
     begin
      (Component as TDBAdvGlowNavigator).Style := TTMSStyle(Index);
      (Component as TDBAdvGlowNavigator).Repaint;
      Designer.Modified;
    end;
  end;
end;

function TDBAdvGlowNavigatorEditor.GetVerb(Index: Integer): string;
begin
  if (Component is TDBAdvGlowNavigator) then
  begin
    //tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsOffice2003Classic,
    //tsOffice2007Luna, tsOffice2007Obsidian, tsWindowsXP, tsWhidbey
    case Index of
    0: Result := 'Office 2003 Blue';
    1: Result := 'Office 2003 Silver';
    2: Result := 'Office 2003 Olive';
    3: Result := 'Office 2003 Classic';
    4: Result := 'Office 2007 Luna';
    5: Result := 'Office 2007 Obsidian';
    6: Result := 'Windows XP';
    7: Result := 'Whidbey';
    8: Result := 'Custom';
    9: Result := 'Office 2007 Silver';
    10: Result := 'Windows Vista';
    11: Result := 'Windows 7';
    12: Result := 'Terminal';
    13: Result := 'Office 2010 Blue';
    14: Result := 'Office 2010 Silver';
    15: Result := 'Office 2010 Black';
    end;
  end;
end;

function TDBAdvGlowNavigatorEditor.GetVerbCount: Integer;
begin
  Result := 16;
end;

procedure Register;
begin
  RegisterComponentEditor(TDBAdvGlowNavigator,TDBAdvGlowNavigatorEditor);
end;


end.
