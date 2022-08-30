{***************************************************************************}
{ TMS Spell Check component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit TMSSpellCheckRegDE;

interface

uses
  SysUtils, Classes, ToolsAPI, DesignIntf, DesignEditors, DesignMenus,
  DesignWindows, TMSSpellCheck;

type
  TSpellCheckerEditor = class(TComponentEditor)
  private
    FComponent: TComponent;
    FDesigner: IDesigner;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;

    property Component: TComponent read FComponent;
    property Designer: IDesigner read FDesigner;
  end;

procedure Register;

implementation

constructor TSpellCheckerEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited;
  FComponent := AComponent;
  FDesigner := ADesigner;
end;

procedure TSpellCheckerEditor.Edit;
begin
  TAdvSpellCheck(FComponent).OpenSettingsDialog;
end;

procedure TSpellCheckerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
  0: TAdvSpellCheck(FComponent).OpenSettingsDialog;
  end;
end;

function TSpellCheckerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TSpellCheckerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := '&SpellChecker settings';
  else
    raise ENotImplemented.Create
      ('TSpellChecker has only one verb (index = 0) supported.');
  end;
end;

procedure Register;
begin
  RegisterComponentEditor(TAdvSpellCheck, TSpellCheckerEditor);
end;

end.
