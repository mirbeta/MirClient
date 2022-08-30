{********************************************************************}
{ TPICKDLG component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}
unit pdlgregde;

interface
{$I TMSDEFS.INC}
uses
  Pickdlg, Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TPickDialogEditor = class(TComponentEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

                     
procedure Register;

implementation

{ TPickDialogEditor }

procedure TPickDialogEditor.ExecuteVerb(Index: integer);
begin
  (Component as TPickDialog).Execute;
end;

function TPickDialogEditor.GetVerb(index: integer): string;
begin
  Result := '&Show dialog';
end;

function TPickDialogEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

procedure Register;
begin

  RegisterComponentEditor(TPickDialog,TPickDialogEditor);
end;

end.

