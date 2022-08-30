{********************************************************************}
{ TCALCOMP component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by TMS Software                                            }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit calcregde;

interface
{$I TMSDEFS.INC}
uses
  Calcomp, Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors

{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type

  TCalcompEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TCalcomp,TCalcompEditor);
end;

procedure TCalcompEditor.ExecuteVerb(Index: Integer);
begin
  (Component as TCalComp).Execute;
end;

function TCalcompEditor.GetVerb(Index: Integer): string;
begin
  Result := '&Test';
end;

function TCalcompEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

