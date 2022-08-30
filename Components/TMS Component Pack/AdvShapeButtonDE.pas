{**************************************************************************}
{ TAdvShapeButtonDE DESIGN TIME EDITOR                                     }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2009                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvShapeButtonDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvShapeButton,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TAdvShapeButtonEditor = class(TDefaultEditor)
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

implementation

uses
  Forms, Dialogs;

procedure TAdvShapeButtonEditor.ExecuteVerb(Index: integer);
var
  col: TColorDialog;
begin
  inherited;
  if (Index = 0) then
  begin
    if (Component is TAdvShapeButton) then
    begin
      col := TColorDialog.Create(Application);
      col.Color := (Component as TAdvShapeButton).Appearance.Color;
      if col.Execute then
      begin
        (Component as TAdvShapeButton).UseGlobalColor := true;
        (Component as TAdvShapeButton).Appearance.UpdateButtonColor(col.Color);
        (Component as TAdvShapeButton).UseGlobalColor := false;
      end;
      col.Free;
      Designer.Modified;
    end;
  end;
end;

function TAdvShapeButtonEditor.GetVerb(index: integer): string;
begin
  if index = 0 then
    Result := 'Change Color';
end;

function TAdvShapeButtonEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

end.







