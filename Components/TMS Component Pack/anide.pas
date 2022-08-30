{**************************************************************************}
{ ANIICON design editor                                                    }
{ for Delphi 2.0,3.0,4.0,5.0,6.0 & C++Builder 1,3,4,5,6                    }
{ version 2.0                                                              }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 1998-2001                                                  }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit anide;

interface
{$I TMSDEFS.INC}
uses
  AniIcon, AniEdit, Forms, Windows, Classes, Controls,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
  TAniProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

function TAniProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAniProperty.Edit;
begin
  AniEditor:=tAniEditor.Create(Application);
  try
    if AniEditor.Showmodal=mrOK then
    begin
      if AniEditor.FileName <> '' then
        SetValue(AniEditor.FileName);
    end;
  finally
    AniEditor.Free;
  end;
end;

procedure TAniProperty.SetValue(const Value: String);
var
  i:integer;
begin
  if value <> '' then
    TAniIconFile(GetOrdValue).LoadFromFile(Value)
  else
  begin
    TAniIconFile(GetOrdValue).Assign(nil);
    //repaint parent component
    for i := 0 to PropCount-1 do
      (GetComponent(i) as TAniIcon).Repaint;
  end;
  Modified;
end;

function TAniProperty.GetValue: String;
begin
  if TAniIconFile(GetOrdValue).HasData then
    Result := '(TAniIconFile)'
  else
    Result := '(None)';
end;


end.
 