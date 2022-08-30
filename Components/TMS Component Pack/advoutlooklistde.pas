{*************************************************************************}
{ TMS AdvOutlookList component                                            }
{ for Delphi & C++Builder                                                 }
{ version 1.2                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2005 - 2006                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{$I TMSDEFS.INC}

unit AdvOutLookListDE;

interface

uses
  Classes, Controls, AdvOutLookList
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;



type

  TAdvOutLookListEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;



procedure Register;

implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterComponentEditor(TAdvOutLookList, TAdvOutLookListEditor);
end;

{$IFNDEF DELPHI6_LVL}
procedure TAdvOutLookListEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean); 
{$ELSE}
procedure TAdvOutLookListEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); 
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvOutLookListEditor.ExecuteVerb(Index: integer);
begin
  case Index of
  0: Edit;
  end;
end;

function TAdvOutLookListEditor.GetVerb(index: integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Columns Editor';
  end;
end;

function TAdvOutLookListEditor.GetVerbCount: integer;
begin
 Result := 1;
end;

end.