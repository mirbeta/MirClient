{***************************************************************************}
{ TAdvAlertWindow component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2015                                        }
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
{$I TMSDEFS.INC}

unit AdvAlertWindowRegDE;

interface

uses
  Classes, AdvAlertWindow, SysUtils, DesignIntf, DesignEditors;
  
type
  TAdvAlertWindowEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const Prop: IProperty; var Continue:Boolean); override;
  {$ENDIF}
 
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvAlertWindow,TAdvAlertWindowEditor);
end;

{ TAdvAlertWindowEditor }

{$IFDEF DELPHI6_LVL}
procedure TAdvAlertWindowEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
{$ELSE}
procedure TAdvAlertWindowEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
{$IFDEF DELPHI6_LVL}
  PropName := Prop.GetName;
{$ELSE}
  PropName := PropertyEditor.GetName;
{$ENDIF}
  if (CompareText(PropName, 'AlertMessages') = 0) then
  begin
{$IFDEF DELPHI6_LVL}
    Prop.Edit;
{$ELSE}
    PropertyEditor.Edit;
{$ENDIF}
    Continue := False;
  end;
end;


procedure TAdvAlertWindowEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0..3:
    begin
      TAdvAlertWindow(Component).Style := TAdvAlertWindowStyle(Index);
      Designer.Modified;
    end;
  end;
end;

function TAdvAlertWindowEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Outlook 2003 Blue look';
    1: Result := 'Outlook 2003 Silver look';
    2: Result := 'Outlook 2003 Olive look';
    3: Result := 'Outlook 2003 Classic look';
  end;
end;

function TAdvAlertWindowEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;


end.
