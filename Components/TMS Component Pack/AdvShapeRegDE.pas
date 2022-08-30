{********************************************************************}
{ AdvShapeRegDE components                                           }
{ for Delphi & C++Builder                                            }
{ version 1.0                                                        }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2007                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit AdvShapeRegDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvShape, htmlde,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
 {$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TAdvShapeEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
  end;


procedure Register;

implementation

uses
  SysUtils;

{$IFDEF DELPHI6_LVL}
procedure TAdvShapeEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
{$ELSE}
procedure TAdvShapeEditor.EditProperty(PropertyEditor: TPropertyEditor;
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
  if (CompareText(PropName, 'Text') = 0) then
  begin
{$IFDEF DELPHI6_LVL}
    Prop.Edit;
{$ELSE}
    PropertyEditor.Edit;
{$ENDIF}
    Continue := False;
  end;
end;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TAdvShape, 'Text', THTMLStringProperty);
  RegisterComponentEditor(TAdvShape,TAdvShapeEditor);
end;



end.

