{***************************************************************************}
{ TAdvCardList component                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2015                                        }
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

unit AdvCardListRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, Controls, AdvCardList, AdvCardListGradient, DBAdvCardList, Forms, Graphics,
  DesignIntf, DesignEditors
  {$IFDEF TMSPACK}
  , HTMLSDE
  {$ENDIF}
  ;

type

  TAdvCardListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;

  TAdvCardTemplateEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
  end;

  TAdvGradientProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  SysUtils;


procedure TAdvCardListEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;

  if (CompareText(PropName, 'CardTemplate') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvCardTemplateEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;

  if (CompareText(PropName, 'Items') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


{ TAdvGradientProperty }

procedure TAdvGradientProperty.Edit;
var
  ge: TGradientEditor;
begin
  ge := TGradientEditor.Create(Application);
  ge.Color := TAdvGradient(GetOrdValue);

  if ge.ShowModal = mrOk then
    SetOrdValue(Longint(ge.Color));
  ge.Free;
end;

function TAdvGradientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAdvGradient),TCustomAdvCardList,'',TAdvGradientProperty);
  RegisterPropertyEditor(TypeInfo(TAdvGradient),TAdvCardAppearance,'',TAdvGradientProperty);

  {$IFDEF TMSPACK}
  RegisterPropertyEditor(TypeInfo(string), TDBAdvCardTemplateItem, 'HTMLTemplate', THTMLStringProperty);
  {$ENDIF}
end;

end.
