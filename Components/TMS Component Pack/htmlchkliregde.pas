{***************************************************************************}
{ THTMListbox component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ Copyright © 2001 - 2012                                                   }
{   TMS Software                                                            }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
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

unit htmlchkliregde;

interface

uses
  Classes, HtmlChkList, ColorDlg, Dialogs, Forms, Controls, AdvGradient,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TGradientColorProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;


procedure Register;

implementation

procedure Register;
begin
  //RegisterPropertyEditor(TypeInfo(TGradientStyle),THTMLCheckList,'SelectionColors',TGradientColorProperty);
end;

function TGradientColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TGradientColorProperty.Edit;
var
  ColorChooser: TColorChooser;
  HL: THTMLCheckList;
begin
  ColorChooser := TColorChooser.Create(Application);

  HL := THTMLCheckList(GetComponent(0));

  ColorChooser.ColorFrom := HL.SelectionColors.ColorFrom;
  ColorChooser.ColorTo := HL.SelectionColors.ColorTo;
  ColorChooser.BorderColor := HL.SelectionColors.BorderColor;
  ColorChooser.Direction := HL.SelectionColors.Direction;

  if ColorChooser.ShowModal = mrOk then
  begin
    HL.SelectionColors.ColorFrom := ColorChooser.ColorFrom;
    HL.SelectionColors.ColorTo := ColorChooser.ColorTo;
    HL.SelectionColors.BorderColor := ColorChooser.BorderColor;
    HL.SelectionColors.Direction := ColorChooser.Direction;
  end;

  Modified;

  ColorChooser.Free;
end;


end.

