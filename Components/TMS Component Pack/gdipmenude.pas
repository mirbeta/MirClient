{**************************************************************************}
{ TGDIPMenuDE DESIGN TIME EDITOR                                           }
{ for Delphi & C++Builder                                                  }
{ version 1.0                                                              }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2007                                              }
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

unit GDIPMenuDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothMegaMenuEditor, GDIPMenu,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TGDIPMenuProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  SysUtils, Forms;

{ TFillProperty }

procedure TGDIPMenuProperty.Edit;
var
  FillEditor: TAdvSmoothMegaMenuEditorForm;
  m: TGDIPMenu;
begin
  FillEditor := TAdvSmoothMegaMenuEditorForm.Create(Application);
  m := TGDIPMenu(GetOrdValue);
  FillEditor.MenuPreview := m;
  FillEditor.Init(true);
  try
    FillEditor.ShowModal;
  finally
    Modified;
    FillEditor.Free;
  end;
end;


function TGDIPMenuProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.







