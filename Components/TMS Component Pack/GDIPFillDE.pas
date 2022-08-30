{**************************************************************************}
{ TGDIPFillDE DESIGN TIME EDITOR                                           }
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

unit GDIPFillDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothFillPreview, AdvSmoothFillEditor, GDIPFill,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TGDIPFillProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  SysUtils, Forms;

{ TFillProperty }

procedure TGDIPFillProperty.Edit;
var
  FillEditor: TAdvSmoothFillEditorForm;
  f: TGDIPFill;
begin
  FillEditor := TAdvSmoothFillEditorForm.Create(Application);
  f := TGDIPFill(GetOrdValue);
  FillEditor.FillPreview := f;
  FillEditor.Init(true);
  try
    FillEditor.ShowModal;
  finally
    Modified;
    FillEditor.Free;
  end;
end;


function TGDIPFillProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties, paMultiSelect];
end;

end.







