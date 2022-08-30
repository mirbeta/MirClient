{**************************************************************************}
{ TGDIPPictureContainerDE DESIGN TIME EDITOR                               }
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

unit GDIPPictureContainerDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, GDIPPictureContainerEditor, GDIPPictureContainer,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TGDIPPictureContainerPropEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue:
Boolean); override;
  {$ENDIF}
  end;

  TGDIPPictureContainerProp = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  SysUtils, Forms;

{ TFillProperty }

procedure TGDIPPictureContainerProp.Edit;
var
  GDIPPicEdit: TGDIPPictureContainerEditor;
  p: TPictureCollection;
begin
  GDIPPicEdit := TGDIPPictureContainerEditor.Create(Application);
  try
    p := TPictureCollection(GetOrdValue);
    GDIPPicEdit.PictureCol := p;
    GDIPPicEdit.Init;
    GDIPPicEdit.ShowModal;
  finally
    Modified;
    GDIPPicEdit.Free;
  end;
end;

{$IFDEF DELPHI6_LVL}
procedure TGDIPPictureContainerPropEditor.EditProperty(const PropertyEditor:
IProperty; var Continue: Boolean);
{$ELSE}
procedure TGDIPPictureContainerPropEditor.EditProperty(PropertyEditor:
TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

function TGDIPPictureContainerProp.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.







