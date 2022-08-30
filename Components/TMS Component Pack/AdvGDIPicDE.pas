{**************************************************************************}
{ TAdvGDIPPicture design editor                                            }
{ for Delphi & C++Builder                                                  }
{ version 1.0                                                              }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2009                                                       }
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

unit AdvGDIPicDE;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls, Dialogs, ExtDlgs, AdvGDIPicture, GDIPicture, Menus,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
  TAdvGDIPPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

implementation

{ TAdvGDIPPictureProperty }

procedure TAdvGDIPPictureProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.png)|*.jpg;*.jpeg;*.gif;*.bmp;*.png|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|PNG files (*.png)|*.png';

  if opendialog.Execute then
  begin
    TGDIPPicture(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Modified;
  end;
  OpenDialog.Free;
end;

function TAdvGDIPPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TAdvGDIPPictureProperty.GetValue: String;
begin
  if not TGDIPPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TAdvGDIPPictureProperty.SetValue(const Value: String);
begin
  inherited;
end;

end.
