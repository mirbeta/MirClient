{**************************************************************************}
{ TAdvSmoothLabelGDIPicture design editor                                  }
{ for Delphi & C++Builder                                                  }
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
unit AdvGDIPDe;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls, Dialogs, ExtDlgs, AdvGDIP, Menus,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TGDIPPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

implementation

{ TGDIPPictureProperty }

procedure TGDIPPictureProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.png;*.ico;*tiff)|*.jpg;*.jpeg;*.gif;*.' +
      'bmp;*.png;*.ico|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*' +
      '.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|PNG ' +
      'files (*.png)|*.png|ICO files (*.ico)|*.ico|TIFF files (*.tiff)|*.tiff';

  if Opendialog.Execute then
  begin
    TAdvGDIPPicture(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Modified;
  end;
  OpenDialog.Free;
end;

function TGDIPPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TGDIPPictureProperty.GetValue: String;
begin
  if not TAdvGDIPPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TGDIPPictureProperty.SetValue(const Value: String);
var
  gdip: TAdvGDIPPicture;
begin
  inherited;

  if (Value = '') then  // picture is cleared
  begin
    gdip := TAdvGDIPPicture(GetOrdValue);
    if Assigned(gdip) then
      gdip.Assign(nil);
  end;
end;

end.
