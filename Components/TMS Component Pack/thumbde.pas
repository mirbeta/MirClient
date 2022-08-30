{**************************************************************************}
{ TThumbnailList design editor                                             }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3,4,5                            }
{ version 1.1                                                              }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2001                                                       }
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

unit ThumbDE;

interface
{$I TMSDEFS.INC}
uses
  Forms, Windows, Classes, Controls, Dialogs, Extdlgs, ThumbnailList,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
  TThumbPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

{ TAdvPictureProperty }

procedure TThumbPictureProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.jpg|';
  OpenDialog.Filter := OpenDialog.Filter + 'JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|';
  OpenDialog.Filter := OpenDialog.Filter + 'Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';

  if OpenDialog.Execute then
  begin
    SetValue(OpenDialog.FileName);
  end;
  OpenDialog.Free;
end;

function TThumbPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TThumbPictureProperty.GetValue: String;
begin
  if not TIPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TThumbPictureProperty.SetValue(const Value: String);
begin
  inherited;
  if Value <> '' then
    TIPicture(GetOrdValue).LoadFromFile(Value)
  else
  begin
    TIPicture(GetOrdValue).Assign(nil);
  end;
end;

end.
