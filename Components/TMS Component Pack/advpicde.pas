{**************************************************************************}
{ TAdvPicture design editor                                                }
{ for Delphi 5.0,6.0,7.0,2005,2006 & C++Builder 5,6,2006                   }
{ version 1.3                                                              }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2001 - 2006                                                }
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

unit advpicde;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls, Dialogs, ExtDlgs, AdvPicture, Menus,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TAdvPictureProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

implementation

{ TAdvPictureProperty }

procedure TAdvPictureProperty.Edit;
var
  opendialog: TOpenPictureDialog;
begin
  inherited;
  opendialog := TOpenPictureDialog.Create(nil);

  opendialog.Filter := 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|';
  opendialog.Filter := opendialog.Filter+'Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';

  if Opendialog.Execute then
  begin
    TIPicture(GetOrdValue).LoadFromFile(Opendialog.FileName);
    Modified;
  end;
  opendialog.Free;
end;

function TAdvPictureProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TAdvPictureProperty.GetValue: String;
begin
  if not TIPicture(GetOrdValue).Empty then
    Result := '(TPicture)'
  else
    Result := '(None)';
end;

procedure TAdvPictureProperty.SetValue(const Value: String);
var
  ipic: TIPicture;
begin
  inherited;
  if Value = '' then
  begin
    ipic := TIPicture(GetOrdValue);
    if Assigned(ipic) then
      ipic.Assign(nil);
  end;
end;

end.
