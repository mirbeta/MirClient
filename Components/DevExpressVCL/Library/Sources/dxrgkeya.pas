{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Registry path property editor                     }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRgKeya;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmdxAddNewRegKey = class(TForm)
    Edit: TEdit;
    lblName: TLabel;
    bOk: TButton;
    bCancel: TButton;
    procedure EditChange(Sender: TObject);
  private
  public
  end;

function dxGetNewRegistryKey : String;

implementation

{$R *.DFM}
function dxGetNewRegistryKey : String;
var
  AForm : TfrmdxAddNewRegKey;
begin
  AForm := TfrmdxAddNewRegKey.Create(nil);
  AForm.ShowModal;
  if(AForm.ModalResult = mrOK) then
    Result := AForm.Edit.Text
  else Result := '';
  AForm.Free;
end;

procedure TfrmdxAddNewRegKey.EditChange(Sender: TObject);
begin
  bOk.Enabled := Edit.Text <> '';
end;

end.
