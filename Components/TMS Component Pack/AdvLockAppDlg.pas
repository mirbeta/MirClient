{***************************************************************************}
{ TAdvLockApp component                                                     }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               }
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

unit AdvLockAppDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask;

type
  TAdvUnlockForm = class(TForm)
    lbl_usr: TLabel;
    lbl_pwd: TLabel;
    lb_Username: TLabel;
    me_Password: TMaskEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor CreateWindow(AOwner: TComponent; UserName: string); overload;
  end;

var
  AdvUnLockForm: TAdvUnlockForm;

implementation

{$R *.dfm}

constructor TAdvUnlockForm.CreateWindow(AOwner: TComponent; UserName: string);
begin
  inherited Create(AOwner);
  lb_Username.Caption := UserName;
end;

end.
