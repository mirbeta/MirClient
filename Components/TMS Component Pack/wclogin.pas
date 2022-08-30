{*******************************************************************}
{ TWebCopy component                                                }
{ for Delphi & C++Builder                                           }
{ version 1.5                                                       }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1998-2005                                          }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WcLogin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TWcLoginForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetLogin(Prompt:string;var UserName, Password: string): boolean;

var
  WcLoginForm: TWcLoginForm;

implementation

{$R *.dfm}

function GetLogin(Prompt: string; var UserName, Password: string): boolean;
var
  lf: TWcLoginForm;
begin
  Result := false;
  lf := TWcLoginForm.Create(Application);
  try
    lf.Caption := Prompt;
    if lf.ShowModal = mrOk then
    begin
      UserName := lf.edit1.Text;
      Password := lf.edit2.Text;
      Result := true;
    end;
  finally
    lf.Free;
  end;
end;

end.
