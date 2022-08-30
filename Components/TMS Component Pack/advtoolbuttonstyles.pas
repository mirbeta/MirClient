{***********************************************************************}
{ TAdvToolButton Styler component                                       }
{ for Delphi & C++ Builder                                              }
{ version 1.2.1                                                         }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2002-2005                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvToolButtonStyles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdvToolBtn;

type
  TAdvToolButtonStyleForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetStyle(StyleIndex: Integer; AdvToolButton: TAdvToolButton);
  end;

var
  AdvToolButtonStyleForm: TAdvToolButtonStyleForm;

implementation

{$R *.dfm}

{ TPlanStyleForm }

procedure TAdvToolButtonStyleForm.SetStyle(StyleIndex: Integer;
  AdvToolButton: TAdvToolButton);
begin
  AdvToolButton.Look := StyleIndex;
end;

end.
