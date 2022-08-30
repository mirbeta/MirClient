{*************************************************************************}
{ TWinXP component                                                        }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2001 - 2011                                      }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit WinXP;

interface

{$R WINXP.RES}

uses
  Windows, Messages, SysUtils, Classes, Forms;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWinXP = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS System', [TWinXP]);
end;

{ TWinXP }

constructor TWinXP.Create(AOwner: TComponent);
var
  I,Instances:Integer;

begin
  inherited Create(AOwner);
  if not (AOwner is TForm) then
    raise Exception.Create('Control parent must be a form!');

  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is TWinXP) then Inc(Instances);
  if (Instances > 1) then
    raise Exception.Create('Only one instance of TWinXP allowed on form');
end;



end.
 