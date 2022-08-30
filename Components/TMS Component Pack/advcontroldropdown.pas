{***************************************************************************}
{ TAdvControlDropdown components                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2013                                        }
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

{$I TMSDEFS.INC}

unit AdvControlDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, AdvDropDown;

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvControlDropDown = class(TAdvDropDown)
  private
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Control;
  end;

implementation

//------------------------------------------------------------------------------

{ TAdvControlDropDown }

constructor TAdvControlDropDown.Create(AOwner: TComponent);
begin
  inherited;

end;

//------------------------------------------------------------------------------

destructor TAdvControlDropDown.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvControlDropDown.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    SetCenterControl;
end;

//------------------------------------------------------------------------------

end.
