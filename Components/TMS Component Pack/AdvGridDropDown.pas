{ ************************************************************************* }
{ TMS TAdvGridDropDown component                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{ copyright ©  2011 - 2015                                                  }
{ Email : info@tmssoftware.com                                              }
{ Web : http://www.tmssoftware.com                                          }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{ ************************************************************************* }

unit AdvGridDropDown;

interface

uses
  Classes, AdvCustomGridDropDown, AdvGrid;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridDropDown = class(TCustomAdvGridDropDown)
  private
    function GetAdvGrid: TAdvStringGrid;
  published
  public
    property Grid: TAdvStringGrid read GetAdvGrid;
  end;

implementation

{ TAdvGridDropDown }

function TAdvGridDropDown.GetAdvGrid: TAdvStringGrid;
begin
  Result := (GetGrid as TAdvStringGrid);
end;

end.
