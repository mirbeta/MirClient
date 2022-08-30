{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{ version 2.2                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright �  2005 - 2006                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvOfficePagerReg;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvOfficePager, AdvOfficePagerStylers;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvOfficePager, TAdvOfficePagerOfficeStyler
                                   {, TAdvOfficePagerFantasyStyler}]);
end;

end.

