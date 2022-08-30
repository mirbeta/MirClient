{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
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

unit TMSW7Reg;

interface

uses
  Classes,  W7Bars, W7Buttons, W7Images, W7Labels, W7Panels, W7ListViewItems, W7ProgressBars, W7NaviButtons;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS W7',[TW7ToolBar, TW7InformationBar, TW7ToolButton, TW7PageSelector, TW7SpeedButton, TW7Image, TW7ActiveLabel, TW7ListViewItem, TW7TaskItem, TW7NavigationButton, TW7NavigationFrame, TW7LeftPanel, TW7CaptionPanel, TW7Panel, TW7ProgressBar]);
end;

end.

