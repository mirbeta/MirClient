{***************************************************************************}
{ TMS Spell Check component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit TMSSpellUpdateDictProgForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Graphics,
  Forms, Dialogs, StdCtrls, ComCtrls;

type
  TUpdateDictProgFrm = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UpdateDictProgFrm: TUpdateDictProgFrm;

implementation

{$R *.dfm}

end.
