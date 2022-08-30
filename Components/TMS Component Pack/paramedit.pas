{**************************************************************************}
{ Parameter controls property editor                                       }
{                                                                          }
{ Copyright © 2001 - 2012                                                  }
{  TMS Software                                                            }
{  Email : info@tmssoftware.com                                            }
{  Web : http://www.tmssoftware.com                                        }
{                                                                          }
{**************************************************************************}

unit paramedit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TParamInfo = class(TForm)
    Label1: TLabel;
    pid: TEdit;
    Label2: TLabel;
    pval: TEdit;
    Label3: TLabel;
    phint: TEdit;
    Label4: TLabel;
    ptype: TComboBox;
    Label5: TLabel;
    pextra: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ParamInfo: TParamInfo;

implementation

{$R *.dfm}

end.
