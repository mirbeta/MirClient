{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvMemoAC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, Buttons;

type
  TMemoAC = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ckDoAutoCorrect: TCheckBox;
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoAC: TMemoAC;

implementation

{$R *.dfm}


type
   TSGCracker = class(TStringGrid);


procedure TMemoAC.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    StringGrid1.RowCount := StringGrid1.RowCount + 1;

  if (Key = VK_DELETE) and not StringGrid1.EditorMode then
  begin
    if StringGrid1.RowCount > 2 then
      TSGCracker(StringGrid1).DeleteRow(StringGrid1.Row)
    else
    begin
      StringGrid1.Cells[1,1] := '';
      StringGrid1.Cells[2,1] := '';
    end;
  end;
end;

procedure TMemoAC.SpeedButton1Click(Sender: TObject);
begin
  StringGrid1.RowCount := StringGrid1.RowCount + 1;

end;

procedure TMemoAC.SpeedButton2Click(Sender: TObject);
begin
  if not StringGrid1.EditorMode then
  begin
    if StringGrid1.RowCount > 2 then
      TSGCracker(StringGrid1).DeleteRow(StringGrid1.Row)
    else
    begin
      StringGrid1.Cells[1,1] := '';
      StringGrid1.Cells[2,1] := '';
    end;
  end;
end;

procedure TMemoAC.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0,0] := 'Incorrect text';
  StringGrid1.Cells[1,0] := 'Correct text';
end;

end.
