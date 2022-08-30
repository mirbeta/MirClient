unit uStringsEdt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TStringEdt = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
  private
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    { Private declarations }
  public
    { Public declarations }
    property Lines: TStrings read GetLines write SetLines;
  end;

  PStringsData = ^ TStringsData;
  TStringsData = record
    Caption: String;
    Lines: String;
  end;

function ShowStringsEditor(StringsData: PStringsData): boolean;

var
  StringEdt: TStringEdt;

implementation

{$R *.dfm}

function ShowStringsEditor(StringsData: PStringsData): boolean;
begin
  with TStringEdt.Create(nil) do
  begin
    Caption     :=  StringsData^.Caption;
    Lines.Text  :=  StringsData^.Lines;
    Result      :=  ShowModal=mrOk;
    if Result then
      StringsData^.Lines  :=  Lines.Text;
    Free;
  end;
end;

{ TStringEdt }

function TStringEdt.GetLines: TStrings;
begin
  Result  :=  Memo1.Lines;
end;

procedure TStringEdt.SetLines(const Value: TStrings);
begin
  Memo1.Lines.Assign(Value);
end;

end.
