unit seldlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tselectdialog = class(TForm)
    tablelist: TListBox;
    OK: TButton;
    Cancel: TButton;
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  selectdialog: Tselectdialog;

implementation

{$R *.DFM}

procedure Tselectdialog.OKClick(Sender: TObject);
begin
 modalresult:=mrOk;
end;

procedure Tselectdialog.CancelClick(Sender: TObject);
begin
 modalresult:=mrCancel;
end;

end.
