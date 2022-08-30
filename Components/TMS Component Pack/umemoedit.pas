unit uMemoEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AdvMemo, StdCtrls, ExtCtrls, Menus, Buttons, ExtDlgs, Printers;

type
  TTMSMemoEdit = class(TForm)
    AdvMemo1: TAdvMemo;
    Panel1: TPanel;
    spbCut: TSpeedButton;
    spbPaste: TSpeedButton;
    spbSave: TSpeedButton;
    spbOpen: TSpeedButton;
    spbCopy: TSpeedButton;
    spbUndo: TSpeedButton;
    spbRedo: TSpeedButton;
    spbNew: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Func: TLabel;
    procedure spbCutClick(Sender: TObject);
    procedure spbPasteClick(Sender: TObject);
    procedure spbUndoClick(Sender: TObject);
    procedure spbRedoClick(Sender: TObject);
    procedure spbCopyClick(Sender: TObject);
    procedure spbNewClick(Sender: TObject);
    procedure spbOpenClick(Sender: TObject);
    procedure spbSaveClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TMSMemoEdit: TTMSMemoEdit;

implementation


{$R *.DFM}

procedure TTMSMemoEdit.spbCutClick(Sender: TObject);
begin
  AdvMemo1.CutToClipBoard;
end;

procedure TTMSMemoEdit.spbPasteClick(Sender: TObject);
begin
  AdvMemo1.PasteFromClipBoard;
end;

procedure TTMSMemoEdit.spbUndoClick(Sender: TObject);
begin
  AdvMemo1.Undo;
end;

procedure TTMSMemoEdit.spbRedoClick(Sender: TObject);
begin
  AdvMemo1.Redo;
end;

procedure TTMSMemoEdit.spbCopyClick(Sender: TObject);
begin
  AdvMemo1.CopyToClipBoard;
end;

procedure TTMSMemoEdit.spbNewClick(Sender: TObject);
begin
  AdvMemo1.Clear;
end;

procedure TTMSMemoEdit.spbOpenClick(Sender: TObject);
var
  filename:string;
  extension:string;
  length_filename:Integer;

begin
  if OpenDialog1.Execute then
  begin
    filename := OpenDialog1.FileName;
    AdvMemo1.Lines.LoadFromFile(filename);
    //determine the extension of the filename
    length_filename := Length(filename);
    extension := Copy(filename,length_filename - 2,3);
  end;
end;

procedure TTMSMemoEdit.spbSaveClick(Sender: TObject);
var
  filename: string;

begin
  if SaveDialog1.Execute then
  begin
    filename := SaveDialog1.FileName;
    AdvMemo1.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TTMSMemoEdit.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TTMSMemoEdit.SpeedButton2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.



