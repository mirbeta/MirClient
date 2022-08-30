unit main;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, DB, DBTables, ComCtrls,  ShellAPI, dxmdaset,
  dxtrprds, dxtree, dxdbtree;

type
  TFMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    DBTreeView1: TdxDBTreeView;
    Table: TTable;
    DataSource1: TDataSource;
    RadioGroup: TRadioGroup;
    Label1: TLabel;
    ELevels: TEdit;
    Button1: TButton;
    BitBtn1: TBitBtn;
    ImageList1: TImageList;
    DBTreePrintDataSet: TdxDBTreePrintData;
    procedure ELevelsKeyPress(Sender: TObject; var Key: Char);
    procedure ELevelsExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DBTreeView1AddNewItem(Sender: TObject;
      var DBTreeNode: TdxDBTreeNode);
    procedure FormCreate(Sender: TObject);
  end;

var
  FMain: TFMain;

implementation

uses
  Variants,
  dbtreeqr;

{$R *.DFM}

procedure TFMain.ELevelsKeyPress(Sender: TObject; var Key: Char);
begin
  if((Key < '0') Or (Key > '9')) And (Key <> Char(VK_BACK)) then begin
    Key := #0;
    MessageBeep(0);
  end;
end;

procedure TFMain.ELevelsExit(Sender: TObject);
begin
  if(ELevels.Text = '') then
    ELevels.Text := '0';
  DBTreePrintDataSet.LevelCount := StrToInt(ELevels.Text);
end;

procedure TFMain.Button1Click(Sender: TObject);
begin
  if (RadioGroup.ItemIndex = 0) then
    DBTreePrintDataSet.RootValue := ''
  else DBTreePrintDataSet.RootValue := Table.FindField('pr_id').AsString;
  DBTreePrintDataSet.Open;
  QRListForm.QuickReport.Preview;
  DBTreePrintDataSet.Close;
end;


procedure TFMain.DBTreeView1AddNewItem(Sender: TObject;
  var DBTreeNode: TdxDBTreeNode);
begin
 if VarIsNull(DBTreeNode.ParentFieldValue) then exit;
 if (DBTreeNode.KeyFieldValue >= 150)
 and (DBTreeNode.KeyFieldValue < 200)then begin
    DBTreeNode.ImageIndex := 0;
    DBTreeNode.SelectedIndex := 0;
    exit;
 end;
   if (DBTreeNode.KeyFieldValue >= 200) then begin
    DBTreeNode.ImageIndex := 1;
    DBTreeNode.SelectedIndex := 1;
    exit;
 end;
 DBTreeNode.ImageIndex := 2;
 DBTreeNode.SelectedIndex := 2;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  Table.DataBaseName := ExtractFilePath(Application.ExeName);
  Table.Open;
end;

end.
