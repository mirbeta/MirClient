unit IssueListItems;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IssueListForm, ExtCtrls, StdCtrls, cxControls, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxDBEdit, DBCtrls, ComCtrls, ToolWin,
  ImgList, cxMaskEdit, cxDropDownEdit, cxCalendar, cxLookupEdit,
  cxDBLookupComboBox, cxImageComboBox, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, cxDBLookupEdit, cxNavigator, cxLabel,
  cxPCdxBarPopupMenu, cxPC;

type
  TfrmItems = class(TfrmBasic)
    PageControl: TcxPageControl;
    tsGeneral: TcxTabSheet;
    tsDescription: TcxTabSheet;
    cxDBMemo1: TcxDBMemo;
    cxDBDateEdit1: TcxDBDateEdit;
    cxDBDateEdit2: TcxDBDateEdit;
    cxDBDateEdit3: TcxDBDateEdit;
    cxDBImageComboBox1: TcxDBImageComboBox;
    cxDBImageComboBox2: TcxDBImageComboBox;
    cxDBImageComboBox3: TcxDBImageComboBox;
    cxDBLookupComboBox1: TcxDBLookupComboBox;
    cxDBLookupComboBox2: TcxDBLookupComboBox;
    cxDBLookupComboBox3: TcxDBLookupComboBox;
    cxDBTextEdit1: TcxDBTextEdit;
    Label1: TcxLabel;
    Label10: TcxLabel;
    Label11: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    Label5: TcxLabel;
    Label6: TcxLabel;
    Label7: TcxLabel;
    Label8: TcxLabel;
  private
    { Private declarations }
  public
  end;

var
  frmItems: TfrmItems;

implementation

{$R *.dfm}

uses
  IssueListForms, IssueListConst, IssueListData;

{ TfrmItems }

initialization

 FormManager.RegisterForm(ItemsFormID, TfrmItems, 'PROJECT ITEMS');

end.
