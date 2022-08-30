unit StylesSimpleDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, cxControls, ShellAPI, ComCtrls, ToolWin, ImgList, cxStyles,
  StdCtrls, ExtCtrls, Buttons,  Menus, ActnList, cxCustomData, cxGraphics,
  cxFilter, cxData, cxEdit, cxDBData, cxClasses, cxListBox, cxContainer, cxTL,
  cxMaskEdit, cxDBLookupComboBox,  cxCurrencyEdit, cxMemo, cxCheckBox,
  cxLookAndFeels, cxInplaceContainer, cxDBTL, cxTLData, cxTextEdit, cxMRUEdit,
  DemoBasicMain, cxLookAndFeelPainters;

type
  TStylesSimpleDemoMainForm = class(TDemoBasicMainForm)
    cxDBTreeList: TcxDBTreeList;
    cxDBTreeList1ID: TcxDBTreeListColumn;
    cxDBTreeList1PARENTID: TcxDBTreeListColumn;
    cxDBTreeList1MANAGERID: TcxDBTreeListColumn;
    cxDBTreeList1NAME: TcxDBTreeListColumn;
    cxDBTreeList1BUDGET: TcxDBTreeListColumn;
    cxDBTreeList1LOCATION: TcxDBTreeListColumn;
    cxDBTreeList1PHONE: TcxDBTreeListColumn;
    cxDBTreeList1FAX: TcxDBTreeListColumn;
    cxDBTreeList1EMAIL: TcxDBTreeListColumn;
    cxDBTreeList1VACANCY: TcxDBTreeListColumn;
    miSeparator1: TMenuItem;
    actShowStyleDialog: TAction;
    ShowStyleDialog1: TMenuItem;
    actFooter: TAction;
    actHeaders: TAction;
    actPreview: TAction;
    actIndicator: TAction;
    Preview1: TMenuItem;
    actIndicator1: TMenuItem;
    Header1: TMenuItem;
    Footer1: TMenuItem;
    N1: TMenuItem;
    procedure actHeadersExecute(Sender: TObject);
    procedure actFooterExecute(Sender: TObject);
    procedure actIndicatorExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actShowStyleDialogExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxDBTreeListInitInsertingRecord(Sender: TcxCustomDBTreeList;
      AFocusedNode: TcxDBTreeListNode; var AHandled: Boolean);
  private
     procedure RestoreDefaults(Sender: TObject);
  end;

var
  StylesSimpleDemoMainForm: TStylesSimpleDemoMainForm;

implementation

uses
  StylesSimpleDemoData, StylesSimpleDemoEdit, StylesSimpleDemoStylesDialog, SkinDemoUtils;

{$R *.dfm}

procedure TStylesSimpleDemoMainForm.RestoreDefaults(Sender: TObject);
begin
  with cxDBTreeList.Styles do
  begin
    Background := nil;
    BandBackground := nil;
    BandContent := nil;
    BandHeader := nil;
    ColumnFooter := nil;
    ColumnHeader := nil;
    Content := nil;
    ContentEven := nil;
    ContentOdd := nil;
    Footer := nil;
    Inactive := nil;
    IncSearch := nil;
    Indicator := nil;
    Preview := nil;
    Selection := nil;
    StyleSheet := StylesSimpleDemoDataDM.UserStyleSheet;
  end;
end;

procedure TStylesSimpleDemoMainForm.actHeadersExecute(Sender: TObject);
begin
  cxDBTreeList.OptionsView.Headers := not cxDBTreeList.OptionsView.Headers;
  TAction(Sender).Checked := cxDBTreeList.OptionsView.Headers;
end;

procedure TStylesSimpleDemoMainForm.actFooterExecute(Sender: TObject);
begin
  cxDBTreeList.OptionsView.Footer := not cxDBTreeList.OptionsView.Footer;
  TAction(Sender).Checked := cxDBTreeList.OptionsView.Footer;
end;

procedure TStylesSimpleDemoMainForm.actIndicatorExecute(Sender: TObject);
begin
  cxDBTreeList.OptionsView.Indicator := not cxDBTreeList.OptionsView.Indicator;
  TAction(Sender).Checked := cxDBTreeList.OptionsView.Indicator;
end;

procedure TStylesSimpleDemoMainForm.actPreviewExecute(Sender: TObject);
begin
  cxDBTreeList.Preview.Visible := not cxDBTreeList.Preview.Visible;
  TAction(Sender).Checked := cxDBTreeList.Preview.Visible;
end;

procedure TStylesSimpleDemoMainForm.FormShow(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the StyleSimpleDemo.doc file');

//}
  StylesSimpleDemoStylesDialogForm.RestoreDefaults := RestoreDefaults;
  StylesSimpleDemoStylesDialogForm.Show;
end;

procedure TStylesSimpleDemoMainForm.actShowStyleDialogExecute(
  Sender: TObject);
begin
  StylesSimpleDemoStylesDialogForm.Show;
end;

procedure TStylesSimpleDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  cxDBTreeList.FullExpand;
end;

procedure TStylesSimpleDemoMainForm.cxDBTreeListInitInsertingRecord(
  Sender: TcxCustomDBTreeList; AFocusedNode: TcxDBTreeListNode;
  var AHandled: Boolean);
begin
  StylesSimpleDemoDataDM.SetParentValue(AFocusedNode.ParentKeyValue);
end;

end.



