unit StylesSimpleDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, cxControls, ShellAPI, ComCtrls, ToolWin, ImgList, cxStyles,
  StdCtrls, ExtCtrls, Buttons,  Menus, ActnList, cxCustomData, cxGraphics,
  cxFilter, cxData, cxEdit, cxDBData, cxClasses, cxListBox, cxContainer,
  cxMaskEdit, cxDBLookupComboBox,  cxCurrencyEdit, cxMemo, cxCheckBox,
  cxLookAndFeels, cxInplaceContainer, cxTextEdit, cxMRUEdit,
  DemoBasicMain, cxVGrid, cxDBVGrid, cxHyperLinkEdit;

type
  TStylesSimpleDemoMainForm = class(TDemoBasicMainForm)
    cxDBVerticalGrid: TcxDBVerticalGrid;
    cxDBVerticalGridOrderInfo: TcxCategoryRow;
    cxDBVerticalGridPurchaseDate: TcxDBEditorRow;
    cxDBVerticalGridQuantity: TcxDBEditorRow;
    cxDBVerticalGridTime: TcxDBEditorRow;
    cxDBVerticalGridPaymentAmount: TcxDBEditorRow;
    cxDBVerticalGridPaymentType: TcxDBEditorRow;
    cxDBVerticalGridCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridCommonCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridSpouse: TcxDBEditorRow;
    cxDBVerticalGridFirstName: TcxDBEditorRow;
    cxDBVerticalGridPrefix: TcxDBEditorRow;
    cxDBVerticalGridLastName: TcxDBEditorRow;
    cxDBVerticalGridTitle: TcxDBEditorRow;
    cxDBVerticalGridCustomerContacts: TcxCategoryRow;
    cxDBVerticalGridEmail: TcxDBEditorRow;
    cxDBVerticalGridHomePhone: TcxDBEditorRow;
    cxDBVerticalGridState: TcxDBEditorRow;
    cxDBVerticalGridAddress: TcxDBEditorRow;
    cxDBVerticalGridCity: TcxDBEditorRow;
    cxDBVerticalGridZipCode: TcxDBEditorRow;
    cxDBVerticalGridFaxPhone: TcxDBEditorRow;
    cxDBVerticalGridOccupation: TcxDBEditorRow;
    cxDBVerticalGridCustomer: TcxDBEditorRow;
    cxDBVerticalGridCompany: TcxDBEditorRow;
    cxDBVerticalGridCarInfo: TcxCategoryRow;
    cxDBVerticalGridCyl: TcxDBEditorRow;
    cxDBVerticalGridHP: TcxDBEditorRow;
    cxDBVerticalGridTrademark: TcxDBEditorRow;
    cxDBVerticalGridModel: TcxDBEditorRow;
    cxDBVerticalGridLiter: TcxDBEditorRow;
    cxDBVerticalGridMPG_City: TcxDBEditorRow;
    cxDBVerticalGridMPG_Highway: TcxDBEditorRow;
    cxDBVerticalGridTransmissAutomatic: TcxDBEditorRow;
    cxDBVerticalGridTransmissSpeedCount: TcxDBEditorRow;
    cxDBVerticalGridCategory: TcxDBEditorRow;
    cxDBVerticalGridCars_Description: TcxDBEditorRow;
    cxDBVerticalGridHyperlink: TcxDBEditorRow;
    cxDBVerticalGridPicture: TcxDBEditorRow;
    cxDBVerticalGridPrice: TcxDBEditorRow;
    cxDBVerticalGridCar: TcxCategoryRow;
    cxDBVerticalGridEngine: TcxCategoryRow;
    cxDBVerticalGridTransmission: TcxCategoryRow;
    cxDBVerticalGridMPG: TcxCategoryRow;
    cxDBVerticalGridOthers: TcxCategoryRow;
    cxDBVerticalGridNotes: TcxCategoryRow;
    cxDBVerticalGridPhonesAndFaxes: TcxCategoryRow;
    cxDBVerticalGridCategoryAddress: TcxCategoryRow;
    actShowStyleDialog: TAction;
    ShowStyleDialog1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure actShowStyleDialogExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     procedure RestoreDefaults(Sender: TObject);
  public
     procedure StylesFormClosed(Sender: TObject; var Action: TCloseAction);
  end;

var
  StylesSimpleDemoMainForm: TStylesSimpleDemoMainForm;

implementation

uses StylesSimpleDemoData, StylesSimpleDemoEdit, StylesSimpleDemoStylesDialog;

{$R *.dfm}

procedure TStylesSimpleDemoMainForm.RestoreDefaults(Sender: TObject);
begin
  with cxDBVerticalGrid.Styles do
  begin
    Background := nil;
    Category := nil;
    Header := nil;
    Content := nil;
    Inactive := nil;
    IncSearch := nil;
    Selection := nil;
    StyleSheet := StylesSimpleDemoDataDM.UserStyleSheet;
  end;
end;

procedure TStylesSimpleDemoMainForm.FormShow(Sender: TObject);
begin
  StylesSimpleDemoStylesDialogForm.RestoreDefaults := RestoreDefaults;
  StylesSimpleDemoStylesDialogForm.Show;
end;

procedure TStylesSimpleDemoMainForm.actShowStyleDialogExecute(
  Sender: TObject);
begin
  if not StylesSimpleDemoStylesDialogForm.Visible then
  begin
    StylesSimpleDemoStylesDialogForm.Show;
    TCustomAction(Sender).Checked := True;
  end
  else
  begin
    StylesSimpleDemoStylesDialogForm.Hide;
    TCustomAction(Sender).Checked := False;
  end
end;

procedure TStylesSimpleDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  cxDBVerticalGrid.FullExpand;
end;

procedure TStylesSimpleDemoMainForm.StylesFormClosed(Sender: TObject; var Action: TCloseAction);
begin
  actShowStyleDialog.Checked := False;
end;

end.



