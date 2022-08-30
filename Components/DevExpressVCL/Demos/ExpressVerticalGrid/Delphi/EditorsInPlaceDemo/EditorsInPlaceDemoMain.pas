unit EditorsInPlaceDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, DemoBasicMain, cxContainer, cxEdit, cxTextEdit, cxStyles, cxMaskEdit,
  cxCalendar, cxCurrencyEdit, cxMemo, cxInplaceContainer, cxDBLookupComboBox,
  cxTimeEdit, cxCalc, cxSpinEdit, cxImageComboBox, cxDropDownEdit, cxEditRepositoryItems,
  cxRadioGroup, cxImage, cxBlobEdit, cxCheckBox, cxHyperLinkEdit, cxButtonEdit, cxMRUEdit,
  cxGraphics, cxCustomData, cxDataUtils, cxVGrid, cxDBVGrid, cxClasses, Variants, CarsData, cxLookAndFeelPainters;

type
  TEditorsInPlaceDemoMainForm = class(TDemoBasicMainForm)
    miShowEditButtons: TMenuItem;
    miEditBtnsAlways: TMenuItem;
    miEditBtnsFocused: TMenuItem;
    miEditBtnsNever: TMenuItem;
    N1: TMenuItem;
    vgOrders: TcxDBVerticalGrid;
    vgOrdersCompany: TcxCategoryRow;
    vgOrdersCustomerID: TcxDBEditorRow;
    vgOrdersCustomerEmail: TcxDBEditorRow;
    vgOrdersPurchaseInfo: TcxCategoryRow;
    vgOrdersPaymentType: TcxDBEditorRow;
    vgOrdersPaymentAmount: TcxDBEditorRow;
    vgOrdersTime: TcxDBEditorRow;
    vgOrdersPurchaseDate: TcxDBEditorRow;
    vgOrdersQuantity: TcxDBEditorRow;
    vgOrdersCar: TcxCategoryRow;
    vgOrdersProductID: TcxDBEditorRow;
    vgOrdersCarInfo: TcxDBEditorRow;
    procedure vgOrdersCompanyEmailPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure miShowEditBtnsClick(Sender: TObject);
    procedure vgOrdersCarInfoPropertiesGetDisplayText(
      Sender: TcxCustomEditorRowProperties; ARecord: Integer;
      var AText: String);
    procedure vgOrdersCarInfoEditPropertiesInitPopup(Sender: TObject);
    procedure vgOrdersCarInfoEditPropertiesCloseUp(Sender: TObject);
  private
  end;

var
  EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm;

implementation

uses EditorsInPlaceDemoData, ShellAPI, EditorsInPlaceDemoCarInfo;

{$R *.dfm}

procedure TEditorsInPlaceDemoMainForm.vgOrdersCompanyEmailPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShellExecute(Handle, PChar('OPEN'), PChar('mailto:' +
    VarToStr(vgOrdersCustomerEmail.Properties.Value)),
    nil, nil, SW_SHOWMAXIMIZED);
end;

procedure TEditorsInPlaceDemoMainForm.miShowEditBtnsClick(
  Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  vgOrders.OptionsView.ShowEditButtons := TcxEditingControlEditShowButtons(TMenuItem(Sender).Tag);
end;

procedure TEditorsInPlaceDemoMainForm.vgOrdersCarInfoPropertiesGetDisplayText(
  Sender: TcxCustomEditorRowProperties; ARecord: Integer;
  var AText: String);
begin
  AText := 'Click here';
end;

procedure TEditorsInPlaceDemoMainForm.vgOrdersCarInfoEditPropertiesInitPopup(
  Sender: TObject);
var
  ACarID: Variant;
begin
  EditorsInPlaceDemoCarInfoForm.PopupEdit := TcxPopupEdit(Sender);
  ACarID := vgOrdersProductID.Properties.Value;
  EditorsInPlaceDemoCarInfoForm.InitPopupPanel(ACarID);
end;

procedure TEditorsInPlaceDemoMainForm.vgOrdersCarInfoEditPropertiesCloseUp(
  Sender: TObject);
begin
  with EditorsInPlaceDemoCarInfoForm, vgOrders.DataController do
    if (vgOrdersProductID.Properties.Value <> EditValue) and Accepted then
      vgOrdersProductID.Properties.Value := EditValue;
end;

end.
