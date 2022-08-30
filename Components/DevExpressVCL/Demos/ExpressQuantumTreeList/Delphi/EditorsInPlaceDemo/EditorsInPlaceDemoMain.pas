unit EditorsInPlaceDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls,
  cxContainer, cxEdit, cxTextEdit, cxStyles, cxTL,
  cxMaskEdit, cxCalendar, cxCurrencyEdit, cxMemo, cxInplaceContainer,
  cxDBTL, cxTLData, cxDBLookupComboBox, cxTimeEdit, cxCalc, cxSpinEdit,
  cxImageComboBox, cxDropDownEdit, cxEditRepositoryItems, cxRadioGroup,
  cxImage, cxBlobEdit, cxCheckBox, cxHyperLinkEdit, cxButtonEdit, cxMRUEdit,
  cxGraphics, cxCustomData, cxDataUtils, cxLookAndFeelPainters,
  DemoBasicMain, SkinDemoUtils, cxClasses, CarsData;

type
  TEditorsInPlaceDemoMainForm = class(TDemoBasicMainForm)
    tlOrders: TcxDBTreeList;
    tlOrdersCustomerID: TcxDBTreeListColumn;
    tlOrdersProductID: TcxDBTreeListColumn;
    tlOrdersPurchaseDate: TcxDBTreeListColumn;
    tlOrdersTime: TcxDBTreeListColumn;
    tlOrdersPaymentType: TcxDBTreeListColumn;
    tlOrdersPaymentAmount: TcxDBTreeListColumn;
    tlOrdersQuantity: TcxDBTreeListColumn;
    tlOrdersCarInfo: TcxDBTreeListColumn;
    tlOrdersCompanyEmail: TcxDBTreeListColumn;
    miShowEditButtons: TMenuItem;
    miEditBtnsAlways: TMenuItem;
    miEditBtnsFocused: TMenuItem;
    miEditBtnsNever: TMenuItem;
    N1: TMenuItem;
    procedure tlOrdersCarInfoGetDisplayText(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; var Value: String);
    procedure tlOrdersCompanyEmailPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure tlOrdersCarInfoPropertiesInitPopup(Sender: TObject);
    procedure tlOrdersCarInfoPropertiesCloseUp(Sender: TObject);
    procedure miShowEditBtnsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  end;

var
  EditorsInPlaceDemoMainForm: TEditorsInPlaceDemoMainForm;

implementation

uses EditorsInPlaceDemoData, ShellAPI, EditorsInPlaceDemoCarInfo;

{$R *.dfm}

procedure TEditorsInPlaceDemoMainForm.tlOrdersCarInfoGetDisplayText(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: String);
begin
  Value :=  'Click here.'
end;

procedure TEditorsInPlaceDemoMainForm.tlOrdersCompanyEmailPropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  ShellExecute(Handle, PChar('OPEN'), PChar('mailto:' +
    VarToStr(tlOrders.FocusedNode.Values[tlOrdersCompanyEmail.ItemIndex])),
    nil, nil, SW_SHOWMAXIMIZED);

//}
end;

procedure TEditorsInPlaceDemoMainForm.tlOrdersCarInfoPropertiesInitPopup(
  Sender: TObject);
var
  ANode: TcxDBTreeListNode;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  with EditorsInPlaceDemoCarInfoForm do
  begin
    PopupEdit := TcxPopupEdit(Sender);
    ANode := TcxDBTreeListNode(tlOrders.FocusedNode);
    InitPopupPanel(ANode.Values[tlOrdersProductID.ItemIndex]);
  end;

//}
end;

procedure TEditorsInPlaceDemoMainForm.tlOrdersCarInfoPropertiesCloseUp(
  Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  with EditorsInPlaceDemoCarInfoForm, tlOrders do
    if (FocusedNode.Values[tlOrdersProductID.ItemIndex] <> EditValue) and Accepted then
      DataController.SetEditValue(tlOrdersProductID.ItemIndex, EditValue, evsValue);

//}
end;

procedure TEditorsInPlaceDemoMainForm.miShowEditBtnsClick(
  Sender: TObject);
begin
  SetMenuItemChecked(Sender, True);
  tlOrders.OptionsView.ShowEditButtons :=
    TcxEditingControlEditShowButtons(TdxBaseMenuItem(Sender).Tag);
end;

procedure TEditorsInPlaceDemoMainForm.FormShow(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the doc file');

//}
end;

end.
