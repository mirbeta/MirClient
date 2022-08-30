unit EditorsInPlaceDemoCarInfo;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EditorsInPlaceDemoData, cxStyles, cxTL, cxMaskEdit, cxMemo,
  cxBlobEdit, cxCurrencyEdit, cxInplaceContainer, cxDBTL, cxControls,
  cxTLData, ExtCtrls, cxRadioGroup, cxImage, cxCheckBox, cxImageComboBox,
  cxDropDownEdit, cxTextEdit, cxHyperLinkEdit, cxMRUEdit, ImgList,
  cxGraphics, cxCustomData, cxLookAndFeelPainters, StdCtrls, cxButtons,
  Menus, cxLookAndFeels, CarsData, cxDBLookupComboBox;

type
  TEditorsInPlaceDemoCarInfoForm = class(TForm)
    pnlCarInfo: TPanel;
    tlCarInfo: TcxDBTreeList;
    tlCarInfoTrademark: TcxDBTreeListColumn;
    tlCarInfoModel: TcxDBTreeListColumn;
    tlCarInfoHP: TcxDBTreeListColumn;
    tlCarInfoTorque: TcxDBTreeListColumn;
    tlCarInfoCyl: TcxDBTreeListColumn;
    tlCarInfoTransmissSpeedCount: TcxDBTreeListColumn;
    tlCarInfoTransmissAutomatic: TcxDBTreeListColumn;
    tlCarInfoMPG_City: TcxDBTreeListColumn;
    tlCarInfoMPG_Highway: TcxDBTreeListColumn;
    tlCarInfoCategory: TcxDBTreeListColumn;
    tlCarInfoDescription: TcxDBTreeListColumn;
    tlCarInfoHyperlink: TcxDBTreeListColumn;
    tlCarInfoPicture: TcxDBTreeListColumn;
    tlCarInfoPrice: TcxDBTreeListColumn;
    tlCarInfoBlobImage: TcxDBTreeListColumn;
    tlCarInfoCategoryCaption: TcxDBTreeListColumn;
    tlCarInfoCarCaption: TcxDBTreeListColumn;
    tlCarInfoLargePictureCaption: TcxDBTreeListColumn;
    tlCarInfoEngineCaption: TcxDBTreeListColumn;
    tlCarInfoTransmissCaption: TcxDBTreeListColumn;
    tlCarInfoMPG: TcxDBTreeListColumn;
    tlCarInfoPriceCaption: TcxDBTreeListColumn;
    tlCarInfoTransmissAutomatCaption: TcxDBTreeListColumn;
    pnlButtons: TPanel;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure tlCarInfoTopRecordIndexChanged(Sender: TObject);
    procedure tlCarInfoTrademarkPropertiesButtonClick(Sender: TObject);
    procedure tlCarInfoGetCaptionDisplayText(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; var Value: String);
    procedure FormShow(Sender: TObject);
  private
    FPopupEdit: TcxPopupEdit;
    FAccepted: Boolean;
    FEditValue: Variant;
    procedure ClosePopup(AAccepted: Boolean);
  public
    procedure InitPopupPanel(ACarID: Variant);
    property PopupEdit: TcxPopupEdit read FPopupEdit write FPopupEdit;
    property Accepted: Boolean read FAccepted write FAccepted;
    property EditValue: Variant read FEditValue;
  end;

var
  EditorsInPlaceDemoCarInfoForm: TEditorsInPlaceDemoCarInfoForm;

implementation

uses
 Types;

{$R *.dfm}

procedure TEditorsInPlaceDemoCarInfoForm.btnCancelClick(Sender: TObject);
begin
  ClosePopup(False);
end;

procedure TEditorsInPlaceDemoCarInfoForm.btnOKClick(Sender: TObject);
begin
  ClosePopup(True);
end;

procedure TEditorsInPlaceDemoCarInfoForm.tlCarInfoTopRecordIndexChanged(
  Sender: TObject);
begin
  FEditValue := TcxDBTreeListNode(tlCarInfo.TopVisibleNode).KeyValue;
end;

procedure TEditorsInPlaceDemoCarInfoForm.tlCarInfoTrademarkPropertiesButtonClick(
  Sender: TObject);
begin
  ShowMessage('Button click event handler');
end;

procedure TEditorsInPlaceDemoCarInfoForm.InitPopupPanel(ACarID: Variant);
begin
  tlCarInfo.HandleNeeded;
  tlCarInfo.DataController.LocateByKey(ACarID);
  FAccepted := False;
end;

procedure TEditorsInPlaceDemoCarInfoForm.ClosePopup(AAccepted: Boolean);
begin
  if FPopupEdit = nil then Exit;
  FAccepted := AAccepted;
  FPopupEdit.DroppedDown := False;
end;

procedure TEditorsInPlaceDemoCarInfoForm.tlCarInfoGetCaptionDisplayText(
  Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: String);
begin
  Value := Sender.Caption.Text;
end;

procedure TEditorsInPlaceDemoCarInfoForm.FormShow(Sender: TObject);
begin
  FEditValue := TcxDBTreeListNode(tlCarInfo.TopVisibleNode).KeyValue;
end;

end.
