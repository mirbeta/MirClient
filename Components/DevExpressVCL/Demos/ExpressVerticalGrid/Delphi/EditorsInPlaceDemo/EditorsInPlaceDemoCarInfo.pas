unit EditorsInPlaceDemoCarInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, EditorsInPlaceDemoData, cxStyles, cxMaskEdit, cxMemo,
  cxBlobEdit, cxCurrencyEdit, cxInplaceContainer, cxControls,
  ExtCtrls, cxRadioGroup, cxImage, cxCheckBox, cxImageComboBox,
  cxDropDownEdit, cxTextEdit, cxHyperLinkEdit, cxMRUEdit, ImgList,
  cxGraphics, cxCustomData, cxLookAndFeelPainters, StdCtrls, cxButtons,
  cxDBVGrid, cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses, cxVGrid, CarsData;

type
  TEditorsInPlaceDemoCarInfoForm = class(TForm)
    pnlCarInfo: TPanel;
    vgCarInfo: TcxDBVerticalGrid;
    Panel2: TPanel;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    vgCarInfoCar: TcxDBMultiEditorRow;
    vgCarInfoCategory: TcxDBMultiEditorRow;
    vgCarInfoImage: TcxDBEditorRow;
    vgCarInfoLargeImageAndPrice: TcxDBMultiEditorRow;
    vgCarInfoEngine: TcxDBMultiEditorRow;
    vgCarInfoTransmission: TcxDBMultiEditorRow;
    vgCarInfoMPG: TcxDBMultiEditorRow;
    vgCarInfoWebSite: TcxDBMultiEditorRow;
    vgCarInfoDescription: TcxDBEditorRow;
    vgCarInfoID: TcxDBEditorRow;
    procedure GetDisplayText(
      Sender: TcxCustomEditorRowProperties; ARecord: Integer;
      var AText: String);
    procedure vgCarInfoStylesGetContentStyle(Sender: TObject;
      AEditProp: TcxCustomEditorRowProperties; AFocused: Boolean;
      ARecordIndex: Integer; var AStyle: TcxStyle);
    procedure vgCarInfoLeftVisibleRecordIndexChanged(Sender: TObject);
    procedure cxButtonClick(Sender: TObject);
    procedure OnEditPropertiesButtonClick(
      Sender: TObject);
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

{$R *.dfm}

procedure TEditorsInPlaceDemoCarInfoForm.InitPopupPanel(ACarID: Variant);
begin
  vgCarInfo.DataController.KeyFieldNames := 'ID';
  vgCarInfo.DataController.LocateByKey(ACarID);
  FAccepted := False;
end;

procedure TEditorsInPlaceDemoCarInfoForm.ClosePopup(AAccepted: Boolean);
begin
  if FPopupEdit = nil then Exit;
  FAccepted := AAccepted;
  FPopupEdit.DroppedDown := False;
end;

procedure TEditorsInPlaceDemoCarInfoForm.GetDisplayText(
  Sender: TcxCustomEditorRowProperties; ARecord: Integer;
  var AText: String);
begin
  AText := Sender.Caption;
end;

procedure TEditorsInPlaceDemoCarInfoForm.vgCarInfoStylesGetContentStyle(
  Sender: TObject; AEditProp: TcxCustomEditorRowProperties;
  AFocused: Boolean; ARecordIndex: Integer; var AStyle: TcxStyle);
begin
  if AEditProp.Row is TcxCustomMultiEditorRow then
    if TcxMultiEditorRow(AEditProp.Row).Properties.Editors[0] = AEditProp then
      AStyle := EditorsInPlaceDemoDataDM.styCaption;
end;

procedure TEditorsInPlaceDemoCarInfoForm.vgCarInfoLeftVisibleRecordIndexChanged(
  Sender: TObject);
begin
  FEditValue := vgCarInfoID.Properties.Values[vgCarInfo.LeftVisibleRecord];
end;

procedure TEditorsInPlaceDemoCarInfoForm.cxButtonClick(Sender: TObject);
begin
  ClosePopup(Boolean(TcxButton(Sender).Tag));
end;

procedure TEditorsInPlaceDemoCarInfoForm.OnEditPropertiesButtonClick(
  Sender: TObject);
begin
  ShowMessage('Button click event handler');
end;

end.

