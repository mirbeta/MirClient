unit HybridAppFrameCustomerEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit, cxButtonEdit,
  dxLayoutControl, dxLayoutControlAdapters, Menus, dxScreenTip, dxMapControlTypes,
  dxMapControlOpenStreetMapImageryDataProvider, dxMapItem, dxCustomMapItemLayer, dxMapItemLayer, dxMapLayer,
  dxMapImageTileLayer, dxMapControl, cxMemo, cxRichEdit, cxDBRichEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit,
  cxDBLookupComboBox, cxDBEdit, dxCustomHint, cxHint, StdCtrls, cxButtons, DB, dxCustomTileControl, HybridAppDM,
  cxGroupBox;

type
  TfrmCustomerEdit = class(TfrmBase)
    stRepository: TdxScreenTipRepository;
    stRepositoryScreenTip1: TdxScreenTip;
    cxHintStyleController1: TcxHintStyleController;
    dxLayoutGroup3: TdxLayoutGroup;
    liName: TdxLayoutItem;
    edName: TcxDBTextEdit;
    liAddress: TdxLayoutItem;
    edHomeAddress: TcxDBTextEdit;
    liCity: TdxLayoutItem;
    edHomeCity: TcxDBTextEdit;
    liState: TdxLayoutItem;
    edHomeState: TcxDBLookupComboBox;
    liZipCode: TdxLayoutItem;
    edHomeZipCode: TcxDBTextEdit;
    liFax: TdxLayoutItem;
    edFax: TcxDBTextEdit;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    liBillingAddress: TdxLayoutItem;
    edBillingAddress: TcxDBTextEdit;
    liBillingCity: TdxLayoutItem;
    edBillingCity: TcxDBTextEdit;
    liBillingState: TdxLayoutItem;
    edBillingState: TcxDBLookupComboBox;
    liBillingZipCode: TdxLayoutItem;
    edBillingZipCode: TcxDBTextEdit;
    liProfile: TdxLayoutItem;
    edProfile: TcxDBRichEdit;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    liPhone: TdxLayoutItem;
    edPhone: TcxDBTextEdit;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    dxLayoutItem16: TdxLayoutItem;
    dxMapControl1: TdxMapControl;
    dxMapControl1ImageTileLayer1: TdxMapImageTileLayer;
    mcItemLayer: TdxMapItemLayer;
    mcItemLayerDot1: TdxMapDot;
    dxLayoutItem1: TdxLayoutItem;
    btnSave: TcxButton;
    dxLayoutItem17: TdxLayoutItem;
    btnCancel: TcxButton;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  protected
    function GetDataSet: TDataSet; override;
    function GetParentFrameTileItem: TdxTileControlItem; override;
    procedure DoAfterActivate; override;
    procedure DoOnBackButtonClick; override;
    procedure Translate; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

procedure TfrmCustomerEdit.btnCancelClick(Sender: TObject);
begin
  if not CanDeactivate then
    Exit;
  DataSet.Cancel;
  ReturnToParentFrame;
end;

procedure TfrmCustomerEdit.btnSaveClick(Sender: TObject);
begin
  SaveData;
  ReturnToParentFrame;
end;

function TfrmCustomerEdit.GetDataSet: TDataSet;
begin
  Result := edName.DataBinding.DataSource.DataSet;
end;

function TfrmCustomerEdit.GetParentFrameTileItem: TdxTileControlItem;
begin
  Result := MainForm.tbiCustomers;
end;

procedure TfrmCustomerEdit.DoAfterActivate;

  procedure AddMapDot(const ASize: Integer; const ACity: string; const ALongitude, ALatitude: Double);
  var
    ADot: TdxMapItem;
  begin
    ADot := mcItemLayer.MapItems.Add(TdxMapDot);
    ADot.Hint := ACity;
    TdxMapDot(ADot).Location.Longitude := ALongitude;
    TdxMapDot(ADot).Location.Latitude := ALatitude;
    TdxMapDot(ADot).Size := ASize;
  end;

var
  ALongitude, ALatitude: Double;
begin
  inherited ;
  ALongitude := DataSet.FieldByName('HomeOffice_Longitude').AsFloat;
  ALatitude := DataSet.FieldByName('HomeOffice_Latitude').AsFloat;

  dxMapControl1.CenterPoint.Longitude := ALongitude;
  dxMapControl1.CenterPoint.Latitude := ALatitude;

  mcItemLayer.MapItems.Clear;
  AddMapDot(40, DataSet.FieldByName('HomeOffice_City').AsString, ALongitude, ALatitude);
  DM.clCustomerStores.First;
  while not DM.clCustomerStores.EOF do
  begin
    AddMapDot(30, DM.clCustomerStores.FieldByName('Address_City').AsString,
      DM.clCustomerStores.FieldByName('Address_Longitude').AsFloat,
      DM.clCustomerStores.FieldByName('Address_Latitude').AsFloat);
    DM.clCustomerStores.Next;
  end;
end;

procedure TfrmCustomerEdit.DoOnBackButtonClick;
begin
  btnCancel.Click;
end;

procedure TfrmCustomerEdit.Translate;
begin
  inherited Translate;

  liAddress.Caption := cxGetResourceString(@sAddressLabel);
  liName.Caption := cxGetResourceString(@sNameLabel);
  liCity.Caption := cxGetResourceString(@sCityLabel);
  liState.Caption := cxGetResourceString(@sStateLabel);
  liPhone.Caption := cxGetResourceString(@sPhoneLabel);
  liZipCode.Caption := cxGetResourceString(@sZipCodeLabel);
  liFax.Caption := cxGetResourceString(@sFaxLabel);
  liBillingAddress.Caption := cxGetResourceString(@sBillingAddressLabel);
  liBillingCity.Caption := cxGetResourceString(@sCityLabel);
  liBillingState.Caption := cxGetResourceString(@sStateLabel);
  liBillingZipCode.Caption := cxGetResourceString(@sZipCodeLabel);
  liProfile.Caption := cxGetResourceString(@sProfileLabel);

  btnSave.Caption := cxGetResourceString(@sSaveButton);
  btnCancel.Caption := cxGetResourceString(@sCancelButton);
end;

initialization
  RegisterFrame(IDCustomerEdit, TfrmCustomerEdit);

end.
