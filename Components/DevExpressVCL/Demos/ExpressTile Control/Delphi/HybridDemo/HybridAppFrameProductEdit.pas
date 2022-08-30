unit HybridAppFrameProductEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit, cxButtonEdit,
  dxLayoutControl, dxLayoutControlAdapters, Menus, cxDBEdit, cxScrollBox, cxMemo, cxRichEdit, cxDBRichEdit,
  cxCurrencyEdit, cxSpinEdit, cxDropDownEdit, cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, cxCheckBox, cxCalendar,
  StdCtrls, cxButtons, DB, dxCustomTileControl, HybridAppDM, cxGroupBox, dxCustomPreview, dxPDFViewer, dxPDFDocument,
  dxBarBuiltInMenu, dxPDFText, dxPDFRecognizedObject, dxPDFDocumentViewer;

type
  TfrmProductEdit = class(TfrmBase)
    dxLayoutGroup3: TdxLayoutGroup;
    liProductionStartDate: TdxLayoutItem;
    edStartDate: TcxDBDateEdit;
    liAvailableForSale: TdxLayoutItem;
    edAvailable: TcxDBCheckBox;
    liSupportEngineer: TdxLayoutItem;
    edSupport: TcxDBLookupComboBox;
    liProductEngineer: TdxLayoutItem;
    edEngineer: TcxDBLookupComboBox;
    liCategory: TdxLayoutItem;
    edCategory: TcxDBLookupComboBox;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    liCurrentInventory: TdxLayoutItem;
    edCurrentInventory: TcxDBSpinEdit;
    liBackorders: TdxLayoutItem;
    edBackorder: TcxDBSpinEdit;
    liCost: TdxLayoutItem;
    edCost: TcxDBCurrencyEdit;
    liSalePrice: TdxLayoutItem;
    edSalePrice: TcxDBCurrencyEdit;
    liRetailPrice: TdxLayoutItem;
    edRetailPrice: TcxDBCurrencyEdit;
    liDescription: TdxLayoutItem;
    edDescription: TcxDBRichEdit;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    dxLayoutItem17: TdxLayoutItem;
    pdfProduct: TdxPDFViewer;
    dxLayoutItem1: TdxLayoutItem;
    btnSave: TcxButton;
    dxLayoutItem18: TdxLayoutItem;
    btnCancel: TcxButton;
    dxLayoutItem19: TdxLayoutItem;
    btnZoomIn: TcxButton;
    dxLayoutItem20: TdxLayoutItem;
    btnZoomOut: TcxButton;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure pdfProductZoomFactorChanged(Sender: TObject);
  private
    procedure CheckZoomButtons;
  protected
    function GetDataSet: TDataSet; override;
    function GetParentFrameTileItem: TdxTileControlItem; override;
    procedure DoBeforeActivate; override;
    procedure DoOnBackButtonClick; override;
    procedure Translate; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

type
  TdxTileControlDetailSiteAccess = class(TdxTileControlDetailSite);

procedure TfrmProductEdit.btnCancelClick(Sender: TObject);
begin
  if not CanDeactivate then
    Exit;
  DataSet.Cancel;
  ReturnToParentFrame;
end;

procedure TfrmProductEdit.btnSaveClick(Sender: TObject);
begin
  SaveData;
  ReturnToParentFrame;
end;

procedure TfrmProductEdit.btnZoomInClick(Sender: TObject);
begin
  pdfProduct.ZoomIn;
end;

procedure TfrmProductEdit.btnZoomOutClick(Sender: TObject);
begin
  pdfProduct.ZoomOut;
end;

function TfrmProductEdit.GetDataSet: TDataSet;
begin
  Result := edStartDate.DataBinding.DataSource.DataSet;
end;

function TfrmProductEdit.GetParentFrameTileItem: TdxTileControlItem;
begin
  Result := MainForm.tbiProducts;
end;

procedure TfrmProductEdit.DoBeforeActivate;
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    TBlobField(DataSet.FieldByName('PDF')).SaveToStream(AStream);
    pdfProduct.LoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
  pdfProduct.OptionsZoom.ZoomFactor := 80;
end;

procedure TfrmProductEdit.DoOnBackButtonClick;
begin
  btnCancel.Click;
end;

procedure TfrmProductEdit.Translate;
begin
  inherited Translate;

  liProductionStartDate.Caption := cxGetResourceString(@sProductionStartDateLabel);
  liAvailableForSale.Caption := cxGetResourceString(@sAvailableForSaleLabel);
  liSupportEngineer.Caption := cxGetResourceString(@sSupportEngineerLabel);
  liProductEngineer.Caption := cxGetResourceString(@sProductEngineerLabel);
  liCategory.Caption := cxGetResourceString(@sCategoryLabel);
  liCurrentInventory.Caption := cxGetResourceString(@sCurrentInventoryLabel);
  liBackorders.Caption := cxGetResourceString(@sBackOrdersLabel);
  liCost.Caption := cxGetResourceString(@sCostLabel);
  liSalePrice.Caption := cxGetResourceString(@sSalePriceLabel);
  liRetailPrice.Caption := cxGetResourceString(@sRatailPriceLabel);
  liDescription.Caption := cxGetResourceString(@sDescriptionLabel);

  btnSave.Caption := cxGetResourceString(@sSaveButton);
  btnCancel.Caption := cxGetResourceString(@sCancelButton);
  btnZoomIn.Caption := cxGetResourceString(@sZoomInButton);
  btnZoomOut.Caption := cxGetResourceString(@sZoomOutButton);
end;

procedure TfrmProductEdit.CheckZoomButtons;
var
  AZoomOptions: TdxPDFViewerOptionsZoom;
begin
  AZoomOptions := pdfProduct.OptionsZoom;
  btnZoomIn.Enabled := AZoomOptions.ZoomFactor < AZoomOptions.MaxZoomFactor;
  btnZoomOut.Enabled := AZoomOptions.ZoomFactor > AZoomOptions.MinZoomFactor;
end;

procedure TfrmProductEdit.pdfProductZoomFactorChanged(Sender: TObject);
begin
  CheckZoomButtons;
end;

initialization
  RegisterFrame(IDProductEdit, TfrmProductEdit);

end.
