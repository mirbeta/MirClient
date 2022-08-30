unit RatingControlDemoMain;

interface

{$I cxVer.inc}

uses BaseForm, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxStyles, cxCustomData, DB, cxDBData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxNavigator, cxContainer, ImgList, Controls, dxmdaset,
  cxExtEditRepositoryItems, cxEditRepositoryItems, cxDropDownEdit, cxCheckBox,
  StdCtrls, cxRadioGroup, cxSpinEdit, cxTextEdit, cxMaskEdit, cxLabel,
  cxGroupBox, cxGridLevel, dxLayoutContainer, cxGridViewLayoutContainer,
  cxGridLayoutView, cxGridCustomTableView, cxGridDBLayoutView, cxGridCustomView,
  cxGridCustomLayoutView, cxGrid, cxGridCardView, cxGridTableView, cxClasses,
  Menus, ComCtrls, Classes, RatingControlDemoImagePicker, cxDBLookupComboBox;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
    dsCars: TDataSource;
    EditRepository: TcxEditRepository;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryPrice: TcxEditRepositoryCurrencyItem;
    GridLevel1: TcxGridLevel;
    LayoutViewGroup_Root: TdxLayoutGroup;
    LayoutView: TcxGridDBLayoutView;
    LayoutViewLayoutItem1: TcxGridLayoutItem;
    LayoutViewRecId: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem2: TcxGridLayoutItem;
    LayoutViewID: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem3: TcxGridLayoutItem;
    LayoutViewTrademark: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem4: TcxGridLayoutItem;
    LayoutViewModel: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem12: TcxGridLayoutItem;
    LayoutViewCategory: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem15: TcxGridLayoutItem;
    LayoutViewPicture: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem16: TcxGridLayoutItem;
    LayoutViewPrice: TcxGridDBLayoutViewItem;
    LayoutViewGroup4: TdxLayoutGroup;
    LayoutViewGroup5: TdxLayoutGroup;
    LayoutViewSpaceItem1: TdxLayoutEmptySpaceItem;
    stValues: TcxStyle;
    stItems: TcxStyle;
    LayoutViewSeparatorItem1: TdxLayoutSeparatorItem;
    LayoutViewGroup11: TdxLayoutGroup;
    stHeader: TcxStyle;
    stRecordCaption: TcxStyle;
    mdCars: TdxMemData;
    GroupBox: TcxGroupBox;
    Images: TcxImageList;
    stRecordSelected: TcxStyle;
    LayoutViewLayoutItem17: TcxGridLayoutItem;
    LayoutViewRating: TcxGridDBLayoutViewItem;
    EditRepositoryRating: TcxEditRepositoryRatingControl;
    lbOrientation: TcxLabel;
    lbItemCount: TcxLabel;
    lbStep: TcxLabel;
    lbCustomImages: TcxLabel;
    cmbOrientation: TcxComboBox;
    seItemCount: TcxSpinEdit;
    gbFillPrecision: TcxGroupBox;
    rbFull: TcxRadioButton;
    rbHalf: TcxRadioButton;
    rbExact: TcxRadioButton;
    seStep: TcxSpinEdit;
    cbReverseDirection: TcxCheckBox;
    cbAllowHover: TcxCheckBox;
    peChooseImage: TcxPopupEdit;
    mdCarsID: TIntegerField;
    mdCarsTrademarkID: TIntegerField;
    mdCarsName: TWideStringField;
    mdCarsModification: TWideStringField;
    mdCarsCategoryID: TIntegerField;
    mdCarsPrice: TBCDField;
    mdCarsMPG_City: TIntegerField;
    mdCarsMPG_Highway: TIntegerField;
    mdCarsDoors: TIntegerField;
    mdCarsBodyStyleID: TIntegerField;
    mdCarsCilinders: TIntegerField;
    mdCarsHorsepower: TWideStringField;
    mdCarsTorque: TWideStringField;
    mdCarsTransmission_Speeds: TWideStringField;
    mdCarsTransmission_Type: TIntegerField;
    mdCarsDescription: TWideMemoField;
    mdCarsImage: TBlobField;
    mdCategory: TdxMemData;
    mdCategoryID: TIntegerField;
    mdCategoryName: TWideStringField;
    mdCategoryPicture: TBlobField;
    mdTrademark: TdxMemData;
    mdTrademarkID: TIntegerField;
    mdTrademarkName: TWideStringField;
    mdTrademarkSite: TWideStringField;
    mdTrademarkLogo: TBlobField;
    mdTrademarkDescription: TWideMemoField;
    dsCategory: TDataSource;
    dsTrademark: TDataSource;
    mdCarsRating: TFloatField;
    procedure AllowHoverChange(Sender: TObject);
    procedure ChooseImageCloseUp(Sender: TObject);
    procedure ChooseImageInitPopup(Sender: TObject);
    procedure FillPrecisionChange(Sender: TObject);
    procedure ItemCountChange(Sender: TObject);
    procedure OrientationChange(Sender: TObject);
    procedure ReverseDirectionChange(Sender: TObject);
    procedure StepChange(Sender: TObject);
    procedure LayoutViewTrademarkStylesGetContentStyle(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
  private
    FPopupWindow: TfrmImagePicker;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Forms, SysUtils, dxCore, dxRatingControl;

{$R *.dfm}

procedure TfrmMain.AllowHoverChange(Sender: TObject);
begin
  EditRepositoryRating.Properties.AllowHover := cbAllowHover.Checked;
end;

procedure TfrmMain.ChooseImageCloseUp(Sender: TObject);
begin
  FPopupWindow.Release;
end;

procedure TfrmMain.ChooseImageInitPopup(Sender: TObject);
begin
  FPopupWindow := TfrmImagePicker.Create(Self);
  FPopupWindow.Initialize(EditRepositoryRating.Properties);
  peChooseImage.Properties.PopupControl := FPopupWindow;
end;

procedure TfrmMain.FillPrecisionChange(Sender: TObject);
begin
  if rbFull.Checked then
    EditRepositoryRating.Properties.FillPrecision := rcfpFull
  else
    if rbHalf.Checked then
      EditRepositoryRating.Properties.FillPrecision := rcfpHalf
    else
      EditRepositoryRating.Properties.FillPrecision := rcfpExact;
end;

procedure TfrmMain.ItemCountChange(Sender: TObject);
begin
  EditRepositoryRating.Properties.ItemCount := seItemCount.Value;
end;

procedure TfrmMain.OrientationChange(Sender: TObject);
begin
  if cmbOrientation.Text = 'Horizontal' then
    EditRepositoryRating.Properties.Orientation := orHorizontal
  else
    EditRepositoryRating.Properties.Orientation := orVertical;
end;

procedure TfrmMain.ReverseDirectionChange(Sender: TObject);
begin
  EditRepositoryRating.Properties.ReverseDirection := cbReverseDirection.Checked;
end;

procedure TfrmMain.StepChange(Sender: TObject);
begin
  EditRepositoryRating.Properties.Step := seStep.Value;
end;

procedure TfrmMain.LayoutViewTrademarkStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  AStyle := stHeader;
end;

end.
