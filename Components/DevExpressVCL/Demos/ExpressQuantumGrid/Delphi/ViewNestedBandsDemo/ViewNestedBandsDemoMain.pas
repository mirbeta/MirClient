unit ViewNestedBandsDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView, cxControls,
  cxGridDBTableView, cxGridCustomView, cxGrid, cxCustomData, ExtCtrls, ActnList,
  ImgList, Menus, ComCtrls, cxGridCustomPopupMenu, cxGridPopupMenu, DB, Grids,
  DBGrids, cxDBData, cxStyles, cxGraphics, cxFilter, cxData, cxEdit, cxClasses,
  cxDataStorage, cxMRUEdit, cxButtonEdit, cxDropDownEdit, cxCheckBox, cxMaskEdit,
  cxDBLookupComboBox, cxTimeEdit, cxImageComboBox, cxSpinEdit, cxCalc, cxImage,
  cxBlobEdit, cxRadioGroup, cxMemo, cxHyperLinkEdit, cxGridBandedTableView,
  cxLookAndFeels, cxGridDBBandedTableView, cxCalendar, cxLookAndFeelPainters,
  BaseForm, cxNavigator, cxGridCardView, CarsDataForGrid;

type
  TViewNestedBandsDemoMainForm = class(TfmBaseForm)
    bvOrders: TcxGridDBBandedTableView;
    clnCarCyl: TcxGridDBBandedColumn;
    clnCarHP: TcxGridDBBandedColumn;
    clnCarMPG_City: TcxGridDBBandedColumn;
    clnCarMPG_Highway: TcxGridDBBandedColumn;
    clnCarTorque: TcxGridDBBandedColumn;
    clnCarTransMissAuto: TcxGridDBBandedColumn;
    clnCarTransmissSpeedCount: TcxGridDBBandedColumn;
    clnCustomerAddres: TcxGridDBBandedColumn;
    clnCustomerCompany: TcxGridDBBandedColumn;
    clnCustomerFax: TcxGridDBBandedColumn;
    clnCustomerID: TcxGridDBBandedColumn;
    clnCustomerOccupation: TcxGridDBBandedColumn;
    clnCustomerPhone: TcxGridDBBandedColumn;
    clnCustomerZipCode: TcxGridDBBandedColumn;
    clnOrdersProductID: TcxGridDBBandedColumn;
    clnPaymentAmount: TcxGridDBBandedColumn;
    clnPaymentType: TcxGridDBBandedColumn;
    clnPurchaseDate: TcxGridDBBandedColumn;
    clnQuantity: TcxGridDBBandedColumn;
    Grid: TcxGrid;
    lvOrders: TcxGridLevel;
    miBandsQuickCustomization: TMenuItem;
    miCellMerging: TMenuItem;
    miColumnsQuickCustomization: TMenuItem;
    miNestedBands: TMenuItem;
    miOptions: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure miBandsQuickCustomizationClick(Sender: TObject);
    procedure miCellMergingClick(Sender: TObject);
    procedure miColumnsQuickCustomizationClick(Sender: TObject);
    procedure miNestedBandsClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FNestedBands: TList;
    procedure AddNestedBandInfo(ABand: TcxGridBand);
    procedure AdjustCellMerging(AUseCelMerging: Boolean);
    procedure AdjustNestedBands(AUseNestedBands: Boolean);
    procedure ChangeBandVisibility(AIndex: Integer; AVisible: Boolean);
    procedure HideNestedBands;
    procedure ReleaseNestedBandInfos;
    procedure ShowNestedBands;
  end;

var
  ViewNestedBandsDemoMainForm: TViewNestedBandsDemoMainForm;

implementation

uses
  AboutDemoForm, ViewNestedBandsDemoData;

{$R *.dfm}

type
  TcxNestedBandInfo = class
  private
    FBand, FParentBand: TcxGridBand;
    FColumnIndex: Integer;
    FCaption: TCaption;
  public
    constructor Create(ABand: TcxGridBand);
    procedure RestoreBand;
    property Band: TcxGridBand read FBand;
    property Caption: TCaption read FCaption;
    property ColumnIndex: Integer read FColumnIndex;
    property ParentBand: TcxGridBand read FParentBand;
  end;

constructor TcxNestedBandInfo.Create(ABand: TcxGridBand);
begin
  inherited Create;
  FBand := ABand;
  FParentBand := ABand.Bands[ABand.Position.BandIndex];
  FCaption := ABand.Caption;
  FColumnIndex := ABand.Position.ColIndex;
end;

procedure TcxNestedBandInfo.RestoreBand;
begin
  FBand.Caption := FCaption;
  FBand.Position.BandIndex := FParentBand.Index;
  FBand.Position.ColIndex := FColumnIndex;
end;

{ TViewNestedBandsDemoMainForm }

constructor TViewNestedBandsDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNestedBands := TList.Create;
end;

destructor TViewNestedBandsDemoMainForm.Destroy;
begin
  inherited Destroy;
  ReleaseNestedBandInfos;
  FNestedBands.Free;
end;

procedure TViewNestedBandsDemoMainForm.AddNestedBandInfo(ABand: TcxGridBand);
begin
  FNestedBands.Add(TcxNestedBandInfo.Create(ABand));
end;

procedure TViewNestedBandsDemoMainForm.AdjustCellMerging(AUseCelMerging: Boolean);
begin
  bvOrders.BeginUpdate;
  try
    clnCustomerID.Options.CellMerging := AUseCelMerging;
    clnOrdersProductID.Options.CellMerging := AUseCelMerging;
    clnPurchaseDate.Options.CellMerging := AUseCelMerging;
    clnPaymentType.Options.CellMerging := AUseCelMerging;
    clnCustomerCompany.Options.CellMerging := AUseCelMerging;
    clnCustomerAddres.Options.CellMerging := AUseCelMerging;
    clnCustomerFax.Options.CellMerging := AUseCelMerging;
    clnCustomerPhone.Options.CellMerging := AUseCelMerging;
    clnCustomerOccupation.Options.CellMerging := AUseCelMerging;
    clnCustomerZipCode.Options.CellMerging := AUseCelMerging;
    clnCarCyl.Options.CellMerging := AUseCelMerging;
    clnCarHP.Options.CellMerging := AUseCelMerging;
    clnCarTorque.Options.CellMerging := AUseCelMerging;
    clnCarMPG_City.Options.CellMerging := AUseCelMerging;
    clnCarMPG_Highway.Options.CellMerging := AUseCelMerging;
    clnCarTransMissAuto.Options.CellMerging := AUseCelMerging;
    clnCarTransmissSpeedCount.Options.CellMerging := AUseCelMerging;
  finally
    bvOrders.EndUpdate;
  end;
end;

procedure TViewNestedBandsDemoMainForm.AdjustNestedBands(AUseNestedBands: Boolean);
begin
  if AUseNestedBands then
    ShowNestedBands
  else
    HideNestedBands;
end;

procedure TViewNestedBandsDemoMainForm.ChangeBandVisibility(
  AIndex: Integer; AVisible: Boolean);
begin
  bvOrders.Bands[AIndex].Visible := AVisible;
end;

procedure TViewNestedBandsDemoMainForm.HideNestedBands;
var
  I: Integer;
begin
  ReleaseNestedBandInfos;
  bvOrders.BeginUpdate;
  try
    for I := 0 to bvOrders.Bands.Count - 1 do
      if bvOrders.Bands[I].Position.BandIndex <> -1 then
      begin
        AddNestedBandInfo(bvOrders.Bands[I]);
        ChangeBandVisibility(bvOrders.Bands[I].Position.BandIndex, False);
        bvOrders.Bands[I].Caption :=
          bvOrders.Bands[bvOrders.Bands[I].Position.BandIndex].Caption + '''s ' +
            bvOrders.Bands[I].Caption;
        bvOrders.Bands[I].Position.BandIndex := -1;
      end
   finally
     bvOrders.EndUpdate;
   end;
end;

procedure TViewNestedBandsDemoMainForm.ReleaseNestedBandInfos;
var
  I: Integer;
begin
  for I := 0 to FNestedBands.Count - 1 do
    if TObject(FNestedBands[I]) is TcxNestedBandInfo then
      TcxNestedBandInfo(FNestedBands[I]).Free;
  FNestedBands.Clear;
end;

procedure TViewNestedBandsDemoMainForm.ShowNestedBands;
var
  I: Integer;
begin
  bvOrders.BeginUpdate;
  try
    for I := 0 to FNestedBands.Count - 1 do
      if TObject(FNestedBands[I]) is TcxNestedBandInfo then
      begin
        TcxNestedBandInfo(FNestedBands[I]).RestoreBand;
        ChangeBandVisibility(TcxNestedBandInfo(FNestedBands[I]).ParentBand.Index, True);
      end;
    ReleaseNestedBandInfos;
  finally
    bvOrders.EndUpdate;
  end;
end;

procedure TViewNestedBandsDemoMainForm.miNestedBandsClick(Sender: TObject);
begin
  AdjustNestedBands(GetMenuItemChecked(Sender));
  bvOrders.OptionsCustomize.NestedBands := GetMenuItemChecked(Sender);
end;

procedure TViewNestedBandsDemoMainForm.miBandsQuickCustomizationClick(Sender: TObject);
begin
  bvOrders.OptionsCustomize.BandsQuickCustomization := GetMenuItemChecked(Sender);
end;

procedure TViewNestedBandsDemoMainForm.miColumnsQuickCustomizationClick(Sender: TObject);
begin
  bvOrders.OptionsCustomize.ColumnsQuickCustomization := GetMenuItemChecked(Sender);
end;

procedure TViewNestedBandsDemoMainForm.miCellMergingClick(Sender: TObject);
begin
  AdjustCellMerging(GetMenuItemChecked(Sender));
end;

end.
