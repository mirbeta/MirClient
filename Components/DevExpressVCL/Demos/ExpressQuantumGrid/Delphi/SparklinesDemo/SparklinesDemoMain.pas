unit SparklinesDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus, Graphics,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView, cxGridCardView,
  cxGridDBCardView, cxClasses, cxControls, cxGridCustomView, cxGrid, ComCtrls,
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, DB, cxDBData,
  cxDataStorage, cxLookAndFeelPainters, cxLookAndFeels, cxHyperLinkEdit,
  cxImageComboBox, cxDBLookupComboBox, cxMemo, cxGridTableView, BaseForm,
  dxmdaset, cxNavigator, cxImage, dxSparkline, dxDBSparkline, cxGridDBTableView;

type
  TfmSparklines = class(TfmBaseForm)
    miOptions: TMenuItem;
    miPaymentAmountTotalByTrademark: TMenuItem;
    cxgCarOrders: TcxGrid;
    cxgCarOrdersTableView: TcxGridDBTableView;
    cxgCarOrdersTableViewRecId: TcxGridDBColumn;
    cxgCarOrdersTableViewTrademark: TcxGridDBColumn;
    cxgCarOrdersTableViewProductName: TcxGridDBColumn;
    cxgCarOrdersTableViewTrademarkSite: TcxGridDBColumn;
    cxgCarOrdersTableViewPhoto: TcxGridDBColumn;
    cxgCarOrdersTableViewPrice: TcxGridDBColumn;
    cxgCarOrdersTableViewPayments: TcxGridDBColumn;
    cxgCarOrdersLevel1: TcxGridLevel;
    mdsCarOrders: TdxMemData;
    mdsCarOrdersID: TIntegerField;
    mdsCarOrdersTrademark: TStringField;
    mdsCarOrdersModel: TStringField;
    mdsCarOrdersTrademark_Site: TStringField;
    mdsCarOrdersPhoto: TBlobField;
    mdsCarOrdersPrice: TCurrencyField;
    dsCarOrders: TDataSource;
    mdsOrderDetails: TdxMemData;
    mdsOrderDetailsProductID: TIntegerField;
    mdsOrderDetailsTrademarkID: TIntegerField;
    mdsOrderDetailsPurchaseData: TDateTimeField;
    mdsOrderDetailsCount: TIntegerField;
    mdsOrderDetailsPrice: TCurrencyField;
    mdsOrderDetailsSales: TCurrencyField;
    mdsOrderDetailsSalesByTrademark: TCurrencyField;
    dsOrderDetails: TDataSource;
    Markers1: TMenuItem;
    miShowMinMarkers: TMenuItem;
    miMaxvaluemarkers: TMenuItem;
    miShowValueMarker: TMenuItem;
    miSyle: TMenuItem;
    miArea: TMenuItem;
    miLine: TMenuItem;
    miBar: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    cxImageList1: TcxImageList;
    procedure miPaymentAmountTotalByTrademarkClick(Sender: TObject);
    procedure miSeriesStyleClick(Sender: TObject);
    procedure miMarkersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
  end;

var
  fmSparklines: TfmSparklines;

implementation

{$R *.dfm}

uses
  DateUtils, DemoUtils, AboutDemoForm;

procedure TfmSparklines.miSeriesStyleClick(Sender: TObject);
var
  ASeries: TdxLookupSparklineSeriesCollection;
const
  AType: array[0..2] of TdxSparklineSeriesType = (stArea,  stLine, stBar);
begin
  MenuItemSetChecked(Sender, True);
  ASeries := TdxLookupSparklineProperties(cxgCarOrdersTableViewPayments.Properties).Series;
  ASeries[0].SeriesType := AType[TComponent(Sender).Tag];
  ASeries[1].SeriesType := AType[TComponent(Sender).Tag];
  //
  MenuItemSetChecked(miArea.Name, ASeries[0].SeriesType = stArea);
  MenuItemSetChecked(miLine.Name, ASeries[0].SeriesType = stLine);
  MenuItemSetChecked(miBar.Name, ASeries[0].SeriesType = stBar);
end;

procedure TfmSparklines.FormCreate(Sender: TObject);
begin
  inherited;
  cxgCarOrdersTableView.BeginUpdate;
  try
    mdsCarOrders.LoadFromBinaryFile('..\..\Data\CarOrders.mds');
    mdsOrderDetails.LoadFromBinaryFile('..\..\Data\OrderDetails.mds');
  finally
    cxgCarOrdersTableView.EndUpdate;
  end;
  cxgCarOrdersTableView.ViewData.Expand(True);
  cxgCarOrdersTableView.Controller.TopRowIndex := 0;
  cxgCarOrdersTableView.ViewData.Rows[0].Focused := True;
end;

procedure TfmSparklines.miMarkersClick(Sender: TObject);
var
  ASeries: TdxLookupSparklineSeriesCollection;
  AChecked: Boolean;
const
  AColors: array[Boolean] of TColor = (clNone, clDefault);
begin
  AChecked := GetMenuItemChecked(Sender);
  ASeries := TdxLookupSparklineProperties(cxgCarOrdersTableViewPayments.Properties).Series;
  case TMenuItem(Sender).Tag of
    0:
    begin
      ASeries[0].MarkerColor := AColors[AChecked];
      ASeries[1].MarkerColor := AColors[AChecked];
    end;
    1:
    begin
      ASeries[0].MinPointColor := AColors[AChecked];
      ASeries[1].MinPointColor := AColors[AChecked];
    end;
    2:
    begin
      ASeries[0].MaxPointColor := AColors[AChecked];
      ASeries[1].MaxPointColor := AColors[AChecked];
    end;
  else
  end;
end;

procedure TfmSparklines.miPaymentAmountTotalByTrademarkClick(Sender: TObject);
begin
  TdxLookupSparklineProperties(cxgCarOrdersTableViewPayments.Properties).Series[1].Visible := GetMenuItemChecked(Sender);
end;

end.
