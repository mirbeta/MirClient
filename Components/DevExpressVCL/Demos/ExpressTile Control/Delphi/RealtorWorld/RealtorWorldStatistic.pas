unit RealtorWorldStatistic;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, RealtorWorldDM, Grids, DBGrids, DBClient, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, cxGroupBox, RealtorWorldBaseFrame, RealtorWorldHomePhotosBase,
  cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, DB, cxDBData,
  cxClasses, cxGridChartView, cxGridDBChartView, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxSplitter, dxCustomTileControl, dxTileControl;

type
  TfrmStatistic = class(TfrmHomePhotosBase)
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxGroupBox2: TcxGroupBox;
    cxGroupBox3: TcxGroupBox;
    cxSplitter2: TcxSplitter;
    grHousePrice: TcxGrid;
    grHousePriceDBTableView1: TcxGridDBTableView;
    grHousePriceDBChartView1: TcxGridDBChartView;
    grHousePriceDBChartView1Series1: TcxGridDBChartSeries;
    grHousePriceLevel1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxGridDBTableView1: TcxGridDBTableView;
    cxGridDBChartView1: TcxGridDBChartView;
    cxGridDBChartSeries1: TcxGridDBChartSeries;
    cxGridLevel1: TcxGridLevel;
    grHousePriceDBChartView1Series2: TcxGridDBChartSeries;
    cxGrid2: TcxGrid;
    cxGridDBTableView2: TcxGridDBTableView;
    cxGridDBChartView2: TcxGridDBChartView;
    cxGridLevel2: TcxGridLevel;
    cxGroupBox1: TcxGroupBox;
    cxSplitter3: TcxSplitter;
    cxGridDBChartView2Series1: TcxGridDBChartSeries;
    cxStyle2: TcxStyle;
    procedure cxSplitter2BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
  private
    procedure UpdateGridHousePrice;
  protected
    procedure OnItemClick(Sender: TdxTileControlItem); override;
  end;

implementation

{$R *.dfm}

procedure TfrmStatistic.OnItemClick(Sender: TdxTileControlItem);
var
  AHousesID: Integer;
begin
  AHousesID := Sender.Tag;
  DMRealtorWorld.clHouseRating.Filter := 'HouseID = ' + IntToStr(AHousesID);
  DMRealtorWorld.clHousePrice.Filter := 'HouseID = ' + IntToStr(AHousesID);
  UpdateGridHousePrice;
  DMRealtorWorld.clHousesSimular.Filter := 'HouseID = ' + IntToStr(AHousesID);
end;

procedure TfrmStatistic.UpdateGridHousePrice;
begin
end;

procedure TfrmStatistic.cxSplitter2BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

initialization
  RegisterFrame(IDStatistic, TfrmStatistic);

end.
