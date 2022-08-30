unit RealtorWorldResearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, cxCustomData, cxStyles, cxEdit, cxSplitter, ExtCtrls,
  cxCustomPivotGrid, cxDBPivotGrid, RealtorWorldDM, cxContainer, cxGroupBox,
  cxFilter, cxData, cxDataStorage, DB, cxDBData, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridChartView, cxGridCustomView,
  dxmdaset, cxPivotGridCustomDataSet, cxPivotGridSummaryDataSet,
  cxGridLevel, cxGrid, dxGDIPlusClasses, cxImage, RealtorWorldBaseFrame,
  cxGridDBChartView;

type
  TfrmResearch = class(TfrmBase)
    pgResearch: TcxDBPivotGrid;
    pgfYear: TcxDBPivotGridField;
    pgfCount: TcxDBPivotGridField;
    pgfRegion: TcxDBPivotGridField;
    pgfSeasonallyAdjusted: TcxDBPivotGridField;
    pgfStatus: TcxDBPivotGridField;
    pgfMonth: TcxDBPivotGridField;
    cxSplitter1: TcxSplitter;
    cxGroupBox1: TcxGroupBox;
    cxgChart: TcxGrid;
    cvChart: TcxGridDBChartView;
    cxGridDBChartDataGroup6: TcxGridDBChartDataGroup;
    cxGridDBChartDataGroup5: TcxGridDBChartDataGroup;
    cxGridDBChartSeries11: TcxGridDBChartSeries;
    cxGridDBChartSeries12: TcxGridDBChartSeries;
    cxGridDBChartSeries13: TcxGridDBChartSeries;
    cxGridDBChartSeries14: TcxGridDBChartSeries;
    cxGridLevel3: TcxGridLevel;
    procedure cxSplitter1BeforeClose(Sender: TObject;
      var AllowClose: Boolean);
  private
    procedure InitializeFrame;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TfrmResearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeFrame;
  cxgChart.BeginUpdate;
  try
    cvChart.DataGroups[0].ActiveValue := 'For Sale';
    cvChart.DataGroups[1].ActiveValue := 'Not Seasonally Adjus';
    cvChart.ActiveDataLevel := 2;
  finally
    cxgChart.EndUpdate;
  end;
end;

procedure TfrmResearch.InitializeFrame;
var
  I: Integer;
begin
  pgResearch.BeginUpdate;
  try
    for I := 0 to pgResearch.FieldCount - 1 do
      pgResearch.Fields[I].ExpandAll;
    pgfRegion.Filter.Values.Add(Null);
  finally
    pgResearch.EndUpdate;
  end;
  pgfRegion.ApplyBestFit;
end;

procedure TfrmResearch.cxSplitter1BeforeClose(Sender: TObject;
  var AllowClose: Boolean);
begin
  AllowClose := False;
end;

initialization
  RegisterFrame(IDResearch, TfrmResearch);

end.
