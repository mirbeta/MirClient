unit DataSetsMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, DemoBasicDM, cxClasses, cxGraphics, cxCustomData,
  cxStyles, cxCustomPivotGrid, cxDBPivotGrid, cxControls, cxLookAndFeels,
  Menus, StdCtrls, DB, cxDBData, dxmdaset, cxPivotGridSummaryDataSet,
  cxGridCustomView, cxGridChartView, cxGridDBChartView, cxGridLevel, cxGrid,
  cxFilter, cxData, cxDataStorage, cxEdit, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxPivotGridCustomDataSet,
  cxPivotGridDrillDownDataSet, cxGeometry, cxLookAndFeelPainters, dxSparkline, cxNavigator;

const
  CM_APPLYBESTFIT = WM_USER + 1;
  CM_UPDATEGRIDVIEW = WM_USER + 2;

type
  TfrmDataSets = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    SummaryChartLevel: TcxGridLevel;
    Grid: TcxGrid;
    SummaryChartView: TcxGridDBChartView;
    PivotGridSummaryDataSet: TcxPivotGridSummaryDataSet;
    PivotGridSummaryDataSource: TDataSource;
    SummaryTableLevel: TcxGridLevel;
    SummaryTableView: TcxGridDBTableView;
    PivotGridDrillDownDataSet: TcxPivotGridDrillDownDataSet;
    PivotGridDrillDownDataSource: TDataSource;
    DrillDownTableLevel: TcxGridLevel;
    DrillDownTableView: TcxGridDBTableView;
    procedure GridActiveTabChanged(Sender: TcxCustomGrid;
      ALevel: TcxGridLevel);
    procedure PivotGridDataSetDataChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PivotGridDataSetCreateField(
      Sender: TcxPivotGridCustomDataSet;
      APivotGridField: TcxPivotGridField; ADataSetField: TField);
    procedure pgfPaymentAmountCalculateCustomSummary(Sender: TcxPivotGridField; ASummary: TcxPivotGridCrossCellSummary);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CreateFooterSummary(ATableView: TcxGridTableView);
    procedure UpdateSummaryChartView;
    procedure UpdateSummaryTableView;
    procedure UpdateDrillDownTableView;
  protected
    SparklineTemplate: TdxSparklineProperties;
    procedure CMApplyBestFit(var Message: TMessage); message CM_APPLYBESTFIT;
    procedure CMUpdateGridView(var Message: TMessage); message CM_UPDATEGRIDVIEW;
    function GetPivotGrid: TcxCustomPivotGrid; override;
  end;

var
  frmDataSets: TfrmDataSets;

implementation

{$R *.dfm}

procedure TfrmDataSets.FormCreate(Sender: TObject);
begin
  inherited;
  PivotGrid.ViewData.FocusedCell := cxNullPoint;
  SparklineTemplate := TdxSparklineProperties.Create(Self);
  SparklineTemplate.Assign(pgfPaymentAmount.Properties);
end;

procedure TfrmDataSets.FormDestroy(Sender: TObject);
begin
  SparklineTemplate.Free;
  inherited;
end;

function TfrmDataSets.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmDataSets.UpdateSummaryChartView;
var
  I: Integer;
begin
  SummaryChartView.BeginUpdate;
  try
    SummaryChartView.ClearSeries;
    SummaryChartView.ClearDataGroups;
    for I := 0 to PivotGridSummaryDataSet.FieldCount - 1 do
    begin
      case TcxPivotGridFieldArea(PivotGridSummaryDataSet.Fields[I].Tag - 1) of
        faColumn, faRow:
          SummaryChartView.CreateDataGroup.DataBinding.FieldName := PivotGridSummaryDataSet.Fields[I].FieldName;
        faData:
          SummaryChartView.CreateSeries.DataBinding.FieldName := PivotGridSummaryDataSet.Fields[I].FieldName;
      end;
    end;
  finally
    SummaryChartView.EndUpdate;
  end;
end;

procedure TfrmDataSets.CreateFooterSummary(ATableView: TcxGridTableView);

  procedure CreateFooterSummaryCell(AGridColumn: TcxGridColumn);
  begin
    AGridColumn.Summary.FooterKind := skSum;
    AGridColumn.Summary.FooterFormat := ',.00';
  end;

  function ColumnByCaption(ACaption: string): TcxGridColumn;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ATableView.ColumnCount - 1 do
      if ATableView.Columns[I].Caption = ACaption then
      begin
        Result := ATableView.Columns[I];
        Break;
      end;
  end;

var
  I: Integer;
begin
  for I := 0 to PivotGrid.FieldCount - 1 do
  begin
    if (PivotGrid.Fields[I].Area = faData) then
      if not PivotGrid.Fields[I].DataBinding.ValueTypeClass.IsString then
        CreateFooterSummaryCell(ColumnByCaption(PivotGrid.Fields[I].Caption));
  end;
end;

procedure TfrmDataSets.UpdateSummaryTableView;
begin
  SummaryTableView.BeginUpdate;
  try
    SummaryTableView.ClearItems;
    SummaryTableView.DataController.CreateAllItems;
    CreateFooterSummary(SummaryTableView);
  finally
    SummaryTableView.EndUpdate;
  end;
  if HandleAllocated then
    PostMessage(Handle, CM_APPLYBESTFIT, Integer(SummaryTableView), 0)
end;

procedure TfrmDataSets.UpdateDrillDownTableView;
begin
  DrillDownTableView.BeginUpdate;
  try
    DrillDownTableView.ClearItems;
    DrillDownTableView.DataController.CreateAllItems;
    CreateFooterSummary(DrillDownTableView);
  finally
    DrillDownTableView.EndUpdate;
  end;

  if HandleAllocated then
    PostMessage(Handle, CM_APPLYBESTFIT, Integer(DrillDownTableView), 0);
end;

procedure TfrmDataSets.CMApplyBestFit(var Message: TMessage);
begin
  if TObject(Message.WParam) is TcxGridDBTableView then
    TcxGridDBTableView(Message.WParam).ApplyBestFit;
end;

procedure TfrmDataSets.CMUpdateGridView(var Message: TMessage);
begin
  case Message.WParam of
    0: UpdateSummaryTableView;
    1: UpdateSummaryChartView;
    2: UpdateDrillDownTableView;
  end;
end;

procedure TfrmDataSets.GridActiveTabChanged(Sender: TcxCustomGrid;
  ALevel: TcxGridLevel);
begin
  inherited;

  if (ALevel.Tag <> 2) and PivotGridSummaryDataSet.SynchronizeData then
    PivotGridDataSetDataChanged(nil);

  PivotGridDrillDownDataSet.SynchronizeData := ALevel.Tag = 2;
  PivotGridSummaryDataSet.SynchronizeData := not PivotGridDrillDownDataSet.SynchronizeData;
end;

procedure TfrmDataSets.pgfPaymentAmountCalculateCustomSummary(Sender: TcxPivotGridField;
  ASummary: TcxPivotGridCrossCellSummary);
var
  I: Integer;
  ARecords: TcxPivotGridRecords;
begin
  if SparklineTemplate = nil then
    Exit;
  ARecords := ASummary.Owner.Records;
  SparklineTemplate.DataController.RecordCount := ARecords.Count;
  for I := 0 to ARecords.Count - 1 do
    SparklineTemplate.DataController.Values[I, SparklineTemplate.Series[0].DataIndex] :=
      GetPivotGrid.DataController.Values[ARecords[I], pgfPaymentAmount.Index];
  ASummary.Custom := SparklineTemplate.DataValue;
end;

procedure TfrmDataSets.PivotGridDataSetDataChanged(
  Sender: TObject);
begin
  inherited;

  if HandleAllocated then
    PostMessage(Handle, CM_UPDATEGRIDVIEW, Grid.ActiveLevel.Tag, 0);
end;

procedure TfrmDataSets.PivotGridDataSetCreateField(
  Sender: TcxPivotGridCustomDataSet; APivotGridField: TcxPivotGridField;
  ADataSetField: TField);
begin
  inherited;
  if ADataSetField is TStringField then
    ADataSetField.Size := TcxDBPivotGridField(APivotGridField).DataBinding.DBField.Size;
end;

end.
