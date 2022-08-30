unit ChartConnectionDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Variants, Forms, Dialogs,
  SysUtils, Menus, StdCtrls, Controls, cxLookAndFeels, DemoBasicMain,
  cxControls, cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, DB, cxDBData,
  Classes, cxClasses, cxPivotGridChartConnection, cxGridLevel,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, cxGridChartView;

type
  TfrmChartConnection = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    cxPivotGridChartConnection: TcxPivotGridChartConnection;
    cxGridDBTableView1: TcxGridDBTableView;
    cxGrid: TcxGrid;
    cxGridLevel: TcxGridLevel;
    cxGridChartView: TcxGridChartView;
    miChartOptions: TMenuItem;
    miSourceData: TMenuItem;
    miVisibleCells: TMenuItem;
    miSelectedCells: TMenuItem;
    miSourceForCategories: TMenuItem;
    miSourceRow: TMenuItem;
    miSourceColumn: TMenuItem;
    N3: TMenuItem;
    miChartType: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure pgfPaymentTypeGetGroupImageIndex(Sender: TcxPivotGridField;
      const AItem: TcxPivotGridViewDataItem; var AImageIndex: Integer;
      var AImageAlignHorz: TAlignment;
      var AImageAlignVert: TcxAlignmentVert);
    procedure miVisibleCellsClick(Sender: TObject);
    procedure miSourceColumnClick(Sender: TObject);
    procedure miChartColumDiagramClick(Sender: TObject);
    procedure cxGridChartViewActiveDiagramChanged(Sender: TcxGridChartView;
      ADiagram: TcxGridChartDiagram);
  private
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
    procedure InitializeDiagramList;
  end;

var
  frmChartConnection: TfrmChartConnection;

implementation

{$R *.dfm}

uses
  DemoUtils;

function TfrmChartConnection.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmChartConnection.InitializeDiagramList;
var
  ADiagram: TcxGridChartDiagram;
  AMenuItem: TMenuItem;
  I: Integer;
begin
  miChartType.Clear;
  for I := 0 to cxGridChartView.DiagramCount - 1 do
  begin
    ADiagram := cxGridChartView.Diagrams[I];
    AMenuItem := TMenuItem.Create(miChartType);
    AMenuItem.AutoCheck := True;
    AMenuItem.RadioItem := True;
    AMenuItem.Checked := ADiagram = cxGridChartView.ActiveDiagram;
    AMenuItem.OnClick := miChartColumDiagramClick;
    AMenuItem.Caption := ADiagram.DisplayText;
    AMenuItem.Tag := Integer(ADiagram);
    miChartType.Add(AMenuItem);
  end;
end;

procedure TfrmChartConnection.FormCreate(Sender: TObject);
begin
  inherited;
  PivotGrid.ApplyBestFit;
  InitializeDiagramList;
end;

procedure TfrmChartConnection.pgfPaymentTypeGetGroupImageIndex(
  Sender: TcxPivotGridField; const AItem: TcxPivotGridViewDataItem;
  var AImageIndex: Integer; var AImageAlignHorz: TAlignment;
  var AImageAlignVert: TcxAlignmentVert);
var
  Card: string;
begin
  Card := VarToStr(AItem.Value);
  if SameText(Card, 'AmEx') then
    AImageIndex := 1
  else
    if SameText(Card, 'Cash') then
      AImageIndex := 0
    else
      if SameText(Card, 'Master') then
        AImageIndex := 2
      else
        if SameText(Card, 'Visa') then
          AImageIndex := 3;
end;

procedure TfrmChartConnection.miVisibleCellsClick(Sender: TObject);
begin
  cxPivotGridChartConnection.SourceData :=
    TcxPivotGridChartViewSourceData(TComponent(Sender).Tag);
end;

procedure TfrmChartConnection.miSourceColumnClick(Sender: TObject);
begin
  cxPivotGridChartConnection.SourceForCategories :=
    TcxPivotGridChartViewSourceForCategories(TComponent(Sender).Tag);
end;

procedure TfrmChartConnection.miChartColumDiagramClick(Sender: TObject);
begin
  cxGridChartView.ActiveDiagram := TcxGridChartDiagram(TComponent(Sender).Tag);
end;

procedure TfrmChartConnection.cxGridChartViewActiveDiagramChanged(
  Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram);
var
  AItem: TObject;
begin
  AItem := dxDemoFindMenuItem(mmMain, 'miChartType');
  dxDemoCheckedItemByTag(AItem, Integer(cxGridChartView.ActiveDiagram));  
end;

end.
