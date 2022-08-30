unit MultipleTotalsMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxLookAndFeels, Menus, StdCtrls, cxControls,
  cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles, cxClasses,
  cxGraphics, cxCustomData, Variants;

type
  TfrmMultipleTotals = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseMonth: TcxDBPivotGridField;
    cxDBPivotGrid1PaymentType: TcxDBPivotGridField;
    cxDBPivotGrid1Quantity: TcxDBPivotGridField;
    cxDBPivotGrid1CarName: TcxDBPivotGridField;
    cxDBPivotGrid1UnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    cxDBPivotGrid1PaymentAmount: TcxDBPivotGridField;
    pgfPurchaseQuarter: TcxDBPivotGridField;
    procedure PivotGridStylesGetGroupHeaderStyle(Sender: TcxCustomPivotGrid;
      AItem: TcxPivotGridViewDataItem; var AStyle: TcxStyle);
    procedure CalculateCustomSummary(
      Sender: TcxPivotGridField; ASummary: TcxPivotGridCrossCellSummary);
    procedure PivotGridStylesGetContentStyle(Sender: TcxCustomPivotGrid;
      ACell: TcxPivotGridDataCellViewInfo; var AStyle: TcxStyle);
  private
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmMultipleTotals: TfrmMultipleTotals;

implementation

{$R *.dfm}

function TfrmMultipleTotals.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmMultipleTotals.PivotGridStylesGetGroupHeaderStyle(
  Sender: TcxCustomPivotGrid; AItem: TcxPivotGridViewDataItem;
  var AStyle: TcxStyle);
begin
   if AItem.IsTotal or not AItem.Expanded and (AItem.Field = pgfCompanyName) then
     AStyle := dmOrders.stBoldBlueFont;
end;

procedure TfrmMultipleTotals.PivotGridStylesGetContentStyle(
  Sender: TcxCustomPivotGrid; ACell: TcxPivotGridDataCellViewInfo;
  var AStyle: TcxStyle);
begin
  if ACell.SummaryType = stCustom then
    AStyle := dmOrders.stBoldRedFont;
end;

procedure TfrmMultipleTotals.CalculateCustomSummary(
  Sender: TcxPivotGridField; ASummary: TcxPivotGridCrossCellSummary);

  function VarToDouble(const AValue: Variant): Double;
  begin
    Result := 0;
    if not VarIsNull(AValue) then
      Result := AValue;
  end;

var
  ARow, AColumn: TcxPivotGridGroupItem;
  AGrandTotalValue, ASum: Double;

begin
  if pgfCompanyName.Area = faRow then
  begin
    ARow := ASummary.Owner.Row;
    AColumn := ASummary.Owner.Column;
  end
  else // rotated to 90 degrees
  begin
    ARow := ASummary.Owner.Column;
    AColumn := ASummary.Owner.Row;
  end;
  if AColumn.Parent = nil then
    ASummary.Custom := 100
  else
  begin
    AGrandTotalValue := VarToDouble(ARow.GetCellByCrossItem(
      AColumn.Parent).GetSummaryByField(Sender, stSum));
    ASum := VarToDouble(ASummary.Sum);
    if AGrandTotalValue <> 0 then
      ASummary.Custom := ASum / AGrandTotalValue  * 100
    else
      ASummary.Custom := 0;
  end;
end;



end.
