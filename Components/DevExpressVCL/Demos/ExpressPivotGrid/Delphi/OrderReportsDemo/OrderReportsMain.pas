unit OrderReportsMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Variants, Forms, Dialogs,
  SysUtils, Menus, StdCtrls, Controls, cxLookAndFeels, DemoBasicMain,
  cxControls, cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, DB, cxDBData,
  Classes, cxClasses;

type
  TfrmOrderReport = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    procedure FormCreate(Sender: TObject);
    procedure pgfPaymentTypeGetGroupImageIndex(Sender: TcxPivotGridField;
      const AItem: TcxPivotGridViewDataItem; var AImageIndex: Integer;
      var AImageAlignHorz: TAlignment;
      var AImageAlignVert: TcxAlignmentVert);
  private
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmOrderReport: TfrmOrderReport;

implementation

{$R *.dfm}

function TfrmOrderReport.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmOrderReport.FormCreate(Sender: TObject);
begin
  inherited;
  PivotGrid.ApplyBestFit;
end;

procedure TfrmOrderReport.pgfPaymentTypeGetGroupImageIndex(
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

end.
