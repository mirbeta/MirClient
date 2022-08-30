unit CompactLayoutMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Variants, Forms, Dialogs,
  SysUtils, Menus, StdCtrls, Controls, cxLookAndFeels, DemoBasicMain,
  cxControls, cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit, DB, cxDBData,
  Classes, cxClasses, cxLookAndFeelPainters, cxSplitter;

type
  TfrmCompactLayout = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPurchaseDate: TcxDBPivotGridField;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    miCustomization: TMenuItem;
    cxSplitter1: TcxSplitter;
    procedure FormCreate(Sender: TObject);
    procedure pgfPaymentTypeGetGroupImageIndex(Sender: TcxPivotGridField;
      const AItem: TcxPivotGridViewDataItem; var AImageIndex: Integer;
      var AImageAlignHorz: TAlignment;
      var AImageAlignVert: TcxAlignmentVert);
    procedure miCustomizationClick(Sender: TObject);
    procedure DBPivotGridCustomization(Sender: TObject);
  private
  protected
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmCompactLayout: TfrmCompactLayout;

implementation

{$R *.dfm}

uses
  DemoUtils;

function TfrmCompactLayout.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmCompactLayout.FormCreate(Sender: TObject);
begin
  inherited;
  PivotGrid.ApplyBestFit;
  PivotGrid.Customization.Site := Self;
  pgfPurchaseDate.ExpandAll;
  pgfCarName.ExpandAll;
  pgfCompanyName.ExpandAll;
  PivotGrid.Customization.Visible := True;
end;

procedure TfrmCompactLayout.pgfPaymentTypeGetGroupImageIndex(
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

procedure TfrmCompactLayout.miCustomizationClick(Sender: TObject);
begin
  inherited;
  PivotGrid.Customization.Visible := not PivotGrid.Customization.Visible;
end;

procedure TfrmCompactLayout.DBPivotGridCustomization(Sender: TObject);
begin
  inherited;
  dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miCustomization'), PivotGrid.Customization.Visible);
  if PivotGrid.Customization.Visible then
    PivotGrid.Customization.Form.Align := alLeft;
  cxSplitter1.Control := PivotGrid.Customization.Form;
  cxSplitter1.Visible := PivotGrid.Customization.Visible;
end;

end.
