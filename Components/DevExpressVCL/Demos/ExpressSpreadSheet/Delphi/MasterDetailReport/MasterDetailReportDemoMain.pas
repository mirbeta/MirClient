unit MasterDetailReportDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ReportDesignerBaseForm, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, Menus, StdCtrls, cxFilterControl,
  cxCheckBox, cxGroupBox, ExtCtrls, cxSplitter, dxSpreadSheetCore, dxSpreadSheetReportDesigner, DB, dxmdaset, ComCtrls, ReportPreviewUnit;

type
  TfrmMasterDetail = class(TfrmReportDesignerBase)
    ReportDesignerDetail1: TdxSpreadSheetReportDetail;
    ReportDesignerDetail2: TdxSpreadSheetReportDetail;
    dsMaster: TDataSource;
    mdsMaster: TdxMemData;
    mdsMasterSupplierID: TAutoIncField;
    mdsMasterCompanyName: TWideStringField;
    mdsMasterContactName: TWideStringField;
    mdsMasterContactTitle: TWideStringField;
    mdsMasterAddress: TWideStringField;
    mdsMasterCity: TWideStringField;
    mdsMasterRegion: TWideStringField;
    mdsMasterPostalCode: TWideStringField;
    mdsMasterCountry: TWideStringField;
    mdsMasterPhone: TWideStringField;
    mdsMasterFax: TWideStringField;
    mdsMasterHomePage: TWideMemoField;
    dsDetailLevel0: TDataSource;
    mdsDetailLevel0: TdxMemData;
    mdsDetailLevel0ProductID: TAutoIncField;
    mdsDetailLevel0ProductName: TWideStringField;
    mdsDetailLevel0SupplierID: TIntegerField;
    mdsDetailLevel0CategoryID: TIntegerField;
    mdsDetailLevel0QuantityPerUnit: TWideStringField;
    mdsDetailLevel0UnitPrice: TBCDField;
    mdsDetailLevel0UnitsInStock: TSmallintField;
    mdsDetailLevel0UnitsOnOrder: TSmallintField;
    mdsDetailLevel0ReorderLevel: TSmallintField;
    mdsDetailLevel0Discontinued: TBooleanField;
    mdsDetailLevel0EAN13: TWideStringField;
    dsDetailLevel1: TDataSource;
    mdsDetailLevel1: TdxMemData;
    mdsDetailLevel1OrderID: TIntegerField;
    mdsDetailLevel1ProductID: TIntegerField;
    mdsDetailLevel1UnitPrice: TBCDField;
    mdsDetailLevel1Quantity: TSmallintField;
    mdsDetailLevel1Discount: TFloatField;
    mdsDetailLevel1SubTotal: TCurrencyField;
    procedure mdsDetailLevel1CalcFields(DataSet: TDataSet);
    procedure ReportDesignerAfterBuild(Sender: TObject);
  public
    procedure Initialize; override;
  end;

var
  frmMasterDetail: TfrmMasterDetail;

implementation

{$R *.dfm}

uses
  dxHashUtils;

procedure TfrmMasterDetail.Initialize;
begin
  ReportDesigner.LoadFromFile('..\..\Data\MasterDetailTemplate.xlsx');
  LoadDataset(mdsMaster, '..\..\Data\Suppliers.mds');
  LoadDataset(mdsDetailLevel0, '..\..\Data\Products.mds');
  LoadDataset(mdsDetailLevel1, '..\..\Data\OrderReports.mds');
end;

procedure TfrmMasterDetail.mdsDetailLevel1CalcFields(DataSet: TDataSet);
begin
  mdsDetailLevel1.FieldByName('SubTotal').Value :=
    mdsDetailLevel1.FieldByName('Quantity').Value * mdsDetailLevel1.FieldByName('UnitPrice').Value;
end;

procedure TfrmMasterDetail.ReportDesignerAfterBuild(Sender: TObject);
var
  I: Integer;
  ACell: TdxSpreadSheetCell;
begin
  for I := 0 to Preview.ssResult.SheetCount - 1 do
  begin
    ACell := TdxSpreadSheetTableView(Preview.ssResult.Sheets[I]).Cells[4, 1];
    if (ACell <> nil) and (ACell.AsString <> '') then
      Preview.ssResult.Sheets[I].Caption := ACell.AsString;
  end;
end;

end.
