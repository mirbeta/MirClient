unit InvoiceReportDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ReportDesignerBaseForm, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxClasses, Menus, StdCtrls, cxFilterControl, cxCheckBox, cxGroupBox, ExtCtrls, cxSplitter, dxSpreadSheetCore,
  dxSpreadSheetReportDesigner, ComCtrls, DB, dxmdaset, ReportPreviewUnit, dxSpreadSheetTypes;

type
  TfrmInvoiceReport = class(TfrmReportDesignerBase)
    mdsInvoice: TdxMemData;
    dsInvoice: TDataSource;
    mdsInvoiceShipName: TWideStringField;
    mdsInvoiceShipAddress: TWideStringField;
    mdsInvoiceShipCity: TWideStringField;
    mdsInvoiceShipRegion: TWideStringField;
    mdsInvoiceShipPostalCode: TWideStringField;
    mdsInvoiceShipCountry: TWideStringField;
    mdsInvoiceCustomerID: TWideStringField;
    mdsInvoiceCustomers_CompanyName: TWideStringField;
    mdsInvoiceAddress: TWideStringField;
    mdsInvoiceCity: TWideStringField;
    mdsInvoiceRegion: TWideStringField;
    mdsInvoicePostalCode: TWideStringField;
    mdsInvoiceCountry: TWideStringField;
    mdsInvoiceSalesperson: TWideStringField;
    mdsInvoiceOrderID: TAutoIncField;
    mdsInvoiceOrderDate: TDateTimeField;
    mdsInvoiceRequiredDate: TDateTimeField;
    mdsInvoiceShippedDate: TDateTimeField;
    mdsInvoiceShippers_CompanyName: TWideStringField;
    mdsInvoiceProductID: TIntegerField;
    mdsInvoiceProductName: TWideStringField;
    mdsInvoiceUnitPrice: TBCDField;
    mdsInvoiceQuantity: TSmallintField;
    mdsInvoiceDiscount: TFloatField;
    mdsInvoiceExtendedPrice: TBCDField;
    mdsInvoiceFreight: TBCDField;
    procedure ReportDesignerAfterBuild(Sender: TObject);
  private
    { Private declarations }
  public
     procedure Initialize; override;
  end;

var
  frmInvoiceReport: TfrmInvoiceReport;

implementation

{$R *.dfm}

uses
  dxHashUtils;

procedure TfrmInvoiceReport.Initialize;
begin
  ReportDesigner.LoadFromFile('..\..\Data\InvoiceTemplate.xlsx');
  LoadDataset(mdsInvoice, '..\..\Data\Invoices.mds');
  inherited Initialize;
end;

procedure TfrmInvoiceReport.ReportDesignerAfterBuild(Sender: TObject);
var
  I: Integer;
  ACell: TdxSpreadSheetCell;
begin
  for I := 0 to Preview.ssResult.SheetCount - 1 do
  begin
    ACell := TdxSpreadSheetTableView(Preview.ssResult.Sheets[I]).Cells[15, 1];
    if (ACell <> nil) and (ACell.DataType = cdtInteger) then
      Preview.ssResult.Sheets[I].Caption := Format('Order ID %d', [ACell.AsInteger]);
  end;
end;

end.
