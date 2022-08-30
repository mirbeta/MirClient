unit SimpleReportDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ReportDesignerBaseForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxClasses, Menus, StdCtrls, cxFilterControl, cxCheckBox,  cxGroupBox, ExtCtrls, cxSplitter, dxSpreadSheetCore,
  dxSpreadSheetReportDesigner, dxSpreadSheet, DB, dxmdaset, ComCtrls;


type
  TfrmSimpleReport = class(TfrmReportDesignerBase)
    dsOrders: TDataSource;
    mdsOrders: TdxMemData;
  private
    { Private declarations }
  public
    procedure BeforePreview(ASpreadSheet: TdxSpreadSheet); override;
    procedure Initialize; override;
  end;

var
  frmSimpleReport: TfrmSimpleReport;

implementation

{$R *.dfm}

{ TfrmSimpleReport }

procedure TfrmSimpleReport.BeforePreview(ASpreadSheet: TdxSpreadSheet);
begin
  //
end;

procedure TfrmSimpleReport.Initialize;
begin
  LoadDataset(mdsOrders, '..\..\Data\OrderDetails.mds');
  ReportDesigner.LoadFromFile('..\..\Data\SimpleReportTemplate.xlsx');
  inherited Initialize;
end;

end.
