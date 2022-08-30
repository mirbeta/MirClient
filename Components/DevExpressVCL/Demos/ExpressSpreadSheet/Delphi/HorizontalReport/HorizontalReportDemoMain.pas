unit HorizontalReportDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ReportDesignerBaseForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, DB, dxmdaset, cxClasses, Menus, StdCtrls, cxFilterControl, cxCheckBox, cxGroupBox, ExtCtrls, cxSplitter, 
  dxSpreadSheetCore, dxSpreadSheetReportDesigner, ComCtrls;

type
  TfrmHorizontalReport = class(TfrmReportDesignerBase)
    mdsEmployees: TdxMemData;
    dsEmployees: TDataSource;
  private
    { Private declarations }
  public
    procedure Initialize; override;
  end;

var
  frmHorizontalReport: TfrmHorizontalReport;

implementation

{$R *.dfm}

procedure TfrmHorizontalReport.Initialize;
begin
  LoadDataset(mdsEmployees, '..\..\Data\Employees.mds');
  ReportDesigner.LoadFromFile('..\..\Data\HorizontalReportTemplate.xlsx');
  ReportDesigner.Options.Orientation := roHorizontal;
  inherited Initialize;
end;

end.
