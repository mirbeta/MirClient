unit TeeChartRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, checklst, StdCtrls, Grids, ComCtrls, ShellAPI,
   ToolWin, Db, Buttons,
  Series, DBChart, TeEngine, ExtCtrls, TeeProcs, Chart, dxPSDBTCLnk,
  dxPSCore, dxPSTCLnk, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, ImgList, dxPSGraphicLnk, dxPSCompsProvider,
  dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore, dxPSPDFExport,
  cxDrawTextUtils, dxPSPrVwStd, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPScxPageControlProducer, DemoBasicMain,
  cxGraphics, ActnList, dxPSPrVwAdv, dxPSPrVwRibbon, dxmdaset;

type
  TTeeChartRLMainForm = class(TDemoBasicMainForm)
    PageControl1: TPageControl;
    tsTeeChart: TTabSheet;
    tsDBTeeChart: TTabSheet;
    TeeChart: TChart;
    Series5: TBarSeries;
    Series7: TBarSeries;
    Series6: TBarSeries;
    DBChart: TDBChart;
    PieSeries1: TPieSeries;
    DataSource1: TDataSource;
    dxComponentPrinterLink1: TdxTeeChartReportLink;
    dxComponentPrinterLink2: TdxDBTeeChartReportLink;
    mdTeeChart: TdxMemData;
    mdTeeChartNAME: TStringField;
    mdTeeChartSIZE: TSmallintField;
    mdTeeChartWEIGHT: TSmallintField;
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  TeeChartRLMainForm: TTeeChartRLMainForm;

implementation

{$R *.DFM}

procedure TTeeChartRLMainForm.PageControl1Change(Sender: TObject);
var
  t: Integer;
begin
  dxComponentPrinter.CurrentLink := dxComponentPrinter.ReportLink[TPageControl(Sender).ActivePage.PageIndex];
  with TeeChart do
  begin
    AnimatedZoom:=True;
    AnimatedZoomSteps:=4;
    for t:=0 to SeriesCount-1 do
    with Series[t] do
         FillSampleValues(NumSampleValues);
    UndoZoom;
  end;

end;

procedure TTeeChartRLMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  mdTeeChart.LoadFromBinaryFile('..\..\Data\DBTreeChart.dat');
  mdTeeChart.Open;
  PageControl1Change(PageControl1);
end;

end.
