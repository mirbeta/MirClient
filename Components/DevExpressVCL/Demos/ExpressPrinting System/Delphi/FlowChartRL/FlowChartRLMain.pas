unit FlowChartRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, checklst, StdCtrls, Grids, ComCtrls, ShellAPI, ToolWin, Db, Buttons,
  dxPSCore, dxPSdxFCLnk, dxflchrt, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg,
  dxBkgnd, dxWrap, dxPrnDev, ImgList, dxPSGraphicLnk, DemoBasicMain,
  dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPScxPageControlProducer, cxGraphics, ActnList;

type
  TFlowChartRLMainForm = class(TDemoBasicMainForm)
    ImageList1: TImageList;
    dxFlowChart: TdxFlowChart;
    dxComponentPrinterLink1: TdxFlowChartReportLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FlowChartRLMainForm: TFlowChartRLMainForm;

implementation


{$R *.DFM}

end.
