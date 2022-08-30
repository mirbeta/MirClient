unit OrgChartRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, checklst, StdCtrls, Grids, ComCtrls, ShellAPI, ToolWin, Db, Buttons,
  dxPSdxDBOCLnk, dxPSCore, dxPSdxOCLnk, dxdborgc, dxorgchr, dxPSGlbl,
  dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, ImgList,
  dxPSGraphicLnk, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  DemoBasicMain, dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils,
  dxPSPrVwStd, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, cxGraphics, ActnList, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxPScxDBEditorLnks, dxPSTextLnk, dxmdaset,
  DBClient, MidasLib;

type
  TOrgChartRLMainForm = class(TDemoBasicMainForm)
    PageControl1: TPageControl;
    tsOrgChart: TTabSheet;
    tsDBOrgChart: TTabSheet;
    dxDBOrgChart: TdxDbOrgChart;
    dsOCTable: TDataSource;
    dxComponentPrinterLink1: TdxOrgChartReportLink;
    dxComponentPrinterLink2: TdxDBOrgChartReportLink;
    dxOrgChart: TdxOrgChart;
    ilTree: TcxImageList;
    Table1: TClientDataSet;
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dxDBOrgChartCreateNode(Sender: TObject; Node: TdxOcNode);
  private
    function GetShape(ShapeName : String) : TdxOcShape;
    function GetImageAlign(const AAlignName: string): TdxOcImageAlign;
  public
    { Public declarations }
  end;

var
  OrgChartRLMainForm: TOrgChartRLMainForm;

implementation

{$R *.DFM}

procedure TOrgChartRLMainForm.PageControl1Change(Sender: TObject);
begin
  dxComponentPrinter.CurrentLink := dxComponentPrinter.ReportLink[TPageControl(Sender).ActivePage.PageIndex];
end;

procedure TOrgChartRLMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  Table1.Open;

  dxOrgChart.FullExpand;
  dxDBOrgChart.FullExpand;
  PageControl1Change(PageControl1);
end;

procedure TOrgChartRLMainForm.dxDBOrgChartCreateNode(Sender: TObject;
  Node: TdxOcNode);
begin
  if Table1.FindField('width').AsInteger > 50 then
    Node.Width := Table1.FindField('width').AsInteger;
  if Table1.FindField('height').AsInteger > 50 then
    Node.Height := Table1.FindField('height').AsInteger;
  Node.Shape := GetShape(Table1.FindField('type').AsString);
  Node.Color := Table1.FindField('color').AsInteger;
  Node.ImageAlign := GetImageAlign(Table1.FindField('ImageAlign').AsString);
end;

function TOrgChartRLMainForm.GetImageAlign(const AAlignName: string): TdxOcImageAlign;
const
  AlignMap: array[TdxOcImageAlign] of string = (
   'None',
   'Left-Top', 'Left-Center', 'Left-Bottom',
   'Right-Top', 'Right-Center', 'Right-Bottom',
   'Top-Left', 'Top-Center', 'Top-Right',
   'Bottom-Left', 'Bottom-Center', 'Bottom-Right');
var
  AIndex: TdxOcImageAlign;
begin
  Result := Low(TdxOcImageAlign);
  for AIndex := Low(TdxOcImageAlign) to High(TdxOcImageAlign) do
    if SameText(AlignMap[AIndex], AAlignName) then
    begin
      Result := AIndex;
      Break;
    end;
end;

function TOrgChartRLMainForm.GetShape(ShapeName : String) : TdxOcShape;
const ShapeArray: array[0..3] of string = ('Rectange', 'Round Rect', 'Ellipse', 'Diamond');
var i : integer;
begin
  Result := TdxOcShape(0);
  for i := 0 to 3 do
    if AnsiUpperCase(ShapeArray[i]) = AnsiUpperCase(ShapeName) then  begin
      Result := TdxOcShape(i);
      break;
    end;
end;


end.
