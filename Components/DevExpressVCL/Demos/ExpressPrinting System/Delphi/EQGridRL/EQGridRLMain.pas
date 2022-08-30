unit EQGridRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd,
  dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, dxPSCore, ActnList, ImgList, Menus, ComCtrls,
  ToolWin, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxEdit, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridCustomView, cxGrid, cxClasses,
  cxEditRepositoryItems, dxPScxCommon, dxPScxGridLnk;

type
  TEQGridRLMainForm = class(TDemoBasicMainForm)
    edrepMain: TcxEditRepository;
    edrepCenterText: TcxEditRepositoryTextItem;
    edrepRightText: TcxEditRepositoryTextItem;
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
    tvssDevExpress: TcxGridTableViewStyleSheet;
    cxGrid: TcxGrid;
    tvPlanets: TcxGridTableView;
    tvPlanetsNAME: TcxGridColumn;
    tvPlanetsNO: TcxGridColumn;
    tvPlanetsORBITS: TcxGridColumn;
    tvPlanetsDISTANCE: TcxGridColumn;
    tvPlanetsPERIOD: TcxGridColumn;
    tvPlanetsDISCOVERER: TcxGridColumn;
    tvPlanetsDATE: TcxGridColumn;
    tvPlanetsRADIUS: TcxGridColumn;
    lvPlanets: TcxGridLevel;
    dxComponentPrinterLink1: TdxGridReportLink;
    procedure actFullExpandExecute(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CustomizeColumns;
    procedure LoadData;
  end;

var
  EQGridRLMainForm: TEQGridRLMainForm;

implementation

{$R *.dfm}

uses
  cxFormats;

procedure TEQGridRLMainForm.actFullExpandExecute(Sender: TObject);
begin
  inherited;
  tvPlanets.DataController.Groups.FullExpand;
end;

procedure TEQGridRLMainForm.actFullCollapseExecute(Sender: TObject);
begin
  inherited;
  tvPlanets.DataController.Groups.FullCollapse;
end;

procedure TEQGridRLMainForm.CustomizeColumns;
const
  cDistance = 3;
  cPeriod = 4;
  cRadius = 7;
var
  I: Integer;
begin
  dxFormatSettings.DecimalSeparator := '.';
  with tvPlanets do
  for I := 0 to ColumnCount - 1 do
    if I in [cDistance, cRadius] then
      Columns[I].DataBinding.ValueTypeClass := TcxIntegerValueType
    else
      if I in [cPeriod] then
      Columns[I].DataBinding.ValueTypeClass := TcxFloatValueType
      else
       Columns[I].DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TEQGridRLMainForm.LoadData;
const
  AFileName = 'nineplanets.txt';
  AHeaderLineCount = 2;

var
  ARecords, AValues: TStringList;
  I: Integer;

  procedure InitRecord(const Str: string);
  var
    J: Integer;
    V: Variant;
  begin
    AValues.CommaText := Str;
    for J := 0 to AValues.Count - 1 do
     if AValues.Strings[J] <> '-' then
     begin
      V := AValues.Strings[J];
      if not VarIsNull(V) then
      begin
        if tvPlanets.Columns[J].DataBinding.ValueTypeClass = TcxFloatValueType then
          V := StrToFloat(V);
        tvPlanets.DataController.Values[I, J] := V;
      end;
     end;
  end;

begin
  if not FileExists(AFileName) then
    raise Exception.Create('Data file not found');

  ARecords := TStringList.Create;
  AValues := TStringList.Create;

  with ARecords do
  try
    LoadFromFile(AFileName);
    tvPlanets.BeginUpdate;
    tvPlanets.DataController.RecordCount := Count - AHeaderLineCount;
    for I := 0 to Count - (AHeaderLineCount + 1) do
      InitRecord(Strings[I + AHeaderLineCount]);
  finally
    tvPlanets.EndUpdate;
    ARecords.Free;
    AValues.Free;
  end;
end;

procedure TEQGridRLMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  CustomizeColumns;
  LoadData;
end;

procedure TEQGridRLMainForm.FormShow(Sender: TObject);
begin
  tvPlanets.DataController.Groups.FullExpand;
end;

end.
