unit cxTreeListRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd,
  dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd,
  dxPScxEditorProducers, dxPScxExtEditorProducers, cxDataStorage,
  dxPScxPageControlProducer, dxPSCore, ActnList, ImgList, Menus, ComCtrls,
  ToolWin, StdCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxCustomData, cxStyles, cxTL, cxTextEdit,
  cxInplaceContainer, cxEdit, cxEditRepositoryItems, cxClasses,
  dxPScxCommon, dxPScxTLLnk;

type
  TcxTreeListRLMainForm = class(TDemoBasicMainForm)
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
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    miFullExpand: TMenuItem;
    miFullCollapsing: TMenuItem;
    actFullExpand: TAction;
    actFullCollapse: TAction;
    tbtnFullCollapse: TToolButton;
    tbtnFullExpand: TToolButton;
    edrepMain: TcxEditRepository;
    edrepCenterText: TcxEditRepositoryTextItem;
    edrepRightText: TcxEditRepositoryTextItem;
    ilPlanets: TImageList;
    tlPlanets: TcxTreeList;
    clName: TcxTreeListColumn;
    clOrbitNumb: TcxTreeListColumn;
    clOrbits: TcxTreeListColumn;
    clDistance: TcxTreeListColumn;
    clPeriod: TcxTreeListColumn;
    clDiscoverer: TcxTreeListColumn;
    clDate: TcxTreeListColumn;
    clRadius: TcxTreeListColumn;
    clImageIndex: TcxTreeListColumn;
    N1: TMenuItem;
    dxComponentPrinterLink1: TcxTreeListReportLink;
    procedure actFullExpandExecute(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CustomizeColumns;
    procedure LoadData;
  end;

var
  cxTreeListRLMainForm: TcxTreeListRLMainForm;

implementation

{$R *.dfm}

uses
  cxFormats;

procedure TcxTreeListRLMainForm.actFullExpandExecute(Sender: TObject);
begin
  tlPlanets.FullExpand;
end;

procedure TcxTreeListRLMainForm.actFullCollapseExecute(Sender: TObject);
begin
  tlPlanets.FullCollapse;
end;

procedure TcxTreeListRLMainForm.CustomizeColumns;
const
  cDistance = 3;
  cPeriod = 4;
  cRadius = 7;
  cImageIndex = 8;
var
  I: Integer;
begin
  with tlPlanets do
  for I := 0 to ColumnCount - 1 do
    if I in [cDistance, cRadius, cImageIndex] then
      Columns[I].DataBinding.ValueTypeClass := TcxIntegerValueType
    else
      if I in [cPeriod] then
      Columns[I].DataBinding.ValueTypeClass := TcxFloatValueType
      else
       Columns[I].DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TcxTreeListRLMainForm.LoadData;
const
  AFileName = 'nineplanets.txt';
  AHeaderLineCount = 2;
  AParentKeyField = 2;
  AKeyField = 0;
  AImageField = 8;

var
  ARecords, AValues: TStringList;
  I: Integer;

  function AddNode(AParentNode: TcxTreeListNode;
    const ARecord: string): TcxTreeListNode;
  var
    S1: string;
    J: Integer;
    V: Variant;
  begin
    Result := AParentNode.AddChild;
    AValues.CommaText := ARecord;
    for J := 0 to AValues.Count - 1 do
      if AValues.Strings[J] <> '-' then
      begin
        S1 := AValues.Strings[J];
        if Pos('.', S1) <> 0 then
          S1[Pos('.', S1)] := dxFormatSettings.DecimalSeparator;
        V := S1;
        if not VarIsNull(V) then
          Result.Values[J] := V;
      end;
    Result.ImageIndex :=  Result.Values[AImageField];
    Result.SelectedIndex := Result.Values[AImageField];
  end;

  procedure AddNodes(AParentNode: TcxTreeListNode;
     const AParentKeyValue: string);
     function GetFieldValue(ARecord: string; AFieldIndex: Integer): string;
     begin
       AValues.CommaText := ARecord;
       Result := AValues.Strings[AFieldIndex];
     end;
  var
    J: Integer;
    ANode: TcxTreeListNode;
  begin
    for J := 0 to ARecords.Count - 1 do
      if GetFieldValue(ARecords.Strings[J], AParentKeyField) = AParentKeyValue then
      begin
        ANode := AddNode(AParentNode, ARecords.Strings[J]);
        AddNodes(ANode, GetFieldValue(ARecords.Strings[J], AKeyField));
      end;
  end;

begin
  if not FileExists(AFileName) then
    raise Exception.Create('Data file not found');

  ARecords := TStringList.Create;
  AValues := TStringList.Create;

  tlPlanets.BeginUpdate;
  with ARecords do
    try
      LoadFromFile(AFileName);
      for I := 0 to AHeaderLineCount - 1 do
        Delete(0);
      AddNodes(tlPlanets.Root, '-');
    finally
      tlPlanets.EndUpdate;
      ARecords.Free;
      AValues.Free;
    end;
end;

procedure TcxTreeListRLMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  CustomizeColumns;
  LoadData;
  tlPlanets.FullCollapse;
  tlPlanets.Root[0].Expanded := True;
end;

end.
