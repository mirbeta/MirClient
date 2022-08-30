unit UnboundModeDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, DemoBasicMain, cxContainer, cxEdit, cxTextEdit, cxStyles, cxTL,
  cxInplaceContainer, cxEditRepositoryItems, cxGraphics, cxCustomData;

type
  TUnboundModeDemoMainForm = class(TDemoBasicMainForm)
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
    ilPlanets: TImageList;
    edrepMain: TcxEditRepository;
    edrepCenterText: TcxEditRepositoryTextItem;
    edrepRightText: TcxEditRepositoryTextItem;
    miSeparator1: TMenuItem;
    miDropNodeIndicator: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tlPlanetsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure miDropNodeIndicatorClick(Sender: TObject);
  private
    procedure CustomizeColumns;
    procedure LoadData;
  end;

var
  UnboundModeDemoMainForm: TUnboundModeDemoMainForm;

implementation


uses 
  UnboundModeDemoData, ShellAPI, cxDataStorage, cxFormats, SkinDemoUtils;

{$R *.dfm}

procedure TUnboundModeDemoMainForm.CustomizeColumns;
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

procedure TUnboundModeDemoMainForm.LoadData;
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

procedure TUnboundModeDemoMainForm.FormCreate(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}
  inherited;
  CustomizeColumns;
  LoadData;
  tlPlanets.FullCollapse;
  tlPlanets.Root[0].Expanded := True;

//}
end;

procedure TUnboundModeDemoMainForm.FormShow(Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code

  ShowMessage('WARNING: tutorial not completed. First, please apply the steps '+
              'shown in the doc file');

//}
end;

procedure TUnboundModeDemoMainForm.tlPlanetsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited;
  //
end;

procedure TUnboundModeDemoMainForm.miDropNodeIndicatorClick(
  Sender: TObject);
begin
  tlPlanets.OptionsView.DropNodeIndicator := GetMenuItemChecked(Sender);
end;

end.
