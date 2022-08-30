unit CellLevelMultiselectDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxHyperLinkEdit, cxImageComboBox, cxDBLookupComboBox,
  cxMemo, cxGridTableView, BaseForm, cxLabel;

type
  TCellLevelMultiselectDemoMainForm = class(TfmBaseForm)
    Panel1: TPanel;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    lblSelectedRows: TcxLabel;
    lblSelectedColumns: TcxLabel;
    lblSelectedCells: TcxLabel;
    lblSelectedSummary: TcxLabel;
    styleSelected: TcxStyle;
    styleNormal: TcxStyle;
    Grid: TcxGrid;
    TableView: TcxGridTableView;
    Level: TcxGridLevel;
    procedure TableViewSelectionChanged(Sender: TcxCustomGridTableView);
    procedure TableViewCustomDrawIndicatorCell(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo;
      var ADone: Boolean);
    procedure TableViewStylesGetHeaderStyle(Sender: TcxGridTableView;
      AColumn: TcxGridColumn; var AStyle: TcxStyle);
    procedure TableViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FAnchorLinkedAreaLastColumn: TcxGridColumn;
    procedure CreateSpreadSheet;
    procedure CreateColumns;
    procedure CreateRows;
    function GetColumnCaption(Index: Integer): string;
    procedure SetColumnsSelected(AFromColumn, AToColumn: TcxGridColumn;
      ASelected: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  CellLevelMultiselectDemoMainForm: TCellLevelMultiselectDemoMainForm;

implementation

{$R *.dfm}

uses
  Variants, Dialogs, AboutDemoForm, cxVariants;

const
  RecordCount = 500;
  ColumnCount = 256;
  
constructor TCellLevelMultiselectDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSpreadSheet;
  TableView.Controller.SelectCells(
    TableView.Columns[1], TableView.Columns[5], 2, 12);
end;

procedure TCellLevelMultiselectDemoMainForm.TableViewSelectionChanged(
  Sender: TcxCustomGridTableView);

  function SelectedRowCount: Integer;
  begin
    Result := TableView.Controller.SelectedRowCount;
  end;

  function SelectedColumnCount: Integer;
  begin
    Result := TableView.Controller.SelectedColumnCount;
  end;

  function GetSummOfSelection: Integer;
  var
    I, J: Integer;
    val: Variant;
  begin
    Result := 0;
    for I := 0 to SelectedRowCount - 1 do
      for J := 0 to SelectedColumnCount - 1 do
      begin
        val := TableView.DataController.GetValue(
          TableView.Controller.SelectedRows[I].RecordIndex,
          TableView.Controller.SelectedColumns[J].Index);
        if not VarIsNull(val) then
          Inc(Result,  Integer(val));
      end;
  end;

begin
  TableView.ViewInfo.HeaderViewInfo.Update;
  lblSelectedRows.Caption :=
    FloatToStrF(SelectedRowCount, ffNumber, 15, 0);
  lblSelectedColumns.Caption  :=
    FloatToStrF(SelectedColumnCount, ffNumber, 15, 0);
  lblSelectedCells.Caption  :=
    FloatToStrF(SelectedRowCount * SelectedColumnCount, ffNumber, 15, 0);
  lblSelectedSummary.Caption := FloatToStrF(GetSummOfSelection, ffNumber, 15, 0);
end;

procedure TCellLevelMultiselectDemoMainForm.TableViewCustomDrawIndicatorCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
var
  AIndicatorViewInfo: TcxGridIndicatorRowItemViewInfo;
  ATextRect: TRect;
  AStyle: TcxStyle;
begin
  if not (AViewInfo is TcxGridIndicatorRowItemViewInfo) then
    Exit;
  ATextRect := AViewInfo.ContentBounds;
  AIndicatorViewInfo := AViewInfo as TcxGridIndicatorRowItemViewInfo;
  InflateRect(ATextRect, -2, -1);
  if AIndicatorViewInfo.GridRecord.Selected then
    AStyle := styleSelected
  else
    AStyle := styleNormal;
  Sender.LookAndFeelPainter.DrawHeader(ACanvas, AViewInfo.ContentBounds,
    ATextRect, [], cxBordersAll, cxbsNormal, taCenter, vaCenter,
    False, False, IntToStr(AIndicatorViewInfo.GridRecord.Index + 1),
    AStyle.Font, AStyle.TextColor, AStyle.Color);
  ADone := True;
end;

procedure TCellLevelMultiselectDemoMainForm.TableViewStylesGetHeaderStyle(
  Sender: TcxGridTableView; AColumn: TcxGridColumn; var AStyle: TcxStyle);
begin
  if AColumn = nil then
    Exit;
  if AColumn.Selected then
    AStyle := styleSelected
  else
    AStyle := styleNormal;
end;

procedure TCellLevelMultiselectDemoMainForm.CreateSpreadSheet;
begin
  TableView.BeginUpdate;
  try
    CreateColumns;
    CreateRows;
  finally
    TableView.EndUpdate;
  end;
end;

procedure TCellLevelMultiselectDemoMainForm.CreateColumns;
var
  I: Integer;
  AColumn: TcxGridColumn;
begin
  for I := 0 to ColumnCount - 1 do
  begin
    AColumn := TableView.CreateColumn;
    AColumn.Caption := GetColumnCaption(I);
    AColumn.HeaderAlignmentHorz := taCenter;
    AColumn.DataBinding.ValueType := 'Integer';
  end;
end;

procedure TCellLevelMultiselectDemoMainForm.CreateRows;
var
  I, J: Integer;
begin
  TableView.DataController.RecordCount := RecordCount;
  Randomize;
  for I := 0 to RecordCount - 1 do
    for J := 0 to ColumnCount - 1 do
      TableView.DataController.SetValue(I, J, Random(100));
end;

function TCellLevelMultiselectDemoMainForm.GetColumnCaption(Index: Integer): string;
const
  AlphabetSymbolCount = Integer('Z') - Integer('A') + 1;
begin
  if Index div AlphabetSymbolCount > 0 then
     Result := GetColumnCaption(Index div AlphabetSymbolCount - 1)
  else Result := '';
  Result := Result + char(Integer('A') + Index mod AlphabetSymbolCount);
end;

procedure TCellLevelMultiselectDemoMainForm.SetColumnsSelected(
  AFromColumn, AToColumn: TcxGridColumn; ASelected: Boolean);
var
  I: Integer;
  AFromColIndex, AToColIndex: Integer;
begin
  AFromColIndex := AFromColumn.VisibleIndex;
  AToColIndex := AToColumn.VisibleIndex;
  if AFromColIndex > AToColIndex then
  begin
    I := AToColIndex;
    AToColIndex := AFromColIndex;
    AFromColIndex := I;
  end;
  TableView.BeginUpdate;
  try
    for I := AFromColIndex to AToColIndex do
      TableView.VisibleColumns[I].Selected := ASelected;
  finally
    TableView.EndUpdate;
  end;
  TableViewSelectionChanged(nil);
end;

procedure TCellLevelMultiselectDemoMainForm.TableViewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
  AColumn: TcxGridColumn;
begin
  AHitTest := TableView.GetHitTest(X, Y);
  if not (AHitTest is TcxGridColumnHeaderHitTest) then
    Exit;
  AColumn := (AHitTest as TcxGridColumnHeaderHitTest).Column;
  TableView.BeginUpdate;
  try
    if ssShift in Shift then
    begin
      if FAnchorLinkedAreaLastColumn = nil then
        TableView.Controller.ClearSelection
      else
        SetColumnsSelected(TableView.Controller.CellSelectionAnchor,
          FAnchorLinkedAreaLastColumn, False);
      SetColumnsSelected(TableView.Controller.CellSelectionAnchor, AColumn, True);
    end
    else
    begin
      if ssCtrl in Shift then
        SetColumnsSelected(AColumn, AColumn, True)
      else
        TableView.Controller.SelectColumns(AColumn, AColumn);
      AColumn.Focused := True;
      TableView.Controller.CellSelectionAnchor := AColumn;
    end;
    TableView.DataController.SelectAll;
    FAnchorLinkedAreaLastColumn := AColumn;
  finally
    TableView.EndUpdate;
  end;
end;

end.
