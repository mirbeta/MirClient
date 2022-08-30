unit OutlineDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSpreadSheetCore, dxSpreadSheetFunctions, dxSpreadSheetGraphics,
  dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheet, BaseForm, dxCore, dxCoreClasses, dxHashUtils, 
  dxSpreadSheetCoreHistory, dxSpreadSheetPrinting, dxSpreadSheetFormulas, cxClasses, ComCtrls,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers,
  dxSpreadSheetHyperlinks, dxSpreadSheetUtils, dxSpreadSheetCoreStyles, dxSpreadSheetCoreStrs, dxSpreadSheetStyles,
  cxSplitter, dxSpreadSheetFormulaBar;

type
  TfrmOutline = class(TfmBaseForm)
    miGroupColumns: TMenuItem;
    miGrouping: TMenuItem;
    miGroupRows: TMenuItem;
    miLine2: TMenuItem;
    miUngroupColumns: TMenuItem;
    miUngroupRows: TMenuItem;
    SpreadSheet: TdxSpreadSheet;
    GroupExpandButtonPosition1: TMenuItem;
    N2: TMenuItem;
    miGroupStart: TMenuItem;
    miGroupFinish: TMenuItem;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;

    procedure FormCreate(Sender: TObject);
    procedure miGroupColumnsClick(Sender: TObject);
    procedure miGroupRowsClick(Sender: TObject);
    procedure miUngroupColumnsClick(Sender: TObject);
    procedure miUngroupRowsClick(Sender: TObject);
    procedure miGroupFinishClick(Sender: TObject);
  public
    procedure AfterConstruction; override;
    function GetSpreadSheet: TdxSpreadSheet; override;
    procedure UpdateExpandButtonState;
  end;

var
  frmOutline: TfrmOutline;

implementation

{$R *.dfm}

{ TfrmOutline }

procedure TfrmOutline.AfterConstruction;
begin
  inherited;
  UpdateExpandButtonState;
end;

procedure TfrmOutline.FormCreate(Sender: TObject);
begin
  inherited;
  SpreadSheet.LoadFromFile('..\..\Data\OutlineGrouping.xlsx');
  WindowState := wsMaximized;
end;

function TfrmOutline.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

procedure TfrmOutline.miGroupColumnsClick(Sender: TObject);
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := SpreadSheet.ActiveSheetAsTable;
  if ASheet.Selection.Count > 0 then
    ASheet.Columns.Groups.Add(ASheet.Selection.Area.Left, ASheet.Selection.Area.Right);
end;

procedure TfrmOutline.miGroupFinishClick(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.Rows.Groups.ExpandButtonPosition :=
    TdxSpreadSheetTableItemGroupExpandButtonPosition(TComponent(Sender).Tag);
  SpreadSheet.ActiveSheetAsTable.Columns.Groups.ExpandButtonPosition :=
    TdxSpreadSheetTableItemGroupExpandButtonPosition(TComponent(Sender).Tag);
  UpdateExpandButtonState;
end;

procedure TfrmOutline.miGroupRowsClick(Sender: TObject);
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := SpreadSheet.ActiveSheetAsTable;
  if ASheet.Selection.Count > 0 then
    ASheet.Rows.Groups.Add(ASheet.Selection.Area.Top, ASheet.Selection.Area.Bottom);
end;

procedure TfrmOutline.miUngroupColumnsClick(Sender: TObject);
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := SpreadSheet.ActiveSheetAsTable;
  if ASheet.Selection.Count > 0 then
    ASheet.Columns.Groups.Delete(ASheet.Selection.Area.Left, ASheet.Selection.Area.Right);
end;

procedure TfrmOutline.miUngroupRowsClick(Sender: TObject);
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := SpreadSheet.ActiveSheetAsTable;
  if ASheet.Selection.Count > 0 then
    ASheet.Rows.Groups.Delete(ASheet.Selection.Area.Top, ASheet.Selection.Area.Bottom);
end;

procedure TfrmOutline.UpdateExpandButtonState;
begin
  MenuItemSetChecked('miGroupFinish', SpreadSheet.ActiveSheetAsTable.Rows.Groups.ExpandButtonPosition = gebpGroupFinish);
  MenuItemSetChecked('miGroupStart', SpreadSheet.ActiveSheetAsTable.Rows.Groups.ExpandButtonPosition = gebpGroupStart);
end;

end.
