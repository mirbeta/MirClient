unit DemoBasicMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxLookAndFeels, StdCtrls, Menus, cxCustomPivotGrid, cxExportPivotGridLink,
  cxClasses, ActnList;

type
  TfrmDemoBasicMain = class(TForm)
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miExport: TMenuItem;
    miExportToExcel: TMenuItem;
    miExportToText: TMenuItem;
    miExportToHtml: TMenuItem;
    miExportToXml: TMenuItem;
    Separator1: TMenuItem;
    miExit: TMenuItem;
    miOptions: TMenuItem;
    miAbout: TMenuItem;
    SaveDialog: TSaveDialog;
    lbDescrip: TLabel;
    miTotalsPosition: TMenuItem;
    miShowTotalsForSingleValues: TMenuItem;
    miShowRowTotals: TMenuItem;
    miShowColumnTotals: TMenuItem;
    miShowColumnGrandTotals: TMenuItem;
    miShowRowGrandTotals: TMenuItem;
    miTotalsVisibility: TMenuItem;
    miShowGrandTotalsForSingleValues: TMenuItem;
    N2: TMenuItem;
    miElementsVisibility: TMenuItem;
    miShowFilterFields: TMenuItem;
    miShowColumnFields: TMenuItem;
    miShowDataFields: TMenuItem;
    miShowRowFields: TMenuItem;
    miShowFilterSeparator: TMenuItem;
    N1: TMenuItem;
    miColumnTotalsPosition: TMenuItem;
    miRowTotalsPosition: TMenuItem;
    miColumnTotalsPositionFar: TMenuItem;
    miColumnTotalsPositionNear: TMenuItem;
    miRowTotalsPositionFar: TMenuItem;
    miRowTotalsPositionNear: TMenuItem;
    miSelection: TMenuItem;
    miMultiSelect: TMenuItem;
    miHideFocusRect: TMenuItem;
    miHideSelection: TMenuItem;
    miIncludeCells: TMenuItem;
    miCrossCells: TMenuItem;
    miGrandTotalsCells: TMenuItem;
    miTotalsCells: TMenuItem;
    miRowTotalsPositionTree: TMenuItem;
    miExportToXLSX: TMenuItem;
    miTouchMode: TMenuItem;
    cxLookAndFeelController1: TcxLookAndFeelController;
    miCopyToClipboard: TMenuItem;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    miCopyToClipboardIncludeHeaders: TMenuItem;
    miCopyToClipboardColumnHeaders: TMenuItem;
    miCopyToClipboardRowHeaders: TMenuItem;
    miCopyToClipboardRowHeadersAll: TMenuItem;
    miAllCopyToClipboardRowInnermostOnly: TMenuItem;
    miCopyToClipboardColumnHeadersAll: TMenuItem;
    InnermostOnly1: TMenuItem;
    miWYSIWYG1: TMenuItem;
    miDataOnly1: TMenuItem;
    miWYSIWYG2: TMenuItem;
    miDataOnly2: TMenuItem;
    procedure miExportToClick(Sender: TObject);
    procedure miTotalsLocationClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miTotalsVisibilityClick(Sender: TObject);
    procedure miElementsVisibilityClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure miHideFocusRectClick(Sender: TObject);
    procedure miHideSelectionClick(Sender: TObject);
    procedure IncludeCellsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miTouchModeClick(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure miCopyToClipboardIncludeHeadersClick(Sender: TObject);
  private
    { Private declarations }
    procedure CopyToClipboard;
  protected
    procedure AddLookAndFeelMenu; virtual;
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; virtual;
    function GetPivotGrid: TcxCustomPivotGrid; virtual;
    function IsNativeDefaultStyle: Boolean; virtual;
    procedure SetDefaultLookAndFeel; virtual;
    procedure SyncElementsVisibilityWithMenu;
    procedure SyncTotalVisibilityWithMenu;
    procedure SyncMenuWithElementsVisibility;
    procedure SyncMenuWithOptionsSelection;
    procedure SyncMenuWithTotalsPosition;
    procedure SyncMenuWithTotalVisibility;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;

    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid;
  end;

implementation

{$R *.dfm}

uses
{$IFDEF EXPRESSSKINS}
  {$I dxSkins.inc}
  dxSkinsStrs, dxSkinsForm, dxSkinInfo,
  {$IFDEF EXPRESSPAGECONTROL}
    dxSkinscxPCPainter,
  {$ENDIF}
  {$IFDEF EXPRESSSCHEDULER}
    dxSkinscxSchedulerPainter,
  {$ENDIF}
  {$IFDEF EXPRESSDOCKINGLIBRARY}
    dxSkinsdxDockControlPainter,
  {$ENDIF}
  {$IFDEF EXPRESSNAVBAR}
    dxSkinsdxNavBarPainter,
  {$ENDIF}
  {$IFDEF EXPRESSBARS}
    dxSkinsdxBarPainter,
    dxSkinsdxStatusBarPainter,
  {$IFDEF EXPRESSPAGECONTROL}
    {$IFDEF EXPRESSEDITORS}
      dxBarSkinnedCustForm,
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
  AboutDemoForm, DemoUtils;

constructor TfrmDemoBasicMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.Title := Caption;
  PivotGrid.ApplyBestFit;
end;

procedure TfrmDemoBasicMain.AfterConstruction;
begin
  inherited;
  dxBarConvertMainMenu(mmMain);
  SyncMenuWithTotalsPosition;
  SyncMenuWithTotalVisibility;
  SyncMenuWithElementsVisibility;
  SyncMenuWithOptionsSelection;
end;

function TfrmDemoBasicMain.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := nil;
end;

procedure TfrmDemoBasicMain.SyncElementsVisibilityWithMenu;
begin
  with PivotGrid.OptionsView do
  begin
    ColumnFields := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnFields'));
    DataFields := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowDataFields'));
    FilterFields := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowFilterFields'));
    FilterSeparator := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowFilterSeparator'));
    RowFields := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowRowFields'));
  end;
end;

procedure TfrmDemoBasicMain.SyncTotalVisibilityWithMenu;
begin
  with PivotGrid.OptionsView do
  begin
    ColumnTotals := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnTotals'));
    RowTotals := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowRowTotals'));
    ColumnGrandTotals := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnGrandTotals'));
    RowGrandTotals := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowRowGrandTotals'));
    TotalsForSingleValues := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowTotalsForSingleValues'));
    GrandTotalsForSingleValues := dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miShowGrandTotalsForSingleValues'));
  end;
end;

procedure TfrmDemoBasicMain.SyncMenuWithElementsVisibility;
begin
  with PivotGrid.OptionsView do
  begin
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnFields'), ColumnFields);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowDataFields'), DataFields);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowFilterFields'), FilterFields);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowFilterSeparator'), FilterSeparator);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowRowFields'), RowFields);
  end;
end;

procedure TfrmDemoBasicMain.SyncMenuWithOptionsSelection;
begin
  with PivotGrid.OptionsSelection do
  begin
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miMultiSelect'), MultiSelect);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miCrossCells'), osiCrossCells in IncludeCells);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miGrandTotalsCells'), osiGrandTotalCells in IncludeCells);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miTotalsCells'), osiTotalCells in IncludeCells);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miHideFocusRect'), HideFocusRect);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miHideSelection'), HideSelection);
  end;
end;

procedure TfrmDemoBasicMain.SyncMenuWithTotalsPosition;
begin
  with PivotGrid.OptionsView do
  begin
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miColumnTotalsPositionFar'), ColumnTotalsLocation = ctlFar);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miColumnTotalsPositionNear'), ColumnTotalsLocation = ctlNear);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miRowTotalsPositionFar'), RowTotalsLocation = rtlFar);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miRowTotalsPositionNear'), RowTotalsLocation = rtlNear);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miRowTotalsPositionTree'), RowTotalsLocation = rtlTree);
  end;
end;

procedure TfrmDemoBasicMain.SyncMenuWithTotalVisibility;
begin
  with PivotGrid.OptionsView do
  begin
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnTotals'), ColumnTotals);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowRowTotals'), RowTotals);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowColumnGrandTotals'), ColumnGrandTotals);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowRowGrandTotals'), RowGrandTotals);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowTotalsForSingleValues'), TotalsForSingleValues);
    dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miShowGrandTotalsForSingleValues'), GrandTotalsForSingleValues);
  end;
end;

procedure TfrmDemoBasicMain.miExportToClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    case TComponent(Sender).Tag of
      2:
        cxExportPivotGridToText(SaveDialog.FileName, PivotGrid);
      3:
        cxExportPivotGridToHTML(SaveDialog.FileName, PivotGrid);
      4:
        cxExportPivotGridToXML(SaveDialog.FileName, PivotGrid);
      11:
        cxExportPivotGridDataToExcel(ChangeFileExt(SaveDialog.FileName, '.xls'), PivotGrid);
      12:
        cxExportPivotGridToExcel(SaveDialog.FileName, PivotGrid);
      51:
        cxExportPivotGridDataToExcel(ChangeFileExt(SaveDialog.FileName, '.xlsx'), PivotGrid);
      52:
        cxExportPivotGridToXLSX(SaveDialog.FileName, PivotGrid);
    end;
  end;
end;

procedure TfrmDemoBasicMain.miTotalsLocationClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0, 1:
      PivotGrid.OptionsView.ColumnTotalsLocation := TcxPivotGridColumnTotalsLocation(TComponent(Sender).Tag);
    2, 3, 4:
      PivotGrid.OptionsView.RowTotalsLocation := TcxPivotGridRowTotalsLocation(TComponent(Sender).Tag - 2);
  end;
  SyncMenuWithTotalsPosition;
end;

procedure TfrmDemoBasicMain.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmDemoBasicMain.miTotalsVisibilityClick(Sender: TObject);
begin
  SyncTotalVisibilityWithMenu;
end;

procedure TfrmDemoBasicMain.miTouchModeClick(Sender: TObject);
begin
  cxLookAndFeelController1.TouchMode := dxDemoIsMenuChecked(Sender);
end;

procedure TfrmDemoBasicMain.miElementsVisibilityClick(Sender: TObject);
begin
  SyncElementsVisibilityWithMenu;
end;

procedure TfrmDemoBasicMain.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfrmDemoBasicMain.miMultiSelectClick(Sender: TObject);
begin
  PivotGrid.OptionsSelection.MultiSelect := not PivotGrid.OptionsSelection.MultiSelect;
end;

procedure TfrmDemoBasicMain.miHideFocusRectClick(Sender: TObject);
begin
  PivotGrid.OptionsSelection.HideFocusRect := not PivotGrid.OptionsSelection.HideFocusRect;
end;

procedure TfrmDemoBasicMain.miHideSelectionClick(Sender: TObject);
begin
  PivotGrid.OptionsSelection.HideSelection := not PivotGrid.OptionsSelection.HideSelection;
end;

procedure TfrmDemoBasicMain.IncludeCellsClick(Sender: TObject);
var
  AIncludeCells: TcxPivotGridOptionsSelectionIncludes;
begin
  dxDemoMenuItemSetChecked(Sender, dxDemoIsMenuChecked(Sender));
  AIncludeCells := [];
  if dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miCrossCells')) then
    AIncludeCells := AIncludeCells + [osiCrossCells];
  if dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miGrandTotalsCells')) then
    AIncludeCells := AIncludeCells + [osiGrandTotalCells];
  if dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miTotalsCells')) then
    AIncludeCells := AIncludeCells + [osiTotalCells];
  PivotGrid.OptionsSelection.IncludeCells := AIncludeCells;
end;

procedure TfrmDemoBasicMain.FormCreate(Sender: TObject);
begin
  SetDefaultLookAndFeel;
  AddLookAndFeelMenu;
  miTouchMode.Checked := cxIsTouchModeEnabled;
end;

procedure TfrmDemoBasicMain.AddLookAndFeelMenu;
begin
  mmMain.Items.Insert(mmMain.Items.IndexOf(miAbout), CreateLookAndFeelMenuItems(mmMain.Items
    {$IFNDEF EXPRESSBARS}, TActionList.Create(Self) {$ENDIF}));
end;

function TfrmDemoBasicMain.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfOffice11;
end;

function TfrmDemoBasicMain.IsNativeDefaultStyle: Boolean;
begin
  Result := False;
end;

procedure TfrmDemoBasicMain.SetDefaultLookAndFeel;
begin
  dxDemoLookAndFeelController.NativeStyle := IsNativeDefaultStyle;
  dxDemoLookAndFeelController.Kind := GetDefaultLookAndFeelKind;
end;

procedure TfrmDemoBasicMain.CopyToClipboard;
begin
  GetPivotGrid.CopyToClipboard(False, miCopyToClipboardIncludeHeaders.Checked,
    miCopyToClipboardRowHeadersAll.Checked, miCopyToClipboardColumnHeadersAll.Checked);
end;

procedure TfrmDemoBasicMain.Action1Execute(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TfrmDemoBasicMain.miCopyToClipboardIncludeHeadersClick(Sender: TObject);
begin
  miCopyToClipboardColumnHeaders.Enabled := miCopyToClipboardIncludeHeaders.Checked;
  miCopyToClipboardRowHeaders.Enabled := miCopyToClipboardIncludeHeaders.Checked;
end;

end.
