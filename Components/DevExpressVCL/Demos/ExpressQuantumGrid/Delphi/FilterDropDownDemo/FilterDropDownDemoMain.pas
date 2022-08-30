unit FilterDropDownDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib, dxDateRanges, dxFilterPopupWindow,
  dxmdaset, dxFilterValueContainer, cxCurrencyEdit;

type
  TfrmMain = class(TfmBaseForm)
    alAction: TActionList;
    Grid: TcxGrid;
    TableView: TcxGridDBTableView;
    GridLevel1: TcxGridLevel;
    TableViewRecId: TcxGridDBColumn;
    TableViewID: TcxGridDBColumn;
    TableViewTrademark: TcxGridDBColumn;
    TableViewName: TcxGridDBColumn;
    TableViewModification: TcxGridDBColumn;
    TableViewPrice: TcxGridDBColumn;
    TableViewMPGCity: TcxGridDBColumn;
    TableViewMPGHighway: TcxGridDBColumn;
    TableViewBodyStyleID: TcxGridDBColumn;
    TableViewCilinders: TcxGridDBColumn;
    TableViewSalesDate: TcxGridDBColumn;
    TableViewBodyStyle: TcxGridDBColumn;
    acExcelModeApplyChangesImmediately: TAction;
    acExcelModeApplyChangesOnTabOrOKButtonClick: TAction;
    acExcelModeDateTimePageTypeTree: TAction;
    acExcelModeDateTimePageTypeList: TAction;
    acExcelModeNumericPageTypeRange: TAction;
    acExcelModeNumericPageTypeList: TAction;
    acClassicModeApplyChangesImmediately: TAction;
    acClassicModeApplyChangesOnButtonClick: TAction;
    miClassicModeMultiSelect: TMenuItem;
    acClassicModeMultiSelect: TAction;
    acExcelModeDateTimePageType: TAction;
    acExcelModeApplyChanges: TAction;
    acExcelModeNumericPageType: TAction;
    acClassicModeApplyChanges: TAction;
    acFilterPopupMode: TAction;
    miFilterPopup: TMenuItem;
    miFilterPopupMode: TMenuItem;
    miFilterPopupModeClassic: TMenuItem;
    miFilterPopupModeExcel: TMenuItem;
    acFilterPopupModeClassic: TAction;
    acFilterPopupModeExcel: TAction;
    miExcelModeApplyChanges: TMenuItem;
    miExcelModeDateTimePage: TMenuItem;
    miExcelModeNumericPage: TMenuItem;
    miExcelModeApplyChangesImmediatly: TMenuItem;
    miExcelModeTabOrOKButtonClick: TMenuItem;
    miDateTimePageTree: TMenuItem;
    miDateTimePageList: TMenuItem;
    miNumericPageTree: TMenuItem;
    miNumericPageList: TMenuItem;
    miClassicModeApplyChanges: TMenuItem;
    miClassicModeApplyChangesImmediatly: TMenuItem;
    miClassicModeApplyChangesButtonClick: TMenuItem;
    miSeparator: TMenuItem;
    procedure acFilterPopupModeExecute(Sender: TObject);
    procedure acExcelModeApplyChangesExecute(Sender: TObject);
    procedure acDateTimePageTypeExecute(Sender: TObject);
    procedure acNumericPageTypeExecute(Sender: TObject);
    procedure acClassicModeApplyChangesExecute(Sender: TObject);
    procedure acClassicModeMultiSelectExecute(Sender: TObject);
    procedure acDoNothingExecute(Sender: TObject);
  protected
    procedure UpdateFilterPopupActions;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFindPanel, Variants, AboutDemoForm;

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.acClassicModeApplyChangesExecute(Sender: TObject);
begin
  if acClassicModeApplyChangesImmediately.Checked then
    TableView.Filtering.ColumnPopup.ApplyMultiSelectChanges := fpacImmediately
  else
    TableView.Filtering.ColumnPopup.ApplyMultiSelectChanges := fpacOnButtonClick;
end;

procedure TfrmMain.acClassicModeMultiSelectExecute(Sender: TObject);
begin
  TableView.Filtering.ColumnPopup.MultiSelect := acClassicModeMultiSelect.Checked;
end;

procedure TfrmMain.acDateTimePageTypeExecute(Sender: TObject);
begin
  if acExcelModeDateTimePageTypeTree.Checked then
    TableView.Filtering.ColumnExcelPopup.DateTimeValuesPageType := dvptTree
  else
    TableView.Filtering.ColumnExcelPopup.DateTimeValuesPageType := dvptList;
end;

procedure TfrmMain.acDoNothingExecute(Sender: TObject);
begin
//do nothing
end;

procedure TfrmMain.acExcelModeApplyChangesExecute(Sender: TObject);
begin
  if acExcelModeApplyChangesImmediately.Checked then
    TableView.Filtering.ColumnExcelPopup.ApplyChanges := efacImmediately
  else
    TableView.Filtering.ColumnExcelPopup.ApplyChanges := efacOnTabOrOKButtonClick;
end;

procedure TfrmMain.acFilterPopupModeExecute(Sender: TObject);
begin
  if acFilterPopupModeClassic.Checked then
    TableView.Filtering.ColumnPopupMode := fpmClassic
  else
    TableView.Filtering.ColumnPopupMode := fpmExcel;
  UpdateFilterPopupActions;
end;

procedure TfrmMain.acNumericPageTypeExecute(Sender: TObject);
begin
  if acExcelModeNumericPageTypeRange.Checked then
    TableView.Filtering.ColumnExcelPopup.NumericValuesPageType := nvptRange
  else
    TableView.Filtering.ColumnExcelPopup.NumericValuesPageType := nvptList;
end;

procedure TfrmMain.UpdateFilterPopupActions;
var
  AModeName: string;
begin
  acExcelModeDateTimePageType.Visible := TableView.Filtering.ColumnPopupMode = fpmExcel;
  acExcelModeNumericPageType.Visible := TableView.Filtering.ColumnPopupMode = fpmExcel;
  acExcelModeApplyChanges.Visible := TableView.Filtering.ColumnPopupMode = fpmExcel;
  acClassicModeApplyChanges.Visible := TableView.Filtering.ColumnPopupMode = fpmClassic;
  acClassicModeMultiSelect.Visible := TableView.Filtering.ColumnPopupMode = fpmClassic;
  if TableView.Filtering.ColumnPopupMode = fpmExcel then
    AModeName := 'Excel'
  else
    AModeName := 'Classic';
  acFilterPopupMode.Caption := 'Mode ' + '(' + AModeName + ')';
end;

end.
