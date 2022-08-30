unit SimpleVerticalGridDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  cxStyles, cxMaskEdit, cxBlobEdit, cxCurrencyEdit, cxSpinEdit,
  cxCheckBox, cxHyperLinkEdit, cxEditRepositoryItems, cxEdit,
  cxLookAndFeels, ActnList, ImgList, Menus, cxInplaceContainer,
  cxControls, ComCtrls, StdCtrls, DemoBasicMain, cxGraphics,
  cxVGrid, cxDBVGrid, DB, DBClient, cxClasses, cxImage,
  cxLookAndFeelPainters;
                                                                
type
  TSimpleVerticalGridDemoMainForm = class(TDemoBasicMainForm)
    miView: TMenuItem;
    miBehavior: TMenuItem;
    miSeparator2: TMenuItem;
    miExplorerStyleCategory: TMenuItem;
    miHeaders: TMenuItem;
    miGridLines: TMenuItem;
    miIncSearch: TMenuItem;
    miImmediateEditor: TMenuItem;
    ImageList: TImageList;
    cxEditRepository1: TcxEditRepository;
    cxEditRepository1ImageComboBoxItem1: TcxEditRepositoryImageComboBoxItem;
    cxEditRepository1ImageComboBoxItem2: TcxEditRepositoryImageComboBoxItem;
    cxEditRepository1CalcItem1: TcxEditRepositoryCalcItem;
    cxDBVerticalGrid: TcxDBVerticalGrid;
    actHeaders: TAction;
    actGridLines: TAction;
    actExplorerStyleCategory: TAction;
    actLayoutStyleBandsView: TAction;
    actLayoutStyleMultiRecordView: TAction;
    actLayoutStyleSingleRecordView: TAction;
    LayoutStyle1: TMenuItem;
    actLayoutStyleBandsView1: TMenuItem;
    actLayoutStyleMultiRecordView1: TMenuItem;
    actLayoutStyleSingleRecordView1: TMenuItem;
    actBandSizing: TAction;
    BandSizing1: TMenuItem;
    actCellHints: TAction;
    CellHints1: TMenuItem;
    actRowSizing: TAction;
    RowSizing1: TMenuItem;
    actImmediateEditor: TAction;
    fldTrademark: TcxDBEditorRow;
    fldModel: TcxDBEditorRow;
    fldCategory: TcxDBEditorRow;
    rowPerformance_Attributes: TcxCategoryRow;
    fldHP: TcxDBEditorRow;
    fldLiter: TcxDBEditorRow;
    fldCyl: TcxDBEditorRow;
    fldTransmissSpeedCount: TcxDBEditorRow;
    fldTransmissAutomatic: TcxDBEditorRow;
    cxDBVerticalGrid1DBMultiEditorRow1: TcxDBMultiEditorRow;
    rowNotes: TcxCategoryRow;
    fldDescription: TcxDBEditorRow;
    fldHyperlink: TcxDBEditorRow;
    rowOthers: TcxCategoryRow;
    fldPrice: TcxDBEditorRow;
    fldPicture: TcxDBEditorRow;
    PaintStyle1: TMenuItem;
    Net1: TMenuItem;
    Delphi1: TMenuItem;
    cxDBVerticalGridID: TcxDBMultiEditorRow;
    procedure miExplorerStyleCategoryClick(Sender: TObject);
    procedure miHeadersClick(Sender: TObject);
    procedure miGridLinesClick(Sender: TObject);
    procedure miIncSearchClick(Sender: TObject);
    procedure cxDBVerticalGridStylesGetContentStyle(Sender: TObject;
      AEditProp: TcxCustomEditorRowProperties; AFocused: Boolean;
      ARecordIndex: Integer; var AStyle: TcxStyle);
    procedure LayOutStyleExecute(Sender: TObject);
    procedure actBandSizingExecute(Sender: TObject);
    procedure actCellHintsExecute(Sender: TObject);
    procedure actRowSizingExecute(Sender: TObject);
    procedure actImmediateEditorExecute(Sender: TObject);
    procedure actPaintStyleExecute(Sender: TObject);
  end;

var
  SimpleVerticalGridDemoMainForm: TSimpleVerticalGridDemoMainForm;

implementation

uses SimpleVerticalGridDemoData;

{$R *.dfm}

procedure TSimpleVerticalGridDemoMainForm.miExplorerStyleCategoryClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  cxDBVerticalGrid.OptionsView.CategoryExplorerStyle := TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Checked then
    cxDBVerticalGrid.Styles.Category := SimpleVerticalGridDemoMainDM.cxStyle1
  else
    cxDBVerticalGrid.Styles.Category := nil;
end;

procedure TSimpleVerticalGridDemoMainForm.miHeadersClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  cxDBVerticalGrid.OptionsView.ShowHeaders := TMenuItem(Sender).Checked;
end;

procedure TSimpleVerticalGridDemoMainForm.miGridLinesClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Checked then
    cxDBVerticalGrid.OptionsView.GridLines := vglBoth
  else
    cxDBVerticalGrid.OptionsView.GridLines := vglNone;
end;

procedure TSimpleVerticalGridDemoMainForm.miIncSearchClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  cxDBVerticalGrid.OptionsBehavior.IncSearch := TMenuItem(Sender).Checked;
end;

procedure TSimpleVerticalGridDemoMainForm.cxDBVerticalGridStylesGetContentStyle(
  Sender: TObject; AEditProp: TcxCustomEditorRowProperties;
  AFocused: Boolean; ARecordIndex: Integer; var AStyle: TcxStyle);
begin
  if ARecordIndex = cxDBVerticalGrid.DataController.FocusedRowIndex then
    AStyle := SimpleVerticalGridDemoMainDM.cxStyle8
  else
    AStyle := SimpleVerticalGridDemoMainDM.cxStyle3;
end;

procedure TSimpleVerticalGridDemoMainForm.LayOutStyleExecute(
  Sender: TObject);
begin
  if not TMenuItem(Sender).Checked then
  begin
    TMenuItem(Sender).Checked := True;
    cxDBVerticalGrid.LayoutStyle := TcxvgLayoutStyle(TMenuItem(Sender).Tag);
  end;
end;

procedure TSimpleVerticalGridDemoMainForm.actBandSizingExecute(
  Sender: TObject);
begin
  TCustomAction(Sender).Checked := not TCustomAction(Sender).Checked;
  cxDBVerticalGrid.OptionsBehavior.BandSizing := TCustomAction(Sender).Checked
end;

procedure TSimpleVerticalGridDemoMainForm.actCellHintsExecute(
  Sender: TObject);
begin
  TCustomAction(Sender).Checked := not TCustomAction(Sender).Checked;
  cxDBVerticalGrid.OptionsBehavior.CellHints := TCustomAction(Sender).Checked;
end;

procedure TSimpleVerticalGridDemoMainForm.actRowSizingExecute(
  Sender: TObject);
begin
  TCustomAction(Sender).Checked := not TCustomAction(Sender).Checked;
  cxDBVerticalGrid.OptionsBehavior.RowSizing := TCustomAction(Sender).Checked;
end;

procedure TSimpleVerticalGridDemoMainForm.actImmediateEditorExecute(
  Sender: TObject);
begin
  TCustomAction(Sender).Checked := not TCustomAction(Sender).Checked;
  cxDBVerticalGrid.OptionsBehavior.ImmediateEditor := TCustomAction(Sender).Checked;
end;

procedure TSimpleVerticalGridDemoMainForm.actPaintStyleExecute(
  Sender: TObject);
begin
  if not TMenuItem(Sender).Checked then
  begin
    TMenuItem(Sender).Checked := True;
    cxDBVerticalGrid.OptionsView.PaintStyle := TcxvgPaintStyle(TMenuItem(Sender).Tag);
    case cxDBVerticalGrid.OptionsView.PaintStyle of
      psdotNet: begin
                  cxDBVerticalGrid.Styles.StyleSheet := SimpleVerticalGridDemoMainDM.cxVerticalGridStyleSheetDevExpress;
                  cxDBVerticalGrid.Styles.OnGetContentStyle := cxDBVerticalGridStylesGetContentStyle;
                end;
      psDelphi: begin
                  cxDBVerticalGrid.Styles.StyleSheet := nil;
                  cxDBVerticalGrid.Styles.OnGetContentStyle := nil;
                end;
    end;
    cxDBVerticalGrid.LayoutChanged;
  end;
end;

end.
