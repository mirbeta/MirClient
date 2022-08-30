unit ReportDesignerBaseForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSpreadSheetCore,
  dxSpreadSheetFunctions, dxSpreadSheetGraphics, dxSpreadSheetClasses, dxSpreadSheetTypes, SkinDemoUtils,
  dxSpreadSheet, dxSpreadSheetReportDesigner, BaseForm, cxContainer, cxEdit, cxSplitter, cxGroupBox, ExtCtrls, DB,
  DBClient, Menus, cxClasses, ComCtrls, ReportPreviewUnit, cxLabel, cxTextEdit, cxCheckBox, cxFilterControl, dxmdaset,
  StdCtrls;

type
  TfrmReportDesignerBase = class(TfmBaseForm)
    ReportDesigner: TdxSpreadSheetReportDesigner;
    cxSplitter1: TcxSplitter;
    Panel1: TPanel;
    cxSplitter2: TcxSplitter;
    cxgFieldChooserSite: TcxGroupBox;
    cxgFilter: TcxGroupBox;
    miDesignView: TMenuItem;
    miPreview: TMenuItem;
    miSections: TMenuItem;
    N2: TMenuItem;
    asd1: TMenuItem;
    miRemove: TMenuItem;
    miHeader: TMenuItem;
    miFooter: TMenuItem;
    miDetail: TMenuItem;
    N3: TMenuItem;
    cbxUseFilter: TcxCheckBox;
    Filter: TcxFilterControl;
    btnApply: TButton;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure miDesignViewClick(Sender: TObject);
    procedure miPreviewClick(Sender: TObject);
    procedure miRemoveClick(Sender: TObject);
    procedure OnSectionClick(Sender: TObject);
    procedure ReportDesignerSelectionChanged(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  protected
    FilterFileName: string;
  public
    procedure AfterConstruction; override;
    procedure BeforePreview(ASpreadSheet: TdxSpreadSheet); virtual;
    function GetSpreadSheet: TdxSpreadSheet; override;
    procedure Initialize; virtual;
    procedure LoadDataset(ADataSet: TdxMemData; const AFileName: string);
  end;

var
  frmReportDesignerBase: TfrmReportDesignerBase;

implementation

{$R *.dfm}

procedure TfrmReportDesignerBase.miDesignViewClick(Sender: TObject);
var
  AItem: TdxBaseMenuItem;
begin
  if FindMenuItem('miDesignView', AItem) then
    ReportDesigner.Options.DesignView := GetMenuItemChecked(AItem);
end;

procedure TfrmReportDesignerBase.miPreviewClick(Sender: TObject);
begin
  FilterChanged(nil);
  if Preview = nil then
    Preview := TfrmPreview.Create(nil);
  Preview.ssResult.ClearAll;
  ReportDesigner.Build(Preview.ssResult);
  BeforePreview(Preview.ssResult);
  Preview.Show;
end;

procedure TfrmReportDesignerBase.miRemoveClick(Sender: TObject);
var
  ARow, AColumn: Integer;
  ASection: TdxSpreadSheetReportSection;
begin
  ARow := ReportDesigner.ActiveSheetAsTable.Selection.FocusedRow;
  AColumn := ReportDesigner.ActiveSheetAsTable.Selection.FocusedColumn;
  //
  if ReportDesigner.FindSectionByCell(ARow, AColumn, ASection) then
    ReportDesigner.RemoveSection(ASection);
  ReportDesignerSelectionChanged(nil);
end;

procedure TfrmReportDesignerBase.OnSectionClick(Sender: TObject);
var
  R: TRect;
begin
  R := ReportDesigner.ActiveSheetAsTable.Selection.Area;
  case (Sender as TComponent).Tag of
    0:
      ReportDesigner.SetHeaderSection(R);
    1:
      ReportDesigner.SetDetailSection(R, -1);
    2:
      ReportDesigner.SetFooterSection(R);
  end;
end;

procedure TfrmReportDesignerBase.ReportDesignerSelectionChanged(Sender: TObject);
var
  ARow, AColumn: Integer;
  ASection: TdxSpreadSheetReportSection;
begin
  ARow := ReportDesigner.ActiveSheetAsTable.Selection.FocusedRow;
  AColumn := ReportDesigner.ActiveSheetAsTable.Selection.FocusedColumn;
  //
  if not ReportDesigner.FindSectionByCell(ARow, AColumn, ASection) then
    ASection := nil;
  //
  MenuItemSetChecked('miHeader', (ASection <> nil) and (ASection.SectionType = rstHeader));
  MenuItemSetChecked('miDetail', (ASection <> nil) and (ASection.SectionType = rstDetail));
  MenuItemSetChecked('miFooter', (ASection <> nil) and (ASection.SectionType = rstFooter));
end;

procedure TfrmReportDesignerBase.btnApplyClick(Sender: TObject);
begin
  Filter.ApplyFilter;
  cbxUseFilter.Checked := Filter.FilterText <> '';
end;

procedure TfrmReportDesignerBase.btnClearClick(Sender: TObject);
begin
  Filter.Clear;
  cbxUseFilter.Checked := Filter.FilterText <> '';
end;

procedure TfrmReportDesignerBase.FilterChanged(Sender: TObject);
begin
  if cbxUseFilter.Checked then
    Filter.ApplyFilter;
  ReportDesigner.DataBinding.DataController.Filter.Active := cbxUseFilter.Checked;
end;

procedure TfrmReportDesignerBase.FormCreate(Sender: TObject);
begin
  inherited;
  Initialize;
  WindowState := wsMaximized;
  ReportDesigner.FieldChooser.Site := cxgFieldChooserSite;
  ReportDesigner.FieldChooser.Show;
end;

procedure TfrmReportDesignerBase.FormDestroy(Sender: TObject);
begin
  if FilterFileName <> '' then
  Filter.SaveToFile(FilterFileName);
  FreeAndNil(Preview);
end;

procedure TfrmReportDesignerBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FilterChanged(nil);
end;

function TfrmReportDesignerBase.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := TdxSpreadSheet(ReportDesigner);
end;

procedure TfrmReportDesignerBase.BeforePreview(ASpreadSheet: TdxSpreadSheet);
begin

end;

procedure TfrmReportDesignerBase.Initialize;
begin
  // todo: need override
  if ReportDesigner.DataBinding.DataController.DataSet <> nil then
  begin
    FilterFileName := '..\..\Data\' + ReportDesigner.DataBinding.DataController.DataSet.Name;
    if FilterFileName <> '' then
      FilterFileName := ChangeFileExt(FilterFileName, '.flt');
  end;
  if FileExists(FilterFileName) then
  begin
    Filter.LoadFromFile(FilterFileName);
    cbxUseFilter.Checked := Filter.FilterText <> '';
  end;
end;

procedure TfrmReportDesignerBase.LoadDataset(ADataSet: TdxMemData; const AFileName: string);
var
  I: Integer;
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    ADataSet.DisableControls;
    try
      for I := ADataSet.Fields.Count - 1 downto 0 do
        if ADataSet.Fields[I].FieldKind = fkData then
          ADataSet.Fields[I].Free;
      ADataSet.CreateFieldsFromStream(AFileStream);
      AFileStream.Position := 0;
      ADataSet.LoadFromStream(AFileStream);
      ADataSet.Active := True;
    finally
      ADataSet.EnableControls;
    end;
  finally
    AFileStream.Free;
  end;
end;

end.

