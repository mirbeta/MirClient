unit SpreadSheetRLMain;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,  ToolWin,
  Menus, ImgList, cxControls, StdActns, ActnList, dxPSCore, dxPSBaseGridLnk, dxPSdxSpreadSheetLnk, dxPSGlbl, dxPSUtl,
  dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, DemoBasicMain,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxPSPrVwAdv, dxPSPrVwRibbon,
  dxmdaset, DB, cxClasses, dxSpreadSheet, dxCore, dxCoreClasses, dxHashUtils, dxSpreadSheetCore,
  dxSpreadSheetCoreHistory, dxSpreadSheetPrinting, dxSpreadSheetFormulas, dxSpreadSheetFunctions, dxSpreadSheetGraphics,
  dxSpreadSheetClasses, dxSpreadSheetTypes, dxBarBuiltInMenu, dxSpreadSheetFormatCellsDialog;


type
  TSpreadSheetRLForm = class(TDemoBasicMainForm)
    SaveDialog: TSaveDialog;
    actDeleteCells: TAction;
    actSave: TAction;
    actInsertCells: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actFormatCells: TAction;
    actOpen: TAction;
    actSetPrintArea: TAction;
    actClearPrintArea: TAction;
    miSaveSpreadSheet: TMenuItem;
    LoadData1: TMenuItem;
    MenuItem6: TMenuItem;
    PrintArea1: TMenuItem;
    SetPrintArea1: TMenuItem;
    ClearPrintArea1: TMenuItem;
    mnuEdit: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    N7: TMenuItem;
    miCells: TMenuItem;
    miFormat: TMenuItem;
    miDeletecells: TMenuItem;
    Insertcells1: TMenuItem;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolBar2: TToolBar;
    pnCellsRect: TPanel;
    Panel2: TPanel;
    edtCellEdit: TEdit;
    dxSpreadSheet1: TdxSpreadSheet;
    dxComponentPrinterLink1: TdxSpreadSheetReportLnk;
    OpenDialog: TOpenDialog;
    tbsOpen: TToolButton;
    procedure actPreviewExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actSetPrintAreaExecute(Sender: TObject);
    procedure actClearPrintAreaExecute(Sender: TObject);
    procedure actPrintSetupExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actDeleteCellsExecute(Sender: TObject);
    procedure actInsertCellsExecute(Sender: TObject);
    procedure actFormatCellsExecute(Sender: TObject);
    procedure edtCellEditChange(Sender: TObject);
    procedure dxSpreadSheet1ActiveCellChanging(Sender: TdxSpreadSheetTableView; const ANewActiveCell: TPoint;
      var ACanSelect: Boolean);
  private
    function GetCell(ARow, AColumn: Integer): TdxSpreadSheetCell;
    function GetSpreadSheet: TdxSpreadSheet;
  public
    property SpreadSheet: TdxSpreadSheet read GetSpreadSheet;
  end;

var
  SpreadSheetRLForm: TSpreadSheetRLForm;

implementation

resourcestring
  sdxInvalidPrintArea = 'You''ve selected a single cell for print area.' + #13#10#13#10 +
    'If this is correct, click OK.' + #13#10 +
    'If you selected single cell by mistake, click Cancel, select the cell you want to include, and then click Set Print Area again.';


{$R *.dfm}
{$R SpreadSheetData.res}

function TSpreadSheetRLForm.GetCell(ARow, AColumn: Integer): TdxSpreadSheetCell;
begin
  Result := dxSpreadSheet1.ActiveSheetAsTable.Cells[ARow, AColumn];
end;

function TSpreadSheetRLForm.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := dxSpreadSheet1;
end;

procedure TSpreadSheetRLForm.FormCreate(Sender: TObject);
var
  ASource: TResourceStream;
begin
  ASource := TResourceStream.Create(HInstance, 'DXSPREADSHEETTEMPLATE', RT_RCDATA);
  try
     SpreadSheet.LoadFromStream(ASource);
  finally
    ASource.Free;
  end;
end;

procedure TSpreadSheetRLForm.actCopyExecute(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.CopyToClipboard;
end;

procedure TSpreadSheetRLForm.actCutExecute(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.CutToClipboard;
end;

procedure TSpreadSheetRLForm.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSpreadSheetRLForm.actDeleteCellsExecute(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.DeleteCells;
end;

procedure TSpreadSheetRLForm.actFormatCellsExecute(Sender: TObject);
begin
  ShowFormatCellsDialog(SpreadSheet.ActiveSheetAsTable);
end;

procedure TSpreadSheetRLForm.actInsertCellsExecute(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.InsertCells;
end;

procedure TSpreadSheetRLForm.actPasteExecute(Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.PasteFromClipboard;
end;

procedure TSpreadSheetRLForm.actOpenExecute(Sender: TObject);
begin
  OpenDialog.Filter := dxSpreadSheetFormatsRepository.GetOpenDialogFilter;
  if OpenDialog.Execute then
    SpreadSheet.LoadFromFile(OpenDialog.FileName);
end;

procedure TSpreadSheetRLForm.actPreviewExecute(Sender: TObject);
begin
  dxComponentPrinter.ReportLink[0].Preview;
end;

procedure TSpreadSheetRLForm.actPrintExecute(Sender: TObject);
begin
  dxComponentPrinter.ReportLink[0].Print(True, nil);
end;

procedure TSpreadSheetRLForm.actSaveExecute(Sender: TObject);
begin
  SaveDialog.Filter := dxSpreadSheetFormatsRepository.GetSaveDialogFilter;
  if SaveDialog.Execute then
    SpreadSheet.SaveToFile(SaveDialog.FileName);
end;

procedure TSpreadSheetRLForm.actSetPrintAreaExecute(Sender: TObject);
var
  R: TRect;
begin
  R := SpreadSheet.ActiveSheetAsTable.Selection.Area;
  if (R.Right <> R.Left) or (R.Bottom <> R.Top) or (MessageDlg(sdxInvalidPrintArea, mtWarning, [mbOK, mbCancel], 0) = mrOK) then
    TdxSpreadSheetCustomReportLink(dxComponentPrinter.ReportLink[0]).PrintArea := R;
end;

procedure TSpreadSheetRLForm.dxSpreadSheet1ActiveCellChanging(Sender: TdxSpreadSheetTableView;
  const ANewActiveCell: TPoint; var ACanSelect: Boolean);
var
  ACell: TdxSpreadSheetCell;
begin
  ACell := GetCell(ANewActiveCell.Y, ANewActiveCell.X);
  if ACell <> nil then
  begin
    if ACell.IsFormula then
      edtCellEdit.Text := ACell.AsFormula.AsText
    else
      edtCellEdit.Text := ACell.AsString
  end
  else
    edtCellEdit.Text := '';
end;

procedure TSpreadSheetRLForm.edtCellEditChange(Sender: TObject);
var
  R: TRect;
begin
  if edtCellEdit.Focused then
  begin
    R := SpreadSheet.ActiveSheetAsTable.Selection.Area;
    SpreadSheet.ActiveSheetAsTable.CreateCell(R.Top, R.Left).SetText(edtCellEdit.Text, True);
  end;
end;


procedure TSpreadSheetRLForm.actClearPrintAreaExecute(Sender: TObject);
begin
  TdxSpreadSheetCustomReportLink(dxComponentPrinter.ReportLink[0]).ClearPrintArea;
end;

procedure TSpreadSheetRLForm.actPrintSetupExecute(Sender: TObject);
begin
  dxComponentPrinter.ReportLink[0].PageSetup;
end;

end.
