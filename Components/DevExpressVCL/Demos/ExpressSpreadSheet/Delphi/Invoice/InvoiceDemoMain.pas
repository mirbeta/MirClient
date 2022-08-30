unit InvoiceDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxSkinsCore, dxSpreadSheetCore, dxSpreadSheetFunctions,
  dxSpreadSheetGraphics, dxSpreadSheetClasses, dxSpreadSheetTypes,
  dxSpreadSheet, BaseForm, dxCore, dxCoreClasses, dxHashUtils, dxSpreadSheetCoreHistory, dxSpreadSheetCoreStyles,
  dxSpreadSheetCoreStrs, dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetContainers, dxSpreadSheetFormulas, dxSpreadSheetHyperlinks, dxSpreadSheetStyles, dxSpreadSheetPrinting,
  dxSpreadSheetUtils, cxSplitter, dxSpreadSheetFormulaBar;

type
  TfrmInvoice = class(TfmBaseForm)
    SpreadSheet: TdxSpreadSheet;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    function GetSpreadSheet: TdxSpreadSheet; override;
  end;

var
  frmInvoice: TfrmInvoice;

implementation

{$R *.dfm}

procedure TfrmInvoice.FormCreate(Sender: TObject);
begin
  inherited;
  SpreadSheet.LoadFromFile('..\..\Data\Invoice.xlsx');
  WindowState := wsMaximized;
end;

function TfrmInvoice.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

end.
