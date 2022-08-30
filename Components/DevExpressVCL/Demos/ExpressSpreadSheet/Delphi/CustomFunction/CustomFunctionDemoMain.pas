unit CustomFunctionDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, cxGeometry, Math, dxSpreadSheetFormulas,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSpreadSheetCore, dxSpreadSheetFunctions,
  dxSpreadSheetGraphics, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheet, BaseForm, cxDateUtils,
  dxSpreadSheetUtils, cxVariants, dxCore, dxCoreClasses, dxHashUtils, dxSpreadSheetCoreHistory, dxSpreadSheetCoreStyles,
  dxSpreadSheetCoreStrs, dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetContainers, dxSpreadSheetHyperlinks, dxSpreadSheetStyles, dxSpreadSheetPrinting, cxSplitter,
  dxSpreadSheetFormulaBar;

type
  TfrmCustomFunction = class(TfmBaseForm)
    SpreadSheet: TdxSpreadSheet;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;
    procedure FormCreate(Sender: TObject);
  private
  protected
    function GetSpreadSheet: TdxSpreadSheet; override;
  public
  end;

procedure fpiTriangleArea(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
procedure fnTriangleArea(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);

var
  frmCustomFunction: TfrmCustomFunction;

resourcestring
  sTriangleArea = 'TriangleArea';

implementation

{$R *.dfm}

uses
  dxSpreadSheetCoreFormulasTokens;

procedure TfrmCustomFunction.FormCreate(Sender: TObject);
begin
  SpreadSheet.LoadFromFile('..\..\Data\CustomFunction.xlsx');
  dxSpreadSheetFunctionsRepository.Add(@sTriangleArea, fnTriangleArea, fpiTriangleArea, frkValue, 2048, ftMath);

  SpreadSheet.ActiveSheetAsTable.CreateCell(12, 5).SetText(StringReplace('=TriangleArea(F9,F10,F11)',
    ',', SpreadSheet.FormulaController.FormatSettings.Data.ListSeparator, [rfReplaceAll, rfIgnoreCase]), True);
end;

procedure fpiTriangleArea(var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo);
begin
  AParamCount := 3;
  SetLength(AParamKind, AParamCount);
  AParamKind[0] := fpkValue;
  AParamKind[1] := fpkValue;
  AParamKind[2] := fpkValue;
end;

procedure fnTriangleArea(Sender: TdxSpreadSheetFormulaResult; const AParams: TdxSpreadSheetFormulaToken);
var
  P1, P2, P3: Variant;
begin
  if Sender.GetParamsCount(AParams) <> 3 then
    Sender.SetError(ecValue)
  else
    if Sender.ExtractNumericParameter(P1, AParams) then
      if Sender.ExtractNumericParameter(P2, AParams, 1) then
        if Sender.ExtractNumericParameter(P3, AParams, 2) then
          Sender.AddValue(0.5 * P1 * P2 * SIN(P3 * Pi / 180));
end;

function TfrmCustomFunction.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

end.
