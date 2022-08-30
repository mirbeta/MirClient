unit ConditionalFormattingDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSpreadSheetCore, dxSpreadSheetFunctions, dxSpreadSheetGraphics,
  dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheet, BaseForm, dxCore, dxCoreClasses, dxHashUtils, ComCtrls,
  dxSpreadSheetCoreHistory, dxSpreadSheetPrinting, dxSpreadSheetFormulas, cxClasses, dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingRules, dxSpreadSheetCoreStyles, dxSpreadSheetCoreStrs, dxSpreadSheetContainers,
  dxSpreadSheetHyperlinks, dxSpreadSheetStyles, dxSpreadSheetUtils, cxSplitter, dxSpreadSheetFormulaBar;

type
  { TfrmConditionalFormatting }

  TfrmConditionalFormatting = class(TfmBaseForm)
    SpreadSheet: TdxSpreadSheet;
    miConditionalFormatting: TMenuItem;
    miManageRules: TMenuItem;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;

    procedure FormCreate(Sender: TObject);
    procedure miManageRulesClick(Sender: TObject);
  public
    function GetSpreadSheet: TdxSpreadSheet; override;
  end;

var
  frmConditionalFormatting: TfrmConditionalFormatting;

implementation

{$R *.dfm}

{$IFDEF EXPRESSTREELIST}
uses
  dxSpreadSheetConditionalFormattingRulesManagerDialog;
{$ENDIF}

const
  sdxWarningText = 'Conditional formatting dialogs are only available if the ExpressQuantumTreeList Suite is installed.';

{ TfrmConditionalFormatting }

procedure TfrmConditionalFormatting.FormCreate(Sender: TObject);
begin
  inherited;
  SpreadSheet.LoadFromFile('..\..\Data\TopTradingPartners.xlsx');
  WindowState := wsMaximized;
end;

function TfrmConditionalFormatting.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

procedure TfrmConditionalFormatting.miManageRulesClick(Sender: TObject);
begin
{$IFDEF EXPRESSTREELIST}
  ShowConditionalFormattingRulesManagerDialog(SpreadSheet);
{$ELSE}
  MessageDlg(sdxWarningText, mtWarning, [mbOK], 0);
{$ENDIF}
end;

end.
