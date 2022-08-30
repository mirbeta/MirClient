unit CommentsDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus, Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSpreadSheetCore, dxSpreadSheetFunctions, dxSpreadSheetGraphics,
  dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheet, BaseForm, dxCore, dxCoreClasses, dxHashUtils, ComCtrls,
  dxSpreadSheetCoreHistory, dxSpreadSheetPrinting, dxSpreadSheetFormulas, cxClasses, dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetCoreStyles, dxSpreadSheetCoreStrs,
  dxSpreadSheetHyperlinks, dxSpreadSheetStyles, dxSpreadSheetUtils, cxSplitter, dxSpreadSheetFormulaBar;

type
  { TfrmComments }

  TfrmComments = class(TfmBaseForm)
    SpreadSheet: TdxSpreadSheet;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;

    procedure FormCreate(Sender: TObject);
  public
    function GetSpreadSheet: TdxSpreadSheet; override;
  end;

var
  frmComments: TfrmComments;

implementation

{$R *.dfm}

{ TfrmComments }

procedure TfrmComments.FormCreate(Sender: TObject);
begin
  inherited;
  SpreadSheet.LoadFromFile('..\..\Data\Comments_template.xlsx');
  WindowState := wsMaximized;
end;

function TfrmComments.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

end.
