unit ReportPreviewUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, BaseForm, cxGraphics, cxControls, 
  cxLookAndFeels, cxLookAndFeelPainters, dxCore, dxCoreClasses, dxHashUtils, dxSpreadSheetCore, dxSpreadSheetCoreHistory, 
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetClasses, dxSpreadSheetContainers, 
  dxSpreadSheetFormulas, dxSpreadSheetHyperlinks, dxSpreadSheetFunctions, dxSpreadSheetGraphics, dxSpreadSheetPrinting, 
  dxSpreadSheetTypes, dxSpreadSheetUtils, dxSpreadSheet, cxClasses, Menus, ComCtrls;

type
  TfrmPreview = class(TForm)
    ssResult: TdxSpreadSheet;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    miSaveAs: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    Options1: TMenuItem;
    miShowFormulas: TMenuItem;
    sveDialog: TSaveDialog;
    procedure miCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miSaveAsClick(Sender: TObject);
    procedure miShowFormulasClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Preview: TfrmPreview;

implementation

{$R *.dfm}

procedure TfrmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  Preview := nil;
end;

procedure TfrmPreview.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPreview.miShowFormulasClick(Sender: TObject);
var
  I: Integer;
begin
  ssResult.BeginUpdate;
  try
    for I := 0 to ssResult.SheetCount - 1 do
      if ssResult.Sheets[I] is TdxSpreadSheetTableView then
        TdxSpreadSheetTableView(ssResult.Sheets[I]).Options.ShowFormulas := bDefault;
    ssResult.OptionsView.ShowFormulas := not ssResult.OptionsView.ShowFormulas;
  finally
    ssResult.EndUpdate;
  end;
end;

procedure TfrmPreview.miSaveAsClick(Sender: TObject);
var
  AFileName: string;
begin
  sveDialog.Filter := dxSpreadSheetFormatsRepository.GetSaveDialogFilter;
  if sveDialog.Execute then
  begin
    AFileName := sveDialog.FileName;
    if ExtractFileExt(AFileName) = '' then
    begin
      if (sveDialog.FilterIndex > 0) and (sveDialog.FilterIndex < dxSpreadSheetFormatsRepository.Count) then
        AFileName := AFileName + dxSpreadSheetFormatsRepository.Items[sveDialog.FilterIndex].GetExt
      else
       AFileName := AFileName + '.xlsx';
    end;
    ssResult.SaveToFile(AFileName);
  end;
end;

end.
