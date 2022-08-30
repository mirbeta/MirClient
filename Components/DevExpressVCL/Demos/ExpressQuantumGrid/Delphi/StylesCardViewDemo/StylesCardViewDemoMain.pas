unit StylesCardViewDemoMain;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxGridCustomLayoutView, BaseForm, cxGridTableView;

type
  TStylesCardViewDemoMainForm = class(TfmBaseForm)
    lvDevExpress: TcxGridLevel;
    cxGrid: TcxGrid;
    cvDevExpress: TcxGridDBCardView;
    cvDevExpressID: TcxGridDBCardViewRow;
    cvDevExpressFIRSTNAME: TcxGridDBCardViewRow;
    cvDevExpressSECONDNAME: TcxGridDBCardViewRow;
    cvDevExpressGENDER: TcxGridDBCardViewRow;
    cvDevExpressBIRTHNAME: TcxGridDBCardViewRow;
    cvDevExpressDATEOFBIRTH: TcxGridDBCardViewRow;
    cvDevExpressBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvDevExpressLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvDevExpressBIOGRAPHY: TcxGridDBCardViewRow;
    cvDevExpressNICKNAME: TcxGridDBCardViewRow;
    cvDevExpressFullname: TcxGridDBCardViewRow;
    lvSlate: TcxGridLevel;
    lvHighContrast: TcxGridLevel;
    cvSlate: TcxGridDBCardView;
    cvHighContrast: TcxGridDBCardView;
    cvSlateID: TcxGridDBCardViewRow;
    cvSlateFIRSTNAME: TcxGridDBCardViewRow;
    cvSlateSECONDNAME: TcxGridDBCardViewRow;
    cvSlateGENDER: TcxGridDBCardViewRow;
    cvSlateBIRTHNAME: TcxGridDBCardViewRow;
    cvSlateDATEOFBIRTH: TcxGridDBCardViewRow;
    cvSlateBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvSlateLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvSlateBIOGRAPHY: TcxGridDBCardViewRow;
    cvSlateNICKNAME: TcxGridDBCardViewRow;
    cvSlateFullName: TcxGridDBCardViewRow;
    cvHighContrastID: TcxGridDBCardViewRow;
    cvHighContrastFIRSTNAME: TcxGridDBCardViewRow;
    cvHighContrastSECONDNAME: TcxGridDBCardViewRow;
    cvHighContrastGENDER: TcxGridDBCardViewRow;
    cvHighContrastBIRTHNAME: TcxGridDBCardViewRow;
    cvHighContrastDATEOFBIRTH: TcxGridDBCardViewRow;
    cvHighContrastBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvHighContrastLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvHighContrastBIOGRAPHY: TcxGridDBCardViewRow;
    cvHighContrastNICKNAME: TcxGridDBCardViewRow;
    cvHighContrastFullName: TcxGridDBCardViewRow;
    pnlLeft: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    spedCardWidth: TcxSpinEdit;
    Label2: TLabel;
    spedCardBorderWidth: TcxSpinEdit;
    cbCellAutoHeight: TcxCheckBox;
    Panel2: TPanel;
    btnEdit: TcxButton;
    lvUserDefined: TcxGridLevel;
    cvUserDefined: TcxGridDBCardView;
    cvUserDefinedFullName: TcxGridDBCardViewRow;
    cvUserDefinedID: TcxGridDBCardViewRow;
    cvUserDefinedFIRSTNAME: TcxGridDBCardViewRow;
    cvUserDefinedSECONDNAME: TcxGridDBCardViewRow;
    cvUserDefinedGENDER: TcxGridDBCardViewRow;
    cvUserDefinedBIRTHNAME: TcxGridDBCardViewRow;
    cvUserDefinedDATEOFBIRTH: TcxGridDBCardViewRow;
    cvUserDefinedBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvUserDefinedLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvUserDefinedBIOGRAPHY: TcxGridDBCardViewRow;
    cvUserDefinedNICKNAME: TcxGridDBCardViewRow;
    procedure cxSpinEdit2PropertiesChange(Sender: TObject);
    procedure cbSellAutoHeightPropertiesChange(Sender: TObject);
    procedure cxGridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
    procedure spedCardWidthPropertiesChange(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure spedCardWidthKeyPress(Sender: TObject; var Key: Char);
    procedure cxGridLayoutChanged(Sender: TcxCustomGrid; AGridView: TcxCustomGridView);
    procedure FormCreate(Sender: TObject);
  private
    procedure GetViewOptions(AView: TcxGridDBCardView);
  end;

var
  StylesCardViewDemoMainForm: TStylesCardViewDemoMainForm;

implementation

{$R *.dfm}

uses
  StylesCardViewDemoData, cxStyleSheetEditor, Dialogs, AboutDemoForm,
  cxGridStyleSheetsPreview, dxCore;

procedure TStylesCardViewDemoMainForm.spedCardWidthPropertiesChange(Sender: TObject);
begin
  with TcxGridDBCardView(cxGrid.FocusedView) do
    OptionsView.CardWidth := spedCardWidth.Value;
end;

procedure TStylesCardViewDemoMainForm.cxSpinEdit2PropertiesChange(Sender: TObject);
begin
  with TcxGridDBCardView(cxGrid.FocusedView) do
    OptionsView.CardBorderWidth := spedCardBorderWidth.Value;
end;

procedure TStylesCardViewDemoMainForm.cbSellAutoHeightPropertiesChange(Sender: TObject);
begin
  with TcxGridDBCardView(cxGrid.FocusedView) do
    OptionsView.CellAutoHeight := cbCellAutoHeight.Checked;
end;

procedure TStylesCardViewDemoMainForm.cxGridActiveTabChanged(
  Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  btnEdit.Enabled := ALevel = lvUserDefined;
  GetViewOptions(TcxGridDBCardView(ALevel.GridView));
end;

procedure TStylesCardViewDemoMainForm.GetViewOptions(AView: TcxGridDBCardView);
begin
  with AView.OptionsView do
  begin
    cbCellAutoHeight.Checked := CellAutoHeight;
    spedCardBorderWidth.Value := CardBorderWidth;
    CardBorderWidth := spedCardBorderWidth.Value;
    spedCardWidth.Value := CardWidth;
  end;
end;

procedure TStylesCardViewDemoMainForm.btnEditClick(Sender: TObject);
begin
  ShowcxStyleSheetEditor(StylesCardViewDemoMainDM.cvssUserDefined, nil)
end;

procedure TStylesCardViewDemoMainForm.spedCardWidthKeyPress(
  Sender: TObject; var Key: Char);
begin
  if not (dxCharInSet(Key, ['0'..'9']) or (Key = #8)) then
    Key := #0;
end;

procedure TStylesCardViewDemoMainForm.cxGridLayoutChanged(
  Sender: TcxCustomGrid; AGridView: TcxCustomGridView);
begin
  if TcxGrid(Sender).FocusedView <> nil then
  with TcxGridDbCardView(TcxGrid(Sender).FocusedView) do
  begin
    spedCardWidth.Value := OptionsView.CardWidth;
    spedCardBorderWidth.Value := OptionsView.CardBorderWidth;
  end;
end;

procedure TStylesCardViewDemoMainForm.FormCreate(Sender: TObject);
begin
  spedCardWidth.Properties.MinValue := cxGridCardMinWidth;
  spedCardBorderWidth.Properties.MinValue := cxGridCardBorderMinWidth;
end;

end.  
