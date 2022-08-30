unit EBar2000DemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dxBar, dxBarExtItems, stdctrls, Buttons, ExtCtrls, ImgList, ActnList, 
  ComCtrls, EBarsUtils, cxClasses;

type
  TEBar2000DemoMainForm = class(TForm)
    dxBarManager: TdxBarManager;
    siFile: TdxBarSubItem;
    siEdit: TdxBarSubItem;
    siView: TdxBarSubItem;
    siInsert: TdxBarSubItem;
    siFormat: TdxBarSubItem;
    siTools: TdxBarSubItem;
    siTable: TdxBarSubItem;
    siWindow: TdxBarSubItem;
    siHelp: TdxBarSubItem;
    btnNew: TdxBarLargeButton;
    btnNewWebPage: TdxBarLargeButton;
    btnNewEmailMessage: TdxBarLargeButton;
    btnNewBlankDocument: TdxBarLargeButton;
    btnOpen: TdxBarLargeButton;
    btnClose: TdxBarLargeButton;
    btnCloseAll: TdxBarLargeButton;
    btnSave: TdxBarLargeButton;
    btnSaveAs: TdxBarLargeButton;
    btnSaveAsWebPage: TdxBarLargeButton;
    btnSaveAll: TdxBarLargeButton;
    btnSaveVersion: TdxBarLargeButton;
    btnWebPagePreview: TdxBarLargeButton;
    btnPageSetup: TdxBarLargeButton;
    btnPrintPreview: TdxBarLargeButton;
    btnPrint: TdxBarLargeButton;
    btnPrintSetup: TdxBarLargeButton;
    btnMailRecipient: TdxBarLargeButton;
    btnSendNow: TdxBarLargeButton;
    btnRoundRecipient: TdxBarLargeButton;
    btnExchengeFolder: TdxBarLargeButton;
    btnFaxRecipient: TdxBarLargeButton;
    btnSendToMicrosoftPowerPoint: TdxBarLargeButton;
    btnVersion: TdxBarLargeButton;
    btnProperties: TdxBarLargeButton;
    btnExit: TdxBarLargeButton;
    btnUndo: TdxBarLargeButton;
    btnRedo: TdxBarLargeButton;
    btnRepeat: TdxBarLargeButton;
    btnCut: TdxBarLargeButton;
    btnCopy: TdxBarLargeButton;
    btnPaste: TdxBarLargeButton;
    btnPasteTable: TdxBarLargeButton;
    btnPasteSpecial: TdxBarLargeButton;
    btnPasetAsHyperlink: TdxBarLargeButton;
    btnClear: TdxBarLargeButton;
    dxBarButton1: TdxBarLargeButton;
    btnFind: TdxBarLargeButton;
    btnFindNext: TdxBarLargeButton;
    btnReplace: TdxBarLargeButton;
    btnGoTo: TdxBarLargeButton;
    btnLinks: TdxBarLargeButton;
    btnObject: TdxBarLargeButton;
    btnPicture: TdxBarLargeButton;
    btnCreateTextBoxLink: TdxBarLargeButton;
    btnBreakForwardLink: TdxBarLargeButton;
    btnNextTextBox: TdxBarLargeButton;
    btnPreviousTextBox: TdxBarLargeButton;
    btnNextComment: TdxBarLargeButton;
    btnPreviousComment: TdxBarLargeButton;
    cbZoom: TdxBarCombo;
    btnRuler: TdxBarLargeButton;
    btnShowAll: TdxBarLargeButton;
    btnDocumentMap: TdxBarLargeButton;
    btnViewFieldCodes: TdxBarLargeButton;
    btnShowFieldShading: TdxBarLargeButton;
    btnFullScreen: TdxBarLargeButton;
    btnMagnifier: TdxBarLargeButton;
    btnZoom100: TdxBarLargeButton;
    btnFitToWindow: TdxBarLargeButton;
    btnOnePage: TdxBarLargeButton;
    btnNormal: TdxBarLargeButton;
    btnWebLayout: TdxBarLargeButton;
    btnPrintLayout: TdxBarLargeButton;
    btnOutLine: TdxBarLargeButton;
    siToolBars: TdxBarSubItem;
    btnComments: TdxBarLargeButton;
    btnHTMLSource: TdxBarLargeButton;
    dxBarButton2: TdxBarLargeButton;
    dxBarButton3: TdxBarLargeButton;
    btnPromote: TdxBarLargeButton;
    btnDemote: TdxBarLargeButton;
    btnDemoteToBodyText: TdxBarLargeButton;
    btnMoveUp: TdxBarLargeButton;
    btnMoveDown: TdxBarLargeButton;
    btnExpand: TdxBarLargeButton;
    btnCollapse: TdxBarLargeButton;
    btnAll: TdxBarLargeButton;
    btnShowHeading1: TdxBarLargeButton;
    btnShowHeading2: TdxBarLargeButton;
    btnShowHeading3: TdxBarLargeButton;
    btnShowHeading4: TdxBarLargeButton;
    btnShowHeading5: TdxBarLargeButton;
    btnFirstLineOnlyView: TdxBarLargeButton;
    btnShowFormatting: TdxBarLargeButton;
    btnMasterDocumentView: TdxBarLargeButton;
    btnNextHeader: TdxBarLargeButton;
    btnPreviousHeader1: TdxBarLargeButton;
    btnHideBodyText: TdxBarLargeButton;
    btnPageBreak: TdxBarLargeButton;
    btnInsertColumnBreak: TdxBarLargeButton;
    btnInsertSectionBreak: TdxBarLargeButton;
    btnPageNumber: TdxBarLargeButton;
    btnNumberOfPages: TdxBarLargeButton;
    btnDate: TdxBarLargeButton;
    btnTime: TdxBarLargeButton;
    btnPageNumbers: TdxBarLargeButton;
    btnDateAndTime: TdxBarLargeButton;
    btnAutoText: TdxBarLargeButton;
    btnSumbol: TdxBarLargeButton;
    btnComment: TdxBarLargeButton;
    btnDeleteComment: TdxBarLargeButton;
    btnFootnote: TdxBarLargeButton;
    InsertListNumField: TdxBarLargeButton;
    btnHyperlink: TdxBarLargeButton;
    btnFromFile: TdxBarLargeButton;
    btnTextBox: TdxBarLargeButton;
    btnHorizontal: TdxBarLargeButton;
    btnFile: TdxBarLargeButton;
    btnInsertExcelSpreadsheet: TdxBarLargeButton;
    btnChart: TdxBarLargeButton;
    btnWordArt: TdxBarLargeButton;
    btnEquationEditor: TdxBarLargeButton;
    btnVoiceComment: TdxBarLargeButton;
    btnPenComment: TdxBarLargeButton;
    btnFromScannerOrCamear: TdxBarLargeButton;
    btnClipArt: TdxBarLargeButton;
    btnDataBase: TdxBarLargeButton;
    btnAddressBook: TdxBarLargeButton;
    btnObject1: TdxBarLargeButton;
    cbFontName: TdxBarFontNameCombo;
    cnFontSize: TdxBarCombo;
    cbStyle: TdxBarCombo;
    btnBold: TdxBarLargeButton;
    btnItalic: TdxBarLargeButton;
    btnUnderline: TdxBarLargeButton;
    btnFormatPainter: TdxBarLargeButton;
    btnGrowFont: TdxBarLargeButton;
    btnShrinkFont: TdxBarLargeButton;
    btnGrowFont1Pt: TdxBarLargeButton;
    btnShinkFont1Pt: TdxBarLargeButton;
    btnSmallCaps: TdxBarLargeButton;
    btnAllCaps: TdxBarLargeButton;
    btnSuperscript: TdxBarLargeButton;
    btnsubscript: TdxBarLargeButton;
    btnDecreaseIndent: TdxBarLargeButton;
    btnIncreaseIndent: TdxBarLargeButton;
    btnNumbering: TdxBarLargeButton;
    btnBullets: TdxBarLargeButton;
    btnAlignLeft: TdxBarLargeButton;
    btnCenter: TdxBarLargeButton;
    btnAlignRight: TdxBarLargeButton;
    btnJustify: TdxBarLargeButton;
    btnColumns: TdxBarLargeButton;
    btnAutoFormat: TdxBarLargeButton;
    btnInsertListNumField: TdxBarLargeButton;
    btnChangeTextDirection: TdxBarLargeButton;
    btnFont: TdxBarLargeButton;
    btnParagraph: TdxBarLargeButton;
    btnBulletsAndNumbering1: TdxBarLargeButton;
    btnGropCap: TdxBarLargeButton;
    btnStyle: TdxBarLargeButton;
    btnTextDirection: TdxBarLargeButton;
    btnChangeCase: TdxBarLargeButton;
    btnTheme: TdxBarLargeButton;
    btnCOMAddIns: TdxBarLargeButton;
    btnSpellingandGrammar: TdxBarLargeButton;
    btnNextMisspelling: TdxBarLargeButton;
    btnHideSpellingErrors: TdxBarLargeButton;
    btnHideGrammarErrors: TdxBarLargeButton;
    btnSpelling: TdxBarLargeButton;
    btnDictionary: TdxBarLargeButton;
    btnGrammar: TdxBarLargeButton;
    btnSetLanguage: TdxBarLargeButton;
    btnThesaurus: TdxBarLargeButton;
    btnWordCount: TdxBarLargeButton;
    btnAutoSummarize: TdxBarLargeButton;
    btnResumarize: TdxBarLargeButton;
    btnHighlightShowOnlySummary: TdxBarLargeButton;
    btnAutoCorrect: TdxBarLargeButton;
    btnToolsAutoCorrectExceptions: TdxBarLargeButton;
    btnHighlightChanges: TdxBarLargeButton;
    btnTrackChanges: TdxBarLargeButton;
    btnAcceptOrRejectChanges: TdxBarLargeButton;
    btnAcceptChange: TdxBarLargeButton;
    btnRejectChange: TdxBarLargeButton;
    btnNextChange: TdxBarLargeButton;
    btnPreviousChange: TdxBarLargeButton;
    btnCompareDocuments: TdxBarLargeButton;
    btnMergeDocuments: TdxBarLargeButton;
    btnProtectDocument: TdxBarLargeButton;
    btnEnvelopesAndLabels: TdxBarLargeButton;
    btnLetterWizard: TdxBarLargeButton;
    btnMacros: TdxBarLargeButton;
    btnRecordMacroStopRecorder: TdxBarLargeButton;
    btnSecurity: TdxBarLargeButton;
    btnVisualBasicEditor: TdxBarLargeButton;
    btnMicrosoftScriptEditor: TdxBarLargeButton;
    btnInsertScript: TdxBarLargeButton;
    btnRemoveAllScripts: TdxBarLargeButton;
    btnShowAllScripts: TdxBarLargeButton;
    btnStopRecordind: TdxBarLargeButton;
    btnPauseRecording: TdxBarLargeButton;
    btnUpdateField: TdxBarLargeButton;
    btnRepaginate: TdxBarLargeButton;
    btnShrinkOnePage: TdxBarLargeButton;
    btnInsertTable: TdxBarLargeButton;
    dxBarButton4: TdxBarLargeButton;
    btnCells: TdxBarLargeButton;
    btnRows: TdxBarLargeButton;
    btnColumnsToTheLeft: TdxBarLargeButton;
    btnDeleteCells: TdxBarLargeButton;
    btnDeleteRows: TdxBarLargeButton;
    btnDeleteColumns: TdxBarLargeButton;
    btnGridLines: TdxBarLargeButton;
    btnTableAutoFormat: TdxBarLargeButton;
    btnAutoSum: TdxBarLargeButton;
    btnSortAscepring: TdxBarLargeButton;
    btnSortDescending: TdxBarLargeButton;
    btnFindInField: TdxBarLargeButton;
    btnTableInsert: TdxBarSubItem;
    btnTablesandBordersToolbar: TdxBarLargeButton;
    btnDrawTable: TdxBarLargeButton;
    btnEraser: TdxBarLargeButton;
    btnMergeCells: TdxBarLargeButton;
    btnAplitCells: TdxBarLargeButton;
    btnAlignTop: TdxBarLargeButton;
    btnCenterVerticaly: TdxBarLargeButton;
    btnAlignBottom: TdxBarLargeButton;
    siCellAlignment: TdxBarSubItem;
    btnDistributeRowsEvenly: TdxBarLargeButton;
    btnDistributeColumnsEvenly: TdxBarLargeButton;
    btnNewWindow: TdxBarLargeButton;
    btnArrangeAll: TdxBarLargeButton;
    btnSplit: TdxBarLargeButton;
    btnNextWindow: TdxBarLargeButton;
    btnPreviousWindow: TdxBarLargeButton;
    dxBarButton7: TdxBarLargeButton;
    dxBarButton8: TdxBarLargeButton;
    dxBarButton9: TdxBarLargeButton;
    dxBarButton10: TdxBarLargeButton;
    dxBarButton11: TdxBarLargeButton;
    dxBarButton12: TdxBarLargeButton;
    dxBarButton13: TdxBarLargeButton;
    dxBarButton14: TdxBarLargeButton;
    Images: TImageList;
    siSendTo: TdxBarSubItem;
    siPicture: TdxBarSubItem;
    siLanguage: TdxBarSubItem;
    btnCustomize: TdxBarLargeButton;
    siMacro: TdxBarSubItem;
    siInsertTable: TdxBarSubItem;
    siDeleteTable: TdxBarSubItem;
    siAutoFit: TdxBarSubItem;
    btnTableProperties: TdxBarLargeButton;
    StyleImages: TImageList;
    btnFontColor: TdxBarLargeButton;
    btnHighlight: TdxBarLargeButton;
    FontColorPopupMenu: TdxBarPopupMenu;
    HightlightColorPopupMenu: TdxBarPopupMenu;
    btnFontColorItem: TdxBarLargeButton;
    btnDownFontColorItem: TdxBarLargeButton;
    btnCustomFontColor: TdxBarLargeButton;
    ColorDialog: TColorDialog;
    btnColorItem: TdxBarLargeButton;
    btnDownColorItem: TdxBarLargeButton;
    btnCustomColor: TdxBarLargeButton;
    btnStandard: TdxBarLargeButton;
    btnFormatting: TdxBarLargeButton;
    FontDialog: TFontDialog;
    btnInternet: TdxBarLargeButton;
    cbAddress: TdxBarCombo;
    btnBack: TdxBarLargeButton;
    btnForward: TdxBarLargeButton;
    btnStop: TdxBarLargeButton;
    btnRefresh: TdxBarLargeButton;
    btnHome: TdxBarLargeButton;
    ilHotImages: TImageList;
    ilDisabledImages: TImageList;
    btnAttach: TdxBarLargeButton;
    dxBarButton5: TdxBarLargeButton;
    dxBarButton6: TdxBarLargeButton;
    dxBarButton15: TdxBarLargeButton;
    dxBarButton16: TdxBarLargeButton;
    dxBarButton17: TdxBarLargeButton;
    dxBarButton18: TdxBarLargeButton;
    dxBarButton19: TdxBarLargeButton;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    gbOptions: TGroupBox;
    rgMenuAnimations: TRadioGroup;
    bgBar1: TGroupBox;
    SpeedButton1: TSpeedButton;
    cbAllowCustomizingBar1: TCheckBox;
    cbAllowQuickCustomizingBar1: TCheckBox;
    cbAllowResetBar1: TCheckBox;
    CheckBox1: TCheckBox;
    gbBar2: TGroupBox;
    SpeedButton2: TSpeedButton;
    cbAllowCustomizingBar2: TCheckBox;
    cbAllowQuickCustomizingBar2: TCheckBox;
    cbAllowResetBar2: TCheckBox;
    CheckBox2: TCheckBox;
    rgBar3: TGroupBox;
    cbRotateWhenVertical: TCheckBox;
    rgStyle: TRadioGroup;
    gbImages: TGroupBox;
    cbHotImages: TCheckBox;
    cbDisabledImages: TCheckBox;
    CheckBox4: TCheckBox;
    gbMiscellaneous: TGroupBox;
    sbFont: TSpeedButton;
    sbDockColor: TSpeedButton;
    cbCanCustomize: TCheckBox;
    cbAllowReset: TCheckBox;
    cbShowHelpButton: TCheckBox;
    cbSunkenBorder: TCheckBox;
    cbMenusShowRecentItemsFirst: TCheckBox;
    cbShowFullMenusAfterDelay: TCheckBox;
    cbStretchGlyphs: TCheckBox;
    cbShowCaptions: TCheckBox;
    gbAlphaBlending: TGroupBox;
    tbStandard: TTrackBar;
    dxBarLargeButton1: TdxBarLargeButton;
    Label2: TLabel;
    Label3: TLabel;
    dxBarLargeButton2: TdxBarLargeButton;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    tbFormatting: TTrackBar;
    Label6: TLabel;
    procedure cbStyleMeasureItem(Sender: TdxBarCustomCombo;
      AIndex: Integer; var AHeight: Integer);
    procedure cbStyleDrawItem(Sender: TdxBarCustomCombo; AIndex: Integer;
      ARect: TRect; AState: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure btnFontColorItemClick(Sender: TObject);
    procedure btnCustomFontColorClick(Sender: TObject);
    procedure btnColorItemClick(Sender: TObject);
    procedure btnCustomColorClick(Sender: TObject);
    procedure btnStandardClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure cbCanCustomizeClick(Sender: TObject);
    procedure sbDockColorClick(Sender: TObject);
    procedure cbAllowResetClick(Sender: TObject);
    procedure cbShowHelpButtonClick(Sender: TObject);
    procedure cbSunkenBorderClick(Sender: TObject);
    procedure cbMenusShowRecentItemsFirstClick(Sender: TObject);
    procedure rgMenuAnimationsClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure cbAllowCustomizingBar1Click(Sender: TObject);
    procedure cbAllowQuickCustomizingBar1Click(Sender: TObject);
    procedure cbAllowResetBar1Click(Sender: TObject);
    procedure cbAllowCustomizingBar2Click(Sender: TObject);
    procedure cbAllowQuickCustomizingBar2Click(Sender: TObject);
    procedure cbAllowResetBar2Click(Sender: TObject);
    procedure cbShowFullMenusAfterDelayClick(Sender: TObject);
    procedure dxBarManagerHelpButtonClick(Sender: TObject);
    procedure cbRotateWhenVerticalClick(Sender: TObject);
    procedure btnCustomizeClick(Sender: TObject);
    procedure dxBarManagerBarVisibleChange(Sender: TdxBarManager;
      ABar: TdxBar);
    procedure cbStretchGlyphsClick(Sender: TObject);
    procedure rgStyleClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure cbShowCaptionsClick(Sender: TObject);
    procedure cbHotImagesClick(Sender: TObject);
    procedure cbDisabledImagesClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateColorItems(AButton, ADownButton, ACustomButton,
      AItemButton: TdxBarLargeButton; APopupMenu: TdxBarPopupMenu);
  public
    { Public declarations }
    FUpdateVisible : Boolean;
  end;

var
  EBar2000DemoMainForm: TEBar2000DemoMainForm;

implementation

{$R *.DFM}

procedure TEBar2000DemoMainForm.cbStyleMeasureItem(Sender: TdxBarCustomCombo;
  AIndex: Integer; var AHeight: Integer);
begin
  AHeight := 35;
end;

procedure TEBar2000DemoMainForm.cbStyleDrawItem(Sender: TdxBarCustomCombo;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  if Sender.DroppedDown then
    with Sender.Canvas do
    begin
      StyleImages.Draw(Sender.Canvas, ARect.Left, ARect.Top, AIndex);
      FrameRect(ARect);
      InflateRect(ARect, -1, -1);
      FrameRect(ARect);
    end
  else
    Sender.Canvas.TextRect(ARect, ARect.Left + 1, ARect.Top, TdxBarCombo(Sender).Text);
end;

procedure TEBar2000DemoMainForm.FormCreate(Sender: TObject);
begin
  cbAddress.Items.Add(dxStartURL);
  cbAddress.Items.Add(dxSupportURL);

  TrackBarChange(tbStandard);
  TrackBarChange(tbFormatting);
  btnFontColor.Tag := clBlue;
  UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
  btnHighlight.Tag := clYellow;
  UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
  FUpdateVisible := True;
  rgStyleClick(nil);
end;

procedure TEBar2000DemoMainForm.UpdateColorItems(AButton, ADownButton, ACustomButton, AItemButton: TdxBarLargeButton;
  APopupMenu: TdxBarPopupMenu);
const
  Colors: array [0..15] of TColor = (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver,
    clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
  ColorNames: array [0..15] of string = ('Black', 'Maroon', 'Green', 'Olive', 'Navy', 'Purple', 'Teal', 'Gray', 'Silver',
    'Red', 'Lime', 'Yellow', 'Blue', 'Fuchsia', 'Aqua', 'White');
var
  I: Integer;
  B: TBitmap;
begin
  B := TBitmap.Create;
  B.Width := Images.Width + 1;
  B.Height := Images.Height;
  APopupMenu.ItemLinks.Clear;
  for I := 0 to 15 do
  begin
    if Colors[I] <> clBlack then
      B.Canvas.Brush.Color := clBlack
    else
      B.Canvas.Brush.Color := clWhite;
    B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
    B.Canvas.Brush.Color := Colors[I];
    B.Canvas.Pen.Color := clGray;
    if AButton.Tag = Colors[I] then
      B.Canvas.Rectangle(1, 1, B.Width - 3, B.Height - 2)
    else
      B.Canvas.Rectangle(2, 2, B.Width - 2, B.Height - 1);
    with APopupMenu.ItemLinks.Add do
    begin
      if AButton.Tag = Colors[I] then
      begin
        Item := ADownButton;
        ADownButton.Down := True;
      end
      else
        Item := AItemButton;
      Data := Colors[I];
      UserCaption := ColorNames[I];
      UserGlyph.Assign(B);
    end;
  end;
  with APopupMenu.ItemLinks.Add do
  begin
    Item := ACustomButton;
    UserCaption := 'Custom ...';
    BeginGroup := True;
  end;
  B.Free;
end;

procedure TEBar2000DemoMainForm.btnFontColorItemClick(Sender: TObject);
begin
  btnFontColor.Tag := TdxBarLargeButton(Sender).ClickItemLink.Data;
  UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
end;

procedure TEBar2000DemoMainForm.btnCustomFontColorClick(Sender: TObject);
begin
  ColorDialog.Color := btnFontColor.Tag;
  if ColorDialog.Execute then
  begin
    btnFontColor.Tag := ColorDialog.Color;
    UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
  end;
end;

procedure TEBar2000DemoMainForm.btnColorItemClick(Sender: TObject);
begin
  btnHighlight.Tag := TdxBarLargeButton(Sender).ClickItemLink.Data;
  UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
end;

procedure TEBar2000DemoMainForm.btnCustomColorClick(Sender: TObject);
begin
  ColorDialog.Color := btnHighlight.Tag;
  if ColorDialog.Execute then
  begin
    btnHighlight.Tag := ColorDialog.Color;
    UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
  end;
end;

procedure TEBar2000DemoMainForm.btnStandardClick(Sender: TObject);
begin
  FUpdateVisible := False;
  dxBarManager.Bars[TdxBarLargeButton(Sender).Tag].Visible := TdxBarLargeButton(Sender).Down;
  FUpdateVisible := True;
end;

procedure TEBar2000DemoMainForm.sbFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(dxBarManager.Font);
  if FontDialog.Execute then
    dxBarManager.Font.Assign(FontDialog.Font);
end;

procedure TEBar2000DemoMainForm.cbCanCustomizeClick(Sender: TObject);
begin
  dxBarManager.CanCustomize := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.sbDockColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    dxBarManager.DockColor := ColorDialog.Color;
end;

procedure TEBar2000DemoMainForm.cbAllowResetClick(Sender: TObject);
begin
  dxBarManager.AllowReset := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbShowHelpButtonClick(Sender: TObject);
begin
  dxBarManager.ShowHelpButton := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbSunkenBorderClick(Sender: TObject);
begin
  dxBarManager.SunkenBorder := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbMenusShowRecentItemsFirstClick(Sender: TObject);
begin
  dxBarManager.MenusShowRecentItemsFirst := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.rgMenuAnimationsClick(Sender: TObject);
begin
  dxBarManager.MenuAnimations := TdxBarMenuAnimations(rgMenuAnimations.ItemIndex);
end;

procedure TEBar2000DemoMainForm.SpeedButton1Click(Sender: TObject);
begin
  FontDialog.Font.Assign(dxBarManager.Bars[1].Font);
  if FontDialog.Execute then
    dxBarManager.bars[1].Font.Assign(FontDialog.Font);
end;

procedure TEBar2000DemoMainForm.SpeedButton2Click(Sender: TObject);
begin
  FontDialog.Font.Assign(dxBarManager.Bars[2].Font);
  if FontDialog.Execute then
    dxBarManager.bars[2].Font.Assign(FontDialog.Font);
end;

procedure TEBar2000DemoMainForm.cbAllowCustomizingBar1Click(Sender: TObject);
begin
  dxBarManager.Bars[1].AllowCustomizing := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbAllowQuickCustomizingBar1Click(Sender: TObject);
begin
  dxBarManager.Bars[1].AllowQuickCustomizing := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbAllowResetBar1Click(Sender: TObject);
begin
  dxBarManager.Bars[1].AllowReset := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbAllowCustomizingBar2Click(Sender: TObject);
begin
  dxBarManager.Bars[2].AllowCustomizing := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbAllowQuickCustomizingBar2Click(Sender: TObject);
begin
  dxBarManager.Bars[2].AllowQuickCustomizing := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbAllowResetBar2Click(Sender: TObject);
begin
  dxBarManager.Bars[2].AllowReset := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbShowFullMenusAfterDelayClick(Sender: TObject);
begin
  dxBarManager.ShowFullMenusAfterDelay := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.dxBarManagerHelpButtonClick(Sender: TObject);
begin
  MessageDlg('You click on Help Button .', mtInformation, [mbOK],0);
end;

procedure TEBar2000DemoMainForm.cbRotateWhenVerticalClick(Sender: TObject);
begin
  dxBarManager.Bars[3].RotateWhenVertical := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.btnCustomizeClick(Sender: TObject);
begin
  dxBarManager.Customizing(True);
end;

procedure TEBar2000DemoMainForm.dxBarManagerBarVisibleChange(Sender: TdxBarManager;
  ABar: TdxBar);
begin
  if FUpdateVisible and HandleAllocated then
    case ABar.Index of
      1: btnStandard.Down := ABar.Visible;
      2: btnFormatting.Down := ABar.Visible;
      3: btnInternet.Down := ABar.Visible;
    end;
end;

procedure TEBar2000DemoMainForm.cbStretchGlyphsClick(Sender: TObject);
begin
  dxBarManager.StretchGlyphs := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.rgStyleClick(Sender: TObject);
begin
  dxBarManager.Style := TdxBarManagerStyle(rgStyle.ItemIndex);
  Caption := Format('ExpressBars (%s Style)',[rgStyle.Items[rgStyle.ItemIndex]]);
end;

procedure TEBar2000DemoMainForm.CheckBox1Click(Sender: TObject);
begin
  dxBarManager.Bars[1].MultiLine := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.CheckBox2Click(Sender: TObject);
begin
  dxBarManager.Bars[2].MultiLine := TCheckBox(Sender).Checked;
end;

procedure TEBar2000DemoMainForm.cbShowCaptionsClick(Sender: TObject);
var
  i: Integer;
  AChecked: Boolean;
begin
  dxBarManager.BeginUpdate;
  AChecked := TCheckBox(Sender).Checked;
  for i:=0 to ComponentCount - 1 do
    if Components[i] is TdxBarLargeButton then
      TdxBarLargeButton(Components[i]).ShowCaption := AChecked;
  dxBarManager.EndUpdate;
end;

procedure TEBar2000DemoMainForm.cbHotImagesClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    dxBarManager.HotImages := ilHotImages
  else
    dxBarManager.HotImages := nil;
end;

procedure TEBar2000DemoMainForm.cbDisabledImagesClick(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    dxBarManager.DisabledImages := ilDisabledImages
  else
    dxBarManager.DisabledImages := nil;
end;

procedure TEBar2000DemoMainForm.CheckBox4Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    dxBarManager.LargeImages := Images
  else
    dxBarManager.LargeImages := nil;
end;

procedure TEBar2000DemoMainForm.TrackBarChange(Sender: TObject);
begin
  dxBarManager.Bars[TTrackBar(Sender).Tag].AlphaBlendValue := TTrackBar(Sender).Position * 15;
end;

end.
