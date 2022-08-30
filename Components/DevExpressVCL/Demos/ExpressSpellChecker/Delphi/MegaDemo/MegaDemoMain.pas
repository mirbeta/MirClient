unit MegaDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxSpellChecker, StdCtrls, ComCtrls, cxControls, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, Menus, cxLookAndFeelPainters,
  cxButtons, cxLookAndFeels, cxLabel, cxDropDownEdit, cxCalendar,
  cxMaskEdit, ExtCtrls, cxGroupBox, ActnList, ImgList, cxGraphics, ShellApi,
  cxClasses, cxPC, 
  jpeg, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  DB, cxDBData, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  dxmdaset, cxGridLevel, cxGridCustomView, cxGrid, dxBar, cxImage,
  cxBarEditItem, cxColorComboBox, cxBlobEdit, dxGDIPlusClasses,
  cxCheckBox, cxRadioGroup, cxCheckGroup;

type
  TfmMain = class(TForm)
    dxSpellChecker1: TdxSpellChecker;
    alMain: TActionList;
    actDownloads: TAction;
    actSupport: TAction;
    actDXOnTheWeb: TAction;
    actProducts: TAction;
    actExit: TAction;
    aOutlookSpellType: TAction;
    aWordSpellType: TAction;
    aCheckFromCursorPos: TAction;
    aCheckSelectedTextFirst: TAction;
    aIgnoreEmails: TAction;
    aIgnoreMixedCaseWords: TAction;
    aCAYTActive: TAction;
    aIgnoreRepeatedWords: TAction;
    aIgnoreUpperCaseWords: TAction;
    aIgnoreURLs: TAction;
    aIgnoreWordsWithNumbers: TAction;
    aCheckSpelling: TAction;
    Panel2: TPanel;
    cxPageControl1: TcxPageControl;
    cxTabSheet1: TcxTabSheet;
    cxLabel16: TcxLabel;
    edtName: TcxTextEdit;
    cxLabel14: TcxLabel;
    deBirthDate: TcxDateEdit;
    cxLabel17: TcxLabel;
    edtObjective: TcxTextEdit;
    reAbout: TcxRichEdit;
    cxLabel7: TcxLabel;
    cxLabel1: TcxLabel;
    edtAdress: TcxTextEdit;
    cxButton3: TcxButton;
    cxButton5: TcxButton;
    cxButton6: TcxButton;
    Image2: TImage;
    cxTabSheet2: TcxTabSheet;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    dxMemData1: TdxMemData;
    DataSource1: TDataSource;
    dxMemData1Photo: TBlobField;
    dxMemData1Title: TStringField;
    dxMemData1FirstName: TStringField;
    dxMemData1LastName: TStringField;
    dxMemData1TitleOfCourtesy: TStringField;
    dxMemData1BirthDate: TDateField;
    dxMemData1Notes: TMemoField;
    cxGrid1DBTableView1RecId: TcxGridDBColumn;
    cxGrid1DBTableView1Photo: TcxGridDBColumn;
    cxGrid1DBTableView1FirstName: TcxGridDBColumn;
    cxGrid1DBTableView1LastName: TcxGridDBColumn;
    cxGrid1DBTableView1Title: TcxGridDBColumn;
    cxGrid1DBTableView1CourtesyTitle: TcxGridDBColumn;
    cxGrid1DBTableView1BirthDate: TcxGridDBColumn;
    cxGrid1DBTableView1Notes: TcxGridDBColumn;
    dxBarManager: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    CheckSpelling1: TdxBarButton;
    Exit1: TdxBarButton;
    File1: TdxBarSubItem;
    Outlook1: TdxBarButton;
    Word1: TdxBarButton;
    dfsd1: TdxBarSubItem;
    aCAYTActive1: TdxBarButton;
    CheckFromCursorPos1: TdxBarButton;
    CheckSelectedTextFirst1: TdxBarButton;
    IgnoreEmails1: TdxBarButton;
    IgnoreMixedCaseWords1: TdxBarButton;
    IgnoreRepeatedWords1: TdxBarButton;
    IgnoreUppercaseWords1: TdxBarButton;
    IgnoreURLs1: TdxBarButton;
    Spelling1: TdxBarSubItem;
    Options1: TdxBarSubItem;
    aFlat1: TdxBarButton;
    Standard1: TdxBarButton;
    UltraFlat1: TdxBarButton;
    Office111: TdxBarButton;
    NativeStyel1: TdxBarButton;
    View1: TdxBarSubItem;
    DeveloperExpressProducts1: TdxBarButton;
    DeveloperExpressDownloads1: TdxBarButton;
    DeveloperExpressontheWeb1: TdxBarButton;
    DevExpressSupportCenter1: TdxBarButton;
    Help1: TdxBarSubItem;
    cxPageControl2: TcxPageControl;
    cxTabSheet3: TcxTabSheet;
    Image1: TImage;
    cxLabel2: TcxLabel;
    memInterests: TcxMemo;
    cxButton7: TcxButton;
    Image3: TImage;
    beiSearch: TcxBarEditItem;
    Image4: TImage;
    dxBarButton1: TdxBarButton;
    cxPageControl4: TcxPageControl;
    cxTabSheet5: TcxTabSheet;
    btnCheckSpelling: TcxButton;
    cxCheckBox3: TcxCheckBox;
    cxButton1: TcxButton;
    cxLookAndFeelController1: TcxLookAndFeelController;
    cxGroupBox1: TcxGroupBox;
    cxLabel3: TcxLabel;
    cxLabel4: TcxLabel;
    cxPageControl3: TcxPageControl;
    cxTabSheet4: TcxTabSheet;
    rgSpellingFormType: TcxRadioGroup;
    cgSpellingOptions: TcxGroupBox;
    cxCheckBox1: TcxCheckBox;
    cxCheckBox2: TcxCheckBox;
    cxCheckBox4: TcxCheckBox;
    cxCheckBox5: TcxCheckBox;
    cxCheckBox6: TcxCheckBox;
    cxCheckBox7: TcxCheckBox;
    cxCheckBox8: TcxCheckBox;
    cxCheckBox9: TcxCheckBox;
    Image5: TImage;
    gbAutoCorrectOptions: TcxGroupBox;
    cbActive: TcxCheckBox;
    cbCorrectCapsLock: TcxCheckBox;
    cbCorrectInitialCaps: TcxCheckBox;
    cbCorrectSentenceCaps: TcxCheckBox;
    cbDisableCapsLock: TcxCheckBox;
    cbReplaceTextAsYouType: TcxCheckBox;
    aAutoCorrectActive: TAction;
    aCorrectCapsLock: TAction;
    aCorrectInitialCaps: TAction;
    aCorrectSentenceCaps: TAction;
    aDisableCapsLock: TAction;
    aReplaceTextAsYouType: TAction;
    aAddDictionary: TAction;
    dxBarButton2: TdxBarButton;
    procedure actExitExecute(Sender: TObject);
    procedure aViewExecute(Sender: TObject);

    procedure aOutlookSpellTypeExecute(Sender: TObject);
    procedure aCheckFromCursorPosExecute(Sender: TObject);
    procedure aCheckSelectedTextFirstExecute(Sender: TObject);
    procedure aIgnoreEmailsExecute(Sender: TObject);
    procedure aIgnoreMixedCaseWordsExecute(Sender: TObject);
    procedure aCAYTActiveExecute(Sender: TObject);
    procedure aIgnoreRepeatedWordsExecute(Sender: TObject);
    procedure aIgnoreUpperCaseWordsExecute(Sender: TObject);
    procedure aIgnoreWordsWithNumbersExecute(Sender: TObject);
    procedure aIgnoreURLsExecute(Sender: TObject);
    procedure aCheckSpellingExecute(Sender: TObject);

    procedure cxButton3Click(Sender: TObject);
    procedure cxButton5Click(Sender: TObject);
    procedure cxButton6Click(Sender: TObject);
    procedure cxButton7Click(Sender: TObject);
    procedure cxPageControl1Change(Sender: TObject);
    procedure rgSpellingFormTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aAutoCorrectActiveExecute(Sender: TObject);
    procedure aCorrectCapsLockExecute(Sender: TObject);
    procedure aCorrectInitialCapsExecute(Sender: TObject);
    procedure aCorrectSentenceCapsExecute(Sender: TObject);
    procedure aDisableCapsLockExecute(Sender: TObject);
    procedure aReplaceTextAsYouTypeExecute(Sender: TObject);
    procedure aAddDictionaryExecute(Sender: TObject);
    procedure dxSpellChecker1CheckAsYouTypeStart(
      Sender: TdxCustomSpellChecker; AControl: TWinControl;
      var AAllow: Boolean);
    procedure actDXOnTheWebExecute(Sender: TObject);
  private
    procedure AutoCorrectOptionsChanged(Sender: TdxSpellCheckerAutoCorrectOptions);
    procedure SpellingOptionsChanged(Sender: TdxSpellCheckerSpellingOptions);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  AddDictionaryForm, DemoUtils;

type
  TcxCheckGroupAccess = class(TcxCheckGroup);

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.actDXOnTheWebExecute(Sender: TObject);
begin
  ShowWebPage(TdxWebPageType(TComponent(Sender).Tag));
end;

procedure TfmMain.aOutlookSpellTypeExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingFormType := TdxSpellCheckerSpellingFormType(TAction(Sender).Tag);
  rgSpellingFormType.ItemIndex := TAction(Sender).Tag;
end;

procedure TfmMain.aCheckFromCursorPosExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.CheckFromCursorPos := aCheckFromCursorPos.Checked;
end;

procedure TfmMain.aCheckSelectedTextFirstExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.CheckSelectedTextFirst := aCheckSelectedTextFirst.Checked;
end;

procedure TfmMain.aIgnoreEmailsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreEmails := aIgnoreEmails.Checked;
end;

procedure TfmMain.aIgnoreMixedCaseWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreMixedCaseWords := aIgnoreMixedCaseWords.Checked;
end;

procedure TfmMain.aIgnoreRepeatedWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreRepeatedWords := aIgnoreRepeatedWords.Checked;
end;

procedure TfmMain.aIgnoreUpperCaseWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreUpperCaseWords := aIgnoreUpperCaseWords.Checked;
end;

procedure TfmMain.aIgnoreURLsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreUrls := aIgnoreURLs.Checked;
end;

procedure TfmMain.aIgnoreWordsWithNumbersExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreWordsWithNumbers :=
    aIgnoreWordsWithNumbers.Checked;
end;

procedure TfmMain.aAutoCorrectActiveExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.Active := aAutoCorrectActive.Checked;
end;

procedure TfmMain.aCorrectCapsLockExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.CorrectCapsLock := aCorrectCapsLock.Checked;
end;

procedure TfmMain.aCorrectInitialCapsExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.CorrectInitialCaps :=
    aCorrectInitialCaps.Checked;
end;

procedure TfmMain.aCorrectSentenceCapsExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.CorrectSentenceCaps :=
    aCorrectSentenceCaps.Checked;
end;

procedure TfmMain.aDisableCapsLockExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.DisableCapsLock :=
    aDisableCapsLock.Checked;
end;

procedure TfmMain.aReplaceTextAsYouTypeExecute(Sender: TObject);
begin
  dxSpellChecker1.AutoCorrectOptions.ReplaceTextAsYouType :=
    aReplaceTextAsYouType.Checked;
end;

procedure TfmMain.aCAYTActiveExecute(Sender: TObject);
begin
  dxSpellChecker1.CheckAsYouTypeOptions.Active := aCAYTActive.Checked;
end;

procedure TfmMain.aViewExecute(Sender: TObject);
begin
  HandleLookAndFeelChangeCommand(Sender, cxLookAndFeelController1);
end;

procedure TfmMain.aCheckSpellingExecute(Sender: TObject);
begin
  dxSpellChecker1.CheckContainer(cxPageControl1.ActivePage, True);
end;

procedure TfmMain.cxButton3Click(Sender: TObject);
begin
  dxSpellChecker1.Check(edtObjective);
end;

procedure TfmMain.cxButton5Click(Sender: TObject);
begin
  dxSpellChecker1.Check(edtAdress);
end;

procedure TfmMain.cxButton6Click(Sender: TObject);
begin
  dxSpellChecker1.Check(reAbout);
end;

procedure TfmMain.cxButton7Click(Sender: TObject);
begin
  dxSpellChecker1.Check(memInterests);
end;

procedure TfmMain.cxPageControl1Change(Sender: TObject);
begin
  case cxPageControl1.ActivePageIndex of
    0: cxLabel3.Caption := 'ExpressEditors';
    1: cxLabel3.Caption := 'ExpressQuantumGrid';
  end;
end;

procedure TfmMain.rgSpellingFormTypeClick(Sender: TObject);
begin
  case rgSpellingFormType.ItemIndex of
    0: aOutlookSpellType.Execute;
    1: aWordSpellType.Execute;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  cxLookAndFeelController1.NativeStyle := True;
  dxSpellChecker1.SpellingOptions.OnChanged := SpellingOptionsChanged;
  dxSpellChecker1.AutoCorrectOptions.OnChanged := AutoCorrectOptionsChanged;
  AutoCorrectOptionsChanged(nil);
end;

procedure TfmMain.AutoCorrectOptionsChanged(Sender: TdxSpellCheckerAutoCorrectOptions);
begin
  aAutoCorrectActive.Checked := dxSpellChecker1.AutoCorrectOptions.Active;
  aCorrectCapsLock.Checked := dxSpellChecker1.AutoCorrectOptions.CorrectCapsLock;
  aCorrectInitialCaps.Checked := dxSpellChecker1.AutoCorrectOptions.CorrectInitialCaps;
  aCorrectSentenceCaps.Checked := dxSpellChecker1.AutoCorrectOptions.CorrectSentenceCaps;
  aDisableCapsLock.Checked := dxSpellChecker1.AutoCorrectOptions.DisableCapsLock;
  aReplaceTextAsYouType.Checked := dxSpellChecker1.AutoCorrectOptions.ReplaceTextAsYouType;
end;

procedure TfmMain.SpellingOptionsChanged(Sender: TdxSpellCheckerSpellingOptions);
begin
  aCheckFromCursorPos.Checked := dxSpellChecker1.SpellingOptions.CheckFromCursorPos;
  aCheckSelectedTextFirst.Checked := dxSpellChecker1.SpellingOptions.CheckSelectedTextFirst;
  aIgnoreEmails.Checked := dxSpellChecker1.SpellingOptions.IgnoreEmails;
  aIgnoreMixedCaseWords.Checked := dxSpellChecker1.SpellingOptions.IgnoreMixedCaseWords;
  aIgnoreRepeatedWords.Checked := dxSpellChecker1.SpellingOptions.IgnoreRepeatedWords;
  aIgnoreUpperCaseWords.Checked := dxSpellChecker1.SpellingOptions.IgnoreUpperCaseWords;
  aIgnoreURLs.Checked := dxSpellChecker1.SpellingOptions.IgnoreUrls;
  aIgnoreWordsWithNumbers.Checked := dxSpellChecker1.SpellingOptions.IgnoreWordsWithNumbers;
end;

procedure TfmMain.aAddDictionaryExecute(Sender: TObject);
begin
  AddDictionary(dxSpellChecker1);
end;

procedure TfmMain.dxSpellChecker1CheckAsYouTypeStart(
  Sender: TdxCustomSpellChecker; AControl: TWinControl;
  var AAllow: Boolean);
begin
  AAllow := GetParentForm(AControl) <> fmAddDictionary;
end;

end.
