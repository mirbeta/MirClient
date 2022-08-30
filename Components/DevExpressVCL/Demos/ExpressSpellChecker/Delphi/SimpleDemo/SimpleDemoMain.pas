unit SimpleDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxSpellChecker, StdCtrls, ComCtrls, cxControls, cxContainer,
  cxEdit, cxTextEdit, cxMemo, cxRichEdit, Menus, cxLookAndFeelPainters,
  cxButtons, cxLookAndFeels, cxLabel, cxDropDownEdit, cxCalendar,
  cxMaskEdit, ExtCtrls, cxGroupBox, ActnList, ImgList, cxGraphics, ShellApi;

type
  TfmCV = class(TForm)
    dxSpellChecker1: TdxSpellChecker;
    cxLookAndFeelController1: TcxLookAndFeelController;
    gbPersonal: TcxGroupBox;
    cxLabel8: TcxLabel;
    cxTextEdit5: TcxTextEdit;
    cxLabel13: TcxLabel;
    cxMaskEdit3: TcxMaskEdit;
    cxLabel14: TcxLabel;
    cxDateEdit2: TcxDateEdit;
    cxMaskEdit4: TcxMaskEdit;
    cxLabel15: TcxLabel;
    cxTextEdit6: TcxTextEdit;
    edtName: TcxTextEdit;
    cxLabel16: TcxLabel;
    cxLabel17: TcxLabel;
    gbProfessional: TcxGroupBox;
    cxLabel6: TcxLabel;
    cxTextEdit3: TcxTextEdit;
    cxLabel7: TcxLabel;
    cxMemo1: TcxMemo;
    cxLabel10: TcxLabel;
    cxLabel11: TcxLabel;
    cxRichEdit2: TcxRichEdit;
    cxRichEdit1: TcxRichEdit;
    Panel1: TPanel;
    btnCheckSpelling: TcxButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    alMain: TActionList;
    actDownloads: TAction;
    actSupport: TAction;
    actDXOnTheWeb: TAction;
    actProducts: TAction;
    actExit: TAction;
    Exit1: TMenuItem;
    DeveloperExpressProducts1: TMenuItem;
    DeveloperExpressDownloads1: TMenuItem;
    DeveloperExpressontheWeb1: TMenuItem;
    DevExpressSupportCenter1: TMenuItem;
    aOutlookSpellType: TAction;
    dfsd1: TMenuItem;
    aWordSpellType: TAction;
    Outlook1: TMenuItem;
    Word1: TMenuItem;
    aCheckFromCursorPos: TAction;
    CheckFromCursorPos1: TMenuItem;
    aCheckSelectedTextFirst: TAction;
    aIgnoreEmails: TAction;
    CheckSelectedTextFirst1: TMenuItem;
    IgnoreEmails1: TMenuItem;
    aIgnoreMixedCaseWords: TAction;
    IgnoreMixedCaseWords1: TMenuItem;
    aCAYTActive: TAction;
    aCAYTActive1: TMenuItem;
    aIgnoreRepeatedWords: TAction;
    IgnoreRepeatedWords1: TMenuItem;
    aIgnoreUpperCaseWords: TAction;
    IgnoreUppercaseWords1: TMenuItem;
    aIgnoreURLs: TAction;
    IgnoreURLs1: TMenuItem;
    aIgnoreWordsWithNumbers: TAction;
    Spelling1: TMenuItem;
    aCheckSpelling: TAction;
    CheckSpelling1: TMenuItem;
    N1: TMenuItem;
    cxButton1: TcxButton;
    IgnoreWordsWithNumbers1: TMenuItem;
    cxGroupBox1: TcxGroupBox;
    cxLabel1: TcxLabel;
    procedure actExitExecute(Sender: TObject);
    procedure actDXOnTheWebExecute(Sender: TObject);
    procedure aOutlookSpellTypeExecute(Sender: TObject);
    procedure aCheckFromCursorPosExecute(Sender: TObject);
    procedure aCheckSelectedTextFirstExecute(Sender: TObject);
    procedure aIgnoreEmailsExecute(Sender: TObject);
    procedure aIgnoreMixedCaseWordsExecute(Sender: TObject);
    procedure aCAYTActiveExecute(Sender: TObject);
    procedure dxSpellChecker1CheckAsYouTypeStart(
      Sender: TdxCustomSpellChecker; AControl: TWinControl;
      var AAllow: Boolean);
    procedure dxSpellChecker1CheckControlInContainer(
      Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow,
      AContinue: Boolean);
    procedure aIgnoreRepeatedWordsExecute(Sender: TObject);
    procedure aIgnoreUpperCaseWordsExecute(Sender: TObject);
    procedure aIgnoreWordsWithNumbersExecute(Sender: TObject);
    procedure aIgnoreURLsExecute(Sender: TObject);
    procedure aCheckSpellingExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmCV: TfmCV;

implementation

uses
  DemoUtils;

{$R *.dfm}

procedure TfmCV.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmCV.actDXOnTheWebExecute(Sender: TObject);
begin
  ShowWebPage(TdxWebPageType(TComponent(Sender).Tag));
end;

procedure TfmCV.aOutlookSpellTypeExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingFormType := TdxSpellCheckerSpellingFormType(TAction(Sender).Tag);
end;

procedure TfmCV.aCheckFromCursorPosExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.CheckFromCursorPos := aCheckFromCursorPos.Checked;
end;

procedure TfmCV.aCheckSelectedTextFirstExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.CheckSelectedTextFirst := aCheckSelectedTextFirst.Checked;
end;

procedure TfmCV.aIgnoreEmailsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreEmails := aIgnoreEmails.Checked;
end;

procedure TfmCV.aIgnoreMixedCaseWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreMixedCaseWords := aIgnoreMixedCaseWords.Checked;
end;

procedure TfmCV.aIgnoreRepeatedWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreRepeatedWords := aIgnoreRepeatedWords.Checked;
end;

procedure TfmCV.aIgnoreUpperCaseWordsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreUpperCaseWords := aIgnoreUpperCaseWords.Checked;
end;

procedure TfmCV.aIgnoreURLsExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreUrls := aIgnoreURLs.Checked;
end;

procedure TfmCV.aIgnoreWordsWithNumbersExecute(Sender: TObject);
begin
  dxSpellChecker1.SpellingOptions.IgnoreWordsWithNumbers := aIgnoreWordsWithNumbers.Checked;
end;

procedure TfmCV.aCAYTActiveExecute(Sender: TObject);
begin
  dxSpellChecker1.CheckAsYouTypeOptions.Active := aCAYTActive.Checked;
end;

procedure TfmCV.dxSpellChecker1CheckAsYouTypeStart(
  Sender: TdxCustomSpellChecker; AControl: TWinControl;
  var AAllow: Boolean);
begin
  AAllow := AControl <> edtName;
end;

procedure TfmCV.dxSpellChecker1CheckControlInContainer(
  Sender: TdxCustomSpellChecker; AControl: TWinControl; var AAllow,
  AContinue: Boolean);
begin
  AAllow := AControl <> edtName;
end;

procedure TfmCV.aCheckSpellingExecute(Sender: TObject);
begin
  dxSpellChecker1.CheckContainer(Self, True);
end;

procedure TfmCV.FormCreate(Sender: TObject);
begin
  cxLookAndFeelController1.NativeStyle := True;
  MainMenu1.Items.Insert(MainMenu1.Items.IndexOf(Help1),
    CreateLookAndFeelMenuItems(MainMenu1.Items, cxLookAndFeelController1));
end;

end.
