//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MegaDemoMain.h"
#include "AddDictionaryForm.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxBarEditItem"
#pragma link "cxButtons"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxCheckGroup"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxImage"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxPC"
#pragma link "cxRadioGroup"
#pragma link "cxRichEdit"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "dxBar"
#pragma link "dxGDIPlusClasses"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TfmMain *fmMain;
//---------------------------------------------------------------------------
void AddDictionary(TdxCustomSpellChecker *ASpellChecker)
{
  if (fmAddDictionary->ShowModal() == mrOk) {
    fmAddDictionary->Add(ASpellChecker);
  }
}
//---------------------------------------------------------------------------
__fastcall TfmMain::TfmMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::FormCreate(TObject *Sender)
{
  cxLookAndFeelController1->NativeStyle = true;
  dxSpellChecker1->SpellingOptions->OnChanged = SpellingOptionsChanged;
  dxSpellChecker1->AutoCorrectOptions->OnChanged = AutoCorrectOptionsChanged;
  AutoCorrectOptionsChanged(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::AutoCorrectOptionsChanged(TdxSpellCheckerAutoCorrectOptions *Sender)
{
  aAutoCorrectActive->Checked = dxSpellChecker1->AutoCorrectOptions->Active;
  aCorrectCapsLock->Checked = dxSpellChecker1->AutoCorrectOptions->CorrectCapsLock;
  aCorrectInitialCaps->Checked = dxSpellChecker1->AutoCorrectOptions->CorrectInitialCaps;
  aCorrectSentenceCaps->Checked = dxSpellChecker1->AutoCorrectOptions->CorrectSentenceCaps;
  aDisableCapsLock->Checked = dxSpellChecker1->AutoCorrectOptions->DisableCapsLock;
  aReplaceTextAsYouType->Checked = dxSpellChecker1->AutoCorrectOptions->ReplaceTextAsYouType;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::SpellingOptionsChanged(TdxSpellCheckerSpellingOptions* Sender)
{
  aCheckFromCursorPos->Checked = dxSpellChecker1->SpellingOptions->CheckFromCursorPos;
  aCheckSelectedTextFirst->Checked = dxSpellChecker1->SpellingOptions->CheckSelectedTextFirst;
  aIgnoreEmails->Checked = dxSpellChecker1->SpellingOptions->IgnoreEmails;
  aIgnoreMixedCaseWords->Checked = dxSpellChecker1->SpellingOptions->IgnoreMixedCaseWords;
  aIgnoreRepeatedWords->Checked = dxSpellChecker1->SpellingOptions->IgnoreRepeatedWords;
  aIgnoreUpperCaseWords->Checked = dxSpellChecker1->SpellingOptions->IgnoreUpperCaseWords;
  aIgnoreURLs->Checked = dxSpellChecker1->SpellingOptions->IgnoreUrls;
  aIgnoreWordsWithNumbers->Checked = dxSpellChecker1->SpellingOptions->IgnoreWordsWithNumbers;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::cxButton3Click(TObject *Sender)
{
  dxSpellChecker1->Check(edtObjective);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::cxButton5Click(TObject *Sender)
{
  dxSpellChecker1->Check(edtAdress);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::cxButton6Click(TObject *Sender)
{
  dxSpellChecker1->Check(reAbout);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::cxButton7Click(TObject *Sender)
{
  dxSpellChecker1->Check(memInterests);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::cxPageControl1Change(TObject *Sender)
{
  switch (cxPageControl1->ActivePageIndex)
  {
    case 0: cxLabel3->Caption = "ExpressEditors"; break;
    case 1: cxLabel3->Caption = "ExpressQuantumGrid"; break;
  };
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::rgSpellingFormTypeClick(TObject *Sender)
{
  switch (rgSpellingFormType->ItemIndex)
  {
    case 0: aOutlookSpellType->Execute(); break;
    case 1: aWordSpellType->Execute(); break;
  };
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::actExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::actDXOnTheWebExecute(TObject *Sender)
{
  ShowWebPage((TdxWebPageType)(((TComponent*)Sender)->Tag));
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aViewExecute(TObject *Sender)
{
  HandleLookAndFeelChangeCommand(Sender, cxLookAndFeelController1);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aOutlookSpellTypeExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingFormType = TdxSpellCheckerSpellingFormType(((TAction*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aCheckFromCursorPosExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->CheckFromCursorPos = aCheckFromCursorPos->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aCheckSelectedTextFirstExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->CheckSelectedTextFirst = aCheckSelectedTextFirst->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreEmailsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreEmails = aIgnoreEmails->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreMixedCaseWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreMixedCaseWords = aIgnoreMixedCaseWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aCAYTActiveExecute(TObject *Sender)
{
  dxSpellChecker1->CheckAsYouTypeOptions->Active = aCAYTActive->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreRepeatedWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreRepeatedWords = aIgnoreRepeatedWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreUpperCaseWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreUpperCaseWords = aIgnoreUpperCaseWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreWordsWithNumbersExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreWordsWithNumbers = aIgnoreWordsWithNumbers->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aIgnoreURLsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreUrls = aIgnoreURLs->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::aCheckSpellingExecute(TObject *Sender)
{
  dxSpellChecker1->CheckContainer(cxPageControl1->ActivePage, True);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aAutoCorrectActiveExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->Active = aAutoCorrectActive->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aCorrectCapsLockExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->CorrectCapsLock = aCorrectCapsLock->Checked;	
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aCorrectInitialCapsExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->CorrectInitialCaps = aCorrectInitialCaps->Checked;	
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aCorrectSentenceCapsExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->CorrectSentenceCaps = aCorrectSentenceCaps->Checked;	
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aDisableCapsLockExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->DisableCapsLock = aDisableCapsLock->Checked;	
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aReplaceTextAsYouTypeExecute(TObject *Sender)
{
  dxSpellChecker1->AutoCorrectOptions->ReplaceTextAsYouType = aReplaceTextAsYouType->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::aAddDictionaryExecute(TObject *Sender)
{
  AddDictionary(dxSpellChecker1);
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::dxSpellChecker1CheckAsYouTypeStart(TdxCustomSpellChecker *Sender,
		  TWinControl *AControl, bool &AAllow)
{
  AAllow = GetParentForm(AControl) != fmAddDictionary;
}
//---------------------------------------------------------------------------

