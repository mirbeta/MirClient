//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleDemoMain.h"
#include "DemoUtils.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCalendar"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxSpellChecker"
#pragma link "cxGraphics"
#pragma resource "*.dfm"
TfmCV *fmCV;
//---------------------------------------------------------------------------
__fastcall TfmCV::TfmCV(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::dxSpellChecker1CheckAsYouTypeStart(
	  TdxCustomSpellChecker *Sender, TWinControl *AControl, bool &AAllow)
{
  AAllow = AControl != edtName;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::dxSpellChecker1CheckControlInContainer(
	  TdxCustomSpellChecker *Sender, TWinControl *AControl, bool &AAllow,
	  bool &AContinue)
{
  AAllow = AControl != edtName;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aOutlookSpellTypeExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingFormType = TdxSpellCheckerSpellingFormType(((TAction*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aCheckFromCursorPosExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->CheckFromCursorPos = aCheckFromCursorPos->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aCheckSelectedTextFirstExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->CheckSelectedTextFirst = aCheckSelectedTextFirst->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreEmailsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreEmails = aIgnoreEmails->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreMixedCaseWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreMixedCaseWords = aIgnoreMixedCaseWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aCAYTActiveExecute(TObject *Sender)
{
  dxSpellChecker1->CheckAsYouTypeOptions->Active = aCAYTActive->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreRepeatedWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreRepeatedWords = aIgnoreRepeatedWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreUpperCaseWordsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreUpperCaseWords = aIgnoreUpperCaseWords->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreURLsExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreUrls = aIgnoreURLs->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aIgnoreWordsWithNumbersExecute(TObject *Sender)
{
  dxSpellChecker1->SpellingOptions->IgnoreWordsWithNumbers = aIgnoreWordsWithNumbers->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::aCheckSpellingExecute(TObject *Sender)
{
  dxSpellChecker1->CheckContainer(this, True);
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::actDXOnTheWebExecute(TObject *Sender)
{
  ShowWebPage((TdxWebPageType)(((TComponent*)Sender)->Tag));
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::actExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfmCV::FormCreate(TObject *Sender)
{
  cxLookAndFeelController1->NativeStyle = true;
  MainMenu1->Items->Insert(MainMenu1->Items->IndexOf(Help1),
	CreateLookAndFeelMenuItems(MainMenu1->Items, cxLookAndFeelController1));
}
//---------------------------------------------------------------------------
