//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AddDictionaryForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtonEdit"
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxHyperLinkEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxRadioGroup"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TfmAddDictionary *fmAddDictionary;
//---------------------------------------------------------------------------
__fastcall TfmAddDictionary::TfmAddDictionary(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmAddDictionary::Add(TdxCustomSpellChecker *ASpellChecker)
{
  TdxSpellCheckerDictionaryItem *ADictionaryItem;
  ADictionaryItem = ASpellChecker->DictionaryItems->Add();
  switch (rgDictionatyType->ItemIndex) {
	case 0: {
	  ADictionaryItem->DictionaryTypeClass = __classid(TdxHunspellDictionary);
	  TdxHunspellDictionary *AHunspellDictionary;
	  AHunspellDictionary = (TdxHunspellDictionary*) ADictionaryItem->DictionaryType;
	  AHunspellDictionary->GrammarPath = beAffFile->Text;
	  AHunspellDictionary->DictionaryPath = beDicFile->Text;
	  AHunspellDictionary->Language = (int) cbLang->Properties->Items->Objects[cbLang->ItemIndex];
	  break;
	}
	case 1: {
	  ADictionaryItem->DictionaryTypeClass = __classid(TdxOpenOfficeDictionary);
	  TdxOpenOfficeDictionary *AOpenOfficeDictionary;
	  AOpenOfficeDictionary = (TdxOpenOfficeDictionary*) ADictionaryItem->DictionaryType;
	  AOpenOfficeDictionary->GrammarPath = beAffFile->Text;
	  AOpenOfficeDictionary->DictionaryPath = beDicFile->Text;
	  AOpenOfficeDictionary->Language = (int) cbLang->Properties->Items->Objects[cbLang->ItemIndex];
	  break;
	}
	case 2: {
	  ADictionaryItem->DictionaryTypeClass = __classid(TdxISpellDictionary);
	  TdxISpellDictionary *AISpellDictionary;
	  AISpellDictionary = (TdxISpellDictionary*) ADictionaryItem->DictionaryType;
	  AISpellDictionary->GrammarPath = beAffFile->Text;
	  AISpellDictionary->DictionaryPath = beDicFile->Text;
	  AISpellDictionary->Language = (int) cbLang->Properties->Items->Objects[cbLang->ItemIndex];
	  break;
	}
  }
  ShowHourglassCursor();
  try
  {
	ADictionaryItem->DictionaryType->Load(dlmDirectLoad);
  }
  __finally
  {
	HideHourglassCursor();
  }
}

//---------------------------------------------------------------------------

void __fastcall TfmAddDictionary::FormCreate(TObject *Sender)
{
  TdxSpellCheckerCodePages *ACodePages;
  int I;
  {
	for (I = 0; I < dxLanguages()->Count; I++) {
	  cbLang->Properties->Items->AddObject(dxLanguages()->Name[I], (TObject*)dxLanguages()->LocaleID[I]);
	}
	cbLang->ItemIndex = dxLanguages()->IndexOf((unsigned)dxLanguages()->GetDefaultLanguageLCID());
  }
  ACodePages = new TdxSpellCheckerCodePages(True);
  try
  {
	for (I = 0; I < ACodePages->Count; I++) {
	  cbCodePage->Properties->Items->AddObject(ACodePages->Name[I], (TObject*)ACodePages->Code[I]);
	}
	for (I = 0; I < ACodePages->Count; I++) {
	  if (ACodePages->Code[I] == GetACP()) {
		cbCodePage->ItemIndex = I; 
		break;
	  }
	}
  }
  __finally
  {
	ACodePages->Free();
  }

}
//---------------------------------------------------------------------------

void __fastcall TfmAddDictionary::rgDictionatyTypePropertiesChange(TObject *Sender)
          
{
  TcxRadioGroup *ARadioGroup = (TcxRadioGroup*)Sender;
  Label4->Visible = ARadioGroup->ItemIndex == 2;
  cbCodePage->Visible = Label4->Visible;
  Label5->Visible = !Label4->Visible;	
  hlLink->Visible = !Label4->Visible;
}
//---------------------------------------------------------------------------

void __fastcall TfmAddDictionary::beAffFilePropertiesButtonClick(TObject *Sender,
          int AButtonIndex)
{
  OpenDialog1->FileName = "";
  OpenDialog1->Filter = "Affix files (*.aff)|*.aff|All files (*.*)|*.*";
  if (OpenDialog1->Execute()) {
	beAffFile->Text = OpenDialog1->FileName;  
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmAddDictionary::beDicFilePropertiesButtonClick(TObject *Sender,
		  int AButtonIndex)
{
  OpenDialog1->FileName = "";
  OpenDialog1->Filter = "Dictionary files (*.dic)|*.dic|All files (*.*)|*.*";
  if (OpenDialog1->Execute()) {
	beDicFile->Text = OpenDialog1->FileName;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmAddDictionary::beAffFilePropertiesChange(TObject *Sender)
{
  btnAdd->Enabled = FileExists(beAffFile->Text) && FileExists(beDicFile->Text);	
}
//---------------------------------------------------------------------------

