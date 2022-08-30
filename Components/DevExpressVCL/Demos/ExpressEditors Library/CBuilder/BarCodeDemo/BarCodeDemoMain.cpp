//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BarCodeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxPC"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxBarCode"
#pragma link "dxBevel"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"

TdxBarCodeDemoForm *dxBarCodeDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxBarCodeDemoForm::TdxBarCodeDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
  FIndexToSymbologyClassName[0] = "TdxBarCode11Symbology";
  FIndexToSymbologyClassName[1] = "TdxBarCode39Symbology";
  FIndexToSymbologyClassName[2] = "TdxBarCode39ExtendedSymbology";
  FIndexToSymbologyClassName[3] = "TdxBarCode93Symbology";
  FIndexToSymbologyClassName[4] = "TdxBarCode93ExtendedSymbology";
  FIndexToSymbologyClassName[5] = "TdxBarCode128Symbology";
  FIndexToSymbologyClassName[6] = "TdxBarCodeEAN8Symbology";
  FIndexToSymbologyClassName[7] = "TdxBarCodeEAN13Symbology";
  FIndexToSymbologyClassName[8] = "TdxBarCodeInterleaved2Of5Symbology";
  FIndexToSymbologyClassName[9] = "TdxBarCodeMSISymbology";
  FIndexToSymbologyClassName[10] = "TdxBarCodeUPCASymbology";
  FIndexToSymbologyClassName[11] = "TdxBarCodeUPCESymbology";
  FIndexToSymbologyClassName[12] = "TdxBarCodeQRCodeSymbology";
}
//---------------------------------------------------------------------------
void __fastcall TdxBarCodeDemoForm::tcMainChange(TObject *Sender)
{
  if (tcMain->TabIndex < 0)
	exit(0);

  FLoading = True;
  try
  {
	BarCode->Style->Font->Size = seFontSize->Value;
	BarCode->Properties->BeginUpdate();
	try
	{
	  BarCode->Properties->BarCodeSymbologyClassName = FIndexToSymbologyClassName[tcMain->TabIndex];
	  pcSettings->Pages[1]->Enabled = (tcMain->TabIndex == 1) || (tcMain->TabIndex == 2)|| (tcMain->TabIndex == 5) ||
		(tcMain->TabIndex == 8) || (tcMain->TabIndex == 12);
	  if (!pcSettings->Pages[1]->Enabled)
		pcSettings->ActivePageIndex = 0;

	  gbFontSize->Visible = !((tcMain->TabIndex == 6) || (tcMain->TabIndex == 7) || (tcMain->TabIndex == 10) ||
		(tcMain->TabIndex == 11));
	  gbWideNarrowRatio->Visible = (tcMain->TabIndex == 1) || (tcMain->TabIndex == 2) || (tcMain->TabIndex == 8);
	  gbWideNarrowRatio->Top = 500;
	  gbCalculateCheckSum->Visible = gbWideNarrowRatio->Visible;
	  gbCalculateCheckSum->Top = 500;
	  gbCharacterSet->Visible = tcMain->TabIndex == 5;
	  gbCalculateCheckSum->Top = 500;
	  gbCompactionMode->Visible = tcMain->TabIndex == 12;
	  gbCalculateCheckSum->Top = 500;
	  gbErrorCorrectionLevel->Visible = gbCompactionMode->Visible;
	  gbCalculateCheckSum->Top = 500;
	  gbSizeVersion->Visible = gbCompactionMode->Visible;
	  gbCalculateCheckSum->Top = 500;
	  BarCode->Properties->RotationAngle = (TcxRotationAngle)(cbRotationAngle->ItemIndex);
	  BarCode->Properties->FitMode = (TdxBarCodeFitMode)(cbFitMode->ItemIndex);
	  if (dynamic_cast<TdxBarCode128Symbology*>(BarCode->Properties->Symbology))
		dynamic_cast<TdxBarCode128Symbology*>(BarCode->Properties->Symbology)->CharacterSet = (TdxBarCode128CharacterSet)(cbCharacterSet->ItemIndex);
	  if (dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology))
	  {
		dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->CompactionMode = (TdxQRCodeCompactionMode)(cbCompactionMode->ItemIndex);
		dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->ErrorCorrectionLevel = (TdxQRCodeErrorCorrectionLevel)(cbErrorCorrectionLevel->ItemIndex);
		dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->Version = cbSizeVersion->ItemIndex;
	  }
	  SetWideNarrowRatio();
	  SetCalculateCheckSum();
	  switch (tcMain->TabIndex)
	  {
		case 0:
		{
		  memText->Text = "01234-56789";
		}; break;
		case 1:
		{
		  memText->Text = "ABC-1234";
		}; break;
		case 2:
		{
		  memText->Text = "Abc-123";
		}; break;
		case 3:
		{
		  memText->Text = "ABC-1234";
		}; break;
		case 4:
		{
		  memText->Text = "Abc-123";
		}; break;
		case 5:
		{
		  memText->Text = "Abc-123";
		}; break;
		case 6:
		{
		  memText->Text = "0123456";
		}; break;
		case 7:
		{
		  memText->Text = "012345678901";
		}; break;
		case 8:
		{
		  memText->Text = "012345678901";
		}; break;
		case 9:
		{
		  memText->Text = "012345678901";
		}; break;
		case 10:
		{
		  memText->Text = "01234567890";
		}; break;
		case 11:
		{
		  memText->Text = "0123456";
		}; break;
		case 12:
		{
		  memText->Text = "https://www.devexpress.com/";
		}
	  }
	  if (tcMain->TabIndex == 12)
	  {
		seModuleWidth->Value = 5;
	  }
	  else
		seModuleWidth->Value = 2;

	  tsShowText->Checked = tcMain->TabIndex != 12;
	  BarCode->Text = memText->Text;
	  BarCode->Properties->ShowText = tsShowText->Checked;
	  BarCode->Properties->ModuleWidth = seModuleWidth->Value;
	}
	__finally
	{
	  BarCode->Properties->EndUpdate();
	}
  }
  __finally
  {
	FLoading = False;
  }
}
//---------------------------------------------------------------------------
void TdxBarCodeDemoForm::SetWideNarrowRatio()
{
  if (dynamic_cast<TdxBarCodeInterleaved2Of5Symbology*>(BarCode->Properties->Symbology))
  {
	dynamic_cast<TdxBarCodeInterleaved2Of5Symbology*>(BarCode->Properties->Symbology)->WideNarrowRatio = seWideNarrowRatio->Value;
  }
  else
  {
	if (dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology))
	{
	  dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology)->WideNarrowRatio = seWideNarrowRatio->Value;
	}
	else
	{
	  if (dynamic_cast<TdxBarCode39ExtendedSymbology*>(BarCode->Properties->Symbology))
		dynamic_cast<TdxBarCode39ExtendedSymbology*>(BarCode->Properties->Symbology)->WideNarrowRatio = seWideNarrowRatio->Value;
	}
  }
}
//---------------------------------------------------------------------------
void TdxBarCodeDemoForm::SetCalculateCheckSum()
{
  if (dynamic_cast<TdxBarCodeInterleaved2Of5Symbology*>(BarCode->Properties->Symbology))
  {
	dynamic_cast<TdxBarCodeInterleaved2Of5Symbology*>(BarCode->Properties->Symbology)->Checksum = tsCalculateCheckSum->Checked;
  }
  else
  {
	if (dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology))
	{
	  dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology)->Checksum = tsCalculateCheckSum->Checked;
	}
	else
	{
	  if (dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology))
		dynamic_cast<TdxBarCode39Symbology*>(BarCode->Properties->Symbology)->Checksum = tsCalculateCheckSum->Checked;
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::memTextPropertiesChange(TObject *Sender)
{
  if (!FLoading)
	BarCode->Text = memText->Text;
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::seFontSizePropertiesChange(TObject *Sender)
{
  if (!FLoading && (seFontSize->Value > 0))
	BarCode->Style->Font->Size = seFontSize->Value;
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::cbRotationAnglePropertiesChange(TObject *Sender)

{
  if (!FLoading)
	BarCode->Properties->RotationAngle = TcxRotationAngle(cbRotationAngle->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::seModuleWidthPropertiesChange(TObject *Sender)
{
  if (!FLoading && (seModuleWidth->Value > 0))
	BarCode->Properties->ModuleWidth = seModuleWidth->Value;
}
//---------------------------------------------------------------------------


void __fastcall TdxBarCodeDemoForm::cbFitModePropertiesChange(TObject *Sender)
{
  if (!FLoading)
	BarCode->Properties->FitMode = TdxBarCodeFitMode(cbFitMode->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::tsShowTextPropertiesChange(TObject *Sender)
{
  if (!FLoading)
	BarCode->Properties->ShowText = tsShowText->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::seWideNarrowRatioPropertiesChange(TObject *Sender)
{
  if (!FLoading && (seWideNarrowRatio->Value > 0))
	SetWideNarrowRatio();
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::tsCalculateCheckSumPropertiesChange(TObject *Sender)
{
  if (!FLoading)
	SetCalculateCheckSum();
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::cbCharacterSetPropertiesChange(TObject *Sender)
{
  if (!FLoading && dynamic_cast<TdxBarCode128Symbology*>(BarCode->Properties->Symbology))
	  dynamic_cast<TdxBarCode128Symbology*>(BarCode->Properties->Symbology)->CharacterSet = (TdxBarCode128CharacterSet)(cbCharacterSet->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::cbCompactionModePropertiesChange(TObject *Sender)
{
  if (!FLoading && dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology))
	dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->CompactionMode = (TdxQRCodeCompactionMode)(cbCompactionMode->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::cbErrorCorrectionLevelPropertiesChange(TObject *Sender)
{
  if (!FLoading && dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology))
	dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->ErrorCorrectionLevel = (TdxQRCodeErrorCorrectionLevel)(cbErrorCorrectionLevel->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::cbSizeVersionPropertiesChange(TObject *Sender)
{
  if (!FLoading && dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology))
	dynamic_cast<TdxBarCodeQRCodeSymbology*>(BarCode->Properties->Symbology)->Version = cbSizeVersion->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TdxBarCodeDemoForm::FormCreate(TObject *Sender)
{
  tcMain->TabIndex = 0;
  pcSettings->ActivePageIndex = 0;
  memText->Text = "0123456789000";
  tcMainChange(Sender);
}
//---------------------------------------------------------------------------

