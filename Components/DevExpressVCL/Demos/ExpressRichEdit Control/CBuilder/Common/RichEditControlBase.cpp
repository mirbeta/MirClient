//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RichEditControlBase.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "RichEditControlDemoAppGlyphs.res"
#pragma resource "*.dfm"

//http://qc.embarcadero.com/wc/qcmain.aspx?d=99647
#if defined(_WIN32) && !defined(_WIN64)
	#pragma alias "toupper" = "_toupper"
	#pragma alias "tolower" = "_tolower"
	#pragma alias "isupper" = "_isupper"
	#pragma alias "isalnum" = "_isalnum"
	#pragma alias "isalpha" = "_isalpha"
	#pragma alias "iscntrl" = "_iscntrl"
	#pragma alias "isdigit" = "_isdigit"
	#pragma alias "isgraph" = "_isgraph"
	#pragma alias "isleadbyte" = "_isleadbyte"
	#pragma alias "islower" = "_islower"
	#pragma alias "isprint" = "_isprint"
	#pragma alias "ispunct" = "_ispunct"
	#pragma alias "isspace" = "_isspace"
	#pragma alias "isxdigit" = "_isxdigit"
	#pragma alias "strchr" = "_strchr"
	#pragma alias "strncmp" = "_strncmp"
	#pragma alias "memcmp" = "_memcmp"
	#pragma alias "memmove" = "_memmove"
#endif

TfrmRichEditControlBase *frmRichEditControlBase;
//---------------------------------------------------------------------------

__fastcall TfrmRichEditControlBase::TfrmRichEditControlBase(TComponent* Owner)
	: TdxRibbonForm(Owner)
{
}

void __fastcall TfrmRichEditControlBase::acQATAboveAndBelowRibbonUpdate(TObject *Sender)
{
  acQATAboveRibbon->Checked = Ribbon->QuickAccessToolbar->Position == qtpAboveRibbon;
  acQATBelowRibbon->Checked = Ribbon->QuickAccessToolbar->Position == qtpBelowRibbon;
}

void __fastcall TfrmRichEditControlBase::acQATAboveAndBelowRibbonExecute(TObject *Sender)
{
  if (((TAction *)Sender)->Tag != 0)
	Ribbon->QuickAccessToolbar->Position = qtpBelowRibbon;
  else
	Ribbon->QuickAccessToolbar->Position = qtpAboveRibbon;
}

void __fastcall TfrmRichEditControlBase::acQATApplicationButtonExecute(TObject *Sender)
{
  Ribbon->ApplicationButton->Visible = bbApplicationButton->Down;
}

void __fastcall TfrmRichEditControlBase::bbOptionsClick(TObject *Sender)
{
  TdxRibbonColorSchemeAccent AColorSchemeAccent = Ribbon->ColorSchemeAccent;
  TRibbonDemoStyle AStyle = RibbonDemoStyle;
  TScreenTipOptions* AScreenTipOptions = new TScreenTipOptions;
  try
  {
	AScreenTipOptions->ShowScreenTips = bmBarManager->ShowHint;
	AScreenTipOptions->ShowDescripitons = stBarScreenTips->ShowDescription;
	if (ExecuteRibbonDemoOptions(AScreenTipOptions, &AStyle, &AColorSchemeAccent))
	{
	  RibbonDemoStyle = AStyle;
	  Ribbon->ColorSchemeAccent = AColorSchemeAccent;
	  bmBarManager->ShowHint = AScreenTipOptions->ShowScreenTips;
	  stBarScreenTips->ShowDescription = AScreenTipOptions->ShowDescripitons;
	  UpdateColorSchemeRelatedControls();
	}
  }
  __finally
  {
	delete AScreenTipOptions;
  };
}

void __fastcall TfrmRichEditControlBase::bbQATVisibleClick(TObject *Sender)
{
  Ribbon->QuickAccessToolbar->Visible = bbQATVisible->Down;
}

void __fastcall TfrmRichEditControlBase::bbRibbonFormClick(TObject *Sender)
{
  Ribbon->SupportNonClientDrawing = bbRibbonForm->Down;
}

void __fastcall TfrmRichEditControlBase::FormCreate(TObject *Sender)
{
  InitializeLookAndFeel();
  bbRibbonForm->Down = Ribbon->SupportNonClientDrawing;
  bbApplicationButton->Down = Ribbon->ApplicationButton->Visible;
  bbQATVisible->Down = Ribbon->QuickAccessToolbar->Visible;
}

TRibbonDemoStyle TfrmRichEditControlBase::GetRibbonDemoStyle()
{
  switch(Ribbon->Style)
  {
	case rs2007:
	  return rdsOffice2007;
	case rs2013:
	  return rdsOffice2013;
	case rs2016:
	  return rdsOffice2016;
	case rs2016Tablet:
	  return rdsOffice2016Tablet;
	case rs2019:
	  return rdsOffice2019;
  }
  if (Ribbon->EnableTabAero)
	return rdsOffice2010;
  else
	return rdsScenic;
}

void TfrmRichEditControlBase::InitializeLookAndFeel()
{
  RibbonDemoStyle = rdsOffice2019;
}

void __fastcall TfrmRichEditControlBase::scgiLookAndFeelPopulate(TObject *Sender)
{
//do nothing
}

void __fastcall TfrmRichEditControlBase::scgiLookAndFeelSelected(TObject *Sender, const UnicodeString ASkinName)
{
  UpdateColorSchemeRelatedControls();
}

void TfrmRichEditControlBase::UpdateColorSchemeRelatedControls()
{
//do nothing
}

void TfrmRichEditControlBase::SetRibbonDemoStyle(TRibbonDemoStyle AStyle)
{
  const String NamesMap[rs2019 + 1] = {"RIBBONAPPGLYPH", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010", "RIBBONAPPGLYPH2010"};
  Ribbon->Style = RibbonDemoStyleToRibbonStyle(AStyle);
  if ((AStyle == rdsOffice2007) || (AStyle == rdsScenic))
  {
	Ribbon->EnableTabAero = False;
	Ribbon->ApplicationButton->Menu = ApplicationMenu;
  }
  else
  {
	Ribbon->EnableTabAero = True;
  }

  Ribbon->ApplicationButton->Glyph->LoadFromResource((unsigned int)HInstance, NamesMap[Ribbon->Style], (wchar_t*)RT_BITMAP);
  Ribbon->ApplicationButton->StretchGlyph = Ribbon->Style == rs2007;
  DisableAero = (AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet) || (AStyle == rdsOffice2019);
  UpdateColorSchemeRelatedControls();
}
