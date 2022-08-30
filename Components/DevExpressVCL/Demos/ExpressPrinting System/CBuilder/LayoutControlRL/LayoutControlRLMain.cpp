//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "LayoutControlRLMain.h"
#include "DemoDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxDrawTextUtils"
#pragma link "cxGraphics"
#pragma link "DemoBasicMain"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSCore"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "dxLayoutLookAndFeels"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBEdit"
#pragma link "cxDBNavigator"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxNavigator"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxTimeEdit"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxPSContainerLnk"
#pragma link "dxPScxDBEditorLnks"
#pragma link "dxPSdxLCLnk"
#pragma link "dxPSTextLnk"
#pragma resource "*.dfm"
TLayoutControlMainForm *LayoutControlMainForm;
//---------------------------------------------------------------------------
__fastcall TLayoutControlMainForm::TLayoutControlMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TLayoutControlMainForm::acLayoutStyleExecute(TObject *Sender)
{
  int ATag = ((TAction*)Sender)->Tag;
  switch (ATag) {
	case 0:
	  lcMain->LayoutLookAndFeel = dxLayoutStandardLookAndFeel1;
	  break;
	case 1:
	  lcMain->LayoutLookAndFeel = dxLayoutOfficeLookAndFeel1;
	  break;
	case 2:
	  lcMain->LayoutLookAndFeel = dxLayoutWebLookAndFeel1;
	  break;
  default:
	lcMain->LayoutLookAndFeel = dxLayoutCxLookAndFeel1;
	switch (ATag) {
	  case 3:
	  case 4:
	  case 5:
	  case 6:
		dxLayoutCxLookAndFeel1->LookAndFeel->NativeStyle = false;
		dxLayoutCxLookAndFeel1->LookAndFeel->Kind = TcxLookAndFeelKind(ATag - 3);
		break;
	  case 7:
		dxLayoutCxLookAndFeel1->LookAndFeel->NativeStyle = true;
		break;
	};
  };
}
//---------------------------------------------------------------------------

void __fastcall TLayoutControlMainForm::dxLayoutGroup1Button0Click(TObject *Sender)
{
  dxLayoutGroup1->Parent = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TLayoutControlMainForm::Customization1Click(TObject *Sender)
{
  lcMain->Customization = true;
}
//---------------------------------------------------------------------------


void __fastcall TLayoutControlMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  acLayoutStandard->Execute();
}
//---------------------------------------------------------------------------

