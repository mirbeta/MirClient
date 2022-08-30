//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CompactLayoutMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TfrmCompactLayout *frmCompactLayout;
//---------------------------------------------------------------------------
__fastcall TfrmCompactLayout::TfrmCompactLayout(TComponent* Owner)
		: TfrmDemoBasicMain(Owner)
{
}

void __fastcall TfrmCompactLayout::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender);
  PivotGrid()->ApplyBestFit();
  PivotGrid()->Customization->Site = this;
  pgfPurchaseDate->ExpandAll();
  pgfCarName->ExpandAll();
  pgfCompanyName->ExpandAll();
  PivotGrid()->Customization->Visible = true;
}

TcxCustomPivotGrid* __fastcall TfrmCompactLayout::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------
void __fastcall TfrmCompactLayout::pgfPaymentTypeGetGroupImageIndex(
	  TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
	  int &AImageIndex, TAlignment &AImageAlignHorz,
	  TcxAlignmentVert &AImageAlignVert)
{
  AnsiString Card = ((TcxPivotGridViewDataItem *)AItem)->Value;
  if (SameText(Card, "Cash")) AImageIndex = 0;
  else if (SameText(Card, "AmEx")) AImageIndex = 1;
	   else if (SameText(Card, "Master")) AImageIndex = 2;
			else if (SameText(Card, "Visa")) AImageIndex = 3;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCompactLayout::miCustomizationClick(TObject * Sender)
{
  PivotGrid()->Customization->Visible = !PivotGrid()->Customization->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCompactLayout::DBPivotGridCustomization(TObject * Sender)
{
  miCustomization->Checked = PivotGrid()->Customization->Visible;
  if (PivotGrid()->Customization->Visible)
    PivotGrid()->Customization->Form->Align = alLeft;
  cxSplitter1->Control = PivotGrid()->Customization->Form;
  cxSplitter1->Visible = PivotGrid()->Customization->Visible;
}
//---------------------------------------------------------------------------
