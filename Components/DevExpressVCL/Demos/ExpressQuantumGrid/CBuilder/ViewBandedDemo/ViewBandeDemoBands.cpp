//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewBandeDemoBands.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxListBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TViewBandeDemoBandsForm *ViewBandeDemoBandsForm;
//---------------------------------------------------------------------------
__fastcall TViewBandeDemoBandsForm::TViewBandeDemoBandsForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TViewBandeDemoBandsForm::btnCancelClick(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------
