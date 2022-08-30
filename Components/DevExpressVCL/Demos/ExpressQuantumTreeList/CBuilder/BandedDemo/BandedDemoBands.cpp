//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BandedDemoBands.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxButtons"
#pragma link "cxListBox"
#pragma link "cxCustomListBox"
#pragma link "cxEdit"
#pragma resource "*.dfm"
TBandedDemoBandsForm *BandedDemoBandsForm;
//---------------------------------------------------------------------------
__fastcall TBandedDemoBandsForm::TBandedDemoBandsForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TBandedDemoBandsForm::btnCancelClick(TObject &Sender)
{
  Close();
}
//---------------------------------------------------------------------------
