//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnboundModeDemoFastestSweepers.h"
#include "UnboundModeDemoTypes.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TUnboundModeDemoFastestSweepersForm *UnboundModeDemoFastestSweepersForm;
//---------------------------------------------------------------------------
__fastcall TUnboundModeDemoFastestSweepersForm::TUnboundModeDemoFastestSweepersForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

int __fastcall TUnboundModeDemoFastestSweepersForm::ShowModal()
{
  SetFormPosition(this, 35, 45);
  return (TForm::ShowModal());
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoFastestSweepersForm::FormCreate(
      TObject *Sender)
{
  FastestTimesResetted = false;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoFastestSweepersForm::btnResetScoresClick(
      TObject *Sender)
{
  if (FastestTimesResetted)
    return;
  FastestTimesResetted = true;
  lbBeginnerTime->Caption = IntToStr(999);
  lbIntermediateTime->Caption = IntToStr(999);
  lbExpertTime->Caption = IntToStr(999);
  lbBeginnerName->Caption = "Anonymous";
  lbIntermediateName->Caption = "Anonymous";
  ibExpertName->Caption = "Anonymous";
}
//---------------------------------------------------------------------------


