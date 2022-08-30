//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListSchedule.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IssueListForm"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDBEdit"
#pragma link "cxEdit"
#pragma link "cxTextEdit"
#pragma link "cxGraphics"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma resource "*.dfm"
TfrmSchedule *frmSchedule;
//---------------------------------------------------------------------------
__fastcall TfrmSchedule::TfrmSchedule(TComponent* Owner)
  : TfrmBasic(Owner)
{
}
//---------------------------------------------------------------------------
