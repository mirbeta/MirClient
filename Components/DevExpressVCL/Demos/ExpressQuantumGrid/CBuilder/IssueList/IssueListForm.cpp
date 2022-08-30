//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListForm.h"
#include "IssueListMain.h"
#include "IssueListGrid.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxNavigator"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TfrmBasic *frmBasic;
//---------------------------------------------------------------------------
__fastcall TfrmBasic::TfrmBasic(TComponent* Owner)
  : TForm(Owner)
{
  Visible = false;
  Align = alClient;
  FGridForm = IssueListMainForm->GridForm;
}
//---------------------------------------------------------------------------

String TfrmBasic::GetCaption()
{
  return lbCaption->Caption;
}
//---------------------------------------------------------------------------
void TfrmBasic::SetCaption(String ACaption)
{
  lbCaption->Caption = "  " + ACaption;
}
//---------------------------------------------------------------------------

