//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListProjects.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IssueListForm"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDBEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxLookupEdit"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBLookupEdit"
#pragma link "cxGraphics"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxNavigator"
#pragma resource "*.dfm"
TfrmProjects *frmProjects;
//---------------------------------------------------------------------------
__fastcall TfrmProjects::TfrmProjects(TComponent* Owner)
  : TfrmBasic(Owner)
{
}
//---------------------------------------------------------------------------
