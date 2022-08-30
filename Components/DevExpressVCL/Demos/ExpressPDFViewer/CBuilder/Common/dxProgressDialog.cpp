//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dxProgressDialog.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxProgressBar"
#pragma resource "*.dfm"
TfrmProgress *frmProgress;
//---------------------------------------------------------------------------
__fastcall TfrmProgress::TfrmProgress(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
