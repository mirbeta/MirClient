//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldUnderConstruction.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxImage"
#pragma link "dxGDIPlusClasses"
#pragma resource "*.dfm"
TfrmUnderConstruction *frmUnderConstruction;
//---------------------------------------------------------------------------
__fastcall TfrmUnderConstruction::TfrmUnderConstruction(TComponent* Owner)
	: TfrmBase(Owner)
{
}
//---------------------------------------------------------------------------
