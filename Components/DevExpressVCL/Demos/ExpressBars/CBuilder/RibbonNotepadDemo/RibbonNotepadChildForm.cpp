//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonNotepadChildForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "NotepadChildForm"
#pragma resource "*.dfm"
TfrmRibbonNotepadChild *frmRibbonNotepadChild;
//---------------------------------------------------------------------------
__fastcall TfrmRibbonNotepadChild::TfrmRibbonNotepadChild(TComponent* Owner)
	: TfrmNotepadChild(Owner)
{
}
//---------------------------------------------------------------------------
