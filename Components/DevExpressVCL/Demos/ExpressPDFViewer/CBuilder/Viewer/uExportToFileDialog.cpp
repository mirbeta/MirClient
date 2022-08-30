//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uExportToFileDialog.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxCheckBox"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TfrmExportToFileDialog *frmExportToFileDialog;
//---------------------------------------------------------------------------
__fastcall TfrmExportToFileDialog::TfrmExportToFileDialog(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
