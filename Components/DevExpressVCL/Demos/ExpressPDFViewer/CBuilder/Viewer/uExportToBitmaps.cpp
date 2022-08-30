//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uExportToBitmaps.h"
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
#pragma resource "*.dfm"
TfrmExportToBitmaps *frmExportToBitmaps;
//---------------------------------------------------------------------------
__fastcall TfrmExportToBitmaps::TfrmExportToBitmaps(TComponent* Owner)
	: TfrmExportToFileDialog(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmExportToBitmaps::FormCreate(TObject *Sender)
{
  cbOpenAfterExport->Caption = "Open folder after export";
}
//---------------------------------------------------------------------------

