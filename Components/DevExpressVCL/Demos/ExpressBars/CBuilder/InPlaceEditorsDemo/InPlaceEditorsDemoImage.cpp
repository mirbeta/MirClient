//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InPlaceEditorsDemoImage.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxImage"
#pragma resource "*.dfm"
TfrmImageEditors *frmImageEditors;
//---------------------------------------------------------------------------
__fastcall TfrmImageEditors::TfrmImageEditors(TComponent* Owner)
	: TEditorDemoBaseFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmImageEditors::SetParameters(TStream* AStream)
{
  if (AStream != NULL)
    cxImage1->Picture->Bitmap->LoadFromStream(AStream);
  else
    cxImage1->Picture->Bitmap = 0;
}

