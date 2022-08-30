//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TformAboutDemo *FForm;

//---------------------------------------------------------------------------

void ShowAboutDemoForm()
{
  if (FForm == NULL){
    TStringList *ADescription = new TStringList();
    try{
      ADescription->LoadFromFile(ExtractFilePath(Application->ExeName) + "About.txt");
      FForm = new TformAboutDemo(ADescription->Text);
    }
    __finally{
      delete ADescription;
    }
  }
  FForm->Show();
}
//---------------------------------------------------------------------------
__fastcall TformAboutDemo::TformAboutDemo(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
__fastcall TformAboutDemo::TformAboutDemo(const String ADescription)
        : TForm(Application)
{
  AssignBounds();
  redDescription->Lines->Text = ADescription;
}
//---------------------------------------------------------------------------
void __fastcall TformAboutDemo::AssignBounds()
{
  Left = Application->MainForm->BoundsRect.Right;
  Top = Application->MainForm->BoundsRect.Top;
  Height = Application->MainForm->Height;
  TRect ADesktopArea = GetDesktopWorkArea(Point(Left, Top));
  if (BoundsRect.Right > ADesktopArea.Right){
    int AOffset = BoundsRect.Right - ADesktopArea.Right;
    Left = Left - AOffset;
    if (Application->MainForm->Left > AOffset)
      Application->MainForm->Left = Application->MainForm->Left - AOffset;
    else
      Application->MainForm->Left = 0;
  }
}
