//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AboutDemoForm.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TformAboutDemo *FForm;

//---------------------------------------------------------------------------

void ShowAboutDemoForm()
{
  if (FForm == NULL){
	FForm = new TformAboutDemo(Application);
	TStringList *ADescription = new TStringList();
	try{
	  ADescription->LoadFromFile(ExtractFilePath(Application->ExeName) + "About.txt");
	  FForm->Show();
	  FForm->redDescription->Lines->Text = ADescription->Text;
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
  AssignBounds();
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
