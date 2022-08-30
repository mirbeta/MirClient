//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Puzzle.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "cxClasses"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TfrmPuzzle *frmPuzzle;
//---------------------------------------------------------------------------
__fastcall TfrmPuzzle::TfrmPuzzle(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmPuzzle::FormCreate(TObject *Sender)
{
  Position = poDesigned;
}
//---------------------------------------------------------------------------

void __fastcall TfrmPuzzle::About1Click(TObject *Sender)
{
  ShowMessage("Solve the puzzle by dragging items");
}
//---------------------------------------------------------------------------

void __fastcall TfrmPuzzle::FormShow(TObject *Sender)
{
  Shufflepuzzle1Click(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TfrmPuzzle::Shufflepuzzle1Click(TObject *Sender)
{
  TList* AList = new TList;
  __try {
	AList->Add(lcItem1);
	AList->Add(lcItem2);
	AList->Add(lcItem3);
	AList->Add(lcItem4);
	AList->Add(lcItem5);
	AList->Add(lcItem6);
	AList->Add(lcItem7);
	AList->Add(lcItem8);
	AList->Add(lcItem9);
	lcMain->BeginUpdate();
	__try {
	  lcMain->Items->LayoutDirection = ldVertical;
	  Randomize;
	  for (int I = 0; I <= 2; I++) {
		TdxCustomLayoutGroup* AGroup = lcMain->CreateGroup(__classid(TdxLayoutGroup), lcMainGroup_Root1);
		AGroup->LayoutDirection = ldHorizontal;
		AGroup->Hidden = True;
		AGroup->MoveTo(lcMain->Items, 0);
		for (int J = 0; J <= 2; J++) {
		  TdxLayoutItem* AItem = (TdxLayoutItem*)(AList->Items[Random(AList->Count)]);
		  AList->Extract(AItem);
		  AItem->MoveTo(AGroup, J);
		};
	  };
	}
	__finally {
	  lcMain->EndUpdate();
	};
  }
  __finally {
	AList->Free();
  };
}
//---------------------------------------------------------------------------

