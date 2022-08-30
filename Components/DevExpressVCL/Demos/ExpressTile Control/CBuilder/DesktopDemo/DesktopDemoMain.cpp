//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DesktopDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxCustomTileControl"
#pragma link "dxGDIPlusClasses"
#pragma link "dxTileControl"
#pragma link "dxAnimation"
#pragma link "cxImageList"
#pragma resource "*.dfm"
TDesktopDemoMainForm *DesktopDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TDesktopDemoMainForm::TDesktopDemoMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
String __fastcall TDesktopDemoMainForm::GetDayName(Word ADayOfWeek)
{
  String AResult;

  switch (ADayOfWeek)
  {
	case 1:
	{
	  AResult == "Sunday";
	  break;
	}
	case 2:
	{
	  AResult == "Monday";
	  break;
	}
	case 3:
	{
	  AResult == "Tuesday";
	  break;
	}
	case 4:
	{
	  AResult == "Wednesday";
	  break;
	}
	case 5:
	{
	  AResult == "Thursday";
	  break;
	}
	case 6:
	{
	  AResult == "Friday";
	  break;
	}
  default:
	{
	  AResult == "Saturday";
	  break;
	}
  }
  return AResult;
}
//---------------------------------------------------------------------------
void __fastcall TDesktopDemoMainForm::FormShow(TObject *Sender)
{
  ItemCalendar->Text2->Value = DayOfTheMonth(Now());
  ItemCalendar->Text4->Value = GetDayName(DayOfWeek(Now()));
  UpdateActionBarsItems();
}
//---------------------------------------------------------------------------
void __fastcall TDesktopDemoMainForm::ItemFoodsActiveFrameChanged(
      TdxTileControlItem *Sender)
{
  const TdxDrawAnimationMode AAnimationModes [2] = {amScrollUp, amScrollDown};
  
  Sender->AnimationMode = AAnimationModes[Sender->AnimationMode == amScrollUp];
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::tcaCustomizeOnClick(
      TdxTileControlActionBarItem *Sender)
{
  dxTileControl1->OptionsBehavior->GroupRenaming = True;
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::tcaExitClick(
	  TdxTileControlActionBarItem *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::dxTileControl1KeyPress(TObject *Sender, Char &Key)
{
  if (Key == VK_ESCAPE)
  {
    if (dxTileControl1->DragAndDropState != ddsNone)
      dxTileControl1->FinishDragAndDrop(False);
    else
      if (dxTileControl1->OptionsBehavior->GroupRenaming)
        dxTileControl1->OptionsBehavior->GroupRenaming = false;
      else
        Close();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::UpdateActionBarsItems()
{
  tcaClearSelection->Visible = dxTileControl1->CheckedItemCount > 0;
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::dxTileControl1ItemCheck(TdxCustomTileControl *Sender, TdxTileControlItem *AItem)
{
  UpdateActionBarsItems();
}
//---------------------------------------------------------------------------

void __fastcall TDesktopDemoMainForm::tcaClearSelectionClick(TdxTileControlActionBarItem *Sender)
{
  for (int I = dxTileControl1->CheckedItemCount - 1; I >= 0 ; I--)
    dxTileControl1->CheckedItems[I]->Checked = false;
}
//---------------------------------------------------------------------------
