//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "VirtualModeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxCalendar"
#pragma link "cxInplaceContainer"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"

TfmVirtualModeDemoMain *fmVirtualModeDemoMain;
//---------------------------------------------------------------------------
__fastcall TfmVirtualModeDemoMain::TfmVirtualModeDemoMain(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TfmVirtualModeDemoMain::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  sbMain->AutoHint = false;
  SmartLoad = true;
}
//---------------------------------------------------------------------------


void __fastcall TfmVirtualModeDemoMain::miShowTreeLinesClick(TObject *Sender)
{
  TreeList->OptionsView->TreeLineStyle =
    (TcxTreeListTreeLineStyle)((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miShowIndicatorClick(TObject *Sender)
{
  TreeList->OptionsView->Indicator = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miShowRootClick(TObject *Sender)
{
  TreeList->OptionsView->ShowRoot = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miShowButtonsClick(TObject *Sender)
{
  TreeList->OptionsView->Buttons = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miSmartLoadModeClick(TObject *Sender)
{
  SmartLoad = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miCellAutoHeightClick(TObject *Sender)
{
  TreeList->OptionsView->CellAutoHeight = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miCellEndEllipsisClick(TObject *Sender)
{
  TreeList->OptionsView->CellEndEllipsis = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::miColumnAutoWidthClick(TObject *Sender)
{
  TreeList->OptionsView->ColumnAutoWidth = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::ShowLoadingTime(int ALoadingTime)
{
  sbMain->Panels->Items[1]->Text = "Loaded in " + MsecToStr(ALoadingTime) + " s";
  ShowPerformance(false);
}

void __fastcall TfmVirtualModeDemoMain::TreeListExpanding(
  TcxCustomTreeList *Sender, TcxTreeListNode *ANode, bool &Allow)
{
  if (!FStartExpanding)
  {
    FStartExpanding = True;
    FNodeCount = TreeList->AbsoluteCount;
    FStartExpandingTick = GetTickCount();
    PostMessage(Handle, WM_TREELISTEXPANDED, 0, 0);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::WMTreeListExpanded(TMessage &Message)
{
  FStartExpanding = false;
  FStartExpandingTick = GetTickCount() - FStartExpandingTick;
  ShowPerformance(true);
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::ShowPerformance(bool AExpanded)
{
  sbMain->Panels->Items[0]->Text = "Total nodes: " + IntToStr(TreeList->AbsoluteCount);
  if (AExpanded)
  {
    sbMain->Panels->Items[2]->Text = "Expanded in " + MsecToStr(FStartExpandingTick) + " s";
    if (SmartLoad)
    {
      sbMain->Panels->Items[2]->Text = sbMain->Panels->Items[2]->Text + ", " +
        IntToStr(TreeList->AbsoluteCount - FNodeCount) +
        " nodes have been created";
    }
  }
  else
  {
    sbMain->Panels->Items[2]->Text = "";
  }
}
//---------------------------------------------------------------------------

AnsiString __fastcall TfmVirtualModeDemoMain::MsecToStr(int AMsec)
{
  return Format(AnsiString("%2.3f"), ARRAYOFCONST(((float) AMsec / 1000)));
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::FullExpand1Click(TObject *Sender)
{
  TreeList->FullExpand();
}
//---------------------------------------------------------------------------

bool _fastcall TfmVirtualModeDemoMain::GetSmartLoad()
{
  return TreeList->OptionsData->SmartLoad;
}

void __fastcall TfmVirtualModeDemoMain::SetSmartLoad(bool AValue)
{
  ShowHourglassCursor();
  __try
	{
	  TreeList->OnGetChildCount = NULL;
	  TreeList->OptionsData->SmartLoad = AValue;
	  TreeList->OnGetChildCount = TreeListGetChildCount;
	  int ALoadingTime = GetTickCount();
	  TreeList->FullRefresh();
	  ShowLoadingTime(GetTickCount() - ALoadingTime);
	}
  __finally
	{
      HideHourglassCursor();
	}

}
void __fastcall TfmVirtualModeDemoMain::TreeListGetChildCount(TcxCustomTreeList *Sender,
          TcxTreeListNode *AParentNode, int &ACount)
{
  if (AParentNode->Level < 4)
	ACount = 10;
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::TreeListGetNodeValue(TcxCustomTreeList *Sender,
          TcxTreeListNode *ANode, TcxTreeListColumn *AColumn, Variant &AValue)
{
  switch (AColumn->ItemIndex) {
	case 0:
	  AValue = ANode->VisibleIndex; break;
	case 1:
	  AValue = "Level: " + IntToStr(ANode->Level); break;
	case 2:
	  AValue = Now() + ANode->VisibleIndex * 0.001; break;
  default:
	AValue = Null;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmVirtualModeDemoMain::TreeListDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept)
{
 //
}
//---------------------------------------------------------------------------

