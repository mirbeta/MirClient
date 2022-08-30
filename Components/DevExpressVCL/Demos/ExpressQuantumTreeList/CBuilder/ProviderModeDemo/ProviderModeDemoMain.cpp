//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ProviderModeDemoMain.h"
#include "ProviderModeDemoClasses.h"
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

TProviderModeDemoMainForm *ProviderModeDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TProviderModeDemoMainForm::TProviderModeDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TProviderModeDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  sbMain->AutoHint = false;
  SmartLoad = true;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::FormDestroy(TObject *Sender)
{
  delete TreeList->DataController->CustomDataSource;
  TreeList->DataController->CustomDataSource = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miShowTreeLinesClick(TObject *Sender)
{
  TreeList->OptionsView->TreeLineStyle =
    (TcxTreeListTreeLineStyle)((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miShowIndicatorClick(TObject *Sender)
{
  TreeList->OptionsView->Indicator = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miShowRootClick(TObject *Sender)
{
  TreeList->OptionsView->ShowRoot = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miShowButtonsClick(TObject *Sender)
{
  TreeList->OptionsView->Buttons = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miSmartLoadModeClick(TObject *Sender)
{
  SmartLoad = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miCellAutoHeightClick(TObject *Sender)
{
  TreeList->OptionsView->CellAutoHeight = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miCellEndEllipsisClick(TObject *Sender)
{
  TreeList->OptionsView->CellEndEllipsis = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miColumnAutoWidthClick(TObject *Sender)
{
  TreeList->OptionsView->ColumnAutoWidth = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::ShowLoadingTime(int ALoadingTime)
{
  sbMain->Panels->Items[1]->Text = "Loaded in " + MsecToStr(ALoadingTime) + " s";
  ShowPerformance(false);
}

void __fastcall TProviderModeDemoMainForm::TreeListExpanding(
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

void __fastcall TProviderModeDemoMainForm::WMTreeListExpanded(TMessage &Message)
{
  FStartExpanding = false;
  FStartExpandingTick = GetTickCount() - FStartExpandingTick;
  ShowPerformance(true);
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::ShowPerformance(bool AExpanded)
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

AnsiString __fastcall TProviderModeDemoMainForm::MsecToStr(int AMsec)
{
  return Format(AnsiString("%2.3f"), ARRAYOFCONST(((float) AMsec / 1000)));
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::FullExpand1Click(TObject *Sender)
{
  TreeList->FullExpand();
}
//---------------------------------------------------------------------------

bool _fastcall TProviderModeDemoMainForm::GetSmartLoad()
{
  return TreeList->OptionsData->SmartLoad;
}

void __fastcall TProviderModeDemoMainForm::SetSmartLoad(bool AValue)
{
  TreeList->OptionsData->SmartLoad = AValue; 
  int ALoadingTime = GetTickCount();
  RecreateDemoDataSource(TreeList);
  ShowLoadingTime(GetTickCount() - ALoadingTime);
}
void __fastcall TProviderModeDemoMainForm::TreeListDragOver(TObject *Sender, TObject *Source,
          int X, int Y, TDragState State, bool &Accept)
{
//
}
//---------------------------------------------------------------------------

