//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "GridMenuViewsDemoMain.h"
#include "GridMenuViewsDemoData.h"
#include "AboutDemoForm.h"
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
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCardView"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxSpinEdit"
#pragma resource "*.dfm"
TGridMenuViewsDemoMainForm *GridMenuViewsDemoMainForm;

//---------------------------------------------------------------------------
__fastcall TGridMenuViewsDemoMainForm::TGridMenuViewsDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::FormShow(TObject *Sender)
{
 if (GridMenuViewsDemoDataDM->cdsOrders->Active)
 {
  tvOrders->DataController->Groups->FullCollapse();
  tvOrders->DataController->Groups->ChangeExpanding(0, false, false);
  tvOrders->DataController->GotoFirst();
 }
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miCopyToClipboardClick(TObject *Sender)
{
  Variant AValue;
  TcxCustomGridHitTest *AHitTest = GridPopupMenu->HitTest;
#if (__BORLANDC__ >= 0x0610) // C++Builder 12
  TcxGridViewHitType AHitType = GetHitTypeByHitCode(AHitTest->HitTestCode());
#else
  TcxGridViewHitType AHitType = GetHitTypeByHitCode(AHitTest->HitTestCode(AHitTest->ClassType()));
#endif
  if (!dynamic_cast<TcxGridFooterCellViewInfo*>(AHitTest->ViewInfo))
    return;
  switch (AHitType){
    case gvhtFooterCell:
      AValue =
        GetFooterSummaryValue((TcxGridFooterCellHitTest*)AHitTest);
      break;
    case gvhtGroupFooterCell:
      AValue =
        GetGroupFooterSummaryValue((TcxGridGroupFooterCellHitTest*)AHitTest);
  }
  Clipboard()->AsText = VarToStr(AValue);
  MessageDlg("Clipboard: "+ VarToStr(AValue), mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

int TGridMenuViewsDemoMainForm::GetSummaryItemIndexByColumn(TcxDataSummaryItems *ASummaryItems,
  TcxGridColumn *AColumn)
{
  int Result = -1;
  for (int I = 0; I < ASummaryItems->Count; I++)

    if ((((TcxGridTableSummaryItem*)(ASummaryItems->Items[I]))->Column == AColumn)
      && (ASummaryItems->Items[I]->Position == spFooter))
      Result =  I;
  return Result;
}
//---------------------------------------------------------------------------

Variant TGridMenuViewsDemoMainForm::GetFooterSummaryValue(TcxGridFooterCellHitTest *AHitTest)
{
  TcxDataSummary *ASummary = tvOrders->DataController->Summary;
  return ASummary->FooterSummaryValues[
    GetSummaryItemIndexByColumn(ASummary->FooterSummaryItems, AHitTest->Column)];
}
//---------------------------------------------------------------------------

Variant TGridMenuViewsDemoMainForm::GetGroupFooterSummaryValue(TcxGridGroupFooterCellHitTest *AHitTest)
{
  TcxDataSummary *ASummary = tvOrders->DataController->Summary;
  TcxDataControllerGroups *ADataControllerGroups = tvOrders->DataController->Groups;
  int ARowIndex =
    ((TcxGridRowFooterCellViewInfo*)(AHitTest->ViewInfo))->GridRecord->Index;
  int ADataGroupIndex =
    ADataControllerGroups->DataGroupIndexByRowIndex[ARowIndex];
  return ASummary->GroupSummaryValues[ADataGroupIndex]
    [GetSummaryItemIndexByColumn(ASummary->DefaultGroupSummaryItems,
    AHitTest->Column)];
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::FormCreate(TObject *Sender)
{
  InsertMenuItem();
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::GridMenuPopup(TComponent *ASenderMenu,
    TcxCustomGridHitTest *AHitTest, int X, int Y)
{
  PopupMenu->Popup(X, Y);
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miDeleteClick(TObject *Sender)
{
  tvOrders->Controller->DeleteSelection();
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miInsertClick(TObject *Sender)
{
  tvOrders->DataController->Insert();
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miUseBuiltInPopupMenuClick(TObject *Sender)
{
  GridPopupMenu->UseBuiltInPopupMenus = ((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Checked)
  {
    InsertMenuItem();
    miAddCopyToClipboard->Enabled = true;
    miAddCopyToClipboard->Checked = true;
  }
  else
    miAddCopyToClipboard->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miAddCopyToClipboardClick(TObject *Sender)
{
  if (GridPopupMenu->UseBuiltInPopupMenus)
	FMenuItem->Visible = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TGridMenuViewsDemoMainForm::miUseCustomPopupMenuClick(TObject *Sender)
{
  if (((TMenuItem*)Sender)->Checked){
    GridPopupMenu->PopupMenus->Items[0]->OnPopup = GridMenuPopup;
    GridPopupMenu->PopupMenus->Items[0]->PopupMenu = PopupMenu;
  }
  else{
    GridPopupMenu->PopupMenus->Items[0]->OnPopup = NULL;
    GridPopupMenu->PopupMenus->Items[0]->PopupMenu = NULL;
  }
}
//---------------------------------------------------------------------------

void TGridMenuViewsDemoMainForm::InsertMenuItem()
{
  TComponent *AMenu = NULL;
  TcxGridDefaultPopupMenu *ABuiltInMenus = GridPopupMenu->BuiltInPopupMenus;
  for (int I = 0; I < ABuiltInMenus->Count; I++)
    if ((TcxGridViewHitTypes()<<gvhtFooter<<gvhtFooterCell<<gvhtGroupFooter<<gvhtGroupFooterCell) *
      ABuiltInMenus->MenuInfos[I]->HitTypes != TcxGridViewHitTypes())
        AMenu = ABuiltInMenus->MenuInfos[I]->PopupMenu;
  if (AMenu != NULL && AMenu->InheritsFrom(__classid(TPopupMenu)))
  {
    FMenuItem = new TMenuItem(this);
    FMenuItem->Caption = "-";
    ((TPopupMenu*)AMenu)->Items->Add(FMenuItem);
    FMenuItem = new TMenuItem(this);
    FMenuItem->Caption = "Copy To Clipboard";
    FMenuItem->Hint = "Copy the contents to clipboard";
    FMenuItem->OnClick = miCopyToClipboardClick;
    ((TPopupMenu*)AMenu)->Items->Add(FMenuItem);
  }
}
//---------------------------------------------------------------------------



