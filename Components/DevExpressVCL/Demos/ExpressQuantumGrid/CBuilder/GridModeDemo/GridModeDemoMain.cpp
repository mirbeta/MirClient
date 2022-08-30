//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "GridModeDemoMain.h"
#include "GridModeDemoData.h"
#include "AboutDemoForm.h"
#include "GridModeDemoTerminate.h"
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
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCalc"
#pragma link "cxCalendar"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxCurrencyEdit"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImageComboBox"
#pragma link "cxProgressBar"
#pragma link "cxSpinEdit"
#pragma resource "*.dfm"
TGridModeDemoMainForm *GridModeDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TGridModeDemoMainForm::TGridModeDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::FormCreate(TObject *Sender)
{
  try{
    GridModeDemoTerminateForm = new TGridModeDemoTerminateForm(this);
    GridModeDemoTerminateForm->lbDesc->Caption = strLoadData;
    GridModeDemoTerminateForm->Show();
    Application->ProcessMessages();
    GridModeDemoDataDM->CopyDBToLocalPlace(false);
    GridModeDemoDataDM->OpenTables(true);
    InitPopulateButton();
  }
  __finally{
    GridModeDemoTerminateForm->Close();
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoMainForm::EnableSummaries(bool AEnable, TcxGridDBTableView *AView)
{
  TcxAfterSummaryEvent AAfterSummaryEvent = NULL;
  if (AEnable)
    AAfterSummaryEvent = tvOrdersDataControllerSummaryAfterSummary;
  try{
    AView->BeginUpdate();
    AView->DataController->Summary->OnAfterSummary =
      AAfterSummaryEvent;
    AView->OptionsView->Footer = AEnable;
  }
  __finally{
    AView->EndUpdate();
  }
  if (AView->IsMaster)
     AView->DataController->ClearDetails();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::miCalculateSummariesClick(TObject *Sender)
{
  EnableSummaries(((TMenuItem*)Sender)->Checked, tvOrders);
  EnableSummaries(((TMenuItem*)Sender)->Checked, tvCars);
  if (((TMenuItem*)Sender)->Checked){
    tvCars->DataController->Summary->BeginUpdate();
    tvCars->DataController->Summary->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoMainForm::EnableSorting(TcxGridDBTableView *AView, bool AEnable)
{
  AView->DataController->ClearSorting(false);
  TNotifyEvent ASortingChangedEvent = NULL;
  if (AEnable)
    ASortingChangedEvent = tvDataControllerSortingChanged;
  AView->DataController->OnSortingChanged =
    ASortingChangedEvent;
  AView->OptionsCustomize->ColumnSorting = AEnable;
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::miEnableSortingClick(TObject *Sender)
{
  EnableSorting(tvOrders, ((TMenuItem*)Sender)->Checked);
  EnableSorting(tvCars, ((TMenuItem*)Sender)->Checked);
  tvCars->DataController->ClearDetails();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::miEnableFilteringClick(TObject *Sender)
{
  if (!tvOrders->DataController->Filter->IsEmpty())
    tvOrders->DataController->Filter->Clear();
  if (!tvCars->DataController->Filter->IsEmpty())
    tvCars->DataController->Filter->Clear();
  tvOrders->OptionsCustomize->ColumnFiltering = ((TMenuItem*)Sender)->Checked;
  tvCars->OptionsCustomize->ColumnFiltering = ((TMenuItem*)Sender)->Checked;
  tvCars->DataController->ClearDetails();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::tvDataControllerFilterGetValueList(
    TcxFilterCriteria *Sender, int AItemIndex,
    TcxDataFilterValueList *AValueList)
{
  TDataSet *ADataSet = ((TcxGridDBTableView*)Grid->FocusedView)->DataController->DataSet;
  TcxGridDBColumn *AColumn = ((TcxGridDBTableView*)Grid->FocusedView)->Columns[AItemIndex];
  bool AIsLookupColumn = (AColumn->PropertiesClass == __classid(TcxLookupComboBoxProperties));
  try{
    Screen->Cursor = crHourGlass;
    GridModeDemoDataDM->qryHelper->SQL->Clear();
    GridModeDemoDataDM->qryHelper->SQL->Add(
      "Select DISTINCT " + AColumn->DataBinding->FieldName + " From " +
      GridModeDemoDataDM->GetTableNameByDataSet(ADataSet));
    GridModeDemoDataDM->qryHelper->SQL->Add(
      GetSQLCondition((TcxGridDBTableView*)Grid->FocusedView, false));
    GridModeDemoDataDM->qryHelper->Open();
    GridModeDemoDataDM->qryHelper->First();
    while (!GridModeDemoDataDM->qryHelper->Eof){
      Variant ADisplayValue = GridModeDemoDataDM->qryHelper->Fields->Fields[0]->Value;
      if (AIsLookupColumn){
        TcxLookupComboBoxProperties *AProperties =
          (TcxLookupComboBoxProperties*)AColumn->GetProperties();
        ADataSet = AProperties->ListSource->DataSet;
        ADisplayValue = ADataSet->Lookup(
          AProperties->KeyFieldNames,ADisplayValue, AProperties->ListFieldNames);
      }
      if (VarIsNull(ADisplayValue)) return;
      AValueList->Add(fviValue, GridModeDemoDataDM->qryHelper->Fields->Fields[0]->Value,
        ADisplayValue, false);
      GridModeDemoDataDM->qryHelper->Next();
    }
    GridModeDemoDataDM->qryHelper->Close();
  }
  __finally{
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoMainForm::SortClone(TcxGridDBDataController *ADataController)
{
  TcxCustomGridTableView *AView = ADataController->GridView;
  if (!AView->PatternGridView->IsPattern) return;
  TcxGridDBTableView* APatternView = ((TcxGridDBTableView*)AView->PatternGridView);
  try{
    AView->BeginUpdate();
    APatternView->BeginUpdate();
    APatternView->DataController->ClearSorting(false);
    for (int I = 0; I < AView->SortedItemCount; I++){
      String AFieldName =
		((TcxGridDBColumn*)AView->SortedItems[I])->DataBinding->FieldName;
      TcxGridDBColumn *AColumn =
        APatternView->GetColumnByFieldName(AFieldName);
      if (AColumn->SortOrder != AView->SortedItems[I]->SortOrder)
       AColumn->SortOrder = AView->SortedItems[I]->SortOrder;
    }
  }
  __finally{
    APatternView->EndUpdate();
    AView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoMainForm::SortPattern(TcxGridDBDataController *ADataController)
{
    TcxGridDBTableView *AView = (TcxGridDBTableView*)ADataController->GridView;
    TStrings *ASortList = new TStringList();
    try{
      AView->BeginUpdate();
      String AOrder, AFieldName;
      int AIndex;
      for (int I = 0; I < AView->SortedItemCount; I++){
        AIndex = ADataController->GetSortingItemIndex(I);
        AFieldName = AView->Columns[AIndex]->DataBinding->FieldName;
        if (AView->Columns[AIndex]->SortOrder == soAscending)
          AOrder = " ASC, ";
        else
          AOrder = " DESC, ";
        ASortList->Add(AFieldName + AOrder);
      }
      GridModeDemoDataDM->
        ApplySortToQuery((TQuery*)ADataController->DataSet, ASortList);
    }
    __finally{
      delete ASortList;
      AView->EndUpdate();
    }
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::tvDataControllerSortingChanged(TObject *Sender)
{
  try{
    Screen->Cursor = crHourGlass;
	TcxGridDBDataController *ADataCOntroller = (TcxGridDBDataController*)Sender;
    TcxCustomGridTableView *AGridView = ADataCOntroller->GridView;
    if (AGridView->IsMaster || AGridView->IsPattern)
      SortPattern(ADataCOntroller);
    else
      SortClone(ADataCOntroller);
  }
  __finally{
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------

String TGridModeDemoMainForm::SummaryKindToStr(TcxSummaryKind AKind)
{
  switch (AKind){
    case skSum:
      return "SUM";
    case skMin:
      return "MIN";
    case skMax:
      return "MAX";
    case skCount:
      return "Count";
    case skAverage:
      return "AVG";
  }
  return "";
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::tvOrdersDataControllerSummaryAfterSummary(
    TcxDataSummary *ASender)
{
  TcxGridDBDataController *ADataController = (TcxGridDBDataController*)ASender->DataController;
  TcxGridDBTableView *AView = (TcxGridDBTableView*)ADataController->GridView;
  try{
    Screen->Cursor = crHourGlass;
    if (!AView->IsPattern && ADataController->DataSet->Active){
      String AFieldName, AStr = "Select ";
      for (int I = 0; I < ASender->FooterSummaryItems->Count; I++){
        AFieldName =
		  ((TcxGridDBTableSummaryItem*)ASender->FooterSummaryItems->Items[I])->FieldName;
        AStr = AStr + SummaryKindToStr(ASender->FooterSummaryItems->Items[I]->Kind)+
          "(" + AFieldName + "), ";
      }
      AStr.Delete(AStr.Length()-1, 2);
      if (ADataController->DataSet == NULL) return;
      if ((ADataController->DataSet->State == dsBrowse) &&
       ((TQuery*)ADataController->DataSet)->UpdatesPending)
        ((TQuery*)ADataController->DataSet)->ApplyUpdates();
      AStr = AStr + " From " +
        GridModeDemoDataDM->GetTableNameByDataSet(ADataController->DataSet);
      GridModeDemoDataDM->qryHelper->SQL->Clear();
      GridModeDemoDataDM->qryHelper->SQL->Add(AStr);
      GridModeDemoDataDM->qryHelper->SQL->Add(GetSQLCondition(AView, true));
      GridModeDemoDataDM->qryHelper->Open();
      GridModeDemoDataDM->qryHelper->First();
      for (int I = 0; I < ASender->FooterSummaryItems->Count; I++)
        ASender->FooterSummaryValues[I] =
          GridModeDemoDataDM->qryHelper->Fields->Fields[I]->Value;
      GridModeDemoDataDM->qryHelper->Close();
    }
  }
  __finally{
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::DoStepItforProgressBar(TObject *Sender)
{
	ProgressBar->Position = ProgressBar->Position + 1;
	Application->ProcessMessages();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::btnPopulateClick(TObject *Sender)
{
  if (MessageDlg("This operation will take some time, Continue ?",
    mtWarning, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes){
    pnlProgress->Visible = true;
    GridModeDemoDataDM->OnPopulateProgress = DoStepItforProgressBar;
    GridModeDemoDataDM->qryOrdersPopulate();
    pnlProgress->Visible = false;
    tvCars->DataController->ClearDetails();
    btnPopulate->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  if (GridModeDemoDataDM->IsOrdersPopulated()){
    if (!(MessageDlg("Do you want to delete previously inserted records ?",
      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes)) return;
    GridModeDemoDataDM->ClearOrders();
  }
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::miRecreateDBClick(TObject *Sender)
{
  GridModeDemoDataDM->CopyDBToLocalPlace(true);
  InitPopulateButton();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete GridModeDemoTerminateForm;
  GridModeDemoTerminateForm = NULL;
}
//---------------------------------------------------------------------------

void TGridModeDemoMainForm::InitPopulateButton()
{
  btnPopulate->Enabled = !GridModeDemoDataDM->IsOrdersPopulated();
}
//---------------------------------------------------------------------------

String TGridModeDemoMainForm::GetSQLCondition(TcxGridDBTableView *AView, bool AAddFilter)
{
  String Result = "",
    AStr = "where ";
  TcxGridDBDataController *ADataController = AView->DataController;
  if (AView->IsDetail){
    AStr = " and ";
    String AFieldName = ADataController->DetailKeyFieldNames;
    String AMasterKeyFieldName = ADataController->MasterKeyFieldNames;
	TcxGridDBTableView *AMasterGridView =
      (TcxGridDBTableView*)AView->MasterGridView;
    TDataSet *AMasterDataSet = AMasterGridView->DataController->DataSet;
    Variant AMasterKeyValue = AMasterDataSet->FindField(AMasterKeyFieldName)->Value;
    Result = "where " + AFieldName + " = " + VarToStr(AMasterKeyValue);
  }
  if (AAddFilter && !ADataController->Filter->IsEmpty() && ADataController->Filter->Active)
    Result = Result + AStr + ADataController->Filter->FilterText;
  return Result;
}
//---------------------------------------------------------------------------



