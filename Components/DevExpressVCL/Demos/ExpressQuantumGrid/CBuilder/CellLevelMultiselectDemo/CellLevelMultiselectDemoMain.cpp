//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "CellLevelMultiselectDemoMain.h"
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
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma resource "*.dfm"
TCellLevelMultiselectDemoMainForm *CellLevelMultiselectDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TCellLevelMultiselectDemoMainForm::TCellLevelMultiselectDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
  CreateSpreadSheet();
  TableView->Controller->SelectCells(
    TableView->Columns[1], TableView->Columns[5], 2, 12);
}
//---------------------------------------------------------------------------

int TCellLevelMultiselectDemoMainForm::SelectedRowCount()
{
  return TableView->Controller->SelectedRowCount;
}

int TCellLevelMultiselectDemoMainForm::SelectedColumnCount()
{
  return TableView->Controller->SelectedColumnCount;
}

int TCellLevelMultiselectDemoMainForm::GetSummOfSelection()
{
  Variant val;
  int Result = 0;
  for (int I = 0; I < SelectedRowCount(); I++)
    for (int J = 0; J < SelectedColumnCount(); J++){
      val = TableView->DataController->GetValue(
        TableView->Controller->SelectedRows[I]->RecordIndex,
        TableView->Controller->SelectedColumns[J]->Index);
      if (!VarIsNull(val))
        Result += (int)val;
    }
  return Result;
}

void __fastcall TCellLevelMultiselectDemoMainForm::TableViewSelectionChanged(
      TcxCustomGridTableView *Sender)
{
  TableView->ViewInfo->HeaderViewInfo->Update();
  lblSelectedRows->Caption =
    FloatToStrF(SelectedRowCount(), ffNumber, 15, 0);
  lblSelectedColumns->Caption  =
    FloatToStrF(SelectedColumnCount(), ffNumber, 15, 0);
  lblSelectedCells->Caption  =
    FloatToStrF(SelectedRowCount() * SelectedColumnCount(), ffNumber, 15, 0);
  lblSelectedSummary->Caption = FloatToStrF(GetSummOfSelection(), ffNumber, 15, 0);
}
//---------------------------------------------------------------------------

void __fastcall TCellLevelMultiselectDemoMainForm::TableViewCustomDrawColumnHeader(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridColumnHeaderViewInfo *AViewInfo, bool &ADone)
{
  InternalDrawItem(ACanvas, AViewInfo->Bounds,
    TcxBorders()<<bLeft<<bRight<<bTop<<bBottom, AViewInfo->Text,
      AViewInfo->Column->Selected);
  ADone = true;
}
//---------------------------------------------------------------------------

void __fastcall TCellLevelMultiselectDemoMainForm::TableViewCustomDrawIndicatorCell(
      TcxGridTableView *Sender, TcxCanvas *ACanvas,
      TcxCustomGridIndicatorItemViewInfo *AViewInfo, bool &ADone)
{
  if (dynamic_cast<TcxGridIndicatorRowItemViewInfo*>(AViewInfo) == 0)
    return;
  TcxCustomGridRow *ARecord =
    ((TcxGridIndicatorRowItemViewInfo*)AViewInfo)->GridRecord;
  InternalDrawItem(ACanvas, AViewInfo->ContentBounds,
    TcxBorders()<<bLeft<<bRight<<bBottom, IntToStr(ARecord->Index + 1),
      ARecord->Selected);
  ADone = true;
}
//---------------------------------------------------------------------------

void __fastcall TCellLevelMultiselectDemoMainForm::TableViewMouseDown(
      TObject *Sender, TMouseButton Button, TShiftState Shift, int X,
      int Y)
{
  TcxCustomGridHitTest *AHitTest = TableView->GetHitTest(X, Y);
  if (dynamic_cast<TcxGridColumnHeaderHitTest*>(AHitTest) == 0)
    return;
  TcxGridColumn *AColumn = ((TcxGridColumnHeaderHitTest*)AHitTest)->Column;
  TableView->BeginUpdate();
  try{
    if (Shift.Contains(ssShift)){
      if (FAnchorLinkedAreaLastColumn == NULL)
        TableView->Controller->ClearSelection();
      else
        SetColumnsSelected(TableView->Controller->CellSelectionAnchor,
          FAnchorLinkedAreaLastColumn, false);
      SetColumnsSelected(TableView->Controller->CellSelectionAnchor, AColumn, true);
    }
    else{
      if (Shift.Contains(ssCtrl))
        SetColumnsSelected(AColumn, AColumn, true);
      else
        TableView->Controller->SelectColumns(AColumn, AColumn);
      AColumn->Focused = true;
      TableView->Controller->CellSelectionAnchor = AColumn;
    }
    TableView->DataController->SelectAll();
    FAnchorLinkedAreaLastColumn = AColumn;
  }
  __finally{
    TableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TCellLevelMultiselectDemoMainForm::CreateSpreadSheet()
{
  TableView->BeginUpdate();
  try{
    CreateColumns();
    CreateRows();
  }
  __finally{
    TableView->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TCellLevelMultiselectDemoMainForm::CreateColumns()
{
  TcxGridColumn *AColumn;
  for (int I = 0; I < ColumnCount; I++){
    AColumn = TableView->CreateColumn();
    AColumn->Caption = GetColumnCaption(I);
    AColumn->HeaderAlignmentHorz = taCenter;
    AColumn->DataBinding->ValueType = "Integer";
  }
}
//---------------------------------------------------------------------------

void TCellLevelMultiselectDemoMainForm::CreateRows()
{
  TableView->DataController->RecordCount = RecordCount;
  randomize();
  for (int I = 0; I < RecordCount; I++)
    for (int J = 0; J < ColumnCount; J++)
      TableView->DataController->SetValue(I, J, random(100));
}
//---------------------------------------------------------------------------

String TCellLevelMultiselectDemoMainForm::GetColumnCaption(int Index)
{
  const int AlphabetSymbolCount = (int)'Z' - (int)'A' + 1;
  String Result;
  if (div(Index, AlphabetSymbolCount).quot > 0)
     Result = GetColumnCaption(div(Index, AlphabetSymbolCount).quot - 1);
  else
    Result = "";
  Result = Result + char((int)'A' + div(Index, AlphabetSymbolCount).rem);
  return Result;
}
//---------------------------------------------------------------------------

void TCellLevelMultiselectDemoMainForm::InternalDrawItem(TcxCanvas *ACanvas,
  TRect ABounds, TcxBorders ABorders, String AText, bool AIsSelected)
{
  const TColor SelectedItemBorderColor = TColor(0xC56A31);
  TRect ATextRect = ABounds;
  InflateRect(&ATextRect, -2, -1);
  TcxStyle *AStyle;
  TColor ABorderColor;
  if (AIsSelected){
	AStyle = styleSelected;
  }
  else{
	AStyle = styleNormal;
  }
  TcxViewParams AParams;
  AParams.Bitmap = NULL;
  AParams.Color = AStyle->Color;
  ACanvas->FillRect(ABounds, AParams, False);
  ACanvas->Font = AStyle->Font;
  ACanvas->Font->Color = AStyle->TextColor;
  ACanvas->DrawTexT(AText, ATextRect,
	cxAlignHCenter | cxAlignVCenter, True);
}
//---------------------------------------------------------------------------

void TCellLevelMultiselectDemoMainForm::SetColumnsSelected(
  TcxGridColumn *AFromColumn, TcxGridColumn *AToColumn, bool ASelected)
{
  int AFromColIndex = AFromColumn->VisibleIndex;
  int AToColIndex = AToColumn->VisibleIndex;
  int I;
  if (AFromColIndex > AToColIndex){
    I = AToColIndex;
    AToColIndex = AFromColIndex;
    AFromColIndex = I;
  }
  TableView->BeginUpdate();
  try{
    for (int I = AFromColIndex; I <= AToColIndex; I++)
      TableView->VisibleColumns[I]->Selected = ASelected;
  }
  __finally{
    TableView->EndUpdate();
  }
  TableViewSelectionChanged(NULL);
}
//---------------------------------------------------------------------------


void __fastcall TCellLevelMultiselectDemoMainForm::TableViewStylesGetHeaderStyle(
	TcxGridTableView *Sender, TcxGridColumn *AColumn, TcxStyle *&AStyle)
{
  if (AColumn)
  {
	  if (AColumn->Selected)
		AStyle = styleSelected;
	  else
		AStyle = styleNormal;
  }
}
//---------------------------------------------------------------------------

