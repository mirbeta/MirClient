//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "InplaceEditFormMain.h"
#include "AboutDemoForm.h"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::actCustomizeEditFormExecute(TObject *Sender)
{
  TableView->Controller->ShowEditFormCustomizationDialog();
}

void __fastcall TfrmMain::actEditModeChange(TObject *Sender)
{
  if (Sender == actInplace)
  {
	TableView->OptionsBehavior->EditMode = emInplace;
	actCustomizeEditForm->Enabled = False;
  }
  else
  {
	if (Sender == actInplaceEditForm)
	  TableView->OptionsBehavior->EditMode = emInplaceEditForm;
	else
	  TableView->OptionsBehavior->EditMode = emInplaceEditFormHideCurrentRow;
	actCustomizeEditForm->Enabled = True;
  }
}

void __fastcall TfrmMain::actHotTrackExecute(TObject *Sender)
{
  TableView->EditForm->ItemHotTrack = actHotTrack->Checked;
}

void __fastcall TfrmMain::FormShow(TObject *Sender)
{
  TableView->DataController->GotoFirst();
  TableView->ViewData->Rows[0]->Expand(False);
  TableView->ViewData->Rows[1]->Focused = True;
  TcxCustomGridRow* ARow = TableView->ViewData->Rows[1];
  TcxGridDataRow* ADataRow = (TcxGridDataRow*)(ARow);
  ADataRow->EditFormVisible = True;
}
