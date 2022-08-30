//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CardLayoutDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCardView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBCardView"
#pragma link "cxGridLevel"
#pragma link "cxStyles"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxGridTableView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TfmBaseForm(Owner)
{
	UpdateMenuValues();
}
//---------------------------------------------------------------------------
void TfrmMain::UpdateMenuValues()
{
  TcxGridCardView* AView = (TcxGridCardView*)Grid->ActiveView;
  miCardAutoWidth->Checked = AView->OptionsView->CardAutoWidth;
  miCellSelection->Checked = AView->OptionsSelection->CellSelect;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCardAutoWidthClick(TObject *Sender)
{
  TcxGridCardView* AView = (TcxGridCardView*)Grid->ActiveView;
  AView->OptionsView->CardAutoWidth = !AView->OptionsView->CardAutoWidth;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCellSelectionClick(TObject *Sender)
{
  TcxGridCardView* AView = (TcxGridCardView*)Grid->ActiveView;
  AView->OptionsSelection->CellSelect = !AView->OptionsSelection->CellSelect;
  UpdateMenuValues();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::miCustomizeClick(TObject *Sender)
{
  TcxGridCardView* AView = (TcxGridCardView*)Grid->ActiveView;
  AView->Controller->Customization = true;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::GridActiveTabChanged(TcxCustomGrid *Sender,
      TcxGridLevel *ALevel)
{
  UpdateMenuValues();
}
//---------------------------------------------------------------------------
