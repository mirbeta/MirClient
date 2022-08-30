//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ViewNestedBandsDemoMain.h"
#include "ViewNestedBandsDemoData.h"
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
#pragma link "cxGridBandedTableView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxCalendar"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxImageComboBox"
#pragma resource "*.dfm"


/* TcxNestedBandInfo */
//---------------------------------------------------------------------------
__fastcall TcxNestedBandInfo::TcxNestedBandInfo(TcxGridBand *ABand)
{
  FBand = ABand;
  FParentBand = ABand->Bands->Items[ABand->Position->BandIndex];
  FCaption = ABand->Caption;
  FColumnIndex = ABand->Position->ColIndex;
}

//---------------------------------------------------------------------------
void  __fastcall TcxNestedBandInfo::RestoreBand()
{
  FBand->Caption = FCaption;
  FBand->Position->BandIndex = FParentBand->Index;
  FBand->Position->ColIndex = FColumnIndex;
}

TViewNestedBandsDemoMainForm *ViewNestedBandsDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TViewNestedBandsDemoMainForm::TViewNestedBandsDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
  FNestedBands = new TList();
}
//---------------------------------------------------------------------------

__fastcall TViewNestedBandsDemoMainForm::~TViewNestedBandsDemoMainForm(void)
{
  ReleaseNestedBandInfos();
  delete FNestedBands;
}
//---------------------------------------------------------------------------

void __fastcall TViewNestedBandsDemoMainForm:: miNestedBandsClick(TObject *Sender)
{
  AdjustNestedBands(((TMenuItem*)Sender)->Checked);
  bvOrders->OptionsCustomize->NestedBands =((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewNestedBandsDemoMainForm:: miBandsQuickCustomizationClick(TObject *Sender)
{
  bvOrders->OptionsCustomize->BandsQuickCustomization =((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewNestedBandsDemoMainForm:: miColumnsQuickCustomizationClick(TObject *Sender)
{
  bvOrders->OptionsCustomize->ColumnsQuickCustomization =((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewNestedBandsDemoMainForm:: miCellMergingClick(TObject *Sender)
{
  AdjustCellMerging(((TMenuItem*)Sender)->Checked);
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::AddNestedBandInfo(TcxGridBand *ABand)
{
  FNestedBands->Add(new TcxNestedBandInfo(ABand));
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::AdjustCellMerging(bool AUseCelMerging)
{
  bvOrders->BeginUpdate();
  try{
	clnCustomerID->Options->CellMerging = AUseCelMerging;
	clnOrdersProductID->Options->CellMerging = AUseCelMerging;
	clnPurchaseDate->Options->CellMerging = AUseCelMerging;
	clnPaymentType->Options->CellMerging = AUseCelMerging;
	clnCustomerCompany->Options->CellMerging = AUseCelMerging;
	clnCustomerAddres->Options->CellMerging = AUseCelMerging;
	clnCustomerFax->Options->CellMerging = AUseCelMerging;
	clnCustomerPhone->Options->CellMerging = AUseCelMerging;
	clnCustomerOccupation->Options->CellMerging = AUseCelMerging;
	clnCustomerZipCode->Options->CellMerging = AUseCelMerging;
	clnCarCyl->Options->CellMerging = AUseCelMerging;
	clnCarHP->Options->CellMerging = AUseCelMerging;
	clnCarTorque->Options->CellMerging = AUseCelMerging;
	clnCarMPG_City->Options->CellMerging = AUseCelMerging;
	clnCarMPG_Highway->Options->CellMerging = AUseCelMerging;
	clnCarTransMissAuto->Options->CellMerging = AUseCelMerging;
	clnCarTransmissSpeedCount->Options->CellMerging = AUseCelMerging;
  }
  __finally{
	bvOrders->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::AdjustNestedBands(bool AUseNestedBands)
{
  if (AUseNestedBands)
	ShowNestedBands();
  else
	HideNestedBands();
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::ChangeBandVisibility(int AIndex, bool AVisible)
{
  bvOrders->Bands->Items[AIndex]->Visible = AVisible;
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::HideNestedBands()
{
  ReleaseNestedBandInfos();
  bvOrders->BeginUpdate();
  try{
	for (int I = 0; I < bvOrders->Bands->Count; I++)
	  if (bvOrders->Bands->Items[I]->Position->BandIndex != -1){
		AddNestedBandInfo(bvOrders->Bands->Items[I]);
		ChangeBandVisibility(bvOrders->Bands->Items[I]->Position->BandIndex, false);
		bvOrders->Bands->Items[I]->Caption =
		  bvOrders->Bands->Items[bvOrders->Bands->Items[I]->Position->BandIndex]->Caption + "'s " +
			bvOrders->Bands->Items[I]->Caption;
		bvOrders->Bands->Items[I]->Position->BandIndex = -1;
	  }
   }
   __finally{
	 bvOrders->EndUpdate();
   }
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::ReleaseNestedBandInfos()
{
  for (int I = 0; I < FNestedBands->Count; I++)
	if (FNestedBands->Items[I] != NULL)
	  delete (TcxNestedBandInfo*)FNestedBands->Items[I];
  FNestedBands->Clear();
}
//---------------------------------------------------------------------------

void TViewNestedBandsDemoMainForm::ShowNestedBands()
{
  bvOrders->BeginUpdate();
  try{
	for (int I = 0; I < FNestedBands->Count; I++)
	  if (FNestedBands->Items[I] != NULL){
		((TcxNestedBandInfo*)FNestedBands->Items[I])->RestoreBand();
		ChangeBandVisibility(
		  ((TcxNestedBandInfo*)FNestedBands->Items[I])->ParentBand->Index, true);
	  }
	ReleaseNestedBandInfos();
  }
  __finally{
	bvOrders->EndUpdate();
  }
}
//---------------------------------------------------------------------------


