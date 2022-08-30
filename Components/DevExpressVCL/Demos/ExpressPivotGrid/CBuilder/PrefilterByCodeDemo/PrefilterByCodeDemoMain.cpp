//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "PrefilterByCodeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxClasses"
#pragma link "cxCustomData"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TfmPrefilterByCode *fmPrefilterByCode;

const int DoeEnterpriseID = 12;
const int QuantityMinID = 3;
const int HillCorporationID = 13;
const int DevelopmentHouseID = 22;

//---------------------------------------------------------------------------
__fastcall TfmPrefilterByCode::TfmPrefilterByCode(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}
TcxCustomPivotGrid* __fastcall TfmPrefilterByCode::PivotGrid()
{
  return DBPivotGrid;
}

//---------------------------------------------------------------------------
void __fastcall TfmPrefilterByCode::pgfPaymentTypeGetGroupImageIndex(
      TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
      int &AImageIndex, TAlignment &AImageAlignHorz,
      TcxAlignmentVert &AImageAlignVert)
{
  AnsiString Card = ((TcxPivotGridViewDataItem *)AItem)->Value;
  if (SameText(Card, "Cash")) AImageIndex = 0;
  else if (SameText(Card, "AmEx")) AImageIndex = 1;
       else if (SameText(Card, "Master")) AImageIndex = 2;
            else if (SameText(Card, "Visa")) AImageIndex = 3;
}
//---------------------------------------------------------------------------

void __fastcall TfmPrefilterByCode::Bottom1Click(TObject *Sender)
{
  DBPivotGrid->OptionsPrefilter->Position =
	(TcxPivotGridPrefilterPosition)((TMenuItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TfmPrefilterByCode::cbFiltersChange(TObject *Sender)
{
  SetFilter((TUserFiltering)(int)((TComboBox*)Sender)->Items->Objects[((TComboBox*)Sender)->ItemIndex]);
}
//---------------------------------------------------------------------------

void __fastcall TfmPrefilterByCode::DBPivotGridFilterChanged(
      TObject *Sender)
{
  if (ComponentState.Contains(csDestroying)) return;
  if (!FLock)
	if (Prefilter->IsEmpty() || !Prefilter->Active)
	  cbFilters->ItemIndex = GetFilterIndex(ufNone);
	else
	  cbFilters->ItemIndex = GetFilterIndex(ufCustom);
}
//---------------------------------------------------------------------------

TDate TfmPrefilterByCode::GetDate(TDateType ADateType)
{
  TDate ADate = Date();
  switch (ADateType){
	case dtFirst:
	  ADate =  EncodeDate(2002, 1, 1);
      break;
	case dtLast:
	  ADate =  EncodeDate(2002, 1, 31);
  }
  return ADate;	
}
//---------------------------------------------------------------------------

int TfmPrefilterByCode::GetFilterIndex(const TUserFiltering AFiltering)
{
  for (int I = 0; I < cbFilters->Items->Count; I++)
    if ((TUserFiltering)(int)(cbFilters->Items->Objects[I]) == AFiltering)
      return I;
  return -1;	
}
//---------------------------------------------------------------------------

TcxDBDataFilterCriteria* __fastcall TfmPrefilterByCode::GetPrefilter()
{
  return DBPivotGrid->DataController->Filter;	
}
//---------------------------------------------------------------------------

void TfmPrefilterByCode::PopulateFilterList()
{
  String AFilterDesc[] = {
        "No prefilter",
        "Custom prefilter",
	"QUANTITY > 3",
	"PAYMENTTYPE is Visa or AmericanExpress",
	"All cars purchased by Doe Enterprises using Visa",
	"All purchases in December 2002",
	"Only Mercedes",
	"Only Mercedes purchased by Hill Corporation or Development House"
  };

  cbFilters->Clear();
  for (int I = 0; I < sizeof(AFilterDesc)/sizeof (AFilterDesc[0]); I++)
    cbFilters->Items->AddObject(AFilterDesc[I], (TObject*)I);
  cbFilters->ItemIndex = GetFilterIndex(ufSimple);
  SetFilter(ufSimple);	
}
//---------------------------------------------------------------------------

void TfmPrefilterByCode::SetFilter(const TUserFiltering AFiltering)
{
  if (AFiltering == ufCustom)
	{ DBPivotGrid->ShowPrefilterDialog(); }
  else
	{ Prefilter->BeginUpdate();
	  FLock = True;
	  try {
		TDate ADate;
		TcxFilterCriteriaItemList *AList = Prefilter->Root;
		AList->Clear();
		int Bounds[2] = {0, 1};
		Variant A;
		switch (AFiltering){
		  case ufSimple:
			AList->AddItem(pgfQuantity, foGreater, QuantityMinID, "3");
			break;
		  case ufList:
			AList->BoolOperatorKind = fboOr;
			AList->AddItem(pgfPaymentType, foEqual, "Visa", "Visa");
			AList->AddItem( pgfPaymentType, foEqual, "AmEx", "American Express");
			break;
		  case ufTwoField:
			AList->BoolOperatorKind = fboAnd;
			AList->AddItem(pgfCompanyName, foEqual, DoeEnterpriseID, "Doe Enterprises");
			AList->AddItem(pgfPaymentType, foEqual, "Visa",
			  "Visa");
			break;
		  case ufBetween:
			AList->BoolOperatorKind = fboAnd;
			ADate = GetDate(dtFirst);
			AList->AddItem(pgfPurchaseDate, foGreaterEqual, ADate, DateToStr(ADate));
			ADate = GetDate(dtLast);
			AList->AddItem( pgfPurchaseDate, foLessEqual, ADate, DateToStr(ADate));
			break;
		  case ufUserFilter:
			SetOnlyMercedesFilter(Prefilter->Root);
			break;
		  case ufGroup:
			AList->BoolOperatorKind = fboAnd;
			SetOnlyMercedesFilter(AList->AddItemList(fboOr));
			A = VarArrayCreate(Bounds, 1, varVariant);
			A.PutElement(HillCorporationID, 0);
			A.PutElement(DevelopmentHouseID, 1);
			AList->AddItem( pgfCompanyName, foInList, A, "Hill Corporation, Development House");
			break;
		}
		  Prefilter->Active = True;
	  }
	  __finally {
        Prefilter->EndUpdate();
		FLock = false;
	  }
	}
}
//---------------------------------------------------------------------------

void TfmPrefilterByCode::SetOnlyMercedesFilter(TcxFilterCriteriaItemList *AFilterCriteriaList)
{
  AFilterCriteriaList->Clear();
  AFilterCriteriaList->BoolOperatorKind = fboOr;
  AFilterCriteriaList->
	AddItem(pgfCarName, foEqual, 1, "SL500");
  AFilterCriteriaList->
	AddItem(pgfCarName, foEqual, 2,  "CLK55");
  AFilterCriteriaList->
	AddItem(pgfCarName, foEqual, 3, "C230");
}
//---------------------------------------------------------------------------


void __fastcall TfmPrefilterByCode::FormCreate(TObject *Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  PopulateFilterList();	
}
//---------------------------------------------------------------------------

