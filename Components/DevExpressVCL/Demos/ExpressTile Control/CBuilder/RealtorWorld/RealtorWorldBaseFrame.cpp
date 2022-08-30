//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldBaseFrame.h"
#include "RealtorWorldAgents.h"
#include "RealtorWorldListing.h"
#include "RealtorWorldLoanCalculator.h"
#include "RealtorWorldMortgageRate.h"
#include "RealtorWorldResearch.h"
#include "RealtorWorldStatistic.h"
#include "RealtorWorldSystemInformation.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmBase *frmBase;

const int IDFirst = 0;
const int IDPhotos = IDFirst;
const int IDResearch = 1;
const int IDAgents = 2;
const int IDStatistic = 3;
const int IDMortgageRate = 4;
const int IDLoanCalculator = 5;
const int IDSystemInformation = 6;
const int IDLast = IDSystemInformation;
//---------------------------------------------------------------------------
__fastcall TfrmBase::TfrmBase(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmBase::SelectItem(int APhotoID, int AAgentID)
{
}
//---------------------------------------------------------------------------
TfrmBase* __fastcall CreateFrameByID(TComponent* AOwner, int AID)
{
  TfrmBase* AFrame;
  switch (AID)
  {
		case IDPhotos:
		{
			AFrame = new TfrmListing(AOwner);
			break;
		}
		case IDResearch:
		{
			AFrame = new TfrmResearch(AOwner);
			break;
		}
		case IDAgents:
		{
			AFrame = new TfrmAgents(AOwner);
			break;
		}
		case IDStatistic:
		{
			AFrame = new TfrmStatistic(AOwner);
			break;
		}
		case IDMortgageRate:
		{
			AFrame = new TfrmMortgageRate(AOwner);
			break;
		}
		case IDLoanCalculator:
		{
			AFrame = new TfrmLoanCalculator(AOwner);
			break;
		}
		case IDSystemInformation:
		{
			AFrame = new TfrmSystemInformation(AOwner);
			break;
		}
  }
  return AFrame;
}
