//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldLoanCalculator.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxButtons"
#pragma link "cxCurrencyEdit"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxSplitter"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "dxmdaset"
#pragma link "cxNavigator"
#pragma link "dxDateRanges"
#pragma resource "*.dfm"
TfrmLoanCalculator *frmLoanCalculator;

const
  Word dxTermsOfLoan[7] = {1, 5, 10, 15, 20, 25, 30};

//---------------------------------------------------------------------------
__fastcall TfrmLoanCalculator::TfrmLoanCalculator(TComponent* Owner)
	: TfrmBase(Owner)
{
  InitializeSourceData();
  btnCalculateClick(NULL);
  cxGrid1DBTableView1->ApplyBestFit(cxGrid1DBTableView1Month);
}
//---------------------------------------------------------------------------
float __fastcall TfrmLoanCalculator::GetAnuitentMontlyPayment(float ALoan, float AInterest, Word AMonthsCount)
{
  float ASpecifiedInterest;
  ASpecifiedInterest = AInterest / (100 * 12);
  return(ALoan * ASpecifiedInterest / (1 - Power(1 + ASpecifiedInterest, -AMonthsCount)));
}
//---------------------------------------------------------------------------
float __fastcall TfrmLoanCalculator::GetMontlyInterestPayment(float ALoanRest, float AInterest)
{
  return (ALoanRest * AInterest/100 * 1/12);
}
//---------------------------------------------------------------------------
String __fastcall TfrmLoanCalculator::GetMonthName(TDate ADate)
{
  return FormatDateTime("mmmm', 'yyyy", ADate);
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose)

{
  AllowClose = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::btnCalculateClick(TObject *Sender)
{
  float ALoan, AInterest, AMontlyPayment;
  Word AMonthsCount;
  TFormatSettings AFmtSettings;
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, AFmtSettings);
  ALoan = seLoan->Value;
  AInterest = GetSelectedInterest();
  AMonthsCount = GetSelectedTerm() * 12;
  AMontlyPayment = GetAnuitentMontlyPayment(ALoan, AInterest, AMonthsCount);
  lblMontlyPayment->Caption = FloatToStrF(AMontlyPayment, ffCurrency, 10, 2, AFmtSettings);
  CalculateMonthlyPayments(ALoan, AInterest, AMontlyPayment, GetSelectedStartMonth(), AMonthsCount);
  CalculateYearlyPayments();
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::CalculateYearlyPayments()
{
  TBookmark ABookmark;
  Word AYearPrev, AYear;
  Screen->Cursor = crHourGlass;
  mdMonthlyPayments->DisableControls();
  ABookmark = mdMonthlyPayments->GetBookmark();
  mdYearlyPayments->Close();
  mdYearlyPayments->Open();
  AYearPrev = 0;
  __try
  {
	mdMonthlyPayments->First();
	while (!mdMonthlyPayments->Eof)
    {
      AYear = YearOf(mdMonthlyPayments->FieldByName("Date")->AsDateTime);
	  if (AYearPrev != AYear)
	  {
		if (dsInsert == mdYearlyPayments->State)
		{
		  mdYearlyPayments->Post();
		};
		mdYearlyPayments->Append();
	  };
	  mdYearlyPayments->FieldByName("Year")->AsInteger = AYear;
      mdYearlyPayments->FieldByName("Interest")->AsFloat = mdYearlyPayments->FieldByName("Interest")->AsFloat + mdMonthlyPayments->FieldByName("Interest")->AsFloat;
      mdYearlyPayments->FieldByName("Principal")->AsFloat = mdYearlyPayments->FieldByName("Principal")->AsFloat + mdMonthlyPayments->FieldByName("Principal")->AsFloat;
      AYearPrev = AYear;
      mdMonthlyPayments->Next();
	};
	if (dsInsert == mdYearlyPayments->State)
	{
	  mdYearlyPayments->Post();
	};
	mdMonthlyPayments->GotoBookmark(ABookmark);
  }
  __finally
  {
	mdMonthlyPayments->FreeBookmark(ABookmark);
  };
  mdMonthlyPayments->EnableControls();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::CalculateMonthlyPayments(float ALoan, float AInterest, float AMontlyPayment, TDate AStartMonth, Word AMonthsCount)
{
  Word ANum;
  float AInterestPayment;
  Screen->Cursor = crHourGlass;
  mdMonthlyPayments->DisableControls();
  mdMonthlyPayments->Close();
  mdMonthlyPayments->Open();
  for (int ANum = 1; ANum <= AMonthsCount; ANum++)
  {
    AInterestPayment = GetMontlyInterestPayment(ALoan, AInterest);
	if (ANum == AMonthsCount)
	{
	  AMontlyPayment = ALoan + AInterestPayment;
	};
    ALoan = ALoan + AInterestPayment - AMontlyPayment;
    mdMonthlyPayments->Append();
    mdMonthlyPayments->FieldByName("Date")->AsDateTime = AStartMonth;
    mdMonthlyPayments->FieldByName("Month")->AsString = IntToStr(ANum) + " (" + GetMonthName(AStartMonth) + ")";
    mdMonthlyPayments->FieldByName("Balance")->AsFloat = ALoan;
    mdMonthlyPayments->FieldByName("Interest")->AsFloat = AInterestPayment;
    mdMonthlyPayments->FieldByName("Principal")->AsFloat = AMontlyPayment - AInterestPayment;
	mdMonthlyPayments->Post();
	AStartMonth = IncMonth(AStartMonth);
  };
  mdMonthlyPayments->First();
  mdMonthlyPayments->EnableControls();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::InitializeInterests()
{
  int ACount = 100;
  float AValue;
  FMinInterest = 2.5;
  FStepInterest = 0.125;
  cbInterests->Properties->Items->Clear();
  cbInterests->ItemIndex = -1;
  for (int I = 0; I < ACount; I++)
  {
	AValue = FMinInterest + I * FStepInterest;
	AnsiString A = "%.3f";
	cbInterests->Properties->Items->Add(Format(A, ARRAYOFCONST((AValue)))+"%");
	if (abs(AValue - 5.625) < 0.000001)
	{
	  cbInterests->ItemIndex = cbInterests->Properties->Items->Count - 1;
	};
  }
  if (cbInterests->ItemIndex == -1) cbInterests->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::InitializeTerms()
{
  const String AEnding [2] = {"", "s"};
  Word AValue;
  cbTerms->Properties->Items->Clear();
  cbTerms->ItemIndex = -1;
  for (int I = 0; I < 7; I++)
  {
    AValue = dxTermsOfLoan[I];
	cbTerms->Properties->Items->Add(IntToStr(AValue) + " year" + AEnding[dxTermsOfLoan[I] > 1]);
    if (AValue == 15) cbTerms->ItemIndex = cbTerms->Properties->Items->Count - 1;
  };
  if (cbTerms->ItemIndex == -1) cbTerms->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::InitializeStartMonths()
{
  TDate AStartMonth;  // !
  Word AYear, AMonth, ADay;
  int ACountMonts;
  DecodeDate(Date(), AYear, AMonth, ADay);
  AStartMonth = IncMonth(RecodeDate(Date(), AYear, AMonth, 1));
  AMonth = MonthOf(AStartMonth);
  ACountMonts = 13 - AMonth;
  FStartMonts.Length = ACountMonts;
  cbStartMonths->Properties->Items->Clear();
  cbStartMonths->ItemIndex = -1;
  for (int I = AMonth; I <= 12; I++)
  {
    FStartMonts[I - AMonth] = AStartMonth;
    cbStartMonths->Properties->Items->Add(GetMonthName(FStartMonts[I - AMonth]));
    AStartMonth = IncMonth(AStartMonth);
  }
  cbStartMonths->ItemIndex = 0;
}
//---------------------------------------------------------------------------
float __fastcall TfrmLoanCalculator::GetSelectedInterest()
{
  return (FMinInterest + cbInterests->ItemIndex * FStepInterest);
}
//---------------------------------------------------------------------------
Word __fastcall TfrmLoanCalculator::GetSelectedTerm()
{
  return dxTermsOfLoan[cbTerms->ItemIndex];
}
//---------------------------------------------------------------------------
TDate __fastcall TfrmLoanCalculator::GetSelectedStartMonth()
{
  return FStartMonts[cbStartMonths->ItemIndex];
}
//---------------------------------------------------------------------------
void __fastcall TfrmLoanCalculator::InitializeSourceData()
{
  seLoan->Value = 250000;
  InitializeInterests();
  InitializeTerms();
  InitializeStartMonths();
}
//---------------------------------------------------------------------------
