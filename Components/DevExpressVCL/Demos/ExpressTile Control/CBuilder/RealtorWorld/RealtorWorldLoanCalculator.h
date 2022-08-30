//---------------------------------------------------------------------------

#ifndef RealtorWorldLoanCalculatorH
#define RealtorWorldLoanCalculatorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBChartView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxSplitter.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "dxmdaset.hpp"
#include <DB.hpp>
#include <Menus.hpp>
#include "RealtorWorldBaseFrame.h"
//---------------------------------------------------------------------------
class TfrmLoanCalculator : public TfrmBase
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox1;
	TcxGrid *cxGrid2;
	TcxGridDBChartView *cxGrid2DBChartView1;
	TcxGridDBChartSeries *cxGrid2DBChartView1Series2;
	TcxGridDBChartSeries *cxGrid2DBChartView1Series1;
	TcxGridLevel *cxGrid2Level1;
	TcxGroupBox *cxGroupBox3;
	TcxGroupBox *cxGroupBox5;
	TcxLabel *cxLabel5;
	TcxLabel *lblMontlyPayment;
	TcxGroupBox *cxGroupBox6;
	TcxGroupBox *cxGroupBox4;
	TcxLabel *cxLabel6;
	TcxSpinEdit *seLoan;
	TcxComboBox *cbInterests;
	TcxLabel *cxLabel2;
	TcxLabel *cxLabel3;
	TcxComboBox *cbTerms;
	TcxLabel *cxLabel4;
	TcxComboBox *cbStartMonths;
	TcxButton *btnCalculate;
	TcxGroupBox *cxGroupBox2;
	TcxGrid *cxGrid1;
	TcxGridDBTableView *cxGrid1DBTableView1;
	TcxGridDBColumn *cxGrid1DBTableView1RecId;
	TcxGridDBColumn *cxGrid1DBTableView1Month;
	TcxGridDBColumn *cxGrid1DBTableView1Balance;
	TcxGridDBColumn *cxGrid1DBTableView1Interest;
	TcxGridDBColumn *cxGrid1DBTableView1Principal;
	TcxGridLevel *cxGrid1Level1;
	TcxSplitter *cxSplitter1;
	TdxMemData *mdMonthlyPayments;
	TStringField *mdMonthlyPaymentsMonth;
	TFloatField *mdMonthlyPaymentsBalance;
	TFloatField *mdMonthlyPaymentsInterest;
	TFloatField *mdMonthlyPaymentsPrincipal;
	TDateField *mdMonthlyPaymentsDate;
	TdxMemData *mdYearlyPayments;
	TIntegerField *mdYearlyPaymentsYear;
	TFloatField *FloatField2;
	TFloatField *FloatField3;
	TDataSource *dtsMonthlyPayments;
	TDataSource *dtsYearlyPayments;
	void __fastcall cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose);
	void __fastcall btnCalculateClick(TObject *Sender);
private:	// User declarations
	float FMinInterest;
	float FStepInterest;
	DynamicArray<TDate> FStartMonts;

	void __fastcall CalculateYearlyPayments();
	void __fastcall CalculateMonthlyPayments(float ALoan, float AInterest, float AMontlyPayment, TDate AStartMonth, Word AMonthsCount);
	void __fastcall InitializeInterests();
	void __fastcall InitializeTerms();
	void __fastcall InitializeStartMonths();
	float __fastcall GetSelectedInterest();
	Word __fastcall GetSelectedTerm();
	TDate __fastcall GetSelectedStartMonth();
	float __fastcall GetAnuitentMontlyPayment(float ALoan, float AInterest, Word AMonthsCount);
	float __fastcall GetMontlyInterestPayment(float ALoanRest, float AInterest);
	String __fastcall GetMonthName(TDate ADate);
public:		// User declarations
	__fastcall TfrmLoanCalculator(TComponent* Owner);
	void __fastcall InitializeSourceData();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmLoanCalculator *frmLoanCalculator;
//---------------------------------------------------------------------------
#endif
