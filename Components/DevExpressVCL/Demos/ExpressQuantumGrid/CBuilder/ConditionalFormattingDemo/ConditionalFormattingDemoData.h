//---------------------------------------------------------------------------

#ifndef ConditionalFormattingDemoDataH
#define ConditionalFormattingDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <Db.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TConditionalFormattingDemoMainDM : public TDataModule
{
__published:	// IDE-managed Components
	TClientDataSet *cdsConditionalFormatting;
	TStringField *cdsConditionalFormattingState;
	TFloatField *cdsConditionalFormattingSales;
	TFloatField *cdsConditionalFormattingProfit;
	TFloatField *cdsConditionalFormattingSalesVsTarget;
	TFloatField *cdsConditionalFormattingMarketShare;
	TFloatField *cdsConditionalFormattingCustomersSatisfaction;
	TDataSource *dsConditionalFormatting;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TConditionalFormattingDemoMainDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TConditionalFormattingDemoMainDM *ConditionalFormattingDemoMainDM;
//---------------------------------------------------------------------------
#endif
 