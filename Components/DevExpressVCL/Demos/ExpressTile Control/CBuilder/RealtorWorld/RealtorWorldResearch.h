//---------------------------------------------------------------------------

#ifndef RealtorWorldResearchH
#define RealtorWorldResearchH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSplitter.hpp"
#include "cxStyles.hpp"
#include "dxGDIPlusClasses.hpp"
#include "RealtorWorldBaseFrame.h"
#include "cxDBData.hpp"
#include "cxGrid.hpp"
#include "cxGridChartView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBChartView.hpp"
#include "cxGridLevel.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TfrmResearch : public TfrmBase
{
__published:	// IDE-managed Components
	TcxSplitter *cxSplitter1;
	TcxDBPivotGrid *pgResearch;
	TcxDBPivotGridField *pgfYear;
	TcxDBPivotGridField *pgfCount;
	TcxDBPivotGridField *pgfRegion;
	TcxDBPivotGridField *pgfSeasonallyAdjusted;
	TcxDBPivotGridField *pgfStatus;
	TcxDBPivotGridField *pgfMonth;
	TcxGroupBox *cxGroupBox1;
	TcxGrid *cxgChart;
	TcxGridDBChartView *cvChart;
	TcxGridDBChartDataGroup *cxGridDBChartDataGroup6;
	TcxGridDBChartDataGroup *cxGridDBChartDataGroup5;
	TcxGridDBChartSeries *cxGridDBChartSeries11;
	TcxGridDBChartSeries *cxGridDBChartSeries12;
	TcxGridDBChartSeries *cxGridDBChartSeries13;
	TcxGridDBChartSeries *cxGridDBChartSeries14;
	TcxGridLevel *cxGridLevel3;
	void __fastcall cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose);
private:	// User declarations
	void __fastcall InitializeFrame();
public:		// User declarations
	__fastcall TfrmResearch(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmResearch *frmResearch;
//---------------------------------------------------------------------------
#endif
