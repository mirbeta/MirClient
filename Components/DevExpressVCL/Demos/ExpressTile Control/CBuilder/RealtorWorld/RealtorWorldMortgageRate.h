//---------------------------------------------------------------------------

#ifndef RealtorWorldMortgageRateH
#define RealtorWorldMortgageRateH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxCalendar.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
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
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSplitter.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include "RealtorWorldBaseFrame.h"
//---------------------------------------------------------------------------
class TfrmMortgageRate : public TfrmBase
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox1;
	TcxGrid *cxGrid2;
	TcxGridDBTableView *cxGrid2DBTableView1;
	TcxGridDBChartView *cxGrid2DBChartView1;
	TcxGridDBChartSeries *cxGrid2DBChartView1Series1;
	TcxGridDBChartSeries *cxGrid2DBChartView1Series2;
	TcxGridDBChartSeries *cxGrid2DBChartView1Series3;
	TcxGridLevel *cxGrid2Level1;
	TcxGroupBox *cxGroupBox2;
	TcxGrid *cxGrid1;
	TcxGridDBTableView *cxGrid1DBTableView1;
	TcxGridDBColumn *cxGrid1DBTableView1Date1;
	TcxGridDBColumn *cxGrid1DBTableView1FRM30;
	TcxGridDBColumn *cxGrid1DBTableView1FRM15;
	TcxGridDBColumn *cxGrid1DBTableView1ARM1;
	TcxGridLevel *cxGrid1Level1;
	TcxSplitter *cxSplitter1;
	void __fastcall cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmMortgageRate(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMortgageRate *frmMortgageRate;
//---------------------------------------------------------------------------
#endif
