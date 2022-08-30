//---------------------------------------------------------------------------

#ifndef RealtorWorldStatisticH
#define RealtorWorldStatisticH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
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
#include "dxCustomTileControl.hpp"
#include "dxTileControl.hpp"
#include <DB.hpp>
#include "RealtorWorldBaseFrame.h"
#include "RealtorWorldHomePhotosBase.h"
//---------------------------------------------------------------------------
class TfrmStatistic : public TfrmHomePhotosBase
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox2;
	TcxGrid *cxGrid2;
	TcxGridDBTableView *cxGridDBTableView2;
	TcxGridDBChartView *cxGridDBChartView2;
	TcxGridDBChartSeries *cxGridDBChartView2Series1;
	TcxGridLevel *cxGridLevel2;
	TcxGroupBox *cxGroupBox3;
	TcxGrid *grHousePrice;
	TcxGridDBTableView *grHousePriceDBTableView1;
	TcxGridDBChartView *grHousePriceDBChartView1;
	TcxGridDBChartSeries *grHousePriceDBChartView1Series1;
	TcxGridDBChartSeries *grHousePriceDBChartView1Series2;
	TcxGridLevel *grHousePriceLevel1;
	TcxGrid *cxGrid1;
	TcxGridDBTableView *cxGridDBTableView1;
	TcxGridDBChartView *cxGridDBChartView1;
	TcxGridDBChartSeries *cxGridDBChartSeries1;
	TcxGridLevel *cxGridLevel1;
	TcxSplitter *cxSplitter2;
    TcxSplitter *cxSplitter3;
	TcxStyleRepository *cxStyleRepository1;
	TcxStyle *cxStyle1;
	void __fastcall cxSplitter2BeforeClose(TObject *Sender, bool &AllowClose);
private:	// User declarations
	void __fastcall UpdateGridHousePrice();
protected:
	void __fastcall OnItemClick(TdxTileControlItem *Sender);
public:		// User declarations
	__fastcall TfrmStatistic(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmStatistic *frmStatistic;
//---------------------------------------------------------------------------
#endif
