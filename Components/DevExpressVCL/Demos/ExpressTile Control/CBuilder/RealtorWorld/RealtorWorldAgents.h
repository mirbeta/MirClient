//---------------------------------------------------------------------------

#ifndef RealtorWorldAgentsH
#define RealtorWorldAgentsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCurrencyEdit.hpp"
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
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMemo.hpp"
#include "cxSplitter.hpp"
#include "cxStyles.hpp"
#include "dxCustomTileControl.hpp"
#include "dxTileControl.hpp"
#include <DB.hpp>
#include <DBClient.hpp>
#include <ExtCtrls.hpp>
#include "RealtorWorldBaseFrame.h"
//---------------------------------------------------------------------------
class TfrmAgents : public TfrmBase
{
__published:	// IDE-managed Components
	TClientDataSet *cdsChart;
	TIntegerField *cdsChartMidWest;
	TIntegerField *cdsChartNorthEast;
	TIntegerField *cdsChartSouth;
	TIntegerField *cdsChartWest;
	TIntegerField *cdsChartAgentID;
	TIntegerField *cdsChartDate;
	TcxSplitter *cxSplitter2;
	TcxStyleRepository *cxStyleRepository1;
	TcxStyle *cxStyle1;
	TPanel *pnDetailSite;
	TcxGrid *cxgDetailInfo;
	TcxGridDBTableView *cxGridDBTableView1;
	TcxGridDBColumn *cxGridDBTableView1Photo;
	TcxGridDBColumn *cxGridDBTableView1Address;
	TcxGridDBColumn *cxGridDBTableView1Beds;
	TcxGridDBColumn *cxGridDBTableView1Baths;
	TcxGridDBColumn *cxGridDBTableView1HouseSize;
	TcxGridDBColumn *cxGridDBTableView1Price;
	TcxGridLevel *cxGridLevel1;
	TcxGrid *cxgDetailChart;
	TcxGridDBChartView *cxgDetailChartDBChartView1;
	TcxGridDBChartSeries *cxgNorthEastSeries;
	TcxGridDBChartSeries *cxgMidWestSeries;
	TcxGridDBChartSeries *cxgSouthSeries;
	TcxGridDBChartSeries *cxgWestSeries;
	TcxGridLevel *cxgDetailChartLevel1;
	TcxSplitter *cxSplitter1;
	TdxTileControl *tcAgents;
	TDataSource *dsChart;
	void __fastcall cxSplitter2BeforeClose(TObject *Sender, bool &AllowClose);
	void __fastcall AA(TObject *Sender, int &NewWidth, int &NewHeight, bool &Resize);
private:	// User declarations
	void __fastcall InitializeFrame();
	void __fastcall InitializeChartDataSet();
	void __fastcall OnItemClick(TdxTileControlItem *Sender);
public:		// User declarations
	__fastcall TfrmAgents(TComponent* Owner);
	void __fastcall SelectItem(int APhotoID, int AAgentID);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAgents *frmAgents;
//---------------------------------------------------------------------------
#endif
