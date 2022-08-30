//---------------------------------------------------------------------------
#ifndef TeeChartRLMainH
#define TeeChartRLMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Graphics.hpp>
#include <Forms.hpp>
#include <checklst.hpp>
#include <ComCtrls.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#if __BORLANDC__ >= 0x0642
  #include <VCLTee.Chart.hpp>
  #include <VCLTee.DBChart.hpp>
  #include <VCLTee.Series.hpp>
  #include <VCLTee.TeEngine.hpp>
  #include <VCLTee.TeeProcs.hpp>
#else
  #include <Chart.hpp>
  #include <DBChart.hpp>
  #include <Series.hpp>
  #include <TeEngine.hpp>
  #include <TeeProcs.hpp>
#endif
#include <ExtCtrls.hpp>
#include "dxPSCore.hpp"
#include "dxPSDBTCLnk.hpp"
#include "dxPSTCLnk.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSGraphicLnk.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include <DB.hpp>
#include <ImgList.hpp>
#include "cxDrawTextUtils.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "cxGraphics.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TTeeChartRLMainForm : public TDemoBasicMainForm
{
__published:  // IDE-managed Components
    TPageControl *PageControl1;
    TTabSheet *tsDBTeeChart;
    TTabSheet *tsTeeChart;
    TBarSeries *Series5;
    TBarSeries *Series7;
    TBarSeries *Series6;
    TDataSource *DataSource1;
    TChart *TeeChart;
    TDBChart *DBChart;
	TdxMemData *mdTeeChart;
	TStringField *mdTeeChartNAME;
	TSmallintField *mdTeeChartSIZE;
	TSmallintField *mdTeeChartWEIGHT;
    TPieSeries *PieSeries1;
    void __fastcall PageControl1Change(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
public:   // User declarations
    __fastcall TTeeChartRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TTeeChartRLMainForm *TeeChartRLMainForm;
//---------------------------------------------------------------------------
#endif
