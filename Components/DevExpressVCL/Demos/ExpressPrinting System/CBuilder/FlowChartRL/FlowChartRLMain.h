//---------------------------------------------------------------------------
#ifndef FlowChartRLMainH
#define FlowChartRLMainH
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
#include <Db.hpp>
#include "dxflchrt.hpp"
#include "dxPSCore.hpp"
#include "dxPSdxFCLnk.hpp"
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
#include <ImgList.hpp>
#include "cxDrawTextUtils.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "DemoBasicMain.h"
#include "cxGraphics.hpp"
#include <ActnList.hpp>
//---------------------------------------------------------------------------
class TFlowChartRLMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
        TdxFlowChart *dxFlowChart;
		TdxFlowChartReportLink *dxComponentPrinterLink1;
        TImageList *ImageList1;
    
private:	// User declarations
public:		// User declarations
    __fastcall TFlowChartRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFlowChartRLMainForm *FlowChartRLMainForm;
//---------------------------------------------------------------------------
#endif
