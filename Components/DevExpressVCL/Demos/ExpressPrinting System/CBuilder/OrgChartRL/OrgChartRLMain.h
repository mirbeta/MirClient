//---------------------------------------------------------------------------
#ifndef OrgChartRLMainH
#define OrgChartRLMainH
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
#include "dxorgchr.hpp"
#include "dxdborgc.hpp"
#include "dxPSCore.hpp"
#include "dxPSdxDBOCLnk.hpp"
#include "dxPSdxOCLnk.hpp"
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
#include "cxControls.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxmdaset.hpp"
#include <DBClient.hpp>

//---------------------------------------------------------------------------
class TOrgChartRLMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TPageControl *PageControl1;
	TTabSheet *tsDBOrgChart;
	TTabSheet *tsOrgChart;
    TdxOrgChart *dxOrgChart;
	TdxDbOrgChart *dxDBOrgChart;
	TDataSource *dsOCTable;
	TClientDataSet *Table1;
    void __fastcall PageControl1Change(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall dxDBOrgChartCreateNode(TObject *Sender,
          TdxOcNode *Node);
private:	// User declarations
	TdxOcShape __fastcall GetShape(AnsiString ShapeName);
	TdxOcImageAlign __fastcall GetImageAlign(AnsiString AlignName);
public:		// User declarations
    __fastcall TOrgChartRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TOrgChartRLMainForm *OrgChartRLMainForm;
//---------------------------------------------------------------------------
#endif
