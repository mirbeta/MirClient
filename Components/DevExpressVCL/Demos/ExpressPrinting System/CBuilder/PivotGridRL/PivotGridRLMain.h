//---------------------------------------------------------------------------

#ifndef PivotGridRLMainH
#define PivotGridRLMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxPivotGrid.hpp"
#include "dxPSCore.hpp"
#include "dxPScxCommon.hpp"
#include "dxPScxPivotGridLnk.hpp"
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include "cxDrawTextUtils.hpp"
#include "DemoBasicMain.h"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include <ActnList.hpp>

//---------------------------------------------------------------------------
class TPivotGridRLMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
        TcxPivotGrid *PivotGrid;
        TcxPivotGridField *pgfPurchaseQuarter;
        TcxPivotGridField *pgfPurchaseMonth;
        TcxPivotGridField *pgfPaymentType;
        TcxPivotGridField *pgfQuantity;
        TcxPivotGridField *pgfCarName;
        TcxPivotGridField *pgfUnitPrice;
        TcxPivotGridField *pgfCompanyName;
        TcxPivotGridField *pgfPaymentAmount;
        TImageList *PaymentTypeImages;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall pgfPaymentTypeGetGroupImageIndex(
          TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
          int &AImageIndex, TAlignment &AImageAlignHorz,
          TcxAlignmentVert &AImageAlignVert);
private:	// User declarations
public:		// User declarations
        __fastcall TPivotGridRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPivotGridRLMainForm *PivotGridRLMainForm;
//---------------------------------------------------------------------------
#endif
