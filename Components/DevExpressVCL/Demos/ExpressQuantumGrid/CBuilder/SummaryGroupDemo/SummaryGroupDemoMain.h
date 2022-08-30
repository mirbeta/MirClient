//---------------------------------------------------------------------------

#ifndef SummaryGroupDemoMainH
#define SummaryGroupDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxLookAndFeels.hpp"
#include "cxBlobEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxImageComboBox.hpp"
#include "cxSpinEdit.hpp"
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TSummaryGroupDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
        TcxGrid *Grid;
        TcxGridDBTableView *tvOrders;
        TcxGridDBColumn *tvOrdersCustomerID;
        TcxGridDBColumn *tvOrdersProductID;
        TcxGridDBColumn *tvOrdersDescription;
        TcxGridDBColumn *tvOrdersPurchaseDate;
        TcxGridDBColumn *tvOrdersPaymentAmount;
        TcxGridDBColumn *tvOrdersPaymentType;
        TcxGridDBColumn *tvOrdersQuantity;
        TcxGridLevel *lvOrders;
        TMenuItem *miOptions;
        TMenuItem *miSummaries;
        TMenuItem *miSelectedRecordsOnly;
        TMenuItem *miIgnoreNullValues;
        TMenuItem *N1;
        TMenuItem *miGroupSummaryLayout;
        TMenuItem *miGroupSummaryLayoutStandard;
        TMenuItem *miGroupSummaryLayoutAlignWithColumns;
        TMenuItem *miGroupSummaryLayoutAlignWithColumnsAndDistribute;
        TMenuItem *miGroupFooter;
        TMenuItem *miGroupFooterNeverShow;
        TMenuItem *miGroupFooterShowWhenExpand;
        TMenuItem *miGroupFooterAlwaysShow;
        TMenuItem *N3;
        TMenuItem *miGroupFootersAtCarLevel;
        TMenuItem *miGroupFootersAtCompanyLevel;
        TMenuItem *N2;
        TMenuItem *miMultiSelect;
		TcxGridPopupMenu *cxGridPopupMenu1;
		void __fastcall FormCreate(TObject *Sender);
		void __fastcall FormShow(TObject *Sender);
        void __fastcall miSelectedRecordsOnlyClick(TObject *Sender);
        void __fastcall miIgnoreNullValuesClick(TObject *Sender);
        void __fastcall miMultiSelectClick(TObject *Sender);
        void __fastcall miGroupFooterShowClick(TObject *Sender);
        void __fastcall miGroupSummaryLayoutClick(TObject *Sender);
        void __fastcall miGroupFootersAtLevelClick(TObject *Sender);
protected:
    void UpdateMenu();
public:		// User declarations
  __fastcall TSummaryGroupDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryGroupDemoMainForm *SummaryGroupDemoMainForm;
//---------------------------------------------------------------------------
#endif
