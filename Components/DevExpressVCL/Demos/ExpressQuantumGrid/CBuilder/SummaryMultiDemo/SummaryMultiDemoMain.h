//---------------------------------------------------------------------------

#ifndef SummaryMultiDemoMainH
#define SummaryMultiDemoMainH
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
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TSummaryMultiDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
        TMenuItem *miOptions;
        TMenuItem *miSummaries;
        TMenuItem *miSelectedRecordsOnly;
        TMenuItem *miIgnoreNullValues;
        TMenuItem *miUseOnAfterSummaryEvent;
        TMenuItem *N1;
        TMenuItem *miGroupFooters;
        TMenuItem *miGroupFooterNeverShow;
        TMenuItem *miGroupFooterShowWhenExpand;
        TMenuItem *miGroupFooterAlwaysShow;
        TMenuItem *miMultipleSummariesInFooter;
        TMenuItem *miMultipleSummariesInGroupFooters;
        TMenuItem *N2;
        TMenuItem *miMultiSelect;
        TcxGridPopupMenu *cxGridPopupMenu1;
        TcxGrid *Grid;
        TcxGridDBTableView *tvOrders;
        TcxGridDBColumn *tvOrdersCustomerID;
        TcxGridDBColumn *tvOrdersProductID;
        TcxGridDBColumn *tvOrdersPurchaseDate;
        TcxGridDBColumn *tvOrdersPaymentAmount;
        TcxGridDBColumn *tvOrdersPaymentType;
        TcxGridDBColumn *tvOrdersQuantity;
        TcxGridLevel *lvOrders;
		void __fastcall FormShow(TObject *Sender);
		void __fastcall miSelectedRecordsOnlyClick(TObject *Sender);
		void __fastcall miIgnoreNullValuesClick(TObject *Sender);
		void __fastcall miMultiSelectClick(TObject *Sender);
		void __fastcall miUseOnAfterSummaryEventClick(TObject *Sender);
		void __fastcall tvOrdersDataControllerSummaryAfterSummary(TcxDataSummary* ASender);
		void __fastcall tvOrdersDataControllerSummaryDefaultGroupSummaryItemsSummary(
		  TcxDataSummaryItems *ASender, const TcxSummaryEventArguments &Arguments,
		  TcxSummaryEventOutArguments &OutArguments);
		void __fastcall miGroupFootersClick(TObject *Sender);
		void __fastcall miMultipleSummariesInFooterClick(TObject *Sender);
		void __fastcall miMultipleSummariesInGroupFootersClick(TObject *Sender);
		void __fastcall tvOrdersStylesGetFooterSummaryStyle(
		  TcxGridTableView *AView, TcxCustomGridRow *ARow,
		  TcxGridColumn *AColumn, int AFooterGroupLevel,
		  TcxDataSummaryItem *ASummaryItem, TcxStyle *&AStyle);
		void __fastcall tvOrdersStylesGetGroupSummaryStyle(
		  TcxGridTableView *Sender, TcxGridGroupRow *ARow,
		  TcxGridColumn *AColumn, TcxDataSummaryItem *ASummaryItem,
		  TcxStyle *&AStyle);
private:	// User declarations
		Variant CalculateFooterSummaryValue(int AIndex, TcxSummaryKind AKind);
protected:
		void UpdateMenu();
public:		// User declarations
  __fastcall TSummaryMultiDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryMultiDemoMainForm *SummaryMultiDemoMainForm;
//---------------------------------------------------------------------------
#endif
