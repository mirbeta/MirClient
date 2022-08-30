//---------------------------------------------------------------------------

#ifndef SummaryFooterDemoMainH
#define SummaryFooterDemoMainH
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
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCardView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TSummaryFooterDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
		TMenuItem *miOptions;
		TMenuItem *miSummaries;
		TcxGrid *Grid;
		TcxGridDBTableView *tvCars;
		TcxGridDBColumn *tvCarsTrademark;
		TcxGridDBColumn *tvCarsModel;
		TcxGridDBColumn *tvCarsPicture;
		TcxGridDBColumn *tvCarshp;
		TcxGridDBColumn *tvCarsTorque;
		TcxGridDBColumn *tvCarscyl;
		TcxGridDBColumn *tvCarsTransmissSpeedCount;
		TcxGridDBColumn *tvCarsTransmissAutomatic;
		TcxGridDBColumn *tvCarsMPG_City;
		TcxGridDBColumn *tvCarsMPG_Highway;
		TcxGridDBColumn *tvCarsCategory;
		TcxGridDBColumn *tvCarsDescription;
		TcxGridDBColumn *tvCarsHyperlink;
		TcxGridDBColumn *tvCarsPrice;
		TcxGridDBTableView *tvOrders;
		TcxGridDBColumn *tvOrdersCustomerID;
		TcxGridDBColumn *tvOrdersPurchaseDate;
		TcxGridDBColumn *tvOrdersTime;
		TcxGridDBColumn *tvOrdersPaymentType;
		TcxGridDBColumn *tvOrdersPaymentAmount;
		TcxGridDBColumn *tvOrdersDescription;
		TcxGridDBColumn *tvOrdersQuantity;
		TcxGridLevel *lvCars;
		TcxGridLevel *lvOrders;
		TMenuItem *miCustomizeSummaries;
		TMenuItem *N2;
  		TMenuItem *miSelectedRecordOnly;
		TMenuItem *miIgnoreNullValues;
		TMenuItem *N1;
        TMenuItem *miFooter;
        TMenuItem *miMultiSelect;
        TcxGridPopupMenu *cxGridPopupMenu1;
		void __fastcall FormShow(TObject *Sender);
		void __fastcall miCustomizeSummariesClick(TObject *Sender);
		void __fastcall miSelectedRecordOnlyClick(TObject *Sender);
		void __fastcall miIgnoreNullValuesClick(TObject *Sender);
		void __fastcall miMultiSelectClick(TObject *Sender);
        void __fastcall GridFocusedViewChanged(TcxCustomGrid *Sender,
          TcxCustomGridView *APrevFocusedView, TcxCustomGridView *AFocusedView);
        void __fastcall miFooterClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSummaryFooterDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummaryFooterDemoMainForm *SummaryFooterDemoMainForm;
//---------------------------------------------------------------------------
#endif
