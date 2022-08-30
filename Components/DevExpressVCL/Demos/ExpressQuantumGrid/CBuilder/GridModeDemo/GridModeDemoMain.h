//---------------------------------------------------------------------------

#ifndef GridModeDemoMainH
#define GridModeDemoMainH
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
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "cxDBLookupComboBox.hpp"
#include "BaseForm.h"
#include "dxCore.hpp"
#include "cxBlobEdit.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxGridCardView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxProgressBar.hpp"
#include "cxSpinEdit.hpp"
//---------------------------------------------------------------------------
class TGridModeDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
        TcxGrid *Grid;
        TcxGridDBTableView *tvCars;
        TcxGridDBColumn *tvCarsTrademark;
        TcxGridDBColumn *tvCarsModel;
        TcxGridDBColumn *tvCarshp;
        TcxGridDBColumn *tvCarsliter;
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
        TcxGridDBColumn *tvOrdersPaymentType;
        TcxGridDBColumn *tvOrdersPaymentAmount;
        TcxGridLevel *lvCars;
        TcxGridLevel *lvOrders;
        TPanel *pnlPopulate;
        TcxButton *btnPopulate;
        TPanel *pnlProgress;
	TcxProgressBar *ProgressBar;
        TMenuItem *miOptions;
        TMenuItem *miCustomizeViews;
        TMenuItem *miCalculateSummaries;
        TMenuItem *miEnableSorting;
        TMenuItem *miEnableFiltering;
        TMenuItem *N1;
        TMenuItem *miRecreateDB;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall miCalculateSummariesClick(TObject *Sender);
  void __fastcall miEnableSortingClick(TObject *Sender);
  void __fastcall miEnableFilteringClick(TObject *Sender);
  void __fastcall tvDataControllerFilterGetValueList(
      TcxFilterCriteria *Sender, int AItemIndex,
      TcxDataFilterValueList *AValueList);
  void __fastcall tvDataControllerSortingChanged(TObject *Sender);
  void __fastcall tvOrdersDataControllerSummaryAfterSummary(
      TcxDataSummary *ASender);
  void __fastcall btnPopulateClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall miRecreateDBClick(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:  // User declarations
  void EnableSummaries(bool AEnable, TcxGridDBTableView *AView);
  void EnableSorting(TcxGridDBTableView *AView, bool AEnable);
  void InitPopulateButton();
  void SortClone(TcxGridDBDataController *ADataController);
  void SortPattern(TcxGridDBDataController *ADataController);
  String SummaryKindToStr(TcxSummaryKind AKind);
  String GetSQLCondition(TcxGridDBTableView *AView, bool AAddFilter);
public:   // User declarations
  __fastcall TGridModeDemoMainForm(TComponent* Owner);
  void __fastcall DoStepItforProgressBar(TObject *Sender);
};
//---------------------------------------------------------------------------
extern PACKAGE TGridModeDemoMainForm *GridModeDemoMainForm;
//---------------------------------------------------------------------------
#endif
