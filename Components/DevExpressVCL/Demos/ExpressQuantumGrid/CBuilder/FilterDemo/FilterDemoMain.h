//---------------------------------------------------------------------------

#ifndef FilterDemoMainH
#define FilterDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "BaseForm.h"
#include "cxGridCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
        TPanel *Panel1;
        TPanel *pnlMaskInfo;
        TcxGrid *Grid;
        TcxGridTableView *TableView;
        TcxGridColumn *TableViewOrderID;
        TcxGridColumn *TableViewCompany;
        TcxGridColumn *TableViewCountry;
        TcxGridColumn *TableViewProduct;
        TcxGridColumn *TableViewOrderDate;
        TcxGridColumn *TableViewQuantity;
        TcxGridLevel *GridLevel1;
        TDataSource *dsCompanies;
        TDataSource *dsCountries;
        TMenuItem *miView;
        TMenuItem *miColumnFilterPopupMultiSelect;
        TMenuItem *miApplyMultiSelectChanges;
        TMenuItem *miApplyMultiSelectChangesImmediately;
        TMenuItem *miApplyMultiSelectChangesOnButtonClick;
        TMenuItem *N1;
        TMenuItem *miColumnFilterPopupFilteredList;
        TMenuItem *N2;
        TMenuItem *miFilterRow;
        TMenuItem *miApplyFilterRowChanges;
        TMenuItem *miApplyFilterRowChangesOnCellExit;
        TMenuItem *miApplyFilterRowChangesImmediately;
        TMenuItem *miDateTimeFilters;
        TMenuItem *miDateTimeFilterRelativeDays;
        TMenuItem *miDateTimeFilterRelativeDayPeriods;
        TMenuItem *miDateTimeFilterRelativeWeeks;
        TMenuItem *miDateTimeFilterRelativeMonths;
        TMenuItem *miDateTimeFilterRelativeYears;
        TMenuItem *miDateTimeFilterPastFuture;
        TMenuItem *miDateTimeFilterMonths;
        TMenuItem *miDateTimeFilterYears;
        TcxEditRepository *erMain;
        TcxEditRepositoryImageItem *erMainFlag;
	TClientDataSet *cdsProducts;
	TAutoIncField *cdsProductsID;
	TStringField *cdsProductsName;
	TMemoField *cdsProductsDescription;
	TStringField *cdsProductsPlatform;
	TBlobField *cdsProductsLogo;
	TMemoField *cdsProductsLink;
	TClientDataSet *cdsCountries;
	TAutoIncField *cdsCountriesID;
	TStringField *cdsCountriesNAME;
	TStringField *cdsCountriesACRONYM;
	TBlobField *cdsCountriesNATIONALFLAG;
	TClientDataSet *cdsCompanies;
	TAutoIncField *cdsCompaniesID;
	TIntegerField *cdsCompaniesCOMPANYTYPEID;
	TIntegerField *cdsCompaniesCOUNTRYID;
	TStringField *cdsCompaniesCOMPANYNAME;
	TStringField *cdsCompaniesCOMPANYWEBSITE;
	TMenuItem *miAllowOperatorCustomization;
        void __fastcall FormCreate(TObject *Sender);
	void __fastcall miColumnFilterPopupMultiSelectClick(TObject *Sender);
	void __fastcall miApplyMultiSelectChangesClick(TObject *Sender);
	void __fastcall miColumnFilterPopupFilteredListClick(TObject *Sender);
	void __fastcall miFilterRowClick(TObject *Sender);
	void __fastcall miApplyFilterRowChangesClick(TObject *Sender);
	void __fastcall miDateTimeFilterClick(TObject *Sender);
	void __fastcall miAllowOperatorCustomizationClick(TObject *Sender);
protected:
    void GenerateData();
    AnsiString GetProductName(int AID);
    void UpdateMenuValues();
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
