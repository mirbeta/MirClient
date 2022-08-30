//---------------------------------------------------------------------------

#ifndef PrefilterByCodeDemoMainH
#define PrefilterByCodeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxDBPivotGrid.hpp"
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------

enum TDateType { dtFirst, dtLast };

enum TUserFiltering { ufNone, ufCustom, ufSimple, ufList, ufTwoField, ufBetween,
    ufUserFilter, ufGroup };

class TfmPrefilterByCode : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
    TcxDBPivotGrid *DBPivotGrid;
    TcxDBPivotGridField *pgfPurchaseDate;
    TcxDBPivotGridField *pgfPaymentType;
    TcxDBPivotGridField *pgfQuantity;
    TcxDBPivotGridField *pgfCarName;
    TcxDBPivotGridField *pgfUnitPrice;
    TcxDBPivotGridField *pgfCompanyName;
    TcxDBPivotGridField *pgfPaymentAmount;
    TPanel *Panel1;
    TLabel *Label1;
    TComboBox *cbFilters;
    TMenuItem *PrefilterPosition1;
    TMenuItem *Bottom1;
    TMenuItem *op1;
    void __fastcall pgfPaymentTypeGetGroupImageIndex(
      TcxPivotGridField *Sender, const TcxPivotGridViewDataItem *AItem,
      int &AImageIndex, TAlignment &AImageAlignHorz,
      TcxAlignmentVert &AImageAlignVert);
    void __fastcall Bottom1Click(TObject *Sender);
    void __fastcall cbFiltersChange(TObject *Sender);
    void __fastcall DBPivotGridFilterChanged(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
    TcxCustomPivotGrid* __fastcall PivotGrid();
    bool FLock;
    TDate GetDate(TDateType ADateType);
    int GetFilterIndex(const TUserFiltering AFiltering);
    TcxDBDataFilterCriteria* __fastcall GetPrefilter();
    void PopulateFilterList();
    void SetFilter(const TUserFiltering AFiltering);
    void SetOnlyMercedesFilter(TcxFilterCriteriaItemList *AFilterCriteriaList);
protected:
    __property TcxDBDataFilterCriteria* Prefilter = {read = GetPrefilter};
public:		// User declarations
    __fastcall TfmPrefilterByCode(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmPrefilterByCode *fmPrefilterByCode;
//---------------------------------------------------------------------------
#endif
