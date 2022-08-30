//---------------------------------------------------------------------------

#ifndef FilterByCodeDemoMainH
#define FilterByCodeDemoMainH
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
#include <ExtCtrls.hpp>
#include "cxCalendar.hpp"
#include "cxDataStorage.hpp"
#include "cxSpinEdit.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxContainer.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridCardView.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
//---------------------------------------------------------------------------

enum TDateType { dtFirstOfYear, dtLastOfYear };

enum TUserFiltering { ufNone, ufCustom, ufSimple, ufLike, ufTwoField, ufBetween,
    ufUserFilter, ufGroup, ufList };

class TFilterByCodeDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridDBTableView *tvCustomers;
  TcxGridDBColumn *tvCustomersFIRSTNAME;
  TcxGridDBColumn *tvCustomersLASTNAME;
  TcxGridDBColumn *tvCustomersCOMPANYNAME;
  TcxGridDBColumn *tvCustomersPAYMENTTYPE;
  TcxGridDBColumn *tvCustomersPRODUCTID;
  TcxGridDBColumn *tvCustomersCUSTOMER;
  TcxGridDBColumn *tvCustomersPURCHASEDATE;
  TcxGridDBColumn *tvCustomersPAYMENTAMOUNT;
  TcxGridDBColumn *tvCustomersCOPIES;
  TcxGridLevel *lvCustomers;
  TPanel *Panel1;
	TcxLabel *Label1;
	TcxComboBox *cbFilters;
  TMenuItem *miOptions;
  TMenuItem *miFilterBoxPosition;
  TMenuItem *miFilterBoxPosTop;
  TMenuItem *miFilterBoxPosBottom;
  void __fastcall miFilterBoxPosClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall tvCustomersDataControllerFilterChanged(TObject *Sender);
  void __fastcall cbFiltersChange(TObject *Sender);
  void __fastcall tvCustomersPRODUCTIDUserFiltering(TcxCustomGridTableItem *Sender,
          const Variant &AValue, const String ADisplayText);
  void __fastcall tvCustomersPRODUCTIDGetFilterValues(
      TcxCustomGridTableItem *Sender, TcxDataFilterValueList *AValueList);
private:  // User declarations
  bool FLock;
  TDate GetDate(TDateType ADateType);
  int GetFilterIndex(const TUserFiltering AFiltering);
  void PopulateFilterList();
  void SetFilter(const TUserFiltering AFiltering);
  void SetOnlyGridsFilter(TcxFilterCriteriaItemList *AFilterCriteriaList);
public:   // User declarations
  __fastcall TFilterByCodeDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilterByCodeDemoMainForm *FilterByCodeDemoMainForm;
//---------------------------------------------------------------------------
#endif
