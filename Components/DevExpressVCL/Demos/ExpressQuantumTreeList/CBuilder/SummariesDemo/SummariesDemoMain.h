//---------------------------------------------------------------------------

#ifndef SummariesDemoMainH
#define SummariesDemoMainH
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
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxCalc.hpp"
#include "cxDBEditRepository.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxImageComboBox.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTimeEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxCalendar.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxCheckBox.hpp"
//---------------------------------------------------------------------------
class TSummariesDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *tlDepartments;
  TcxDBTreeListColumn *clName;
  TcxDBTreeListColumn *clBudget;
  TcxDBTreeListColumn *clPhone;
  TcxDBTreeListColumn *clFax;
  TcxDBTreeListColumn *clEmail;
  TcxDBTreeListColumn *clVacancy;
  TLabel* lblSummary;
  TcxImageList *ilUser;
  void __fastcall clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText(TcxTreeListSummaryItem *Sender,
  const Variant &AValue, String &AText);
  void __fastcall tlDepartmentsAfterSummary(TObject *Sender);
  void __fastcall tlDepartmentsSummary(TcxCustomTreeList *ASender, const TcxTreeListSummaryEventArguments &Arguments,
	TcxTreeListSummaryEventOutArguments &OutArguments);
  void __fastcall tlDepartmentsPopupMenusFooterMenuClick(TcxCustomTreeList *Sender,
	TObject *AItem, bool &AHandled);
  void __fastcall tlDepartmentsPopupMenusFooterMenuPopup(TcxCustomTreeList *Sender,
	TcxTreeListPopupMenu *AContextMenu, bool &AHandled);
  void __fastcall FormCreate(TObject *Sender);
private:
  bool FAlternateCounting;
  bool FCheckBudget;
  bool FCheckVacancies;
public:		// User declarations
  __fastcall TSummariesDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSummariesDemoMainForm *SummariesDemoMainForm;
//---------------------------------------------------------------------------
#endif
