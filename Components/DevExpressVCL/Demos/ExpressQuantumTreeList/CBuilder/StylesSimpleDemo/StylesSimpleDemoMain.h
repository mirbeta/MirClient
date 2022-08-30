//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoMainH
#define StylesSimpleDemoMainH
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
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxMRUEdit.hpp"
#include "cxTextEdit.hpp"
//---------------------------------------------------------------------------
class TStylesSimpleDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeList1ID;
  TcxDBTreeListColumn *cxDBTreeList1PARENTID;
  TcxDBTreeListColumn *cxDBTreeList1MANAGERID;
  TcxDBTreeListColumn *cxDBTreeList1NAME;
  TcxDBTreeListColumn *cxDBTreeList1BUDGET;
  TcxDBTreeListColumn *cxDBTreeList1LOCATION;
  TcxDBTreeListColumn *cxDBTreeList1PHONE;
  TcxDBTreeListColumn *cxDBTreeList1FAX;
  TcxDBTreeListColumn *cxDBTreeList1EMAIL;
  TcxDBTreeListColumn *cxDBTreeList1VACANCY;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall actHeadersExecute(TObject *Sender);
  void __fastcall actFooterExecute(TObject *Sender);
  void __fastcall actIndicatorExecute(TObject *Sender);
  void __fastcall actPreviewExecute(TObject *Sender);
  void __fastcall actShowStyleDialogExecute(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall cxDBTreeListInitInsertingRecord(TcxCustomDBTreeList *Sender,
    TcxDBTreeListNode *AFocusedNode, bool &AHandled);
private:
  void __fastcall RestoreDefaults(TObject *Sender);
public:		// User declarations
  __fastcall TStylesSimpleDemoMainForm(TComponent* Owner);
  virtual bool __fastcall IsNativeDefaultStyle();
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoMainForm *StylesSimpleDemoMainForm;
//---------------------------------------------------------------------------
#endif
