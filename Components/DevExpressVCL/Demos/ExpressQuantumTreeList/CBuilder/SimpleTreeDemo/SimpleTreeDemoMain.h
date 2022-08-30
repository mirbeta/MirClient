//---------------------------------------------------------------------------

#ifndef SimpleTreeDemoMainH
#define SimpleTreeDemoMainH
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
#include "cxDBTL.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
//---------------------------------------------------------------------------
class TSimpleTreeDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListPARENTID;
  TcxDBTreeListColumn *cxDBTreeListNAME;
  TcxDBTreeListColumn *cxDBTreeListBUDGET;
  TcxDBTreeListColumn *cxDBTreeListPHONE;
  TcxDBTreeListColumn *cxDBTreeListFAX;
  TcxDBTreeListColumn *cxDBTreeListEMAIL;
  TcxDBTreeListColumn *cxDBTreeListVACANCY;
  TPopupMenu *mnuNodeOptions;
  TMenuItem *miNodeDelete;
  TMenuItem *miNodeAdd;
  TMenuItem *miNodeAddChild;
  TMenuItem *N2;
  TMenuItem *miExpand;
  TMenuItem *miCollapse;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall mnuNodeOptionsPopup(TObject *Sender);
  void __fastcall miFullCollapseClick(TObject *Sender);
  void __fastcall miFullExpandClick(TObject *Sender);
  void __fastcall miHeadersClick(TObject *Sender);
  void __fastcall miIndicatorClick(TObject *Sender);
  void __fastcall miButtonsClick(TObject *Sender);
  void __fastcall miShowRootClick(TObject *Sender);
  void __fastcall miColumnCustomizationClick(TObject *Sender);
  void __fastcall miNodeDeleteClick(TObject *Sender);
  void __fastcall miNodeAddClick(TObject *Sender);
  void __fastcall miNodeAddChildClick(TObject *Sender);
  void __fastcall miExpandClick(TObject *Sender);
  void __fastcall miCollapseClick(TObject *Sender);
  void __fastcall miPreviewClick(TObject *Sender);
  void __fastcall cxDBTreeListInitInsertingRecord(TcxCustomDBTreeList *Sender,
    TcxDBTreeListNode *AFocusedNode, bool &AHandled);
private:
  TcxDBTreeListNode *FHitNode;
  void __fastcall InsertNode(int AParentID);
public:		// User declarations
  __fastcall TSimpleTreeDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSimpleTreeDemoMainForm *SimpleTreeDemoMainForm;
//---------------------------------------------------------------------------
#endif
