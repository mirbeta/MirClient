//---------------------------------------------------------------------------

#ifndef DragDropDemoDictionaryH
#define DragDropDemoDictionaryH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxCheckBox.hpp"
#include "cxControls.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxCustomData.hpp"
#include "cxDBTL.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxStyles.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TDragDropDemoDictionaryForm : public TForm
{
__published:	// IDE-managed Components
  TPageControl *pcDictionary;
  TTabSheet *tsDepartments;
  TLabel *lsc;
  TcxDBTreeList *tlDeptDict;
  TcxDBTreeListColumn *tlDeptDictPARENTID;
  TcxDBTreeListColumn *tlDeptDictNAME;
  TcxDBTreeListColumn *tlDeptDictBUDGET;
  TcxDBTreeListColumn *tlDeptDictPHONE;
  TcxDBTreeListColumn *tlDeptDictFAX;
  TcxDBTreeListColumn *tlDeptDictEMAIL;
  TcxDBTreeListColumn *tlDeptDictVACANCY;
  TTabSheet *tsPersons;
  TLabel *Label1;
  TcxDBTreeList *tlEmplDict;
  TcxDBTreeListColumn *tlEmplDictName;
  TcxDBTreeListColumn *tlEmplDictCountry;
  TcxDBTreeListColumn *tlEmplDictPostalCode;
  TcxDBTreeListColumn *tlEmplDictCity;
  TcxDBTreeListColumn *tlEmplDictAddress;
  TcxDBTreeListColumn *tlEmplDictPhone;
  TcxDBTreeListColumn *tlEmplDictFax;
  TcxDBTreeListColumn *tlEmplDictEMAIL;
  TcxDBTreeListColumn *tlEmplDictHOMEPAGE;
  TcxDBTreeListColumn *tlEmplDictDepartmentID;
  void __fastcall tlDictDragOver(TObject *Sender, TObject *Source, int X, int Y,
    TDragState State, bool &Accept);
  void __fastcall tlDictMoveTo(TcxCustomTreeList *Sender, TcxTreeListNode *AttachNode,
    TcxTreeListNodeAttachMode AttachMode, TList *Nodes, bool &IsCopy, bool &Done);
public:
  __fastcall TDragDropDemoDictionaryForm(TComponent* Owner);
  void SetDeptSelectionParentValue(Variant AParentValue);
  void SetEmplSelectionDeptID(Variant ADepartmentID);
};

void SetSelectedNodesValue(TcxDBTreeList *ATreeList, int AItemIndex,
  Variant AValue);

//---------------------------------------------------------------------------
extern PACKAGE TDragDropDemoDictionaryForm *DragDropDemoDictionaryForm;
//---------------------------------------------------------------------------
#endif
