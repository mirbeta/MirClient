//---------------------------------------------------------------------------

#ifndef DragDropDemoMainH
#define DragDropDemoMainH
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
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TDragDropDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TSplitter *Splitter1;
  TPanel *pnlDepartments;
  TcxDBTreeList *tlDepartments;
  TcxDBTreeListColumn *tlDepartmentsID;
  TcxDBTreeListColumn *tlDepartmentsPARENTID;
  TcxDBTreeListColumn *tlDepartmentsNAME;
  TcxDBTreeListColumn *tlDepartmentsBUDGET;
  TcxDBTreeListColumn *tlDepartmentsPHONE;
  TcxDBTreeListColumn *tlDepartmentsFAX;
  TcxDBTreeListColumn *tlDepartmentsEMAIL;
  TcxDBTreeListColumn *tlDepartmentsVACANCY;
  TPanel *pnlDeptCaption;
  TPanel *pnlEmployees;
  TPanel *pnlEmplCaption;
  TcxDBTreeList *tlEmployees;
  TcxDBTreeListColumn *tlEmployeesName;
  TcxDBTreeListColumn *tlEmployeesCountry;
  TcxDBTreeListColumn *tlEmployeesPostalCode;
  TcxDBTreeListColumn *tlEmployeesCity;
  TcxDBTreeListColumn *tlEmployeesAddress;
  TcxDBTreeListColumn *tlEmployeesPhone;
  TcxDBTreeListColumn *tlEmployeesFax;
  TcxDBTreeListColumn *tlEmployeesEMAIL;
  TcxDBTreeListColumn *tlEmployeesHOMEPAGE;
  TcxDBTreeListColumn *tlEmployeesDepartmentID;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miColumnCustomizationClick(TObject *Sender);
  void __fastcall miDragCollapseClick(TObject *Sender);
  void __fastcall miDragExpandeClick(TObject *Sender);
  void __fastcall ShowDictionaries1Click(TObject *Sender);
  void __fastcall tlDepartmentsInitInsertingRecord(TcxCustomDBTreeList *Sender,
    TcxDBTreeListNode *AFocusedNode, bool &AHandled);
  void __fastcall tlDepartmentsDragDrop(TObject *Sender, TObject *Source, int X,
    int Y);
  void __fastcall tlDepartmentsDragOver(TObject *Sender, TObject *Source, int X,
    int Y, TDragState State, bool &Accept);
  void __fastcall tlEmployeesDragDrop(TObject *Sender, TObject *Source, int X,
    int Y);
  void __fastcall tlEmployeesDragOver(TObject *Sender, TObject *Source, int X,
    int Y, TDragState State, bool &Accept);
  void __fastcall tlEmployeesMoveTo(TcxCustomTreeList *Sender,
    TcxTreeListNode *AttachNode, TcxTreeListNodeAttachMode AttachMode,
    TList Nodes, bool &IsCopy, bool &Done);
private:
  bool IsHitAtNode(TcxDBTreeList *ATreeList, int X, int Y);
  bool IsDropAsChild(TcxDBTreeList *Sender);
  void SetEmplDictSelectionDeptID(Variant AValue);
public:		// User declarations
  __fastcall TDragDropDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDragDropDemoMainForm *DragDropDemoMainForm;
//---------------------------------------------------------------------------
#endif
