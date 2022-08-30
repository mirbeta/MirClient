//---------------------------------------------------------------------------

#ifndef StylesMultiDemoMainH
#define StylesMultiDemoMainH
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
#include "cxButtons.hpp"
#include "cxCalc.hpp"
#include "cxCheckBox.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxStyleSheetEditor.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
enum TcxStyleRepositoryType {shtNone, shtPredefined, shtUserDefined};

class TStylesMultiDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TPanel *pnlLeft;
  TGroupBox *gbUserDefined;
  TcxButton *btnLoad;
  TcxButton *btnSave;
  TcxButton *btnEdit;
  TGroupBox *gbPredefined;
  TcxTreeList *tlStyleSheets;
  TcxTreeListColumn *clnRadio;
  TcxTreeListColumn *clnGroupName;
  TPanel *pnlCurrentStyleSheet;
  TSplitter *Splitter;
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListPARENTID;
  TcxDBTreeListColumn *cxDBTreeListNAME;
  TcxDBTreeListColumn *cxDBTreeListPHONE;
  TcxDBTreeListColumn *cxDBTreeListFAX;
  TcxDBTreeListColumn *cxDBTreeListBUDGET;
  TcxDBTreeListColumn *cxDBTreeListVACANCY;
  TcxDBTreeListColumn *cxDBTreeListManager;
  TcxDBTreeListColumn *cxDBTreeListManagerPhone;
  TcxDBTreeListColumn *cxDBTreeListManagerEmail;
  TcxDBTreeListColumn *cxDBTreeListManagerAdress;
  TOpenDialog *OpenDialog;
  TSaveDialog *SaveDialog;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall actSaveToFileExecute(TObject *Sender);
  void __fastcall actLoadFromFileExecute(TObject *Sender);
  void __fastcall actEditStyleSheetExecute(TObject *Sender);
  void __fastcall FormActivate(TObject *Sender);
  void __fastcall tlStyleSheetsIsGroupNode(TcxCustomTreeList *Sender,
    TcxTreeListNode *ANode, bool &IsGroup);
  void __fastcall tlStyleSheetsStylesGetContentStyle(TcxCustomTreeList *Sender,
    TcxTreeListColumn *AColumn, TcxTreeListNode *ANode, TcxStyle *&AStyle);
  void __fastcall tlStyleSheetsStylesGetNodeIndentStyle(TcxCustomTreeList *Sender,
    TcxTreeListNode *ANode, int ALevel, TcxStyle *&AStyle);
  void __fastcall tlStyleSheetsSelectionChanged(TObject *Sender);
  void __fastcall cxDBTreeListInitInsertingRecord(TcxCustomDBTreeList *Sender,
    TcxDBTreeListNode *AFocusedNode, bool &AHandled);
        void __fastcall cxDBTreeListDragOver(TObject *Sender,
          TObject *Source, int X, int Y, TDragState State, bool &Accept);
private:
  TcxTreeListStyleSheet* __fastcall GetCurrentStyleSheet();
  void __fastcall CreateStyleSheetsList(TcxStyleRepositoryType AStyleRepositoryType);
  void __fastcall UpdateGridStyleSheets(TcxTreeListStyleSheet *AStyleSheet);
  void __fastcall ChangeVisibility(TcxStyleRepositoryType AType);
  void __fastcall ClearUserDefinedStyleSheets();
  void __fastcall LoadUserDefinedStyleSheets(TFileName AFileName);
  void __fastcall SaveUserDefinedStyleSheets(TFileName AFileName);
  void __fastcall SelectFistChild(TcxStyleRepositoryType AStyleRepositoryType);
public:		// User declarations
  __fastcall TStylesMultiDemoMainForm(TComponent* Owner);
  virtual bool __fastcall IsNativeDefaultStyle();
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesMultiDemoMainForm *StylesMultiDemoMainForm;
//---------------------------------------------------------------------------
#endif
