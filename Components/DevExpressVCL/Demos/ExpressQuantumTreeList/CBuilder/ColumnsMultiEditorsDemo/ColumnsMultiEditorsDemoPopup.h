//---------------------------------------------------------------------------

#ifndef ColumnsMultiEditorsDemoPopupH
#define ColumnsMultiEditorsDemoPopupH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TColumnsMultiEditorsDemoPopupForm : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlPopup;
  TcxTreeList *tlPopup;
  TcxTreeListColumn *clText;
  TImageList *ilPoupuImages;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *stlHotRoot;
  TcxStyle *stlContenet;
  TcxStyle *stlHotItem;
  void __fastcall tlPopupGetNodeImageIndex(TcxCustomTreeList *Sender,
      TcxTreeListNode *ANode, TcxTreeListImageIndexType AIndexType,
      TcxImageIndex &AIndex);
  void __fastcall tlPopupHotTrackNode(TcxCustomTreeList *Sender, TcxTreeListNode *ANode,
      TShiftState AShift, TCursor &ACursor);
  void __fastcall tlPopupClick(TObject *Sender);
  void __fastcall tlPopupStylesGetHotTrackStyle(TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
      TcxTreeListNode *ANode, TcxStyle *&AStyle);
  void __fastcall tlPopupKeyDown(TObject *Sender, Word &Key,
      TShiftState Shift);
private:
  TcxPopupEdit *FPopupEdit;
  void ClosePopupForm(bool Accept);
public:
  __property TcxPopupEdit *PopupEdit = {read = FPopupEdit, write = FPopupEdit};
  __fastcall TColumnsMultiEditorsDemoPopupForm(TComponent* Owner);
};
#endif
