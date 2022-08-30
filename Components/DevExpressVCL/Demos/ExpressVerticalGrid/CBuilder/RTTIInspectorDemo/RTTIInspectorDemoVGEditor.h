//---------------------------------------------------------------------------

#ifndef RTTIInspectorDemoVGEditorH
#define RTTIInspectorDemoVGEditorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxVGrid.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TcxVerticalGridEditor : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TcxButton *btCategory;
  TcxButton *btEditor;
  TcxButton *btClose;
  TcxButton *btMultiEditor;
  TcxButton *btDelete;
  TcxButton *btClear;
  TcxButton *ShowVerticalGridLayoutEditor;
  TPanel *Panel2;
  TListBox *lbRows;
  TPopupMenu *PopupMenu;
  TMenuItem *miEditor;
  TMenuItem *miCategory;
  TMenuItem *miMultieditor;
  TMenuItem *N1;
  TMenuItem *miDelete;
  TMenuItem *miClearAll;
  void __fastcall btCloseClick(TObject *Sender);
  void __fastcall lbRowsClick(TObject *Sender);
  void __fastcall btCategoryClick(TObject *Sender);
  void __fastcall btEditorClick(TObject *Sender);
  void __fastcall btMultiEditorClick(TObject *Sender);
  void __fastcall btDeleteClick(TObject *Sender);
  void __fastcall btClearClick(TObject *Sender);
  void __fastcall FormActivate(TObject *Sender);
  void __fastcall miEditorClick(TObject *Sender);
  void __fastcall miCategoryClick(TObject *Sender);
  void __fastcall miMultieditorClick(TObject *Sender);
  void __fastcall miDeleteClick(TObject *Sender);
  void __fastcall miClearAllClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall ShowVerticalGridLayoutEditorClick(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:
  TcxCustomVerticalGrid *FVerticalGrid;
  TNotifyEvent FOnObjectSelected;
  void __fastcall AddRow(TMetaClass* ARowClass);
  TcxCustomVerticalGrid* __fastcall GetVerticalGrid(void);
  void __fastcall UpdateButtons();
  void __fastcall UpdateItems();
  void __fastcall SetVerticalGrid(TcxCustomVerticalGrid *Value);
  void __fastcall SelectItem(void *AItem);
protected:
  virtual void __fastcall DoObjectSelected(TObject *Sender);
public:
  __fastcall TcxVerticalGridEditor(TComponent* Owner);
  void __fastcall DoItemsModified();
  void __fastcall InitFormEditor();
  __property TNotifyEvent OnObjectSelected = {read=FOnObjectSelected, write=FOnObjectSelected};
  __property TcxCustomVerticalGrid *VerticalGrid = {read=GetVerticalGrid, write=SetVerticalGrid};
};
//---------------------------------------------------------------------------

TcxVerticalGridEditor *GetVerticalGridEditor(TcxCustomVerticalGrid *AVerticalGrid,
  TNotifyEvent AEvent);
#endif
