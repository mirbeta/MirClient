//---------------------------------------------------------------------------


#ifndef SampleDockingListBoxH
#define SampleDockingListBoxH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
//---------------------------------------------------------------------------
class TSampleDockingListBoxFrame : public TForm
{
__published:	// IDE-managed Components
  TListBox *ListBox;
  TEdit *Edit;
  TButton *btnAdd;
  TButton *btnDelete;
  TButton *btnClear;
  TActionList *ActionList1;
  TAction *actAdd;
  TAction *actDelete;
  TAction *actClear;
  void __fastcall actAddExecute(TObject *Sender);
  void __fastcall actDeleteExecute(TObject *Sender);
  void __fastcall actClearExecute(TObject *Sender);
  void __fastcall actAddUpdate(TObject *Sender);
  void __fastcall actDeleteUpdate(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSampleDockingListBoxFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSampleDockingListBoxFrame *SampleDockingListBoxFrame;
//---------------------------------------------------------------------------
#endif
