//---------------------------------------------------------------------------

#ifndef ColumnsShareDemoLookupCustomizeH
#define ColumnsShareDemoLookupCustomizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include <ComCtrls.hpp>
#include "cxListBox.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeels.hpp"
#include "cxPC.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TColumnsShareDemoLookupCustomizeForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDescr;
  TcxButton *btnClose;
	TcxPageControl *PageControl1;
	TcxTabSheet *tsLookupListFields;
	TcxLabel *lbDescription;
	TcxLabel *Label3;
	TcxLabel *Label1;
  TcxButton *btnAdd;
  TcxButton *btnDelete;
  TcxListBox *lbUnlinkedColumns;
  TcxListBox *lbListColumns;
	TcxTabSheet *tsLookupProperties;
	TcxLabel *Label5;
	TcxLabel *Label4;
	TcxLabel *Label2;
  TcxCheckBox *chbHeaders;
  TcxCheckBox *chbIncrementalFilltering;
  TcxCheckBox *chbImmediateDropDown;
  TcxCheckBox *chbDropDownAutoSize;
  TcxSpinEdit *seListFieldIndex;
  TcxSpinEdit *seDropDownRows;
  TcxComboBox *cbDropDownListStyle;
  void __fastcall btnAddClick(TObject *Sender);
  void __fastcall btnDeleteClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnCloseClick(TObject *Sender);
  void __fastcall lbUnlinkedColumnsDblClick(TObject *Sender);
  void __fastcall lbUnlinkedColumnsKeyPress(TObject *Sender, char &Key);
  void __fastcall lbListColumnsKeyPress(TObject *Sender, char &Key);
  void __fastcall chbDropDownAutoSizeClick(TObject *Sender);
  void __fastcall chbImmediateDropDownClick(TObject *Sender);
  void __fastcall chbIncrementalFillteringClick(TObject *Sender);
  void __fastcall chbHeadersClick(TObject *Sender);
  void __fastcall cbDropDownListStylePropertiesChange(TObject *Sender);
  void __fastcall seDropDownRowsPropertiesChange(TObject *Sender);
  void __fastcall seListFieldIndexPropertiesChange(TObject *Sender);
private:	// User declarations
  TcxLookupComboBoxProperties* FEditProperty;
  void __fastcall CancelEditing();
  void __fastcall CustomizeListBoxes();
  void __fastcall CustomizeParams();
  void __fastcall lbMoveItem(TcxListBox* ASourceListBox, TcxListBox* ADestinationListBox);
  void __fastcall lbDeleteSelection(TcxListBox* AListBox);
public:		// User declarations
  __fastcall TColumnsShareDemoLookupCustomizeForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TColumnsShareDemoLookupCustomizeForm *ColumnsShareDemoLookupCustomizeForm;
//---------------------------------------------------------------------------
#endif
