//---------------------------------------------------------------------------

#ifndef IssueListItemsH
#define IssueListItemsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "IssueListForm.h"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include "cxCalendar.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDBEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxTextEdit.hpp"
#include <ComCtrls.hpp>
#include "cxDBLookupComboBox.hpp"
#include "cxDBLookupEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
#include "cxPC.hpp"
//---------------------------------------------------------------------------
class TfrmItems : public TfrmBasic
{
__published:	// IDE-managed Components
	TcxPageControl *PageControl;
	TcxTabSheet *tsGeneral;
	TcxLabel *Label2;
	TcxLabel *Label1;
	TcxLabel *Label5;
	TcxLabel *Label4;
	TcxLabel *Label10;
	TcxLabel *Label8;
	TcxLabel *Label6;
	TcxLabel *Label3;
	TcxLabel *Label7;
	TcxLabel *Label11;
  TcxDBTextEdit *cxDBTextEdit1;
  TcxDBLookupComboBox *cxDBLookupComboBox3;
  TcxDBImageComboBox *cxDBImageComboBox3;
  TcxDBImageComboBox *cxDBImageComboBox2;
  TcxDBImageComboBox *cxDBImageComboBox1;
  TcxDBLookupComboBox *cxDBLookupComboBox1;
  TcxDBDateEdit *cxDBDateEdit3;
  TcxDBLookupComboBox *cxDBLookupComboBox2;
  TcxDBDateEdit *cxDBDateEdit2;
  TcxDBDateEdit *cxDBDateEdit1;
	TcxTabSheet *tsDescription;
  TcxDBMemo *cxDBMemo1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmItems(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmItems *frmItems;
//---------------------------------------------------------------------------
#endif
