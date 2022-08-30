//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoCarsH
#define EditorsInPlaceDemoCarsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxCalc.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDBEdit.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
#include "cxListBox.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "CarsDataForGrid.h" 
#include "cxLookupEdit.hpp"
#include "cxDBLookupEdit.hpp"
#include "cxDBLookupComboBox.hpp"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoCarsForm : public TForm
{
__published:	// IDE-managed Components
  TcxGroupBox *pnlCarInfo;
  TcxLabel *Label6;
  TcxDBMemo *cxDBMemo1;
  TcxGroupBox *Panel1;
  TcxLabel *Label3;
  TcxLabel *Label4;
  TcxDBTextEdit *cxDBTextEdit1;
  TcxDBTextEdit *cxDBTextEdit2;
  TcxGroupBox *GroupBox1;
  TcxDBTextEdit *cxDBTextEdit5;
  TcxDBTextEdit *cxDBTextEdit4;
  TcxDBTextEdit *cxDBTextEdit8;
  TcxGroupBox *GroupBox2;
  TcxDBCheckBox *cxDBCheckBox;
  TcxDBListBox *cxDBListBox1;
  TcxGroupBox *GroupBox3;
  TcxDBTextEdit *cxDBTextEdit7;
  TcxDBTextEdit *cxDBTextEdit6;
private:	// User declarations
public:		// User declarations
  __fastcall TEditorsInPlaceDemoCarsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoCarsForm *EditorsInPlaceDemoCarsForm;
//---------------------------------------------------------------------------
#endif
 