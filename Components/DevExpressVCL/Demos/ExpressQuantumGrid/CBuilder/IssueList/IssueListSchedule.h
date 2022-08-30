//---------------------------------------------------------------------------

#ifndef IssueListScheduleH
#define IssueListScheduleH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "IssueListForm.h"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDBEdit.hpp"
#include "cxEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
//---------------------------------------------------------------------------
class TfrmSchedule : public TfrmBasic
{
__published:	// IDE-managed Components
	TcxLabel *Label5;
  TcxDBTextEdit *cxDBTextEdit1;
	TcxLabel *Label9;
  TcxDBTextEdit *cxDBTextEdit2;
	TcxLabel *Label2;
  TcxDBTextEdit *cxDBTextEdit3;
	TcxLabel *Label3;
  TcxDBTextEdit *cxDBTextEdit4;
	TcxLabel *Label6;
  TcxDBTextEdit *cxDBTextEdit5;
	TcxLabel *Label7;
  TcxDBTextEdit *cxDBTextEdit6;
	TcxLabel *Label8;
  TcxDBTextEdit *cxDBTextEdit7;
	TcxLabel *Label1;
  TcxDBTextEdit *cxDBTextEdit8;
	TcxLabel *Label4;
  TcxDBTextEdit *cxDBTextEdit9;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmSchedule(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSchedule *frmSchedule;
//---------------------------------------------------------------------------
#endif
