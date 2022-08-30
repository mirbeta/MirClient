//---------------------------------------------------------------------------

#ifndef IssueListDepartmentsH
#define IssueListDepartmentsH
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
class TfrmDepartments : public TfrmBasic
{
__published:	// IDE-managed Components
  TcxDBTextEdit *cxDBTextEdit1;
	TcxLabel *Label1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmDepartments(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmDepartments *frmDepartments;
//---------------------------------------------------------------------------
#endif
