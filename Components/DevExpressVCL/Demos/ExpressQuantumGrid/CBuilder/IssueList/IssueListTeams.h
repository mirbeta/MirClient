//---------------------------------------------------------------------------

#ifndef IssueListTeamsH
#define IssueListTeamsH
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
#include "cxDBLookupComboBox.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBLookupEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxNavigator.hpp"
//---------------------------------------------------------------------------
class TfrmTeams : public TfrmBasic
{
__published:	// IDE-managed Components
	TcxLabel *Label5;
  TcxDBLookupComboBox *cxDBLookupComboBox3;
	TcxLabel *Label1;
  TcxDBLookupComboBox *cxDBLookupComboBox1;
	TcxLabel *Label9;
  TcxDBTextEdit *cxDBTextEdit1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmTeams(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmTeams *frmTeams;
//---------------------------------------------------------------------------
#endif
