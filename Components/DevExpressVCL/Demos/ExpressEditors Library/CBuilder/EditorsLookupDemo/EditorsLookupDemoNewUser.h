//---------------------------------------------------------------------------

#ifndef EditorsLookupDemoNewUserH
#define EditorsLookupDemoNewUserH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDBEdit.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookupEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxDBLookupEdit.hpp"
//---------------------------------------------------------------------------
class TEditorsLookupDemoNewUserForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TLabel *Label8;
  TLabel *Label9;
  TLabel *Label10;
  TLabel *Label11;
  TLabel *Label12;
  TcxDBTextEdit *edFirstName;
  TcxDBTextEdit *edMidleName;
  TcxDBTextEdit *edLastName;
  TcxDBTextEdit *edCountry;
  TcxDBTextEdit *edCity;
  TcxDBMaskEdit *mePostalCode;
  TcxDBTextEdit *edAddress;
  TcxDBMaskEdit *mePhone;
  TcxDBMaskEdit *meFax;
  TcxDBHyperLinkEdit *heEMail;
  TcxDBHyperLinkEdit *heHomePAge;
  TcxDBLookupComboBox *lcbDepartment;
  TcxButton *btnOK;
  TcxButton *btnCancel;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
  void DeleteSpaces(String &AStr);
public:		// User declarations
  __fastcall TEditorsLookupDemoNewUserForm(TComponent* Owner);
  int ShowEx(String AName);
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsLookupDemoNewUserForm *EditorsLookupDemoNewUserForm;
//---------------------------------------------------------------------------
#endif
 