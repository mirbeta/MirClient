//---------------------------------------------------------------------------

#ifndef GridModeDemoTerminateH
#define GridModeDemoTerminateH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
const String strDeleting = "Deleting previously inserted records...";
const String strInserting = "Inserting records...";
const String strLoadData = "Loading data ...";

class TGridModeDemoTerminateForm : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TLabel *lbDesc;
  TLabel *Label2;
private:	// User declarations
public:		// User declarations
  __fastcall TGridModeDemoTerminateForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGridModeDemoTerminateForm *GridModeDemoTerminateForm;
//---------------------------------------------------------------------------
#endif
