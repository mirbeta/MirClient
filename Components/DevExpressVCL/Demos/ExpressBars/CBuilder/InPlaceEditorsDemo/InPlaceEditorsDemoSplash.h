//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoSplashH
#define InPlaceEditorsDemoSplashH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmLoading : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
private:	// User declarations
public:		// User declarations
  __fastcall TfrmLoading(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmLoading *frmLoading;
//---------------------------------------------------------------------------
#endif
