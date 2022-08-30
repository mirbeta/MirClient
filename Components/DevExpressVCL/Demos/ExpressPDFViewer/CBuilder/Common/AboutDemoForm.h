//---------------------------------------------------------------------------

#ifndef AboutDemoFormH
#define AboutDemoFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include "cxControls.hpp"
//---------------------------------------------------------------------------
class TformAboutDemo : public TForm
{
__published:	// IDE-managed Components
        TRichEdit *redDescription;
private:	// User declarations
		void __fastcall AssignBounds();
public:		// User declarations
		__fastcall TformAboutDemo(TComponent* Owner);
};

void ShowAboutDemoForm();
//---------------------------------------------------------------------------
#endif
