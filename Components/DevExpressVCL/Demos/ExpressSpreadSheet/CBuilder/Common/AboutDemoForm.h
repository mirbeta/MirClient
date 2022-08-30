//---------------------------------------------------------------------------

#ifndef AboutDemoFormH
#define AboutDemoFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxGeometry.hpp"
//---------------------------------------------------------------------------
class TformAboutDemo : public TForm
{
__published:	// IDE-managed Components
	TcxRichEdit *redDescription;
private:	// User declarations
		void __fastcall AssignBounds();
public:		// User declarations
		__fastcall TformAboutDemo(TComponent* Owner);
		__fastcall TformAboutDemo(const String ADescription);
};

void ShowAboutDemoForm();
void ShowAboutDemoFormEx(const TRect ABounds);
//---------------------------------------------------------------------------
extern PACKAGE TformAboutDemo *formAboutDemo;
//---------------------------------------------------------------------------
#endif
