//---------------------------------------------------------------------------


#ifndef SampleDockingRichTextH
#define SampleDockingRichTextH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TSampleDockingRichTextFrame : public TForm
{
__published:	// IDE-managed Components
  TRichEdit *RichEdit1;
private:	// User declarations
public:		// User declarations
  __fastcall TSampleDockingRichTextFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSampleDockingRichTextFrame *SampleDockingRichTextFrame;
//---------------------------------------------------------------------------
#endif
