//---------------------------------------------------------------------------


#ifndef SampleDockingRadioGroupH
#define SampleDockingRadioGroupH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TSampleDockingRadioGroupFrame : public TForm
{
__published:	// IDE-managed Components
  TRadioGroup *RadioGroup1;
private:	// User declarations
public:		// User declarations
  __fastcall TSampleDockingRadioGroupFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSampleDockingRadioGroupFrame *SampleDockingRadioGroupFrame;
//---------------------------------------------------------------------------
#endif
