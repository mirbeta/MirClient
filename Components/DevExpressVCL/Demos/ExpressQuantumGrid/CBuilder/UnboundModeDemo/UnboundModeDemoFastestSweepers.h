//---------------------------------------------------------------------------

#ifndef UnboundModeDemoFastestSweepersH
#define UnboundModeDemoFastestSweepersH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TUnboundModeDemoFastestSweepersForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbBeginner;
  TLabel *lbIntermediate;
  TLabel *lbExpert;
  TLabel *lbExpertTime;
  TLabel *lbIntermediateTime;
  TLabel *lbBeginnerTime;
  TLabel *ibExpertName;
  TLabel *lbIntermediateName;
  TLabel *lbBeginnerName;
  TcxButton *bntOK;
  TcxButton *btnResetScores;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall btnResetScoresClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TUnboundModeDemoFastestSweepersForm(TComponent* Owner);
    bool FastestTimesResetted;
    int __fastcall ShowModal();
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundModeDemoFastestSweepersForm *UnboundModeDemoFastestSweepersForm;
//---------------------------------------------------------------------------
#endif
