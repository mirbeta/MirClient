//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoCheckBoxesH
#define InPlaceEditorsDemoCheckBoxesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "InPlaceEditorsDemoFrameManager.h"
//---------------------------------------------------------------------------
class TfrmCheckBoxes : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TPanel *Panel2;
  TGroupBox *GroupBox1;
  TShape *Shape1;
  TShape *Shape2;
  TShape *Shape3;
  TGroupBox *GroupBox2;
  TShape *Shape4;
  TShape *Shape5;
  TShape *Shape6;
  void __fastcall Panel1Resize(TObject *Sender);
private:
  void __fastcall SetColor(TShape* AShape, bool ASelected, TColor AColor);
  void __fastcall SetColors(TColor AColors[], AnsiString AMultiSelectValue, AnsiString ASelectValue);
public:
  void __fastcall SetParameters(AnsiString ASelectValue, AnsiString AMultiSelectValue, bool ABlackAndWhite);
  __fastcall TfrmCheckBoxes(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCheckBoxes *frmCheckBoxes;
//---------------------------------------------------------------------------
#endif
