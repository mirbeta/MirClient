//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoTextH
#define InPlaceEditorsDemoTextH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "InPlaceEditorsDemoFrameManager.h"
//---------------------------------------------------------------------------
class TfrmTextEditors : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TPaintBox *PaintBox1;
  void __fastcall PaintBox1Paint(TObject *Sender);
private:
  AnsiString FCompany;
  AnsiString FWWW;
  AnsiString FPhone;
public:
  void __fastcall SetParameters(AnsiString ACompany, AnsiString AHyperLink, AnsiString APhone);
   __fastcall TfrmTextEditors(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmTextEditors *frmTextEditors;
//---------------------------------------------------------------------------
#endif
