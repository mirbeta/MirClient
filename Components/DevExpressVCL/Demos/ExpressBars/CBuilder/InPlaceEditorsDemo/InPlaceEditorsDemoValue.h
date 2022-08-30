//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoValueH
#define InPlaceEditorsDemoValueH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "InPlaceEditorsDemoFrameManager.h"
//---------------------------------------------------------------------------
class TfrmValueEditors : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TPaintBox *PaintBox1;
  void __fastcall PaintBox1Paint(TObject *Sender);
private:
  String FDate;
  String FTime;
  String FValue;
  String FFontName;
  int FFontSize;
public:
  void __fastcall SetParameters(int AFontSize, AnsiString ADate, AnsiString ATime, Currency AMoney);
  __fastcall TfrmValueEditors(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmValueEditors *frmValueEditors;
//---------------------------------------------------------------------------
#endif
