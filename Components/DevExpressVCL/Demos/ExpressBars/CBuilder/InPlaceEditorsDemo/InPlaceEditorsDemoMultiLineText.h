//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoMultiLineTextH
#define InPlaceEditorsDemoMultiLineTextH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "InPlaceEditorsDemoFrameManager.h"
//---------------------------------------------------------------------------
class TfrmMultiLineTextEditors : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TRichEdit *RichEdit1;
  TMemo *Memo1;
  TSplitter *Splitter1;
  void __fastcall FormCreate(TObject *Sender);
private:
  void __fastcall LoadText(TStrings* AStrings, TStream* AStream);
public:
  void __fastcall SetParameters(TStringStream* ARichTextStream, TStringStream* APlainTextStream);
  __fastcall TfrmMultiLineTextEditors(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMultiLineTextEditors *frmMultiLineTextEditors;
//---------------------------------------------------------------------------
#endif
