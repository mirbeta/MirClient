//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoimageH
#define InPlaceEditorsDemoimageH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxImage.hpp"
#include "InPlaceEditorsDemoFrameManager.h"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TfrmImageEditors : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TcxImage *cxImage1;
private:	// User declarations
public:
  void __fastcall SetParameters(TStream* AStream);
  __fastcall TfrmImageEditors(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmImageEditors *frmImageEditors;
//---------------------------------------------------------------------------
#endif
