//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoComboBoxesH
#define InPlaceEditorsDemoComboBoxesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "InPlaceEditorsDemoFrameManager.h"

//---------------------------------------------------------------------------
class TfrmComboBoxes : public TEditorDemoBaseFrame
{
__published:	// IDE-managed Components
  TPaintBox *PaintBox1;
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall PaintBox1Paint(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:
  AnsiString FPath;
  AnsiString FPerson;
  AnsiString FFontName;
  int FFontSize;
  Graphics::TColor FColor;
  Graphics::TBitmap* FImage;

public:
  void __fastcall SetParameters(TColor AColor, AnsiString AFontName, AnsiString AFolder,
  int AFontSize, Graphics::TBitmap* AImage, String APerson);
  __fastcall TfrmComboBoxes(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmComboBoxes *frmComboBoxes;
//---------------------------------------------------------------------------
#endif
