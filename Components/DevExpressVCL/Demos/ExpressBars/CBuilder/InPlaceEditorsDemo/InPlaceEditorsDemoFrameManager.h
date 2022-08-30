//---------------------------------------------------------------------------

#ifndef InPlaceEditorsDemoFrameManagerH
#define InPlaceEditorsDemoFrameManagerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TEditorDemoBaseFrame : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblFrameDescription;
public:
  __fastcall TEditorDemoBaseFrame(TComponent* Owner);
  void __fastcall DrawText(AnsiString AStrings[], TPaintBox* APaintBox, int AStringsCount);
  void __fastcall SetDescription(AnsiString ADescription);
  bool __fastcall GetDescriptionVisible();
  void __fastcall SetDescriptionVisible(bool AValue);
  __property bool DescriptionVisible = {read = GetDescriptionVisible, write = SetDescriptionVisible};
};

class TEditorDemoFrameManager: public TObject
{
private:
  TList *FFrameList;
  TEditorDemoBaseFrame* __fastcall GetFrame(int AIndex);
  int __fastcall GetFramesCount();
public:
  virtual __fastcall TEditorDemoFrameManager();
  __fastcall ~TEditorDemoFrameManager();
  void __fastcall UpdateFrameColors(TColor AColor);
  void __fastcall AddFrame(TForm* AFrame, int AFrameId);
  void __fastcall SetDescriptionsVisible(bool AValue);

  __property int FramesCount = {read = GetFramesCount};
  __property TEditorDemoBaseFrame* Frames[int AIndex] = {read = GetFrame};
};

//---------------------------------------------------------------------------
extern PACKAGE TEditorDemoBaseFrame *EditorDemoBaseFrame;

TEditorDemoFrameManager* EditorDemoFrameManager();
//---------------------------------------------------------------------------
#endif
