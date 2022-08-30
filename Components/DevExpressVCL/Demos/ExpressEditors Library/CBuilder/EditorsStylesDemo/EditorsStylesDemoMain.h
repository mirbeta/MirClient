//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoMainH
#define EditorsStylesDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxGroupBox.hpp"
#include "cxHint.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxSpinEdit.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "cxTreeView.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "EditorsStylesDemoBase.h"
#include "cxPC.hpp"
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxGraphics.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
#include "dxCustomHint.hpp"
//---------------------------------------------------------------------------
class TEditorsStylesDemoDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TcxGroupBox *cxGroupBox;
  TcxGroupBox *cxGroupBox1;
  TcxTreeView *cxTreeView;
  TcxPageControl *PageControl;
  TcxGroupBox *gbDescription;
  TcxSplitter *cxSplitter1;
  TcxSplitter *cxSplitter;
  TImageList *ilMain;
  TOpenDialog *OpenDialog;
  TcxHintStyleController *cxHintStyleController;
  TMenuItem *miFileOpen;
  TMenuItem *miFileSave;
  TMenuItem *N1;
  TMenuItem *miStyle;
  TMenuItem *miStyleCurrentPage;
  TMenuItem *miCurDisplayStyle;
  TMenuItem *miCurDefLightBlue;
  TMenuItem *miCurDefLightGray;
  TMenuItem *miCurDefWood;
  TMenuItem *miCurDefRainyDay;
  TMenuItem *miCurDefBrick;
  TMenuItem *miCurDefDeepSea;
  TMenuItem *miCurHintStyle;
  TMenuItem *miCurHintStyleLightInfo;
  TMenuItem *miCurHintStyleLightSlideLeft;
  TMenuItem *miCurHintStyleBlueSlideUp;
  TMenuItem *miCurHintStyleRoundedInfo;
  TMenuItem *miCurHintStyleStandard;
  TMenuItem *N3;
  TMenuItem *nohints1;
  TMenuItem *miStyleAllPages;
  TMenuItem *miDefDisplayStyle;
  TMenuItem *miDefLightBlue;
  TMenuItem *miDefLightGray;
  TMenuItem *miDefWood;
  TMenuItem *miDefRainyDay;
  TMenuItem *miDefBrick;
  TMenuItem *miDefDeepSea;
  TMenuItem *miDefHintStyle;
  TMenuItem *miHintStyleLightInfo;
  TMenuItem *miHintStyleLightSlideLeft;
  TMenuItem *miHintStyleBlueSlideUp;
  TMenuItem *miHintStyleRoundedInfo;
  TMenuItem *miHintStyleStandard;
  TMenuItem *N4;
  TMenuItem *nohints2;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall cxTreeViewChange(TObject *Sender, TTreeNode *Node);
  void __fastcall miCurDisplayStyleClick(TObject *Sender);
  void __fastcall miDefaultDisplayStyleClick(TObject *Sender);
  void __fastcall miCurHintStyleClick(TObject *Sender);
  void __fastcall miHintStyleClick(TObject *Sender);
  void __fastcall miFileOpenClick(TObject *Sender);
  void __fastcall miFileSaveClick(TObject *Sender);
  void __fastcall PageControlChange(TObject *Sender);
private:
  void __fastcall UncheckMenuItems(TMenuItem* AItems);
  bool FDefaultHintStyle;
  bool FDefaultDisplayStyle;
  TcxStyleSheetType FCurrentDisplayStyle;
  void __fastcall AdjustDescriptionsPageControl();
  void __fastcall FillTreeViewItems();
  void __fastcall AddChildNodes(TcxExtEditorTypes AExtEditorTypes, TStrings* AStrings, TTreeNode* ANode);
  void __fastcall RegisterFrames();
  void __fastcall AdjustNotePane(TEditorsStylesDemoBaseFrame* AFrame);
  void __fastcall AdjustFileNameStatusPanel();
  void __fastcall AdjustFrameDisplayStyle(TEditorsStylesDemoBaseFrame* AFrame, bool ADefaultDisplayStyle);
  void __fastcall UpdateCurrentHintStyle(TcxHintType AHintType, bool ADefaultHintStyle);
  void __fastcall AdjustFileMenu();
  void __fastcall SetHintStyle(TcxHintType AHintType);
  void __fastcall SetLightInfoHintStyle();
  void __fastcall SetLightSlideLeftHintStyle();
  void __fastcall SetBlueSlideUpHintStyle();
  void __fastcall SetRoundedInfoHintStyle();
  void __fastcall SetStandardHintStyle();
  void __fastcall SetNoHintStyle();
  void __fastcall UpdateFileNameStatusPanel(String AFileName);
  TcxHintStyle* __fastcall GetAdvancedHintStyle();
public:		// User declarations
  __fastcall TEditorsStylesDemoDemoMainForm(TComponent* Owner);
  __property bool DefaultHintStyle = {read=FDefaultHintStyle, write=FDefaultHintStyle};
  __property bool DefaultDisplayStyle = {read=FDefaultDisplayStyle, write=FDefaultDisplayStyle};
  __property TcxHintStyle* AdvancedHintStyle = {read=GetAdvancedHintStyle};
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoDemoMainForm *EditorsStylesDemoDemoMainForm;
//---------------------------------------------------------------------------
#endif
