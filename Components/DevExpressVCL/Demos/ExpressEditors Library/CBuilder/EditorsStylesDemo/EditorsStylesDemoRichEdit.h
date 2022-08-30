//---------------------------------------------------------------------------

#ifndef EditorsStylesDemoRichEditH
#define EditorsStylesDemoRichEditH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxPropertiesStore.hpp"
#include "cxTextEdit.hpp"
#include "EditorsStylesDemoBase.h"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxButtons.hpp"
#include "cxColorComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinButton.hpp"
#include "cxSpinEdit.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include "cxRichEdit.hpp"
//---------------------------------------------------------------------------
class  TcxCustomColorComboBoxPropertiesAccess : public TcxCustomColorComboBoxProperties {
public:
  int __fastcall IndexByValue(const Variant &AValue) { return(TcxCustomColorComboBoxProperties::IndexByValue(AValue)); };
  AnsiString __fastcall GetDescriptionByIndex(int AIndex) { return(TcxCustomColorComboBoxProperties::GetDescriptionByIndex(AIndex)); };
  __property Items;
};

class TEditorsStylesDemoRichEditFrame : public TEditorsStylesDemoBaseFrame
{
__published:  // IDE-managed Components
  TCoolBar *ControlBar;
  TToolBar *ToolBar1;
  TcxButton *btnColorSchemes;
  TToolButton *ToolButton6;
  TcxColorComboBox *cxColorComboBox;
  TToolButton *ToolButton2;
  TcxFontNameComboBox *fcbFontName;
  TToolButton *ToolButton1;
  TcxMaskEdit *meFontSize;
  TcxSpinButton *cxSpinButton;
  TToolBar *StandardToolBar;
  TToolButton *ToolButton3;
  TToolButton *OpenButton;
  TToolButton *ToolButton4;
  TToolButton *PrintButton;
  TToolButton *ToolButton5;
  TToolButton *CutButton;
  TToolButton *CopyButton;
  TToolButton *PasteButton;
  TToolButton *ToolButton10;
  TToolButton *BoldButton;
  TToolButton *ItalicButton;
  TToolButton *UnderlineButton;
  TToolButton *ToolButton16;
  TToolButton *LeftAlign;
  TToolButton *CenterAlign;
  TToolButton *RightAlign;
  TToolButton *ToolButton20;
  TToolButton *BulletsButton;
  TcxRichEdit *RichEdit;
  TImageList *ToolbarImages;
  TActionList *ActionList;
  TAction *actEditCut;
  TAction *actEditCopy;
  TAction *actEditPaste;
  TAction *actOpen;
  TAction *actPrint;
  TAction *actSaveFile;
  TAction *actBold;
  TAction *actItalic;
  TAction *actUnderline;
  TAction *actAlignLeft;
  TAction *actAlignRight;
  TAction *actAlignCenter;
  TAction *actBullets;
  TAction *actNewFile;
  TOpenDialog *OpenDialog;
  TPrintDialog *PrintDialog;
  TSaveDialog *SaveDialog;
  TPopupMenu *pmColorSchemes;
  TMenuItem *miStandard;
  TMenuItem *miHTML;
  TMenuItem *miWeb;
  TMenuItem *miWebSorted;
  void __fastcall actEditCutExecute(TObject *Sender);
  void __fastcall actEditCopyExecute(TObject *Sender);
  void __fastcall actEditPasteExecute(TObject *Sender);
  void __fastcall actOpenExecute(TObject *Sender);
  void __fastcall actPrintExecute(TObject *Sender);
  void __fastcall actSaveFileExecute(TObject *Sender);
  void __fastcall actNewFileExecute(TObject *Sender);
  void __fastcall actEditCutUpdate(TObject *Sender);
  void __fastcall actEditCopyUpdate(TObject *Sender);
  void __fastcall actEditPasteUpdate(TObject *Sender);
  void __fastcall RichEditSelectionChange(TObject *Sender);
  void __fastcall ColorSchemeButtonClick(TObject *Sender);
  void __fastcall meFontSizePropertiesChange(TObject *Sender);
  void __fastcall fcbFontNamePropertiesChange(TObject *Sender);
  void __fastcall cxColorComboBoxPropertiesChange(TObject *Sender);
  void __fastcall actBoldExecute(TObject *Sender);
  void __fastcall actItalicExecute(TObject *Sender);
  void __fastcall actUnderlineExecute(TObject *Sender);
  void __fastcall actAlignLeftExecute(TObject *Sender);
  void __fastcall actAlignRightExecute(TObject *Sender);
  void __fastcall actAlignCenterExecute(TObject *Sender);
  void __fastcall actBulletsExecute(TObject *Sender);
  void __fastcall fcbFontNamePropertiesFontPreviewButtonClick(
          TObject *Sender, TcxFontButtonType ButtonType);
  void __fastcall fcbFontNamePropertiesInitPopup(TObject *Sender);
  void __fastcall actSaveFileUpdate(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall RichEditPropertiesChange(TObject *Sender);
private:  // User declarations
  bool FUpdating;
  bool FChanged;
  void __fastcall SetFileName(String AFileName);
  TTextAttributes* __fastcall CurrText();
public:
  __fastcall TEditorsStylesDemoRichEditFrame(TComponent* Owner);
  String __fastcall Name();
  String __fastcall BriefName();
  bool MenuOpenFileVisible();
  bool MenuSaveFileVisible();
  virtual void OpenFile(TObject *Sender);
  virtual void SaveFile(TObject *Sender);
  String StylesIniPath();
  TColor GetStyleBackgroundColor();
  String Description();
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsStylesDemoRichEditFrame *EditorsStylesDemoRichEditFrame;
//---------------------------------------------------------------------------
#endif
