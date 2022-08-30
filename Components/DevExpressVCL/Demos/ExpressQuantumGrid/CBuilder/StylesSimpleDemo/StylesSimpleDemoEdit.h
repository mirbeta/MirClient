//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoEditH
#define StylesSimpleDemoEditH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtonEdit.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxStyles.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
//---------------------------------------------------------------------------
class TStylesSimpleDemoEditForm : public TForm
{
__published:	// IDE-managed Components
  TButton *btnOK;
  TButton *nbtCancel;
  TGroupBox *DesignGroupBox;
  TLabel *lbColor;
  TLabel *lbTextColor;
  TLabel *lbFont;
  TLabel *lbColorValue;
  TLabel *lbTextColorValue;
  TLabel *lbBitmap;
  TImage *imgExample;
  TcxButtonEdit *btnedFont;
  TcxButtonEdit *btnedTextColor;
  TcxButtonEdit *btnedColor;
  TcxButtonEdit *btnedBitmap;
  TColorDialog *ColorDialog;
  TFontDialog *FontDialog;
  TOpenPictureDialog *OpenPictureDialog;
  void __fastcall btnedTextColorKeyPress(TObject *Sender, Char &Key);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall nbtCancelClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall btnedColorPropertiesButtonClick(TObject *Sender,
      int AButtonIndex);
  void __fastcall btnedFontPropertiesButtonClick(TObject *Sender,
      int AButtonIndex);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall btnedBitmapPropertiesButtonClick(TObject *Sender,
          int AButtonIndex);
private:	// User declarations
  TColor HoldColor;
  TColor HoldTextColor;
  TFont *HoldFont;
  TcxStyle *FStyle;
  void SaveStyleParams();
  void RestoreStyleParams();
  void RefreshStyleInfo();
public:		// User declarations
  __fastcall TStylesSimpleDemoEditForm(TComponent* Owner);
  __property TcxStyle *CurrentStyle = {read=FStyle, write=FStyle};
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoEditForm *StylesSimpleDemoEditForm;
//---------------------------------------------------------------------------
#endif
