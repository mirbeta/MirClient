//---------------------------------------------------------------------------

#ifndef StylesSimpleDemoEditH
#define StylesSimpleDemoEditH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtonEdit.hpp"
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
//---------------------------------------------------------------------------
class TStylesSimpleDemoEditForm : public TForm
{
__published:	// IDE-managed Components
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
  TcxButton *btnOK1;
  TcxButton *nbtCancel1;
  TColorDialog *ColorDialog;
  TFontDialog *FontDialog;
  TOpenPictureDialog *OpenPictureDialog;
  void __fastcall  btnedTextColorKeyPress(TObject *Sender, char &Key);
  void __fastcall  FormShow(TObject *Sender);
  void __fastcall  nbtCancelClick(TObject *Sender);
  void __fastcall  FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall  btnedColorPropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
  void __fastcall  btnedFontPropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
  void __fastcall  FormCreate(TObject *Sender);
  void __fastcall  FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall  btnedBitmapPropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
private:	// User declarations
    TColor HoldColor;
    TColor HoldTextColor;
    TFont *HoldFont;
    Graphics::TBitmap *HoldBitmap;
    Graphics::TBitmap *FSampleBitmap;
    TcxStyle *FStyle;
  void __fastcall  SaveStyleParams();
  void __fastcall  RestoreStyleParams();
  void __fastcall  RefreshStyleInfo();
public:		// User declarations
  __fastcall TStylesSimpleDemoEditForm(TComponent* Owner);
  __property TcxStyle *CurrentStyle = {read=FStyle, write=FStyle};
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesSimpleDemoEditForm *StylesSimpleDemoEditForm;
//---------------------------------------------------------------------------
#endif

bool ChangeStyle(TcxStyle *AStyle);
