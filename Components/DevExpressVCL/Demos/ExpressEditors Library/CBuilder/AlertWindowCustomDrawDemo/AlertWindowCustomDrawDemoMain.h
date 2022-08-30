//---------------------------------------------------------------------------

#ifndef AlertWindowCustomDrawDemoMainH
#define AlertWindowCustomDrawDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Math.hpp>
#include <sysdefs.h>
#include "cxButtons.hpp"
#include "cxColorComboBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxAlertWindow.hpp"
#include "dxBevel.hpp"
#include "dxDrawRichTextUtils.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "BaseForm.h"
//---------------------------------------------------------------------------
const AnsiString RTFFilter = "Rich Text Files (*.RTF)|*.RTF";
const AnsiString DefaultFileName = "Document1.rtf";

class TfmAlertWindowCustomDraw : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxBevel *bvlLeft;
	TdxBevel *bvlTop;
	TdxBevel *bvlRight;
	TPanel *Panel1;
	TcxRichEdit *redtMessageText;
	TToolBar *tlbTextAttribute;
	TToolButton *btnOpen;
	TToolButton *btnSave;
	TToolButton *btnSeparator1;
	TToolButton *btnAlignLeft;
	TToolButton *btnAlignCenter;
	TToolButton *btnAlignRight;
	TToolButton *btnSeparator2;
	TToolButton *btnFont;
	TcxButton *btnShow;
	TToolBar *tlbFont;
	TToolButton *btnBold;
	TToolButton *btnItalic;
	TToolButton *btnUnderline;
	TcxComboBox *cbTextSize;
	TcxColorComboBox *ccbTextColor;
	TcxFontNameComboBox *cbbFontName;
	TFontDialog *fdMessageText;
	TdxAlertWindowManager *awmCustomDrawDemo1;
	TcxImageList *imglMessages;
	TImageList *ilHotImages;
	void __fastcall Open1Click(TObject *Sender);
	void __fastcall Save1Click(TObject *Sender);
	void __fastcall btnAlignLeftClick(TObject *Sender);
	void __fastcall btnAlignCenterClick(TObject *Sender);
	void __fastcall btnAlignRightClick(TObject *Sender);
	void __fastcall btnFontClick(TObject *Sender);
	void __fastcall btnBoldClick(TObject *Sender);
	void __fastcall btnItalicClick(TObject *Sender);
	void __fastcall btnUnderlineClick(TObject *Sender);
	void __fastcall cbTextSizePropertiesChange(TObject *Sender);
	void __fastcall ccbTextColorPropertiesChange(TObject *Sender);
	void __fastcall cbbFontNamePropertiesChange(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall btnShowClick(TObject *Sender);
	void __fastcall fdMessageTextApply(TObject *Sender, HWND Wnd);
	void __fastcall awmCustomDrawDemo1CustomDrawMessageText(TObject *Sender, TdxAlertWindow *AAlertWindow,
          TcxCanvas *ACanvas, TdxAlertWindowMessageTextViewInfo *AViewInfo,
          bool &ADone);
	void __fastcall awmCustomDrawDemo1MeasureMessageText(TObject *Sender, TdxAlertWindow *AAlertWindow,
          int &AWidth, int &AHeight);
	void __fastcall cbbFontNamePropertiesFontPreviewButtonClick(TObject *Sender, TcxFontButtonType ButtonType);
	void __fastcall awmCustomDrawDemo1BeforeShow(TObject *Sender, TdxAlertWindow *AAlertWindow);
	void __fastcall redtMessageTextPropertiesChange(TObject *Sender);
	void __fastcall redtMessageTextPropertiesSelectionChange(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);


private:	// User declarations
	void CanvasCopyRect(TCanvas *ASourceCanvas, TCanvas *ADestCanvas, TRect ASourceRect, TRect ADestRect, int ACopyMode);
	void DrawTransparentRichEdit(TcxCanvas *ACanvas, TRect ARect, TcxRichEdit *ARichEdit, int AMinCharIndex, int AMaxCharIndex);
	void GetRichEditVisibleRange(HWND ARichHandle, int *AMinCharIndex, int *AMaxCharIndex);
	void GetSelAttributes();
	void RedrawAlertWindows();
	void SetSelAttributes();
public:		// User declarations
	__fastcall TfmAlertWindowCustomDraw(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmAlertWindowCustomDraw *fmAlertWindowCustomDraw;
//---------------------------------------------------------------------------
#endif
