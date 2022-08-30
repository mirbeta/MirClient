//---------------------------------------------------------------------------

#ifndef ImageViewerDemoResizeImageH
#define ImageViewerDemoResizeImageH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxCheckGroupBox.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TImageViewerDemoResizeImageForm : public TForm
{
__published:	// IDE-managed Components
	TcxLabel *cxLabel1;
	TcxSpinEdit *seScale;
	TdxCheckGroupBox *cgbPixels;
	TcxLabel *cxLabel2;
	TcxLabel *cxLabel3;
	TcxTextEdit *teWidth;
	TcxTextEdit *teHeight;
	TcxButton *btnApply;
	TcxButton *cxButton1;
	TcxCheckBox *cbAspectRatio;
	void __fastcall cgbPixelsPropertiesChange(TObject *Sender);
	void __fastcall teHorizontalPropertiesValidate(TObject *Sender,
	  Variant &DisplayValue, TCaption &ErrorText, Boolean &Error);
	void __fastcall teWidthPropertiesChange(TObject *Sender);
	void __fastcall teHeightPropertiesChange(TObject *Sender);
	void __fastcall cbAspectRatioPropertiesChange(TObject *Sender);
private:	// User declarations
	Integer FGlyphHeight;
	Integer FGlyphWidth;
	Integer FLockCount;

	void SetGlyphHeight(Integer Value);
	void SetGlyphWidth(Integer Value);

	void CalculateHeight();
	void CalculateWidth();
public:		// User declarations
	__fastcall TImageViewerDemoResizeImageForm(TComponent* Owner);

	__property Integer GlyphHeight = {read=FGlyphHeight, write=SetGlyphHeight};
	__property Integer GlyphWidth = {read=FGlyphWidth, write=SetGlyphWidth};
};
//---------------------------------------------------------------------------
extern PACKAGE TImageViewerDemoResizeImageForm *ImageViewerDemoResizeImageForm;
//---------------------------------------------------------------------------
#endif
