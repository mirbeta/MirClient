//---------------------------------------------------------------------------
#ifndef RatingControlDemoImagePickerH
#define RatingControlDemoImagePickerH
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp>
#include <Variants.hpp>
#include <Classes.hpp>
#include <Graphics.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <cxGraphics.hpp>
#include <cxControls.hpp>
#include <cxLookAndFeels.hpp>
#include <cxLookAndFeelPainters.hpp>
#include <cxContainer.hpp>
#include <cxEdit.hpp>
#include <Menus.hpp>
#include <StdCtrls.hpp>
#include <cxButtons.hpp>
#include <cxLabel.hpp>
#include <dxGDIPlusClasses.hpp>
#include <cxImage.hpp>
#include <dxRatingControl.hpp>
//---------------------------------------------------------------------------
class TfrmImagePicker : public TForm
{
__published:	// IDE-managed Components
	TcxImage* imgUnchecked;
	TcxImage* imgHover;
	TcxImage* imgChecked;
	TcxLabel* lbCaption;
	TcxButton* btnApply;
	TcxButton* btnReset;
	void __fastcall ApplyClick(TObject* Sender);
	void __fastcall ResetClick(TObject* Sender);
  private:
	TdxRatingControlProperties* FProperties;
  public:
	void Initialize(TdxRatingControlProperties* AProperties);
	__fastcall TfrmImagePicker(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmImagePicker *frmImagePicker;
//---------------------------------------------------------------------------
#endif
