//---------------------------------------------------------------------------

#ifndef RibbonNotepadDemoGallerySetupH
#define RibbonNotepadDemoGallerySetupH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------

class TColorDialogSetupForm : public TForm
{
__published:	// IDE-managed Components
		TButton *btnOK;
		TButton *btnCancel;
		TCheckBox *chkRemoveHorizontalItemPadding;
		TCheckBox *chkRemoveVerticalItemPadding;
private:	// User declarations
public:		// User declarations
		__fastcall TColorDialogSetupForm(TComponent* Owner);
		bool __fastcall GetSettings(bool &RemoveHorizontalItemPadding,
		  bool &RemoveVerticalItemPadding);
};
//---------------------------------------------------------------------------
extern PACKAGE TColorDialogSetupForm *ColorDialogSetupForm;
//---------------------------------------------------------------------------
#endif
