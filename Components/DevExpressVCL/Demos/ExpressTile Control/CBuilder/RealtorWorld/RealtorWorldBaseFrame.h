//---------------------------------------------------------------------------

#ifndef RealtorWorldBaseFrameH
#define RealtorWorldBaseFrameH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TfrmBase : public TFrame
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall TfrmBase(TComponent* Owner);
	virtual void __fastcall SelectItem(int APhotoID, int AAgentID);
};
TfrmBase* __fastcall CreateFrameByID(TComponent* AOwner, int AID);
//---------------------------------------------------------------------------
extern PACKAGE TfrmBase *frmBase;
//---------------------------------------------------------------------------
#endif
