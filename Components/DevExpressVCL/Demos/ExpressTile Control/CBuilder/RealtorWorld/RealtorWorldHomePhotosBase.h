//---------------------------------------------------------------------------

#ifndef RealtorWorldHomePhotosBaseH
#define RealtorWorldHomePhotosBaseH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DB.hpp>
#include "RealtorWorldBaseFrame.h"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSplitter.hpp"
#include "dxCustomTileControl.hpp"
#include "dxTileControl.hpp"
//---------------------------------------------------------------------------
class TfrmHomePhotosBase : public TfrmBase
{
__published:	// IDE-managed Components
	TcxSplitter *cxSplitter1;
	TdxTileControl *tcHomePhotos;
	TdxTileControlGroup *tcHomePhotosdxTileControlGroup1;
	void __fastcall cxSplitter1BeforeClose(TObject *Sender, bool &AllowClose);
private:	// User declarations
protected:
	virtual void __fastcall OnItemClick(TdxTileControlItem *Sender);
public:		// User declarations
	__fastcall TfrmHomePhotosBase(TComponent* Owner);
	void __fastcall SelectItem(int APhotoID, int AAgentID);
	virtual void __fastcall InitializeFrame();

};
//---------------------------------------------------------------------------
extern PACKAGE TfrmHomePhotosBase *frmHomePhotosBase;
//---------------------------------------------------------------------------
#endif
