//---------------------------------------------------------------------------

#ifndef RealtorWorldListingH
#define RealtorWorldListingH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "dxCustomTileControl.hpp"
#include "dxImageSlider.hpp"
#include "dxTileControl.hpp"
#include <jpeg.hpp>
#include "RealtorWorldBaseFrame.h"
#include "RealtorWorldHomePhotosBase.h"
//---------------------------------------------------------------------------
class TfrmListing : public TfrmHomePhotosBase
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox1;
	TcxGroupBox *cxGroupBox2;
	TdxImageSlider *imgsHome;
	TcxRichEdit *reFeatures;
	TcxSplitter *cxSplitter3;
	TcxSplitter *cxSplitter2;
	TcxGroupBox *cxGroupBox3;
	TcxImage *imgHomePlan;
	TcxImageCollection *icPlans;
	TcxImageCollectionItem *icPlansItem1;
	TcxImageCollectionItem *icPlansItem2;
	TcxImageCollectionItem *icPlansItem3;
	TcxImageCollectionItem *icPlansItem4;
	TcxImageCollectionItem *icPlansItem5;
	TcxImageCollection *icSlider;
	void __fastcall imgHomePlanMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall cxSplitter3BeforeClose(TObject *Sender, bool &AllowClose);
private:	// User declarations
    AnsiString __fastcall BitmapToRTF(Graphics::TBitmap *pict);
	void __fastcall InitializeFeatures();
	void __fastcall InitializeFeaturesOfAgent();
	void __fastcall InitializeFeaturesOfHouse();
	void __fastcall ReplaceInFeatures(String ATokenStr, String S);
protected:
	void __fastcall OnItemClick(TdxTileControlItem *Sender);
public:		// User declarations
	__fastcall TfrmListing(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmListing *frmListing;
//---------------------------------------------------------------------------
class TcxImageAccess : public TcxImage
{
public:
  void __fastcall Centre(void);
};
#endif
