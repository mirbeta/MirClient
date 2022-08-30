//---------------------------------------------------------------------------

#ifndef CheckGroupsDemoMainH
#define CheckGroupsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfmGheckGroupsDemo : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxTreeList *tlDXInstallation;
	TcxTreeListColumn *cxTreeList1Column1;
	TcxEditRepository *cxEditRepository1;
	TcxEditRepositoryTextItem *cxEditRepository1TextItem1;
	TcxImageList *cxImageList1;
	TcxStyleRepository *cxStyleRepository1;
	TcxStyle *cxStyle1;
	TMenuItem *N1;
	TMenuItem *ShowTreeLines1;
	void __fastcall ShowTreeLines1Click(TObject *Sender);

private:	// User declarations
public:		// User declarations
	__fastcall TfmGheckGroupsDemo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmGheckGroupsDemo *fmGheckGroupsDemo;
//---------------------------------------------------------------------------
#endif
