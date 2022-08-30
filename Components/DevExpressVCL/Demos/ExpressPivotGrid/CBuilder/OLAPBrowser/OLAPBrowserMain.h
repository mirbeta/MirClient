//---------------------------------------------------------------------------

#ifndef OLAPBrowserMainH
#define OLAPBrowserMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxCustomPivotGrid.hpp"
#include "cxGraphics.hpp"
#include "cxPivotGrid.hpp"
#include "cxPivotGridOLAPDataSource.hpp"
#include "cxPivotGridOLAPConnectionDesigner.hpp"
#include "cxStyles.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "ExtCtrls.hpp"
#include "cxSplitter.hpp"

//---------------------------------------------------------------------------
class TfrmOlapBrowser : public TfrmDemoBasicMain
{
__published:	// IDE-managed Components
	TcxPivotGrid *UnboundPivotGrid;
	TMenuItem *NewConnection1;
	TMenuItem *N3;
    TPanel *Panel1;
    TcxSplitter *cxSplitter1;
	TcxPivotGridOLAPDataSource *OLAPDataSource;
	TMenuItem *miADOMDProvider;
	TMenuItem *miLockedStateImage;
	TMenuItem *miLockedStateImageMode;
	TMenuItem *miLockedStateImageEffect;
	TMenuItem *miLockedStateImageModeNever;
	TMenuItem *miLockedStateImageModeImmediate;
	TMenuItem *miLockedStateImageModePending;
	TMenuItem *miLockedStateImageEffectLight;
	TMenuItem *miLockedStateImageEffectDark;
	TMenuItem *miOLEDBProvider;
	TMenuItem *miProvider;
	void __fastcall ChangeProviderClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall LockedViewImageClick(TObject *Sender);
	void __fastcall NewConnection1Click(TObject *Sender);
  private:	// User declarations
	void __fastcall SetFieldPos(const String AFieldName, TcxPivotGridFieldArea AArea);
	void __fastcall LoadDefaultLayout(bool AActivate);
	TcxCustomPivotGrid* __fastcall PivotGrid();
  protected:
	void __fastcall SyncMenuWithOptionsLockedStateImage();
public:		// User declarations
	__fastcall TfrmOlapBrowser(TComponent* Owner);
	void __fastcall AfterConstruction();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmOlapBrowser *frmOlapBrowser;
//---------------------------------------------------------------------------
#endif
