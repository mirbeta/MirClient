//---------------------------------------------------------------------------

#ifndef DemoBasicMainH
#define DemoBasicMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxDrawTextUtils.hpp"
#include "cxGraphics.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSCore.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TDemoBasicMainForm : public TForm
{
__published:	// IDE-managed Components
	TdxComponentPrinter *dxComponentPrinter;
	TdxPSEngineController *dxPSEngineController1;
	TcxImageList *ilMain;
	TLabel *lbDescrip;
	TMainMenu *mmMain;
	TMenuItem *miFile;
	TMenuItem *miPageSetup;
	TMenuItem *miPrintPreview;
	TMenuItem *Print1;
	TMenuItem *miAntiAliasing;
	TMenuItem *N2;
	TMenuItem *miExit;
	TMenuItem *miOptions;
	TMenuItem *miPirntingOptions;
	TMenuItem *miPreviewDialogStyles;
	TMenuItem *miDialogsLookAndFeel;
	TMenuItem *miKind;
	TMenuItem *miFlat;
	TMenuItem *miStandard;
	TMenuItem *miUltraFlat;
	TMenuItem *miOffice11;
	TMenuItem *miNativeStyle;
	TMenuItem *miSeparator2;
	TMenuItem *miShowDemoDescription;
	TMenuItem *miHelp;
	TMenuItem *miDeveloperExpressontheweb;
	TMenuItem *N10;
	TMenuItem *Aboutthisdemo1;
	TStatusBar *sbMain;
	TActionList *sty;
	TAction *actDXOnTheWeb;
	TAction *actExit;
	TAction *actShowDemoDescription;
	TAction *actPageSetup;
	TAction *actPreview;
	TAction *actPrint;
	TAction *actDesigner;
	TAction *actAbout;
	TAction *actImageAntiAliasing;
	TToolBar *ToolBar1;
	TToolButton *tbtnPageSetup;
	TToolButton *tbtnPreview;
	TToolButton *tbtnPrint;
	TToolButton *tbtnDesigner;
	TToolButton *ToolButton1;
	void __fastcall actDXOnTheWebExecute(TObject *Sender);
	void __fastcall actExitExecute(TObject *Sender);
	void __fastcall actShowDemoDescriptionExecute(TObject *Sender);
	void __fastcall actPageSetupExecute(TObject *Sender);
	void __fastcall actPreviewExecute(TObject *Sender);
	void __fastcall actPrintExecute(TObject *Sender);
	void __fastcall actDesignerExecute(TObject *Sender);
	void __fastcall actAboutExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall DialogsLookAndFeelChanged(TObject *Sender);
	void __fastcall AlwaysEnabled(TObject *Sender);
	void __fastcall actImageAntiAliasingExecute(TObject *Sender);
private:	// User declarations
	TMenuItem* __fastcall CreateStyleItem(TComponent *AOwner, String ACaption, int ATag);
	void __fastcall PreviewDialogStyleClick(TObject *Sender);
public:		// User declarations
	__fastcall TDemoBasicMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
#endif
