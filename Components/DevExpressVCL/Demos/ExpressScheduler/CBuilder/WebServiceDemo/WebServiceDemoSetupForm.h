//---------------------------------------------------------------------------

#ifndef WebServiceDemoSetupFormH
#define WebServiceDemoSetupFormH
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
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxCustomWizardControl.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxWizardControl.hpp"
#include "cxCheckBox.hpp"
#include "cxButtons.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include <Menus.hpp>

//---------------------------------------------------------------------------
class TWebServiceDemoSetupWizard : public TForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *dxLayoutControl2;
	TcxRichEdit *reGoogleApi;
	TcxTextEdit *teGoogleApiClientSecret;
	TcxTextEdit *teGoogleApiClientID;
	TcxRichEdit *reMSGraph;
	TcxTextEdit *teMSGraphClientID;
	TcxTextEdit *teMSGraphClientSecret;
	TcxButton *btnStart;
	TcxButton *btnCancel;
	TcxRichEdit *reAbout;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutItem *dxLayoutItem5;
	TdxLayoutItem *dxLayoutItem6;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutItem *dxLayoutItem2;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutGroup *dxLayoutGroup5;
	TdxLayoutItem *dxLayoutItem7;
	TdxLayoutItem *dxLayoutItem9;
	TdxLayoutItem *dxLayoutItem10;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall reURLClick(TcxCustomRichEdit *Sender, const UnicodeString URLText,
          TMouseButton Button);
	void __fastcall teChange(TObject *Sender);
private:	// User declarations
    void __fastcall UpdateStateButtons();
public:		// User declarations
	__fastcall TWebServiceDemoSetupWizard(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebServiceDemoSetupWizard *WebServiceDemoSetupWizard;
//---------------------------------------------------------------------------
#endif
