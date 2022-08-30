//---------------------------------------------------------------------------

#ifndef LayoutControlRLMainH
#define LayoutControlRLMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxDrawTextUtils.hpp"
#include "cxGraphics.hpp"
#include "DemoBasicMain.h"
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
#include "dxLayoutLookAndFeels.hpp"
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBEdit.hpp"
#include "cxDBNavigator.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxNavigator.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxTimeEdit.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxPSContainerLnk.hpp"
#include "dxPScxDBEditorLnks.hpp"
#include "dxPSdxLCLnk.hpp"
#include "dxPSTextLnk.hpp"
//---------------------------------------------------------------------------
class TLayoutControlMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxImageList *cxImageList1;
	TMenuItem *Customization1;
	TMenuItem *N1;
	TdxLayoutControl *lcMain;
	TcxDBDateEdit *cxDBDateEdit1;
	TcxDBTimeEdit *cxDBTimeEdit1;
	TcxDBComboBox *cxDBComboBox1;
	TcxDBCalcEdit *cxDBCalcEdit1;
	TcxDBCurrencyEdit *cxDBCurrencyEdit1;
	TcxDBTextEdit *cxDBTextEdit1;
	TcxDBTextEdit *cxDBTextEdit2;
	TcxDBTextEdit *cxDBTextEdit3;
	TcxDBTextEdit *cxDBTextEdit4;
	TcxDBTextEdit *cxDBTextEdit5;
	TcxDBCheckBox *cxDBCheckBox1;
	TcxDBTextEdit *cxDBTextEdit6;
	TcxDBTextEdit *cxDBTextEdit7;
	TcxDBTextEdit *cxDBTextEdit8;
	TcxDBTextEdit *cxDBTextEdit9;
	TcxDBMaskEdit *cxDBMaskEdit1;
	TcxDBTextEdit *cxDBTextEdit10;
	TcxDBMaskEdit *cxDBMaskEdit2;
	TcxDBMaskEdit *cxDBMaskEdit3;
	TcxDBHyperLinkEdit *cxDBHyperLinkEdit1;
	TcxDBTextEdit *cxDBTextEdit12;
	TcxDBTextEdit *cxDBTextEdit11;
	TcxDBHyperLinkEdit *cxDBHyperLinkEdit2;
	TcxDBCurrencyEdit *cxDBCurrencyEdit2;
	TcxDBSpinEdit *cxDBSpinEdit1;
	TcxDBSpinEdit *cxDBSpinEdit2;
	TcxDBSpinEdit *cxDBSpinEdit3;
	TcxDBSpinEdit *cxDBSpinEdit4;
	TcxDBSpinEdit *cxDBSpinEdit5;
	TcxDBSpinEdit *cxDBSpinEdit6;
	TcxDBCheckBox *cxDBCheckBox2;
	TcxDBImage *cxDBImage1;
	TcxDBMemo *cxDBMemo1;
	TcxDBNavigator *cxDBNavigator1;
	TdxLayoutGroup *lcMainGroup_Root1;
	TdxLayoutGroup *lcMainGroup2;
	TdxLayoutGroup *lcMainGroup7;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutItem *lcMainItem2;
	TdxLayoutGroup *lcMainGroup8;
	TdxLayoutItem *lcMainItem3;
	TdxLayoutItem *lcMainItem24;
	TdxLayoutItem *lcMainItem4;
	TdxLayoutItem *lcMainItem5;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutGroup *lcMainGroup18;
	TdxLayoutGroup *lcMainGroup17;
	TdxLayoutItem *lcMainItem8;
	TdxLayoutItem *lcMainItem6;
	TdxLayoutItem *lcMainItem7;
	TdxLayoutSeparatorItem *lcMainSeparatorItem2;
	TdxLayoutItem *lcMainItem9;
	TdxLayoutSplitterItem *dxLayoutSplitterItem1;
	TdxLayoutGroup *lcMainGroup10;
	TdxLayoutItem *lcMainItem14;
	TdxLayoutItem *lcMainItem15;
	TdxLayoutItem *lcMainItem16;
	TdxLayoutSplitterItem *lcMainSplitterItem2;
	TdxLayoutGroup *lcMainGroup16;
	TdxLayoutItem *lcMainItem12;
	TdxLayoutItem *lcMainItem10;
	TdxLayoutItem *lcMainItem13;
	TdxLayoutSeparatorItem *dxLayoutSeparatorItem1;
	TdxLayoutItem *lcMainItem17;
	TdxLayoutGroup *lcMainGroup13;
	TdxLayoutItem *lcMainItem18;
	TdxLayoutItem *lcMainItem19;
	TdxLayoutItem *lcMainItem20;
	TdxLayoutGroup *lcMainGroup3;
	TdxLayoutGroup *lcMainGroup4;
	TdxLayoutItem *lcMainItem21;
	TdxLayoutItem *lcMainItem22;
	TdxLayoutItem *lcMainItem23;
	TdxLayoutGroup *lcMainGroup5;
	TdxLayoutGroup *lcMainGroup14;
	TdxLayoutItem *lcMainItem25;
	TdxLayoutItem *lcMainItem26;
	TdxLayoutItem *lcMainItem27;
	TdxLayoutGroup *lcMainGroup15;
	TdxLayoutItem *lcMainItem31;
	TdxLayoutItem *lcMainItem30;
	TdxLayoutGroup *lcMainGroup6;
	TdxLayoutItem *lcMainItem32;
	TdxLayoutItem *lcMainItem33;
	TdxLayoutItem *lcMainItem34;
	TdxLayoutItem *lcMainItem11;
	TdxLayoutItem *lcMainItem28;
	TdxLayoutItem *lcMainItem29;
	TdxLayoutControlReportLink *dxComponentPrinterLink1;
	TMenuItem *miStyle;
	TMenuItem *miUsecxLookAndFeel;
	TMenuItem *miUsecxLookAndFeelNative;
	TMenuItem *miUsecxLookAndFeelOffice11;
	TMenuItem *miUsecxLookAndFeelStandard;
	TMenuItem *miUsecxLookAndFeelFlat;
	TMenuItem *miUsecxLookAndFeelUltraFlat;
	TMenuItem *miLayoutWeb;
	TMenuItem *miLayoutOffice;
	TMenuItem *miLayoutStandard;
	TAction *acLayoutOffice;
	TAction *acLayoutWeb;
	TAction *acFlat;
	TAction *acStandard;
	TAction *acUltraFlat;
	TAction *acOffice11;
	TAction *acNative;
	TAction *acLayoutStandard;
	TdxLayoutLookAndFeelList *llcfMain;
	TdxLayoutStandardLookAndFeel *dxLayoutStandardLookAndFeel1;
	TdxLayoutOfficeLookAndFeel *dxLayoutOfficeLookAndFeel1;
	TdxLayoutWebLookAndFeel *dxLayoutWebLookAndFeel1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
	TdxLayoutSkinLookAndFeel *dxLayoutSkinLookAndFeel1;
	void __fastcall acLayoutStyleExecute(TObject *Sender);
	void __fastcall dxLayoutGroup1Button0Click(TObject *Sender);
	void __fastcall Customization1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TLayoutControlMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TLayoutControlMainForm *LayoutControlMainForm;
//---------------------------------------------------------------------------
#endif
