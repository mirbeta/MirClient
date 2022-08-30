//---------------------------------------------------------------------------

#ifndef cxEditorsDemoMainH
#define cxEditorsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BasicDemoMain.h"
#include "cxControls.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutContainer.hpp"
#include <ActnList.hpp>
#include <Menus.hpp>
#include "cxCalc.hpp"
#include "cxCalendar.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBEdit.hpp"
#include "cxDBNavigator.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxNavigator.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxTimeEdit.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmEditorsDemoMain : public TfrmBasicDemoMain
{
__published:	// IDE-managed Components
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
	TcxImageList *cxImageList1;
        TdxLayoutGroup *lcMainGroup1;
	TdxLayoutGroup *lcMainGroup2;
	TdxLayoutGroup *lcMainGroup3;
	TdxLayoutGroup *lcMainGroup4;
	TdxLayoutGroup *lcMainGroup5;
	TdxLayoutGroup *lcMainGroup6;
	TdxLayoutGroup *lcMainGroup7;
	TdxLayoutGroup *lcMainGroup8;
	TdxLayoutGroup *lcMainGroup13;
	TdxLayoutGroup *lcMainGroup14;
	TdxLayoutGroup *lcMainGroup15;
	TdxLayoutGroup *lcMainGroup16;
	TdxLayoutGroup *lcMainGroup17;
	TdxLayoutGroup *lcMainGroup18;
	TdxLayoutItem *lcMainItem1;
	TdxLayoutItem *lcMainItem2;
	TdxLayoutItem *lcMainItem3;
	TdxLayoutItem *lcMainItem4;
	TdxLayoutItem *lcMainItem5;
	TdxLayoutItem *lcMainItem6;
	TdxLayoutItem *lcMainItem7;
	TdxLayoutItem *lcMainItem8;
	TdxLayoutItem *lcMainItem9;
	TdxLayoutItem *lcMainItem10;
	TdxLayoutItem *lcMainItem11;
	TdxLayoutItem *lcMainItem12;
	TdxLayoutItem *lcMainItem13;
	TdxLayoutItem *lcMainItem14;
	TdxLayoutItem *lcMainItem15;
	TdxLayoutItem *lcMainItem16;
	TdxLayoutItem *lcMainItem17;
	TdxLayoutItem *lcMainItem18;
	TdxLayoutItem *lcMainItem19;
	TdxLayoutItem *lcMainItem20;
	TdxLayoutItem *lcMainItem21;
	TdxLayoutItem *lcMainItem22;
	TdxLayoutItem *lcMainItem25;
	TdxLayoutItem *lcMainItem26;
	TdxLayoutItem *lcMainItem27;
	TdxLayoutItem *lcMainItem28;
	TdxLayoutItem *lcMainItem29;
	TdxLayoutItem *lcMainItem31;
	TdxLayoutItem *lcMainItem30;
	TdxLayoutItem *lcMainItem32;
	TdxLayoutItem *lcMainItem33;
	TdxLayoutItem *lcMainItem34;
        TdxLayoutSplitterItem *lcMainSplitterItem1;
        TdxLayoutSeparatorItem *lcMainSeparatorItem1;
        TdxLayoutSplitterItem *lcMainSplitterItem2;
        TdxLayoutSeparatorItem *lcMainSeparatorItem2;
	void __fastcall lcMainGroup1Button0Click(TObject *Sender);
	void __fastcall lcMainSplitterItem3CanResize(TObject *Sender, TdxCustomLayoutItem *AItem,
          int &ANewSize, bool &AAccept);

private:	// User declarations
public:		// User declarations
	__fastcall TfrmEditorsDemoMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEditorsDemoMain *frmEditorsDemoMain;
//---------------------------------------------------------------------------
#endif
