//---------------------------------------------------------------------------

#ifndef EditorsDemoMainH
#define EditorsDemoMainH
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
#include "dxLayoutControlAdapters.hpp"
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Mask.hpp>
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmEditorsDemoMain : public TfrmBasicDemoMain
{
__published:	// IDE-managed Components
	TDBEdit *DBEdit1;
	TDBEdit *DBEdit2;
	TDBEdit *DBEdit4;
	TDBEdit *DBEdit5;
	TDBEdit *DBEdit6;
	TDBEdit *DBEdit7;
	TDBEdit *DBEdit8;
	TDBEdit *DBEdit9;
	TDBCheckBox *DBCheckBox1;
	TDBEdit *DBEdit10;
	TDBEdit *DBEdit11;
	TDBEdit *DBEdit12;
	TDBEdit *DBEdit13;
	TDBEdit *DBEdit14;
	TDBEdit *DBEdit15;
	TDBEdit *DBEdit16;
	TDBEdit *DBEdit17;
	TDBEdit *DBEdit18;
	TDBEdit *DBEdit19;
	TDBEdit *DBEdit20;
	TDBEdit *DBEdit21;
	TDBEdit *DBEdit22;
	TDBEdit *DBEdit23;
	TDBEdit *DBEdit24;
	TDBEdit *DBEdit25;
	TDBEdit *DBEdit26;
	TDBEdit *DBEdit27;
	TDBCheckBox *DBCheckBox2;
	TDBEdit *DBEdit28;
	TDBMemo *DBMemo1;
	TDBNavigator *DBNavigator1;
	TDBComboBox *DBComboBox2;
	TDBEdit *DBEdit29;
	TdxLayoutGroup *lcMainGroup17;
	TdxLayoutItem *lcMainItem34;
	TdxLayoutGroup *lcMainGroup16;
	TdxLayoutItem *lcMainItem2;
	TdxLayoutItem *lcMainItem4;
	TdxLayoutItem *lcMainItem5;
	TdxLayoutGroup *lcMainGroup2;
	TdxLayoutGroup *lcMainGroup10;
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
	TdxLayoutGroup *lcMainGroup13;
	TdxLayoutItem *lcMainItem18;
	TdxLayoutItem *lcMainItem19;
	TdxLayoutItem *lcMainItem20;
	TdxLayoutGroup *lcMainGroup3;
	TdxLayoutGroup *lcMainGroup4;
	TdxLayoutItem *lcMainItem21;
	TdxLayoutItem *lcMainItem22;
	TdxLayoutGroup *lcMainGroup5;
	TdxLayoutGroup *lcMainGroup14;
	TdxLayoutItem *lcMainItem25;
	TdxLayoutItem *lcMainItem26;
	TdxLayoutItem *lcMainItem27;
	TdxLayoutItem *lcMainItem28;
	TdxLayoutItem *lcMainItem29;
	TdxLayoutGroup *lcMainGroup15;
	TdxLayoutItem *lcMainItem30;
	TdxLayoutItem *lcMainItem31;
	TdxLayoutGroup *lcMainGroup6;
	TdxLayoutItem *lcMainItem32;
	TdxLayoutItem *lcMainItem33;
	TdxLayoutItem *lcMainItem24;
	TdxLayoutItem *lcMainItem23;
	TcxImageList *cxImageList1;
	TdxLayoutGroup *lcMainGroup7;
	TdxLayoutGroup *lcMainGroup8;
	TdxLayoutSeparatorItem *lcMainSeparatorItem1;
	TdxLayoutSeparatorItem *lcMainSeparatorItem2;
	TdxLayoutSplitterItem *lcMainSplitterItem1;
	TdxLayoutSplitterItem *lcMainSplitterItem2;
	void __fastcall lcMainGroup2Button0Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmEditorsDemoMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmEditorsDemoMain *frmEditorsDemoMain;
//---------------------------------------------------------------------------
#endif
