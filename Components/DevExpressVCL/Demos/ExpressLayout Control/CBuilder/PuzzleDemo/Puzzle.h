//---------------------------------------------------------------------------

#ifndef PuzzleH
#define PuzzleH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "cxClasses.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmPuzzle : public TForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *lcMain;
	TdxLayoutGroup *lcMainGroup_Root1;
	TdxLayoutGroup *lcMainGroup4;
	TdxLayoutImageItem *lcItem1;
	TdxLayoutImageItem *lcItem2;
	TdxLayoutImageItem *lcItem3;
	TdxLayoutImageItem *lcItem4;
	TdxLayoutImageItem *lcItem5;
	TdxLayoutImageItem *lcItem6;
	TdxLayoutImageItem *lcItem7;
	TdxLayoutImageItem *lcItem8;
	TdxLayoutImageItem *lcItem9;
	TdxLayoutGroup *lcMainGroup5;
	TdxLayoutGroup *lcMainGroup6;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList10;
	TdxLayoutStandardLookAndFeel *dxLayoutStandardLookAndFeel10;
	TMainMenu *MainMenu1;
	TMenuItem *About1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall About1Click(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	void __fastcall Shufflepuzzle1Click(TObject *Sender);
public:		// User declarations
	__fastcall TfrmPuzzle(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmPuzzle *frmPuzzle;
//---------------------------------------------------------------------------
#endif
