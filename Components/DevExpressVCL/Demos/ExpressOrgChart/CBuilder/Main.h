//---------------------------------------------------------------------------
#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxorgchr.hpp"
#include <Menus.hpp>
#include "dxdborgc.hpp"
#include <DBGrids.hpp>
#include "Grids.hpp"
#include <DB.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include "options.h"
#include <ExtCtrls.hpp>
#include <Db.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <ImgList.hpp>
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TMainMenu *MainMenu;
	TMenuItem *File1;
	TMenuItem *Exit1;
	TMenuItem *Edit1;
	TMenuItem *AddNode1;
	TMenuItem *AddChildeNode1;
	TMenuItem *RenameNode1;
	TMenuItem *DeleteNode1;
	TMenuItem *View1;
	TMenuItem *ItZoom;
	TMenuItem *ItRotated;
	TMenuItem *ItAnimated;
	TMenuItem *It3D;
	TMenuItem *N1;
	TMenuItem *ItFullExpand;
	TMenuItem *ItFullCollapse;
	TMenuItem *Options1;
	TClientDataSet *Table1;
	TDataSource *DataSource1;
	TPageControl *PC;
	TdxDbOrgChart *DBTree;
	TDBGrid *DBGrid1;
	TcxImageList *ilTree;
	TdxOrgChart *Tree;
	TTabSheet *tsOrgChart;
	TTabSheet *tsDBOrgChart;
	TColorDialog *ColorDialog;
	TSplitter *Splitter1;
	TPanel *Panel1;
	TButton *cxButton1;
    TMenuItem *miLooknFeel;
    TMenuItem *miKindOffice11;
    TMenuItem *miKindStandard;
    TMenuItem *miKindFlat;
    TMenuItem *miKindUltraFlat;
    TMenuItem *miKindNative;
    TMenuItem *miAntialiasing;
	TcxLookAndFeelController *cxLookAndFeelController;
    TAutoIncField *Table1ID;
    TIntegerField *Table1PARENT;
    TStringField *Table1NAME;
    TDateField *Table1CDATE;
    TStringField *Table1CBY;
    TIntegerField *Table1WIDTH;
    TIntegerField *Table1HEIGHT;
    TStringField *Table1TYPE;
    TIntegerField *Table1COLOR;
    TIntegerField *Table1IMAGE;
    TStringField *Table1IMAGEALIGN;
    TIntegerField *Table1ORDER;
    TStringField *Table1ALIGN;
	TIntegerField *Table1Align_num;
	TIntegerField *Table1Type_num;
	TIntegerField *Table1ImageAlign_num;
	void __fastcall DBGrid1DrawColumnCell(TObject *Sender, const TRect &Rect,
		int DataCol, TColumn *Column, TGridDrawState State);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall DBGrid1ColEnter(TObject *Sender);
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall AddNode1Click(TObject *Sender);
	void __fastcall AddChildeNode1Click(TObject *Sender);
	void __fastcall RenameNode1Click(TObject *Sender);
	void __fastcall DeleteNode1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall PCChange(TObject *Sender);
	void __fastcall ItZoomClick(TObject *Sender);
	void __fastcall ItRotatedClick(TObject *Sender);
	void __fastcall ItAnimatedClick(TObject *Sender);
	void __fastcall It3DClick(TObject *Sender);
	void __fastcall ItFullExpandClick(TObject *Sender);
	void __fastcall ItFullCollapseClick(TObject *Sender);
	void __fastcall DBTreeCreateNode(TObject *Sender, TdxOcNode *Node);
	void __fastcall Table1AfterInsert(TDataSet *DataSet);
	void __fastcall DBGrid1DblClick(TObject *Sender);
	void __fastcall DBGrid1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall TableTYPEChange(TField *Sender);
	void __fastcall TableALIGNChange(TField *Sender);
	void __fastcall TableIMAGEALIGNChange(TField *Sender);
	void __fastcall Options1Click(TObject *Sender);
	void __fastcall TreeCreateNode(TObject *Sender, TdxOcNode *Node);
	void __fastcall miAntialiasingClick(TObject *Sender);
	void __fastcall miKindNativeClick(TObject* Sender);
	void __fastcall Table1CalcFields(TDataSet *DataSet);
private:	// User declarations
public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
    TdxOcShape __fastcall GetShape(AnsiString ShapeName);
    TdxOcNodeAlign __fastcall GetNodeAlign(AnsiString AlignName);
    TdxOcImageAlign __fastcall GetImageAlign(AnsiString AlignName);
};
//---------------------------------------------------------------------------
extern TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
