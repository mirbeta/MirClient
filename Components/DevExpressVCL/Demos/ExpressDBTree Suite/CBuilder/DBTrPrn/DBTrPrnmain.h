//---------------------------------------------------------------------------
#ifndef DBTrPrnmainH
#define DBTrPrnmainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include "dxdbtree.hpp"
#include "dxtree.hpp"
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <DBTables.hpp>
#include <Db.hpp>
#include "dxdbtree.hpp"
#include "dxmdaset.hpp"
#include "dxtree.hpp"
#include "dxtrprds.hpp"
//---------------------------------------------------------------------------
class TFMain : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TRadioGroup *RadioGroup;
	TEdit *ELevels;
	TButton *Button1;
	TBitBtn *BitBtn1;
	TPanel *Panel2;
        TdxDBTreeView *DBTreeView1;
	TTable *Table;
	TDataSource *DataSource1;
	TImageList *ImageList1;
        TdxDBTreePrintData *DBTreePrintDataSet;
	void __fastcall ELevelsKeyPress(TObject *Sender, char &Key);
	void __fastcall ELevelsExit(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall DBTreeView1AddNewItem(TObject *Sender,
          TdxDBTreeNode *&DBTreeNode);
        void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TFMain *FMain;
//---------------------------------------------------------------------------
#endif
