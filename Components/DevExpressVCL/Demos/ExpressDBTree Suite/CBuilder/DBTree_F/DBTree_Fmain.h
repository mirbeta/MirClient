//---------------------------------------------------------------------------
#ifndef DBTree_FmainH
#define DBTree_FmainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <DBCtrls.hpp>
#include <DB.hpp>
#include "dxdbtree.hpp"
#include "dxtree.hpp"
#include <Db.hpp>
#include <Mask.hpp>
#include "dxmdaset.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
  int  MaxValue;
__published:	// IDE-managed Components
	TdxDBTreeView *DBTreeView1;
	TPanel *Panel1;
	TButton *Button1;
	TBitBtn *BitBtnAdd;
	TBitBtn *BitBtnAddChild;
	TBitBtn *BitBtnEdit;
	TBitBtn *BitBtnDel;
	TBitBtn *BitBtn2;
	TButton *Button2;
	TButton *Button3;
	TCheckBox *CheckBox1;
	TCheckBox *CheckBox2;
	TCheckBox *CheckBox3;
	TDBNavigator *DBNavigator1;
	TDataSource *DataSource1;
	TDBEdit *DBEdit1;
	TDBEdit *DBEdit2;
	TdxMemData *dxMemData1;
	TIntegerField *dxMemData1id;
	TIntegerField *dxMemData1parent;
	TStringField *dxMemData1name;
	TStringField *dxMemData1buffer;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall BitBtnAddClick(TObject *Sender);
	void __fastcall BitBtnAddChildClick(TObject *Sender);
	void __fastcall BitBtnEditClick(TObject *Sender);
	void __fastcall BitBtnDelClick(TObject *Sender);
	void __fastcall BitBtn2Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall CheckBox1Click(TObject *Sender);
	void __fastcall CheckBox2Click(TObject *Sender);
	void __fastcall CheckBox3Click(TObject *Sender);
		void __fastcall DBTreeView1CreateNewKeyValue(TObject *Sender,
		Variant &NewKeyValue);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
