//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <StdCtrls.hpp>
#include "DBTree_Fmain.h"
#include "dbview.h"
#include "dxtree.hpp"
#include "dxdbtree.hpp"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxdbtree"
#pragma link "dxtree"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Form2->DBGrid1->DataSource = DataSource1;
  Form2->ShowModal();
  Form2->DBGrid1->DataSource = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  int i, j, k, l, id, parent;
  TIndexOptions  Options;
  Options = Options << ixPrimary << ixUnique;
  Options = Options >> ixPrimary >> ixUnique;
  dxMemData1->Open();

  for (i = 0; i < 10; i++)
	for (j = 0; j < 10; j++)
	  for (k = 0; k < 10; k++)
		for (l = 0; l < 10; l++) {
			dxMemData1->Append();
			id = l + k*10 + j* 100 + i * 1000;
			parent = k + j* 10 + i * 100 - 1;
			dxMemData1->FindField("id")->AsInteger = id;
			dxMemData1->FindField("parent")->AsInteger = parent;
			dxMemData1->FindField("Name")->AsString = "TreeNode item No " +  IntToStr(id);
			dxMemData1->FindField("Buffer")->AsString = "No " + IntToStr(id);
			dxMemData1->Post();
		  }

  dxMemData1->First();
  MaxValue = 10000;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  dxMemData1->Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtnAddClick(TObject *Sender)
{
	if (DBTreeView1->Selected != NULL)
	DBTreeView1->Items->Add(DBTreeView1->Selected, "New Item");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtnAddChildClick(TObject *Sender)
{
  if (DBTreeView1->Selected != NULL)
	DBTreeView1->Items->AddChild(DBTreeView1->Selected,"Child of " + DBTreeView1->Selected->Text);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtnEditClick(TObject *Sender)
{
  if (DBTreeView1->Selected != NULL)
    DBTreeView1->Selected->EditText();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtnDelClick(TObject *Sender)
{
  if (DBTreeView1->Selected != NULL)
    DBTreeView1->Selected->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BitBtn2Click(TObject *Sender)
{
  TTreeNode * tr;

  if (DBTreeView1->Selected != NULL){
    tr = DBTreeView1->Selected->Parent;
    if(tr != NULL){
      if(tr->Parent != NULL)
        DBTreeView1->Selected->MoveTo(tr->Parent, naAddChild);
      else DBTreeView1->Selected->MoveTo(NULL, naAdd);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  DBTreeView1->DataSource = DataSource1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
    DBTreeView1->DataSource = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  if(CheckBox1->Checked)
    DBTreeView1->Options = DBTreeView1->Options << trCanDBNavigate;
  else DBTreeView1->Options = DBTreeView1->Options  >> trCanDBNavigate;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  if(CheckBox2->Checked)
    DBTreeView1->Options = DBTreeView1->Options << trCheckHasChildren;
  else DBTreeView1->Options = DBTreeView1->Options >> trCheckHasChildren;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
  if(CheckBox3->Checked)
    DBTreeView1->Options = DBTreeView1->Options << trSmartRecordCopy;
  else DBTreeView1->Options = DBTreeView1->Options >> trSmartRecordCopy;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DBTreeView1CreateNewKeyValue(TObject *Sender,
        Variant &NewKeyValue)
{
  MaxValue ++;
  NewKeyValue = MaxValue;
}
//---------------------------------------------------------------------------
