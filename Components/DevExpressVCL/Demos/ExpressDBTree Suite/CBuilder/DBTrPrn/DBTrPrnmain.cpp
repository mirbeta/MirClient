//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <shellapi.h>
#include "DBTrPrnmain.h"
#include "dbtreeqr.h"
//---------------------------------------------------------------------------
#pragma link "dxdbtree"
#pragma link "dxtree"
#pragma link "dxdbtree"
#pragma link "dxmdaset"
#pragma link "dxtree"
#pragma link "dxtrprds"
#pragma resource "*.dfm"
TFMain *FMain;
//---------------------------------------------------------------------------
__fastcall TFMain::TFMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFMain::ELevelsKeyPress(TObject *Sender, char &Key)
{
  if(((Key < '0') || (Key > '9')) && (Key != VK_BACK)){
    Key = 0;
    MessageBeep(0);
 }
}
//---------------------------------------------------------------------------
void __fastcall TFMain::ELevelsExit(TObject *Sender)
{
  if(ELevels->Text == "")
    ELevels->Text = "0";
  DBTreePrintDataSet->LevelCount = StrToInt(ELevels->Text);
}
//---------------------------------------------------------------------------
void __fastcall TFMain::Button1Click(TObject *Sender)
{
  if (RadioGroup->ItemIndex == 0)
    DBTreePrintDataSet->RootValue = "";
  else DBTreePrintDataSet->RootValue = Table->FindField("pr_id")->AsString;
  DBTreePrintDataSet->Open();
  QRListForm->QuickReport->Preview();
  DBTreePrintDataSet->Close();
}
//---------------------------------------------------------------------------

void __fastcall TFMain::DBTreeView1AddNewItem(TObject *Sender,
      TdxDBTreeNode *&DBTreeNode)
{
 DBTreeNode->ImageIndex = 2;
 DBTreeNode->SelectedIndex = 2;
}
//---------------------------------------------------------------------------
void __fastcall TFMain::FormCreate(TObject *Sender)
{
  Table->DatabaseName = ExtractFilePath(Application->ExeName);
  Table->Open();
}
//---------------------------------------------------------------------------

