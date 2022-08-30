//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TreeEditmain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxdbtrel"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
  TTreeNode *tr, *tr1;

  mdCustomer->LoadFromBinaryFile("..\\..\\Data\\Customer.dat");
  mdCustomer->Open();
  mdCustomer->DisableControls();
  DBTreeViewEdit1->Items->Clear();
  while (! mdCustomer->Eof ){
    tr = DBTreeViewEdit1->GetTreeNodeByText(NULL, mdCustomer->FindField("country")->AsString, false);
    if(tr == NULL)
      tr = DBTreeViewEdit1->Items->Insert(NULL, mdCustomer->FindField("country")->AsString);
    if(mdCustomer->FindField("state")->AsString != ""){
      tr1 = DBTreeViewEdit1->GetTreeNodeByText(tr, mdCustomer->FindField("state")->AsString, false);
      if(tr1 == NULL)
        tr1 = DBTreeViewEdit1->Items->AddChild(tr, mdCustomer->FindField("state")->AsString);
      tr = tr1;
    }
	tr1 = DBTreeViewEdit1->GetTreeNodeByText(tr, mdCustomer->FindField("city")->AsString, false);
    if(tr1 == NULL)
      DBTreeViewEdit1->Items->AddChild(tr, mdCustomer->FindField("city")->AsString);
	mdCustomer->Next();
  }
  mdCustomer->First();
  mdCustomer->EnableControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DBTreeViewEdit1CloseUp(TObject *Sender, bool Accept)
{
  TTreeNode * tr;

  tr = DBTreeViewEdit1->Selected;
  if(Accept){
   mdCustomer->FindField("state")->AsString = "";
    while ((tr != NULL) && (tr->Parent != NULL)){
      tr = tr->Parent;
      if(tr->Level == 1)
        mdCustomer->FindField("state")->AsString = tr->Text;
      if(tr->Level == 0)
		mdCustomer->FindField("country")->AsString = tr->Text;
    }
  }
}
//---------------------------------------------------------------------------
