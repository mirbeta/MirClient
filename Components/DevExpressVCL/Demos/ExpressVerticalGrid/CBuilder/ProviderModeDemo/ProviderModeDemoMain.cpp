//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ProviderModeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxInplaceContainer"
#pragma link "cxNavigator"
#pragma link "cxVGrid"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TProviderModeDemoMainForm *ProviderModeDemoMainForm;

const String TabChar = "\t";
//---------------------------------------------------------------------------
__fastcall TProviderModeDemoMainForm::TProviderModeDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  CustomerList = new TCustomerList();
  GenerateColumns();
  LoadData();
  CustomerDataSource = new TCustomerDataSource(CustomerList);
  cxVirtualVerticalGrid->DataController->CustomDataSource =
    CustomerDataSource;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::FormDestroy(TObject *Sender)
{
  cxVirtualVerticalGrid->DataController->CustomDataSource = NULL;
  delete CustomerDataSource;
  delete CustomerList;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::FormCloseQuery(TObject *Sender,
  bool &CanClose)
{
  int i = -1;
  if (CustomerDataSource->Modified)
    i = MessageDlg("Do you want to save the changes ?", mtConfirmation, TMsgDlgButtons() <<mbYes << mbNo << mbCancel, 0);
  switch (i) {
    case mrYes: SaveData(); break;
    case mrCancel: CanClose = false; break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::miLinesClick(TObject *Sender)
{
    switch (((TMenuItem*)Sender)->Tag){
      case 0: cxVirtualVerticalGrid->OptionsView->GridLines = vglNone; break;
      case 1: cxVirtualVerticalGrid->OptionsView->GridLines = vglHorizontal; break;
      case 2: cxVirtualVerticalGrid->OptionsView->GridLines = vglVertical; break;
      case 3: cxVirtualVerticalGrid->OptionsView->GridLines = vglBoth; break;
    }
  ((TMenuItem*)Sender)->Checked = true;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::actCellAutoHeightExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  cxVirtualVerticalGrid->OptionsView->CellAutoHeight = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::actCellEndEllipsisExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  cxVirtualVerticalGrid->OptionsView->CellEndEllipsis = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::GenerateColumns()
{
    cxVirtualVerticalGrid->ClearRows();

    TcxEditorRow *ARow =
      (TcxEditorRow*)cxVirtualVerticalGrid->AddChild(NULL, __classid(TcxEditorRow));
    ARow->Properties->Caption = "ID";

    ARow =
      (TcxEditorRow*)cxVirtualVerticalGrid->AddChild(NULL, __classid(TcxEditorRow));
    ARow->Properties->Caption = "Customer";

    ARow =
      (TcxEditorRow*)cxVirtualVerticalGrid->AddChild(NULL, __classid(TcxEditorRow));
    ARow->Properties->Caption = "Company";
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::LoadData()
{
  TCustomer* ACustomer;
  AnsiString s;
  TStringList* StringList = new TStringList();
  try{
    StringList->LoadFromFile("contacts.txt");
    for(int i = 0; i < StringList->Count; i++) {
      ACustomer = new TCustomer(CustomerList->NextID);
      s = StringList->Strings[i];
      ACustomer->Name = s.SubString(1, s.Pos(TabChar) - 1);
      ACustomer->Description = s.SubString(s.Pos(TabChar) + 1, s.Length());
      CustomerList->Add(ACustomer);
    }
  }
   __finally {
     delete StringList;
   }
}
//---------------------------------------------------------------------------

void __fastcall TProviderModeDemoMainForm::SaveData()
{
  TCustomer* ACustomer;
  TStringList* StringList = new TStringList();
  try {
    for(int i = 0; i < CustomerList->Count; i++) {
       ACustomer = CustomerList->Customers[i];
       StringList->Add(ACustomer->Name + TabChar + ACustomer->Description);
    }
    StringList->SaveToFile("contacts.txt");
  }
  __finally {
    delete StringList;
  }
}
//---------------------------------------------------------------------------


