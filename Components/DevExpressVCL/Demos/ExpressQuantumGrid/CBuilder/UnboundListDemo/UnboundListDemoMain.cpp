//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundListDemoMain.h"
#include "cxDataStorage.hpp"
#include "AboutDemoForm.h"
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
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridPopupMenu"
#pragma link "cxDataStorage"
#pragma link "cxLookAndFeels"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TUnboundListDemoMainForm *UnboundListDemoMainForm;
const String TabChar = "\t";

//---------------------------------------------------------------------------
__fastcall TUnboundListDemoMainForm::TUnboundListDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TUnboundListDemoMainForm::FormCreate(TObject *Sender)
{
  CustomerList = new TCustomerList();
  CustomerDataSource = new TCustomerDataSource(CustomerList);
  CustomizeGrid();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundListDemoMainForm::FormDestroy(TObject *Sender)
{
  delete CustomerList;
  delete CustomerDataSource;
}

//---------------------------------------------------------------------------
void __fastcall TUnboundListDemoMainForm::GenerateColumns()
{
  TcxGridColumn* GridColumn;
  tvCustomers->ClearItems();

  GridColumn = tvCustomers->CreateColumn();
  GridColumn->Caption = "ID";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxIntegerValueType);
  GridColumn->Width = 50;

  GridColumn = tvCustomers->CreateColumn();
  GridColumn->Caption = "Customer";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  GridColumn->Width = 200;

  GridColumn = tvCustomers->CreateColumn();
  GridColumn->Caption = "Company";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  GridColumn->Width = 200;

  tvCustomers->DataController->CustomDataSource = CustomerDataSource;
}

//---------------------------------------------------------------------------
void __fastcall TUnboundListDemoMainForm::LoadData()
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
  CustomerDataSource->DataChanged();
  tvCustomers->DataController->GotoFirst();
}

//---------------------------------------------------------------------------
void __fastcall TUnboundListDemoMainForm::CustomizeGrid()
{
  GenerateColumns();
  LoadData();
}

//---------------------------------------------------------------------------
void __fastcall TUnboundListDemoMainForm::SaveData()
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
void __fastcall TUnboundListDemoMainForm::FormCloseQuery(TObject *Sender,
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

