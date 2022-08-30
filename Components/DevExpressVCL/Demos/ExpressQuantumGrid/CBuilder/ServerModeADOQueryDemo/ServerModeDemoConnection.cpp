//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ServerModeDemoConnection.h"
#include "ServerModeDemoData.h"
#include "ServerModeDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxProgressBar"
#pragma link "cxRadioGroup"
#pragma link "cxSpinEdit"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TServerModeDemoConnectionForm *ServerModeDemoConnectionForm;
//---------------------------------------------------------------------------
__fastcall TServerModeDemoConnectionForm::TServerModeDemoConnectionForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::AfterConstruction()
{
  TfmBaseForm::AfterConstruction();
  edDatabase->Text = GetDatabaseName();
  edOrdersTableName->Text = GetOrdersTableName();
  edCustomersTableName->Text = GetCustomersTableName();
  lbDescription->Caption = GetDescription();
  Caption = GetCaption();
  mDescription->Lines->LoadFromFile((ExtractFilePath(Application->ExeName) + "ConnectionFormDescription.txt"));
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::btAddRecordsAndStartDemoClick(
      TObject *Sender)
{
  ButtonsEnabled(false);
  Enabled = false;
  SetProgressBarPosition(this, 0);
  try
  {
	CreateDatabaseAndConnect();
	CreateTable();
	ServerModeDemoDataDM->AddRecords(seCount->Value, SetProgressBarPosition);
  }
  __finally
  {
	Enabled = true;
	SetProgressBarPosition(this, 0);
	btTestConnection->Enabled = True;
  }
  StartDemo();
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::btStartDemoClick(TObject *Sender)
{
  ButtonsEnabled(false);
  try
  {
	Connect(edDatabase->Text);
	StartDemo();
  }
  __finally
  {
	btTestConnection->Enabled = True;
  }
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::btTestConnectionClick(
	  TObject *Sender)
{
  int ACount;
  ShowHourglassCursor();
  try
  {
	lbCurrentCount->Caption = "";
	try
	{
	  Connect("master");
	}
	__except(EXCEPTION_EXECUTE_HANDLER)
	{
	  btAddRecordsAndStartDemo->Enabled = false;
	  btStartDemo->Enabled = false;
	}
  }
  __finally
  {
	HideHourglassCursor();
  }
  ACount = ServerModeDemoDataDM->GetRecordsCount();
  btAddRecordsAndStartDemo->Enabled = true;
  btStartDemo->Enabled = ACount > 0;
  if (ACount)
	lbCurrentCount->Caption = "Current record count = " + FormatFloat("#,###", ACount);
  MessageDlg(L"Successful connection.", mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::rgConnectUsingPropertiesChange(
	  TObject *Sender)
{
  edLoginName->Enabled = rgConnectUsing->ItemIndex > 0;
  edPassword->Enabled = edLoginName->Enabled;
}
//---------------------------------------------------------------------------
void TServerModeDemoConnectionForm::Connect(String ADatabaseName)
{
  ServerModeDemoDataDM->Connect(edSQLServer->Text, ADatabaseName, edLoginName->Text, edPassword->Text,
	rgConnectUsing->ItemIndex == 0);
}
//---------------------------------------------------------------------------
void TServerModeDemoConnectionForm::CreateDatabaseAndConnect()
{
  Connect("master");
  try
  {
	ServerModeDemoDataDM->CreateDatabase();
  }
  __except(EXCEPTION_EXECUTE_HANDLER)
  {
	throw(Exception("Cannot create a database " + GetDatabaseName()));
  }
  Connect(GetDatabaseName());
}
//---------------------------------------------------------------------------
void TServerModeDemoConnectionForm::CreateTable()
{
  try
  {
	ServerModeDemoDataDM->CreateTable();
  }
  __except(EXCEPTION_EXECUTE_HANDLER)
  {
	throw(Exception("Cannot create a tables " + GetOrdersTableName() + ";" + GetCustomersTableName() + "'"));
  }
}
//---------------------------------------------------------------------------
void __fastcall TServerModeDemoConnectionForm::SetProgressBarPosition(System::TObject *Sender, double Value)
{
  ProgressBar->Position = (int)Value;
  ProgressBar->Invalidate();
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void TServerModeDemoConnectionForm::ButtonsEnabled(Boolean AValue)
{
  btTestConnection->Enabled = AValue;
  btAddRecordsAndStartDemo->Enabled = AValue;
  btStartDemo->Enabled = AValue;
}
//---------------------------------------------------------------------------
void TServerModeDemoConnectionForm::StartDemo()
{
  ServerModeDemoMainForm->Initialize();
  ServerModeDemoMainForm->Show();
  Hide();
}

