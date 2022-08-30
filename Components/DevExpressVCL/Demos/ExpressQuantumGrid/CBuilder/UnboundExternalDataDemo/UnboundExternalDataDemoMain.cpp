//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundExternalDataDemoMain.h"
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
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TUnboundExternalDataDemoMainForm *UnboundExternalDataDemoMainForm;
const String rsIniFileName = "odbcinst_test.ini";
//---------------------------------------------------------------------------
__fastcall TUnboundExternalDataDemoMainForm::TUnboundExternalDataDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::miInsertSectionClick(TObject *Sender)
{
  cxGrid->FocusedView->DataController->Insert();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::miDeleteSectionClick(TObject *Sender)
{
  cxGrid->FocusedView->DataController->DeleteFocused();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::miOpenClick(TObject *Sender)
{
  if (IniFile->Modified){
    int I = MessageDlg("Do you want to save the changes ?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
    switch (I) {
      case mrYes: IniFile->SaveValues(); break;
      case mrCancel: return;
    }
  }

  OpenDialog->InitialDir = ExtractFilePath(Application->ExeName);
  if(OpenDialog->Execute()) {
    Load(OpenDialog->FileName);
    UpdateFileInfo(OpenDialog->FileName);
    ResetChanges();
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::miSaveClick(TObject *Sender)
{
  IniFile->SaveValues();
  ResetChanges();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::miSaveAsClick(TObject *Sender)
{
  if(SaveDialog->Execute()) {
    IniFile->SaveAs(SaveDialog->FileName);
    UpdateFileInfo(SaveDialog->FileName);
    ResetChanges();
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::CustomizeGrid()
{
  GenerateColumns();
  LoadData();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::GenerateColumns()
{
  TcxGridColumn* GridColumn;

  GridColumn = tvSections->CreateColumn();
  GridColumn->Caption = "Section Name";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  GridColumn->Width = 600;

  tvSections->DataController->CustomDataSource = UserDataSource;

  GridColumn = tvValues->CreateColumn();
  GridColumn->Caption = "Parameter Name";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  GridColumn->DataBinding->Data = (TObject *)0;
  GridColumn->Width = 300;

  GridColumn = tvValues->CreateColumn();
  GridColumn->Caption = "Parameter Value";
  GridColumn->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
  GridColumn->DataBinding->Data = (TObject *)1;
  GridColumn->Width = 300;
  tvValues->DataController->CustomDataSource = UserDetailDataSource;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::LoadData()
{
  Load(IniFile->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::Load(String const AFileName)
{
  IniFile->Rename(AFileName, true);
  UserDataSource->DataChanged();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::UpdateFileInfo(String const AFileName)
{
  sbMain->Panels->Items[1]->Text = ExtractFileName(AFileName);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::ResetChanges()
{
  FChangesCount = 0;
  sbMain->Panels->Items[2]->Text = "Changes Count: 0";
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::DoSmthOnModify(TObject* Sender)
{
  FChangesCount++;
  sbMain->Panels->Items[2]->Text = Format(AnsiString("Changes Count: %d"), ARRAYOFCONST((FChangesCount)));
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::AfterConstruction(void)
{
  IniFile = new TUserIniFile(ExtractFilePath(Application->ExeName) + rsIniFileName);
  UpdateFileInfo(IniFile->FileName);
  IniFile->OnModify = DoSmthOnModify;
  UserDataSource = new TUserDataSource(IniFile);
  UserDetailDataSource = new TUserDetailDataSource(UserDataSource);

  CustomizeGrid();
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::FormCloseQuery(
      TObject *Sender, bool &CanClose)
{
  int i = -1;
  if(IniFile->Modified)
    i = MessageDlg("Do you want to save the changes ?", mtConfirmation, TMsgDlgButtons() << mbYes << mbNo << mbCancel, 0);
  switch (i) {
    case mrYes: IniFile->SaveValues(); break;
    case mrCancel: CanClose = false; break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::FormDestroy(
      TObject *Sender)
{
  delete UserDetailDataSource;
  delete UserDataSource;
  delete IniFile;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundExternalDataDemoMainForm::sbMainResize(
      TObject *Sender)
{
  sbMain->Panels->Items[0]->Width = Width - (sbMain->Panels->Items[1]->Width +
    sbMain->Panels->Items[2]->Width);
}
//---------------------------------------------------------------------------


