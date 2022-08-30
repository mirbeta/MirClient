//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include <Math.hpp>
#pragma hdrstop

#include "GridModeDemoData.h"
#include "GridModeDemoTerminate.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TGridModeDemoDataDM *GridModeDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TGridModeDemoDataDM::TGridModeDemoDataDM(TComponent* Owner)
  : TDataModule(Owner)
{
  FStartIDValue = 0;
  FEndIDValue = 0;
}
//---------------------------------------------------------------------------

void TGridModeDemoDataDM::ApplySortToQuery(TQuery *AQuery, TStrings *ASortList)
{
  String ASortString = "";
  for (int I = 0; I < ASortList->Count; I++)
    ASortString = ASortString + ASortList->Strings[I];
  ASortString.Delete(ASortString.Length() - 1, 2);
  try{
    AQuery->DisableControls();
    AQuery->Close();
    if (ASortString != "") ASortString = "order by " + ASortString;
    if (AQuery->SQL->Count > 1) AQuery->SQL->Strings[1] = ASortString;
    else AQuery->SQL->Add(ASortString);
  }
  __finally{
    AQuery->Open();
    AQuery->EnableControls();
  }
}
//---------------------------------------------------------------------------
bool TGridModeDemoDataDM::CopyDataFile(String APath, String AFileName)
{
  return(CopyFile(((AnsiString)(APath + AFileName)).c_str(), ((AnsiString)AFileName).c_str(), false));
}
//---------------------------------------------------------------------------
bool TGridModeDemoDataDM::CopyTableToCurrentDir(String ATableName)
{
  bool Result = false;
  TSearchRec ASearchRec;
  if (FindFirst(DataPath + ATableName + ".*", faAnyFile, ASearchRec) == 0){
	Result = CopyDataFile(DataPath, ASearchRec.Name);
	while (FindNext(ASearchRec) == 0)
	  Result = Result && CopyDataFile(DataPath, ASearchRec.Name);
	FindClose(ASearchRec);
  }
  return Result;
}
//---------------------------------------------------------------------------

bool TGridModeDemoDataDM::CopyDBToLocalPlace(bool ARewrite)
{
  bool Result = True;
  try{
    Screen->Cursor = crHourGlass;
    OpenTables(false);
    for (int I = 0; I < ARRAYSIZE(CarsTableNames); I++)
      if (ARewrite || !FileExists(CarsTableNames[I] + ".DB"))
        Result = Result &&
        	CopyTableToCurrentDir(CarsTableNames[I]);
  }
  __finally{
    OpenTables(true);
    Screen->Cursor = crDefault;
  }
  if (Result){
    FStartIDValue = 0;
    FEndIDValue = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoDataDM::qryAfterDelete(TDataSet *DataSet)
{
  ((TQuery*)DataSet)->ApplyUpdates();
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoDataDM::qryAfterPost(TDataSet *DataSet)
{
  int AKeyValue = -1;
  ((TQuery*)DataSet)->ApplyUpdates();
  if (FPrevDataSetState == dsInsert)
    try{
      DataSet->Close();
      qryHelper->SQL->Clear();
      qryHelper->SQL->Add("select MAX(ID) from " +
        GetTableNameByDataSet(DataSet));
      qryHelper->Open();
      AKeyValue = qryHelper->Fields->Fields[0]->AsInteger;
    }
    __finally{
      qryHelper->Close();
      DataSet->Open();
      DataSet->Locate("ID",AKeyValue , TLocateOptions());
    }
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoDataDM::qryBeforePost(TDataSet *DataSet)
{
  FPrevDataSetState = DataSet->State;
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoDataDM::qryCarsBeforeScroll(TDataSet* DataSet)
{
  Screen->Cursor = crHourGlass;
}
//---------------------------------------------------------------------------

void __fastcall TGridModeDemoDataDM::qryCarsAfterScroll(TDataSet* DataSet)
{
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------

void TGridModeDemoDataDM::ClearOrders()
{
  GridModeDemoTerminateForm->lbDesc->Caption = strDeleting;
  GridModeDemoTerminateForm->Show();
  try{
    Screen->Cursor = crHourGlass;
    Application->ProcessMessages();
    qryHelper->SQL->Clear();
    qryHelper->SQL->Add("delete from Orders ");
    qryHelper->SQL->Add("where ID > " + IntToStr(FStartIDValue));
    qryHelper->SQL->Add("and ID <= " + IntToStr(FEndIDValue));
    qryHelper->ExecSQL();
  }
  __finally{
    GridModeDemoTerminateForm->Close();
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------

String TGridModeDemoDataDM::GetTableNameByDataSet(TDataSet* DataSet)
{
  return DataSet->Name.SubString(4, DataSet->Name.Length() - 3);
}
//---------------------------------------------------------------------------

bool TGridModeDemoDataDM::IsOrdersPopulated()
{
  if (FEndIDValue > FStartIDValue) return true;
  qryHelper->SQL->Clear();
  qryHelper->SQL->Add("Select Count(ID) from Orders");
  qryHelper->Open();
  bool Result = (qryHelper->Fields->Fields[0]->AsInteger >= 100000);
  qryHelper->Close();
  if (Result) return true;
  return false;
}
//---------------------------------------------------------------------------

void TGridModeDemoDataDM::FillKeyValues(TList *AList, TDataSet *ADataSet, String AKeyFieldName)
{
#if (__BORLANDC__ >= 0x0610) // C++Builder 12
  TBookmark ABookmark = ADataSet->Bookmark;
#else
  TBookmarkStr ABookmark = ADataSet->Bookmark;
#endif
  try{
    ADataSet->DisableControls();
    ADataSet->First();
    int AKeyValue;
    while (!ADataSet->Eof){
      AKeyValue = Integer(ADataSet->FindField(AKeyFieldName)->Value);
      AList->Add((TObject*)AKeyValue);
      ADataSet->Next();
    }
  }
  __finally{
    ADataSet->Bookmark = ABookmark;
    ADataSet->EnableControls();
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoDataDM::qryOrdersPopulate()
{
  TList *ACarList = new TList();
  TList *ACustomersList = new TList();
  GridModeDemoTerminateForm->lbDesc->Caption = strInserting;
  GridModeDemoTerminateForm->Show();
  Application->ProcessMessages();
  FillKeyValues(ACarList, qryCars, "ID");
  FillKeyValues(ACustomersList, qryCustomers, "ID");
  const String APaymentTypes[] = {"AmEx","Cash","Visa","Master"};
  try{
    Screen->Cursor = crHourGlass;
    qryOrders->DisableControls();
    qryOrders->Close();
    qryHelper->SQL->Clear();
    qryHelper->SQL->Add("Select * from orders");
    qryHelper->Open();
    qryHelper->Last();
    FStartIDValue = qryHelper->FindField("ID")->Value;
    TDateTime ADateTime = Date();
    Randomize();
    int ARecordNo;
    for (int I = 0; I < 100000; I++){
      if ((I > 0) && ( div(I, 100).rem == 0) && FOnPopulateProgress != NULL)
        FOnPopulateProgress(this);
      qryHelper->Append();
      ARecordNo = random(ACustomersList->Count);
      qryHelper->FindField("CustomerID")->Value =
        (int)ACustomersList->Items[ARecordNo];
      ARecordNo = random(ACarList->Count);
      qryHelper->FindField("ProductID")->Value = (int)ACarList->Items[ARecordNo];
      qryHelper->FindField("PurchaseDate")->Value = ADateTime - random(1095);
      qryHelper->FindField("PaymentType")->Value = APaymentTypes[random(4)];
      qryHelper->FindField("PaymentAmount")->Value = 20000 + random(500000);
      qryHelper->Post();
    };
  }
  __finally{
    FEndIDValue = qryHelper->FindField("ID")->Value;
    qryHelper->Close();
    qryOrders->Open();
    qryOrders->EnableControls();
    Screen->Cursor = crDefault;
    GridModeDemoTerminateForm->Close();
  }
}
//---------------------------------------------------------------------------

void TGridModeDemoDataDM::OpenTables(bool AOpen)
{
  try{
    Screen->Cursor = crHourGlass;
    DataBase->Connected = false;
    qryCars->Active = AOpen;
    qryCustomers->Active = AOpen;
    qryOrders->Active = AOpen;
  }
  __finally{
    Screen->Cursor = crDefault;
  }
}
//---------------------------------------------------------------------------


