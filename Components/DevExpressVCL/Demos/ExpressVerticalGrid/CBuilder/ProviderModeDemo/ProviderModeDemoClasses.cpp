//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ProviderModeDemoClasses.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)


String TCustomer::GetDescription()
{
  return(FDescription);
}

void TCustomer::SetDescription(String Value)
{
  if(FDescription != Value)
    FDescription = Value;
}

String TCustomer::GetName()
{
  return(FName);
}

void TCustomer::SetName(String Value)
{
  if(FName != Value)
    FName = Value;
}
void TCustomer::SetID(int Value)
{
  if (FID != Value)
    FID = Value;
}

void TCustomerList::ReleaseAllCustomers()
{
  for(int i = 0; i < Count; i++)
    ReleaseCustomer(i);
  FList->Clear();
}

void TCustomerList::ReleaseCustomer(int AIndex)
{
  delete (TCustomer*) FList->Items[AIndex];
}

TCustomer* TCustomerList::GetCustomer(int AIndex)
{
  return (TCustomer*) FList->Items[AIndex];
}

int TCustomerList::GetCount()
{
  return(FList->Count);
}

TCustomerList::TCustomerList()
{
  FList = new TList();
  FNextID = 1;
}

TCustomerList::~TCustomerList()
{
  ReleaseAllCustomers();
  delete FList;
}

void TCustomerList::Clear()
{
  ReleaseAllCustomers();
}

int TCustomerList::Add(TCustomer* Customer)
{
  int Res;
  Res = FList->Add(Customer);
  FNextID++;
  return (Res);
}

void TCustomerList::Delete(int AIndex)
{
  ReleaseCustomer(AIndex);
  FList->Delete(AIndex);
}

void TCustomerList::Insert(int AIndex, TCustomer* Customer)
{
  FList->Insert(AIndex, Customer);
  FNextID++;
}

void __fastcall TCustomerDataSource::DeleteRecord(void * ARecordHandle)
{
  FCustomers->Delete((int)ARecordHandle);
  DataChanged();
  if(!Modified)
    FModified = true;
}

int __fastcall TCustomerDataSource::GetRecordCount(void)
{
  return(FCustomers->Count);
}

Variant __fastcall TCustomerDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  int AColumnId = GetDefaultItemID((int)AItemHandle);
  TCustomer* ACustomer = FCustomers->Customers[(int)ARecordHandle];
  Variant Result;
  switch (AColumnId) {
    case IndexOfID:
      Result = ACustomer->ID; break;
    case IndexOfName:
      Result = ACustomer->Name; break;
    case IndexOfDescription:
      Result = ACustomer->Description; break;
  }
  return (Result);
}

void * __fastcall TCustomerDataSource::InsertRecord(void * ARecordHandle)
{
  TCustomer* ACustomer = new TCustomer(FCustomers->NextID);
  FCustomers->Insert((int)ARecordHandle, ACustomer);
  TcxDataRecordHandle* Result = (TcxDataRecordHandle*)ARecordHandle;
  DataChanged();
  if(!Modified)
    FModified = true;
  return (Result);
}

void * __fastcall TCustomerDataSource::AppendRecord(void)
{
  TCustomer* ACustomer = new TCustomer(FCustomers->NextID);
  TcxDataRecordHandle* Result = (TcxDataRecordHandle*)FCustomers->Add(ACustomer);
  DataChanged();
  if (!Modified)
    FModified = true;
  return (Result);
}

void __fastcall TCustomerDataSource::SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue)
{
  int AColumnId = GetDefaultItemID((int)AItemHandle);
  TCustomer* ACustomer = FCustomers->Customers[(int)ARecordHandle];
  switch (AColumnId) {
    case IndexOfID:
      if (VarIsNull(AValue))
        ACustomer->ID = 0;
      else
	ACustomer->ID = AValue; 
      break;
    case IndexOfName:
      ACustomer->Name = VarToStr(AValue); break;
    case IndexOfDescription:
      ACustomer->Description = VarToStr(AValue); break;
  }
  if (!Modified)
    FModified = true;
}


