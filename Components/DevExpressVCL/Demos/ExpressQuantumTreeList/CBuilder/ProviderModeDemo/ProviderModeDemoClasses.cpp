//---------------------------------------------------------------------------


#pragma hdrstop

#include "ProviderModeDemoClasses.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

//---------------------------------------------------------------------------

void RecreateDemoDataSource(TcxVirtualTreeList *ATreeList)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  TObject *ADataSource = ATreeList->CustomDataSource;
  ATreeList->CustomDataSource = NULL;
  if (ADataSource != NULL)
    delete ADataSource;
  if (ATreeList->OptionsData->SmartLoad)
    ATreeList->CustomDataSource = new TcxSmartLoadDemoDataSource();
  else
    ATreeList->CustomDataSource = new TcxLoadAllRecordsDemoDataSource();

//*/
}

//---------------------------------------------------------------------------

__fastcall TcxProviderRecordHandle::TcxProviderRecordHandle(
  TcxProviderRecordHandle *AParent)
{
  FParent = AParent;
  FDataLoaded = false;
  FChildList = new TList();
  if (AParent != NULL)
	Parent->AddExistChild(this);
  if (AParent != NULL)
	FChildLevelCount = AParent->ChildLevelCount - 1;
  else
	FChildLevelCount = cxProviderDemoLevelCount;
}
//---------------------------------------------------------------------------

__fastcall TcxProviderRecordHandle::~TcxProviderRecordHandle()
{
  __try{
	DeleteChildren();
  }
  __finally{
	delete FChildList;
    FChildList = NULL;
	if ((Parent != NULL) && !Parent->Deletion)
	  Parent->RemoveChild(this);
  }
}
//---------------------------------------------------------------------------

TcxProviderRecordHandle* __fastcall TcxProviderRecordHandle::Add()
{
  return new TcxProviderRecordHandle(Parent);
}
//---------------------------------------------------------------------------

TcxProviderRecordHandle* __fastcall TcxProviderRecordHandle::AddChild()
{
  return new TcxProviderRecordHandle(this);
}
//---------------------------------------------------------------------------

void __fastcall TcxProviderRecordHandle::AddExistChild(
  TcxProviderRecordHandle* ARecordHandle)
{
  FChildList->Add(ARecordHandle);
}
//---------------------------------------------------------------------------

void __fastcall TcxProviderRecordHandle::DeleteChildren()
{
  FDeletion = true;
  __try{
	for (int I = Count - 1; I = 0 ; I--)
	  delete (TcxProviderRecordHandle*)FChildList->Items[I]; 
  }
  __finally{
	FChildList->Clear();
	FDeletion = false;
  }
}
//---------------------------------------------------------------------------

int __fastcall TcxProviderRecordHandle::GetChildIndex(
  TcxProviderRecordHandle* ARecordHandle)
{
  return FChildList->IndexOf(ARecordHandle);
}
//---------------------------------------------------------------------------

void __fastcall TcxProviderRecordHandle::NodeMoveTo(
  TcxProviderRecordHandle *AttachRecordHandle,
  TcxTreeListNodeAttachMode AttachMode, bool IsCopy)
{
  switch (AttachMode) {
    case tlamAdd:
    case tlamAddFirst:
    case tlamInsert:
      ChangeParent(AttachRecordHandle->Parent);
      break;
    case tlamAddChild:
    case tlamAddChildFirst:
      ChangeParent(AttachRecordHandle);
      break;
  }
}

void __fastcall TcxProviderRecordHandle::ChangeParent(TcxProviderRecordHandle *AParent)
{
  if (FParent != AParent){
    FParent->RemoveChild(this);
    AParent->AddExistChild(this);
    FParent = AParent;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxProviderRecordHandle::RemoveChild(
  TcxProviderRecordHandle* ARecordHandle)
{
  FChildList->Remove(ARecordHandle);
}
//---------------------------------------------------------------------------

int __fastcall TcxProviderRecordHandle::GetCount()
{
  return FChildList->Count;
}
//---------------------------------------------------------------------------

TcxProviderRecordHandle* __fastcall TcxProviderRecordHandle::GetItem(int Index)
{
  return (TcxProviderRecordHandle*)FChildList->Items[Index];
}
//---------------------------------------------------------------------------

int __fastcall TcxProviderRecordHandle::GetIndex()
{
  if (Parent != NULL)
    return Parent->GetChildIndex(this);
  else
    return -1;
}
//---------------------------------------------------------------------------

int __fastcall TcxProviderRecordHandle::GetLevel()
{
  int Result = -1;
  TcxProviderRecordHandle *AParent = Parent;
  while (AParent != NULL){
    AParent = AParent->Parent;
    Result ++;
  }
  return Result;
}
//---------------------------------------------------------------------------

int __fastcall TcxProviderRecordHandle::GetTotalCount()
{
  int Result = Count;
  for (int I = 0; I < Count; I++)
    Result += Items[I]->TotalCount;
  return Result;
}
//---------------------------------------------------------------------------


/*TcxCustomDemoDataSource*/

__fastcall TcxCustomDemoDataSource::TcxCustomDemoDataSource(void)
{
  FRootHandle = new TcxProviderRecordHandle(NULL);
}
//---------------------------------------------------------------------------

__fastcall TcxCustomDemoDataSource::~TcxCustomDemoDataSource(void)
{
  delete FRootHandle;
}
//---------------------------------------------------------------------------

void * __fastcall TcxCustomDemoDataSource::AppendRecord(void)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  TcxDataRecordHandle Result = InsertRecordHandle(RootHandle, true);
  ((TcxProviderRecordHandle*)Result)->DataLoaded = true;
  DataChanged();
  return Result;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TcxCustomDemoDataSource::DeleteRecord(void * ARecordHandle)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  delete (TcxProviderRecordHandle*)ARecordHandle;
  DataChanged();

//*/
}
//---------------------------------------------------------------------------

Variant __fastcall TcxCustomDemoDataSource::GetValue(
  void * ARecordHandle, void * AItemHandle)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  TcxProviderRecordHandle *AHandle = (TcxProviderRecordHandle*)ARecordHandle;
  Variant Result;
  switch ((int)AItemHandle){
    case 0:
      Result = AHandle->IntValue;
      break;
    case 1:
      Result = AHandle->Text;
      break;
    case 2:
      Result = AHandle->Date;
      break;
    case 3:
      if (AHandle->Parent == NULL)
        Result = -1;
      else
        Result = AHandle->Parent->IntValue;
  }
  return Result;

//*/
}
//---------------------------------------------------------------------------

void * __fastcall TcxCustomDemoDataSource::InsertRecord(void * ARecordHandle)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  TcxDataRecordHandle Result =
    InsertRecordHandle((TcxProviderRecordHandle*)ARecordHandle, false);
  ((TcxProviderRecordHandle*)Result)->DataLoaded = true;
  DataChanged();
  return Result;

//*/
}
//---------------------------------------------------------------------------

TcxProviderRecordHandle* __fastcall TcxCustomDemoDataSource::InsertRecordHandle(
  TcxProviderRecordHandle *AParentHandle, bool AIsChild)
{
  FID++;
  TcxProviderRecordHandle* Result;
  if (AIsChild)
    Result = AParentHandle->AddChild();
  else
    Result = AParentHandle->Add();
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TcxCustomDemoDataSource::NodeMoveTo(void * ARecordHandle,
  void * AttachRecordHandle, Cxtl::TcxTreeListNodeAttachMode AttachMode, bool IsCopy)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  if (IsCopy){
    TcxProviderRecordHandle* AProviderRecordHandle =
      InsertRecordHandle((TcxProviderRecordHandle*)AttachRecordHandle,
        (AttachMode == tlamAddChild || AttachMode == tlamAddChildFirst));
    AProviderRecordHandle->IntValue =
      ((TcxProviderRecordHandle*)ARecordHandle)->IntValue;
    AProviderRecordHandle->Date = ((TcxProviderRecordHandle*)ARecordHandle)->Date;
    AProviderRecordHandle->Text = ((TcxProviderRecordHandle*)ARecordHandle)->Text;
  }
  else
    ((TcxProviderRecordHandle*)ARecordHandle)->NodeMoveTo(
      (TcxProviderRecordHandle*)AttachRecordHandle, AttachMode, IsCopy);
  DataChanged();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TcxCustomDemoDataSource::SetValue(void * ARecordHandle,
  void * AItemHandle, const Variant &AValue)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  TcxProviderRecordHandle *ProviderRecordHandle =
    (TcxProviderRecordHandle*)ARecordHandle;
  switch ((int)AItemHandle){
    case 0:
      ProviderRecordHandle->IntValue = AValue;
      break;
    case 1:
      ProviderRecordHandle->Text = AValue;
      break;
    case 2:
      ProviderRecordHandle->Date = VarToDateTime(AValue);
  }

//*/
}
//---------------------------------------------------------------------------

void __fastcall TcxCustomDemoDataSource::GenerateChildRecords(
  TcxProviderRecordHandle *AParentHandle)
{
  for (int I = 0; I < cxProviderDemoRecordsPerLevel; I++){
    TcxProviderRecordHandle *ProviderRecordHandle =
      InsertRecordHandle(AParentHandle, true);
    ProviderRecordHandle->IntValue = FID;
    ProviderRecordHandle->Text = "Text" + IntToStr(ProviderRecordHandle->IntValue);
    ProviderRecordHandle->Date = Now() + ProviderRecordHandle->IntValue * 0.001;
  }
  AParentHandle->DataLoaded = true;
}
//---------------------------------------------------------------------------

/*TcxSmartLoadDemoDataSource */

__fastcall TcxSmartLoadDemoDataSource::TcxSmartLoadDemoDataSource(void):TcxCustomDemoDataSource()
{
  GenerateChildRecords(FRootHandle);
}
//---------------------------------------------------------------------------

void * __fastcall TcxSmartLoadDemoDataSource::AppendRecord(void)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  int AIndex = DataController->FocusedRecordIndex;
  TcxDataRecordHandle Result;
  if (AIndex == -1)
    Result = RootHandle;
  else
    Result = ((TcxProviderRecordHandle*)GetRecordHandleByIndex(AIndex))->Parent;
  Result = InsertRecordHandle((TcxProviderRecordHandle*)Result, true);
  ((TcxProviderRecordHandle*)Result)->DataLoaded = true;
  DataChanged();
  return Result;

//*/
}
//---------------------------------------------------------------------------

int __fastcall TcxSmartLoadDemoDataSource::GetChildCount(
  void * AParentHandle)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  return GetCountFromItem((TcxProviderRecordHandle*)AParentHandle);

//*/
}

int __fastcall TcxSmartLoadDemoDataSource::GetCountFromItem(
  TcxProviderRecordHandle *AItem)
{
  int Result = AItem->Count;
  if (!AItem->DataLoaded){
    if (AItem->ChildLevelCount > 0)
      Result += cxProviderDemoRecordsPerLevel;
    else
      AItem->DataLoaded = true;
  }
  return Result;
}
//---------------------------------------------------------------------------

void * __fastcall TcxSmartLoadDemoDataSource::GetChildRecordHandle(
  void * AParentHandle, int AChildIndex)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  return GetChildItemHandle((TcxProviderRecordHandle*)AParentHandle, AChildIndex);

//*/
}

TcxDataRecordHandle __fastcall TcxSmartLoadDemoDataSource::GetChildItemHandle(
  TcxProviderRecordHandle *AItem, int AChildIndex)
{
  if (!AItem->DataLoaded)
    GenerateChildRecords(AItem);
  return AItem->Items[AChildIndex];
}
//---------------------------------------------------------------------------

void * __fastcall TcxSmartLoadDemoDataSource::GetRootRecordHandle(void)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  return FRootHandle;

//*/
}
//---------------------------------------------------------------------------


/*TcxLoadAllRecordsDemoDataSource */

__fastcall TcxLoadAllRecordsDemoDataSource::TcxLoadAllRecordsDemoDataSource(void):
  TcxCustomDemoDataSource()
{
  FRecordsList = new TList;
  CreateAllRecords();
}
//---------------------------------------------------------------------------

__fastcall TcxLoadAllRecordsDemoDataSource::~TcxLoadAllRecordsDemoDataSource(void)
{
  delete FRecordsList;
}
//---------------------------------------------------------------------------

void __fastcall TcxLoadAllRecordsDemoDataSource::DeleteRecord(
  void * ARecordHandle)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  FRecordsList->Remove(ARecordHandle);
  TcxCustomDemoDataSource::DeleteRecord(ARecordHandle);

//*/
}
//---------------------------------------------------------------------------

int __fastcall TcxLoadAllRecordsDemoDataSource::GetRecordCount(void)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  return FRecordsList->Count;

//*/
}
//---------------------------------------------------------------------------

void * __fastcall TcxLoadAllRecordsDemoDataSource::GetParentRecordHandle(void * ARecordHandle)
{
  return ((TcxProviderRecordHandle*)ARecordHandle)->Parent;
}
//---------------------------------------------------------------------------

void * __fastcall TcxLoadAllRecordsDemoDataSource::GetRecordHandle(
  int ARecordIndex)
{
/* remove/add the closing slash on this line to disable/enable the following code*/

  return FRecordsList->Items[ARecordIndex];

//*/
}
//---------------------------------------------------------------------------

TcxProviderRecordHandle* __fastcall TcxLoadAllRecordsDemoDataSource::InsertRecordHandle(
  TcxProviderRecordHandle *AParentHandle, bool AIsChild)
{
  TcxProviderRecordHandle *Result =
    TcxCustomDemoDataSource::InsertRecordHandle(AParentHandle, AIsChild);
  FRecordsList->Add(Result);
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TcxLoadAllRecordsDemoDataSource::CreateAllRecords()
{
  DoCreateRecords(RootHandle, 1);
}

void __fastcall TcxLoadAllRecordsDemoDataSource::DoCreateRecords(
  TcxProviderRecordHandle *AParent, int ALevel)
{
  if (ALevel > cxProviderDemoLevelCount) return;
  GenerateChildRecords(AParent);
  for (int I = 0; I < AParent->Count; I++)
    DoCreateRecords(AParent->Items[I], ALevel + 1);
}
//---------------------------------------------------------------------------

