//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DBUriStreamProvider.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

_di_IdxUriStreamService GetUriStreamService(TdxRichEditControl* ARichEdit)
{
  TdxCustomDocumentModel* AServiceProvider = ARichEdit->InnerControl->DocumentModel;
  _di_IdxServiceProvider AServiceProviderIntf;
  AServiceProvider->GetInterface(AServiceProviderIntf);
  TGUID AGUID = Comobj::StringToGUID("{079683F3-DB6C-4DE9-A0AE-8EBB45900199}");
  return (_di_IdxUriStreamService)AServiceProviderIntf->GetService(AGUID);
}

_di_IdxUriStreamProvider GetUriStreamProvider(TClientDataSet* ADataSet,
	UnicodeString AKey, UnicodeString AField, UnicodeString APrefix)
{
  TdxDBUriStreamProvider* AUriProvider = new TdxDBUriStreamProvider(ADataSet, AKey, AField, APrefix);
  _di_IdxUriStreamProvider AUriProviderIntf;
  AUriProvider->GetInterface(AUriProviderIntf);
  return AUriProviderIntf;
}

void RegistrationDBUriStreamProvider(TdxRichEditControl* ARichEdit, TClientDataSet* ADataSet,
	UnicodeString AKey, UnicodeString AField, UnicodeString APrefix)
{
  _di_IdxUriStreamService AService = GetUriStreamService(ARichEdit);
  _di_IdxUriStreamProvider AUriProviderIntf = GetUriStreamProvider(ADataSet, AKey, AField, APrefix);
  AService->RegisterProvider(AUriProviderIntf);
}

__fastcall TdxDBUriStreamProvider::TdxDBUriStreamProvider(TClientDataSet* ADataSet,
	UnicodeString AKey, UnicodeString AField, UnicodeString APrefix) : TCppInterfacedObject<IdxUriStreamProvider>()
{
  FDataSet = new TClientDataSet(NULL);
  LoadData(ADataSet);
  FField = AField;
  FKey = AKey;
  FPrefix = APrefix;
}

__fastcall TdxDBUriStreamProvider::~TdxDBUriStreamProvider()
{
  delete FDataSet;
}

void TdxDBUriStreamProvider::GetCloneData(TClientDataSet* ADataSet)
{
  ADataSet->CloneCursor(FDataSet, True);
}

void TdxDBUriStreamProvider::LoadData(TClientDataSet* ADataSet)
{
  FDataSet->CloneCursor(ADataSet, True);
}

TStream* __fastcall TdxDBUriStreamProvider::GetStream(const UnicodeString AUrl)
{
  TStream* Result = NULL;
  if (Pos(FPrefix, Trim(AUrl)) == 1)
  {
	int Id;
	TLocateOptions AOptions;
	UnicodeString AString = AUrl.Trim();
	AString = AString.SubString(9, AUrl.Length());
	if (!TryStrToInt(AString, Id))
	  return Result;
	TClientDataSet* ADataSet = new TClientDataSet(NULL);
	GetCloneData(ADataSet);
	if (ADataSet->Locate(FKey, Id, AOptions))
	{
	  TBlobField* AField = (TBlobField*)(ADataSet->FieldByName(FField));
	  Result = new TMemoryStream();
	  AField->SaveToStream(Result);
	  Result->Position = 0;
	}
	delete ADataSet;
  }
  return Result;
}

bool __fastcall TdxDBUriStreamProvider::IsAsyncLoadingSupported(const UnicodeString AUrl)
{
  return false;
}
