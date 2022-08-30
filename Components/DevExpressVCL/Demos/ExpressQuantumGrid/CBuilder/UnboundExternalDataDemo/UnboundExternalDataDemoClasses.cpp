#include "UnboundExternalDataDemoClasses.h"
#include "SysUtils.hpp"

String TUserIniFile::GetSectionName(int AIndex)
{
  return(Sections->Strings[AIndex]);
}
//---------------------------------------------------------------------------

void TUserIniFile::ReleaseSection(int AIndex)
{
  delete (TStrings*)Sections->Objects[AIndex];
}
//---------------------------------------------------------------------------

void TUserIniFile::RecreateSections()
{
  const String CR = "\n";
  String* S;
  TFileStream* FileStream = new TFileStream(FileName, fmCreate);
  try {
    for(int i=0; i < SectionCount(); i++) {
      FileStream->Seek(0, soFromEnd);
      S = new String();
      *S = Format((String)"%s[%s]%s", ARRAYOFCONST((CR, SectionNames[i], CR)));
      FileStream->WriteBuffer((const void *)S, S->Length());
      delete S;
    }
  }
  __finally {
    delete FileStream;
  }
}
//---------------------------------------------------------------------------

void TUserIniFile::RecreateValues()
{
  TStrings* AStrings;
  for(int i=0; i < SectionCount(); i++) {
    AStrings = (TStrings*)Sections->Objects[i];
    for(int j=0; j < AStrings->Count; j++)
      WriteString(SectionNames[i], AStrings->Names[j], AStrings->Values[AStrings->Names[j]]);
  };
}
//---------------------------------------------------------------------------

void TUserIniFile::SetModified(bool Value)
{
  if(OnModify)
    FOnModify(this);
  if (FModified != Value)
    FModified = Value;
}
//---------------------------------------------------------------------------

__fastcall TUserIniFile::TUserIniFile(const String FileName) : TIniFile(FileName)
{
  FSections = new TStringList();
}
//---------------------------------------------------------------------------

__fastcall TUserIniFile::~TUserIniFile()
{
  Clear();
  delete FSections;
}
//---------------------------------------------------------------------------

int TUserIniFile::SectionCount()
{
  return(Sections->Count);
}
//---------------------------------------------------------------------------

TStrings* TUserIniFile::InsertSection(int AIndex, String const Section, bool AAppend)
{
  TStringList* Result = new TStringList();
  try {
    if (AAppend)
      Sections->AddObject(Section, Result);
    else
      Sections->InsertObject(AIndex, Section, Result);
  }
  catch (Exception &err) {
    delete Result;
    Result = NULL;
  };
  return (Result);
}
//---------------------------------------------------------------------------

void TUserIniFile::ChangeSectionName(int AIndex, String const Section)
{
  Sections->Strings[AIndex] = Section;
}
//---------------------------------------------------------------------------

void TUserIniFile::DeleteSection(int AIndex)
{
  ReleaseSection(AIndex);
  Sections->Delete(AIndex);
}
//---------------------------------------------------------------------------

void TUserIniFile::Clear()
{
  for(int i=0; i < SectionCount(); i++)
    ReleaseSection(i);
  Sections->Clear();
}
//---------------------------------------------------------------------------

void TUserIniFile::ClearObjects()
{
  for(int i=0; i < Sections->Count; i++)
    delete (TStringList*)Sections->Objects[i];
  Sections->Clear();
}
//---------------------------------------------------------------------------

void TUserIniFile::LoadValues()
{
  TStringList* ASections, *AValues;
  if ((FileName != "") && FileExists(FileName)) {
    ClearObjects();
    ASections = new TStringList();
    try {
      ReadSections(ASections);
      for(int i=0; i < ASections->Count; i++) {
        AValues = new TStringList();
        ReadSectionValues(ASections->Strings[i], AValues);
        Sections->AddObject(ASections->Strings[i], (TObject*)AValues);
      };
    }
    __finally {
      delete ASections;
    };
  };
}
//---------------------------------------------------------------------------

void TUserIniFile::SaveValues()
{
  RecreateSections();
  RecreateValues();
  Modified = false;
}
//---------------------------------------------------------------------------

void TUserIniFile::SaveAs(String const AFileName)
{
  Rename(AFileName, False);
  SaveValues();
  Modified = False;
}
//---------------------------------------------------------------------------

void TUserIniFile::Rename(String const AFileName, bool Reload)
{
  String *PS = &FileName;
  *PS = AFileName;
  if(Reload)
    LoadValues();
}
//---------------------------------------------------------------------------

int __fastcall TUserDataSource::GetRecordCount(void)
{
  return (DataIniFile->Sections->Count);
}
//---------------------------------------------------------------------------

Variant __fastcall TUserDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  return(DataIniFile->Sections->Strings[(int)ARecordHandle]);
}
//---------------------------------------------------------------------------

void __fastcall TUserDataSource::SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue)
{
  DataIniFile->ChangeSectionName((int)ARecordHandle, VarAsType(AValue, varOleStr));
  DataIniFile->Modified = true;
}
//---------------------------------------------------------------------------

void * __fastcall TUserDataSource::AppendRecord(void)
{
  DataIniFile->InsertSection(-1, "...", true);
  DataIniFile->Modified = true;
  void * Result = (void*)(DataIniFile->Sections->Count-1);
  DataChanged();
  return (Result);
}
//---------------------------------------------------------------------------

void * __fastcall TUserDataSource::InsertRecord(void * ARecordHandle)
{
  DataIniFile->InsertSection(Integer(ARecordHandle), "...");
  DataIniFile->Modified = true;
  void * Result = ARecordHandle;
  DataChanged();
  return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TUserDataSource::DeleteRecord(void * ARecordHandle)
{
  DataIniFile->DeleteSection((int)ARecordHandle);
  DataIniFile->Modified = true;
  DataChanged();
}
//---------------------------------------------------------------------------

int __fastcall TUserDetailDataSource::GetRecordCount(void)
{
  int Result = 0;
  if (GetMasterRecordIndex() >= 0)
    Result = ((TStrings*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()])->Count;
  return (Result);
}
//---------------------------------------------------------------------------

Variant __fastcall TUserDetailDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  int AColumnId = GetDefaultItemID(int(AItemHandle));
  TStrings* AStrings = (TStrings*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()];
  Variant Result;
  if(AColumnId)
    Result = AStrings->Values[AStrings->Names[(int)ARecordHandle]];
  else
    Result = AStrings->Names[(int)ARecordHandle];
  return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TUserDetailDataSource::SetValue(void * ARecordHandle, void * AItemHandle, const Variant &AValue)
{
  String S1, S2;
  int AColumnId = GetDefaultItemID(int(AItemHandle));
  TStrings* AStrings = (TStrings*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()];
  if (AColumnId == 0) { // set name
    if (VarIsNull(AValue))
      S1 = "";
    else S1 = AValue;
    S2 = AStrings->Values[AStrings->Names[(int)ARecordHandle]];
  }
  else { // set value
    S1 = AStrings->Names[(int)ARecordHandle];
    if(VarIsNull(AValue))
      S2 = "";
    else S2 = AValue;
  };
  AStrings->Strings[(int)ARecordHandle]= Format((String)"%S=%S", ARRAYOFCONST((S1,S2)));
  MasterDataSource->DataIniFile->Modified = true;
}

//---------------------------------------------------------------------------

void * __fastcall TUserDetailDataSource::AppendRecord(void)
{
  TStrings* AStrings = (TStrings*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()];
  void * Result = (TcxDataRecordHandle*)AStrings->Add("");
  MasterDataSource->DataIniFile->Modified = true;
  DataChanged();
  return (Result);
}
//---------------------------------------------------------------------------

void * __fastcall TUserDetailDataSource::InsertRecord(void * ARecordHandle)
{
  TStrings* AStrings = (TStrings*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()];
  AStrings->Insert((int)ARecordHandle, "");
  MasterDataSource->DataIniFile->Modified = true;
  void * Result = ARecordHandle;
  DataChanged();
  return (Result);
}
//---------------------------------------------------------------------------

void __fastcall TUserDetailDataSource::DeleteRecord(void * ARecordHandle)
{
  TStrings* AStrings = (TStringList*)MasterDataSource->DataIniFile->Sections->Objects[GetMasterRecordIndex()];
  AStrings->Delete((int)ARecordHandle);
  MasterDataSource->DataIniFile->Modified = true;
  DataChanged();
}
//---------------------------------------------------------------------------

int TUserDetailDataSource::GetMasterRecordIndex()
{
  return (DataController->GetMasterRecordIndex());
}
//---------------------------------------------------------------------------


