//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnboundExternalMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomPivotGrid"
#pragma link "cxDBPivotGrid"
#pragma link "cxPivotGrid"
#pragma resource "*.dfm"
TfrmUnboundExternal *frmUnboundExternal;
//---------------------------------------------------------------------------
__fastcall TfrmUnboundExternal::TfrmUnboundExternal(TComponent* Owner)
        : TfrmDemoBasicMain(Owner)
{
}

void __fastcall TfrmUnboundExternal::FormCreate(TObject* Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  TcxExternalDataSource* ExternalData = new TcxExternalDataSource("ExternalData.dat");
  PivotGrid()->BeginUpdate();
  try {
    for (int I = 0; I < ExternalData->FieldCount; I++) {
      TcxPivotGridField* AField = PivotGrid()->CreateField();
      AField->Caption = ExternalData->FieldNames[I];
      AField->DataBinding->ValueType = ExternalData->FieldTypes[I];
      AField->Visible = True;
    }
    SetFieldPos("PaymentType", faColumn);
    SetFieldPos("Payment Amount", faData);
    SetFieldPos("Quantity", faData);
    SetFieldPos("Company Name", faRow);
    SetFieldPos("Car Name", faRow);
    PivotGrid()->DataController->CustomDataSource = ExternalData;
  }
  __finally {
    PivotGrid()->EndUpdate();
    PivotGrid()->ApplyBestFit();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmUnboundExternal::FormDestroy(TObject *Sender)
{
  delete ExternalData;
}
//---------------------------------------------------------------------------

TcxCustomPivotGrid* __fastcall TfrmUnboundExternal::PivotGrid()
{
  return UnboundPivotGrid;
}
//---------------------------------------------------------------------------

void __fastcall TfrmUnboundExternal::SetFieldPos(
  const String AFieldName, TcxPivotGridFieldArea AArea)
{
  TcxPivotGridField* AField = PivotGrid()->GetFieldByName(AFieldName);
  AField->Area = AArea;
}

//  TcxExternalDataSource

String TcxExternalDataSource::GetFieldName(int AIndex)
{
  return GetValueFromString(Records->Strings[0], AIndex);
}

String TcxExternalDataSource::GetFieldType(int AIndex)
{
  return GetValueFromString(Records->Strings[1], AIndex);
}

__fastcall TcxExternalDataSource::TcxExternalDataSource(const String FileName)
  : TcxCustomDataSource()
{
  Records = new TStringList();
  Records->LoadFromFile(FileName);
  AfterLoad();
}

__fastcall TcxExternalDataSource::~TcxExternalDataSource(void)
{
  delete Records;
}

void __fastcall TcxExternalDataSource::AfterLoad()
{
  String AItem;
  String S = Records->Strings[0];
  FFieldCount = 0;
  while (true) {
    FFieldCount = FFieldCount + 1;
    AItem = GetItemFromStr(S);
    S = S.SubString(AItem.Length() + 2, MaxInt);
    if ((AItem.Length() == 0) || (S.Length() == 0)) break;
  }
}

int __fastcall TcxExternalDataSource::GetRecordCount()
{
  return Records->Count - 2;
}

Variant __fastcall TcxExternalDataSource::GetValue(void * ARecordHandle, void * AItemHandle)
{
  Variant AResult = GetValueFromString(Records->Strings[(int)ARecordHandle + 2], (int)AItemHandle);
  return  AResult.AsType(GetVarTypeByName(FieldTypes[(int)AItemHandle]));
}

Variant __fastcall TcxExternalDataSource::GetValueFromString(String ARecord, int AIndex)
{
  String S = "";
  for (int I = 0; I <= AIndex; I++) {
    S = GetItemFromStr(ARecord);
    ARecord = ARecord.SubString(S.Length() + 2, MaxInt);
  }
  return S;
}

int __fastcall TcxExternalDataSource::GetVarTypeByName(const String AName)
{
  int AResult;
  if (SameText(AName, "Currency"))
    AResult = varCurrency;
  else
    if (SameText(AName, "DateTime"))
      AResult = varDate;
    else
      if (SameText(AName, "Integer"))
        AResult = varInteger;
      else
        AResult = varString;
  return AResult;
}

String __fastcall TcxExternalDataSource::GetItemFromStr(const String AString)
{
  int ACount = 0;
  for (int I = 1; I <= AString.Length(); I++) {
    if ((AString[I] == '\t') || (AString[I] == '\n') || (AString[I] == '\r'))
      break;
    else
      ACount = ACount + 1;
  }
  return AString.SubString(1, ACount);
}

//---------------------------------------------------------------------------


