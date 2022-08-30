//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "UnboundSimpleDemoMain.h"
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
#pragma link "cxEditRepositoryItems"
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TUnboundSimpleDemoMainForm *UnboundSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TUnboundSimpleDemoMainForm::TUnboundSimpleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TUnboundSimpleDemoMainForm::CustomizeColumns()
{
  const int cDistance = 3;
  const int cPeriod = 4;
  const int cRadius = 7;
  dxFormatSettings->DecimalSeparator = '.';
  for(int i=0; i < tvPlanets->ColumnCount; i++)
    if((i == cDistance) || (i == cRadius))
      tvPlanets->Columns[i]->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxIntegerValueType);
    else
      if (i == cPeriod)
        tvPlanets->Columns[i]->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxFloatValueType);
      else
        tvPlanets->Columns[i]->DataBinding->ValueTypeClass = (TcxValueTypeClass)__classid(TcxStringValueType);
}
//---------------------------------------------------------------------------

void __fastcall TUnboundSimpleDemoMainForm::InitRecord(String const Str, int AInt, TStringList* AValues)
{
  Variant V;
#if __BORLANDC__ < 0x560
  Extended E;
#endif
  AValues->CommaText = Str;
  for(int j = 0; j < AValues->Count; j++)
    if(AValues->Strings[j] != "-") {
      V = AValues->Strings[j];
      if(!VarIsNull(V)) {
#if __BORLANDC__ < 0x560
        if(AnsiPos(DecimalSeparator, V) != 0) {
          if(TextToFloat(AValues->Strings[j].c_str(), &E, fvExtended))
            tvPlanets->DataController->Values[AInt][j] = E;
          else
            tvPlanets->DataController->Values[AInt][j] = V;
        }
        else
#endif
          tvPlanets->DataController->Values[AInt][j] = V;
      }
    }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundSimpleDemoMainForm::LoadData()
{
  const String AFileName = "nineplanets.txt";
  const int AHeaderLineCount = 2;

  if (!FileExists(AFileName))
    throw Exception("Data file not found");

  TStringList* ARecords = new TStringList();
  try {
    TStringList* AValues = new TStringList();
    try {
      tvPlanets->BeginUpdate();
      try {
        ARecords->LoadFromFile(AFileName);
        tvPlanets->DataController->RecordCount = ARecords->Count - AHeaderLineCount;
        for (int i=0; i < ARecords->Count - (AHeaderLineCount + 1) + 1; i++)
          InitRecord(ARecords->Strings[i + AHeaderLineCount], i, AValues);
      }
      __finally {
        tvPlanets->EndUpdate();
      }
    }
    __finally {
      delete AValues;
    }
  }
  __finally {
      delete ARecords;
  }    
}
//---------------------------------------------------------------------------

void __fastcall TUnboundSimpleDemoMainForm::SetFilter()
{
  tvPlanets->DataController->Filter->AddItem(NULL, tvPlanetsORBITS, foEqual, "Sun", "Sun");
  tvPlanets->DataController->Filter->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundSimpleDemoMainForm::FormCreate(TObject *Sender)
{
  tvPlanets->BeginUpdate();
  try{
    CustomizeColumns();
    LoadData();
    SetFilter();
  }
  __finally{
    tvPlanets->EndUpdate();
  }  
}
//---------------------------------------------------------------------------


