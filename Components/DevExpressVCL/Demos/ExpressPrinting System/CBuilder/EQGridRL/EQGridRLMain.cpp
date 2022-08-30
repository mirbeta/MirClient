//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "EQGridRLMain.h"
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
#pragma link "dxPSCore"
#pragma link "dxPScxGridLnk"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "cxGridCustomView"
#pragma link "cxDataStorage"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxDrawTextUtils"
#pragma link "DemoBasicMain"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "dxPScxCommon"
#pragma resource "*.dfm"
TEQGridRLMainForm *EQGridRLMainForm;
//---------------------------------------------------------------------------
__fastcall TEQGridRLMainForm::TEQGridRLMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actLookAndFeelKindUltraFlatExecute(TObject *Sender)
{
  if(cxGrid->LookAndFeel->Kind != lfUltraFlat){
    cxGrid->LookAndFeel->Kind = lfUltraFlat;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actLookAndFeelKindFlatExecute(TObject *Sender)
{
  if(cxGrid->LookAndFeel->Kind != lfFlat){
    cxGrid->LookAndFeel->Kind = lfFlat;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actLookAndFeelKindStandardExecute(TObject *Sender)
{
  if(cxGrid->LookAndFeel->Kind != lfStandard){
    cxGrid->LookAndFeel->Kind = lfStandard;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::miNativeStyleClick(TObject *Sender)
{
  cxGrid->LookAndFeel->NativeStyle = !cxGrid->LookAndFeel->NativeStyle;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------



void __fastcall TEQGridRLMainForm::actShowDemoDescriptionExecute(
      TObject *Sender)
{
  lbDescrip->Visible = !lbDescrip->Visible;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actGridNativeStyleExecute(
      TObject *Sender)
{
  cxGrid->LookAndFeel->NativeStyle = !cxGrid->LookAndFeel->NativeStyle;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actDownloadsExecute(TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://www.devexpress.com/downloads/index.asp", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actForumExecute(TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://24.234.251.34/dxforum/dxforumisapi.dll/", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actDXOnTheWebExecute(
      TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://www.devexpress.com/index.shtm", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actProductsExecute(TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://www.devexpress.com/products/index.asp", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::AlwaysEnabled(TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actExitExecute(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------


void __fastcall TEQGridRLMainForm::CustomizeColumns()
{
  const int cDistance = 3;
  const int cPeriod = 4;
  const int cRadius = 7;
  dxFormatSettings->DecimalSeparator = '.';
  for(int i=0; i < tvPlanets->ColumnCount; i++)
    if((i == cDistance) || (i == cRadius))
      tvPlanets->Columns[i]->DataBinding->ValueTypeClass = __classid(TcxIntegerValueType);
    else
      if (i == cPeriod)
        tvPlanets->Columns[i]->DataBinding->ValueTypeClass = __classid(TcxFloatValueType);
      else
        tvPlanets->Columns[i]->DataBinding->ValueTypeClass = __classid(TcxStringValueType);
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::InitRecord(String const Str, int AInt, TStringList* AValues)
{
  Variant V;
  AValues->CommaText = Str;
  for(int i=0; i < AValues->Count; i++)
   if (AValues->Strings[i] != "-") {
     V = AValues->Strings[i];
      if (!VarIsNull(V)) {
        if (tvPlanets->Columns[i]->DataBinding->ValueTypeClass == __classid(TcxFloatValueType))
          V = VarAsType(StrToFloat(V), varDouble);
        tvPlanets->DataController->Values[AInt][i] = V;
      }
   }
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::LoadData()
{
  const String AFileName = "nineplanets.txt";
  const int AHeaderLineCount = 2;

  if (!FileExists(AFileName))
    throw Exception("Data file not found");

  TStringList* ARecords = new TStringList();
  TStringList* AValues = new TStringList();
  try {
    ARecords->LoadFromFile(AFileName);
    tvPlanets->BeginUpdate();
    tvPlanets->DataController->RecordCount = ARecords->Count - AHeaderLineCount;
    for (int i=0; i < ARecords->Count - (AHeaderLineCount + 1) + 1; i++)
      InitRecord(ARecords->Strings[i + AHeaderLineCount], i, AValues);
  }
  __finally {
    tvPlanets->EndUpdate();
    delete ARecords;
    delete AValues;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  CustomizeColumns();
  LoadData();
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::FormShow(TObject *Sender)
{
  tvPlanets->DataController->Groups->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actFullExpandExecute(TObject *Sender)
{
  tvPlanets->DataController->Groups->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TEQGridRLMainForm::actFullCollapseExecute(TObject *Sender)
{
  tvPlanets->DataController->Groups->FullCollapse();
}
//---------------------------------------------------------------------------

 
