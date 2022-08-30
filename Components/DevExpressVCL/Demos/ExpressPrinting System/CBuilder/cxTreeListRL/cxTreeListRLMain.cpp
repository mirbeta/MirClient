//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "cxTreeListRLMain.h"
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
#pragma link "cxStyles"
#pragma link "cxEditRepositoryItems"
#pragma link "dxPSCore"
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
#pragma link "dxPScxCommon"
#pragma link "cxInplaceContainer"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "dxPScxTLLnk"
#pragma link "cxDrawTextUtils"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "DemoBasicMain"
#pragma resource "*.dfm"
TcxTreeListRLMainForm *cxTreeListRLMainForm;
//---------------------------------------------------------------------------
__fastcall TcxTreeListRLMainForm::TcxTreeListRLMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::miUltraFlatClick(TObject *Sender)
{
  if(tlPlanets->LookAndFeel->Kind != lfUltraFlat){
    tlPlanets->LookAndFeel->Kind = lfUltraFlat;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::miFlatClick(TObject *Sender)
{
  if(tlPlanets->LookAndFeel->Kind != lfFlat){
	tlPlanets->LookAndFeel->Kind = lfFlat;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::miStandardClick(TObject *Sender)
{
  if(tlPlanets->LookAndFeel->Kind != lfStandard){
	tlPlanets->LookAndFeel->Kind = lfStandard;
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::miNativeStyleClick(TObject *Sender)
{
  tlPlanets->LookAndFeel->NativeStyle = !tlPlanets->LookAndFeel->NativeStyle;
  ((TCustomAction*)Sender)->Checked = !((TCustomAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::CustomizeColumns()
{
  const String DecimalSeparator = '.';
  for (int I = 0; I < tlPlanets->ColumnCount; I++)
    if (I == DistanceColIndex || I == RadiusColIndex || I == ImageIndexColIndex)
      tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
        __classid(TcxIntegerValueType);
    else{
      if (I == PeriodColIndex)
        tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
          __classid(TcxFloatValueType);
      else
        tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
          __classid(TcxStringValueType);
    }
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::LoadData()
{
  if (!FileExists(FileName))
    throw Exception("Data file not found");

  FRecords = new TStringList();
  FValues = new TStringList();

  tlPlanets->BeginUpdate();
  __try{
    FRecords->LoadFromFile(FileName);
    for (int I = 0; I < HeaderLineCount; I++)
      FRecords->Delete(0);
    AddNodes(tlPlanets->Root, "-");
  }
  __finally{
    tlPlanets->EndUpdate();
    delete FRecords;
    delete FValues;
  }
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  CustomizeColumns();
  LoadData();
  tlPlanets->FullCollapse();
  tlPlanets->Root->Items[0]->Expanded = true;
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::actFullExpandExecute(TObject *Sender)
{
  tlPlanets->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::actFullCollapseExecute(TObject *Sender)
{
  tlPlanets->FullCollapse();
}
//---------------------------------------------------------------------------

TcxTreeListNode* __fastcall TcxTreeListRLMainForm::AddNode(
  TcxTreeListNode *AParentNode, String ARecord)
{
  String S;
  TcxTreeListNode *Result = AParentNode->AddChild();
  FValues->CommaText = ARecord;
  for (int J = 0; J < FValues->Count; J++)
    if (FValues->Strings[J] != "-"){
      S = FValues->Strings[J];
      if (S.Pos('.')!=0) {S[S.Pos('.')] = dxFormatSettings->DecimalSeparator;}
      Variant V = S;
      if (!VarIsNull(V))
        Result->Values[J] = V;
    }
  Result->ImageIndex =  Result->Values[ImageField];
  Result->SelectedIndex = Result->Values[ImageField];
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TcxTreeListRLMainForm::AddNodes(TcxTreeListNode *AParentNode,
  String AParentKeyValue)
{
  TcxTreeListNode *ANode;
  for (int J = 0; J < FRecords->Count; J++)
    if (GetFieldValue(FRecords->Strings[J], ParentKeyField) == AParentKeyValue){
      ANode = AddNode(AParentNode, FRecords->Strings[J]);
      AddNodes(ANode, GetFieldValue(FRecords->Strings[J], KeyField));
    }
}
//---------------------------------------------------------------------------

String __fastcall TcxTreeListRLMainForm::GetFieldValue(String ARecord,
  int AFieldIndex)
{
 FValues->CommaText = ARecord;
 return FValues->Strings[AFieldIndex];
}
//---------------------------------------------------------------------------





