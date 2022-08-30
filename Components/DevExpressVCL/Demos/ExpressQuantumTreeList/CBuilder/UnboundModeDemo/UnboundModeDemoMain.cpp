//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "UnboundModeDemoMain.h"
#include "UnboundModeDemoData.h"
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
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma resource "*.dfm"
TUnboundModeDemoMainForm *UnboundModeDemoMainForm;

  const int HeaderLineCount = 2;
  const int ParentKeyField = 2;
  const int KeyField = 0;
  const int ImageField = 8;
  const int DistanceColIndex = 3;
  const int PeriodColIndex = 4;
  const int RadiusColIndex = 7;
  const int ImageIndexColIndex = 8;

//---------------------------------------------------------------------------
__fastcall TUnboundModeDemoMainForm::TUnboundModeDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormShow(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::FormCreate(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */
  TDemoBasicMainForm::FormCreate(Sender);
  CustomizeColumns();
  LoadData();
  tlPlanets->FullCollapse();
  tlPlanets->Root->Items[0]->Expanded = true;

//*/
}
//---------------------------------------------------------------------------
void __fastcall TUnboundModeDemoMainForm::miDropNodeIndicatorClick(TObject *Sender)
{
  tlPlanets->OptionsView->DropNodeIndicator = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::CustomizeColumns()
{
  const String DecimalSeparator = '.';
  for (int I = 0; I < tlPlanets->ColumnCount; I++)
    if (I == DistanceColIndex || I == RadiusColIndex || I == ImageIndexColIndex)
      tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
        (TcxValueTypeClass)__classid(TcxIntegerValueType);
    else{
      if (I == PeriodColIndex)
        tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
          (TcxValueTypeClass)__classid(TcxFloatValueType);
      else
        tlPlanets->Columns[I]->DataBinding->ValueTypeClass =
          (TcxValueTypeClass)__classid(TcxStringValueType);
    }
}
//---------------------------------------------------------------------------

void __fastcall TUnboundModeDemoMainForm::LoadData()
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

TcxTreeListNode* __fastcall TUnboundModeDemoMainForm::AddNode(
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

void __fastcall TUnboundModeDemoMainForm::AddNodes(TcxTreeListNode *AParentNode,
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

String __fastcall TUnboundModeDemoMainForm::GetFieldValue(String ARecord,
  int AFieldIndex)
{
 FValues->CommaText = ARecord;
 return FValues->Strings[AFieldIndex];
}
//---------------------------------------------------------------------------


void __fastcall TUnboundModeDemoMainForm::tlPlanetsDragOver(
      TObject *Sender, TObject *Source, int X, int Y, TDragState State,
      bool &Accept)
{
//        
}
//---------------------------------------------------------------------------

