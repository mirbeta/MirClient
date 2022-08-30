//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoPlanets.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxLabel"
#pragma link "cxListView"
#pragma link "cxMCListBox"
#pragma link "cxSplitter"
#pragma resource "*.dfm"
TEditorsStylesDemoPlanetsFrame *EditorsStylesDemoPlanetsFrame;
//---------------------------------------------------------------------------
const String cxPlanetsFileName = "nineplanets.txt";

__fastcall TEditorsStylesDemoPlanetsFrame::TEditorsStylesDemoPlanetsFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  FRecordValues = new TStringList();
  cxMCListBox->Items->LoadFromFile(cxPlanetsFileName);
  InitCurrentRecordValues(cxMCListBox->Items->Strings[0]);
  cxMCListBox->Items->Delete(0);
  for (int i=0; i < cxMCListBox->HeaderSections->Count; i++)
    cxMCListBox->HeaderSections->Items[i]->Text = FRecordValues->Strings[i];
  HintStyle = hcstBlueSlideUp;
  if (cxMCListBox->Count > 0) {
    cxMCListBox->ItemIndex = 0;
    cxMCListBoxClick(NULL);
  }
  FDisplayStyle = shtLightGray;
  FTempDisplayStyle = shtLightGray;
}
//---------------------------------------------------------------------------

__fastcall TEditorsStylesDemoPlanetsFrame::~TEditorsStylesDemoPlanetsFrame()
{
  delete FRecordValues;
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoPlanetsFrame::Name()
{
  return ("Solar System");
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoPlanetsFrame::BriefName()
{
  return ("Solar");
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoPlanetsFrame::UpdateSatellites()
{
  String APlanetName = GetValue(cxMCListBox->ItemIndex, 0);
  cxListView->Items->Clear();
  cxListView->ViewStyle = vsIcon;
  TListItem* ListItem;
  for (int i=0; i < cxMCListBox->Count; i++)
    if (GetValue(i, 2) == APlanetName) {
      ListItem = cxListView->Items->Add();
        ListItem->Caption = GetValue(i, 0);
        ListItem->ImageIndex = StrToInt(GetValue(i, 6));
    }
  if (cxListView->Items->Count == 0) {
    TListItem *AItem = cxListView->Items->Add();
    AItem->Caption = "There are no satellites";
    AItem->ImageIndex = -1;
    cxListView->ViewStyle = vsList;
  }
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoPlanetsFrame::GetValue(int ARecordIndex, int AColIndex)
{
  InitCurrentRecordValues(cxMCListBox->Items->Strings[ARecordIndex]);
  String Result = "-1";
  if (FRecordValues->Count > AColIndex)
    Result = FRecordValues->Strings[AColIndex];
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoPlanetsFrame::cxMCListBoxClick(
      TObject *Sender)
{
  UpdateSatellites();
}
//---------------------------------------------------------------------------

int __fastcall TEditorsStylesDemoPlanetsFrame::GetIndexByName(String AName)
{
  for (int i=0; i < cxMCListBox->Count; i++)
   if (GetValue(i, 0) == AName) {
     return(i);
   }
  return -1;
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoPlanetsFrame::InitCurrentRecordValues(String ARecord)
{
 Char ADelimiter = cxMCListBox->Delimiter;
 String AString = "";
 for (int I = 1; I <= ARecord.Length(); I++)
   if (ARecord[I] == ADelimiter)
     AString = AString + "\r\n";
   else
     AString = AString + ARecord[I];
 FRecordValues->Clear();
 FRecordValues->Text = AString;

}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoPlanetsFrame::cxListViewInfoTip(
      TObject *Sender, TListItem *Item, String &InfoTip)
{
  InfoTip = "";
  int ARecordIndex = GetIndexByName(Item->Caption);
  for (int i=1; i < cxMCListBox->HeaderSections->Count; i++) {
    InfoTip = InfoTip + cxMCListBox->HeaderSections->Items[i]->Text + ": " +
      GetValue(ARecordIndex, i) + "\r\n";
  }
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoPlanetsFrame::StylesIniPath()
{
  return "StylesFrmSolarSystem\\";
}
//---------------------------------------------------------------------------

TColor TEditorsStylesDemoPlanetsFrame::GetStyleBackgroundColor()
{
  return (cxListView->Style->Color);
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoPlanetsFrame::Description()
{
  return ("Solar System Notes");
}
//---------------------------------------------------------------------------

