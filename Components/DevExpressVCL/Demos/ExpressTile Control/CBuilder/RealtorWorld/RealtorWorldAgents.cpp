//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RealtorWorldAgents.h"
#include "RealtorWorldMain.h"
#include "RealtorWorldDM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "RealtorWorldBaseFrame"
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxCurrencyEdit"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridChartView"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBChartView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxImage"
#pragma link "cxMemo"
#pragma link "cxSplitter"
#pragma link "cxStyles"
#pragma link "dxCustomTileControl"
#pragma link "dxTileControl"
#pragma link "cxNavigator"
#pragma link "dxDateRanges"
#pragma resource "*.dfm"
TfrmAgents *frmAgents;
//---------------------------------------------------------------------------
__fastcall TfrmAgents::TfrmAgents(TComponent* Owner)
	: TfrmBase(Owner)
{
  InitializeFrame();
  InitializeChartDataSet();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::cxSplitter2BeforeClose(TObject *Sender, bool &AllowClose)
{
  AllowClose = false;
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::AA(TObject *Sender, int &NewWidth, int &NewHeight, bool &Resize)
{
//
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::InitializeFrame()
{
  TdxTileControlItem* AItem;
  TDataSet* dsAgents;
  tcAgents->BeginUpdate();
  __try
  {
    dsAgents = DMRealtorWorld->clHomesAndAgents;
    dsAgents->First();
    while (!dsAgents->Eof)
    {
      AItem = tcAgents->CreateItem(True);
      AItem->Glyph->Image->LoadFromFieldValue(dsAgents->FieldByName("Photo")->Value);
      AItem->Style = frmRealtorWorld->tlAgents->Style;
      AItem->Glyph->Align = oaMiddleRight;
      AItem->Glyph->Image->Scale(70, 100);
      AItem->Tag = dsAgents->FieldByName("ID")->AsInteger;
      AItem->Style->Font->Size = 13;
      AItem->Text1->Value = dsAgents->FieldByName("FirstName")->AsString + " " + dsAgents->FieldByName("LastName")->AsString;
      AItem->Text1->IndentHorz = 10;
      AItem->Text1->IndentVert = 10;
	  AItem->Text2->Value = dsAgents->FieldByName("Phone")->AsString;
      AItem->Text2->Align = oaTopLeft;
      AItem->Text2->IndentHorz = 10;
      AItem->Text2->IndentVert = 30;
	  AItem->OnClick = OnItemClick;
	  dsAgents->Next();
	};
  }
  __finally
  {
    tcAgents->EndUpdate();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::InitializeChartDataSet()
{
  cdsChart->DisableControls();
  __try
  {
	for (int I = 1; I < 7; I++)
	  for (int AYear = 2003; AYear < 2013; AYear++)
	  {
       cdsChart->Append();
       cdsChartDate->Value = AYear;
       cdsChartMidWest->Value = Random(20) + 4;
       cdsChartNorthEast->Value = Random(20) + 5;
       cdsChartSouth->Value = Random(20) + 2;
       cdsChartWest->Value = Random(20) + 3;
       cdsChartAgentID->Value = I;
	   cdsChart->Post();
      };
  }
  __finally
  {
	cdsChart->EnableControls();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::OnItemClick(TdxTileControlItem *Sender)
{
  DMRealtorWorld->clHomesDetail->Filter = "AgentID=" + IntToStr(Sender->Tag);
  cdsChart->Filter = "AgentID=" + IntToStr(Sender->Tag);
  DMRealtorWorld->clHomesDetail->First();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAgents::SelectItem(int APhotoID, int AAgentID)
{
  for (int i = 0; i < tcAgents->Items->Count; i++)
  {
	if (tcAgents->Items->Items[i]->Tag == AAgentID)
	{
	  tcAgents->Items->Items[i]->MakeVisible();
	  tcAgents->Items->Items[i]->Click();
	};
  };
}
//---------------------------------------------------------------------------
