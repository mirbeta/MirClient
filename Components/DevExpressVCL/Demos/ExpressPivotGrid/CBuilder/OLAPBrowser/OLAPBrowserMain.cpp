//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OLAPBrowserMain.h"
#include "DemoUtils.h"
#include "cxPivotGridOLAPADOMDProvider.hpp"
#include "cxPivotGridOLAPOLEDBProvider.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxCustomPivotGrid"
#pragma link "cxGraphics"
#pragma link "cxPivotGrid"
#pragma link "cxPivotGridOLAPDataSource"
#pragma link "cxPivotGridOLAPConnectionDesigner"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TfrmOlapBrowser *frmOlapBrowser;
//---------------------------------------------------------------------------
__fastcall TfrmOlapBrowser::TfrmOlapBrowser(TComponent* Owner)
	: TfrmDemoBasicMain(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmOlapBrowser::AfterConstruction()
{
  SyncMenuWithOptionsLockedStateImage();
}

TcxCustomPivotGrid* __fastcall TfrmOlapBrowser::PivotGrid()
{
  return UnboundPivotGrid;
}
//---------------------------------------------------------------------------

void __fastcall TfrmOlapBrowser::SetFieldPos(
  const String AFieldName, TcxPivotGridFieldArea AArea)
{
	TcxPivotGridField* AField = PivotGrid()->GetFieldByName(AFieldName);
	if (AField != NULL)
	{
		AField->Area = AArea;
		AField->Visible = true;
	}
}

void __fastcall TfrmOlapBrowser::FormCreate(TObject *Sender)
{
  TfrmDemoBasicMain::FormCreate(Sender); 
  String APath = ExpandFileName((String)"..\\..\\Data\\Northwind.cub");
  if (!FileExists(APath)) return;
  String ConnectionString = "Provider=MSOLAP;Integrated Security=SSPI;Persist Security Info=False;Data Source=";
  OLAPDataSource->ConnectionString = ConnectionString + APath;
  LoadDefaultLayout(True);
  PivotGrid()->Customization->Visible = true;
}

void __fastcall TfrmOlapBrowser::ChangeProviderClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  bool AActive = OLAPDataSource->Active;
  if (Sender == miADOMDProvider)
	OLAPDataSource->ProviderClassName = "TcxPivotGridOLAPADOMDProvider";
  else
	OLAPDataSource->ProviderClassName = "TcxPivotGridOLAPOLEDBProvider";
  if (!OLAPDataSource->Active)
	LoadDefaultLayout(AActive);
}

void __fastcall TfrmOlapBrowser::NewConnection1Click(TObject *Sender)
{
	int I, J;
	String ACube;
	String ANewConnectionString =
		cxPivotGridOLAPCreateConnectionString(ACube, OLAPDataSource->ProviderClass, PivotGrid()->LookAndFeel);
	if (ANewConnectionString != "") {
		PivotGrid()->BeginUpdate();
		try {
			OLAPDataSource->ConnectionString = ANewConnectionString;
			OLAPDataSource->Cube = ACube;
			OLAPDataSource->Active = True;
			OLAPDataSource->RetrieveFields(PivotGrid());
			for (I = 0; I < PivotGrid()->Groups->Count - 1; I++) {
				for (J = 1; J < PivotGrid()->Groups->Items[I]->FieldCount; J++) {
					PivotGrid()->Groups->Items[I]->Fields[J]->Visible = true;
				}
			}
		}
		__finally {
			PivotGrid()->EndUpdate();
			PivotGrid()->Customization->Visible = true;
		};
	}
}

void __fastcall TfrmOlapBrowser::SyncMenuWithOptionsLockedStateImage()
{
  TcxPivotGridOptionsLockedStateImage *AOptionsLockedStateImage = PivotGrid()->OptionsLockedStateImage;
  miLockedStateImageModeImmediate->Checked = AOptionsLockedStateImage->Mode == lsimImmediate;
  miLockedStateImageModeNever->Checked = AOptionsLockedStateImage->Mode == lsimNever;
  miLockedStateImageModePending->Checked = AOptionsLockedStateImage->Mode == lsimPending;
  miLockedStateImageEffectLight->Checked = AOptionsLockedStateImage->Effect == lsieLight;
  miLockedStateImageEffectDark->Checked = AOptionsLockedStateImage->Effect == lsieDark;
}

void __fastcall TfrmOlapBrowser::LockedViewImageClick(TObject *Sender)
{
  int ATag = ((TMenuItem*)Sender)->Tag;
  if (ATag < 3)
	PivotGrid()->OptionsLockedStateImage->Mode = TcxLockedStateImageShowingMode(ATag);
  else
	PivotGrid()->OptionsLockedStateImage->Effect = TcxLockedStateImageEffect(ATag - 2);

  SyncMenuWithOptionsLockedStateImage();
}

void __fastcall TfrmOlapBrowser::LoadDefaultLayout(bool AActivate)
{
  OLAPDataSource->Active = AActivate;
  PivotGrid()->BeginUpdate();
  try {
	if (AActivate) {
		OLAPDataSource->RetrieveFields(PivotGrid());
		SetFieldPos("Country", faColumn);
		SetFieldPos("City", faColumn);
		SetFieldPos("Category Name", faRow);
		SetFieldPos("Products", faRow);
		SetFieldPos("Quantity", faData);
		SetFieldPos("Discount", faData);
	}
  }
  __finally {
	PivotGrid()->EndUpdate();
	PivotGrid()->ApplyBestFit();
  }
}
//---------------------------------------------------------------------------

