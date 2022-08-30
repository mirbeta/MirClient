//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxflchrt"
#pragma link "dxfcedit"
#pragma link "cxImageList"
#pragma link "dxFlowChartDesigner"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TMainForm *MainForm;
bool FUpdate;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::dxFlowChartDblClick(TObject *Sender)
{
  TMemoryStream *AStream;
  TBlobField *AField;

  if (Table->RecordCount != 0)
    if (ShowFlowChartEditor(dxFlowChart, "ExpressFlowChart Editor")) {
      FUpdate = false;
      Table->Edit();
      AStream = new TMemoryStream;
      dxFlowChart->SaveToStream(AStream);
      AStream->Position = 0;
      AField = (TBlobField*)(Table->FieldByName("Chart"));
      AField->LoadFromStream(AStream);
      delete AStream;
      Table->FieldByName("BkColor")->AsInteger = dxFlowChart->Color;
      Table->Post();
      FUpdate = true;
    };
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TableAfterScroll(TDataSet *DataSet)
{
  TMemoryStream *AStream;
  TBlobField *AField;

  if (! FUpdate) return;
  dxFlowChart->BeginUpdate();
  dxFlowChart->Clear();
  AField = (TBlobField*)(Table->FieldByName("Chart"));
  if (! (AField->IsNull)) {
    AStream = new TMemoryStream;
    AField->SaveToStream(AStream);
    AStream->Position = 0;
	dxFlowChart->LoadFromStream(AStream);
    delete AStream;
  };
  dxFlowChart->Color = clWindow;
  dxFlowChart->EndUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  FUpdate = true;
  Table->Open();
  Table->LoadFromBinaryFile("Data.bin");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnOnWebClick(TObject *Sender)
{
  ShellExecute(Handle, "OPEN", "http://www.devexpress.com", NULL, NULL, SW_SHOWMAXIMIZED);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnSaveClick(TObject *Sender)
{
  if (SaveDialog->Execute())
    dxFlowChart->SaveToFile(SaveDialog->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::DataSourceDataChange(TObject *Sender,
      TField *Field)
{
  bool En = Table->RecordCount > 0;
  miOpen->Enabled = En;
  miSaveAs->Enabled = En;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::miAntialiasingClick(TObject *Sender)
{
   dxFlowChart->Antialiasing = miAntialiasing->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::miOpenClick(TObject *Sender)
{
  TMemoryStream *AStream;

  if (OpenDialog->Execute()) {
    dxFlowChart->LoadFromFile(OpenDialog->FileName);
    if (Table->RecordCount != 0) {
      FUpdate = false;
      Table->Edit();
      AStream = new TMemoryStream;
      dxFlowChart->SaveToStream(AStream);
	  AStream->Position = 0;
	  TableChart->LoadFromStream(AStream);
      delete AStream;
      Table->FieldByName("BkColor")->AsInteger = dxFlowChart->Color;
      Table->Post();
      FUpdate = true;
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnFitClick(TObject *Sender)
{
  miFit->Checked = ! miFit->Checked;
  if (miFit->Checked) {
    dxFlowChart->Zoom = 0;
    miZoomIn->Enabled = false;
    miZoomOut->Enabled = false;
    miActualSize->Enabled = false;
  } else {
    dxFlowChart->Zoom = 100;
    miZoomIn->Enabled = true;
    miZoomOut->Enabled = true;
    miActualSize->Enabled = true;
  };
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnZoomInClick(TObject *Sender)
{
  miZoomOut->Enabled = true;
  if (dxFlowChart->Zoom < 490) dxFlowChart->Zoom = dxFlowChart->Zoom + 10; else miZoomIn->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::btnZoomOutClick(TObject *Sender)
{
  miZoomIn->Enabled = true;
  if (dxFlowChart->Zoom > 20) dxFlowChart->Zoom = dxFlowChart->Zoom - 10; else miZoomOut->Enabled = false;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::miActualSizeClick(TObject *Sender)
{
  dxFlowChart->Zoom = 100;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TableBeforeInsert(TDataSet *DataSet)
{
  if ((DataSet->FieldByName("ChartName")->AsString == "") &&
	(TBlobField*)(DataSet->FieldByName("Chart")->IsNull))
	DataSet->Delete();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TableAfterInsert(TDataSet *DataSet)
{
  FUpdate = false;
  Table->Edit();
  Table->FieldByName("BkColor")->AsInteger = clWindow;
  Table->Post();
  FUpdate = true;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
{
	 Table->SaveToBinaryFile("Data.bin");
}
//---------------------------------------------------------------------------

