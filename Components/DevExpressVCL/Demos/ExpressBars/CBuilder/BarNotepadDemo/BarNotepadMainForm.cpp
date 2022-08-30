//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BarNotepadMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "NotepadMainForm"
#pragma link "cxClasses"
#pragma resource "*.dfm"
TfrmBarsNotepadMain *frmBarsNotepadMain;

//---------------------------------------------------------------------------

void TBarRecentDocumentsController::DoLoad(TCustomIniFile* AConfig)
{
  Integer ACount;
  TRecentDocumentsController::DoLoad(AConfig);
  FBarList->Items->BeginUpdate();
  try
  {
	FBarList->Items->Clear();
	ACount = AConfig->ReadInteger(ClassName(), "Count", 0);
	for (int I = 0; I <= ACount - 1; I++)
	  FBarList->Items->Add(AConfig->ReadString(ClassName(), IntToStr(I), ""));
  }
  __finally
  {
	FBarList->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TBarRecentDocumentsController::DoSave(TCustomIniFile* AConfig)
{
  TRecentDocumentsController::DoSave(AConfig);
  AConfig->WriteInteger(ClassName(), "Count", 0);
  for (int I = 0; I <= FBarList->Items->Count - 1; I++)
	AConfig->WriteString(ClassName(), IntToStr(I), FBarList->Items->Strings[I]);
}
//---------------------------------------------------------------------------

__fastcall TBarRecentDocumentsController::TBarRecentDocumentsController(TdxBarListItem* ABarList)
{
  FBarList = ABarList;
}
//---------------------------------------------------------------------------

void TBarRecentDocumentsController::Add(const String AFileName)
{
  Integer AIndex;
  FBarList->Items->BeginUpdate();
  try
  {
	AIndex = FBarList->Items->IndexOf(AFileName);
	if (AIndex >= 0)
	  FBarList->Items->Move(AIndex, 0);
	else
	  FBarList->Items->Add(AFileName);

	while (FBarList->Items->Count > 10)
	{
	   FBarList->Items->Delete(9);
	}
  }
  __finally
  {
	FBarList->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

__fastcall TfrmBarsNotepadMain::TfrmBarsNotepadMain(TComponent* Owner): TfrmNotepadMain(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmBarsNotepadMain::bbTabbedViewClick(TObject *Sender)
{
  dxTabbedMDIManager1->Active = bbTabbedView->Down;
}
//---------------------------------------------------------------------------

void __fastcall TfrmBarsNotepadMain::FormShow(TObject *Sender)
{
  TfrmNotepadMain::FormShow(Sender);
  PopulateZoomFactors();
  UpdateImageIndexes();
}
//---------------------------------------------------------------------------

void __fastcall TfrmBarsNotepadMain::liZoomClick(TObject *Sender)
{
  if (FUpdatingControls == 0)
  {
	Editor->Properties->ZoomFactor = double (Integer(liZoom->Items->Objects[liZoom->ItemIndex])) / 100;
	UpdateControls();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmBarsNotepadMain::liRecentDocumentsClick(TObject *Sender)
{
  OpenFile(liRecentDocuments->Items[liRecentDocuments->ItemIndex].Text);
}
//---------------------------------------------------------------------------

void TfrmBarsNotepadMain::PopulateZoomFactors()
{
  const Integer ZoomValues[9] = {50, 80, 100, 110, 120, 140, 150, 200, 400};
  for (int I = 0; I <= 8; I++)
	liZoom->Items->AddObject(IntToStr(ZoomValues[I]) + " %", (TObject*)ZoomValues[I]);
}
//---------------------------------------------------------------------------

void TfrmBarsNotepadMain::UpdateImageIndexes()
{
  bbBarsHelp->ImageIndex = 18;
  bbDockingHelp->ImageIndex = 18;
  bbDXSupport->ImageIndex = 20;
  bbDXDownloads->ImageIndex = 20;
  bbDXOnWeb->ImageIndex = 20;
  bbDXProducts->ImageIndex = 20;
  bbMyDX->ImageIndex = 20;
}
//---------------------------------------------------------------------------

TRecentDocumentsController* TfrmBarsNotepadMain::CreateRecentDocumentsController()
{
  return new TBarRecentDocumentsController(liRecentDocuments);
}
//---------------------------------------------------------------------------

void TfrmBarsNotepadMain::DoUpdateControls(TfrmNotepadChild* AActiveChild)
{
  Integer AZoomFactor;
  TfrmNotepadMain::DoUpdateControls(AActiveChild);
  bbTabbedView->Down = dxTabbedMDIManager1->Active;
  bsZoom->Visible = VisibleTodxBarVisible(AActiveChild);
  if (AActiveChild)
  {
	AZoomFactor = Round(AActiveChild->Editor->Properties->ZoomFactor * 100);
	liZoom->ItemIndex = liZoom->Items->IndexOfObject((TObject*)AZoomFactor);
	bsZoom->Caption = IntToStr(AZoomFactor) + " %";
	Editor->PopupMenu = pmEditor;
  }
}
//---------------------------------------------------------------------------


