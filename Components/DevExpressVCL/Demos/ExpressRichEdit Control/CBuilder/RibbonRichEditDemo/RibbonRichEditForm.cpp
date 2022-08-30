//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RibbonRichEditForm.h"
//---------------------------------------------------------------------------
#pragma link "dxRichEdit.NativeApi"

#pragma package(smart_init)
#pragma resource "*.dfm"

TfrmRibbonRichEditForm *frmRibbonRichEditForm;
//---------------------------------------------------------------------------
String GetInitDocumentFileName()
{
  return ExtractFilePath(Application->ExeName) + "InitDocument.ini";
}

String GetRecentDocumentsFileName()
{
  return ExtractFilePath(Application->ExeName) + "RecentDocuments.ini";
}

TdxRibbonRecentDocumentsController* TfrmRibbonRichEditForm::CreateRecentDocumentsController()
{
  return new TdxRibbonRecentDocumentsController(bvgcRecentPaths, bvgcRecentDocuments);
}

//---------------------------------------------------------------------------

TdxRibbonRecentDocumentsController::TdxRibbonRecentDocumentsController(TdxRibbonBackstageViewGalleryControl* ARecentPaths, TdxRibbonBackstageViewGalleryControl* ARecentDocuments)
{
  FRecentDocuments = ARecentDocuments;
  FRecentDocuments->Gallery->Groups->Add();
  FRecentDocuments->Tag = 0;

  FRecentPaths = ARecentPaths;
  FRecentPaths->Gallery->Groups->Add();
  FRecentPaths->Tag = 1;
}

//---------------------------------------------------------------------------
void TdxRibbonRecentDocumentsController::LoadFromIniFile(const String AFileName)
{
  TIniFile *AIniFile;
  AIniFile = new TIniFile(AFileName);
  try
  {
	DoLoad(AIniFile);
  }
  __finally
  {
	delete AIniFile;
  }
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::SaveToIniFile(const String AFileName)
{
  TIniFile *AIniFile;
  AIniFile = new TIniFile(AFileName);
  try
  {
	DoSave(AIniFile);
  }
  __finally
  {
	delete AIniFile;
  }
}

//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem* TdxRibbonRecentDocumentsController::GetItemByValue(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue)
{
  TdxRibbonBackstageViewGalleryItem* AResult;
  for (int I = 0; I <= AGroup->Items->Count - 1; I++)
	if (SameText(AGroup->Items->Items[I]->Hint, AValue))
	  return AGroup->Items->Items[I];
  return NULL;
}
//---------------------------------------------------------------------------

TdxRibbonBackstageViewGalleryItem* TdxRibbonRecentDocumentsController::InternalAdd(TdxRibbonBackstageViewGalleryGroup* AGroup, const String AValue)
{
  TdxRibbonBackstageViewGalleryItem* AResult;
  AResult = GetItemByValue(AGroup, AValue);
  if (AResult == NULL)
  {
	AResult = AGroup->Items->Add();
	AResult->Caption = ExtractFileName(AValue);
	AResult->Hint = AValue;
	AResult->Description = AValue;
	AResult->ImageIndex = AGroup->GetParentComponent()->Tag;
  }
  AResult->Index = 0;

  while (AGroup->Items->Count > 19)
	AGroup->Items->Delete(9);
  return AResult;
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::InternalLoad(TdxRibbonBackstageViewGalleryGroup* AGroup, TCustomIniFile* AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem* AItem;
  AGroup->Items->BeginUpdate();
  try
  {
	AGroup->Items->Clear();
	for (int I = 0; I <= AIniFile->ReadInteger(ASection, "Count", 0) - 1; I++)
	{
	  AItem = InternalAdd(AGroup, AIniFile->ReadString(ASection, IntToStr(I), ""));
	  AItem->Pinned = AIniFile->ReadBool(ASection, IntToStr(I) + "Pinned", False);
	}
  }
  __finally
  {
	AGroup->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::InternalSave(TdxRibbonBackstageViewGalleryGroup* AGroup, TCustomIniFile* AIniFile, const String ASection)
{
  TdxRibbonBackstageViewGalleryItem *AItem;
  int AIndex = 0;
  AIniFile->EraseSection(ASection);
  AIniFile->WriteInteger(ASection, "Count", AGroup->Items->Count);
  for (int I = AGroup->Items->Count - 1; I >= 0; I--)
  {
	AItem = AGroup->Items->Items[I];
	AIniFile->WriteString(ASection, IntToStr(AIndex), AItem->Hint);
	AIniFile->WriteBool(ASection, IntToStr(AIndex) + "Pinned", AItem->Pinned);
	AIndex++;
  }
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::DoLoad(TCustomIniFile* AConfig)
{
  InternalLoad((TdxRibbonBackstageViewGalleryGroup*)(FRecentDocuments->Gallery->Groups->Items[0]), AConfig, "RecentDocuments");
  InternalLoad((TdxRibbonBackstageViewGalleryGroup*)(FRecentPaths->Gallery->Groups->Items[0]), AConfig, "RecentPaths");
}
//---------------------------------------------------------------------------

void TdxRibbonRecentDocumentsController::DoSave(TCustomIniFile* AConfig)
{
  InternalSave((TdxRibbonBackstageViewGalleryGroup*)(FRecentDocuments->Gallery->Groups->Items[0]), AConfig, "RecentDocuments");
  InternalSave((TdxRibbonBackstageViewGalleryGroup*)(FRecentPaths->Gallery->Groups->Items[0]), AConfig, "RecentPaths");
}

//---------------------------------------------------------------------------

__fastcall TfrmRibbonRichEditForm::TfrmRibbonRichEditForm(TComponent* Owner)
	: TfrmRibbonRichEditMain(Owner)
{
}

void TfrmRibbonRichEditForm::DoBackstageViewTabChanged()
{
  if (!ComponentState.Contains(csReading))
  {
	gbBackstageViewTabCaption->Parent = rbvBackstageView->ActiveTab;
	lbbvTabCaption2010->Caption = rbvBackstageView->ActiveTab->Caption;
	lbbvTabCaption2013->Caption = rbvBackstageView->ActiveTab->Caption;
	if (bvtsOpen->Active || bvtsSaveAs->Active)
	{
	  bvgcLocationsRecentDocumentsGroup->Visible = rbvBackstageView->ActiveTab == bvtsOpen;
	  if (bvgcLocationsRecentDocumentsGroup->Visible && !bvgcLocationsComputerItem->Checked)
		bvgcLocationsRecentDocumentsItem->Checked = True;
	  else
		bvgcLocationsComputerItem->Checked = True;

	  gbLocationsMain->Parent = rbvBackstageView->ActiveTab;
	}
  }
}

void TfrmRibbonRichEditForm::SetRibbonDemoStyle(TRibbonDemoStyle AStyle)
{

  if ((AStyle == rdsOffice2007) || (AStyle == rdsScenic))
  {
	rtHelp->Visible = True;
  }
  else
  {
	Ribbon->ApplicationButton->Menu = rbvBackstageView;
	rtHelp->Visible = False;
  }

  lbbvTabCaption2010->Visible = AStyle == rdsOffice2010;
  lbbvTabCaption2013->Visible = (AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet) || (AStyle == rdsOffice2019);
  if ((AStyle == rdsOffice2013) || (AStyle == rdsOffice2016) || (AStyle == rdsOffice2016Tablet) || (AStyle == rdsOffice2019))
	gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2013->Style->Font) + bvSpacer7->Height;
  else
	gbBackstageViewTabCaption->Height = cxTextHeight(lbbvTabCaption2010->Style->Font) + bvSpacer7->Height;

  TfrmRibbonRichEditMain::SetRibbonDemoStyle(AStyle);
}

void TfrmRibbonRichEditForm::UpdateColorSchemeRelatedControls()
{
  lbComputer->Style->TextColor = Ribbon->ColorScheme->GetPartColor(rspTabHeaderText, DXBAR_ACTIVE);
  lbRecentDocuments->Style->TextColor = lbComputer->Style->TextColor;
  lbbvTabCaption2010->Style->TextColor = Ribbon->ColorScheme->GetPartColor(DXBAR_BACKSTAGEVIEW_TEXTCOLOR);
  lbbvTabCaption2013->Style->TextColor = lbbvTabCaption2010->Style->TextColor;
  lbRecentFolders->Style->TextColor = lbbvTabCaption2010->Style->TextColor;
  lbCurrentFolder->Style->TextColor = lbbvTabCaption2010->Style->TextColor;
}

void __fastcall TfrmRibbonRichEditForm::rbvBackstageViewPopup(TObject *Sender)
{
  gbRecentPathsPaneCurrentFolder->Visible = bvgcCurrentFolder->Gallery->Groups->Count > 0;
  DoBackstageViewTabChanged();
}

void __fastcall TfrmRibbonRichEditForm::rbvBackstageViewTabChanged(TObject *Sender)
{
  DoBackstageViewTabChanged();
}

void __fastcall TfrmRibbonRichEditForm::RichEditControlDocumentLoaded(TObject *Sender)
{
//  UnicodeString AName = RichEditControl->Options->DocumentSaveOptions->CurrentFileName;
//  RecentDocumentsController->Add(AName);
}

void __fastcall TfrmRibbonRichEditForm::btnBrowsePathClick(TObject *Sender)
{
  Boolean AHandled;
  if (bvtsSaveAs->Active)
	AHandled = acSaveAs->Execute();
  else
	AHandled = acOpenDocument->Execute();

  if (AHandled)
	rbvBackstageView->Hide();
}

void __fastcall TfrmRibbonRichEditForm::bvgcLocationsItemClick(TObject *Sender,
	TdxRibbonBackstageViewGalleryItem *AItem)
{
  gbRecentDocumentsPane->Visible = bvgcLocationsRecentDocumentsItem->Checked;
  gbRecentPathsPane->Visible = bvgcLocationsComputerItem->Checked;
}

void __fastcall TfrmRibbonRichEditForm::bvgcRecentDocumentsItemClick(TObject *Sender,
		  TdxRibbonBackstageViewGalleryItem *AItem)
{
  RichEditControl->LoadDocument(AItem->Hint);
  rbvBackstageView->Hide();
}

void __fastcall TfrmRibbonRichEditForm::bvgcRecentPathsItemClick(TObject *Sender,
	TdxRibbonBackstageViewGalleryItem *AItem)
{
  Boolean AHandled;
  if (bvtsSaveAs->Active)
	AHandled = acSaveAs->Execute();
  else
	AHandled = acOpenDocument->Execute();
  if (AHandled)
	rbvBackstageView->Hide();
}

void __fastcall TfrmRibbonRichEditForm::FormCreate(TObject *Sender)
{
  TfrmRibbonRichEditMain::FormCreate(Sender);
  FRecentDocumentsController = CreateRecentDocumentsController();
  FRecentDocumentsController->LoadFromIniFile(GetRecentDocumentsFileName());
  UnicodeString AAboutFileName = ExtractFilePath(Application->ExeName) + "About.txt";
  if (FileExists(AAboutFileName))
	meAbout->Lines->LoadFromFile(AAboutFileName);
  lblSupport->OnClick = dmCommonData->actSupport->OnExecute;
  lblHelpBars->OnClick = dmCommonData->actBarsHelp->OnExecute;
  lblHelpDocking->OnClick = dmCommonData->actDockingHelp->OnExecute;
  lblProducts->OnClick = dmCommonData->actProducts->OnExecute;
  lblClientCenter->OnClick = dmCommonData->actMyDX->OnExecute;
  lblDXonWeb->OnClick = dmCommonData->actDXOnTheWeb->OnExecute;
  lblDownloads->OnClick = dmCommonData->actDownloads->OnExecute;
}

void __fastcall TfrmRibbonRichEditForm::FormDestroy(TObject *Sender)
{
  FRecentDocumentsController->SaveToIniFile(GetRecentDocumentsFileName());
  delete FRecentDocumentsController;
  TfrmRibbonRichEditMain::FormDestroy(Sender);
}

void __fastcall TfrmRibbonRichEditForm::FormShow(TObject *Sender)
{
  TIniFile* AIniFile;
  String ADocumentName;
  AIniFile = new TIniFile(GetInitDocumentFileName());
  try
  {
    ADocumentName = AIniFile->ReadString("DocumentName", "DocumentName", "");
    if (FileExists(ADocumentName))
      RichEditControl->Document->LoadDocument(ADocumentName, TdxRichEditDocumentFormat::Undefined);
  }
  __finally
  {
	delete AIniFile;
  }
}
