//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "main.h"
#include "dxPSRes.hpp"
#include "Splash.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxPSBaseGridLnk"
#pragma link "dxPSCore"
#pragma link "dxPSDBBasedXplorer"
#pragma link "dxPSGrLnks"
#pragma link "dxPSStdGrLnk"
#pragma link "dxPSDBBasedXplorer"
#pragma link "dxPSGrLnks"
#pragma link "dxPSStdGrLnk"
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
#pragma link "cxDrawTextUtils"
#pragma link "cxGraphics"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "cxLookAndFeels"
#pragma link "dxmdaset"
#pragma link "cxGroupBox"
#pragma link "cxControls"
#pragma resource "*.dfm"

/*
 Follow files must be linked if you want to support all types of saved reports,
 i.e. reports that were created from all types of ReportLinks.
 These files contain registration information for all item types used to create them
*/

// cxSpreadSheet
//#pragma link "dxPScxSSLnk"

// Generic Container
//#pragma link "dxPSShapes"
//#pragma link "dxPSContainerLnk"

// dxLayoutControl
//#pragma link "dxPSdxLC2Lnk"

// cxGrid, cxTreeList, cxVerticalGrid and any others cx-family products
//#pragma link "dxPScxGrid6Lnk"
//#pragma link "dxPScxCommon"
//#pragma link "dxPScxExtCommon"

TfmMain *fmMain;

/*TdxFormExplorerChangeNotifier*/
__fastcall TdxFormExplorerChangeNotifier::TdxFormExplorerChangeNotifier(
  TfmMain *AForm) : TdxPSExplorerChangeNotifierAdapter(AForm->Explorer)
{
  FForm = AForm;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormExplorerChangeNotifier::ItemDataLoaded(
  TdxPSExplorerItem* AnItem)
{
  Form->UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TdxFormExplorerChangeNotifier::ItemDataUnloaded(
  TdxPSExplorerItem* AnItem)
{
  Form->UpdateControls();
}
//---------------------------------------------------------------------------

/*TFormHelper*/
HRESULT __stdcall TFormHelper::QueryInterface(const GUID& IID, void **Obj)
{
  if (IID == IID_IdxPSExplorerContextCommandBuilder){
    *Obj = (IdxPSExplorerContextCommandBuilder*)this;
    return true;
  }
  if (IID == IID_IdxPSExplorerTreeContainerHost){
    *Obj = (IdxPSExplorerTreeContainerHost*)this;
    return true;
  }
  if (IID == IID_IUnknown){
    *Obj = this;
    return true;
  }

  return false;
}
//---------------------------------------------------------------------------

ULONG __stdcall TFormHelper::AddRef()
{
   return -1;
}
//---------------------------------------------------------------------------

ULONG __stdcall TFormHelper::Release()
{
   return -1;
}
//---------------------------------------------------------------------------

bool __fastcall TFormHelper::GetFlat(void)
{
  return FForm->GetFlat();
}
//---------------------------------------------------------------------------

TBasedxReportLink* __fastcall TFormHelper::GetReportLink(void)
{
  return FForm->GetReportLink();
}
//---------------------------------------------------------------------------

TcxControl* __fastcall TFormHelper::GetTreeContainerParent(void)
{
  return FForm->GetTreeContainerParent();
}
//---------------------------------------------------------------------------

void __fastcall TFormHelper::UpdateState(void)
{
  FForm->UpdateState();
}
//---------------------------------------------------------------------------

void __fastcall TFormHelper::AddExplorerContextCommand(
      TCustomdxPSExplorerContextCommand* ACommand)
{
  FForm->AddExplorerContextCommand(ACommand);
}
//---------------------------------------------------------------------------

void __fastcall TFormHelper::UpdateExplorerContextCommands(void)
{
  FForm->UpdateExplorerContextCommands();
}
//---------------------------------------------------------------------------

/*TfmMain*/
__fastcall TfmMain::TfmMain(TComponent* Owner)
        : TForm(Owner)
{
  TfmSplash *AfmSplash = new TfmSplash(this);
  try{
    AfmSplash->ShowModal();
  }
  __finally{
    delete AfmSplash;
  }
}
//---------------------------------------------------------------------------

__fastcall TfmMain::~TfmMain(void)
{
  delete FReportDocument;
  delete FExplorerChangeNotifier;
  ExplorerUnloadItemDataClick(NULL);
  delete FExplorerContextCommandToolButtons;
  delete FExplorerContextCommandPopupMenuItems;
  delete FExplorerContextCommandMenuItems;
  delete FExplorerTree;
  delete FFormHelper;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FilePreviewClick(TObject *Sender)
{
  if (IsSelectedItemLoaded())
    ComponentPrinter->Preview(true, NULL);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FilePageSetupClick(TObject *Sender)
{
  if (IsSelectedItemLoaded())
    ComponentPrinter->PageSetup(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FileExitClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FilePrintClick(TObject *Sender)
{
  if (IsSelectedItemLoaded())
    ComponentPrinter->Print(true, NULL, NULL);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerCreateNewFolderClick(TObject *Sender)
{
  if (ExplorerTree->CanCreateFolder())
    Explorer->CreateNewFolder(ExplorerTree->CreationParent);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerDeleteItemClick(TObject *Sender)
{
  ExplorerTree->DeleteSelection(true);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerItemShowPropertySheetsClick(TObject *Sender)
{
  ExplorerTree->ShowSelectedItemPropertySheets();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerLoadItemDataClick(TObject *Sender)
{
  if (ExplorerTree->CanLoadSelectedItemData())
    ExplorerTree->LoadSelectedItemData();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerRenameItemClick(TObject *Sender)
{
  ExplorerTree->BeginEdit(true);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerUnloadItemDataClick(TObject *Sender)
{
  if (ExplorerTree->CanUnloadItemData())
    ExplorerTree->UnloadItemData();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::pmExplorerPopup(TObject *Sender)
{
  UpdateExplorerContextCommands();
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::TreeChangeTimerTimer(TObject *Sender)
{
  TCustomdxPSExplorerItem *Item = ExplorerTree->SelectedItem;
  if ((Item == LastSelectedItem) && (dynamic_cast<TdxPSExplorerItem*>(Item) != 0))
    if (!((TdxPSExplorerItem*)Item)->IsCurrentlyLoaded)
    {
      ((TTimer*)Sender)->Enabled = false;
      LoadItemPreview((TdxPSExplorerItem*)Item);
    }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FormCreate(TObject *Sender)
{
  FExplorerContextCommandMenuItems = new TList();
  FExplorerContextCommandPopupMenuItems = new TList();
  FExplorerContextCommandToolButtons = new TList();
  FFormHelper = new TFormHelper(this);

  AssignDataSets();
  CreatePreviewBox();
  CreateExplorerTree();
  ActiveControl = ExplorerTree->TreeView;
  UpdateControls();
}
//---------------------------------------------------------------------------

// IdxPSExplorerTreeContainerHost
bool __fastcall TfmMain::GetFlat()
{
  return false;
}
//---------------------------------------------------------------------------

TBasedxReportLink* __fastcall TfmMain::GetReportLink()
{
  return ComponentPrinter->CurrentLink;
}
//---------------------------------------------------------------------------

TcxControl* __fastcall TfmMain::GetTreeContainerParent()
{
  return pnlExplorerTreeHost;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::UpdateState()
{
  UpdateControls();
}
//---------------------------------------------------------------------------

//IdxPSExplorerContextCommandBuilder
bool __fastcall TfmMain::IsCommandSeparator(TCustomdxPSExplorerContextCommand *ACommand)
{
  return dynamic_cast<TdxPSExplorerContextCommandSeparator*>(ACommand) != 0;
}

TMenuItem* __fastcall TfmMain::AddExplorerContextCommandMenuItem(TMenuItem *AParent,
  TCustomdxPSExplorerContextCommand *ACommand)
{
  TMenuItem *Result = new TMenuItem(this);
  Result->ImageIndex = ilMain->AddMasked(ACommand->Bitmap, ACommand->Bitmap->TransparentColor);
  Result->Caption = ACommand->Caption;
  Result->Enabled = ACommand->Enabled();
  Result->Hint = ACommand->Hint;
  Result->ShortCut = ACommand->ShortCut;
  Result->Tag = (int)ACommand;
  Result->OnClick = ExplorerContextCommandClick;
  AParent->Insert(0, Result);
  return Result;
}

void __fastcall TfmMain::AddExplorerContextCommand(
  TCustomdxPSExplorerContextCommand *ACommand)
{
  TMenuItem *MenuItem = AddExplorerContextCommandMenuItem(pmExplorer->Items, ACommand);
  if (!IsCommandSeparator(ACommand) &&
    (FExplorerContextCommandPopupMenuItems->IndexOf(MenuItem) == -1))
    FExplorerContextCommandPopupMenuItems->Add(MenuItem);

  MenuItem = AddExplorerContextCommandMenuItem(miExplorer, ACommand);
  if (!IsCommandSeparator(ACommand) &&
    (FExplorerContextCommandMenuItems->IndexOf(MenuItem) == -1))
    FExplorerContextCommandMenuItems->Add(MenuItem);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::UpdateMenuItems()
{
  TMenuItem *AItem;
  for (int I = 0; I < ExplorerContextCommandMenuItemCount; I++)
  {
    AItem = ExplorerContextCommandMenuItems[I];
    AItem->Enabled = ((TCustomdxPSExplorerContextCommand*)AItem->Tag)->Enabled();
  }
}

void __fastcall TfmMain::UpdatePopupMenuItems()
{
  TMenuItem *AItem;
  for (int I = 0; I < ExplorerContextCommandPopupMenuItemCount; I++)
  {
    AItem = ExplorerContextCommandPopupMenuItems[I];
    AItem->Enabled = ((TCustomdxPSExplorerContextCommand*)AItem->Tag)->Enabled();
  }
}

void __fastcall TfmMain::UpdateToolButtons()
{
  TToolButton *AButton;
  for (int I = 0; I < ExplorerContextCommandToolButtonCount; I++)
  {
    AButton = ExplorerContextCommandToolButtons[I];
    AButton->Enabled =
      ((TCustomdxPSExplorerContextCommand*)AButton->Tag)->Enabled();
   }
}

void __fastcall TfmMain::UpdateExplorerContextCommands()
{
  if (!ComponentState.Contains(csDestroying))
  {
    UpdateMenuItems();
    UpdatePopupMenuItems();
    UpdateToolButtons();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::ExplorerContextCommandClick(TObject *Sender)
{
  TCustomdxPSExplorerContextCommand *Command =
    (TCustomdxPSExplorerContextCommand*)((TMenuItem*)Sender)->Tag;
  if (Command->Enabled())
    Command->Execute();
}
//---------------------------------------------------------------------------

TMenuItem* __fastcall TfmMain::GetExplorerContextCommandMenuItem(int Index)
{
  return (TMenuItem*)FExplorerContextCommandMenuItems->Items[Index];
}
//---------------------------------------------------------------------------

int __fastcall TfmMain::GetExplorerContextCommandMenuItemCount()
{
  return FExplorerContextCommandMenuItems->Count;
}
//---------------------------------------------------------------------------

TMenuItem* __fastcall TfmMain::GetExplorerContextCommandPopupMenuItem(int Index)
{
  return (TMenuItem*)FExplorerContextCommandPopupMenuItems->Items[Index];
}
//---------------------------------------------------------------------------

int __fastcall TfmMain::GetExplorerContextCommandPopupMenuItemCount()
{
  return FExplorerContextCommandPopupMenuItems->Count;
}
//---------------------------------------------------------------------------

TToolButton* __fastcall TfmMain::GetExplorerContextCommandToolButton(int Index)
{
  return (TToolButton*)FExplorerContextCommandToolButtons->Items[Index];
}
//---------------------------------------------------------------------------

int __fastcall TfmMain::GetExplorerContextCommandToolButtonCount()
{
  return FExplorerContextCommandToolButtons->Count;
}
//---------------------------------------------------------------------------

bool __fastcall TfmMain::GetIsReportItemSelected()
{
  return dynamic_cast<TdxPSExplorerItem*>(ExplorerTree->SelectedItem) != 0;
}
//---------------------------------------------------------------------------

bool __fastcall TfmMain::GetIsReportValid()
{
  return PreviewGraphic != NULL;
}
//---------------------------------------------------------------------------

TGraphic* __fastcall TfmMain::GetPreviewGraphic()
{
  if (ReportDocument != NULL)
    return ReportDocument->Preview;
  else
    return NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::AssignDataSets()
{
  mdFolders->Close();
  mdFolders->LoadFromBinaryFile("..\\..\\Data\\Folders(AutoInc).dat");
  mdFolders->Open();

  mdItems->Close();
  mdItems->LoadFromBinaryFile("..\\..\\Data\\Items(AutoInc).dat");
  mdItems->Open();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::CreateExplorerTree()
{
  IdxPSExplorerContextCommands *ExplorerContextCommands = NULL;
  FExplorerTree =
    (TdxPSExplorerTreeViewContainer*)Explorer->CreateTree(
      (IdxPSExplorerTreeContainerHost *)FFormHelper);
  ExplorerTree->TreeView->OnChange = DoExplorerTreeChange;
  ExplorerTree->TreeView->OnDblClick = DoExplorerTreeDblClick;
  ExplorerTree->TreeView->OnMouseUp = DoShowExplorerPopup;

  Explorer->BuildTree(ExplorerTree);
  FExplorerChangeNotifier = new TdxFormExplorerChangeNotifier(this);

  if (Supports(Explorer, IID_IdxPSExplorerContextCommands, &ExplorerContextCommands))
	  ExplorerContextCommands->BuildCommandSet((IdxPSExplorerContextCommandBuilder*)FFormHelper);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::CreatePreviewBox()
{
  FPreviewBox = new TdxPSImageScrollBox(this);
  FPreviewBox->Parent = this;
  FPreviewBox->Align = alClient;
  FPreviewBox->Visible = true;//}false;
}
//---------------------------------------------------------------------------

bool __fastcall TfmMain::IsSelectedItemLoaded()
{
  ExplorerTree->LoadSelectedItemData();
  return ExplorerTree->IsSelectedItemCurrentlyLoaded();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::LoadItemPreview(TdxPSExplorerItem *AnItem)
{
  delete FReportDocument;
  TStream *Stream = AnItem->CreateDataStream(smRead);
  if (Stream != NULL){
	try{
	  try{
#if __BORLANDC__ >= 0x610   // BCB version < 12
		FReportDocument = TBasedxReportLink::ExtractReportDocument(
		  Stream, false);
#else
		FReportDocument = TBasedxReportLink::ExtractReportDocument(
		  __classid(TBasedxReportLink), Stream, false);
#endif
	  }
      catch(...){
        FReportDocument = NULL;
      }
    }
	__finally{
      delete Stream;
    }
  }
  UpdateControls();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::UpdateControls()
{
  actLoad->Enabled = ExplorerTree->CanLoadSelectedItemData();
  actUnLoad->Enabled = ExplorerTree->CanUnloadItemData();
  actPageSetup->Enabled = IsReportItemSelected;
  actPrintPreview->Enabled = IsReportItemSelected;
  actPrint->Enabled = IsReportItemSelected;
  actNewFolder->Enabled = ExplorerTree->CanCreateFolder();
  actDelete->Enabled = ExplorerTree->CanDeleteSelection();
  actRename->Enabled = ExplorerTree->CanRenameSelectedItem();
  actProperties->Enabled = ExplorerTree->CanShowPropertySheetsForSelectedItem();

  PreviewBox->Enabled = IsReportValid;
  PreviewBox->Picture->Assign(PreviewGraphic);
  if (IsReportValid)
    PreviewBox->HintText = "";
  else
    PreviewBox->HintText = Dxpsres_sdxNone;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::DoExplorerTreeChange(TObject *Sender, TTreeNode *ANode)
{
  TreeChangeTimer->Enabled = false;
  LastSelectedItem = ExplorerTree->SelectedItem;
  TreeChangeTimer->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::DoExplorerTreeDblClick(TObject *Sender)
{
  LastSelectedItem = ExplorerTree->SelectedItem;
  TreeChangeTimerTimer(TreeChangeTimer);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::DoShowExplorerPopup(TObject *Sender, TMouseButton Button,
  TShiftState Shift, int X, int Y)
{
  if (Button == mbRight)
    pmExplorer->Popup(Mouse->CursorPos.x, Mouse->CursorPos.y);
}
//---------------------------------------------------------------------------



void __fastcall TfmMain::ExplorerItemDataLoadError(
      TCustomdxPSExplorer *Sender, TdxPSExplorerItem *AnItem,
      bool &AShowErrorMessage, String &AText)
{
  const String CRLF = "\n";
  const String ErrorText = "Cannot Load Item %s." + CRLF +
    "You should uncomment appropriate directives in h/cpp files" + CRLF +
    CRLF +
    "Please read ReadMe.txt.";
  AShowErrorMessage = true;
  AText = Format(ErrorText, ARRAYOFCONST((((TdxPSDBBasedExplorerItem*)AnItem)->Name)));
}
//---------------------------------------------------------------------------

