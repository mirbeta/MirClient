//---------------------------------------------------------------------------

#include <vcl.h>
#include "..\cxDemosBCB.inc"
#pragma hdrstop

#include "ColumnsMultiEditorsDemoMain.h"
#include "ColumnsMultiEditorsDemoData.h"
#include "ColumnsMultiEditorsDemoPopup.h"
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
#pragma link "cxDBEditRepository"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxTLExportLink"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TColumnsMultiEditorsDemoMainForm *ColumnsMultiEditorsDemoMainForm;
//---------------------------------------------------------------------------

/* TColumnsMultiEditorsDemoDataSource implementation*/

const String ImageFileName = "..\\..\\"
    #if BCB6
      "Data"
    #else
      "Data"
    #endif
     "\\Car.bmp";
const String scxDefaultValueBlobItem = "Please add text here...";
const String scxDefaultValueButtonItem = "Press me...";
const String scxDefaultValueMemoItem =
  "Robbins studied drama at UCLA where he graduated with honors in 1981. "
  "That same year, he formed the Actors' Gang, an experimental ensemble that"
  " expressed radical political observations through the European avant-garde"
  " form of theater.";
const String scxDefaultValueMRUItem = "What's your favorite color?";
const String scxDefaultValuePopupItem = "Pop me up...";
const String scxDefaultValueTextItem = "Text";
const String scxDefaultValueMRUItemClick = "You've pressed the MRU Inplace Editor button.";

void TColumnsMultiEditorsDemoDataSource::FillEditorsNames()
{
  FEditorNames[(int)etImage] = "Graphic Editor";
  FEditorNames[(int)etImageComboBox] = "Image ComboBox Editor";
  FEditorNames[(int)etLookupComboBox] = "Lookup ComboBox Editor";
  FEditorNames[(int)etMask] = "Advanced Mask Editor";
  FEditorNames[(int)etMemo] = "Rich Text Editor";
}

void TColumnsMultiEditorsDemoDataSource::FillEditorsValues()
{
  FValues[(int)etBlob] = scxDefaultValueBlobItem;
  FValues[(int)etButton] = scxDefaultValueButtonItem;
  FValues[(int)etCalc] = 12345;
  FValues[(int)etCheckBox] = True;
  FValues[(int)etComboBox] = "Green";
  FValues[(int)etCurrency] = 555.35;
  FValues[(int)etDate] = Date();
  FValues[(int)etHyperLink] = "http://www.devexpress.com";
  TcxImage *AImage = new TcxImage((TComponent*)NULL);
  __try{
    AImage->Picture->LoadFromFile(ImageFileName);
    FValues[(int)etImage] = AImage->EditValue;
  }
  __finally{
    AImage->Free();
  }
  FValues[(int)etImageComboBox] = 2;
  FValues[(int)etLookupComboBox] = ColumnsMultiEditorsDemoDataDM->mdPersonsID->Value;
  FValues[(int)etMask] = "(234)897-235";
  TStringList *S = new TStringList();
  __try{
	S->LoadFromFile("..\\..\\Data\\Data.rtf");
	FValues[(int)etMemo] = S->Text;
  }
  __finally{
	S->Free();
  }
  FValues[(int)etMRU] =  scxDefaultValueMRUItem;
  FValues[(int)etPopup] = scxDefaultValuePopupItem;
  FValues[(int)etRadioGroup] = 0;
  FValues[(int)etSpinItem] = 10;
  FValues[(int)etText] = scxDefaultValueTextItem;
  FValues[(int)etTime] = Now();
}

void TColumnsMultiEditorsDemoDataSource::FillEditorsCategories()
{
  FCategories[(int)etBlob] = ectBlobs;
  FCategories[(int)etButton] = ectStandard;
  FCategories[(int)etCalc] = ectPopups;
  FCategories[(int)etCheckBox] = ectStandard;
  FCategories[(int)etComboBox] = ectComboBoxes;
  FCategories[(int)etCurrency] = ectStandard;
  FCategories[(int)etDate] = ectPopups;
  FCategories[(int)etHyperLink] = ectStandard;
  FCategories[(int)etImage] = ectBlobs;
  FCategories[(int)etImageComboBox] = ectComboBoxes;
  FCategories[(int)etLookupComboBox] = ectComboBoxes;
  FCategories[(int)etMask] = ectStandard;
  FCategories[(int)etMemo] = ectBlobs;
  FCategories[(int)etMRU] = ectComboBoxes;
  FCategories[(int)etPopup] = ectPopups;
  FCategories[(int)etRadioGroup] = ectStandard;
  FCategories[(int)etSpinItem] = ectStandard;
  FCategories[(int)etText] = ectStandard;
  FCategories[(int)etTime] = ectStandard;
}

__fastcall TColumnsMultiEditorsDemoDataSource::TColumnsMultiEditorsDemoDataSource(
    TcxVirtualTreeList *ATreeList, TcxEditRepository *AEditRepository)
{
  FTreeList = ATreeList;
  FEditRepository = AEditRepository;
  FillEditorsNames();
  FillEditorsValues();
  FillEditorsCategories();
}
//---------------------------------------------------------------------------

int TColumnsMultiEditorsDemoDataSource::RootCount(void)
{
  return cxEditorsCategoryCount;
}
//---------------------------------------------------------------------------

int __fastcall TColumnsMultiEditorsDemoDataSource::GetRecordCount(void)
{
  return RootCount() + cxEditorsTypeCount;
}
//---------------------------------------------------------------------------

Variant __fastcall TColumnsMultiEditorsDemoDataSource::GetValue(
    void * ARecordHandle, void * AItemHandle)
{
  int ARecordIndex = (int)ARecordHandle;
  Variant Result;
  switch ((int)AItemHandle){
    case 0:
      if (ARecordIndex < RootCount())
        Result = EditorsCategoryNames[ARecordIndex];
      else
        Result = GetEditorName(ARecordIndex - RootCount());
      break;
    case 1:
      if ((int)ARecordHandle > RootCount() - 1)
        Result = GetEditorValue(ARecordIndex - RootCount());
  }
  return Result;
}
//---------------------------------------------------------------------------

void * __fastcall TColumnsMultiEditorsDemoDataSource::GetParentRecordHandle(void * ARecordHandle)
{
  if ((int)ARecordHandle < RootCount())
  {  
	return (TcxTreeListCustomDataSource::GetParentRecordHandle(ARecordHandle));
  }
  else
  {
	return (void*)(FCategories[(int)ARecordHandle - RootCount()]);
  }
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoDataSource::SetValue(
    void * ARecordHandle, void * AItemHandle, const Variant &AValue)
{
  int ARecordIndex = Integer(ARecordHandle);
  if ((ARecordIndex > RootCount() - 1) && ((int)AItemHandle == 1))
    SetEditorValue(ARecordIndex - RootCount(), AValue);
}
//---------------------------------------------------------------------------

String TColumnsMultiEditorsDemoDataSource::GetEditorName(int AEditorIndex)
{
  String Result = FEditorNames[AEditorIndex];
  if (Result == ""){
    Result = FEditRepository->Items[AEditorIndex]->Name;
    Result = Result.SubString(FEditRepository->Name.Length() + 1, Result.Length());
    Result = Result.SubString(1, Result.Pos("Item") - 1) + " Editor";
  }
  return Result;
}
//---------------------------------------------------------------------------

Variant TColumnsMultiEditorsDemoDataSource::GetEditorValue(int AEditorIndex)
{
  return FValues[AEditorIndex];
}
//---------------------------------------------------------------------------

void TColumnsMultiEditorsDemoDataSource::SetEditorValue(
    int AEditorIndex, const Variant AValue)
{
  FValues[AEditorIndex] = AValue;
}
//---------------------------------------------------------------------------



/*TColumnsMultiEditorsDemoMainForm implementation*/

__fastcall TColumnsMultiEditorsDemoMainForm::TColumnsMultiEditorsDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::FormShow(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed-> First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  FPopupForm = new TColumnsMultiEditorsDemoPopupForm(NULL);
  EditRepositoryPopupItem->Properties->PopupControl = FPopupForm->pnlPopup;
  TreeList->DataController->CustomDataSource =
    new TColumnsMultiEditorsDemoDataSource(TreeList, EditRepository);
  TreeList->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::FormDestroy(TObject *Sender)
{
  FPopupForm->Free();
  TreeList->DataController->CustomDataSource->Free();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::clnSampleGetEditProperties(
  TcxTreeListColumn *Sender, TcxTreeListNode *ANode, TcxCustomEditProperties *&EditProperties)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  if (GetNodeItemIndex(ANode) < 0) return;
    EditProperties = EditRepository->Items[GetNodeItemIndex(ANode)]->Properties;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::EditRepositoryButtonItemPropertiesButtonClick(
  TObject *Sender, int AButtonIndex)
{
  ShowMessage("Press me...");
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::EditRepositoryPopupItemPropertiesInitPopup(
  TObject *Sender)
{
  FPopupForm->PopupEdit = (TcxPopupEdit*)Sender;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::TreeListStylesGetContentStyle(
  TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn, TcxTreeListNode *ANode, TcxStyle *&AStyle)
{
  if ((ANode != NULL)&&(ANode->Level == 0))
  {
    AStyle = ColumnsMultiEditorsDemoDataDM->stlGroupNode;
  }
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoMainForm::TreeListStylesGetNodeIndentStyle(
  TcxCustomTreeList *Sender, TcxTreeListNode *ANode, int ALevel, TcxStyle *&AStyle)
{
  AStyle = ColumnsMultiEditorsDemoDataDM->cxStyle1;
}
//---------------------------------------------------------------------------

int TColumnsMultiEditorsDemoMainForm::GetNodeItemIndex(TcxTreeListNode *ANode)
{
  int res = -1;
  if (ANode->Level != 0)
  {
	res = (int)(((TcxVirtualTreeListNode*)ANode)->RecordHandle) - RootCount();
  }
  return res;
}
//---------------------------------------------------------------------------

int TColumnsMultiEditorsDemoMainForm::RootCount()
{
  return ((TColumnsMultiEditorsDemoDataSource*)
	TreeList->DataController->CustomDataSource)->RootCount();
}
//---------------------------------------------------------------------------

 void __fastcall TColumnsMultiEditorsDemoMainForm::miExporttoHTMLClick(TObject *Sender)
{
  String AFileName = "TreeListExport.html";
  cxExportTLToHTML(AFileName, TreeList);
  AfterExport(AFileName);
}
//---------------------------------------------------------------------------

 void __fastcall TColumnsMultiEditorsDemoMainForm::ExportToXLS1Click(TObject *Sender)
{
  String AFileName = "TreeListExport.xls";
  cxExportTLToExcel(AFileName, TreeList);
  AfterExport(AFileName);
}
//---------------------------------------------------------------------------

 void __fastcall TColumnsMultiEditorsDemoMainForm::miExporttoXLSXClick(TObject *Sender)
{
  String AFileName = "TreeListExport.xlsx";
  cxExportTLToXLSX(AFileName, TreeList);
  AfterExport(AFileName);
}
//---------------------------------------------------------------------------

 void TColumnsMultiEditorsDemoMainForm::AfterExport(AnsiString AFileName)
{
 AnsiString S = "Do you want to open " + AFileName + " file?";
 if (FileExists(AFileName)) {
   if (MessageBox(0, S.c_str(), "Export", MB_YESNO) == mrYes) {
	 ShellExecute(0, "OPEN", AFileName.c_str(), NULL, NULL, SW_SHOWNORMAL);
   }
 }
}
//---------------------------------------------------------------------------

