//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoEditor.h"
#include "CustomDrawDemoMain.h"
#include "CustomDrawDemoTypes.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxMRUEdit"
#pragma link "cxRadioGroup"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBLookupComboBox"
#pragma link "cxMemo"
#pragma resource "*.dfm"
TCustomDrawDemoEditorForm *CustomDrawDemoEditorForm;
//---------------------------------------------------------------------------
__fastcall TCustomDrawDemoEditorForm::TCustomDrawDemoEditorForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::FormCreate(TObject *Sender)
{
  FillCustomDrawItemList();
  FillBkImageTypeList();
  FillColorSchemeList();
  OpenDialog->InitialDir = ExtractFilePath(Application->ExeName);
  tlCustomDrawItems->Root->getFirstChild()->Focused = true;
}

void TCustomDrawDemoEditorForm::FillCustomDrawItemList()
{
  TcxItemCustomDrawInfo *AItem;
  TcxCustomDrawInfo *AInfo = CustomDrawDemoMainForm->CustomDrawInfo;
  for (int I = 0; I < AInfo->Count; I++){
    TcxTreeListNode *ANode = tlCustomDrawItems->Root->AddChild();
    AItem = AInfo->GetItemByIndex(I);
    ANode->Data = AItem;
    ANode->Values[0] = CustomDrawAreaNames[AItem->DrawArea];
  }
}

void TCustomDrawDemoEditorForm::FillBkImageTypeList()
{
  for (int I = 0; I < BkImageCount; I++)
    if ((TBkImage)I == bkiUserDefined)
      mruBkImage->Properties->LookupItems->Add("User Defined");
    else
      mruBkImage->Properties->LookupItems->Add(BkImageResNames[I]);
}

void TCustomDrawDemoEditorForm::FillColorSchemeList()
{
  for (int I = 0; I < ColorSchemeCount; I++)
    cbGradient->Properties->Items->Add(ColorSchemeNames[I]);
}

//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::AdjustSettings(TcxTreeListNode *ASelectedNode)
{
    TcxItemCustomDrawInfo *AItem = ((TcxItemCustomDrawInfo*)ASelectedNode->Data);
    rbBackGroundImage->Checked = AItem->DrawingStyle == cdsBkImage;
    rbGradient->Checked = AItem->DrawingStyle == cdsGradient;
    rpendsOnTheData->Checked = AItem->DrawingStyle == cdsDependsOnData;
    rfaultDrawing->Checked = AItem->DrawingStyle == cdsDefaultDrawing;
    chbOwnerDrawText->Checked = AItem->OwnerTextDraw;
    rpendsOnTheData->Visible = AItem->ItemType == itCell;
    mruBkImage->Text = BkImageResNames[(int)AItem->BkImageType];
    cbGradient->ItemIndex = (int)AItem->ColorScheme;
    AdjustControlsEnable();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::tlCustomDrawItemsSelectionChanged(TObject *Sender)
{
  if (tlCustomDrawItems->SelectionCount > 0)
    AdjustSettings(tlCustomDrawItems->Selections[0]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::rbRadioButtonClick(TObject *Sender)
{
  ((TcxRadioButton*)Sender)->Checked = true;
  TcxItemCustomDrawInfo *AItem = GetSelectedDrawItem();
  if (AItem != NULL){
    AItem->DrawingStyle = (TCustomDrawingStyle)((TcxRadioButton*)Sender)->Tag;
    AdjustControlsEnable();
    CustomDrawDemoMainForm->cxDBTreeList->LayoutChanged();
  }
}
//---------------------------------------------------------------------------

TBkImage TCustomDrawDemoEditorForm::GetBkImageTypeByName(String AName)
{
  TBkImage Result = bkiUserDefined;
  for (int I = 0; I < BkImageCount; I++)
    if (BkImageResNames[I] == AName){
      Result = (TBkImage)I;
      break;
    }
  return Result;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::mruBkImagePropertiesEditValueChanged(TObject *Sender)
{
  GetSelectedDrawItem()->BkImageType =
    GetBkImageTypeByName(((TcxMRUEdit*)Sender)->EditValue);
  CustomDrawDemoMainForm->cxDBTreeList->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::mruBkImagePropertiesButtonClick(TObject *Sender)
{
  if (OpenDialog->Execute()){
    Graphics::TBitmap *ABitmap = new Graphics::TBitmap();
    ABitmap->LoadFromFile(OpenDialog->FileName);
    GetSelectedDrawItem()->Bitmap = ABitmap;
    ((TcxCustomEdit*)Sender)->EditValue = "User Defined";
    CustomDrawDemoMainForm->cxDBTreeList->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::cbGradientPropertiesChange(TObject *Sender)
{
  GetSelectedDrawItem()->ColorScheme = (TcxColorScheme)((TcxComboBox*)Sender)->ItemIndex;
  CustomDrawDemoMainForm->cxDBTreeList->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::chbOwnerDrawTextPropertiesChange(TObject *Sender)
{
  AdjustControlsEnable();
  GetSelectedDrawItem()->OwnerTextDraw = chbOwnerDrawText->Checked;
  CustomDrawDemoMainForm->cxDBTreeList->LayoutChanged();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::sbFontClick(TObject *Sender)
{
  if (FontDialog->Execute()){
    TFont *AFont = new TFont();
    AFont->Assign(FontDialog->Font);
    GetSelectedDrawItem()->Font = AFont;
    CustomDrawDemoMainForm->cxDBTreeList->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::btnCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::AdjustControlsEnable()
{
  mruBkImage->Enabled = rbBackGroundImage->Checked;
  cbGradient->Enabled = rbGradient->Checked;
  chbOwnerDrawText->Visible = GetSelectedDrawItem()->ItemType == itCell &&
    !(rfaultDrawing->Checked || rpendsOnTheData->Checked);
  TcxItemCustomDrawType AItemType = GetSelectedDrawItem()->ItemType;
  lbFont->Enabled =
    AItemType == itText || AItemType == itCell && chbOwnerDrawText->Checked;
  sbFont->Enabled = lbFont->Enabled;
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TCustomDrawDemoEditorForm::GetSelectedDrawItem()
{
  TcxItemCustomDrawInfo *Result = NULL;
  if (tlCustomDrawItems->SelectionCount > 0)
    Result = ((TcxItemCustomDrawInfo*)tlCustomDrawItems->Selections[0]->Data);
  return Result;
}
//---------------------------------------------------------------------------


