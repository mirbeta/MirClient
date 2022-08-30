//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawDemoEditor.h"
#include "CustomDrawDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
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
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TCustomDrawDemoEditorForm *CustomDrawDemoEditorForm;
//---------------------------------------------------------------------------
__fastcall TCustomDrawDemoEditorForm::TCustomDrawDemoEditorForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::AdjustControlsEnable()
{
  mruBkImage->Enabled = rbBackGroundImage->Checked;
  cbGradient->Enabled = rbGradient->Checked;
  chbOwnerDrawText->Visible = GetSelectedDrawItem()->ItemType == itCell &&
    !(rbDefaultDrawing->Checked || rbDependsOnTheData->Checked);
  lbFont->Enabled = chbOwnerDrawText->Checked && chbOwnerDrawText->Enabled;
  sbFont->Enabled = lbFont->Enabled;
}
//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::AdjustSettings(TcxCategoryRow* ASelectedRow)
{
  gbEventHandlerSettings->Visible = ASelectedRow->Tag != 0;
  if (gbEventHandlerSettings->Visible) {
    TcxItemCustomDrawInfo *AItem = ((TcxItemCustomDrawInfo*)ASelectedRow->Tag);
    rbBackGroundImage->Checked = AItem->DrawingStyle == cdsBkImage;
    rbGradient->Checked = AItem->DrawingStyle == cdsGradient;
    rbDependsOnTheData->Checked = AItem->DrawingStyle == cdsDependsOnData;
    rbDefaultDrawing->Checked = AItem->DrawingStyle == cdsDefaultDrawing;
    chbOwnerDrawText->Checked = AItem->OwnerTextDraw;
    rbDependsOnTheData->Visible = AItem->ItemType == itCell;
    mruBkImage->Text = BkImageResNames[(int)AItem->BkImageType];
    cbGradient->ItemIndex = (int)AItem->ColorScheme;
    AdjustControlsEnable();
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

void TCustomDrawDemoEditorForm::FillCustomDrawItemList()
{
  TcxItemCustomDrawInfo*  AItem;
  TcxCategoryRow*  ACategory;
  for (int i=0; i < CustomDrawDemoMainForm->CustomDrawInfo->Count; i++) {
    ACategory = (TcxCategoryRow*)vgCustomDrawItems->AddChild(vgDrawItemCategory, __classid(TcxCategoryRow));
    AItem = CustomDrawDemoMainForm->CustomDrawInfo->GetItemByIndex(i);
    ACategory->Tag = (int)AItem;
    ACategory->Properties->Caption = CustomDrawAreaNames[AItem->DrawArea];
  }
}
//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::FillBkImageTypeList()
{
  for (int I = 0; I < BkImageCount; I++)
    if ((TBkImage)I == bkiUserDefined)
      mruBkImage->Properties->LookupItems->Add("User Defined");
    else
      mruBkImage->Properties->LookupItems->Add(BkImageResNames[I]);
}
//---------------------------------------------------------------------------

void TCustomDrawDemoEditorForm::FillColorSchemeList()
{
  for (int I = 0; I < ColorSchemeCount; I++)
    cbGradient->Properties->Items->Add(ColorSchemeNames[I]);
}
//---------------------------------------------------------------------------

TcxItemCustomDrawInfo* TCustomDrawDemoEditorForm::GetSelectedDrawItem()
{
  TcxItemCustomDrawInfo* Result = NULL;
  if (vgCustomDrawItems->FocusedRow != NULL)
    Result = (TcxItemCustomDrawInfo*)vgCustomDrawItems->FocusedRow->Tag;
  return (Result);  
}
//---------------------------------------------------------------------------


void __fastcall TCustomDrawDemoEditorForm::FormCreate(TObject *Sender)
{
  FillCustomDrawItemList();
  FillBkImageTypeList();
  FillColorSchemeList();
  OpenDialog->InitialDir = ExtractFilePath(Application->ExeName);
  vgCustomDrawItems->FocusedRow = vgCustomDrawItems->Rows->Items[1];
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::btnCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::mruBkImagePropertiesButtonClick(
      TObject *Sender)
{
  if (OpenDialog->Execute()){
    Graphics::TBitmap *ABitmap = new Graphics::TBitmap();
    ABitmap->LoadFromFile(OpenDialog->FileName);
    GetSelectedDrawItem()->Bitmap = ABitmap;
    ((TcxCustomEdit*)Sender)->EditValue = "User Defined";
    CustomDrawDemoMainForm->cxDBVerticalGrid->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::mruBkImagePropertiesEditValueChanged(
      TObject *Sender)
{
  GetSelectedDrawItem()->BkImageType =
    GetBkImageTypeByName(((TcxMRUEdit*)Sender)->EditValue);
  CustomDrawDemoMainForm->cxDBVerticalGrid->LayoutChanged();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::rbRadioButtonClick(
      TObject *Sender)
{
  ((TcxRadioButton*)Sender)->Checked = true;
  TcxItemCustomDrawInfo *AItem = GetSelectedDrawItem();
  if (AItem != NULL){
    AItem->DrawingStyle = (TCustomDrawingStyle)((TcxRadioButton*)Sender)->Tag;
    AdjustControlsEnable();
    CustomDrawDemoMainForm->cxDBVerticalGrid->LayoutChanged();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::chbOwnerDrawTextPropertiesChange(
      TObject *Sender)
{
  AdjustControlsEnable();
  GetSelectedDrawItem()->OwnerTextDraw = chbOwnerDrawText->Checked;
  CustomDrawDemoMainForm->cxDBVerticalGrid->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::sbFontClick(TObject *Sender)
{
  if (FontDialog->Execute()){
    TFont *AFont = new TFont();
    AFont->Assign(FontDialog->Font);
    GetSelectedDrawItem()->Font = AFont;
    CustomDrawDemoMainForm->cxDBVerticalGrid->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::cbGradientPropertiesChange(
      TObject *Sender)
{
  GetSelectedDrawItem()->ColorScheme = (TcxColorScheme)((TcxComboBox*)Sender)->ItemIndex;
  CustomDrawDemoMainForm->cxDBVerticalGrid->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::vgCustomDrawItemsItemChanged(
      TObject *Sender, TcxCustomRow *AOldRow, int AOldCellIndex)
{
  if (vgCustomDrawItems->FocusedRow != NULL)
    AdjustSettings((TcxCategoryRow*)vgCustomDrawItems->FocusedRow);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::vgCustomDrawItemsDrawRowHeader(
      TObject *Sender, TcxCanvas *ACanvas, TcxvgPainter *APainter,
      TcxCustomRowHeaderInfo *AHeaderViewInfo, bool &Done)
{
  AHeaderViewInfo->FocusRect = Rect(0, 0, 0, 0);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawDemoEditorForm::vgCustomDrawItemsStylesGetCategoryStyle(
      TObject *Sender, TcxCustomRow *ARow, TcxStyle *&AStyle)
{
   if (vgCustomDrawItems->FocusedRow == ARow)
    AStyle = CustomDrawDemoDataDM->cxVerticalGridStyleSheetDevExpress->Styles->Selection; else
   if (ARow->Level > 0)
     AStyle = CustomDrawDemoDataDM->cxVerticalGridStyleSheetDevExpress->Styles->Content;
}
//---------------------------------------------------------------------------

