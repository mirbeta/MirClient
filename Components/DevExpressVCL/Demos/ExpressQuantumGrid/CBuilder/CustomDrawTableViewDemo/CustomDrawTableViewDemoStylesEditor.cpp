//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomDrawTableViewDemoStylesEditor.h"
#include "CustomDrawTableViewDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxMRUEdit"
#pragma link "cxTextEdit"
#pragma link "cxRadioGroup"
#pragma link "cxImageComboBox"
#pragma resource "*.dfm"
TCustomDrawTableViewDemoStylesEditorForm *CustomDrawTableViewDemoStylesEditorForm;
//---------------------------------------------------------------------------
__fastcall TCustomDrawTableViewDemoStylesEditorForm::TCustomDrawTableViewDemoStylesEditorForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetFontVisibility(bool IsVisible)
{
  lbFont->Visible = IsVisible;
  sbFont->Visible = IsVisible;
  pnSampleText->Visible = IsVisible;
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetDependsOnTheDataVisibility(bool IsVisible)
{
  rbDependsOnTheData->Visible = IsVisible;
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetIndicatorCellVisibility(bool AIsVisible)
{
  lbIndicatorGlyph->Visible = AIsVisible;
  icbIndicatorImages->Visible = AIsVisible;
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetIndicatorImage(TViewType AViewType)
{
  icbIndicatorImages->ItemIndex = CustomDrawTableViewDemoMainForm->IndicatorImageIndex[AViewType];
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::tvCustomDrawItemsClick(
      TObject *Sender)
{
  if (tvCustomDrawItems->Selected != NULL) {
    if (tvCustomDrawItems->Selected->Data == NULL)
      gbEventHandlerSettings->Visible = false;
    else {
      if (!gbEventHandlerSettings->Visible)
        gbEventHandlerSettings->Visible = true;
      switch(((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea) {
        case cdaPartBackGround: {
          SetDependsOnTheDataVisibility();
          SetFontVisibility();
          SetIndicatorCellVisibility(false);
        }; break;
        case cdaColumnHeader:
        case cdaFooterCell:
        case cdaGroupCell: {
          SetDependsOnTheDataVisibility();
          SetFontVisibility(true);
          SetIndicatorCellVisibility(false);
        }; break;
        case cdaCell: {
          SetDependsOnTheDataVisibility(true);
          SetFontVisibility(true);
          SetIndicatorCellVisibility(false);
        }; break;
        case cdaIndicatorCell: {
          SetDependsOnTheDataVisibility();
          SetFontVisibility(false);
          SetIndicatorCellVisibility(true);
          SetIndicatorImage(((PCustomDrawItem)(tvCustomDrawItems->Selected->Data))->ViewType);
        }; break;
      };
        SetProperties(CustomDrawTableViewDemoMainForm->CustomDrawingStyle[
        ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
        [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea]);
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetProperties(TCustomDrawingStyle ACustomDrawingStyle)
{
  FIsUpdating = true;
  mruBkImage->Text = GetBkImageText();
  cbGradient->Text = GetGradientColorText();
  FIsUpdating = false;
  SetFont();
  switch(ACustomDrawingStyle) {
    case cdsBkImage: rbBackGroundImageClick(rbBackGroundImage); break;
    case cdsGradient: rbGradientClick(rbGradient); break;
    case cdsDependsOnData: rbDependsOnTheDataClick(rbDependsOnTheData); break;
    case cdsDefaultDrawing: rbDafaultDrawingClick(rbDafaultDrawing); break;
  }
}
//---------------------------------------------------------------------------

String __fastcall TCustomDrawTableViewDemoStylesEditorForm::GetBkImageText()
{
  if ((tvCustomDrawItems->Selected != NULL) && (tvCustomDrawItems->Selected->Data != NULL))
    switch (CustomDrawTableViewDemoMainForm->CustomBkImage[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
    [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea]) {
      case bkiTile: return("Tile");
      case bkiSky: return("Sky");
      case bkiEgypt: return("Egypt");
      case bkiMyFace: return("My Face");
      case bkiUserDefined: return("User Defined");
    }
  return("");
}
//---------------------------------------------------------------------------

String __fastcall TCustomDrawTableViewDemoStylesEditorForm::GetGradientColorText()
{
  if ((tvCustomDrawItems->Selected != NULL) && (tvCustomDrawItems->Selected->Data != NULL))
    switch (CustomDrawTableViewDemoMainForm->CustomColorScheme[((PCustomDrawItem)tvCustomDrawItems->
      Selected->Data)->ViewType][((PCustomDrawItem)tvCustomDrawItems->
      Selected->Data)->CustomDrawArea]) {
      case csGrey: return("Grey");
      case csGold: return("Gold");
      case csBlue: return("Blue");
      case csGreen: return("Green");
    }
  return("");
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::sbFontClick(
      TObject *Sender)
{
  FontDialog->Font->Assign(CustomDrawTableViewDemoMainForm->Font[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
  [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea]);
  if (FontDialog->Execute()) {
    pnSampleText->Font->Assign(FontDialog->Font);
    CustomDrawTableViewDemoMainForm->Font[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
    [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = FontDialog->Font;
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::cbGradientPropertiesChange(
      TObject *Sender)
{
  CustomDrawTableViewDemoTypesH::TColorScheme ColorScheme;
  if (FIsUpdating) return;
  if (tvCustomDrawItems->Selected != NULL) {
    ColorScheme = csGrey;
    if (((TcxComboBox*)Sender)->Text == "Blue")
      ColorScheme = csBlue; else
    if (((TcxComboBox*)Sender)->Text == "Gold")
      ColorScheme = csGold; else
    if (((TcxComboBox*)Sender)->Text == "Green")
      ColorScheme = csGreen;
    CustomDrawTableViewDemoMainForm->CustomColorScheme[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
    [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = ColorScheme;
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::rbBackGroundImageClick(TObject *Sender)
{
  if (!((TcxRadioButton*)Sender)->Checked)
    ((TcxRadioButton*)Sender)->Checked = true;
  DisableControls(mruBkImage);
  CustomDrawTableViewDemoMainForm->CustomDrawingStyle[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType][
  ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = cdsBkImage;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::SetFont()
{
  pnSampleText->Font->Assign(CustomDrawTableViewDemoMainForm->Font[
  ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType][
  ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea]);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::rbGradientClick(
      TObject *Sender)
{
  if(!((TcxRadioButton*)Sender)->Checked)
    ((TcxRadioButton*)Sender)->Checked = true;
  DisableControls(cbGradient);
  CustomDrawTableViewDemoMainForm->CustomDrawingStyle[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
  [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = cdsGradient;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::rbDafaultDrawingClick(
      TObject *Sender)
{
  if(!((TcxRadioButton*)Sender)->Checked)
    ((TcxRadioButton*)Sender)->Checked = true;
  DisableControls(NULL);
  CustomDrawTableViewDemoMainForm->CustomDrawingStyle[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType][
  ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = cdsDefaultDrawing;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::rbDependsOnTheDataClick(
      TObject *Sender)
{
  if(!((TcxRadioButton*)Sender)->Checked)
    ((TcxRadioButton*)Sender)->Checked = true;
  DisableControls(NULL);
  CustomDrawTableViewDemoMainForm->CustomDrawingStyle[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
  [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = cdsDependsOnData;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::DisableControls(TcxCustomMaskEdit* Sender)
{
  for(int i=0; i < ComponentCount - 1; i++)
    if ((dynamic_cast<TcxCustomMaskEdit*>(Components[i]) != NULL) && (Components[i] != Sender) && (dynamic_cast<TcxImageComboBox*>(Components[i]) == NULL))
      ((TControl*)Components[i])->Enabled = false;
  if (Sender != NULL)
    Sender->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::FormCreate(
      TObject *Sender)
{
  PCustomDrawItem p;
  TTreeNode *Node = tvCustomDrawItems->Items->Item[0];
  for (int i=0; i < 2; i++)
    for (int j=0; j < 6; j++) {
      p = new TCustomDrawItem;
      p->ViewType = (TViewType)i;
      p->CustomDrawArea = (TCustomDrawArea)j;
      if (Node->HasChildren)
        Node = Node->GetNext();
      if (Node != NULL)
        Node->Data = p;
      Node = Node->GetNext();
    }
  tvCustomDrawItems->FullExpand();
  tvCustomDrawItems->Selected =
    tvCustomDrawItems->Items->GetFirstNode()->getFirstChild();
  tvCustomDrawItems->Items->GetFirstNode()->getFirstChild()->Focused = true;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::FormDestroy(
      TObject *Sender)
{
  for (int i=0; i < tvCustomDrawItems->Items->Count; i++)
    if (tvCustomDrawItems->Items->Item[i]->Data != NULL)
      delete tvCustomDrawItems->Items->Item[i]->Data;
  if (FUserDefinedImage == NULL) {
    delete FUserDefinedImage;
    FUserDefinedImage = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::mruBkImagePropertiesChange(
      TObject *Sender)
{
  TBkImage BkImage;
  if(FIsUpdating) return;
  if(tvCustomDrawItems->Selected != NULL) {
    BkImage = bkiUserDefined;
    if(((TcxMRUEdit*)Sender)->Text == "Tile")
      BkImage = bkiTile; else
    if(((TcxMRUEdit*)Sender)->Text == "Sky")
      BkImage = bkiSky; else
    if(((TcxMRUEdit*)Sender)->Text == "Egypt")
      BkImage = bkiEgypt; else
    if(((TcxMRUEdit*)Sender)->Text == "My Face")
      BkImage = bkiMyFace;
    CustomDrawTableViewDemoMainForm->CustomBkImage[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
    [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = BkImage;
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::mruBkImagePropertiesButtonClick(
      TObject *Sender)
{
  if(OpenDialog->Execute()) {
    if(FUserDefinedImage == NULL) {
      delete FUserDefinedImage;
      FUserDefinedImage = NULL;
    };
    FUserDefinedImage = new Graphics::TBitmap();
    try {
      FUserDefinedImage->LoadFromFile(OpenDialog->FileName);
      FIsUpdating = true;
      mruBkImage->Text = "User Defined";
      FIsUpdating = false;
      CustomDrawTableViewDemoMainForm->UserDefindedBitmap[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
      [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = FUserDefinedImage;
      CustomDrawTableViewDemoMainForm->CustomBkImage[((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
      [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea] = bkiUserDefined;
    }
    __finally {
      FUserDefinedImage = NULL;
    }
  };
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::FormShow(
      TObject *Sender)
{
  if ((tvCustomDrawItems->Selected != NULL) && (tvCustomDrawItems->Selected->Data != NULL))
    SetProperties(CustomDrawTableViewDemoMainForm->CustomDrawingStyle[
    ((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->ViewType]
    [((PCustomDrawItem)tvCustomDrawItems->Selected->Data)->CustomDrawArea]);
    tvCustomDrawItemsClick(tvCustomDrawItems->Selected);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::mruBkImageKeyPress(
      TObject *Sender, char &Key)
{
  Key = 7;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::btnCloseClick(
      TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawTableViewDemoStylesEditorForm::icbIndicatorImagesPropertiesEditValueChanged(
      TObject *Sender)
{
  if (tvCustomDrawItems->Selected->Data != NULL)
      CustomDrawTableViewDemoMainForm->IndicatorImageIndex[
      ((PCustomDrawItem)(tvCustomDrawItems->Selected->Data))->ViewType] = ((TcxComboBox*)Sender)->ItemIndex;
}
//---------------------------------------------------------------------------

