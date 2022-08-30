//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "EBar2000DemoMain.h"
#include "EBarsDemoRating.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxBar"
#pragma link "dxBarExtItems"
#pragma resource "*.dfm"
TEBar2000DemoMainForm *EBar2000DemoMainForm;
bool FUpdateVisible;
//---------------------------------------------------------------------------
__fastcall TEBar2000DemoMainForm::TEBar2000DemoMainForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbStyleMeasureItem(TdxBarCustomCombo *Sender,
      int AIndex, int &AHeight)
{
  AHeight = 35;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbStyleDrawItem(TdxBarCustomCombo *Sender,
      int AIndex, TRect &ARect, TOwnerDrawState AState)
{
  tagRECT R = (tagRECT)ARect;

  if (Sender->DroppedDown)
    {
      StyleImages->Draw(Sender->Canvas, R.left, R.top, AIndex
      #if __BORLANDC__  > 0x530
      , true
      #endif
      );
      Sender->Canvas->FrameRect(R);
      InflateRect(&R, -1, -1);
      Sender->Canvas->FrameRect(R);
    }
  else
    Sender->Canvas->TextRect(R, R.left + 1, R.top, Sender->Text);
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::FormCreate(TObject *Sender)
{
  cbAddress->Items->Add(dxStartURL);
  cbAddress->Items->Add(dxSupportURL);

  TrackBarChange(tbStandard);
  TrackBarChange(tbFormatting);
  btnFontColor->Tag = clBlue;
  UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
  btnHighlight->Tag = clYellow;
  UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
  FUpdateVisible = true;
  rgStyleClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::UpdateColorItems(TdxBarLargeButton *AButton, TdxBarLargeButton *ADownButton,
 TdxBarLargeButton *ACustomButton, TdxBarLargeButton *AItemButton, TdxBarPopupMenu *APopupMenu)
{
const TColor Colors[] = {clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray, clSilver,
    clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite};
const AnsiString ColorNames[] = {"Black", "Maroon", "Green", "Olive", "Navy", "Purple", "Teal", "Gray", "Silver",
    "Red", "Lime", "Yellow", "Blue", "Fuchsia", "Aqua", "White"};
  int I;
  Graphics::TBitmap *B;
  TdxBarItemLink *ItemLink;

  B = new Graphics::TBitmap;
  B->Width = Images->Width + 1;
  B->Height = Images->Height;
  APopupMenu->ItemLinks->Clear();
  for( I = 0; I <= 15; I++)
    {
      if (Colors[I] != clBlack) B->Canvas->Brush->Color = clBlack;
      else B->Canvas->Brush->Color = clWhite;
      B->Canvas->FillRect(Rect(0, 0, B->Width, B->Height));
      B->Canvas->Brush->Color = Colors[I];
      B->Canvas->Pen->Color = clGray;
      if (AButton->Tag == Colors[I]) B->Canvas->Rectangle(1, 1, B->Width - 3, B->Height - 2);
      else B->Canvas->Rectangle(2, 2, B->Width - 2, B->Height - 1);

      ItemLink = APopupMenu->ItemLinks->Add();
      if (AButton->Tag == Colors[I])
        {
          ItemLink->Item = ADownButton;
          ADownButton->Down = true;
        }
      else ItemLink->Item = AItemButton;
      ItemLink->Data = Colors[I];
      ItemLink->UserCaption = ColorNames[I];
      ItemLink->UserGlyph->Assign(B);
    };

  ItemLink = APopupMenu->ItemLinks->Add();
  ItemLink->Item = ACustomButton;
  ItemLink->UserCaption = "Custom ...";
  ItemLink->BeginGroup = true;

  delete B;
};
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::btnFontColorItemClick(TObject *Sender)
{
  btnFontColor->Tag = ((TdxBarButton*)Sender)->ClickItemLink->Data;
  UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::btnCustomFontColorClick(TObject *Sender)
{
  ColorDialog->Color = (TColor)(btnFontColor->Tag);
  if (ColorDialog->Execute())
    {
      btnFontColor->Tag = ColorDialog->Color;
      UpdateColorItems(btnFontColor, btnDownFontColorItem, btnCustomFontColor, btnFontColorItem, FontColorPopupMenu);
    };
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::btnColorItemClick(TObject *Sender)
{
  btnHighlight->Tag = ((TdxBarButton*)Sender)->ClickItemLink->Data;
  UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::btnCustomColorClick(TObject *Sender)
{
  ColorDialog->Color = (TColor)(btnHighlight->Tag);
  if (ColorDialog->Execute())
    {
      btnHighlight->Tag = ColorDialog->Color;
      UpdateColorItems(btnHighlight, btnDownColorItem, btnCustomColor, btnColorItem, HightlightColorPopupMenu);
    };
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::btnStandardClick(TObject *Sender)
{
  FUpdateVisible = false;
  dxBarManager->Bars->Items[((TdxBarButton*)Sender)->Tag]->Visible = ((TdxBarButton*)Sender)->Down;
  FUpdateVisible = true;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::sbFontClick(TObject *Sender)
{
  FontDialog->Font->Assign(dxBarManager->Font);
  if (FontDialog->Execute()) dxBarManager->Font->Assign(FontDialog->Font);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbCanCustomizeClick(TObject *Sender)
{
  dxBarManager->CanCustomize = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::sbDockColorClick(TObject *Sender)
{
  if (ColorDialog->Execute())
      dxBarManager->DockColor = ColorDialog->Color;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowResetClick(TObject *Sender)
{
  dxBarManager->AllowReset = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbShowHelpButtonClick(TObject *Sender)
{
  dxBarManager->ShowHelpButton = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbSunkenBorderClick(TObject *Sender)
{
  dxBarManager->SunkenBorder = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbMenusShowRecentItemsFirstClick(
      TObject *Sender)
{
  dxBarManager->MenusShowRecentItemsFirst = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::rgMenuAnimationsClick(TObject *Sender)
{
  dxBarManager->MenuAnimations = (TdxBarMenuAnimations)(rgMenuAnimations->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::SpeedButton1Click(TObject *Sender)
{
  FontDialog->Font->Assign(dxBarManager->Bars->Items[1]->Font);
  if (FontDialog->Execute())
    dxBarManager->Bars->Items[1]->Font->Assign(FontDialog->Font);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::SpeedButton2Click(TObject *Sender)
{
  FontDialog->Font->Assign(dxBarManager->Bars->Items[2]->Font);
  if (FontDialog->Execute())
    dxBarManager->Bars->Items[2]->Font->Assign(FontDialog->Font);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowCustomizingBar1Click(TObject *Sender)
{
  dxBarManager->Bars->Items[1]->AllowCustomizing = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowQuickCustomizingBar1Click(
      TObject *Sender)
{
  dxBarManager->Bars->Items[1]->AllowQuickCustomizing = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowResetBar1Click(TObject *Sender)
{
  dxBarManager->Bars->Items[1]->AllowReset = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowCustomizingBar2Click(TObject *Sender)
{
  dxBarManager->Bars->Items[2]->AllowCustomizing = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowQuickCustomizingBar2Click(
      TObject *Sender)
{
  dxBarManager->Bars->Items[2]->AllowQuickCustomizing = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbAllowResetBar2Click(TObject *Sender)
{
  dxBarManager->Bars->Items[2]->AllowReset = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbShowFullMenusAfterDelayClick(TObject *Sender)
{
  dxBarManager->ShowFullMenusAfterDelay = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::dxBarManagerHelpButtonClick(TObject *Sender)
{
  MessageDlg("You click on Help Button .", mtInformation, TMsgDlgButtons() << mbOK,0);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbRotateWhenVerticalClick(TObject *Sender)
{
  dxBarManager->Bars->Items[3]->RotateWhenVertical = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::btnCustomizeClick(TObject *Sender)
{
  dxBarManager->Customizing(true);
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::dxBarManagerBarVisibleChange(
      TdxBarManager *Sender, TdxBar *ABar)
{
  if ( FUpdateVisible && HandleAllocated())
    switch (ABar->Index) {
      case 1: btnStandard->Down = ABar->Visible;
      case 2: btnFormatting->Down = ABar->Visible;
      case 3: btnInternet->Down = ABar->Visible;
    }
}
//---------------------------------------------------------------------------
void __fastcall TEBar2000DemoMainForm::cbStretchGlyphsClick(TObject *Sender)
{
  dxBarManager->StretchGlyphs = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::CheckBox2Click(TObject *Sender)
{
  dxBarManager->Bars->Items[2]->MultiLine = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::rgStyleClick(TObject *Sender)
{
  dxBarManager->Style = (TdxBarManagerStyle)rgStyle->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::CheckBox1Click(TObject *Sender)
{
  dxBarManager->Bars->Items[1]->MultiLine = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::TrackBarChange(TObject *Sender)
{
  dxBarManager->Bars->Items[((TTrackBar*)Sender)->Tag]->AlphaBlendValue = (byte)(((TTrackBar*)Sender)->Position * 15);
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::cbHotImagesClick(TObject *Sender)
{
  if (((TCheckBox*)Sender)->Checked)
    dxBarManager->HotImages = ilHotImages;
  else
    dxBarManager->HotImages = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::cbDisabledImagesClick(TObject *Sender)
{
  if (((TCheckBox*)Sender)->Checked) {
    dxBarManager->DisabledImages = ilDisabledImages;
    dxBarManager->DisabledLargeImages = ilDisabledImages;
  }
  else {
    dxBarManager->DisabledLargeImages = NULL;
    dxBarManager->DisabledImages = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::CheckBox4Click(TObject *Sender)
{
  if (((TCheckBox*)Sender)->Checked)
    dxBarManager->LargeImages = Images;
  else
    dxBarManager->LargeImages = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TEBar2000DemoMainForm::cbShowCaptionsClick(TObject *Sender)
{
 dxBarManager->BeginUpdate();
  try {
  bool AChecked = ((TCheckBox*)Sender)->Checked;
  for (int i=0; i < ComponentCount - 1; i++)
    if (dynamic_cast<TdxBarLargeButton*>(Components[i]))
      ((TdxBarLargeButton*)Components[i])->ShowCaption = AChecked;
  }
  __finally {
    dxBarManager->EndUpdate();
  }
}
//---------------------------------------------------------------------------
