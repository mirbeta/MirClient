//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BackgroundDemoMain.h"
#include "EBarsDemoRating.h"
#include "Math.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxBar"
#pragma link "dxBarExtItems"
#pragma resource "*.dfm"
TBackgroundDemoMainForm *BackgroundDemoMainForm;
const String sCustomImage = "(custom image)";
const String sDefaultImage = "(default image)";
//---------------------------------------------------------------------------
__fastcall TBackgroundDemoMainForm::TBackgroundDemoMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TBackgroundDemoMainForm::FormCreate(TObject *Sender)
{
  FPath = ExtractFilePath(Application->ExeName);

  FBarBackgroudBitmap = new Graphics::TBitmap();
  FBarSubmenuBitmap = new Graphics::TBitmap();
  FSatusbarBackgroudBitmap = new Graphics::TBitmap();
  FPopupmenuBitmap = new Graphics::TBitmap();

  cbBarClick(NULL);
  cbSubMenuClick(NULL);
  cbBackgroundBitmapClick(NULL);
  cbBackgroundBitmapPMClick(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::FormDestroy(TObject *Sender)
{
  delete FBarBackgroudBitmap;
  delete FBarSubmenuBitmap;
  delete FSatusbarBackgroudBitmap;
  delete FPopupmenuBitmap;
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbBarClick(TObject *Sender)
{
  if (cbBar->Checked) {
    if (lbBarBackgroud->Caption == sDefaultImage)
      dxBarManager1->Backgrounds->Bar->LoadFromFile(FPath + "b01.bmp");
    else
      dxBarManager1->Backgrounds->Bar->Assign(FBarBackgroudBitmap);
    lbBarBackgroud->Enabled = true;
    sbBarBackgroud->Enabled = true;
  }
  else {
    dxBarManager1->Backgrounds->Bar->Assign(NULL);
    lbBarBackgroud->Enabled = false;
    sbBarBackgroud->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbSubMenuClick(TObject *Sender)
{
  if (cbSubMenu->Checked) {
    if (lbBarSubmenu->Caption == sDefaultImage)
      dxBarManager1->Backgrounds->SubMenu->LoadFromFile(FPath + "b02.bmp");
    else
      dxBarManager1->Backgrounds->SubMenu->Assign(FBarSubmenuBitmap);
    lbBarSubmenu->Enabled = true;
    sbBarSubmenu->Enabled = true;
  }
  else {
    dxBarManager1->Backgrounds->SubMenu->Assign(NULL);
    lbBarSubmenu->Enabled = false;
    sbBarSubmenu->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbBackgroundBitmapClick(
      TObject *Sender)
{
  if (cbBackgroundBitmap->Checked) {
    if (lbSatusbarBackgroud->Caption == sDefaultImage)
      dxBarManager1->Bars->Items[4]->BackgroundBitmap->LoadFromFile(FPath + "b03.bmp");
    else
      dxBarManager1->Bars->Items[4]->BackgroundBitmap->Assign(FSatusbarBackgroudBitmap);
    lbSatusbarBackgroud->Enabled = true;
    sbSatusbarBackgroud->Enabled = true;
  }
  else {
    dxBarManager1->Bars->Items[4]->BackgroundBitmap->Assign(NULL);
    lbSatusbarBackgroud->Enabled = false;
    sbSatusbarBackgroud->Enabled = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbBackgroundBitmapPMClick(
      TObject *Sender)
{
  if (cbBackgroundBitmapPM->Checked) {
    if (lbPopupmenu->Caption == sDefaultImage)
      dxBarPopupMenu1->BackgroundBitmap->LoadFromFile(FPath + "b04.bmp");
    else
      dxBarPopupMenu1->BackgroundBitmap->Assign(FPopupmenuBitmap);
    lbPopupmenu->Enabled = true;
    sbPopupmenu->Enabled = true;
  }
  else {
    lbPopupmenu->Enabled = false;
    sbPopupmenu->Enabled = false;
    dxBarPopupMenu1->BackgroundBitmap->Assign(NULL);
  }
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::btnRestoreDefaultsClick(
      TObject *Sender)
{
  if((!cbBar->Checked) || (lbBarBackgroud->Caption == sCustomImage)) {
    dxBarManager1->Backgrounds->Bar->LoadFromFile(FPath + "b01.bmp");
    lbBarBackgroud->Caption = sDefaultImage;
    cbBar->Checked = true;
  };

  if((!cbSubMenu->Checked) || (lbBarSubmenu->Caption == sCustomImage)) {
    dxBarManager1->Backgrounds->SubMenu->LoadFromFile(FPath + "b02.bmp");
    lbBarSubmenu->Caption = sDefaultImage;
    cbSubMenu->Checked = true;
  };

  if((!cbBackgroundBitmap->Checked) || (lbSatusbarBackgroud->Caption == sCustomImage)) {
    dxBarManager1->Bars->Items[4]->BackgroundBitmap->LoadFromFile(FPath + "b03.bmp");
    lbSatusbarBackgroud->Caption = sDefaultImage;
    cbBackgroundBitmap->Checked = true;
  };

  if((!cbBackgroundBitmapPM->Checked) || (lbPopupmenu->Caption == sCustomImage)) {
    dxBarPopupMenu1->BackgroundBitmap->LoadFromFile(FPath + "b04.bmp");
    lbPopupmenu->Caption = sDefaultImage;
    cbBackgroundBitmapPM->Checked = true;
  };
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbShowImageCaptionsClick(
      TObject *Sender)
{
  dxBarManager1->BeginUpdate();
  bool AChecked = ((TCheckBox*)Sender)->Checked;
  for (int i=0; i < ComponentCount; i++)
    if (dynamic_cast<TdxBarLargeButton*>(Components[i]))
      ((TdxBarLargeButton*)Components[i])->ShowCaption = AChecked;
  dxBarManager1->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::cbAssignHotImagesClick(
      TObject *Sender)
{
  if (((TCheckBox*)Sender)->Checked)
    dxBarManager1->HotImages = ilHotImages;
  else
    dxBarManager1->HotImages = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::BarManagerStyleClick(
      TObject *Sender)
{
  dxBarManager1->Style = (TdxBarManagerStyle)BarManagerStyle->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::sbBarBackgroudClick(
      TObject *Sender)
{
  if (OpenPictureDialog->Execute())
    switch (((TComponent*)Sender)->Tag) {
      case 0: {
          FBarBackgroudBitmap->LoadFromFile(OpenPictureDialog->FileName);
          dxBarManager1->Backgrounds->Bar->Assign(FBarBackgroudBitmap);
          lbBarBackgroud->Caption = sCustomImage; } break;
      case 1: {
          FBarSubmenuBitmap->LoadFromFile(OpenPictureDialog->FileName);
          dxBarManager1->Backgrounds->SubMenu->Assign(FBarSubmenuBitmap);
          lbBarSubmenu->Caption = sCustomImage;
         } break;
      case 2: {
           FSatusbarBackgroudBitmap->LoadFromFile(OpenPictureDialog->FileName);
           dxBarManager1->Bars->Items[4]->BackgroundBitmap->Assign(FSatusbarBackgroudBitmap);
           lbSatusbarBackgroud->Caption = sCustomImage;
         } break;
      case 3: {
          FPopupmenuBitmap->LoadFromFile(OpenPictureDialog->FileName);
          dxBarPopupMenu1->BackgroundBitmap->Assign(FPopupmenuBitmap);
          lbPopupmenu->Caption = sCustomImage;
         }
    }
}
//---------------------------------------------------------------------------
void __fastcall TBackgroundDemoMainForm::dxBarButton1Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------

void __fastcall TBackgroundDemoMainForm::dxBarSpinEdit1CurChange(
      TObject *Sender)
{
  dxBarProgressItem1->Position = floor(dxBarSpinEdit1->CurValue);
  dxBarSpinEdit1->CurValue = dxBarProgressItem1->Position;
}
//---------------------------------------------------------------------------

