//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "AlphaBlendingMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxNavBar"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma link "dxNavBarStyles"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma resource "*.dfm"
TfmAlphaBlendingMain *fmAlphaBlendingMain;
//---------------------------------------------------------------------------
__fastcall TfmAlphaBlendingMain::TfmAlphaBlendingMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmAlphaBlendingMain::lbBgDblClick(TObject *Sender)
{
  ClearBgImage(nbMain->StyleBackground);
  iBg->Picture->Graphic = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::lbBgGroup1DblClick(TObject *Sender)
{
  ClearBgImage(nbMain->Groups->Items[0]->StyleBackground);
  iBgGroup1->Picture->Graphic = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::lbBgGroup2DblClick(TObject *Sender)
{
  ClearBgImage(nbMain->Groups->Items[1]->StyleBackground);
  iBgGroup2->Picture->Graphic = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::lbBgGroup3DblClick(TObject *Sender)
{
  ClearBgImage(nbMain->Groups->Items[2]->StyleBackground);
  iBgGroup3->Picture->Graphic = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::iBgClick(TObject *Sender)
{
  SetBgImage(nbMain->StyleBackground);
  iBg->Picture->Assign(nbMain->StyleBackground->Style->Image);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::iBgGroup1Click(TObject *Sender)
{
  SetBgImage(nbMain->Groups->Items[0]->StyleBackground);
  iBgGroup1->Picture->Assign(nbMain->Groups->Items[0]->StyleBackground->Style->Image);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::iBgGroup2Click(TObject *Sender)
{
  SetBgImage(nbMain->Groups->Items[1]->StyleBackground);
  iBgGroup2->Picture->Assign(nbMain->Groups->Items[1]->StyleBackground->Style->Image);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::iBgGroup3Click(TObject *Sender)
{
  SetBgImage(nbMain->Groups->Items[2]->StyleBackground);
  iBgGroup3->Picture->Assign(nbMain->Groups->Items[2]->StyleBackground->Style->Image);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::tbBgChange(TObject *Sender)
{
  SetBgAlphaBlending(nbMain->StyleBackground, (Byte)tbBg->Position);        
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::tbBgGroup1Change(TObject *Sender)
{
  SetBgAlphaBlending(nbMain->Groups->Items[0]->StyleBackground, (Byte)tbBgGroup1->Position);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::tbBgGroup2Change(TObject *Sender)
{
  SetBgAlphaBlending(nbMain->Groups->Items[1]->StyleBackground, (Byte)tbBgGroup2->Position);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::tbBgGroup3Change(TObject *Sender)
{
  SetBgAlphaBlending(nbMain->Groups->Items[2]->StyleBackground, (Byte)tbBgGroup3->Position);        
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::FormCreate(TObject *Sender)
{
  tbBg->Position = nbMain->StyleBackground->Style->AlphaBlending;
  tbBgGroup1->Position = nbMain->Groups->Items[0]->StyleBackground->Style->AlphaBlending;
  tbBgGroup2->Position = nbMain->Groups->Items[1]->StyleBackground->Style->AlphaBlending;
  tbBgGroup3->Position = nbMain->Groups->Items[2]->StyleBackground->Style->AlphaBlending;

  iBg->Picture->Assign(nbMain->StyleBackground->Style->Image);
  iBgGroup1->Picture->Assign(nbMain->Groups->Items[0]->StyleBackground->Style->Image);
  iBgGroup2->Picture->Assign(nbMain->Groups->Items[1]->StyleBackground->Style->Image);
  iBgGroup3->Picture->Assign(nbMain->Groups->Items[2]->StyleBackground->Style->Image);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainGroupHotTrack(TObject *Sender,
      TdxNavBarGroup *AGroup)
{
  if (AGroup == bgDetails)
    bgDetails->LargeImageIndex = 2;
  else bgDetails->LargeImageIndex = 3;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainCalcGroupClientHeight(
      TObject *Sender, TdxNavBarGroupViewInfo *AViewInfo, int &AHeight)
{
  if (AViewInfo->Group == bgDetails && AViewInfo->Group->Expanded)
    AHeight = 50;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainCustomDrawGroupClientForeground(
      TObject *Sender, TCanvas *ACanvas, TdxNavBarGroupViewInfo *AViewInfo,
      bool &AHandled)
{
  if (AViewInfo->Group == bgDetails && AViewInfo->Group->Expanded)
  {
    ACanvas->Brush->Style = bsClear;
    ACanvas->Font->Color = clBlack;
    ACanvas->Font->Size = 8;
    ACanvas->Font->Style = ACanvas->Font->Style.Clear();
    ACanvas->Font->Style = ACanvas->Font->Style << fsBold;
    ACanvas->TextOut(AViewInfo->ItemsRect.Left + 10, AViewInfo->ItemsRect.Top + 10, "My Documents");
    ACanvas->Font->Style = ACanvas->Font->Style >> fsBold;
    ACanvas->TextOut(AViewInfo->ItemsRect.Left + 10, AViewInfo->ItemsRect.Top + 24, "System Folder");
    AHandled = True;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainGetGroupHint(TObject *Sender,
      TdxNavBarGroup *AGroup, String &AHint)
{
  if (AGroup == bgDetails)
    AHint = "Custom hint for " + AGroup->Caption;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainCalcGroupHintRect(
      TObject *Sender, TdxNavBarGroup *AGroup,
      TdxNavBarViewInfo *AViewInfo, TRect &R)
{
  if (AGroup == bgDetails)
  {
    R.Right = R.Left + 300;
    R.Bottom = R.Top + 30;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmAlphaBlendingMain::nbMainCustomDrawGroupHint(
      TObject *Sender, TCanvas *ACanvas, TdxNavBarGroup *AGroup,
	  String AHint, TRect &R, bool &AHandled)
{
  tagRECT ARect;
  ARect = R;
  if (AGroup == bgDetails)
  {
    ACanvas->Pen->Width = 1;
	ACanvas->Pen->Style = psSolid;
	ACanvas->Pen->Color = clYellow;
	ACanvas->Brush->Style = bsSolid;
	ACanvas->Brush->Color = clTeal;
	ACanvas->Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    ACanvas->Brush->Style = bsClear;
	ACanvas->Font->Color = clYellow;
    ACanvas->Font->Size = 14;
	ACanvas->Font->Style = ACanvas->Font->Style << fsBold;
	DrawText(ACanvas->Handle, dxStringToAnsiString(AHint).c_str(), AHint.Length(), &ARect,
	  DT_CENTER | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS | DT_NOPREFIX);
    AHandled = True;
  }
}
//---------------------------------------------------------------------------

void TfmAlphaBlendingMain::ClearBgImage(TdxNavBarStyleItem *AStyleItem)
{
  AStyleItem->Style->Image->Graphic = NULL;
}
//---------------------------------------------------------------------------
void TfmAlphaBlendingMain::SetBgImage(TdxNavBarStyleItem *AStyleItem)
{
  if (OpenPictureDialog1->Execute()) 
    AStyleItem->Style->Image->LoadFromFile(OpenPictureDialog1->FileName);
}
//---------------------------------------------------------------------------
void TfmAlphaBlendingMain::SetBgAlphaBlending(TdxNavBarStyleItem *AStyleItem, Byte AValue)
{
  AStyleItem->Style->AlphaBlending = AValue;
  AStyleItem->Style->AlphaBlending2 = AValue;
}
//---------------------------------------------------------------------------



