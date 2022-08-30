//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AlertWindowCustomDrawDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxColorComboBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxAlertWindow"
#pragma link "dxBevel"
#pragma link "BaseForm"
#pragma resource "*.dfm"

TfmAlertWindowCustomDraw *fmAlertWindowCustomDraw;
//---------------------------------------------------------------------------
__fastcall TfmAlertWindowCustomDraw::TfmAlertWindowCustomDraw(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::GetRichEditVisibleRange(HWND ARichHandle,
	int *AMinCharIndex, int *AMaxCharIndex)
{
   TRect ARect;
   SendMessage(ARichHandle, EM_GETRECT, 0, Longint(&ARect));
   *AMinCharIndex = SendMessage(ARichHandle, EM_CHARFROMPOS, 0, Longint(&(ARect.Left)));
   *AMaxCharIndex = SendMessage(ARichHandle, EM_CHARFROMPOS, 0, Longint(&(ARect.Right)));
}

//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::CanvasCopyRect(TCanvas *ASourceCanvas,
  TCanvas *ADestCanvas, TRect ASourceRect, TRect ADestRect, int ACopyMode)
{
  ASourceCanvas->CopyMode = ACopyMode;
  ASourceCanvas->CopyRect(ASourceRect, ADestCanvas, ADestRect);
}
//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::DrawTransparentRichEdit(TcxCanvas *ACanvas,
  TRect ARect, TcxRichEdit *ARichEdit, int AMinCharIndex, int AMaxCharIndex)
{
  int AHeight;
  int AStoreWindowLong;
  TRichEdit *ARich;
  TRect AContentRect;

  ARich = (TRichEdit*)(ARichEdit->InnerControl);
  AStoreWindowLong = GetWindowLong(ARich->Handle, GWL_EXSTYLE);
  SetWindowLong(ARich->Handle, GWL_EXSTYLE, AStoreWindowLong | WS_EX_TRANSPARENT);
  AContentRect = cxRectContent(ARect, ARichEdit->ContentParams.Offsets);
  dxDrawRichEdit(ACanvas->Canvas, AContentRect, ARich, AMinCharIndex,
	AMaxCharIndex, False, AHeight);
  SetWindowLong(ARich->Handle, GWL_EXSTYLE, AStoreWindowLong);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::awmCustomDrawDemo1CustomDrawMessageText(TObject *Sender,
		  TdxAlertWindow *AAlertWindow, TcxCanvas *ACanvas, TdxAlertWindowMessageTextViewInfo *AViewInfo,
		  bool &ADone)
{
  int AMinCharIndex, AMaxCharIndex;
  GetRichEditVisibleRange(redtMessageText->InnerControl->Handle, &AMinCharIndex, &AMaxCharIndex);
  DrawTransparentRichEdit(ACanvas, AViewInfo->Bounds, redtMessageText, AMinCharIndex, AMaxCharIndex);
  ADone = True;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::awmCustomDrawDemo1MeasureMessageText(TObject *Sender,
		  TdxAlertWindow *AAlertWindow, int &AWidth, int &AHeight)
{
  AWidth = cxRectWidth(redtMessageText->InnerControl->ClientRect) - 1;
  AHeight = cxRectHeight(redtMessageText->InnerControl->ClientRect);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnAlignLeftClick(TObject *Sender)
{
  redtMessageText->Paragraph->Alignment = taLeftJustify;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnAlignCenterClick(TObject *Sender)
{
  redtMessageText->Paragraph->Alignment = taCenter;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnAlignRightClick(TObject *Sender)
{
  redtMessageText->Paragraph->Alignment = taRightJustify;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnFontClick(TObject *Sender)
{
  fdMessageText->Font->Color = redtMessageText->SelAttributes->Color;
  fdMessageText->Font->Name = redtMessageText->SelAttributes->Name;
  fdMessageText->Font->Size = redtMessageText->SelAttributes->Size;
  fdMessageText->Font->Style = redtMessageText->SelAttributes->Style;
  fdMessageText->Font->Pitch = redtMessageText->SelAttributes->Pitch;
  if (fdMessageText->Execute())
	SetSelAttributes();
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnShowClick(TObject *Sender)
{
  awmCustomDrawDemo1->Show("Andrew Fuller", "", 0);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::fdMessageTextApply(TObject *Sender, HWND Wnd)
{
  SetSelAttributes();
}
//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::GetSelAttributes()
{
  btnBold->Down = redtMessageText->SelAttributes->Style.Contains(fsBold);
  btnItalic->Down = redtMessageText->SelAttributes->Style.Contains(fsItalic);
  btnUnderline->Down = redtMessageText->SelAttributes->Style.Contains(fsUnderline);
  cbbFontName->Text = redtMessageText->SelAttributes->Name;
  cbbFontName->Properties->FontPreview->FontStyle = redtMessageText->SelAttributes->Style;
  cbTextSize->Text = IntToStr(abs(redtMessageText->SelAttributes->Size));
  ccbTextColor->ColorValue = redtMessageText->SelAttributes->Color;
  switch (redtMessageText->Paragraph->Alignment)
  {
	case taLeftJustify: btnAlignLeft->Down = True;
	case taCenter: btnAlignCenter->Down = True;
	case taRightJustify: btnAlignRight->Down = True;
  }
}
//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::SetSelAttributes()
{
  redtMessageText->SelAttributes->Color = fdMessageText->Font->Color;
  redtMessageText->SelAttributes->Name = fdMessageText->Font->Name;
  redtMessageText->SelAttributes->Size = fdMessageText->Font->Size;
  redtMessageText->SelAttributes->Style = fdMessageText->Font->Style;
  redtMessageText->SelAttributes->Pitch = fdMessageText->Font->Pitch;
}
//---------------------------------------------------------------------------


void __fastcall TfmAlertWindowCustomDraw::btnBoldClick(TObject *Sender)
{
  TFontStyles AFontStyle;
  AFontStyle = redtMessageText->SelAttributes->Style;
  if (btnBold->Down)
	AFontStyle << fsBold;
  else
	AFontStyle >> fsBold;
  redtMessageText->SelAttributes->Style = AFontStyle;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnItalicClick(TObject *Sender)
{
  TFontStyles AFontStyle;
  AFontStyle = redtMessageText->SelAttributes->Style;
  if (btnItalic->Down)
	AFontStyle << fsItalic;
  else
	AFontStyle >> fsItalic;
  redtMessageText->SelAttributes->Style = AFontStyle;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::btnUnderlineClick(TObject *Sender)
{
  TFontStyles AFontStyle;
  AFontStyle = redtMessageText->SelAttributes->Style;
  if (btnUnderline->Down)
	AFontStyle << fsUnderline;
  else
	AFontStyle >> fsUnderline;
  redtMessageText->SelAttributes->Style = AFontStyle;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::cbbFontNamePropertiesChange(TObject *Sender)
{
  if (cbbFontName->Focused())
	redtMessageText->SelAttributes->Name = cbbFontName->Text;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::cbbFontNamePropertiesFontPreviewButtonClick(TObject *Sender,
		  TcxFontButtonType ButtonType)
{
  redtMessageText->SelAttributes->Style = cbbFontName->Properties->FontPreview->FontStyle;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::FormShow(TObject *Sender)
{
  redtMessageText->Lines->LoadFromFile(DefaultFileName);
  GetSelAttributes();
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::Open1Click(TObject *Sender)
{
  TOpenDialog *AOpenDialog = new TOpenDialog(this);
  AOpenDialog->Filter = RTFFilter;
  if (AOpenDialog->Execute())
  {
	redtMessageText->Clear();
	redtMessageText->Lines->LoadFromFile(AOpenDialog->FileName);
  }
  delete AOpenDialog;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::Save1Click(TObject *Sender)
{
  AnsiString RTFFilter = "Rich Text Files (*.RTF)|*.RTF";
  TSaveDialog *ASaveDialog = new TSaveDialog(this);
  ASaveDialog->Filter = RTFFilter;
  if (ASaveDialog->Execute())
  {
	ASaveDialog->FileName = ChangeFileExt(ASaveDialog->FileName, '.rtf');
	redtMessageText->Lines->SaveToFile(ASaveDialog->FileName);
  }
  delete ASaveDialog;
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::cbTextSizePropertiesChange(TObject *Sender)
{
  if (cbTextSize->Focused())
	redtMessageText->SelAttributes->Size = StrToInt(cbTextSize->Text);
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::ccbTextColorPropertiesChange(TObject *Sender)
{
  if (ccbTextColor->Focused())
	redtMessageText->SelAttributes->Color = ccbTextColor->ColorValue;
}
//---------------------------------------------------------------------------

void TfmAlertWindowCustomDraw::RedrawAlertWindows()
{
  int I;
  for (I = 0; I <= awmCustomDrawDemo1->Count - 1; I++)
	awmCustomDrawDemo1->Items[I]->Invalidate();
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::awmCustomDrawDemo1BeforeShow(TObject *Sender,
          TdxAlertWindow *AAlertWindow)
{
  AAlertWindow->Top = Top;
  if (Left - Screen->WorkAreaRect.Left >= Screen->WorkAreaRect.Right - (Left + Width))
  {
	AAlertWindow->Left = Left - cxRectWidth(AAlertWindow->BoundsRect);
	AAlertWindow->OptionsAnimate->ShowingAnimationDirection = awmdLeft;
  }
  else
  {
	AAlertWindow->Left = Left + Width;
	AAlertWindow->OptionsAnimate->ShowingAnimationDirection = awmdRight;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::redtMessageTextPropertiesChange(TObject *Sender)

{
  RedrawAlertWindows();
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::redtMessageTextPropertiesSelectionChange(TObject *Sender)

{
  GetSelAttributes();
  RedrawAlertWindows();
}
//---------------------------------------------------------------------------

void __fastcall TfmAlertWindowCustomDraw::FormCreate(TObject *Sender)
{
  RootLookAndFeel()->NativeStyle = True;
}
//---------------------------------------------------------------------------

