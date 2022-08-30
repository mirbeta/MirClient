//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoRichEdit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxButtons"
#pragma link "cxColorComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxFontNameComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinButton"
#pragma link "cxSpinEdit"
#pragma link "cxRichEdit"
#pragma resource "*.dfm"
TEditorsStylesDemoRichEditFrame *EditorsStylesDemoRichEditFrame;
String FFileName = "Untitled";
String ButtonString  = "Color Palette: ";
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoRichEditFrame::TEditorsStylesDemoRichEditFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  OpenDialog->InitialDir = ExtractFilePath(ParamStr(0));
  SaveDialog->InitialDir = OpenDialog->InitialDir;
  SetFileName("lipsum.rtf");
  RichEdit->Lines->LoadFromFile("lipsum.rtf");
  CurrText()->Name = "MS Sans Serif";
  RichEditSelectionChange(this);
  ColorSchemeButtonClick(miStandard);

  HintStyle = hcstLightInfo;
  FDisplayStyle = shtLightBlue;
  FTempDisplayStyle = shtLightBlue;
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoRichEditFrame::Name()
{
  return "Text Processing";
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoRichEditFrame::BriefName()
{
  return "Text";
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoRichEditFrame::MenuOpenFileVisible()
{
  return true;
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoRichEditFrame::MenuSaveFileVisible()
{
  return true;
}
//---------------------------------------------------------------------------
void TEditorsStylesDemoRichEditFrame::OpenFile(TObject* Sender)
{
  actOpenExecute(NULL);
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoRichEditFrame::SaveFile(TObject* Sender)
{
  actSaveFileExecute(NULL);
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoRichEditFrame::StylesIniPath()
{
  return "StylesFrmRichEdit\\";
}
//---------------------------------------------------------------------------

TColor TEditorsStylesDemoRichEditFrame::GetStyleBackgroundColor()
{
 return (RichEdit->Style->Color);
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoRichEditFrame::Description()
{
  return "Text Processing Notes";
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditCutExecute(
      TObject *Sender)
{
  RichEdit->CutToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditCopyExecute(
      TObject *Sender)
{
  RichEdit->CopyToClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditPasteExecute(
      TObject *Sender)
{
  RichEdit->PasteFromClipboard();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actOpenExecute(TObject *Sender)
{
  if (OpenDialog->Execute()) {
    FFileName = OpenDialog->FileName;
    RichEdit->Lines->LoadFromFile(FFileName);
    RichEdit->Properties->ReadOnly =  OpenDialog->Options.Contains(ofReadOnly);
    DoOnFileNameChanged();
    FChanged = False;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actPrintExecute(
      TObject *Sender)
{
  if (PrintDialog->Execute())
    RichEdit->Print(FFileName);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actSaveFileExecute(
      TObject *Sender)
{
  if (FFileName == "Untitled") {
    if (SaveDialog->Execute()) {
	  if (FileExists(SaveDialog->FileName)) {
        TMsgDlgButtons buttons = mbYesNoCancel;
		AnsiString S = "Overwrite?";
		if (MessageDlg(Format(S, ARRAYOFCONST((SaveDialog->FileName))),
          mtConfirmation, buttons, 0) !=  mrYes) return;
      }
      RichEdit->Lines->SaveToFile(SaveDialog->FileName);
      SetFileName(SaveDialog->FileName);
      FChanged = False;
    }
  }
  else {
    RichEdit->Lines->SaveToFile(FFileName);
    FChanged = False;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actNewFileExecute(
      TObject *Sender)
{
  SetFileName("Untitled");
  RichEdit->Lines->Clear();
  FChanged = False;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditCutUpdate(
      TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = (RichEdit) && (RichEdit->SelLength > 0);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditCopyUpdate(
      TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = actEditCut->Enabled;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actEditPasteUpdate(
      TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = RichEdit->HandleAllocated() &&
	(RichEdit->Perform(EM_CANPASTE, 0, 0) != 0);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::RichEditSelectionChange(
      TObject *Sender)
{
  try {
    FUpdating = true;
    BoldButton->Down = CurrText()->Style.Contains(fsBold);
    ItalicButton->Down = CurrText()->Style.Contains(fsItalic);
    UnderlineButton->Down = CurrText()->Style.Contains(fsUnderline);
    BulletsButton->Down = (bool)RichEdit->Paragraph->Numbering;
    meFontSize->Text = IntToStr(CurrText()->Size);
    fcbFontName->EditValue = CurrText()->Name;
    cxColorComboBox->EditValue = CurrText()->Color;
    switch ((int)RichEdit->Paragraph->Alignment) {
      case 0: {LeftAlign->Down = true; break;}
      case 1: {RightAlign->Down = true; break;}
      case 2: {CenterAlign->Down = true; break;}
   }
  }
  __finally {
    FUpdating = false;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::SetFileName(String AFileName)
{
  FFileName = AFileName;
  DoOnFileNameChanged();
  AnsiString S = "%s - %s";
  Caption = Format(S, ARRAYOFCONST((ExtractFileName(AFileName), Application->Title)));
}
//---------------------------------------------------------------------------

TTextAttributes* __fastcall TEditorsStylesDemoRichEditFrame::CurrText()
{
  return RichEdit->SelAttributes;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::ColorSchemeButtonClick(
      TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked) {
    int cl = VarAsType(cxColorComboBox->EditValue, varInteger);
    TColor AColor = TColor(cl);
    ((TMenuItem*)Sender)->Checked = true;
    switch (((TComponent*)Sender)->Tag) {
      case 0: {
           cxColorComboBox->Properties->PrepareDelphiColorList(false, false);
           cxColorComboBox->Properties->NamingConvention = cxncDelphi;
           btnColorSchemes->Caption = ButtonString + "Delphi Colors";
           break; }
      case 1: {
           cxColorComboBox->Properties->PrepareHTML4ColorList(false, false);
           cxColorComboBox->Properties->NamingConvention = cxncHTML4;
           btnColorSchemes->Caption = ButtonString + "16 Standard Colors";
           break; }
      case 2: {
           cxColorComboBox->Properties->PrepareX11ColorList(false, false);
           cxColorComboBox->Properties->NamingConvention = cxncX11;
           btnColorSchemes->Caption = ButtonString + "Web Colors";
           break; }
      case 3: {
           cxColorComboBox->Properties->PrepareX11OrderedColorList(false, false);
           cxColorComboBox->Properties->NamingConvention = cxncX11;
           btnColorSchemes->Caption = ButtonString + "Web Colors By Hue";
           break; }
    }
    TcxCustomColorComboBoxPropertiesAccess* AProperties = (TcxCustomColorComboBoxPropertiesAccess*)cxColorComboBox->Properties;
    int AIndex = AProperties->IndexByValue(cxColorComboBox->EditValue);
    if (AIndex != -1) {
      String S = AProperties->GetDescriptionByIndex(AIndex);
      AProperties->Items->Items[AIndex]->Description = S;
    }
    cxColorComboBox->EditValue = AColor;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::meFontSizePropertiesChange(
      TObject *Sender)
{
  if (FUpdating) return;
  if (meFontSize->EditText != "")
    CurrText()->Size = StrToInt(meFontSize->EditText);
  else
    CurrText()->Size = 0;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::fcbFontNamePropertiesChange(
      TObject *Sender)
{
  if (FUpdating) return;
  CurrText()->Name = ((TcxFontNameComboBox*)Sender)->Text;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::cxColorComboBoxPropertiesChange(
      TObject *Sender)
{
  if (FUpdating) return;
  if (cxColorComboBox->ItemIndex != -1) {
    int cl = VarAsType(cxColorComboBox->EditValue, varInteger);
    CurrText()->Color = TColor(cl);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actBoldExecute(TObject *Sender)
{
  if (FUpdating) return;
  if (BoldButton->Down)
    CurrText()->Style = CurrText()->Style + (TFontStyles() << fsBold);
  else
    CurrText()->Style = CurrText()->Style - (TFontStyles() << fsBold);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actItalicExecute(
      TObject *Sender)
{
  if (FUpdating) return;
  if (ItalicButton->Down)
    CurrText()->Style = CurrText()->Style + (TFontStyles() << fsItalic);
  else
    CurrText()->Style = CurrText()->Style - (TFontStyles() << fsItalic);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actUnderlineExecute(
      TObject *Sender)
{
  if (FUpdating) return;
  if (UnderlineButton->Down)
    CurrText()->Style = CurrText()->Style + (TFontStyles() << fsUnderline);
  else
    CurrText()->Style = CurrText()->Style - (TFontStyles() << fsUnderline);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actAlignLeftExecute(
      TObject *Sender)
{
  if (FUpdating || (RichEdit == NULL)) return;
  RichEdit->Paragraph->Alignment = (TAlignment)((TControl*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actAlignRightExecute(
      TObject *Sender)
{
  if (FUpdating || (RichEdit == NULL)) return;
  RichEdit->Paragraph->Alignment = (TAlignment)((TControl*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actAlignCenterExecute(
      TObject *Sender)
{
  if (FUpdating || (RichEdit == NULL)) return;
  RichEdit->Paragraph->Alignment = (TAlignment)((TControl*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actBulletsExecute(
      TObject *Sender)
{
  if (FUpdating || (RichEdit == NULL)) return;
  RichEdit->Paragraph->Numbering = (TNumberingStyle)BulletsButton->Down;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::fcbFontNamePropertiesFontPreviewButtonClick(
      TObject *Sender, TcxFontButtonType ButtonType)
{
  CurrText()->Style = fcbFontName->Properties->FontPreview->FontStyle;
  RichEditSelectionChange(NULL);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::fcbFontNamePropertiesInitPopup(
      TObject *Sender)
{
  fcbFontName->Properties->FontPreview->FontStyle = CurrText()->Style;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::actSaveFileUpdate(
      TObject *Sender)
{
  ((TCustomAction*)Sender)->Enabled = FChanged;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::FormShow(TObject *Sender)
{
  FChanged = False;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoRichEditFrame::RichEditPropertiesChange(
      TObject *Sender)
{
  FChanged = True;
}
//---------------------------------------------------------------------------

