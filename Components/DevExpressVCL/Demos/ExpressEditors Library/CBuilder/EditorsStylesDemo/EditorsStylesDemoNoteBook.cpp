//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "EditorsStylesDemoNoteBook.h"
#include "EditorsStylesDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxPropertiesStore"
#pragma link "cxTextEdit"
#pragma link "EditorsStylesDemoBase"
#pragma link "cxCalendar"
#pragma link "cxClasses"
#pragma link "cxColorComboBox"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBColorComboBox"
#pragma link "cxDBData"
#pragma link "cxDBEdit"
#pragma link "cxDBFontNameComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxFilter"
#pragma link "cxFontNameComboBox"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxStyles"
#pragma link "cxImage"
#pragma link "cxNavigator"
#pragma link "cxDBNavigator"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxColorGallery"
#pragma link "dxDBColorGallery"
#pragma link "dxGalleryControl"
#pragma resource "*.dfm"
TEditorsStylesDemoNoteBookFrame *EditorsStylesDemoNoteBookFrame;
//---------------------------------------------------------------------------
__fastcall TEditorsStylesDemoNoteBookFrame::TEditorsStylesDemoNoteBookFrame(TComponent* Owner)
  : TEditorsStylesDemoBaseFrame(Owner)
{
  cbFontColor->Properties->PrepareDelphiColorList(false, false);
  HintStyle = hcstRoundedInfo;
  FDisplayStyle = shtWood;
  FTempDisplayStyle = shtWood;

  FSpiralImageControl = new TSpiralImageControl(this);
  FSpiralImageControl->Parent = pnSpiral;
  FSpiralImageControl->Align = alClient;
  FSpiralImageControl->Bitmap->Width = pnSpiral->Width;
  FSpiralImageControl->Bitmap->Height =  25;
  Graphics::TBitmap* ABitmap = new Graphics::TBitmap();
  try {
    ABitmap->LoadFromFile(StylesIniPath() + "scWood.bmp");
    FSpiralImageControl->Bitmap->Canvas->StretchDraw(FSpiralImageControl->Bitmap->Canvas->ClipRect, ABitmap);
  }
  __finally {
    delete ABitmap;
  }
  cxFontNameComboBox->EditValue = "MS Sans Serif";
}
//---------------------------------------------------------------------------

void __fastcall TSpiralImageControl::Paint(void)
{
  TcxCanvas* cxCanvas = new TcxCanvas(Canvas);
  try {
	cxCanvas->FillRect(ClientRect, FBitmap, False);
  }
  __finally {
    delete(cxCanvas);
  }
}
//---------------------------------------------------------------------------

__fastcall TSpiralImageControl::TSpiralImageControl(Classes::TComponent* AOwner) : TCustomControl(AOwner)
{
  FBitmap = new Graphics::TBitmap();
}
//---------------------------------------------------------------------------

__fastcall TSpiralImageControl::~TSpiralImageControl(void)
{
  delete FBitmap;
}
//---------------------------------------------------------------------------

bool TEditorsStylesDemoNoteBookFrame::ShowControlsAboveDescription()
{
  return true;
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoNoteBookFrame::Name()
{
  return "Notebook";
}
//---------------------------------------------------------------------------

String __fastcall TEditorsStylesDemoNoteBookFrame::BriefName()
{
  return "Notebook";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoNoteBookFrame::StylesIniPath()
{
  return "StylesFrmNoteBook\\";
}
//---------------------------------------------------------------------------

String TEditorsStylesDemoNoteBookFrame::Description()
{
  return "Notebook Notes";
}
//---------------------------------------------------------------------------

void TEditorsStylesDemoNoteBookFrame::ChangeDisplayStyle(TcxStyleSheetType ADisplayStyle)
{
  String sFileName;
  TEditorsStylesDemoBaseFrame::ChangeDisplayStyle(ADisplayStyle);
  switch (FTempDisplayStyle) {
    case shtLightBlue: { sFileName = "scLightBlue.bmp"; break; }
    case shtLightGray: { sFileName = "scLightGray.bmp"; break; }
    case shtWood: { sFileName = "scWood.bmp"; break; }
    case shtRainyDay: { sFileName = "scRainyDay.bmp"; break; }
    case shtBrick: { sFileName = "scBrick.bmp"; break; }
    case shtDeepSea: { sFileName = "scDeepSea.bmp"; break; }
  }
  Graphics::TBitmap* ABitmap = new Graphics::TBitmap();
  try {
    ABitmap->LoadFromFile(StylesIniPath() + sFileName);
    FSpiralImageControl->Bitmap->Canvas->StretchDraw(FSpiralImageControl->Bitmap->Canvas->ClipRect, ABitmap);
    FSpiralImageControl->Paint();
  }
  __finally {
    delete ABitmap;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoNoteBookFrame::tblNoteBookAfterScroll(
      TDataSet *DataSet)
{
  if (DataSet->State == dsInsert) {
    dbmText->Style->Font->Color = TColor(0x00000000);
    dbmText->Style->Font->Name = "MS Sans Serif";
  }
  else {
    dbmText->Style->Font->Color = TColor(EditorsStylesDemoDataDM->tblNoteBookNOTEFONTCOLOR->Value);
    dbmText->Style->Font->Name = EditorsStylesDemoDataDM->tblNoteBookNOTEFONT->Value;
    dbmText->Style->Font->Size = EditorsStylesDemoDataDM->tblNoteBookNOTETEXTSIZE->Value;
    dbmText->Style->Color = TColor(EditorsStylesDemoDataDM->tblNoteBookNOTETEXTBKCOLOR->Value);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoNoteBookFrame::dsNoteBookDataChange(
      TObject *Sender, TField *Field)
{
  if (Field == EditorsStylesDemoDataDM->tblNoteBookNOTEFONT)
    dbmText->Style->Font->Name = EditorsStylesDemoDataDM->tblNoteBookNOTEFONT->Value;
  else if (Field == EditorsStylesDemoDataDM->tblNoteBookNOTEFONTCOLOR)
    dbmText->Style->Font->Color = TColor(EditorsStylesDemoDataDM->tblNoteBookNOTEFONTCOLOR->Value);
  else if (Field == EditorsStylesDemoDataDM->tblNoteBookNOTETEXTSIZE)
    dbmText->Style->Font->Size = EditorsStylesDemoDataDM->tblNoteBookNOTETEXTSIZE->Value;
  else if (Field == EditorsStylesDemoDataDM->tblNoteBookNOTETEXTBKCOLOR)
    dbmText->Style->Color = TColor(EditorsStylesDemoDataDM->tblNoteBookNOTETEXTBKCOLOR->Value);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoNoteBookFrame::cxFontNameComboBoxPropertiesChange(
      TObject *Sender)
{
  memDescrip->Style->Font->Name = cxFontNameComboBox->EditValue;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoNoteBookFrame::cxSpinEditPropertiesChange(
      TObject *Sender)
{
  memDescrip->Style->Font->Size = StrToInt(cxSpinEdit->Text);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsStylesDemoNoteBookFrame::FormShow(TObject *Sender)
{
  EditorsStylesDemoDataDM->tblNoteBook->AfterScroll = tblNoteBookAfterScroll;
  EditorsStylesDemoDataDM->dsNoteBook->OnDataChange = dsNoteBookDataChange;
  tblNoteBookAfterScroll(EditorsStylesDemoDataDM->tblNoteBook);
}
//---------------------------------------------------------------------------

