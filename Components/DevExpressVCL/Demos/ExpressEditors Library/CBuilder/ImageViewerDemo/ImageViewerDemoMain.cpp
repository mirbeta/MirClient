//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ImageViewerDemoMain.h"
#include "AboutDemoForm.h"
#include "ImageViewerDemoResizeImage.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxPC"
#pragma link "cxSpinEdit"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "cxTrackBar"
#pragma link "dxBevel"
#pragma link "dxCheckGroupBox"
#pragma link "dxColorEdit"
#pragma link "dxDockControl"
#pragma link "dxDockPanel"
#pragma link "dxGalleryControl"
#pragma link "dxGDIPlusClasses"
#pragma link "dxZoomTrackBar"
#pragma link "cxImage"
#pragma link "dxToggleSwitch"
#pragma resource "*.dfm"
TImageViewerDemoMainForm *ImageViewerDemoMainForm;

//---------------------------------------------------------------------------

__fastcall TImageViewerDemoMainForm::TImageViewerDemoMainForm(TComponent* Owner) : TForm(Owner)
{
  FGlyph = new TcxBitmap();
}
//---------------------------------------------------------------------------

__fastcall TImageViewerDemoMainForm::~TImageViewerDemoMainForm()
{
  delete FGlyph;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::FormCreate(TObject *Sender)
{
  cdsFilms->LoadFromFile("..\\..\\Data\\Films.xml");
  cdsFilms->Open();
  cdsGenres->LoadFromFile("..\\..\\Data\\Genres.xml");
  cdsGenres->Open();
  cdsFilmsGenres->LoadFromFile("..\\..\\Data\\FilmsGenres.xml");
  cdsFilmsGenres->Open();

  miBottomSide->Click();
  miAutoColumnCount->Click();
  tbItemSize->Position = 3;

  PopulateGallery(cbSorted->Checked);
  SetDefaultItem();
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::GetGenresIDs(Integer &AActionID, Integer &AComedyID, Integer &ADramaID)
{
  cdsGenres->First();
  while (! cdsGenres->Eof) {
	if (ActionGenre == cdsGenres->FieldByName("Name")->AsString)
	  AActionID = cdsGenres->FieldByName("ID")->AsInteger;
	if (ComedyGenre == cdsGenres->FieldByName("Name")->AsString)
	  AComedyID = cdsGenres->FieldByName("ID")->AsInteger;
	if (DramaGenre == cdsGenres->FieldByName("Name")->AsString)
	  ADramaID = cdsGenres->FieldByName("ID")->AsInteger;
	cdsGenres->Next();
  }
}
//---------------------------------------------------------------------------

int TImageViewerDemoMainForm::FilmGenreGroup(Integer AFilmID, Integer AActionID, Integer AComedyID, Integer ADramaID)
{
  Integer Result = 3;
  Integer AGenreID;
  cdsFilmsGenres->FindFirst();
  do {
	if (AFilmID == cdsFilmsGenres->FieldByName("FilmID")->AsInteger) {
	  AGenreID = cdsFilmsGenres->FieldByName("GenreID")->AsInteger;
	  if (AGenreID == AActionID)
		Result = 0;
	  if (AGenreID == AComedyID)
		Result = 1;
	  if (AGenreID == ADramaID)
		Result = 2;
	}
  }
  while (cdsFilmsGenres->FindNext() && Result == 3);
  return Result;
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::PopulateGallery(bool ASorted)
{
  TMemoryStream *AStream;
  TdxSmartImage *AImage;
  Integer AGroupIndex, AFilmID, AActionID, AComedyID, ADramaID;

  AGroupIndex = 0;
  //dxGalleryControl->BeginUpdate();
  try {
	dxGalleryControl->Gallery->Groups->Clear();
	dxGalleryControl->Gallery->Groups->Add();
	if (ASorted) {
	  dxGalleryControl->Gallery->Groups->Add();
	  dxGalleryControl->Gallery->Groups->Add();
	  dxGalleryControl->Gallery->Groups->Add();
	  dxGalleryControl->Gallery->Groups->Groups[0]->Caption = ActionGenre;
	  dxGalleryControl->Gallery->Groups->Groups[1]->Caption = ComedyGenre;
	  dxGalleryControl->Gallery->Groups->Groups[2]->Caption = DramaGenre;
	  dxGalleryControl->Gallery->Groups->Groups[3]->Caption = OtherGenres;
	}

	GetGenresIDs(AActionID, AComedyID, ADramaID);

	cdsFilms->First();

	AStream = new TMemoryStream();
	try {
	  while (! cdsFilms->Eof) {
		if (! cdsFilms->Fields->FieldByName("Photo")->IsNull) {
		  AImage = new TdxSmartImage();
		  try {
			AStream->Position = 0;
			TField *AField = cdsFilms->Fields->FieldByName("Photo");
			TBlobField *ABlobField = (TBlobField *)AField;
			ABlobField->SaveToStream(AStream);
			AStream->Position = 0;
			AImage->LoadFromStream(AStream);
			AFilmID = cdsFilms->Fields->FieldByName("ID")->AsInteger;
			if (ASorted)
			  AGroupIndex = FilmGenreGroup(AFilmID, AActionID, AComedyID, ADramaID);
			TdxGalleryControlItem *AItem = dxGalleryControl->Gallery->Groups->Groups[AGroupIndex]->Items->Add();
			AItem->Glyph->Assign(AImage);
			AItem->Caption = cdsFilms->Fields->FieldByName("Caption")->AsString;
			AItem->Tag = AFilmID;
		  }
		  __finally {
			delete AImage;
		  }

		}
		cdsFilms->Next();
	  }
	}
	__finally {
	  delete AStream;
	}
  }
  __finally {
	//dxGalleryControl->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::ResizeGlyph(Single AScale, Integer AWidth, Integer AHeight, Boolean IsInPixels)
{
  TRect ARect = cxNullRect;
  if (IsInPixels) {
	ARect.Right = AWidth;
	ARect.Bottom = AHeight;
  }
  else {
	ARect.Right = div(FGlyph->Width * AScale, 100).quot;
	ARect.Bottom = div(FGlyph->Height * AScale, 100).quot;
  }

  TcxBitmap *ABitmap = new TcxBitmap;
  try {
	ABitmap->Assign(FGlyph);
	FGlyph->SetSize(ARect);
	FGlyph->Canvas->StretchDraw(ARect, ABitmap);
  }
  __finally {
	delete ABitmap;
  }
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::SetDefaultItem()
{
  if (dxGalleryControl->Gallery->Groups->Groups[0] && dxGalleryControl->Gallery->Groups->Groups[0]->Items->Items[0])
	  dxGalleryControl->Gallery->ClickItem(dxGalleryControl->Gallery->Groups->Groups[0]->Items->Items[0]);
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::Draw()
{
  imgMain->Picture->Assign(FGlyph);
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::SetTextPosition(TcxPosition APosition)
{
  FStoredPosition = APosition;
  dxGalleryControl->ItemTextPosition = APosition;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::cbSortedClick(TObject *Sender)
{
  PopulateGallery(cbSorted->Checked);
  SetDefaultItem();
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::dxGalleryControlItemClick(TObject *Sender, TdxGalleryControlItem *AItem)
{
  FCurrentItem = AItem;
  FGlyph->Assign(AItem->Glyph);
  Draw();
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miInvisibleClick(TObject *Sender)
{
  SetTextPosition(posNone);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miBottomSideClick(TObject *Sender)
{
  SetTextPosition(posBottom);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miTopSideClick(TObject *Sender)
{
  SetTextPosition(posTop);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miLeftSideClick(TObject *Sender)
{
  SetTextPosition(posLeft);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miRightSideClick(TObject *Sender)
{
  SetTextPosition(posRight);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::sbTextSettingsClick(TObject *Sender)
{
  TPoint P = sbTextSettings->ClientToScreen((Point)(0, sbTextSettings->Height));
  pmTextSettings->Popup(P.x, P.y);
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miAutoColumnCountClick(TObject *Sender)
{
  dxGalleryControl->ColumnCount = 0;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miThreeColumnsClick(TObject *Sender)
{
  dxGalleryControl->ColumnCount = 3;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miFourColumnsClick(TObject *Sender)
{
  dxGalleryControl->ColumnCount = 4;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::miFiveColumnsClick(TObject *Sender)
{
  dxGalleryControl->ColumnCount = 5;
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::sbColumnSettingsClick(TObject *Sender)
{
  TPoint P = sbColumnSettings->ClientToScreen((Point)(0, sbTextSettings->Height));
  pmColumnSettings->Popup(P.x, P.y);
}
//---------------------------------------------------------------------------

void TImageViewerDemoMainForm::SetupTextMenu(TcxPosition APosition)
{
  switch (APosition)
  {
	case posTop: miTopSide->Checked = True; break;
	case posBottom: miBottomSide->Checked = True; break;
	case posLeft: miLeftSide->Checked = True; break;
	case posRight: miRightSide->Checked = True; break;
	case posNone: miInvisible->Checked = True; break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::tbItemSizePropertiesChange(TObject *Sender)
{
  if (tbItemSize->Position == tbItemSize->Properties->Min){
	if (FStoredPosition != posNone) {
	  dxGalleryControl->ItemTextPosition = posRight;
	  SetupTextMenu(posRight);
	}
	dxGalleryControl->ItemImageSize->Size = cxSize(32, 32);
  }
  else {
	dxGalleryControl->ItemTextPosition = FStoredPosition;
	SetupTextMenu(FStoredPosition);
	if (tbItemSize->Position == tbItemSize->Properties->Max) {
	  dxGalleryControl->ItemImageSize->Size = cxSize(0, 0);
	}
	else {
	  int AFactor = tbItemSize->Position;
	  dxGalleryControl->ItemImageSize->Size = cxSize(40 * AFactor, 40 * AFactor);
	}
  }
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::sbInfoClick(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::sbResizeClick(TObject *Sender)
{
  ImageViewerDemoResizeImageForm->GlyphHeight = FGlyph->Height;
  ImageViewerDemoResizeImageForm->GlyphWidth = FGlyph->Width;
  if (ImageViewerDemoResizeImageForm->ShowModal() == mrOk) {
	ResizeGlyph(ImageViewerDemoResizeImageForm->seScale->Value, StrToInt(ImageViewerDemoResizeImageForm->teWidth->Text),
	  StrToInt(ImageViewerDemoResizeImageForm->teHeight->Text), ImageViewerDemoResizeImageForm->cgbPixels->CheckBox->Checked);
	FCurrentItem->Glyph->Assign(FGlyph);
  }
}
//---------------------------------------------------------------------------

void __fastcall TImageViewerDemoMainForm::tsHidePanelPropertiesChange(TObject *Sender)
{
  gbRightPanel->Visible = tsHidePanel->Checked;
  cxSplitter1->Enabled = tsHidePanel->Checked;
  if (tsHidePanel->Checked)
	cxSplitter1->Left = gbRightPanel->Left - 10;
}
//---------------------------------------------------------------------------

