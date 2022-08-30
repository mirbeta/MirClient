//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "CustomDrawCardViewDemoMain.h"
#include "FilmsDemoData.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TCustomDrawCardViewDemoMainForm *CustomDrawCardViewDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TCustomDrawCardViewDemoMainForm::TCustomDrawCardViewDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

String __fastcall TCustomDrawCardViewDemoMainForm::TextToDraw(TcxGridTableDataCellViewInfo *AViewInfo)
{
  if (dynamic_cast<TcxGridCardRowDataViewInfo*>(AViewInfo) != NULL) {
    if (AViewInfo->Item->Index == cvPersonsGENDER->Index) {
      if ((int)VarAsType(AViewInfo->GridRecord->Values[cvPersonsGENDER->Index], varBoolean))
        return("Male");
      else
        return("Female");
    }
    else
      return(AViewInfo->GridRecord->DisplayTexts[AViewInfo->Item->Index]);
  }
  else
    return(VarAsType(AViewInfo->Item->Caption, varString));
};
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::cvPersonsCustomDrawCell(
      TcxCustomGridTableView *Sender, TcxCanvas *ACanvas,
      TcxGridTableDataCellViewInfo *AViewInfo, bool &ADone)
{
  TRect ARec = AViewInfo->Bounds;
  ACanvas->Canvas->Font->Assign(FFont);

  String ATextToDraw = TextToDraw(AViewInfo);
  switch ((int)FCustomDrawingStyle) {
	case cdsBkImage: {
      ACanvas->FillRect(ARec, FCurrentBitmap);
    } break;
    case cdsGradient: {
      bool AIsVertical = ((FColorScheme == csGrey) || (FColorScheme == csGold));
      if (dynamic_cast<TcxGridCardRowDataViewInfo*>(AViewInfo) != NULL)
        DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme][1], ColorScheme[(int)FColorScheme][0], 40, AIsVertical);
      else
        DrawGradient(ACanvas->Canvas, ARec, ColorScheme[(int)FColorScheme][0], ColorScheme[(int)FColorScheme][1], 40, AIsVertical);
    } break;
    case cdsDependOnData:
    {
      ACanvas->Canvas->Brush->Style = bsSolid;
      ACanvas->Canvas->Brush->Color = clBlueLight;
      ACanvas->Canvas->FillRect(ARec);
      if ((int)VarAsType(AViewInfo->GridRecord->Values[cvPersonsGENDER->Index], varBoolean))
        ACanvas->Canvas->Font->Color = clBlue;
      else
        ACanvas->Canvas->Font->Color = clFuchsia;
      if ((dynamic_cast<TcxGridCardRowDataViewInfo*>(AViewInfo) != NULL) && (AViewInfo->Item->Index == cvPersonsGENDER->Index)) {
        bool ABool = (int)VarAsType(AViewInfo->GridRecord->Values[cvPersonsGENDER->Index], varBoolean);
        ilPics->Draw(ACanvas->Canvas, ARec.Left + 2, ARec.Top, (int)ABool, true);
        ATextToDraw = "";
      } break;
    }
  }

  SetBkMode(ACanvas->Canvas->Handle, TRANSPARENT);
  ACanvas->DrawTexT(ATextToDraw, ((TcxCustomTextEditViewInfo*)(AViewInfo->EditViewInfo))->TextRect, 0, true);
  ADone = (FCustomDrawingStyle != cdsDefaultDrawing);
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::GridsStyles()
{
  	cvPersons->OptionsView->SeparatorColor = ColorScheme[2][2];
	cvPersons->Styles->Background = stBlueSky;
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::FormCreate(
      TObject *Sender)
{
  FColorScheme = csBlue;
  GridsStyles();

  FCustomDrawingStyle = cdsBkImage;

  FFont = new TFont();

  FCarBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FCarBitmap, "CAR");

  FSkyBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FSkyBitmap, "SKY");

  FEgyptBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FEgyptBitmap, "EGYPT");

  FMyFaceBitmap = new Graphics::TBitmap();
  LoadImageFromRes(FMyFaceBitmap, "MYFACE");

  FTileBitmap =  new Graphics::TBitmap();
  LoadImageFromRes(FTileBitmap, "TILE");

  FUserDefinedImage = new Graphics::TBitmap();
  FCurrentBitmap = FTileBitmap;

  FilmsDemoDM->cdsFilmsPersons->MasterSource = FilmsDemoDM->dsPersonLines;
  FilmsDemoDM->cdsFilmsPersons->IndexFieldNames = "PersonLineID";
  FilmsDemoDM->cdsFilmsPersons->MasterFields = "ID";
}
//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::AssignCustomDrawProc()
{
  if (!cvPersons->OnCustomDrawCell)
    cvPersons->OnCustomDrawCell = cvPersonsCustomDrawCell;
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::UncheckMenuItem(TMenuItem* AMenuItem)
{
  for(int i=0; i < ComponentCount; i++)
	if((dynamic_cast<TMenuItem*>(Components[i]) != NULL) && (Components[i] != AMenuItem) &&
      ((TMenuItem*)Components[i])->Checked)
        ((TMenuItem*)Components[i])->Checked = false;
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::SetBkImage(TMenuItem* AMenuItem, Graphics::TBitmap* ABitMap)
{
  if(!((TMenuItem*)AMenuItem)->Checked) {
    ((TMenuItem*)AMenuItem)->Checked = true;
    if(FCustomDrawingStyle != cdsBkImage) {
      UncheckMenuItem(AMenuItem);
      GridsStyles();
      FCustomDrawingStyle = cdsBkImage;
      miBackgroundImage->Checked = true;
    }
    FCurrentBitmap = ABitMap;
    AssignCustomDrawProc();
    cvPersons->Painter->Invalidate();
  };
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::SetGradientColor(TMenuItem* AMenuItem,
  CustomDrawCardViewDemoTypesH::TColorScheme AColorScheme, TcxStyle *AStyle)
{
	if((FCustomDrawingStyle != cdsGradient) || (FColorScheme != AColorScheme))
	{
		FColorScheme = AColorScheme;
		AMenuItem->Checked = true;
		if (FCustomDrawingStyle != cdsGradient)
		{
			UncheckMenuItem(AMenuItem);
			FCustomDrawingStyle = cdsGradient;
			miGradient->Checked = true;
		};
		cvPersons->OptionsView->SeparatorColor = (TColor)(ColorScheme[(int)AColorScheme][2]);
		cvPersons->Styles->Background = AStyle;
		AssignCustomDrawProc();
    	cvPersons->Painter->Invalidate();
  	};
}

//---------------------------------------------------------------------------
void __fastcall TCustomDrawCardViewDemoMainForm::FormDestroy(
      TObject *Sender)
{
  FCurrentBitmap = NULL;
  delete FUserDefinedImage;
  delete FEgyptBitmap;
  delete FMyFaceBitmap;
  delete FTileBitmap;
  delete FCarBitmap;
  delete FSkyBitmap;
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miFontClick(
      TObject *Sender)
{
  FontDialog->Font->Assign(FFont);
  if (FontDialog->Execute()) {
    FFont->Assign(FontDialog->Font);
    cvPersons->Styles->ContentEven->Font->Assign(FontDialog->Font);
    cvPersons->Styles->ContentOdd->Font->Assign(FontDialog->Font);
    cvPersons->LayoutChanged(false);
    cvPersons->Painter->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miTileClick(
      TObject *Sender)
{
  SetBkImage((TMenuItem*)Sender, FTileBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miSkyClick(
      TObject *Sender)
{
  SetBkImage((TMenuItem*)Sender, FSkyBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miEgyptClick(
      TObject *Sender)
{
  SetBkImage((TMenuItem*)Sender, FEgyptBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miMyFaceClick(
      TObject *Sender)
{
  SetBkImage((TMenuItem*)Sender, FMyFaceBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miCarClick(
      TObject *Sender)
{
  SetBkImage((TMenuItem*)Sender, FCarBitmap);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miLoadClick(TObject *Sender)
{
  if(OpenDialog->Execute()){
    ((TMenuItem*)Sender)->Checked = true;
    if(FCustomDrawingStyle != cdsBkImage){
      UncheckMenuItem((TMenuItem*)Sender);
      GridsStyles();
      FCustomDrawingStyle = cdsBkImage;
      miBackgroundImage->Checked = true;
    }

    FUserDefinedImage->LoadFromFile(OpenDialog->FileName);
    FCurrentBitmap = FUserDefinedImage;
    AssignCustomDrawProc();
    cvPersons->Painter->Invalidate();
    SetCurrentDir(ExtractFilePath(Application->ExeName));
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miDependOnDataDrawingClick(
      TObject *Sender)
{
  if(!((TMenuItem*)Sender)->Checked) {
    ((TMenuItem*)Sender)->Checked = true;
    UncheckMenuItem((TMenuItem*)Sender);
    FCustomDrawingStyle = cdsDependOnData;
    GridsStyles();
    AssignCustomDrawProc();
    cvPersons->Painter->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miDefaultDrawingClick(
      TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked) {
    ((TMenuItem*)Sender)->Checked = true;
    UncheckMenuItem((TMenuItem*)Sender);
    FCustomDrawingStyle = cdsDefaultDrawing;
    cvPersons->OnCustomDrawCell = NULL;
    cvPersons->Styles->Background = NULL;
    cvPersons->OptionsView->SeparatorColor = clBtnFace;
    cvPersons->Painter->Invalidate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miGreyClick(TObject *Sender)
{
  SetGradientColor((TMenuItem*)Sender, csGrey, stGreyLight);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miGreenClick(TObject *Sender)
{
  SetGradientColor((TMenuItem*)Sender, csGreen, cxStyle2);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miGoldClick(TObject *Sender)
{
  SetGradientColor((TMenuItem*)Sender, csGold, stYellowLight);
}
//---------------------------------------------------------------------------

void __fastcall TCustomDrawCardViewDemoMainForm::miBlueClick(TObject *Sender)
{
  SetGradientColor((TMenuItem*)Sender, csBlue, stBlueSky);
}
//---------------------------------------------------------------------------
