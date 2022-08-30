//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ViewTableSimpleDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"
TViewTableSimpleDemoMainDM *ViewTableSimpleDemoMainDM;
//---------------------------------------------------------------------------
__fastcall TViewTableSimpleDemoMainDM::TViewTableSimpleDemoMainDM(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TViewTableSimpleDemoMainDM::cdsGenresAfterScroll(TDataSet *DataSet)
{
  SetFilter();
}
//---------------------------------------------------------------------------


void __fastcall TViewTableSimpleDemoMainDM::cdsFilmsBeforePost(TDataSet *DataSet)
{
  if (cdsFilms->State == dsInsert)
	cdsFilmsGenres->Insert();
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainDM::cdsFilmsAfterPost(TDataSet *DataSet)
{
  if (cdsFilmsGenres->State == dsInsert)
  {
	cdsFilms->Filtered = False;
	cdsFilms->Last();
	cdsFilmsGenres->FieldByName("GENREID")->Value = cdsGenres->FieldByName("ID")->Value;
	cdsFilmsGenres->FieldByName("FILMID")->Value = cdsFilms->FieldByName("ID")->Value;
	cdsFilmsGenres->FieldByName("PHOTO")->Value = cdsFilms->FieldByName("PHOTO")->Value;
	cdsFilmsGenres->FieldByName("ICON")->Value = cdsFilms->FieldByName("ICON")->Value;
	cdsFilmsGenres->Post();
	cdsFilms->Filtered = True;
	SetFilter();
  }
}

void TViewTableSimpleDemoMainDM::SetFilter()
{
  if (cdsFilmsGenres->Active) {
	String AStr = "";
	cdsFilmsGenres->First();
	while (!cdsFilmsGenres->Eof)
	{
	  AStr = AStr + "ID = " + IntToStr(cdsFilmsGenres->FieldByName("FILMID")->AsInteger) + " or ";
	  cdsFilmsGenres->Next();
	}
	cdsFilms->Filter = AStr + "ID = 0";
  }
}
//---------------------------------------------------------------------------

void __fastcall TViewTableSimpleDemoMainDM::DataModuleCreate(TObject *Sender)
{
  cdsGenres->LoadFromFile("..\\..\\Data\\Genres.xml");
  cdsFilms->LoadFromFile("..\\..\\Data\\Films.xml");
  cdsFilmsGenres->LoadFromFile("..\\..\\Data\\Filmsgenres.xml");
  cdsFilms->Open();
  cdsFilmsGenres->Open();
  cdsGenres->Open();
}
//---------------------------------------------------------------------------

