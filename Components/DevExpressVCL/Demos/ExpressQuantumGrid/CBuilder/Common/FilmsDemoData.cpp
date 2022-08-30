//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "FilmsDemoData.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxEdit"
#pragma link "cxEditRepositoryItems"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TFilmsDemoDM *FilmsDemoDM;
//---------------------------------------------------------------------------
__fastcall TFilmsDemoDM::TFilmsDemoDM(TComponent* Owner): TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
bool TFilmsDemoDM::InsertCompany(int AFilmID, int ACompanyID)
{
	int Bounds[2] = {0, 1};
	Variant AKeyValues = VarArrayCreate(Bounds, 1, varInteger);
	AKeyValues.PutElement(AFilmID, 0);
	AKeyValues.PutElement(ACompanyID, 1);

	bool Result = !cdsFilmsCompanies->Locate("FILMID;COMPANYID", AKeyValues, TLocateOptions()) &&
		cdsCompanies->Locate("ID", ACompanyID, TLocateOptions());

	if (Result)
	{
		cdsFilmsCompanies->Insert();
		cdsFilmsCompaniesFilmID->Value = AFilmID;
		cdsFilmsCompaniesCompanyID->Value = ACompanyID;
		cdsFilmsCompaniesCompanyName->Value = cdsCompaniesCOMPANYNAME->Value;
		cdsFilmsCompanies->Post();
		cdsFilmsCompanies->Locate("FILMID;COMPANYID", AKeyValues, TLocateOptions());
	};
  	return Result;
};
//---------------------------------------------------------------------------
bool TFilmsDemoDM::InsertFilm(int AFilmID, int AGenreID)
{
	int Bounds[2] = {0, 1};
	Variant AKeyValues = VarArrayCreate(Bounds, 1, varInteger);
	AKeyValues.PutElement(AGenreID, 0);
	AKeyValues.PutElement(AFilmID, 1);
	
	bool Result = !cdsFilmsGenres->Locate("GENREID;FILMID", AKeyValues, TLocateOptions()) &&
		cdsFilms->Locate("ID", AFilmID, TLocateOptions());

	if (Result)
	{
		cdsFilmsGenres->Insert();
		cdsFilmsGenresGENREID->Value = AGenreID;
		cdsFilmsGenresFILMID->Value = AFilmID;
	}
	else
		cdsFilmsGenres->Edit();

	cdsFilmsGenresPHOTO->Value = cdsFilmsPHOTO->Value;
	cdsFilmsGenresICON->Value = cdsFilmsICON->Value;
	cdsFilmsGenres->Post();
	RefreshFilms();    
	return Result;
};
//---------------------------------------------------------------------------
bool TFilmsDemoDM::InsertPerson(int AFilmID, int APersonID, int APersonLineID)
{
	int Bounds[2] = {0, 1};
	Variant AKeyValues = VarArrayCreate(Bounds, 1, varInteger);
	AKeyValues.PutElement(AFilmID, 0);
	AKeyValues.PutElement(APersonID, 1);

	bool Result = !cdsFilmsPersons->Locate("FILMID;PERSONID", AKeyValues, TLocateOptions()) &&
		cdsPersons->Locate("ID", APersonID, TLocateOptions());

	if (Result)
	{
		cdsFilmsPersons->Insert();
		cdsFilmsPersonsFilmID->Value = AFilmID;
		cdsFilmsPersonsPersonID->Value = APersonID;
		cdsFilmsPersonsPersonLineID->Value = APersonLineID;
		cdsFilmsPersonsBIOGRAPHY->Value = cdsPersonsBIOGRAPHY->Value;
		cdsFilmsPersonsBIRTHCOUNTRY->Value = cdsPersonsBIRTHCOUNTRY->Value;
		cdsFilmsPersonsBIRTHNAME->Value = cdsPersonsBIRTHNAME->Value;
		cdsFilmsPersonsDATEOFBIRTH->Value = cdsPersonsDATEOFBIRTH->Value;
		cdsFilmsPersonsFIRSTNAME->Value = cdsPersonsFIRSTNAME->Value;
		cdsFilmsPersonsLOCATIONOFBIRTH->Value = cdsPersonsLOCATIONOFBIRTH->Value;
		cdsFilmsPersonsNICKNAME->Value = cdsPersonsNICKNAME->Value;
		cdsFilmsPersonsSECONDNAME->Value = cdsPersonsSECONDNAME->Value;
		cdsFilmsPersonsHOMEPAGE->Value = cdsPersonsHOMEPAGE->Value;
		cdsFilmsPersonsGender->Value = cdsPersonsGENDER->Value;
		cdsFilmsPersons->Post();
		cdsFilmsPersons->Locate("FILMID;PERSONID", AKeyValues, TLocateOptions());
	};
  	return Result;
};
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsAfterPost(TDataSet *DataSet)
{
  	InsertFilm(cdsFilmsID->Value, cdsGenresID->Value);
	EndFilmsUpdate();
};
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsBeforeDelete(TDataSet *DataSet)
{
	int Bounds[2] = {0, 1};
	Variant AKeyValues = VarArrayCreate(Bounds, 1, varInteger);

	while (true)
	{
		AKeyValues.PutElement(cdsGenresID->Value, 0);
		AKeyValues.PutElement(cdsFilmsID->Value, 1);
		if (cdsFilmsGenres->Locate("GENREID;FILMID", AKeyValues, TLocateOptions()))
			break;
		else
			cdsFilmsGenres->Delete();
	}
};
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::mdGenresBeforeScroll(TDataSet *DataSet)
{
	if (FilmsFiltered && cdsFilms->Active && (cdsFilms->State != dsBrowse))
	{
		cdsFilms->Post();
	}
};
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::DataModuleCreate(TObject *Sender)
{
	String DataPath = "..\\..\\Data\\";
	cdsPersons->LoadFromFile(DataPath + "Persons.xml");
	cdsPersons->Open();
	cdsCompanies->LoadFromFile(DataPath + "Companies.xml");
	cdsCompanies->Open();
	cdsCountries->LoadFromFile(DataPath + "Countries.xml");
	cdsCountries->Open();
	cdsCompanyTypes->LoadFromFile(DataPath + "CompanyTypes.xml");
	cdsCompanyTypes->Open();
	cdsPersonLines->LoadFromFile(DataPath + "PersonLines.xml");
	cdsPersonLines->Open();
	cdsGenres->LoadFromFile(DataPath + "Genres.xml");
	cdsGenres->Open();
	cdsFilms->LoadFromFile(DataPath + "Films.xml");
	cdsFilms->Open();
	cdsFilmsGenres->LoadFromFile(DataPath + "FilmsGenres.xml");
	cdsFilmsGenres->Open();
	cdsFilmsPersons->LoadFromFile(DataPath + "FilmsPersons.xml");
	cdsFilmsPersons->Open();
	cdsFilmsCompanies->LoadFromFile(DataPath + "FilmsCompanies.xml");
	cdsFilmsCompanies->Open();
	cdsFilmsScreens->LoadFromFile(DataPath + "FilmsScreens.xml");
	cdsFilmsScreens->Open();
};
//---------------------------------------------------------------------------
void TFilmsDemoDM::RefreshFilms()
{
	if (FilmsFiltered && (FFilterLockCount == 0))
	{
		FilmsFiltered = false;
		FilmsFiltered = true;
	}
}
//---------------------------------------------------------------------------
bool TFilmsDemoDM::GetFilmsFiltered()
{
	return cdsFilms->Filtered;
};
//---------------------------------------------------------------------------
void TFilmsDemoDM::BeginFilmsUpdate()
{
	FFilterLockCount++;
};
//---------------------------------------------------------------------------
void TFilmsDemoDM::EndFilmsUpdate()
{
	FFilterLockCount--;
	RefreshFilms();
};
//---------------------------------------------------------------------------
void TFilmsDemoDM::SetFilmsFiltered(bool Value)
{
	cdsFilms->Filtered = Value;
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsGenresAfterDelete(TDataSet *DataSet)
{
	RefreshFilms();
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsGenresAfterScroll(TDataSet *DataSet)
{
	RefreshFilms();
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsPersonsCalcFields(TDataSet *DataSet)
{
	cdsPersonsName->Value = cdsPersonsFIRSTNAME->Value + ' ' + cdsPersonsSECONDNAME->Value;
}
//---------------------------------------------------------------------------
bool TFilmsDemoDM::CanShowFilm(int AFilmId)
{
    bool Result = false;
    if (cdsFilmsGenres->FindFirst())
    {
		do
		{
			Result = AFilmId == cdsFilmsGenresFILMID->Value;
		}
		while (cdsFilmsGenres->FindNext() && !Result);
	}
	return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsFilterRecord(TDataSet *DataSet, bool &Accept)
{
	Variant V = DataSet->FieldValues["ID"];
	Accept = (FFilterLockCount > 0) || !cdsFilmsGenres->Active || CanShowFilm(V);
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsGenresAfterPost(TDataSet *DataSet)
{
	RefreshFilms();
}
//---------------------------------------------------------------------------
void __fastcall TFilmsDemoDM::cdsFilmsBeforePost(TDataSet *DataSet)
{
  BeginFilmsUpdate();
}
//---------------------------------------------------------------------------


