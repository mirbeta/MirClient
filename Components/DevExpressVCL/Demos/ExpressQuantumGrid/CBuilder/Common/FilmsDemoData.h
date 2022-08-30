//---------------------------------------------------------------------------

#ifndef FilmsDemoDataH
#define FilmsDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <DB.hpp>
#include <DBClient.hpp>
#include "cxLookAndFeels.hpp"
//---------------------------------------------------------------------------
class TFilmsDemoDM : public TDataModule
{
__published:  // IDE-managed Components
    TClientDataSet *cdsFilmsGenres;
    TDataSource *dsGenres;
    TDataSource *dsFilms;
    TClientDataSet *cdsFilms;
    TDataSource *dsPersons;
    TDataSource *dsCompanies;
    TDataSource *dsCountries;
    TDataSource *dsCompanyTypes;
    TDataSource *dsFilmsPersons;
    TDataSource *dsFilmsCompanies;
    TDataSource *dsFilmsScreens;
    TClientDataSet *cdsFilmsPersons;
    TClientDataSet *cdsFilmsCompanies;
    TClientDataSet *cdsFilmsScreens;
    TStringField *cdsFilmsPersonsName;
    TIntegerField *cdsFilmsPersonsID;
    TIntegerField *cdsFilmsPersonsFilmID;
    TIntegerField *cdsFilmsPersonsPersonID;
    TIntegerField *cdsFilmsPersonsPersonLineID;
    TMemoField *cdsFilmsPersonsBIOGRAPHY;
    TIntegerField *cdsFilmsPersonsBIRTHCOUNTRY;
    TStringField *cdsFilmsPersonsBIRTHNAME;
    TDateTimeField *cdsFilmsPersonsDATEOFBIRTH;
    TStringField *cdsFilmsPersonsFIRSTNAME;
    TStringField *cdsFilmsPersonsLOCATIONOFBIRTH;
    TStringField *cdsFilmsPersonsNICKNAME;
    TStringField *cdsFilmsPersonsSECONDNAME;
    TStringField *cdsFilmsPersonsHOMEPAGE;
    TBooleanField *cdsFilmsPersonsGender;
    TIntegerField *cdsFilmsCompaniesID;
    TIntegerField *cdsFilmsCompaniesFilmID;
    TIntegerField *cdsFilmsCompaniesCompanyID;
    TStringField *cdsFilmsCompaniesCompanyName;
    TAutoIncField *cdsFilmsScreensID;
    TIntegerField *cdsFilmsScreensFILMID;
    TBlobField *cdsFilmsScreensSCREEN;
    TBlobField *cdsFilmsScreensICON;
    TClientDataSet *cdsCompanyTypes;
    TClientDataSet *cdsCountries;
    TClientDataSet *cdsCompanies;
    TClientDataSet *cdsPersons;
    TClientDataSet *cdsGenres;
    TAutoIncField *cdsGenresID;
    TStringField *cdsGenresNAME;
    TAutoIncField *cdsPersonsID;
    TStringField *cdsPersonsFIRSTNAME;
    TStringField *cdsPersonsSECONDNAME;
    TBooleanField *cdsPersonsGENDER;
    TStringField *cdsPersonsBIRTHNAME;
    TDateTimeField *cdsPersonsDATEOFBIRTH;
    TIntegerField *cdsPersonsBIRTHCOUNTRY;
    TStringField *cdsPersonsLOCATIONOFBIRTH;
    TMemoField *cdsPersonsBIOGRAPHY;
    TStringField *cdsPersonsNICKNAME;
    TStringField *cdsPersonsHOMEPAGE;
    TAutoIncField *cdsCompaniesID;
    TIntegerField *cdsCompaniesCOMPANYTYPEID;
    TIntegerField *cdsCompaniesCOUNTRYID;
    TStringField *cdsCompaniesCOMPANYNAME;
    TStringField *cdsCompaniesCOMPANYWEBSITE;
    TAutoIncField *cdsCompanyTypesID;
    TStringField *cdsCompanyTypesNAME;
    TStringField *cdsPersonsName;
    TClientDataSet *cdsPersonLines;
    TDataSource *dsPersonLines;
    TAutoIncField *cdsPersonLinesID;
    TStringField *cdsPersonLinesNAME;
    TDataSource *dsFilmsGenres;
    TAutoIncField *cdsFilmsID;
    TStringField *cdsFilmsCAPTION;
    TIntegerField *cdsFilmsYEAR;
    TStringField *cdsFilmsTAGLINE;
    TStringField *cdsFilmsPLOTOUTLINE;
    TIntegerField *cdsFilmsRUNTIME;
    TStringField *cdsFilmsCOLOR;
    TBlobField *cdsFilmsPHOTO;
    TBlobField *cdsFilmsICON;
    TStringField *cdsFilmsWEBSITE;
    TAutoIncField *cdsFilmsGenresID;
    TIntegerField *cdsFilmsGenresFILMID;
    TIntegerField *cdsFilmsGenresGENREID;
    TBlobField *cdsFilmsGenresPHOTO;
    TBlobField *cdsFilmsGenresICON;
    TStringField *cdsFilmsGenresCaption;
    TIntegerField *cdsFilmsGenresYear;
    TStringField *cdsFilmsGenresTAGLINE;
    TStringField *cdsFilmsGenresPLOTOUTLINE;
    TIntegerField *cdsFilmsGenresRunTime;
    TStringField *cdsFilmsGenresWebsite;
    TIntegerField *cdsFilmsCompaniesTypeID;
    TStringField *cdsFilmsCompaniesType;
    TStringField *cdsFilmsCompaniesWebSite;
    TIntegerField *cdsFilmsCompaniesCountryID;
    TStringField *cdsFilmsCompaniesCountry;
    void __fastcall cdsFilmsAfterPost(TDataSet *DataSet);
    void __fastcall cdsFilmsBeforeDelete(TDataSet *DataSet);
    void __fastcall mdGenresBeforeScroll(TDataSet *DataSet);
    void __fastcall DataModuleCreate(TObject *Sender);
    void __fastcall cdsFilmsGenresAfterDelete(TDataSet *DataSet);
    void __fastcall cdsGenresAfterScroll(TDataSet *DataSet);
    void __fastcall cdsPersonsCalcFields(TDataSet *DataSet);
    void __fastcall cdsFilmsFilterRecord(TDataSet *DataSet, bool &Accept);
    void __fastcall cdsFilmsGenresAfterPost(TDataSet *DataSet);
    void __fastcall cdsFilmsBeforePost(TDataSet *DataSet);
private:  // User declarations
	int FFilterLockCount;

	bool CanShowFilm(int AFilmId);
    void RefreshFilms();
    bool GetFilmsFiltered();
    void SetFilmsFiltered(bool Value);
protected:
    void BeginFilmsUpdate();
    void EndFilmsUpdate();
public:   // User declarations
	__fastcall TFilmsDemoDM(TComponent* Owner);

    bool InsertCompany(int AFilmID, int ACompanyID);
    bool InsertFilm(int AFilmID, int AGenreID);
    bool InsertPerson(int AFilmID, int APersonID, int APersonLineID);
	
	__property bool FilmsFiltered = {read=GetFilmsFiltered, write=SetFilmsFiltered};
};
//---------------------------------------------------------------------------
extern PACKAGE TFilmsDemoDM *FilmsDemoDM;
//---------------------------------------------------------------------------
#endif
