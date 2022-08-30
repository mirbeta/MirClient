unit FilmsDemoData;

interface

uses
  {$IFDEF CLR}
  System.ComponentModel,
  {$ENDIF}
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridTableView,
  Forms, dxmdaset, DBClient, MidasLib;

type
  TFilmsDemoDM = class(TDataModule)
    cdsFilmsGenres: TClientDataSet;
    dsGenres: TDataSource;
    dsFilms: TDataSource;
    cdsFilms: TClientDataSet;
    dsPersons: TDataSource;
    dsCompanies: TDataSource;
    dsCountries: TDataSource;
    dsCompanyTypes: TDataSource;
    dsFilmsPersons: TDataSource;
    dsFilmsCompanies: TDataSource;
    dsFilmsScreens: TDataSource;
    cdsFilmsPersons: TClientDataSet;
    cdsFilmsCompanies: TClientDataSet;
    cdsFilmsScreens: TClientDataSet;
    cdsFilmsPersonsName: TStringField;
    cdsFilmsPersonsID: TIntegerField;
    cdsFilmsPersonsFilmID: TIntegerField;
    cdsFilmsPersonsPersonID: TIntegerField;
    cdsFilmsPersonsPersonLineID: TIntegerField;
    cdsFilmsPersonsBIOGRAPHY: TMemoField;
    cdsFilmsPersonsBIRTHCOUNTRY: TIntegerField;
    cdsFilmsPersonsBIRTHNAME: TStringField;
    cdsFilmsPersonsDATEOFBIRTH: TDateTimeField;
    cdsFilmsPersonsFIRSTNAME: TStringField;
    cdsFilmsPersonsLOCATIONOFBIRTH: TStringField;
    cdsFilmsPersonsNICKNAME: TStringField;
    cdsFilmsPersonsSECONDNAME: TStringField;
    cdsFilmsPersonsHOMEPAGE: TStringField;
    cdsFilmsPersonsGender: TBooleanField;
    cdsFilmsCompaniesID: TIntegerField;
    cdsFilmsCompaniesFilmID: TIntegerField;
    cdsFilmsCompaniesCompanyID: TIntegerField;
    cdsFilmsCompaniesCompanyName: TStringField;
    cdsFilmsScreensID: TAutoIncField;
    cdsFilmsScreensFILMID: TIntegerField;
    cdsFilmsScreensSCREEN: TBlobField;
    cdsFilmsScreensICON: TBlobField;
    cdsCompanyTypes: TClientDataSet;
    cdsCountries: TClientDataSet;
    cdsCompanies: TClientDataSet;
    cdsPersons: TClientDataSet;
    cdsGenres: TClientDataSet;
    cdsGenresID: TAutoIncField;
    cdsGenresNAME: TStringField;
    cdsPersonsID: TAutoIncField;
    cdsPersonsFIRSTNAME: TStringField;
    cdsPersonsSECONDNAME: TStringField;
    cdsPersonsGENDER: TBooleanField;
    cdsPersonsBIRTHNAME: TStringField;
    cdsPersonsDATEOFBIRTH: TDateTimeField;
    cdsPersonsBIRTHCOUNTRY: TIntegerField;
    cdsPersonsLOCATIONOFBIRTH: TStringField;
    cdsPersonsBIOGRAPHY: TMemoField;
    cdsPersonsNICKNAME: TStringField;
    cdsPersonsHOMEPAGE: TStringField;
    cdsCompaniesID: TAutoIncField;
    cdsCompaniesCOMPANYTYPEID: TIntegerField;
    cdsCompaniesCOUNTRYID: TIntegerField;
    cdsCompaniesCOMPANYNAME: TStringField;
    cdsCompaniesCOMPANYWEBSITE: TStringField;
    cdsCompanyTypesID: TAutoIncField;
    cdsCompanyTypesNAME: TStringField;
    cdsPersonsName: TStringField;
    cdsPersonLines: TClientDataSet;
    dsPersonLines: TDataSource;
    cdsPersonLinesID: TAutoIncField;
    cdsPersonLinesNAME: TStringField;
    dsFilmsGenres: TDataSource;
    cdsFilmsID: TAutoIncField;
    cdsFilmsCAPTION: TStringField;
    cdsFilmsYEAR: TIntegerField;
    cdsFilmsTAGLINE: TStringField;
    cdsFilmsPLOTOUTLINE: TStringField;
    cdsFilmsRUNTIME: TIntegerField;
    cdsFilmsCOLOR: TStringField;
    cdsFilmsPHOTO: TBlobField;
    cdsFilmsICON: TBlobField;
    cdsFilmsWEBSITE: TStringField;
    cdsFilmsGenresID: TAutoIncField;
    cdsFilmsGenresFILMID: TIntegerField;
    cdsFilmsGenresGENREID: TIntegerField;
    cdsFilmsGenresPHOTO: TBlobField;
    cdsFilmsGenresICON: TBlobField;
    cdsFilmsGenresCaption: TStringField;
    cdsFilmsGenresYear: TIntegerField;
    cdsFilmsGenresTAGLINE: TStringField;
    cdsFilmsGenresPLOTOUTLINE: TStringField;
    cdsFilmsGenresRunTime: TIntegerField;
    cdsFilmsGenresWebsite: TStringField;
    cdsFilmsCompaniesTypeID: TIntegerField;
    cdsFilmsCompaniesType: TStringField;
    cdsFilmsCompaniesWebSite: TStringField;
    cdsFilmsCompaniesCountryID: TIntegerField;
    cdsFilmsCompaniesCountry: TStringField;
    procedure cdsFilmsAfterPost(DataSet: TDataSet);
    procedure cdsFilmsBeforeDelete(DataSet: TDataSet);
    procedure mdGenresBeforeScroll(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
    procedure cdsFilmsGenresAfterDelete(DataSet: TDataSet);
    procedure cdsGenresAfterScroll(DataSet: TDataSet);
    procedure cdsPersonsCalcFields(DataSet: TDataSet);
    procedure cdsFilmsFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
    procedure cdsFilmsGenresAfterPost(DataSet: TDataSet);
    procedure cdsFilmsBeforePost(DataSet: TDataSet);
  private
    FFilterLockCount: Integer;

    procedure RefreshFilms;
    function GetFilmsFiltered: Boolean;
    procedure SetFilmsFiltered(Value: Boolean);
  protected
    procedure BeginFilmsUpdate;
    procedure EndFilmsUpdate;
  public
    function InsertCompany(AFilmID, ACompanyID: Integer): Boolean;
    function InsertFilm(AFilmID, AGenreID: Integer): Boolean;
    function InsertPerson(AFilmID, APersonID, APersonLineID: Integer): Boolean;

    property FilmsFiltered: Boolean read GetFilmsFiltered write SetFilmsFiltered;
  end;

var
  FilmsDemoDM: TFilmsDemoDM;

implementation

{$R *.dfm}

uses
  Variants;

type
  TClientDataSetAccess = class(TClientDataSet);

function TFilmsDemoDM.InsertCompany(AFilmID, ACompanyID: Integer): Boolean;
begin
  Result := not cdsFilmsCompanies.Locate('FILMID;COMPANYID', VarArrayOf([AFilmID, ACompanyID]), []) and
    cdsCompanies.Locate('ID', ACompanyID, []);
  if Result then
  begin
    cdsFilmsCompanies.Insert;
    cdsFilmsCompaniesFilmID.Value := AFilmID;
    cdsFilmsCompaniesCompanyID.Value := ACompanyID;
    cdsFilmsCompaniesCompanyName.Value := cdsCompaniesCOMPANYNAME.Value;
    cdsFilmsCompanies.Post;
    cdsFilmsCompanies.Locate('FILMID;COMPANYID', VarArrayOf([AFilmID, ACompanyID]), []);
  end;
end;

function TFilmsDemoDM.InsertFilm(AFilmID, AGenreID: Integer): Boolean;
begin
  Result := not cdsFilmsGenres.Locate('GENREID;FILMID', VarArrayOf([AGenreID, AFilmID]), []) and
    cdsFilms.Locate('ID', AFilmID, []);
  if Result then
  begin
    cdsFilmsGenres.Insert;
    cdsFilmsGenresGENREID.Value := AGenreID;
    cdsFilmsGenresFILMID.Value := AFilmID;
  end
  else
    cdsFilmsGenres.Edit;

  cdsFilmsGenresPHOTO.Value := cdsFilmsPHOTO.Value;
  cdsFilmsGenresICON.Value := cdsFilmsICON.Value;
  cdsFilmsGenres.Post;
  RefreshFilms;
end;

function TFilmsDemoDM.InsertPerson(AFilmID, APersonID, APersonLineID: Integer): Boolean;
begin
  Result := not cdsFilmsPersons.Locate('FILMID;PERSONID', VarArrayOf([AFilmID, APersonID]), []) and
    cdsPersons.Locate('ID', APersonID, []);
  if Result then
  begin
    cdsFilmsPersons.Insert;
    cdsFilmsPersonsFilmID.Value := AFilmID;
    cdsFilmsPersonsPersonID.Value := APersonID;
    cdsFilmsPersonsPersonLineID.Value := APersonLineID;
    cdsFilmsPersonsBIOGRAPHY.Value := cdsPersonsBIOGRAPHY.Value;
    cdsFilmsPersonsBIRTHCOUNTRY.Value := cdsPersonsBIRTHCOUNTRY.Value;
    cdsFilmsPersonsBIRTHNAME.Value := cdsPersonsBIRTHNAME.Value;
    cdsFilmsPersonsDATEOFBIRTH.Value := cdsPersonsDATEOFBIRTH.Value;
    cdsFilmsPersonsFIRSTNAME.Value := cdsPersonsFIRSTNAME.Value;
    cdsFilmsPersonsLOCATIONOFBIRTH.Value := cdsPersonsLOCATIONOFBIRTH.Value;
    cdsFilmsPersonsNICKNAME.Value := cdsPersonsNICKNAME.Value;
    cdsFilmsPersonsSECONDNAME.Value := cdsPersonsSECONDNAME.Value;
    cdsFilmsPersonsHOMEPAGE.Value := cdsPersonsHOMEPAGE.Value;
    cdsFilmsPersonsGender.Value := cdsPersonsGENDER.Value;
    cdsFilmsPersons.Post;
    cdsFilmsPersons.Locate('FILMID;PERSONID', VarArrayOf([AFilmID, APersonID]), []);
  end;
end;

procedure TFilmsDemoDM.cdsFilmsAfterPost(DataSet: TDataSet);
begin
  InsertFilm(cdsFilmsID.Value, cdsGenresID.Value);
  EndFilmsUpdate;
end;

procedure TFilmsDemoDM.cdsFilmsBeforeDelete(
  DataSet: TDataSet);
begin
  while not cdsFilmsGenres.Locate('GENREID;FILMID', VarArrayOf([cdsGenresID.Value, cdsFilmsID.Value]), []) do
    cdsFilmsGenres.Delete;
end;

procedure TFilmsDemoDM.mdGenresBeforeScroll(
  DataSet: TDataSet);
begin
  if FilmsFiltered and cdsFilms.Active and (cdsFilms.State <> dsBrowse) then
    cdsFilms.Post;
end;

procedure TFilmsDemoDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  cdsPersons.LoadFromFile(DataPath + 'Persons.xml');
  cdsPersons.Open;
  cdsCompanies.LoadFromFile(DataPath + 'Companies.xml');
  cdsCompanies.Open;
  cdsCountries.LoadFromFile(DataPath + 'Countries.xml');
  cdsCountries.Open;
  cdsCompanyTypes.LoadFromFile(DataPath + 'CompanyTypes.xml');
  cdsCompanyTypes.Open;
  cdsPersonLines.LoadFromFile(DataPath + 'PersonLines.xml');
  cdsPersonLines.Open;
  cdsGenres.LoadFromFile(DataPath + 'Genres.xml');
  cdsGenres.Open;
  cdsFilms.LoadFromFile(DataPath + 'Films.xml');
  cdsFilms.Open;
  cdsFilmsGenres.LoadFromFile(DataPath + 'FilmsGenres.xml');
  cdsFilmsGenres.Open;
  cdsFilmsPersons.LoadFromFile(DataPath + 'FilmsPersons.xml');
  cdsFilmsPersons.Open;
  cdsFilmsCompanies.LoadFromFile(DataPath + 'FilmsCompanies.xml');
  cdsFilmsCompanies.Open;
  cdsFilmsScreens.LoadFromFile(DataPath + 'FilmsScreens.xml');
  cdsFilmsScreens.Open;
end;

procedure TFilmsDemoDM.RefreshFilms;
begin
  if FilmsFiltered and (FFilterLockCount = 0) then
  begin
    FilmsFiltered := False;
    FilmsFiltered:= True;
  end;
end;

function TFilmsDemoDM.GetFilmsFiltered: Boolean;
begin
  Result := cdsFilms.Filtered;
end;

procedure TFilmsDemoDM.BeginFilmsUpdate;
begin
  Inc(FFilterLockCount);
end;

procedure TFilmsDemoDM.EndFilmsUpdate;
begin
  Dec(FFilterLockCount);
  RefreshFilms;
end;

procedure TFilmsDemoDM.SetFilmsFiltered(Value: Boolean);
begin
  cdsFilms.Filtered := Value;
end;

procedure TFilmsDemoDM.cdsFilmsGenresAfterDelete(DataSet: TDataSet);
begin
  RefreshFilms;
end;

procedure TFilmsDemoDM.cdsGenresAfterScroll(DataSet: TDataSet);
begin
  RefreshFilms;
end;

procedure TFilmsDemoDM.cdsPersonsCalcFields(DataSet: TDataSet);
begin
  cdsPersonsName.Value := cdsPersonsFIRSTNAME.Value + ' ' + cdsPersonsSECONDNAME.Value;
end;

procedure TFilmsDemoDM.cdsFilmsFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);

  function CanShowFilm(AFilmId: Integer): Boolean;
  begin
    Result := False;
    if cdsFilmsGenres.FindFirst then
    repeat
      Result := AFilmId = cdsFilmsGenresFILMID.Value;
    until not cdsFilmsGenres.FindNext or Result;
  end;

var
  V: Variant;
begin
  V := DataSet.FieldValues['ID'];
  Accept := (FFilterLockCount > 0) or not cdsFilmsGenres.Active or CanShowFilm(V);
end;

procedure TFilmsDemoDM.cdsFilmsGenresAfterPost(DataSet: TDataSet);
begin
  RefreshFilms;
end;

procedure TFilmsDemoDM.cdsFilmsBeforePost(DataSet: TDataSet);
begin
  BeginFilmsUpdate;
end;

end.
