unit FilmsDemoData;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, DB, Forms,
  cxStyles, cxClasses, cxGridTableView, dxCoreClasses,
{$IFDEF DELPHIXE5}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Dapt, FireDAC.Phys.SQLiteWrapper, FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite,
{$ELSE}
  uADStanIntf, uADStanOption, uADStanError, uADGUIxIntf, uADPhysIntf, uADStanDef,
  uADStanPool, uADStanAsync, uADPhysManager, uADStanExprFuncs, uADGUIxFormsWait, uADCompGUIx, uADPhysSQLite,
  uADCompClient,uADDatSManager, uADDAptIntf, uADDAptManager,
{$ENDIF}
  dxEMF.Core, dxEMF.DB.SQLite, dxEMF.DataProvider.FireDAC, dxEMF.Data, dxEMF.DataSet,
  Films.Entities;

type
  TFilmsDemoDM = class(TDataModule)
    Session: TdxEMFSession;
    FireDACDataProvider: TdxEMFFireDACDataProvider;
    dxEMFDataContext1: TdxEMFDataContext;
    dsGenres: TDataSource;
    dsFilms: TDataSource;
    dsPersons: TDataSource;
    dsCompanies: TDataSource;
    dsCountries: TDataSource;
    dsCompanyTypes: TDataSource;
    dsFilmsPersons: TDataSource;
    dsFilmsCompanies: TDataSource;
    dsFilmsScreens: TDataSource;
    edsFilmsWEBSITE: TWideStringField;
    edsFilmsFilmPersons: TDataSetField;
    dsPersonLines: TDataSource;
    dsFilmsGenres: TDataSource;
    edsFilms: TdxEMFDataSet;
    edsFilmsID: TAutoIncField;
    edsFilmsCAPTION: TWideStringField;
    edsFilmsYEAR: TIntegerField;
    edsFilmsTAGLINE: TWideStringField;
    edsFilmsPLOTOUTLINE: TWideStringField;
    edsFilmsRUNTIME: TIntegerField;
    edsFilmsCOLOR: TWideStringField;
    edsFilmsPHOTO: TBlobField;
    edsFilmsICON: TBlobField;
    edsPersonLines: TdxEMFDataSet;
    edsPersonLinesID: TAutoIncField;
    edsPersonLinesNAME: TWideStringField;
    edsFilmsPersons: TdxEMFDataSet;
    edsFilmsPersonsID: TAutoIncField;
    edsFilmsPersonsName: TWideStringField;
    edsFilmsPersonsBIRTHNAME: TWideStringField;
    edsFilmsPersonsPersonLineID: TdxEntityField;
    edsFilmsPersonsFilmID: TdxEntityField;
    edsFilmsPersonsPersonID: TdxEntityField;
    edsFilmsPersonsBIRTHCOUNTRY: TdxEntityField;
    edsFilmsPersonsBIOGRAPHY: TWideMemoField;
    edsFilmsPersonsDATEOFBIRTH: TDateTimeField;
    edsFilmsPersonsFIRSTNAME: TWideStringField;
    edsFilmsPersonsLOCATIONOFBIRTH: TWideStringField;
    edsFilmsPersonsNICKNAME: TWideStringField;
    edsFilmsPersonsSECONDNAME: TWideStringField;
    edsFilmsPersonsHOMEPAGE: TWideStringField;
    edsFilmsPersonsGender: TBooleanField;
    edsFilmsCompanies: TdxEMFDataSet;
    edsFilmsCompaniesID: TAutoIncField;
    edsFilmsCompaniesFilmID: TdxEntityField;
    edsFilmsCompaniesCompanyID: TdxEntityField;
    edsFilmsCompaniesCompanyName: TWideStringField;
    edsFilmsCompaniesTypeID: TIntegerField;
    edsFilmsCompaniesType: TWideStringField;
    edsFilmsCompaniesWebSite: TWideStringField;
    edsFilmsCompaniesCountryID: TIntegerField;
    edsFilmsCompaniesCountry: TWideStringField;
    edsCompanyTypes: TdxEMFDataSet;
    edsCompanyTypesID: TAutoIncField;
    edsCompanyTypesNAME: TWideStringField;
    edsCompanies: TdxEMFDataSet;
    edsCompaniesID: TAutoIncField;
    edsCompaniesCOMPANYTYPEID: TdxEntityField;
    edsCompaniesCOUNTRYID: TdxEntityField;
    edsCompaniesCOMPANYNAME: TWideStringField;
    edsCompaniesCOMPANYWEBSITE: TWideStringField;
    edsCountries: TdxEMFDataSet;
    edsCountriesID: TAutoIncField;
    edsCountriesNAME: TWideStringField;
    edsCountriesACRONYM: TWideStringField;
    edsCountriesNATIONALFLAG: TBlobField;
    edsPersons: TdxEMFDataSet;
    edsPersonsID: TAutoIncField;
    edsPersonsFIRSTNAME: TWideStringField;
    edsPersonsSECONDNAME: TWideStringField;
    edsPersonsGENDER: TBooleanField;
    edsPersonsBIRTHNAME: TWideStringField;
    edsPersonsDATEOFBIRTH: TDateTimeField;
    edsPersonsBIRTHCOUNTRY: TdxEntityField;
    edsPersonsLOCATIONOFBIRTH: TWideStringField;
    edsPersonsBIOGRAPHY: TWideMemoField;
    edsPersonsNICKNAME: TWideStringField;
    edsPersonsHOMEPAGE: TWideStringField;
    edsPersonsName: TWideStringField;
    edsGenres: TdxEMFDataSet;
    edsGenresID: TAutoIncField;
    edsGenresNAME: TWideStringField;
    edsGenresFilmsGenres: TDataSetField;
    edsFilmsScreens: TdxEMFDataSet;
    edsFilmsScreensID: TAutoIncField;
    edsFilmsScreensFILMID: TdxEntityField;
    edsFilmsScreensSCREEN: TBlobField;
    edsFilmsScreensICON: TBlobField;
    edsFilmsGenres: TdxEMFDataSet;
    edsFilmsGenresID: TAutoIncField;
    edsFilmsGenresFILMID: TdxEntityField;
    edsFilmsGenresGENREID: TdxEntityField;
    edsFilmsGenresPHOTO: TBlobField;
    edsFilmsGenresICON: TBlobField;
    edsFilmsGenresCaption: TWideStringField;
    edsFilmsGenresYear: TIntegerField;
    edsFilmsGenresTAGLINE: TWideStringField;
    edsFilmsGenresPLOTOUTLINE: TWideStringField;
    edsFilmsGenresRunTime: TIntegerField;
    edsFilmsGenresWebsite: TWideStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure edsPersonsCalcFields(DataSet: TDataSet);
    procedure edsRefreshFilms(DataSet: TDataSet);
    procedure edsFilmsFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure edsFilmsAfterPost(DataSet: TDataSet);
    procedure edsFilmsBeforeDelete(DataSet: TDataSet);
    procedure edsGenresAfterScroll(DataSet: TDataSet);
    procedure edsFilmsBeforePost(DataSet: TDataSet);
  private
{$IFDEF DELPHIXE5}
    FConnection: TFDConnection;
{$ELSE}
    FConnection: TADConnection;
{$ENDIF}
    FFilterLockCount: Integer;

    procedure RefreshFilms;
    function GetFilmsFiltered: Boolean;
    procedure SetFilmsFiltered(Value: Boolean);
    procedure DBInitialization;
  protected
    procedure BeginFilmsUpdate;
    procedure EndFilmsUpdate;
  public
    procedure AssignFilmsPersons;

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
  Variants, RTTI,
  dxEMF.DB.Criteria,
  dxEMF.Types,
  dxEMF.Utils;

function TFilmsDemoDM.InsertCompany(AFilmID, ACompanyID: Integer): Boolean;
begin
  Result := not edsFilmsCompanies.Locate('FILMID;COMPANYID', VarArrayOf([AFilmID, ACompanyID]), []) and
    edsCompanies.Locate('ID', ACompanyID, []);
  if Result then
  begin
    edsFilmsCompanies.Insert;
    edsFilmsCompaniesFilmID.AsObject := Session.Find(TFilms, AFilmID);
    edsFilmsCompaniesCompanyID.AsObject := Session.Find(TCompanies, ACompanyID);
    edsFilmsCompanies.Post;
    edsFilmsCompanies.Locate('FILMID;COMPANYID', VarArrayOf([AFilmID, ACompanyID]), []);
  end;
end;

function TFilmsDemoDM.InsertFilm(AFilmID, AGenreID: Integer): Boolean;
begin
  Result := not edsFilmsGenres.Locate('GENREID;FILMID', VarArrayOf([AGenreID, AFilmID]), []) and
    edsFilms.Locate('ID', AFilmID, []);
  if Result then
  begin
    edsFilmsGenres.Insert;
    edsFilmsGenresGENREID.AsObject := Session.Find(TGenres, AGenreID);
    edsFilmsGenresFILMID.AsObject := Session.Find(TFilms, AFilmID);
    edsFilmsGenres.Post;
  end;

  RefreshFilms;
end;

function TFilmsDemoDM.InsertPerson(AFilmID, APersonID, APersonLineID: Integer): Boolean;
begin
  Result := not edsFilmsPersons.Locate('FILMID;PERSONID', VarArrayOf([AFilmID, APersonID]), []) and
    edsPersons.Locate('ID', APersonID, []);
  if Result then
  begin
    edsFilmsPersons.Insert;
    edsFilmsPersonsFilmID.AsObject := Session.Find(TFilms, AFilmID);
    edsFilmsPersonsPersonID.AsObject := Session.Find(TPersons, APersonID);
    edsFilmsPersonsPersonLineID.AsObject := Session.Find(TPersonLines, APersonLineID);
    edsFilmsPersons.Post;
  end;
end;

procedure TFilmsDemoDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\..\Data\';
  DBFileName = 'Films.s3db';
begin
{$IFDEF DELPHIXE5}
  FConnection := TFDConnection.Create(Self);
{$ELSE}
  FConnection := TADConnection.Create(Self);
{$ENDIF}
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Values['Database'] := DataPath + DBFileName;
  FireDACDataProvider.Connection := FConnection;

  DBInitialization;
end;

procedure TFilmsDemoDM.DBInitialization;
begin
  edsPersons.Open;
  edsCompanies.Open;
  edsCountries.Open;
  edsCompanyTypes.Open;
  edsPersonLines.Open;
  edsFilms.Open;
  edsGenres.Open;
  edsFilmsPersons.Open;
  edsFilmsCompanies.Open;
  edsFilmsScreens.Open;
end;

procedure TFilmsDemoDM.RefreshFilms;
begin
  if FilmsFiltered and (FFilterLockCount = 0) then
  begin
    FilmsFiltered := False;
    FilmsFiltered := True;
  end;
end;

function TFilmsDemoDM.GetFilmsFiltered: Boolean;
begin
  Result := edsFilms.Filtered;
end;

procedure TFilmsDemoDM.AssignFilmsPersons;
begin
  { TODO : check message }
  // The grid control's data controller requires that you sort records by FilmID field values to enable built-in record management
  edsFilmsPersons.DataSetField := nil;
  edsFilmsPersons.EntityClass := TFilmsPersons;
  edsFilmsPersons.Open;
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
  edsFilms.Filtered := Value;
end;

procedure TFilmsDemoDM.edsFilmsAfterPost(DataSet: TDataSet);
begin
  InsertFilm(edsFilmsID.Value, edsGenresID.Value);
  EndFilmsUpdate;
end;

procedure TFilmsDemoDM.edsPersonsCalcFields(DataSet: TDataSet);
begin
  edsPersonsName.Value := edsPersonsFIRSTNAME.Value + ' ' + edsPersonsSECONDNAME.Value;
end;

procedure TFilmsDemoDM.edsRefreshFilms(DataSet: TDataSet);
begin
  RefreshFilms;
end;

procedure TFilmsDemoDM.edsFilmsBeforeDelete(DataSet: TDataSet);
begin
  while edsFilmsGenres.Locate('FILMID', edsFilmsID.Value, []) do
    edsFilmsGenres.Delete;
end;

procedure TFilmsDemoDM.edsFilmsBeforePost(DataSet: TDataSet);
begin
  BeginFilmsUpdate;
end;

procedure TFilmsDemoDM.edsFilmsFilterRecord(DataSet: TDataSet; var Accept: Boolean);

  function CanShowFilm(AFilmId: Integer): Boolean;
  begin
    Result := False;
    if edsFilmsGenres.FindFirst then
    repeat
      Result := AFilmId = edsFilmsGenresFILMID.Value;
    until not edsFilmsGenres.FindNext or Result;
  end;

var
  V: Variant;
begin
  V := DataSet.FieldValues['ID'];
  Accept := (FFilterLockCount > 0) or not edsFilmsGenres.Active or CanShowFilm(V);
end;

procedure TFilmsDemoDM.edsGenresAfterScroll(DataSet: TDataSet);
begin
  RefreshFilms;
end;

end.
