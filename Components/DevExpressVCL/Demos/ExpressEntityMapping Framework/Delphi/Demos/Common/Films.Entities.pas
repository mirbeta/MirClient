unit Films.Entities;

interface

uses
  SysUtils,
  dxCoreClasses,
  dxEMF.Attributes,
  dxEMF.Metadata,
  dxEMF.Core,
  dxEMF.Types;

type

  TFilmsPersons = class;
  TFilmsGenres = class;
  TCountries = class;

  [Entity]
  [Indexes('SECONDNAME')]
  TPersons = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(50)]
    FFIRSTNAME: string;
    [Column, Size(50)]
    FSECONDNAME: string;
    [Column]
    FGENDER: Boolean;
    [Column, Size(50)]
    FBIRTHNAME: string;
    [Column, Nullable]
    FDATEOFBIRTH: TDateTime;
    [Column]
    FBIRTHCOUNTRY: TCountries;
    [Column, Size(50)]
    FLOCATIONOFBIRTH: string;
    [Column, Blob]
    FBIOGRAPHY: string;
    [Column, Size(50)]
    FNICKNAME: string;
    [Column, Size(100)]
    FHOMEPAGE: string;
  public
    property ID: Integer read FID write FID;
    property FIRSTNAME: string read FFIRSTNAME write FFIRSTNAME;
    property SECONDNAME: string read FSECONDNAME write FSECONDNAME;
    property GENDER: Boolean read FGENDER write FGENDER;
    property BIRTHNAME: string read FBIRTHNAME write FBIRTHNAME;
    property DATEOFBIRTH: TDateTime read FDATEOFBIRTH write FDATEOFBIRTH;
    property BIRTHCOUNTRY: TCountries read FBIRTHCOUNTRY write FBIRTHCOUNTRY;
    property LOCATIONOFBIRTH: string read FLOCATIONOFBIRTH write FLOCATIONOFBIRTH;
    property BIOGRAPHY: string read FBIOGRAPHY write FBIOGRAPHY;
    property NICKNAME: string read FNICKNAME write FNICKNAME;
    property HOMEPAGE: string read FHOMEPAGE write FHOMEPAGE;
  end;

  [Entity]
  TCompanyTypes = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(50)]
    FNAME: string;
  public
    property ID: Integer read FID write FID;
    property NAME: string read FNAME write FNAME;
  end;

  [Entity]
  TCountries = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(60)]
    FNAME: string;
    [Column, Size(50)]
    FACRONYM: string;
    FNATIONALFLAG: TBytes;
  public
    property ID: Integer read FID write FID;
    property NAME: string read FNAME write FNAME;
    property ACRONYM: string read FACRONYM write FACRONYM;
    [Column, Blob]
    property NATIONALFLAG: TBytes read FNATIONALFLAG write FNATIONALFLAG;
  end;

  [Entity]
  TCompanies = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column]
    FCOMPANYTYPEID: TCompanyTypes;
    [Column]
    FCOUNTRYID: TCountries;
    [Column, Size(50)]
    FCOMPANYNAME: string;
    [Column, Size(50)]
    FCOMPANYWEBSITE: string;
  public
    property ID: Integer read FID write FID;
    property COMPANYTYPEID: TCompanyTypes read FCOMPANYTYPEID write FCOMPANYTYPEID;
    property COUNTRYID: TCountries read FCOUNTRYID write FCOUNTRYID;
    property COMPANYNAME: string read FCOMPANYNAME write FCOMPANYNAME;
    property COMPANYWEBSITE: string read FCOMPANYWEBSITE write FCOMPANYWEBSITE;
  end;

  [Entity]
  TPersonLines = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(50)]
    FNAME: string;
  public
    property ID: Integer read FID write FID;
    property NAME: string read FNAME write FNAME;
  end;

  [Entity]
  TGenres = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(50)]
    FNAME: string;
    [Association('FilmsGenres')]
    FFilmsGenres: IdxEMFCollection<TFilmsGenres>;
  public
    constructor Create;
    property ID: Integer read FID write FID;
    property NAME: string read FNAME write FNAME;
    property FilmsGenres: IdxEMFCollection<TFilmsGenres> read FFilmsGenres;
  end;

  [Entity]
  TFilms = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Size(50), Nullable]
    FCAPTION: string;
    [Column, Nullable]
    FYEAR: Integer;
    [Column, Size(250), Nullable]
    FTAGLINE: string;
    [Column, Size(200), Nullable]
    FPLOTOUTLINE: string;
    [Column, Nullable]
    FRUNTIME: Integer;
    [Column, Size(50), Nullable]
    FCOLOR: string;
    FPHOTO: TBytes;
    FICON: TBytes;
    [Column, Size(50), Nullable]
    FWEBSITE: string;
    [Association('FilmsPersons')]
    FFilmPersons: IdxEMFCollection<TFilmsPersons>;
  public
    constructor Create;
    property ID: Integer read FID write FID;
    property CAPTION: string read FCAPTION write FCAPTION;
    property YEAR: Integer read FYEAR write FYEAR;
    property TAGLINE: string read FTAGLINE write FTAGLINE;
    property PLOTOUTLINE: string read FPLOTOUTLINE write FPLOTOUTLINE;
    property RUNTIME: Integer read FRUNTIME write FRUNTIME;
    property COLOR: string read FCOLOR write FCOLOR;
    [Column, Nullable]
    property PHOTO: TBytes read FPHOTO write FPHOTO;
    [Column, Nullable]
    property ICON: TBytes read FICON write FICON;
    property WEBSITE: string read FWEBSITE write FWEBSITE;
    property FilmPersons: IdxEMFCollection<TFilmsPersons> read FFilmPersons;
  end;

  [Entity]
  TFilmsGenres = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column]
    FFILMID: TFilms;
    [Column, Association('FilmsGenres')]
    FGENREID: TGenres;
    function GetICON: TBytes;
    function GetPHOTO: TBytes;
  public
    property ID: Integer read FID write FID;
    property FILMID: TFilms read FFILMID write FFILMID;
    property GENREID: TGenres read FGENREID write FGENREID;
    [VirtualColumn]
    property PHOTO: TBytes read GetPHOTO;
    [VirtualColumn]
    property ICON: TBytes read GetICON;
  end;

  [Entity]
  TFilmsScreens = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column]
    FFILMID: TFilms;
    FSCREEN: TBytes;
    FICON: TBytes;
  public
    property ID: Integer read FID write FID;
    property FILMID: TFilms read FFILMID write FFILMID;
    [Column, Blob]
    property SCREEN: TBytes read FSCREEN write FSCREEN;
    [Column, Blob]
    property ICON: TBytes read FICON write FICON;
  end;

  [Entity]
  TFilmsCompanies = class
  private
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column]
    FFilmID: TFilms;
    [Column]
    FCompanyID: TCompanies;
    function GetCompanyName: string;
  public
    property ID: Integer read FID write FID;
    property FilmID: TFilms read FFilmID write FFilmID;
    property CompanyID: TCompanies read FCompanyID write FCompanyID;
    [VirtualColumn]
    property CompanyName: string read GetCompanyName;
  end;

  [Entity]
  [Indexes('FilmID')]
  [Indexes('PersonID')]
  TFilmsPersons = class
  private
    [Column, Size(20), Nullable]
    FName: string;
    [Column, Key, Generator(TdxGeneratorType.Identity)]
    FID: Integer;
    [Column, Association('FilmsPersons')]
    FFilmID: TFilms;
    [Column]
    FPersonID: TPersons;
    [Column]
    FPersonLineID: TPersonLines;
    function GetBIOGRAPHY: TdxNullableValue<string>; inline;
    function GetBIRTHCOUNTRY: TCountries; inline;
    function GetBIRTHNAME: TdxNullableValue<string>; inline;
    function GetDATEOFBIRTH: TdxNullableValue<TDateTime>; inline;
    function GetFIRSTNAME: TdxNullableValue<string>; inline;
    function GetGender: TdxNullableValue<Boolean>; inline;
    function GetHOMEPAGE: TdxNullableValue<string>; inline;
    function GetLOCATIONOFBIRTH: TdxNullableValue<string>; inline;
    function GetNICKNAME: TdxNullableValue<string>; inline;
    function GetSECONDNAME: TdxNullableValue<string>; inline;
  public
    property Name: string read FName write FName;
    property ID: Integer read FID write FID;
    property FilmID: TFilms read FFilmID write FFilmID;
    property PersonID: TPersons read FPersonID write FPersonID;
    property PersonLineID: TPersonLines read FPersonLineID write FPersonLineID;
    [VirtualColumn]
    property BIOGRAPHY: TdxNullableValue<string> read GetBIOGRAPHY;
    [VirtualColumn]
    property BIRTHCOUNTRY: TCountries read GetBIRTHCOUNTRY;
    [VirtualColumn]
    property BIRTHNAME: TdxNullableValue<string> read GetBIRTHNAME;
    [VirtualColumn]
    property DATEOFBIRTH: TdxNullableValue<TDateTime> read GetDATEOFBIRTH;
    [VirtualColumn]
    property FIRSTNAME: TdxNullableValue<string> read GetFIRSTNAME;
    [VirtualColumn]
    property LOCATIONOFBIRTH: TdxNullableValue<string> read GetLOCATIONOFBIRTH;
    [VirtualColumn]
    property NICKNAME: TdxNullableValue<string> read GetNICKNAME;
    [VirtualColumn]
    property SECONDNAME: TdxNullableValue<string> read GetSECONDNAME;
    [VirtualColumn]
    property HOMEPAGE: TdxNullableValue<string> read GetHOMEPAGE;
    [VirtualColumn]
    property Gender: TdxNullableValue<Boolean> read GetGender;
  end;

implementation

{ TFilms }

constructor TFilms.Create;
begin
  inherited Create;
  FFilmPersons := TdxEMFCollections.Create<TFilmsPersons>(Self, 'FFilmPersons');
end;

{ TFilmsGenres }

function TFilmsGenres.GetICON: TBytes;
begin
  if FFILMID = nil then
    Result := nil
  else
    Result := FFILMID.ICON;
end;

function TFilmsGenres.GetPHOTO: TBytes;
begin
  if FFILMID = nil then
    Result := nil
  else
    Result := FFILMID.PHOTO;
end;

{ TFilmsPersons }

function TFilmsPersons.GetBIOGRAPHY: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.BIOGRAPHY;
end;

function TFilmsPersons.GetBIRTHCOUNTRY: TCountries;
begin
  if FPersonID = nil then
    Result := nil
  else
    Result := FPersonID.BIRTHCOUNTRY;
end;

function TFilmsPersons.GetBIRTHNAME: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.BIRTHNAME;
end;

function TFilmsPersons.GetDATEOFBIRTH: TdxNullableValue<TDateTime>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<TDateTime>.Null
  else
    Result := FPersonID.DATEOFBIRTH;
end;

function TFilmsPersons.GetFIRSTNAME: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.FIRSTNAME;
end;

function TFilmsPersons.GetGender: TdxNullableValue<Boolean>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<Boolean>.Null
  else
    Result := FPersonID.Gender;
end;

function TFilmsPersons.GetHOMEPAGE: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.HOMEPAGE;
end;

function TFilmsPersons.GetLOCATIONOFBIRTH: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.LOCATIONOFBIRTH;
end;

function TFilmsPersons.GetNICKNAME: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.NICKNAME;
end;

function TFilmsPersons.GetSECONDNAME: TdxNullableValue<string>;
begin
  if FPersonID = nil then
    Result := TdxNullableValue<string>.Null
  else
    Result := FPersonID.SECONDNAME;
end;

{ TGenres }

constructor TGenres.Create;
begin
  inherited Create;
  FFilmsGenres := TdxEMFCollections.Create<TFilmsGenres>(Self, 'FFilmsGenres');
end;

{ TFilmsCompanies }

function TFilmsCompanies.GetCompanyName: string;
begin
  if FCompanyID = nil then
    Result := ''
  else
    Result := FCompanyID.COMPANYNAME;
end;

initialization
  EntityManager.RegisterEntities([
    TCompanies,
    TCompanyTypes,
    TCountries,
    TFilms,
    TFilmsCompanies,
    TFilmsGenres,
    TFilmsPersons,
    TFilmsScreens,
    TGenres,
    TPersonLines,
    TPersons]);

finalization
  EntityManager.UnRegisterEntities([
    TCompanies,
    TCompanyTypes,
    TCountries,
    TFilms,
    TFilmsCompanies,
    TFilmsGenres,
    TFilmsPersons,
    TFilmsScreens,
    TGenres,
    TPersonLines,
    TPersons]);
end.
