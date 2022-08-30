unit Films.Linq;

interface

uses
  dxEMF.Linq,
  dxEMF.Linq.Expressions,
  Films.Entities;

type
  ICompaniesExpression = interface;
  ICompanyTypesExpression = interface;
  ICompaniesCollectionExpression = interface;
  ICountriesExpression = interface;
  IPersonsCollectionExpression = interface;
  IFilmsExpression = interface;
  IFilmsCompaniesExpression = interface;
  IFilmsGenresExpression = interface;
  IFilmsPersonsExpression = interface;
  IFilmsScreensExpression = interface;
  IGenresExpression = interface;
  IPersonLinesExpression = interface;
  IPersonsExpression = interface;

  ICompaniesExpression = interface(IdxEntityInfo)
  ['{BB87AEDA-896F-47B9-8557-437F3DBE0577}']
    function ID: TdxLinqExpression;
    function COMPANYTYPEID: ICompanyTypesExpression;
    function COUNTRYID: ICountriesExpression;
    function COMPANYNAME: TdxLinqExpression;
    function COMPANYWEBSITE: TdxLinqExpression;
  end;

  ICompanyTypesExpression = interface(IdxEntityInfo)
  ['{5E8F4C96-AD0B-4EE6-B72F-6C81652A4D99}']
    function ID: TdxLinqExpression;
    function NAME: TdxLinqExpression;
    function Companies_Collection: ICompaniesCollectionExpression;
  end;

  ICountriesExpression = interface(IdxEntityInfo)
  ['{4CC69D7D-4A52-4DB4-871C-1E94FBE7E7CF}']
    function ID: TdxLinqExpression;
    function NAME: TdxLinqExpression;
    function ACRONYM: TdxLinqExpression;
    function NATIONALFLAG: TdxLinqExpression;
    function Persons_Collection: IPersonsCollectionExpression;
    function Companies_Collection: ICompaniesCollectionExpression;
  end;

  IFilmsExpression = interface(IdxEntityInfo)
  ['{8CC590E8-4972-4EB4-BE7E-B1565930800D}']
    function ID: TdxLinqExpression;
    function CAPTION: TdxLinqExpression;
    function YEAR: TdxLinqExpression;
    function TAGLINE: TdxLinqExpression;
    function PLOTOUTLINE: TdxLinqExpression;
    function RUNTIME: TdxLinqExpression;
    function COLOR: TdxLinqExpression;
    function WEBSITE: TdxLinqExpression;
    function PHOTO: TdxLinqExpression;
    function ICON: TdxLinqExpression;
  end;

  IFilmsCompaniesExpression = interface(IdxEntityInfo)
  ['{F15C7AA1-E015-4AC0-BEB6-0A10D0B55564}']
    function ID: TdxLinqExpression;
    function FilmID: TdxLinqExpression;
    function CompanyID: TdxLinqExpression;
  end;

  IFilmsGenresExpression = interface(IdxEntityInfo)
  ['{7749DD82-C800-4901-909E-2BCE3C6AF1D4}']
    function ID: TdxLinqExpression;
    function FILMID: TdxLinqExpression;
    function GENREID: TdxLinqExpression;
  end;

  IFilmsPersonsExpression = interface(IdxEntityInfo)
  ['{D16EBE0F-6BE3-4555-82CC-4EB8B7BF03D6}']
    function Name: TdxLinqExpression;
    function ID: TdxLinqExpression;
    function FilmID: TdxLinqExpression;
    function PersonID: TdxLinqExpression;
    function PersonLineID: TdxLinqExpression;
  end;

  IFilmsScreensExpression = interface(IdxEntityInfo)
  ['{130C4442-0B2E-4BB3-8642-C38763C10DF9}']
    function ID: TdxLinqExpression;
    function FILMID: TdxLinqExpression;
    function SCREEN: TdxLinqExpression;
    function ICON: TdxLinqExpression;
  end;

  IGenresExpression = interface(IdxEntityInfo)
  ['{5E0095F3-CCAC-4D00-9970-1B46B3049262}']
    function ID: TdxLinqExpression;
    function NAME: TdxLinqExpression;
  end;

  IPersonLinesExpression = interface(IdxEntityInfo)
  ['{63253739-635E-4663-B2CF-2A3B4A9FDE26}']
    function ID: TdxLinqExpression;
    function NAME: TdxLinqExpression;
  end;

  IPersonsExpression = interface(IdxEntityInfo)
  ['{6BA2362B-0BF1-4F03-AB8E-CE30F30082D0}']
    function ID: TdxLinqExpression;
    function FIRSTNAME: TdxLinqExpression;
    function SECONDNAME: TdxLinqExpression;
    function GENDER: TdxLinqExpression;
    function BIRTHNAME: TdxLinqExpression;
    function DATEOFBIRTH: TdxLinqExpression;
    function BIRTHCOUNTRY: ICountriesExpression;
    function LOCATIONOFBIRTH: TdxLinqExpression;
    function BIOGRAPHY: TdxLinqExpression;
    function NICKNAME: TdxLinqExpression;
    function HOMEPAGE: TdxLinqExpression;
  end;

  ICompaniesCollectionExpression = interface(IdxLinqCollectionExpression<ICompaniesExpression>)
  ['{FFAB00A5-DE80-4CF5-A9B3-1587F54B92DD}']
  end;

  IPersonsCollectionExpression = interface(IdxLinqCollectionExpression<IPersonsExpression>)
  ['{BF4A22EC-CCB1-4C1E-886A-21D77E3C375B}']
  end;

  IDataModel1Context = interface(IdxDataContext)
  ['{C4AD6BB5-67C9-4616-99CE-ED57BC1B251E}']
    function Companies: ICompaniesExpression;
    function CompanyTypes: ICompanyTypesExpression;
    function Countries: ICountriesExpression;
    function Films: IFilmsExpression;
    function FilmsCompanies: IFilmsCompaniesExpression;
    function FilmsGenres: IFilmsGenresExpression;
    function FilmsPersons: IFilmsPersonsExpression;
    function FilmsScreens: IFilmsScreensExpression;
    function Genres: IGenresExpression;
    function PersonLines: IPersonLinesExpression;
    function Persons: IPersonsExpression;
  end;

implementation

initialization
  TdxLinqExpressionFactory.Register<TCompanies, ICompaniesExpression>;
  TdxLinqExpressionFactory.Register<TCompanyTypes, ICompanyTypesExpression>;
  TdxLinqExpressionFactory.Register<TCountries, ICountriesExpression>;
  TdxLinqExpressionFactory.Register<TFilms, IFilmsExpression>;
  TdxLinqExpressionFactory.Register<TFilmsCompanies, IFilmsCompaniesExpression>;
  TdxLinqExpressionFactory.Register<TFilmsGenres, IFilmsGenresExpression>;
  TdxLinqExpressionFactory.Register<TFilmsPersons, IFilmsPersonsExpression>;
  TdxLinqExpressionFactory.Register<TFilmsScreens, IFilmsScreensExpression>;
  TdxLinqExpressionFactory.Register<TGenres, IGenresExpression>;
  TdxLinqExpressionFactory.Register<TPersonLines, IPersonLinesExpression>;
  TdxLinqExpressionFactory.Register<TPersons, IPersonsExpression>;

  TdxLinqExpressionFactory.Register<IDataModel1Context>;

finalization
  TdxLinqExpressionFactory.UnRegister([TCompanies, TCompanyTypes, TCountries, TFilms, TFilmsCompanies,
    TFilmsGenres, TFilmsPersons, TFilmsScreens, TGenres, TPersonLines, TPersons]);
  TdxLinqExpressionFactory.UnRegister<IDataModel1Context>;
end.
