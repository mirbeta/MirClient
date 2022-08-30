object FilmsDemoDM: TFilmsDemoDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 479
  Width = 662
  object dsGenres: TDataSource
    DataSet = edsGenres
    Left = 528
    Top = 288
  end
  object dsFilms: TDataSource
    DataSet = edsFilms
    Left = 384
    Top = 288
  end
  object dsPersons: TDataSource
    DataSet = edsPersons
    Left = 24
    Top = 288
  end
  object dsCompanies: TDataSource
    DataSet = edsCompanies
    Left = 112
    Top = 288
  end
  object dsCountries: TDataSource
    DataSet = edsCountries
    Left = 200
    Top = 288
  end
  object dsCompanyTypes: TDataSource
    DataSet = edsCompanyTypes
    Left = 296
    Top = 288
  end
  object dsFilmsPersons: TDataSource
    DataSet = edsFilmsPersons
    Left = 24
    Top = 88
  end
  object dsFilmsCompanies: TDataSource
    DataSet = edsFilmsCompanies
    Left = 112
    Top = 88
  end
  object dsFilmsScreens: TDataSource
    DataSet = edsFilmsScreens
    Left = 200
    Top = 88
  end
  object dsPersonLines: TDataSource
    DataSet = edsPersonLines
    Left = 456
    Top = 288
  end
  object dsFilmsGenres: TDataSource
    DataSet = edsFilmsGenres
    Left = 288
    Top = 88
  end
  object FireDACDataProvider: TdxEMFFireDACDataProvider
    Options.DBEngine = 'SQLite'
    Left = 112
    Top = 16
  end
  object Session: TdxEMFSession
    DataProvider = FireDACDataProvider
    Left = 32
    Top = 16
  end
  object edsFilms: TdxEMFDataSet
    EntityName = 'TFilms'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    BeforePost = edsFilmsBeforePost
    AfterPost = edsFilmsAfterPost
    BeforeDelete = edsFilmsBeforeDelete
    OnFilterRecord = edsFilmsFilterRecord
    Left = 384
    Top = 232
    object edsFilmsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object edsFilmsCAPTION: TWideStringField
      FieldName = 'CAPTION'
      Size = 50
    end
    object edsFilmsYEAR: TIntegerField
      FieldName = 'YEAR'
    end
    object edsFilmsTAGLINE: TWideStringField
      FieldName = 'TAGLINE'
      Size = 250
    end
    object edsFilmsPLOTOUTLINE: TWideStringField
      DisplayWidth = 50
      FieldName = 'PLOTOUTLINE'
      Size = 200
    end
    object edsFilmsRUNTIME: TIntegerField
      FieldName = 'RUNTIME'
    end
    object edsFilmsCOLOR: TWideStringField
      FieldName = 'COLOR'
      Size = 50
    end
    object edsFilmsPHOTO: TBlobField
      FieldName = 'PHOTO'
      Size = 10
    end
    object edsFilmsICON: TBlobField
      FieldName = 'ICON'
      Size = 10
    end
    object edsFilmsWEBSITE: TWideStringField
      FieldName = 'WEBSITE'
      Size = 50
    end
    object edsFilmsFilmPersons: TDataSetField
      FieldName = 'FilmPersons'
      ReadOnly = True
    end
  end
  object edsFilmsPersons: TdxEMFDataSet
    EntityName = 'TFilmsPersons'
    DataContext = dxEMFDataContext1
    Session = Session
    SortByExpressionDefinitions = <
      item
        ExpressionText = 'FilmID'
      end>
    Params = <>
    Left = 24
    Top = 144
    object edsFilmsPersonsName: TWideStringField
      FieldName = 'Name'
    end
    object edsFilmsPersonsID: TAutoIncField
      FieldName = 'ID'
    end
    object edsFilmsPersonsFilmID: TdxEntityField
      FieldName = 'FilmID'
    end
    object edsFilmsPersonsPersonID: TdxEntityField
      FieldName = 'PersonID'
    end
    object edsFilmsPersonsPersonLineID: TdxEntityField
      FieldName = 'PersonLineID'
    end
    object edsFilmsPersonsBIOGRAPHY: TWideMemoField
      FieldKind = fkInternalCalc
      FieldName = 'BIOGRAPHY'
      BlobType = ftWideString
      Size = 10
    end
    object edsFilmsPersonsBIRTHCOUNTRY: TdxEntityField
      FieldKind = fkInternalCalc
      FieldName = 'BIRTHCOUNTRY'
    end
    object edsFilmsPersonsBIRTHNAME: TWideStringField
      DisplayLabel = 'BIRTHNAM'
      FieldKind = fkInternalCalc
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object edsFilmsPersonsDATEOFBIRTH: TDateTimeField
      FieldKind = fkInternalCalc
      FieldName = 'DATEOFBIRTH'
    end
    object edsFilmsPersonsFIRSTNAME: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object edsFilmsPersonsLOCATIONOFBIRTH: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object edsFilmsPersonsNICKNAME: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'NICKNAME'
      Size = 50
    end
    object edsFilmsPersonsSECONDNAME: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object edsFilmsPersonsHOMEPAGE: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object edsFilmsPersonsGender: TBooleanField
      FieldKind = fkInternalCalc
      FieldName = 'Gender'
    end
  end
  object edsPersonLines: TdxEMFDataSet
    EntityName = 'TPersonLines'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    Left = 456
    Top = 232
    object edsPersonLinesID: TAutoIncField
      FieldName = 'ID'
    end
    object edsPersonLinesNAME: TWideStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object edsFilmsCompanies: TdxEMFDataSet
    EntityName = 'TFilmsCompanies'
    DataContext = dxEMFDataContext1
    Session = Session
    SortByExpressionDefinitions = <
      item
        ExpressionText = 'FilmID'
      end>
    Params = <>
    Left = 112
    Top = 144
    object edsFilmsCompaniesID: TAutoIncField
      FieldName = 'ID'
    end
    object edsFilmsCompaniesFilmID: TdxEntityField
      FieldName = 'FilmID'
    end
    object edsFilmsCompaniesCompanyID: TdxEntityField
      FieldName = 'CompanyID'
    end
    object edsFilmsCompaniesCompanyName: TWideStringField
      FieldKind = fkInternalCalc
      FieldName = 'CompanyName'
      Size = 50
    end
    object edsFilmsCompaniesTypeID: TIntegerField
      FieldKind = fkLookup
      FieldName = 'TypeID'
      LookupDataSet = edsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COMPANYTYPEID'
      KeyFields = 'CompanyID'
      Lookup = True
    end
    object edsFilmsCompaniesType: TWideStringField
      FieldKind = fkLookup
      FieldName = 'Type'
      LookupDataSet = edsCompanyTypes
      LookupKeyFields = 'ID'
      LookupResultField = 'NAME'
      KeyFields = 'TypeID'
      Size = 50
      Lookup = True
    end
    object edsFilmsCompaniesWebSite: TWideStringField
      FieldKind = fkLookup
      FieldName = 'WebSite'
      LookupDataSet = edsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COMPANYWEBSITE'
      KeyFields = 'CompanyID'
      Size = 50
      Lookup = True
    end
    object edsFilmsCompaniesCountryID: TIntegerField
      FieldKind = fkLookup
      FieldName = 'CountryID'
      LookupDataSet = edsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COUNTRYID'
      KeyFields = 'CompanyID'
      Lookup = True
    end
    object edsFilmsCompaniesCountry: TWideStringField
      FieldKind = fkLookup
      FieldName = 'Country'
      LookupDataSet = edsCountries
      LookupKeyFields = 'ID'
      LookupResultField = 'NAME'
      KeyFields = 'CountryID'
      Size = 50
      Lookup = True
    end
  end
  object edsCompanyTypes: TdxEMFDataSet
    EntityName = 'TCompanyTypes'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    Left = 296
    Top = 232
    object edsCompanyTypesID: TAutoIncField
      FieldName = 'ID'
    end
    object edsCompanyTypesNAME: TWideStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object edsCompanies: TdxEMFDataSet
    EntityName = 'TCompanies'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    Left = 112
    Top = 232
    object edsCompaniesID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object edsCompaniesCOMPANYTYPEID: TdxEntityField
      FieldName = 'COMPANYTYPEID'
    end
    object edsCompaniesCOUNTRYID: TdxEntityField
      FieldName = 'COUNTRYID'
    end
    object edsCompaniesCOMPANYNAME: TWideStringField
      FieldName = 'COMPANYNAME'
      Size = 50
    end
    object edsCompaniesCOMPANYWEBSITE: TWideStringField
      FieldName = 'COMPANYWEBSITE'
      Size = 50
    end
  end
  object edsCountries: TdxEMFDataSet
    EntityName = 'TCountries'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    Left = 200
    Top = 232
    object edsCountriesID: TAutoIncField
      FieldName = 'ID'
    end
    object edsCountriesNAME: TWideStringField
      FieldName = 'NAME'
      Size = 60
    end
    object edsCountriesACRONYM: TWideStringField
      FieldName = 'ACRONYM'
      Size = 50
    end
    object edsCountriesNATIONALFLAG: TBlobField
      FieldName = 'NATIONALFLAG'
    end
  end
  object edsPersons: TdxEMFDataSet
    EntityName = 'TPersons'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    OnCalcFields = edsPersonsCalcFields
    Left = 24
    Top = 232
    object edsPersonsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object edsPersonsFIRSTNAME: TWideStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object edsPersonsSECONDNAME: TWideStringField
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object edsPersonsGENDER: TBooleanField
      FieldName = 'GENDER'
    end
    object edsPersonsBIRTHNAME: TWideStringField
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object edsPersonsDATEOFBIRTH: TDateTimeField
      FieldName = 'DATEOFBIRTH'
    end
    object edsPersonsBIRTHCOUNTRY: TdxEntityField
      FieldName = 'BIRTHCOUNTRY'
    end
    object edsPersonsLOCATIONOFBIRTH: TWideStringField
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object edsPersonsBIOGRAPHY: TWideMemoField
      FieldName = 'BIOGRAPHY'
      BlobType = ftMemo
      Size = 10
    end
    object edsPersonsNICKNAME: TWideStringField
      FieldName = 'NICKNAME'
      Size = 50
    end
    object edsPersonsHOMEPAGE: TWideStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object edsPersonsName: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'Name'
      Calculated = True
    end
  end
  object edsGenres: TdxEMFDataSet
    EntityName = 'TGenres'
    DataContext = dxEMFDataContext1
    Session = Session
    Params = <>
    AfterScroll = edsGenresAfterScroll
    Left = 528
    Top = 232
    object edsGenresID: TAutoIncField
      FieldName = 'ID'
    end
    object edsGenresNAME: TWideStringField
      FieldName = 'NAME'
      Size = 50
    end
    object edsGenresFilmsGenres: TDataSetField
      FieldName = 'FilmsGenres'
      ReadOnly = True
    end
  end
  object edsFilmsScreens: TdxEMFDataSet
    EntityName = 'TFilmsScreens'
    DataContext = dxEMFDataContext1
    Session = Session
    SortByExpressionDefinitions = <
      item
        ExpressionText = 'FilmID'
      end>
    Params = <>
    Left = 200
    Top = 144
    object edsFilmsScreensID: TAutoIncField
      FieldName = 'ID'
    end
    object edsFilmsScreensFILMID: TdxEntityField
      FieldName = 'FILMID'
    end
    object edsFilmsScreensSCREEN: TBlobField
      FieldName = 'SCREEN'
      ReadOnly = True
    end
    object edsFilmsScreensICON: TBlobField
      FieldName = 'ICON'
      ReadOnly = True
    end
  end
  object edsFilmsGenres: TdxEMFDataSet
    DataContext = dxEMFDataContext1
    Session = Session
    DataSetField = edsGenresFilmsGenres
    Params = <>
    AfterPost = edsRefreshFilms
    AfterDelete = edsRefreshFilms
    Left = 288
    Top = 144
    object edsFilmsGenresID: TAutoIncField
      FieldName = 'ID'
    end
    object edsFilmsGenresFILMID: TdxEntityField
      FieldName = 'FILMID'
    end
    object edsFilmsGenresGENREID: TdxEntityField
      FieldName = 'GENREID'
    end
    object edsFilmsGenresPHOTO: TBlobField
      FieldKind = fkInternalCalc
      FieldName = 'PHOTO'
    end
    object edsFilmsGenresICON: TBlobField
      FieldKind = fkInternalCalc
      FieldName = 'ICON'
    end
    object edsFilmsGenresCaption: TWideStringField
      FieldKind = fkLookup
      FieldName = 'Caption'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'CAPTION'
      KeyFields = 'FILMID'
      LookupCache = True
      Size = 50
      Lookup = True
    end
    object edsFilmsGenresYear: TIntegerField
      FieldKind = fkLookup
      FieldName = 'Year'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'YEAR'
      KeyFields = 'FILMID'
      LookupCache = True
      Lookup = True
    end
    object edsFilmsGenresTAGLINE: TWideStringField
      FieldKind = fkLookup
      FieldName = 'TAGLINE'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'TAGLINE'
      KeyFields = 'FILMID'
      LookupCache = True
      Size = 50
      Lookup = True
    end
    object edsFilmsGenresPLOTOUTLINE: TWideStringField
      FieldKind = fkLookup
      FieldName = 'PLOTOUTLINE'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'PLOTOUTLINE'
      KeyFields = 'FILMID'
      LookupCache = True
      Size = 200
      Lookup = True
    end
    object edsFilmsGenresRunTime: TIntegerField
      FieldKind = fkLookup
      FieldName = 'RunTime'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'RUNTIME'
      KeyFields = 'FILMID'
      LookupCache = True
      Lookup = True
    end
    object edsFilmsGenresWebsite: TWideStringField
      FieldKind = fkLookup
      FieldName = 'Website'
      LookupDataSet = edsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'WEBSITE'
      KeyFields = 'FILMID'
      LookupCache = True
      Size = 50
      Lookup = True
    end
  end
  object dxEMFDataContext1: TdxEMFDataContext
    PackageName = 'Films'
    Left = 224
    Top = 16
  end
end
