object FilmsDemoDM: TFilmsDemoDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 479
  Width = 741
  object dsGenres: TDataSource
    DataSet = cdsGenres
    Left = 528
    Top = 304
  end
  object cdsFilmsGenres: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'FILMID'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'GENREID'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'PHOTO'
        Attributes = [faUnNamed]
        DataType = ftBlob
      end
      item
        Name = 'ICON'
        Attributes = [faUnNamed]
        DataType = ftBlob
      end>
    IndexDefs = <>
    IndexFieldNames = 'GENREID'
    MasterFields = 'ID'
    MasterSource = dsGenres
    Params = <>
    StoreDefs = True
    AfterPost = cdsFilmsGenresAfterPost
    AfterDelete = cdsFilmsGenresAfterDelete
    Left = 424
    Top = 56
    object cdsFilmsGenresID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsFilmsGenresFILMID: TIntegerField
      FieldName = 'FILMID'
    end
    object cdsFilmsGenresGENREID: TIntegerField
      FieldName = 'GENREID'
    end
    object cdsFilmsGenresPHOTO: TBlobField
      FieldName = 'PHOTO'
    end
    object cdsFilmsGenresICON: TBlobField
      FieldName = 'ICON'
    end
    object cdsFilmsGenresCaption: TStringField
      FieldKind = fkLookup
      FieldName = 'Caption'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'CAPTION'
      KeyFields = 'FILMID'
      Lookup = True
    end
    object cdsFilmsGenresYear: TIntegerField
      FieldKind = fkLookup
      FieldName = 'Year'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'YEAR'
      KeyFields = 'FILMID'
      Lookup = True
    end
    object cdsFilmsGenresTAGLINE: TStringField
      FieldKind = fkLookup
      FieldName = 'TAGLINE'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'TAGLINE'
      KeyFields = 'FILMID'
      Lookup = True
    end
    object cdsFilmsGenresPLOTOUTLINE: TStringField
      FieldKind = fkLookup
      FieldName = 'PLOTOUTLINE'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'PLOTOUTLINE'
      KeyFields = 'FILMID'
      Size = 200
      Lookup = True
    end
    object cdsFilmsGenresRunTime: TIntegerField
      FieldKind = fkLookup
      FieldName = 'RunTime'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'RUNTIME'
      KeyFields = 'FILMID'
      Lookup = True
    end
    object cdsFilmsGenresWebsite: TStringField
      FieldKind = fkLookup
      FieldName = 'Website'
      LookupDataSet = cdsFilms
      LookupKeyFields = 'ID'
      LookupResultField = 'WEBSITE'
      KeyFields = 'FILMID'
      Lookup = True
    end
  end
  object dsFilms: TDataSource
    DataSet = cdsFilms
    Left = 384
    Top = 304
  end
  object cdsFilms: TClientDataSet
    Aggregates = <>
    Filtered = True
    IndexFieldNames = 'ID'
    Params = <>
    BeforePost = cdsFilmsBeforePost
    AfterPost = cdsFilmsAfterPost
    BeforeDelete = cdsFilmsBeforeDelete
    OnFilterRecord = cdsFilmsFilterRecord
    Left = 384
    Top = 248
    object cdsFilmsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsFilmsCAPTION: TStringField
      FieldName = 'CAPTION'
      Size = 50
    end
    object cdsFilmsYEAR: TIntegerField
      FieldName = 'YEAR'
    end
    object cdsFilmsTAGLINE: TStringField
      FieldName = 'TAGLINE'
      Size = 250
    end
    object cdsFilmsPLOTOUTLINE: TStringField
      DisplayWidth = 50
      FieldName = 'PLOTOUTLINE'
      Size = 200
    end
    object cdsFilmsRUNTIME: TIntegerField
      FieldName = 'RUNTIME'
    end
    object cdsFilmsCOLOR: TStringField
      FieldName = 'COLOR'
      Size = 50
    end
    object cdsFilmsPHOTO: TBlobField
      FieldName = 'PHOTO'
      Size = 10
    end
    object cdsFilmsICON: TBlobField
      FieldName = 'ICON'
      Size = 10
    end
    object cdsFilmsWEBSITE: TStringField
      FieldName = 'WEBSITE'
      Size = 50
    end
  end
  object dsPersons: TDataSource
    DataSet = cdsPersons
    Left = 24
    Top = 304
  end
  object dsCompanies: TDataSource
    DataSet = cdsCompanies
    Left = 112
    Top = 304
  end
  object dsCountries: TDataSource
    DataSet = cdsCountries
    Left = 200
    Top = 304
  end
  object dsCompanyTypes: TDataSource
    DataSet = cdsCompanyTypes
    Left = 296
    Top = 304
  end
  object dsFilmsPersons: TDataSource
    DataSet = cdsFilmsPersons
    Left = 160
    Top = 8
  end
  object dsFilmsCompanies: TDataSource
    DataSet = cdsFilmsCompanies
    Left = 248
    Top = 8
  end
  object dsFilmsScreens: TDataSource
    DataSet = cdsFilmsScreens
    Left = 336
    Top = 8
  end
  object cdsFilmsPersons: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'FilmID'
    Params = <>
    Left = 160
    Top = 56
    object cdsFilmsPersonsName: TStringField
      FieldName = 'Name'
      ReadOnly = True
    end
    object cdsFilmsPersonsID: TIntegerField
      FieldName = 'ID'
    end
    object cdsFilmsPersonsFilmID: TIntegerField
      FieldName = 'FilmID'
    end
    object cdsFilmsPersonsPersonID: TIntegerField
      FieldName = 'PersonID'
    end
    object cdsFilmsPersonsPersonLineID: TIntegerField
      FieldName = 'PersonLineID'
    end
    object cdsFilmsPersonsBIOGRAPHY: TMemoField
      FieldName = 'BIOGRAPHY'
      BlobType = ftMemo
      Size = 10
    end
    object cdsFilmsPersonsBIRTHCOUNTRY: TIntegerField
      FieldName = 'BIRTHCOUNTRY'
    end
    object cdsFilmsPersonsBIRTHNAME: TStringField
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object cdsFilmsPersonsDATEOFBIRTH: TDateTimeField
      FieldName = 'DATEOFBIRTH'
    end
    object cdsFilmsPersonsFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object cdsFilmsPersonsLOCATIONOFBIRTH: TStringField
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object cdsFilmsPersonsNICKNAME: TStringField
      FieldName = 'NICKNAME'
      Size = 50
    end
    object cdsFilmsPersonsSECONDNAME: TStringField
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object cdsFilmsPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object cdsFilmsPersonsGender: TBooleanField
      FieldName = 'Gender'
    end
  end
  object cdsFilmsCompanies: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'FilmID'
    Params = <>
    Left = 248
    Top = 56
    object cdsFilmsCompaniesID: TIntegerField
      AutoGenerateValue = arAutoInc
      FieldName = 'ID'
    end
    object cdsFilmsCompaniesFilmID: TIntegerField
      FieldName = 'FilmID'
    end
    object cdsFilmsCompaniesCompanyID: TIntegerField
      FieldName = 'CompanyID'
    end
    object cdsFilmsCompaniesCompanyName: TStringField
      FieldName = 'CompanyName'
      Size = 50
    end
    object cdsFilmsCompaniesTypeID: TIntegerField
      FieldKind = fkLookup
      FieldName = 'TypeID'
      LookupDataSet = cdsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COMPANYTYPEID'
      KeyFields = 'CompanyID'
      Lookup = True
    end
    object cdsFilmsCompaniesType: TStringField
      FieldKind = fkLookup
      FieldName = 'Type'
      LookupDataSet = cdsCompanyTypes
      LookupKeyFields = 'ID'
      LookupResultField = 'NAME'
      KeyFields = 'TypeID'
      Lookup = True
    end
    object cdsFilmsCompaniesWebSite: TStringField
      FieldKind = fkLookup
      FieldName = 'WebSite'
      LookupDataSet = cdsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COMPANYWEBSITE'
      KeyFields = 'CompanyID'
      Lookup = True
    end
    object cdsFilmsCompaniesCountryID: TIntegerField
      FieldKind = fkLookup
      FieldName = 'CountryID'
      LookupDataSet = cdsCompanies
      LookupKeyFields = 'ID'
      LookupResultField = 'COUNTRYID'
      KeyFields = 'CompanyID'
      Lookup = True
    end
    object cdsFilmsCompaniesCountry: TStringField
      FieldKind = fkLookup
      FieldName = 'Country'
      LookupDataSet = cdsCountries
      LookupKeyFields = 'ID'
      LookupResultField = 'NAME'
      KeyFields = 'CountryID'
      Lookup = True
    end
  end
  object cdsFilmsScreens: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'FilmID'
    Params = <>
    Left = 336
    Top = 56
    object cdsFilmsScreensID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsFilmsScreensFILMID: TIntegerField
      FieldName = 'FILMID'
    end
    object cdsFilmsScreensSCREEN: TBlobField
      FieldName = 'SCREEN'
      BlobType = ftParadoxOle
      Size = 10
    end
    object cdsFilmsScreensICON: TBlobField
      FieldName = 'ICON'
      BlobType = ftParadoxOle
      Size = 10
    end
  end
  object cdsCompanyTypes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 296
    Top = 248
    object cdsCompanyTypesID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsCompanyTypesNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object cdsCountries: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 200
    Top = 248
  end
  object cdsCompanies: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 248
    object cdsCompaniesID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object cdsCompaniesCOMPANYTYPEID: TIntegerField
      FieldName = 'COMPANYTYPEID'
    end
    object cdsCompaniesCOUNTRYID: TIntegerField
      FieldName = 'COUNTRYID'
    end
    object cdsCompaniesCOMPANYNAME: TStringField
      FieldName = 'COMPANYNAME'
      Size = 50
    end
    object cdsCompaniesCOMPANYWEBSITE: TStringField
      FieldName = 'COMPANYWEBSITE'
      Size = 50
    end
  end
  object cdsPersons: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'FIRSTNAME'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end
      item
        Name = 'SECONDNAME'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end
      item
        Name = 'GENDER'
        Attributes = [faUnNamed]
        DataType = ftBoolean
      end
      item
        Name = 'BIRTHNAME'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end
      item
        Name = 'DATEOFBIRTH'
        Attributes = [faUnNamed]
        DataType = ftDateTime
      end
      item
        Name = 'BIRTHCOUNTRY'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'LOCATIONOFBIRTH'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end
      item
        Name = 'BIOGRAPHY'
        Attributes = [faUnNamed]
        DataType = ftMemo
        Size = 10
      end
      item
        Name = 'NICKNAME'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end
      item
        Name = 'HOMEPAGE'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 100
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = cdsPersonsCalcFields
    Left = 24
    Top = 248
    object cdsPersonsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsPersonsFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object cdsPersonsSECONDNAME: TStringField
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object cdsPersonsGENDER: TBooleanField
      FieldName = 'GENDER'
    end
    object cdsPersonsBIRTHNAME: TStringField
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object cdsPersonsDATEOFBIRTH: TDateTimeField
      FieldName = 'DATEOFBIRTH'
    end
    object cdsPersonsBIRTHCOUNTRY: TIntegerField
      FieldName = 'BIRTHCOUNTRY'
    end
    object cdsPersonsLOCATIONOFBIRTH: TStringField
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object cdsPersonsBIOGRAPHY: TMemoField
      FieldName = 'BIOGRAPHY'
      BlobType = ftMemo
      Size = 10
    end
    object cdsPersonsNICKNAME: TStringField
      FieldName = 'NICKNAME'
      Size = 50
    end
    object cdsPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object cdsPersonsName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Name'
      Calculated = True
    end
  end
  object cdsGenres: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'NAME'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 50
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterScroll = cdsGenresAfterScroll
    Left = 528
    Top = 248
    object cdsGenresID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsGenresNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object cdsPersonLines: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 456
    Top = 248
    object cdsPersonLinesID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object cdsPersonLinesNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object dsPersonLines: TDataSource
    DataSet = cdsPersonLines
    Left = 456
    Top = 304
  end
  object dsFilmsGenres: TDataSource
    DataSet = cdsFilmsGenres
    Left = 424
    Top = 8
  end
end
