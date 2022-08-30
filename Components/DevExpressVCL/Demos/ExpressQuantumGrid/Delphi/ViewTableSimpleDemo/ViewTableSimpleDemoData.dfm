object ViewTableSimpleDemoMainDM: TViewTableSimpleDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 529
  Top = 399
  Height = 479
  Width = 741
  object dsGENRES: TDataSource
    DataSet = cdsGenres
    Left = 32
    Top = 64
  end
  object dsFilms: TDataSource
    DataSet = cdsFilms
    Left = 32
    Top = 112
  end
  object cdsGenres: TClientDataSet
    Aggregates = <>
    Params = <>
    AfterScroll = cdsGenresAfterScroll
    Left = 152
    Top = 60
    object cdsGenresID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsGenresNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
  end
  object cdsFilms: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    BeforePost = cdsFilmsBeforePost
    AfterPost = cdsFilmsAfterPost
    Left = 152
    Top = 120
  end
  object cdsFilmsGenres: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'GENREID'
    MasterFields = 'ID'
    MasterSource = dsGENRES
    PacketRecords = 0
    Params = <>
    Left = 152
    Top = 188
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
  end
end
