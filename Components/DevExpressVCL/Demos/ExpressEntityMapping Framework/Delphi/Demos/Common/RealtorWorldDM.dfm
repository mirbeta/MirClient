object DMRealtorWorld: TDMRealtorWorld
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 324
  Width = 683
  object dsHomesDetail: TDataSource
    DataSet = edsHomesDetail
    Left = 56
    Top = 152
  end
  object EMFSession: TdxEMFSession
    DataProvider = EMFADODataProvider
    Left = 272
    Top = 16
  end
  object EMFADODataProvider: TdxEMFADODataProvider
    Options.AutoCreate = SchemaOnly
    Options.DBEngine = 'MSAccess'
    Connection = ADOConnection
    Left = 168
    Top = 16
  end
  object ADOConnection: TADOConnection
    ConnectionString = 'Provider=Microsoft.Jet.OLEDB.4.0;Persist Security Info=False'
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 48
    Top = 16
  end
  object edsHomesDetail: TdxEMFDataSet
    EntityName = 'RealtorWorld.Entities.THomes'
    PackageName = 'RealtorWorld'
    Session = EMFSession
    CriteriaText = 'Agent = :AgentID'
    Params = <
      item
        DataType = ftUnknown
        Name = 'AgentID'
        ParamType = ptInput
      end>
    Left = 56
    Top = 96
    object edsHomesDetailID: TIntegerField
      FieldName = 'ID'
      Required = True
    end
    object edsHomesDetailAddress: TWideStringField
      FieldName = 'Address'
      Size = 50
    end
    object edsHomesDetailBeds: TSmallintField
      FieldName = 'Beds'
      Required = True
    end
    object edsHomesDetailBaths: TSmallintField
      FieldName = 'Baths'
      Required = True
    end
    object edsHomesDetailHouseSize: TFloatField
      FieldName = 'HouseSize'
      Required = True
    end
    object edsHomesDetailLotSize: TFloatField
      FieldName = 'LotSize'
      Required = True
    end
    object edsHomesDetailPrice: TFloatField
      FieldName = 'Price'
      Required = True
    end
    object edsHomesDetailFeatures: TWideMemoField
      FieldName = 'Features'
      BlobType = ftWideMemo
      Size = -1
    end
    object edsHomesDetailYearBuilt: TIntegerField
      FieldName = 'YearBuilt'
      Required = True
    end
    object edsHomesDetailType: TIntegerField
      FieldName = 'Type'
      Required = True
    end
    object edsHomesDetailStatus: TIntegerField
      FieldName = 'Status'
      Required = True
    end
    object edsHomesDetailPhoto: TBlobField
      FieldName = 'Photo'
      Size = -1
    end
    object edsHomesDetailAgent: TdxEntityField
      FieldName = 'Agent'
      Required = True
    end
  end
  object edsChart: TdxEMFDataSet
    EntityName = 'RealtorWorld.Entities.TCharts'
    PackageName = 'RealtorWorld'
    Params = <>
    ReadOnly = True
    Filtered = True
    Left = 152
    Top = 96
    object edsChartAgent: TdxEntityField
      FieldName = 'Agent'
      Required = True
    end
    object edsChartNorthEast: TIntegerField
      DisplayLabel = 'North-East'
      FieldName = 'NorthEast'
      Required = True
    end
    object edsChartMidWest: TIntegerField
      DisplayLabel = 'Mid-West'
      FieldName = 'MidWest'
      Required = True
    end
    object edsChartSouth: TIntegerField
      FieldName = 'South'
      Required = True
    end
    object edsChartWest: TIntegerField
      FieldName = 'West'
      Required = True
    end
    object edsChartDate: TIntegerField
      FieldName = 'Date'
      Required = True
    end
  end
end
