object DMRealtorWorld: TDMRealtorWorld
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 712
  Top = 449
  Height = 319
  Width = 933
  object dsHomePhotos: TDataSource
    DataSet = clHomePhotos
    Left = 40
    Top = 16
  end
  object dsHomesAndAgents: TDataSource
    DataSet = clHomesAndAgents
    Left = 152
    Top = 16
  end
  object dsHouseSales: TDataSource
    DataSet = clHousesSales
    Left = 376
    Top = 16
  end
  object dsMortgage: TDataSource
    DataSet = clMortgage
    Left = 488
    Top = 16
  end
  object dsHomesAndHomes: TDataSource
    DataSet = clHomesAndHomes
    Left = 264
    Top = 16
  end
  object clMortgage: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 488
    Top = 72
    object clMortgageDate1: TDateTimeField
      FieldName = 'Date1'
    end
    object clMortgageFRM30: TFloatField
      FieldName = 'FRM30'
    end
    object clMortgageFRM15: TFloatField
      FieldName = 'FRM15'
    end
    object clMortgageARM1: TFloatField
      FieldName = 'ARM1'
    end
  end
  object clHousesSales: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 376
    Top = 72
    object clHousesSalesID: TIntegerField
      FieldName = 'ID'
    end
    object clHousesSalesDate1: TDateTimeField
      FieldName = 'Date1'
    end
    object clHousesSalesCount1: TIntegerField
      FieldName = 'Count1'
    end
    object clHousesSalesRegion: TMemoField
      FieldName = 'Region'
      BlobType = ftMemo
    end
    object clHousesSalesSeasonallyAdjusted: TMemoField
      FieldName = 'SeasonallyAdjusted'
      BlobType = ftMemo
    end
    object clHousesSalesType1: TMemoField
      FieldName = 'Type1'
      BlobType = ftMemo
    end
  end
  object clHomesAndHomes: TClientDataSet
    Aggregates = <>
    Params = <>
    OnCalcFields = clHomesAndHomesCalcFields
    Left = 264
    Top = 72
    object clHomesAndHomesID: TIntegerField
      FieldName = 'ID'
    end
    object clHomesAndHomesAddress: TMemoField
      FieldName = 'Address'
      BlobType = ftMemo
    end
    object clHomesAndHomesBeds: TSmallintField
      FieldName = 'Beds'
    end
    object clHomesAndHomesBaths: TSmallintField
      FieldName = 'Baths'
    end
    object clHomesAndHomesHouseSize: TFloatField
      FieldName = 'HouseSize'
      DisplayFormat = '#.00 Sq Ft'
    end
    object clHomesAndHomesLotSize: TFloatField
      FieldName = 'LotSize'
    end
    object clHomesAndHomesPrice: TFloatField
      FieldName = 'Price'
    end
    object clHomesAndHomesFeatures: TMemoField
      FieldName = 'Features'
      BlobType = ftMemo
    end
    object clHomesAndHomesYearBuilt: TMemoField
      FieldName = 'YearBuilt'
      BlobType = ftMemo
    end
    object clHomesAndHomesType: TIntegerField
      FieldName = 'Type'
    end
    object clHomesAndHomesStatus: TIntegerField
      FieldName = 'Status'
    end
    object clHomesAndHomesPhoto: TBlobField
      FieldName = 'Photo'
    end
    object clHomesAndHomesAgentID: TIntegerField
      FieldKind = fkInternalCalc
      FieldName = 'AgentID'
      Calculated = True
    end
    object clHomesAndHomesYearID: TIntegerField
      FieldKind = fkInternalCalc
      FieldName = 'YearID'
      Calculated = True
    end
  end
  object clHomesAndAgents: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ID'
    Params = <>
    Left = 152
    Top = 72
  end
  object clHomePhotos: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ParentID; ID'
    Params = <>
    Left = 40
    Top = 72
  end
  object dspHomesAndHomes: TDataSetProvider
    DataSet = clHomesAndHomes
    Left = 264
    Top = 136
  end
  object dsHomesDetail: TDataSource
    DataSet = clHomesDetail
    Left = 248
    Top = 192
  end
  object clHomesDetail: TClientDataSet
    Aggregates = <>
    Filter = 'AgentID=1'
    Filtered = True
    FieldDefs = <
      item
        Name = 'ID'
        DataType = ftInteger
      end
      item
        Name = 'Address'
        DataType = ftMemo
      end
      item
        Name = 'Beds'
        DataType = ftSmallint
      end
      item
        Name = 'Baths'
        DataType = ftSmallint
      end
      item
        Name = 'HouseSize'
        DataType = ftFloat
      end
      item
        Name = 'LotSize'
        DataType = ftFloat
      end
      item
        Name = 'Price'
        DataType = ftFloat
      end
      item
        Name = 'Features'
        DataType = ftMemo
      end
      item
        Name = 'YearBuilt'
        DataType = ftMemo
      end
      item
        Name = 'Type'
        DataType = ftInteger
      end
      item
        Name = 'Status'
        DataType = ftInteger
      end
      item
        Name = 'Photo'
        DataType = ftBlob
      end
      item
        Name = 'AgentID'
        Attributes = [faReadonly]
        DataType = ftInteger
      end
      item
        Name = 'YearID'
        Attributes = [faReadonly]
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'YearID'
    Params = <>
    ProviderName = 'dspHomesAndHomes'
    StoreDefs = True
    Left = 344
    Top = 192
    object clHomesDetailID: TIntegerField
      FieldName = 'ID'
    end
    object clHomesDetailAddress: TMemoField
      FieldName = 'Address'
      BlobType = ftMemo
    end
    object clHomesDetailBeds: TSmallintField
      FieldName = 'Beds'
    end
    object clHomesDetailBaths: TSmallintField
      FieldName = 'Baths'
    end
    object clHomesDetailHouseSize: TFloatField
      FieldName = 'HouseSize'
      DisplayFormat = '0,000.00 Sq Ft '
    end
    object clHomesDetailLotSize: TFloatField
      FieldName = 'LotSize'
    end
    object clHomesDetailPrice: TFloatField
      FieldName = 'Price'
    end
    object clHomesDetailFeatures: TMemoField
      FieldName = 'Features'
      BlobType = ftMemo
    end
    object clHomesDetailYearBuilt: TMemoField
      FieldName = 'YearBuilt'
      BlobType = ftMemo
    end
    object clHomesDetailType: TIntegerField
      FieldName = 'Type'
    end
    object clHomesDetailStatus: TIntegerField
      FieldName = 'Status'
    end
    object clHomesDetailPhoto: TBlobField
      FieldName = 'Photo'
    end
    object clHomesDetailAgentID: TIntegerField
      FieldName = 'AgentID'
      ReadOnly = True
    end
    object clHomesDetailYearID: TIntegerField
      FieldName = 'YearID'
      ReadOnly = True
    end
  end
  object dsHouseSalsesChart: TDataSource
    DataSet = clHouseSalesChart
    Left = 24
    Top = 176
  end
  object clHouseSalesChart: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 104
    Top = 192
    object clHouseSalesChartDate: TDateField
      FieldName = 'Date'
    end
    object clHouseSalesChartState: TStringField
      FieldName = 'State'
    end
    object clHouseSalesChartSeasonallyAdjusted: TStringField
      FieldName = 'SeasonallyAdjusted'
    end
    object clHouseSalesChartMidWest: TIntegerField
      FieldName = 'Mid-West'
    end
    object clHouseSalesChartNorthEast: TIntegerField
      FieldName = 'North-East'
    end
    object clHouseSalesChartSouth: TIntegerField
      FieldName = 'South'
    end
    object clHouseSalesChartWest: TIntegerField
      FieldName = 'West'
    end
    object clHouseSalesChartAll: TIntegerField
      FieldName = 'All'
    end
  end
  object dsHouseRating: TDataSource
    DataSet = clHouseRating
    Left = 600
    Top = 104
  end
  object dsHousesSimular: TDataSource
    DataSet = clHousesSimular
    Left = 704
    Top = 104
  end
  object clHousesSimular: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 704
    Top = 152
    object IntegerField1: TIntegerField
      FieldName = 'HouseID'
    end
    object clHousesSimularYear: TIntegerField
      FieldName = 'Year'
    end
    object clHousesSimularProposals: TIntegerField
      FieldName = 'Proposals'
    end
    object clHousesSimularSold: TIntegerField
      FieldName = 'Sold'
    end
  end
  object dsHousePrice: TDataSource
    DataSet = clHousePrice
    Left = 800
    Top = 104
  end
  object clHousePrice: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 800
    Top = 152
    object IntegerField2: TIntegerField
      FieldName = 'HouseID'
    end
    object clHousePriceDate: TDateField
      FieldName = 'Date'
    end
    object clHousePricePrice: TFloatField
      FieldName = 'Price'
    end
  end
  object clHouseRating: TClientDataSet
    Aggregates = <>
    Filtered = True
    Params = <>
    Left = 600
    Top = 160
    object clHouseRatingHouseID: TIntegerField
      FieldName = 'HouseID'
    end
    object clHouseRatingRegionName: TStringField
      FieldName = 'RegionName'
      Size = 15
    end
    object clHouseRatingValue: TIntegerField
      FieldName = 'Value'
    end
  end
  object dsResearchChart: TDataSource
    DataSet = clResearchChart
    Left = 496
    Top = 216
  end
  object clResearchChart: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 584
    Top = 216
    object clResearchChartDate: TDateField
      FieldName = 'Date'
    end
    object clResearchChartState: TStringField
      FieldName = 'State'
    end
    object clResearchChartSeasonallyAdjusted: TStringField
      FieldName = 'SeasonallyAdjusted'
    end
    object clResearchChartMidWest: TIntegerField
      FieldName = 'Mid-West'
    end
    object clResearchChartNorthEast: TIntegerField
      FieldName = 'North-East'
    end
    object clResearchChartSouth: TIntegerField
      FieldName = 'South'
    end
    object clResearchChartWest: TIntegerField
      FieldName = 'West'
    end
    object clResearchChartAll: TIntegerField
      FieldName = 'All'
    end
  end
end
