object dmCars: TdmCars
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 383
  Width = 410
  object mdModels: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F140000000400000003000300494400040000000300
      0C0054726164656D61726B494400FF000000140005004E616D6500FF00000014
      000D004D6F64696669636174696F6E000400000003000B0043617465676F7279
      494400220000000800060050726963650004000000030009004D504720436974
      79000400000003000C004D50472048696768776179000400000003000600446F
      6F7273000400000003000C00426F64795374796C654944000400000003000A00
      43696C696E6465727300FF00000014000B00486F727365706F77657200FF0000
      0014000700546F7271756500FF000000140014005472616E736D697373696F6E
      205370656564730004000000030012005472616E736D697373696F6E20547970
      65000000000019000C004465736372697074696F6E00000000000D000600496D
      61676500000000000D00060050686F746F00080000000B000E0044656C697665
      72792044617465000200000005000800496E53746F636B00}
    SortOptions = []
    OnCalcFields = mdModelsCalcFields
    Left = 48
    Top = 16
    object mdModelsID: TIntegerField
      FieldName = 'ID'
    end
    object mdModelsTrademarkID: TIntegerField
      FieldName = 'TrademarkID'
    end
    object mdModelsTrademark: TWideStringField
      FieldKind = fkLookup
      FieldName = 'Trademark'
      LookupDataSet = mdTrademark
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'TrademarkID'
      Lookup = True
    end
    object mdModelsName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
    object mdModelsFullName: TWideStringField
      FieldKind = fkCalculated
      FieldName = 'FullName'
      Size = 255
      Calculated = True
    end
    object mdModelsModification: TWideStringField
      FieldName = 'Modification'
      Size = 255
    end
    object mdModelsCategoryID: TIntegerField
      FieldName = 'CategoryID'
    end
    object mdModelsCategory: TStringField
      FieldKind = fkLookup
      FieldName = 'Category'
      LookupDataSet = mdCategory
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'CategoryID'
      Lookup = True
    end
    object mdModelsPrice: TBCDField
      FieldName = 'Price'
    end
    object mdModelsMPG_City: TIntegerField
      FieldName = 'MPG City'
    end
    object mdModelsMPG_Highway: TIntegerField
      FieldName = 'MPG Highway'
    end
    object mdModelsDoors: TIntegerField
      FieldName = 'Doors'
    end
    object mdModelsBodyStyleID: TIntegerField
      FieldName = 'BodyStyleID'
    end
    object mdModelsBodyStyle: TStringField
      FieldKind = fkLookup
      FieldName = 'BodyStyle'
      LookupDataSet = mdBodyStyle
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'BodyStyleID'
      Size = 30
      Lookup = True
    end
    object mdModelsCilinders: TIntegerField
      DisplayLabel = 'Cylinders'
      FieldName = 'Cilinders'
    end
    object mdModelsHorsepower: TWideStringField
      FieldName = 'Horsepower'
      Size = 255
    end
    object mdModelsTorque: TWideStringField
      FieldName = 'Torque'
      Size = 255
    end
    object mdModelsTransmission_Speeds: TWideStringField
      FieldName = 'Transmission Speeds'
      Size = 255
    end
    object mdModelsTransmission_Type: TIntegerField
      FieldName = 'Transmission Type'
    end
    object mdModelsTransmissionTypeName: TStringField
      FieldKind = fkLookup
      FieldName = 'TransmissionTypeName'
      LookupDataSet = mdTransmissionType
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'Transmission Type'
      Lookup = True
    end
    object mdModelsDescription: TWideMemoField
      FieldName = 'Description'
      BlobType = ftWideMemo
    end
    object mdModelsImage: TBlobField
      FieldName = 'Image'
    end
    object mdModelsPhoto: TBlobField
      FieldName = 'Photo'
    end
    object mdModelsDelivery_Date: TDateTimeField
      FieldName = 'Delivery Date'
    end
    object mdModelsInStock: TBooleanField
      FieldName = 'InStock'
    end
    object mdModelsHyperlink: TStringField
      FieldKind = fkLookup
      FieldName = 'Hyperlink'
      LookupDataSet = mdTrademark
      LookupKeyFields = 'ID'
      LookupResultField = 'Site'
      KeyFields = 'TrademarkID'
      Size = 255
      Lookup = True
    end
  end
  object mdBodyStyle: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000300494400FF0000001400
      05004E616D6500}
    SortOptions = []
    Left = 48
    Top = 72
    object mdBodyStyleID: TIntegerField
      FieldName = 'ID'
    end
    object mdBodyStyleName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
  end
  object mdCategory: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F030000000400000003000300494400FF0000001400
      05004E616D6500000000000D0008005069637475726500}
    SortOptions = []
    Left = 48
    Top = 136
    object mdCategoryID: TIntegerField
      FieldName = 'ID'
    end
    object mdCategoryName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
    object mdCategoryPicture: TBlobField
      FieldName = 'Picture'
    end
  end
  object mdTrademark: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F050000000400000003000300494400FF0000001400
      05004E616D6500FF000000140005005369746500000000000D0005004C6F676F
      000000000019000C004465736372697074696F6E00}
    SortOptions = []
    Left = 48
    Top = 192
    object mdTrademarkID: TIntegerField
      FieldName = 'ID'
    end
    object mdTrademarkName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
    object mdTrademarkSite: TWideStringField
      FieldName = 'Site'
      Size = 255
    end
    object mdTrademarkLogo: TBlobField
      FieldName = 'Logo'
    end
    object mdTrademarkDescription: TWideMemoField
      FieldName = 'Description'
      BlobType = ftWideMemo
    end
  end
  object mdTransmissionType: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F020000000400000003000300494400FF0000001400
      05004E616D6500}
    SortOptions = []
    Left = 48
    Top = 248
    object mdTransmissionTypeID: TIntegerField
      FieldName = 'ID'
    end
    object mdTransmissionTypeName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
  end
  object dsModels: TDataSource
    DataSet = mdModels
    Left = 88
    Top = 16
  end
  object dsBodyStyle: TDataSource
    DataSet = mdBodyStyle
    Left = 88
    Top = 72
  end
  object dsCategory: TDataSource
    DataSet = mdCategory
    Left = 88
    Top = 136
  end
  object dsTrademark: TDataSource
    DataSet = mdTrademark
    Left = 88
    Top = 192
  end
  object dsTransmissionType: TDataSource
    DataSet = mdTransmissionType
    Left = 88
    Top = 248
  end
  object EditRepository: TcxEditRepository
    Left = 216
    Top = 17
    PixelsPerInch = 96
    object EditRepositoryCategoryLookup: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsCategory
    end
    object EditRepositoryTransmissionTypeLookup: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsTransmissionType
    end
    object EditRepositoryBodyStyleLookup: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Name'
        end>
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = dsBodyStyle
    end
    object EditRepositoryMemoBlob: TcxEditRepositoryBlobItem
      Properties.BlobEditKind = bekMemo
      Properties.BlobPaintStyle = bpsText
      Properties.MemoScrollBars = ssVertical
    end
    object EditRepositoryImageBlob: TcxEditRepositoryBlobItem
      Properties.BlobEditKind = bekPict
      Properties.PictureGraphicClassName = 'TdxSmartImage'
    end
    object EditRepositoryImage: TcxEditRepositoryImageItem
      Properties.GraphicClassName = 'TdxSmartImage'
    end
    object EditRepositoryMemo: TcxEditRepositoryMemoItem
      Properties.ScrollBars = ssVertical
    end
    object EditRepositoryTransmissionTypeCheckBox: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = '1'
      Properties.ValueUnchecked = '2'
    end
    object EditRepositoryPrice: TcxEditRepositoryCurrencyItem
      Properties.AutoSelect = False
      Properties.HideSelection = False
    end
  end
  object mdCarOrders: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 46
    Top = 304
    object mdCarOrdersID: TIntegerField
      FieldName = 'ID'
      Visible = False
    end
    object mdCarOrdersTrademark: TStringField
      FieldName = 'Trademark'
    end
    object mdCarOrdersName: TWideStringField
      DisplayLabel = 'Model'
      FieldName = 'Name'
      Size = 255
    end
    object mdCarOrdersModification: TWideStringField
      FieldName = 'Modification'
      Size = 255
    end
    object mdCarOrdersPrice: TBCDField
      FieldName = 'Price'
    end
    object mdCarOrdersMPG_City: TIntegerField
      DisplayLabel = 'City (mpg)'
      FieldName = 'MPG City'
    end
    object mdCarOrdersMPG_Highway: TIntegerField
      DisplayLabel = 'Highway (mpg)'
      FieldName = 'MPG Highway'
    end
    object mdCarOrdersBodyStyleID: TIntegerField
      FieldName = 'BodyStyleID'
      Visible = False
    end
    object mdCarOrdersCilinders: TIntegerField
      DisplayLabel = 'Cylinders'
      FieldName = 'Cilinders'
    end
    object mdCarOrdersSalesDate: TDateField
      DisplayLabel = 'Sales Date'
      FieldName = 'SalesDate'
    end
    object mdCarOrdersBodyStyle: TStringField
      DisplayLabel = 'Body Style'
      FieldKind = fkLookup
      FieldName = 'BodyStyle'
      LookupDataSet = mdBodyStyle
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'BodyStyleID'
      Size = 40
      Lookup = True
    end
    object mdCarOrdersParentID: TIntegerField
      FieldName = 'ParentID'
    end
  end
  object dsCarOrders: TDataSource
    DataSet = mdCarOrders
    Left = 88
    Top = 304
  end
end
