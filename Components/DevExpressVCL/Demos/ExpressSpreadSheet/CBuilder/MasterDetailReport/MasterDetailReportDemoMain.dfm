inherited frmMasterDetail: TfrmMasterDetail
  Caption = 'DX SpreadSheet - Master-Detail Reports Demo'
  ClientHeight = 431
  ClientWidth = 864
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbMain: TStatusBar
    Top = 412
    Width = 864
  end
  inherited ReportDesigner: TdxSpreadSheetReportDesigner
    Width = 526
    Height = 412
    DataBinding.DataSource = dsMaster
    DataBinding.Options.DisplayName = 'Suppliers'
    Options.ReportMode = rmMultipleSheets
    OnAfterBuild = ReportDesignerAfterBuild
    Data = {
      E101000044585353763242460900000042465320000000000000000001000101
      010100000000000001004246532000000000424653200200000001000000200B
      00000007000000430061006C0069006200720069000000000000200000002000
      000000200000000020000000002000000000200007000000470045004E004500
      520041004C0000000000000200000000000000000101000000200B0000000700
      0000430061006C00690062007200690000000000002000000020000000002000
      00000020000000002000000000200007000000470045004E004500520041004C
      000000000000020000000000000000014246532001000000424653201D000000
      5400640078005300700072006500610064005300680065006500740052006500
      70006F00720074005400610062006C0065005600690065007700060000005300
      6800650065007400310001FFFFFFFFFFFFFFFF64000000020000000200000002
      0000005500000014000000020000000200000000020000000000000100000000
      0001010000424653205500000000000000424653200000000042465320140000
      0000000000424653200000000000000000000000000100000000000000000000
      0000000000000000004246532000000000000000000000000042465320000000
      0000000000}
    object ReportDesignerDetail1: TdxSpreadSheetReportDetail
      DataGroups = <>
      DataSource = dsDetailLevel0
      MasterKeyFieldName = 'SupplierID'
      DetailKeyFieldName = 'SupplierID'
      Options.DisplayName = 'Products'
      SectionID = 0
      SortedFields = <>
      object ReportDesignerDetail2: TdxSpreadSheetReportDetail
        DataGroups = <>
        DataSource = dsDetailLevel1
        MasterKeyFieldName = 'ProductID'
        DetailKeyFieldName = 'ProductID'
        Options.DisplayName = 'OrderReports'
        SectionID = 1
        SortedFields = <>
      end
    end
  end
  inherited cxSplitter1: TcxSplitter
    Left = 526
    Height = 412
  end
  inherited Panel1: TPanel
    Left = 534
    Height = 412
    inherited cxSplitter2: TcxSplitter
      Top = 134
    end
    inherited cxgFieldChooserSite: TcxGroupBox
      Height = 134
    end
    inherited cxgFilter: TcxGroupBox
      Top = 138
      inherited cbxUseFilter: TcxCheckBox
      end
    end
  end
  object dsMaster: TDataSource
    DataSet = mdsMaster
    Left = 112
    Top = 168
  end
  object mdsMaster: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    SortedField = 'SupplierID'
    Left = 192
    Top = 168
    object mdsMasterSupplierID: TAutoIncField
      FieldName = 'SupplierID'
    end
    object mdsMasterCompanyName: TWideStringField
      FieldName = 'CompanyName'
      Size = 40
    end
    object mdsMasterContactName: TWideStringField
      FieldName = 'ContactName'
      Size = 30
    end
    object mdsMasterContactTitle: TWideStringField
      FieldName = 'ContactTitle'
      Size = 30
    end
    object mdsMasterAddress: TWideStringField
      FieldName = 'Address'
      Size = 60
    end
    object mdsMasterCity: TWideStringField
      FieldName = 'City'
      Size = 15
    end
    object mdsMasterRegion: TWideStringField
      FieldName = 'Region'
      Size = 15
    end
    object mdsMasterPostalCode: TWideStringField
      FieldName = 'PostalCode'
      Size = 10
    end
    object mdsMasterCountry: TWideStringField
      FieldName = 'Country'
      Size = 15
    end
    object mdsMasterPhone: TWideStringField
      FieldName = 'Phone'
      Size = 24
    end
    object mdsMasterFax: TWideStringField
      FieldName = 'Fax'
      Size = 24
    end
    object mdsMasterHomePage: TWideMemoField
      FieldName = 'HomePage'
      BlobType = ftWideMemo
    end
  end
  object dsDetailLevel0: TDataSource
    DataSet = mdsDetailLevel0
    Left = 112
    Top = 219
  end
  object mdsDetailLevel0: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    SortedField = 'SupplierID'
    Left = 192
    Top = 219
    object mdsDetailLevel0ProductID: TAutoIncField
      FieldName = 'ProductID'
    end
    object mdsDetailLevel0ProductName: TWideStringField
      FieldName = 'ProductName'
      Size = 40
    end
    object mdsDetailLevel0SupplierID: TIntegerField
      FieldName = 'SupplierID'
    end
    object mdsDetailLevel0CategoryID: TIntegerField
      FieldName = 'CategoryID'
    end
    object mdsDetailLevel0QuantityPerUnit: TWideStringField
      FieldName = 'QuantityPerUnit'
    end
    object mdsDetailLevel0UnitPrice: TBCDField
      FieldName = 'UnitPrice'
    end
    object mdsDetailLevel0UnitsInStock: TSmallintField
      FieldName = 'UnitsInStock'
    end
    object mdsDetailLevel0UnitsOnOrder: TSmallintField
      FieldName = 'UnitsOnOrder'
    end
    object mdsDetailLevel0ReorderLevel: TSmallintField
      FieldName = 'ReorderLevel'
    end
    object mdsDetailLevel0Discontinued: TBooleanField
      FieldName = 'Discontinued'
    end
    object mdsDetailLevel0EAN13: TWideStringField
      FieldName = 'EAN13'
      Size = 12
    end
  end
  object dsDetailLevel1: TDataSource
    DataSet = mdsDetailLevel1
    Left = 112
    Top = 275
  end
  object mdsDetailLevel1: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    SortedField = 'ProductID'
    OnCalcFields = mdsDetailLevel1CalcFields
    Left = 192
    Top = 275
    object mdsDetailLevel1OrderID: TIntegerField
      FieldName = 'OrderID'
    end
    object mdsDetailLevel1ProductID: TIntegerField
      FieldName = 'ProductID'
    end
    object mdsDetailLevel1UnitPrice: TBCDField
      FieldName = 'UnitPrice'
    end
    object mdsDetailLevel1Quantity: TSmallintField
      FieldName = 'Quantity'
    end
    object mdsDetailLevel1Discount: TFloatField
      FieldName = 'Discount'
    end
    object mdsDetailLevel1SubTotal: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'SubTotal'
      Calculated = True
    end
  end
end
