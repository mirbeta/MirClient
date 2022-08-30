object dmDemo: TdmDemo
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 393
  Width = 676
  object dsOrders: TDataSource
    DataSet = mdOrders
    Left = 144
    Top = 80
  end
  object llcfMain: TdxLayoutLookAndFeelList
    Left = 16
    object dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel
    end
    object dxLayoutOfficeLookAndFeel1: TdxLayoutOfficeLookAndFeel
    end
    object dxLayoutWebLookAndFeel1: TdxLayoutWebLookAndFeel
    end
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
  object mdOrders: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    Left = 152
    Top = 16
    object mdOrdersPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
    end
    object mdOrdersOrders_Time: TDateTimeField
      DisplayLabel = 'Time'
      FieldName = 'Orders_Time'
    end
    object mdOrdersPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object mdOrdersPaymentAmount: TFloatField
      FieldName = 'PaymentAmount'
    end
    object mdOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object mdOrdersFirstName: TStringField
      FieldKind = fkLookup
      FieldName = 'FirstName'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'FirstName'
      KeyFields = 'Customers_ID'
      Size = 25
      Lookup = True
    end
    object mdOrdersLastName: TStringField
      FieldKind = fkLookup
      FieldName = 'LastName'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'LastName'
      KeyFields = 'Customers_ID'
      Size = 25
      Lookup = True
    end
    object mdOrdersCompany: TStringField
      FieldKind = fkLookup
      FieldName = 'Company'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Company'
      KeyFields = 'Customers_ID'
      Size = 50
      Lookup = True
    end
    object mdOrdersPrefix: TStringField
      FieldKind = fkLookup
      FieldName = 'Prefix'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Prefix'
      KeyFields = 'Customers_ID'
      Size = 15
      Lookup = True
    end
    object mdOrdersTitle: TStringField
      FieldKind = fkLookup
      FieldName = 'Title'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Title'
      KeyFields = 'Customers_ID'
      Size = 15
      Lookup = True
    end
    object mdOrdersAddress: TStringField
      FieldKind = fkLookup
      FieldName = 'Address'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Address'
      KeyFields = 'Customers_ID'
      Size = 50
      Lookup = True
    end
    object mdOrdersCity: TStringField
      FieldKind = fkLookup
      FieldName = 'City'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'City'
      KeyFields = 'Customers_ID'
      Lookup = True
    end
    object mdOrdersState: TStringField
      FieldKind = fkLookup
      FieldName = 'State'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'State'
      KeyFields = 'Customers_ID'
      Size = 2
      Lookup = True
    end
    object mdOrdersZipCode: TStringField
      FieldKind = fkLookup
      FieldName = 'ZipCode'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'ZipCode'
      KeyFields = 'Customers_ID'
      Size = 10
      Lookup = True
    end
    object mdOrdersSource: TStringField
      FieldKind = fkLookup
      FieldName = 'Source'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Source'
      KeyFields = 'Customers_ID'
      Size = 10
      Lookup = True
    end
    object mdOrdersCustomer: TStringField
      FieldKind = fkLookup
      FieldName = 'Customer'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Customer'
      KeyFields = 'Customers_ID'
      Size = 1
      Lookup = True
    end
    object mdOrdersCustomerPhoto: TBlobField
      FieldName = 'CustomerPhoto'
    end
    object mdOrdersHomePhone: TStringField
      FieldKind = fkLookup
      FieldName = 'HomePhone'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'HomePhone'
      KeyFields = 'Customers_ID'
      Size = 15
      Lookup = True
    end
    object mdOrdersFaxPhone: TStringField
      FieldKind = fkLookup
      FieldName = 'FaxPhone'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'FaxPhone'
      KeyFields = 'Customers_ID'
      Size = 15
      Lookup = True
    end
    object mdOrdersSpouse: TStringField
      FieldKind = fkLookup
      FieldName = 'Spouse'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Spouse'
      KeyFields = 'Customers_ID'
      Size = 50
      Lookup = True
    end
    object mdOrdersOccupation: TStringField
      FieldKind = fkLookup
      FieldName = 'Occupation'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Occupation'
      KeyFields = 'Customers_ID'
      Size = 25
      Lookup = True
    end
    object mdOrdersEmail: TStringField
      FieldKind = fkLookup
      FieldName = 'Email'
      LookupDataSet = mdCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Email'
      KeyFields = 'Customers_ID'
      Size = 255
      Lookup = True
    end
    object mdOrdersTrademark: TStringField
      FieldName = 'Trademark'
      Size = 50
    end
    object mdOrdersModel: TStringField
      FieldName = 'Model'
      Size = 50
    end
    object mdOrdersHP: TSmallintField
      FieldName = 'HP'
    end
    object mdOrdersLiter: TFloatField
      FieldName = 'Liter'
    end
    object mdOrdersCyl: TSmallintField
      FieldName = 'Cyl'
    end
    object mdOrdersTransmissSpeedCount: TSmallintField
      FieldName = 'TransmissSpeedCount'
    end
    object mdOrdersTransmissAutomatic: TStringField
      FieldName = 'TransmissAutomatic'
      Size = 3
    end
    object mdOrdersMPG_City: TSmallintField
      FieldName = 'MPG_City'
    end
    object mdOrdersMPG_Highway: TSmallintField
      FieldName = 'MPG_Highway'
    end
    object mdOrdersCategory: TStringField
      FieldName = 'Category'
      Size = 7
    end
    object mdOrdersCars_Description: TMemoField
      FieldName = 'Cars_Description'
      BlobType = ftMemo
      Size = 10
    end
    object mdOrdersHyperlink: TStringField
      FieldName = 'Hyperlink'
      Size = 50
    end
    object mdOrdersPicture: TBlobField
      FieldName = 'Picture'
      Size = 10
    end
    object mdOrdersPrice: TFloatField
      FieldName = 'Price'
    end
    object mdOrdersCustomers_ID: TIntegerField
      FieldName = 'Customers_ID'
    end
    object mdOrdersOrders_ID: TIntegerField
      FieldName = 'Orders_ID'
    end
    object mdOrdersCars_ID: TIntegerField
      FieldName = 'Cars_ID'
    end
  end
  object mdCars: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    Left = 248
    Top = 16
    object mdCarsID: TIntegerField
      FieldName = 'ID'
    end
    object mdCarsTrademark: TStringField
      FieldName = 'Trademark'
      Size = 50
    end
    object mdCarsModel: TStringField
      FieldName = 'Model'
      Size = 50
    end
    object mdCarsHP: TSmallintField
      FieldName = 'HP'
    end
    object mdCarsLiter: TFloatField
      FieldName = 'Liter'
    end
    object mdCarsCyl: TSmallintField
      FieldName = 'Cyl'
    end
    object mdCarsTransmissSpeedCount: TSmallintField
      FieldName = 'TransmissSpeedCount'
    end
    object mdCarsTransmissAutomatic: TStringField
      FieldName = 'TransmissAutomatic'
      Size = 3
    end
    object mdCarsMPG_City: TSmallintField
      FieldName = 'MPG_City'
    end
    object mdCarsMPG_Highway: TSmallintField
      FieldName = 'MPG_Highway'
    end
    object mdCarsCategory: TStringField
      FieldName = 'Category'
      Size = 7
    end
    object mdCarsCars_Description: TMemoField
      FieldName = 'Cars_Description'
      BlobType = ftMemo
      Size = 10
    end
    object mdCarsHyperlink: TStringField
      FieldName = 'Hyperlink'
      Size = 50
    end
    object mdCarsPicture: TBlobField
      FieldName = 'Picture'
      Size = 10
    end
    object mdCarsPrice: TFloatField
      FieldName = 'Price'
    end
  end
  object dsCars: TDataSource
    DataSet = mdCars
    Left = 248
    Top = 80
  end
  object mdCustomers: TdxMemData
    Active = True
    Indexes = <>
    SortOptions = []
    Left = 352
    Top = 24
    object mdCustomersID: TIntegerField
      FieldName = 'ID'
    end
    object mdCustomersFirstName: TStringField
      FieldName = 'FirstName'
      Size = 25
    end
    object mdCustomersLastName: TStringField
      FieldName = 'LastName'
      Size = 25
    end
    object mdCustomersCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
    object mdCustomersPrefix: TStringField
      FieldName = 'Prefix'
      Size = 15
    end
    object mdCustomersTitle: TStringField
      FieldName = 'Title'
      Size = 15
    end
    object mdCustomersAddress: TStringField
      FieldName = 'Address'
      Size = 50
    end
    object mdCustomersCity: TStringField
      FieldName = 'City'
    end
    object mdCustomersState: TStringField
      FieldName = 'State'
      Size = 2
    end
    object mdCustomersZipCode: TStringField
      FieldName = 'ZipCode'
      Size = 10
    end
    object mdCustomersSource: TStringField
      FieldName = 'Source'
      Size = 10
    end
    object mdCustomersCustomer: TStringField
      FieldName = 'Customer'
      Size = 1
    end
    object mdCustomersPhoto: TBlobField
      FieldName = 'Photo'
    end
    object mdCustomersHomePhone: TStringField
      FieldName = 'HomePhone'
      Size = 15
    end
    object mdCustomersFaxPhone: TStringField
      FieldName = 'FaxPhone'
      Size = 15
    end
    object mdCustomersSpouse: TStringField
      FieldName = 'Spouse'
      Size = 50
    end
    object mdCustomersOccupation: TStringField
      FieldName = 'Occupation'
      Size = 25
    end
    object mdCustomersEmail: TStringField
      FieldName = 'Email'
      Size = 70
    end
    object mdCustomersDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
    end
  end
  object dsCustomers: TDataSource
    DataSet = mdCustomers
    Left = 360
    Top = 80
  end
end
