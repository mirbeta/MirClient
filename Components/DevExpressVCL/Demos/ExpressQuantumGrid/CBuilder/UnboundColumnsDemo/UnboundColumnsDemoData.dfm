object UnboundColumnsDemoDataDM: TUnboundColumnsDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 350
  Top = 198
  Height = 234
  Width = 374
  object tblCustomers: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 40
    Top = 56
    object tblCustomersID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object tblCustomersFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object tblCustomersLASTNAME: TStringField
      FieldName = 'LASTNAME'
      Size = 50
    end
    object tblCustomersCOMPANYNAME: TStringField
      FieldName = 'COMPANYNAME'
      Size = 50
    end
    object tblCustomersPAYMENTTYPE: TIntegerField
      FieldName = 'PAYMENTTYPE'
    end
    object tblCustomersPRODUCTID: TIntegerField
      FieldName = 'PRODUCTID'
    end
    object tblCustomersCUSTOMER: TBooleanField
      FieldName = 'CUSTOMER'
    end
    object tblCustomersPURCHASEDATE: TDateTimeField
      FieldName = 'PURCHASEDATE'
    end
    object tblCustomersPAYMENTAMOUNT: TCurrencyField
      FieldName = 'PAYMENTAMOUNT'
    end
    object tblCustomersCOPIES: TIntegerField
      FieldName = 'COPIES'
    end
  end
  object dsCustomers: TDataSource
    DataSet = tblCustomers
    Left = 40
    Top = 104
  end
end
