object ViewBandedFixedDemoDMMain: TViewBandedFixedDemoDMMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 306
  Top = 181
  Height = 367
  Width = 305
  object dsSCHEDULER: TDataSource
    DataSet = tblSCHEDULER
    Left = 32
    Top = 64
  end
  object dsUSERS: TDataSource
    DataSet = tblUSERS
    Left = 32
    Top = 112
  end
  object dsPROJECTS: TDataSource
    DataSet = tblPROJECTS
    Left = 32
    Top = 168
  end
  object tblSCHEDULER: TClientDataSet
    Aggregates = <>
    Params = <>
    OnCalcFields = tblSCHEDULERCalcFields
    Left = 112
    Top = 56
    object tblSCHEDULERID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object PROJECTID: TIntegerField
      FieldName = 'PROJECTID'
    end
    object USERID: TIntegerField
      FieldName = 'USERID'
    end
    object SUNDAY: TSmallintField
      FieldName = 'SUNDAY'
    end
    object MONDAY: TSmallintField
      FieldName = 'MONDAY'
    end
    object TUESDAY: TSmallintField
      FieldName = 'TUESDAY'
    end
    object WEDNESDAY: TSmallintField
      FieldName = 'WEDNESDAY'
    end
    object THURSDAY: TSmallintField
      FieldName = 'THURSDAY'
    end
    object FRIDAY: TSmallintField
      FieldName = 'FRIDAY'
    end
    object SATURDAY: TSmallintField
      FieldName = 'SATURDAY'
    end
    object RowAvg: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'RowAvg'
      Calculated = True
    end
    object RowSum: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'RowSum'
      Calculated = True
    end
    object UserName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'UserName'
      Calculated = True
    end
    object FirstName: TStringField
      FieldKind = fkLookup
      FieldName = 'FirstName'
      LookupDataSet = tblUSERS
      LookupKeyFields = 'ID'
      LookupResultField = 'FNAME'
      KeyFields = 'USERID'
      Lookup = True
    end
    object MiddleName: TStringField
      FieldKind = fkLookup
      FieldName = 'MiddleName'
      LookupDataSet = tblUSERS
      LookupKeyFields = 'ID'
      LookupResultField = 'MNAME'
      KeyFields = 'USERID'
      Lookup = True
    end
    object LastName: TStringField
      FieldKind = fkLookup
      FieldName = 'LastName'
      LookupDataSet = tblUSERS
      LookupKeyFields = 'ID'
      LookupResultField = 'LNAME'
      KeyFields = 'USERID'
      Lookup = True
    end
  end
  object tblUSERS: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 112
    object tblUSERSID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object tblUSERSFNAME: TStringField
      FieldName = 'FNAME'
      Size = 25
    end
    object tblUSERSMNAME: TStringField
      FieldName = 'MNAME'
    end
    object tblUSERSLNAME: TStringField
      FieldName = 'LNAME'
      Size = 25
    end
    object tblUSERSEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 50
    end
    object tblUSERSPHONE: TStringField
      FieldName = 'PHONE'
      Size = 24
    end
    object tblUSERSDEPARTMENTID: TIntegerField
      FieldName = 'DEPARTMENTID'
    end
  end
  object tblPROJECTS: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 168
    object tblPROJECTSID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object tblPROJECTSNAME: TStringField
      FieldName = 'NAME'
      Size = 100
    end
    object tblPROJECTSMANAGERID: TIntegerField
      FieldName = 'MANAGERID'
    end
  end
end
