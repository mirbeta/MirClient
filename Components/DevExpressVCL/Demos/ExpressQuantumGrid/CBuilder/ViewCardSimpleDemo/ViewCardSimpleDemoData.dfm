object ViewCardSimpleDemoMainDM: TViewCardSimpleDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 235
  Top = 192
  Height = 272
  Width = 248
  object tlbDEPARTMENTS: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 144
    Top = 64
    object tlbDEPARTMENTSID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object tlbDEPARTMENTSNAME: TStringField
      FieldName = 'NAME'
      Size = 100
    end
  end
  object dsDEPARTMENTS: TDataSource
    DataSet = tlbDEPARTMENTS
    Left = 40
    Top = 64
  end
  object dsUSERS: TDataSource
    DataSet = tlbUSERS
    Left = 40
    Top = 120
  end
  object tlbUSERS: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'DEPARTMENTID'
    MasterFields = 'ID'
    MasterSource = dsDEPARTMENTS
    Params = <>
    Left = 144
    Top = 120
  end
end
