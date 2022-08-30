object DM: TDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 583
  Top = 329
  Height = 178
  Width = 355
  object dsHomePhotos: TDataSource
    DataSet = clHomePhotos
    Left = 40
    Top = 16
  end
  object clHomePhotos: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ParentID; ID'
    Params = <>
    Left = 40
    Top = 72
  end
  object dsHomesAndAgents: TDataSource
    DataSet = clHomesAndAgents
    Left = 152
    Top = 16
  end
  object clHomesAndAgents: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ID'
    Params = <>
    Left = 152
    Top = 72
  end
  object dsHomesAndHomes: TDataSource
    DataSet = clHomesAndHomes
    Left = 264
    Top = 16
  end
  object clHomesAndHomes: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 264
    Top = 72
  end
end
