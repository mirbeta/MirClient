object ConditionalFormattingDemoMainDM: TConditionalFormattingDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 310
  Width = 402
  object cdsConditionalFormatting: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 72
    Top = 32
    object cdsConditionalFormattingState: TStringField
      FieldName = 'State'
    end
    object cdsConditionalFormattingSales: TFloatField
      FieldName = 'Sales'
    end
    object cdsConditionalFormattingProfit: TFloatField
      FieldName = 'Profit'
    end
    object cdsConditionalFormattingSalesVsTarget: TFloatField
      DisplayLabel = 'Sales vs Target'
      FieldName = 'SalesVsTarget'
    end
    object cdsConditionalFormattingMarketShare: TFloatField
      DisplayLabel = 'Market Share'
      FieldName = 'MarketShare'
    end
    object cdsConditionalFormattingCustomersSatisfaction: TFloatField
      DisplayLabel = 'Satisfaction'
      FieldName = 'CustomersSatisfaction'
    end
  end
  object dsConditionalFormatting: TDataSource
    DataSet = cdsConditionalFormatting
    Left = 112
    Top = 32
  end
end
