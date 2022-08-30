inherited frmMultipleTotals: TfrmMultipleTotals
  Left = 224
  Top = 226
  Width = 814
  Caption = 'Multiple Totals with Custom Summary'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 806
    Caption = 
      'This demo shows how you can provide multiple types of subtotals ' +
      'for the outermost group values (either the row or column).'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 32
    Width = 806
    Height = 511
    Align = alClient
    DataSource = dmOrders.dsOrders
    Groups = <>
    OptionsSelection.MultiSelect = True
    Styles.OnGetColumnHeaderStyle = PivotGridStylesGetGroupHeaderStyle
    Styles.OnGetContentStyle = PivotGridStylesGetContentStyle
    Styles.OnGetRowHeaderStyle = PivotGridStylesGetGroupHeaderStyle
    TabOrder = 0
    TabStop = True
    object pgfPurchaseQuarter: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Purchase Quarter'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateQuarter
      Visible = True
    end
    object pgfPurchaseMonth: TcxDBPivotGridField
      AreaIndex = 2
      IsCaptionAssigned = True
      Caption = 'Purchase Month'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateMonth
      Visible = True
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      CustomTotals = <
        item
          SummaryType = stMin
        end
        item
          SummaryType = stMax
        end
        item
          SummaryType = stCount
        end
        item
        end
        item
          SummaryType = stAverage
        end
        item
          SummaryType = stCustom
          DisplayFormat = '0.00 %'
        end>
      DataBinding.FieldName = 'Company Name'
      TotalsVisibility = tvCustom
      Visible = True
      Width = 127
    end
    object cxDBPivotGrid1PaymentType: TcxDBPivotGridField
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
    end
    object cxDBPivotGrid1Quantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      DataBinding.FieldName = 'Quantity'
      Visible = True
      OnCalculateCustomSummary = CalculateCustomSummary
    end
    object cxDBPivotGrid1CarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      DataBinding.FieldName = 'Car Name'
      Visible = True
    end
    object cxDBPivotGrid1UnitPrice: TcxDBPivotGridField
      AreaIndex = 1
      DataBinding.FieldName = 'Unit Price'
      Visible = True
    end
    object cxDBPivotGrid1PaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      Visible = True
      OnCalculateCustomSummary = CalculateCustomSummary
    end
  end
end
