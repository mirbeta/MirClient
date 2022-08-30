inherited frmEditorsInPlace: TfrmEditorsInPlace
  Left = 285
  Top = 142
  Width = 926
  Height = 604
  Caption = 'PivotGrid - EditorsInPlace Demo'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 918
    Height = 16
    Caption = 'This demo uses in-place editors to display values as completion percentages in progress bars.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 32
    Width = 918
    Height = 519
    Align = alClient
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    TabOrder = 0
    object pgfPurchaseDate: TcxDBPivotGridField
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Purchase Date'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateMonth
      Visible = True
    end
    object pgfPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
    end
    object pgfQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      DataBinding.FieldName = 'Quantity'
      DisplayFormat = '0'
      Visible = True
      Width = 49
    end
    object pgfCarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      DataBinding.FieldName = 'Car Name'
      Visible = True
      Width = 77
    end
    object pgfUnitPrice: TcxDBPivotGridField
      AreaIndex = 0
      DataBinding.FieldName = 'Unit Price'
      Visible = True
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      Visible = True
      Width = 131
    end
    object pgfPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      PropertiesClassName = 'TcxProgressBarProperties'
      SummaryVariation = svPercentOfColumn
      Visible = True
      Width = 90
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      inherited miElementsVisibility: TMenuItem
        inherited miShowColumnFields: TMenuItem
          Caption = 'Show Column Fields'
        end
        inherited miShowDataFields: TMenuItem
          Caption = 'Show Data Fields'
        end
        inherited miShowFilterFields: TMenuItem
          Caption = 'Show Filter Fields'
        end
        inherited miShowFilterSeparator: TMenuItem
          Caption = 'Show Filter Separator'
        end
        inherited miShowRowFields: TMenuItem
          Caption = 'Show Row Fields'
        end
      end
    end
  end
end
