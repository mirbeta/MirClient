inherited frmOrderReport: TfrmOrderReport
  Left = 32
  Top = 83
  Caption = 'PivotGrid - Order Reports'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo allows you to display summarized data on car sales by ' +
      'filtering source data, organizing the layout via drag-and-drop o' +
      'perations and customizing the appearance settings of visual elem' +
      'ents using a set of options.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 32
    Width = 807
    Height = 511
    Align = alClient
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    TabOrder = 0
    TabStop = True
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
      OnGetGroupImageIndex = pgfPaymentTypeGetGroupImageIndex
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
