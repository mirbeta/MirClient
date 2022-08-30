inherited UnboundColumnsDemoMainForm: TUnboundColumnsDemoMainForm
  Left = 107
  Top = 75
  Caption = 'ExpressQuantumGrid UnboundColumns Demo'
  ClientHeight = 465
  ClientWidth = 831
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 831
    Caption = 
      'Experiment with unbound columns within the data-aware GridView. ' +
      'Click '#39'About this demo'#39' for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 446
    Width = 831
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 831
    Height = 430
    Align = alClient
    TabOrder = 1
    object BandedTableView: TcxGridDBBandedTableView
      DataController.DataModeController.SmartRefresh = True
      DataController.DataSource = UnboundColumnsDemoDataDM.dsCustomers
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      Styles.OnGetContentStyle = BandedTableViewStylesGetContentStyle
      Bands = <
        item
          Caption = 'Unbound Data (Data is randomly generated )'
          Width = 392
        end
        item
          Caption = 'Bound Data'
          Width = 428
        end>
      object BandedTableViewFIRSTNAME: TcxGridDBBandedColumn
        Caption = 'First Name'
        DataBinding.FieldName = 'FIRSTNAME'
        Width = 73
        Position.BandIndex = 1
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object BandedTableViewLASTNAME: TcxGridDBBandedColumn
        Caption = 'Last Name'
        DataBinding.FieldName = 'LASTNAME'
        Width = 74
        Position.BandIndex = 1
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object BandedTableViewCOMPANYNAME: TcxGridDBBandedColumn
        Caption = 'Company'
        DataBinding.FieldName = 'COMPANYNAME'
        Width = 78
        Position.BandIndex = 1
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object BandedTableViewPURCHASEDATE: TcxGridDBBandedColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PURCHASEDATE'
        Width = 55
        Position.BandIndex = 1
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
      object BandedTableViewPAYMENTAMOUNT: TcxGridDBBandedColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PAYMENTAMOUNT'
        Width = 55
        Position.BandIndex = 1
        Position.ColIndex = 4
        Position.RowIndex = 0
      end
      object BandedTableViewCOPIES: TcxGridDBBandedColumn
        Caption = 'Copies'
        DataBinding.FieldName = 'COPIES'
        Width = 44
        Position.BandIndex = 1
        Position.ColIndex = 5
        Position.RowIndex = 0
      end
      object BandedTableViewSelected: TcxGridDBBandedColumn
        Caption = 'Selected'
        DataBinding.ValueType = 'Boolean'
        PropertiesClassName = 'TdxToggleSwitchProperties'
        Width = 67
        Position.BandIndex = 0
        Position.ColIndex = 0
        Position.RowIndex = 0
      end
      object BandedTableViewSupportRequests: TcxGridDBBandedColumn
        Caption = 'Support Requests'
        DataBinding.ValueType = 'Integer'
        PropertiesClassName = 'TcxSpinEditProperties'
        Width = 130
        Position.BandIndex = 0
        Position.ColIndex = 1
        Position.RowIndex = 0
      end
      object BandedTableViewLastSupportRequest: TcxGridDBBandedColumn
        Caption = 'Last Support Request'
        DataBinding.ValueType = 'DateTime'
        PropertiesClassName = 'TcxDateEditProperties'
        Width = 129
        Position.BandIndex = 0
        Position.ColIndex = 2
        Position.RowIndex = 0
      end
      object BandedTableViewComments: TcxGridDBBandedColumn
        Caption = 'Comments'
        DataBinding.ValueType = 'String'
        PropertiesClassName = 'TcxBlobEditProperties'
        Properties.BlobEditKind = bekMemo
        Width = 66
        Position.BandIndex = 0
        Position.ColIndex = 3
        Position.RowIndex = 0
      end
    end
    object Level: TcxGridLevel
      GridView = BandedTableView
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 536
    Top = 24
    PixelsPerInch = 96
    object styChecked: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16313312
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = 97182
    end
  end
end
