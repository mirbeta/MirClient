inherited FilterControlDemoMainForm: TFilterControlDemoMainForm
  Left = 50
  Top = 113
  ActiveControl = pnlFilterDialog
  Caption = 'ExpressQuantumGrid Filter Control Demo'
  ClientHeight = 516
  ClientWidth = 937
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 937
    Caption = 
      'This demo shows use of the FilterControl with EQGrid and also wi' +
      'th a standard DBGrid. Click '#39'About this demo'#39' for more informati' +
      'on.'
  end
  object Splitter: TSplitter [1]
    Left = 401
    Top = 16
    Height = 481
    Beveled = True
    ResizeStyle = rsLine
  end
  inherited sbMain: TStatusBar
    Top = 497
    Width = 937
  end
  object PageControl: TcxPageControl [3]
    Left = 404
    Top = 16
    Width = 533
    Height = 481
    Align = alClient
    TabOrder = 1
    Properties.ActivePage = tsGrid4DisplayStyle
    Properties.CustomButtons.Buttons = <>
    Properties.MultiLine = True
    OnChange = PageControlChange
    ClientRectBottom = 477
    ClientRectLeft = 4
    ClientRectRight = 529
    ClientRectTop = 24
    object tsGrid4DisplayStyle: TcxTabSheet
      Caption = 'Filtered EQGrid'
      object Grid: TcxGrid
        Left = 0
        Top = 0
        Width = 525
        Height = 453
        Align = alClient
        TabOrder = 0
        object tvOrders: TcxGridDBTableView
          Navigator.Buttons.CustomButtons = <>
          OnFilterControlDialogShow = tvOrdersFilterControlDialogShow
          DataController.DataSource = FilterControlDemoDataDM.dsOrders
          DataController.DetailKeyFieldNames = 'ProductID'
          DataController.Filter.OnChanged = tvOrdersDataControllerFilterChanged
          DataController.Filter.Active = True
          DataController.KeyFieldNames = 'ID'
          DataController.MasterKeyFieldNames = 'ID'
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsBehavior.FocusCellOnTab = True
          OptionsView.CellAutoHeight = True
          OptionsView.Indicator = True
          Styles.StyleSheet = GridTableViewStyleSheetDevExpress
          object tvOrdersCustomerID: TcxGridDBColumn
            Caption = 'Company'
            DataBinding.FieldName = 'CustomerID'
            PropertiesClassName = 'TcxLookupComboBoxProperties'
            Properties.KeyFieldNames = 'ID'
            Properties.ListColumns = <
              item
                FieldName = 'Company'
              end>
            Properties.ListSource = FilterControlDemoDataDM.dsCustomers
            Width = 150
          end
          object tvOrdersProductID: TcxGridDBColumn
            Caption = 'Car'
            DataBinding.FieldName = 'ProductID'
            RepositoryItem = CarsEditorItem
            Width = 200
          end
          object tvOrdersPurchaseDate: TcxGridDBColumn
            DataBinding.FieldName = 'PurchaseDate'
            PropertiesClassName = 'TcxDateEditProperties'
            Width = 101
          end
          object tvOrdersTime: TcxGridDBColumn
            DataBinding.FieldName = 'Time'
            PropertiesClassName = 'TcxTimeEditProperties'
            Properties.TimeFormat = tfHourMin
            Width = 68
          end
          object tvOrdersPaymentAmount: TcxGridDBColumn
            DataBinding.FieldName = 'PaymentAmount'
            PropertiesClassName = 'TcxCalcEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Width = 111
          end
          object tvOrdersPaymentType: TcxGridDBColumn
            DataBinding.FieldName = 'PaymentType'
            PropertiesClassName = 'TcxImageComboBoxProperties'
            Properties.Images = FilterControlDemoDataDM.PaymentTypeImages
            Properties.Items = <
              item
                Description = 'Am. Express'
                ImageIndex = 3
                Value = 'AmEx'
              end
              item
                Description = 'Cash'
                ImageIndex = 0
                Value = 'Cash'
              end
              item
                Description = 'Master'
                ImageIndex = 2
                Value = 'Master'
              end
              item
                Description = 'Visa'
                ImageIndex = 1
                Value = 'Visa'
              end>
            Width = 100
          end
          object tvOrdersQuantity: TcxGridDBColumn
            DataBinding.FieldName = 'Quantity'
            PropertiesClassName = 'TcxSpinEditProperties'
            Width = 83
          end
        end
        object lvOrders: TcxGridLevel
          GridView = tvOrders
        end
      end
    end
    object tsStandardDisplayStyle: TcxTabSheet
      Caption = 'Standard DBGrid using the FilterControl'
      ImageIndex = 1
      object Label1: TLabel
        Left = 0
        Top = 413
        Width = 525
        Height = 40
        Align = alBottom
        AutoSize = False
        Caption = 
          'Note that, although our filter control will work with a standard' +
          ' grid, you do not have the cool built in functionality provided ' +
          'by ExpressQuantumGrid'
        Color = 4707838
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        WordWrap = True
      end
      object DBGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 525
        Height = 413
        Align = alClient
        Color = 16247513
        DataSource = FilterControlDemoDataDM.dsOrdersStd
        FixedColor = 12937777
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWhite
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = [fsBold]
      end
    end
  end
  object pnlFilterDialog: TPanel [4]
    Left = 0
    Top = 16
    Width = 401
    Height = 481
    Align = alLeft
    BevelOuter = bvNone
    Color = 16247513
    Constraints.MinHeight = 150
    Constraints.MinWidth = 225
    TabOrder = 2
    object cxFilterControl: TcxFilterControl
      Left = 0
      Top = 0
      Width = 233
      Height = 161
      LinkComponent = tvOrders
      TabOrder = 0
      OnApplyFilter = cxFilterControlApplyFilter
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 429
      Width = 401
      Height = 52
      Align = alBottom
      BevelOuter = bvNone
      Color = 16247513
      TabOrder = 2
      object btnLoad: TcxButton
        Left = 8
        Top = 16
        Width = 65
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Load...'
        Colors.Default = 15451300
        Colors.Normal = 15451300
        Colors.Pressed = 15451300
        TabOrder = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = btnLoadClick
      end
      object btnSaveAs: TcxButton
        Left = 80
        Top = 16
        Width = 65
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Save As...'
        Colors.Default = 15451300
        Colors.Normal = 15451300
        Colors.Pressed = 15451300
        TabOrder = 1
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = btnSaveAsClick
      end
      object btnApply: TcxButton
        Left = 328
        Top = 16
        Width = 65
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Colors.Default = 15451300
        Colors.Normal = 15451300
        Colors.Pressed = 15451300
        TabOrder = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = btnApplyClick
      end
      object btnOK: TcxButton
        Left = 168
        Top = 16
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Colors.Default = 15451300
        Colors.Normal = 15451300
        Colors.Pressed = 15451300
        ModalResult = 1
        TabOrder = 3
        Visible = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = btnApplyClick
      end
      object btnCancel: TcxButton
        Left = 248
        Top = 16
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        Colors.Default = 15451300
        Colors.Normal = 15451300
        Colors.Pressed = 15451300
        ModalResult = 2
        TabOrder = 4
        Visible = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object cxDBFilterControl: TcxDBFilterControl
      Left = 0
      Top = 184
      Width = 233
      Height = 201
      DataSet = FilterControlDemoDataDM.tblOrdersStd
      Items = <
        item
          Caption = 'Car'
          FieldName = 'ProductID'
          RepositoryItem = CarsEditorItem
        end
        item
          FieldName = 'PurchaseDate'
          PropertiesClassName = 'TcxDateEditProperties'
        end
        item
          FieldName = 'Time'
          PropertiesClassName = 'TcxTimeEditProperties'
        end
        item
          FieldName = 'Quantity'
          PropertiesClassName = 'TcxSpinEditProperties'
        end
        item
          Caption = 'Company'
          FieldName = 'CustomerID'
          RepositoryItem = CustomersEditorItem
        end
        item
          Caption = 'Payment Amount'
          FieldName = 'PaymentAmount'
          PropertiesClassName = 'TcxCalcEditProperties'
        end>
      TabOrder = 1
      Visible = False
      OnApplyFilter = cxDBFilterControlApplyFilter
    end
  end
  inherited mmMain: TMainMenu
    Left = 528
    Top = 8
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miFilterControl: TMenuItem
        Caption = '&Filter Dialog'
        object miFilterControlPosition: TMenuItem
          Caption = '&Position'
          object miFilterControlNone: TMenuItem
            Caption = '&None'
            GroupIndex = 1
            Hint = 'Setting this option hides the filter dialog '
            RadioItem = True
            OnClick = miFilterControlPosClick
          end
          object miFilterControlLeft: TMenuItem
            Tag = 1
            Caption = '&Left'
            Checked = True
            GroupIndex = 1
            Hint = 'Setting this option aligns the filter dialog to the left'
            RadioItem = True
            OnClick = miFilterControlPosClick
          end
          object miFilterControlTop: TMenuItem
            Tag = 2
            Caption = '&Top'
            GroupIndex = 1
            Hint = 'Setting this option aligns the filter dialog to the top'
            RadioItem = True
            OnClick = miFilterControlPosClick
          end
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object miFilterControlModal: TMenuItem
          Caption = 'Show &modal'
          Hint = 'Setting this option displays the modal filter dialog'
          OnClick = miFilterControlModalClick
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = Grid
    PopupMenus = <
      item
        GridView = tvOrders
        HitTypes = [gvhtColumnHeader]
        Index = 0
        PopupMenu = PopupMenu
      end>
    UseBuiltInPopupMenus = False
    Left = 416
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Filter = 'GridFilters(*.flt)|*.flt|All files|*.*'
    Left = 720
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Filter = 'Filters(*.flt)|*.flt'
    Left = 688
    Top = 8
  end
  object PopupMenu: TPopupMenu
    Left = 416
    Top = 40
    object miShowFilterPanel: TMenuItem
      Caption = 'Show filter &panel'
      object miFilterPnlNever: TMenuItem
        AutoCheck = True
        Caption = '&Never'
        GroupIndex = 1
        Hint = 'Setting this option hides the filter panel '
        RadioItem = True
        OnClick = miShowFilterPnlClick
      end
      object miFilterPnlNeverNonEmpty: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Non &Empty'
        GroupIndex = 1
        Hint = 
          'Setting this option makes the filter panel visible if filter is ' +
          'non empty'
        RadioItem = True
        OnClick = miShowFilterPnlClick
      end
      object miFilterPnlAlways: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = '&Always'
        Checked = True
        GroupIndex = 1
        Hint = 'Displays the brief description of the current demo features'
        RadioItem = True
        OnClick = miShowFilterPnlClick
      end
    end
    object miShowFilterBtn: TMenuItem
      AutoCheck = True
      Caption = 'Show customize filter &button'
      Checked = True
      OnClick = miShowFilterBtnClick
    end
  end
  object cxEditRepository: TcxEditRepository
    Left = 560
    Top = 8
    object CustomersEditorItem: TcxEditRepositoryLookupComboBoxItem
      Properties.Alignment.Horz = taLeftJustify
      Properties.DropDownWidth = 150
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'Company'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListSource = FilterControlDemoDataDM.dsCustomers
      Properties.PopupAlignment = taRightJustify
    end
    object CarsEditorItem: TcxEditRepositoryLookupComboBoxItem
      Properties.DropDownWidth = 300
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListSource = dmGridCars.dsModels
      Properties.PopupAlignment = taRightJustify
    end
  end
end
