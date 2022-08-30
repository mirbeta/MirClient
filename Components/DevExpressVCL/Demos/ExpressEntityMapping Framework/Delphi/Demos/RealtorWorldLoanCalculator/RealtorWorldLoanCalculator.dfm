object frmLoanCalculator: TfrmLoanCalculator
  Left = 0
  Top = 0
  ClientHeight = 580
  ClientWidth = 1232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescription: TLabel
    Left = 0
    Top = 0
    Width = 1232
    Height = 16
    Align = alTop
    Caption = 
      'This demo shows how to populate TdxEMFDataSet components with ob' +
      'jects stored in memory and visualize them with the Table and Cha' +
      'rt Views provided by the ExpressQuantumGrid control.'
    Color = 12937777
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    WordWrap = True
  end
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 32
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    PanelStyle.Active = True
    TabOrder = 0
    Height = 548
    Width = 857
    object cxGrid2: TcxGrid
      Left = 2
      Top = 192
      Width = 853
      Height = 354
      Align = alClient
      BorderStyle = cxcbsNone
      TabOrder = 0
      object cxGrid2DBChartView1: TcxGridDBChartView
        Categories.DataBinding.FieldName = 'year'
        DataController.DataSource = dtsYearlyPayments
        DiagramStackedColumn.Active = True
        Legend.Border = lbSingle
        Title.Alignment = cpaCenter
        Title.Text = 'Payment by Year'
        Title.Position = cppTop
        object cxGrid2DBChartView1Series2: TcxGridDBChartSeries
          DataBinding.FieldName = 'Interest'
        end
        object cxGrid2DBChartView1Series1: TcxGridDBChartSeries
          DataBinding.FieldName = 'Principal'
        end
      end
      object cxGrid2Level1: TcxGridLevel
        GridView = cxGrid2DBChartView1
      end
    end
    object cxGroupBox3: TcxGroupBox
      Left = 2
      Top = 2
      Align = alTop
      Style.BorderStyle = ebsNone
      Style.Edges = [bLeft, bTop, bRight, bBottom]
      TabOrder = 1
      Height = 190
      Width = 853
      object cxGroupBox5: TcxGroupBox
        Left = 233
        Top = 18
        Align = alClient
        Style.BorderStyle = ebsNone
        TabOrder = 0
        Height = 170
        Width = 618
        object cxLabel5: TcxLabel
          Left = 2
          Top = 80
          Align = alTop
          Caption = 'Your Monthly Payment'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -16
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Properties.Alignment.Horz = taCenter
          Transparent = True
          AnchorX = 309
        end
        object lblMonthlyPayment: TcxLabel
          Left = 2
          Top = 105
          Align = alTop
          Caption = 'lblMonthlyPayment'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -19
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.Alignment.Horz = taCenter
          Transparent = True
          AnchorX = 309
        end
        object cxGroupBox6: TcxGroupBox
          Left = 2
          Top = 18
          Align = alTop
          Style.BorderStyle = ebsNone
          TabOrder = 2
          Height = 62
          Width = 614
        end
      end
      object cxGroupBox4: TcxGroupBox
        Left = 2
        Top = 18
        Align = alLeft
        Style.BorderStyle = ebsNone
        TabOrder = 1
        Height = 170
        Width = 231
        object cxLabel6: TcxLabel
          Left = 7
          Top = 23
          Caption = 'Loan Amount:'
          Transparent = True
        end
        object seLoan: TcxSpinEdit
          Left = 92
          Top = 22
          Properties.Alignment.Horz = taRightJustify
          Properties.AssignedValues.EditFormat = True
          Properties.AssignedValues.MinValue = True
          Properties.DisplayFormat = '$,0;-$,0'
          Properties.Increment = 1000.000000000000000000
          Properties.MaxValue = 999999999999.000000000000000000
          Properties.UseDisplayFormatWhenEditing = True
          Properties.UseLeftAlignmentOnEditing = False
          TabOrder = 1
          Width = 125
        end
        object cbInterests: TcxComboBox
          Left = 92
          Top = 49
          Properties.Alignment.Horz = taRightJustify
          Properties.DropDownListStyle = lsFixedList
          Properties.PopupAlignment = taRightJustify
          Properties.UseLeftAlignmentOnEditing = False
          TabOrder = 2
          Width = 125
        end
        object cxLabel2: TcxLabel
          Left = 7
          Top = 51
          Caption = 'Interest Rate:'
          Transparent = True
        end
        object cxLabel3: TcxLabel
          Left = 7
          Top = 81
          Caption = 'Term of Loan:'
          Transparent = True
        end
        object cbTerms: TcxComboBox
          Left = 92
          Top = 79
          Properties.Alignment.Horz = taRightJustify
          Properties.DropDownListStyle = lsFixedList
          Properties.PopupAlignment = taRightJustify
          Properties.UseLeftAlignmentOnEditing = False
          TabOrder = 5
          Width = 125
        end
        object cxLabel4: TcxLabel
          Left = 7
          Top = 111
          Caption = 'Start Month:'
          Transparent = True
        end
        object cbStartMonths: TcxComboBox
          Left = 92
          Top = 109
          Properties.Alignment.Horz = taRightJustify
          Properties.DropDownListStyle = lsFixedList
          Properties.PopupAlignment = taRightJustify
          Properties.UseLeftAlignmentOnEditing = False
          TabOrder = 7
          Width = 125
        end
        object btnCalculate: TcxButton
          Left = 7
          Top = 136
          Width = 210
          Height = 25
          Caption = 'Calculate'
          TabOrder = 8
          OnClick = btnCalculateClick
        end
      end
    end
  end
  object cxSplitter1: TcxSplitter
    Left = 857
    Top = 32
    Width = 8
    Height = 548
    Control = cxGroupBox1
    OnBeforeClose = cxSplitter1BeforeClose
  end
  object cxGroupBox2: TcxGroupBox
    Left = 865
    Top = 32
    Align = alClient
    PanelStyle.Active = True
    TabOrder = 2
    Height = 548
    Width = 367
    object cxGrid1: TcxGrid
      Left = 2
      Top = 2
      Width = 363
      Height = 544
      Align = alClient
      BorderStyle = cxcbsNone
      TabOrder = 0
      object cxGrid1DBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = dtsMonthlyPayments
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <
          item
            Format = '$,0;-$,0'
            Kind = skSum
            FieldName = 'Interest'
            Column = cxGrid1DBTableView1Interest
          end
          item
            Format = '$,0;-$,0'
            Kind = skSum
            FieldName = 'Principal'
            Column = cxGrid1DBTableView1Principal
          end>
        DataController.Summary.SummaryGroups = <>
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.CellSelect = False
        OptionsSelection.HideFocusRectOnExit = False
        OptionsSelection.HideSelection = True
        OptionsSelection.UnselectFocusedRecordOnExit = False
        OptionsView.ColumnAutoWidth = True
        OptionsView.Footer = True
        OptionsView.GroupByBox = False
        object cxGrid1DBTableView1RecId: TcxGridDBColumn
          DataBinding.FieldName = 'RecId'
          Visible = False
        end
        object cxGrid1DBTableView1Month: TcxGridDBColumn
          DataBinding.FieldName = 'Month'
        end
        object cxGrid1DBTableView1Balance: TcxGridDBColumn
          DataBinding.FieldName = 'Balance'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0;-$,0'
          Properties.UseThousandSeparator = True
        end
        object cxGrid1DBTableView1Interest: TcxGridDBColumn
          DataBinding.FieldName = 'Interest'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0;-$,0'
          Properties.UseThousandSeparator = True
        end
        object cxGrid1DBTableView1Principal: TcxGridDBColumn
          DataBinding.FieldName = 'Principal'
          PropertiesClassName = 'TcxCurrencyEditProperties'
          Properties.DisplayFormat = '$,0;-$,0'
          Properties.UseThousandSeparator = True
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1DBTableView1
      end
    end
  end
  object dtsMonthlyPayments: TDataSource
    DataSet = edsMonthlyPayments
    Left = 168
    Top = 408
  end
  object dtsYearlyPayments: TDataSource
    DataSet = edsYearlyPayments
    Left = 64
    Top = 408
  end
  object edsYearlyPayments: TdxEMFDataSet
    Params = <>
    ReadOnly = True
    Left = 64
    Top = 352
  end
  object edsMonthlyPayments: TdxEMFDataSet
    Params = <>
    ReadOnly = True
    Left = 168
    Top = 352
  end
  object mmMain: TMainMenu
    Left = 296
    Top = 352
    object miAbout: TMenuItem
      Caption = '&About this demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
end
