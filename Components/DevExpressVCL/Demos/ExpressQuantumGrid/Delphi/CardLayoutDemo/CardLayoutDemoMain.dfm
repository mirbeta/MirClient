inherited frmMain: TfrmMain
  Left = 300
  Top = 120
  Caption = 'ExpressQuantumGrid CardLayout Demo'
  ClientHeight = 668
  ClientWidth = 747
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 747
    Height = 29
    AutoSize = False
    Caption = 
      '  This demo shows different layout options available for the Car' +
      'dView. Click '#39'About this demo'#39' for more information.'
    Layout = tlCenter
  end
  object Grid: TcxGrid [1]
    Left = 0
    Top = 29
    Width = 747
    Height = 620
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = GridActiveTabChanged
    object cvHorizontal: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = dmGridCars.dsModels
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.LayeredRows = True
      OptionsCustomize.RowMoving = True
      OptionsView.CardAutoWidth = True
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 300
      OptionsView.CategorySeparatorWidth = 1
      OptionsView.CellAutoHeight = True
      OptionsView.SeparatorColor = clSilver
      Styles.StyleSheet = CardsStyleSheet
      object cvHorizontalTrademark: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Trademark'
        Options.ShowCaption = False
        Position.BeginsLayer = True
        Styles.Content = styleCardHeader
      end
      object cvHorizontalModel: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Name'
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Styles.Content = styleCardHeader
      end
      object cvHorizontalPicture: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Photo'
        RepositoryItem = dmGridCars.EditRepositoryImage
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvHorizontalRow3: TcxGridDBCardViewRow
        Caption = 'Engine'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = True
      end
      object cvHorizontalHP: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Horsepower'
        Position.BeginsLayer = True
      end
      object cvHorizontalTorque: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Torque'
        Position.BeginsLayer = True
      end
      object cvHorizontalCyl: TcxGridDBCardViewRow
        Caption = 'Cylinders'
        DataBinding.FieldName = 'Cilinders'
        Position.BeginsLayer = False
      end
      object cvHorizontalRow4: TcxGridDBCardViewRow
        Caption = 'Transmission'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = True
      end
      object cvHorizontalTransmissSpeedCount: TcxGridDBCardViewRow
        Caption = 'Speed count'
        DataBinding.FieldName = 'Transmission Speeds'
        Position.BeginsLayer = True
      end
      object cvHorizontalTransmissAutomatic: TcxGridDBCardViewRow
        Caption = 'Automatic'
        DataBinding.FieldName = 'Transmission Type'
        RepositoryItem = dmGridCars.EditRepositoryTransmissionTypeCheckBox
        Position.BeginsLayer = False
      end
      object cvHorizontalRow2: TcxGridDBCardViewRow
        Caption = 'Fuel economy'
        RepositoryItem = EditRepositoryFuelEconomy
        Kind = rkCategory
        Options.Editing = False
        Position.BeginsLayer = True
        Styles.Content = styleCategoryRow
      end
      object cvHorizontalMPG_City: TcxGridDBCardViewRow
        Caption = 'City (mpg)'
        DataBinding.FieldName = 'MPG City'
        Position.BeginsLayer = True
      end
      object cvHorizontalMPG_Highway: TcxGridDBCardViewRow
        Caption = 'Highway (mpg)'
        DataBinding.FieldName = 'MPG Highway'
        Position.BeginsLayer = False
      end
      object cvHorizontalCategory: TcxGridDBCardViewRow
        DataBinding.FieldName = 'CategoryID'
        RepositoryItem = dmGridCars.EditRepositoryCategoryLookup
        Visible = False
        Position.BeginsLayer = True
      end
      object cvHorizontalRow1: TcxGridDBCardViewRow
        Caption = 'Additional information'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = True
      end
      object cvHorizontalDescription: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Description'
        RepositoryItem = dmGridCars.EditRepositoryMemo
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvHorizontalHyperlink: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Hyperlink'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Options.ShowCaption = False
        Position.BeginsLayer = True
      end
      object cvHorizontalPrice: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Styles.Content = stylePrice
      end
    end
    object cvVertical: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = dmGridCars.dsModels
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      LayoutDirection = ldVertical
      OptionsCustomize.LayeredRows = True
      OptionsCustomize.RowMoving = True
      OptionsView.CardAutoWidth = True
      OptionsView.CardIndent = 7
      OptionsView.CardWidth = 10000
      OptionsView.CellAutoHeight = True
      OptionsView.LayerSeparatorWidth = 1
      OptionsView.SeparatorWidth = 0
      RowLayout = rlVertical
      Styles.StyleSheet = CardsStyleSheet
      object cvVerticalTrademark: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Trademark'
        Options.ShowCaption = False
        Position.BeginsLayer = True
        Styles.Content = styleCardHeader
      end
      object cvVerticalModel: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Name'
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Styles.Content = styleCardHeader
      end
      object cvVerticalPicture: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Photo'
        RepositoryItem = EditRepositoryImage
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Position.Width = 250
      end
      object cvVerticalHyperlink: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Hyperlink'
        RepositoryItem = EditRepositoryHyperLink
        Options.ShowCaption = False
        Position.BeginsLayer = False
      end
      object cvVerticalPrice: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Price'
        RepositoryItem = EditRepositoryPrice
        Options.ShowCaption = False
        Position.BeginsLayer = False
        Styles.Content = stylePrice
      end
      object cvVerticalRow1: TcxGridDBCardViewRow
        Caption = 'Engine'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = True
      end
      object cvVerticalHP: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Horsepower'
        Position.BeginsLayer = False
      end
      object cvVerticalLiter: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Torque'
        Position.BeginsLayer = False
      end
      object cvVerticalCyl: TcxGridDBCardViewRow
        Caption = 'Cylinders'
        DataBinding.FieldName = 'Cilinders'
        Position.BeginsLayer = False
      end
      object cvVerticalRow2: TcxGridDBCardViewRow
        Caption = 'Transmission'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = False
      end
      object cvVerticalTransmissSpeedCount: TcxGridDBCardViewRow
        Caption = 'Speed count'
        DataBinding.FieldName = 'Transmission Speeds'
        Position.BeginsLayer = False
      end
      object cvVerticalTransmissAutomatic: TcxGridDBCardViewRow
        Caption = 'Automatic'
        DataBinding.FieldName = 'Transmission Type'
        RepositoryItem = dmGridCars.EditRepositoryTransmissionTypeCheckBox
        Position.BeginsLayer = False
      end
      object cvVerticalCategory: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Category'
        Visible = False
        Position.BeginsLayer = False
      end
      object cvVerticalFuelEconomy: TcxGridDBCardViewRow
        Caption = 'Fuel economy'
        RepositoryItem = EditRepositoryFuelEconomy
        Kind = rkCategory
        Options.Editing = False
        Position.BeginsLayer = False
      end
      object cvVerticalMPG_City: TcxGridDBCardViewRow
        Caption = 'City (mpg)'
        DataBinding.FieldName = 'MPG City'
        Position.BeginsLayer = False
      end
      object cvVerticalMPG_Highway: TcxGridDBCardViewRow
        Caption = 'Highway (mpg)'
        DataBinding.FieldName = 'MPG Highway'
        Position.BeginsLayer = False
      end
      object cvVerticalRow3: TcxGridDBCardViewRow
        Caption = 'Additional information'
        Kind = rkCategory
        Options.ShowData = False
        Position.BeginsLayer = True
      end
      object cvVerticalDescription: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Description'
        RepositoryItem = EditRepositoryMemo
        Options.ShowCaption = False
        Position.BeginsLayer = False
      end
    end
    object GridLevel1: TcxGridLevel
      Caption = 'Horizontal Row Layout + Horizontal Card Layout'
      GridView = cvHorizontal
    end
    object GridLevel2: TcxGridLevel
      Caption = 'Vertical Row Layout + Vertical Card Layout'
      GridView = cvVertical
    end
  end
  inherited sbMain: TStatusBar
    Top = 649
    Width = 747
  end
  inherited mmMain: TMainMenu
    Left = 300
    Top = 116
    object miView: TMenuItem [1]
      Caption = '&View'
      object miCardAutoWidth: TMenuItem
        AutoCheck = True
        Caption = 'Card auto width'
        OnClick = miCardAutoWidthClick
      end
      object miCellSelection: TMenuItem
        AutoCheck = True
        Caption = 'Cell selection'
        OnClick = miCellSelectionClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miCustomize: TMenuItem
        Caption = 'Customize...'
        OnClick = miCustomizeClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 332
    Top = 116
    PixelsPerInch = 96
    object styleSelection: TcxStyle [24]
      AssignedValues = [svColor, svTextColor]
      Color = 5228795
      TextColor = clBlack
    end
    object styleCardHeader: TcxStyle [25]
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
    object styleCardBorder: TcxStyle [26]
      AssignedValues = [svColor]
      Color = 13603685
    end
    object styleBackground: TcxStyle [27]
      AssignedValues = [svColor]
      Color = 15918293
    end
    object styleCategoryRow: TcxStyle [28]
      AssignedValues = [svColor, svFont]
      Color = 16766389
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
    object stylePrice: TcxStyle [29]
      AssignedValues = [svTextColor]
      TextColor = clRed
    end
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
    object CardsStyleSheet: TcxGridCardViewStyleSheet
      Styles.Background = styleBackground
      Styles.Selection = styleSelection
      Styles.CardBorder = styleCardBorder
      Styles.CategoryRow = styleCategoryRow
      Styles.CategorySeparator = styleCardBorder
      Styles.LayerSeparator = styleCardBorder
      BuiltIn = True
    end
  end
  object EditRepository: TcxEditRepository
    Left = 364
    Top = 116
    object EditRepositoryImage: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
    object EditRepositoryMemo: TcxEditRepositoryMemoItem
      Properties.VisibleLineCount = 15
    end
    object EditRepositoryHyperLink: TcxEditRepositoryHyperLinkItem
    end
    object EditRepositoryPrice: TcxEditRepositoryCurrencyItem
    end
    object EditRepositoryFuelEconomy: TcxEditRepositoryTextItem
      Properties.Alignment.Horz = taRightJustify
    end
    object EditRepositoryAutomatic: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Yes'
      Properties.ValueUnchecked = 'No'
    end
  end
end
