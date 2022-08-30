inherited SummariesDemoMainForm: TSummariesDemoMainForm
  Left = 287
  Top = 147
  Caption = 'ExpressQuantumTreeList SummariesDemo'
  ClientHeight = 565
  ClientWidth = 970
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 970
    Caption = 
      'This demo shows how to calculate built-in and custom summaries u' + 
      'sing the footer and group footer context menus, and how to extend '+
     'these menus with custom options. See Help/About to learn more.'
  end
  object lblSummary: TLabel [1]
    Left = 0
    Top = 530
    Width = 970
    Height = 16
    Align = alBottom
    Color = clInactiveCaptionText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  inherited sbMain: TStatusBar
    Top = 546
    Width = 970
  end
  object tlDepartments: TcxDBTreeList [3]
    Left = 0
    Top = 32
    Width = 970
    Height = 498
    Align = alClient
    Bands = <
      item
        Caption.Text = 'Band + 1'
      end>
    DataController.DataSource = SummariesDemoDataDM.dsDepartments
    DataController.ParentField = 'PARENTID'
    DataController.KeyField = 'ID'
    OptionsBehavior.CellHints = True
    OptionsBehavior.FooterHints = True
    OptionsBehavior.HeaderHints = True
    OptionsSelection.MultiSelect = True
    OptionsView.ColumnAutoWidth = True
    OptionsView.Footer = True
    OptionsView.GroupFooters = tlgfVisibleWhenExpanded
    OptionsView.Indicator = True
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    PopupMenus.FooterMenu.UseBuiltInMenu = True
    PopupMenus.FooterMenu.OnClick = tlDepartmentsPopupMenusFooterMenuClick
    PopupMenus.FooterMenu.OnPopup = tlDepartmentsPopupMenusFooterMenuPopup
    PopupMenus.GroupFooterMenu.UseBuiltInMenu = True
    PopupMenus.GroupFooterMenu.OnClick = tlDepartmentsPopupMenusFooterMenuClick
    PopupMenus.GroupFooterMenu.OnPopup = tlDepartmentsPopupMenusFooterMenuPopup
    RootValue = -1
    Styles.StyleSheet = SummariesDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    OnAfterSummary = tlDepartmentsAfterSummary
    OnSummary = tlDepartmentsSummary
    object clName: TcxDBTreeListColumn
      DataBinding.FieldName = 'NAME'
      Width = 381
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <
        item
          Kind = skCount
          OnGetText = clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText
        end>
      Summary.GroupFooterSummaryItems = <
        item
          Kind = skCount
          OnGetText = clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText
        end>
    end
    object clBudget: TcxDBTreeListColumn
      PropertiesClassName = 'TcxCurrencyEditProperties'
      Properties.DisplayFormat = 'SUM=$,0.00;-$,0.00'
      DataBinding.FieldName = 'BUDGET'
      Width = 157
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <
        item
          Format = 'SUM=$,0.00;-$,0.00'
          Kind = skSum
        end
        item
          Kind = skMin
        end
        item
          Kind = skMax
        end>
      Summary.GroupFooterSummaryItems = <
        item
          Format = 'SUM=$,0.00;-$,0.00'
          Kind = skSum
        end
        item
          Kind = skMin
        end
        item
          Kind = skMax
        end>
    end
    object clPhone: TcxDBTreeListColumn
      DataBinding.FieldName = 'PHONE'
      Width = 95
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object clFax: TcxDBTreeListColumn
      DataBinding.FieldName = 'FAX'
      Width = 105
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object clEmail: TcxDBTreeListColumn
      DataBinding.FieldName = 'EMAIL'
      Width = 118
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object clVacancy: TcxDBTreeListColumn
      DataBinding.FieldName = 'VACANCY'
      Width = 132
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  object ilUser: TcxImageList
    FormatVersion = 1
    DesignInfo = 524824
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000000000000000FF0000339999003399
          990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000FF000000FF0000339999003399
          990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000FF000000FF0000339999003399
          990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000FF000000FF0000339999003399
          990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000FF000000FF0000339999003399
          990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF000000000000FF000000FF000000FF00003399
          99003399990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000FF000000FF000000FF000000FF00003399
          99003399990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000FF000000FF000000FF000000FF00003399
          99003399990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000FF000000FF000000FF000000FF00003399
          99003399990000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000000000000FF00000080000000800000008000000080
          00000080000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000000000008000000000000000000000000000000000
          000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000B2B2B200DDDDDD00B2B2B2000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0000000000FFFFFF00EAEAEA00DDDDDD000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF0022222200B2B2B200EAEAEA00B2B2B2000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00000000001616160000000000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        MaskColor = clFuchsia
      end>
  end
end
