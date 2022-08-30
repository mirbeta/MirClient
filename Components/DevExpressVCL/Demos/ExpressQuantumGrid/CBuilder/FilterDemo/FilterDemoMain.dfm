inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Filter Demo'
  ClientHeight = 542
  ClientWidth = 704
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 704
    Caption = 
      '  This demo shows the advanced filter capabilities of the Table ' +
      'View'
  end
  inherited sbMain: TStatusBar
    Top = 523
    Width = 704
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 44
    Width = 704
    Height = 479
    Align = alClient
    TabOrder = 0
    object TableView: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = True
      Navigator.Buttons.PriorPage.Visible = True
      Navigator.Buttons.Prior.Visible = True
      Navigator.Buttons.Next.Visible = True
      Navigator.Buttons.NextPage.Visible = True
      Navigator.Buttons.Last.Visible = True
      Navigator.Buttons.Insert.Visible = True
      Navigator.Buttons.Delete.Visible = True
      Navigator.Buttons.Edit.Visible = True
      Navigator.Buttons.Post.Visible = True
      Navigator.Buttons.Cancel.Visible = True
      Navigator.Buttons.Refresh.Visible = True
      Navigator.Buttons.SaveBookmark.Visible = True
      Navigator.Buttons.GotoBookmark.Visible = True
      Navigator.Buttons.Filter.Visible = True
      DataController.Filter.Options = [fcoCaseInsensitive]
      DataController.Filter.PercentWildcard = '*'
      DataController.Filter.UnderscoreWildcard = '?'
      DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoSortByDisplayText]
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DateTimeHandling.Filters = [dtfRelativeDays, dtfRelativeDayPeriods, dtfRelativeWeeks, dtfRelativeMonths, dtfRelativeYears, dtfPastFuture, dtfMonths, dtfYears]
      DateTimeHandling.IgnoreTimeForFiltering = True
      DateTimeHandling.Grouping = dtgRelativeToToday
      Filtering.ColumnPopup.MaxDropDownItemCount = 30
      FilterRow.Visible = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object TableViewOrderID: TcxGridColumn
        Caption = 'Order ID'
        DataBinding.ValueType = 'Integer'
        Width = 49
      end
      object TableViewCompany: TcxGridColumn
        Caption = 'Company'
        Width = 205
      end
      object TableViewCountry: TcxGridColumn
        Caption = 'Country'
        DataBinding.ValueType = 'Integer'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.ClearKey = 46
        Properties.DropDownAutoSize = True
        Properties.DropDownRows = 25
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            Caption = 'Name'
            Fixed = True
            SortOrder = soAscending
            Width = 265
            FieldName = 'NAME'
          end
          item
            Caption = 'Flag'
            Fixed = True
            RepositoryItem = erMainFlag
            Width = 60
            FieldName = 'NATIONALFLAG'
          end>
        Properties.ListOptions.GridLines = glHorizontal
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = dsCountries
        Width = 110
      end
      object TableViewProduct: TcxGridColumn
        Caption = 'Product'
        Width = 130
      end
      object TableViewOrderDate: TcxGridColumn
        Caption = 'Order Date'
        DataBinding.ValueType = 'DateTime'
        Width = 130
      end
      object TableViewQuantity: TcxGridColumn
        Caption = 'Quantity'
        DataBinding.ValueType = 'Integer'
        Width = 50
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = TableView
    end
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 16
    Width = 704
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    Color = clGray
    TabOrder = 1
    object pnlMaskInfo: TPanel
      Left = 1
      Top = 1
      Width = 702
      Height = 26
      Align = alClient
      BevelOuter = bvNone
      Caption = 
        'You can use * and ? symbols in the filter row to define a filter' +
        ' mask (for example: Express* for all products beginning with Exp' +
        'ress)'
      Color = clInfoBk
      TabOrder = 0
    end
  end
  inherited mmMain: TMainMenu
    Left = 300
    Top = 120
    object miView: TMenuItem [1]
      Caption = '&View'
      object miColumnFilterPopupMultiSelect: TMenuItem
        Caption = 'Column Filter Popup MultiSelect'
        Checked = True
        OnClick = miColumnFilterPopupMultiSelectClick
      end
      object miApplyMultiSelectChanges: TMenuItem
        Caption = 'Apply MultiSelect Changes'
        object miApplyMultiSelectChangesImmediately: TMenuItem
          Caption = 'Immediately'
          GroupIndex = 1
          RadioItem = True
          OnClick = miApplyMultiSelectChangesClick
        end
        object miApplyMultiSelectChangesOnButtonClick: TMenuItem
          Tag = 1
          Caption = 'On Button Click'
          GroupIndex = 1
          RadioItem = True
          OnClick = miApplyMultiSelectChangesClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miColumnFilterPopupFilteredList: TMenuItem
        Caption = 'Column Filter Popup Filtered List'
        Checked = True
        OnClick = miColumnFilterPopupFilteredListClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miFilterRow: TMenuItem
        Caption = 'Filter Row'
        Checked = True
        OnClick = miFilterRowClick
      end
      object miApplyFilterRowChanges: TMenuItem
        Caption = 'Apply Filter Row Changes'
        object miApplyFilterRowChangesOnCellExit: TMenuItem
          Caption = 'On cell exit'
          GroupIndex = 2
          RadioItem = True
          OnClick = miApplyFilterRowChangesClick
        end
        object miApplyFilterRowChangesImmediately: TMenuItem
          Tag = 1
          Caption = 'Immediately'
          GroupIndex = 2
          RadioItem = True
          OnClick = miApplyFilterRowChangesClick
        end
      end
      object miAllowOperatorCustomization: TMenuItem
        AutoCheck = True
        Caption = 'Allow Filter Operator Customization'
        OnClick = miAllowOperatorCustomizationClick
      end
    end
    object miDateTimeFilters: TMenuItem [2]
      Caption = '&DateTime Filters'
      object miDateTimeFilterRelativeDays: TMenuItem
        Caption = 'Relative Days'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterRelativeDayPeriods: TMenuItem
        Tag = 1
        Caption = 'Relative Day Periods'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterRelativeWeeks: TMenuItem
        Tag = 2
        Caption = 'Relative Weeks'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterRelativeMonths: TMenuItem
        Tag = 3
        Caption = 'Relative Months'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterRelativeYears: TMenuItem
        Tag = 4
        Caption = 'Relative Years'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterPastFuture: TMenuItem
        Tag = 5
        Caption = 'Past/Future'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterMonths: TMenuItem
        Tag = 6
        Caption = 'Months'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
      object miDateTimeFilterYears: TMenuItem
        Tag = 7
        Caption = 'Years'
        Checked = True
        OnClick = miDateTimeFilterClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
  end
  object dsCompanies: TDataSource
    DataSet = cdsCompanies
    Left = 232
    Top = 156
  end
  object dsCountries: TDataSource
    DataSet = cdsCountries
    Left = 232
    Top = 188
  end
  object erMain: TcxEditRepository
    Left = 300
    Top = 156
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object cdsCompanies: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\COMPANIES.xml'
    Params = <>
    Left = 184
    Top = 152
    object cdsCompaniesID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsCompaniesCOMPANYTYPEID: TIntegerField
      FieldName = 'COMPANYTYPEID'
    end
    object cdsCompaniesCOUNTRYID: TIntegerField
      FieldName = 'COUNTRYID'
    end
    object cdsCompaniesCOMPANYNAME: TStringField
      FieldName = 'COMPANYNAME'
      Size = 50
    end
    object cdsCompaniesCOMPANYWEBSITE: TStringField
      FieldName = 'COMPANYWEBSITE'
      Size = 50
    end
  end
  object cdsCountries: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\COUNTRIES.xml'
    Params = <>
    Left = 184
    Top = 184
    object cdsCountriesID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsCountriesNAME: TStringField
      FieldName = 'NAME'
      Size = 60
    end
    object cdsCountriesACRONYM: TStringField
      FieldName = 'ACRONYM'
      Size = 50
    end
    object cdsCountriesNATIONALFLAG: TBlobField
      FieldName = 'NATIONALFLAG'
      BlobType = ftParadoxOle
      Size = 10
    end
  end
  object cdsProducts: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\PRODUCTS.xml'
    Params = <>
    Left = 184
    Top = 224
    object cdsProductsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsProductsName: TStringField
      FieldName = 'Name'
      Size = 100
    end
    object cdsProductsDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
      Size = 10
    end
    object cdsProductsPlatform: TStringField
      FieldName = 'Platform'
    end
    object cdsProductsLogo: TBlobField
      FieldName = 'Logo'
      BlobType = ftParadoxOle
      Size = 10
    end
    object cdsProductsLink: TMemoField
      FieldName = 'Link'
      BlobType = ftMemo
      Size = 10
    end
  end
end
