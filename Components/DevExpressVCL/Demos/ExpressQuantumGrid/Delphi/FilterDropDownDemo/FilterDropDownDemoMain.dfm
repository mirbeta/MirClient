inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Filter Dropdown Demo'
  ClientHeight = 551
  ClientWidth = 1041
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 1041
    Height = 48
    Caption = 
      'This demo illustrates the filter dropdown'#39's capabilities. Hover ' +
      'the mouse pointer over a column header and click its filter butt' +
      'on to invoke the column'#39's filter dropdown. You can use its UI to' +
      ' create, customize, or apply filter conditions to the grid. Sele' +
      'ct Filter Options in the main menu to choose between Excel-inspi' +
      'red and Classic modes and access their options. Click Help | Abo' +
      'ut this demo for more information.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 1041
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 48
    Width = 1041
    Height = 484
    Align = alClient
    TabOrder = 1
    object TableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = True
      Navigator.Buttons.PriorPage.Visible = True
      Navigator.Buttons.Prior.Visible = True
      Navigator.Buttons.Next.Visible = True
      Navigator.Buttons.NextPage.Visible = True
      Navigator.Buttons.Last.Visible = True
      Navigator.Buttons.Insert.Visible = True
      Navigator.Buttons.Append.Visible = False
      Navigator.Buttons.Delete.Visible = True
      Navigator.Buttons.Edit.Visible = True
      Navigator.Buttons.Post.Visible = True
      Navigator.Buttons.Cancel.Visible = True
      Navigator.Buttons.Refresh.Visible = True
      Navigator.Buttons.SaveBookmark.Visible = True
      Navigator.Buttons.GotoBookmark.Visible = True
      Navigator.Buttons.Filter.Visible = True
      Navigator.Visible = True
      FindPanel.DisplayMode = fpdmManual
      FindPanel.UseExtendedSyntax = True
      DataController.DataSource = dmGridCars.dsCarOrders
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Filtering.ColumnPopupMode = fpmExcel
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object TableViewRecId: TcxGridDBColumn
        DataBinding.FieldName = 'RecId'
        Visible = False
      end
      object TableViewID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Visible = False
      end
      object TableViewTrademark: TcxGridDBColumn
        DataBinding.FieldName = 'Trademark'
        Width = 157
      end
      object TableViewName: TcxGridDBColumn
        DataBinding.FieldName = 'Name'
        Width = 214
      end
      object TableViewBodyStyle: TcxGridDBColumn
        DataBinding.FieldName = 'BodyStyle'
        Width = 125
      end
      object TableViewMPGCity: TcxGridDBColumn
        DataBinding.FieldName = 'MPG City'
        Width = 66
      end
      object TableViewMPGHighway: TcxGridDBColumn
        DataBinding.FieldName = 'MPG Highway'
        Width = 82
      end
      object TableViewCilinders: TcxGridDBColumn
        DataBinding.FieldName = 'Cilinders'
        Width = 51
      end
      object TableViewModification: TcxGridDBColumn
        DataBinding.FieldName = 'Modification'
        Width = 171
      end
      object TableViewPrice: TcxGridDBColumn
        DataBinding.FieldName = 'Price'
        PropertiesClassName = 'TcxCurrencyEditProperties'
        Width = 82
      end
      object TableViewBodyStyleID: TcxGridDBColumn
        DataBinding.FieldName = 'BodyStyleID'
        Visible = False
      end
      object TableViewSalesDate: TcxGridDBColumn
        DataBinding.FieldName = 'SalesDate'
        Width = 83
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = TableView
    end
  end
  inherited mmMain: TMainMenu
    Left = 220
    Top = 192
    object miFilterPopup: TMenuItem [1]
      Caption = 'Filter &Options'
      object miFilterPopupMode: TMenuItem
        Action = acFilterPopupMode
        object miFilterPopupModeClassic: TMenuItem
          Action = acFilterPopupModeClassic
          AutoCheck = True
          GroupIndex = 1
          RadioItem = True
        end
        object miFilterPopupModeExcel: TMenuItem
          Action = acFilterPopupModeExcel
          AutoCheck = True
          GroupIndex = 1
          RadioItem = True
        end
      end
      object miSeparator: TMenuItem
        Caption = '-'
      end
      object miClassicModeMultiSelect: TMenuItem
        Tag = 1
        Action = acClassicModeMultiSelect
        AutoCheck = True
        Caption = 'Multi Select'
      end
      object miClassicModeApplyChanges: TMenuItem
        Tag = 1
        Action = acClassicModeApplyChanges
        object miClassicModeApplyChangesImmediatly: TMenuItem
          Action = acClassicModeApplyChangesImmediately
          AutoCheck = True
        end
        object miClassicModeApplyChangesButtonClick: TMenuItem
          Action = acClassicModeApplyChangesOnButtonClick
          AutoCheck = True
        end
      end
      object miExcelModeApplyChanges: TMenuItem
        Tag = 2
        Action = acExcelModeApplyChanges
        object miExcelModeApplyChangesImmediatly: TMenuItem
          Action = acExcelModeApplyChangesImmediately
          AutoCheck = True
        end
        object miExcelModeTabOrOKButtonClick: TMenuItem
          Action = acExcelModeApplyChangesOnTabOrOKButtonClick
          AutoCheck = True
        end
      end
      object miExcelModeDateTimePage: TMenuItem
        Tag = 2
        Action = acExcelModeDateTimePageType
        object miDateTimePageTree: TMenuItem
          Action = acExcelModeDateTimePageTypeTree
          AutoCheck = True
        end
        object miDateTimePageList: TMenuItem
          Action = acExcelModeDateTimePageTypeList
          AutoCheck = True
        end
      end
      object miExcelModeNumericPage: TMenuItem
        Tag = 2
        Action = acExcelModeNumericPageType
        object miNumericPageTree: TMenuItem
          Action = acExcelModeNumericPageTypeRange
          AutoCheck = True
        end
        object miNumericPageList: TMenuItem
          Action = acExcelModeNumericPageTypeList
          AutoCheck = True
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 128
    Top = 192
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 24
    Top = 192
  end
  object alAction: TActionList
    Left = 304
    Top = 192
    object acFilterPopupModeClassic: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Classic'
      GroupIndex = 1
      OnExecute = acFilterPopupModeExecute
    end
    object acFilterPopupModeExcel: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Excel'
      Checked = True
      GroupIndex = 1
      OnExecute = acFilterPopupModeExecute
    end
    object acExcelModeApplyChangesImmediately: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Immediately'
      Checked = True
      GroupIndex = 2
      OnExecute = acExcelModeApplyChangesExecute
    end
    object acExcelModeApplyChangesOnTabOrOKButtonClick: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'On Tab Or OK Button Click'
      GroupIndex = 2
      OnExecute = acExcelModeApplyChangesExecute
    end
    object acExcelModeDateTimePageTypeTree: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Tree'
      Checked = True
      GroupIndex = 3
      OnExecute = acDateTimePageTypeExecute
    end
    object acExcelModeDateTimePageTypeList: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'List'
      GroupIndex = 3
      OnExecute = acDateTimePageTypeExecute
    end
    object acExcelModeNumericPageTypeRange: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Range'
      Checked = True
      GroupIndex = 4
      OnExecute = acNumericPageTypeExecute
    end
    object acExcelModeNumericPageTypeList: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'List'
      GroupIndex = 4
      OnExecute = acNumericPageTypeExecute
    end
    object acClassicModeApplyChangesImmediately: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'Immediately'
      Checked = True
      GroupIndex = 5
      OnExecute = acClassicModeApplyChangesExecute
    end
    object acClassicModeApplyChangesOnButtonClick: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'On Button Click'
      GroupIndex = 5
      OnExecute = acClassicModeApplyChangesExecute
    end
    object acClassicModeMultiSelect: TAction
      Category = 'FilterPopup'
      AutoCheck = True
      Caption = 'acClassicModeMultiSelect'
      Checked = True
      Visible = False
      OnExecute = acClassicModeMultiSelectExecute
    end
    object acExcelModeDateTimePageType: TAction
      Category = 'FilterPopup'
      Caption = 'Date Time Values Page Style'
      OnExecute = acDoNothingExecute
    end
    object acExcelModeApplyChanges: TAction
      Category = 'FilterPopup'
      Caption = 'Apply Changes'
      OnExecute = acDoNothingExecute
    end
    object acExcelModeNumericPageType: TAction
      Category = 'FilterPopup'
      Caption = 'Numeric Values Page Style'
      OnExecute = acDoNothingExecute
    end
    object acClassicModeApplyChanges: TAction
      Category = 'FilterPopup'
      Caption = 'Apply Changes'
      Visible = False
      OnExecute = acDoNothingExecute
    end
    object acFilterPopupMode: TAction
      Category = 'FilterPopup'
      Caption = 'Mode (Excel)'
      OnExecute = acDoNothingExecute
    end
  end
end
