object EditorsInPlaceDemoCitiesForm: TEditorsInPlaceDemoCitiesForm
  Left = 354
  Top = 217
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select a City'
  ClientHeight = 275
  ClientWidth = 277
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescription: TLabel
    Left = 0
    Top = 0
    Width = 277
    Height = 18
    Align = alTop
    AutoSize = False
    Caption = 'Type a name to see incremental search in action'
    Color = 4707838
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object GridCities: TcxGrid
    Left = 0
    Top = 18
    Width = 277
    Height = 214
    Align = alTop
    TabOrder = 0
    object tvCities: TcxGridDBTableView
      OnKeyDown = tvCitiesKeyDown
      DataController.DataSource = EditorsInPlaceDemoDataDM.dsCities
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      NavigatorButtons.ConfirmDelete = False
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.IncSearch = True
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnHorzSizing = False
      OptionsCustomize.ColumnMoving = False
      OptionsSelection.InvertSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GridLines = glNone
      OptionsView.GroupByBox = False
      Styles.StyleSheet = EditorsInPlaceDemoDataDM.GridTableViewStyleSheetDevExpress
      object tvCitiesCity: TcxGridDBColumn
        DataBinding.FieldName = 'City'
        MinWidth = 604
        SortOrder = soAscending
      end
    end
    object lvCities: TcxGridLevel
      GridView = tvCities
    end
  end
  object btnCancel: TcxButton
    Left = 112
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TcxButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
