object ColumnsSimpleDemoCitiesForm: TColumnsSimpleDemoCitiesForm
  Left = 435
  Top = 222
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Choose a City'
  ClientHeight = 232
  ClientWidth = 307
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescription: TLabel
    Left = 0
    Top = 0
    Width = 307
    Height = 25
    Align = alTop
    AutoSize = False
    Caption = '  Type city name to incremental search'
    Color = 4707838
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
  end
  object pnlCustomize: TPanel
    Left = 216
    Top = 25
    Width = 91
    Height = 207
    Align = alRight
    Color = 15451300
    TabOrder = 1
    object btnSet: TcxButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Set'
      ModalResult = 1
      TabOrder = 0
    end
    object btnAdd: TcxButton
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnDelete: TcxButton
      Left = 8
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnCancel: TcxButton
      Left = 8
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
  object GridCities: TcxGrid
    Left = 0
    Top = 25
    Width = 216
    Height = 207
    Align = alClient
    TabOrder = 0
    object tvCities: TcxGridDBTableView
      OnKeyDown = tvCitiesKeyDown
      NavigatorButtons.ConfirmDelete = False
      DataController.DataSource = ColumnsSimpleDemoDataDM.dsCities
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.ImmediateEditor = False
      OptionsBehavior.IncSearch = True
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnHorzSizing = False
      OptionsCustomize.ColumnMoving = False
      OptionsSelection.InvertSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GridLines = glNone
      OptionsView.GroupByBox = False
      object tvCitiesCity: TcxGridDBColumn
        DataBinding.FieldName = 'City'
        MinWidth = 100
        SortIndex = 0
        SortOrder = soAscending
        Width = 100
      end
    end
    object lvCities: TcxGridLevel
      GridView = tvCities
    end
  end
end
