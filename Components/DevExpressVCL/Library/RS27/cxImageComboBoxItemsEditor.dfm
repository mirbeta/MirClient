object fmImageComboBoxItemsEditor: TfmImageComboBoxItemsEditor
  Left = 287
  Top = 190
  Caption = 'fmImageComboBoxItemsEditor'
  ClientHeight = 266
  ClientWidth = 585
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 581
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 585
    Height = 266
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cxgImageComboBoxItems: TcxGrid
      Left = 10
      Top = 10
      Width = 476
      Height = 246
      TabOrder = 0
      object tvImageComboBoxItems: TcxGridTableView
        OnKeyDown = tvImageComboBoxItemsKeyDown
        Navigator.Buttons.CustomButtons = <>
        OnEditKeyDown = tvImageComboBoxItemsEditKeyDown
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        DataController.OnRecordChanged = tvImageComboBoxItemsDataControllerRecordChanged
        DataController.Data = {
          9D0000000F00000044617461436F6E74726F6C6C657231050000001300000054
          6378496E746567657256616C75655479706512000000546378537472696E6756
          616C75655479706512000000546378537472696E6756616C7565547970651200
          0000546378537472696E6756616C75655479706513000000546378496E746567
          657256616C75655479706501000000445855464D540001010001000000300001
          01}
        OptionsBehavior.CellHints = True
        OptionsBehavior.ColumnHeaderHints = False
        OptionsBehavior.ImmediateEditor = False
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnGrouping = False
        OptionsCustomize.ColumnHidingOnGrouping = False
        OptionsCustomize.ColumnMoving = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Inserting = False
        OptionsSelection.MultiSelect = True
        OptionsSelection.UnselectFocusedRecordOnExit = False
        OptionsView.CellEndEllipsis = True
        OptionsView.ShowEditButtons = gsebAlways
        OptionsView.ColumnAutoWidth = True
        OptionsView.ExpandButtonsForEmptyDetails = False
        OptionsView.GroupByBox = False
        object clnImage: TcxGridColumn
          Caption = 'Image'
          DataBinding.ValueType = 'Integer'
          PropertiesClassName = 'TcxImageComboBoxProperties'
          Properties.ImmediatePost = True
          Properties.Items = <
            item
            end>
          MinWidth = 60
          Options.HorzSizing = False
          Width = 60
        end
        object clnDescription: TcxGridColumn
          Caption = 'Description'
          Width = 168
        end
        object clnValue: TcxGridColumn
          Caption = 'Value'
          PropertiesClassName = 'TcxTextEditProperties'
          Width = 109
        end
        object clnValueType: TcxGridColumn
          Caption = 'ValueType'
          PropertiesClassName = 'TcxComboBoxProperties'
          Properties.DropDownListStyle = lsFixedList
          Properties.ImmediatePost = True
          Properties.OnEditValueChanged = clnValueTypePropertiesEditValueChanged
          Width = 117
        end
        object clnTag: TcxGridColumn
          Caption = 'Tag'
          DataBinding.ValueType = 'Integer'
          PropertiesClassName = 'TcxMaskEditProperties'
          Properties.MaskKind = emkRegExprEx
          Properties.EditMask = '[-]?\d+'
          Width = 51
        end
      end
      object lvImageComboBoxItems: TcxGridLevel
        GridView = tvImageComboBoxItems
      end
    end
    object btnAdd: TcxButton
      Left = 492
      Top = 10
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnInsert: TcxButton
      Left = 492
      Top = 41
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Insert'
      TabOrder = 2
      OnClick = btnInsertClick
    end
    object btnDelete: TcxButton
      Left = 492
      Top = 72
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnSelectAll: TcxButton
      Left = 492
      Top = 103
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Select All'
      TabOrder = 4
      OnClick = btnSelectAllClick
    end
    object btnValueType: TcxButton
      Left = 492
      Top = 134
      Width = 83
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Set Value&Type'
      DropDownMenu = mnuValueTypes
      Kind = cxbkDropDown
      TabOrder = 5
    end
    object btnOk: TcxButton
      Left = 492
      Top = 200
      Width = 83
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 6
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton
      Left = 492
      Top = 231
      Width = 83
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 7
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cxgImageComboBoxItems
      ControlOptions.OriginalHeight = 266
      ControlOptions.OriginalWidth = 480
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnInsert'
      CaptionOptions.Visible = False
      Control = btnInsert
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnSelectAll'
      CaptionOptions.Visible = False
      Control = btnSelectAll
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnValueType'
      CaptionOptions.Visible = False
      Control = btnValueType
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 6
    end
  end
  object mnuValueTypes: TPopupMenu
    Left = 390
    Top = 138
    object miAdd: TMenuItem
      Caption = '&Add'
      OnClick = miValueTypeClick
    end
    object miInsert: TMenuItem
      Caption = '&Insert'
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miHelp: TMenuItem
      Caption = '&Help'
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 56
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
      PixelsPerInch = 96
    end
  end
end
