object dxStatusBarIndicatorEditor: TdxStatusBarIndicatorEditor
  Left = 624
  Top = 110
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 241
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 281
    Height = 241
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object imgExample: TImage
      Left = 198
      Top = 139
      Width = 34
      Height = 15
      Center = True
    end
    object chlbIndicators: TcxCheckListBox
      Left = 10
      Top = 10
      Width = 153
      Height = 149
      Items = <>
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = chlbIndicatorsClick
      OnKeyDown = chlbIndicatorsKeyDown
    end
    object cbItemTypes: TcxComboBox
      Left = 10
      Top = 165
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        'sitOff'
        'sitYellow'
        'sitBlue'
        'sitGreen'
        'sitRed'
        'sitTeal'
        'sitPurple')
      Properties.OnChange = cbItemTypesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 153
    end
    object BtnAdd: TcxButton
      Left = 169
      Top = 10
      Width = 92
      Height = 25
      Caption = '&Add'
      TabOrder = 2
      OnClick = BtnAddClick
    end
    object BtnDelete: TcxButton
      Left = 169
      Top = 41
      Width = 92
      Height = 25
      Caption = '&Delete'
      TabOrder = 3
      OnClick = BtnDeleteClick
    end
    object BtnClear: TcxButton
      Left = 169
      Top = 72
      Width = 92
      Height = 25
      Caption = '&Clear'
      TabOrder = 4
      OnClick = BtnClearClick
    end
    object BtnOK: TcxButton
      Left = 57
      Top = 202
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object BtnCancel: TcxButton
      Left = 138
      Top = 202
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Control = chlbIndicators
      ControlOptions.OriginalHeight = 149
      ControlOptions.OriginalWidth = 153
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Control = cbItemTypes
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 153
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'BtnAdd'
      CaptionOptions.Visible = False
      Control = BtnAdd
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'BtnDelete'
      CaptionOptions.Visible = False
      Control = BtnDelete
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'BtnClear'
      CaptionOptions.Visible = False
      Control = BtnClear
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'BtnOK'
      CaptionOptions.Visible = False
      Control = BtnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup5
      CaptionOptions.Text = 'BtnCancel'
      CaptionOptions.Visible = False
      Control = BtnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liExample: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahCenter
      AlignVert = avCenter
      Control = imgExample
      ControlOptions.OriginalHeight = 15
      ControlOptions.OriginalWidth = 34
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Example'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      Index = 1
      AutoCreated = True
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
