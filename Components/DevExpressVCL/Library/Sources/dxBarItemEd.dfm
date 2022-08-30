object dxBarItemAddEditor: TdxBarItemAddEditor
  Left = 352
  Top = 153
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Add New ExpressBars Item'
  ClientHeight = 161
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 300
    Height = 161
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object ComboBox1: TcxComboBox
      Left = 64
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = ComboBox1Click
      Width = 194
    end
    object ComboBox2: TcxComboBox
      Left = 64
      Top = 35
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Width = 194
    end
    object Edit1: TcxTextEdit
      Left = 64
      Top = 60
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 194
    end
    object Edit2: TcxTextEdit
      Left = 64
      Top = 85
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Text = 'New Item'
      Width = 194
    end
    object BOk: TcxButton
      Left = 58
      Top = 110
      Width = 73
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnClick = BOkClick
    end
    object BCancel: TcxButton
      Left = 137
      Top = 110
      Width = 73
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Type:'
      Control = ComboBox1
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 194
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Category:'
      Control = ComboBox2
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 194
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Name:'
      Control = Edit1
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 194
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Caption:'
      Control = Edit2
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 194
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'BOk'
      CaptionOptions.Visible = False
      Control = BOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'BCancel'
      CaptionOptions.Visible = False
      Control = BCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
