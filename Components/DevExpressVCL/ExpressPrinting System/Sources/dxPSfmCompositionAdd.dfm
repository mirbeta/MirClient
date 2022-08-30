object dxfmCompositionAddItems: TdxfmCompositionAddItems
  Left = 429
  Top = 205
  BorderStyle = bsDialog
  Caption = 'Add Items to Composition'
  ClientHeight = 390
  ClientWidth = 330
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 335
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 330
    Height = 390
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lvItems: TcxListView
      Left = 21
      Top = 44
      Width = 288
      Height = 273
      ColumnClick = False
      Columns = <>
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnDblClick = lvItemsDblClick
    end
    object chbxHideIncludedItems: TcxCheckBox
      Left = 21
      Top = 323
      Caption = 'Hide Already Included Items'
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      OnClick = chbxHideIncludedItemsClick
    end
    object btnOK: TcxButton
      Left = 53
      Top = 357
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object btnCancel: TcxButton
      Left = 144
      Top = 357
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnHelp: TcxButton
      Left = 235
      Top = 357
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Help'
      TabOrder = 4
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshItems: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Available Items'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshItems
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvItems
      ControlOptions.OriginalHeight = 264
      ControlOptions.OriginalWidth = 284
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = tshItems
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'chbxHideIncludedItems'
      CaptionOptions.Visible = False
      Control = chbxHideIncludedItems
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 155
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbbtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object pnlNoItems: TcxLabel
    Tag = 20
    Left = 53
    Top = 66
    TabStop = False
    AutoSize = False
    Caption = 'There are no Items do Display'
    ParentColor = False
    ParentFont = False
    Style.HotTrack = False
    Style.TextColor = clGrayText
    Properties.Alignment.Horz = taCenter
    Transparent = True
    Height = 23
    Width = 217
    AnchorX = 162
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 272
    Top = 56
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
