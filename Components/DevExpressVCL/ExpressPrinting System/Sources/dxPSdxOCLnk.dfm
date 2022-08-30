inherited dxOCReportLinkDesignWindow: TdxOCReportLinkDesignWindow
  Left = 318
  Top = 256
  Caption = 'Report Designer'
  ClientHeight = 361
  ClientWidth = 577
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 577
    Height = 361
    inherited btnApply: TcxButton
      Left = 692
      Top = 302
      TabOrder = 10
    end
    inherited btnCancel: TcxButton
      Left = 611
      Top = 302
      TabOrder = 9
    end
    inherited btnOK: TcxButton
      Left = 530
      Top = 302
      TabOrder = 8
    end
    inherited btnHelp: TcxButton
      Left = 773
      Top = 302
      TabOrder = 11
    end
    inherited btnRestoreOriginal: TcxButton
      Left = 10
      Top = 302
      TabOrder = 12
    end
    inherited btnRestoreDefaults: TcxButton
      Left = 136
      Top = 302
      TabOrder = 13
    end
    inherited btnTitleProperties: TcxButton
      Left = 262
      Top = 302
      TabOrder = 14
    end
    inherited btnFootnoteProperties: TcxButton
      Left = 388
      Top = 302
      TabOrder = 15
    end
    object chbxFullExpand: TcxCheckBox [8]
      Left = 21
      Top = 44
      Caption = 'Full &Expand'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
      OnClick = chbxFullExpandClick
    end
    object chbxDrawBorder: TcxCheckBox [9]
      Left = 21
      Top = 67
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      OnClick = chbxDrawBorderClick
    end
    object stDrawBorder: TcxLabel [10]
      Left = 44
      Top = 67
      TabStop = False
      AutoSize = False
      Caption = ' Draw Border '
      FocusControl = chbxDrawBorder
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      OnClick = stDrawBorderClick
      Height = 18
      Width = 287
      AnchorY = 76
    end
    object pnlPreview: TPanel [11]
      Left = 349
      Top = 29
      Width = 498
      Height = 250
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 7
      object ocPreview: TdxOrgChart
        Left = 0
        Top = 0
        Width = 498
        Height = 250
        BorderStyle = bsNone
        DefaultImageAlign = iaLT
        Options = [ocDblClick, ocEdit, ocCanDrag, ocShowDrag, ocRect3D]
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Visible = False
        Items = {
          564552312E3055000001005A000000FFFFFF1F0100020001160043006F007200
          70006F0072006100740065002000480065006100640071007500610072007400
          650072007300020050000000FFFFFF1F01000100011300530061006C00650073
          00200061006E00640020004D00610072006B006500740069006E006700010050
          000000FFFFFF1F010003000117004600690065006C00640020004F0066006600
          6900630065003A002000430061006E006100640061000D000A00090000005000
          0000FFFFFF1F01000000010B0045006E00670069006E0065006500720069006E
          0067000000}
      end
    end
    object ccbxGridLineColor: TcxColorComboBox [12]
      Tag = 1
      Left = 131
      Top = 91
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 3
      Width = 200
    end
    object chbxTransparent: TcxCheckBox [13]
      Left = 21
      Top = 118
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxTransparentClick
    end
    object stTransparent: TcxLabel [14]
      Left = 44
      Top = 118
      TabStop = False
      AutoSize = False
      Caption = ' &Transparent '
      FocusControl = chbxTransparent
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      OnClick = stTransparentClick
      Height = 18
      Width = 287
      AnchorY = 127
    end
    object ccbxColor: TcxColorComboBox [15]
      Left = 131
      Top = 142
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 6
      Width = 200
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshOptions'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = tshOptions
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Visible = False
      Control = chbxFullExpand
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 79
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxDrawBorder
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = stDrawBorder
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = liTransparent
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = liTransparent
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblColor: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'lblColor'
      Offsets.Left = 27
      Control = ccbxColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liTransparent: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
