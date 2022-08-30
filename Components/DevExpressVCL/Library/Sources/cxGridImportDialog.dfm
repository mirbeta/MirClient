object ImportDialog: TImportDialog
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Import'
  ClientHeight = 321
  ClientWidth = 646
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 646
    Height = 321
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object lbComponentsForImport: TcxListBox
      Left = 10
      Top = 10
      Width = 219
      Height = 300
      ItemHeight = 13
      ListStyle = lbOwnerDrawVariable
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbComponentsForImportClick
      OnDblClick = lbComponentsForImportDblClick
    end
    object btnImport: TcxButton
      Left = 235
      Top = 10
      Width = 80
      Height = 24
      Caption = 'Import ->'
      Enabled = False
      TabOrder = 1
      OnClick = btnImportClick
    end
    object btnClose: TcxButton
      Left = 235
      Top = 40
      Width = 80
      Height = 24
      Cancel = True
      Caption = 'Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
    object cbDeleteAllSublevels: TcxCheckBox
      Left = 332
      Top = 283
      Caption = 'Delete All Sublevels'
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
    end
    object cbImportStyles: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Import Styles'
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Visible = False
      OnClick = cbImportStylesClick
    end
    object rbCreateNewStyleRepository: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 169
      Height = 17
      Caption = 'Create New StyleRepository'
      Checked = True
      TabOrder = 5
      TabStop = True
      Visible = False
      OnClick = rbStyleRepositoryClick
      Transparent = True
    end
    object edNewStyleRepository: TcxTextEdit
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Visible = False
      Width = 273
    end
    object rbUseExistingStyleRepository: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 173
      Height = 17
      Caption = 'Use Existing StyleRepository'
      TabOrder = 7
      TabStop = True
      Visible = False
      OnClick = rbStyleRepositoryClick
      Transparent = True
    end
    object cbStyleRepositories: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Visible = False
      Width = 273
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = lbComponentsForImport
      ControlOptions.OriginalHeight = 300
      ControlOptions.OriginalWidth = 219
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnImport'
      CaptionOptions.Visible = False
      Control = btnImport
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 24
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 2
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = dxLayoutGroup8
      CaptionOptions.Text = '  Levels  '
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 0
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutLabeledItem1: TdxLayoutLabeledItem
      Parent = dxLayoutGroup10
      AlignVert = avClient
      CaptionOptions.Text = 'Destination Level: '
      Index = 0
    end
    object lblLevelName: TdxLayoutLabeledItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = '-'
      LayoutLookAndFeel = dxLayoutCxLookAndFeelBold
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'cbDeleteAllSublevels'
      CaptionOptions.Visible = False
      Control = cbDeleteAllSublevels
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutGroup8
      CaptionOptions.Text = '  Styles  '
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbImportStyles'
      CaptionOptions.Visible = False
      Control = cbImportStyles
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      Offsets.Left = 20
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'rbCreateNewStyleRepository'
      CaptionOptions.Visible = False
      Control = rbCreateNewStyleRepository
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 169
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      Control = edNewStyleRepository
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'rbUseExistingStyleRepository'
      CaptionOptions.Visible = False
      Control = rbUseExistingStyleRepository
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      Control = cbStyleRepositories
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liGridStructureHost: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignVert = avClient
      CaptionOptions.Text = ' '
      CaptionOptions.Layout = clBottom
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 568
    Top = 8
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
    object dxLayoutCxLookAndFeelBold: TdxLayoutCxLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clNavy
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Tahoma'
      ItemOptions.CaptionOptions.Font.Style = [fsBold]
      ItemOptions.CaptionOptions.UseDefaultFont = False
      PixelsPerInch = 96
    end
  end
end
