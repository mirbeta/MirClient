object cxShellBrowserDlg: TcxShellBrowserDlg
  Left = 455
  Top = 160
  ActiveControl = cxStv
  BorderIcons = [biSystemMenu]
  Caption = 'Browse for Folder'
  ClientHeight = 358
  ClientWidth = 308
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 8
    Top = 8
    Width = 297
    Height = 340
    ParentBackground = True
    TabOrder = 0
    Transparent = True
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cxStv: TcxShellTreeView
      AlignWithMargins = True
      Left = 10
      Top = 53
      Width = 277
      Height = 246
      Margins.Left = 10
      Margins.Right = 10
      HideSelection = False
      Indent = 19
      Options.ShowNonFolders = False
      RightClickSelect = True
      TabOrder = 1
      OnChange = cxStvChange
    end
    object cxTeFolder: TcxTextEdit
      AlignWithMargins = True
      Left = 10
      Top = 28
      Margins.Left = 10
      Margins.Top = 0
      Margins.Right = 10
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 277
    end
    object cxButton1: TcxButton
      AlignWithMargins = True
      Left = 131
      Top = 305
      Width = 75
      Height = 25
      Margins.Top = 0
      Margins.Bottom = 0
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object cxButton2: TcxButton
      AlignWithMargins = True
      Left = 212
      Top = 305
      Width = 75
      Height = 25
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object liCurrentFolder: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Current Folder'
      CaptionOptions.Layout = clTop
      Control = cxTeFolder
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 288
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'cxStv'
      CaptionOptions.Visible = False
      Control = cxStv
      ControlOptions.OriginalHeight = 264
      ControlOptions.OriginalWidth = 288
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = cxButton1
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = cxButton2
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 168
    Top = 160
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
