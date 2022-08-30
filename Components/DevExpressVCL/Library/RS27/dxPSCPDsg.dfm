object dxfmCPDesigner: TdxfmCPDesigner
  Left = 435
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Component Printer Designer'
  ClientHeight = 332
  ClientWidth = 454
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 460
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 454
    Height = 332
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lvItems: TcxListView
      Left = 21
      Top = 44
      Width = 310
      Height = 238
      ColumnClick = False
      Columns = <>
      MultiSelect = True
      PopupMenu = pmItems
      RowSelect = True
      Style.TransparentBorder = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lvItemsChange
      OnDblClick = lvItemsDblClick
      OnEdited = lvItemsEdited
      OnEditing = lvItemsEditing
      OnResize = lvItemsResize
    end
    object btnAdd: TcxButton
      Left = 337
      Top = 44
      Width = 96
      Height = 23
      Caption = '&Add...'
      TabOrder = 1
      OnClick = AddClick
    end
    object btnAddComposition: TcxButton
      Left = 337
      Top = 73
      Width = 96
      Height = 23
      Caption = 'Add Composition'
      TabOrder = 2
      OnClick = AddCompositionClick
    end
    object btnDelete: TcxButton
      Left = 337
      Top = 102
      Width = 96
      Height = 23
      Caption = '&Delete...'
      TabOrder = 3
      OnClick = DeleteClick
    end
    object btnDesign: TcxButton
      Left = 337
      Top = 172
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Desi&gn...'
      TabOrder = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = DesignClick
    end
    object btnPageSetup: TcxButton
      Left = 337
      Top = 201
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Pa&ge Setup...'
      TabOrder = 5
      OnClick = PageSetupClick
    end
    object btnPrintPreview: TcxButton
      Left = 337
      Top = 230
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Pre&view...'
      TabOrder = 6
      OnClick = PrintPreviewClick
    end
    object btnPrint: TcxButton
      Left = 337
      Top = 259
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = '&Print...'
      TabOrder = 7
      OnClick = PrintClick
    end
    object btnClose: TcxButton
      Left = 268
      Top = 299
      Width = 85
      Height = 23
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 8
    end
    object btnHelp: TcxButton
      Left = 359
      Top = 299
      Width = 85
      Height = 23
      Caption = '&Help'
      TabOrder = 9
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
    object tbsItems: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Items'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tbsItems
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvItems
      ControlOptions.OriginalHeight = 209
      ControlOptions.OriginalWidth = 314
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = tbsItems
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 6
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnAddComposition'
      CaptionOptions.Visible = False
      Control = btnAddComposition
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnDesign'
      CaptionOptions.Visible = False
      Control = btnDesign
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnPageSetup'
      CaptionOptions.Visible = False
      Control = btnPageSetup
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnPrintPreview'
      CaptionOptions.Visible = False
      Control = btnPrintPreview
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnPrint'
      CaptionOptions.Visible = False
      Control = btnPrint
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object pnlNoItems: TcxLabel
    Left = 31
    Top = 118
    TabStop = False
    AutoSize = False
    Caption = 'There are no Items to display'
    ParentColor = False
    ParentFont = False
    Style.TextColor = clGray
    Properties.Alignment.Horz = taCenter
    Properties.Alignment.Vert = taVCenter
    Transparent = True
    Height = 38
    Width = 301
    AnchorX = 182
    AnchorY = 137
  end
  object pmItems: TPopupMenu
    Images = ilItems
    OnPopup = pmItemsPopup
    Left = 86
    Top = 48
    object miAdd: TMenuItem
      Caption = '&Add...'
      ImageIndex = 14
      ShortCut = 45
      OnClick = AddClick
    end
    object miAddComposition: TMenuItem
      Caption = 'Add Composition'
      OnClick = AddCompositionClick
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ImageIndex = 5
      ShortCut = 46
      OnClick = DeleteClick
    end
    object miRename: TMenuItem
      Caption = '&Rename'
      ShortCut = 113
      OnClick = RenameClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miDesign: TMenuItem
      Caption = 'Desi&gn...'
      Default = True
      ImageIndex = 6
      ShortCut = 16397
      OnClick = DesignClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miPageSetup: TMenuItem
      Caption = 'Pa&ge Setup...'
      ImageIndex = 15
      OnClick = PageSetupClick
    end
    object miPrintPreview: TMenuItem
      Caption = 'Pre&view...'
      ImageIndex = 8
      OnClick = PrintPreviewClick
    end
    object miPrint: TMenuItem
      Caption = '&Print...'
      ImageIndex = 16
      ShortCut = 16464
      OnClick = PrintClick
    end
  end
  object ilItems: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 3145784
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 24
    Top = 48
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
