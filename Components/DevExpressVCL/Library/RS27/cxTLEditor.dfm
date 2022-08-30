object cxTreeListBandColumnDesigner: TcxTreeListBandColumnDesigner
  Left = 480
  Top = 171
  BorderIcons = [biSystemMenu]
  Caption = 'TreeListDesigner'
  ClientHeight = 313
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 362
    Height = 313
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnBAdd: TcxButton
      Left = 236
      Top = 44
      Width = 105
      Height = 22
      Caption = '&Add'
      TabOrder = 1
      OnClick = BandTabButtonsClick
    end
    object btnBDel: TcxButton
      Tag = 1
      Left = 236
      Top = 72
      Width = 105
      Height = 22
      Caption = '&Delete'
      TabOrder = 2
      OnClick = BandTabButtonsClick
    end
    object btnBMoveU: TcxButton
      Tag = 2
      Left = 236
      Top = 100
      Width = 105
      Height = 22
      Caption = 'Move &Up '
      TabOrder = 3
      OnClick = BandTabButtonsClick
    end
    object btnBMoveD: TcxButton
      Tag = 3
      Left = 236
      Top = 128
      Width = 105
      Height = 22
      Caption = 'Move Dow&n'
      TabOrder = 4
      OnClick = BandTabButtonsClick
    end
    object btnBResD: TcxButton
      Tag = 4
      Left = 236
      Top = 156
      Width = 105
      Height = 22
      Caption = 'Rest&ore Defaults'
      TabOrder = 5
      OnClick = BandTabButtonsClick
    end
    object btnBResW: TcxButton
      Tag = 5
      Left = 236
      Top = 184
      Width = 105
      Height = 22
      Caption = 'Restore &Widths'
      TabOrder = 6
      OnClick = BandTabButtonsClick
    end
    object btnCAdd: TcxButton
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = '&Add'
      TabOrder = 8
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCDel: TcxButton
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = '&Delete'
      TabOrder = 9
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCMoveU: TcxButton
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = 'Move &Up '
      TabOrder = 10
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCMoveD: TcxButton
      Tag = 3
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = 'Move Dow&n'
      TabOrder = 11
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCResD: TcxButton
      Tag = 4
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = 'Rest&ore Defaults'
      TabOrder = 12
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCResW: TcxButton
      Tag = 5
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = 'Restore &Widths'
      TabOrder = 13
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object btnCreateAllFields: TcxButton
      Tag = 6
      Left = 10000
      Top = 10000
      Width = 105
      Height = 22
      Caption = 'Create all &fields'
      TabOrder = 14
      Visible = False
      OnClick = ColumnsEventHandle
    end
    object lbxBands: TcxListBox
      Left = 21
      Top = 44
      Width = 209
      Height = 248
      DragMode = dmAutomatic
      ItemHeight = 16
      ListStyle = lbOwnerDrawFixed
      MultiSelect = True
      PopupMenu = pmBands
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = BandsListClick
      OnDrawItem = lbxBandsDrawItem
      OnKeyDown = lbxKeyDown
    end
    object lbxColumns: TcxListBox
      Left = 10000
      Top = 10000
      Width = 209
      Height = 248
      DragMode = dmAutomatic
      ItemHeight = 13
      ListStyle = lbOwnerDrawFixed
      MultiSelect = True
      PopupMenu = pmColumns
      Style.TransparentBorder = False
      TabOrder = 7
      Visible = False
      OnClick = ColumnsListClick
      OnDrawItem = lbxColumnsDrawItem
      OnKeyDown = lbxKeyDown
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object lgPages: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lgPages
      AlignVert = avClient
      CaptionOptions.Text = '  Bands  '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBAdd'
      CaptionOptions.Visible = False
      Control = btnBAdd
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBDel'
      CaptionOptions.Visible = False
      Control = btnBDel
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBMoveU'
      CaptionOptions.Visible = False
      Control = btnBMoveU
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBMoveD'
      CaptionOptions.Visible = False
      Control = btnBMoveD
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBResD'
      CaptionOptions.Visible = False
      Control = btnBResD
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnBResW'
      CaptionOptions.Visible = False
      Control = btnBResW
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lgPages
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = '  Columns  '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCAdd'
      CaptionOptions.Visible = False
      Control = btnCAdd
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCDel'
      CaptionOptions.Visible = False
      Control = btnCDel
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCMoveU'
      CaptionOptions.Visible = False
      Control = btnCMoveU
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCMoveD'
      CaptionOptions.Visible = False
      Control = btnCMoveD
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCResD'
      CaptionOptions.Visible = False
      Control = btnCResD
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCResW'
      CaptionOptions.Visible = False
      Control = btnCResW
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liCreateAllFields: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCreateAllFields'
      CaptionOptions.Visible = False
      Control = btnCreateAllFields
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbxBands
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 209
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbxColumns
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 209
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object pmColumns: TPopupMenu
    Left = 228
    Top = 6
    object mnuCAdd: TMenuItem
      Caption = '&Add...'
      ShortCut = 45
      OnClick = ColumnsEventHandle
    end
    object mnuCDel: TMenuItem
      Tag = 1
      Caption = '&Delete'
      ShortCut = 46
      OnClick = ColumnsEventHandle
    end
    object mnuCMoveU: TMenuItem
      Tag = 2
      Caption = 'Move &Up'
      OnClick = ColumnsEventHandle
    end
    object mnuCMoveD: TMenuItem
      Tag = 3
      Caption = 'Move Dow&n'
      OnClick = ColumnsEventHandle
    end
    object mnuCResD: TMenuItem
      Tag = 4
      Caption = 'Rest&ore Defaults'
      OnClick = ColumnsEventHandle
    end
    object mnuCResW: TMenuItem
      Tag = 5
      Caption = 'Restore &Widths'
      OnClick = ColumnsEventHandle
    end
    object mnuCreateAllFields: TMenuItem
      Tag = 6
      Caption = 'Create all fields'
      OnClick = ColumnsEventHandle
    end
    object mnuCSelectAll: TMenuItem
      Tag = 8
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = ColumnsEventHandle
    end
  end
  object pmBands: TPopupMenu
    Left = 194
    Top = 6
    object mnuBAdd: TMenuItem
      Caption = '&Add'
      ShortCut = 45
      OnClick = BandTabButtonsClick
    end
    object mnuBDel: TMenuItem
      Tag = 1
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BandTabButtonsClick
    end
    object mnuBMoveU: TMenuItem
      Tag = 2
      Caption = 'Move &Up'
      OnClick = BandTabButtonsClick
    end
    object mnuBMoveD: TMenuItem
      Tag = 3
      Caption = 'Move Dow&n'
      OnClick = BandTabButtonsClick
    end
    object mnuBResD: TMenuItem
      Tag = 4
      Caption = 'Rest&ore Defaults'
      OnClick = BandTabButtonsClick
    end
    object mnuBResW: TMenuItem
      Caption = 'Restore &Width'
    end
    object mnuBSelectAll: TMenuItem
      Tag = 8
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = BandTabButtonsClick
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 264
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
