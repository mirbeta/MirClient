object frmItemsEditor: TfrmItemsEditor
  Left = 243
  Top = 174
  Anchors = [akLeft, akTop, akBottom]
  BorderIcons = [biSystemMenu]
  Caption = 'Items Editor'
  ClientHeight = 436
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 497
    Height = 436
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cxTLSite: TcxTreeList
      Left = 10
      Top = 10
      Width = 298
      Height = 376
      Bands = <
        item
          Caption.Text = 'Band + 1'
        end>
      DragMode = dmAutomatic
      Navigator.Buttons.CustomButtons = <>
      OptionsSelection.HideFocusRect = False
      OptionsSelection.InvertSelect = False
      OptionsSelection.MultiSelect = True
      OptionsView.GridLineColor = clBlack
      OptionsView.GridLines = tlglBoth
      PopupMenu = mnuEditItems
      Preview.Visible = True
      TabOrder = 0
      OnDragOver = cxTLSiteDragOver
      object cxTreeList1cxTreeListColumn1: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object btnNewItem: TcxButton
      Tag = 4
      Left = 314
      Top = 10
      Width = 173
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '&New Item'
      TabOrder = 1
      OnClick = cxButtonPress
    end
    object btnNewSubItem: TcxButton
      Tag = 5
      Left = 314
      Top = 38
      Width = 173
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'New &SubItem'
      TabOrder = 2
      OnClick = cxButtonPress
    end
    object btnDelete: TcxButton
      Tag = 6
      Left = 314
      Top = 66
      Width = 173
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 3
      OnClick = cxButtonPress
    end
    object cbCheckGroupType: TcxComboBox
      Left = 314
      Top = 344
      Anchors = [akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'CheckGroup'
        'RadioGroup')
      Properties.OnEditValueChanged = cbCheckGroupTypePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Text = 'None'
      Width = 173
    end
    object btnCustomize: TcxButton
      Left = 10
      Top = 404
      Width = 90
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'C&ustomize'
      TabOrder = 16
      OnClick = cxButtonPress
    end
    object btnOk: TcxButton
      Tag = 1
      Left = 301
      Top = 404
      Width = 90
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 17
    end
    object btnCancel: TcxButton
      Tag = 2
      Left = 397
      Top = 404
      Width = 90
      Height = 22
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 18
    end
    object edtDefImage: TcxTextEdit
      Left = 439
      Top = 132
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Text = '0'
      Width = 36
    end
    object cbEnabled: TcxCheckBox
      Left = 314
      Top = 369
      Anchors = [akTop, akRight]
      Caption = 'Enabled'
      Properties.OnEditValueChanged = cxCheckBox1PropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
    end
    object edtDefSelected: TcxTextEdit
      Tag = 1
      Left = 439
      Top = 157
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Text = '0'
      Width = 36
    end
    object edtDefState: TcxTextEdit
      Tag = 2
      Left = 439
      Top = 182
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Text = '-1'
      Width = 36
    end
    object edtDefOverlay: TcxTextEdit
      Tag = 3
      Left = 439
      Top = 207
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Text = '-1'
      Width = 36
    end
    object edtDefOverlayState: TcxTextEdit
      Tag = 4
      Left = 439
      Top = 232
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Text = '-1'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 36
    end
    object edtImage: TcxTextEdit
      Left = 395
      Top = 132
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Text = '0'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 38
    end
    object edtSelected: TcxTextEdit
      Tag = 1
      Left = 395
      Top = 157
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Text = '0'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 38
    end
    object edtState: TcxTextEdit
      Tag = 2
      Left = 395
      Top = 182
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Text = '-1'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 38
    end
    object edtOverlay: TcxTextEdit
      Tag = 3
      Left = 395
      Top = 207
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Text = '-1'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 38
    end
    object edtOverlayState: TcxTextEdit
      Tag = 4
      Left = 395
      Top = 232
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Text = '-1'
      OnExit = edtImageExit
      OnKeyDown = edtImageKeyDown
      Width = 38
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cxTLSite
      ControlOptions.OriginalHeight = 347
      ControlOptions.OriginalWidth = 312
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnNewItem'
      CaptionOptions.Visible = False
      Control = btnNewItem
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnNewSubItem'
      CaptionOptions.Visible = False
      Control = btnNewSubItem
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 161
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 161
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lbCheckType: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignVert = avTop
      CaptionOptions.Text = 'Check Group Type:'
      CaptionOptions.Layout = clTop
      Control = cbCheckGroupType
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 163
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCustomize'
      CaptionOptions.Visible = False
      Control = btnCustomize
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
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
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object gbIndexes: TdxLayoutGroup
      Parent = dxLayoutGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'Image Indexes'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 3
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = gbIndexes
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 5
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = gbIndexes
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahLeft
      Control = edtDefImage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 36
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cbEnabled'
      CaptionOptions.Visible = False
      Control = cbEnabled
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 58
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      AlignVert = avBottom
      CaptionOptions.Text = 'Separator'
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      Control = edtDefSelected
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      Control = edtDefState
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      Control = edtDefOverlay
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahClient
      Control = edtDefOverlayState
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Image'
      Control = edtImage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 38
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'Selected'
      Control = edtSelected
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 38
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'State'
      Control = edtState
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'Overlay'
      Control = edtOverlay
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'OverlayState'
      Control = edtOverlayState
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutLabeledItem1: TdxLayoutLabeledItem
      Parent = dxLayoutGroup8
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'Current'
      Index = 0
    end
    object dxLayoutLabeledItem2: TdxLayoutLabeledItem
      Parent = dxLayoutGroup9
      CaptionOptions.Text = 'Default'
      Index = 0
    end
  end
  object mnuEditItems: TPopupMenu
    Left = 56
    Top = 48
    object NewItem1: TMenuItem
      Tag = 4
      Caption = '&New Item'
      OnClick = cxButtonPress
    end
    object NewSubItem1: TMenuItem
      Tag = 5
      Caption = 'New &SubItem'
      OnClick = cxButtonPress
    end
    object Delete1: TMenuItem
      Tag = 6
      Caption = '&Delete'
      OnClick = cxButtonPress
    end
    object N1: TMenuItem
      Caption = '-'
      OnClick = cxButtonPress
    end
    object Customize1: TMenuItem
      Caption = 'C&ustomize'
      OnClick = cxButtonPress
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 24
    Top = 48
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end
