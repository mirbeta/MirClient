inherited dxfmCustomContainerDesignWindow: TdxfmCustomContainerDesignWindow
  Left = 457
  Top = 268
  AutoSize = False
  Caption = 'dxfmCustomContainerDesignWindow'
  ClientHeight = 411
  ClientWidth = 654
  Constraints.MinHeight = 440
  Constraints.MinWidth = 660
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 654
    Height = 411
    Align = alClient
    inherited btnApply: TcxButton
      Top = 359
      TabOrder = 39
    end
    inherited btnCancel: TcxButton
      Top = 359
      TabOrder = 38
    end
    inherited btnOK: TcxButton
      Top = 359
      TabOrder = 37
    end
    inherited btnHelp: TcxButton
      Top = 359
      TabOrder = 40
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 359
      TabOrder = 41
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 359
      TabOrder = 42
    end
    inherited btnTitleProperties: TcxButton
      Top = 359
      TabOrder = 43
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 359
      TabOrder = 44
    end
    object imgExpanding: TcxImage [8]
      Left = 21
      Top = 84
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgPagination: TcxImage [9]
      Left = 21
      Top = 200
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgSize: TcxImage [10]
      Left = 21
      Top = 278
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgTabs: TcxImage [11]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgGroups: TcxImage [12]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object lblTransparents: TcxLabel [13]
      Left = 21
      Top = 60
      AutoSize = False
      Caption = 'Transparent'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 816
    end
    object chbxTransparentRoot: TcxCheckBox [14]
      Left = 75
      Top = 84
      Caption = '&Root'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object chbxTransparentControls: TcxCheckBox [15]
      Tag = 1
      Left = 75
      Top = 107
      Caption = 'Controls'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object chbxTransparentContainers: TcxCheckBox [16]
      Tag = 2
      Left = 75
      Top = 130
      Caption = 'Containers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object chbxTransparentGraphics: TcxCheckBox [17]
      Tag = 3
      Left = 75
      Top = 153
      Caption = '&Graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object chbxTransparentGroups: TcxCheckBox [18]
      Tag = 4
      Left = 181
      Top = 84
      Caption = '&Groups'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object chbxTransparentItems: TcxCheckBox [19]
      Tag = 5
      Left = 181
      Top = 107
      Caption = '&Items'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = TransparentClick
      Width = 100
    end
    object lblPagination: TcxLabel [20]
      Left = 21
      Top = 176
      AutoSize = False
      Caption = 'Pagination'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 816
    end
    object chbxPaginateByControls: TcxCheckBox [21]
      Tag = 1
      Left = 75
      Top = 200
      Caption = 'Controls'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      OnClick = PaginationClick
      Width = 100
    end
    object chbxPaginateByControlDetails: TcxCheckBox [22]
      Left = 75
      Top = 223
      Caption = 'Control Details'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      OnClick = PaginationClick
      Width = 100
    end
    object chbxPaginateByGroups: TcxCheckBox [23]
      Tag = 2
      Left = 181
      Top = 200
      Caption = '&Groups'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      OnClick = PaginationClick
      Width = 100
    end
    object chbxPaginateByItems: TcxCheckBox [24]
      Tag = 3
      Left = 181
      Top = 223
      Caption = '&Items'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      OnClick = PaginationClick
      Width = 100
    end
    object lblSize: TcxLabel [25]
      Left = 21
      Top = 254
      AutoSize = False
      Caption = 'Size'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 816
    end
    object chbxAutoWidth: TcxCheckBox [26]
      Left = 75
      Top = 278
      Caption = 'Auto Width'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      OnClick = SizeClick
      Width = 74
    end
    object lbGroups: TcxLabel [27]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Groups'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 816
    end
    object lbTabs: TcxLabel [28]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Tabs'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 816
    end
    object chbxExpandedGroups: TcxCheckBox [29]
      Left = 10000
      Top = 10000
      Caption = 'Expanded Groups'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = GroupsClick
      Width = 200
    end
    object chbxSkipEmptyGroups: TcxCheckBox [30]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Skip Empty Groups'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      OnClick = GroupsClick
      Width = 200
    end
    object chbxUnwrapTabs: TcxCheckBox [31]
      Left = 10000
      Top = 10000
      Caption = '&Unwrap'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = TabsClick
      Width = 200
    end
    object chbxRiseActiveTabOntoTop: TcxCheckBox [32]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Rise Active Tab onto Top'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = TabsClick
      Width = 200
    end
    object lbxAvailableLinks: TcxListBox [33]
      Left = 10000
      Top = 10000
      Width = 326
      Height = 264
      DragMode = dmAutomatic
      ItemHeight = 13
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 25
      Visible = False
      OnClick = lbxAggregatedLinksClick
      OnDblClick = lbxAvailableLinksDblClick
      OnDragDrop = lbxAvailableLinksDragDrop
      OnDragOver = lbxAvailableLinksDragOver
      OnDrawItem = lbxAvailableLinksDrawItem
      OnKeyDown = lbxAvailableLinksKeyDown
    end
    object lbxAggregatedLinks: TcxListBox [34]
      Left = 10000
      Top = 10000
      Width = 326
      Height = 264
      DragMode = dmAutomatic
      ItemHeight = 13
      MultiSelect = True
      Style.TransparentBorder = False
      TabOrder = 28
      Visible = False
      OnClick = lbxAggregatedLinksClick
      OnDblClick = lbxAggregatedLinksDblClick
      OnDragDrop = lbxAggregatedLinksDragDrop
      OnDragOver = lbxAggregatedLinksDragOver
      OnDrawItem = lbxAvailableLinksDrawItem
      OnKeyDown = lbxAggregatedLinksKeyDown
    end
    object sbtnAdd: TcxButton [35]
      Left = 10000
      Top = 10000
      Width = 26
      Height = 26
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E00000000000000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FFFF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF0000
        00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FFFF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FFFF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 26
      Visible = False
      OnClick = sbtnAddClick
    end
    object sbtnRemove: TcxButton [36]
      Left = 10000
      Top = 10000
      Width = 26
      Height = 26
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E00000000000000000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 27
      Visible = False
      OnClick = sbtnRemoveClick
    end
    object btnLinksRemoveInconsistents: TcxButton [37]
      Left = 10000
      Top = 10000
      Width = 120
      Height = 23
      Caption = 'Remove Unneeded'
      TabOrder = 30
      Visible = False
      WordWrap = True
      OnClick = btnLinksRemoveInconsistentsClick
    end
    object btnLinksDesign: TcxButton [38]
      Left = 10000
      Top = 10000
      Width = 120
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Design...'
      TabOrder = 29
      Visible = False
      OnClick = DesignClick
    end
    object tvControls: TcxTreeView [39]
      Left = 10000
      Top = 10000
      Width = 735
      Height = 282
      Style.TransparentBorder = False
      TabOrder = 31
      Visible = False
      OnClick = TreeViewClick
      OnKeyDown = TreeViewKeyDown
      OnKeyPress = TreeViewKeyPress
      OnMouseUp = TreeViewMouseUp
      HideSelection = False
      ReadOnly = True
      StateImages = ilControls
      OnChange = TreeViewChange
      OnCustomDrawItem = TreeViewCustomDrawItem
    end
    object btnControlsCheckAll: TcxButton [40]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Check All'
      TabOrder = 32
      Visible = False
      OnClick = TreeViewCheckAllClick
    end
    object btnControlsExpandAll: TcxButton [41]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'E&xpand All'
      TabOrder = 33
      Visible = False
      OnClick = TreeViewExpandAllClick
    end
    object tvHiddenControls: TcxTreeView [42]
      Left = 10000
      Top = 10000
      Width = 735
      Height = 282
      Style.TransparentBorder = False
      TabOrder = 34
      Visible = False
      OnClick = TreeViewClick
      OnKeyDown = TreeViewKeyDown
      OnKeyPress = TreeViewKeyPress
      OnMouseUp = TreeViewMouseUp
      HideSelection = False
      ReadOnly = True
      StateImages = ilControls
      OnChange = TreeViewChange
      OnCustomDrawItem = TreeViewCustomDrawItem
    end
    object btnHiddenControlsCheckAll: TcxButton [43]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Check All'
      TabOrder = 35
      Visible = False
      OnClick = TreeViewCheckAllClick
    end
    object btnHiddenControlsExpandAll: TcxButton [44]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'E&xpand All'
      TabOrder = 36
      Visible = False
      OnClick = TreeViewExpandAllClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      AlignVert = avBottom
      Index = 2
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblTransparents
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      Control = imgExpanding
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentRoot
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentControls
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentContainers
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lichbxTransparentGroups: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentGroups
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lichbxTransparentItems: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentItems
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblPagination
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      Control = imgPagination
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaginateByControls
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaginateByControlDetails
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lichbxPaginateByGroups: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaginateByGroups
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lichbxPaginateByItems: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaginateByItems
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lilblSize: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblSize
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liimgSize: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = imgSize
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object lichbxAutoWidth: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lbGroups
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lbTabs
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahLeft
      Control = imgTabs
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      Control = imgGroups
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxExpandedGroups
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxSkipEmptyGroups
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxUnwrapTabs
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxRiseActiveTabOntoTop
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlAvailableLinks: TdxLayoutItem
      Parent = tshReportLinks
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'AvailableLinks:'
      CaptionOptions.Layout = clTop
      Control = lbxAvailableLinks
      ControlOptions.OriginalHeight = 240
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblAggregatedLinks: TdxLayoutItem
      Parent = tshReportLinks
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'ggregatedLinks'
      CaptionOptions.Layout = clTop
      Control = lbxAggregatedLinks
      ControlOptions.OriginalHeight = 240
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = pnlMoveButtonsSite
      CaptionOptions.Visible = False
      Control = sbtnAdd
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 26
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = pnlMoveButtonsSite
      CaptionOptions.Visible = False
      Control = sbtnRemove
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 26
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnLinksRemoveInconsistents: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnLinksRemoveInconsistents
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      CaptionOptions.Visible = False
      Control = btnLinksDesign
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = tshControls
      AlignHorz = ahClient
      AlignVert = avClient
      Control = tvControls
      ControlOptions.OriginalHeight = 253
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object PageControl: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 1
    end
    object tshOptions: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Options'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      Index = 2
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      Index = 2
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Behaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup11
      Index = 1
      AutoCreated = True
    end
    object tshReportLinks: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Aggregated Links'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object pnlMoveButtonsSite: TdxLayoutGroup
      Parent = tshReportLinks
      AlignVert = avCenter
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = tshReportLinks
      Index = 3
      AutoCreated = True
    end
    object tshControls: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'Controls'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 3
    end
    object dxLayoutItem32: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnControlsCheckAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem33: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnControlsExpandAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup
      Parent = tshControls
      AlignHorz = ahRight
      Index = 1
      AutoCreated = True
    end
    object tshHiddenControls: TdxLayoutGroup
      Parent = PageControl
      CaptionOptions.Text = 'HiddenControls'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 4
    end
    object dxLayoutItem34: TdxLayoutItem
      Parent = tshHiddenControls
      AlignHorz = ahClient
      AlignVert = avClient
      Control = tvHiddenControls
      ControlOptions.OriginalHeight = 253
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem35: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnHiddenControlsCheckAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem36: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = btnHiddenControlsExpandAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup
      Parent = tshHiddenControls
      AlignHorz = ahRight
      Index = 1
      AutoCreated = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 24
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object pmControls: TPopupMenu
    Images = ilControlsPopup
    OnPopup = pmControlsPopup
    Left = 4
    Top = 301
    object miControlsCheckAll: TMenuItem
      Caption = 'Check All'
      ImageIndex = 0
      OnClick = TreeViewCheckAllClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miControlsCheckStandardControls: TMenuItem
      Caption = 'Check Standard Unprintable Controls'
      Visible = False
      OnClick = UnhideStandardControlsClick
    end
    object miControlsUncheckStandardControls: TMenuItem
      Caption = 'Uncheck Standard Unprintable Controls'
      Visible = False
      OnClick = HideStandardControlsClick
    end
    object miLine2: TMenuItem
      Caption = '-'
      Visible = False
    end
    object miControlsCheckAllChildren: TMenuItem
      Caption = 'Check All Children'
      OnClick = TreeViewCheckAllChildrenClick
    end
    object miControlsUncheckAllChildren: TMenuItem
      Caption = 'Uncheck All Children'
      OnClick = TreeViewUncheckAllChildrenClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miControlsExpandAll: TMenuItem
      Caption = 'Expand All'
      ImageIndex = 1
      OnClick = TreeViewExpandAllClick
    end
  end
  object ilControlsPopup: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 19726396
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000030000000E000000180000001B0000001D0000001F0000
          0021000000230000002400000026000000260000001A00000006000000000000
          0000000000000000000C59442FC57B5E40FF7A5C3FFF795B3FFF785B3DFF7759
          3DFF76593BFF76573AFF745739FF745639FF523D28D20000001A000000000000
          000000000000000000107D5F43FFE8E2DCFFFAF7F4FFF9F7F4FFFAF7F4FFFAF8
          F5FFFAF8F6FFFBF9F7FFFBF9F7FFFBFAF8FF745538FF00000026000000030000
          000D00000016000000277E6043FFE5DCD5FFF6F1ECFFF5EFEBFFBB9470FFF4EF
          E9FFF3EDE8FFF3ECE6FFF2ECE5FFFBF9F7FF74563AFF000000260000000B6D55
          3CC4977652FF8E6E4DFF7F6144FFE5DDD5FFF6F1EDFFB4885DFF874301FFB995
          70FFF4EFE9FFF3EEE7FFF3ECE7FFFBF8F7FF75583BFF000000240000000F9977
          56FFEEE8E2FFF0EFEDFF806245FFE5DDD6FFC39F7BFF934D11FFC09C78FF8F4E
          11FFBC9775FFF3EEE9FFF4EDE8FFFAF8F6FF76583BFF000000220000000E9A78
          56FFECE5DEFFF0EDEBFF816346FFE4DCD6FF965211FFC39F7CFFF6F1EDFFC2A0
          7EFF8F4E11FFBC9775FFF4EEE9FFFAF8F5FF76593CFF000000200000000C9B7A
          57FFECE5DEFFF2EFEDFF826347FFE4DCD5FFC6A17CFFF8F2EFFFF6F2EEFFF6F1
          EDFFC39E7EFF8F4B11FFBC9875FFFAF7F5FF785A3DFF0000001E0000000A9C7B
          58FFECE4DEFFF3F1EFFF826547FFE3DCD6FFF8F4F1FFF8F4F0FFF7F3EFFFF7F2
          EEFFF6F1EEFFC29E7EFF8E4C11FFFAF6F4FF795B3EFF0000001C000000099D7C
          59FFEBE4DEFFF6F3F1FF836547FFE4DBD5FFF8F5F2FFF8F4F1FFF7F3F0FFF7F3
          EFFFF6F2EEFFF6F1EDFFC29F7EFFFAF7F4FF795C3EFF0000001A000000079E7C
          5AFFEAE4DCFFF7F5F3FF836548FFE3DCD5FFE4DBD5FFE4DCD5FFE5DCD5FFE4DC
          D5FFE5DDD5FFE5DDD5FFE5DCD5FFE9E1DCFF7A5D3FFF00000017000000069E7E
          5AFFEAE4DDFFF9F7F5FF9F8973FF836548FF826447FF826347FF826346FF8062
          46FF806244FF7E6143FF7E6043FF7D5F42FF5B4630CA0000000E000000049F7E
          5AFFEAE3DCFFFBFAF8FFF9F7F5FFF7F4F4FFF6F3F1FFF4F2F0FFF2EFEDFFF1EE
          ECFFF1F0EEFF8E6E4BFF000000250000000F0000000C00000003000000039F7E
          5CFFEAE4DCFFEAE3DCFFEBE4DDFFEBE4DCFFEBE4DEFFECE4DEFFECE5DEFFECE5
          DEFFEEE8E2FF967551FF0000001500000000000000000000000000000001765E
          44BF9F7E5CFF9E7D5AFF9E7C5AFF9E7C59FF9C7B59FF9C7B57FF9A7A56FF9A78
          56FF997754FF70573DC90000000D000000000000000000000000000000000000
          00010000000300000004000000050000000700000008000000090000000B0000
          000D0000000E0000000A00000003000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000009676
          54FF967553FF957452FF947452FF937352FF937350FF927250FF917150FF9171
          4FFF90704FFF8F704EFF906F4EFF8F6F4DFF8F6E4DFF00000000000000009777
          55FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000008F6F4EFF00000000000000009878
          55FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000008F6F4EFF00000000000000000000
          0000000000000000000000000000000000000000000500000015000000170000
          0007000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000040000001E054405C2044205C50000
          0028000000070000000000000000000000000000000000000000000000000000
          000000000000000000000000000300000014054604BD3FA04CFF30983EFF0443
          05C7000000260000000600000000000000000000000000000000000000000000
          000000000000000000010000000A064A05B342A54FFF4BB861FF49B55EFF3098
          3EFF054205C50000002500000006000000000000000000000000000000000000
          00000000000000000003064C06AB5DB367FF88D297FF87D195FF86CF94FF84CD
          92FF57A961FF044305C300000023000000060000000000000000000000000000
          000000000000064C06A8097409FF097409FF097409FF087208FF087109FF086E
          09FF086C08FF076A07FF044404B90000000E0000000000000000000000000000
          0000000000000000000000000001000000030000000500000007000000090000
          000C00000010000000130000000E000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000009F7D
          59FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000967553FF00000000000000009F7D
          5AFF000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000967653FF00000000000000009F7E
          5AFF9E7D5AFF9F7D59FF9E7D59FF9D7D59FF9D7C58FF9C7B58FF9C7B58FF9B7B
          57FF9B7A56FF997956FF997855FF987855FF977654FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000030000000D000000160000001A0000001C0000
          001F000000220000002400000026000000260000001A00000006000000000000
          000000000000000000000000000957432EC3795C40FF775A3EFF775A3DFF7658
          3BFF75573AFF74563AFF725538FF725437FF513C28D30000001A000000000000
          000000000000000000000000000B7B5E41FFE7D8CAFFF7F1ECFFF6EFEBFFF4EE
          E9FFF4EBE8FFF4EBE6FFF2EBE4FFF1EAE3FF725437FF00000026000000000000
          00000000000000000000000000097D5F43FFE6D9CDFFF6F2EFFFF7F0ECFFB387
          5EFFF4EEE9FFF5EEE7FFF2EBE6FFF1ECE5FF735539FF00000026000000000000
          00000000000000000000000000077E6144FFE7DBCFFFF7F3F0FFB88D66FF9150
          12FFB4875EFFF4EEE9FFF3ECE7FFF4EDE6FF74563AFF00000024000000000000
          00000000000000000000000000057F6245FFE8DACEFFBC916AFF98561AFFBA8E
          66FF95551AFFB68A63FFF5EEE9FFF5EEE7FF75573AFF00000022987754FF9776
          53FF967552FF957452FF927150FF806346FFE8DCD0FF99581AFFBC9269FFF7F3
          F0FFBB926CFF95551AFFB68B63FFF5EEE9FF76593BFF0000001F997855FF0000
          0000000000000000000000000002816447FFE8DCD1FFC7A27FFFF9F4F2FFF8F5
          F3FFF9F4F0FFBC926CFF97551AFFF5EFEAFF76593DFF0000001C000000000000
          0000000000000000000400000011816548FFE8DCD1FFFAF7F5FFFAF5F3FFF9F4
          F4FFF8F4F1FFF9F4F0FFC49E7CFFF7F1ECFF785B3EFF0000001A000000000000
          00000000000300000019033303A4836548FFE8DCD1FFE8DCD1FFE8DCD1FFE8DB
          CFFFE8DBCEFFE7DACEFFE8D8CDFFE6D8CBFF795C3FFF00000016000000000000
          0002000000100437049E34923DFF6A6F41FF826548FF816547FF816446FF8063
          46FF7F6244FF7E6143FF7D6043FF7B5F41FF5A462FC80000000D000000010000
          0007043A049047A04FFF82CD91FF7FC98DFF419547FF013102A70000001F0000
          00080000000500000007000000090000000B000000090000000300000001053A
          0584097409FF087208FF086F07FF076C07FF066806FF056405FF023102960000
          000A000000000000000000000000000000000000000000000000000000000000
          0001000000020000000400000006000000090000000D000000100000000B0000
          00030000000000000000000000000000000000000000000000009E7E59FF0000
          0000000000000000000000000000000000000000000000000000000000009776
          53FF0000000000000000000000000000000000000000000000009F7E5AFF9E7D
          59FF9E7D59FF9E7D58FF9C7B58FF9C7B58FF9B7A56FF9A7956FF997855FF9877
          54FF000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000030000000D000000160000001A0000001C0000
          001F000000220000002400000026000000260000001A00000006000000000000
          000000000000000000000000000957432EC3795C40FF775A3EFF775A3DFF7658
          3BFF75573AFF74563AFF725538FF725437FF513C28D30000001A000000000000
          000000000000000000000000000B7B5E41FFE7D8CAFFF7F1ECFFF6EFEBFFF4EE
          E9FFF4EBE8FFF4EBE6FFF2EBE4FFF1EAE3FF725437FF00000026000000000000
          00000000000000000000000000097D5F43FFE6D9CDFFF6F2EFFFF7F0ECFFF5EF
          EAFFF4EEE9FFF5EEE7FFF2EBE6FFF1ECE5FF735539FF00000026000000000000
          00000000000000000000000000077E6144FFE7DBCFFFF7F3F0FFF6F1EFFFF5F1
          EEFFF5F0ECFFF4EEE9FFF3ECE7FFF4EDE6FF74563AFF00000024000000000000
          00000000000000000000000000057F6245FFE8DACEFFF8F5F3FFF7F3F0FFF8F2
          EDFFF7F2EEFFF5F0ECFFF5EEE9FFF5EEE7FF75573AFF00000022987754FF9776
          53FF967552FF957452FF927150FF806346FFE8DCD0FFF9F4F2FFF8F4F1FFF7F3
          F0FFF6F2EFFFF7F0EEFFF6EFEAFFF5EEE9FF76593BFF0000001F997855FF0000
          0000000000000000000000000002816447FFE8DCD1FFFAF6F3FFF9F4F2FFF8F5
          F3FFF9F4F0FFF8F3EFFFF7F1ECFFF5EFEAFF76593DFF0000001C000000000000
          0000000000000000000400000011816548FFE8DCD1FFFAF7F5FFFAF5F3FFF9F4
          F4FFF8F4F1FFF9F4F0FFF8F1EDFFF7F1ECFF785B3EFF0000001A000000000000
          00000000000300000019033303A4836548FFE8DCD1FFE8DCD1FFE8DCD1FFE8DB
          CFFFE8DBCEFFE7DACEFFE8D8CDFFE6D8CBFF795C3FFF00000016000000000000
          0002000000100437049E34923DFF6A6F41FF826548FF816547FF816446FF8063
          46FF7F6244FF7E6143FF7D6043FF7B5F41FF5A462FC80000000D000000010000
          0007043A049047A04FFF82CD91FF7FC98DFF419547FF013102A70000001F0000
          00080000000500000007000000090000000B000000090000000300000001053A
          0584097409FF087208FF086F07FF076C07FF066806FF056405FF023102960000
          000A000000000000000000000000000000000000000000000000000000000000
          0001000000020000000400000006000000090000000D000000100000000B0000
          00030000000000000000000000000000000000000000000000009E7E59FF0000
          0000000000000000000000000000000000000000000000000000000000009776
          53FF0000000000000000000000000000000000000000000000009F7E5AFF9E7D
          59FF9E7D59FF9E7D58FF9C7B58FF9C7B58FF9B7A56FF9A7956FF997855FF9877
          54FF000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000A0000001B0000001D0000
          000C000000020000000100000000000000000000000000000000000000000000
          00000000000100000005000000060000000B0A08053A624C35E2604B34E30C09
          0749000000110000000A00000009000000030000000000000000000000000000
          00010000000916120D431C1610560907053D544635BFA99A8AFF9D8D7AFF5241
          30CB0806045118130D6814100A59000000160000000300000000000000000000
          000318130E41917C63FD947F68FF7D654BFAA89887FFC2B9AFFFB6ACA0FF9D8D
          7BFF765E44FB816B51FF7D654BFD17120C5F0000000A00000001000000000000
          00031C16104798836CFFE7E4E0FFC5BDB3FFCEC6BDFFCCC3BAFFC2BAAFFFB6AC
          A0FFB6AA9FFFB6AA9FFF826B52FF1A140D6B0000000B00000001000000000000
          000409070626876E54F9D9D3CEFFD1C9C1FFCFC7BFFFD9D3CCFFE7E2DEFFE5E1
          DDFFD5CFC7FFB6AB9FFF765E44FB080604510000001000000002000000020A08
          061C615040B4BEB2A4FFD3CCC6FFD2CBC4FFB5A89AFF98846EFF99846FFFC9BE
          B3FFE6E1DDFFB6ACA1FF9E8E7DFF534331CD0806043F0000000B000000046853
          3FC5CABFB4FFE0DCD8FFD5CFC9FFCBC5BDFF917B64FF2019123F1F19123F9984
          70FFE7E3DEFFC4BAB1FFB6ACA1FF9E8E7CFF584530D60000001B00000003725C
          46D5CEC3B9FFF1EFEDFFE0DCD8FFC4BDB5FF917B64FF2019133F1F19123F9883
          6EFFD9D3CCFFCCC4BBFFC4BAB1FFAB9D8DFF624C35E20000001A000000010D0B
          081D645543B1CEC3B8FFF1EFEDFFC6BFB9FFAA9D8EFF917C65FF917C64FFB6A8
          9AFFCFC7C0FFCEC6BDFFAC9E8DFF594938C60D0A07400000000A000000000000
          00010A0806198D775EF8EAE7E4FFCFCAC5FFC6C0B9FFC4BDB6FFCBC6BEFFD2CC
          C5FFD1CAC3FFC6BEB6FF7D654BFA0907053E0000000C00000002000000000000
          00001E19123AA18D77FFEFEDEBFFEBE7E4FFF1EFEDFFE1DED9FFD5CFCAFFD3CD
          C7FFD9D4CFFFE7E4E1FF95806AFF1D1710590000000700000001000000000000
          000019150F2F9D8971FDA18D77FF8D775DF8C9BEB2FFF1EFEDFFE1DEDAFFBEB2
          A5FF876E54F998836BFF927C65FD19140E490000000500000000000000000000
          0000000000001915102F1E19123A0A0806195F5040A8C9BEB2FFCAC0B5FF6151
          40B4090706261C16104718130E41000000090000000200000000000000000000
          0000000000000000000000000000000000010D0B081D766048DD766047DE0E0B
          0826000000040000000300000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000100000003000000050000
          0002000000010000000000000000000000000000000000000000}
      end>
  end
  object ilControls: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 19726368
  end
end
