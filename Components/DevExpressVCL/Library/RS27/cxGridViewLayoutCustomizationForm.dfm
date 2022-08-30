inherited cxGridViewLayoutCustomizationForm: TcxGridViewLayoutCustomizationForm
  Left = 611
  Top = 172
  AutoScroll = False
  ActiveControl = nil
  ClientHeight = 524
  ClientWidth = 747
  OnCloseQuery = FormCloseQuery
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 747
    Height = 524
    LayoutLookAndFeel = llfMain
    inherited tvVisibleItems: TcxTreeView
      Left = 36
      Top = 100
      Width = 194
      Height = 373
      TabStop = False
    end
    inherited tvAvailableItems: TcxTreeView
      Left = 10000
      Top = 10000
      Width = 193
      TabStop = False
      Visible = False
    end
    inherited btnClose: TcxButton
      Left = 10000
      Top = 10000
      ModalResult = 1
      TabOrder = 23
      TabStop = False
      Visible = False
    end
    inherited cbTabbedView: TcxCheckBox
      Left = 10000
      Top = 10000
      TabStop = False
      TabOrder = 22
      Visible = False
    end
    inherited btnShowDesignSelectors: TcxButton
      TabStop = False
    end
    inherited btnHighlightRoot: TcxButton
      TabStop = False
    end
    inherited btnRestore: TcxButton
      TabStop = False
    end
    inherited btnStore: TcxButton
      TabStop = False
    end
    inherited btnRedo: TcxButton
      TabStop = False
    end
    inherited btnUndo: TcxButton
      TabStop = False
    end
    inherited btnAlignBy: TcxButton
      Left = 117
      Top = 72
      TabStop = False
    end
    inherited btnTreeViewItemsDelete: TcxButton
      Left = 88
      Top = 72
      TabStop = False
    end
    inherited btnTreeViewCollapseAll: TcxButton
      Left = 59
      Top = 72
      TabStop = False
    end
    inherited btnTreeViewExpandAll: TcxButton
      Left = 36
      Top = 72
      TabStop = False
    end
    inherited btnAvailableItemsViewAsList: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited btnAvailableItemsDelete: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited btnAddItem: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited btnAddGroup: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited btnAvailableItemsCollapseAll: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited btnAvailableItemsExpandAll: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    inherited cxButton1: TcxButton
      Left = 10000
      Top = 10000
      TabStop = False
      Visible = False
    end
    object btnOk: TcxButton [21]
      Left = 581
      Top = 489
      Width = 75
      Height = 25
      Action = acOk
      ModalResult = 1
      TabOrder = 20
    end
    object btnCancel: TcxButton [22]
      Left = 662
      Top = 489
      Width = 75
      Height = 25
      Action = acCancel
      TabOrder = 21
    end
    object cbSaveLayout: TcxCheckBox [23]
      Left = 10
      Top = 491
      Caption = 'Save layout'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 19
      Transparent = True
      Width = 121
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = -1
    end
    inherited lcMainGroup2: TdxLayoutGroup
      Index = 0
    end
    inherited liUndo: TdxLayoutItem
      Index = 0
    end
    inherited liRedo: TdxLayoutItem
      Index = 1
    end
    inherited lsSeparator4: TdxLayoutSeparatorItem
      Index = 2
    end
    inherited liStore: TdxLayoutItem
      Index = 3
    end
    inherited liRestore: TdxLayoutItem
      Index = 4
    end
    inherited liHighlightRoot: TdxLayoutItem
      Index = 5
    end
    inherited liShowDesignSelectors: TdxLayoutItem
      Index = 6
    end
    inherited lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup5
      LayoutDirection = ldTabbed
      Index = 2
    end
    inherited lcgTreeView: TdxLayoutGroup
      Index = 0
    end
    inherited lgTreeView: TdxLayoutGroup
      Index = 0
    end
    inherited lcMainItem10: TdxLayoutItem
      Index = 0
    end
    inherited lcMainItem9: TdxLayoutItem
      Index = 1
    end
    inherited lcMainSeparatorItem3: TdxLayoutSeparatorItem
      Index = 2
    end
    inherited lcMainItem7: TdxLayoutItem
      Index = 3
    end
    inherited lsAlignBy: TdxLayoutSeparatorItem
      Index = 4
    end
    inherited liAlignBy: TdxLayoutItem
      Index = 5
    end
    inherited lcMainItem6: TdxLayoutItem
      Index = 1
    end
    inherited lcgAvailableItems: TdxLayoutGroup
      Index = 1
    end
    inherited lgAvailableItems: TdxLayoutGroup
      Index = 0
    end
    inherited lcMainItem15: TdxLayoutItem
      Index = 0
    end
    inherited lcMainItem14: TdxLayoutItem
      Index = 1
    end
    inherited lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Index = 2
    end
    inherited lcMainItem13: TdxLayoutItem
      Index = 3
    end
    inherited liAddItem: TdxLayoutItem
      Index = 4
    end
    inherited liAddCustomItem: TdxLayoutItem
      Index = 5
    end
    inherited lcMainItem11: TdxLayoutItem
      Index = 6
    end
    inherited lcMainSeparatorItem2: TdxLayoutSeparatorItem
      Index = 7
    end
    inherited lcMainItem3: TdxLayoutItem
      Index = 8
    end
    inherited lcMainItem8: TdxLayoutItem
      Index = 1
    end
    inherited lcMainGroup3: TdxLayoutGroup
      AlignHorz = ahClient
      Index = 2
    end
    inherited lcMainItem4: TdxLayoutItem
      Parent = nil
      Index = -1
    end
    inherited lcMainItem1: TdxLayoutItem
      Parent = nil
      AlignHorz = ahLeft
      Index = -1
    end
    object liGridViewContainer: TdxLayoutItem [58]
      Parent = lcMainGroup5
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'dxLayoutControl1'
      CaptionOptions.Visible = False
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup5: TdxLayoutGroup [59]
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object liOk: TdxLayoutItem [60]
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liCancel: TdxLayoutItem [61]
      Parent = lcMainGroup4
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup4: TdxLayoutGroup [62]
      Parent = lcMainGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object liSaveLayout: TdxLayoutItem [63]
      Parent = lcMainGroup3
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbSaveLayout
      ControlOptions.ShowBorder = False
      Index = 0
    end
    inherited siMainSplitter: TdxLayoutSplitterItem
      Parent = lcMainGroup5
      AlignVert = avClient
      SizeOptions.Width = 4
      Index = 1
    end
  end
  inherited alMain: TActionList
    inherited acClose: TAction
      ShortCut = 0
    end
    object acCancel: TAction
      Category = 'Buttons'
      Caption = 'acCancel'
      OnExecute = acCancelExecute
    end
    object acOk: TAction
      Category = 'Buttons'
      Caption = 'acOk'
      OnExecute = acOkExecute
    end
  end
  inherited ilActions: TcxImageList
    FormatVersion = 1
  end
  inherited ilItems: TcxImageList
    FormatVersion = 1
  end
  inherited ilHelper: TcxImageList
    FormatVersion = 1
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 152
    Top = 392
    object llfMain: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
