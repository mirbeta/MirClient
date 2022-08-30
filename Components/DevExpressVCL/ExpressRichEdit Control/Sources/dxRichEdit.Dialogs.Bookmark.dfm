inherited dxRichEditBookmarkDialogForm: TdxRichEditBookmarkDialogForm
  Caption = 'Bookmark'
  ClientHeight = 292
  ClientWidth = 309
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 309
    Height = 292
    object rbSortByName: TcxRadioButton [0]
      Left = 18
      Top = 183
      Width = 151
      Height = 17
      Caption = '&Name'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbSortByClick
      Transparent = True
    end
    object rbSortByLocation: TcxRadioButton [1]
      Tag = 1
      Left = 175
      Top = 183
      Width = 113
      Height = 17
      Caption = '&Location'
      TabOrder = 3
      OnClick = rbSortByClick
      Transparent = True
    end
    object btnGoTo: TcxButton [2]
      Left = 213
      Top = 206
      Width = 75
      Height = 25
      Caption = '&Go To'
      TabOrder = 6
      OnClick = btnGoToClick
    end
    object btnCancel: TcxButton [3]
      Left = 213
      Top = 249
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 7
    end
    object lbBookmarkName: TcxListBox [4]
      Left = 10
      Top = 49
      Width = 103
      Height = 108
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
      OnDblClick = lbBookmarkNameDblClick
    end
    object edtBookmarkName: TcxTextEdit [5]
      Left = 10
      Top = 28
      AutoSize = False
      Properties.OnChange = edtBookmarkNamePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 21
      Width = 121
    end
    object btnAdd: TcxButton [6]
      Left = 10
      Top = 206
      Width = 75
      Height = 25
      Caption = '&Add'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnClick = btnAddClick
    end
    object btnDelete: TcxButton [7]
      Left = 111
      Top = 206
      Width = 75
      Height = 25
      Caption = '&Delete'
      TabOrder = 5
      OnClick = btnDeleteClick
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
      Index = -1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Offsets.Left = 8
      Control = rbSortByName
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbSortByLocation
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnGoTo
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel2
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lbBookmarkName
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciBookmarkName: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = '&Bookmark Name:'
      CaptionOptions.Layout = clTop
      Control = edtBookmarkName
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnAdd
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahCenter
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnDelete
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblSortBy: TdxLayoutLabeledItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Sort By:'
      Index = 1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 32
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      Offsets.ItemOffset = 0
    end
  end
end
