inherited dxRichEditTableOfContentsForm: TdxRichEditTableOfContentsForm
  Caption = 'Table of Contents'
  ClientHeight = 305
  ClientWidth = 410
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 410
    Height = 305
    object PreviewRichEditControl: TdxSimpleRichEditControl [0]
      Left = 10
      Top = 30
      Width = 380
      Height = 142
      BorderStyle = cxcbsNone
      TabOrder = 0
    end
    object cbShowPageNumbers: TcxCheckBox [1]
      Left = 10
      Top = 178
      Caption = 'Show page numbers'
      Properties.OnChange = cbShowPageNumbersPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
    end
    object cbRightAlignPageNumbers: TcxCheckBox [2]
      Left = 10
      Top = 203
      Caption = 'Right Align Page Numbers'
      Properties.OnChange = cbRightAlignPageNumbersPropertiesChange
      Style.HotTrack = False
      TabOrder = 2
      Transparent = True
    end
    object cbUseHyperlinks: TcxCheckBox [3]
      Left = 10
      Top = 228
      Caption = 'Use hyperlinks instead of page number'
      Properties.OnChange = cbUseHyperlinksPropertiesChange
      Style.HotTrack = False
      TabOrder = 3
      Transparent = True
    end
    object spEditShowLevels: TcxSpinEdit [4]
      Left = 316
      Top = 178
      Properties.MaxValue = 9.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = spEditShowLevelsPropertiesChange
      Style.HotTrack = False
      TabOrder = 4
      Value = 1
      Width = 74
    end
    object btnOk: TcxButton [5]
      Left = 234
      Top = 253
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [6]
      Left = 315
      Top = 253
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object liPrintPreview: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Print Preview'
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'dxSimpleRichEditControl1'
      CaptionOptions.Visible = False
      Control = PreviewRichEditControl
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Control = cbShowPageNumbers
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbRightAlignPageNumbers
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxCheckBox3'
      CaptionOptions.Visible = False
      Control = cbUseHyperlinks
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liEditShowLevels: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'Show levels:'
      Control = spEditShowLevels
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
  end
end
