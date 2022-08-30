inherited dxRichEditCustomSimpleNumberingListForm: TdxRichEditCustomSimpleNumberingListForm
  Caption = 'dxRichEditCustomSimpleNumberingListForm'
  ClientWidth = 361
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 361
    inherited btnOk: TcxButton
      Left = 194
      Top = 205
      TabOrder = 8
    end
    inherited btnCancel: TcxButton
      Left = 275
      Top = 205
      TabOrder = 9
    end
    inherited btnFont: TcxButton
      Left = 275
      Top = 30
      TabOrder = 1
    end
    inherited edtAligned: TdxMeasurementUnitEdit
      Left = 275
      Top = 131
      TabOrder = 6
      Width = 75
    end
    inherited edtIndent: TdxMeasurementUnitEdit
      Left = 275
      Top = 178
      TabOrder = 7
      Width = 75
    end
    object edtNumberFormat: TdxSimpleRichEditControl [5]
      Left = 26
      Top = 30
      Width = 243
      Height = 25
      BorderStyle = cxcbsNone
      TabOrder = 0
    end
    object lblNumberStyle: TcxLabel [6]
      Left = 10
      Top = 61
      Caption = '&Number style:'
      Style.HotTrack = False
      Transparent = True
    end
    object cmbDisplayFormat: TcxComboBox [7]
      Left = 26
      Top = 84
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 3
      Width = 130
    end
    object edStart: TcxSpinEdit [8]
      Left = 275
      Top = 84
      TabOrder = 4
      Width = 75
    end
    object cmbNumberingAlignment: TcxComboBox [9]
      Left = 26
      Top = 131
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 5
      Width = 130
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
      Index = -1
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahRight
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Padding.AssignedValues = [lpavLeft]
      ShowBorder = False
      Index = 1
    end
    object lciDisplayFormat: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Number f&ormat:'
      CaptionOptions.Visible = False
      Control = edtNumberFormat
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group4
      AlignHorz = ahLeft
      Index = 0
      AutoCreated = True
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lblNumberStyle
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxComboBox1'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      Padding.AssignedValues = [lpavLeft]
      Control = cmbDisplayFormat
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciStartAt: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = '&Start at:'
      Control = edStart
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxComboBox1'
      CaptionOptions.Visible = False
      Control = cmbNumberingAlignment
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciAlignedAt: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = '&Aligned at:'
      Control = edtAligned
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciIndentAt: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = '&Indent at:'
      Control = edtIndent
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group7
      AlignHorz = ahRight
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblNumberFormat: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Number f&ormat'
      CaptionOptions.Visible = True
      Index = 0
    end
    object lblNumberPosition: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'N&umber position'
      CaptionOptions.Visible = True
      Index = 3
    end
    object lblTextPosition: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Text position'
      CaptionOptions.Visible = True
      Index = 5
    end
  end
end
