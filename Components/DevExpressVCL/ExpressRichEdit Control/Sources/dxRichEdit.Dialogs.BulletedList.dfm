inherited dxRichEditBulletedListDialogForm: TdxRichEditBulletedListDialogForm
  Caption = 'Customize Bulleted List'
  ClientHeight = 245
  ClientWidth = 297
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 297
    Height = 245
    object lbSimpleSymbol1: TdxSimpleSymbolListBox [0]
      Left = 17
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    object lbSimpleSymbol2: TdxSimpleSymbolListBox [1]
      Left = 63
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    object lbSimpleSymbol3: TdxSimpleSymbolListBox [2]
      Left = 109
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    object lbSimpleSymbol4: TdxSimpleSymbolListBox [3]
      Left = 155
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    object lbSimpleSymbol5: TdxSimpleSymbolListBox [4]
      Left = 201
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    object lbSimpleSymbol6: TdxSimpleSymbolListBox [5]
      Left = 247
      Top = 30
      Width = 40
      Height = 40
      FontName = 'Tahoma'
      ItemHeight = 38
    end
    inherited btnFont: TcxButton [6]
      Left = 131
      Top = 76
      TabOrder = 6
    end
    object btnCharacter: TcxButton [7]
      Left = 212
      Top = 76
      Width = 75
      Height = 25
      Caption = '&Character...'
      TabOrder = 7
      OnClick = btnCharacterClick
    end
    inherited edtAligned: TdxMeasurementUnitEdit [8]
      Left = 216
      Top = 127
      Properties.OnChange = nil
      TabOrder = 8
    end
    inherited edtIndent: TdxMeasurementUnitEdit [9]
      Left = 216
      Top = 174
      TabOrder = 9
    end
    inherited btnOk: TcxButton [10]
      Left = 131
      Top = 201
      TabOrder = 10
    end
    inherited btnCancel: TcxButton [11]
      Left = 212
      Top = 201
      TabOrder = 11
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      ItemIndex = 1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol1
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox2'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol2
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox3'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol3
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox4'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol4
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox5'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol5
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxListBox6'
      CaptionOptions.Visible = False
      Control = lbSimpleSymbol6
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item11: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      Control = btnCharacter
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAlignedAt: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = '&Aligned at:'
      Control = edtAligned
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lciIndentAt: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = '&Indent at:'
      Control = edtIndent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblBulletCharacter: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'B&ullet character'
      CaptionOptions.Visible = True
      Index = 0
    end
    object lblBulletPosition: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Bullet position'
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
