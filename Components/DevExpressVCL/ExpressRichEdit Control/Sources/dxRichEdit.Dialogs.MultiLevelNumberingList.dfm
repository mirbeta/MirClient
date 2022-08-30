inherited dxRichEditMultiLevelNumberingListDialogForm: TdxRichEditMultiLevelNumberingListDialogForm
  Caption = 'Customize Outline Numbered List'
  ClientHeight = 368
  ClientWidth = 326
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 326
    Height = 368
    inherited btnOk: TcxButton
      Left = 160
      Top = 333
      TabOrder = 10
    end
    inherited cmbNumberingAlignment: TcxComboBox [1]
      Top = 221
      TabOrder = 6
    end
    inherited btnCancel: TcxButton [2]
      Left = 241
      Top = 333
      TabOrder = 11
    end
    object lbBoxLevel: TcxListBox [3]
      Left = 26
      Top = 48
      Width = 27
      Height = 147
      Columns = 1
      ItemHeight = 15
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9')
      ListStyle = lbOwnerDrawFixed
      TabOrder = 0
    end
    inherited btnFont: TcxButton [4]
      Left = 241
      Top = 170
      TabOrder = 5
    end
    object cmbFollowNumber: TcxComboBox [5]
      Left = 188
      Top = 295
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 9
      Width = 128
    end
    inherited edStart: TcxSpinEdit [6]
      Left = 241
      Top = 113
    end
    inherited edtAligned: TdxMeasurementUnitEdit [7]
      Left = 241
      Top = 221
      TabOrder = 7
    end
    inherited cmbDisplayFormat: TcxComboBox [8]
      Left = 75
      Top = 113
    end
    inherited edtNumberFormat: TdxSimpleRichEditControl [9]
      Left = 75
      Top = 48
      Width = 241
      TabOrder = 1
    end
    inherited lblNumberStyle: TcxLabel [10]
      Left = 75
      Top = 90
      FocusControl = cmbDisplayFormat
    end
    inherited edtIndent: TdxMeasurementUnitEdit [11]
      Left = 241
      Top = 268
      TabOrder = 8
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    inherited dxLayoutControl1Item1: TdxLayoutItem
      Index = 0
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = 0
    end
    inherited dxLayoutControl1Group2: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avTop
      Index = 0
    end
    inherited lciDisplayFormat: TdxLayoutItem
      AlignHorz = ahClient
      CaptionOptions.Visible = True
      CaptionOptions.Layout = clTop
      Index = 0
    end
    inherited dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignVert = avBottom
      Index = 2
    end
    inherited dxLayoutControl1Group4: TdxLayoutGroup
      Parent = dxLayoutControl1Group1
      AlignVert = avCenter
      Index = 1
    end
    inherited dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup
      Index = 0
      AutoCreated = True
    end
    inherited dxLayoutControl1Item9: TdxLayoutItem
      Offsets.Left = 16
      Index = 0
    end
    inherited dxLayoutControl1Item8: TdxLayoutItem
      Index = 1
    end
    inherited lciStartAt: TdxLayoutItem
      CaptionOptions.Layout = clTop
      Index = 1
    end
    inherited dxLayoutControl1Group3: TdxLayoutGroup
      Index = 3
    end
    inherited dxLayoutControl1Item10: TdxLayoutItem
      Index = 0
    end
    inherited lciAlignedAt: TdxLayoutItem
      Index = 1
    end
    inherited lciIndentAt: TdxLayoutItem
      Index = 5
    end
    inherited dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup
      Index = 1
      AutoCreated = True
    end
    inherited dxLayoutControl1Item2: TdxLayoutItem
      Index = 1
    end
    inherited lblNumberFormat: TdxLayoutSeparatorItem
      CaptionOptions.Text = 'Number format'
      Index = 0
    end
    inherited lblTextPosition: TdxLayoutSeparatorItem [30]
      Top = 254
      Index = 4
    end
    object dxLayoutControl1Group6: TdxLayoutGroup [31]
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group1: TdxLayoutGroup [32]
      Parent = dxLayoutControl1Group6
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciLevel: TdxLayoutItem [33]
      Parent = dxLayoutControl1Group6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Le&vel'
      CaptionOptions.Width = 27
      CaptionOptions.Layout = clTop
      Offsets.Left = 16
      Control = lbBoxLevel
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcilFollowNumberWith: TdxLayoutItem [34]
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'Follo&w Number With:'
      Control = cmbFollowNumber
      ControlOptions.ShowBorder = False
      Index = 6
    end
    inherited lblNumberPosition: TdxLayoutSeparatorItem [35]
      AlignHorz = ahClient
      AlignVert = avTop
      Top = 204
      Index = 2
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 72
  end
end
