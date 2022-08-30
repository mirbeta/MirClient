inherited dxRichEditLineNumberingDialogForm: TdxRichEditLineNumberingDialogForm
  Caption = 'Line Numbers'
  ClientHeight = 245
  ClientWidth = 214
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 214
    Height = 245
    object cbAddLineNumbering: TcxCheckBox [0]
      Left = 10
      Top = 10
      Caption = 'Add &line numbering'
      Style.HotTrack = False
      TabOrder = 0
      Transparent = True
      Width = 156
    end
    object edtStartAt: TcxSpinEdit [1]
      Left = 93
      Top = 37
      Properties.MaxValue = 32767.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidateOnEnter = False
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Properties.OnValidate = edtStartAtPropertiesValidate
      TabOrder = 1
      Value = 1
      Width = 73
    end
    object edtCountBy: TcxSpinEdit [2]
      Left = 93
      Top = 64
      Properties.MaxValue = 100.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidateOnEnter = False
      Properties.ValidationOptions = [evoAllowLoseFocus]
      Properties.OnValidate = edtStartAtPropertiesValidate
      TabOrder = 2
      Value = 1
      Width = 73
    end
    object edtFromText: TdxMeasurementUnitEdit [3]
      Left = 93
      Top = 91
      Properties.ValidationOptions = [evoRaiseException, evoAllowLoseFocus]
      TabOrder = 3
      Width = 73
    end
    object rbNumberingRestartEachPage: TcxRadioButton [4]
      Left = 38
      Top = 138
      Width = 128
      Height = 17
      Caption = 'Restart each &page'
      TabOrder = 4
      GroupIndex = 1
      Transparent = True
    end
    object rbNumberingRestartEachSection: TcxRadioButton [5]
      Tag = 1
      Left = 38
      Top = 161
      Width = 128
      Height = 17
      Caption = 'Restart each &section'
      TabOrder = 5
      GroupIndex = 1
      Transparent = True
    end
    object rbNumberingRestartContinuous: TcxRadioButton [6]
      Tag = 2
      Left = 38
      Top = 184
      Width = 128
      Height = 17
      Caption = '&Continuous'
      TabOrder = 6
      GroupIndex = 1
      Transparent = True
    end
    object btnOk: TcxButton [7]
      Left = 10
      Top = 207
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 7
    end
    object btnCancel: TcxButton [8]
      Left = 91
      Top = 207
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 8
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbAddLineNumbering
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgControlsGroup: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 20
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciStartAt: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Start &at:'
      Control = edtStartAt
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCountBy: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Count &by:'
      Control = edtCountBy
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciFromText: TdxLayoutItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'From &text:'
      Control = edtFromText
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbNumberingRestartEachPage
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 128
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item9: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbNumberingRestartEachSection
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 128
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item10: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbNumberingRestartContinuous
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 128
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group2
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
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblNumbering: TdxLayoutLabeledItem
      Parent = lcgControlsGroup
      CaptionOptions.Text = 'Numbering:'
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end
