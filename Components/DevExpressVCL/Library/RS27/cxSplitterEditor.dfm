object cxSplitEditor: TcxSplitEditor
  Left = 224
  Top = 210
  ActiveControl = cxCbAhd
  BorderStyle = bsDialog
  ClientHeight = 383
  ClientWidth = 466
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  object cxGroupBox1: TcxGroupBox
    Left = 8
    Top = 200
    Caption = ' Preview '
    TabOrder = 0
    Height = 145
    Width = 449
    object cxGroupBox4: TcxGroupBox
      Left = 8
      Top = 16
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      TabOrder = 0
      Transparent = True
      Height = 121
      Width = 433
      object cxListBox1: TcxListBox
        Left = 2
        Top = 2
        Width = 145
        Height = 117
        TabStop = False
        Align = alLeft
        ItemHeight = 13
        TabOrder = 0
      end
      object cxSplit: TcxSplitter
        Left = 147
        Top = 2
        Width = 8
        Height = 117
        AutoPosition = False
        AutoSnap = True
        Control = cxListBox1
      end
      object cxListBox2: TcxListBox
        Left = 155
        Top = 2
        Width = 276
        Height = 117
        TabStop = False
        Align = alClient
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
  object cxGroupBox2: TcxGroupBox
    Left = 8
    Top = 8
    Caption = ' Operation '
    TabOrder = 1
    Height = 189
    Width = 153
    object Label1: TLabel
      Left = 27
      Top = 119
      Width = 40
      Height = 13
      Caption = 'Min Size'
      Transparent = True
    end
    object Label2: TLabel
      Left = 28
      Top = 61
      Width = 88
      Height = 13
      Caption = 'Position after open'
      Transparent = True
    end
    object cxCbAhd: TcxCheckBox
      Left = 8
      Top = 16
      Caption = 'Allow HotZone Drag'
      TabOrder = 0
      Transparent = True
      OnClick = cxCbAhdClick
      Width = 121
    end
    object cxCbAp: TcxCheckBox
      Left = 8
      Top = 40
      Caption = 'Auto Position'
      TabOrder = 1
      Transparent = True
      OnClick = cxCbApClick
      Width = 105
    end
    object cxCbSnap: TcxCheckBox
      Left = 8
      Top = 99
      Caption = 'Auto Snap'
      TabOrder = 3
      Transparent = True
      OnClick = cxCbSnapClick
      Width = 105
    end
    object cxCbRu: TcxCheckBox
      Left = 8
      Top = 161
      Caption = 'Resize Update'
      TabOrder = 5
      Transparent = True
      OnClick = cxCbRuClick
      Width = 105
    end
    object cxSeMs: TcxSpinEdit
      Left = 27
      Top = 133
      Properties.MaxValue = 145.000000000000000000
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeMsPropertiesChange
      TabOrder = 4
      Value = 30
      Width = 89
    end
    object cxSePao: TcxSpinEdit
      Left = 28
      Top = 75
      Properties.MaxValue = 200.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSePaoPropertiesChange
      TabOrder = 2
      Value = 200
      Width = 89
    end
  end
  object cxGroupBox3: TcxGroupBox
    Left = 168
    Top = 8
    Caption = ' Hot Zone '
    TabOrder = 2
    Height = 189
    Width = 289
    object Label3: TLabel
      Left = 9
      Top = 120
      Width = 73
      Height = 13
      Caption = 'HotZone Width'
      Transparent = True
    end
    object cxRbHzNone: TcxRadioButton
      Left = 9
      Top = 40
      Width = 113
      Height = 17
      Caption = 'None'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = cxRbHzNoneClick
      Transparent = True
    end
    object cxRbHzMp8: TcxRadioButton
      Left = 9
      Top = 65
      Width = 96
      Height = 17
      Hint = 'MediaPlayer8'
      Caption = 'Media Player 8'
      TabOrder = 2
      OnClick = cxRbHzMp8Click
      Transparent = True
    end
    object cxRbHzMp9: TcxRadioButton
      Left = 106
      Top = 65
      Width = 113
      Height = 17
      Hint = 'MediaPlayer9'
      Caption = 'Media Player 9'
      TabOrder = 3
      OnClick = cxRbHzMp8Click
      Transparent = True
    end
    object cxRbHzSimple: TcxRadioButton
      Left = 106
      Top = 90
      Width = 113
      Height = 17
      Hint = 'Simple'
      Caption = 'Simple'
      TabOrder = 4
      OnClick = cxRbHzMp8Click
      Transparent = True
    end
    object cxRbHzXp: TcxRadioButton
      Left = 9
      Top = 90
      Width = 96
      Height = 17
      Hint = 'XPTaskBar'
      Caption = 'XP Task Bar'
      TabOrder = 5
      OnClick = cxRbHzMp8Click
      Transparent = True
    end
    object cxCbHzVisible: TcxCheckBox
      Left = 9
      Top = 16
      Caption = 'Visible'
      Enabled = False
      Properties.OnChange = cxCbHzVisiblePropertiesChange
      TabOrder = 0
      Transparent = True
      Width = 121
    end
    object cxTbHzWidth: TcxTrackBar
      Left = 8
      Top = 136
      Position = 10
      Properties.Frequency = 5
      Properties.Max = 100
      Properties.Min = 10
      Properties.SelectionColor = clGreen
      Properties.SelectionEnd = 60
      Properties.SelectionStart = 30
      Properties.OnChange = cxTbHzWidthPropertiesChange
      TabOrder = 6
      Height = 49
      Width = 273
    end
  end
  object cxBtnOK: TcxButton
    Left = 304
    Top = 352
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
  end
  object cxBtnCancel: TcxButton
    Left = 384
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
