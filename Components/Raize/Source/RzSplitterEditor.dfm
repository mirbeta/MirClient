object RzSplitterEditDlg: TRzSplitterEditDlg
  Left = 208
  Top = 118
  BorderStyle = bsDialog
  Caption = ' - Splitter Editor'
  ClientHeight = 330
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOK: TRzButton
    Left = 175
    Top = 296
    Default = True
    ModalResult = 1
    Caption = 'OK'
    Color = 15791348
    HotTrack = True
    TabOrder = 7
  end
  object BtnCancel: TRzButton
    Left = 258
    Top = 296
    ModalResult = 2
    Caption = 'Cancel'
    Color = 15791348
    HotTrack = True
    TabOrder = 8
  end
  object GrpPreview: TRzGroupBox
    Left = 8
    Top = 132
    Width = 145
    Height = 153
    Caption = 'Preview'
    TabOrder = 6
    object SplPreview: TRzSplitter
      Left = 8
      Top = 16
      Width = 129
      Height = 129
      Position = 62
      Percent = 49
      TabOrder = 0
      BarSize = (
        62
        0
        66
        129)
      UpperLeftControls = (
        PbxUpperLeft)
      LowerRightControls = (
        PbxLowerRight)
      object PbxUpperLeft: TPaintBox
        Left = 0
        Top = 0
        Width = 62
        Height = 129
        Align = alClient
        OnClick = PbxUpperLeftClick
        OnPaint = PbxUpperLeftPaint
      end
      object PbxLowerRight: TPaintBox
        Left = 0
        Top = 0
        Width = 63
        Height = 129
        Align = alClient
        OnClick = PbxLowerRightClick
        OnPaint = PbxLowerRightPaint
      end
    end
  end
  object GrpOrientation: TRzRadioGroup
    Left = 164
    Top = 132
    Width = 169
    Height = 37
    Caption = 'Orientation'
    Columns = 2
    ItemHotTrack = True
    ItemIndex = 0
    Items.Strings = (
      'Horizontal'
      'Vertical')
    StartYPos = 0
    TabOrder = 1
    OnClick = GrpOrientationClick
  end
  object ChkRealTime: TRzCheckBox
    Left = 164
    Top = 244
    Width = 108
    Height = 15
    Caption = 'Real Time Drag'
    HotTrack = True
    State = cbUnchecked
    TabOrder = 4
    OnClick = ChkRealTimeClick
  end
  object ChkUsePercent: TRzCheckBox
    Left = 164
    Top = 264
    Width = 152
    Height = 15
    Caption = 'Use Percent Positioning'
    HotTrack = True
    State = cbUnchecked
    TabOrder = 5
    OnClick = ChkUsePercentClick
  end
  object GrpFixedPane: TRzRadioGroup
    Left = 164
    Top = 176
    Width = 169
    Height = 37
    Caption = 'Fixed Pane'
    Columns = 2
    ItemHotTrack = True
    ItemIndex = 0
    Items.Strings = (
      'Left'
      'Right')
    StartYPos = 0
    TabOrder = 2
    OnClick = GrpFixedPaneClick
  end
  object ChkShowHotSpot: TRzCheckBox
    Left = 164
    Top = 224
    Width = 99
    Height = 15
    Caption = 'Show HotSpot'
    HotTrack = True
    State = cbUnchecked
    TabOrder = 3
    OnClick = ChkShowHotSpotClick
  end
  object TbcRegions: TRzTabControl
    Left = 8
    Top = 8
    Width = 325
    Height = 117
    Hint = ''
    ParentColor = False
    TabIndex = 0
    TabOrder = 0
    Tabs = <
      item
        Caption = 'Splitter'
      end
      item
        Caption = 'Splitter Bar'
      end
      item
        Caption = 'Left Pane'
      end
      item
        Caption = 'Right Pane'
      end>
    OnChanging = TbcRegionsChanging
    FixedDimension = 19
    object ChkVisible: TRzCheckBox
      Left = 168
      Top = 93
      Width = 56
      Height = 15
      Caption = 'Visible'
      HotTrack = True
      State = cbUnchecked
      TabOrder = 0
      OnClick = ChkVisibleClick
    end
    object GrpBorder: TRzGroupBox
      Left = 8
      Top = 32
      Width = 149
      Height = 77
      Caption = 'Border Style'
      TabOrder = 1
      TabStop = True
      object LblOuter: TRzLabel
        Left = 8
        Top = 24
        Width = 32
        Height = 13
        Caption = 'Outer'
        ParentColor = False
      end
      object LblInner: TRzLabel
        Left = 8
        Top = 52
        Width = 31
        Height = 13
        Caption = 'Inner'
        ParentColor = False
      end
      object CbxOuter: TRzComboBox
        Tag = 2
        Left = 48
        Top = 20
        Width = 93
        Height = 21
        Style = csDropDownList
        Ctl3D = False
        FlatButtons = True
        FrameVisible = True
        ParentCtl3D = False
        TabOrder = 0
        OnChange = FrameStyleChange
        Items.Strings = (
          'fsNone'
          'fsFlat'
          'fsGroove'
          'fsBump'
          'fsLowered'
          'fsButtonDown'
          'fsRaised'
          'fsButtonUp'
          'fsStatus'
          'fsPopup'
          'fsFlatBold'
          'fsFlatRounded')
      end
      object CbxInner: TRzComboBox
        Tag = 1
        Left = 48
        Top = 48
        Width = 93
        Height = 21
        Style = csDropDownList
        Ctl3D = False
        FlatButtons = True
        FrameVisible = True
        ParentCtl3D = False
        TabOrder = 1
        OnChange = FrameStyleChange
        Items.Strings = (
          'fsNone'
          'fsFlat'
          'fsGroove'
          'fsBump'
          'fsLowered'
          'fsButtonDown'
          'fsRaised'
          'fsButtonUp'
          'fsStatus'
          'fsPopup'
          'fsFlatBold'
          'fsFlatRounded')
      end
      object CbxBarStyle: TRzComboBox
        Left = 48
        Top = 20
        Width = 93
        Height = 21
        Style = csDropDownList
        Ctl3D = False
        FlatButtons = True
        FrameVisible = True
        ParentCtl3D = False
        TabOrder = 2
        Visible = False
        OnChange = CbxBarStyleChange
        Items.Strings = (
          'ssStandard'
          'ssGroove'
          'ssBump')
      end
    end
    object GrpWidth: TRzGroupBox
      Left = 168
      Top = 32
      Width = 149
      Height = 57
      Caption = 'Border Width'
      TabOrder = 2
      TabStop = True
      object TrkWidth: TRzTrackBar
        Left = 6
        Top = 16
        Width = 138
        Max = 20
        Position = 0
        TickStyle = tkOwnerDraw
        OnChange = TrkWidthChange
        OnDrawTick = TrkWidthDrawTick
        TabOrder = 0
      end
    end
  end
end
