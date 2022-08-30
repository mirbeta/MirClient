object RzPanelEditDlg: TRzPanelEditDlg
  Left = 188
  Top = 113
  BorderStyle = bsDialog
  Caption = ' - Panel Editor'
  ClientHeight = 297
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GrpColors: TRzGroupBox
    Left = 360
    Top = 84
    Width = 169
    Height = 169
    Caption = 'Colors'
    TabOrder = 3
    TabStop = True
    object Label1: TRzLabel
      Left = 8
      Top = 56
      Width = 39
      Height = 13
      Caption = 'Border'
      ParentColor = False
    end
    object Label2: TRzLabel
      Left = 8
      Top = 88
      Width = 49
      Height = 13
      Caption = 'Highlight'
      ParentColor = False
    end
    object Label3: TRzLabel
      Left = 8
      Top = 120
      Width = 45
      Height = 13
      Caption = 'Shadow'
      ParentColor = False
    end
    object Label4: TRzLabel
      Left = 8
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Panel'
      ParentColor = False
    end
    object EdtPanelColor: TRzColorEdit
      Left = 64
      Top = 20
      Width = 97
      Height = 21
      CustomColors = RzCustomColors1
      DefaultColor = clBtnFace
      SelectedColor = clBtnFace
      ShowCustomColor = True
      ShowDefaultColor = True
      ShowSystemColors = True
      FlatButtons = True
      FrameVisible = True
      TabOrder = 0
      OnChange = EdtPanelColorChange
    end
    object EdtBorderColor: TRzColorEdit
      Left = 64
      Top = 52
      Width = 97
      Height = 21
      CustomColors = RzCustomColors1
      DefaultColor = clBtnFace
      SelectedColor = clBtnFace
      ShowCustomColor = True
      ShowDefaultColor = True
      ShowSystemColors = True
      FlatButtons = True
      FrameVisible = True
      TabOrder = 1
      OnChange = EdtBorderColorChange
    end
    object EdtBorderHighlight: TRzColorEdit
      Left = 64
      Top = 84
      Width = 97
      Height = 21
      CustomColors = RzCustomColors1
      DefaultColor = clBtnHighlight
      SelectedColor = clBtnHighlight
      ShowCustomColor = True
      ShowDefaultColor = True
      ShowSystemColors = True
      FlatButtons = True
      FrameVisible = True
      TabOrder = 2
      OnChange = EdtBorderHighlightChange
    end
    object EdtBorderShadow: TRzColorEdit
      Left = 64
      Top = 116
      Width = 97
      Height = 21
      CustomColors = RzCustomColors1
      DefaultColor = clBtnShadow
      SelectedColor = clBtnShadow
      ShowCustomColor = True
      ShowDefaultColor = True
      ShowSystemColors = True
      FlatButtons = True
      FrameVisible = True
      TabOrder = 3
      OnChange = EdtBorderShadowChange
    end
  end
  object GrpPreview: TRzGroupBox
    Left = 8
    Top = 84
    Width = 161
    Height = 169
    Caption = 'Preview'
    TabOrder = 4
    object PnlPreview: TRzPanel
      Left = 12
      Top = 20
      Width = 137
      Height = 137
      TabOrder = 0
    end
  end
  object GrpBorderStyle: TRzGroupBox
    Left = 180
    Top = 172
    Width = 169
    Height = 81
    Caption = 'Border Style'
    TabOrder = 2
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
      Left = 52
      Top = 20
      Width = 109
      Height = 21
      Style = csDropDownList
      Ctl3D = False
      FlatButtons = True
      FrameVisible = True
      ParentCtl3D = False
      TabOrder = 0
      OnChange = CbxOuterChange
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
      Left = 52
      Top = 48
      Width = 109
      Height = 21
      Style = csDropDownList
      Ctl3D = False
      FlatButtons = True
      FrameVisible = True
      ParentCtl3D = False
      TabOrder = 1
      OnChange = CbxInnerChange
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
  end
  object BtnOK: TRzButton
    Left = 371
    Top = 264
    Default = True
    ModalResult = 1
    Caption = 'OK'
    HotTrack = True
    TabOrder = 5
  end
  object BtnCancel: TRzButton
    Left = 454
    Top = 264
    Cancel = True
    ModalResult = 2
    Caption = 'Cancel'
    HotTrack = True
    TabOrder = 6
  end
  object GrpWidth: TRzGroupBox
    Left = 180
    Top = 84
    Width = 169
    Height = 81
    Caption = 'Widths'
    TabOrder = 1
    TabStop = True
    object TrkWidth: TRzTrackBar
      Left = 5
      Top = 36
      Width = 153
      Max = 25
      Position = 0
      TickStyle = tkOwnerDraw
      OnChange = TrkWidthChange
      OnDrawTick = TrkWidthDrawTick
      TabOrder = 2
    end
    object OptBorderWidth: TRzRadioButton
      Left = 8
      Top = 16
      Width = 58
      Height = 15
      Caption = 'Border'
      Checked = True
      HotTrack = True
      TabOrder = 0
      TabStop = True
      OnClick = OptBorderWidthClick
    end
    object OptBevelWidth: TRzRadioButton
      Left = 84
      Top = 16
      Width = 51
      Height = 15
      Caption = 'Bevel'
      HotTrack = True
      TabOrder = 1
      OnClick = OptBorderWidthClick
    end
  end
  object GrpCaption: TRzGroupBox
    Left = 8
    Top = 8
    Width = 521
    Height = 69
    Caption = 'Caption'
    TabOrder = 0
    object EdtCaption: TRzEdit
      Left = 12
      Top = 28
      Width = 409
      Height = 21
      Text = ''
      FrameVisible = True
      TabOrder = 0
      OnChange = EdtCaptionChange
    end
    object GrpAlignment: TRzRadioGroup
      Left = 440
      Top = 8
      Width = 69
      Height = 57
      Caption = ''
      Columns = 3
      GroupStyle = gsCustom
      ItemHotTrack = True
      ItemIndex = 4
      Items.Strings = (
        ''
        ''
        ''
        ''
        ''
        ''
        ''
        ''
        '')
      StartXPos = 0
      StartYPos = 0
      TabOrder = 1
      VerticalSpacing = 0
      OnClick = GrpAlignmentClick
    end
  end
  object RzRegIniFile1: TRzRegIniFile
    PathType = ptRegistry
    Left = 16
    Top = 264
  end
  object RzCustomColors1: TRzCustomColors
    Colors.Strings = (
      'ColorA=FFFFFF'
      'ColorB=FFFFFF'
      'ColorC=FFFFFF'
      'ColorD=FFFFFF'
      'ColorE=FFFFFF'
      'ColorF=FFFFFF'
      'ColorG=FFFFFF'
      'ColorH=FFFFFF'
      'ColorI=FFFFFF'
      'ColorJ=FFFFFF'
      'ColorK=FFFFFF'
      'ColorL=FFFFFF'
      'ColorM=FFFFFF'
      'ColorN=FFFFFF'
      'ColorO=FFFFFF'
      'ColorP=FFFFFF')
    RegIniFile = RzRegIniFile1
    Left = 52
    Top = 264
  end
end
