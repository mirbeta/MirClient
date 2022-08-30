object cxHintStyleEditor: TcxHintStyleEditor
  Left = 195
  Top = 125
  BorderStyle = bsDialog
  ClientHeight = 440
  ClientWidth = 576
  Color = clBtnFace
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object lblHintColour: TLabel
    Left = 8
    Top = 412
    Width = 46
    Height = 13
    Caption = 'Hint Color'
  end
  object cxBtnOk: TcxButton
    Left = 415
    Top = 408
    Width = 75
    Height = 25
    Hint = 'Accept your changes and return to the Form Designer'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object cxBtnCancel: TcxButton
    Left = 495
    Top = 408
    Width = 75
    Height = 25
    Hint = 'Discard your changes and return to the Form Designer'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object cxCbStandard: TcxCheckBox
    Left = 304
    Top = 408
    Width = 105
    Height = 21
    Hint = 'Check to display hints in the standard Windows way'
    Properties.DisplayUnchecked = 'False'
    Properties.Caption = 'Standard Hints'
    TabOrder = 7
    OnClick = cxCbStandardClick
  end
  object cxGbHintFont: TcxGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 185
    Alignment = alTopLeft
    Caption = ' Hint Font '
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 80
      Width = 23
      Height = 13
      Caption = 'Style'
    end
    object Label2: TLabel
      Left = 124
      Top = 80
      Width = 20
      Height = 13
      Caption = 'Size'
    end
    object Label3: TLabel
      Left = 12
      Top = 40
      Width = 24
      Height = 13
      Caption = 'Color'
    end
    object cxFnHint: TcxFontNameComboBox
      Left = 12
      Top = 16
      Width = 161
      Height = 21
      Hint = 'Font for the hint windows'
      Properties.FontPreview.Visible = False
      Properties.OnChange = cxFnHintPropertiesChange
      TabOrder = 0
      OnClick = cxFnHintClick
    end
    object cxClbHintFontStyles: TcxCheckListBox
      Left = 12
      Top = 96
      Width = 105
      Height = 81
      Hint = 'Font Style for Hints'
      Columns = 0
      EditValue = 0
      Items = <
        item
          Tag = 0
          Text = 'Bold'
        end
        item
          Tag = 0
          Text = 'Italics'
        end
        item
          Tag = 0
          Text = 'Underline'
        end
        item
          Tag = 0
          Text = 'Strike Thro'
        end>
      ParentColor = False
      ScrollWidth = 0
      TabOrder = 2
      TabWidth = 0
      OnClickCheck = cxClbHintFontStylesClickCheck
    end
    object cxLbHfSize: TcxListBox
      Left = 124
      Top = 96
      Width = 49
      Height = 81
      Hint = 'Font Size for Hints'
      ExtendedSelect = False
      ItemHeight = 13
      ParentColor = False
      TabOrder = 3
      OnClick = cxLbHfSizeClick
    end
    object cxCcbHintFontColour: TcxColorComboBox
      Left = 12
      Top = 56
      Width = 161
      Height = 21
      Properties.DefaultDescription = 'Color not selected'
      Properties.Items = <>
      Properties.PrepareInfo = ''
      TabOrder = 1
      OnClick = cxCcbHintFontColourClick
    end
  end
  object cxgbHintCapFont: TcxGroupBox
    Left = 200
    Top = 8
    Width = 185
    Height = 185
    Alignment = alTopLeft
    Caption = ' Hint Caption Font '
    TabOrder = 1
    object Label4: TLabel
      Left = 12
      Top = 80
      Width = 23
      Height = 13
      Caption = 'Style'
    end
    object Label5: TLabel
      Left = 124
      Top = 80
      Width = 20
      Height = 13
      Caption = 'Size'
    end
    object Label6: TLabel
      Left = 12
      Top = 40
      Width = 24
      Height = 13
      Caption = 'Color'
    end
    object cxFnHinCap: TcxFontNameComboBox
      Left = 12
      Top = 16
      Width = 161
      Height = 21
      Hint = 'Font for the hint windows'
      Properties.FontPreview.Visible = False
      Properties.OnChange = cxFnHinCapPropertiesChange
      TabOrder = 0
      OnClick = cxFnHinCapClick
    end
    object cxClbHintCapFontStyles: TcxCheckListBox
      Tag = 1
      Left = 12
      Top = 96
      Width = 105
      Height = 81
      Hint = 'Font Style for Hint Captions'
      Columns = 0
      EditValue = 0
      Items = <
        item
          Tag = 0
          Text = 'Bold'
        end
        item
          Tag = 0
          Text = 'Italics'
        end
        item
          Tag = 0
          Text = 'Underline'
        end
        item
          Tag = 0
          Text = 'Strike Thro'
        end>
      ParentColor = False
      ScrollWidth = 0
      TabOrder = 2
      TabWidth = 0
      OnClickCheck = cxClbHintFontStylesClickCheck
    end
    object cxLbHcfSize: TcxListBox
      Left = 124
      Top = 96
      Width = 49
      Height = 81
      Hint = 'Font Size for Hint Caption'
      ItemHeight = 13
      ParentColor = False
      TabOrder = 3
      OnClick = cxLbHcfSizeClick
    end
    object cxCcbHintCapColor: TcxColorComboBox
      Left = 12
      Top = 56
      Width = 161
      Height = 21
      Properties.DefaultDescription = 'Color not selected'
      Properties.Items = <>
      Properties.PrepareInfo = ''
      TabOrder = 1
      OnClick = cxCcbHintCapColorClick
    end
  end
  object cxGbHintShape: TcxGroupBox
    Left = 392
    Top = 8
    Width = 177
    Height = 89
    Alignment = alTopLeft
    Caption = ' Shape '
    ParentFont = False
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 14
      Width = 72
      Height = 13
      Caption = 'Callout Position'
    end
    object Label8: TLabel
      Left = 8
      Top = 64
      Width = 68
      Height = 13
      Caption = 'Round Radius'
    end
    object cxCbHintCalloutPos: TcxComboBox
      Left = 8
      Top = 32
      Width = 161
      Height = 21
      Hint = 'The '#39'Callout'#39' position'
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'Auto'
        'Left Bottom'
        'Left Top'
        'Top Left'
        'Top Right'
        'Right Bottom'
        'Right Top'
        'Bottom Right'
        'Bottom Left')
      TabOrder = 0
      OnClick = cxCbHintCalloutPosClick
    end
    object cxcbHintRounded: TcxCheckBox
      Left = 96
      Top = 10
      Width = 75
      Height = 21
      Hint = 'Rounded corners on the hint windows'
      ParentBackground = False
      Properties.DisplayUnchecked = 'False'
      Properties.OnChange = cxcbHintRoundedPropertiesChange
      Properties.Caption = 'Rounded'
      TabOrder = 1
    end
    object cxSeHintRadius: TcxSpinEdit
      Left = 88
      Top = 60
      Width = 81
      Height = 21
      Hint = 'Radius of the rounded corners if enabled'
      Properties.MaxValue = 100
      Properties.MinValue = 5
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeHintRadiusPropertiesChange
      TabOrder = 2
      Value = 11
    end
  end
  object cxGbHintIcons: TcxGroupBox
    Left = 8
    Top = 200
    Width = 185
    Height = 97
    Alignment = alTopLeft
    Caption = ' Icons '
    TabOrder = 4
    object Label11: TLabel
      Left = 8
      Top = 16
      Width = 48
      Height = 13
      Caption = 'Icon Type'
    end
    object cxCbHintIconType: TcxComboBox
      Left = 8
      Top = 32
      Width = 169
      Height = 21
      Hint = 'The Icon type that will be displayed in the hint window'
      ParentFont = False
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'Application'
        'Information'
        'Warning'
        'Error'
        'Question'
        'WinLogo'
        'CurrentApplication')
      TabOrder = 0
      OnClick = cxCbHintIconTypeClick
    end
    object cxRbHIDef: TcxRadioButton
      Left = 8
      Top = 64
      Width = 57
      Height = 17
      Hint = 'Default Icon Size'
      Caption = 'Default'
      TabOrder = 1
      OnClick = cxRbHIDefClick
      ParentBackground = False
    end
    object cxRbHILarge: TcxRadioButton
      Tag = 1
      Left = 72
      Top = 64
      Width = 49
      Height = 17
      Hint = 'Large Icon Size'
      Caption = 'Large'
      TabOrder = 2
      OnClick = cxRbHIDefClick
      ParentBackground = False
    end
    object cxRbHISmall: TcxRadioButton
      Tag = 2
      Left = 128
      Top = 64
      Width = 49
      Height = 17
      Hint = 'Small Icon Size'
      Caption = 'Small'
      TabOrder = 3
      OnClick = cxRbHIDefClick
      ParentBackground = False
    end
  end
  object cxGbHintPause: TcxGroupBox
    Left = 8
    Top = 304
    Width = 185
    Height = 97
    Alignment = alTopLeft
    Caption = ' Timings  '
    TabOrder = 5
    object Label18: TLabel
      Left = 8
      Top = 24
      Width = 77
      Height = 13
      Caption = 'Hint Hide Pause'
    end
    object Label19: TLabel
      Left = 8
      Top = 48
      Width = 52
      Height = 13
      Caption = 'Hint Pause'
    end
    object Label20: TLabel
      Left = 8
      Top = 72
      Width = 80
      Height = 13
      Caption = 'Hint Short Pause'
    end
    object cxSeHintHidePause: TcxSpinEdit
      Left = 96
      Top = 20
      Width = 81
      Height = 21
      Hint = 'The animation delay'
      Properties.Increment = 100
      Properties.LargeIncrement = 500
      Properties.MaxValue = 10000
      Properties.MinValue = 500
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeHintHidePausePropertiesChange
      TabOrder = 0
      Value = 2500
    end
    object cxSeHintPause: TcxSpinEdit
      Left = 96
      Top = 44
      Width = 81
      Height = 21
      Hint = 'The animation delay'
      Properties.Increment = 10
      Properties.LargeIncrement = 100
      Properties.MaxValue = 1000
      Properties.MinValue = 50
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeHintPausePropertiesChange
      TabOrder = 1
      Value = 500
    end
    object cxSeShortHintPause: TcxSpinEdit
      Left = 96
      Top = 68
      Width = 81
      Height = 21
      Hint = 'The animation delay'
      Properties.Increment = 5
      Properties.MaxValue = 1000
      Properties.MinValue = 10
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeHintDelayPropertiesChange
      Properties.OnEditValueChanged = cxSeShortHintPausePropertiesEditValueChanged
      TabOrder = 2
      Value = 50
    end
  end
  object cxCcbHintColour: TcxColorComboBox
    Left = 72
    Top = 408
    Width = 225
    Height = 21
    Hint = 'Background Color for the hint windows'
    Properties.AllowSelectColor = True
    Properties.DefaultDescription = 'Color not selected'
    Properties.ImmediatePost = True
    Properties.OnEditValueChanged = cxCcbHintColourPropertiesEditValueChanged
    Properties.Items = <>
    Properties.PrepareInfo = ''
    TabOrder = 6
  end
  object cxGbHintAnimation: TcxGroupBox
    Left = 392
    Top = 104
    Width = 177
    Height = 89
    Alignment = alTopLeft
    Caption = ' Animation '
    TabOrder = 3
    object Label9: TLabel
      Left = 8
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Animation Style'
    end
    object Label10: TLabel
      Left = 8
      Top = 64
      Width = 30
      Height = 13
      Caption = 'Delay '
    end
    object cxCbHintAniStyle: TcxComboBox
      Left = 8
      Top = 32
      Width = 161
      Height = 21
      Hint = 'The animation style used to display hints'
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Slide From Left'
        'Slide From Right'
        'Slide Downward'
        'Slide Upward'
        'Slide From Center'
        'Hide'
        'Activate'
        'Fade In'
        'Auto'
        'None')
      Properties.OnChange = cxCbHintAniStylePropertiesChange
      TabOrder = 0
    end
    object cxSeHintDelay: TcxSpinEdit
      Left = 88
      Top = 60
      Width = 81
      Height = 21
      Hint = 'The animation delay'
      Properties.MaxValue = 1000
      Properties.MinValue = 50
      Properties.SpinButtons.ShowFastButtons = True
      Properties.OnChange = cxSeHintDelayPropertiesChange
      TabOrder = 1
      Value = 100
    end
  end
  object cxGbPreview: TcxGroupBox
    Left = 200
    Top = 200
    Width = 369
    Height = 201
    Alignment = alTopLeft
    Caption = ' Preview '
    TabOrder = 8
    object pnlPreview: TPanel
      Left = 2
      Top = 16
      Width = 365
      Height = 183
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object cxHsc: TcxHintStyleController
    Global = False
    HintStyle.CaptionFont.Charset = DEFAULT_CHARSET
    HintStyle.CaptionFont.Color = clWindowText
    HintStyle.CaptionFont.Height = -13
    HintStyle.CaptionFont.Name = 'MS Sans Serif'
    HintStyle.CaptionFont.Style = [fsBold]
    HintStyle.Font.Charset = DEFAULT_CHARSET
    HintStyle.Font.Color = clWindowText
    HintStyle.Font.Height = -11
    HintStyle.Font.Name = 'MS Sans Serif'
    HintStyle.Font.Style = []
    HintStyle.RoundRadius = 15
    HintPause = 700
    Left = 216
    Top = 216
  end
  object cxEditStyleController1: TcxEditStyleController
    Style.LookAndFeel.Kind = lfFlat
    Style.LookAndFeel.NativeStyle = True
    StyleDisabled.LookAndFeel.Kind = lfFlat
    StyleDisabled.LookAndFeel.NativeStyle = True
    StyleFocused.LookAndFeel.Kind = lfFlat
    StyleFocused.LookAndFeel.NativeStyle = True
    StyleHot.LookAndFeel.Kind = lfFlat
    StyleHot.LookAndFeel.NativeStyle = True
    Left = 216
    Top = 248
  end
end
