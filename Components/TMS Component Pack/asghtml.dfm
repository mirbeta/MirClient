object AdvGridHTMLSettingsForm: TAdvGridHTMLSettingsForm
  Left = 511
  Top = 680
  BorderStyle = bsDialog
  Caption = 'HTML Settings'
  ClientHeight = 334
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object CellsGroupBox: TGroupBox
    Left = 216
    Top = 8
    Width = 209
    Height = 98
    Caption = 'Cells:'
    TabOrder = 1
    object BorderSizeLabel: TLabel
      Left = 37
      Top = 21
      Width = 57
      Height = 13
      Caption = '&Border Size:'
      FocusControl = BorderSizeSpinEdit
    end
    object CellSpacingLabel: TLabel
      Left = 32
      Top = 45
      Width = 62
      Height = 13
      Caption = 'Ce&ll Spacing:'
      FocusControl = CellSpacingSpinEdit
    end
    object Label1: TLabel
      Left = 32
      Top = 72
      Width = 62
      Height = 13
      Caption = 'Cell &Padding:'
      FocusControl = CellPaddingSpinEdit
    end
    object BorderSizeSpinEdit: TAdvSpinEdit
      Left = 112
      Top = 16
      Width = 49
      Height = 22
      Value = 1
      FloatValue = 1.000000000000000000
      TimeValue = 0.041666666666666660
      HexValue = 0
      IncrementFloat = 0.100000000000000000
      IncrementFloatPage = 1.000000000000000000
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      MaxValue = 10
      MaxFloatValue = 100.000000000000000000
      TabOrder = 0
      Transparent = True
      Visible = True
      Version = '1.6.2.0'
    end
    object CellSpacingSpinEdit: TAdvSpinEdit
      Left = 112
      Top = 41
      Width = 49
      Height = 22
      Value = 0
      DateValue = 41376.363583946760000000
      HexValue = 0
      IncrementFloat = 0.100000000000000000
      IncrementFloatPage = 1.000000000000000000
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      MaxValue = 10
      MaxFloatValue = 100.000000000000000000
      TabOrder = 1
      Transparent = True
      Visible = True
      Version = '1.6.2.0'
    end
    object CellPaddingSpinEdit: TAdvSpinEdit
      Left = 112
      Top = 68
      Width = 49
      Height = 22
      Value = 0
      DateValue = 41376.363583958340000000
      HexValue = 0
      IncrementFloat = 0.100000000000000000
      IncrementFloatPage = 1.000000000000000000
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      MaxValue = 10
      MaxFloatValue = 100.000000000000000000
      TabOrder = 2
      Transparent = True
      Visible = True
      Version = '1.6.2.0'
    end
  end
  object TagsGroupBox: TGroupBox
    Left = 215
    Top = 198
    Width = 209
    Height = 97
    Caption = 'Tags:'
    TabOrder = 2
    object PrefixLabel: TLabel
      Left = 40
      Top = 20
      Width = 56
      Height = 13
      Caption = '&Prefix Tags:'
      FocusControl = PrefixEdit
    end
    object SuffixLabel: TLabel
      Left = 40
      Top = 44
      Width = 56
      Height = 13
      Caption = '&Suffix Tags:'
      FocusControl = SuffixEdit
    end
    object TableStyleLabel: TLabel
      Left = 10
      Top = 68
      Width = 86
      Height = 13
      Caption = 'Table St&yle Tags::'
      FocusControl = TableStyleEdit
    end
    object PrefixEdit: TEdit
      Left = 104
      Top = 16
      Width = 89
      Height = 21
      TabOrder = 0
    end
    object SuffixEdit: TEdit
      Left = 104
      Top = 40
      Width = 89
      Height = 21
      TabOrder = 1
    end
    object TableStyleEdit: TEdit
      Left = 104
      Top = 64
      Width = 89
      Height = 21
      TabOrder = 2
    end
  end
  object FilesGroupBox: TGroupBox
    Left = 8
    Top = 198
    Width = 201
    Height = 96
    Caption = 'Files:'
    TabOrder = 3
    object HeaderLabel: TLabel
      Left = 15
      Top = 20
      Width = 38
      Height = 13
      Caption = '&Header:'
      FocusControl = HeaderEdit
    end
    object FooterLabel: TLabel
      Left = 20
      Top = 54
      Width = 33
      Height = 13
      Caption = 'F&ooter:'
      FocusControl = FooterEdit
    end
    object HeaderEdit: TEdit
      Left = 64
      Top = 16
      Width = 89
      Height = 21
      TabOrder = 0
    end
    object FooterEdit: TEdit
      Left = 64
      Top = 50
      Width = 89
      Height = 21
      TabOrder = 2
    end
    object HeaderButton: TButton
      Left = 160
      Top = 16
      Width = 25
      Height = 21
      Caption = '. . .'
      TabOrder = 1
      OnClick = HeaderButtonClick
    end
    object FooterButton: TButton
      Left = 160
      Top = 50
      Width = 25
      Height = 21
      Caption = '. . .'
      TabOrder = 3
      OnClick = HeaderButtonClick
    end
  end
  object GeneralGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 191
    Caption = 'General:'
    TabOrder = 0
    object TableWidthLabel: TLabel
      Left = 8
      Top = 24
      Width = 117
      Height = 13
      Caption = 'Table &Width (in percent):'
      FocusControl = TableWidthSpinEdit
    end
    object TableColorsCheckBox: TCheckBox
      Left = 16
      Top = 51
      Width = 113
      Height = 17
      Caption = 'Use Table'#39's &Colors'
      TabOrder = 1
    end
    object TableFontsCheckBox: TCheckBox
      Left = 16
      Top = 74
      Width = 113
      Height = 17
      Caption = 'Use Table'#39's &Fonts'
      TabOrder = 2
    end
    object TableWidthSpinEdit: TAdvSpinEdit
      Left = 136
      Top = 19
      Width = 49
      Height = 22
      Value = 100
      FloatValue = 100.000000000000000000
      HexValue = 0
      IncrementFloat = 0.100000000000000000
      IncrementFloatPage = 1.000000000000000000
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clWindowText
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      MaxValue = 100
      MaxFloatValue = 100.000000000000000000
      TabOrder = 0
      Transparent = True
      Visible = True
      Version = '1.6.2.0'
    end
    object ConvertChar: TCheckBox
      Left = 16
      Top = 97
      Width = 169
      Height = 17
      Caption = 'Convert special characters'
      TabOrder = 3
    end
    object ExportNonBreaking: TCheckBox
      Left = 16
      Top = 120
      Width = 153
      Height = 17
      Caption = 'Export as non breaking text'
      TabOrder = 4
    end
    object AutoShow: TCheckBox
      Left = 16
      Top = 143
      Width = 121
      Height = 17
      Caption = 'Auto show output'
      TabOrder = 5
    end
  end
  object OKButton: TBitBtn
    Left = 287
    Top = 301
    Width = 66
    Height = 25
    Caption = 'OK'
    Default = True
    DoubleBuffered = True
    ModalResult = 1
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object CancelButton: TBitBtn
    Left = 359
    Top = 301
    Width = 66
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    DoubleBuffered = True
    ModalResult = 2
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object Preview: TButton
    Left = 217
    Top = 301
    Width = 64
    Height = 25
    Caption = 'Preview'
    TabOrder = 6
    OnClick = PreviewClick
  end
  object GroupBox1: TGroupBox
    Left = 216
    Top = 104
    Width = 209
    Height = 95
    Caption = 'Images'
    TabOrder = 7
    object Label2: TLabel
      Left = 16
      Top = 46
      Width = 32
      Height = 13
      Caption = 'Folder:'
    end
    object Label3: TLabel
      Left = 16
      Top = 75
      Width = 36
      Height = 13
      Caption = 'Names:'
    end
    object ExportImg: TCheckBox
      Left = 16
      Top = 18
      Width = 121
      Height = 17
      Caption = 'Export images'
      TabOrder = 0
    end
    object ImgFolder: TEdit
      Left = 71
      Top = 43
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object ImgBaseName: TEdit
      Left = 71
      Top = 68
      Width = 121
      Height = 21
      TabOrder = 2
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.htm'
    Filter = 'HTML Files (*.htm, *.html)|*.htm;*.html|All Files (*.*)|*.*'
    Left = 16
    Top = 280
  end
end
