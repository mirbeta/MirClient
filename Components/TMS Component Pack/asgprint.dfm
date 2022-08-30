object AdvGridPrintSettingsForm: TAdvGridPrintSettingsForm
  Left = 582
  Top = 596
  BorderStyle = bsDialog
  Caption = 'Print Settings'
  ClientHeight = 387
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PreviewPanel: TPanel
    Left = 273
    Top = 0
    Width = 254
    Height = 346
    Align = alClient
    BevelInner = bvLowered
    Color = clWindow
    TabOrder = 0
    object PreviewPaintBox: TPaintBox
      Left = 2
      Top = 2
      Width = 250
      Height = 342
      Align = alClient
      ParentColor = False
      OnPaint = PreviewPaintBoxPaint
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 273
    Height = 346
    ActivePage = GeneralTabSheet
    Align = alLeft
    TabOrder = 1
    object GeneralTabSheet: TTabSheet
      Caption = 'General'
      object BordersGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 265
        Height = 81
        Align = alTop
        Caption = 'Borders:'
        TabOrder = 0
        object BorderLabel: TLabel
          Left = 40
          Top = 20
          Width = 34
          Height = 13
          Caption = '&Border:'
          FocusControl = BorderComboBox
        end
        object BorderStyleLabel: TLabel
          Left = 16
          Top = 52
          Width = 60
          Height = 13
          Caption = 'Border &Style:'
          FocusControl = BorderStyleComboBox
        end
        object BorderComboBox: TComboBox
          Left = 88
          Top = 16
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = BorderComboBoxChange
          Items.Strings = (
            '(none)'
            'Single'
            'Double'
            'Vertical'
            'Horizontal'
            'Around'
            'Around Vertical'
            'Around Horizontal')
        end
        object BorderStyleComboBox: TComboBox
          Left = 88
          Top = 48
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = BorderStyleComboBoxChange
          Items.Strings = (
            'Solid'
            'Dash'
            'Dot'
            'Dash Dot'
            'Dash Dot Dot'
            '(none)')
        end
      end
      object GeneralGroupBox: TGroupBox
        Left = 0
        Top = 81
        Width = 265
        Height = 106
        Align = alTop
        Caption = 'General:'
        TabOrder = 1
        object FitToPageLabel: TLabel
          Left = 16
          Top = 27
          Width = 54
          Height = 13
          Caption = 'Fit to &Page:'
          FocusControl = FitToPageComboBox
        end
        object FitToPageComboBox: TComboBox
          Left = 80
          Top = 23
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = BorderStyleComboBoxChange
          Items.Strings = (
            'Never'
            'Grow to Fit'
            'Shrink to Fit'
            'Always')
        end
        object AutoSizeCheckBox: TCheckBox
          Left = 143
          Top = 51
          Width = 105
          Height = 17
          Caption = '&AutoSize Columns'
          TabOrder = 3
          OnClick = BorderStyleComboBoxChange
        end
        object CenterCheckBox: TCheckBox
          Left = 143
          Top = 83
          Width = 97
          Height = 17
          Caption = '&Center on Page'
          TabOrder = 4
          OnClick = BorderStyleComboBoxChange
        end
        object RepeatRowsCheckBox: TCheckBox
          Left = 8
          Top = 67
          Width = 121
          Height = 17
          Caption = 'Repeat Fixed Ro&ws'
          TabOrder = 2
          OnClick = BorderStyleComboBoxChange
        end
        object RepeatColumnsCheckBox: TCheckBox
          Left = 8
          Top = 51
          Width = 129
          Height = 17
          Caption = 'Repeat Fi&xed Columns'
          TabOrder = 1
          OnClick = BorderStyleComboBoxChange
        end
        object printgraphicscheckbox: TCheckBox
          Left = 8
          Top = 83
          Width = 97
          Height = 17
          Caption = 'Print graphics'
          TabOrder = 5
          OnClick = BorderStyleComboBoxChange
        end
        object AutoSizeRowCheck: TCheckBox
          Left = 143
          Top = 67
          Width = 97
          Height = 17
          Caption = 'AutoSize Rows'
          TabOrder = 6
          OnClick = AutoSizeRowCheckClick
        end
      end
      object FontGroupBox: TGroupBox
        Left = 0
        Top = 187
        Width = 265
        Height = 131
        Align = alClient
        Caption = 'Fonts:'
        TabOrder = 2
        object HeaderFontButton: TSpeedButton
          Left = 16
          Top = 92
          Width = 115
          Height = 25
          Caption = 'Header Font'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777744444444444477777777777777777777444447774444777777447777
            7447777777744777744777777777444444477777777774477447707770777744
            7447708780777774444778000877777744477707077777777447770707777777
            7777778087777777777777707777777777777777777777777777}
          Spacing = 20
          OnClick = HeaderFontButtonClick
        end
        object FooterFontButton: TSpeedButton
          Left = 136
          Top = 92
          Width = 115
          Height = 25
          Caption = 'Footer Font'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777744444444444477777777777777777777444447774444777777447777
            7447777777744777744777777777444444477777777774477447707770777744
            7447708780777774444778000877777744477707077777777447770707777777
            7777778087777777777777707777777777777777777777777777}
          Spacing = 20
          OnClick = FooterFontButtonClick
        end
        object TableFontButton: TSpeedButton
          Left = 16
          Top = 24
          Width = 115
          Height = 25
          Caption = 'Table Font'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777744444444444477777777777777777777444447774444777777447777
            7447777777744777744777777777444444477777777774477447707770777744
            7447708780777774444778000877777744477707077777777447770707777777
            7777778087777777777777707777777777777777777777777777}
          Spacing = 20
          OnClick = TableFontButtonClick
        end
        object SpeedButton1: TSpeedButton
          Left = 136
          Top = 24
          Width = 115
          Height = 25
          Caption = 'Fixed Font'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777744444444444477777777777777777777444447774444777777447777
            7447777777744777744777777777444444477777777774477447707770777744
            7447708780777774444778000877777744477707077777777447770707777777
            7777778087777777777777707777777777777777777777777777}
          Spacing = 20
          OnClick = SpeedButton1Click
        end
        object DisplFont: TCheckBox
          Left = 16
          Top = 64
          Width = 137
          Height = 17
          Caption = 'Use display font'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = DisplFontClick
        end
      end
    end
    object HeadersTabSheet: TTabSheet
      Caption = 'Headers / Footers'
      object DateTimeGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 265
        Height = 121
        Align = alTop
        Caption = 'Date / Time:'
        TabOrder = 0
        object DatePositionLabel: TLabel
          Left = 16
          Top = 26
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = '&Date Position:'
          FocusControl = DatePositionComboBox
        end
        object TimePositionLabel: TLabel
          Left = 16
          Top = 88
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = '&Time Position:'
          FocusControl = TimePositionComboBox
        end
        object DateFormatLabel: TLabel
          Left = 21
          Top = 57
          Width = 61
          Height = 13
          Alignment = taRightJustify
          Caption = 'Date &Format:'
          FocusControl = DateFormatEdit
        end
        object DateFormatEdit: TEdit
          Left = 88
          Top = 53
          Width = 161
          Height = 21
          Enabled = False
          TabOrder = 1
          OnChange = BorderStyleComboBoxChange
        end
        object DatePositionComboBox: TComboBox
          Left = 88
          Top = 22
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = DatePositionComboBoxChange
          Items.Strings = (
            '(none)'
            'Top Left'
            'Top Right'
            'Top Center'
            'Bottom Left'
            'Bottom Right'
            'Bottom Center')
        end
        object TimePositionComboBox: TComboBox
          Left = 88
          Top = 84
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = BorderStyleComboBoxChange
          Items.Strings = (
            '(none)'
            'Top Left'
            'Top Right'
            'Top Center'
            'Bottom Left'
            'Bottom Right'
            'Bottom Center')
        end
      end
      object TitleGroupBox: TGroupBox
        Left = 0
        Top = 121
        Width = 265
        Height = 104
        Align = alTop
        Caption = 'Title:'
        TabOrder = 1
        object TitleTextLabel: TLabel
          Left = 32
          Top = 64
          Width = 47
          Height = 13
          Caption = 'Title Te&xt:'
          FocusControl = TitleMemo
        end
        object TitlePositionLabel: TLabel
          Left = 16
          Top = 26
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = 'Title Po&sition:'
          FocusControl = TitlePositionComboBox
        end
        object TitleMemo: TMemo
          Left = 88
          Top = 56
          Width = 161
          Height = 33
          ScrollBars = ssVertical
          TabOrder = 1
          WordWrap = False
          OnChange = BorderStyleComboBoxChange
        end
        object TitlePositionComboBox: TComboBox
          Left = 88
          Top = 22
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = TitlePositionComboBoxChange
          Items.Strings = (
            '(none)'
            'Top Left'
            'Top Right'
            'Top Center'
            'Bottom Left'
            'Bottom Right'
            'Bottom Center')
        end
      end
      object PageNumbersGroupBox: TGroupBox
        Left = 0
        Top = 225
        Width = 265
        Height = 93
        Align = alClient
        Caption = 'Page Numbers:'
        TabOrder = 2
        object PagesPrefixLabel: TLabel
          Left = 8
          Top = 64
          Width = 29
          Height = 13
          Caption = 'Pr&efix:'
        end
        object PagesSeparatorLabel: TLabel
          Left = 88
          Top = 64
          Width = 49
          Height = 13
          Caption = 'Sep&arator:'
        end
        object PagesSuffixLabel: TLabel
          Left = 192
          Top = 64
          Width = 29
          Height = 13
          Caption = 'S&uffix:'
        end
        object PagesPositionLabel: TLabel
          Left = 6
          Top = 28
          Width = 73
          Height = 13
          Alignment = taRightJustify
          Caption = '&Pages Position:'
          FocusControl = PagesPositionComboBox
        end
        object PagesPrefixEdit: TEdit
          Left = 40
          Top = 60
          Width = 33
          Height = 21
          TabOrder = 1
          OnChange = BorderStyleComboBoxChange
        end
        object PagesSeparatorEdit: TEdit
          Left = 144
          Top = 60
          Width = 33
          Height = 21
          TabOrder = 2
          OnChange = BorderStyleComboBoxChange
        end
        object PagesSuffixEdit: TEdit
          Left = 224
          Top = 60
          Width = 33
          Height = 21
          TabOrder = 3
          OnChange = BorderStyleComboBoxChange
        end
        object PagesPositionComboBox: TComboBox
          Left = 88
          Top = 24
          Width = 161
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = PagesPositionComboBoxChange
          Items.Strings = (
            '(none)'
            'Top Left'
            'Top Right'
            'Top Center'
            'Bottom Left'
            'Bottom Right'
            'Bottom Center')
        end
      end
    end
    object MarginsTabSheet: TTabSheet
      Caption = 'Margins'
      object MarginsGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 265
        Height = 153
        Align = alTop
        Caption = 'Margins:'
        TabOrder = 0
        object TopMarginLabel: TLabel
          Left = 34
          Top = 25
          Width = 60
          Height = 13
          Alignment = taRightJustify
          Caption = '&Top Margin :'
          FocusControl = TopMarginSpinEdit
        end
        object LeftMarginLabel: TLabel
          Left = 35
          Top = 57
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = '&Left Margin :'
          FocusControl = LeftMarginSpinEdit
        end
        object RightMarginLabel: TLabel
          Left = 28
          Top = 121
          Width = 66
          Height = 13
          Alignment = taRightJustify
          Caption = '&Right Margin :'
          FocusControl = RightMarginSpinEdit
        end
        object BottomMarginLabel: TLabel
          Left = 20
          Top = 89
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = '&Bottom Margin :'
          FocusControl = BottomMarginSpinEdit
        end
        object dim1: TLabel
          Left = 176
          Top = 24
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object dim2: TLabel
          Left = 176
          Top = 56
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object dim3: TLabel
          Left = 176
          Top = 88
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object dim4: TLabel
          Left = 176
          Top = 120
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object TopMarginSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 20
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
          HexValue = 0
          IncrementFloat = 0.100000000000000000
          IncrementFloatPage = 1.000000000000000000
          LabelFont.Charset = DEFAULT_CHARSET
          LabelFont.Color = clWindowText
          LabelFont.Height = -11
          LabelFont.Name = 'MS Sans Serif'
          LabelFont.Style = []
          MaxValue = 500
          MaxFloatValue = 100.000000000000000000
          TabOrder = 0
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object LeftMarginSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 52
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
          HexValue = 0
          IncrementFloat = 0.100000000000000000
          IncrementFloatPage = 1.000000000000000000
          LabelFont.Charset = DEFAULT_CHARSET
          LabelFont.Color = clWindowText
          LabelFont.Height = -11
          LabelFont.Name = 'MS Sans Serif'
          LabelFont.Style = []
          MaxValue = 500
          MaxFloatValue = 100.000000000000000000
          TabOrder = 1
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object RightMarginSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 116
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
          HexValue = 0
          IncrementFloat = 0.100000000000000000
          IncrementFloatPage = 1.000000000000000000
          LabelFont.Charset = DEFAULT_CHARSET
          LabelFont.Color = clWindowText
          LabelFont.Height = -11
          LabelFont.Name = 'MS Sans Serif'
          LabelFont.Style = []
          MaxValue = 500
          MaxFloatValue = 100.000000000000000000
          TabOrder = 3
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object BottomMarginSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 84
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
          HexValue = 0
          IncrementFloat = 0.100000000000000000
          IncrementFloatPage = 1.000000000000000000
          LabelFont.Charset = DEFAULT_CHARSET
          LabelFont.Color = clWindowText
          LabelFont.Height = -11
          LabelFont.Name = 'MS Sans Serif'
          LabelFont.Style = []
          MaxValue = 500
          MaxFloatValue = 100.000000000000000000
          TabOrder = 2
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object dimr1: TRadioButton
          Left = 216
          Top = 24
          Width = 41
          Height = 17
          Caption = 'mm'
          Checked = True
          TabOrder = 4
          TabStop = True
          OnClick = dimr1Click
        end
        object dimr2: TRadioButton
          Left = 216
          Top = 48
          Width = 41
          Height = 17
          Caption = 'inch'
          TabOrder = 5
          OnClick = dimr2Click
        end
      end
      object OrientationGroupBox: TGroupBox
        Left = 0
        Top = 265
        Width = 265
        Height = 53
        Align = alClient
        Caption = 'Orientation:'
        TabOrder = 2
        object OrientationLabel: TLabel
          Left = 16
          Top = 24
          Width = 82
          Height = 13
          Caption = '&Page Orientation:'
          FocusControl = OrientationComboBox
        end
        object OrientationComboBox: TComboBox
          Left = 104
          Top = 20
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = OrientationComboBoxChange
          Items.Strings = (
            'Portrait'
            'Landscape')
        end
      end
      object SpacingGroupBox: TGroupBox
        Left = 0
        Top = 153
        Width = 265
        Height = 112
        Align = alTop
        Caption = 'Spacing:'
        TabOrder = 1
        object RowSpacingLabel: TLabel
          Left = 27
          Top = 25
          Width = 70
          Height = 13
          Caption = 'Row &Spacing :'
          FocusControl = RowSpacingSpinEdit
        end
        object ColumnSpacingLabel: TLabel
          Left = 14
          Top = 57
          Width = 83
          Height = 13
          Caption = 'C&olumn Spacing :'
          FocusControl = ColumnSpacingSpinEdit
        end
        object TitleMarginLabel: TLabel
          Left = 37
          Top = 89
          Width = 61
          Height = 13
          Alignment = taRightJustify
          Caption = 'Title &Margin :'
          FocusControl = TitleMarginSpinEdit
        end
        object dim5: TLabel
          Left = 176
          Top = 24
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object dim6: TLabel
          Left = 176
          Top = 56
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object dim7: TLabel
          Left = 176
          Top = 88
          Width = 16
          Height = 13
          Caption = 'mm'
        end
        object RowSpacingSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 20
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
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
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object ColumnSpacingSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 52
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
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
          TabOrder = 1
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
        object TitleMarginSpinEdit: TAdvSpinEdit
          Left = 104
          Top = 84
          Width = 65
          Height = 22
          Precision = 2
          SpinType = sptFloat
          Value = 0
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
          TabOrder = 2
          Transparent = True
          Visible = True
          Version = '1.6.1.1'
          OnChange = RowSpacingSpinEditChange
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Preferences'
      object FileGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 249
        Height = 113
        Caption = 'Save settings'
        TabOrder = 0
        object LoadSettings: TSpeedButton
          Left = 24
          Top = 27
          Width = 201
          Height = 25
          Caption = '&Load settings'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
            55555555FFFFFFFFFF55555000000000055555577777777775F55500B8B8B8B8
            B05555775F555555575F550F0B8B8B8B8B05557F75F555555575550BF0B8B8B8
            B8B0557F575FFFFFFFF7550FBF0000000000557F557777777777500BFBFBFBFB
            0555577F555555557F550B0FBFBFBFBF05557F7F555555FF75550F0BFBFBF000
            55557F75F555577755550BF0BFBF0B0555557F575FFF757F55550FB700007F05
            55557F557777557F55550BFBFBFBFB0555557F555555557F55550FBFBFBFBF05
            55557FFFFFFFFF7555550000000000555555777777777755555550FBFB055555
            5555575FFF755555555557000075555555555577775555555555}
          NumGlyphs = 2
          Spacing = 20
          OnClick = LoadSettingsClick
        end
        object SaveSettings: TSpeedButton
          Left = 24
          Top = 67
          Width = 201
          Height = 25
          Caption = '&Save settings'
          Flat = True
          Glyph.Data = {
            E6040000424DE604000000000000360000002800000014000000140000000100
            180000000000B004000000000000000000000000000000000000C8D0D4C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D400000000848400848400000000
            0000000000000000000000000000C6C6C6C6C6C6000000008484000000C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D40000000084840084840000000000000000
            00000000000000000000C6C6C6C6C6C6000000008484000000C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4000000008484008484000000000000000000000000
            000000000000C6C6C6C6C6C6000000008484000000C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D400000000848400848400000000000000000000000000000000
            0000000000000000000000008484000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4
            C8D0D40000000084840084840084840084840084840084840084840084840084
            84008484008484008484000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D400
            0000008484008484000000000000000000000000000000000000000000000000
            008484008484000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D40000000084
            84000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600000000
            8484000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4000000008484000000
            C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000000084840000
            00C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4000000008484000000C6C6C6C6
            C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000008484000000C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4000000008484000000C6C6C6C6C6C6C6C6
            C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000008484000000C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6
            C6C6C6C6C6C6C6C6C6C6C6C6000000000000000000C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D4000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
            C6C6C6C6C6C6C6C6000000C6C6C6000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4
            C8D0D40000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8
            D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4
            C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0D4C8D0
            D4C8D0D4C8D0D4C8D0D4}
          Spacing = 20
          OnClick = SaveSettingsClick
        end
      end
    end
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 346
    Width = 527
    Height = 41
    Align = alBottom
    TabOrder = 2
    object OKButton: TBitBtn
      Left = 179
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 0
    end
    object CancelButton: TBitBtn
      Left = 267
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 468
    Top = 352
  end
  object OpenDialog1: TOpenDialog
    Filter = 'INI files (*.ini)|*.ini|All files (*.*)|*.*'
    Left = 400
    Top = 354
  end
  object SaveDialog1: TSaveDialog
    Filter = 'INI files (*.ini)|*.ini|All files (*.*)|*.*'
    Left = 432
    Top = 354
  end
end
