inherited dxBarCodeDemoForm: TdxBarCodeDemoForm
  Left = 0
  Top = 0
  Caption = 'ExpressEditors BarCode Demo'
  ClientHeight = 550
  ClientWidth = 700
  Constraints.MinHeight = 550
  Constraints.MinWidth = 700
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 700
    Height = 0
    AutoSize = False
    Visible = False
  end
  object tcMain: TcxTabControl [1]
    AlignWithMargins = True
    Left = 8
    Top = 24
    Width = 684
    Height = 518
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 0
    Properties.CustomButtons.Buttons = <>
    Properties.MultiLine = True
    Properties.ShowFrame = True
    Properties.Tabs.Strings = (
      'Code 11'
      'Code 39'
      'Code 39 Extended'
      'Code 93'
      'Code 93 Extended'
      'Code128'
      'EAN-8'
      'EAN-13'
      'Interleaved 2 of 5'
      'MSI'
      'UPC-A'
      'UPC-E'
      'QR Code')
    OnChange = tcMainChange
    ClientRectBottom = 517
    ClientRectLeft = 1
    ClientRectRight = 683
    ClientRectTop = 48
    object bvlSeparator: TdxBevel
      AlignWithMargins = True
      Left = 426
      Top = 51
      Width = 2
      Height = 463
      Margins.Right = 2
      Align = alRight
      Shape = dxbsLineCenteredHorz
    end
    object gbSettings: TcxGroupBox
      AlignWithMargins = True
      Left = 430
      Top = 48
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alRight
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      TabOrder = 0
      Transparent = True
      Height = 469
      Width = 250
      object pcSettings: TcxPageControl
        Left = 2
        Top = 76
        Width = 246
        Height = 391
        Align = alClient
        TabOrder = 0
        Properties.ActivePage = tsBaseProperties
        Properties.CustomButtons.Buttons = <>
        Properties.MultiLine = True
        ClientRectBottom = 391
        ClientRectRight = 246
        ClientRectTop = 24
        object tsBaseProperties: TcxTabSheet
          Caption = 'Common Properties'
          ImageIndex = 0
          object gbFontSize: TcxGroupBox
            Left = 0
            Top = 0
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 0
            Transparent = True
            Height = 52
            Width = 246
            object lbFontSize: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Font Size:'
              Transparent = True
            end
            object seFontSize: TcxSpinEdit
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.MaxValue = 30.000000000000000000
              Properties.MinValue = 6.000000000000000000
              Properties.OnChange = seFontSizePropertiesChange
              TabOrder = 1
              Value = 13
              Width = 236
            end
          end
          object gbRotationAngle: TcxGroupBox
            Left = 0
            Top = 52
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 1
            Transparent = True
            Height = 52
            Width = 246
            object lbRotationAngle: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Rotation Angle:'
              Transparent = True
            end
            object cbRotationAngle: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                '0'
                '-90'
                '90'
                '180')
              Properties.OnChange = cbRotationAnglePropertiesChange
              TabOrder = 1
              Text = '0'
              Width = 236
            end
          end
          object gbModuleWidth: TcxGroupBox
            Left = 0
            Top = 104
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 2
            Transparent = True
            Height = 52
            Width = 246
            object lbModuleWidth: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Module Size:'
              Transparent = True
            end
            object seModuleWidth: TcxSpinEdit
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.MaxValue = 20.000000000000000000
              Properties.MinValue = 1.000000000000000000
              Properties.OnChange = seModuleWidthPropertiesChange
              TabOrder = 1
              Value = 2
              Width = 236
            end
          end
          object gbFitMode: TcxGroupBox
            Left = 0
            Top = 156
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 3
            Transparent = True
            Height = 52
            Width = 246
            object lbModuleHeight: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Fit Mode:'
              Transparent = True
            end
            object cbFitMode: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'Normal'
                'Stretch'
                'ProportionalStretch'
                'Fit')
              Properties.OnChange = cbFitModePropertiesChange
              TabOrder = 1
              Text = 'Normal'
              Width = 236
            end
          end
          object gbShowText: TcxGroupBox
            Left = 0
            Top = 208
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 4
            Transparent = True
            Height = 52
            Width = 246
            object lbShowText: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Show Text:'
              Transparent = True
            end
            object tsShowText: TdxToggleSwitch
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Caption = 'Show Text:'
              Checked = True
              Properties.Alignment = taCenter
              Properties.StateIndicator.Kind = sikText
              Properties.OnChange = tsShowTextPropertiesChange
              TabOrder = 1
              Transparent = True
              Width = 236
            end
          end
        end
        object tsCustomProperties: TcxTabSheet
          Caption = 'Specific Properties'
          ImageIndex = 1
          object gbWideNarrowRatio: TcxGroupBox
            Left = 0
            Top = 0
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 0
            Transparent = True
            Height = 52
            Width = 246
            object lbWideNarrowRatio: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Wide Narrow Ratio:'
              Transparent = True
            end
            object seWideNarrowRatio: TcxSpinEdit
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.Increment = 0.100000000000000000
              Properties.LargeIncrement = 1.000000000000000000
              Properties.MaxValue = 3.000000000000000000
              Properties.MinValue = 2.000000000000000000
              Properties.ValueType = vtFloat
              Properties.OnChange = seWideNarrowRatioPropertiesChange
              TabOrder = 1
              Value = 2.000000000000000000
              Width = 236
            end
          end
          object gbCalculateCheckSum: TcxGroupBox
            Left = 0
            Top = 52
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 1
            Transparent = True
            Height = 52
            Width = 246
            object lbCalculateCheckSum: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Checksum:'
              Transparent = True
            end
            object tsCalculateCheckSum: TdxToggleSwitch
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Caption = 'Show Text:'
              Checked = True
              Properties.Alignment = taCenter
              Properties.StateIndicator.Kind = sikText
              Properties.OnChange = tsCalculateCheckSumPropertiesChange
              TabOrder = 1
              Transparent = True
              Width = 236
            end
          end
          object gbCharacterSet: TcxGroupBox
            Left = 0
            Top = 104
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 2
            Transparent = True
            Height = 52
            Width = 246
            object lbCharacterSet: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Character Set:'
              Transparent = True
            end
            object cbCharacterSet: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'Auto'
                'A'
                'B'
                'C')
              Properties.OnChange = cbCharacterSetPropertiesChange
              TabOrder = 1
              Text = 'Auto'
              Width = 236
            end
          end
          object gbCompactionMode: TcxGroupBox
            Left = 0
            Top = 156
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 3
            Transparent = True
            Height = 52
            Width = 246
            object lbCompactionMode: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Compaction Mode:'
              Transparent = True
            end
            object cbCompactionMode: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'Numeric'
                'AlphaNumeric'
                'Byte')
              Properties.OnChange = cbCompactionModePropertiesChange
              TabOrder = 1
              Text = 'Byte'
              Width = 236
            end
          end
          object gbErrorCorrectionLevel: TcxGroupBox
            Left = 0
            Top = 208
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 4
            Transparent = True
            Height = 52
            Width = 246
            object lbErrorCorrectionLevel: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Error Correction Level:'
              Transparent = True
            end
            object cbErrorCorrectionLevel: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'L'
                'M'
                'Q'
                'H')
              Properties.OnChange = cbErrorCorrectionLevelPropertiesChange
              TabOrder = 1
              Text = 'M'
              Width = 236
            end
          end
          object gbSizeVersion: TcxGroupBox
            Left = 0
            Top = 260
            Align = alTop
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            TabOrder = 5
            Transparent = True
            Height = 52
            Width = 246
            object lbSizeVersion: TcxLabel
              AlignWithMargins = True
              Left = 5
              Top = 5
              Align = alTop
              Caption = 'Size Version:'
              Transparent = True
            end
            object cbSizeVersion: TcxComboBox
              AlignWithMargins = True
              Left = 5
              Top = 25
              Margins.Top = 0
              Align = alTop
              Properties.DropDownListStyle = lsFixedList
              Properties.Items.Strings = (
                'Auto'
                'Version 1'
                'Version 2'
                'Version 3'
                'Version 4'
                'Version 5'
                'Version 6'
                'Version 7'
                'Version 8'
                'Version 9'
                'Version 10'
                'Version 11'
                'Version 12'
                'Version 13'
                'Version 14'
                'Version 15'
                'Version 16'
                'Version 17'
                'Version 18'
                'Version 19'
                'Version 20'
                'Version 21'
                'Version 22'
                'Version 23'
                'Version 24'
                'Version 25'
                'Version 26'
                'Version 27'
                'Version 28'
                'Version 29'
                'Version 30'
                'Version 31'
                'Version 32'
                'Version 33'
                'Version 34'
                'Version 35'
                'Version 36'
                'Version 37'
                'Version 38'
                'Version 39'
                'Version 40')
              Properties.OnChange = cbSizeVersionPropertiesChange
              TabOrder = 1
              Text = 'Auto'
              Width = 236
            end
          end
        end
      end
      object lbCodeText: TcxLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Align = alTop
        Caption = 'Text:'
        Transparent = True
      end
      object memText: TcxMemo
        AlignWithMargins = True
        Left = 2
        Top = 25
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Align = alTop
        Lines.Strings = (
          '0123456789000')
        Properties.OnChange = memTextPropertiesChange
        TabOrder = 2
        Height = 45
        Width = 246
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 48
      Width = 422
      Height = 469
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object BarCode: TdxBarCode
        AlignWithMargins = True
        Left = 3
        Top = 3
        Align = alClient
        Enabled = False
        ParentFont = False
        Properties.BarCodeSymbologyClassName = 'TdxBarCodeEAN13Symbology'
        Style.BorderStyle = ebsNone
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clBlack
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        Transparent = True
      end
    end
  end
end
