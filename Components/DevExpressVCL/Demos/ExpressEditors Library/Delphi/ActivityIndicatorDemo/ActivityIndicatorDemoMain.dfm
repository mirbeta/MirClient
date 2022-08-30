inherited dxActivityIndicatorDemoForm: TdxActivityIndicatorDemoForm
  Left = 309
  Top = 279
  Caption = 'ExpressEditors ActivityIndicator Demo'
  ClientHeight = 252
  ClientWidth = 584
  Constraints.MinHeight = 310
  Constraints.MinWidth = 600
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 584
    Visible = False
  end
  object tcMain: TcxTabControl [1]
    AlignWithMargins = True
    Left = 8
    Top = 24
    Width = 568
    Height = 220
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    TabOrder = 0
    Properties.CustomButtons.Buttons = <>
    Properties.ShowFrame = True
    OnChange = tcMainChange
    ClientRectBottom = 219
    ClientRectLeft = 1
    ClientRectRight = 567
    ClientRectTop = 1
    object bvlSeparator: TdxBevel
      AlignWithMargins = True
      Left = 399
      Top = 4
      Width = 2
      Height = 212
      Align = alRight
      Shape = dxbsLineCenteredHorz
    end
    object gbSettings: TcxGroupBox
      AlignWithMargins = True
      Left = 404
      Top = 1
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      TabOrder = 0
      Transparent = True
      Height = 218
      Width = 155
      object gbCommon: TcxGroupBox
        Left = 2
        Top = 2
        Align = alTop
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 0
        Transparent = True
        Height = 52
        Width = 151
        object seAnimationTime: TcxSpinEdit
          AlignWithMargins = True
          Left = 5
          Top = 21
          Margins.Top = 0
          Align = alTop
          Properties.Increment = 100.000000000000000000
          Properties.LargeIncrement = 1000.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.SpinButtons.ShowFastButtons = True
          Properties.OnChange = seAnimationTimePropertiesChange
          Style.TransparentBorder = False
          TabOrder = 0
          Value = 1
          Width = 141
        end
        object lbAnimationTime: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 5
          Align = alTop
          Caption = 'Animation Time, ms:'
          Style.TransparentBorder = False
          Transparent = True
        end
      end
      object gbArcBased: TcxGroupBox
        Left = 2
        Top = 54
        Align = alTop
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 1
        Transparent = True
        Height = 96
        Width = 151
        object seArcThickness: TcxSpinEdit
          AlignWithMargins = True
          Left = 5
          Top = 21
          Margins.Top = 0
          Align = alTop
          Properties.LargeIncrement = 5.000000000000000000
          Properties.MaxValue = 30.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.SpinButtons.ShowFastButtons = True
          Properties.OnChange = seArcPropertiesChange
          Style.TransparentBorder = False
          TabOrder = 0
          Value = 1
          Width = 141
        end
        object lbArcThickness: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 5
          Align = alTop
          Caption = 'Arc Thickness:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object lbArcColor: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 46
          Align = alTop
          Caption = 'Arc Color:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object ccbArcColor: TcxColorComboBox
          AlignWithMargins = True
          Left = 5
          Top = 62
          Margins.Top = 0
          Align = alTop
          Properties.AllowSelectColor = True
          Properties.ColorDialogShowFull = True
          Properties.ColorDialogType = cxcdtAdvanced
          Properties.CustomColors = <>
          Properties.OnChange = seArcPropertiesChange
          Style.TransparentBorder = False
          TabOrder = 3
          Width = 141
        end
      end
      object gbDotBased: TcxGroupBox
        Left = 2
        Top = 150
        Align = alTop
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 2
        Transparent = True
        Height = 139
        Width = 151
        object seDotSize: TcxSpinEdit
          AlignWithMargins = True
          Left = 5
          Top = 21
          Margins.Top = 0
          Align = alTop
          Properties.LargeIncrement = 5.000000000000000000
          Properties.MaxValue = 50.000000000000000000
          Properties.MinValue = 2.000000000000000000
          Properties.SpinButtons.ShowFastButtons = True
          Properties.OnChange = seDotPropertiesChange
          Style.TransparentBorder = False
          TabOrder = 0
          Value = 2
          Width = 141
        end
        object lbDotSize: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 5
          Align = alTop
          Caption = 'Dot Size:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object lbDotColor: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 87
          Align = alTop
          Caption = 'Dot Color:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object ccbDotColor: TcxColorComboBox
          AlignWithMargins = True
          Left = 5
          Top = 103
          Margins.Top = 0
          Align = alTop
          Properties.AllowSelectColor = True
          Properties.ColorDialogShowFull = True
          Properties.ColorDialogType = cxcdtAdvanced
          Properties.CustomColors = <>
          Properties.OnChange = seDotPropertiesChange
          Style.TransparentBorder = False
          TabOrder = 3
          Width = 141
        end
        object lbDotCount: TcxLabel
          AlignWithMargins = True
          Left = 5
          Top = 46
          Align = alTop
          Caption = 'Dot Count:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object seDotCount: TcxSpinEdit
          AlignWithMargins = True
          Left = 5
          Top = 62
          Margins.Top = 0
          Align = alTop
          Properties.MaxValue = 10.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.ValueType = vtInt
          Properties.OnChange = seDotPropertiesChange
          Properties.ZeroLargeIncrement = True
          Style.TransparentBorder = False
          TabOrder = 5
          Value = 1
          Width = 141
        end
      end
    end
    object ActivityIndicator: TdxActivityIndicator
      AlignWithMargins = True
      Left = 61
      Top = 61
      Width = 275
      Height = 98
      Margins.Left = 60
      Margins.Top = 60
      Margins.Right = 60
      Margins.Bottom = 60
      Align = alClient
      Active = True
      Transparent = True
    end
  end
end
