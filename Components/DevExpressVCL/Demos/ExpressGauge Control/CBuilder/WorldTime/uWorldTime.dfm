object frmWorldTime: TfrmWorldTime
  Left = 0
  Top = 0
  Caption = 'World Time'
  ClientHeight = 734
  ClientWidth = 1053
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClocks: TPanel
    Left = 0
    Top = 36
    Width = 1053
    Height = 306
    Align = alTop
    BevelOuter = bvNone
    Constraints.MinHeight = 100
    TabOrder = 0
    OnResize = pnlClocksResize
    object pnlWashingtonClock: TPanel
      Left = 0
      Top = 0
      Width = 300
      Height = 300
      BevelOuter = bvNone
      Caption = 'pnlWashingtonClock'
      TabOrder = 0
      object gcWashingtonTime: TdxGaugeControl
        Left = 0
        Top = 20
        Width = 300
        Height = 280
        Align = alClient
        Constraints.MinHeight = 60
        Constraints.MinWidth = 50
        object gsWashingtonTimeBackgroundLayer: TdxGaugeCircularScale
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.ShowNeedle = False
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'White'
          ZOrder = 3
        end
        object gsWashingtonTimeSecondNeedle: TdxGaugeCircularScale
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 7
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.MinorTickCount = 0
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'White'
          ZOrder = 1
        end
        object gsWashingtonTimeHourNeedle: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.620000004768371600
          OptionsLayout.WidthFactor = 0.620000004768371600
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'CleanWhite'
          Value = 1.500000000000000000
        end
        object gsWashingtonTimeMinuteNeedle: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.899999976158142100
          OptionsLayout.WidthFactor = 0.899999976158142100
          OptionsView.ShowBackground = False
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'CleanWhite'
          Value = 20.000000000000000000
          ZOrder = 2
        end
      end
      object cxLabel1: TcxLabel
        Left = 0
        Top = 0
        Align = alTop
        Caption = 'Washington'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -13
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        AnchorX = 150
        AnchorY = 10
      end
    end
    object pnlParisClock: TPanel
      Left = 304
      Top = 0
      Width = 300
      Height = 300
      BevelOuter = bvNone
      TabOrder = 1
      object gcParisTime: TdxGaugeControl
        Left = 0
        Top = 20
        Width = 300
        Height = 280
        Align = alClient
        Constraints.MinHeight = 60
        Constraints.MinWidth = 50
        object dxGaugeCircularScale1: TdxGaugeCircularScale
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.ShowNeedle = False
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'White'
          ZOrder = 3
        end
        object dxGaugeCircularScale2: TdxGaugeCircularScale
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 7
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.MinorTickCount = 0
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'White'
          ZOrder = 1
        end
        object dxGaugeCircularScale3: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.620000004768371600
          OptionsLayout.WidthFactor = 0.620000004768371600
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'CleanWhite'
          Value = 1.500000000000000000
        end
        object dxGaugeCircularScale4: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.899999976158142100
          OptionsLayout.WidthFactor = 0.899999976158142100
          OptionsView.ShowBackground = False
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'CleanWhite'
          Value = 20.000000000000000000
          ZOrder = 2
        end
      end
      object cxLabel3: TcxLabel
        Left = 0
        Top = 0
        Align = alTop
        Caption = 'Paris'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -13
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        AnchorX = 150
        AnchorY = 10
      end
    end
    object pnlMoscowClock: TPanel
      Left = 608
      Top = 0
      Width = 300
      Height = 300
      BevelOuter = bvNone
      TabOrder = 2
      object gcMoscowTime: TdxGaugeControl
        Left = 0
        Top = 20
        Width = 300
        Height = 280
        Align = alClient
        Constraints.MinHeight = 60
        Constraints.MinWidth = 50
        object dxGaugeCircularScale5: TdxGaugeCircularScale
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.ShowNeedle = False
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'White'
          ZOrder = 3
        end
        object dxGaugeCircularScale6: TdxGaugeCircularScale
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 7
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.MinorTickCount = 0
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'White'
          ZOrder = 1
        end
        object dxGaugeCircularScale7: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.620000004768371600
          OptionsLayout.WidthFactor = 0.620000004768371600
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 13
          OptionsView.MaxValue = 12.000000000000000000
          OptionsView.ShowFirstTick = False
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 12.000000000000000000
          StyleName = 'CleanWhite'
          Value = 1.500000000000000000
        end
        object dxGaugeCircularScale8: TdxGaugeCircularScale
          OptionsLayout.HeightFactor = 0.899999976158142100
          OptionsLayout.WidthFactor = 0.899999976158142100
          OptionsView.ShowBackground = False
          OptionsView.MaxValue = 60.000000000000000000
          OptionsView.ShowLabels = False
          OptionsView.ShowTicks = False
          OptionsView.AngleEnd = -270
          OptionsView.AngleStart = 90
          OptionsView.MaxValue = 60.000000000000000000
          StyleName = 'CleanWhite'
          Value = 20.000000000000000000
          ZOrder = 2
        end
      end
      object cxLabel2: TcxLabel
        Left = 0
        Top = 0
        Align = alTop
        Caption = 'Moscow'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -13
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        AnchorX = 150
        AnchorY = 10
      end
    end
  end
  object pnlLocalTime: TPanel
    Left = 0
    Top = 356
    Width = 1053
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlLocalTime'
    Constraints.MinHeight = 30
    TabOrder = 1
    object gcLocalTime: TdxGaugeControl
      Left = 0
      Top = 0
      Width = 1053
      Height = 378
      Hint = 'Local time'
      Align = alClient
      Constraints.MinHeight = 30
      ShowHint = True
      object gsDigital: TdxGaugeDigitalScale
        OptionsLayout.CenterPositionFactorY = 0.503921568393707300
        OptionsView.DigitCount = 6
        OptionsView.DisplayMode = sdmSevenSegment
        OptionsView.SegmentColorOff = 0
        StyleName = 'White'
      end
    end
  end
  object cxSplitter1: TcxSplitter
    Left = 0
    Top = 342
    Width = 1053
    Height = 14
    AlignSplitter = salBottom
    Control = pnlClocks
  end
  object lbDescription: TcxLabel
    Left = 0
    Top = 0
    Align = alTop
    Caption = 
      'In this demo, gauges are used to show the current time in certai' +
      'n cities around the world. Resize the form'#39's window or drag the ' +
      'splitter under the clocks to scale the gauges.'
    ParentColor = False
    ParentFont = False
    Style.Color = 12937777
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -13
    Style.Font.Name = 'MS Sans Serif'
    Style.Font.Style = [fsBold]
    Style.TextColor = clWhite
    Style.IsFontAssigned = True
    Properties.WordWrap = True
    Width = 1053
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 448
    Top = 320
  end
end
