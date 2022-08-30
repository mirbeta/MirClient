object frmSampleGauges: TfrmSampleGauges
  Left = 0
  Top = 0
  Caption = 'Sample Gauges'
  ClientHeight = 538
  ClientWidth = 970
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 36
    Width = 970
    Height = 502
    ActivePage = tsHybridGauges
    Align = alClient
    Constraints.MinHeight = 150
    Constraints.MinWidth = 180
    TabOrder = 0
    OnChange = PageControl1Change
    OnResize = PageControl1Resize
    object tsCircularGauges: TTabSheet
      Caption = 'Full Circular Gauges'
      object gcCircularWhite: TdxGaugeControl
        Left = 256
        Top = 5
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularScale4: TdxGaugeCircularScale
          OptionsView.MinorTickCount = 5
          OptionsView.LabelOrientation = loCircularOutward
          StyleName = 'White'
          Value = 25.000000000000000000
          object dxGaugeCircularScale4Caption1: TdxGaugeQuantitativeScaleCaption
            Text = 'bar'
            OptionsLayout.CenterPositionFactorY = 0.829999983310699500
            OptionsView.Font.Charset = DEFAULT_CHARSET
            OptionsView.Font.Color = clGrayText
            OptionsView.Font.Height = -16
            OptionsView.Font.Name = 'Tahoma'
            OptionsView.Font.Style = []
            OptionsView.UseOwnFont = True
          end
          object dxGaugeCircularScale4Caption2: TdxGaugeQuantitativeScaleCaption
            Text = 'EN 837-1'
            OptionsLayout.CenterPositionFactorY = 0.400000005960464400
            OptionsView.Font.Charset = DEFAULT_CHARSET
            OptionsView.Font.Color = clGrayText
            OptionsView.Font.Height = -8
            OptionsView.Font.Name = 'Tahoma'
            OptionsView.Font.Style = []
            OptionsView.UseOwnFont = True
          end
          object dxGaugeCircularScale4Range: TdxGaugeCircularScaleRange
            Color = -16744448
            RadiusFactor = 0.550000011920929000
            ValueEnd = 40.000000000000000000
          end
          object dxGaugeCircularScale4Range1: TdxGaugeCircularScaleRange
            Color = -256
            RadiusFactor = 0.550000011920929000
            ValueEnd = 80.000000000000000000
            ValueStart = 40.000000000000000000
          end
          object dxGaugeCircularScale4Range2: TdxGaugeCircularScaleRange
            Color = -65536
            RadiusFactor = 0.550000011920929000
            ValueEnd = 100.000000000000000000
            ValueStart = 80.000000000000000000
          end
        end
      end
      object gcCircularDeepFire: TdxGaugeControl
        Left = 0
        Top = 5
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularScale3: TdxGaugeCircularScale
          OptionsView.MinorTickCount = 5
          StyleName = 'DeepFire'
          Value = 45.000000000000000000
          ZOrder = 1
          object dxGaugeCircularScale3Caption1: TdxGaugeQuantitativeScaleCaption
          end
          object dxGaugeCircularScale3Caption2: TdxGaugeQuantitativeScaleCaption
            Text = 'km/h'
            OptionsLayout.CenterPositionFactorX = 0.239999994635582000
            OptionsLayout.CenterPositionFactorY = 0.779999971389770500
            OptionsView.Font.Charset = DEFAULT_CHARSET
            OptionsView.Font.Color = clSilver
            OptionsView.Font.Height = -9
            OptionsView.Font.Name = 'Tahoma'
            OptionsView.Font.Style = []
            OptionsView.RotationAngle = -50.000000000000000000
            OptionsView.UseOwnFont = True
          end
        end
        object dxGaugeControl6dxGaugeCircularScale1: TdxGaugeCircularScale
          AnchorScaleIndex = 0
          OptionsLayout.HeightFactor = 0.600000023841857900
          OptionsLayout.WidthFactor = 0.600000023841857900
          OptionsView.ShowBackground = False
          OptionsView.MajorTickCount = 9
          OptionsView.MaxValue = 8.000000000000000000
          OptionsView.MinorTickCount = 2
          OptionsView.AngleEnd = 45
          OptionsView.ShowSpindleCap = False
          OptionsView.MaxValue = 8.000000000000000000
          StyleName = 'DeepFire'
          Value = 3.500000000000000000
          object dxGaugeControl6dxGaugeCircularScale1Caption1: TdxGaugeQuantitativeScaleCaption
            Text = 'RPMx1000'
            OptionsLayout.CenterPositionFactorY = 0.699999988079071100
          end
        end
      end
      object gcCircularCleanWhite: TdxGaugeControl
        Left = 512
        Top = 6
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularScale1: TdxGaugeCircularScale
          OptionsView.MinorTickCount = 5
          OptionsView.LabelOrientation = loCircular
          StyleName = 'CleanWhite'
          Value = 76.000000000000000000
        end
      end
    end
    object tsCircularHalfGauges: TTabSheet
      Caption = 'Half-Circular Gauges'
      ImageIndex = 4
      object gcCircularHalfYellowSubmarine: TdxGaugeControl
        Left = 0
        Top = 5
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl4CircularHalfScale1: TdxGaugeCircularHalfScale
          OptionsView.LabelOrientation = loCircular
          StyleName = 'Smart'
          Value = 35.000000000000000000
        end
        object dxGaugeControl4CircularHalfScale2: TdxGaugeCircularHalfScale
          AnchorScaleIndex = 0
          OptionsLayout.CenterPositionFactorY = 0.579394817352294900
          OptionsLayout.HeightFactor = 0.600000023841857900
          OptionsLayout.WidthFactor = 0.600000023841857900
          OptionsView.ShowBackground = False
          OptionsView.LabelOrientation = loCircular
          OptionsView.ShowNeedle = False
          OptionsView.ShowSpindleCap = False
          StyleName = 'Smart'
          Value = 50.000000000000000000
          ZOrder = 1
        end
      end
      object gcCircularHalfClassic: TdxGaugeControl
        Left = 264
        Top = 0
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularHalfScale1: TdxGaugeCircularHalfScale
          StyleName = 'Classic'
          Value = 45.000000000000000000
        end
      end
      object gcCircularHalfCleanWhite: TdxGaugeControl
        Left = 518
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularHalfScale2: TdxGaugeCircularHalfScale
          OptionsView.LabelOrientation = loRadial
          StyleName = 'CleanWhite'
          Value = 75.000000000000000000
          object dxGaugeCircularHalfScale2Range: TdxGaugeCircularScaleRange
            Color = -16744448
            RadiusFactor = 0.949999988079071100
            ValueEnd = 80.000000000000000000
            ValueStart = 20.000000000000000000
            WidthFactor = 0.050000000745058060
          end
          object dxGaugeCircularHalfScale2Range1: TdxGaugeCircularScaleRange
            Color = -256
            RadiusFactor = 0.949999988079071100
            ValueEnd = 20.000000000000000000
            WidthFactor = 0.050000000745058060
          end
          object dxGaugeCircularHalfScale2Range2: TdxGaugeCircularScaleRange
            Color = -65536
            RadiusFactor = 0.949999988079071100
            ValueEnd = 100.000000000000000000
            ValueStart = 80.000000000000000000
            WidthFactor = 0.050000000745058060
          end
        end
      end
    end
    object tsCircularQuarterGauges: TTabSheet
      Caption = 'Quarter-Circular Gauges'
      ImageIndex = 5
      object gcCircularQuarterYellowSubmarine: TdxGaugeControl
        Left = 3
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl7CircularQuarterLeftScale1: TdxGaugeCircularQuarterLeftScale
          OptionsView.LabelOrientation = loCircularOutward
          StyleName = 'YellowSubmarine'
          Value = 20.000000000000000000
        end
      end
      object gcCircularQuarterDeepFire: TdxGaugeControl
        Left = 263
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl8CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale
          StyleName = 'DeepFire'
          Value = 54.000000000000000000
          object dxGaugeControl8CircularQuarterRightScale1Range: TdxGaugeCircularScaleRange
            Color = -16744448
            ValueEnd = 40.000000000000000000
          end
          object dxGaugeControl8CircularQuarterRightScale1Range1: TdxGaugeCircularScaleRange
            Color = -65536
            ValueEnd = 100.000000000000000000
            ValueStart = 75.000000000000000000
          end
          object dxGaugeControl8CircularQuarterRightScale1Range2: TdxGaugeCircularScaleRange
            Color = -256
            ValueEnd = 75.000000000000000000
            ValueStart = 40.000000000000000000
          end
        end
      end
      object gcCircularQuarterSmart: TdxGaugeControl
        Left = 522
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl9CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale
          StyleName = 'DarkNight'
          Value = 30.000000000000000000
        end
      end
    end
    object tsCircularThreeFourth: TTabSheet
      Caption = 'Three-Fourth Circular gauges'
      ImageIndex = 7
      object gcCircularThreeFourthAfrica: TdxGaugeControl
        Left = 3
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl5CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale
          Value = 40.000000000000000000
        end
      end
      object gcCircularThreeFourthFuture: TdxGaugeControl
        Left = 263
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl6CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale
          StyleName = 'Future'
          Value = 50.000000000000000000
        end
      end
      object gcCircularThreeFourthDisco: TdxGaugeControl
        Left = 522
        Top = 3
        Width = 250
        Height = 250
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl7CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale
          StyleName = 'Disco'
          Value = 75.000000000000000000
        end
      end
    end
    object tsCircularWide: TTabSheet
      Caption = 'Wide Circular Gauges'
      ImageIndex = 6
      object gcCircularWideMechanical: TdxGaugeControl
        Left = 259
        Top = 162
        Width = 250
        Height = 142
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object Scale3: TdxGaugeCircularWideScale
          StyleName = 'Mechanical'
          Value = 45.000000000000000000
        end
      end
      object gcCircularWideWhite: TdxGaugeControl
        Left = 3
        Top = 162
        Width = 250
        Height = 142
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object Scale2: TdxGaugeCircularWideScale
          StyleName = 'White'
          Value = 80.000000000000000000
        end
      end
      object gcCircularWideDeepFire: TdxGaugeControl
        Left = 515
        Top = 162
        Width = 250
        Height = 142
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object Scale1: TdxGaugeCircularWideScale
          StyleName = 'DeepFire'
          Value = 20.000000000000000000
        end
      end
      object gcCircularWideSportCar: TdxGaugeControl
        Left = 259
        Top = 3
        Width = 250
        Height = 153
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularWideScale1: TdxGaugeCircularWideScale
          StyleName = 'SportCar'
          Value = 44.000000000000000000
        end
      end
      object gcCircularWideAfrica: TdxGaugeControl
        Left = 3
        Top = 3
        Width = 250
        Height = 153
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularWideScale2: TdxGaugeCircularWideScale
          Value = 35.000000000000000000
        end
      end
      object gcCircularWideDarkNight: TdxGaugeControl
        Left = 515
        Top = 3
        Width = 250
        Height = 153
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeCircularWideScale3: TdxGaugeCircularWideScale
          StyleName = 'DarkNight'
          Value = 80.000000000000000000
        end
      end
    end
    object tsLinearGauges: TTabSheet
      Caption = 'Linear Gauges'
      ImageIndex = 3
      object gcLinearSmart: TdxGaugeControl
        Left = 320
        Top = 8
        Width = 441
        Height = 258
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeLinearScale1: TdxGaugeLinearScale
          OptionsView.AlignElements = taRightJustify
          OptionsView.RotationAngle = ra180
          StyleName = 'Smart'
          Value = 50.000000000000000000
        end
        object dxGaugeControl2LinearScale1: TdxGaugeLinearScale
          AnchorScaleIndex = 0
          OptionsView.ShowBackground = False
          OptionsView.ShowLevelBar = False
          OptionsView.RotationAngle = ra180
          StyleName = 'YellowSubmarine'
          Value = 50.000000000000000000
          ZOrder = 1
          object dxGaugeControl2LinearScale1Range: TdxGaugeLinearScaleRange
            CenterPositionFactor = 0.360000014305114800
            Color = -13640900
            ValueEnd = 20.000000000000000000
            WidthFactor = 0.050000000745058060
            CenterPositionFactor = 0.360000014305114800
          end
          object dxGaugeControl2LinearScale1Range1: TdxGaugeLinearScaleRange
            CenterPositionFactor = 0.360000014305114800
            Color = -256
            ValueEnd = 80.000000000000000000
            ValueStart = 20.000000000000000000
            WidthFactor = 0.050000000745058060
            CenterPositionFactor = 0.360000014305114800
          end
        end
      end
      object gcLinearYellowSubmarine: TdxGaugeControl
        Left = 160
        Top = 8
        Width = 143
        Height = 258
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeLinearScale2: TdxGaugeLinearScale
          OptionsView.LabelOrientation = toBottomToTop
          StyleName = 'YellowSubmarine'
          Value = 84.000000000000000000
        end
        object dxGaugeControl3LinearScale1: TdxGaugeLinearScale
          AnchorScaleIndex = 0
          OptionsView.ShowBackground = False
          OptionsView.MaxValue = 500.000000000000000000
          OptionsView.AlignElements = taRightJustify
          OptionsView.ShowLevelBar = False
          OptionsView.MaxValue = 500.000000000000000000
          StyleName = 'YellowSubmarine'
          Value = 84.000000000000000000
          ZOrder = 1
        end
      end
      object gcLinearIceColdZone: TdxGaugeControl
        Left = 0
        Top = 8
        Width = 143
        Height = 258
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeControl1LinearScale1: TdxGaugeLinearScale
          StyleName = 'DarkNight'
          Value = 35.000000000000000000
          object dxGaugeControl1LinearScale1Caption1: TdxGaugeQuantitativeScaleCaption
            Text = 'Temperature'
            OptionsLayout.CenterPositionFactorX = 0.720000028610229500
            OptionsLayout.CenterPositionFactorY = 0.699999988079071100
            OptionsView.RotationAngle = 90.000000000000000000
          end
        end
      end
    end
    object tsDigital: TTabSheet
      Caption = 'Digital Gauges'
      ImageIndex = 1
      object gcDigitalIceColdZone: TdxGaugeControl
        Left = 11
        Top = 3
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale3: TdxGaugeDigitalScale
          OptionsView.DigitCount = 6
          StyleName = 'IceColdZone'
          Value = '123.4'
        end
      end
      object gcDigitalDeepFire: TdxGaugeControl
        Left = 11
        Top = 129
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale5: TdxGaugeDigitalScale
          OptionsView.DigitCount = 6
          StyleName = 'DeepFire'
          Value = '23:12:14'
        end
      end
      object gcDigitalWhite: TdxGaugeControl
        Left = 393
        Top = 6
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale6: TdxGaugeDigitalScale
          OptionsView.DigitCount = 0
          OptionsView.DisplayMode = sdmMatrix8x14Dots
          OptionsView.SegmentColorOff = 0
          StyleName = 'White'
          Value = 'Gauges'
        end
      end
      object gcDigitalScaleText: TdxGaugeControl
        Left = 393
        Top = 132
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale7: TdxGaugeDigitalScale
          OptionsView.DigitCount = 8
          OptionsView.SegmentColorOff = 0
          StyleName = 'YellowSubmarine'
          Value = '+12,4 ABC'
        end
      end
      object gcDigitalClassic: TdxGaugeControl
        Left = 11
        Top = 255
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale1: TdxGaugeDigitalScale
          OptionsView.DigitCount = 0
          OptionsView.DisplayMode = sdmMatrix8x14Dots
          StyleName = 'Disco'
          Value = 'All Symbols @->-'
        end
      end
      object gcDigitalFuture: TdxGaugeControl
        Left = 393
        Top = 257
        Width = 360
        Height = 120
        BorderStyle = cxcbsNone
        LookAndFeel.NativeStyle = True
        Transparent = True
        object dxGaugeDigitalScale8: TdxGaugeDigitalScale
          OptionsView.DigitCount = 0
          OptionsView.DisplayMode = sdmMatrix8x14Squares
          StyleName = 'Future'
          Value = '+12 C'#39
        end
      end
    end
    object tsHybridGauges: TTabSheet
      Caption = 'Hybrid Gauges'
      ImageIndex = 5
      object pcHybridGauges: TPageControl
        Left = 0
        Top = 0
        Width = 962
        Height = 474
        ActivePage = tsWeather
        Align = alClient
        DoubleBuffered = False
        ParentDoubleBuffered = False
        TabOrder = 0
        OnChange = pcHybridGaugesChange
        object tsCarTester: TTabSheet
          Caption = 'Car Brake and Suspension Tester'
          object gcTester: TdxGaugeControl
            Left = 0
            Top = 0
            Width = 954
            Height = 446
            Align = alClient
            BorderStyle = cxcbsNone
            Transparent = True
            object gsMainScaleBackground: TdxGaugeCircularScale
              AnchorScaleIndex = 10
              OptionsLayout.WidthFactor = 0.467820912599563600
              OptionsView.ShowFirstTick = False
              OptionsView.ShowLabels = False
              OptionsView.ShowLastTick = False
              OptionsView.ShowTicks = False
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              StyleName = 'White'
              ZOrder = 7
            end
            object gsColoredScaleBackground: TdxGaugeCircularScale
              AnchorScaleIndex = 10
              OptionsLayout.CenterPositionFactorX = 0.244455292820930500
              OptionsLayout.CenterPositionFactorY = 0.494536280632019100
              OptionsLayout.HeightFactor = 0.600000023841857900
              OptionsLayout.WidthFactor = 0.271543115377426100
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.ShowNeedle = False
              StyleName = 'White'
              Value = 30.000000000000000000
              ZOrder = 3
            end
            object gsTesterValue4: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 70
              OptionsView.AngleStart = 290
              StyleName = 'CleanWhite'
              Value = 45.000000000000000000
              ZOrder = 2
              object dxGaugeControl3CircularScale3Range: TdxGaugeCircularScaleRange
                Color = -7618928
                RadiusFactor = 0.800000011920928900
                ValueEnd = 30.000000000000000000
                WidthFactor = 0.400000005960464400
              end
              object dxGaugeControl3CircularScale3Range1: TdxGaugeCircularScaleRange
                Color = -1517978
                RadiusFactor = 0.800000011920928900
                ValueEnd = 70.000000000000000000
                ValueStart = 30.000000000000000000
                WidthFactor = 0.400000005960464400
              end
              object dxGaugeControl3CircularScale3Range2: TdxGaugeCircularScaleRange
                Color = -4362642
                RadiusFactor = 0.800000011920928900
                ValueEnd = 100.000000000000000000
                ValueStart = 70.000000000000000000
                WidthFactor = 0.400000005960464400
              end
            end
            object gsTesterValue5: TdxGaugeDigitalScale
              AnchorScaleIndex = 10
              OptionsLayout.CenterPositionFactorX = 0.759197115898132400
              OptionsLayout.CenterPositionFactorY = 0.280619114637374900
              OptionsLayout.HeightFactor = 0.150000005960464500
              OptionsLayout.WidthFactor = 0.192555949091911300
              OptionsView.DisplayMode = sdmSevenSegment
              OptionsView.SegmentColorOff = 0
              StyleName = 'CleanWhite'
              Value = '3,4'
              ZOrder = 5
            end
            object gsTesterValue6: TdxGaugeDigitalScale
              AnchorScaleIndex = 10
              OptionsLayout.CenterPositionFactorX = 0.779332637786865300
              OptionsLayout.CenterPositionFactorY = 0.457055240869522100
              OptionsLayout.HeightFactor = 0.150000005960464500
              OptionsLayout.WidthFactor = 0.185411453247070300
              OptionsView.DisplayMode = sdmSevenSegment
              OptionsView.SegmentColorOff = 0
              StyleName = 'CleanWhite'
              Value = '78'
              ZOrder = 4
            end
            object gsTesterValue7: TdxGaugeDigitalScale
              AnchorScaleIndex = 10
              OptionsLayout.CenterPositionFactorX = 0.759197115898132400
              OptionsLayout.CenterPositionFactorY = 0.639059305191040000
              OptionsLayout.HeightFactor = 0.150000005960464500
              OptionsLayout.WidthFactor = 0.187312915921211200
              OptionsView.DisplayMode = sdmSevenSegment
              OptionsView.SegmentColorOff = 0
              StyleName = 'CleanWhite'
              Value = '1345'
              ZOrder = 6
            end
            object gsTesterValue3: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsLayout.CenterPositionFactorY = 0.560000002384185800
              OptionsLayout.HeightFactor = 0.451999992132186900
              OptionsLayout.WidthFactor = 0.451999992132186900
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -13
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 3
              OptionsView.MaxValue = 20.000000000000000000
              OptionsView.MinorTickCount = 5
              OptionsView.MinValue = -20.000000000000000000
              OptionsView.AngleEnd = 320
              OptionsView.AngleStart = 220
              OptionsView.ShowSpindleCap = False
              OptionsView.MaxValue = 20.000000000000000000
              StyleName = 'CleanWhite'
              Value = -10.000000000000000000
              ZOrder = 11
              object gsTesterValue3Caption1: TdxGaugeQuantitativeScaleCaption
                Text = '[ mm/m ]'
                OptionsLayout.CenterPositionFactorY = 0.720000028610229500
              end
            end
            object gsScaleTicks2: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsLayout.HeightFactor = 0.759999990463256800
              OptionsLayout.WidthFactor = 0.759999990463256800
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -11
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MinorTickCount = 1
              OptionsView.AngleEnd = -20
              OptionsView.AngleStart = 200
              OptionsView.ShowNeedle = False
              StyleName = 'YellowSubmarine'
              ZOrder = 8
              object dxGaugeControl3CircularScale5Range: TdxGaugeCircularScaleRange
                Color = -11912395
                RadiusFactor = 0.790000021457672100
                ValueEnd = 100.000000000000000000
                WidthFactor = 0.004999999888241291
              end
            end
            object gsScaleTicks1: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsLayout.HeightFactor = 0.959999978542327900
              OptionsLayout.WidthFactor = 0.959999978542327900
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -11
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 9
              OptionsView.MaxValue = 8.000000000000000000
              OptionsView.MinorTickCount = 0
              OptionsView.ShowNeedle = False
              OptionsView.MaxValue = 8.000000000000000000
              StyleName = 'SportCar'
              ZOrder = 9
              object dxGaugeControl3CircularScale6Range: TdxGaugeCircularScaleRange
                Color = -11119018
                RadiusFactor = 0.709999978542327900
                ValueEnd = 8.000000000000000000
                WidthFactor = 0.004999999888241291
              end
            end
            object gsMainBackground: TdxGaugeLinearScale
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.ShowLevelBar = False
              OptionsView.RotationAngle = ra0
              StyleName = 'Classic'
              ZOrder = 1
            end
            object gsBackground: TdxGaugeDigitalScale
              AnchorScaleIndex = 9
              OptionsLayout.HeightFactor = 1.200000047683716000
              OptionsLayout.WidthFactor = 1.200000047683716000
              OptionsView.ShowBackground = False
              OptionsView.DigitCount = 4
              OptionsView.SegmentColorOff = 0
              OptionsView.SegmentColorOn = 0
              StyleName = 'White'
            end
            object gsTesterValue1: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsLayout.HeightFactor = 0.959999978542327900
              OptionsLayout.WidthFactor = 0.959999978542327900
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -11
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 9
              OptionsView.MaxValue = 8.000000000000000000
              OptionsView.MinorTickCount = 0
              OptionsView.ShowFirstTick = False
              OptionsView.ShowLabels = False
              OptionsView.ShowLastTick = False
              OptionsView.ShowTicks = False
              OptionsView.MaxValue = 8.000000000000000000
              StyleName = 'CleanWhite'
              Value = 4.599999904632568000
              ZOrder = 12
              object gsTesterValue1Caption1: TdxGaugeQuantitativeScaleCaption
                Text = 'kN'
                OptionsLayout.CenterPositionFactorY = 0.879999995231628400
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gsTesterValue1Caption2: TdxGaugeQuantitativeScaleCaption
                Text = '[%]'
                OptionsLayout.CenterPositionFactorY = 0.819999992847442700
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -9
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gsTesterValue1Caption3: TdxGaugeQuantitativeScaleCaption
                Text = 'VAS 6360'
                OptionsLayout.CenterPositionFactorY = 0.360000014305114800
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGrayText
                OptionsView.Font.Height = -11
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = [fsItalic]
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale7Range: TdxGaugeCircularScaleRange
                Color = -11119018
                RadiusFactor = 0.709999978542327900
                ValueEnd = 8.000000000000000000
                WidthFactor = 0.004999999888241291
              end
            end
            object gsTesterValue2: TdxGaugeCircularScale
              AnchorScaleIndex = 7
              OptionsLayout.HeightFactor = 0.959999978542327900
              OptionsLayout.WidthFactor = 0.959999978542327900
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -11
              OptionsView.Font.Name = 'Arial Narrow'
              OptionsView.Font.Style = []
              OptionsView.MinorTickCount = 0
              OptionsView.ShowFirstTick = False
              OptionsView.ShowLabels = False
              OptionsView.ShowLastTick = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = -20
              OptionsView.AngleStart = 200
              StyleName = 'DarkNight'
              Value = 25.000000000000000000
              ZOrder = 10
            end
          end
        end
        object tsTemperatureGauges: TTabSheet
          Caption = 'Temperature, Volume and Pressure Gauge'
          ImageIndex = 1
          object dxGaugeControl1: TdxGaugeControl
            Left = 0
            Top = 0
            Width = 954
            Height = 446
            Align = alClient
            BorderStyle = cxcbsNone
            Transparent = True
            object gsPressureValue: TdxGaugeCircularScale
              AnchorScaleIndex = 3
              OptionsAnimate.TransitionEffect = ateBounce
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsAnimate.Interval = 800
              OptionsLayout.WidthFactor = 0.423220932483673100
              OptionsView.MajorTickCount = 8
              OptionsView.MaxValue = 7.000000000000000000
              OptionsView.AngleEnd = 90
              OptionsView.AngleStart = 270
              OptionsView.MaxValue = 7.000000000000000000
              StyleName = 'White'
              Value = 3.500000000000000000
              ZOrder = 1
              object gsPressureValueCaption1: TdxGaugeQuantitativeScaleCaption
                Text = 'BAR'
                OptionsLayout.CenterPositionFactorX = 0.300000011920929000
                OptionsLayout.CenterPositionFactorY = 0.449999988079071000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGrayText
                OptionsView.Font.Height = -9
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gsPressureValueCaption2: TdxGaugeQuantitativeScaleCaption
                Text = 'KPAx100'
                OptionsLayout.CenterPositionFactorX = 0.300000011920929000
                OptionsLayout.CenterPositionFactorY = 0.509999990463256800
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGrayText
                OptionsView.Font.Height = -9
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gsPressureValueRange1: TdxGaugeCircularScaleRange
                Color = -65536
                RadiusFactor = 0.600000023841857900
                ValueEnd = 7.000000000000000000
                ValueStart = 5.000000000000000000
                WidthFactor = 1.000000000000000000
              end
              object gsPressureValueRange2: TdxGaugeCircularScaleRange
                Color = -16744448
                RadiusFactor = 0.600000023841857900
                ValueEnd = 2.000000000000000000
                WidthFactor = 1.000000000000000000
              end
            end
            object gsVolumeValue: TdxGaugeLinearScale
              AnchorScaleIndex = 0
              OptionsAnimate.TransitionEffectMode = atmOut
              OptionsAnimate.Enabled = True
              OptionsAnimate.Interval = 900
              OptionsLayout.CenterPositionFactorX = 0.694550216197967500
              OptionsLayout.HeightFactor = 0.690634548664093100
              OptionsLayout.WidthFactor = 0.434138715267181400
              OptionsView.ShowBackground = False
              OptionsView.MajorTickCount = 7
              OptionsView.MaxValue = 1000.000000000000000000
              OptionsView.MinValue = 256.000000000000000000
              OptionsView.LogarithmicBase = 2.000000000000000000
              OptionsView.AlignElements = taRightJustify
              OptionsView.MaxValue = 1000.000000000000000000
              StyleName = 'White'
              Value = 780.000000000000000000
              ZOrder = 2
              object gsVolumeValueCaption1: TdxGaugeQuantitativeScaleCaption
                Text = 'Volume'
                OptionsLayout.CenterPositionFactorX = 1.000000000000000000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGrayText
                OptionsView.Font.Height = -11
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.RotationAngle = 90.000000000000000000
                OptionsView.UseOwnFont = True
              end
            end
            object gsVolumeValueSecondary: TdxGaugeLinearScale
              AnchorScaleIndex = 0
              OptionsAnimate.TransitionEffect = ateBounce
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsAnimate.Interval = 800
              OptionsLayout.CenterPositionFactorX = 0.689950287342071600
              OptionsLayout.HeightFactor = 0.708761453628540000
              OptionsLayout.WidthFactor = 0.367673486471176100
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clBlack
              OptionsView.Font.Height = -13
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 5
              OptionsView.MaxValue = 60.000000000000000000
              OptionsView.MinorTickCount = 0
              OptionsView.MinValue = 20.000000000000000000
              OptionsView.ShowLevelBar = False
              OptionsView.MaxValue = 60.000000000000000000
              StyleName = 'White'
              Value = 45.000000000000000000
              ZOrder = 3
              object gsVolumeValueSecondaryCaption1: TdxGaugeQuantitativeScaleCaption
                Text = 'Temperature C'
                OptionsLayout.CenterPositionFactorX = 0.109999999403953600
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGrayText
                OptionsView.Font.Height = -11
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.RotationAngle = 90.000000000000000000
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl1LinearScale2Range: TdxGaugeLinearScaleRange
                CenterPositionFactor = 0.395000010728836000
                Color = -16744448
                ValueEnd = 30.000000000000000000
                ValueStart = 20.000000000000000000
                WidthFactor = 0.059999998658895490
                CenterPositionFactor = 0.395000010728836000
              end
              object dxGaugeControl1LinearScale2Range1: TdxGaugeLinearScaleRange
                CenterPositionFactor = 0.395000010728836000
                Color = -256
                ValueEnd = 50.000000000000000000
                ValueStart = 30.000000000000000000
                WidthFactor = 0.059999998658895490
                CenterPositionFactor = 0.395000010728836000
              end
              object dxGaugeControl1LinearScale2Range2: TdxGaugeLinearScaleRange
                CenterPositionFactor = 0.395000010728836000
                Color = -65536
                ValueEnd = 60.000000000000000000
                ValueStart = 50.000000000000000000
                WidthFactor = 0.059999998658895490
                CenterPositionFactor = 0.395000010728836000
              end
            end
            object dxGaugeControl1ContainerScale1: TdxGaugeContainerScale
            end
          end
        end
        object TabSheet1: TTabSheet
          Caption = 'Smart Meter'
          ImageIndex = 3
          object dxGaugeControl2: TdxGaugeControl
            Left = 0
            Top = 0
            Width = 954
            Height = 446
            Align = alClient
            BorderStyle = cxcbsNone
            object gcsHazeGas: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.300000011920929000
              OptionsLayout.CenterPositionFactorY = 0.250000000000000000
              OptionsLayout.HeightFactor = 0.479999989271164000
              OptionsLayout.WidthFactor = 0.300000011920929000
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              StyleName = 'CleanWhite'
              Value = 50.000000000000000000
              ZOrder = 1
              object gcsHazeGasCaptionValue: TdxGaugeQuantitativeScaleCaption
                Text = '50'
                OptionsLayout.CenterPositionFactorX = 0.899999976158142100
                OptionsLayout.CenterPositionFactorY = 0.300000011920929000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clGreen
                OptionsView.Font.Height = -40
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gcsHazeCaption1: TdxGaugeQuantitativeScaleCaption
                Text = 'Gas'
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -33
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale1Range1: TdxGaugeCircularScaleRange
                Color = -4144960
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
                WidthFactor = 0.150000005960464500
              end
              object dxGaugeControl2CircularScale1Range2: TdxGaugeCircularScaleRange
                Color = -16744448
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                WidthFactor = 0.150000005960464500
              end
            end
            object dxGaugeControl2ContainerScale1: TdxGaugeContainerScale
            end
            object gcsHazeColdWater: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.699999988079071100
              OptionsLayout.CenterPositionFactorY = 0.250000000000000000
              OptionsLayout.HeightFactor = 0.479999989271164000
              OptionsLayout.WidthFactor = 0.300000011920929000
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              StyleName = 'CleanWhite'
              Value = 50.000000000000000000
              ZOrder = 2
              object gcsHazeColdWaterCaptionValue: TdxGaugeQuantitativeScaleCaption
                Text = '50'
                OptionsLayout.CenterPositionFactorX = 0.899999976158142100
                OptionsLayout.CenterPositionFactorY = 0.300000011920929000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clBlue
                OptionsView.Font.Height = -40
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale1Caption3: TdxGaugeQuantitativeScaleCaption
                Text = 'Cold Water'
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -33
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale1Range3: TdxGaugeCircularScaleRange
                Color = -4144960
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
                WidthFactor = 0.150000005960464500
              end
              object dxGaugeControl2CircularScale1Range4: TdxGaugeCircularScaleRange
                Color = -16776961
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                WidthFactor = 0.150000005960464500
              end
            end
            object gcsHazeHotWater: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.300000011920929000
              OptionsLayout.CenterPositionFactorY = 0.750000000000000000
              OptionsLayout.HeightFactor = 0.479999989271164000
              OptionsLayout.WidthFactor = 0.300000011920929000
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              StyleName = 'CleanWhite'
              Value = 50.000000000000000000
              ZOrder = 3
              object gcsHazeHotWaterCaptionValue: TdxGaugeQuantitativeScaleCaption
                Text = '50'
                OptionsLayout.CenterPositionFactorX = 0.899999976158142100
                OptionsLayout.CenterPositionFactorY = 0.300000011920929000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clRed
                OptionsView.Font.Height = -40
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale2Caption2: TdxGaugeQuantitativeScaleCaption
                Text = 'Hot Water'
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -33
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale2Range1: TdxGaugeCircularScaleRange
                Color = -4144960
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
                WidthFactor = 0.150000005960464500
              end
              object dxGaugeControl2CircularScale2Range2: TdxGaugeCircularScaleRange
                Color = -65536
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                WidthFactor = 0.150000005960464500
              end
            end
            object gcsHazeElectricity: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.699999988079071100
              OptionsLayout.CenterPositionFactorY = 0.750000000000000000
              OptionsLayout.HeightFactor = 0.479999989271164000
              OptionsLayout.WidthFactor = 0.300000011920929000
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              StyleName = 'CleanWhite'
              Value = 50.000000000000000000
              ZOrder = 4
              object gcsHazeElectricityCaptionValue: TdxGaugeQuantitativeScaleCaption
                Text = '50'
                OptionsLayout.CenterPositionFactorX = 0.899999976158142100
                OptionsLayout.CenterPositionFactorY = 0.300000011920929000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = 4227327
                OptionsView.Font.Height = -40
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale3Caption2: TdxGaugeQuantitativeScaleCaption
                Text = 'Electricity'
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -33
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl2CircularScale3Range1: TdxGaugeCircularScaleRange
                Color = -4144960
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
                WidthFactor = 0.150000005960464500
              end
              object dxGaugeControl2CircularScale3Range2: TdxGaugeCircularScaleRange
                Color = -32768
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                WidthFactor = 0.150000005960464500
              end
            end
          end
        end
        object tsWeather: TTabSheet
          Caption = 'Weather Forecast'
          ImageIndex = 4
          object gcWeather: TdxGaugeControl
            Left = 0
            Top = 0
            Width = 954
            Height = 446
            Align = alClient
            BorderStyle = cxcbsNone
            object dxGaugeControl3ContainerScale1: TdxGaugeContainerScale
              OptionsLayout.CenterPositionFactorY = 0.501972377300262500
            end
            object gcsWeatherLosAnglesHumidity: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.159248247742652900
              OptionsLayout.HeightFactor = 0.649999976158142100
              OptionsLayout.WidthFactor = 0.349999994039535600
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = -90
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              StyleName = 'White'
              Value = 50.000000000000000000
              OnAnimate = gcsWeatherLosAnglesHumidityAnimate
              ZOrder = 1
              object gcsWeatherLosAnglesHumidityCaption: TdxGaugeQuantitativeScaleCaption
                Text = 'h: 85%'
                OptionsLayout.CenterPositionFactorY = 0.569999992847442600
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clSilver
                OptionsView.Font.Height = -16
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale1Range1: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
              end
              object dxGaugeControl3CircularScale1Range2: TdxGaugeCircularScaleRange
                Color = -6776680
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 50.000000000000000000
              end
            end
            object gcsWeatherLosAnglesTemperature: TdxGaugeCircularScale
              AnchorScaleIndex = 1
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.HeightFactor = 1.123580336570740000
              OptionsLayout.WidthFactor = 1.111517071723938000
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clSilver
              OptionsView.Font.Height = -12
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 3
              OptionsView.MaxValue = 30.000000000000000000
              OptionsView.MinValue = -30.000000000000000000
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              OptionsView.MaxValue = 30.000000000000000000
              StyleName = 'Classic'
              Value = 21.000000000000000000
              OnAnimate = gcsWeatherLosAnglesTemperatureAnimate
              ZOrder = 2
              object gcsWeatherLosAnglesTemperatureCaption: TdxGaugeQuantitativeScaleCaption
                Text = 't: +21 C'
                OptionsLayout.CenterPositionFactorY = 0.449999988079071000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clRed
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale3Caption3: TdxGaugeQuantitativeScaleCaption
                Text = 'Los Angeles'
                OptionsLayout.CenterPositionFactorY = -0.050000000745058060
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale3Range3: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 0.750000000000000000
                ValueEnd = 30.000000000000000000
                ValueStart = -30.000000000000000000
              end
              object gcsWeatherLosAnglesTemperatureRange: TdxGaugeCircularScaleRange
                Color = -65536
                RadiusFactor = 0.750000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 12.000000000000000000
              end
            end
            object gcsWeatherMoscowHumidity: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.500941276550293000
              OptionsLayout.HeightFactor = 0.649999976158142100
              OptionsLayout.WidthFactor = 0.349999994039535600
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = -90
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              StyleName = 'White'
              Value = 60.000000000000000000
              OnAnimate = gcsWeatherMoscowHumidityAnimate
              ZOrder = 3
              object gcsWeatherMoscowHumidityCaption: TdxGaugeQuantitativeScaleCaption
                Text = 'h: 60%'
                OptionsLayout.CenterPositionFactorY = 0.569999992847442600
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clSilver
                OptionsView.Font.Height = -16
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale4Range1: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
              end
              object gcsWeatherMowcowHumidityRange: TdxGaugeCircularScaleRange
                Color = -6776680
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 50.000000000000000000
              end
            end
            object gcsWeatherMoscowTemperature: TdxGaugeCircularScale
              AnchorScaleIndex = 3
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.HeightFactor = 1.123580336570740000
              OptionsLayout.WidthFactor = 1.111517071723938000
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clSilver
              OptionsView.Font.Height = -12
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 3
              OptionsView.MaxValue = 30.000000000000000000
              OptionsView.MinValue = -30.000000000000000000
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              OptionsView.MaxValue = 30.000000000000000000
              StyleName = 'Classic'
              Value = -3.000000000000000000
              OnAnimate = gcsWeatherMoscowTemperatureAnimate
              ZOrder = 4
              object gcsWeatherMoscowTemperatureCaption: TdxGaugeQuantitativeScaleCaption
                Text = 't: -3 C'
                OptionsLayout.CenterPositionFactorY = 0.449999988079071000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clBlue
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale2Caption3: TdxGaugeQuantitativeScaleCaption
                Text = 'Moscow'
                OptionsLayout.CenterPositionFactorY = -0.050000000745058060
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object gcsWeatherDate: TdxGaugeQuantitativeScaleCaption
                Text = 'Date'
                OptionsLayout.CenterPositionFactorY = 1.100000023841858000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale2Range1: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 0.750000000000000000
                ValueEnd = 30.000000000000000000
                ValueStart = -30.000000000000000000
              end
              object gcsWeatherMoscowTemperatureRange: TdxGaugeCircularScaleRange
                Color = -16776961
                RadiusFactor = 0.750000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 12.000000000000000000
              end
            end
            object gcsWeatherLondonHumidity: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.CenterPositionFactorX = 0.828865289688110300
              OptionsLayout.HeightFactor = 0.649999976158142100
              OptionsLayout.WidthFactor = 0.349999994039535600
              OptionsView.ShowBackground = False
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = -90
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              StyleName = 'White'
              Value = 75.000000000000000000
              OnAnimate = gcsWeatherLondonHumidityAnimate
              ZOrder = 5
              object gcsWeatherLondonHumidityCaption: TdxGaugeQuantitativeScaleCaption
                Text = 'h: 75%'
                OptionsLayout.CenterPositionFactorY = 0.569999992847442600
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clSilver
                OptionsView.Font.Height = -16
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale5Range1: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 1.000000000000000000
                ValueEnd = 100.000000000000000000
              end
              object gcsWeatherLondonHumidityRange: TdxGaugeCircularScaleRange
                Color = -6776680
                RadiusFactor = 1.000000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 50.000000000000000000
              end
            end
            object gcsWeatherLondonTemperature: TdxGaugeCircularScale
              AnchorScaleIndex = 5
              OptionsAnimate.TransitionEffect = ateCubic
              OptionsAnimate.TransitionEffectMode = atmInOut
              OptionsAnimate.Enabled = True
              OptionsLayout.HeightFactor = 1.123580336570740000
              OptionsLayout.WidthFactor = 1.111517071723938000
              OptionsView.ShowBackground = False
              OptionsView.Font.Charset = DEFAULT_CHARSET
              OptionsView.Font.Color = clSilver
              OptionsView.Font.Height = -12
              OptionsView.Font.Name = 'Tahoma'
              OptionsView.Font.Style = []
              OptionsView.MajorTickCount = 3
              OptionsView.MaxValue = 30.000000000000000000
              OptionsView.MinValue = -30.000000000000000000
              OptionsView.ShowTicks = False
              OptionsView.AngleEnd = 45
              OptionsView.AngleStart = 270
              OptionsView.ShowNeedle = False
              OptionsView.ShowSpindleCap = False
              OptionsView.MaxValue = 30.000000000000000000
              StyleName = 'Classic'
              Value = 15.000000000000000000
              OnAnimate = gcsWeatherLondonTemperatureAnimate
              ZOrder = 6
              object gcsWeatherLondonTemperatureCaption: TdxGaugeQuantitativeScaleCaption
                Text = 't: +15 C'#39
                OptionsLayout.CenterPositionFactorY = 0.449999988079071000
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clRed
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale6Caption3: TdxGaugeQuantitativeScaleCaption
                Text = 'London'
                OptionsLayout.CenterPositionFactorY = -0.050000000745058060
                OptionsView.Font.Charset = DEFAULT_CHARSET
                OptionsView.Font.Color = clWindowText
                OptionsView.Font.Height = -19
                OptionsView.Font.Name = 'Tahoma'
                OptionsView.Font.Style = []
                OptionsView.UseOwnFont = True
              end
              object dxGaugeControl3CircularScale6Range1: TdxGaugeCircularScaleRange
                Color = -1907998
                RadiusFactor = 0.750000000000000000
                ValueEnd = 30.000000000000000000
                ValueStart = -30.000000000000000000
              end
              object gcsWeatherLondonTemperatureRange: TdxGaugeCircularScaleRange
                Color = -65536
                RadiusFactor = 0.750000000000000000
                LinkedWithScaleValue = rlsvValueEnd
                ValueEnd = 12.000000000000000000
              end
            end
          end
        end
        object tsSimpleHybrid: TTabSheet
          Caption = 'Simple Hybrid Gauges'
          ImageIndex = 2
          object gcHybridDarkNight: TdxGaugeControl
            Left = 0
            Top = 0
            Width = 260
            Height = 260
            Align = alCustom
            BorderStyle = cxcbsNone
            LookAndFeel.NativeStyle = True
            Transparent = True
            object dxGaugeCircularScale2: TdxGaugeCircularScale
              StyleName = 'DarkNight'
              Value = 72.000000000000000000
            end
            object dxGaugeDigitalScale4: TdxGaugeDigitalScale
              AnchorScaleIndex = 0
              OptionsLayout.CenterPositionFactorX = 0.579999983310699500
              OptionsLayout.CenterPositionFactorY = 0.750000000000000000
              OptionsLayout.HeightFactor = 0.159999996423721300
              OptionsLayout.WidthFactor = 0.319999992847442600
              OptionsView.DigitCount = 3
              OptionsView.DisplayMode = sdmSevenSegment
              StyleName = 'DarkNight'
              Value = '72'
              ZOrder = 1
            end
          end
          object gcHybrid: TdxGaugeControl
            Left = 532
            Top = 0
            Width = 260
            Height = 260
            Align = alCustom
            BorderStyle = cxcbsNone
            LookAndFeel.NativeStyle = True
            Transparent = True
            object dxGaugeCircularScale5: TdxGaugeCircularScale
              AnchorScaleIndex = 2
              OptionsLayout.CenterPositionFactorX = 0.299563497304916400
              OptionsLayout.CenterPositionFactorY = 0.726132452487945600
              OptionsLayout.HeightFactor = 0.465173423290252700
              OptionsLayout.WidthFactor = 0.519227564334869400
              OptionsView.MinorTickCount = 5
              StyleName = 'DeepFire'
              Value = 45.000000000000000000
              ZOrder = 2
            end
            object dxGaugeControl11dxGaugeDigitalScale1: TdxGaugeDigitalScale
              AnchorScaleIndex = 2
              OptionsLayout.CenterPositionFactorX = 0.283615887165069600
              OptionsLayout.CenterPositionFactorY = 0.406243115663528400
              OptionsLayout.HeightFactor = 0.139715045690536500
              OptionsLayout.WidthFactor = 0.491119205951690700
              OptionsView.DigitCount = 6
              OptionsView.DisplayMode = sdmSevenSegment
              StyleName = 'IceColdZone'
              Value = '13:01:37'
              ZOrder = 1
            end
            object gcHybridContainerScale1: TdxGaugeContainerScale
              OptionsLayout.HeightFactor = 0.907691955566406300
              OptionsLayout.WidthFactor = 0.884614944458007800
            end
            object gcHybridLinearScale1: TdxGaugeLinearScale
              AnchorScaleIndex = 2
              OptionsLayout.CenterPositionFactorX = 0.805523037910461400
              OptionsLayout.CenterPositionFactorY = 0.627609491348266600
              OptionsLayout.HeightFactor = 0.668214857578277600
              OptionsLayout.WidthFactor = 0.380225300788879400
              Value = 45.000000000000000000
              ZOrder = 3
              object gcHybridLinearScale1Caption1: TdxGaugeQuantitativeScaleCaption
                Text = 'ExpressGauge Control'
                OptionsLayout.CenterPositionFactorX = 0.800000011920928900
                OptionsLayout.CenterPositionFactorY = 0.649999976158142100
                OptionsView.RotationAngle = 90.000000000000000000
              end
            end
            object gcHybridCircularWideScale1: TdxGaugeCircularWideScale
              AnchorScaleIndex = 2
              OptionsLayout.CenterPositionFactorX = 0.526187717914581300
              OptionsLayout.CenterPositionFactorY = 0.146947056055069000
              OptionsLayout.HeightFactor = 0.276879131793975800
              OptionsLayout.WidthFactor = 0.903978347778320300
              StyleName = 'Disco'
              Value = 68.000000000000000000
              ZOrder = 4
            end
          end
          object gcHybridIceColdZone: TdxGaugeControl
            Left = 266
            Top = 0
            Width = 260
            Height = 260
            Align = alCustom
            BorderStyle = cxcbsNone
            LookAndFeel.NativeStyle = True
            Transparent = True
            object dxGaugeControl3dxGaugeCircularScale1: TdxGaugeCircularScale
              OptionsView.MinorTickCount = 5
              StyleName = 'IceColdZone'
              Value = 45.000000000000000000
            end
            object dxGaugeDigitalScale2: TdxGaugeDigitalScale
              AnchorScaleIndex = 0
              OptionsLayout.CenterPositionFactorY = 0.300000011920929000
              OptionsLayout.HeightFactor = 0.159999996423721300
              OptionsLayout.WidthFactor = 0.319999992847442600
              OptionsView.DigitCount = 3
              OptionsView.DisplayMode = sdmSevenSegment
              StyleName = 'IceColdZone'
              Value = '45'
              ZOrder = 1
            end
            object gcHybridIceColdZoneCircularScale1: TdxGaugeCircularScale
              AnchorScaleIndex = 0
              OptionsView.ShowBackground = False
              OptionsView.MinorTickCount = 5
              OptionsView.ShowLabels = False
              OptionsView.ShowTicks = False
              StyleName = 'IceColdZone'
              Value = 45.000000000000000000
              ZOrder = 2
              object gcHybridIceColdZoneCircularScale1Caption1: TdxGaugeQuantitativeScaleCaption
                Text = 'HPM km/h'
                OptionsLayout.CenterPositionFactorY = 0.699999988079071100
              end
            end
          end
        end
      end
    end
  end
  object lbDescription: TcxLabel
    Left = 0
    Top = 0
    Align = alTop
    Caption = 
      'This demo illustrates sample gauges created with circular, linea' +
      'r and digital scales. Select a tab to browse through the gauges ' +
      'and resize the form'#39's window to scale them.'
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
    Width = 970
  end
  object tCarTesterTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tCarTesterTimerTimer
    Left = 48
    Top = 120
  end
  object tTemperatureTimer: TTimer
    Enabled = False
    OnTimer = tTemperatureTimerTimer
    Left = 48
    Top = 232
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 776
    Top = 152
  end
  object tHazesTimer: TTimer
    Enabled = False
    Interval = 1500
    OnTimer = tHazesTimerTimer
    Left = 48
    Top = 176
  end
  object tWeatherTimer: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tWeatherTimerTimer
    Left = 48
    Top = 288
  end
end
