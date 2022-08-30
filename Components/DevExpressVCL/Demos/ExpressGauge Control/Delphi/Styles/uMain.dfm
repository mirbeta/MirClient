object frmGaugeStyles: TfrmGaugeStyles
  Left = 608
  Top = 201
  Caption = 'Styles Demo'
  ClientHeight = 567
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cxLabel2: TcxLabel
    Left = 0
    Top = 0
    Align = alTop
    Caption = 
      'This demo illustrates the built-in styles available for gauges. ' +
      'Select an option on the right to apply the corresponding style t' +
      'o the gauges. Move the slider'#39's thumb to adjust the gauge'#39's valu' +
      'e.'
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
    Width = 900
  end
  object cxTrackBar1: TcxTrackBar
    Left = 0
    Top = 513
    Align = alBottom
    Properties.Frequency = 10
    Properties.Max = 1000
    Properties.OnChange = cxTrackBar1PropertiesChange
    TabOrder = 1
    Height = 54
    Width = 900
  end
  object tcStyles: TcxTabControl
    Left = 0
    Top = 36
    Width = 900
    Height = 477
    Align = alClient
    TabOrder = 2
    Properties.CustomButtons.Buttons = <>
    Properties.MultiLine = True
    OnChange = tcStylesChange
    ClientRectBottom = 477
    ClientRectRight = 900
    ClientRectTop = 0
    object dxGaugeControl1: TdxGaugeControl
      Left = 0
      Top = 0
      Width = 900
      Height = 477
      Align = alClient
      BorderStyle = cxcbsNone
      Constraints.MinHeight = 60
      Constraints.MinWidth = 60
      LookAndFeel.NativeStyle = True
      Transparent = True
      object dxGaugeControl1CircularScale1: TdxGaugeCircularScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.517177760601043700
        OptionsLayout.CenterPositionFactorY = 0.593439519405365000
        OptionsLayout.HeightFactor = 0.423865467309951800
        OptionsLayout.WidthFactor = 0.220325142145156900
        OptionsView.Font.Charset = DEFAULT_CHARSET
        OptionsView.Font.Color = clWhite
        OptionsView.Font.Height = -9
        OptionsView.Font.Name = 'Tahoma'
        OptionsView.Font.Style = []
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 7
      end
      object dxGaugeControl1DigitalScale1: TdxGaugeDigitalScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.519999980926513700
        OptionsLayout.CenterPositionFactorY = 0.901263952255249000
        OptionsLayout.HeightFactor = 0.172268748283386200
        OptionsLayout.WidthFactor = 0.449999988079071000
        OptionsView.DigitCount = 0
        OptionsView.DisplayMode = sdmMatrix8x14Dots
        ZOrder = 1
      end
      object dxGaugeControl1LinearScale1: TdxGaugeLinearScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.886385500431060800
        OptionsLayout.CenterPositionFactorY = 0.260797262191772500
        OptionsLayout.HeightFactor = 0.500000000000000000
        OptionsLayout.WidthFactor = 0.176537871360778800
        OptionsView.Font.Charset = DEFAULT_CHARSET
        OptionsView.Font.Color = clWhite
        OptionsView.Font.Height = -11
        OptionsView.Font.Name = 'Tahoma'
        OptionsView.Font.Style = []
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 4
      end
      object dxGaugeControl1CircularHalfScale1: TdxGaugeCircularHalfScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.139029338955879200
        OptionsLayout.CenterPositionFactorY = 0.824705898761749300
        OptionsLayout.HeightFactor = 0.300000011920929000
        OptionsLayout.WidthFactor = 0.266139864921569800
        OptionsView.Font.Charset = DEFAULT_CHARSET
        OptionsView.Font.Color = clWhite
        OptionsView.Font.Height = -9
        OptionsView.Font.Name = 'Tahoma'
        OptionsView.Font.Style = []
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 2
      end
      object dxGaugeControl1CircularQuarterLeftScale1: TdxGaugeCircularQuarterLeftScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.129999995231628400
        OptionsLayout.CenterPositionFactorY = 0.231764703989028900
        OptionsLayout.HeightFactor = 0.400000005960464500
        OptionsLayout.WidthFactor = 0.235307604074478100
        OptionsView.Font.Charset = DEFAULT_CHARSET
        OptionsView.Font.Color = clWhite
        OptionsView.Font.Height = -9
        OptionsView.Font.Name = 'Tahoma'
        OptionsView.Font.Style = []
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 5
      end
      object dxGaugeControl1CircularQuarterRightScale1: TdxGaugeCircularQuarterRightScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.876021802425384500
        OptionsLayout.CenterPositionFactorY = 0.789411783218383800
        OptionsLayout.HeightFactor = 0.400000005960464500
        OptionsLayout.WidthFactor = 0.239882439374923700
        OptionsView.Font.Charset = DEFAULT_CHARSET
        OptionsView.Font.Color = clWhite
        OptionsView.Font.Height = -9
        OptionsView.Font.Name = 'Tahoma'
        OptionsView.Font.Style = []
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 3
      end
      object dxGaugeControl1ContainerScale1: TdxGaugeContainerScale
        OptionsLayout.HeightFactor = 0.988029718399047900
      end
      object dxGaugeControl1CircularThreeFourthScale1: TdxGaugeCircularThreeFourthScale
        AnchorScaleIndex = 6
        OptionsLayout.CenterPositionFactorX = 0.515989303588867200
        OptionsLayout.CenterPositionFactorY = 0.189934581518173200
        OptionsLayout.HeightFactor = 0.348739355802536000
        OptionsLayout.WidthFactor = 0.293453663587570200
        OptionsView.MaxValue = 1000.000000000000000000
        OptionsView.MaxValue = 1000.000000000000000000
        ZOrder = 6
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 24
    Top = 40
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 112
    Top = 40
  end
end
