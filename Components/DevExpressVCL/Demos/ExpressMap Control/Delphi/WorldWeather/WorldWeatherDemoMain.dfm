inherited WorldWeatherDemoMainForm: TWorldWeatherDemoMainForm
  Caption = 'WorldWeather Demo'
  ClientHeight = 557
  ClientWidth = 995
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxMapControl1: TdxMapControl
    Width = 995
    Height = 557
    CenterPoint.Longitude = -24.000000000000000000
    CenterPoint.Latitude = 34.000000000000000000
    OptionsBehavior.MapItemSelectMode = mismSingle
    PopupMenu = PopupMenu1
    ZoomLevel = 3.000000000000000000
    object dxMapControl1ImageTileLayer1: TdxMapImageTileLayer
      ProviderClassName = 'TdxMapControlOpenStreetMapImageryDataProvider'
      Provider.MaxParallelConnectionCount = 5
      Provider.Subdomains.Strings = (
        'a'
        'b'
        'c')
      Provider.UrlTemplate = 'http://[subdomain].tile.openstreetmap.org/[z]/[x]/[y].png'
    end
    object dxMapControl1ImageTileLayer2: TdxMapImageTileLayer
      Visible = False
      ProviderClassName = 'TdxMapControlOpenStreetMapImageryDataProvider'
      Provider.MaxParallelConnectionCount = 5
      Provider.Subdomains.Strings = (
        '1'
        '2'
        '3')
      Provider.UrlTemplate = 
        'http://a[subdomain].tile.openweathermap.org/map/clouds/[z]/[x]/[' +
        'y].png'
    end
    object dxMapControl1ItemLayer1: TdxMapItemLayer
      ProjectionClassName = 'TdxMapControlSphericalMercatorProjection'
    end
  end
  object ListBox1: TListBox [1]
    Left = 848
    Top = 8
    Width = 139
    Height = 81
    ItemHeight = 13
    Items.Strings = (
      'New York'
      'Los Angeles, US'
      'Tokyo'
      'Moscow, RU'
      'Seoul'
      'Jakarta'
      'Tehran'
      'Mexico City'
      'Bangkok'
      'London, UK'
      'Beijing'
      'Bogota'
      'Lima'
      'Cairo'
      'Baghdad'
      'Hong Kong'
      'Dhaka'
      'Singapore'
      'Santiago'
      'Ankara'
      'Riyadh'
      'Kinshasa'
      'Berlin'
      'Damascus, SY'
      'Hanoi'
      'Madrid'
      'Pyongyang'
      'Kabul'
      'Addis Ababa'
      'Santo Domingo'
      'Buenos Aires'
      'Kiev'
      'Roma'
      'Fairbanks'
      'Vancouver'
      'Toronto'
      'Houston'
      'Panama'
      'Rio de Janeiro'
      'Sydney'
      'Darwin'
      'Dunedin'
      'Auckland'
      'Noumea'
      'Jayapura'
      'Perth'
      'Casablanca'
      'Sabha'
      'Yellowknife'
      'Sept-Iles'
      'Ushuaia'
      'Cape Town'
      'Dakar'
      'Antananarivo'
      'Rundu'
      'NDjamena'
      'Coroata')
    TabOrder = 1
    Visible = False
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object Additionallayer1: TMenuItem
        Caption = 'Additional Layer'
        object None1: TMenuItem
          AutoCheck = True
          Caption = 'None'
          Checked = True
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object Clouds1: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Clouds Cover'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object Precipitation1: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'Quantity Of Precipitation'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object Pressure1: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'Sea Level Pressure'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object Wind1: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'Wind Speed'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object emperature1: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = 'Temperature'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
        object Snow1: TMenuItem
          Tag = 6
          AutoCheck = True
          Caption = 'Snow Precipitation'
          GroupIndex = 2
          RadioItem = True
          OnClick = None1Click
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object emperaturemeasure1: TMenuItem
        Caption = 'Temperature Units'
        object Celsius1: TMenuItem
          AutoCheck = True
          Caption = 'Celsius'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = cxRadioButton1Click
        end
        object Fahrenheit1: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Fahrenheit'
          GroupIndex = 1
          RadioItem = True
          OnClick = cxRadioButton1Click
        end
      end
      object Addcity1: TMenuItem
        Caption = 'Add City...'
        OnClick = cxButton1Click
      end
      object Settings1: TMenuItem
        Caption = 'Show/Hide Cities...'
        OnClick = Settings1Click
      end
    end
  end
  object cxHintStyleController1: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    HintHidePause = 5000
    Left = 488
    Top = 288
  end
  object dxScreenTipRepository1: TdxScreenTipRepository
    AssignedFonts = [stbHeader]
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = 5000268
    HeaderFont.Height = -16
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = [fsBold]
    Left = 664
    Top = 288
    object dxScreenTipRepository1ScreenTip1: TdxScreenTip
      Header.Text = 'dxScreenTipRepository1ScreenTip1'
      Description.PlainText = False
      Description.Text = 
        '{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fnil\fcharset0 Segoe ' +
        'UI;}}'#13#10'{\colortbl ;\red76\green76\blue76;}'#13#10'\viewkind4\uc1\pard\' +
        'cf1\lang1033\b\f0\fs18 Pressure:\b0  %Pressure%\par'#13#10'\b Wind:\b0' +
        '  %Wind%\par'#13#10'\b Humidity:\b0  %Humidity%\par'#13#10'\par'#13#10'}'#13#10
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 304
    Top = 136
    object ShowWeatherforGeoPoint: TMenuItem
      Caption = 'Add geopoint'
      OnClick = ShowWeatherforGeoPointClick
    end
    object Hidecity1: TMenuItem
      Caption = 'Hide city'
      OnClick = Hidecity1Click
    end
  end
end
