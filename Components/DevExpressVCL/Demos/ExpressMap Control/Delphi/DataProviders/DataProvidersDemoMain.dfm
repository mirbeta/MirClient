inherited DataProvidersDemoMainForm: TDataProvidersDemoMainForm
  Caption = 'DataProviders Demo'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited cxGroupBox2: TcxGroupBox
    inherited dxMapControl1: TdxMapControl
      object dxMapControl1ImageTileLayer1: TdxMapImageTileLayer
      end
      object dxMapControl1ItemLayer1: TdxMapItemLayer
        ProjectionClassName = 'TdxMapControlSphericalMercatorProjection'
        object dxMapControl1ItemLayer1CustomItem1: TdxMapCustomElement
          Style.AssignedValues = [mcsvFont]
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsBold]
          Location.Longitude = -118.255867000000000000
          Location.Latitude = 34.153454000000000000
          Text = 'DevExpress Offices'
        end
      end
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 640
    Top = 216
  end
  inherited mmMain: TMainMenu
    Left = 512
    Top = 224
    inherited miOptions: TMenuItem
      object OpenStreetMapProvider1: TMenuItem
        AutoCheck = True
        Caption = 'OpenStreetMap'
        GroupIndex = 1
        RadioItem = True
        OnClick = MapKindClick
      end
      object miRoad: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Bing Road'
        GroupIndex = 1
        RadioItem = True
        OnClick = MapKindClick
      end
      object miArea: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Bing Area'
        GroupIndex = 1
        RadioItem = True
        OnClick = MapKindClick
      end
      object miHybrid: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'Bing Hybrid'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = MapKindClick
      end
    end
  end
end
