inherited frmOlapBrowser: TfrmOlapBrowser
  Left = 228
  Top = 187
  Caption = 'PivotGrid - OLAP Browser Demo'
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo shows you how to mine data from an OLAP cube. To run t' +
      'his demo you must have the MSOLAP 10.0 OLE DB provider properly ' +
      'installed on your system.'
  end
  object UnboundPivotGrid: TcxPivotGrid [1]
    Left = 244
    Top = 32
    Width = 563
    Height = 512
    Customization.Site = Panel1
    OLAPDataSource = OLAPDataSource
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Groups = <>
    OptionsLockedStateImage.Effect = lsieLight
    OptionsLockedStateImage.ShowText = True
    OptionsLockedStateImage.Mode = lsimImmediate
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 32
    Width = 236
    Height = 512
    Align = alLeft
    TabOrder = 1
  end
  object cxSplitter1: TcxSplitter [3]
    Left = 236
    Top = 32
    Width = 8
    Height = 512
    HotZoneClassName = 'TcxSimpleStyle'
    HotZone.SizePercent = 75
  end
  inherited mmMain: TMainMenu
    inherited miFile: TMenuItem
      object NewConnection1: TMenuItem [0]
        Caption = 'New Connection'
        OnClick = NewConnection1Click
      end
      object miProvider: TMenuItem [1]
        Caption = 'Provider'
        object miADOMDProvider: TMenuItem
          Caption = 'ADO MD'
          Checked = True
          GroupIndex = 16
          RadioItem = True
          OnClick = ChangeProviderClick
        end
        object miOLEDBProvider: TMenuItem
          Caption = 'OLE DB'
          GroupIndex = 16
          RadioItem = True
          OnClick = ChangeProviderClick
        end
      end
      object N3: TMenuItem [2]
        Caption = '-'
      end
    end
    inherited miOptions: TMenuItem
      object miLockedStateImage: TMenuItem [5]
        Caption = 'Locked View Image'
        object miLockedStateImageMode: TMenuItem
          Caption = 'Mode'
          object miLockedStateImageModeNever: TMenuItem
            AutoCheck = True
            Caption = 'Never'
            GroupIndex = 13
            RadioItem = True
            OnClick = LockedViewImageClick
          end
          object miLockedStateImageModeImmediate: TMenuItem
            Tag = 2
            AutoCheck = True
            Caption = 'Immediate'
            Checked = True
            GroupIndex = 13
            RadioItem = True
            OnClick = LockedViewImageClick
          end
          object miLockedStateImageModePending: TMenuItem
            Tag = 1
            AutoCheck = True
            Caption = 'Pending'
            GroupIndex = 13
            RadioItem = True
            OnClick = LockedViewImageClick
          end
        end
        object miLockedStateImageEffect: TMenuItem
          Caption = 'Effect'
          object miLockedStateImageEffectLight: TMenuItem
            Tag = 3
            AutoCheck = True
            Caption = 'Light'
            Checked = True
            GroupIndex = 14
            RadioItem = True
            OnClick = LockedViewImageClick
          end
          object miLockedStateImageEffectDark: TMenuItem
            Tag = 4
            AutoCheck = True
            Caption = 'Dark'
            GroupIndex = 14
            RadioItem = True
            OnClick = LockedViewImageClick
          end
        end
      end
    end
  end
  object OLAPDataSource: TcxPivotGridOLAPDataSource
    ProviderClassName = 'TcxPivotGridOLAPADOMDProvider'
    Left = 712
  end
end
