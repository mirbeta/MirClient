object EditorsMaskDemoMainDM: TEditorsMaskDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 223
  Top = 182
  Height = 655
  Width = 733
  object StyleRepository: TcxStyleRepository
    Left = 184
    Top = 16
    PixelsPerInch = 96
    object stBlueDark: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object stGold: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object stBlueLight: TcxStyle
      AssignedValues = [svColor]
      Color = 16247513
    end
    object stBlueBright: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16749885
      TextColor = clWhite
    end
    object stYellowLight: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object stGreyLight: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object stBlueSky: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
  end
  object DataSourceUSERS: TDataSource
    DataSet = cdsUsers
    Left = 84
    Top = 116
  end
  object DataSourceDEPARTMENTS: TDataSource
    DataSet = cdsDepartments
    Left = 88
    Top = 68
  end
  object cdsUsers: TClientDataSet
    Aggregates = <>
    MasterFields = 'ID'
    MasterSource = DataSourceDEPARTMENTS
    PacketRecords = 0
    Params = <>
    Left = 16
    Top = 16
  end
  object cdsDepartments: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 88
    Top = 16
  end
end
