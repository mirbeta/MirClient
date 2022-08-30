object StylesSimpleDemoMainDM: TStylesSimpleDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 289
  Top = 148
  Height = 222
  Width = 379
  object dsPersons: TDataSource
    DataSet = cdsPersons
    Left = 40
    Top = 112
  end
  object dsCountries: TDataSource
    DataSet = cdsCountries
    Left = 104
    Top = 112
  end
  object StyleRepository: TcxStyleRepository
    Left = 192
    Top = 16
    object Sunny: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clNavy
    end
    object Dark: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 15451300
      TextColor = clWhite
    end
    object Golden: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object Summer: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object Autumn: TcxStyle
      AssignedValues = [svColor]
      Color = clBtnFace
    end
    object Bright: TcxStyle
      AssignedValues = [svColor]
      Color = 16749885
    end
    object Cold: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object Spring: TcxStyle
      AssignedValues = [svColor]
      Color = 16247513
    end
    object Light: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object Winter: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
    end
    object Depth: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object UserStyleSheet: TcxGridTableViewStyleSheet
      Caption = 'User Defined Style Sheet'
      Styles.Background = Dark
      Styles.Content = Spring
      Styles.ContentEven = Autumn
      Styles.ContentOdd = Spring
      Styles.Inactive = Summer
      Styles.IncSearch = Golden
      Styles.Selection = Bright
      Styles.FilterBox = Sunny
      Styles.Footer = Light
      Styles.Group = Cold
      Styles.GroupByBox = Golden
      Styles.Header = Depth
      Styles.Indicator = Autumn
      Styles.Preview = Winter
      BuiltIn = True
    end
  end
  object cdsPersons: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 40
    Top = 72
    object cdsPersonsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsPersonsFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object cdsPersonsSECONDNAME: TStringField
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object cdsPersonsGENDER: TBooleanField
      FieldName = 'GENDER'
    end
    object cdsPersonsBIRTHNAME: TStringField
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object cdsPersonsDATEOFBIRTH: TDateTimeField
      FieldName = 'DATEOFBIRTH'
    end
    object cdsPersonsBIRTHCOUNTRY: TIntegerField
      FieldName = 'BIRTHCOUNTRY'
    end
    object cdsPersonsLOCATIONOFBIRTH: TStringField
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object cdsPersonsBIOGRAPHY: TMemoField
      FieldName = 'BIOGRAPHY'
      BlobType = ftMemo
      Size = 10
    end
    object cdsPersonsNICKNAME: TStringField
      FieldName = 'NICKNAME'
      Size = 50
    end
    object cdsPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
  end
  object cdsCountries: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 104
    Top = 72
    object cdsCountriesID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsCountriesNAME: TStringField
      FieldName = 'NAME'
      Size = 60
    end
    object cdsCountriesACRONYM: TStringField
      FieldName = 'ACRONYM'
      Size = 50
    end
    object cdsCountriesNATIONALFLAG: TBlobField
      FieldName = 'NATIONALFLAG'
      BlobType = ftParadoxOle
      Size = 10
    end
  end
end
