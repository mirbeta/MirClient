object StylesSimpleDemoDataDM: TStylesSimpleDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 615
  Top = 322
  Height = 222
  Width = 379
  object dsDEPARTMENTS: TDataSource
    DataSet = mdDEPARTMENTS
    Left = 40
    Top = 112
  end
  object dsPERSONS: TDataSource
    DataSet = mdPERSONS
    Left = 136
    Top = 112
  end
  object StyleRepository: TcxStyleRepository
    Left = 308
    Top = 28
    PixelsPerInch = 96
    object Sunny: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clNavy
    end
    object Dark: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15451300
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object Golden: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object Summer: TcxStyle
      AssignedValues = [svColor]
      Color = 15519398
    end
    object Autumn: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15252642
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = 11032875
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
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object UserStyleSheet: TcxTreeListStyleSheet
      Caption = 'User Defined Style Sheet'
      Styles.Background = Dark
      Styles.Content = Autumn
      Styles.Inactive = Dark
      Styles.Selection = Bright
      Styles.BandBackground = Dark
      Styles.BandContent = Dark
      Styles.BandHeader = Depth
      Styles.ColumnHeader = Autumn
      Styles.ContentEven = Spring
      Styles.ContentOdd = Summer
      Styles.Footer = Light
      Styles.IncSearch = Golden
      Styles.Indicator = Dark
      Styles.Preview = Light
      BuiltIn = True
    end
  end
  object mdDEPARTMENTS: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0A000000040000000C000300494400040000000300
      0900504152454E544944000400000003000A004D414E41474552494400320000
      00010005004E414D450008000000060007004255444745540032000000010009
      004C4F434154494F4E00320000000100060050484F4E45003200000001000400
      46415800FF00000001000600454D41494C000200000005000800564143414E43
      5900}
    SortOptions = []
    Left = 40
    Top = 64
    object mdDEPARTMENTSID: TAutoIncField
      FieldName = 'ID'
    end
    object mdDEPARTMENTSPARENTID: TIntegerField
      FieldName = 'PARENTID'
    end
    object mdDEPARTMENTSMANAGERID: TIntegerField
      FieldName = 'MANAGERID'
    end
    object mdDEPARTMENTSNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object mdDEPARTMENTSBUDGET: TFloatField
      FieldName = 'BUDGET'
    end
    object mdDEPARTMENTSLOCATION: TStringField
      FieldName = 'LOCATION'
      Size = 50
    end
    object mdDEPARTMENTSPHONE: TStringField
      FieldName = 'PHONE'
      Size = 50
    end
    object mdDEPARTMENTSFAX: TStringField
      FieldName = 'FAX'
      Size = 50
    end
    object mdDEPARTMENTSEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
    object mdDEPARTMENTSVACANCY: TBooleanField
      FieldName = 'VACANCY'
    end
  end
  object mdPERSONS: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0B000000040000000C000300494400320000000100
      05004E616D65000F00000001000800436F756E747279000A00000001000B0050
      6F7374616C436F6465000F000000010005004369747900640000000100080041
      64647265737300180000000100060050686F6E65001800000001000400466178
      006400000001000600454D41494C006400000001000900484F4D455041474500
      0400000003000D004465706172746D656E74494400}
    SortOptions = []
    Left = 136
    Top = 64
    object mdPERSONSID: TAutoIncField
      FieldName = 'ID'
    end
    object mdPERSONSName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object mdPERSONSCountry: TStringField
      FieldName = 'Country'
      Size = 15
    end
    object mdPERSONSPostalCode: TStringField
      FieldName = 'PostalCode'
      Size = 10
    end
    object mdPERSONSCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object mdPERSONSAddress: TStringField
      FieldName = 'Address'
      Size = 100
    end
    object mdPERSONSPhone: TStringField
      FieldName = 'Phone'
      Size = 24
    end
    object mdPERSONSFax: TStringField
      FieldName = 'Fax'
      Size = 24
    end
    object mdPERSONSEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 100
    end
    object mdPERSONSHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object mdPERSONSDepartmentID: TIntegerField
      FieldName = 'DepartmentID'
    end
  end
end
