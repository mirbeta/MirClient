object EditorsInPlaceValidationDemoDataDM: TEditorsInPlaceValidationDemoDataDM
  OldCreateOrder = False
  Height = 219
  Width = 271
  object StyleRepository: TcxStyleRepository
    Left = 164
    Top = 8
    PixelsPerInch = 96
    object styCaption: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14329948
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 14590588
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13795663
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clYellow
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clNavy
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14590588
      TextColor = clWhite
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor]
      Color = 15185807
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15120279
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle3
      Styles.Inactive = cxStyle5
      Styles.Selection = cxStyle7
      Styles.Category = cxStyle2
      Styles.Header = cxStyle4
      Styles.IncSearch = cxStyle6
      BuiltIn = True
    end
  end
  object dxMemData1: TdxMemData
    Active = True
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F050000001400000001000A0046697273744E616D65
      0014000000010009004C6173744E616D65002800000001000800416464726573
      73001400000001000C0050686F6E654E756D626572001400000001000600456D
      61696C0001040000004A6F686E00011900000031323320486F6D65204C616E65
      2C20486F6D657376696C6C650000010500000048656E72790001160000003433
      3620317374204176652C20436C6576656C616E64010E00000028383030292032
      34342D31303639010F000000696E666F40686F74626F782E636F6D0105000000
      4672616E6B0106000000486F6C6D6573011C0000003334392047726170686963
      2044657369676E204C2C204E65776D616E000001070000004C65746963696101
      04000000466F72640100000000010E0000002835353529203737362D31353636
      010B000000666F726440686F74626F7801050000004B6172656E010600000048
      6F6C6D65730100000000010E0000002835353529203334322D32353734000105
      000000526F67657201090000004D696368656C736F6E011E0000003339323020
      4D696368656C736F6E2044722E2C20427269646765666F7264010E0000002835
      353529203935342D353138380111000000726F6765726D406D796D61696C2E62
      6F78}
    SortOptions = []
    Left = 56
    Top = 24
    object dxMemData1FirstName: TStringField
      FieldName = 'FirstName'
    end
    object dxMemData1LastName: TStringField
      FieldName = 'LastName'
    end
    object dxMemData1Address: TStringField
      FieldName = 'Address'
      Size = 40
    end
    object dxMemData1PhoneNumber: TStringField
      FieldName = 'PhoneNumber'
    end
    object dxMemData1Email: TStringField
      FieldName = 'Email'
    end
  end
  object DataSource: TDataSource
    DataSet = dxMemData1
    Left = 56
    Top = 80
  end
end
