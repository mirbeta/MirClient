object SimpleTreeDemoDataDM: TSimpleTreeDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 689
  Top = 389
  Height = 229
  Width = 325
  object dsDepartments: TDataSource
    DataSet = mdDepartments
    Left = 40
    Top = 112
  end
  object StyleRepository: TcxStyleRepository
    Left = 136
    Top = 8
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15519398
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle8: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle9: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle11: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle12: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object TreeListStyleSheetDevExpress: TcxTreeListStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle5
      Styles.Inactive = cxStyle9
      Styles.Selection = cxStyle13
      Styles.BandBackground = cxStyle2
      Styles.BandHeader = cxStyle3
      Styles.ColumnHeader = cxStyle4
      Styles.ContentEven = cxStyle6
      Styles.ContentOdd = cxStyle7
      Styles.Footer = cxStyle8
      Styles.IncSearch = cxStyle10
      Styles.Indicator = cxStyle11
      Styles.Preview = cxStyle12
      BuiltIn = True
    end
  end
  object mdDepartments: TdxMemData
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
    object mdDepartmentsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdDepartmentsPARENTID: TIntegerField
      FieldName = 'PARENTID'
    end
    object mdDepartmentsMANAGERID: TIntegerField
      FieldName = 'MANAGERID'
    end
    object mdDepartmentsNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object mdDepartmentsBUDGET: TFloatField
      FieldName = 'BUDGET'
    end
    object mdDepartmentsLOCATION: TStringField
      FieldName = 'LOCATION'
      Size = 50
    end
    object mdDepartmentsPHONE: TStringField
      FieldName = 'PHONE'
      Size = 50
    end
    object mdDepartmentsFAX: TStringField
      FieldName = 'FAX'
      Size = 50
    end
    object mdDepartmentsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
    object mdDepartmentsVACANCY: TBooleanField
      FieldName = 'VACANCY'
    end
  end
end
