object BandedFixedDemoDataDM: TBandedFixedDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 753
  Top = 414
  Height = 261
  Width = 325
  object dsSheduler: TDataSource
    DataSet = mdSheduler
    Left = 40
    Top = 156
  end
  object dsPersons: TDataSource
    DataSet = mdPersons
    Left = 144
    Top = 156
  end
  object StyleRepository: TcxStyleRepository
    Left = 160
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
      Color = 15252642
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = 11032875
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
      Color = 15784893
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
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
      AssignedValues = [svColor, svTextColor]
      Color = 15451300
      TextColor = clBlack
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
      Color = 16048336
      TextColor = clBlack
    end
    object stlGroupNode: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15253902
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object stlFixedBand: TcxStyle
      AssignedValues = [svColor]
      Color = 15322014
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
      Styles.ContentEven = cxStyle7
      Styles.ContentOdd = cxStyle6
      Styles.Footer = cxStyle8
      Styles.IncSearch = cxStyle10
      Styles.Indicator = cxStyle11
      Styles.Preview = cxStyle12
      BuiltIn = True
    end
  end
  object dsProjects: TDataSource
    DataSet = mdProjects
    Left = 208
    Top = 156
  end
  object mdSheduler: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0A000000040000000C000300494400040000000300
      0A0050524F4A4543544944000400000003000700555345524944000200000002
      00070053554E4441590002000000020007004D4F4E4441590002000000020008
      0054554553444159000200000002000A005745444E4553444159000200000002
      0009005448555253444159000200000002000700465249444159000200000002
      000900534154555244415900}
    SortOptions = []
    OnCalcFields = mdShedulerCalcFields
    Left = 40
    Top = 100
    object mdShedulerID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
      Visible = False
    end
    object mdShedulerPROJECTID: TIntegerField
      FieldName = 'PROJECTID'
      ReadOnly = True
      Visible = False
    end
    object mdShedulerProjectManagerID: TIntegerField
      FieldKind = fkLookup
      FieldName = 'ProjectManagerID'
      LookupDataSet = mdProjects
      LookupKeyFields = 'ID'
      LookupResultField = 'MANAGERID'
      KeyFields = 'PROJECTID'
      ReadOnly = True
      Visible = False
      Lookup = True
    end
    object mdShedulerUSERID: TIntegerField
      FieldName = 'USERID'
      ReadOnly = True
    end
    object mdShedulerSUNDAY: TSmallintField
      FieldName = 'SUNDAY'
    end
    object mdShedulerMONDAY: TSmallintField
      FieldName = 'MONDAY'
    end
    object mdShedulerTUESDAY: TSmallintField
      FieldName = 'TUESDAY'
    end
    object mdShedulerWEDNESDAY: TSmallintField
      FieldName = 'WEDNESDAY'
    end
    object mdShedulerTHURSDAY: TSmallintField
      FieldName = 'THURSDAY'
    end
    object mdShedulerFRIDAY: TSmallintField
      FieldName = 'FRIDAY'
    end
    object mdShedulerSATURDAY: TSmallintField
      FieldName = 'SATURDAY'
    end
    object mdShedulerWeekSum: TIntegerField
      FieldKind = fkInternalCalc
      FieldName = 'WeekSum'
      Calculated = True
    end
    object mdShedulerWeekAVG: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'WeekAVG'
      Calculated = True
    end
  end
  object mdPersons: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0B000000040000000C000300494400320000000100
      05004E616D65000F00000001000800436F756E747279000A00000001000B0050
      6F7374616C436F6465000F000000010005004369747900640000000100080041
      64647265737300180000000100060050686F6E65001800000001000400466178
      006400000001000600454D41494C006400000001000900484F4D455041474500
      0400000003000D004465706172746D656E74494400}
    SortOptions = []
    Left = 148
    Top = 100
    object mdPersonsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdPersonsName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object mdPersonsCountry: TStringField
      FieldName = 'Country'
      Size = 15
    end
    object mdPersonsPostalCode: TStringField
      FieldName = 'PostalCode'
      Size = 10
    end
    object mdPersonsCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object mdPersonsAddress: TStringField
      FieldName = 'Address'
      Size = 100
    end
    object mdPersonsPhone: TStringField
      FieldName = 'Phone'
      Size = 24
    end
    object mdPersonsFax: TStringField
      FieldName = 'Fax'
      Size = 24
    end
    object mdPersonsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 100
    end
    object mdPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object mdPersonsDepartmentID: TIntegerField
      FieldName = 'DepartmentID'
    end
  end
  object mdProjects: TdxMemData
    Active = True
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F03000000040000000C000300494400640000000100
      05004E414D45000400000003000A004D414E41474552494400}
    SortOptions = []
    Left = 212
    Top = 100
    object mdProjectsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdProjectsNAME: TStringField
      FieldName = 'NAME'
      Size = 100
    end
    object mdProjectsMANAGERID: TIntegerField
      FieldName = 'MANAGERID'
    end
  end
end
