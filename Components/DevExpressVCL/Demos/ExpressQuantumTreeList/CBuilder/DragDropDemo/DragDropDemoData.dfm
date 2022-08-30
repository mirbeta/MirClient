object DragDropDemoDataDM: TDragDropDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 565
  Top = 332
  Height = 338
  Width = 336
  object dsDepartments: TDataSource
    DataSet = cdsDepartments
    Left = 32
    Top = 120
  end
  object dsPersons: TDataSource
    DataSet = cdsPersons
    Left = 120
    Top = 120
  end
  object StyleRepository: TcxStyleRepository
    Left = 248
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
    object cxStyle14: TcxStyle
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
  object dsDeptDict: TDataSource
    DataSet = cdsDeptDict
    Left = 32
    Top = 232
  end
  object dsPersDict: TDataSource
    DataSet = cdsPersDict
    Left = 120
    Top = 232
  end
  object cdsDepartments: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'PARENTID'
        DataType = ftInteger
      end
      item
        Name = 'MANAGERID'
        DataType = ftInteger
      end
      item
        Name = 'NAME'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'BUDGET'
        DataType = ftFloat
      end
      item
        Name = 'LOCATION'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'PHONE'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'FAX'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'EMAIL'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'VACANCY'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    ProviderName = 'dspDepartments'
    StoreDefs = True
    AfterPost = cdsDepartmentsAfterPost
    Left = 32
    Top = 64
    object cdsDepartmentsID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
      Visible = False
    end
    object cdsDepartmentsPARENTID: TIntegerField
      FieldName = 'PARENTID'
      Visible = False
    end
    object cdsDepartmentsNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object cdsDepartmentsBUDGET: TFloatField
      FieldName = 'BUDGET'
    end
    object cdsDepartmentsPHONE: TStringField
      FieldName = 'PHONE'
      Size = 50
    end
    object cdsDepartmentsFAX: TStringField
      FieldName = 'FAX'
      Size = 50
    end
    object cdsDepartmentsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 255
    end
    object cdsDepartmentsVACANCY: TBooleanField
      FieldName = 'VACANCY'
    end
  end
  object cdsPersons: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Country'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'PostalCode'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'City'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'Address'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'Phone'
        DataType = ftString
        Size = 24
      end
      item
        Name = 'Fax'
        DataType = ftString
        Size = 24
      end
      item
        Name = 'EMAIL'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'HOMEPAGE'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'DepartmentID'
        DataType = ftInteger
      end>
    IndexDefs = <
      item
        Name = 'cdsPersonsIndex1'
        Fields = 'ID'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'xDepartmentID'
        Fields = 'DepartmentID'
        Options = [ixCaseInsensitive]
      end>
    IndexFieldNames = 'DepartmentID'
    MasterFields = 'ID'
    MasterSource = dsDepartments
    Params = <>
    ProviderName = 'dspPersons'
    StoreDefs = True
    AfterPost = cdsPersonsAfterPost
    Left = 120
    Top = 64
    object cdsPersonsID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object cdsPersonsName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object cdsPersonsCountry: TStringField
      FieldName = 'Country'
      Size = 15
    end
    object cdsPersonsPostalCode: TStringField
      FieldName = 'PostalCode'
      Size = 10
    end
    object cdsPersonsCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object cdsPersonsAddress: TStringField
      FieldName = 'Address'
      Size = 100
    end
    object cdsPersonsPhone: TStringField
      FieldName = 'Phone'
      Size = 24
    end
    object cdsPersonsFax: TStringField
      FieldName = 'Fax'
      Size = 24
    end
    object cdsPersonsEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 100
    end
    object cdsPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object cdsPersonsDepartmentID: TIntegerField
      FieldName = 'DepartmentID'
    end
  end
  object cdsDeptDict: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'PARENTID'
        DataType = ftInteger
      end
      item
        Name = 'MANAGERID'
        DataType = ftInteger
      end
      item
        Name = 'NAME'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'BUDGET'
        DataType = ftFloat
      end
      item
        Name = 'LOCATION'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'PHONE'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'FAX'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'EMAIL'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'VACANCY'
        DataType = ftBoolean
      end>
    IndexDefs = <
      item
        Name = 'cdsDeptDictIndex1'
        Fields = 'ID'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'xManagerID'
        Fields = 'MANAGERID'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'xParentID'
        Fields = 'PARENTID'
        Options = [ixCaseInsensitive]
      end>
    Params = <>
    ProviderName = 'dspDepartments'
    StoreDefs = True
    AfterPost = cdsDeptDictAfterPost
    Left = 32
    Top = 184
  end
  object cdsPersDict: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Country'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'PostalCode'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'City'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'Address'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'Phone'
        DataType = ftString
        Size = 24
      end
      item
        Name = 'Fax'
        DataType = ftString
        Size = 24
      end
      item
        Name = 'EMAIL'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'HOMEPAGE'
        DataType = ftString
        Size = 100
      end
      item
        Name = 'DepartmentID'
        DataType = ftInteger
      end>
    IndexDefs = <>
    Params = <>
    ProviderName = 'dspPersons'
    StoreDefs = True
    AfterPost = cdsPersDictAfterPost
    Left = 120
    Top = 184
  end
  object dspDepartments: TDataSetProvider
    DataSet = cdsDeptData
    ResolveToDataSet = True
    Left = 188
    Top = 184
  end
  object dspPersons: TDataSetProvider
    DataSet = cdsPersData
    ResolveToDataSet = True
    Left = 256
    Top = 184
  end
  object cdsDeptData: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\DEPARTMENTS.xml'
    Params = <>
    Left = 188
    Top = 232
  end
  object cdsPersData: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\PERSONS.xml'
    Params = <>
    Left = 256
    Top = 232
  end
end
