object ViewBandedDemoDataDM: TViewBandedDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 735
  Top = 167
  Height = 406
  Width = 309
  object dsUSERS: TDataSource
    DataSet = mdUsers
    Left = 40
    Top = 112
  end
  object dsPROJECTS: TDataSource
    DataSet = mdProjects
    Left = 40
    Top = 56
  end
  object dsITEMS: TDataSource
    DataSet = mdItems
    Left = 40
    Top = 176
  end
  object mdProjects: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F03000000040000000C000300494400640000000100
      05004E414D45000400000003000A004D414E41474552494400}
    SortOptions = []
    Left = 120
    Top = 56
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
  object mdUsers: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0D000000040000000C000300494400190000000100
      0600464E414D450014000000010006004D4E414D450019000000010006004C4E
      414D45000F00000001000800434F554E545259000A00000001000B00504F5354
      414C434F4445000F0000000100050043495459003C0000000100080041444452
      45535300180000000100060050484F4E45001800000001000400464158003200
      000001000600454D41494C003200000001000900484F4D455041474500040000
      0003000D004445504152544D454E54494400}
    SortOptions = []
    OnCalcFields = mdUsersCalcFields
    Left = 120
    Top = 112
    object mdUsersID: TAutoIncField
      FieldName = 'ID'
    end
    object mdUsersFNAME: TStringField
      FieldName = 'FNAME'
      Size = 25
    end
    object mdUsersMNAME: TStringField
      FieldName = 'MNAME'
    end
    object mdUsersLNAME: TStringField
      FieldName = 'LNAME'
      Size = 25
    end
    object mdUsersCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Size = 15
    end
    object mdUsersPOSTALCODE: TStringField
      FieldName = 'POSTALCODE'
      Size = 10
    end
    object mdUsersCITY: TStringField
      FieldName = 'CITY'
      Size = 15
    end
    object mdUsersADDRESS: TStringField
      FieldName = 'ADDRESS'
      Size = 60
    end
    object mdUsersPHONE: TStringField
      FieldName = 'PHONE'
      Size = 24
    end
    object mdUsersFAX: TStringField
      FieldName = 'FAX'
      Size = 24
    end
    object mdUsersEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 50
    end
    object mdUsersHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 50
    end
    object mdUsersDEPARTMENTID: TIntegerField
      FieldName = 'DEPARTMENTID'
    end
    object mdUsersName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'Name'
      Calculated = True
    end
  end
  object mdItems: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0D000000040000000C000300494400320000000100
      05004E414D4500020000000500050054595045000400000003000A0050524F4A
      45435449440002000000020009005052494F5249545900020000000200070053
      5441545553000400000003000A0043524541544F52494400080000000B000C00
      43524541544544444154450004000000030008004F574E455249440008000000
      0B0011004C4153544D4F4449464945444441544500080000000B000A00464958
      45444441544500000000000E000C004445534352495054494F4E00000000000E
      000B005245534F4C5554494F4E00}
    SortOptions = []
    Left = 120
    Top = 176
    object mdItemsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdItemsNAME: TStringField
      FieldName = 'NAME'
      Size = 50
    end
    object mdItemsTYPE: TBooleanField
      FieldName = 'TYPE'
    end
    object mdItemsPROJECTID: TIntegerField
      FieldName = 'PROJECTID'
    end
    object mdItemsPRIORITY: TSmallintField
      FieldName = 'PRIORITY'
    end
    object mdItemsSTATUS: TSmallintField
      FieldName = 'STATUS'
    end
    object mdItemsCREATORID: TIntegerField
      FieldName = 'CREATORID'
    end
    object mdItemsCREATEDDATE: TDateTimeField
      FieldName = 'CREATEDDATE'
    end
    object mdItemsOWNERID: TIntegerField
      FieldName = 'OWNERID'
    end
    object mdItemsLASTMODIFIEDDATE: TDateTimeField
      FieldName = 'LASTMODIFIEDDATE'
    end
    object mdItemsFIXEDDATE: TDateTimeField
      FieldName = 'FIXEDDATE'
    end
    object mdItemsDESCRIPTION: TMemoField
      FieldName = 'DESCRIPTION'
      BlobType = ftMemo
    end
    object mdItemsRESOLUTION: TMemoField
      FieldName = 'RESOLUTION'
      BlobType = ftMemo
    end
  end
end
