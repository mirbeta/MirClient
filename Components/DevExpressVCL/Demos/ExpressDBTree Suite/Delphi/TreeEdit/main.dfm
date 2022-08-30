object FMain: TFMain
  Left = 455
  Top = 274
  Width = 539
  Height = 416
  Caption = 'Example of using dxDBTreeViewEdit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000003
    333300000000033B3BBB3300000003B3BBB77777777703333337FFFFFFF7033B
    3BB7F889FFF703B3BBB7F8FFFFF703333337F88CFFF7033B3BB7F8FFFFF703B3
    BBB7F8F889F703333337F8F8FFF7033BBBB7F882FFF700033337F8FFFFF70000
    0007F8FFFFF700000007FFFFFFF700000007777777F70000000000000000E0FF
    0000803F00008000000080000000800000008000000080000000800000008000
    00008000000080000000E0000000FE000000FE000000FE000000FFFF0000}
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 315
    Width = 523
    Height = 63
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 262
      Top = 6
      Width = 203
      Height = 26
      Caption = 
        'On CloseUp event  the dxDBTreeViewEdit also changes state and co' +
        'untry fields'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      WordWrap = True
    end
    object DBTreeViewEdit1: TdxDBTreeViewEdit
      Left = 8
      Top = 8
      Width = 243
      Height = 21
      CanSelectParents = False
      DropDownRows = 15
      ParentColor = False
      TabOrder = 0
      TabStop = True
      Text = 'DBTreeViewEdit1'
      TreeViewColor = clWindow
      TreeViewCursor = crDefault
      TreeViewFont.Charset = DEFAULT_CHARSET
      TreeViewFont.Color = clWindowText
      TreeViewFont.Height = -11
      TreeViewFont.Name = 'MS Sans Serif'
      TreeViewFont.Style = []
      TreeViewIndent = 19
      TreeViewReadOnly = False
      TreeViewShowButtons = True
      TreeViewShowHint = False
      TreeViewShowLines = True
      TreeViewShowRoot = True
      TreeViewSortType = stNone
      OnCloseUp = DBTreeViewEdit1CloseUp
      DividedChar = '.'
      TextStyle = tvtsShort
      DataField = 'City'
      DataSource = DataSource1
    end
    object DBNavigator1: TDBNavigator
      Left = 11
      Top = 35
      Width = 240
      Height = 25
      DataSource = DataSource1
      TabOrder = 1
    end
    object BitBtn1: TBitBtn
      Left = 443
      Top = 35
      Width = 75
      Height = 25
      TabOrder = 2
      Kind = bkClose
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 523
    Height = 315
    Align = alClient
    BevelInner = bvLowered
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object DBGrid1: TDBGrid
      Left = 6
      Top = 6
      Width = 511
      Height = 303
      Align = alClient
      DataSource = DataSource1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'CustNo'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Company'
          Width = 150
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'City'
          Width = 85
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'State'
          Width = 48
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Country'
          Visible = True
        end>
    end
  end
  object DataSource1: TDataSource
    DataSet = mdCustomer
    Left = 160
    Top = 224
  end
  object mdCustomer: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F0D0000000800000006000700437573744E6F001E00
      000001000800436F6D70616E79001E000000010006004164647231001E000000
      010006004164647232000F000000010005004369747900140000000100060053
      74617465000A000000010004005A6970001400000001000800436F756E747279
      000F0000000100060050686F6E65000F00000001000400464158000800000006
      00080054617852617465001400000001000800436F6E7461637400080000000B
      0010004C617374496E766F6963654461746500}
    SortOptions = []
    Left = 200
    Top = 240
    object mdCustomerCustNo: TFloatField
      FieldName = 'CustNo'
    end
    object mdCustomerCompany: TStringField
      FieldName = 'Company'
      Size = 30
    end
    object mdCustomerAddr1: TStringField
      FieldName = 'Addr1'
      Size = 30
    end
    object mdCustomerAddr2: TStringField
      FieldName = 'Addr2'
      Size = 30
    end
    object mdCustomerCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object mdCustomerState: TStringField
      FieldName = 'State'
    end
    object mdCustomerZip: TStringField
      FieldName = 'Zip'
      Size = 10
    end
    object mdCustomerCountry: TStringField
      FieldName = 'Country'
    end
    object mdCustomerPhone: TStringField
      FieldName = 'Phone'
      Size = 15
    end
    object mdCustomerFAX: TStringField
      FieldName = 'FAX'
      Size = 15
    end
    object mdCustomerTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object mdCustomerContact: TStringField
      FieldName = 'Contact'
    end
    object mdCustomerLastInvoiceDate: TDateTimeField
      FieldName = 'LastInvoiceDate'
    end
  end
end
