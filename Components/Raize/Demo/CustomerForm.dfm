object FrmCustomer: TFrmCustomer
  Left = 258
  Top = 121
  ActiveControl = EdtRemarks
  BorderStyle = bsDialog
  Caption = 'Select a Customer'
  ClientHeight = 484
  ClientWidth = 362
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RzLabel9: TRzLabel
    Left = 16
    Top = 88
    Width = 86
    Height = 13
    Caption = 'Customer Number'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel7: TRzLabel
    Left = 180
    Top = 128
    Width = 50
    Height = 13
    Caption = 'Last Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel6: TRzLabel
    Left = 16
    Top = 128
    Width = 51
    Height = 13
    Caption = 'First Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel4: TRzLabel
    Left = 272
    Top = 228
    Width = 57
    Height = 13
    Caption = 'Postal Code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel3: TRzLabel
    Left = 176
    Top = 228
    Width = 52
    Height = 13
    Caption = 'State/Prov'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel2: TRzLabel
    Left = 16
    Top = 228
    Width = 19
    Height = 13
    Caption = 'City'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel15: TRzLabel
    Left = 208
    Top = 88
    Width = 50
    Height = 13
    Caption = 'VIP Status'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel14: TRzLabel
    Left = 16
    Top = 424
    Width = 41
    Height = 13
    Caption = 'Remarks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel13: TRzLabel
    Left = 16
    Top = 348
    Width = 24
    Height = 13
    Caption = 'Email'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel12: TRzLabel
    Left = 180
    Top = 308
    Width = 18
    Height = 13
    Caption = 'Fax'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel11: TRzLabel
    Left = 16
    Top = 308
    Width = 30
    Height = 13
    Caption = 'Phone'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel10: TRzLabel
    Left = 16
    Top = 268
    Width = 39
    Height = 13
    Caption = 'Country'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzLabel1: TRzLabel
    Left = 16
    Top = 188
    Width = 39
    Height = 13
    Caption = 'Address'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BevelWidth = 0
  end
  object RzDBLabel1: TRzDBLabel
    Left = 12
    Top = 40
    Width = 301
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    DataField = 'Last_Name'
    DataSource = srcCustomers
  end
  object CbxState: TRzDBComboBox
    Left = 172
    Top = 208
    Width = 97
    Height = 21
    DataField = 'State/Prov'
    DataSource = srcCustomers
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ItemHeight = 13
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 7
    Items.Strings = (
      'AK'
      'AL'
      'AR'
      'AZ'
      'CA'
      'CO'
      'CT'
      'DC'
      'DE'
      'FL'
      'GA'
      'HI'
      'IA'
      'ID'
      'IL'
      'IN'
      'KS'
      'KY'
      'LA'
      'MA'
      'MD'
      'ME'
      'MI'
      'MN'
      'MO'
      'MS'
      'MT'
      'NC'
      'ND'
      'NE'
      'NH'
      'NJ'
      'NM'
      'NV'
      'NY'
      'OH'
      'OK'
      'OR'
      'PA'
      'RI'
      'SC'
      'SD'
      'TN'
      'TX'
      'UT'
      'VA'
      'VT'
      'WA'
      'WI'
      'WV'
      'WY')
  end
  object EdtRemarks: TRzDBMemo
    Left = 12
    Top = 368
    Width = 337
    Height = 57
    Ctl3D = True
    DataField = 'Remarks'
    DataSource = srcCustomers
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 13
    FrameController = RzFrameController1
  end
  object EdtPostalCode: TRzDBEdit
    Left = 268
    Top = 208
    Width = 81
    Height = 21
    DataSource = srcCustomers
    DataField = 'Post_Code'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 8
  end
  object EdtPhone: TRzDBEdit
    Left = 12
    Top = 288
    Width = 165
    Height = 21
    DataSource = srcCustomers
    DataField = 'Phone'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 10
  end
  object EdtLastName: TRzDBEdit
    Left = 176
    Top = 108
    Width = 173
    Height = 21
    DataSource = srcCustomers
    DataField = 'Last_Name'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 3
  end
  object EdtFirstName: TRzDBEdit
    Left = 12
    Top = 108
    Width = 165
    Height = 21
    DataSource = srcCustomers
    DataField = 'First_Name'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 2
  end
  object EdtFax: TRzDBEdit
    Left = 176
    Top = 288
    Width = 173
    Height = 21
    DataSource = srcCustomers
    DataField = 'Fax'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 11
  end
  object EdtEmail: TRzDBEdit
    Left = 12
    Top = 328
    Width = 337
    Height = 21
    DataSource = srcCustomers
    DataField = 'EMail'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 12
  end
  object EdtCustNo: TRzDBEdit
    Left = 12
    Top = 68
    Width = 121
    Height = 21
    DataSource = srcCustomers
    DataField = 'CustNo'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
  end
  object EdtCountry: TRzDBEdit
    Left = 12
    Top = 248
    Width = 337
    Height = 21
    DataSource = srcCustomers
    DataField = 'Country'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 9
  end
  object EdtCity: TRzDBEdit
    Left = 12
    Top = 208
    Width = 161
    Height = 21
    DataSource = srcCustomers
    DataField = 'City'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 6
  end
  object EdtAddress2: TRzDBEdit
    Left = 12
    Top = 168
    Width = 337
    Height = 21
    DataSource = srcCustomers
    DataField = 'Address2'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 5
  end
  object EdtAddress1: TRzDBEdit
    Left = 12
    Top = 148
    Width = 337
    Height = 21
    DataSource = srcCustomers
    DataField = 'Address1'
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 4
  end
  object CbxVIPStatus: TRzDBComboBox
    Left = 204
    Top = 68
    Width = 145
    Height = 21
    DataField = 'VIP_Status'
    DataSource = srcCustomers
    Style = csDropDownList
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7879740
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    FrameController = RzFrameController1
    ItemHeight = 13
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 1
    Items.Strings = (
      ''
      'VIP'
      'VIP Exec')
  end
  object RzPanel1: TRzPanel
    Left = 0
    Top = 444
    Width = 362
    Height = 40
    Align = alBottom
    BorderOuter = fsFlatBold
    BorderSides = [sdTop]
    Color = 16119543
    TabOrder = 14
    object RzButton1: TRzButton
      Left = 188
      Top = 8
      Default = True
      FrameColor = 7617536
      ModalResult = 1
      Caption = 'OK'
      Color = 15791348
      HotTrack = True
      TabOrder = 0
    end
    object RzButton2: TRzButton
      Left = 274
      Top = 8
      Cancel = True
      FrameColor = 7617536
      ModalResult = 2
      Caption = 'Cancel'
      Color = 15791348
      HotTrack = True
      TabOrder = 1
    end
  end
  object RzPanel2: TRzPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 33
    Align = alTop
    BorderOuter = fsFlatBold
    BorderSides = [sdBottom]
    BorderColor = 16119543
    BorderWidth = 2
    Color = 16119543
    TabOrder = 15
    object RzDBNavigator1: TRzDBNavigator
      Left = 2
      Top = 2
      Width = 358
      Height = 25
      ButtonVisualStyle = vsGradient
      DataSource = srcCustomers
      Align = alTop
      BorderOuter = fsNone
      Color = 16119543
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
    end
  end
  object srcCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 216
    Top = 40
  end
  object RzFrameController1: TRzFrameController
    Color = clWhite
    FocusColor = 16250613
    FrameHotColor = clBlack
    FrameVisible = True
    FramingPreference = fpCustomFraming
    Left = 260
    Top = 40
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 180
    Top = 40
  end
end
