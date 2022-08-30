object JsonDemoForm: TJsonDemoForm
  Left = 96
  Top = 180
  Caption = 
    'ICS SSL Json Object Signing Demos - http://www.overbyte.be - V8.' +
    '54 - 25th April 2018'
  ClientHeight = 636
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Label25: TLabel
    Left = 8
    Top = 280
    Width = 216
    Height = 14
    Caption = 'Shared secret HMAC Key (32, 48 or 64 long)'
  end
  object Label26: TLabel
    Left = 10
    Top = 250
    Width = 148
    Height = 14
    Caption = 'Private RSA or ECDSA Key File'
  end
  object Label27: TLabel
    Left = 8
    Top = 310
    Width = 69
    Height = 14
    Caption = 'JWS Algorithm'
  end
  object Label1: TLabel
    Left = 10
    Top = 5
    Width = 77
    Height = 14
    Caption = 'Clear Text Lines'
  end
  object Label2: TLabel
    Left = 8
    Top = 115
    Width = 105
    Height = 14
    Caption = 'Base64 Encoded Text'
  end
  object Label3: TLabel
    Left = 10
    Top = 155
    Width = 87
    Height = 14
    Caption = 'Hex Encoded Text'
  end
  object Label4: TLabel
    Left = 280
    Top = 306
    Width = 418
    Height = 65
    Caption = 
      'This demo illustrates and tests many of the low level encoding a' +
      'nd decoding functions'#13#10'used in REST and JOSE HTTPS applications.' +
      ' Hash digests are a short string calculated '#13#10'from an inpiut, us' +
      'ed to sign an input either with a shared secret HMAC key, or wit' +
      'h a '#13#10'secret private key, where only a public key is needed to v' +
      'erify the input is not altered. '#13#10'  '
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 195
    Width = 88
    Height = 14
    Caption = 'URL Encoded Text'
  end
  object TextLines: TMemo
    Left = 8
    Top = 20
    Width = 583
    Height = 89
    Lines.Strings = (
      '{"LimitInfo": {"Used": 30.0,"Limit": 10000.0 },"success": true}')
    ScrollBars = ssBoth
    TabOrder = 0
    OnDblClick = TextLinesDblClick
  end
  object TestHmacKey: TEdit
    Left = 245
    Top = 275
    Width = 226
    Height = 22
    MaxLength = 64
    TabOrder = 5
    Text = 'mysecretkeyneedstobe32byteslong.'
  end
  object TestJWSAlg: TComboBox
    Left = 113
    Top = 305
    Width = 111
    Height = 22
    ItemHeight = 14
    ItemIndex = 1
    TabOrder = 6
    Text = 'jsigHmac256'
    Items.Strings = (
      'jsigNone'
      'jsigHmac256'
      'jsigHmac384'
      'jsigHmac512'
      'jsigRsa256'
      'jsigRsa384'
      'jsigRsa512'
      'jsigEcdsa256'
      'jsigEcdsa384'
      'jsigEcdsa512'
      'jsigRsaPss256'
      'jsigRsaPss384'
      'jsigRsaPss512'
      'jsigEdDSA')
  end
  object doBase64Dec: TButton
    Left = 705
    Top = 25
    Width = 84
    Height = 25
    Caption = 'Base64 Decode'
    TabOrder = 8
    OnClick = doBase64DecClick
  end
  object doTestUrlEnc: TButton
    Left = 610
    Top = 87
    Width = 84
    Height = 25
    Caption = 'Test Base64Url'
    TabOrder = 11
    OnClick = doTestUrlEncClick
  end
  object doB64URLEn: TButton
    Left = 610
    Top = 56
    Width = 84
    Height = 25
    Caption = 'B64Url Encode'
    TabOrder = 9
    OnClick = doB64URLEnClick
  end
  object doB64URLDec: TButton
    Left = 705
    Top = 56
    Width = 84
    Height = 25
    Caption = 'B64Url Decode'
    TabOrder = 10
    OnClick = doB64URLDecClick
  end
  object doEncodeURL: TButton
    Left = 610
    Top = 149
    Width = 84
    Height = 25
    Caption = 'Encode URL'
    TabOrder = 15
    OnClick = doEncodeURLClick
  end
  object doDecodeURL: TButton
    Left = 705
    Top = 149
    Width = 84
    Height = 25
    Caption = 'Decode URL'
    TabOrder = 16
    OnClick = doDecodeURLClick
  end
  object doTestSign: TButton
    Left = 705
    Top = 180
    Width = 84
    Height = 25
    Caption = 'Sign Prv Key'
    TabOrder = 18
    OnClick = doTestSignClick
  end
  object doJWS: TButton
    Left = 609
    Top = 211
    Width = 84
    Height = 25
    Caption = 'Json Web Sig'
    TabOrder = 19
    OnClick = doJWSClick
  end
  object doSignHmac: TButton
    Left = 610
    Top = 180
    Width = 84
    Height = 25
    Caption = 'Sign HMAC'
    TabOrder = 17
    OnClick = doSignHmacClick
  end
  object LogWin: TMemo
    Left = 0
    Top = 376
    Width = 823
    Height = 260
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 20
    WordWrap = False
  end
  object Base64Text: TEdit
    Left = 8
    Top = 130
    Width = 583
    Height = 22
    TabOrder = 1
    Text = 
      'eyJMaW1pdEluZm8iOiB7IlVzZWQiOiAzMC4wLCJMaW1pdCI6IDEwMDAwLjAgfSwi' +
      'c3VjY2VzcyI6IHRydWV9'
  end
  object HexText: TEdit
    Left = 8
    Top = 170
    Width = 583
    Height = 22
    TabOrder = 2
    Text = 
      '7b224c696d6974496e666f223a207b2255736564223a2033302e302c224c696d' +
      '6974223a2031303030302e30207d2c2273756363657373223a20747275657d'
  end
  object doHexDec: TButton
    Left = 705
    Top = 118
    Width = 84
    Height = 25
    Caption = 'Hex Decode'
    TabOrder = 13
    OnClick = doHexDecClick
  end
  object doHexEncode: TButton
    Left = 609
    Top = 118
    Width = 84
    Height = 25
    Caption = 'Hex Encode'
    TabOrder = 14
    OnClick = doHexEncodeClick
  end
  object doBase64Enc: TButton
    Left = 609
    Top = 25
    Width = 84
    Height = 25
    Caption = 'Base64 Encode'
    TabOrder = 7
    OnClick = doBase64EncClick
  end
  object TestPrivKeyFile: TComboBox
    Left = 180
    Top = 245
    Width = 406
    Height = 22
    ItemHeight = 14
    ItemIndex = 0
    TabOrder = 4
    Text = 'jose-ras-prvkey.pem'
    Items.Strings = (
      'jose-ras-prvkey.pem'
      'jose-ec-prvkey.pem'
      'jose-raspsss-prvkey.pem'
      'jose-ed25519-prvkey.pem')
  end
  object doHashDigest: TButton
    Left = 705
    Top = 87
    Width = 84
    Height = 25
    Caption = 'Hash Digests'
    TabOrder = 12
    OnClick = doHashDigestClick
  end
  object URLText: TEdit
    Left = 8
    Top = 210
    Width = 583
    Height = 22
    TabOrder = 3
    Text = 
      '%7B%22LimitInfo%22%3A%20%7B%22Used%22%3A%2030%2E0%2C%22Limit%22%' +
      '3A%2010000%2E0%20%7D%2C%22success%22%3A%20true%7D'
  end
end
