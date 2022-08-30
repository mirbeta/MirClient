object frmPemTool1: TfrmPemTool1
  Left = 212
  Top = 124
  ClientHeight = 798
  ClientWidth = 880
  Color = clBtnFace
  Constraints.MinHeight = 379
  Constraints.MinWidth = 527
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    880
    798)
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 880
    Height = 759
    ActivePage = TabViewCerts
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabCertLv: TTabSheet
      Caption = 'List Certificates'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LvCerts: TListView
        Left = 0
        Top = 0
        Width = 872
        Height = 663
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Common Name'
          end
          item
            AutoSize = True
            Caption = 'Issued to'
          end
          item
            AutoSize = True
            Caption = 'Issuer'
          end
          item
            Caption = 'Expires at'
            Width = 70
          end
          item
            Caption = 'File Name'
            Width = 150
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmLv
        SmallImages = ImageList1
        SortType = stData
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = LvCertsColumnClick
        OnCompare = LvCertsCompare
        OnCustomDraw = LvCertsCustomDraw
        OnDblClick = LvCertsDblClick
      end
      object Panel1: TPanel
        Left = 0
        Top = 663
        Width = 872
        Height = 67
        Align = alBottom
        Alignment = taLeftJustify
        TabOrder = 1
        object Label4: TLabel
          Left = 4
          Top = 35
          Width = 47
          Height = 14
          Caption = 'Directory:'
        end
        object Label8: TLabel
          Left = 500
          Top = 10
          Width = 144
          Height = 14
          Caption = 'PKCS12 Certificate Password'
        end
        object btnRefresh: TButton
          Left = 423
          Top = 6
          Width = 71
          Height = 21
          Caption = '&Refresh'
          TabOrder = 3
          OnClick = btnRefreshClick
        end
        object CurrentCertDirEdit: TEdit
          Left = 57
          Top = 33
          Width = 709
          Height = 22
          TabOrder = 0
          Text = 'CurrentCertDirEdit'
          OnChange = CurrentCertDirEditChange
        end
        object btnDeleteCert: TButton
          Left = 342
          Top = 6
          Width = 75
          Height = 21
          Caption = '&Delete'
          TabOrder = 2
          OnClick = btnDeleteCertClick
        end
        object btnCopyCert: TButton
          Left = 252
          Top = 6
          Width = 75
          Height = 21
          Caption = '&Copy'
          TabOrder = 1
          OnClick = btnCopyCertClick
        end
        object SelCurrDir: TBitBtn
          Left = 797
          Top = 35
          Width = 31
          Height = 25
          TabOrder = 4
          OnClick = SelCurrDirClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object btnShowBundleFile: TButton
          Left = 12
          Top = 5
          Width = 109
          Height = 21
          Caption = '&View Bundle File'
          TabOrder = 6
          OnClick = btnShowBundleFileClick
        end
        object btnShowOneFile: TButton
          Left = 132
          Top = 5
          Width = 109
          Height = 21
          Caption = '&View Single File'
          TabOrder = 5
          OnClick = btnShowOneFileClick
        end
        object CertPassword: TEdit
          Left = 655
          Top = 7
          Width = 156
          Height = 22
          TabOrder = 7
        end
      end
    end
    object TabImport: TTabSheet
      Caption = 'Import Certificates'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        872
        730)
      object Bevel2: TBevel
        Left = 5
        Top = 305
        Width = 864
        Height = 64
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
        ExplicitWidth = 744
      end
      object Bevel1: TBevel
        Left = 5
        Top = 3
        Width = 864
        Height = 283
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
        ExplicitWidth = 744
      end
      object Label1: TLabel
        Left = 20
        Top = 28
        Width = 327
        Height = 56
        Caption = 
          'Current user'#39's Windows-System-Certificate-Store is opened.'#13#10'Then' +
          ' the DER formated certs are read and translated to PEM format.'#13#10 +
          'Certs are stored to the specified folder in the form of Hash.0.'#13 +
          #10'The '#39'Cert. Store Type'#39' box has static values: CA, ROOT, MY.'
      end
      object Label3: TLabel
        Left = 22
        Top = 94
        Width = 81
        Height = 14
        Caption = 'Cert. Store Type:'
      end
      object Label2: TLabel
        Left = 22
        Top = 118
        Width = 74
        Height = 14
        Caption = 'Destination Dir.:'
      end
      object Label5: TLabel
        Left = 20
        Top = 12
        Width = 338
        Height = 14
        Caption = 'Import a Windows Ceritificate Store to a Folder in PEM Format'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TLabel
        Left = 20
        Top = 319
        Width = 26
        Height = 14
        Caption = 'Misc'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel3: TBevel
        Left = 5
        Top = 385
        Width = 864
        Height = 76
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
        ExplicitWidth = 768
      end
      object Label7: TLabel
        Left = 16
        Top = 399
        Width = 177
        Height = 14
        Caption = 'Program Signing (Authenticode)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ComboBoxStoreType: TComboBox
        Left = 106
        Top = 90
        Width = 227
        Height = 22
        Hint = 'Select a Windows store type'
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 0
        Items.Strings = (
          'Certificate Authorities'
          'Root Certificate Authorities'
          'My Own Certificates'
          'Trusted Certificates'
          'Email Certificates')
      end
      object DestDirEdit: TEdit
        Left = 106
        Top = 114
        Width = 265
        Height = 22
        Hint = 'Existing destination directory '
        TabOrder = 1
        Text = 'DestDirEdit'
        OnChange = DestDirEditChange
      end
      object CheckBoxWarnDestNotEmpty: TCheckBox
        Left = 106
        Top = 145
        Width = 243
        Height = 17
        Caption = 'Warn me if destination folder is not empty'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBoxOverwriteExisting: TCheckBox
        Left = 106
        Top = 165
        Width = 243
        Height = 17
        Hint = 
          'If enabled, existing certs with the same name are overwritten.'#13#10 +
          'If not enabled, file extensions are changed. '#13#10'(e.g. 9d66eef0.0,' +
          ' 9d66eef0.1 etc)'
        Caption = 'Overwrite existing files, don'#39't change file ext.'
        TabOrder = 3
      end
      object CheckBoxEmptyDestDir: TCheckBox
        Left = 106
        Top = 185
        Width = 243
        Height = 17
        Hint = 'Warning! - deletes any file in destination folder '
        Caption = 'Empty destination directory'
        TabOrder = 4
      end
      object btnImport: TButton
        Left = 104
        Top = 252
        Width = 229
        Height = 21
        Caption = 'Start import from Windows'
        TabOrder = 6
        OnClick = btnImportClick
      end
      object CheckBoxWriteToBundle: TCheckBox
        Left = 106
        Top = 205
        Width = 145
        Height = 17
        Caption = 'Create a CA bundle file'
        TabOrder = 5
      end
      object SelImpDir: TBitBtn
        Left = 388
        Top = 113
        Width = 31
        Height = 25
        TabOrder = 7
        OnClick = SelImpDirClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object btnImportPemFile: TButton
        Left = 16
        Top = 339
        Width = 231
        Height = 21
        Caption = 'Import/Hash a PEM Cert File to Destination Dir.'
        TabOrder = 8
        OnClick = btnImportPemFileClick
      end
      object CheckBoxComment: TCheckBox
        Left = 106
        Top = 225
        Width = 145
        Height = 17
        Caption = 'Add Comments to file'
        TabOrder = 9
      end
      object btnCheckSigned: TButton
        Left = 112
        Top = 419
        Width = 95
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '&Check Signed'
        TabOrder = 10
        OnClick = btnCheckSignedClick
      end
    end
    object TabViewCerts: TTabSheet
      Caption = 'Create Certificates'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BoxLoadCert: TGroupBox
        Left = 3
        Top = 3
        Width = 860
        Height = 213
        Caption = 
          'Load Certificate and/or Private Key from File or Lines - PEM, DE' +
          'R, P12, PFX, CER, CRT, P7B, P7S, P7C '
        TabOrder = 0
        object Label9: TLabel
          Left = 5
          Top = 25
          Width = 44
          Height = 14
          Caption = 'Directory'
        end
        object Label10: TLabel
          Left = 5
          Top = 55
          Width = 39
          Height = 14
          Caption = 'Cert File'
        end
        object Label11: TLabel
          Left = 359
          Top = 85
          Width = 69
          Height = 14
          Caption = 'Inter Certs File'
        end
        object Label12: TLabel
          Left = 571
          Top = 55
          Width = 59
          Height = 14
          Caption = 'Request File'
        end
        object Label13: TLabel
          Left = 352
          Top = 110
          Width = 85
          Height = 28
          Caption = 'Base64 Encoded DER (PEM)'
          WordWrap = True
        end
        object Label14: TLabel
          Left = 650
          Top = 85
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label27: TLabel
          Left = 280
          Top = 55
          Width = 57
          Height = 14
          Caption = 'Prv Key File'
        end
        object Label33: TLabel
          Left = 5
          Top = 145
          Width = 69
          Height = 14
          Caption = 'CA Bundle File'
        end
        object LoadDirectory: TEdit
          Left = 55
          Top = 20
          Width = 668
          Height = 22
          TabOrder = 0
        end
        object SelLoadDir: TBitBtn
          Left = 758
          Top = 19
          Width = 31
          Height = 25
          TabOrder = 1
          OnClick = SelLoadDirClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object CertLinesOld: TMemo
          Left = 454
          Top = 110
          Width = 387
          Height = 91
          ScrollBars = ssBoth
          TabOrder = 18
        end
        object doLoadCert: TButton
          Left = 5
          Top = 80
          Width = 92
          Height = 21
          Caption = 'Load Certificate '
          TabOrder = 8
          OnClick = doLoadCertClick
        end
        object LoadCertFile: TEdit
          Left = 57
          Top = 48
          Width = 163
          Height = 22
          TabOrder = 2
          Text = 'mycertificate.pem'
        end
        object LoadPrivatetKey: TEdit
          Left = 350
          Top = 50
          Width = 163
          Height = 22
          TabOrder = 4
          Text = 'myprivatekey.pem'
        end
        object LoadRequestFile: TEdit
          Left = 650
          Top = 50
          Width = 166
          Height = 22
          TabOrder = 6
          Text = 'mycertrequest.pem'
        end
        object SelCertFile: TBitBtn
          Left = 240
          Top = 45
          Width = 31
          Height = 25
          TabOrder = 3
          OnClick = SelCertFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object SelPrvKeyFile: TBitBtn
          Left = 520
          Top = 45
          Width = 31
          Height = 25
          TabOrder = 5
          OnClick = SelPrvKeyFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object SelReqFile: TBitBtn
          Left = 822
          Top = 45
          Width = 31
          Height = 25
          TabOrder = 7
          OnClick = SelReqFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object doLoadPrvKey: TButton
          Left = 106
          Top = 80
          Width = 80
          Height = 21
          Caption = 'Load Key'
          TabOrder = 9
          OnClick = doLoadPrvKeyClick
        end
        object doLoadReq: TButton
          Left = 192
          Top = 80
          Width = 79
          Height = 21
          Caption = 'Load Request '
          TabOrder = 10
          OnClick = doLoadReqClick
        end
        object doLoadBase64: TButton
          Left = 332
          Top = 155
          Width = 109
          Height = 21
          Caption = 'Load Base64 Cert'
          TabOrder = 17
          OnClick = doLoadBase64Click
        end
        object LoadCertPW: TEdit
          Left = 720
          Top = 80
          Width = 133
          Height = 22
          TabOrder = 14
        end
        object LoadInterCerts: TEdit
          Left = 444
          Top = 82
          Width = 163
          Height = 22
          TabOrder = 12
          Text = 'intercerts.pem'
        end
        object doLoadInters: TButton
          Left = 277
          Top = 80
          Width = 69
          Height = 21
          Caption = 'Load Inters'
          TabOrder = 11
          OnClick = doLoadIntersClick
        end
        object SelIntersFile: TBitBtn
          Left = 613
          Top = 75
          Width = 31
          Height = 25
          TabOrder = 13
          OnClick = SelIntersFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object LoadCertPrivKey: TCheckBox
          Left = 9
          Top = 111
          Width = 148
          Height = 25
          Caption = 'Load Private Key from Cert File'
          TabOrder = 15
          WordWrap = True
        end
        object LoadCertInters: TCheckBox
          Left = 178
          Top = 107
          Width = 168
          Height = 17
          Caption = 'Load Inters from Cert File'
          TabOrder = 16
        end
        object LoadCaBundleFile: TEdit
          Left = 97
          Top = 142
          Width = 163
          Height = 22
          TabOrder = 19
          Text = 'mycabundle.pem'
        end
        object SelCAFile: TBitBtn
          Left = 280
          Top = 139
          Width = 31
          Height = 25
          TabOrder = 20
          OnClick = SelCAFileClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object doLoadCABundle: TButton
          Left = 5
          Top = 170
          Width = 99
          Height = 21
          Caption = 'Load CA Bundle'
          TabOrder = 21
          OnClick = doLoadCABundleClick
        end
      end
      object BoxCertProc: TGroupBox
        Left = 5
        Top = 227
        Width = 860
        Height = 287
        Caption = 
          'Certificate Processing - Create Key,  Create Request, Create Sel' +
          'f Signed, Create by Signing Request (properties on separate tab)'
        TabOrder = 1
        object LabelStateCert: TLabel
          Left = 10
          Top = 50
          Width = 296
          Height = 136
          AutoSize = False
          Caption = 'Certificate: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStateReq: TLabel
          Left = 595
          Top = 143
          Width = 257
          Height = 101
          AutoSize = False
          Caption = 'Certificate Request: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStatePrivKey: TLabel
          Left = 315
          Top = 50
          Width = 273
          Height = 41
          AutoSize = False
          Caption = 'Private Key: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStateCACert: TLabel
          Left = 10
          Top = 200
          Width = 296
          Height = 76
          AutoSize = False
          Caption = 'CA Certificate: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object Label24: TLabel
          Left = 315
          Top = 250
          Width = 44
          Height = 28
          Caption = 'CA Files Directory'
          WordWrap = True
        end
        object LabelInters: TLabel
          Left = 315
          Top = 97
          Width = 273
          Height = 147
          AutoSize = False
          Caption = 'Intermediate Certificates: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object doClearCerts: TButton
          Left = 10
          Top = 20
          Width = 133
          Height = 21
          Caption = 'Clear Certs and Keys'
          TabOrder = 0
          OnClick = doClearCertsClick
        end
        object doCreateReqProps: TButton
          Left = 149
          Top = 20
          Width = 152
          Height = 21
          Caption = 'Create Request from Props'
          TabOrder = 1
          OnClick = doCreateReqPropsClick
        end
        object doCreateReqCert: TButton
          Left = 307
          Top = 20
          Width = 144
          Height = 21
          Caption = 'Create Request from Cert'
          TabOrder = 2
          OnClick = doCreateReqCertClick
        end
        object doCreateSelfCert: TButton
          Left = 457
          Top = 20
          Width = 187
          Height = 21
          Caption = 'Create Self Signed Cert from Props'
          TabOrder = 3
          OnClick = doCreateSelfCertClick
        end
        object doCreateCACert: TButton
          Left = 650
          Top = 20
          Width = 191
          Height = 21
          Caption = 'Create Cert from Req, Sign with CA'
          TabOrder = 4
          OnClick = doCreateCACertClick
        end
        object CAFilesDir: TEdit
          Left = 378
          Top = 255
          Width = 423
          Height = 22
          TabOrder = 6
          OnClick = CAFilesDirClick
        end
        object SelCertsDB: TBitBtn
          Left = 820
          Top = 250
          Width = 31
          Height = 25
          TabOrder = 7
          OnClick = SelCertsDBClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object doCreateBundle: TButton
          Left = 594
          Top = 51
          Width = 127
          Height = 21
          Caption = 'Create Cert Bundle'
          TabOrder = 5
          OnClick = doCreateBundleClick
        end
        object doCheckBundleWin: TButton
          Left = 594
          Top = 78
          Width = 127
          Height = 21
          Caption = 'Check Cert Bundle (Win)'
          TabOrder = 8
          OnClick = doCheckBundleWinClick
        end
        object doCheckBundleSelf: TButton
          Left = 594
          Top = 105
          Width = 127
          Height = 21
          Caption = 'Check Cert Bundle (Self)'
          TabOrder = 9
          OnClick = doCheckBundleSelfClick
        end
      end
      object BoxCertSave: TGroupBox
        Left = 5
        Top = 525
        Width = 860
        Height = 201
        Caption = 
          'Save New Certificate, Public and Private Key and Certificate Req' +
          'uest to File'
        TabOrder = 2
        object Label17: TLabel
          Left = 5
          Top = 25
          Width = 44
          Height = 14
          Caption = 'Directory'
        end
        object Label18: TLabel
          Left = 5
          Top = 55
          Width = 62
          Height = 14
          Caption = 'PEM Cert File'
        end
        object Label19: TLabel
          Left = 5
          Top = 115
          Width = 97
          Height = 14
          Caption = 'PEM Private Key File'
        end
        object Label20: TLabel
          Left = 569
          Top = 55
          Width = 61
          Height = 14
          Caption = 'PEM Req File'
        end
        object Label22: TLabel
          Left = 570
          Top = 85
          Width = 75
          Height = 14
          Caption = 'PKCS7 Cert File'
        end
        object Label21: TLabel
          Left = 290
          Top = 55
          Width = 62
          Height = 14
          Caption = 'DER Cert File'
        end
        object Label23: TLabel
          Left = 290
          Top = 85
          Width = 81
          Height = 14
          Caption = 'PKCS12 Cert File'
        end
        object Label25: TLabel
          Left = 290
          Top = 115
          Width = 92
          Height = 14
          Caption = 'PEM Public Key File'
        end
        object Label26: TLabel
          Left = 570
          Top = 115
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label32: TLabel
          Left = 611
          Top = 138
          Width = 224
          Height = 14
          Caption = '(Must also set Key File Encryption on next tab)'
        end
        object SaveDirectory: TEdit
          Left = 55
          Top = 20
          Width = 668
          Height = 22
          TabOrder = 0
        end
        object SelSaveDir: TBitBtn
          Left = 752
          Top = 15
          Width = 31
          Height = 25
          TabOrder = 1
          OnClick = SelSaveDirClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
            333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
            300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
            333337F373F773333333303330033333333337F3377333333333303333333333
            333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
            333337777F337F33333330330BB00333333337F373F773333333303330033333
            333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
            333377777F77377733330BBB0333333333337F337F33333333330BB003333333
            333373F773333333333330033333333333333773333333333333}
          NumGlyphs = 2
        end
        object doSaveCertPem: TButton
          Left = 5
          Top = 165
          Width = 97
          Height = 21
          Caption = 'Save Pem Cert'
          TabOrder = 14
          OnClick = doSaveCertPemClick
        end
        object SaveCertPem: TEdit
          Left = 78
          Top = 50
          Width = 200
          Height = 22
          TabOrder = 2
          Text = 'newpemcert.pem'
        end
        object SavePrvFileFile: TEdit
          Left = 115
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 9
          Text = 'newprivatekey.pem'
        end
        object SaveReqCertFile: TEdit
          Left = 656
          Top = 50
          Width = 166
          Height = 22
          TabOrder = 4
          Text = 'newrequest.pem'
        end
        object doSaveCertDer: TButton
          Left = 108
          Top = 165
          Width = 96
          Height = 21
          Caption = 'Save DER Cert'
          TabOrder = 15
          OnClick = doSaveCertDerClick
        end
        object doSaveReqCert: TButton
          Left = 692
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Cert Request '
          TabOrder = 20
          OnClick = doSaveReqCertClick
        end
        object SavePkcs7File: TEdit
          Left = 656
          Top = 80
          Width = 163
          Height = 22
          TabOrder = 8
          Text = 'newcert.p7b'
        end
        object SavePrivateKey: TCheckBox
          Left = 5
          Top = 80
          Width = 166
          Height = 17
          Caption = 'Save Private Key in Cert File'
          TabOrder = 5
        end
        object SaveCertDer: TEdit
          Left = 390
          Top = 52
          Width = 163
          Height = 22
          TabOrder = 3
          Text = 'newdercert.der'
        end
        object SavePkcs12File: TEdit
          Left = 391
          Top = 80
          Width = 163
          Height = 22
          TabOrder = 7
          Text = 'newcert.p12'
        end
        object SavePubKeyFile: TEdit
          Left = 390
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 10
          Text = 'newpublickey.pem'
        end
        object SaveInterCerts: TCheckBox
          Left = 175
          Top = 80
          Width = 109
          Height = 17
          Caption = 'Save Inter Certs'
          TabOrder = 6
        end
        object SaveCertPW: TEdit
          Left = 656
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 11
        end
        object SaveAutoReplace: TCheckBox
          Left = 5
          Top = 140
          Width = 210
          Height = 17
          Caption = 'Automatically Replace Existing File'
          TabOrder = 12
        end
        object doSavePkcs12: TButton
          Left = 212
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save PKCS12 Cert'
          TabOrder = 16
          OnClick = doSavePkcs12Click
        end
        object doSavePkcs7Cert: TButton
          Left = 330
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save PKCS7 Cert '
          TabOrder = 17
          OnClick = doSavePkcs7CertClick
        end
        object doSavePrivKey: TButton
          Left = 452
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Private Key'
          TabOrder = 18
          OnClick = doSavePrivKeyClick
        end
        object doSavePubKey: TButton
          Left = 572
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Public Key'
          TabOrder = 19
          OnClick = doSavePubKeyClick
        end
        object CertAddComment: TCheckBox
          Left = 221
          Top = 140
          Width = 173
          Height = 17
          Caption = 'Add Comments to PEM File:'
          Checked = True
          State = cbChecked
          TabOrder = 13
        end
      end
    end
    object TabNew: TTabSheet
      Caption = 'New Certificate Properties'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBoxCertCreate: TGroupBox
        Left = 3
        Top = 337
        Width = 858
        Height = 404
        Caption = 'New Certificate or Request Properties '
        TabOrder = 0
        object lbCountry: TLabel
          Left = 5
          Top = 44
          Width = 69
          Height = 14
          Caption = 'Country Code:'
        end
        object lbState: TLabel
          Left = 5
          Top = 70
          Width = 28
          Height = 14
          Caption = 'State:'
        end
        object lbLocality: TLabel
          Left = 5
          Top = 95
          Width = 40
          Height = 14
          Caption = 'Locality:'
        end
        object lbOrganization: TLabel
          Left = 5
          Top = 120
          Width = 64
          Height = 14
          Caption = 'Organization:'
        end
        object lbOrganizationalUnit: TLabel
          Left = 5
          Top = 145
          Width = 93
          Height = 14
          Caption = 'Organizational Unit:'
        end
        object lbCommonName: TLabel
          Left = 352
          Top = 25
          Width = 146
          Height = 41
          AutoSize = False
          Caption = 
            'Common Name (server domain name or company name or email or CA n' +
            'ame):'
          WordWrap = True
        end
        object lbEMail: TLabel
          Left = 5
          Top = 170
          Width = 74
          Height = 14
          Caption = 'E-Mail address:'
        end
        object lbDays: TLabel
          Left = 355
          Top = 160
          Width = 61
          Height = 14
          Caption = 'Expiry Days:'
        end
        object Label28: TLabel
          Left = 5
          Top = 20
          Width = 150
          Height = 14
          Caption = 'Issued to (Subject) Information:'
        end
        object Label29: TLabel
          Left = 5
          Top = 195
          Width = 51
          Height = 14
          Caption = 'Desription:'
        end
        object Label30: TLabel
          Left = 352
          Top = 80
          Width = 127
          Height = 14
          Caption = 'Alternate Names: Domains'
        end
        object Label31: TLabel
          Left = 352
          Top = 130
          Width = 150
          Height = 14
          Caption = 'Alternate Names: IP Addresses'
        end
        object CertCountry: TEdit
          Left = 126
          Top = 40
          Width = 31
          Height = 22
          TabOrder = 0
        end
        object CertState: TEdit
          Left = 126
          Top = 65
          Width = 190
          Height = 22
          TabOrder = 1
        end
        object CertLocality: TEdit
          Left = 126
          Top = 90
          Width = 190
          Height = 22
          TabOrder = 2
        end
        object CertOrganization: TEdit
          Left = 126
          Top = 115
          Width = 215
          Height = 22
          TabOrder = 3
        end
        object CertOrganizationalUnit: TEdit
          Left = 126
          Top = 140
          Width = 215
          Height = 22
          TabOrder = 4
        end
        object CertCommonName: TEdit
          Left = 518
          Top = 30
          Width = 290
          Height = 22
          TabOrder = 7
          Text = 'www.domain.com'
        end
        object CertEMail: TEdit
          Left = 126
          Top = 165
          Width = 215
          Height = 22
          TabOrder = 5
        end
        object CertDays: TEdit
          Left = 432
          Top = 155
          Width = 70
          Height = 22
          TabOrder = 10
          Text = '366'
        end
        object CertSignHash: TRadioGroup
          Left = 570
          Top = 175
          Width = 271
          Height = 71
          Caption = 'Sign Digest Hash'
          Columns = 3
          ItemIndex = 2
          Items.Strings = (
            'SHA1 (old)'
            'SHA224'
            'SHA256'
            'SHA384'
            'SHA512'
            'SHA3_224'
            'SHA3_256'
            'SHA3_384'
            'SHA3_512')
          TabOrder = 12
        end
        object CertDescr: TEdit
          Left = 126
          Top = 190
          Width = 215
          Height = 22
          TabOrder = 6
        end
        object GroupBox1: TGroupBox
          Left = 10
          Top = 215
          Width = 200
          Height = 48
          Caption = 'Basic Constraints'
          TabOrder = 13
          object CertIsCA: TCheckBox
            Left = 10
            Top = 20
            Width = 181
            Height = 17
            Caption = 'Certificate Authority (for signing) '
            TabOrder = 0
          end
        end
        object GroupBox2: TGroupBox
          Left = 230
          Top = 215
          Width = 185
          Height = 166
          Caption = 'Key Usage'
          TabOrder = 14
          object CertUsageCertSign: TCheckBox
            Left = 10
            Top = 20
            Width = 126
            Height = 17
            Caption = 'Certificate Sign'
            TabOrder = 0
          end
          object CertUsageCRLSign: TCheckBox
            Left = 10
            Top = 40
            Width = 126
            Height = 17
            Caption = 'CRL Sign'
            TabOrder = 1
          end
          object CertUsageDigSign: TCheckBox
            Left = 10
            Top = 60
            Width = 126
            Height = 17
            Caption = 'Digital Signature'
            TabOrder = 2
          end
          object CertUsageDataEn: TCheckBox
            Left = 10
            Top = 80
            Width = 126
            Height = 17
            Caption = 'Data Encipherment'
            TabOrder = 3
          end
          object CertUsageKeyEn: TCheckBox
            Left = 10
            Top = 100
            Width = 126
            Height = 17
            Caption = 'Key Encipherment'
            TabOrder = 4
          end
          object CertUsageKeyAgree: TCheckBox
            Left = 10
            Top = 120
            Width = 126
            Height = 17
            Caption = 'Key Agreement'
            TabOrder = 5
          end
          object CertUsageNonRepud: TCheckBox
            Left = 10
            Top = 140
            Width = 126
            Height = 17
            Caption = 'Key Repudiation'
            TabOrder = 6
          end
        end
        object GroupBox3: TGroupBox
          Left = 10
          Top = 269
          Width = 200
          Height = 112
          Caption = 'Extended Key Usage'
          TabOrder = 15
          object CertExtServer: TCheckBox
            Left = 10
            Top = 20
            Width = 181
            Height = 17
            Caption = 'TLS Web Server Authentication'
            TabOrder = 0
          end
          object CertExtClient: TCheckBox
            Left = 10
            Top = 40
            Width = 181
            Height = 17
            Caption = 'TLS Web Client Authentication'
            TabOrder = 1
          end
          object CertExtEmail: TCheckBox
            Left = 10
            Top = 60
            Width = 126
            Height = 17
            Caption = 'E-mail Protection'
            TabOrder = 2
          end
          object CertExtCodeSign: TCheckBox
            Left = 10
            Top = 80
            Width = 126
            Height = 17
            Caption = 'Code Signing'
            TabOrder = 3
          end
        end
        object CertLinesNew: TMemo
          Left = 433
          Top = 257
          Width = 401
          Height = 124
          ScrollBars = ssBoth
          TabOrder = 16
        end
        object CertAltDomains: TMemo
          Left = 518
          Top = 60
          Width = 290
          Height = 54
          Lines.Strings = (
            'www.domain.com'
            'mail.domain.com'
            'domain.com')
          ScrollBars = ssVertical
          TabOrder = 8
        end
        object CertAltIPs: TMemo
          Left = 518
          Top = 120
          Width = 290
          Height = 45
          ScrollBars = ssVertical
          TabOrder = 9
        end
        object NewCertCopyExt: TCheckBox
          Left = 437
          Top = 193
          Width = 124
          Height = 53
          Caption = 'Copy Request Extensions for New Certificate'
          TabOrder = 11
          WordWrap = True
        end
      end
      object GroupKeys: TGroupBox
        Left = 3
        Top = 10
        Width = 553
        Height = 311
        Caption = 'New Private and Public Key Properties'
        TabOrder = 1
        object KeyType: TRadioGroup
          Left = 10
          Top = 15
          Width = 251
          Height = 274
          Caption = 'Key Type'
          ItemIndex = 1
          Items.Strings = (
            'RSA 1,024 bits (level 1 - 80 bits)'
            'RSA 2,048 bits (level 2 - 112 bits) '
            'RSA 3,072 bits (level 3 - 128 bits, NIST min)'
            'RSA 4,096 bits (level 3 - 128 bits)'
            'RSA 7,680 bits (level 4 - 192 bits)'
            'RSA 15,360 bits (level 5 - 256 bits)'
            'Elliptic Curve secp256  (level 3 - 128 bits) '
            'Elliptic Curve secp384  (level 4 - 192 bits) '
            'Elliptic Curve secp512  (level 5 - 256 bits) '
            'EdDSA ED25519 (level 3 - 128 bits)  '
            'RSA-PSS 2,048 bits (level 2 - 112 bits) '
            'RSA-PSS 3,072 bits (level 3 - 128 bits)'
            'RSA-PSS 4,096 bits (level 3 - 128 bits)'
            'RSA-PSS 7,680 bits (level 4 - 192 bits)'
            'RSA-PSS 15,360 bits (level 5 - 256 bits)')
          TabOrder = 0
        end
        object KeyEncrypt: TRadioGroup
          Left = 275
          Top = 15
          Width = 185
          Height = 129
          Caption = 'Key File Encryption'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Triple DES'
            'IDEA'
            'AES128'
            'AES192'
            'AES256'
            'Blowfish')
          TabOrder = 1
        end
        object KeyPairLines: TMemo
          Left = 275
          Top = 187
          Width = 261
          Height = 114
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 3
        end
        object doGenKey: TButton
          Left = 275
          Top = 158
          Width = 109
          Height = 21
          Caption = 'Generate Key Pair'
          TabOrder = 2
          OnClick = doGenKeyClick
        end
      end
      object GroupDHParam: TGroupBox
        Left = 570
        Top = 15
        Width = 291
        Height = 306
        Caption = 'New DH Parameters'
        TabOrder = 2
        object Label15: TLabel
          Left = 20
          Top = 117
          Width = 257
          Height = 28
          Caption = 
            'Beware DH Params can take a long time to calculate, several hour' +
            's for the longest (PC speed depenent).'
          WordWrap = True
        end
        object Label16: TLabel
          Left = 10
          Top = 160
          Width = 46
          Height = 14
          Caption = 'File Name'
        end
        object DHParamSize: TRadioGroup
          Left = 10
          Top = 15
          Width = 185
          Height = 96
          Caption = 'DHParam Size'
          ItemIndex = 2
          Items.Strings = (
            '768-bits (few secs)'
            '1024-bits (many secs)'
            '2048-bits (few mins)'
            '4096-bits (few hours)'
            '8192-bits (few days)')
          TabOrder = 0
        end
        object DHParamFile: TEdit
          Left = 69
          Top = 155
          Width = 166
          Height = 22
          TabOrder = 1
          Text = 'newdhparams.pem'
        end
        object DHParamsLines: TMemo
          Left = 15
          Top = 210
          Width = 246
          Height = 89
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 3
        end
        object doDHParams: TButton
          Left = 15
          Top = 183
          Width = 126
          Height = 21
          Caption = 'Generate DH Params'
          TabOrder = 2
          OnClick = doDHParamsClick
        end
      end
    end
  end
  object ProgressBar1: TProgressBar
    Left = 801
    Top = 4
    Width = 73
    Height = 16
    Anchors = [akTop, akRight]
    TabOrder = 1
    Visible = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 759
    Width = 880
    Height = 39
    Align = alBottom
    TabOrder = 2
    object Status: TLabel
      Left = 5
      Top = 5
      Width = 864
      Height = 26
      AutoSize = False
      Caption = 'Status'
      Color = clActiveBorder
      ParentColor = False
      WordWrap = True
    end
  end
  object pmLv: TPopupMenu
    Left = 349
    Top = 753
    object pmShowDetails: TMenuItem
      Caption = 'Show Details'
      OnClick = LvCertsDblClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmCopy: TMenuItem
      Caption = 'Copy Certificate'
      OnClick = btnCopyCertClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmDelete: TMenuItem
      Caption = 'Delete Certificate'
      OnClick = btnDeleteCertClick
    end
  end
  object ImageList1: TImageList
    Left = 260
    Top = 751
    Bitmap = {
      494C010103000500280010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000084848400C6C6C600C6C6
      C600C6C6C60084848400C6C6C600C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C6008484840000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C600000000000000
      000000FFFF00000000000000000000000000000000000000000000FFFF000000
      000000000000C6C6C60000840000000000000000000000000000000000000000
      000000000000000000000000000084848400FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C600000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000C6C6C60000840000000000000000000000000000000000000000
      00000000000000000000848484000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000008400008484840000FFFF000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000008484840000840000000000000000000000000000000000000000
      00000000000000000000848484000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C600000000000000
      000000FFFF0000000000000000008400000000000000000000008400000000FF
      FF0000000000C6C6C60000840000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C60000000000C6C6
      C60000848400C6C6C6008400000000FFFF00C6C6C60084000000000000008400
      000000000000C6C6C60000840000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000848484000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000084848400000000000084
      8400848484000084840000000000840000000000000000000000C6C6C6000000
      0000000000008484840000840000000000000000000000000000000000000000
      000084848400000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000848484000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C60000000000C6C6
      C60000848400C6C6C600000000000000000000000000000000000000000000FF
      FF0000000000C6C6C60000840000000000000000000000000000000000000000
      000084848400848484008484840084848400848484008484840084848400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084848400FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000840000C6C6C60000FFFF000000
      0000000000000000000000FFFF00000000000000000000FFFF00000000000000
      000000000000C6C6C60000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000084848400C6C6C600C6C6
      C600C6C6C600C6C6C60084848400C6C6C600C6C6C600C6C6C60084848400C6C6
      C600C6C6C6008484840000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000084000000840000008400000084
      0000008400000084000000840000008400000084000000840000008400000084
      0000008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF00008000FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF0000
      37D8FE7FF00F00003000FDBFF7EF00001F78FDBFFBDF000036C8FBDFFBDF0000
      2028FBDFFDBF000022D8F7EFFDBF000023E8F00FFE7F00001DB8FFFFFFFF0000
      0000FFFFFFFF00000001FFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object OpenDlg: TOpenDialog
    Filter = 'All Files *.*|*.*|PEM Files *.pem|*.pem'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 178
    Top = 747
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 752
    object MMFile: TMenuItem
      Caption = '&File'
      object MMFileExit: TMenuItem
        Caption = '&Exit'
        OnClick = MMFileExitClick
      end
    end
    object MMExtras: TMenuItem
      Caption = '&Extras'
      object MMExtrasCreateSelfSignedCert: TMenuItem
        Caption = 'Create a self-signed certificate..'
        OnClick = MMExtrasCreateSelfSignedCertClick
      end
      object MMExtrasCreateCertRequest: TMenuItem
        Caption = 'Create a certificate request..'
        OnClick = MMExtrasCreateCertRequestClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringRSA: TMenuItem
        Caption = 'RSA encrypt/decrypt..'
        OnClick = MMExtrasEncryptStringRSAClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt string'
        OnClick = MMExtrasEncryptStringBlowfishClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStreamBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt stream'
        OnClick = MMExtrasEncryptStreamBlowfishClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptFileBlowfish: TMenuItem
        Caption = 'Blowfish encrypt file..'
        OnClick = MMExtrasEncryptFileBlowfishClick
      end
      object MMExtrasDecryptFileBlowfish: TMenuItem
        Caption = 'Blowfish decrypt file..'
        OnClick = MMExtrasDecryptFileBlowfishClick
      end
    end
    object About1: TMenuItem
      Caption = 'About'
      OnClick = About1Click
    end
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Certificate Directory'
    Left = 215
    Top = 750
  end
end
