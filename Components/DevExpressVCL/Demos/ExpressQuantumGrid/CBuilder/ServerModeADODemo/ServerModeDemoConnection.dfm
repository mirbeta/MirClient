inherited ServerModeDemoConnectionForm: TServerModeDemoConnectionForm
  Left = 579
  Top = 121
  BorderStyle = bsDialog
  Caption = 'ExpressQuantumGrid ServerMode Demo'
  ClientHeight = 477
  ClientWidth = 454
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 454
    Transparent = False
  end
  inherited sbMain: TStatusBar
    Top = 458
    Width = 454
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 16
    Width = 454
    Height = 442
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbSQLServer: TcxLabel
      Left = 20
      Top = 214
      Anchors = [akLeft, akBottom]
      Caption = 'SQL Server:'
      Transparent = True
    end
    object lbDatabase: TcxLabel
      Left = 20
      Top = 239
      Anchors = [akLeft, akBottom]
      Caption = 'Database:'
      Transparent = True
    end
    object lbLoginName: TcxLabel
      Left = 20
      Top = 365
      Anchors = [akLeft, akBottom]
      Caption = 'Login name:'
      Transparent = True
    end
    object lbPassword: TcxLabel
      Left = 20
      Top = 389
      Anchors = [akLeft, akBottom]
      Caption = 'Password:'
      Transparent = True
    end
    object lbRecordCount: TcxLabel
      Left = 263
      Top = 337
      Anchors = [akLeft, akBottom]
      Caption = 'Record count:'
      Transparent = True
    end
    object edSQLServer: TcxTextEdit
      Left = 80
      Top = 213
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      Text = 'localhost'
      Width = 157
    end
    object rgConnectUsing: TcxRadioGroup
      Left = 20
      Top = 292
      Anchors = [akLeft, akBottom]
      Caption = ' Connect using: '
      Properties.Items = <
        item
          Caption = 'Windows authentication'
        end
        item
          Caption = 'SQL Server authentication'
        end>
      Properties.OnChange = rgConnectUsingPropertiesChange
      ItemIndex = 0
      TabOrder = 7
      Height = 65
      Width = 217
    end
    object edLoginName: TcxTextEdit
      Left = 86
      Top = 364
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 9
      Text = 'sa'
      Width = 151
    end
    object edPassword: TcxTextEdit
      Left = 86
      Top = 389
      Anchors = [akLeft, akBottom]
      Enabled = False
      Properties.EchoMode = eemPassword
      TabOrder = 11
      Width = 151
    end
    object btAddRecordsAndStartDemo: TcxButton
      Left = 263
      Top = 362
      Width = 168
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Add Records and Start Demo'
      Enabled = False
      TabOrder = 14
      OnClick = btAddRecordsAndStartDemoClick
    end
    object btStartDemo: TcxButton
      Left = 263
      Top = 389
      Width = 168
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Start Demo'
      Enabled = False
      TabOrder = 15
      OnClick = btStartDemoClick
    end
    object seCount: TcxSpinEdit
      Left = 336
      Top = 337
      Anchors = [akLeft, akBottom]
      Properties.Alignment.Horz = taRightJustify
      Properties.Alignment.Vert = taVCenter
      Properties.DisplayFormat = '#,###'
      Properties.Increment = 50000.000000000000000000
      Properties.LargeIncrement = 100000.000000000000000000
      Properties.MinValue = 50000.000000000000000000
      TabOrder = 13
      Value = 100000
      Width = 95
    end
    object ProgressBar: TcxProgressBar
      Left = 7
      Top = 421
      TabStop = False
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Properties.PeakValue = 50.000000000000000000
      Properties.ShowTextStyle = cxtsText
      TabOrder = 18
      Height = 10
      Width = 440
    end
    object mDescription: TcxMemo
      Left = 7
      Top = 7
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      Properties.ReadOnly = True
      TabOrder = 0
      Height = 199
      Width = 440
    end
    object lbTableName: TcxLabel
      Left = 20
      Top = 264
      Anchors = [akLeft, akBottom]
      Caption = 'Table:'
      Transparent = True
    end
    object edDatabase: TcxTextEdit
      Left = 80
      Top = 238
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 4
      Text = 'ServerModeGridDemo'
      Width = 157
    end
    object edTableName: TcxTextEdit
      Left = 80
      Top = 264
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 6
      Text = 'ServerModeGridTableDemo'
      Width = 157
    end
    object btTestConnection: TcxButton
      Left = 263
      Top = 292
      Width = 168
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'Test connection'
      Default = True
      TabOrder = 12
      OnClick = btTestConnectionClick
    end
    object lbCurrentCount: TcxLabel
      Left = 263
      Top = 214
      Anchors = [akLeft, akBottom]
      AutoSize = False
      ParentFont = False
      Style.Font.Style = [fsBold]
      Properties.Alignment.Vert = taVCenter
      Properties.WordWrap = True
      Transparent = True
      Height = 67
      Width = 168
      AnchorY = 248
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
end
