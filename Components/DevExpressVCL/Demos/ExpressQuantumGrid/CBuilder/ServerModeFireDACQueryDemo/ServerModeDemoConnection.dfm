inherited ServerModeDemoConnectionForm: TServerModeDemoConnectionForm
  Left = 579
  Top = 121
  BorderStyle = bsDialog
  Caption = 'ExpressQuantumGrid ServerMode Demo'
  ClientHeight = 568
  ClientWidth = 485
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 485
    Transparent = False
  end
  inherited sbMain: TStatusBar
    Top = 549
    Width = 485
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 16
    Width = 485
    Height = 533
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbSQLServer: TcxLabel
      Left = 20
      Top = 275
      Anchors = [akLeft, akBottom]
      Caption = 'SQL Server:'
      Transparent = True
    end
    object lbDatabase: TcxLabel
      Left = 20
      Top = 302
      Anchors = [akLeft, akBottom]
      Caption = 'Database:'
      Transparent = True
    end
    object lbLoginName: TcxLabel
      Left = 20
      Top = 454
      Anchors = [akLeft, akBottom]
      Caption = 'Login name:'
      Transparent = True
    end
    object lbPassword: TcxLabel
      Left = 20
      Top = 481
      Anchors = [akLeft, akBottom]
      Caption = 'Password:'
      Transparent = True
    end
    object lbRecordCount: TcxLabel
      Left = 295
      Top = 426
      Anchors = [akLeft, akBottom]
      Caption = 'Record count:'
      Transparent = True
    end
    object edSQLServer: TcxTextEdit
      Left = 112
      Top = 274
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      Text = 'localhost'
      Width = 161
    end
    object rgConnectUsing: TcxRadioGroup
      Left = 20
      Top = 382
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
      Width = 253
    end
    object edLoginName: TcxTextEdit
      Left = 86
      Top = 453
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 9
      Text = 'sa'
      Width = 187
    end
    object edPassword: TcxTextEdit
      Left = 86
      Top = 480
      Anchors = [akLeft, akBottom]
      Enabled = False
      Properties.EchoMode = eemPassword
      TabOrder = 11
      Width = 187
    end
    object btAddRecordsAndStartDemo: TcxButton
      Left = 295
      Top = 453
      Width = 168
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Add Records and Start Demo'
      Enabled = False
      TabOrder = 14
      OnClick = btAddRecordsAndStartDemoClick
    end
    object btStartDemo: TcxButton
      Left = 295
      Top = 480
      Width = 168
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Start Demo'
      Enabled = False
      TabOrder = 15
      OnClick = btStartDemoClick
    end
    object seCount: TcxSpinEdit
      Left = 368
      Top = 426
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
      Top = 512
      TabStop = False
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Properties.PeakValue = 50.000000000000000000
      Properties.ShowTextStyle = cxtsText
      TabOrder = 18
      Height = 10
      Width = 471
    end
    object mDescription: TcxMemo
      Left = 7
      Top = 8
      TabStop = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      Properties.ReadOnly = True
      TabOrder = 0
      Height = 259
      Width = 471
    end
    object lbOrdersTableName: TcxLabel
      Left = 20
      Top = 329
      Anchors = [akLeft, akBottom]
      Caption = 'Orders Table:'
      Transparent = True
    end
    object edDatabase: TcxTextEdit
      Left = 112
      Top = 301
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 4
      Text = 'ServerModeGridDemo'
      Width = 161
    end
    object edOrdersTableName: TcxTextEdit
      Left = 112
      Top = 328
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 6
      Text = 'ServerModeGridTableDemo'
      Width = 161
    end
    object btTestConnection: TcxButton
      Left = 295
      Top = 382
      Width = 168
      Height = 22
      Anchors = [akLeft, akBottom]
      Caption = 'Test connection'
      Default = True
      TabOrder = 12
      OnClick = btTestConnectionClick
    end
    object lbCurrentCount: TcxLabel
      Left = 295
      Top = 275
      Anchors = [akLeft, akBottom]
      AutoSize = False
      ParentFont = False
      Style.Font.Style = [fsBold]
      Properties.Alignment.Vert = taVCenter
      Properties.WordWrap = True
      Transparent = True
      Height = 97
      Width = 168
      AnchorY = 324
    end
    object lbCustomersTableName: TcxLabel
      Left = 20
      Top = 356
      Anchors = [akLeft, akBottom]
      Caption = 'Customers Table:'
      Transparent = True
    end
    object edCustomersTableName: TcxTextEdit
      Left = 112
      Top = 355
      Anchors = [akLeft, akBottom]
      Enabled = False
      TabOrder = 20
      Text = 'ServerModeGridTableDemo'
      Width = 161
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
