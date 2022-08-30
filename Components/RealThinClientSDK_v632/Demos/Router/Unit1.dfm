object RtcRouterMainForm: TRtcRouterMainForm
  Left = 251
  Top = 225
  Width = 472
  Height = 593
  AutoSize = True
  Caption = 'RTC Router'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 5
    Top = 118
    Width = 101
    Height = 16
    Caption = 'Outgoig Address'
  end
  object Label2: TLabel
    Left = 5
    Top = 148
    Width = 81
    Height = 16
    Caption = 'Outgoing Port'
  end
  object Label4: TLabel
    Left = 5
    Top = 5
    Width = 92
    Height = 16
    Caption = 'Incomming Port'
  end
  object Label7: TLabel
    Left = 5
    Top = 413
    Width = 69
    Height = 16
    Caption = 'LOG Folder'
  end
  object Label8: TLabel
    Left = 5
    Top = 285
    Width = 106
    Height = 16
    Caption = 'Outgoing root URI'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 103
    Width = 454
    Height = 4
  end
  object Bevel2: TBevel
    Left = 0
    Top = 364
    Width = 454
    Height = 4
  end
  object Label9: TLabel
    Left = 384
    Top = 39
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label12: TLabel
    Left = 384
    Top = 69
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label14: TLabel
    Left = 379
    Top = 212
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Label16: TLabel
    Left = 379
    Top = 182
    Width = 60
    Height = 16
    Caption = 'Sec/pack'
  end
  object Bevel3: TBevel
    Left = 148
    Top = 29
    Width = 302
    Height = 66
    Style = bsRaised
  end
  object Bevel4: TBevel
    Left = 153
    Top = 172
    Width = 292
    Height = 65
    Style = bsRaised
  end
  object Label6: TLabel
    Left = 10
    Top = 522
    Width = 131
    Height = 16
    Caption = 'Outgoing Connections'
  end
  object Label3: TLabel
    Left = 241
    Top = 522
    Width = 51
    Height = 16
    Caption = 'Threads'
  end
  object Bevel5: TBevel
    Left = 0
    Top = 271
    Width = 449
    Height = 3
  end
  object eToAddr: TEdit
    Left = 118
    Top = 113
    Width = 331
    Height = 24
    TabOrder = 8
    Text = 'www.realthinclient.com'
    OnChange = eToAddrChange
  end
  object eToPort: TEdit
    Left = 118
    Top = 143
    Width = 55
    Height = 24
    TabOrder = 9
    Text = '80'
  end
  object bConnect: TButton
    Left = 369
    Top = 507
    Width = 80
    Height = 41
    Caption = 'START'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 31
    OnClick = bConnectClick
  end
  object eFromPort: TEdit
    Left = 118
    Top = 0
    Width = 55
    Height = 24
    TabOrder = 0
    Text = '80'
  end
  object xServerMulti: TCheckBox
    Left = 182
    Top = 0
    Width = 119
    Height = 26
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object xServerBlocking: TCheckBox
    Left = 310
    Top = 0
    Width = 139
    Height = 26
    Caption = 'Blocking WinSock'
    TabOrder = 2
  end
  object xClientMulti: TCheckBox
    Left = 182
    Top = 143
    Width = 119
    Height = 25
    Caption = 'Multi-Threaded'
    Checked = True
    State = cbChecked
    TabOrder = 10
  end
  object xClientBlocking: TCheckBox
    Left = 310
    Top = 143
    Width = 139
    Height = 25
    Caption = 'Blocking WinSock'
    TabOrder = 11
  end
  object xChangeHost: TCheckBox
    Left = 5
    Top = 310
    Width = 109
    Height = 26
    Caption = 'Change Host'
    Checked = True
    State = cbChecked
    TabOrder = 25
  end
  object eToHost: TEdit
    Left = 118
    Top = 310
    Width = 331
    Height = 24
    TabOrder = 26
    Text = 'www.realthinclient.com'
  end
  object xChangeURLs: TCheckBox
    Left = 5
    Top = 340
    Width = 395
    Height = 20
    Caption = 'Try to replace direct URLs on HTML Pages with relative URIs'
    TabOrder = 27
  end
  object xForceHttp10: TCheckBox
    Left = 5
    Top = 182
    Width = 124
    Height = 21
    Caption = 'Force HTTP/1.0'
    TabOrder = 12
  end
  object xResponseBuffer: TCheckBox
    Left = 5
    Top = 212
    Width = 134
    Height = 20
    Caption = 'Buffer Responses'
    TabOrder = 13
  end
  object xDebugLog: TCheckBox
    Left = 5
    Top = 379
    Width = 100
    Height = 21
    Caption = 'Debug LOG'
    TabOrder = 28
  end
  object xRequestBuffer: TCheckBox
    Left = 15
    Top = 54
    Width = 129
    Height = 21
    Caption = 'Buffer Requests'
    TabOrder = 3
  end
  object eLogFolder: TEdit
    Left = 84
    Top = 408
    Width = 365
    Height = 24
    TabOrder = 17
    OnChange = eLogFolderChange
  end
  object cReqOrder: TRadioGroup
    Left = 10
    Top = 448
    Width = 439
    Height = 55
    Caption = 'Request Forwarding Order'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Standard'
      'Reverse'
      'Random'
      'Chance')
    TabOrder = 18
  end
  object xEventLog: TCheckBox
    Left = 108
    Top = 379
    Width = 95
    Height = 21
    Caption = 'Event LOG'
    TabOrder = 14
  end
  object eToURI: TEdit
    Left = 118
    Top = 280
    Width = 331
    Height = 24
    TabOrder = 24
    OnExit = eToURIExit
  end
  object xBuffLog: TCheckBox
    Left = 212
    Top = 379
    Width = 153
    Height = 21
    Caption = 'LOG Buffering (faster)'
    TabOrder = 15
    OnClick = xBuffLogClick
  end
  object bDumpLog: TButton
    Left = 369
    Top = 369
    Width = 80
    Height = 36
    Caption = 'Dump Log'
    TabOrder = 16
    OnClick = bDumpLogClick
  end
  object xRequestInTimeouts: TCheckBox
    Left = 157
    Top = 39
    Width = 154
    Height = 21
    Caption = 'Request IN Timeout'
    TabOrder = 4
  end
  object xResponseOutTimeout: TCheckBox
    Left = 157
    Top = 69
    Width = 174
    Height = 21
    Caption = 'Response OUT Timeout'
    TabOrder = 6
  end
  object xRequestOutTimeout: TCheckBox
    Left = 162
    Top = 182
    Width = 159
    Height = 21
    Caption = 'Request OUT Timeout'
    TabOrder = 20
  end
  object xResponseInTimeout: TCheckBox
    Left = 162
    Top = 212
    Width = 154
    Height = 20
    Caption = 'Response IN Timeout'
    TabOrder = 22
  end
  object eRequestInTime: TSpinEdit
    Left = 330
    Top = 34
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 5
    Value = 5
  end
  object eResponseOutTime: TSpinEdit
    Left = 330
    Top = 64
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 7
    Value = 5
  end
  object eResponseInTime: TSpinEdit
    Left = 325
    Top = 207
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 23
    Value = 5
  end
  object eRequestOutTime: TSpinEdit
    Left = 325
    Top = 177
    Width = 50
    Height = 26
    MaxValue = 60
    MinValue = 0
    TabOrder = 21
    Value = 5
  end
  object eConCount: TSpinEdit
    Left = 148
    Top = 517
    Width = 80
    Height = 26
    MaxValue = 30000
    MinValue = 1
    TabOrder = 29
    Value = 1
  end
  object eThrCount: TSpinEdit
    Left = 295
    Top = 517
    Width = 66
    Height = 26
    MaxValue = 2000
    MinValue = 1
    TabOrder = 30
    Value = 64
  end
  object xPostReturnBeforeResponseSent: TCheckBox
    Left = 5
    Top = 241
    Width = 429
    Height = 21
    Caption = 
      'PostReturn before ResponseSent (faster, but can result in higher' +
      ' load)'
    TabOrder = 19
    OnClick = xPostReturnBeforeResponseSentClick
  end
  object Server: TRtcHttpServer
    ServerPort = '80'
    Left = 120
    Top = 344
  end
  object DataRouter: TRtcDataRouter
    Server = Server
    CheckOrder = 10
    OnCheckRequestI = DataRouterCheckRequest
    OnPostNewRequestI = DataRouterPostNewRequest
    OnPostOldRequestI = DataRouterPostOldRequest
    OnQueuedRequestI = DataRouterQueuedRequest
    OnPostReturn = DataRouterPostReturn
    OnRequestBeginO = DataRouterRequestBegin
    OnRequestReceiveAbortI = DataRouterRequestReceiveAbort
    OnRequestReceivedI = DataRouterRequestReceived
    OnRequestSendAbortO = DataRouterRequestSendAbort
    OnRequestSentO = DataRouterRequestSent
    OnResponseBeginO = DataRouterResponseBegin
    OnResponseReceiveAbortO = DataRouterResponseReceiveAbort
    OnResponseReceivedO = DataRouterResponseReceived
    OnResponseSendAbortI = DataRouterResponseSendAbort
    OnResponseSentI = DataRouterResponseSent
    OnDebugLog = DataRouterDebugLog
    Left = 172
    Top = 344
  end
  object StatProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = StatProviderCheckRequest
    OnDataReceived = StatProviderDataReceived
    Left = 236
    Top = 344
  end
  object DumpProvider: TRtcDataProvider
    Server = Server
    OnCheckRequest = DumpProviderCheckRequest
    OnDataReceived = DumpProviderDataReceived
    Left = 308
    Top = 344
  end
end
