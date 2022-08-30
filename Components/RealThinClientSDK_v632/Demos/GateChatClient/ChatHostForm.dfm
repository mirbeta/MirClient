object ChatHostFrm: TChatHostFrm
  Left = 370
  Top = 211
  Width = 687
  Height = 480
  Caption = 'Chat Host'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 549
    Top = 0
    Width = 5
    Height = 435
    Align = alRight
    ResizeStyle = rsLine
  end
  object Panel2: TPanel
    Left = 554
    Top = 0
    Width = 115
    Height = 435
    Align = alRight
    TabOrder = 1
    object eUsers: TListBox
      Left = 1
      Top = 65
      Width = 113
      Height = 369
      TabStop = False
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 18
      ParentFont = False
      TabOrder = 0
      OnDblClick = eUsersDblClick
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 113
      Height = 64
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 4
        Top = 44
        Width = 53
        Height = 16
        Caption = 'In Room:'
      end
      object btnAddUser: TButton
        Left = 4
        Top = 4
        Width = 105
        Height = 33
        Hint = 'Invite a User to this CHAT Room'
        Caption = 'INVITE'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = btnAddUserClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 549
    Height = 435
    Align = alClient
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 1
      Top = 279
      Width = 547
      Height = 5
      Cursor = crVSplit
      Align = alBottom
      MinSize = 50
    end
    object Panel4: TPanel
      Left = 1
      Top = 284
      Width = 547
      Height = 150
      Align = alBottom
      TabOrder = 0
      object eMessage: TMemo
        Left = 1
        Top = 38
        Width = 545
        Height = 111
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
        OnKeyPress = eMessageKeyPress
      end
      object Panel9: TPanel
        Left = 1
        Top = 1
        Width = 545
        Height = 37
        Align = alTop
        TabOrder = 1
        object pbPenColor: TColorBox
          Left = 8
          Top = 8
          Width = 145
          Height = 22
          ItemHeight = 16
          TabOrder = 0
        end
        object pbPenWidth: TTrackBar
          Left = 156
          Top = 4
          Width = 113
          Height = 29
          Min = 1
          Position = 2
          TabOrder = 1
        end
        object Panel10: TPanel
          Left = 495
          Top = 1
          Width = 49
          Height = 35
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object btnClearDrawing: TBitBtn
            Left = 0
            Top = 4
            Width = 45
            Height = 29
            Caption = 'CLR'
            TabOrder = 0
            OnClick = btnClearDrawingClick
          end
        end
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 547
      Height = 278
      Align = alClient
      TabOrder = 1
      object spChat: TSplitter
        Left = 284
        Top = 1
        Height = 276
        Align = alRight
        MinSize = 50
      end
      object panChat: TPanel
        Left = 287
        Top = 1
        Width = 259
        Height = 276
        Align = alRight
        TabOrder = 0
        object eChat: TMemo
          Left = 1
          Top = 1
          Width = 257
          Height = 274
          TabStop = False
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object panDraw: TPanel
        Left = 1
        Top = 1
        Width = 283
        Height = 276
        Align = alClient
        TabOrder = 1
        object pbDrawing: TPaintBox
          Left = 1
          Top = 1
          Width = 281
          Height = 274
          Cursor = crCross
          Align = alClient
          OnMouseDown = pbDrawingMouseDown
          OnMouseMove = pbDrawingMouseMove
          OnMouseUp = pbDrawingMouseUp
          OnPaint = pbDrawingPaint
        end
      end
    end
  end
  object Link: TRtcGateClientLink
    AfterLogOut = LinkAfterLogOut
    OnDataReceived = LinkDataReceived
    OnInfoReceived = LinkInfoReceived
    OnReadyToSend = LinkReadyToSend
    OnStreamReset = LinkStreamReset
    Left = 32
    Top = 84
  end
  object runPaintJob: TRtcQuickJob
    AccessGUI = True
    OnExecute = runPaintJobExecute
    Left = 93
    Top = 84
  end
end
