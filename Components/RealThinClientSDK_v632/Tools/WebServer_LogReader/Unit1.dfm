object Form1: TForm1
  Left = 234
  Top = 178
  Width = 875
  Height = 641
  Caption = 'RTC WebServer Log Analyzer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 405
    Top = 0
    Width = 5
    Height = 605
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 605
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 403
      Height = 292
      Align = alTop
      TabOrder = 0
      DesignSize = (
        403
        292)
      object Label1: TLabel
        Left = 8
        Top = 212
        Width = 318
        Height = 13
        Caption = 
          'LOG File Name (full path): You can use * and ? to load multiple ' +
          'files.'
      end
      object Label2: TLabel
        Left = 8
        Top = 4
        Width = 165
        Height = 13
        Caption = 'Starting Page (leave Blank for Any)'
      end
      object Label3: TLabel
        Left = 8
        Top = 256
        Width = 73
        Height = 13
        Caption = 'Unique Visitors:'
      end
      object eVisitorCnt: TLabel
        Left = 88
        Top = 256
        Width = 9
        Height = 13
        Caption = '---'
      end
      object Label5: TLabel
        Left = 8
        Top = 44
        Width = 209
        Height = 13
        Caption = 'Starting Page Refferer (Leave blank for Any)'
      end
      object Label6: TLabel
        Left = 8
        Top = 272
        Width = 204
        Height = 13
        Caption = 'Click a Line in the List to see Details (Right)'
      end
      object Label9: TLabel
        Left = 8
        Top = 88
        Width = 346
        Height = 13
        Caption = 
          'Page which has to be visited for the User to be listed (Leave Bl' +
          'ank if Any)'
      end
      object Label10: TLabel
        Left = 8
        Top = 128
        Width = 276
        Height = 13
        Caption = 'Page which should NOT be visited for the User to be listed'
      end
      object Label11: TLabel
        Left = 8
        Top = 168
        Width = 72
        Height = 13
        Caption = 'Min Pageviews'
      end
      object Label12: TLabel
        Left = 172
        Top = 168
        Width = 77
        Height = 13
        Caption = 'Min Time on site'
      end
      object Label13: TLabel
        Left = 88
        Top = 168
        Width = 75
        Height = 13
        Caption = 'Max Pageviews'
      end
      object Label14: TLabel
        Left = 256
        Top = 168
        Width = 80
        Height = 13
        Caption = 'Max Time on site'
      end
      object Label15: TLabel
        Left = 344
        Top = 188
        Width = 54
        Height = 13
        Caption = 'HH:MM:SS'
      end
      object eFileName: TEdit
        Left = 8
        Top = 228
        Width = 353
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 8
      end
      object btnOpen: TButton
        Left = 364
        Top = 228
        Width = 25
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 11
        OnClick = btnOpenClick
      end
      object eStart: TEdit
        Left = 8
        Top = 20
        Width = 381
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnLoad: TButton
        Left = 284
        Top = 256
        Width = 107
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Append from File(s)'
        TabOrder = 10
        OnClick = btnLoadClick
      end
      object eRef: TEdit
        Left = 8
        Top = 60
        Width = 381
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object btnNew: TButton
        Left = 232
        Top = 256
        Width = 49
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Clear'
        TabOrder = 9
        OnClick = btnNewClick
      end
      object eMandatoryPage: TEdit
        Left = 8
        Top = 104
        Width = 381
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object eNegativePage: TEdit
        Left = 8
        Top = 144
        Width = 381
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object eMinPages: TEdit
        Left = 8
        Top = 184
        Width = 77
        Height = 21
        TabOrder = 4
      end
      object eMinTime: TEdit
        Left = 172
        Top = 184
        Width = 81
        Height = 21
        TabOrder = 6
      end
      object eMaxPages: TEdit
        Left = 88
        Top = 184
        Width = 77
        Height = 21
        TabOrder = 5
      end
      object eMaxTime: TEdit
        Left = 256
        Top = 184
        Width = 85
        Height = 21
        TabOrder = 7
      end
    end
    object eVisitors: TListBox
      Left = 1
      Top = 293
      Width = 403
      Height = 311
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      ScrollWidth = 1024
      TabOrder = 1
      OnClick = eVisitorsClick
    end
  end
  object Panel4: TPanel
    Left = 410
    Top = 0
    Width = 449
    Height = 605
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 447
      Height = 52
      Align = alTop
      TabOrder = 0
      DesignSize = (
        447
        52)
      object Label4: TLabel
        Left = 8
        Top = 8
        Width = 44
        Height = 13
        Caption = 'Visitor IP:'
      end
      object Label7: TLabel
        Left = 176
        Top = 8
        Width = 91
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Time spent on Site:'
      end
      object Label8: TLabel
        Left = 335
        Top = 8
        Width = 55
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Pageviews:'
      end
      object lVisitRef: TLabel
        Left = 8
        Top = 32
        Width = 41
        Height = 13
        Caption = 'Refferer:'
      end
      object eVisIP: TEdit
        Left = 60
        Top = 4
        Width = 97
        Height = 21
        ReadOnly = True
        TabOrder = 0
        OnEnter = eVisIPEnter
      end
      object eTimeSite: TEdit
        Left = 271
        Top = 4
        Width = 57
        Height = 21
        Anchors = [akTop, akRight]
        ReadOnly = True
        TabOrder = 1
      end
      object ePageVisit: TEdit
        Left = 395
        Top = 4
        Width = 45
        Height = 21
        Anchors = [akTop, akRight]
        ReadOnly = True
        TabOrder = 2
      end
      object eVisitRef: TEdit
        Left = 60
        Top = 28
        Width = 380
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 3
      end
    end
    object eInfo: TMemo
      Left = 1
      Top = 53
      Width = 447
      Height = 551
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
  end
  object OpenDlg: TOpenDialog
    DefaultExt = '*.LOG'
    Left = 352
    Top = 4
  end
end
