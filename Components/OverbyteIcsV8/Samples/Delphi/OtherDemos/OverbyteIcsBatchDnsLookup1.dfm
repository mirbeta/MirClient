object BatchDnsLookupForm: TBatchDnsLookupForm
  Left = 202
  Top = 118
  BorderStyle = bsToolWindow
  Caption = 'ICS batch async DNS lookup new API (IPv6 and IPv4)'
  ClientHeight = 449
  ClientWidth = 366
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    366
    449)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 384
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Min. #'
    ExplicitTop = 298
  end
  object Label2: TLabel
    Left = 69
    Top = 384
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Max. #'
    ExplicitTop = 298
  end
  object Label3: TLabel
    Left = 8
    Top = 365
    Width = 295
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Lookup threads shared by all instances in this thread context '
    ExplicitTop = 279
  end
  object Label4: TLabel
    Left = 8
    Top = 345
    Width = 149
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Number of TWSocket instances'
    ExplicitTop = 259
  end
  object Label5: TLabel
    Left = 8
    Top = 404
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'SocketFamily'
    ExplicitTop = 318
  end
  object Label6: TLabel
    Left = 132
    Top = 384
    Width = 157
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(Max - Min time out after 60 sec)'
    ExplicitTop = 298
  end
  object StartButton: TButton
    Left = 223
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start'
    TabOrder = 6
    OnClick = StartButtonClick
    ExplicitTop = 332
  end
  object DnsNamesMemo: TMemo
    Left = 8
    Top = 8
    Width = 168
    Height = 326
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'DnsNamesMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object ResultMemo: TMemo
    Left = 190
    Top = 8
    Width = 165
    Height = 326
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'ResultMemo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object MinEdit: TEdit
    Left = 40
    Top = 381
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '0'
    ExplicitTop = 295
  end
  object MaxEdit: TEdit
    Left = 105
    Top = 381
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Text = '2'
    ExplicitTop = 295
  end
  object InstancesEdit: TEdit
    Left = 159
    Top = 342
    Width = 36
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '3'
    ExplicitTop = 256
  end
  object SocketFamilyComboBox: TComboBox
    Left = 8
    Top = 420
    Width = 157
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 5
    Text = 'sfAny'
    OnChange = SocketFamilyComboBoxChange
    Items.Strings = (
      'sfAny'
      'sfAnyIPv4'
      'sfAnyIPv6'
      'sfIPv4 (old API)'
      'sfIPv6')
  end
  object UseThread: TCheckBox
    Left = 210
    Top = 345
    Width = 131
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use Thread (new API)'
    TabOrder = 7
  end
end
