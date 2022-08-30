object frmCancelReservation: TfrmCancelReservation
  Left = 300
  Top = 150
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Cancel reservation'
  ClientHeight = 220
  ClientWidth = 437
  Color = clBtnFace
  Constraints.MinHeight = 254
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbxCustomers: TcxListBox
    Left = 11
    Top = 24
    Width = 165
    Height = 158
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    Style.BorderColor = clBtnShadow
    Style.BorderStyle = cbsSingle
    Style.HotTrack = False
    TabOrder = 0
    OnClick = lbxCustomersClick
  end
  object btnCancelReserv: TcxButton
    Left = 175
    Top = 189
    Width = 120
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel reservation'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnCancelReservClick
  end
  object btnClose: TcxButton
    Left = 307
    Top = 189
    Width = 120
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 3
  end
  object lbxEvents: TcxListBox
    Left = 184
    Top = 24
    Width = 243
    Height = 158
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    Style.BorderColor = clBtnShadow
    Style.BorderStyle = cbsSingle
    Style.Color = clWindow
    Style.HotTrack = False
    TabOrder = 1
    OnClick = lbxEventsClick
  end
  object lbSelectCustomer: TcxLabel
    Left = 11
    Top = 5
    Caption = '1. Select customer:'
    FocusControl = lbxCustomers
  end
  object lbCancelReservation: TcxLabel
    Left = 184
    Top = 5
    Caption = '2. Select reservation:'
  end
end
