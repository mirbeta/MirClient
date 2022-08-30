object frmRentCar: TfrmRentCar
  Left = 252
  Top = 150
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Rent a car '
  ClientHeight = 525
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbChoosePeriod: TLabel
    Left = 10
    Top = 387
    Width = 146
    Height = 13
    Caption = '4. Choose the available period:'
  end
  object lbChooseDate: TLabel
    Left = 10
    Top = 225
    Width = 93
    Height = 13
    Caption = '3. Choose the date:'
  end
  object lbChooseCar: TLabel
    Left = 10
    Top = 56
    Width = 100
    Height = 13
    Caption = '2. Choose the model:'
  end
  object lbCustomerName: TLabel
    Left = 10
    Top = 8
    Width = 140
    Height = 13
    Caption = '1. Enter the customer'#39's name:'
    FocusControl = edtUserName
  end
  object lbChooseTime: TLabel
    Left = 600
    Top = 9
    Width = 88
    Height = 13
    Caption = '4.Choose the time:'
  end
  object lbxPeriod: TcxListBox
    Left = 10
    Top = 406
    Width = 580
    Height = 81
    ItemHeight = 13
    Style.BorderColor = clBtnShadow
    Style.BorderStyle = cbsSingle
    Style.HotTrack = False
    TabOrder = 3
    OnClick = lbxPeriodClick
  end
  object DateNavigator: TcxDateNavigator
    Left = 10
    Top = 244
    Width = 580
    Height = 135
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    SelectPeriod = False
    ShowDatesContainingEventsInBold = False
    Styles.Background = cxBoldStyle
    TabOrder = 2
    OnCustomDrawContent = DateNavigatorCustomDrawContent
    OnCustomDrawDayNumber = DateNavigatorCustomDrawDayNumber
    OnSelectionChanged = DateNavigatorSelectionChanged
  end
  object lvCars: TListView
    Left = 10
    Top = 76
    Width = 580
    Height = 143
    Columns = <>
    HotTrackStyles = [htUnderlineCold, htUnderlineHot]
    Items.ItemData = {
      03100100000400000001000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      001242004D00570020002D0020005A0034002000530064007200690076006500
      33003500690002000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000001A43
      0068006500760072006F006C006500740020002D00200043006F007200760065
      00740074006500200043006F0075007000650003000000FFFFFFFFFFFFFFFF00
      000000FFFFFFFF000000001144006F0064006700650020002D00200053005200
      540020005600690070006500720004000000FFFFFFFFFFFFFFFF00000000FFFF
      FFFF000000001746006F007200640020002D0020004D0075007300740061006E
      006700200047005400200043006F00750070006500}
    LargeImages = ResourceDemoMainForm.imgCars
    RowSelect = True
    TabOrder = 1
    OnSelectItem = lvCarsSelectItem
  end
  object edtUserName: TcxTextEdit
    Left = 10
    Top = 28
    TabOrder = 0
    Text = 'Anonymous'
    Width = 580
  end
  object TimeScheduler: TcxScheduler
    Left = 600
    Top = 29
    Width = 153
    Height = 457
    DateNavigator.ColCount = 4
    DateNavigator.Visible = False
    ViewDay.Active = True
    ViewDay.TimeRulerPopupMenu.Items = [rpmi60min, rpmi30min, rpmi15min, rpmi10min, rpmi6min, rpmi5min]
    ViewDay.TimeScale = 60
    OnCustomDrawContent = TimeSchedulerCustomDrawContent
    ContentPopupMenu.Items = []
    ControlBox.Visible = False
    EventOperations.DialogShowing = False
    EventOperations.ReadOnly = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OptionsView.WorkDays = [dSunday, dMonday, dTuesday, dWednesday, dThursday, dFriday, dSaturday]
    OptionsView.WorkFinish = 0.999305555555555600
    TabOrder = 4
    OnKeyDown = TimeSchedulerKeyDown
    Splitters = {
      090000007E00000098000000830000009CFEFFFF01000000A1FEFFFFCA010000}
    StoredClientBounds = {010000000100000098000000C8010000}
  end
  object btnRent: TcxButton
    Left = 494
    Top = 493
    Width = 120
    Height = 25
    Caption = 'Rent'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = btnRentClick
  end
  object btnCancel: TcxButton
    Left = 634
    Top = 493
    Width = 120
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 6
  end
  object cxStyleRepository: TcxStyleRepository
    Left = 168
    Top = 80
    PixelsPerInch = 96
    object cxBoldStyle: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
  end
end
