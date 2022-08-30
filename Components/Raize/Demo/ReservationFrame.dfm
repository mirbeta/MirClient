object FmeReservations: TFmeReservations
  Left = 0
  Top = 0
  Width = 508
  Height = 559
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object RzPanel1: TRzPanel
    Left = 0
    Top = 36
    Width = 453
    Height = 469
    BorderOuter = fsNone
    ParentColor = True
    TabOrder = 0
    OnPaint = RzPanel1Paint
    object pnlTicket: TRzPanel
      Left = 8
      Top = 188
      Width = 433
      Height = 249
      BorderOuter = fsFlat
      Color = 11855600
      FlatColor = 5607865
      FlatColorAdjustment = 0
      TabOrder = 0
      VisualStyle = vsClassic
      object RzLabel9: TRzLabel
        Left = 18
        Top = 52
        Width = 46
        Height = 14
        Caption = 'Customer'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
        BorderOuter = fsFlat
        BorderSides = [sdBottom]
        BorderHighlight = clWhite
        BorderShadow = 6080734
        FlatColor = 5607865
        FlatColorAdjustment = 0
        FrameController = RzFrameController1
      end
      object RzLabel4: TRzLabel
        Left = 18
        Top = 85
        Width = 68
        Height = 13
        Caption = 'Event Number'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel1: TRzLabel
        Left = 338
        Top = 85
        Width = 33
        Height = 13
        Caption = 'Tickets'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel5: TRzLabel
        Left = 138
        Top = 128
        Width = 95
        Height = 13
        Caption = 'Credit Card Number'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel6: TRzLabel
        Left = 18
        Top = 128
        Width = 94
        Height = 13
        Caption = 'Method of Payment'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel10: TRzLabel
        Left = 310
        Top = 128
        Width = 74
        Height = 13
        Caption = 'Expiration Date'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel11: TRzLabel
        Left = 18
        Top = 172
        Width = 60
        Height = 13
        Caption = 'Amount Paid'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object RzLabel12: TRzLabel
        Left = 138
        Top = 200
        Width = 73
        Height = 13
        Caption = 'Payment Notes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object stsEventName: TRzDBStatusPane
        Left = 136
        Top = 64
        Width = 189
        Height = 22
        Hint = 'EventName'
        FlatColor = 5607865
        FlatColorAdjustment = 0
        Transparent = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        DataField = 'EventName'
        DataSource = srcReservations
      end
      object RzLabel2: TRzLabel
        Left = 16
        Top = 4
        Width = 212
        Height = 25
        Caption = 'Ticket Order Form'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -21
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RzLabel3: TRzLabel
        Left = 142
        Top = 85
        Width = 58
        Height = 13
        Caption = 'Event Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        BevelWidth = 0
      end
      object edtCustomer: TRzDBButtonEdit
        Left = 16
        Top = 32
        Width = 397
        Height = 21
        DataSource = srcReservations
        DataField = 'CustName'
        Ctl3D = True
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 0
        ButtonKind = bkFind
        AltBtnWidth = 15
        ButtonWidth = 15
        FlatButtonColor = 9229030
        OnButtonClick = edtCustomerButtonClick
      end
      object spnTickets: TRzDBSpinEdit
        Left = 336
        Top = 64
        Width = 77
        Height = 21
        DataSource = srcReservations
        DataField = 'NumTickets'
        Ctl3D = True
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 2
        FlatButtonColor = 9229030
        Max = 100.000000000000000000
      end
      object edtEvent: TRzDBButtonEdit
        Left = 16
        Top = 64
        Width = 113
        Height = 21
        DataSource = srcReservations
        DataField = 'EventNo'
        Ctl3D = True
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        ButtonHint = 'Search by Name'
        ButtonKind = bkFind
        AltBtnWidth = 15
        ButtonWidth = 15
        FlatButtonColor = 9229030
        OnButtonClick = edtEventButtonClick
      end
      object edtNotes: TRzDBMemo
        Left = 136
        Top = 152
        Width = 277
        Height = 49
        Ctl3D = True
        DataField = 'Pay_Notes'
        DataSource = srcReservations
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 8
        FrameControllerNotifications = [fcpColor, fcpFocusColor, fcpDisabledColor, fcpReadOnlyColor, fcpReadOnlyColorOnFocus, fcpParentColor, fcpFlatButtonColor, fcpFlatButtons, fcpFrameColor, fcpFrameHotColor, fcpFrameHotTrack, fcpFrameHotStyle, fcpFrameStyle, fcpFrameVisible, fcpFramingPreference]
        FrameController = RzFrameController1
        FrameSides = [sdLeft, sdBottom]
      end
      object edtAmount: TRzDBEdit
        Left = 16
        Top = 152
        Width = 109
        Height = 21
        DataSource = srcReservations
        DataField = 'Amt_Paid'
        Ctl3D = True
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 6
      end
      object chkPaid: TRzDBCheckBox
        Left = 16
        Top = 198
        Width = 108
        Height = 15
        DataField = 'Paid'
        DataSource = srcReservations
        ValueChecked = 'True'
        ValueUnchecked = 'False'
        Caption = 'Payment Received'
        FocusColor = 13824245
        FrameColor = 5607865
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 7879740
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        FrameController = RzFrameController1
        HighlightColor = 4611970
        HotTrack = True
        HotTrackColor = 7256535
        ParentFont = False
        TabOrder = 7
      end
      object edtCardNumber: TRzDBEdit
        Left = 136
        Top = 108
        Width = 161
        Height = 21
        DataSource = srcReservations
        DataField = 'Card_No'
        Ctl3D = True
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 4
      end
      object cbxMethod: TRzDBComboBox
        Left = 16
        Top = 108
        Width = 113
        Height = 21
        DataField = 'Pay_Method'
        DataSource = srcReservations
        Ctl3D = False
        FrameController = RzFrameController1
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 3
        Items.Strings = (
          'American Express'
          'Cash/Check'
          'Diners Club'
          'MasterCard'
          'Visa'
          'Will Call')
        Values.Strings = (
          'AMEX'
          'CASH/CK'
          'DINERS'
          'MC'
          'VISA'
          'WILL CALL')
      end
      object edtExpirationDate: TRzDBDateTimeEdit
        Left = 308
        Top = 108
        Width = 105
        Height = 21
        DataSource = srcReservations
        DataField = 'Card_Exp'
        FrameController = RzFrameController1
        ParentColor = True
        TabOrder = 5
        EditType = etDate
        PopupHeight = 0
        PopupWidth = 0
      end
      object RzStatusBar1: TRzStatusBar
        Left = 1
        Top = 229
        Width = 431
        Height = 19
        AutoStyle = False
        ShowSizeGrip = False
        BorderInner = fsNone
        BorderOuter = fsNone
        BorderSides = [sdLeft, sdTop, sdRight, sdBottom]
        BorderWidth = 0
        GradientColorStyle = gcsCustom
        GradientColorStart = 13824245
        GradientColorStop = 9229030
        ParentColor = True
        TabOrder = 9
        VisualStyle = vsGradient
        object RzDBStateStatus1: TRzDBStateStatus
          Left = 158
          Top = 0
          Width = 104
          Height = 19
          FillColor = 9229030
          FlatColor = 7256535
          FlatColorAdjustment = 0
          ParentFillColor = False
          Align = alLeft
          DataSource = srcReservations
        end
        object stsResNo: TRzDBStatusPane
          Left = 0
          Top = 0
          Width = 158
          Height = 19
          Hint = 'ResNo'
          FillColor = 9229030
          FlatColor = 7256535
          FlatColorAdjustment = 0
          ParentFillColor = False
          Align = alLeft
          ParentShowHint = False
          ShowHint = False
          FieldLabel = 'Order Number'
          FieldLabelColor = 5607865
          Alignment = taRightJustify
          DataField = 'ResNo'
          DataSource = srcReservations
        end
        object RzClockStatus: TRzClockStatus
          Left = 262
          Top = 0
          Width = 169
          Height = 19
          FillColor = 9229030
          FlatColor = 7256535
          FlatColorAdjustment = 0
          ParentFillColor = False
          Align = alClient
          Format = 'dd mmmm yyyy'
          Alignment = taCenter
        end
      end
    end
    object RzPanel2: TRzPanel
      Left = 8
      Top = 5
      Width = 432
      Height = 35
      BorderOuter = fsFlat
      Color = 11855600
      FlatColor = 5607865
      TabOrder = 1
      object navReservations: TRzDBNavigator
        Left = 12
        Top = 5
        Width = 410
        Height = 25
        ButtonVisualStyle = vsGradient
        DataSource = srcReservations
        BorderOuter = fsNone
        Color = 11855600
        TabOrder = 0
      end
    end
    object grdReservations: TRzDBGrid
      Left = 8
      Top = 56
      Width = 433
      Height = 111
      Color = 14480890
      DataSource = srcReservations
      DefaultDrawing = True
      FixedColor = 11855600
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Verdana'
      TitleFont.Style = []
      FrameColor = 5607865
      FrameVisible = True
      FramingPreference = fpCustomFraming
      FixedLineColor = 5607865
      LineColor = 11855600
      Columns = <
        item
          Expanded = False
          FieldName = 'ResNo'
          Title.Caption = 'Order #'
          Width = 50
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'CustName'
          Title.Caption = 'Customer'
          Width = 150
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'EventName'
          Title.Caption = 'Event Name'
          Width = 150
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'NumTickets'
          Title.Caption = 'Tickets'
          Width = 45
          Visible = True
        end>
    end
  end
  object pnlHeader: TRzPanel
    Left = 0
    Top = 0
    Width = 508
    Height = 35
    Align = alTop
    Alignment = taLeftJustify
    BorderOuter = fsFlat
    BorderSides = [sdBottom]
    Caption = 'Data-Aware Controls'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9856100
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    GradientColorStop = 11855600
    TextMargin = 4
    ParentFont = False
    TabOrder = 1
    VisualStyle = vsGradient
    WordWrap = False
  end
  object srcReservations: TDataSource
    DataSet = cdsReservations
    Left = 40
    Top = 360
  end
  object srcEvents: TDataSource
    DataSet = cdsEvents
    Left = 116
    Top = 360
  end
  object srcCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 184
    Top = 360
  end
  object dlgLookupEvent: TRzDBLookupDialog
    Dataset = cdsEventLookup
    GridOptions = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    KeyField = 'EventNo'
    SearchEdit = edtEvent
    SearchField = 'Event_Name'
    ShowNavigatorHints = True
    UpdateSearchEdit = True
    OnDrawDataCell = dlgLookupEventDrawDataCell
    Caption = 'Select an Event'
    CaptionOK = 'OK'
    CaptionCancel = 'Cancel'
    CaptionHelp = '&Help'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Height = 303
    Width = 558
    Left = 236
    Top = 360
  end
  object RzFrameController1: TRzFrameController
    FlatButtonColor = 9229030
    FocusColor = 13824245
    FrameColor = 5607865
    FrameHotStyle = fsGroove
    FrameSides = [sdBottom]
    FrameVisible = True
    FramingPreference = fpCustomFraming
    ParentColor = True
    Left = 320
    Top = 360
  end
  object cdsReservations: TClientDataSet
    Aggregates = <>
    FileName = 'Reservations.cds'
    Params = <>
    OnCalcFields = cdsReservationsCalcFields
    Left = 12
    Top = 360
    object cdsReservationsResNo: TAutoIncField
      FieldName = 'ResNo'
      ReadOnly = True
    end
    object cdsReservationsEventNo: TIntegerField
      FieldName = 'EventNo'
    end
    object cdsReservationsCustNo: TIntegerField
      FieldName = 'CustNo'
    end
    object cdsReservationsNumTickets: TIntegerField
      FieldName = 'NumTickets'
    end
    object cdsReservationsAmt_Paid: TCurrencyField
      FieldName = 'Amt_Paid'
    end
    object cdsReservationsPay_Method: TStringField
      FieldName = 'Pay_Method'
      Size = 10
    end
    object cdsReservationsCard_No: TStringField
      FieldName = 'Card_No'
    end
    object cdsReservationsCard_Exp: TDateField
      FieldName = 'Card_Exp'
    end
    object cdsReservationsPay_Notes: TMemoField
      FieldName = 'Pay_Notes'
      BlobType = ftMemo
      Size = 30
    end
    object cdsReservationsPurge_Date: TDateField
      FieldName = 'Purge_Date'
    end
    object cdsReservationsPaid: TBooleanField
      FieldName = 'Paid'
    end
    object cdsReservationsCustName: TStringField
      FieldKind = fkCalculated
      FieldName = 'CustName'
      ProviderFlags = []
      Size = 40
      Calculated = True
    end
    object cdsReservationsEventName: TStringField
      FieldKind = fkCalculated
      FieldName = 'EventName'
      ProviderFlags = []
      Size = 40
      Calculated = True
    end
  end
  object cdsEvents: TClientDataSet
    Aggregates = <>
    FileName = 'Events.cds'
    Params = <>
    Left = 88
    Top = 360
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    FileName = 'Customers.cds'
    Params = <>
    Left = 156
    Top = 360
  end
  object cdsEventLookup: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 264
    Top = 360
    object cdsEventLookupEventNo: TAutoIncField
      FieldName = 'EventNo'
      ReadOnly = True
    end
    object cdsEventLookupVenueNo: TIntegerField
      FieldName = 'VenueNo'
    end
    object cdsEventLookupEvent_Name: TStringField
      FieldName = 'Event_Name'
      Size = 30
    end
    object cdsEventLookupEvent_Date: TDateField
      FieldName = 'Event_Date'
    end
    object cdsEventLookupEvent_Time: TTimeField
      FieldName = 'Event_Time'
    end
    object cdsEventLookupTicket_price: TCurrencyField
      FieldName = 'Ticket_price'
    end
  end
end
