inherited StylesMainForm: TStylesMainForm
  Caption = 'ExpressScheduler StylesDemo'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo shows how styles (using colors and images) can be used' +
      ' to change or enhance the visual appearance of the scheduler. Cl' +
      'ick '#39'About this demo'#39' for more information.'
  end
  inherited Scheduler: TcxScheduler
    DateNavigator.RowCount = 3
    DateNavigator.Styles.Background = stBackground
    DateNavigator.Styles.Content = stDateContent
    DateNavigator.Styles.Header = stHeaders
    ViewDay.Styles.HeaderContainer = stContainer
    ViewDay.Styles.TimeRuler = stTimeRuler
    ControlBox.Visible = False
    OptionsView.VertSplitterWidth = 50
    Storage = SchedulerDBStorage
    Styles.Background = stBackground
    Styles.Content = stContent
    Styles.Event = stEvents
    Styles.GroupSeparator = stGroupSeparator
    Styles.DayHeader = stHeaders
    Styles.ResourceHeader = stResources
    Styles.Selection = stContentSelection
    Styles.VertSplitter = stVertSplitter
    Splitters = {
      30020000FB000000BF02000000010000F20100000100000024020000BC010000}
    inherited pnlControls: TPanel
      Height = 243
      inherited Memo1: TMemo
        Height = 243
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 416
    Top = 0
    inherited miView: TMenuItem
      inherited miControlBox: TMenuItem
        Checked = False
      end
    end
    object CustomDraw1: TMenuItem [2]
      Caption = '&Styles'
      object Events1: TMenuItem
        Caption = 'Events'
        Checked = True
        OnClick = miStylesItemClick
      end
      object Headers1: TMenuItem
        Tag = 1
        Caption = 'Headers'
        Checked = True
        OnClick = miStylesItemClick
      end
      object Content1: TMenuItem
        Tag = 2
        Caption = 'Content'
        Checked = True
        OnClick = miStylesItemClick
      end
      object Contentselection1: TMenuItem
        Tag = 3
        Caption = 'Content selection'
        Checked = True
        OnClick = miStylesItemClick
      end
      object miResourcesStyle: TMenuItem
        Tag = 4
        Caption = 'Resources'
        Checked = True
        OnClick = miStylesItemClick
      end
      object Groupseparator1: TMenuItem
        Tag = 5
        Caption = 'Group separator'
        Checked = True
        OnClick = miStylesItemClick
      end
      object miSplitter: TMenuItem
        Caption = 'Splitter'
        Checked = True
        OnClick = miSplitterClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object ViewDay1: TMenuItem
        Caption = 'ViewDay'
        object Container1: TMenuItem
          Tag = 6
          Caption = 'Container'
          Checked = True
          OnClick = miStylesItemClick
        end
        object imeRuler1: TMenuItem
          Tag = 7
          Caption = 'Time Ruler'
          Checked = True
          OnClick = miStylesItemClick
        end
      end
      object DateNavigator1: TMenuItem
        Caption = 'DateNavigator'
        object Daycaptions1: TMenuItem
          Tag = 9
          Caption = 'Background'
          Checked = True
          OnClick = miStylesItemClick
        end
        object Days1: TMenuItem
          Tag = 10
          Caption = 'Content'
          Checked = True
          OnClick = miStylesItemClick
        end
        object Monthheaders1: TMenuItem
          Tag = 8
          Caption = 'Month headers'
          Checked = True
          OnClick = miStylesItemClick
        end
      end
    end
  end
  inherited Timer1: TTimer
    Left = 448
    Top = 0
  end
  inherited SaveDialog: TSaveDialog
    Left = 480
    Top = 0
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 272
    Top = 168
    PixelsPerInch = 96
    object stEvents: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13827327
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = 4210816
    end
    object stHeaders: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15655131
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      TextColor = 10771219
    end
    object stContent: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = 15920618
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object stContentSelection: TcxStyle
      AssignedValues = [svColor]
      Color = 15838827
    end
    object stResources: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15726065
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = 8388672
    end
    object stGroupSeparator: TcxStyle
      AssignedValues = [svColor]
      Color = 14342349
    end
    object stContainer: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14862294
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clMaroon
    end
    object stBackground: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = 12905700
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object stDateContent: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14546160
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = 8022057
    end
    object stVertSplitter: TcxStyle
      AssignedValues = [svBitmap]
      Bitmap.Data = {
        0E0F0000424D0E0F000000000000360000002800000032000000190000000100
        180000000000D80E0000120B0000120B00000000000000000000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87B9AA21F32383B
        6069446F7A4876814A7883406A7499C0CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87B9AA21F32383B5F68436E7947747F
        4874803E666F99C0CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE87B9AA21F3238395D66416A74436E79436D78395E6798BF
        C9B0DCE8AFDBE7AFDBE7AFDBE7AFDBE7AFDBE7AFDBE7B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E87B9AA21F323835575F3B616B3D646F3D636D35575F97BDC7ADD8E3ADD8E3AD
        D8E3ADD9E4ADD9E4AED9E5AFDAE6AFDBE7AFDBE7B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87A99A11F313731
        5058375A633A5F693C616B35586090B5BEA4CDD9A4CDD9A4CDD9A5CED9A6D0DB
        A8D2DDAAD4E0ADD8E3AFDAE6AFDBE7B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8AFDBE777959D1D2F35304E55385B643C636D
        4068723A5F6884A6AE96BBC596BCC697BDC798BEC899C0CA9DC4CFA2CAD5A8D2
        DDACD7E3AED9E5AFDBE7B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8AFDBE7ADD9E4708C931B2B3133525A3D636D426C7746717D3E67707C9B
        A38EB2BB8EB2BB8FB3BC8FB3BC90B3BD92B7C199BFC99FC7D2A6D0DBABD6E2AE
        D9E5AFDBE7B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8AFDBE7AED9E5ACD7E3A7D1
        DC67818819292E375A62416B7546737F497782406A747FA0A894B9C393B8C292
        B7C191B5BF8EB2BB8DB1BB90B4BE96BBC59DC5CFA6CFDAACD7E3AFDAE6B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8AFDAE6ADD8E3A8D3DEA3CCD79BC1CC647D841A2A2F3A
        5E67436E794876814A7884416B758BAFB8A2CAD5A1C9D49FC6D19BC1CC94B9C4
        90B3BD8DB1BB90B3BD96BCC6A0C8D3A9D3DFAED9E5AFDBE7B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8AFDBE7AF
        DAE6ABD6E2A6CFDA9EC6D096BBC590B4BE6A848B1C2D323B6069446F7A487682
        4A7884416B7595BBC4ACD7E3ABD6E2A9D3DFA5CED99FC7D298BEC892B6C08FB3
        BC91B5BF9AC0CBA4CDD9ACD7E3AFDBE7B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8AFDBE7AED9E5AAD4E0A4CCD89BC1CC
        93B8C28FB3BC91B5BF728F971E30363B6069446F7A4876824A7884416B7598BF
        C9AFDBE7AFDAE6AED9E5ACD7E3A8D2DDA2CAD59AC0CB92B6C090B3BD95BAC5A0
        C8D3AAD5E1AFDAE6B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8AFDBE7AED9E5A9D3DFA2CAD599BFC990B4BE8EB2BB92B7C19BC1
        CC78969E1F32383B6069446F7A4876824A7884416B7599C0CAB0DCE8B0DCE8AF
        DBE7AFDAE6ADD8E3A8D3DEA0C8D396BBC58FB3BC91B5BF9DC4CFA8D3DEAFDAE6
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8AFDBE7AED9
        E5A9D3DFA2CAD598BEC890B4BE8EB2BB93B8C29CC3CEA5CED97B9AA21F32383B
        6069446F7A4876824A7884416B7599C0CAB0DCE8B0DCE8B0DCE8B0DCE8AFDAE6
        ACD7E3A4CDD999BFC98FB3BC90B4BE9BC2CDA8D2DDAED9E5B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE84C6A71547982537B85A1C9D497BDC790
        B3BD8EB2BB94B9C39DC5CFA7D1DCADD8E37B9AA21F32383B6069446F7A487682
        4A7884416B7599C0CAB0DCE8B0DCE8B0DCE8B0DCE8AFDBE7ADD9E4A6CFDA99C0
        CA8FB3BC2C484E395E67426D77AED9E5B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE837454A4F6A7160858F638A945E8791557B85476A738EB2BB94B9C39DC5CF
        A6D0DBADD8E3AFDBE77B9AA21F32383B6069446F7A4876824A7884416B7599C0
        CAB0DCE8B0DCE8B0DCE8B0DCE8AFDBE7ADD9E4A6D0DB18262A284148375B6441
        697447758249778244707BB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE845565B62838B
        6A8E975E80894646464646464646464646464646464646464646464646464646
        4646464646464646464646464646464646464646464646464646464646464646
        46464646464646464646464646464646464646464646464646464979844B7B87
        46737EB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8323A3B5A757C67858D6380870000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000002B464E4A788344707B3D636DB0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8323A3B59727868838A5461643131313939394242423E3E3E3A
        3A3A3D3D3D404040434343454545474847474747464647454645444445434444
        4243434242424141423D3E3E3A3B3A3737373333332E2E2F2A2A2B2525262020
        200E0E0F0101000000000C131544707B406872385C65B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE82E
        3434546A70607A807580847474748E8E8E9E9E9EA3A3A3A5A5A5AEAFAFB9B9B9
        C2C2C3C9CACAD0D0D1D0D0D0CFCFD0CDCFCFCDCDCDCCCCCDCBCCCCCACBCBC8C8
        C8C0C0C0B7B7B8ACACADA0A1A19394948485857777776667684646462222220B
        0B0B090F103D636D395D66304F56B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8363E40546A70
        61777C898D8E949494A5A5A5B6B6B6C4C4C4D2D2D2DFDFDFEBEBEBF5F5F5FDFD
        FDFEFEFEFEFEFEFEFEFEFEFEFEFEFEFEFDFDFEFCFCFDF8F9F9EEEEEFE2E3E3D5
        D6D7C5C6C7B5B6B6A4A5A59395958282836B6B6C4B4B4C27272725383E34555E
        2C474EB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8272A2C333B3E495F654A6167465E
        6436484C29383DB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87B9AA21F32383B
        6069446F7A4876824A7884416B7599C0CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8080C0E101A1D2842492B464D2C474E2034391A2A2FB0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8262C2E273031252E30B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87B9AA21F32383B6069446F7A487682
        4A7884416B7599C0CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE80B12140E181A101A1DB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE80000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE87B9AA21F32383B6069446F7A4876824A7884416B7599C0
        CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        0000B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E87B9AA21F32383B6069446F7A4876824A7884416B7599C0CAB0DCE8B0DCE8B0
        DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE87B9AA21F32383B
        6069446F7A4876824A7884416B7599C0CAB0DCE8B0DCE8B0DCE8B0DCE8B0DCE8
        B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE8B0DC
        E8B0DCE8B0DCE8B0DCE8B0DCE8B0DCE80000}
    end
    object stTimeRuler: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13429481
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = 27242
    end
  end
  object SchedulerDataSource: TDataSource
    DataSet = mdEvents
    Left = 248
    Top = 80
  end
  object SchedulerDBStorage: TcxSchedulerDBStorage
    UseActualTimeRange = True
    Resources.Items = <
      item
        Name = 'Sinan Demir'
        ResourceID = 0
      end
      item
        Name = 'Bastian Bauwens'
        ResourceID = 1
      end
      item
        Name = 'Oliver Sturm'
        ResourceID = 2
      end>
    SmartRefresh = True
    CustomFields = <
      item
        FieldName = 'SyncIDField'
      end>
    DataSource = SchedulerDataSource
    FieldNames.ActualFinish = 'ActualFinish'
    FieldNames.ActualStart = 'ActualStart'
    FieldNames.Caption = 'Caption'
    FieldNames.EventType = 'Type'
    FieldNames.Finish = 'Finish'
    FieldNames.ID = 'ID'
    FieldNames.LabelColor = 'LabelColor'
    FieldNames.Location = 'Location'
    FieldNames.Message = 'Message'
    FieldNames.Options = 'Options'
    FieldNames.ParentID = 'ParentID'
    FieldNames.RecurrenceIndex = 'RecurrenceIndex'
    FieldNames.RecurrenceInfo = 'RecurrenceInfo'
    FieldNames.ReminderDate = 'ReminderDate'
    FieldNames.ReminderMinutesBeforeStart = 'ReminderMinutes'
    FieldNames.ResourceID = 'ResourceID'
    FieldNames.Start = 'Start'
    FieldNames.State = 'State'
    Left = 216
    Top = 80
  end
  object mdEvents: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F13000000040000000C000300494400040000000300
      0900506172656E7449440004000000030005005479706500080000000B000600
      537461727400080000000B00070046696E6973680004000000030008004F7074
      696F6E7300FF0000000100080043617074696F6E000400000003001000526563
      757272656E6365496E64657800000000000D000F00526563757272656E636549
      6E666F00000000000D000B005265736F75726365494400FF000000010009004C
      6F636174696F6E00FF000000010008004D65737361676500080000000B000D00
      52656D696E6465724461746500040000000300100052656D696E6465724D696E
      757465730004000000030006005374617465000400000003000B004C6162656C
      436F6C6F7200080000000B000C0041637475616C537461727400080000000B00
      0D0041637475616C46696E69736800FF00000001000C0053796E634944466965
      6C6400}
    SortOptions = []
    Left = 280
    Top = 80
    object mdEventsID: TAutoIncField
      FieldName = 'ID'
    end
    object mdEventsParentID: TIntegerField
      FieldName = 'ParentID'
    end
    object mdEventsType: TIntegerField
      FieldName = 'Type'
    end
    object mdEventsStart: TDateTimeField
      FieldName = 'Start'
    end
    object mdEventsFinish: TDateTimeField
      FieldName = 'Finish'
    end
    object mdEventsOptions: TIntegerField
      FieldName = 'Options'
    end
    object mdEventsCaption: TStringField
      FieldName = 'Caption'
      Size = 255
    end
    object mdEventsRecurrenceIndex: TIntegerField
      FieldName = 'RecurrenceIndex'
    end
    object mdEventsRecurrenceInfo: TBlobField
      FieldName = 'RecurrenceInfo'
    end
    object mdEventsResourceID: TBlobField
      FieldName = 'ResourceID'
    end
    object mdEventsLocation: TStringField
      FieldName = 'Location'
      Size = 255
    end
    object mdEventsMessage: TStringField
      FieldName = 'Message'
      Size = 255
    end
    object mdEventsReminderDate: TDateTimeField
      FieldName = 'ReminderDate'
    end
    object mdEventsReminderMinutes: TIntegerField
      FieldName = 'ReminderMinutes'
    end
    object mdEventsState: TIntegerField
      FieldName = 'State'
    end
    object mdEventsLabelColor: TIntegerField
      FieldName = 'LabelColor'
    end
    object mdEventsActualStart: TDateTimeField
      FieldName = 'ActualStart'
    end
    object mdEventsActualFinish: TDateTimeField
      FieldName = 'ActualFinish'
    end
    object mdEventsSyncIDField: TStringField
      FieldName = 'SyncIDField'
      Size = 255
    end
  end
end
