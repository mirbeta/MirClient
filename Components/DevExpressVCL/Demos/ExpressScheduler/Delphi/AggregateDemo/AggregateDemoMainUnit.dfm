inherited AggregateDemoMainForm: TAggregateDemoMainForm
  Width = 770
  Height = 665
  Caption = 'ExpressScheduler AggregateDemo '
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 762
    Caption = 
      'This demo shows how events can be scheduled in the scheduler, us' +
      'ing the bound and unbound storage simultaneously. Click '#39'About ' +
      'this demo'#39' for more information.'
  end
  inherited Scheduler: TcxScheduler
    Width = 762
    Height = 440
    Storage = SchedulerAggregateStorage
    Splitters = {
      6A020000FB000000F90200000001000065020000010000006A020000B7010000}
    StoredClientBounds = {0100000001000000F9020000B7010000}
    inherited pnlControls: TPanel
      Height = 183
      inherited Memo1: TMemo
        Height = 183
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 143
        Height = 183
        Align = alClient
        TabOrder = 1
        object cxButton1: TcxButton
          Left = 8
          Top = 8
          Width = 127
          Height = 44
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Generate 500 events'#13#10'in DB storage'
          TabOrder = 0
          OnClick = cxButton1Click
        end
        object cxButton2: TcxButton
          Left = 8
          Top = 60
          Width = 127
          Height = 44
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Generate 500 events'#13#10'in Unbound storage'
          TabOrder = 1
          OnClick = cxButton2Click
        end
      end
    end
  end
  inherited StatusBar: TStatusBar
    Top = 592
    Width = 762
    SimplePanel = False
  end
  object DBGrid1: TDBGrid [3]
    Left = 0
    Top = 472
    Width = 762
    Height = 120
    Align = alBottom
    DataSource = SchedulerDataSource
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Caption'
        Width = 500
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Start'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Finish'
        Visible = True
      end>
  end
  object SchedulerDataSource: TDataSource
    DataSet = mdEvents
    Left = 248
    Top = 80
  end
  object SchedulerAggregateStorage: TcxSchedulerAggregateStorage
    Resources.Items = <>
    Links = <
      item
        Default = True
        Storage = SchedulerDBStorage
      end
      item
        Storage = SchedulerStorage
      end>
    OnEventInserting = SchedulerAggregateStorageEventInserting
    Left = 248
    Top = 144
  end
  object SchedulerStorage: TcxSchedulerStorage
    CustomFields = <>
    Reminders.Active = False
    Resources.Items = <>
    Left = 248
    Top = 112
  end
  object SchedulerDBStorage: TcxSchedulerDBStorage
    Reminders.Active = False
    Resources.Items = <>
    CustomFields = <>
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
    Left = 312
    Top = 80
  end
  object mdEvents: TdxMemData
    Active = True
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
    Left = 344
    Top = 144
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
