inherited DBDemoMainForm: TDBDemoMainForm
  Width = 658
  Height = 505
  Caption = 'ExpressScheduler DBDemo '
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 650
    Caption = 
      'This demo shows how the scheduler works when it'#39's connected to a' +
      ' DataSource. Click '#39'About this demo'#39' for more information.'
  end
  inherited Scheduler: TcxScheduler
    Width = 650
    Height = 400
    Storage = SchedulerDBStorage
    Splitters = {
      D0010000FB0000008902000000010000CB01000001000000D00100008F010000}
    StoredClientBounds = {0100000001000000890200008F010000}
    inherited pnlControls: TPanel
      Width = 185
      Height = 143
      inherited Memo1: TMemo
        Width = 185
        Height = 143
        TabOrder = 1
        Visible = False
      end
      object cxGroupBox1: TcxGroupBox
        Left = 0
        Top = 0
        Align = alClient
        ParentColor = False
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 0
        Height = 143
        Width = 185
        object cxButton1: TcxButton
          Left = 9
          Top = 13
          Width = 122
          Height = 22
          Caption = 'Generate 5000 events'
          TabOrder = 0
          OnClick = cxButton1Click
        end
        object cxCheckBox1: TcxCheckBox
          Left = 9
          Top = 38
          Caption = 'Smart refresh'
          TabOrder = 1
          OnClick = chDataModeClick
          Width = 121
        end
      end
    end
  end
  inherited StatusBar: TStatusBar
    Top = 432
    Width = 650
    SimplePanel = False
  end
  object SchedulerDataSource: TDataSource
    DataSet = mdEvents
    Left = 248
    Top = 80
  end
  object SchedulerDBStorage: TcxSchedulerDBStorage
    UseActualTimeRange = True
    Resources.Items = <>
    Resources.ResourceID = 'ResourceID'
    Resources.ResourceName = 'ResourceName'
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
    Left = 264
    Top = 136
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
