inherited AgendaViewDemoMainForm: TAgendaViewDemoMainForm
  Left = 199
  Top = 124
  Caption = 'ExpressScheduler Agenda View Demo'
  ClientHeight = 539
  ClientWidth = 819
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 819
    Caption = 
      'This demo shows the use of the Agenda View, which displays a lis' +
      't of appointments grouped by day. Click '#39'About this demo'#39' for mo' +
      're information.'
  end
  inherited Scheduler: TcxScheduler
    Top = 16
    Width = 819
    Height = 504
    DateNavigator.RowCount = 3
    ViewAgenda.Active = True
    ViewDay.Active = False
    EventOperations.SharingBetweenResources = True
    OptionsView.ResourcesPerPage = 10
    Storage = SchedulerStorage
    Splitters = {
      A302000078010000320300007D0100009E02000001000000A3020000F7010000}
    StoredClientBounds = {010000000100000032030000F7010000}
    inherited pnlControls: TPanel
      Height = 122
      inherited Memo1: TMemo
        Height = 122
      end
    end
  end
  inherited StatusBar: TStatusBar
    Top = 520
    Width = 819
  end
  inherited mmMain: TMainMenu
    Left = 440
    Top = 32
    inherited Resources1: TMenuItem
      GroupIndex = 1
    end
    object AgendaOptions1: TMenuItem [5]
      Caption = 'Agenda Options'
      GroupIndex = 1
      object DayHeaderOrientation2: TMenuItem
        Caption = 'Day Headers'
        object Horizontal1: TMenuItem
          Caption = 'Horizontal'
          Checked = True
          GroupIndex = 111
          RadioItem = True
          OnClick = Horizontal1Click
        end
        object Vertical1: TMenuItem
          Tag = 1
          Caption = 'Vertical'
          GroupIndex = 111
          RadioItem = True
          OnClick = Horizontal1Click
        end
      end
      object DisplayMode1: TMenuItem
        Caption = 'Display Mode'
        object AllDays1: TMenuItem
          Caption = 'All Days'
          Checked = True
          GroupIndex = 112
          RadioItem = True
          OnClick = mi_dedUnlimitedClick
        end
        object SelectedDays1: TMenuItem
          Tag = 1
          Caption = 'Selected Days Only'
          GroupIndex = 112
          RadioItem = True
          OnClick = mi_dedUnlimitedClick
        end
        object SelectedNonEmptyDays1: TMenuItem
          Tag = 2
          Caption = 'Selected Non-Empty Days Only'
          GroupIndex = 112
          RadioItem = True
          OnClick = mi_dedUnlimitedClick
        end
      end
      object Showlocation2: TMenuItem
        Caption = 'Show Locations'
        Checked = True
        OnClick = AgendaSettingsClick
      end
      object Showresources2: TMenuItem
        Tag = 1
        Caption = 'Show Resources'
        Checked = True
        OnClick = AgendaSettingsClick
      end
      object Showtimeasclock2: TMenuItem
        Tag = 2
        Caption = 'Time as Clock'
        OnClick = AgendaSettingsClick
      end
    end
    inherited miAbout: TMenuItem
      GroupIndex = 1
    end
  end
  inherited Timer1: TTimer
    Left = 480
    Top = 32
  end
  inherited SaveDialog: TSaveDialog
    Left = 520
    Top = 32
  end
  object SchedulerStorage: TcxSchedulerStorage
    CustomFields = <
      item
        Name = 'IconIndex'
        ValueType = 'Integer'
      end
      item
        Name = 'SyncIDField'
      end>
    Reminders.ReminderWindowLookAndFeel.Kind = lfOffice11
    Resources.Items = <
      item
        ImageIndex = 7
        Name = '(Any)'
        ResourceID = 0
      end
      item
        ImageIndex = 4
        Name = 'Mercedes-Benz SLK 350'
        ResourceID = '4'
      end
      item
        ImageIndex = 2
        Name = 'Audi S8'
        ResourceID = '3'
      end
      item
        ImageIndex = 0
        Name = 'Chevrolet Camaro'
        ResourceID = '1'
      end
      item
        ImageIndex = 1
        Name = 'Lexus IS 350'
        ResourceID = '2'
      end
      item
        ImageIndex = 3
        Name = 'Toyota Tundra 4x4 Reg Cab'
        ResourceID = '5'
      end
      item
        ImageIndex = 6
        Name = 'Nissan Murano'
        ResourceID = '6'
      end
      item
        ImageIndex = 5
        Name = 'Ford Mustang GT Coupe'
        ResourceID = '7'
      end>
    Left = 408
    Top = 32
  end
end
