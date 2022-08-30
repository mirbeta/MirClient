object DemoBasicMainForm: TDemoBasicMainForm
  Left = 239
  Top = 187
  Width = 700
  Height = 550
  Caption = 'ExpressScheduler DemoBasic Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbDescrip: TLabel
    Left = 0
    Top = 0
    Width = 692
    Height = 32
    Align = alTop
    Caption = 'Brief purpose of the demo'#10#13'That'#39's it'
    Color = 4707838
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Scheduler: TcxScheduler
    Left = 0
    Top = 32
    Width = 692
    Height = 445
    DateNavigator.RowCount = 2
    ViewDay.Active = True
    ViewDay.TimeRulerMinutes = True
    Align = alClient
    ControlBox.Control = pnlControls
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OptionsView.AdditionalTimeZoneDaylightSaving = False
    OptionsView.CurrentTimeZoneDaylightSaving = False
    TabOrder = 0
    Splitters = {
      24020000FC000000B3020000010100001F0200000100000024020000BC010000}
    StoredClientBounds = {0100000001000000B3020000BC010000}
    object pnlControls: TPanel
      Left = 0
      Top = 0
      Width = 143
      Height = 187
      Align = alClient
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 143
        Height = 187
        Align = alClient
        BorderStyle = bsNone
        Lines.Strings = (
          'Your controls can be placed '
          'here')
        TabOrder = 0
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 477
    Width = 692
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object mmMain: TMainMenu
    Left = 424
    Top = 8
    object miFile: TMenuItem
      Caption = '&File'
      object Exportto1: TMenuItem
        Caption = 'Export'
        object Separator2: TMenuItem
          Caption = '-'
        end
        object Excel2: TMenuItem
          Tag = 5
          Caption = 'Excel'
          OnClick = ExportToClick
        end
        object Excel1: TMenuItem
          Tag = 1
          Caption = 'Excel 97-2003'
          OnClick = ExportToClick
        end
        object Html1: TMenuItem
          Tag = 3
          Caption = 'HTML'
          OnClick = ExportToClick
        end
        object ext1: TMenuItem
          Tag = 2
          Caption = 'Text'
          OnClick = ExportToClick
        end
        object Xml1: TMenuItem
          Tag = 4
          Caption = 'XML'
          OnClick = ExportToClick
        end
      end
      object Outlooksynchronization1: TMenuItem
        Caption = 'Synchronization'
        object Outlook1: TMenuItem
          Caption = 'Scheduler with Outlook'
          OnClick = SyncClick
        end
        object Outlook2: TMenuItem
          Tag = 1
          Caption = 'Outlook with Scheduler'
          OnClick = SyncClick
        end
      end
      object Separator1: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Caption = 'E&xit'
        Hint = 'Press to quit the demo-program'
        ShortCut = 32856
        OnClick = FileExitExecute
      end
    end
    object miView: TMenuItem
      Caption = '&View'
      object miDay: TMenuItem
        Caption = 'Day'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miWorkweek: TMenuItem
        Tag = 1
        Caption = 'Wo&rk week'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miWeek: TMenuItem
        Tag = 2
        Caption = 'Week'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miMonth: TMenuItem
        Tag = 3
        Caption = 'Month'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miTimeGrid: TMenuItem
        Tag = 4
        Caption = 'TimeGrid'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miYear: TMenuItem
        Tag = 5
        Caption = 'Year'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object miAgenda: TMenuItem
        Tag = 6
        Caption = 'Agenda'
        GroupIndex = 1
        RadioItem = True
        OnClick = ViewModeClick
      end
      object N3: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miGotoDate: TMenuItem
        Tag = 4
        Caption = 'Go to Date...'
        GroupIndex = 1
        ShortCut = 16455
        OnClick = miGotoDateClick
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miViewDateNavigator: TMenuItem
        Caption = 'Date Navigator'
        Checked = True
        GroupIndex = 1
        OnClick = ViewClick
      end
      object miControlBox: TMenuItem
        Tag = 1
        Caption = 'Control Box'
        Checked = True
        GroupIndex = 1
        OnClick = ViewClick
      end
      object miViewposition: TMenuItem
        Caption = 'View Position'
        GroupIndex = 1
        object miAtLeft: TMenuItem
          Caption = 'at Left'
          Checked = True
          RadioItem = True
          OnClick = ViewPositionClick
        end
        object miAtRight: TMenuItem
          Tag = 1
          Caption = 'at Right'
          RadioItem = True
          OnClick = ViewPositionClick
        end
      end
      object miViewStyle: TMenuItem
        Caption = 'View Style'
        GroupIndex = 1
        object miClassic: TMenuItem
          Caption = 'Classic'
          GroupIndex = 1
          RadioItem = True
          OnClick = miViewStyleClick
        end
        object miModern: TMenuItem
          Tag = 1
          Caption = 'Modern'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = miViewStyleClick
        end
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object miDay1: TMenuItem
        Caption = '&Day'
        GroupIndex = 1
        object Alldayarea1: TMenuItem
          Caption = 'All-Day Area'
          GroupIndex = 1
          object Height1: TMenuItem
            Caption = 'Height'
            object miAllDayAreaHeightDefault: TMenuItem
              Caption = 'Default'
              Checked = True
              GroupIndex = 98
              RadioItem = True
              OnClick = AllDayAreaHeightClick
            end
            object mimiAllDayAreaHeight3: TMenuItem
              Tag = 3
              Caption = '3 Events'
              GroupIndex = 98
              RadioItem = True
              OnClick = AllDayAreaHeightClick
            end
            object miAllDayAreaHeight5: TMenuItem
              Tag = 5
              Caption = '5 Events'
              GroupIndex = 98
              RadioItem = True
              OnClick = AllDayAreaHeightClick
            end
          end
          object Scrollbar1: TMenuItem
            Caption = 'Scroll Bar'
            object miAllDayScrollDefault: TMenuItem
              Caption = 'Default'
              Checked = True
              GroupIndex = 99
              RadioItem = True
              OnClick = miAllDayScrollClick
            end
            object miAllDayScrollNever: TMenuItem
              Tag = 1
              Caption = 'Never'
              GroupIndex = 99
              RadioItem = True
              OnClick = miAllDayScrollClick
            end
            object miAllDayScrollAlways: TMenuItem
              Tag = 2
              Caption = 'Always'
              GroupIndex = 99
              RadioItem = True
              OnClick = miAllDayScrollClick
            end
          end
          object miShowEvents: TMenuItem
            Caption = 'Show Events'
            Checked = True
            OnClick = miShowEventsClick
          end
        end
        object miAllDayContainer: TMenuItem
          Caption = 'All-Day Events Container'
          Checked = True
          GroupIndex = 1
          OnClick = miDaySettingsClick
        end
        object Alwaysshoweventtime1: TMenuItem
          Tag = 1
          Caption = 'Always Show Event Time'
          GroupIndex = 1
          OnClick = miDaySettingsClick
        end
        object miWorktimeonly: TMenuItem
          Tag = 2
          Caption = 'Work Time Only'
          GroupIndex = 1
          OnClick = miDaySettingsClick
        end
        object Displayminutesontimeruler1: TMenuItem
          Tag = 3
          Caption = 'Display Minutes on Time Ruler'
          Checked = True
          GroupIndex = 1
          OnClick = miDaySettingsClick
        end
        object miDayHeaderArea: TMenuItem
          Caption = 'Day Header'
          Checked = True
          GroupIndex = 1
          OnClick = miDayHeaderAreaClick
        end
        object miDayHeaderModernDisplayMode: TMenuItem
          Caption = 'Day Header Modern Style'
          GroupIndex = 100
          object miDHMDefault: TMenuItem
            Caption = 'Default'
            Checked = True
            GroupIndex = 100
            RadioItem = True
            OnClick = miDayHeaderModernDisplayModeClick
          end
          object miDHMClassic: TMenuItem
            Tag = 1
            Caption = 'Classic'
            GroupIndex = 100
            RadioItem = True
            OnClick = miDayHeaderModernDisplayModeClick
          end
          object miDHMDayAndDate: TMenuItem
            Tag = 2
            Caption = 'Day of Week, Date'
            GroupIndex = 100
            RadioItem = True
            OnClick = miDayHeaderModernDisplayModeClick
          end
        end
      end
      object miWeek1: TMenuItem
        Tag = 2
        Caption = '&Week'
        GroupIndex = 1
        object miWeekCompressWeekends: TMenuItem
          Caption = 'Compress Weekends'
          Checked = True
          OnClick = miWeekCompressWeekendsClick
        end
        object miSingleColumn: TMenuItem
          Tag = 1
          Caption = 'Single Column'
          OnClick = miWeekViewClick
        end
        object miWeekViewHideWeekend: TMenuItem
          AutoCheck = True
          Caption = 'Hide Weekends'
          OnClick = miWeekViewHideWeekendClick
        end
      end
      object miMonth1: TMenuItem
        Tag = 3
        Caption = '&Month'
        GroupIndex = 1
        object miCompressweekends: TMenuItem
          Caption = 'Compress Weekends'
          Checked = True
          OnClick = miCompressweekendsClick
        end
        object miMonthViewHideWeekEnd: TMenuItem
          AutoCheck = True
          Caption = 'Hide Weekends'
          OnClick = miWeekViewHideWeekendClick
        end
      end
      object miTimeGrid1: TMenuItem
        Tag = 4
        Caption = '&TimeGrid'
        GroupIndex = 1
        object miTimeWorktimeonly: TMenuItem
          Caption = 'Work Time Only'
          OnClick = miTimeWorktimeonlyClick
        end
      end
      object miYear1: TMenuItem
        Tag = 5
        Caption = '&Year'
        GroupIndex = 1
        object Alldayeventsonly1: TMenuItem
          Caption = 'All-Day Events Only'
          OnClick = AlldayeventsonlyClick
        end
      end
      object Agenda1: TMenuItem
        Caption = 'Agenda'
        GroupIndex = 1
        object DayHeaderOrientation1: TMenuItem
          Caption = 'Day Headers'
          object mi_dhoHorizontal: TMenuItem
            Caption = 'Horizontal'
            Checked = True
            GroupIndex = 101
            RadioItem = True
            OnClick = mi_dhoHorizontalClick
          end
          object mi_dhoVertical: TMenuItem
            Tag = 1
            Caption = 'Vertical'
            GroupIndex = 101
            RadioItem = True
            OnClick = mi_dhoHorizontalClick
          end
        end
        object DisplayEmptyDayMode1: TMenuItem
          Caption = 'Display Mode'
          object mi_dedUnlimited: TMenuItem
            Caption = 'All Days'
            Checked = True
            GroupIndex = 102
            RadioItem = True
            OnClick = mi_dedUnlimitedClick
          end
          object mi_dedSelected: TMenuItem
            Tag = 1
            Caption = 'Selected Days Only'
            GroupIndex = 102
            RadioItem = True
            OnClick = mi_dedUnlimitedClick
          end
          object mi_dedHideEmptyDays: TMenuItem
            Tag = 2
            Caption = 'Selected Non-Empty Days Only'
            GroupIndex = 102
            RadioItem = True
            OnClick = mi_dedUnlimitedClick
          end
        end
        object ShowLocation1: TMenuItem
          Caption = 'Show Locations'
          Checked = True
          OnClick = AgendaSettingsClick
        end
        object ShowResources1: TMenuItem
          Tag = 1
          Caption = 'Show Resources'
          Checked = True
          OnClick = AgendaSettingsClick
        end
        object ShowTimeAsClock1: TMenuItem
          Tag = 2
          Caption = 'Time as Clock'
          OnClick = AgendaSettingsClick
        end
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miSelectOnRightClick: TMenuItem
        Caption = 'Select on Right Click'
        GroupIndex = 1
        OnClick = miSelectOnRightClickClick
      end
    end
    object miEventsOpt: TMenuItem
      Caption = 'Events'
      object miIntersection: TMenuItem
        Caption = 'Intersection'
        Checked = True
        OnClick = miIntersectionClick
      end
      object miSharing: TMenuItem
        Caption = 'Sharing Between Resources'
        OnClick = miSharingClick
      end
    end
    object Resources1: TMenuItem
      Caption = 'Resources'
      object GroupBy1: TMenuItem
        Caption = 'Group by'
        object miGroupByNone: TMenuItem
          Caption = 'None'
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupByClick
        end
        object miGroupByResources: TMenuItem
          Tag = 1
          Caption = 'Resource'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupByClick
        end
        object miGroupByDate: TMenuItem
          Tag = 2
          Caption = 'Date'
          GroupIndex = 1
          RadioItem = True
          OnClick = miGroupByClick
        end
      end
      object Resourcelayout1: TMenuItem
        Caption = 'Layout Editor...'
        OnClick = Resourcelayout1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miResPerPage: TMenuItem
        Caption = 'Resources per Page'
        object miAllResPerPage: TMenuItem
          Caption = 'All'
          OnClick = miResCountClick
        end
        object miSeparator: TMenuItem
          Caption = '-'
        end
        object miOneResPerPage: TMenuItem
          Tag = 1
          Caption = '1'
          OnClick = miResCountClick
        end
        object miTwoResPerPage: TMenuItem
          Tag = 2
          Caption = '2'
          OnClick = miResCountClick
        end
        object miThreeResPerPage: TMenuItem
          Tag = 3
          Caption = '3'
          OnClick = miResCountClick
        end
      end
    end
    object miAbout: TMenuItem
      Caption = '&About this Demo'
      Hint = 'Displays the brief description of the current demo features'
      OnClick = miAboutClick
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 504
    Top = 8
  end
  object lfController: TcxLookAndFeelController
    Left = 544
    Top = 8
  end
end
