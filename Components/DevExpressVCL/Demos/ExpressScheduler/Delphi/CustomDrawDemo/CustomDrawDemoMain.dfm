inherited CustomDrawDemoMainForm: TCustomDrawDemoMainForm
  Caption = 'ExpressScheduler CustomDrawDemo'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo shows how custom drawing can be used to change or enha' +
      'nce the visual appearance of the scheduler. Click '#39'About this de' +
      'mo'#39' for more information.'
  end
  inherited Scheduler: TcxScheduler
    DateNavigator.RowCount = 3
    DateNavigator.OnCustomDrawDayCaption = SchedulerDateNavigatorCustomDrawDayCaption
    DateNavigator.OnCustomDrawDayNumber = SchedulerDateNavigatorCustomDrawDayNumber
    DateNavigator.OnCustomDrawContent = SchedulerDateNavigatorCustomDrawContent
    DateNavigator.OnCustomDrawHeader = SchedulerDateNavigatorCustomDrawHeader
    ViewDay.OnCustomDrawContainer = SchedulerViewDayCustomDrawContainer
    ViewDay.OnCustomDrawTimeRuler = SchedulerViewDayCustomDrawRuler
    OnCustomDrawContent = SchedulerCustomDrawContent
    OnCustomDrawDayHeader = SchedulerCustomDrawDayHeader
    OnCustomDrawEvent = SchedulerCustomDrawEvent
    OnCustomDrawGroupSeparator = SchedulerCustomDrawGroupSeparator
    OnCustomDrawResourceHeader = SchedulerCustomDrawResourceHeader
    ControlBox.Visible = False
    Storage = Storage
    Splitters = {
      24020000FB000000B3020000000100001F0200000100000024020000BC010000}
    inherited pnlControls: TPanel
      Height = 243
      inherited Memo1: TMemo
        Height = 243
        Lines.Strings = (
          'Your '
          'controls can '
          'be placed '
          'here')
      end
    end
  end
  inherited mmMain: TMainMenu
    object CustomDraw1: TMenuItem [1]
      Caption = '&CustomDraw'
      object miEvents: TMenuItem
        Caption = 'Events'
        Checked = True
        OnClick = UpdateCustomDraw
      end
      object miHeaders: TMenuItem
        Tag = 1
        Caption = 'Headers'
        Checked = True
        OnClick = UpdateCustomDraw
      end
      object miContent: TMenuItem
        Tag = 2
        Caption = 'Content'
        Checked = True
        OnClick = UpdateCustomDraw
      end
      object miResources: TMenuItem
        Caption = 'Resources'
        Checked = True
        OnClick = UpdateCustomDraw
      end
      object miGroupSeparator: TMenuItem
        Caption = 'Group separator'
        Checked = True
        OnClick = UpdateCustomDraw
      end
      object ViewDay1: TMenuItem
        Caption = 'ViewDay'
        object miContainer: TMenuItem
          Tag = 6
          Caption = 'Container'
          Checked = True
          OnClick = UpdateCustomDraw
        end
        object miTimeRuler: TMenuItem
          Tag = 7
          Caption = 'Time Ruler'
          Checked = True
          OnClick = UpdateCustomDraw
        end
      end
      object DateNavigator1: TMenuItem
        Caption = 'DateNavigator'
        object miMonthHeaders: TMenuItem
          Tag = 3
          Caption = 'Month headers'
          Checked = True
          OnClick = UpdateCustomDraw
        end
        object miDayCaptions: TMenuItem
          Tag = 4
          Caption = 'Day captions'
          Checked = True
          OnClick = UpdateCustomDraw
        end
        object miDays: TMenuItem
          Tag = 5
          Caption = 'Days'
          Checked = True
          OnClick = UpdateCustomDraw
        end
        object miDNContent: TMenuItem
          Caption = 'Content'
          Checked = True
          OnClick = UpdateCustomDraw
        end
      end
    end
    inherited miView: TMenuItem
      inherited miControlBox: TMenuItem
        Checked = False
      end
    end
  end
  object Storage: TcxSchedulerStorage
    CustomFields = <
      item
        Name = 'SyncIDField'
      end>
    Resources.Items = <
      item
        Name = '  Sinan Demir  '
        ResourceID = '0'
      end
      item
        Name = '  Bastian Bauwens  '
        ResourceID = '1'
      end
      item
        Name = '  Oliver Sturm  '
        ResourceID = '2'
      end>
    Left = 16
    Top = 40
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 120
    Top = 120
    object csBoldItalic: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
    end
    object csItalic: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
    end
    object csRed: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
  end
end
