inherited SchedulerRLMainForm: TSchedulerRLMainForm
  Left = 174
  Top = 148
  Caption = 'Report Link Demo - ExpressScheduler'
  ClientHeight = 399
  ClientWidth = 692
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 692
    Caption = 
      'This example demonstrates the ExpressScheduler printing capabili' +
      'ties.'
  end
  inherited sbMain: TStatusBar
    Top = 380
    Width = 692
  end
  inherited ToolBar1: TToolBar
    Width = 692
    object tbtnEvents: TToolButton
      Left = 100
      Top = 0
      Action = actEvents
      ParentShowHint = False
      ShowHint = True
    end
  end
  object Scheduler: TcxScheduler [3]
    Left = 0
    Top = 41
    Width = 692
    Height = 339
    ViewDay.Active = True
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Storage = Storage
    TabOrder = 2
    Splitters = {
      240200007F000000B3020000840000001F020000010000002402000052010000}
    StoredClientBounds = {0100000001000000B302000052010000}
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miEvents: TMenuItem [0]
        Action = actEvents
      end
      object N1: TMenuItem [1]
        Caption = '-'
      end
    end
  end
  inherited sty: TActionList
    object actEvents: TAction
      Caption = 'Generate events'
      Hint = 'Generate events'
      ImageIndex = 9
      OnExecute = tbnGenerateClick
    end
  end
  inherited dxComponentPrinter: TdxComponentPrinter
    CurrentLink = dxComponentPrinterLink1
    object dxComponentPrinterLink1: TcxSchedulerReportLink
      Component = Scheduler
      PrinterPage.DMPaper = 1
      PrinterPage.Footer = 6350
      PrinterPage.Header = 6350
      PrinterPage.PageSize.X = 215900
      PrinterPage.PageSize.Y = 279400
      PrinterPage._dxMeasurementUnits_ = 0
      PrinterPage._dxLastMU_ = 2
      BuiltInReportLink = True
    end
  end
  inherited dxPSEngineController1: TdxPSEngineController
    Active = True
  end
  inherited ilMain: TcxImageList
    FormatVersion = 1
  end
  object Storage: TcxSchedulerStorage
    CustomFields = <>
    Resources.Items = <>
    Left = 208
    Top = 232
  end
end
