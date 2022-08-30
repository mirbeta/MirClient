inherited WebServiceDemoMainForm: TWebServiceDemoMainForm
  Left = 236
  Top = 153
  Caption = 'ExpressScheduler WebServiceDemo'
  ClientHeight = 468
  ClientWidth = 691
  Color = clWhite
  OldCreateOrder = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 691
    Caption = 
      'This demo shows how to access online calendar data using the scheduler. C' +
      'lick '#39'About this demo'#39' for more information.'
  end
  inherited Scheduler: TcxScheduler
    Left = 209
    Width = 482
    Height = 417
    DateNavigator.Visible = False
    BorderStyle = cxcbsNone
    ControlBox.Control = nil
    ControlBox.Visible = False
    OptionsView.GroupSeparatorWidth = 2
    OptionsView.HorzSplitterWidth = 3
    OptionsView.ResourceHeaders.MultilineCaptions = True
    OptionsView.ResourceHeaders.ImagePosition = ipTop
    OptionsView.ResourcesPerPage = 3
    OptionsView.Style = svsModern
    OptionsView.VertSplitterWidth = 3
    OptionsView.ViewPosition = vpRight
    Storage = Storage
    Splitters = {
      00000000FA0000008F000000FD0000008F000000000000009200000078010000}
    StoredClientBounds = {0000000000000000E2010000A1010000}
    inherited pnlControls: TPanel
      Width = 0
      Height = 187
      Align = alCustom
      TabOrder = 1
      Visible = False
      inherited Memo1: TMemo
        Width = 0
        Height = 187
        Lines.Strings = (
          'Your '
          'controls can '
          'be placed '
          'here')
      end
    end
  end
  inherited StatusBar: TStatusBar
    Top = 449
    Width = 691
    Color = clWhite
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 32
    Width = 201
    Height = 417
    Align = alLeft
    TabOrder = 2
    object cxDateNavigator1: TcxDateNavigator
      Left = 1
      Top = 1
      Width = 199
      Height = 147
      Align = alTop
      BorderStyle = cxcbsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Scheduler = Scheduler
      TabOrder = 0
    end
    object tlCalendars: TcxTreeList
      Left = 1
      Top = 189
      Width = 199
      Height = 186
      BorderStyle = cxcbsNone
      Align = alClient
      Bands = <
        item
        end>
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.ConfirmDelete = False
      OptionsBehavior.DragCollapse = False
      OptionsData.CancelOnExit = False
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.CheckGroups = True
      OptionsView.Headers = False
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 1
      OnNodeCheckChanged = tlCalendarsNodeCheckChanged
      object tcName: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tcId: TcxTreeListColumn
        Visible = False
        DataBinding.ValueType = 'String'
        Width = 100
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object btnAddAccount: TcxButton
      Left = 1
      Top = 148
      Width = 199
      Height = 41
      Align = alTop
      Caption = 'Add Calendar...'
      DropDownMenu = pmAddAccount
      Kind = cxbkOfficeDropDown
      TabOrder = 2
    end
    object cxButton1: TcxButton
      Left = 1
      Top = 375
      Width = 199
      Height = 41
      Align = alBottom
      Action = aReloadEvents
      TabOrder = 3
    end
  end
  object cxSplitter1: TcxSplitter [4]
    Left = 201
    Top = 32
    Width = 8
    Height = 417
  end
  inherited mmMain: TMainMenu
    inherited miFile: TMenuItem
      object Refresh1: TMenuItem [0]
        Action = aReloadEvents
      end
      object N5: TMenuItem [1]
        Caption = '-'
      end
    end
    inherited Resources1: TMenuItem
      inherited GroupBy1: TMenuItem
        inherited miGroupByResources: TMenuItem
          Checked = False
        end
        inherited miGroupByDate: TMenuItem
          Checked = True
        end
      end
    end
  end
  object pmAddAccount: TPopupMenu
    Left = 296
    Top = 152
  end
  object Storage: TcxSchedulerWebServiceStorage
    StoreUsingGlobalTime = True
    Reminders.Active = False
    Resources.Items = <>
    Left = 384
    Top = 8
  end
  object alMain: TActionList
    Left = 608
    Top = 16
    object aReloadEvents: TAction
      Caption = 'Reload Events'
      Enabled = False
      ShortCut = 116
      OnExecute = aReloadEventsExecute
    end
  end
end
