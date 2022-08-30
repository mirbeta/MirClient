inherited RangeControlDemoMainForm: TRangeControlDemoMainForm
  Caption = 'ExpressScheduler RangeControl Demo'
  ClientWidth = 604
  Constraints.MinWidth = 620
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 604
    Caption = 
      'This demo shows how to enable date navigation in the scheduler u' +
      'sing the Range Control. Click '#39'About this demo'#39' for more informa' +
      'tion.'
  end
  inherited Scheduler: TcxScheduler
    Top = 229
    Width = 604
    Height = 243
    DateNavigator.Visible = False
    ViewTimeGrid.Scales.TimeStep = 60
    ControlBox.Visible = False
    Storage = SchedulerStorage
    Splitters = {
      1C020000FB000000AB0200000001000017020000010000001C02000069010000}
    StoredClientBounds = {01000000010000005B020000F2000000}
    inherited pnlControls: TPanel
      Height = 243
      inherited Memo1: TMemo
        Height = 243
      end
    end
  end
  inherited StatusBar: TStatusBar
    Width = 604
  end
  object dxRangeControl1: TdxRangeControl [3]
    Left = 0
    Top = 121
    Width = 604
    Height = 100
    Align = alTop
    Client = Scheduler
    ClientPropertiesClassName = 'TcxSchedulerRangeControlClientProperties'
    ClientProperties.MaxValue = 42729d
    ClientProperties.MinValue = 42667d
    ClientProperties.Scales.Day.Active = True
    ClientProperties.Scales.Day.Visible = True
    ClientProperties.Style.EventCounterFont.Charset = DEFAULT_CHARSET
    ClientProperties.Style.EventCounterFont.Color = clWindowText
    ClientProperties.Style.EventCounterFont.Height = -35
    ClientProperties.Style.EventCounterFont.Name = 'Tahoma'
    ClientProperties.Style.EventCounterFont.Style = []
    SelectedRangeMaxValue = 42703d
    SelectedRangeMinValue = 42702d
    TabOrder = 2
    VisibleRangeMaxScaleFactor = 10.000000000000000000
    VisibleRangeMaxValue = 42729d
    VisibleRangeMinValue = 42667d
  end
  object cxSplitter1: TcxSplitter [4]
    Left = 0
    Top = 221
    Width = 604
    Height = 8
    MinSize = 100
    Control = dxRangeControl1
  end
  object cxGroupBox1: TcxGroupBox [5]
    Left = 0
    Top = 0
    Align = alTop
    Style.BorderStyle = ebsNone
    TabOrder = 4
    Transparent = True
    Height = 89
    Width = 604
    object cxCheckBox1: TcxCheckBox
      Left = 3
      Top = 6
      Caption = 'Auto Change Scheduler View'
      Properties.OnEditValueChanged = cxCheckBox1PropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 0
      Transparent = True
    end
    object cxCheckBox2: TcxCheckBox
      Left = 3
      Top = 59
      Caption = 'Auto Format Captions'
      Properties.OnEditValueChanged = cxCheckBox2PropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 1
      Transparent = True
    end
    object cxCheckBox3: TcxCheckBox
      Left = 3
      Top = 33
      Caption = 'Auto Adjust Range Control Settings'
      Properties.OnEditValueChanged = cxCheckBox3PropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 2
      Transparent = True
    end
    object cxLabel1: TcxLabel
      Left = 206
      Top = 8
      Caption = 'Display Events As:'
      Transparent = True
    end
    object cxLabel2: TcxLabel
      Left = 206
      Top = 34
      Caption = 'Thumbnail Height:'
      Transparent = True
    end
    object cxComboBox1: TcxComboBox
      Left = 357
      Top = 6
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Auto'
        'Thumbnails'
        'Numbers')
      Properties.OnEditValueChanged = cxComboBox1PropertiesEditValueChanged
      TabOrder = 5
      Text = 'Auto'
      Width = 84
    end
    object cxSpinEdit1: TcxSpinEdit
      Left = 357
      Top = 33
      Properties.AssignedValues.MinValue = True
      Properties.OnEditValueChanged = cxSpinEdit1PropertiesEditValueChanged
      TabOrder = 6
      Width = 84
    end
    object cxLabel3: TcxLabel
      Left = 206
      Top = 63
      Caption = 'Scale Interval Minimal Width:'
      Transparent = True
    end
    object cxSpinEdit2: TcxSpinEdit
      Left = 357
      Top = 62
      Properties.MinValue = 1.000000000000000000
      Properties.ValueType = vtInt
      Properties.OnEditValueChanged = cxSpinEdit2PropertiesEditValueChanged
      TabOrder = 8
      Value = 30
      Width = 84
    end
  end
  inherited mmMain: TMainMenu
    inherited miView: TMenuItem
      inherited N2: TMenuItem
        Visible = False
      end
      inherited miViewDateNavigator: TMenuItem
        Visible = False
      end
      inherited miControlBox: TMenuItem
        Visible = False
      end
      inherited miViewposition: TMenuItem
        Visible = False
      end
    end
  end
  inherited Timer1: TTimer
    Left = 512
    Top = 56
  end
  object SchedulerStorage: TcxSchedulerStorage
    CustomFields = <>
    Reminders.Active = False
    Resources.Items = <>
    Left = 424
    Top = 72
  end
end
