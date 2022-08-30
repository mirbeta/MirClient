object RecurrEdit: TRecurrEdit
  Left = 498
  Top = 179
  BorderStyle = bsDialog
  Caption = 'Recurrency'
  ClientHeight = 329
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 285
    Top = 296
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 373
    Top = 296
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 449
    Height = 289
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Settings'
      object Freq: TRadioGroup
        Left = 0
        Top = 4
        Width = 121
        Height = 161
        Caption = 'Recurrency pattern'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Hourly'
          'Daily'
          'Weekly'
          'Monthly'
          'Yearly')
        TabOrder = 0
        OnClick = FreqClick
      end
      object GroupBox1: TGroupBox
        Left = 128
        Top = 4
        Width = 313
        Height = 161
        Caption = 'Pattern details'
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 24
          Width = 38
          Height = 13
          Caption = 'Interval:'
        end
        object Notebook1: TNotebook
          Left = 10
          Top = 43
          Width = 295
          Height = 102
          TabOrder = 0
          object TPage
            Left = 0
            Top = 0
            Caption = 'None'
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'Hour'
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'Day'
            object rDay: TRadioButton
              Left = 8
              Top = 8
              Width = 113
              Height = 17
              Caption = 'Every day'
              TabOrder = 0
            end
            object rWeekDay: TRadioButton
              Left = 8
              Top = 32
              Width = 113
              Height = 17
              Caption = 'Every weekday'
              TabOrder = 1
            end
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'Week'
            object cMon: TCheckBox
              Left = 8
              Top = 8
              Width = 49
              Height = 17
              Caption = 'Mon'
              TabOrder = 0
            end
            object cTue: TCheckBox
              Left = 72
              Top = 8
              Width = 49
              Height = 17
              Caption = 'Tue'
              TabOrder = 1
            end
            object cWed: TCheckBox
              Left = 136
              Top = 8
              Width = 57
              Height = 17
              Caption = 'Wed'
              TabOrder = 2
            end
            object cThu: TCheckBox
              Left = 200
              Top = 8
              Width = 73
              Height = 17
              Caption = 'Thu'
              TabOrder = 3
            end
            object cFri: TCheckBox
              Left = 8
              Top = 32
              Width = 57
              Height = 17
              Caption = 'Fri'
              TabOrder = 4
            end
            object cSat: TCheckBox
              Left = 72
              Top = 32
              Width = 57
              Height = 17
              Caption = 'Sat'
              TabOrder = 5
            end
            object cSun: TCheckBox
              Left = 136
              Top = 32
              Width = 65
              Height = 17
              Caption = 'Sun'
              TabOrder = 6
            end
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'Month'
            object rMonthDay: TRadioButton
              Left = 8
              Top = 8
              Width = 177
              Height = 17
              Caption = 'Every same day of the month'
              TabOrder = 0
            end
            object rSpecialDay: TRadioButton
              Left = 8
              Top = 31
              Width = 49
              Height = 17
              Caption = 'Every'
              TabOrder = 1
            end
            object cWeekNum: TComboBox
              Left = 61
              Top = 29
              Width = 68
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 2
              Text = 'first'
              Items.Strings = (
                'first'
                'second'
                'third'
                'fourth')
            end
            object cDay: TComboBox
              Left = 133
              Top = 29
              Width = 109
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 3
              Text = 'week day'
              Items.Strings = (
                'week day'
                'weekend day'
                'monday'
                'tuesday'
                'wednesday'
                'thursday'
                'friday'
                'saturday'
                'sunday')
            end
          end
          object TPage
            Left = 0
            Top = 0
            Caption = 'Year'
            object Label3: TLabel
              Left = 13
              Top = 56
              Width = 5
              Height = 13
              Caption = 'J'
            end
            object Label4: TLabel
              Left = 37
              Top = 56
              Width = 6
              Height = 13
              Caption = 'F'
            end
            object Label5: TLabel
              Left = 58
              Top = 56
              Width = 9
              Height = 13
              Caption = 'M'
            end
            object Label6: TLabel
              Left = 83
              Top = 56
              Width = 7
              Height = 13
              Caption = 'A'
            end
            object Label7: TLabel
              Left = 106
              Top = 56
              Width = 9
              Height = 13
              Caption = 'M'
            end
            object Label8: TLabel
              Left = 131
              Top = 56
              Width = 5
              Height = 13
              Caption = 'J'
            end
            object Label9: TLabel
              Left = 156
              Top = 56
              Width = 5
              Height = 13
              Caption = 'J'
            end
            object Label10: TLabel
              Left = 179
              Top = 56
              Width = 7
              Height = 13
              Caption = 'A'
            end
            object Label11: TLabel
              Left = 203
              Top = 56
              Width = 7
              Height = 13
              Caption = 'S'
            end
            object Label12: TLabel
              Left = 226
              Top = 56
              Width = 8
              Height = 13
              Caption = 'O'
            end
            object Label13: TLabel
              Left = 250
              Top = 56
              Width = 8
              Height = 13
              Caption = 'N'
            end
            object Label14: TLabel
              Left = 274
              Top = 56
              Width = 8
              Height = 13
              Caption = 'D'
            end
            object rYearDay: TRadioButton
              Left = 8
              Top = 8
              Width = 177
              Height = 17
              Caption = 'Every same day of the year'
              TabOrder = 0
            end
            object cYearDay: TComboBox
              Left = 133
              Top = 29
              Width = 109
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 1
              Text = 'week day'
              Items.Strings = (
                'week day'
                'weekend day'
                'monday'
                'tuesday'
                'wednesday'
                'thursday'
                'friday'
                'saturday'
                'sunday')
            end
            object rYearSpecialDay: TRadioButton
              Left = 8
              Top = 32
              Width = 49
              Height = 17
              Caption = 'Every'
              TabOrder = 2
            end
            object cYearWeekNum: TComboBox
              Left = 61
              Top = 29
              Width = 68
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 3
              Text = 'first'
              Items.Strings = (
                'first'
                'second'
                'third'
                'fourth')
            end
            object yck1: TCheckBox
              Left = 8
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 4
            end
            object yck2: TCheckBox
              Left = 32
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 5
            end
            object yck3: TCheckBox
              Left = 56
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 6
            end
            object yck4: TCheckBox
              Left = 80
              Top = 72
              Width = 33
              Height = 17
              BiDiMode = bdLeftToRight
              ParentBiDiMode = False
              TabOrder = 7
            end
            object yck5: TCheckBox
              Left = 104
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 8
            end
            object yck6: TCheckBox
              Left = 128
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 9
            end
            object yck7: TCheckBox
              Left = 152
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 10
            end
            object yck8: TCheckBox
              Left = 176
              Top = 72
              Width = 33
              Height = 17
              TabOrder = 11
            end
            object yck9: TCheckBox
              Left = 200
              Top = 72
              Width = 25
              Height = 17
              TabOrder = 12
            end
            object yck10: TCheckBox
              Left = 224
              Top = 72
              Width = 25
              Height = 17
              TabOrder = 13
            end
            object yck11: TCheckBox
              Left = 248
              Top = 72
              Width = 17
              Height = 17
              TabOrder = 14
            end
            object yck12: TCheckBox
              Left = 272
              Top = 72
              Width = 25
              Height = 17
              TabOrder = 15
            end
          end
        end
        object Interval: TEdit
          Left = 64
          Top = 20
          Width = 41
          Height = 21
          TabOrder = 1
          Text = '1'
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 164
        Width = 441
        Height = 97
        Caption = 'Range'
        TabOrder = 2
        object Label2: TLabel
          Left = 152
          Top = 48
          Width = 56
          Height = 13
          Caption = 'occurences'
        end
        object rInfinite: TRadioButton
          Left = 8
          Top = 24
          Width = 65
          Height = 17
          Caption = 'Infinite'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rUntil: TRadioButton
          Left = 8
          Top = 48
          Width = 73
          Height = 17
          Caption = 'For :'
          TabOrder = 1
        end
        object rUntilDate: TRadioButton
          Left = 8
          Top = 72
          Width = 73
          Height = 17
          Caption = 'Until date : '
          TabOrder = 2
        end
        object cDate: TDateTimePicker
          Left = 80
          Top = 68
          Width = 129
          Height = 21
          Date = 38235.987179884260000000
          Time = 38235.987179884260000000
          TabOrder = 3
        end
        object cOccur: TEdit
          Left = 80
          Top = 42
          Width = 65
          Height = 21
          TabOrder = 4
          Text = '10'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Exceptions'
      ImageIndex = 1
      object Label15: TLabel
        Left = 0
        Top = 88
        Width = 52
        Height = 13
        Caption = 'Exceptions'
      end
      object exsd: TDateTimePicker
        Left = 0
        Top = 16
        Width = 113
        Height = 21
        Date = 38243.670951597240000000
        Time = 38243.670951597240000000
        TabOrder = 0
      end
      object ExList: TListBox
        Left = 0
        Top = 104
        Width = 353
        Height = 153
        ItemHeight = 13
        TabOrder = 1
      end
      object Button3: TButton
        Left = 360
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 360
        Top = 136
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 3
        OnClick = Button4Click
      end
      object exst: TDateTimePicker
        Left = 120
        Top = 16
        Width = 105
        Height = 21
        Date = 38243.670951597240000000
        Time = 38243.670951597240000000
        ShowCheckbox = True
        Kind = dtkTime
        TabOrder = 4
      end
      object exed: TDateTimePicker
        Left = 0
        Top = 48
        Width = 113
        Height = 21
        Date = 38243.670951597240000000
        Time = 38243.670951597240000000
        TabOrder = 5
      end
      object exet: TDateTimePicker
        Left = 120
        Top = 48
        Width = 105
        Height = 21
        Date = 38243.670951597240000000
        Time = 38243.670951597240000000
        ShowCheckbox = True
        Kind = dtkTime
        TabOrder = 6
      end
      object Button5: TButton
        Left = 360
        Top = 168
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 7
        OnClick = Button5Click
      end
    end
  end
end
