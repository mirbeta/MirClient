object EditorsInPlaceDemoCarsForm: TEditorsInPlaceDemoCarsForm
  Left = 286
  Top = 110
  Caption = 'EditorsInPlaceDemoCarsForm'
  ClientHeight = 417
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCarInfo: TcxGroupBox
    Left = 48
    Top = 16
    PanelStyle.Active = True
    TabOrder = 0
    Height = 329
    Width = 473
    object Label6: TcxLabel
      Left = 8
      Top = 183
      Caption = 'Description'
      Transparent = True
    end
    object cxDBMemo1: TcxDBMemo
      Left = 8
      Top = 202
      DataBinding.DataField = 'Description'
      DataBinding.DataSource = dmGridCars.dsModels
      Properties.ScrollBars = ssVertical
      Style.Color = 16247513
      TabOrder = 4
      Height = 118
      Width = 457
    end
    object Panel1: TcxGroupBox
      Left = 8
      Top = 8
      PanelStyle.Active = True
      TabOrder = 0
      Height = 49
      Width = 457
      object Label3: TcxLabel
        Left = 8
        Top = 14
        Caption = 'Trade Mark'
        Transparent = True
      end
      object Label4: TcxLabel
        Left = 236
        Top = 14
        Caption = 'Model'
        Transparent = True
      end
      object cxDBTextEdit1: TcxDBTextEdit
        Left = 68
        Top = 13
        DataBinding.DataField = 'Trademark'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 0
        Width = 121
      end
      object cxDBTextEdit2: TcxDBTextEdit
        Left = 272
        Top = 13
        DataBinding.DataField = 'Name'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 1
        Width = 121
      end
    end
    object GroupBox1: TcxGroupBox
      Left = 8
      Top = 64
      Caption = 'Engine'
      TabOrder = 1
      Height = 110
      Width = 145
      object cxDBTextEdit5: TcxDBTextEdit
        Left = 11
        Top = 22
        DataBinding.DataField = 'Torque'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 0
        Width = 123
      end
      object cxDBTextEdit4: TcxDBTextEdit
        Left = 11
        Top = 70
        DataBinding.DataField = 'Horsepower'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 2
        Width = 123
      end
      object cxDBTextEdit8: TcxDBTextEdit
        Left = 11
        Top = 46
        DataBinding.DataField = 'Cilinders'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 1
        Width = 123
      end
    end
    object GroupBox2: TcxGroupBox
      Left = 160
      Top = 64
      Caption = 'Transmission'
      TabOrder = 2
      Height = 110
      Width = 145
      object cxDBCheckBox: TcxDBCheckBox
        Left = 12
        Top = 70
        Caption = 'Automatic'
        DataBinding.DataField = 'Transmission Type'
        DataBinding.DataSource = dmGridCars.dsModels
        ParentBackground = False
        ParentColor = False
        Properties.ValueChecked = '1'
        Properties.ValueUnchecked = '2'
        Style.Color = 16247513
        TabOrder = 0
        Transparent = True
      end
      object cxDBListBox1: TcxDBListBox
        Left = 12
        Top = 22
        Width = 121
        Height = 45
        ItemHeight = 13
        Items.Strings = (
          '4'
          '5'
          '6')
        Style.Color = 16247513
        TabOrder = 1
        DataBinding.DataSource = dmGridCars.dsModels
        DataBinding.DataField = 'Transmission Speeds'
      end
    end
    object GroupBox3: TcxGroupBox
      Left = 312
      Top = 64
      Caption = 'Miles per gallon'
      TabOrder = 3
      Height = 110
      Width = 153
      object cxDBTextEdit7: TcxDBTextEdit
        Left = 13
        Top = 22
        DataBinding.DataField = 'MPG Highway'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 0
        Width = 127
      end
      object cxDBTextEdit6: TcxDBTextEdit
        Left = 13
        Top = 46
        DataBinding.DataField = 'MPG City'
        DataBinding.DataSource = dmGridCars.dsModels
        Style.Color = 16247513
        TabOrder = 1
        Width = 127
      end
    end
  end
end
