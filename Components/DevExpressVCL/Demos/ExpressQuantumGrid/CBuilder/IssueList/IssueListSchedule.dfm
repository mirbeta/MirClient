inherited frmSchedule: TfrmSchedule
  ClientHeight = 486
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Style.IsFontAssigned = True
  end
  inherited plTop: TPanel
    Height = 426
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 8
      Top = 24
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'SUNDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 0
      Width = 209
    end
    object cxDBTextEdit2: TcxDBTextEdit
      Left = 8
      Top = 64
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'MONDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 1
      Width = 209
    end
    object cxDBTextEdit3: TcxDBTextEdit
      Left = 8
      Top = 104
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'TUESDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 2
      Width = 209
    end
    object cxDBTextEdit4: TcxDBTextEdit
      Left = 8
      Top = 144
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'WEDNESDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 3
      Width = 209
    end
    object cxDBTextEdit5: TcxDBTextEdit
      Left = 8
      Top = 184
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'THURSDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 4
      Width = 209
    end
    object cxDBTextEdit6: TcxDBTextEdit
      Left = 8
      Top = 224
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'THURSDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 5
      Width = 209
    end
    object cxDBTextEdit7: TcxDBTextEdit
      Left = 8
      Top = 264
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'SATURDAY'
      DataBinding.DataSource = dmMain.dsScheduler
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 6
      Width = 209
    end
    object cxDBTextEdit8: TcxDBTextEdit
      Left = 8
      Top = 312
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'RowSum'
      DataBinding.DataSource = dmMain.dsScheduler
      Enabled = False
      Style.Color = clWindow
      Style.StyleController = dmMain.edstcMain
      TabOrder = 7
      Width = 209
    end
    object cxDBTextEdit9: TcxDBTextEdit
      Left = 8
      Top = 352
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'RowAvg'
      DataBinding.DataSource = dmMain.dsScheduler
      Enabled = False
      Style.Color = clWindow
      Style.StyleController = dmMain.edstcMain
      TabOrder = 8
      Width = 209
    end
    object Label1: TcxLabel
      Left = 6
      Top = 296
      Caption = 'Summary'
      ParentFont = False
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 6
      Top = 88
      Caption = 'Tuesday'
      Transparent = True
    end
    object Label3: TcxLabel
      Left = 6
      Top = 128
      Caption = 'Wednesday'
      Transparent = True
    end
    object Label4: TcxLabel
      Left = 6
      Top = 336
      Caption = 'Average'
      ParentFont = False
      Transparent = True
    end
    object Label5: TcxLabel
      Left = 6
      Top = 8
      Caption = 'Sunday'
      Transparent = True
    end
    object Label6: TcxLabel
      Left = 6
      Top = 168
      Caption = 'Thursday'
      Transparent = True
    end
    object Label7: TcxLabel
      Left = 6
      Top = 208
      Caption = 'Friday'
      Transparent = True
    end
    object Label8: TcxLabel
      Left = 6
      Top = 248
      Caption = 'Saturday'
      Transparent = True
    end
    object Label9: TcxLabel
      Left = 6
      Top = 48
      Caption = 'Monday'
      Transparent = True
    end
  end
end
