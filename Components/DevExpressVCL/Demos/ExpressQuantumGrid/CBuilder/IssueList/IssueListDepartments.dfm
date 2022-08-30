inherited frmDepartments: TfrmDepartments
  ClientWidth = 226
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Style.IsFontAssigned = True
    Width = 226
  end
  inherited plTop: TPanel
    Width = 226
    ParentColor = True
    object Label1: TcxLabel
      Left = 8
      Top = 7
      Caption = 'Name'
      Transparent = True
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 8
      Top = 24
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'NAME'
      DataBinding.DataSource = dmMain.dsDepartment
      Style.StyleController = dmMain.edstcMain
      TabOrder = 0
      Width = 211
    end
  end
  inherited cxNavigator: TcxNavigator
    Width = 216
  end
end
