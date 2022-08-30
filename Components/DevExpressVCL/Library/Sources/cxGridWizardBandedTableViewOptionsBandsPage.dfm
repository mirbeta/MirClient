inherited cxGridWizardBandedTableViewOptionsBandsPageFrame: TcxGridWizardBandedTableViewOptionsBandsPageFrame
  inherited lcMain: TdxLayoutControl
    object lbBands: TcxListBox [0]
      Left = 10
      Top = 10
      Width = 277
      Height = 261
      DragMode = dmAutomatic
      ItemHeight = 13
      MultiSelect = True
      PopupMenu = pupmBands
      TabOrder = 0
      OnClick = lbBandsClick
      OnDragOver = lbBandsDragOver
      OnEndDrag = lbBandsEndDrag
    end
    object btnAddBand: TcxButton [1]
      Left = 10
      Top = 295
      Width = 90
      Height = 25
      Action = acAddBand
      TabOrder = 3
    end
    object btnDeleteSelectedBands: TcxButton [2]
      Left = 106
      Top = 295
      Width = 90
      Height = 25
      Action = acDeleteSelectedBands
      TabOrder = 4
    end
    object btnDeleteAllBands: TcxButton [3]
      Left = 202
      Top = 295
      Width = 90
      Height = 25
      Action = acDeleteAllBands
      TabOrder = 5
    end
    object btnEditBand: TcxButton [4]
      Left = 298
      Top = 295
      Width = 90
      Height = 25
      Action = acEditBand
      TabOrder = 6
    end
    object btnMoveUp: TcxButton [5]
      Left = 530
      Top = 121
      Width = 40
      Height = 25
      Action = acMoveUp
      TabOrder = 1
      OptionsImage.Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        00000000000000000000000000020000000A0000000F00000010000000100000
        000F0000000A0000000200000000000000000000000000000000000000000000
        00000000000000000000000000097E5237C1A8663FFFA7633DFFA6623BFFA560
        3AFF754429C10000000900000000000000000000000000000000000000000000
        000000000000000000000000000CB57852FFE6C69AFFE2BE8EFFE2BE8EFFE2BE
        8DFFA6613CFF0000000D00000000000000000000000000000000000000000000
        000000000000000000000000000CB67B55FFE6C79DFFDAAD73FFD9AD73FFE3C0
        8FFFA8653EFF0000000D00000000000000000000000000000000000000000000
        0002000000080000000B00000013B87F59FFE7C79DFFDBAF77FFDBAE77FFE4C1
        91FFA96842FF000000150000000C0000000A0000000300000000000000000000
        0007B77D56FFB67B54FFB47953FFB37751FFE5C495FFDCB279FFDCB17AFFE4C2
        95FFAB6B46FFAA6A44FFAA6842FFA86640FF0000000800000000000000000000
        00057E573DB5E7D3C4FFEACEA8FFE6C69AFFE5C598FFDDB37DFFDDB37DFFE5C4
        97FFE5C597FFE7C79CFFDDC5B7FF673923B60000000600000000000000000000
        0002140E0A24B78869F2F2E5D2FFE6C799FFE7C997FFEACF9FFFEACF9EFFE7C9
        97FFE3C18FFFEFDFCBFF9D6549F3100906260000000200000000000000000000
        0000000000034E372871D7B7A0FFF5E9CDFFEEDAABFFEDD7A8FFEDD7A8FFEED7
        A8FFF4E5C4FFC7A08CFF41261872000000040000000000000000000000000000
        0000000000010000000590684BC7EEE0D3FFF3E5BFFFF1DFB2FFF1DFB2FFF2E2
        B7FFE8D8CBFF7B4C30C700000006000000010000000000000000000000000000
        000000000000000000011D15102DC4997AFAFBF4E4FFF5E8C0FFF4E6BBFFF9F2
        E0FFB07D5FFA19100A2F00000001000000000000000000000000000000000000
        00000000000000000000000000025F453383DFC5B1FFFBF2D7FFFAF2D4FFD3B6
        A2FF533523830000000300000000000000000000000000000000000000000000
        000000000000000000000000000005030309A37C5CD8F4EBDFFFF1E6DAFF9362
        45D80403020A0000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000012E231A3FCFAB8EFDC09679FD291C
        1340000000010000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000174584298674731950000
        0001000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000001000000010000
        0000000000000000000000000000000000000000000000000000}
      PaintStyle = bpsGlyph
    end
    object btnMoveDown: TcxButton [6]
      Left = 530
      Top = 152
      Width = 40
      Height = 25
      Action = acMoveDown
      TabOrder = 2
      OptionsImage.Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000000000003000000090000000A0000
        0003000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000010000000D552C19A64E2916A40000
        000D000000010000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000623130B54A77053FD9E6443FD2011
        0A54000000060000000000000000000000000000000000000000000000000000
        000000000000000000000000000204020117814A2EDEDFBF9BFFD4AC7EFF7942
        26DF030201180000000200000000000000000000000000000000000000000000
        00000000000000000001000000094D2D1C90C69D7FFFDFB987FFDFB785FFBA87
        5FFF472617910000000A00000001000000000000000000000000000000000000
        00000000000000000004180F0A3AA87252FBE5C89FFFDCB27AFFDCB17BFFE0BC
        8EFF9C6140FB160C073B00000004000000000000000000000000000000000000
        0000000000010000000C7E4E32CCDDBFA0FFDFB783FFDDB37DFFDDB37DFFDFB6
        82FFD2AB80FF714027CD0000000D000000020000000000000000000000000000
        000000000006452C1E78C69D7FFFE4C394FFDEB680FFDFB680FFDEB67FFFDEB5
        7FFFE3BF8FFFB8855FFF3E241679000000070000000000000000000000000000
        0002120C0828AA7758F3E9CEAAFFE4C290FFE5C794FFE8CA98FFE8CA99FFE6C5
        93FFE3C18DFFE2C193FF996141F31009062A0000000300000000000000000000
        0006795339B6E0C7ADFFF9F1E0FFFAF2E2FFFAF2E2FFEBD3A3FFECD2A3FFF1DD
        B7FFF0DDB7FFF9F1E0FFDBBEA6FF6B4028B80000000700000000000000000000
        0006C1906AFFC18E68FFC08C66FFBE8A64FFFBF5E5FFEFDAACFFEFD9ACFFF3E2
        BEFFB9805AFFB77E58FFB77C55FFB57A54FF0000000800000000000000000000
        000200000006000000070000000DC08D68FFFBF7E9FFF2E1B4FFF2E1B4FFF5E9
        C5FFB27753FF0000001000000009000000070000000200000000000000000000
        0000000000000000000000000007C3926CFFFCF8EBFFF5E8BCFFF5E7BCFFF8EE
        CCFFB47C56FF0000000800000000000000000000000000000000000000000000
        0000000000000000000000000005C59570FFFDFAEDFFFDFAEDFFFDFAEDFFFDFA
        EDFFB67F5AFF0000000600000000000000000000000000000000000000000000
        000000000000000000000000000393755CBACDA37EFFCBA17DFFCAA07BFFC99D
        79FF8C6A50BB0000000400000000000000000000000000000000000000000000
        0000000000000000000000000001000000020000000400000004000000040000
        0004000000030000000100000000000000000000000000000000}
      PaintStyle = bpsGlyph
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      Index = -1
    end
    object lciBands: TdxLayoutItem
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Bands'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Parent = lcMainGroup1
      Control = lbBands
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciAddBand: TdxLayoutItem
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Parent = dxLayoutGroup2
      Control = btnAddBand
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciDeleteSelectedBands: TdxLayoutItem
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Parent = dxLayoutGroup2
      Control = btnDeleteSelectedBands
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciDeleteAllBands: TdxLayoutItem
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Parent = dxLayoutGroup2
      Control = btnDeleteAllBands
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup2: TdxLayoutGroup
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      Parent = lcMainGroup_Root
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciEditBand: TdxLayoutItem
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Parent = dxLayoutGroup2
      Control = btnEditBand
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciMoveUp: TdxLayoutItem
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Parent = lcMainGroup2
      Control = btnMoveUp
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMoveDown: TdxLayoutItem
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Parent = lcMainGroup2
      Control = btnMoveDown
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup1: TdxLayoutGroup
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      Parent = lcMainGroup_Root
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainGroup2: TdxLayoutGroup
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Text = 'Hidden Group'
      Parent = lcMainGroup1
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
  end
  object pupmBands: TPopupMenu
    Left = 36
    Top = 36
    object Add: TMenuItem
      Action = acAddBand
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object Deleteselected: TMenuItem
      Action = acDeleteSelectedBands
    end
    object Deleteall: TMenuItem
      Action = acDeleteAllBands
    end
    object Separator2: TMenuItem
      Caption = '-'
    end
    object Edit: TMenuItem
      Action = acEditBand
    end
    object Separator3: TMenuItem
      Caption = '-'
    end
    object MoveUp: TMenuItem
      Action = acMoveUp
    end
    object MoveDown: TMenuItem
      Action = acMoveDown
    end
  end
  object aclBands: TActionList
    Left = 4
    Top = 36
    object acAddBand: TAction
      Caption = '&Add'
      OnExecute = acAddBandExecute
    end
    object acDeleteSelectedBands: TAction
      Caption = '&Delete'
      OnExecute = acDeleteSelectedBandsExecute
    end
    object acDeleteAllBands: TAction
      Caption = 'De&lete all'
      OnExecute = acDeleteAllBandsExecute
    end
    object acEditBand: TAction
      Caption = '&Edit'
      OnExecute = acEditBandExecute
    end
    object acMoveUp: TAction
      Caption = 'Move &up'
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move do&wn'
      OnExecute = acMoveDownExecute
    end
  end
end
