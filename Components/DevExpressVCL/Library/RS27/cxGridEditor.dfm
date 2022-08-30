object cxGridEditor: TcxGridEditor
  Left = 457
  Top = 105
  Width = 680
  Height = 450
  HorzScrollBar.Range = 4
  VertScrollBar.Range = 34
  BorderIcons = [biSystemMenu]
  Caption = 'Editing'
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 680
  ParentFont = True
  OldCreateOrder = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 656
    Height = 372
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 240
      Top = 0
      Width = 8
      Height = 372
    end
    object Panel4: TPanel
      Left = 248
      Top = 0
      Width = 408
      Height = 372
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel11: TPanel
        Left = 0
        Top = 8
        Width = 408
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 1
        Color = clBtnShadow
        TabOrder = 0
        object Panel12: TPanel
          Left = 1
          Top = 1
          Width = 406
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          Color = clInfoBk
          TabOrder = 0
          object Label1: TLabel
            Left = 7
            Top = 4
            Width = 26
            Height = 13
            Caption = 'View:'
            Transparent = True
          end
          object LSelectedView: TLabel
            Left = 38
            Top = 4
            Width = 72
            Height = 13
            Caption = '<AViewName>'
            Transparent = True
          end
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 408
        Height = 8
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object Panel13: TPanel
        Left = 0
        Top = 31
        Width = 408
        Height = 8
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
      object Panel15: TPanel
        Left = 0
        Top = 39
        Width = 408
        Height = 333
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 1
        Color = clBtnShadow
        Constraints.MinWidth = 200
        TabOrder = 3
        object PViewFrame: TPanel
          Left = 1
          Top = 1
          Width = 406
          Height = 331
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object PLeft: TPanel
      Left = 0
      Top = 0
      Width = 240
      Height = 372
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Panel7: TPanel
        Left = 0
        Top = 8
        Width = 8
        Height = 364
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 240
        Height = 8
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object Panel14: TPanel
        Left = 8
        Top = 8
        Width = 232
        Height = 364
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 1
        Color = clBtnShadow
        Constraints.MinHeight = 200
        Constraints.MinWidth = 189
        TabOrder = 2
        object PageControl1: TcxPageControl
          Left = 1
          Top = 1
          Width = 230
          Height = 362
          Align = alClient
          TabOrder = 0
          Properties.ActivePage = tsLevels
          Properties.CustomButtons.Buttons = <>
          ClientRectBottom = 362
          ClientRectRight = 230
          ClientRectTop = 24
          object tsLevels: TcxTabSheet
            BorderWidth = 8
            Caption = '  Structure  '
            Color = clBtnFace
            ParentColor = False
            object PLevels: TPanel
              Left = 0
              Top = 0
              Width = 214
              Height = 288
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 1
              Color = clBtnShadow
              TabOrder = 0
            end
            object Panel6: TPanel
              Left = 0
              Top = 288
              Width = 214
              Height = 34
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 1
              object BAddLevel: TcxButton
                Left = 1
                Top = 8
                Width = 80
                Height = 24
                Caption = '&Add Level'
                TabOrder = 0
                OnClick = BAddLevelClick
              end
              object BDeleteLevel: TcxButton
                Left = 90
                Top = 8
                Width = 80
                Height = 24
                Caption = '&Delete Level'
                TabOrder = 1
                OnClick = BDeleteLevelClick
              end
            end
          end
          object TabSheet2: TcxTabSheet
            BorderWidth = 8
            Caption = '  Views  '
            Color = clBtnFace
            ImageIndex = 1
            ParentColor = False
            object PViews: TPanel
              Left = 0
              Top = 0
              Width = 214
              Height = 258
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 1
              Color = clBtnShadow
              TabOrder = 0
              object LBViews: TListBox
                Left = 1
                Top = 1
                Width = 212
                Height = 256
                Align = alClient
                BorderStyle = bsNone
                ItemHeight = 13
                MultiSelect = True
                PopupMenu = PMViewList
                TabOrder = 0
                OnClick = LBViewsClick
                OnKeyPress = GridStructureControlKeyPress
              end
            end
            object Panel9: TPanel
              Left = 0
              Top = 258
              Width = 214
              Height = 64
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 1
              object BAddView: TcxButton
                Left = 1
                Top = 8
                Width = 80
                Height = 24
                Caption = 'Add &View...'
                TabOrder = 0
                OnClick = BAddViewClick
              end
              object BDeleteView: TcxButton
                Left = 90
                Top = 8
                Width = 80
                Height = 24
                Caption = 'D&elete View'
                TabOrder = 1
                OnClick = BDeleteViewClick
              end
              object BEditView: TcxButton
                Left = 1
                Top = 40
                Width = 169
                Height = 24
                Caption = 'Edit View &Layout and Data...'
                TabOrder = 2
                OnClick = miEditLayoutClick
              end
            end
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 372
    Width = 664
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 487
      Top = 0
      Width = 177
      Height = 40
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BClose: TcxButton
        Left = 89
        Top = 8
        Width = 80
        Height = 24
        Cancel = True
        Caption = '&Close'
        TabOrder = 0
        OnClick = BCloseClick
      end
    end
  end
  object Panel10: TPanel
    Left = 656
    Top = 0
    Width = 8
    Height = 372
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
  object PMGridStructureControl: TPopupMenu
    Left = 80
    Top = 64
  end
  object PMViews: TPopupMenu
    Left = 80
    Top = 96
  end
  object PMViewList: TPopupMenu
    Left = 80
    Top = 128
    object miDeleteView: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = BDeleteViewClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miCustomizeEditFormLayout: TMenuItem
      Caption = 'Customize Edit Form Layout...'
      OnClick = miCustomizeEditFormLayoutClick
    end
    object miEditLayout: TMenuItem
      Caption = '&Edit Layout and Data...'
      OnClick = miEditLayoutClick
    end
  end
end
