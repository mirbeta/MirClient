object dlg_msi_Detail: Tdlg_msi_Detail
  Left = 710
  Top = 327
  ClientHeight = 522
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 480
    Width = 500
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel: TPanel
      Left = 410
      Top = 0
      Width = 90
      Height = 42
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object bOK: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Cursor = crHandPoint
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 480
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = ' '
    TabOrder = 1
    object pc: TPageControl
      Left = 10
      Top = 10
      Width = 480
      Height = 460
      ActivePage = TabSheet1
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = ' Properties '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 472
          Height = 432
          Align = alClient
          BevelInner = bvLowered
          BevelOuter = bvNone
          BorderWidth = 10
          Caption = ' '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Notebook: TNotebook
            Left = 11
            Top = 11
            Width = 450
            Height = 410
            Align = alClient
            PageIndex = 3
            TabOrder = 0
            object TPage
              Left = 0
              Top = 0
              Caption = 'Memo'
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Memo: TMemo
                Left = 0
                Top = 0
                Width = 450
                Height = 410
                Align = alClient
                BorderStyle = bsNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Courier New'
                Font.Style = []
                ParentColor = True
                ParentFont = False
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
                WordWrap = False
              end
            end
            object TPage
              Left = 0
              Top = 0
              Caption = 'CheckBox'
              ExplicitWidth = 0
              ExplicitHeight = 0
              object clb: TCheckListBox
                Left = 0
                Top = 0
                Width = 450
                Height = 403
                OnClickCheck = clbClickCheck
                Align = alClient
                BorderStyle = bsNone
                IntegralHeight = True
                ItemHeight = 13
                ParentColor = True
                TabOrder = 0
              end
            end
            object TPage
              Left = 0
              Top = 0
              Caption = 'Listbox'
              ExplicitWidth = 0
              ExplicitHeight = 0
              object lb: TListBox
                Left = 0
                Top = 0
                Width = 450
                Height = 410
                Align = alClient
                BorderStyle = bsNone
                ItemHeight = 13
                ParentColor = True
                TabOrder = 0
                OnDblClick = lbDblClick
              end
            end
            object TPage
              Left = 0
              Top = 0
              Caption = 'ListView'
              object lv: TListView
                Left = 0
                Top = 0
                Width = 450
                Height = 410
                Align = alClient
                BorderStyle = bsNone
                Columns = <
                  item
                    Caption = 'Variable'
                    Width = 150
                  end
                  item
                    AutoSize = True
                    Caption = 'Value'
                  end>
                ColumnClick = False
                HideSelection = False
                ReadOnly = True
                RowSelect = True
                ParentColor = True
                ParentShowHint = False
                ShowColumnHeaders = False
                ShowHint = True
                TabOrder = 0
                ViewStyle = vsReport
                OnAdvancedCustomDrawItem = lvAdvancedCustomDrawItem
                OnAdvancedCustomDrawSubItem = lvAdvancedCustomDrawSubItem
                OnDblClick = lvDblClick
              end
            end
          end
        end
      end
    end
  end
end
