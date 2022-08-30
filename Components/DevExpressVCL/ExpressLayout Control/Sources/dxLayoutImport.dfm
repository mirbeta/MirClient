object fmImport: TfmImport
  Left = 143
  Top = 181
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Import'
  ClientHeight = 257
  ClientWidth = 569
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 569
    Height = 257
    TabOrder = 0
    TabStop = False
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel2
    object seDeltaX: TcxSpinEdit
      Left = 45
      Top = 136
      Properties.MaxValue = 255.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 4
      Value = 1
      Width = 51
    end
    object seDeltaY: TcxSpinEdit
      Left = 194
      Top = 136
      Properties.MaxValue = 255.000000000000000000
      Properties.MinValue = 1.000000000000000000
      TabOrder = 5
      Value = 1
      Width = 51
    end
    object cbConvertPageControls: TcxCheckBox
      Left = 10
      Top = 82
      Caption = 'Import page controls as tabbed groups'
      State = cbsChecked
      TabOrder = 2
      Width = 311
    end
    object btnImport: TcxButton
      Left = 121
      Top = 163
      Width = 97
      Height = 25
      Caption = '&Import'
      ModalResult = 1
      TabOrder = 6
    end
    object btnCancel: TcxButton
      Left = 224
      Top = 163
      Width = 97
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 7
    end
    object cbContainers: TcxComboBox
      Left = 10
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.DropDownRows = 16
      TabOrder = 0
      Width = 311
    end
    object cbUseLabeledItems: TcxCheckBox
      Left = 10
      Top = 55
      Caption = 'Import standalone labels as label items'
      State = cbsChecked
      TabOrder = 1
      Width = 311
    end
    object cbAssociate: TcxCheckBox
      Left = 10
      Top = 109
      Caption = 'Link labels to controls within the range:'
      State = cbsChecked
      TabOrder = 3
      OnClick = cbAssociateClick
      Width = 311
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      object dxLayoutControl1Group4: TdxLayoutGroup
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        object dxLayoutControl1Item6: TdxLayoutItem
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'Choose a control to import data from:'
          CaptionOptions.Layout = clTop
          Control = cbContainers
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Item7: TdxLayoutItem
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'cbUseLabeledItems'
          CaptionOptions.Visible = False
          Control = cbUseLabeledItems
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Item3: TdxLayoutItem
          AlignHorz = ahClient
          AlignVert = avTop
          CaptionOptions.Text = 'pixels down'
          CaptionOptions.Visible = False
          CaptionOptions.Layout = clTop
          Control = cbConvertPageControls
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Item8: TdxLayoutItem
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'cbAssociate'
          CaptionOptions.Visible = False
          Control = cbAssociate
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Group5: TdxLayoutGroup
          Offsets.Left = 20
          ButtonOptions.Buttons = <>
          Hidden = True
          LayoutDirection = ldHorizontal
          ShowBorder = False
          object dxLayoutControl1Item1: TdxLayoutItem
            AlignHorz = ahLeft
            AlignVert = avTop
            CaptionOptions.Text = 'X:'
            Control = seDeltaX
            ControlOptions.ShowBorder = False
          end
          object dxLayoutControl1LabeledItem1: TdxLayoutLabeledItem
            AlignVert = avCenter
            CaptionOptions.Text = 'pixels wide OR'
          end
          object dxLayoutControl1Item2: TdxLayoutItem
            AlignHorz = ahLeft
            AlignVert = avTop
            CaptionOptions.Text = 'Y:'
            Control = seDeltaY
            ControlOptions.ShowBorder = False
          end
          object dxLayoutControl1LabeledItem2: TdxLayoutLabeledItem
            AlignVert = avCenter
            CaptionOptions.Text = 'pixels down'
          end
        end
      end
      object dxLayoutControl1Group2: TdxLayoutGroup
        AlignHorz = ahRight
        AlignVert = avBottom
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        object dxLayoutControl1Item4: TdxLayoutItem
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'btnImport'
          CaptionOptions.Visible = False
          Control = btnImport
          ControlOptions.ShowBorder = False
        end
        object dxLayoutControl1Item5: TdxLayoutItem
          AlignHorz = ahLeft
          AlignVert = avTop
          CaptionOptions.Text = 'btnCancel'
          CaptionOptions.Visible = False
          Control = btnCancel
          ControlOptions.ShowBorder = False
        end
      end
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
    end
  end
end
