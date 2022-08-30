object SampleDockingContainersForm: TSampleDockingContainersForm
  Left = 270
  Top = 145
  Width = 534
  Height = 482
  Caption = 'SampleDockingContainersForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 8
    Top = 8
    Width = 300
    Height = 432
    DockType = 0
    OriginalWidth = 300
    OriginalHeight = 432
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 300
      Height = 432
      DockType = 1
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxTabContainerDockSite2: TdxTabContainerDockSite
      Left = 0
      Top = 0
      Width = 300
      Height = 432
      ActiveChildIndex = 1
      AllowFloating = True
      AutoHide = False
      DockType = 1
      OriginalWidth = 175
      OriginalHeight = 88
      object dxDockPanel2: TdxDockPanel
        Left = 0
        Top = 0
        Width = 296
        Height = 380
        AllowFloating = True
        AutoHide = False
        Caption = 'dxDockPanel2'
        DockType = 1
        OriginalWidth = 145
        OriginalHeight = 50
      end
      object dxVertContainerDockSite1: TdxVertContainerDockSite
        Left = 0
        Top = 0
        Width = 296
        Height = 380
        ActiveChildIndex = -1
        AllowFloating = True
        DockType = 1
        OriginalWidth = 145
        OriginalHeight = 50
        object dxDockPanel4: TdxDockPanel
          Left = 0
          Top = 0
          Width = 296
          Height = 193
          AllowFloating = True
          AutoHide = False
          Caption = 'dxDockPanel4'
          DockType = 3
          OriginalWidth = 145
          OriginalHeight = 193
        end
        object dxDockPanel3: TdxDockPanel
          Left = 0
          Top = 193
          Width = 296
          Height = 187
          AllowFloating = True
          AutoHide = False
          Caption = 'dxDockPanel3'
          DockType = 3
          OriginalWidth = 129
          OriginalHeight = 187
        end
      end
    end
  end
  object cxPropertiesStore2: TcxPropertiesStore
    Components = <
      item
        Component = dxDockPanel2
        Properties.Strings = (
          'AllowDock'
          'AllowDockClients'
          'AllowFloating'
          'AutoHide'
          'Caption'
          'CaptionButtons'
          'Color'
          'Cursor'
          'Dockable'
          'Font'
          'Height'
          'HelpContext'
          'HelpKeyword'
          'Hint'
          'ImageIndex'
          'Left'
          'ManagerColor'
          'ManagerFont'
          'Name'
          'ParentColor'
          'ParentFont'
          'ParentShowHint'
          'PopupMenu'
          'ShowCaption'
          'ShowHint'
          'Tag'
          'Top'
          'Width')
      end
      item
        Component = dxDockSite1
        Properties.Strings = (
          'Align'
          'AllowDockClients'
          'Anchors'
          'Color'
          'Cursor'
          'Font'
          'Height'
          'HelpContext'
          'HelpKeyword'
          'Hint'
          'Left'
          'ManagerColor'
          'ManagerFont'
          'Name'
          'ParentColor'
          'ParentFont'
          'ParentShowHint'
          'PopupMenu'
          'ShowHint'
          'Tag'
          'Top'
          'Visible'
          'Width')
      end>
    StorageName = 'cxPropertiesStore2'
    Left = 16
    Top = 16
  end
end
