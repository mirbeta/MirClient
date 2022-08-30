object dmCommonData: TdmCommonData
  OldCreateOrder = False
  Height = 435
  Width = 789
  object alMain: TActionList
    Left = 96
    Top = 208
    object actSBarsHelp: TAction
      Category = 'Help'
      Caption = 'Express SideBar Help'
      Hint = 'Express &SideBar Help'
      ImageIndex = 0
      OnExecute = actSBarsHelpExecute
    end
    object actBarsHelp: TAction
      Category = 'Help'
      Caption = 'Express&Bars Help'
      Hint = 'Starts the ExpressBars help system'
      ImageIndex = 0
      OnExecute = actBarsHelpExecute
    end
    object actDockingHelp: TAction
      Category = 'Help'
      Caption = 'Express D&ocking Library Help'
      Hint = 'Starts the Express Docking Library help system'
      ImageIndex = 0
      OnExecute = actDockingHelpExecute
    end
    object actRateDemo: TAction
      Category = 'Help'
      Caption = '&Rate this demo...'
      Hint = 'Send feedback to Developer Express'
      ImageIndex = 1
      OnExecute = actRateDemoExecute
    end
    object actMyDX: TAction
      Category = 'Help'
      Caption = 'DevExpress Client C&enter'
      Hint = 'Launches the Developer Express Client Center webpage'
      ImageIndex = 2
      OnExecute = actMyDXExecute
    end
    object actDownloads: TAction
      Category = 'Help'
      Caption = 'Developer Express &Downloads'
      Hint = 
        'Launches the webpage with the list of available downloads of Dev' +
        'eloper Express products'
      ImageIndex = 2
      OnExecute = actDownloadsExecute
    end
    object actSupport: TAction
      Category = 'Help'
      Caption = 'DevExpress Support &Center'
      Hint = 'Launches the webpage with the Developer Express Support Center'
      ImageIndex = 2
      OnExecute = actSupportExecute
    end
    object actProducts: TAction
      Category = 'Help'
      Caption = 'Developer Express &Products'
      Hint = 'Launches the webpage with the list of Developer Express products'
      ImageIndex = 2
      OnExecute = actProductsExecute
    end
    object actDXOnTheWeb: TAction
      Category = 'Help'
      Caption = 'Developer Express on the &Web'
      Hint = 'Launches the official website of Developer Express'
      ImageIndex = 3
      OnExecute = actDXOnTheWebExecute
    end
    object actAbout: TAction
      Category = 'Options'
      Caption = '&About'
      Checked = True
      Hint = 'Display the description of the current demo'
      OnExecute = actAboutExecute
    end
  end
end
