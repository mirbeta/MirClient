{===============================================================================
  RaizeComponentsVcl_Reg Unit

  Raize Components - Registration Unit (Non-Data-Aware Components)

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Modification History
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added code for displaying RC icon on Delphi 2005 Splash Screen.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added registration code for new TRzStringGrid component.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Restructred registration process. Separate Register* procedures are called
      for each palette page.
===============================================================================}

{$I RzComps.inc}
{$WARN SYMBOL_DEPRECATED OFF}

unit RaizeComponentsVcl_Reg;

interface

{$IFDEF WIN32}
procedure Register;
{$ENDIF}

implementation

uses
  Forms,
  Classes,
  Controls,
  SysUtils,
  StdCtrls,
  Buttons,
  Dialogs,
  Graphics,
  ImgList,
  Registry,

  {$IFDEF WIN32}
  ToolsAPI,
  DesignIntf,
  TreeIntf,
  VCLEditors,
  FiltEdit,
  
  { Design Editors }
  RzDesignEditors,
  RzComboBoxEditor,
  RzLabelEditor,
  RzPanelEditor,
  RzSplitterEditor,
  RzToolbarEditor,
  RzTabStopEditor,
  RzCheckListEditor,
  RzCheckListTabStopEditor,
  RzColorNamesEditor,
  RzButtonEditor,
  RzRadioGroupEditor,
  RzCheckGroupEditor,
  RzSelectImageEditor,
  RzGroupBarEditor,
  RzPathBarEditor,
  RzImageListEditor,
  RzShellDesignEditors,
  RzStringListEditor,
  RzCaptionEditor,
  RzAboutEditor,
  {$ENDIF}
  
  { Component Units }
  RzCommon,
  RzGrafx,
  RzPanel,
  RzDlgBtn,
  RzBorder,
  RzLabel,
  RzTrkBar,
  RzSplit,
  RzLstBox,
  RzChkLst,
  RzCmboBx,
  RzEdit,
  RzPrgres,
  RzStatus,
  RzSelDir,
  RzFilSys,
  RzRadGrp,
  RzLookup,
  RzBtnEdt,
  RzSpnEdt,
  RzRadChk,
  RzLaunch,
  RzSndMsg,
  RzButton,
  RzBmpBtn,
  RzBHints,
  RzBckgnd,
  RzListVw,
  RzTreeVw,
  RzAnimtr,
  RzDTP,
  RzTray,
  RzForms,
  RzLine,
  RzTabs,
  RzGroupBar,
  RzPathBar,
  RzPopups,
  RzShellCtrls,
  RzShellDialogs,
  RzShellUtils,
  RzGrids;
     

{$IFDEF WIN32}

{$R RaizeComponentsSplash.res}

{===================================}
{== RegisterRaizePanels Procedure ==}
{===================================}

procedure RegisterRaizePanels( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzPanel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzPanel ] );
    RegisterComponentEditor( TRzPanel, TRzPanelEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzGroupBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzGroupBar ] );
    RegisterComponentEditor( TRzGroupBar, TRzGroupBarEditor );
    RegisterComponentEditor( TRzGroup, TRzGroupEditor );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroup, 'CaptionImageIndex', TRzGroupCaptionImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroupItem, 'ImageIndex', TRzGroupItemImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroupItem, 'DisabledIndex', TRzGroupItemImageIndexProperty );

    RegisterComponents( PalettePage, [ TRzGroupTemplate ] );
    RegisterComponentEditor( TRzGroupTemplate, TRzGroupTemplateEditor );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroupTemplate, 'CaptionImageIndex', TRzGroupTemplateCaptionImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroupTemplateItem, 'ImageIndex', TRzGroupTemplateItemImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGroupTemplateItem, 'DisabledIndex', TRzGroupTemplateItemImageIndexProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzPageControl', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzPageControl ] );
    RegisterComponentEditor( TRzPageControl, TRzPageControlEditor );
    RegisterComponentEditor( TRzTabSheet, TRzPageControlEditor );

    RegisterPropertyEditor( TypeInfo( TRzTabSheet ), TRzPageControl, 'ActivePage', TRzActivePageProperty );
    RegisterPropertyEditor( TypeInfo( TRzTabSheet ), TRzPageControl, 'ActivePageDefault', TRzActivePageProperty );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzTabSheet, 'ImageIndex', TRzTabSheetImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzTabSheet, 'DisabledIndex', TRzTabSheetImageIndexProperty );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzTabData, 'ImageIndex', TRzImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzTabData, 'DisabledIndex', TRzImageIndexProperty );

    RegisterSprigType( TRzPageControl, TRzPageControlSprig );
    RegisterSprigType( TRzTabSheet, TRzTabSheetSprig );
  end;

  if R.ReadBool( RegisterSection, 'TRzTabControl', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTabControl ] );
    RegisterComponentEditor( TRzTabControl, TRzTabControlEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSplitter', True ) then
  begin
    RegisterNoIcon( [ TRzSplitterPane ] );
    RegisterComponents( PalettePage, [ TRzSplitter ] );
    RegisterComponentEditor( TRzSplitter, TRzSplitterEditor );

    RegisterSprigType( TRzSplitter, TRzSplitterSprig );
    RegisterSprigType( TRzSplitterPane, TRzSplitterPaneSprig );
    RegisterComponentEditor( TRzSplitterPane, TRzComponentEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSizePanel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSizePanel ] );
    RegisterComponentEditor( TRzSizePanel, TRzSizePanelEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzToolbar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzToolbar ] );
    RegisterComponentEditor( TRzToolbar, TRzToolbarEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzStatusBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzStatusBar ] );
    RegisterComponentEditor( TRzStatusBar, TRzStatusBarEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzPathBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzPathBar ] );
    RegisterComponentEditor( TRzPathBar, TRzPathBarEditor );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzPathItem, 'ImageIndex', TRzPathItemImageIndexProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzFlowPanel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFlowPanel ] );
    RegisterComponentEditor( TRzFlowPanel, TRzFlowPanelEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzGridPanel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzGridPanel ] );
    RegisterComponentEditor( TRzGridPanel, TRzGridPanelEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzGroupBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzGroupBox ] );
    RegisterComponentEditor( TRzGroupBox, TRzGroupBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzRadioGroup', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzRadioGroup ] );
    RegisterComponentEditor( TRzRadioGroup, TRzRadioGroupEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzCheckGroup', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCheckGroup ] );
    RegisterComponentEditor( TRzCheckGroup, TRzCheckGroupEditor );
  end;

end; {= RegisterRaizePanels =}


{==================================}
{== RegisterRaizeEdits Procedure ==}
{==================================}

procedure RegisterRaizeEdits( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzEdit ] );
    RegisterComponentEditor( TRzCustomEdit, TRzEditControlEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzMaskEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMaskEdit ] );
    // Inherits component editor from TRzCustomEdit
  end;

  if R.ReadBool( RegisterSection, 'TRzButtonEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzButtonEdit ] );
    RegisterComponentEditor( TRzButtonEdit, TRzButtonEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDateTimeEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDateTimeEdit ] );
    RegisterComponentEditor( TRzDateTimeEdit, TRzDateTimeEditEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDateTimeEdit, 'HowToUseMsg', TRzHintProperty );
    RegisterPropertyEditor( TypeInfo( string ), TRzDateTimeEdit, 'Format', TRzDateTimeFormatProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzNumericEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzNumericEdit ] );
    RegisterComponentEditor( TRzNumericEdit, TRzNumericEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSpinEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSpinEdit ] );
    RegisterComponentEditor( TRzSpinEdit, TRzSpinEditEditor );
    RegisterPropertyEditor( TypeInfo( Extended ), TRzSpinEdit, 'Value', TRzSpinValueProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzColorEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzColorEdit ] );
    RegisterComponentEditor( TRzColorEdit, TRzColorEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzExpandEdit', True ) then
    RegisterComponents( PalettePage, [ TRzExpandEdit ] );

  if R.ReadBool( RegisterSection, 'TRzHotKeyEdit', True ) then
    RegisterComponents( PalettePage, [ TRzHotKeyEdit ] );

  if R.ReadBool( RegisterSection, 'TRzMemo', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMemo ] );
    RegisterComponentEditor( TRzMemo, TRzMemoEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzRichEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzRichEdit ] );
    RegisterComponentEditor( TRzRichEdit, TRzRichEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSpinner', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSpinner ] );
    RegisterComponentEditor( TRzSpinner, TRzSpinnerEditor );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzSpinner, 'ImageIndexPlus', TRzImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzSpinner, 'ImageIndexMinus', TRzImageIndexProperty );

    RegisterPropertyEditor( TypeInfo( TBitmap ), TRzSpinner, 'GlyphMinus', TRzSpinnerGlyphProperty );
    RegisterPropertyEditor( TypeInfo( TBitmap ), TRzSpinner, 'GlyphPlus', TRzSpinnerGlyphProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzTrackBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTrackBar ] );
    RegisterComponentEditor( TRzTrackBar, TRzTrackBarEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDateTimePicker', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDateTimePicker ] );
    RegisterPropertyEditor( TypeInfo( string ), TRzDateTimePicker, 'Format', TRzDTPFormatProperty );
  end;
end; {= RegisterRaizeEdits =}


{==================================}
{== RegisterRaizeLists Procedure ==}
{==================================}

procedure RegisterRaizeLists( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzListBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzListBox ] );
    RegisterComponentEditor( TRzListBox, TRzListBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzRankListBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzRankListBox ] );
    RegisterComponentEditor( TRzRankListBox, TRzRankListBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzTabbedListBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTabbedListBox ] );
    RegisterComponentEditor( TRzTabbedListBox, TRzTabbedListBoxEditor );
    RegisterPropertyEditor( TypeInfo( TRzTabStopList ), TRzTabbedListBox, 'TabStops', TRzTabStopProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzCheckList', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCheckList ] );
    RegisterComponentEditor( TRzCheckList, TRzCheckListEditor );
    RegisterPropertyEditor( TypeInfo( TStrings ), TRzCheckList, 'Items', TRzCheckListProperty );
    RegisterPropertyEditor( TypeInfo( TRzTabStopList ), TRzCheckList, 'TabStops', TRzCheckListTabStopProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzEditListBox', True ) then
    RegisterComponents( PalettePage, [ TRzEditListBox ] );

  if R.ReadBool( RegisterSection, 'TRzFontListBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFontListBox ] );
    RegisterComponentEditor( TRzFontListBox, TRzFontListEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzComboBox ] );
    RegisterComponentEditor( TRzComboBox, TRzComboBoxEditor );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzComboBox, 'Text', TRzComboBoxTextProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzImageComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzImageComboBox ] );
    RegisterComponentEditor( TRzImageComboBox, TRzImageComboBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzFontComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFontComboBox ] );
    RegisterComponentEditor( TRzFontComboBox, TRzFontListEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzFontComboBox, 'FontName', TFontNameProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzMRUComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMRUComboBox ] );
    RegisterComponentEditor( TRzMRUComboBox, TRzMRUComboBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzColorComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzColorComboBox ] );
    RegisterComponentEditor( TRzColorComboBox, TRzColorNamesEditor );
    RegisterPropertyEditor( TypeInfo( TRzColorNames ), TRzColorComboBox, 'ColorNames', TRzColorNamesProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzTreeView', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTreeView ] );
    RegisterComponentEditor( TRzCustomTreeView, TRzTreeViewEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzCheckTree', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCheckTree ] );
    RegisterComponentEditor( TRzCheckTree, TRzCheckTreeEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzListView', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzListView ] );
    RegisterComponentEditor( TRzListView, TRzListViewEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzStringGrid', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzStringGrid ] );
    RegisterComponentEditor( TRzStringGrid, TRzStringGridEditor );
  end;

end; {= RegisterRaizeLists =}


{====================================}
{== RegisterRaizeButtons Procedure ==}
{====================================}

procedure RegisterRaizeButtons( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzButton ] );
    RegisterComponentEditor( TRzButton, TRzButtonEditor  );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzButton, 'Caption', TRzCaptionProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzBitBtn', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzBitBtn ] );
    RegisterComponentEditor( TRzBitBtn, TRzBitBtnEditor );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzBitBtn, 'ImageIndex', TRzImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzBitBtn, 'DisabledIndex', TRzImageIndexProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzMenuButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMenuButton ] );
    RegisterComponentEditor( TRzMenuButton, TRzMenuButtonEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDialogButtons', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDialogButtons ] );
    RegisterComponentEditor( TRzDialogButtons, TRzDialogButtonsEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSpinButtons', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSpinButtons ] );
    RegisterComponentEditor( TRzSpinButtons, TRzSpinButtonsEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzRapidFireButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzRapidFireButton ] );
    RegisterComponentEditor( TRzRapidFireButton, TRzRapidFireButtonEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzCheckBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCheckBox ] );
    RegisterComponentEditor( TRzCheckBox, TRzCheckBoxEditor );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzCheckBox, 'Caption', TRzCaptionProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzRadioButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzRadioButton ] );
    RegisterComponentEditor( TRzRadioButton, TRzRadioButtonEditor );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzRadioButton, 'Caption', TRzCaptionProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzToolButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzToolButton ] );
    RegisterComponentEditor( TRzToolButton, TRzToolButtonEditor );

    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzToolButton, 'ImageIndex', TRzToolButtonImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzToolButton, 'DownIndex', TRzToolButtonImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzToolButton, 'DisabledIndex', TRzToolButtonImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzToolButton, 'HotIndex', TRzToolButtonImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzToolButton, 'Caption', TRzCaptionProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzSpacer', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSpacer ] );
    RegisterComponentEditor( TRzSpacer, TRzSpacerEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzToolbarButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzToolbarButton ] );
    RegisterComponentEditor( TRzToolbarButton, TRzSelectImageEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzMenuToolbarButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMenuToolbarButton ] );
    RegisterComponentEditor( TRzMenuToolbarButton, TRzSelectImageEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzShapeButton', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzShapeButton ] );
    RegisterComponentEditor( TRzShapeButton, TRzShapeButtonEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzBmpButton', True ) then
    RegisterComponents( PalettePage, [ TRzBmpButton ] );
end; {= RegisterRaizeButtons =}


{====================================}
{== RegisterRaizeDisplay Procedure ==}
{====================================}

procedure RegisterRaizeDisplay( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzFrameController', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFrameController ] );
    RegisterComponentEditor( TRzFrameController, TRzFrameControllerEditor );
    GroupDescendentsWith( TRzFrameController, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzGroupController', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzGroupController ] );
    RegisterComponentEditor( TRzGroupController, TRzGroupControllerEditor );
  end;
      
  if R.ReadBool( RegisterSection, 'TRzMenuController', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMenuController ] );
  end;

  if R.ReadBool( RegisterSection, 'TRzLabel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzLabel ] );
    RegisterComponentEditor( TRzLabel, TRzLabelEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzURLLabel', True ) then
    RegisterComponents( PalettePage, [ TRzURLLabel ] );

  if R.ReadBool( RegisterSection, 'TRzBorder', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzBorder ] );
    RegisterComponentEditor( TRzBorder, TRzBorderEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzLine', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzLine ] );
    RegisterComponentEditor( TRzLine, TRzLineEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzSeparator', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSeparator ] );
    RegisterComponentEditor( TRzSeparator, TRzSeparatorEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzStatusPane', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzStatusPane ] );
    RegisterComponentEditor( TRzStatusPane, TRzStatusPaneEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzFieldStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFieldStatus ] );
    RegisterComponentEditor( TRzFieldStatus, TRzStatusPaneEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzGlyphStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzGlyphStatus ] );
    RegisterComponentEditor( TRzGlyphStatus, TRzGlyphStatusEditor );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGlyphStatus, 'ImageIndex', TRzImageIndexProperty );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzGlyphStatus, 'DisabledIndex', TRzImageIndexProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzProgressStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzProgressStatus ] );
    RegisterComponentEditor( TRzProgressStatus, TRzProgressStatusEditor );
  end;
  
  if R.ReadBool( RegisterSection, 'TRzMarqueeStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzMarqueeStatus ] );
    RegisterComponentEditor( TRzMarqueeStatus, TRzMarqueeStatusEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzClockStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzClockStatus ] );
    RegisterComponentEditor( TRzClockStatus, TRzClockStatusEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzClockStatus, 'Format', TRzClockStatusFormatProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzKeyStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzKeyStatus ] );
    RegisterComponentEditor( TRzKeyStatus, TRzKeyStatusEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzVersionInfoStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzVersionInfoStatus ] );
    RegisterComponentEditor( TRzVersionInfoStatus, TRzVersionInfoStatusEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzResourceStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzResourceStatus ] );
    RegisterComponentEditor( TRzResourceStatus, TRzResourceStatusEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzProgressBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzProgressBar ] );
    RegisterComponentEditor( TRzProgressBar, TRzProgressBarEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzProgressDisplay', True ) then
    RegisterComponents( PalettePage, [ TRzProgressDisplay ] );

  if R.ReadBool( RegisterSection, 'TRzMeter', True ) then
    RegisterComponents( PalettePage, [ TRzMeter ] );

  if R.ReadBool( RegisterSection, 'TRzLEDDisplay', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzLEDDisplay ] );
    RegisterComponentEditor( TRzLEDDisplay, TRzLEDDisplayEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzBackground', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzBackground ] );
    RegisterComponentEditor( TRzBackground, TRzBackgroundEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzAnimator', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzAnimator ] );
    RegisterComponentEditor( TRzAnimator, TRzAnimatorEditor );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzAnimator, 'ImageIndex', TRzAnimatorImageIndexProperty );
  end;
end; {= RegisterRaizeDisplay =}


{==================================}
{== RegisterRaizeShell Procedure ==}
{==================================}

procedure RegisterRaizeShell( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzShellTree', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzShellTree ] );

    RegisterComponentEditor( TRzShellTree, TRzShellTreeEditor );
    RegisterPropertyEditor( TypeInfo( TRzShellLocator ), nil, '', TRzShellLocatorProperty );
    RegisterPropertyEditor( TypeInfo( TCSIDL ), nil, '', TCSIDLProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzShellList', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzShellList ] );
    RegisterComponentEditor( TRzShellList, TRzShellListEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzShellCombo', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzShellCombo ] );
    RegisterComponentEditor( TRzShellCombo, TRzShellComboEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzOpenDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzOpenDialog ] );
    RegisterComponentEditor( TRzOpenDialog, TRzShellDialogEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzOpenDialog, 'Filter', TFilterProperty );
    GroupDescendentsWith( TRzOpenDialog, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzSaveDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSaveDialog ] );
    RegisterComponentEditor( TRzSaveDialog, TRzShellDialogEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzSaveDialog, 'Filter', TFilterProperty );
    GroupDescendentsWith( TRzSaveDialog, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzSelectFolderDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSelectFolderDialog ] );
    RegisterComponentEditor( TRzSelectFolderDialog, TRzShellDialogEditor );
    GroupDescendentsWith( TRzSelectFolderDialog, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzDirectoryTree', True ) then
    RegisterComponents( PalettePage, [ TRzDirectoryTree ] );

  if R.ReadBool( RegisterSection, 'TRzFileListBox', True ) then
    RegisterComponents( PalettePage, [ TRzFileListBox ] );

  if R.ReadBool( RegisterSection, 'TRzDirectoryListBox', True ) then
    RegisterComponents( PalettePage, [ TRzDirectoryListBox ] );

  if R.ReadBool( RegisterSection, 'TRzDriveComboBox', True ) then
    RegisterComponents( PalettePage, [ TRzDriveComboBox ] );

  if R.ReadBool( RegisterSection, 'TRzSelDirDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSelDirDialog ] );
    GroupDescendentsWith( TRzSelDirDialog, Controls.TControl );
  end;
end; {= RegisterRaizeShell =}


{====================================}
{== RegisterRaizeWidgets Procedure ==}
{====================================}

procedure RegisterRaizeWidgets( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzCalendar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCalendar ] );
    RegisterComponentEditor( TRzCalendar, TRzCalendarEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzTimePicker', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTimePicker ] );
    RegisterComponentEditor( TRzTimePicker, TRzTimePickerEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzTimePicker, 'HowToUseMsg', TRzHintProperty );
    RegisterPropertyEditor( TypeInfo( string ), TRzTimePicker, 'Format', TRzDateTimeFormatProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzCalculator', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCalculator ] );
    RegisterComponentEditor( TRzCalculator, TRzCalculatorEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzColorPicker', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzColorPicker ] );
    RegisterComponentEditor( TRzColorPicker, TRzColorPickerEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzCustomColors', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzCustomColors ] );
    RegisterComponentEditor( TRzCustomColors, TRzCustomColorsEditor );
    RegisterPropertyEditor( TypeInfo( TStrings ), TRzCustomColors, 'Colors', TRzCustomColorsProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzRegIniFile', True ) then
    RegisterComponents( PalettePage, [ TRzRegIniFile ] );

  if R.ReadBool( RegisterSection, 'TRzPropertyStore', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzPropertyStore ] );
    RegisterComponentEditor( TRzPropertyStore, TRzPropertyStoreEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzPropertyItem, 'PropertyName', TRzPropertyItemPropNameProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzFormState', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFormState ] );
    RegisterComponentEditor( TRzFormState, TRzFormStateEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzFormShape', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzFormShape ] );
    RegisterComponentEditor( TRzFormShape, TRzFormShapeEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzLauncher', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzLauncher ] );
    RegisterPropertyEditor( TypeInfo( string ), TRzLauncher, 'FileName', TRzFileNameProperty );
    RegisterPropertyEditor( TypeInfo( string ), TRzLauncher, 'Action', TRzActionProperty );
    GroupDescendentsWith( TRzLauncher, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzTrayIcon', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzTrayIcon ] );
    RegisterPropertyEditor( TypeInfo( Integer ), TRzTrayIcon, 'IconIndex', TRzTrayIconIndexProperty );
    RegisterComponentEditor( TRzTrayIcon, TRzTrayIconEditor );
    GroupDescendentsWith( TRzTrayIcon, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzVersionInfo', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzVersionInfo ] );
    GroupDescendentsWith( TRzVersionInfo, Controls.TControl );
  end;


  if R.ReadBool( RegisterSection, 'TRzBalloonHints', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzBalloonHints ] );
    RegisterComponentEditor( TRzBalloonHints, TRzBalloonHintsEditor );

    GroupDescendentsWith( TRzBalloonHints, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzLookupDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzLookupDialog ] );
    RegisterComponentEditor( TRzLookupDialog, TRzLookupDialogEditor );
    GroupDescendentsWith( TRzLookupDialog, Controls.TControl );
  end;

  if R.ReadBool( RegisterSection, 'TRzSendMessage', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzSendMessage ] );
    GroupDescendentsWith( TRzSendMessage, Controls.TControl );
  end;
end; {= RegisterRaizeWidgets =}


{======================================}
{== RegisterGeneralEditors Procedure ==}
{======================================}

procedure RegisterGeneralEditors( R: TRegIniFile );
begin
  if R.ReadBool( RegisterSection, 'Form Editor', True ) then
    RegisterComponentEditor( TForm, TRzFormEditor );

  if R.ReadBool( RegisterSection, 'Frame Editor', True ) then
    RegisterComponentEditor( TFrame, TRzFrameEditor );

  if R.ReadBool( RegisterSection, 'ImageList Editor', True ) then
  begin
    RegisterComponentEditor( TCustomImageList, TRzImageListEditor );
    RegisterComponentEditor( TImageList, TRzImageListEditor );
  end;

  if R.ReadBool( RegisterSection, 'String List Editor', True ) then
    RegisterPropertyEditor( TypeInfo(TStrings), nil, '', TRzStringListProperty );

  if R.ReadBool( RegisterSection, 'Align Property Editor', True ) then
    RegisterPropertyEditor( TypeInfo( TAlign ), nil, '', TRzAlignProperty );

  {$IFNDEF VCL140_OR_HIGHER}
  if R.ReadBool( RegisterSection, 'Boolean Property Editor', True ) then
    RegisterPropertyEditor( TypeInfo( Boolean ), nil, '', TRzBooleanProperty );
  {$ENDIF}

  if R.ReadBool( RegisterSection, 'Caption Property Editor', True ) then
    RegisterPropertyEditor( TypeInfo( TCaption ), TCustomLabel, 'Caption', TRzCaptionProperty );

  if R.ReadBool( RegisterSection, 'Hint Property Editor', True ) then
    RegisterPropertyEditor( TypeInfo( string ), TComponent, 'Hint', TRzHintProperty );

  if R.ReadBool( RegisterSection, 'GroupItem Caption Property Editor', True ) then
  begin
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzGroupItem, 'Caption', TRzGroupItemCaptionProperty );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzGroupTemplateItem, 'Caption', TRzGroupTemplateItemCaptionProperty );
  end;

  // Always Register the About Box & Frame Style Property Editor

  RegisterPropertyEditor( TypeInfo( TRzAboutInfo ), nil, 'About', TRzAboutBoxProperty );
  RegisterPropertyEditor( TypeInfo( TFrameStyle ), nil, '', TRzFrameStyleProperty );
  RegisterPropertyEditor( TypeInfo( TFrameStyleEx ), nil, '', TRzFrameStyleProperty );
end; {= RegisterGeneralEditors =}



{================================================}
{== RegisterCustomPropertyCategories Procedure ==}
{================================================}

procedure RegisterCustomPropertyCategories;
begin
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzFrameController, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzCustomEdit, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzMemo, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzCustomComboBox, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzCustomListBox, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzCustomTreeView, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzCustomListView, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDirectoryListBox, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzFileListBox, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDriveComboBox, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDateTimePicker, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzRichEdit, 'Color' );
  RegisterPropertiesInCategory( sRzCustomFramingCategoryName, [ 'Frame*', 'FlatButtons', 'FlatButtonColor',
                                                                'FocusColor', 'DisabledColor', 'FramingPreference' ] );

  RegisterPropertiesInCategory( sRzHotSpotCategoryName, [ 'HotSpot*' ] );

  RegisterPropertiesInCategory( sRzBorderStyleCategoryName,
                                [ 'Border*', 'FlatColor', 'BevelWidth' ] );

  RegisterPropertiesInCategory( sRzCustomGlyphsCategoryName,
                                [ 'CustomGlyphs', 'TransparentColor', 'WinMaskColor', 'GlyphDownLeft', 'GlyphUpRight',
                                  'NumGlyphsDownLeft', 'NumGlyphsUpRight', 'UseCustomGlyphs' ] );

  RegisterPropertiesInCategory( sRzTextStyleCategoryName,
                                [ 'TextStyle', 'Alignment', 'AlignmentVertical', 'Blink*', 'LightTextStyle', 'Rotation',
                                  'CenterPoint', 'FlyByEnabled', 'FlyByColor', 'Transparent', 'ShadowDepth', 'Layout',
                                  'Angle', 'HighlightColor', 'ShadowColor', 'ShowAccelChar', 'WordWrap' ] );

  RegisterPropertiesInCategory( sRzTrackStyleCategoryName,
                                [ 'CustomThumb', 'Track*', 'Thumb*', 'TickStyle', 'ShowTicks', 'ShowFocusRect' ] );

  RegisterPropertiesInCategory( sRzPrimaryButtonCategoryName, TRzButtonEdit, [ 'Button*' ] );
  RegisterPropertiesInCategory( sRzAlternateButtonCategoryName, TRzButtonEdit, [ 'AltBtn*' ] );

  RegisterPropertiesInCategory( sRzSplitterCategoryName, TRzSplitter,
                                [ 'FixedPane', 'LowerRight', 'Orientation', 'Position', 'RealTimeDrag', 'SelectedPane',
                                  'SplitterStyle', 'SplitterWidth', 'UpperLeft', 'UsePercent' ] );

  RegisterPropertyInCategory( sInputCategoryName, TRzTrackBar, 'Position' );
  RegisterPropertiesInCategory( sInputCategoryName,
                                [ 'AllowKeyEdit', 'AllowEdit', 'AllowDeleteByKbd', 'TabOnEnter', 'Text', 'Percent',
                                  'PercentMin', 'PercentMax', 'Min', 'Max', 'MinValue', 'MaxValue', 'PartsComplete',
                                  'TotalParts', 'MarginMin', 'MarginMax', 'Value' ] );

  RegisterPropertiesInCategory( sVisualCategoryName, TComponent,
                                [ 'NumSegments', 'InteriorOffset', 'Orientation', 'BarStyle',
                                  'ThemeAware', 'UseGradients', 'GroupBorderSize', 'FlatColor', 'FlatColorAdjustment' ] );

  RegisterPropertyInCategory( sDragNDropCategoryName, 'ShowDockClientCaptions' );

  RegisterPropertyInCategory( sActionCategoryName, 'IgnoreActionCaption' );

  // Localizable Properties
  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzRadioGroup,
                                [ 'Items', 'Columns' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzColorEdit,
                                [ 'CustomColorCaption', 'DefaultColorCaption', 'NoColorCaption' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzColorPicker,
                                [ 'CustomColorCaption', 'DefaultColorCaption', 'NoColorCaption' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzTimePicker,
                                [ 'CaptionAM', 'CaptionPM', 'CaptionSet' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzCalendar,
                                [ 'CaptionClearBtn', 'CaptionTodayBtn', 'NoColorCaption' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzDateTimeEdit,
                                [ 'CaptionAM', 'CaptionPM', 'CaptionSet', 'CaptionClearBtn', 'CaptionTodayBtn',
                                  'NoColorCaption' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzDialogButtons,
                                [ 'CaptionCancel', 'CaptionHelp', 'CaptionOK', 'WidthCancel', 'WidthHelp', 'WidthOK' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzLookupDialog,
                                [ 'Caption', 'CaptionCancel', 'CaptionHelp', 'CaptionOK', 'Prompt' ] );
end; {= RegisterCustomPropertyCategories =}



procedure RegisterWithSplashScreen;
var
  Bmp: TBitmap;
begin
  // Register TestSplash Icon on Delphi Splash Screen
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName( HInstance, 'RAIZECOMPONENTSSPLASH' );

  try
    SplashScreenServices.AddPluginBitmap( sProductName,
                                          Bmp.Handle, False, '', '' );
  finally
    Bmp.Free;
  end;

end;



procedure RegisterGuidelines;
begin
  RegisterComponentGuidelines( TRzCustomButton, TRzCustomButtonGuidelines );
  RegisterComponentGuidelines( TRzButton, TRzButtonGuidelines );
  RegisterComponentGuidelines( TRzToolButton, TRzToolButtonGuidelines );
  RegisterComponentGuidelines( TRzProgressBar, TRzCaptionGuidelines );
  RegisterComponentGuidelines( TRzCustomStatusPane, TRzCaptionGuidelines );
  RegisterComponentGuidelines( TRzLabel, TRzLabelGuidelines );
  RegisterComponentGuidelines( TRzPanel, TRzPanelGuidelines );
  RegisterComponentGuidelines( TRzPageControl, TRzPageControlGuidelines );
  RegisterComponentGuidelines( TRzTabControl, TRzTabControlGuidelines );
end;



{========================}
{== Register Procedure ==}
{========================}

procedure Register;
var
  R: TRegIniFile;
begin
  RegisterWithSplashScreen;

  R := TRegIniFile.Create( RC_SettingsKey );
  try
    RegisterRaizePanels( R, ppRaizePanels );
    RegisterRaizeEdits( R, ppRaizeEdits );
    RegisterRaizeLists( R, ppRaizeLists );
    RegisterRaizeButtons( R, ppRaizeButtons );
    RegisterRaizeDisplay( R, ppRaizeDisplay );
    RegisterRaizeShell( R, ppRaizeShell );
    RegisterRaizeWidgets( R, ppRaizeWidgets );

    RegisterGeneralEditors( R );

    RegisterGuidelines;

    RegisterCustomPropertyCategories;
  finally
    R.Free;
  end;

end; {= Register =}

{$ENDIF}

end.
