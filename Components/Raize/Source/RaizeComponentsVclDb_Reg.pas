{===============================================================================
  RaizeComponentsVclDb_Reg Unit

  Raize Components - Registration Unit (Data-Aware Components)

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Modification History
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added registration code for new TRzDBGrid component.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Restructred registration process. Separate Register* procedures are called
      for each palette page.
===============================================================================}

{$I RzComps.inc}

unit RaizeComponentsVclDb_Reg;

interface

{$IFDEF WIN32}
procedure Register;
{$ENDIF}

implementation

uses
  Forms,
  Controls,
  SysUtils,
  StdCtrls,
  Buttons,
  Dialogs,
  Graphics,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Classes,
  Registry,

  {$IFDEF WIN32}
  DesignIntf,
  VCLEditors,
  DBReg,

  { Design Editors }
  RzDesignEditors,
  RzCaptionEditor,
  RzDBDesignEditors,
  RzDBNavEditor,
  RzDBComboBoxEditor,
  RzDBRadioGroupEditor,
  {$ENDIF}
  
  { Data-Aware Units }
  RzCommon,
  RzGrafx,
  RzDBLbl,
  RzDBTrak,
  RzDBList,
  RzDBCmbo,
  RzDBEdit,
  RzDBGrid,
  RzDBProg,
  RzDBStat,
  RzDBRGrp,
  RzDBLook,
  RzDBBnEd,
  RzDBSpin,
  RzDBNav,
  RzDBChk,
  RzDBDTP;

  
{$IFDEF WIN32}

{===================================}
{== RegisterRaizePanels Procedure ==}
{===================================}

procedure RegisterRaizePanels( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBRadioGroup', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBRadioGroup ] );
    RegisterComponentEditor( TRzDBRadioGroup, TRzDBRadioGroupEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBCheckBoxGroup', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBCheckBoxGroup ] );
    RegisterComponentEditor( TRzDBCheckBoxGroup, TRzDBCheckBoxGroupEditor );
  end;
end; {= RegisterRaizePanels =}


{==================================}
{== RegisterRaizeEdits Procedure ==}
{==================================}

procedure RegisterRaizeEdits( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBEdit ] );
    RegisterComponentEditor( TRzDBEdit, TRzDBControlEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBEdit, 'DataField', TDataFieldAggProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBButtonEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBButtonEdit ] );
    RegisterComponentEditor( TRzDBButtonEdit, TRzDBButtonEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBDateTimeEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBDateTimeEdit ] );
    RegisterComponentEditor( TRzDBDateTimeEdit, TRzDBDateTimeEditEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBDateTimeEdit, 'HowToUseMsg', TRzHintProperty );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBDateTimeEdit, 'Format', TRzDBDateTimeFormatProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBNumericEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBNumericEdit ] );
    RegisterComponentEditor( TRzDBNumericEdit, TRzDBNumericEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBSpinEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBSpinEdit ] );
    RegisterComponentEditor( TRzDBSpinEdit, TRzDBSpinEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBExpandEdit', True ) then
    RegisterComponents( PalettePage, [ TRzDBExpandEdit ] );

  if R.ReadBool( RegisterSection, 'TRzDBMemo', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBMemo ] );
    RegisterComponentEditor( TRzDBMemo, TRzDBMemoEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBRichEdit', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBRichEdit ] );
    RegisterComponentEditor( TRzDBRichEdit, TRzDBRichEditEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBSpinner', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBSpinner ] );
    RegisterComponentEditor( TRzDBSpinner, TRzDBSpinnerEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBTrackBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBTrackBar ] );
    RegisterComponentEditor( TRzDBTrackBar, TRzDBControlEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBDateTimePicker', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBDateTimePicker ] );
    RegisterComponentEditor( TRzDBDateTimePicker, TRzDBControlEditor );
  end;
end; {= RegisterRaizeEdits =}


{==================================}
{== RegisterRaizeLists Procedure ==}
{==================================}

procedure RegisterRaizeLists( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBListBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBListBox ] );
    RegisterComponentEditor( TRzDBListBox, TRzDBListBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBComboBox ] );
    RegisterComponentEditor( TRzDBComboBox, TRzDBComboBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBLookupComboBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBLookupComboBox ] );
    RegisterComponentEditor( TRzDBLookupComboBox, TRzDBLookupComboBoxEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBGrid', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBGrid ] );
    RegisterComponentEditor( TRzDBGrid, TRzDBGridEditor );
  end;

end; {= RegisterRaizeLists =}


{====================================}
{== RegisterRaizeButtons Procedure ==}
{====================================}

procedure RegisterRaizeButtons( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBNavigator', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBNavigator ] );
    RegisterComponentEditor( TRzDBNavigator, TRzDBNavigatorEditor );
    RegisterPropertyEditor( TypeInfo( TImageIndex ), TRzDBNavigatorImageIndexes, '', TRzDBNavigatorImageIndexProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBCheckBox', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBCheckBox ] );
    RegisterComponentEditor( TRzDBCheckBox, TRzDBCheckBoxEditor );
    RegisterPropertyEditor( TypeInfo( TCaption ), TRzDBCheckBox, 'Caption', TRzCaptionProperty );
  end;
end; {= RegisterRaizeButtons =}


{====================================}
{== RegisterRaizeDisplay Procedure ==}
{====================================}

procedure RegisterRaizeDisplay( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBLabel', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBLabel ] );
    RegisterComponentEditor( TRzDBLabel, TRzDBLabelEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBLabel, 'DataField', TDataFieldAggProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBStatusPane', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBStatusPane ] );
    RegisterComponentEditor( TRzDBStatusPane, TRzDBStatusPaneEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBStatusPane, 'DataField', TDataFieldAggProperty );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBStateStatus', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBStateStatus ] );
    RegisterComponentEditor( TRzDBStateStatus, TRzDBStateStatusEditor );
  end;

  if R.ReadBool( RegisterSection, 'TRzDBProgressBar', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBProgressBar ] );
    RegisterComponentEditor( TRzDBProgressBar, TRzDBProgressBarEditor );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBProgressBar, 'BaseField', TDataFieldProperty );
  end;
end; {= RegisterRaizeDisplay =}


{====================================}
{== RegisterRaizeWidgets Procedure ==}
{====================================}

procedure RegisterRaizeWidgets( R: TRegIniFile; const PalettePage: string );
begin
  if R.ReadBool( RegisterSection, 'TRzDBLookupDialog', True ) then
  begin
    RegisterComponents( PalettePage, [ TRzDBLookupDialog ] );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBLookupDialog, 'SearchField', TRzSearchFieldProperty );
    RegisterPropertyEditor( TypeInfo( string ), TRzDBLookupDialog, 'KeyField', TRzSearchFieldProperty );
    GroupDescendentsWith( TRzDBLookupDialog, Controls.TControl );
  end;
end; {= RegisterRaizeWidgets =}



{================================================}
{== RegisterCustomPropertyCategories Procedure ==}
{================================================}

procedure RegisterCustomPropertyCategories;
begin
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDBEdit, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDBMemo, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDBRichEdit, 'Color' );
  RegisterPropertyInCategory( sRzCustomFramingCategoryName, TRzDBLookupComboBox, 'Color' );
  RegisterPropertiesInCategory( sRzPrimaryButtonCategoryName, TRzDBButtonEdit, [ 'Button*' ] );
  RegisterPropertiesInCategory( sRzAlternateButtonCategoryName, TRzDBButtonEdit, [ 'AltBtn*' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzDBRadioGroup,
                                [ 'Items', 'Columns', 'Values' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzDBStateStatus,
                                [ 'StateCaptions' ] );

  RegisterPropertiesInCategory( sLocalizableCategoryName, TRzDBLookupDialog,
                                [ 'Caption', 'CaptionCancel', 'CaptionHelp', 'CaptionOK', 'Prompt' ] );
end; {= RegisterCustomPropertyCategories =}


{========================}
{== Register Procedure ==}
{========================}

procedure Register;
var
  R: TRegIniFile;
begin
  R := TRegIniFile.Create( RC_SettingsKey );
  try
    RegisterRaizePanels( R, ppRaizePanels );
    RegisterRaizeEdits( R, ppRaizeEdits );
    RegisterRaizeLists( R, ppRaizeLists );
    RegisterRaizeButtons( R, ppRaizeButtons );
    RegisterRaizeDisplay( R, ppRaizeDisplay );
    RegisterRaizeWidgets( R, ppRaizeWidgets );

    RegisterCustomPropertyCategories;
  finally
    R.Free;
  end;
end; {= Register =}

{$ENDIF}

end.
