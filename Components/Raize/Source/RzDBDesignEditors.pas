{===============================================================================
  RzDBDesignEditors Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------

  TRzDBControlEditor
    Adds context menu for selecting DataSource components and Data Fields.

  TRzDBListBoxEditor
    Adds context menu to TRzDBListBox component.

  TRzDBLookupComboBoxEditor
    Adds context menu to TRzDBComboBox component.

  TRzDBMemoEditor
    Adds context menu to TRzDBMemo component.

  TRzDBRichEditEditor
    Adds context menu to TRzDBRichEdit component.

  TRzDBDateTimeEditEditor
    Adds context menu to TRzDBDateTimeEdit component.

  TRzDBStatusPaneEditor
    Adds context menu to TRzDBStatusPane component.

  TRzDBStateStatusEditor
    Adds context menu to TRzDBStateStatus component.

  TRzDBLabelEditor
    Adds context menu to TRzDBLabel component.

  TRzDBProgressBarEditor
    Adds context menu to TRzDBProgressBar component.

  TRzDBButtonEditEditor
    Adds context menu to TRzDBButtonEdit component.

  TRzDBNumericEditEditor
    Adds context menu to TRzDBNumericEdit component.

  TRzDBSpinEditEditor
    Adds context menu to TRzDBSpinEdit component.

  TRzDBSpinnerEditor
    Adds context menu to TRzDBSpinner component.

  TRzDBNavigatorEditor
    Adds context menu to TRzDBNavigator component.

  TRzDBCheckBoxEditor
    Adds context menu to TRzDBCheckBox component.

  TRzDBCheckBoxGroupEditor
    Adds context menu to TRzDBCheckBoxGroup component.

  TRzDBGridEditor
    Adds context menu to TRzDBGrid component.

  TRzSearchFieldProperty
    Displays list of available columns in a dataset

  TRzDBDateTimeFormatProperty 
    Provides list of format strings for changing date and time formats.


  Modification History
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed issue where the component editor for a data-aware control would
      raise an exception if connected to a DataSource that was connected to a
      ClientDataSet that did not have any Fields or FieldDefs at design-time.
    * Added WordWrap menu item to the design-time context menu displayed for
      the TRzDBCheckBox control.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added TRzDBGridEditor.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzDBDesignEditors;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  Menus,
  DesignIntf,
  DesignEditors,
  DBReg,
  RzDesignEditors,
  RzEdit,
  RzDBEdit,
  RzDBList,
  RzDBCmbo,
  RzDBChk,
  RzDBLbl,
  RzDBBnEd,
  RzDBStat,
  RzDBNav,
  RzDBSpin,
  RzDBRGrp,
  RzDBGrid;

type
  {== Component Editors ===============================================================================================}

  {==========================================}
  {== TRzDBControlEditor Class Declaration ==}
  {==========================================}

  TRzDBControlEditor = class( TRzDefaultEditor )
  protected
    function DataSourceMenuIndex: Integer; virtual;
    function DataFieldMenuIndex: Integer; virtual;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure DataSourceMenuHandler( Sender: TObject );
    procedure DataFieldMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {==========================================}
  {== TRzDBListBoxEditor Class Declaration ==}
  {==========================================}

  TRzDBListBoxEditor = class( TRzDBControlEditor )
  protected
    function ListBox: TRzDBListBox;
    function AlignMenuIndex: Integer; override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {=================================================}
  {== TRzDBLookupComboBoxEditor Class Declaration ==}
  {=================================================}

  TRzDBLookupComboBoxEditor = class( TRzDBControlEditor )
  protected
    function ListSourceMenuIndex: Integer; virtual;
    function KeyFieldMenuIndex: Integer; virtual;
    function ListFieldMenuIndex: Integer; virtual;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ListSourceMenuHandler( Sender: TObject );
    procedure KeyFieldMenuHandler( Sender: TObject );
    procedure ListFieldMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {=======================================}
  {== TRzDBMemoEditor Class Declaration ==}
  {=======================================}

  TRzDBMemoEditor = class( TRzDBControlEditor )
  protected
    function GetWordWrap: Boolean; virtual;
    procedure SetWordWrap( Value: Boolean ); virtual;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;

    property WordWrap: Boolean
      read GetWordWrap
      write SetWordWrap;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {===========================================}
  {== TRzDBRichEditEditor Class Declaration ==}
  {===========================================}

  TRzDBRichEditEditor = class( TRzDBMemoEditor )
  protected
    function GetWordWrap: Boolean; override;
    procedure SetWordWrap( Value: Boolean ); override;
  end;


  {===============================================}
  {== TRzDBDateTimeEditEditor Class Declaration ==}
  {===============================================}

  TRzDBDateTimeEditEditor = class( TRzDBControlEditor )
  protected
    function DateTimeEdit: TRzDBDateTimeEdit;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ElementsMenuHandler( Sender: TObject );
    procedure FirstDayOfWeekMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {=============================================}
  {== TRzDBStatusPaneEditor Class Declaration ==}
  {=============================================}

  TRzDBStatusPaneEditor = class( TRzDBControlEditor )
  protected
    function FlatStyleMenuIndex: Integer; virtual;
    function AutoSizeMenuIndex: Integer; virtual;
    function AlignmentMenuIndex: Integer; virtual;
    function BlinkingMenuIndex: Integer; virtual;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure AlignmentMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {==============================================}
  {== TRzDBStateStatusEditor Class Declaration ==}
  {==============================================}

  TRzDBStateStatusEditor = class( TRzDBStatusPaneEditor )
  protected
    function StateStatus: TRzDBStateStatus;
    function DataFieldMenuIndex: Integer; override;
    function FlatStyleMenuIndex: Integer; override;
    function AutoSizeMenuIndex: Integer; override;
    function AlignmentMenuIndex: Integer; override;
    function BlinkingMenuIndex: Integer; override;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure GlyphAlignmentMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {========================================}
  {== TRzDBLabelEditor Class Declaration ==}
  {========================================}

  TRzDBLabelEditor = class( TRzDBControlEditor )
  protected
    function LabelControl: TRzDBLabel;
    function AlignMenuIndex: Integer; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure TextStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ): string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {==============================================}
  {== TRzDBProgressBarEditor Class Declaration ==}
  {==============================================}

  TRzDBProgressBarEditor = class( TRzDBControlEditor )
  protected
    function BaseFieldMenuIndex: Integer; virtual;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure BaseFieldMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {=============================================}
  {== TRzDBButtonEditEditor Class Declaration ==}
  {=============================================}

  TRzDBButtonEditEditor = class( TRzDBControlEditor )
  protected
    function ButtonEdit: TRzDBButtonEdit;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure ButtonKindMenuHandler( Sender: TObject );
    procedure AltBtnKindMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {============================================}
  {== TRzNumericEditEditor Class Declaration ==}
  {============================================}

  TRzDBNumericEditEditor = class( TRzDBControlEditor )
  protected
    function NumericEdit: TRzDBNumericEdit;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {=========================================}
  {== TRzSpinEditEditor Class Declaration ==}
  {=========================================}

  TRzDBSpinEditEditor = class( TRzDBControlEditor )
  protected
    function SpinEdit: TRzDBSpinEdit;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure DirectionMenuHandler( Sender: TObject );
    procedure OrientationMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {==========================================}
  {== TRzDBSpinnerEditor Class Declaration ==}
  {==========================================}

  TRzDBSpinnerEditor = class( TRzDBControlEditor )
  protected
    function Spinner: TRzDBSpinner;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {============================================}
  {== TRzDBNavigatorEditor Class Declaration ==}
  {============================================}

  TRzDBNavigatorEditor = class( TRzDBControlEditor )
  protected
    function Navigator: TRzDBNavigator;
    function DataFieldMenuIndex: Integer; override;
    function GetCompRefData( Index: Integer; var CompRefClass: TComponentClass; var CompRefPropName: string;
                             var CompRefMenuHandler: TNotifyEvent ): Boolean; override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {===========================================}
  {== TRzDBCheckBoxEditor Class Declaration ==}
  {===========================================}

  TRzDBCheckBoxEditor = class( TRzDBControlEditor )
  protected
    function CheckBox: TRzDBCheckBox;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;


  {================================================}
  {== TRzDBCheckBoxGroupEditor Class Declaration ==}
  {================================================}

  TRzDBCheckBoxGroupEditor = class( TRzDBControlEditor )
  protected
    function GroupBox: TRzDBCheckBoxGroup;
    procedure PrepareMenuItem( Index: Integer; const Item: TMenuItem ); override;
    procedure GroupStyleMenuHandler( Sender: TObject );
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
  end;


  {=======================================}
  {== TRzDBGridEditor Class Declaration ==}
  {=======================================}

  TRzDBGridEditor = class( TRzDBControlEditor )
  protected
    function Grid: TRzDBGrid;
    function DataFieldMenuIndex: Integer; override;
    function AlignMenuIndex: Integer; override;
  public
    function GetVerbCount: Integer; override;
    function GetVerb( Index: Integer ) : string; override;
    procedure ExecuteVerb( Index: Integer ); override;
  end;




  {== Property Editors ================================================================================================}

  {==============================================}
  {== TRzSearchFieldProperty Class Declaration ==}
  {==============================================}

  TRzSearchFieldProperty = class( TDBStringProperty )
  public
    procedure GetValueList( List: TStrings ); override;
  end;

  
  {===================================================}
  {== TRzDBDateTimeFormatProperty Class Declaration ==}
  {===================================================}

  TRzDBDateTimeFormatProperty = class( TRzDateTimeFormatProperty )
  protected
    function FormatFilter: TRzDateTimeFormatFilter; override;
  end;


implementation

uses
  Controls,
  StdCtrls,
  Graphics,
  DB,
  TypInfo,
  ImgList,
  RzCommon,
  RzPanel,
  RzStatus,
  RzBtnEdt,
  RzSpnEdt,
  RzPopups;

{== Component Editors =================================================================================================}


{================================}
{== TRzDBControlEditor Methods ==}
{================================}

function TRzDBControlEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


function TRzDBControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
  end;
end;


function TRzDBControlEditor.DataSourceMenuIndex: Integer;
begin
  Result := 0;
end;


function TRzDBControlEditor.DataFieldMenuIndex: Integer;
begin
  Result := 1;
end;


procedure TRzDBControlEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, DataSourceCount, DataFieldCount: Integer;
  CompOwner: TComponent;
  DataSource: TDataSource;
  FieldList: TStringList;

  procedure CreateDataSourceMenu( DataSource: TDataSource );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := DataSource.Name;
    NewItem.Checked := GetObjectProp( Component, 'DataSource' ) = DataSource;
    NewItem.OnClick := DataSourceMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateDataFieldMenu( const DataField: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := DataField;
    NewItem.Checked := GetStrProp( Component, 'DataField' ) = DataField;
    NewItem.OnClick := DataFieldMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  if Index = DataSourceMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    DataSourceCount := 0;
    CompOwner := Designer.GetRoot;
    if CompOwner <> nil then
    begin
      for I := 0 to CompOwner.ComponentCount - 1 do
      begin
        if CompOwner.Components[ I ] is TDataSource then
        begin
          Inc( DataSourceCount );
          CreateDataSourceMenu( TDataSource( CompOwner.Components[ I ] ) );
        end;
      end;
    end;
    Item.Enabled := DataSourceCount > 0;
  end;

  if Index = DataFieldMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    DataFieldCount := 0;

    DataSource := GetObjectProp( Component, 'DataSource' ) as TDataSource;
    if ( DataSource <> nil ) and ( DataSource.DataSet <> nil ) then
    begin
      if DataSource.DataSet.Fields.Count > 0 then
      begin
        FieldList := TStringList.Create;
        try
          DataSource.DataSet.GetFieldNames( FieldList );
          DataFieldCount := FieldList.Count;
          for I := 0 to FieldList.Count - 1 do
            CreateDataFieldMenu( FieldList[ I ] );
        finally
          FieldList.Free;
        end;
      end;
    end;
    Item.Enabled := DataFieldCount > 0;
  end;
end;


procedure TRzDBControlEditor.DataSourceMenuHandler( Sender: TObject );
var
  S: string;
  DataSource: TDataSource;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    DataSource := Designer.GetRoot.FindComponent( S ) as TDataSource;
    SetObjectProp( Component, 'DataSource', DataSource );
    DesignerModified;
  end;
end;


procedure TRzDBControlEditor.DataFieldMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    SetStrProp( Component, 'DataField', S );
    DesignerModified;
  end;
end;


{================================}
{== TRzDBListBoxEditor Methods ==}
{================================}

function TRzDBListBoxEditor.ListBox: TRzDBListBox;
begin
  Result := Component as TRzDBListBox;
end;


function TRzDBListBoxEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzDBListBoxEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Edit Items...';
    4: Result := 'Align';
  end;
end;


function TRzDBListBoxEditor.AlignMenuIndex: Integer;
begin
  Result := 4;
end;


procedure TRzDBListBoxEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: EditPropertyByName( 'Items' );
  end;
end;


{=======================================}
{== TRzDBLookupComboBoxEditor Methods ==}
{=======================================}

function TRzDBLookupComboBoxEditor.GetVerbCount: Integer;
begin
  Result := 6;
end;


function TRzDBLookupComboBoxEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Set ListSource';
    4: Result := 'Set KeyField';
    5: Result := 'Set ListField';
  end;
end;


function TRzDBLookupComboBoxEditor.ListSourceMenuIndex: Integer;
begin
  Result := 3;
end;


function TRzDBLookupComboBoxEditor.KeyFieldMenuIndex: Integer;
begin
  Result := 4;
end;


function TRzDBLookupComboBoxEditor.ListFieldMenuIndex: Integer;
begin
  Result := 5;
end;


procedure TRzDBLookupComboBoxEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, ListSourceCount, KeyFieldCount, ListFieldCount: Integer;
  CompOwner: TComponent;
  ListSource: TDataSource;
  FieldList: TStringList;

  procedure CreateListSourceMenu( ListSource: TDataSource );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ListSource.Name;
    NewItem.Checked := GetObjectProp( Component, 'ListSource' ) = ListSource;
    NewItem.OnClick := ListSourceMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateKeyFieldMenu( const KeyField: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := KeyField;
    NewItem.Checked := GetStrProp( Component, 'KeyField' ) = KeyField;
    NewItem.OnClick := KeyFieldMenuHandler;
    Item.Add( NewItem );
  end;


  procedure CreateListFieldMenu( const ListField: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := ListField;
    NewItem.Checked := GetStrProp( Component, 'ListField' ) = ListField;
    NewItem.OnClick := ListFieldMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  if Index = ListSourceMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    ListSourceCount := 0;
    CompOwner := Designer.GetRoot;
    if CompOwner <> nil then
    begin
      for I := 0 to CompOwner.ComponentCount - 1 do
      begin
        if CompOwner.Components[ I ] is TDataSource then
        begin
          Inc( ListSourceCount );
          CreateListSourceMenu( TDataSource( CompOwner.Components[ I ] ) );
        end;
      end;
    end;
    Item.Enabled := ListSourceCount > 0;
  end;

  if Index = KeyFieldMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    KeyFieldCount := 0;

    ListSource := GetObjectProp( Component, 'ListSource' ) as TDataSource;
    if ( ListSource <> nil ) and ( ListSource.DataSet <> nil ) then
    begin
      FieldList := TStringList.Create;
      try
        ListSource.DataSet.GetFieldNames( FieldList );
        KeyFieldCount := FieldList.Count;
        for I := 0 to FieldList.Count - 1 do
          CreateKeyFieldMenu( FieldList[ I ] );
      finally
        FieldList.Free;
      end;
    end;
    Item.Enabled := KeyFieldCount > 0;
  end;

  if Index = ListFieldMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    ListFieldCount := 0;

    ListSource := GetObjectProp( Component, 'ListSource' ) as TDataSource;
    if ( ListSource <> nil ) and ( ListSource.DataSet <> nil ) then
    begin
      FieldList := TStringList.Create;
      try
        ListSource.DataSet.GetFieldNames( FieldList );
        ListFieldCount := FieldList.Count;
        for I := 0 to FieldList.Count - 1 do
          CreateListFieldMenu( FieldList[ I ] );
      finally
        FieldList.Free;
      end;
    end;
    Item.Enabled := ListFieldCount > 0;
  end;

end; {= TRzDBLookupComboBox.PrepareMenuItem =}


procedure TRzDBLookupComboBoxEditor.ListSourceMenuHandler( Sender: TObject );
var
  S: string;
  ListSource: TDataSource;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    ListSource := Designer.GetRoot.FindComponent( S ) as TDataSource;
    SetObjectProp( Component, 'ListSource', ListSource );
    DesignerModified;
  end;
end;


procedure TRzDBLookupComboBoxEditor.KeyFieldMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    SetStrProp( Component, 'KeyField', S );
    DesignerModified;
  end;
end;


procedure TRzDBLookupComboBoxEditor.ListFieldMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    SetStrProp( Component, 'ListField', S );
    DesignerModified;
  end;
end;


{=============================}
{== TRzDBMemoEditor Methods ==}
{=============================}

function TRzDBMemoEditor.GetWordWrap: Boolean;
begin
  Result := ( Component as TRzDBMemo ).WordWrap;
end;


procedure TRzDBMemoEditor.SetWordWrap( Value: Boolean );
begin
  ( Component as TRzDBMemo ).WordWrap := Value;
end;


function TRzDBMemoEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzDBMemoEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Align';
    4: Result := 'Word Wrap';
  end;
end;


function TRzDBMemoEditor.AlignMenuIndex: Integer;
begin
  Result := 3;
end;


procedure TRzDBMemoEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    4: Item.Checked := WordWrap;
  end;
end;


procedure TRzDBMemoEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    4:
    begin
      WordWrap := not WordWrap;
      DesignerModified;
    end;
  end;
end;


{=================================}
{== TRzDBRichEditEditor Methods ==}
{=================================}

function TRzDBRichEditEditor.GetWordWrap: Boolean;
begin
  Result := ( Component as TRzDBRichEdit ).WordWrap;
end;


procedure TRzDBRichEditEditor.SetWordWrap( Value: Boolean );
begin
  ( Component as TRzDBRichEdit ).WordWrap := Value;
end;



{=====================================}
{== TRzDBDateTimeEditEditor Methods ==}
{=====================================}

function TRzDBDateTimeEditEditor.DateTimeEdit: TRzDBDateTimeEdit;
begin
  Result := Component as TRzDBDateTimeEdit;
end;


function TRzDBDateTimeEditEditor.GetVerbCount: Integer;
begin
  Result := 8;
end;


function TRzDBDateTimeEditEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Date';
    4: Result := 'Time';
    5: Result := '-';
    6:
    begin
      if DateTimeEdit.EditType = etDate then
        Result := 'Visible Elements'
      else
        Result := 'Restrict Minutes (by 5)';
    end;

    7:
    begin
      if DateTimeEdit.EditType = etDate then
        Result := 'First Day of Week'
      else
        Result := 'Show How to Select Time Hint';
    end;
  end;
end;


procedure TRzDBDateTimeEditEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateElementsMenu( Element: TRzCalendarElement; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Element );
    NewItem.Checked := Element in DateTimeEdit.CalendarElements;
    NewItem.OnClick := ElementsMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateFirstDayOfWeekMenu( DOW: TRzFirstDayOfWeek; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( DOW );
    NewItem.Checked := DateTimeEdit.FirstDayOfWeek = DOW;
    NewItem.OnClick := FirstDayOfWeekMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    3: Item.Checked := DateTimeEdit.EditType = etDate;
    4: Item.Checked := DateTimeEdit.EditType = etTime;

    6:
    begin
      if DateTimeEdit.EditType = etDate then
      begin
        CreateElementsMenu( ceYear, 'Year' );
        CreateElementsMenu( ceMonth, 'Month' );
        CreateElementsMenu( ceArrows, 'Arrows' );
        CreateElementsMenu( ceWeekNumbers, 'Week Numbers' );
        CreateElementsMenu( ceDaysOfWeek, 'Days of the Week' );
        CreateElementsMenu( ceFillDays, 'Fill Days' );
        CreateElementsMenu( ceTodayButton, 'Today Button' );
        CreateElementsMenu( ceClearButton, 'Clear Button' );
      end
      else
      begin
        Item.Checked := DateTimeEdit.RestrictMinutes;
      end;
    end;

    7:
    begin
      if DateTimeEdit.EditType = etDate then
      begin
        CreateFirstDayOfWeekMenu( fdowMonday, 'Monday' );
        CreateFirstDayOfWeekMenu( fdowTuesday, 'Tuesday' );
        CreateFirstDayOfWeekMenu( fdowWednesday, 'Wednesday' );
        CreateFirstDayOfWeekMenu( fdowThursday, 'Thursday' );
        CreateFirstDayOfWeekMenu( fdowFriday, 'Friday' );
        CreateFirstDayOfWeekMenu( fdowSaturday, 'Saturday' );
        CreateFirstDayOfWeekMenu( fdowSunday, 'Sunday' );
        CreateFirstDayOfWeekMenu( fdowLocale, 'Locale' );
      end
      else
      begin
        Item.Checked := DateTimeEdit.ShowHowToUseHint;
      end;
    end;
  end;
end;


procedure TRzDBDateTimeEditEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: DateTimeEdit.EditType := etDate;
    4: DateTimeEdit.EditType := etTime;

    6:
    begin
      if DateTimeEdit.EditType = etTime then
        DateTimeEdit.RestrictMinutes := not DateTimeEdit.RestrictMinutes;
    end;

    7:
    begin
      if DateTimeEdit.EditType = etTime then
        DateTimeEdit.ShowHowToUseHint := not DateTimeEdit.ShowHowToUseHint;
    end;
  end;
  if Index in [ 3, 4, 6, 7 ] then
    DesignerModified;
end;


procedure TRzDBDateTimeEditEditor.ElementsMenuHandler( Sender: TObject );
var
  MI: TMenuItem;
  Element: TRzCalendarElement;
begin
  MI := TMenuItem( Sender );
  Element := TRzCalendarElement( MI.Tag );
  // Remove the element if checked, b/c menu has not yet been updated to remove the check
  if MI.Checked then
    DateTimeEdit.CalendarElements := DateTimeEdit.CalendarElements - [ Element ]
  else
    DateTimeEdit.CalendarElements := DateTimeEdit.CalendarElements + [ Element ];
  DesignerModified;
end;


procedure TRzDBDateTimeEditEditor.FirstDayOfWeekMenuHandler( Sender: TObject );
begin
  DateTimeEdit.FirstDayOfWeek := TRzFirstDayOfWeek( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


{===================================}
{== TRzDBStatusPaneEditor Methods ==}
{===================================}

function TRzDBStatusPaneEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;


function TRzDBStatusPaneEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Flat Style';
    4: Result := 'AutoSize';
    5: Result := 'Alignment';
    6: Result := 'Blinking';
    7: Result := '-';
    8: Result := 'Align';
  end;
end;

function TRzDBStatusPaneEditor.FlatStyleMenuIndex: Integer;
begin
  Result := 3;
end;


function TRzDBStatusPaneEditor.AutoSizeMenuIndex: Integer;
begin
  Result := 4;
end;


function TRzDBStatusPaneEditor.AlignmentMenuIndex: Integer;
begin
  Result := 5;
end;


function TRzDBStatusPaneEditor.BlinkingMenuIndex: Integer;
begin
  Result := 6;
end;


function TRzDBStatusPaneEditor.AlignMenuIndex: Integer;
begin
  Result := 8;
end;


procedure TRzDBStatusPaneEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateAlignmentMenu( Alignment: TAlignment; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Alignment );
    NewItem.Checked := GetOrdProp( Component, 'Alignment' ) = Ord( Alignment );
    NewItem.OnClick := AlignmentMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  if Index = AutoSizeMenuIndex then
    Item.Checked := GetOrdProp( Component, 'AutoSize' ) = 1
  else if Index = AlignmentMenuIndex then
  begin
    CreateAlignmentMenu( taLeftJustify, 'Left Justify' );
    CreateAlignmentMenu( taRightJustify, 'Right Justify' );
    CreateAlignmentMenu( taCenter, 'Center' );
  end
  else if Index = BlinkingMenuIndex then
    Item.Checked := GetOrdProp( Component, 'Blinking' ) = 1;
end;


procedure TRzDBStatusPaneEditor.ExecuteVerb( Index: Integer );
var
  B: Boolean;
begin
  inherited;

  if Index = FlatStyleMenuIndex then
  begin
    SetOrdProp( Component, 'FrameStyle', Ord( fsFlat ) );
    DesignerModified;
  end
  else if Index = AutoSizeMenuIndex then
  begin
    B := GetOrdProp( Component, 'AutoSize' ) = 1;
    SetOrdProp( Component, 'AutoSize', Ord( not B ) );
    DesignerModified;
  end
  else if Index = BlinkingMenuIndex then
  begin
    B := GetOrdProp( Component, 'Blinking' ) = 1;
    SetOrdProp( Component, 'Blinking', Ord( not B ) );
    DesignerModified;
  end;
end;


procedure TRzDBStatusPaneEditor.AlignmentMenuHandler( Sender: TObject );
begin
  SetOrdProp( Component, 'Alignment', TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{====================================}
{== TRzDBStateStatusEditor Methods ==}
{====================================}

function TRzDBStateStatusEditor.StateStatus: TRzDBStateStatus;
begin
  Result := Component as TRzDBStateStatus;
end;


function TRzDBStateStatusEditor.GetVerbCount: Integer;
begin
  Result := 12;
end;


function TRzDBStateStatusEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := '-';
    2: Result := 'Glyph Alignment';
    3: Result := 'Show Glyph';
    4: Result := 'Show Caption';
    5: Result := '-';
    6: Result := 'Flat Style';
    7: Result := 'AutoSize';
    8: Result := 'Alignment';
    9: Result := 'Blinking';
    10: Result := '-';
    11: Result := 'Align';
  end;
end;


function TRzDBStateStatusEditor.DataFieldMenuIndex: Integer;
begin
  Result := -1;
end;

function TRzDBStateStatusEditor.FlatStyleMenuIndex: Integer;
begin
  Result := 6;
end;


function TRzDBStateStatusEditor.AutoSizeMenuIndex: Integer;
begin
  Result := 7;
end;


function TRzDBStateStatusEditor.AlignmentMenuIndex: Integer;
begin
  Result := 8;
end;


function TRzDBStateStatusEditor.BlinkingMenuIndex: Integer;
begin
  Result := 9;
end;


function TRzDBStateStatusEditor.AlignMenuIndex: Integer;
begin
  Result := 11;
end;


procedure TRzDBStateStatusEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateGlyphAlignmentMenu( GlyphAlignment: TGlyphAlignment; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( GlyphAlignment );
    NewItem.Checked := StateStatus.GlyphAlignment = GlyphAlignment;
    NewItem.OnClick := GlyphAlignmentMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    2:
    begin
      CreateGlyphAlignmentMenu( gaLeft, 'Left' );
      CreateGlyphAlignmentMenu( gaRight, 'Right' );
    end;
    3: Item.Checked := StateStatus.ShowGlyph;
    4: Item.Checked := StateStatus.ShowCaption;
  end;
end;


procedure TRzDBStateStatusEditor.ExecuteVerb( Index: Integer );
begin
  inherited;

  case Index of
    3: StateStatus.ShowGlyph := not StateStatus.ShowGlyph;
    4: StateStatus.ShowCaption := not StateStatus.ShowCaption;
  end;
  if Index in [ 1, 4 ] then
    DesignerModified;
end;


procedure TRzDBStateStatusEditor.GlyphAlignmentMenuHandler( Sender: TObject );
begin
  StateStatus.GlyphAlignment := TGlyphAlignment( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{==============================}
{== TRzDBLabelEditor Methods ==}
{==============================}

function TRzDBLabelEditor.LabelControl: TRzDBLabel;
begin
  Result := Component as TRzDBLabel;
end;


function TRzDBLabelEditor.GetVerbCount: Integer;
begin
  Result := 10;
end;


function TRzDBLabelEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Align';
    4: Result := '-';
    5: Result := 'AutoSize';
    6: Result := 'WordWrap';
    7: Result := 'Transparent';
    8: Result := '-';
    9: Result := 'Text Style';
  end;
end;


function TRzDBLabelEditor.AlignMenuIndex: Integer;
begin
  Result := 3;
end;


procedure TRzDBLabelEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateTextStyleMenu( Style: TTextStyle; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Style );
    NewItem.Checked := LabelControl.TextStyle = Style;
    NewItem.OnClick := TextStyleMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    5: Item.Checked := LabelControl.AutoSize;
    6: Item.Checked := LabelControl.WordWrap;
    7: Item.Checked := LabelControl.Transparent;

    9:
    begin
      CreateTextStyleMenu( tsNormal, 'Normal' );
      CreateTextStyleMenu( tsRaised, 'Raised' );
      CreateTextStyleMenu( tsRecessed, 'Recessed' );
      CreateTextStyleMenu( tsShadow, 'Shadow' );
    end;
  end;
end;


procedure TRzDBLabelEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    5: LabelControl.AutoSize := not LabelControl.AutoSize; // AutoSize
    6: LabelControl.WordWrap := not LabelControl.WordWrap; // WordWrap
    7: LabelControl.Transparent := not LabelControl.Transparent;     // Transparent
  end;
  if Index in [ 5, 6, 7 ] then
    DesignerModified;
end; {= TRzDBLabelEditor.ExecuteVerb =}



procedure TRzDBLabelEditor.TextStyleMenuHandler( Sender: TObject );
begin
  LabelControl.TextStyle := TTextStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;




{====================================}
{== TRzDBProgressBarEditor Methods ==}
{====================================}

function TRzDBProgressBarEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;


function TRzDBProgressBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := 'Set BaseField';
  end;
end;


function TRzDBProgressBarEditor.BaseFieldMenuIndex: Integer;
begin
  Result := 2;
end;


procedure TRzDBProgressBarEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
var
  I, BaseFieldCount: Integer;
  DataSource: TDataSource;
  FieldList: TStringList;

  procedure CreateBaseFieldMenu( const BaseField: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.AutoHotkeys := maManual;
    NewItem.Caption := BaseField;
    NewItem.Checked := GetStrProp( Component, 'BaseField' ) = BaseField;
    NewItem.OnClick := BaseFieldMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  if Index = BaseFieldMenuIndex then
  begin
    Item.AutoHotkeys := maManual;
    BaseFieldCount := 0;

    DataSource := GetObjectProp( Component, 'DataSource' ) as TDataSource;
    if ( DataSource <> nil ) and ( DataSource.DataSet <> nil ) then
    begin
      FieldList := TStringList.Create;
      try
        DataSource.DataSet.GetFieldNames( FieldList );
        BaseFieldCount := FieldList.Count;
        for I := 0 to FieldList.Count - 1 do
          CreateBaseFieldMenu( FieldList[ I ] );
      finally
        FieldList.Free;
      end;
    end;
    Item.Enabled := BaseFieldCount > 0;
  end;
end;


procedure TRzDBProgressBarEditor.BaseFieldMenuHandler( Sender: TObject );
var
  S: string;
begin
  if Designer.GetRoot <> nil then
  begin
    S := TMenuItem( Sender ).Caption;
    SetStrProp( Component, 'BaseField', S );
    DesignerModified;
  end;
end;


{===================================}
{== TRzDBButtonEditEditor Methods ==}
{===================================}

function TRzDBButtonEditEditor.ButtonEdit: TRzDBButtonEdit;
begin
  Result := Component as TRzDBButtonEdit;
end;


function TRzDBButtonEditEditor.GetVerbCount: Integer;
begin
  Result := 11;
end;


function TRzDBButtonEditEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Show Button';
    4: Result := 'Button Kind';
    5: Result := '-';
    6: Result := 'Show Alternate Button';
    7: Result := 'Alternate Button Kind';
    8: Result := '-';
    9: Result := 'Flat Buttons';
    10: Result := 'Allow Key Edit';
  end;
end;


procedure TRzDBButtonEditEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateButtonKindMenu( Kind: TButtonKind; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Kind );
    NewItem.Checked := ButtonEdit.ButtonKind = Kind;
    NewItem.OnClick := ButtonKindMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateAltBtnKindMenu( Kind: TButtonKind; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Kind );
    NewItem.Checked := ButtonEdit.AltBtnKind = Kind;
    NewItem.OnClick := AltBtnKindMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    3: Item.Checked := ButtonEdit.ButtonVisible;

    4: // ButtonKind
    begin
      CreateButtonKindMenu( bkCustom, 'Custom' );
      CreateButtonKindMenu( bkLookup, 'Lookup' );
      CreateButtonKindMenu( bkDropDown, 'DropDown' );
      CreateButtonKindMenu( bkCalendar, 'Calendar' );
      CreateButtonKindMenu( bkAccept, 'Accept' );
      CreateButtonKindMenu( bkReject, 'Reject' );
      CreateButtonKindMenu( bkFolder, 'Folder' );
      CreateButtonKindMenu( bkFind, 'Find' );
      CreateButtonKindMenu( bkSearch, 'Search' );
    end;

    6: Item.Checked := ButtonEdit.AltBtnVisible;

    7: // AltBtnKind
    begin
      CreateAltBtnKindMenu( bkCustom, 'Custom' );
      CreateAltBtnKindMenu( bkLookup, 'Lookup' );
      CreateAltBtnKindMenu( bkDropDown, 'DropDown' );
      CreateAltBtnKindMenu( bkCalendar, 'Calendar' );
      CreateAltBtnKindMenu( bkAccept, 'Accept' );
      CreateAltBtnKindMenu( bkReject, 'Reject' );
      CreateAltBtnKindMenu( bkFolder, 'Folder' );
      CreateAltBtnKindMenu( bkFind, 'Find' );
      CreateAltBtnKindMenu( bkSearch, 'Search' );
    end;

    9: Item.Checked := ButtonEdit.FlatButtons;
    10: Item.Checked := ButtonEdit.AllowKeyEdit;
  end;
end;


procedure TRzDBButtonEditEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: ButtonEdit.ButtonVisible := not ButtonEdit.ButtonVisible;
    6: ButtonEdit.AltBtnVisible := not ButtonEdit.AltBtnVisible;
    9: ButtonEdit.FlatButtons := not ButtonEdit.FlatButtons;
    10: ButtonEdit.AllowKeyEdit := not ButtonEdit.AllowKeyEdit;
  end;
  if Index in [ 3, 6, 9, 10 ] then
    DesignerModified;
end;


procedure TRzDBButtonEditEditor.ButtonKindMenuHandler( Sender: TObject );
begin
  ButtonEdit.ButtonKind := TButtonKind( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzDBButtonEditEditor.AltBtnKindMenuHandler( Sender: TObject );
begin
  ButtonEdit.AltBtnKind := TButtonKind( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


{====================================}
{== TRzDBNumericEditEditor Methods ==}
{====================================}

function TRzDBNumericEditEditor.NumericEdit: TRzDBNumericEdit;
begin
  Result := Component as TRzDBNumericEdit;
end;


function TRzDBNumericEditEditor.GetVerbCount: Integer;
begin
  Result := 6;
end;


function TRzDBNumericEditEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Integers Only';
    4: Result := 'Check Range';
    5: Result := 'Allow Blank';
  end;
end;


procedure TRzDBNumericEditEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := NumericEdit.IntegersOnly;
    4: Item.Checked := NumericEdit.CheckRange;
    5: Item.Checked := NumericEdit.AllowBlank;
  end;
end;


procedure TRzDBNumericEditEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: NumericEdit.IntegersOnly := not NumericEdit.IntegersOnly;
    4: NumericEdit.CheckRange := not NumericEdit.CheckRange;
    5: NumericEdit.AllowBlank := not NumericEdit.AllowBlank;
  end;
  if Index in [ 3, 4, 5 ] then
    DesignerModified;
end;



{=================================}
{== TRzDBSpinEditEditor Methods ==}
{=================================}

function TRzDBSpinEditEditor.SpinEdit: TRzDBSpinEdit;
begin
  Result := Component as TRzDBSpinEdit;
end;


function TRzDBSpinEditEditor.GetVerbCount: Integer;
begin
  Result := 11;
end;


function TRzDBSpinEditEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Integers Only';
    4: Result := 'Allow Key Edit';
    5: Result := 'Check Range';
    6: Result := 'Allow Blank';
    7: Result := '-';
    8: Result := 'Flat Buttons';
    9: Result := 'Direction';
    10: Result := 'Orientation';
  end;
end;


procedure TRzDBSpinEditEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );

  procedure CreateDirectionMenu( Direction: TSpinDirection; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Direction );
    NewItem.Checked := SpinEdit.Direction = Direction;
    NewItem.OnClick := DirectionMenuHandler;
    Item.Add( NewItem );
  end;

  procedure CreateOrientationMenu( Orientation: TOrientation; const Caption: string );
  var
    NewItem: TMenuItem;
  begin
    NewItem := TMenuItem.Create( Item );
    NewItem.Caption := Caption;
    NewItem.Tag := Ord( Orientation );
    NewItem.Checked := SpinEdit.Orientation = Orientation;
    NewItem.OnClick := OrientationMenuHandler;
    Item.Add( NewItem );
  end;

begin
  inherited;

  case Index of
    3: Item.Checked := SpinEdit.IntegersOnly;
    4: Item.Checked := SpinEdit.AllowKeyEdit;
    5: Item.Checked := SpinEdit.CheckRange;
    6: Item.Checked := SpinEdit.AllowBlank;
    8: Item.Checked := SpinEdit.FlatButtons;

    9: // Direction
    begin
      CreateDirectionMenu( sdUpDown, 'Up/Down' );
      CreateDirectionMenu( sdLeftRight, 'Left/Right' );
    end;

    10: // Orientation
    begin
      CreateOrientationMenu( orHorizontal, 'Horizontal' );
      CreateOrientationMenu( orVertical, 'Vertical' );
    end;
  end;
end;


procedure TRzDBSpinEditEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: SpinEdit.IntegersOnly := not SpinEdit.IntegersOnly;
    4: SpinEdit.AllowKeyEdit := not SpinEdit.AllowKeyEdit;
    5: SpinEdit.CheckRange := not SpinEdit.CheckRange;
    6: SpinEdit.AllowBlank := not SpinEdit.AllowBlank;
    8: SpinEdit.FlatButtons := not SpinEdit.FlatButtons;
  end;
  if Index in [ 3, 4, 5, 6, 8 ] then
    DesignerModified;
end;


procedure TRzDBSpinEditEditor.DirectionMenuHandler( Sender: TObject );
begin
  SpinEdit.Direction := TSpinDirection( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


procedure TRzDBSpinEditEditor.OrientationMenuHandler( Sender: TObject );
begin
  SpinEdit.Orientation := TOrientation( TMenuItem( Sender ).Tag );
  DesignerModified;
end;


{================================}
{== TRzDBSpinnerEditor Methods ==}
{================================}

function TRzDBSpinnerEditor.Spinner: TRzDBSpinner;
begin
  Result := Component as TRzDBSpinner;
end;


function TRzDBSpinnerEditor.GetVerbCount: Integer;
begin
  Result := 8;
end;


function TRzDBSpinnerEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Check Range';
    4: Result := '-';
    5: Result := 'Set ImageList';
    6: Result := 'Select Plus Image...';
    7: Result := 'Select Minus Image...';
  end;
end;


function TRzDBSpinnerEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                            var CompRefPropName: string;
                                            var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 5 then
  begin
    CompRefClass := TCustomImageList;
    CompRefPropName := 'Images';
    CompRefMenuHandler := nil;
    Result := True;
  end
end;


procedure TRzDBSpinnerEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: Item.Checked := Spinner.CheckRange;
  end;
end;


procedure TRzDBSpinnerEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3:
    begin
      Spinner.CheckRange := not Spinner.CheckRange;
      DesignerModified;
    end;

    6: EditPropertyByName( 'ImageIndexPlus' );
    7: EditPropertyByName( 'ImageIndexMinus' );
  end;
end;


{==================================}
{== TRzDBNavigatorEditor Methods ==}
{==================================}

function TRzDBNavigatorEditor.Navigator: TRzDBNavigator;
begin
  Result := Component as TRzDBNavigator;
end;


function TRzDBNavigatorEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;


function TRzDBNavigatorEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := '-';
    2: Result := 'Set ImageList';
  end;
end;


function TRzDBNavigatorEditor.DataFieldMenuIndex: Integer;
begin
  Result := -1;
end;


function TRzDBNavigatorEditor.GetCompRefData( Index: Integer; var CompRefClass: TComponentClass;
                                            var CompRefPropName: string;
                                            var CompRefMenuHandler: TNotifyEvent ): Boolean;
begin
  Result := False;
  if Index = 2 then
  begin
    CompRefClass := TCustomImageList;
    CompRefPropName := 'Images';
    CompRefMenuHandler := nil;
    Result := True;
  end
end;



{=================================}
{== TRzDBCheckBoxEditor Methods ==}
{=================================}

function TRzDBCheckBoxEditor.CheckBox: TRzDBCheckBox;
begin
  Result := Component as TRzDBCheckBox;
end;


function TRzDBCheckBoxEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;


function TRzDBCheckBoxEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Optimize Size';
    4: Result := 'AutoSize';
    5: Result := 'WordWrap';
    6: Result := '-';
    7: Result := 'HotTrack';
    8: Result := 'XP Colors';
  end;
end;


procedure TRzDBCheckBoxEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    4: Item.Checked := CheckBox.AutoSize;
    5: Item.Checked := CheckBox.WordWrap;
    7: Item.Checked := CheckBox.HotTrack;
  end;
end;


procedure TRzDBCheckBoxEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    3: // Optimize Size
    begin
      CheckBox.AutoSize := not CheckBox.AutoSize;
      CheckBox.AutoSize := not CheckBox.AutoSize;
      DesignerModified;
    end;

    4: // AutoSize
    begin
      CheckBox.AutoSize := not CheckBox.AutoSize;
      DesignerModified;
    end;

    5: // WordWrap
    begin
      CheckBox.WordWrap := not CheckBox.WordWrap;
      DesignerModified;
    end;

    7: // Hot Track
    begin
      CheckBox.HotTrack := not CheckBox.HotTrack;
      DesignerModified;
    end;

    8:
    begin
      CheckBox.HotTrackColorType := htctActual;
      CheckBox.HotTrack := True;
      CheckBox.HighlightColor := xpRadChkMarkColor;
      CheckBox.HotTrackColor := xpHotTrackColor;
      CheckBox.FrameColor := xpRadChkFrameColor;
      DesignerModified;
    end;
  end;
end;


{======================================}
{== TRzDBCheckBoxGroupEditor Methods ==}
{======================================}

function TRzDBCheckBoxGroupEditor.GroupBox: TRzDBCheckBoxGroup;
begin
  Result := Component as TRzDBCheckBoxGroup;
end;


function TRzDBCheckBoxGroupEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;


function TRzDBCheckBoxGroupEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := 'Set DataField';
    2: Result := '-';
    3: Result := 'Group Style';
  end;
end;


procedure TRzDBCheckBoxGroupEditor.PrepareMenuItem( Index: Integer; const Item: TMenuItem );
begin
  inherited;

  case Index of
    3: // GroupStyle
    begin
      CreateGroupStyleMenuItem( Item, gsFlat, GroupBox.GroupStyle, GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsStandard, GroupBox.GroupStyle, GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsTopLine, GroupBox.GroupStyle, GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsBanner, GroupBox.GroupStyle, GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsUnderline, GroupBox.GroupStyle, GroupStyleMenuHandler );
      CreateGroupStyleMenuItem( Item, gsCustom, GroupBox.GroupStyle, GroupStyleMenuHandler );
    end;
  end;
end;


procedure TRzDBCheckBoxGroupEditor.GroupStyleMenuHandler( Sender: TObject );
begin
  GroupBox.GroupStyle := TRzGroupBoxStyle( TMenuItem( Sender ).Tag );
  DesignerModified;
end;



{=============================}
{== TRzDBGridEditor Methods ==}
{=============================}

function TRzDBGridEditor.Grid: TRzDBGrid;
begin
  Result := Component as TRzDBGrid;
end;


function TRzDBGridEditor.DataFieldMenuIndex: Integer;
begin
  Result := -1;
end;


function TRzDBGridEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;


function TRzDBGridEditor.GetVerb( Index: Integer ): string;
begin
  case Index of
    0: Result := 'Set DataSource';
    1: Result := '-';
    2: Result := 'Edit Columns...';
    3: Result := 'Align';
    4: Result := 'XP Colors';
  end;
end;


function TRzDBGridEditor.AlignMenuIndex: Integer;
begin
  Result := 3;
end;


procedure TRzDBGridEditor.ExecuteVerb( Index: Integer );
begin
  case Index of
    2: EditPropertyByName( 'Columns' );

    4: // XP Colors
    begin
      Grid.FixedColor := clInactiveCaptionText;
      Grid.LineColor := clInactiveCaption;
      Grid.FixedLineColor := xpEditFrameColor;
      Grid.FrameColor := xpEditFrameColor;
      Grid.FrameVisible := True;
      DesignerModified;
    end;
  end;
end;


{== Property Editors ==================================================================================================}


{====================================}
{== TRzSearchFieldProperty Methods ==}
{====================================}

procedure TRzSearchFieldProperty.GetValueList( List: TStrings );
var
  Dataset: TDataset;
begin
  Dataset := GetObjectProp( GetComponent( 0 ), 'Dataset' ) as TDataset;
  if DataSet <> nil then
    DataSet.GetFieldNames( List );
end;



{=========================================}
{== TRzDBDateTimeFormatProperty Methods ==}
{=========================================}

function TRzDBDateTimeFormatProperty.FormatFilter: TRzDateTimeFormatFilter;
var
  C: TPersistent;
begin
  C := GetComponent( 0 );
  if C is TRzDBDateTimeEdit then
  begin
    if TRzDBDateTimeEdit( C ).EditType = etTime then
      Result := ffTimes
    else
      Result := ffDates;
  end
  else
    Result := inherited FormatFilter;
end;


end.
