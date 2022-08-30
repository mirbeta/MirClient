{===============================================================================
  RzDBLook Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBLookupDialog
    Dialog-based component that allows user to select a record from a dataset


  Modification History
  ------------------------------------------------------------------------------
  6.2.2  (09 Apr 2016)
    * Fixed issue where TRzDBLookupDialog would display all records in the
      lookup list even if the dataset was filtered.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Added new SearchMethod property to TRzDBLookupDialog. The default value
      for this property is smLocate, which is the previous behavior of the
      component. Specifically, when the user enters a value into the Search
      edit box in the dialog, the Locate method is used to move the cursor
      to the first matching row in the dataset. When the SearchMethod property
      is changed to smFilter, the dataset displayed in the dialog box is
      filtered to show only those records matching the search string.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem where setting the KeyField or SearchField properties in
      TRzDBLookupDialog caused the IDE to immediately exit when setting at 
      design-time.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Replaced TDBGrid in lookup form with TRzDBGrid.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added FramingPreference property.
===============================================================================}

{$I RzComps.inc}

unit RzDBLook;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Classes,
  Graphics,
  Forms,
  DB,
  RzCommon,
  SysUtils,
  StdCtrls,
  Grids,
  DBGrids,
  DBCtrls,
  RzDBNav;

type
  EInvalidSearchField = class( Exception );
  EInvalidDataset = class( Exception );

  TRzDrawColumnCellEvent = procedure ( Grid: TDBGrid; const Rect: TRect; DataCol: Integer;
                                       Column: TColumn; State: TGridDrawState ) of object;
  TRzDrawDataCellEvent = procedure ( Grid: TDBGrid; const Rect: TRect;
                                     Field: TField; State: TGridDrawState ) of object;

  TRzDBLookupSearchMethod = ( smLocate, smFilter );

  TRzDBLookupDialog = class( TRzDialogComponent )
  private
    FAutoSelect: Boolean;
    FButtonGlyphs: Boolean;
    FCharCase: TEditCharCase;
    FDataset: TDataset;
    FGridOptions: TDBGridOptions;
    FPrompt: string;
    FMoveSrchFldToFront: Boolean;
    FNumbersOnly: Boolean;
    FKeyField: string;
    FSearchField: string;
    FSearchEdit: TCustomEdit;
    FSearchMethod: TRzDBLookupSearchMethod;
    FSearchString: string;
    FShowNavigator: Boolean;
    FShowNavigatorHints: Boolean;
    FNavigatorWidth: Integer;
    FVisibleNavButtons: TRzNavigatorButtons;
    FUpdateSearchEdit: Boolean;

    FOnDrawColumnCell: TRzDrawColumnCellEvent;
    FOnDrawDataCell: TRzDrawDataCellEvent;

    { Internal Event Handlers }
    procedure DrawColumnCellHandler( Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn;
                                     State: TGridDrawState );
    procedure DrawDataCellHandler( Sender: TObject; const Rect: TRect; Field: TField; State: TGridDrawState );
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure DefineProperties( Filer: TFiler ); override;
  public
    constructor Create( AOwner: TComponent ); override;

    function Execute: Boolean;

    property SearchString: string
      read FSearchString
      write FSearchString;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoSelect: Boolean
      read FAutoSelect
      write FAutoSelect
      default True;

    property ButtonGlyphs: Boolean
      read FButtonGlyphs
      write FButtonGlyphs
      default False;

    property CharCase: TEditCharCase
      read FCharCase
      write FCharCase
      default ecNormal;
      
    property Dataset: TDataset
      read FDataset
      write FDataset;

    property GridOptions: TDBGridOptions
      read FGridOptions
      write FGridOptions
      default [ dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines,
                dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit ];

    property MoveSearchField: Boolean
      read FMoveSrchFldToFront
      write FMoveSrchFldToFront
      default True;

    property NumbersOnly: Boolean
      read FNumbersOnly
      write FNumbersOnly
      default False;

    property Prompt: string
      read FPrompt
      write FPrompt;

    property KeyField: string
      read FKeyField
      write FKeyField;

    property NavigatorWidth: Integer
      read FNavigatorWidth
      write FNavigatorWidth
      default 221;

    property SearchEdit: TCustomEdit
      read FSearchEdit
      write FSearchEdit;

    property SearchField: string
      read FSearchField
      write FSearchField;

    property SearchMethod: TRzDBLookupSearchMethod
      read FSearchMethod
      write FSearchMethod;

    property ShowNavigator: Boolean
      read FShowNavigator
      write FShowNavigator
      default True;

    property ShowNavigatorHints: Boolean
      read FShowNavigatorHints
      write FShowNavigatorHints
      default False;

    property UpdateSearchEdit: Boolean
      read FUpdateSearchEdit
      write FUpdateSearchEdit
      default False;

    property VisibleNavButtons: TRzNavigatorButtons
      read FVisibleNavButtons
      write FVisibleNavButtons
      default [ nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
                nbEdit, nbPost, nbCancel, nbRefresh ];

    property OnDrawColumnCell: TRzDrawColumnCellEvent
      read FOnDrawColumnCell
      write FOnDrawColumnCell;

    property OnDrawDataCell: TRzDrawDataCellEvent
      read FOnDrawDataCell
      write FOnDrawDataCell;

    { Inherited Properties & Events }
    property BorderStyle;
    property Caption;
    property CaptionOK;
    property CaptionCancel;
    property CaptionHelp;
    property Font;
    property FrameColor;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property Height default 300;
    property HelpContext;
    property Width default 350;
  end;


implementation

uses
  {&RAS}
  Controls,
  RzDBLookupForm,
  RzCommonBitmaps,
  TypInfo;

resourcestring
  sRzLookupInvalidSearchField = 'RzDBLookupDialog Error: Search field must be selected';
  sRzLookupInvalidDataset     = 'RzDBLookupDialog Error: Dataset property must be specified';


{&RT}
{===============================}
{== TRzDBLookupDialog Methods ==}
{===============================}

constructor TRzDBLookupDialog.Create( AOwner: TComponent );
begin
  inherited;
  Height := 300;
  Width := 350;
  {&RCI}
  FMoveSrchFldToFront := True;
  FAutoSelect := True;
  FButtonGlyphs := False;
  FCharCase := ecNormal;
  FShowNavigator := True;
  FShowNavigatorHints := False;
  FNavigatorWidth := 221;
  FUpdateSearchEdit := False;  { Set to False for backward compatibility }

  FVisibleNavButtons := [ nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
                          nbEdit, nbPost, nbCancel, nbRefresh ];

  FNumbersOnly := False;

  FGridOptions := [ dgEditing, dgTitles, dgIndicator, dgColumnResize,
                    dgColLines, dgRowLines, dgTabs, dgConfirmDelete,
                    dgCancelOnExit ];
                   
end;


procedure TRzDBLookupDialog.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the SearchBtnEdit property was published in v1.6 
  Filer.DefineProperty( 'SearchBtnEdit', TRzOldPropReader.ReadOldIdentProp, nil, False );
end;


procedure TRzDBLookupDialog.Notification( AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FDataset then
      FDataset := nil
    else if AComponent = FSearchEdit then
      FSearchEdit := nil;
  end;
end;


procedure TRzDBLookupDialog.DrawColumnCellHandler( Sender: TObject; const Rect: TRect; DataCol: Integer;
                                                   Column: TColumn; State: TGridDrawState );
begin
  if Assigned( FOnDrawColumnCell ) then
    FOnDrawColumnCell( Sender as TDBGrid, Rect, DataCol, Column, State );
end;


procedure TRzDBLookupDialog.DrawDataCellHandler( Sender: TObject; const Rect: TRect;
                                                 Field: TField; State: TGridDrawState );
begin
  if Assigned( FOnDrawDataCell ) then
    FOnDrawDataCell( Sender as TDBGrid, Rect, Field, State );
end;



function TRzDBLookupDialog.Execute: Boolean;
var
  OldActive: Boolean;
  Dlg: TRzDBLookupForm;
  FDataLink: TDataLink;
begin
  {&RV}
  if FSearchField = '' then
    raise EInvalidSearchField.Create( sRzLookupInvalidSearchField );

  if FDataset = nil then
    raise EInvalidDataset.Create( sRzLookupInvalidDataset );

  OldActive := FDataset.Active;
  FDataset.Open;

  { Dynamically create the Lookup form }
  { RzDBLookupForm declared in the RzDBLkFm unit }
  Dlg := TRzDBLookupForm.Create( Application );
  with Dlg do
  begin
    try
      { Initialize properties of lookup form }
      Dlg.BorderStyle := Self.BorderStyle;
      Dlg.Left := Self.OriginLeft;
      Dlg.Top := Self.OriginTop;
      Dlg.Width := Self.Width;
      Dlg.Height := Self.Height;

      CenterForm( Dlg );

      Dlg.Font := Self.Font;
      Dlg.Caption := Self.Caption;
      Dlg.NumbersOnly := FNumbersOnly;

      PnlButtons.ShowHelpButton := Self.HelpContext <> 0;
      PnlButtons.HelpContext := Self.HelpContext;
      PnlButtons.ShowGlyphs := FButtonGlyphs;
      PnlButtons.CaptionOK := CaptionOK;
      PnlButtons.CaptionCancel := CaptionCancel;
      PnlButtons.CaptionHelp := CaptionHelp;

      NavLookup.VisibleButtons := FVisibleNavButtons;
      NavLookup.Width := FNavigatorWidth;
      NavLookup.ShowHint := FShowNavigatorHints;
      if not FShowNavigator then
        PnlNavigator.Visible := False;

      if FrameVisible then
      begin
        EdtSearch.FrameVisible := True;
        EdtSearch.FrameColor := FrameColor;
        EdtSearch.FrameStyle := FrameStyle;

        GrdLookup.FrameVisible := True;
        GrdLookup.FrameColor := FrameColor;
        GrdLookup.FrameStyle := FrameStyle;
      end;

      if FPrompt = '' then
        LblPrompt.Caption := 'Search for ' + FSearchField
      else
        LblPrompt.Caption := FPrompt;

                               { Set the field to be used for keyboard searching }
      SearchField := FSearchField;

      if FMoveSrchFldToFront then
        FDataset.FieldByName( SearchField ).Index := 0;

      GrdLookup.Options := FGridOptions;

      { Assign event handlers }
      GrdLookup.OnDrawColumnCell := DrawColumnCellHandler;
      GrdLookup.OnDrawDataCell := DrawDataCellHandler;

      SrcLookup.Dataset := FDataset;   { Set dataset used to populate the grid }
                              
      Dlg.UseFilter := FSearchMethod = smFilter;
      if Dlg.UseFilter then
        SrcLookup.Dataset.Filtered := False;
      Dlg.FilterOnStringField := FDataset.FieldByName( SearchField ).DataType in [ ftString, ftWideString ];

      KeyField := FKeyField;

      if FKeyField = '' then
      begin
        if FSearchEdit <> nil then
          EdtSearch.Text := FSearchEdit.Text
        else
          EdtSearch.Text := FSearchString;
        EdtSearchChange( Self );
      end
      else
      begin
        if FSearchEdit <> nil then
          FindKeyField( FSearchEdit.Text )
        else
          FindKeyField( FSearchString );
      end;

      EdtSearch.AutoSelect := FAutoSelect;
      if not FAutoSelect then
        EdtSearch.SelStart := Length( EdtSearch.Text );
      EdtSearch.CharCase := FCharCase;

      Result := ShowModal = idOK;          { Display the dialog box }

      if Result and FUpdateSearchEdit and ( FSearchEdit <> nil ) then
      begin
        FDataLink := TDataLink( FSearchEdit.Perform( cm_GetDataLink, 0, 0 ) );
        if ( FDataLink <> nil ) and ( FDataLink is TFieldDataLink ) then
        begin
          with FDataLink as TFieldDataLink do
          begin
            if Edit then
            begin
              if FKeyField = '' then
                Field.Text := FDataset.FieldByName( FSearchField ).AsString
              else
                Field.Text := FDataset.FieldByName( FKeyField ).AsString;
            end;
          end;
        end
        else
        begin
          if FKeyField = '' then
            FSearchEdit.Text := FDataset.FieldByName( FSearchField ).AsString
          else
            FSearchEdit.Text := FDataset.FieldByName( FKeyField ).AsString;
        end;
      end;

    finally
      Free;                         { Don't forget to free the form }
      FDataset.Active := OldActive;
    end;
  end;
end;


{&RUIF}
end.
