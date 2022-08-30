{===============================================================================
  RzChkLst Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzCheckList
    Enhanced list box where each item is associated with a check box.


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzCheckList to fully support VCL Styles
      introduced in RAD Studio XE2. Including support for custom check box 
      glyphs.
    * Made necessary modifications to TRzCheckList to support 64-bit.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed display issue with TRzCheckList where the background of the check
      boxes would become black when the list is scrolled and the DoubleBuffered
      property was set to True.
    * Updated the appearance of the TRzCheckList design editor.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed problem where the horizontal scroll bar of a TRzCheckList would clip
      the text too short under certain situations.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzCheckList control.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed issue where adding a lot of items to a TRzCheckList would take a
      long time even when Items.BeginUpdate and Items.EndUpdate were used.
    * Surfaced GroupColorFromTheme property in TRzCheckList.
    * Fixed issue where the horizontal scroll bar would appear in the
      TRzCheckList even when there was plenty of room to display all items.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * The TRzCheckList component has been enhanced so that an image can be
      associated with each item in the list. The new Images property is used to
      reference the desired ImageList that contains the images to be displayed.
      The ItemImageIndex[Index] and ItemDisabledIndex[Index] array properties
      are used to specify the image to display when the item is enabled and
      disabled, respectively. The image is display between the check box glyph
      and the item's text.
    * Added new CustomGlyphImages property to TRzCheckList. This property is
      used to reference an ImageList that contains the glyphs to be used for the
      various states of the control. This new property should be used instead of
      the deprecated CustomGlyphs property, which is still available strictly
      for backward compatibility. By referencing an ImageList that holds the
      custom glyphs rather than an embedded bitmap, the actual glyph images are
      stored only once in the application instead of inside each instance of the
      control. When populating a TImageList for use with CustomGlyphImages, each
      index in the ImageList represents a different state.  The following tables
      describe the mapping. The mapping is the same as that of TRzCheckBox,
      which is not true for the old CustomGlyphs mapping.

      TRzCheckGroup CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Grayed
          3    Unchecked - Pressed   (Unused by TRzCheckList)
          4    Checked   - Pressed   (Unused by TRzCheckList)
          5    Grayed    - Pressed   (Unused by TRzCheckList)
          6    Unchecked - Disabled
          7    Checked   - Disabled
          8    Grayed    - Disabled
          9    Unchecked - Hot       (Unused by TRzCheckList)
          10   Checked   - Hot       (Unused by TRzCheckList)
          11   Grayed    - Hot       (Unused by TRzCheckList)
    * Added the WinMaskColor for use with CustomGlyphs and CustomGlyphImages.
      This property provides the same behavior as the WinMaskColor property that
      is available in TRzCheckBox. That is, any pixels of the specified color in
      the custom glyphs are replaced with the current clWindow color.
    * Added new overloaded methods to TRzCheckList for SaveToFile and
      LoadFromFile that accept a TEncoding parameter in order to handle saving
      and loading items that contain Unicode characters. These methods are only
      available in RAD Studio 2009 or later.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surface OnMouseWheel event in TRzCheckList.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Added ItemFillColor and ItemFocusColor properties to TRzCheckList.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed display issues in TRzCheckList when running under RTL systems.
    * TRzCheckList now has an ItemFrameColor property that allows a developer to
      customize the color of the check box frames differently from the frame
      of the check list control itself.
    * The check boxes displayed by the check list utilize the same new drawing
      methods introduced for the TRzCheckBox control.
    * Added new FrameControllerNotifications property to TRzCheckList.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem in TRzCheckList where calling the inherited AddItem method
      from TCustomListBox would cause a list index out of bounds exception.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Modified the TRzCheckListStrings.AddStrings method so that a string list
      that contained the coded state and enable values for each item would get
      correctly added to the TRzCheckList.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem where ItemIndex was not being maintained in TRzCheckList
      when an Item's caption was changed.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Group code was moved from the TRzCheckList class to the TRzCustomListBox.
    * Fixed CheckGroup, UncheckGroup, EnableGroup, DisableGroup methods.
    * Added the AddItemToGroup and InsertItemIntoGroup methods. Also added the
      following support methods: ItemsInGroup and ItemIndexOfGroup.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Test FullColorSupport before all calls to PaintGradient.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Fixed problem where check and enabled states did not get updated correctly
      when the Items.Exchange method was used.
    * The TRzCheckList now supports grouping items in the list. By specifying a
      prefix "//" at the beginning of an item, the check list will display that
      item as a group header rather than a regular check item.  There are
      several new methods that allow users to manipulate the items within the
      group.  For example, SetGroupState, CheckGroup, UncheckGroup, EnableGroup,
      DisableGroup. It is also possible to convert an item to a group and back
      using the ItemToGroup and GroupToItem methods.
    * Added LoadFromFile, LoadFromStream, SaveToFile, and SaveToStream methods,
      which not only save the text of each item, but also the State of each item
      (i.e. cbChecked, cbUnchecked, cbGrayed) and the Enabled state of each
      item.
    * Added XP visual style support.
===============================================================================}

{$I RzComps.inc}

unit RzChkLst;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Classes,
  Controls,
  Forms,
  SysUtils,
  Messages,
  Windows,
  StdCtrls,
  Graphics,
  Menus,
  RzCommon,
  ImgList,
  RzLstBox;

type
  TRzCheckList = class( TRzCustomTabbedListBox )
  private
    FAboutInfo: TRzAboutInfo;
    FChangingItems: Boolean;
    FCheckItems: TStrings;
    FAllowGrayed: Boolean;
    FGlyphWidth: Integer;
    FGlyphHeight: Integer;

    FNumGlyphs: Integer;
    FCustomGlyphs: TBitmap;
    FCustomGlyphImages: TCustomImageList;
    FCustomGlyphImagesChangeLink: TChangeLink;
    FUseCustomGlyphs: Boolean;
    FTransparentColor: TColor;
    FWinMaskColor: TColor;
    FHighlightColor: TColor;
    FItemFillColor: TColor;
    FItemFocusColor: TColor;
    FItemFrameColor: TColor;

    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;

    FSelectedItem: Integer;
    FSaveCheckItems: TStringList;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FToggleOnItemClick: Boolean;
    FChangingState: Boolean;

    FOnChanging: TStateChangingEvent;
    FOnChange: TStateChangeEvent;

    // Internal Event Handlers
    procedure CustomGlyphsChanged( Sender: TObject );
    procedure CustomGlyphImagesChange( Sender: TObject );
    procedure ImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure WMChar( var Msg: TWMChar ); message wm_Char;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure UpdateItemHeight; override;

    function InitialTabStopOffset: Integer; override;

    procedure ToggleCheckState; virtual;
    procedure ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer );
    procedure SelectGlyph( Index: Integer; Glyph: TBitmap ); virtual;

    function OwnerDrawItemIndent: Integer; override;
    function HorzExtentPrefix: string; override;
    procedure DrawListItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;
    procedure UpdateGlyphDimensions;
    procedure InvalidateItemImage( Index: Integer );

    { Event Dispatch Methods }
    function CanChange( Index: Integer; NewState: TCheckBoxState ): Boolean; dynamic;
    procedure Change( Index: Integer; NewState: TCheckBoxState ); dynamic;

    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

    { Property Access Methods }
    function GetItems: TStrings; override;
    function GetItemChecked( Index: Integer ): Boolean; virtual;
    procedure SetItemChecked( Index: Integer; Value: Boolean ); virtual;
    function GetItemEnabled( Index: Integer ): Boolean; virtual;
    procedure SetItemEnabled( Index: Integer; Value: Boolean ); virtual;
    function GetItemState( Index: Integer ): TCheckBoxState; virtual;
    procedure SetItemState( Index: Integer; Value: TCheckBoxState ); virtual;
    function GetItemImageIndex( Index: Integer ): Integer; virtual;
    procedure SetItemImageIndex( Index: Integer; Value: Integer ); virtual;
    function GetItemDisabledIndex( Index: Integer ): Integer; virtual;
    procedure SetItemDisabledIndex( Index: Integer; Value: Integer ); virtual;

    procedure SetItems( Value: TStrings ); virtual;
    procedure SetAllowGrayed( Value: Boolean ); virtual;
    procedure SetCustomGlyphs( Value: TBitmap ); virtual;
    procedure SetCustomGlyphImages( Value: TCustomImageList ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
    procedure SetUseCustomGlyphs( Value: Boolean ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;
    procedure SetItemFillColor( Value: TColor ); virtual;
    procedure SetItemFocusColor( Value: TColor ); virtual;
    procedure SetItemFrameColor( Value: TColor ); virtual;
    procedure SetTransparentColor( Value: TColor ); virtual;
    procedure SetWinMaskColor( Value: TColor ); virtual;

    property GlyphWidth: Integer
      read FGlyphWidth;

    property GlyphHeight: Integer
      read FGlyphHeight;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function AddEx( const S: string; Checked: Boolean; Enabled: Boolean = True;
                    ImageIndex: Integer = -1; DisabledIndex: Integer = -1 ): Integer;
    procedure AddItem( Item: string; AObject: TObject ); override;
    procedure CheckAll;
    procedure UncheckAll;
    function ItemsChecked: Integer;

    {$IFDEF UNICODE}
    procedure LoadFromFile( const FileName: string ); overload;
    procedure LoadFromFile( const FileName: string; Encoding: TEncoding ); overload;
    procedure LoadFromStream( Stream: TStream ); overload;
    procedure LoadFromStream( Stream: TStream; Encoding: TEncoding ); overload;
    procedure SaveToFile( const FileName: string ); overload;
    procedure SaveToFile( const FileName: string; Encoding: TEncoding ); overload;
    procedure SaveToStream( Stream: TStream ); overload;
    procedure SaveToStream( Stream: TStream; Encoding: TEncoding ); overload;
    {$ELSE}
    procedure LoadFromFile( const FileName: string );
    procedure LoadFromStream( Stream: TStream );
    procedure SaveToFile( const FileName: string );
    procedure SaveToStream( Stream: TStream );
    {$ENDIF}

    procedure DefaultDrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

    procedure SetGroupState( GroupIndex: Integer; State: TCheckBoxState );
    procedure CheckGroup( GroupIndex: Integer );
    procedure UncheckGroup( GroupIndex: Integer );
    procedure EnableGroup( GroupIndex: Integer );
    procedure DisableGroup( GroupIndex: Integer );

    property ItemChecked[ Index: Integer ]: Boolean
      read GetItemChecked
      write SetItemChecked;

    property ItemEnabled[ Index: Integer ]: Boolean
      read GetItemEnabled
      write SetItemEnabled;

    property ItemState[ Index: Integer ]: TCheckBoxState
      read GetItemState
      write SetItemState;

    property ItemImageIndex[ Index: Integer ]: Integer
      read GetItemImageIndex
      write SetItemImageIndex;

    property ItemDisabledIndex[ Index: Integer ]: Integer
      read GetItemDisabledIndex
      write SetItemDisabledIndex;

  published
    { Items property is redeclared, so TRzCheckListStrings type can be used }
    property Items: TStrings
      read FCheckItems
      write SetItems;

    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowGrayed: Boolean
      read FAllowGrayed
      write SetAllowGrayed
      default False;

    property CustomGlyphs: TBitmap
      read FCustomGlyphs
      write SetCustomGlyphs;

    property CustomGlyphImages: TCustomImageList
      read FCustomGlyphImages
      write SetCustomGlyphImages;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property ItemFillColor: TColor
      read FItemFillColor
      write SetItemFillColor
      default clWindow;

    property ItemFocusColor: TColor
      read FItemFocusColor
      write SetItemFocusColor
      default clWindow;

    property ItemFrameColor: TColor
      read FItemFrameColor
      write SetItemFrameColor
      default clBtnShadow;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property UseCustomGlyphs: Boolean
      read FUseCustomGlyphs
      write SetUseCustomGlyphs
      default False;

    property TransparentColor: TColor
      read FTransparentColor
      write SetTransparentColor
      default clOlive;

    property WinMaskColor: TColor
      read FWinMaskColor
      write SetWinMaskColor
      default clLime;

    property OnChange: TStateChangeEvent
      read FOnChange
      write FOnChange;

    property OnChanging: TStateChangingEvent
      read FOnChanging
      write FOnChanging;

    property ToggleOnItemClick: Boolean
      read FToggleOnItemClick
      write FToggleOnItemClick
      default False;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property BeepOnInvalidKey;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ShowItemHints default False;
    property ExtendedSelect;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property GroupColor;
    property GroupColorFromTheme;
    property GroupFont;
    property HorzScrollBar;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property UseGradients;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDeleteItems;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMatch;
    property OnMeasureItem;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;



implementation

uses
  {&RAS}
  RTLConsts,
  Themes,
  Consts,
  RzCommonBitmaps,
  RzRadChk,
  RzGrafx;


const
  // DefaultNumGlyphs_CheckList is only used for CustomGlyphs property. The
  // CustomGlyphImages property can be connected to an image list containing
  // check states for a TRzCheckBox, which contains 9 or 12 images.
  DefaultNumGlyphs_CheckList = 6;

type
  TPacket = class
    State: TCheckBoxState;
    Enabled: Boolean;
    ImageIndex: Integer;
    DisabledIndex: Integer;
  end;


  TRzCheckListStrings = class( TStrings )
  private
    FPackets: TList;
    FCheckList: TRzCheckList;
    procedure ReadItemEnabled( Reader: TReader );
    procedure WriteItemEnabled( Writer: TWriter );
    procedure ReadItemState( Reader: TReader );
    procedure WriteItemState( Writer: TWriter );
    procedure ReadItemImageIndex( Reader: TReader );
    procedure WriteItemImageIndex( Writer: TWriter );
    procedure ReadItemDisabledIndex( Reader: TReader );
    procedure WriteItemDisabledIndex( Writer: TWriter );

    procedure ExtractPacketDetailsOrigFormat( Index: Integer; S: string );
    procedure ExtractPacketDetails( Index: Integer; S: string );
    function EmbedPacketDetails( Index: Integer ): string;
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    function Get( Index: Integer ): string; override;
    procedure Put( Index: Integer; const S: string ); override;
    function GetCount: Integer; override;
    function GetObject( Index: Integer ): TObject; override;
    function GetItemEnabled( Index: Integer ): Boolean;
    function GetItemState( Index: Integer ): TCheckBoxState;
    function GetItemImageIndex( Index: Integer ): Integer;
    function GetItemDisabledIndex( Index: Integer ): Integer;

    procedure PutObject( Index: Integer; AObject: TObject ); override;
    procedure SetItemEnabled( Index: Integer; Value: Boolean );
    procedure SetItemState( Index: Integer; Value: TCheckBoxState );
    procedure SetItemImageIndex( Index: Integer; Value: Integer );
    procedure SetItemDisabledIndex( Index: Integer; Value: Integer );
    procedure SetUpdateState( Updating: Boolean ); override;
    procedure AddStrings( Strings: TStrings ); override;
    function AddObjectPacket( const S: string; AObject: TObject; APacket: TPacket ): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add( const S: string ): Integer; override;
    procedure Clear; override;
    procedure Delete( Index: Integer ); override;
    procedure Insert( Index: Integer; const S: string ); override;
    procedure Move( CurIndex, NewIndex: Integer ); override;
    procedure Exchange( Index1, Index2: Integer ); override;

    procedure LoadFromStream( Stream: TStream ); override;
    procedure SaveToStream( Stream: TStream ); override;
    {$IFDEF UNICODE}
    procedure LoadFromStream( Stream: TStream; Encoding: TEncoding ); override;
    procedure SaveToStream( Stream: TStream; Encoding: TEncoding ); override;
    {$ENDIF}

    property ItemEnabled[ Index: Integer ]: Boolean
      read GetItemEnabled
      write SetItemEnabled;

    property ItemState[ Index: Integer ]: TCheckBoxState
      read GetItemState
      write SetItemState;

    property ItemImageIndex[ Index: Integer ]: Integer
      read GetItemImageIndex
      write SetItemImageIndex;

    property ItemDisabledIndex[ Index: Integer ]: Integer
      read GetItemDisabledIndex
      write SetItemDisabledIndex;
  end;


  // TPacketStringList used to hold check state info during
  // DestroyWnd/CreateWnd transition

  TPacketStringList = class( TStringList )
  private
    FPackets: TList;
  protected
    procedure AddStrings( Strings: TStrings ); override;
    function AddObjectPacket( const S: string; AObject: TObject; APacket: TPacket ): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject( const S: string; AObject: TObject ): Integer; override;
    procedure Clear; override;
  end;


function Max( A, B: Integer ): Integer;
begin
  if A >= B then
    Result := A
  else
    Result := B;
end;


{&RT}
{==========================}
{== TRzCheckList Methods ==}
{==========================}

constructor TRzCheckList.Create( AOwner: TComponent );
begin
  inherited;

  Items.Free;  { Release memory allocated by ancestor }
  FCheckItems := TRzCheckListStrings.Create;
  TRzCheckListStrings( FCheckItems ).FCheckList := Self;
  FChangingItems := False;

  FToggleOnItemClick := False;
  Style := lbOwnerDrawFixed;

  FAllowGrayed := False;
  FNumGlyphs := DefaultNumGlyphs_CheckList;
  FGlyphWidth := DefaultGlyphWidth;
  FGlyphHeight := DefaultGlyphHeight;

  FCustomGlyphs := TBitmap.Create;
  FCustomGlyphs.OnChange := CustomGlyphsChanged;
  FCustomGlyphImagesChangeLink := TChangeLink.Create;
  FCustomGlyphImagesChangeLink.OnChange := CustomGlyphImagesChange;
  FUseCustomGlyphs := False;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  FTransparentColor := clOlive;
  FWinMaskColor := clLime;
  FHighlightColor := clHighlight;
  FItemFillColor := clWindow;
  FItemFocusColor := clWindow;
  FItemFrameColor := clBtnShadow;

  UpdateItemHeight;
  ShowGroups := True;
  {&RCI}
end;


destructor TRzCheckList.Destroy;
begin
  FCustomGlyphs.Free;
  FCustomGlyphImagesChangeLink.Free;
  FImagesChangeLink.Free;
  FCheckItems.Free;
  FSaveCheckItems.Free;
  inherited;
end;


procedure TRzCheckList.CreateWnd;
begin
  inherited;
  if FSaveCheckItems <> nil then
  begin
    FCheckItems.Assign( FSaveCheckItems );
    TopIndex := FSaveTopIndex;
    ItemIndex := FSaveItemIndex;
    FSaveCheckItems.Free;
    FSaveCheckItems := nil;
  end;
  {&RV}
end;


procedure TRzCheckList.DestroyWnd;
begin
  if not ( csLoading in ComponentState ) then
  begin
    if FCheckItems.Count > 0 then
    begin
      FSaveCheckItems := TPacketStringList.Create;
      FSaveCheckItems.Assign( FCheckItems );
      FSaveTopIndex := TopIndex;
      FSaveItemIndex := ItemIndex;
    end;
  end;
  inherited;
end;


procedure TRzCheckList.Loaded;
begin
  inherited;
  if ( csDesigning in ComponentState ) and ( FCheckItems.Count > 0 ) then
    Repaint;
  if HorzScrollBar then
  begin
    SendMessage( Handle, lb_SetHorizontalExtent, 0, 0 );
    AdjustHorzExtent;
  end;

  {&RV}
end;


procedure TRzCheckList.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if Operation = opRemove then
  begin
    // Call access method so connections to link object can be cleared
    if AComponent = FCustomGlyphImages then
      SetCustomGlyphImages( nil )
    else if AComponent = FImages then
      SetImages( nil );
  end;
end;


procedure TRzCheckList.UpdateItemHeight;
begin
  ItemHeight := Max( FGlyphHeight + 4, GetMinFontHeight( Font ) );
end;


procedure TRzCheckList.ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer );
var
  DestRct: TRect;
begin
  DestRct := Rect( 0, 0, W, H );

  Bitmap.Width := W;
  Bitmap.Height := H;
  Bitmap.Canvas.CopyRect( DestRct, Source.Canvas, Rect( Index * W, 0, (Index + 1 ) * W, H ) );
end;


procedure TRzCheckList.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  UpdateItemHeight;
end;



function TRzCheckList.CanChange( Index: Integer; NewState: TCheckBoxState ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, Index, NewState, Result );
end;


procedure TRzCheckList.Change( Index: Integer; NewState: TCheckBoxState );
begin
  if Assigned( FOnChange ) then
    FOnChange( Self, Index, NewState );
end;


function TRzCheckList.GetItems: TStrings;
begin
  Result := Items;
end;


function TRzCheckList.AddEx( const S: string; Checked: Boolean; Enabled: Boolean = True;
                             ImageIndex: Integer = -1; DisabledIndex: Integer = -1 ): Integer;
begin
  Result := Items.Add( S );
  ItemChecked[ Result ] := Checked;
  ItemEnabled[ Result ] := Enabled;
  if ImageIndex <> -1 then
    ItemImageIndex[ Result ] := ImageIndex;
  if DisabledIndex <> -1 then
    ItemDisabledIndex[ Result ] := DisabledIndex;
end;


procedure TRzCheckList.AddItem( Item: string; AObject: TObject );
var
  S: string;
begin
  SetString( S, PChar( Item ), StrLen( PChar( Item ) ) );
  GetItems.AddObject( S, AObject );
end;


procedure TRzCheckList.CheckAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    ItemChecked[ I ] := True;
end;


procedure TRzCheckList.UncheckAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    ItemChecked[ I ] := False;
end;


function TRzCheckList.ItemsChecked: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if ItemChecked[ I ] and not ItemIsGroup[ I ] then
      Inc( Result );
  end;
end;


function TRzCheckList.GetItemChecked( Index: Integer ): Boolean;
begin
  Result := TRzCheckListStrings( Items ).ItemState[ Index ] = cbChecked;
end;


procedure TRzCheckList.SetItemChecked( Index: Integer; Value: Boolean );
begin
  if Value then
    ItemState[ Index ] := cbChecked
  else
    ItemState[ Index ] := cbUnchecked;
end;


function TRzCheckList.GetItemEnabled( Index: Integer ): Boolean;
begin
  Result := TRzCheckListStrings( Items ).ItemEnabled[ Index ];
end;


procedure TRzCheckList.SetItemEnabled( Index: Integer; Value: Boolean );
begin
  if ItemEnabled[ Index ] <> Value then
  begin
    TRzCheckListStrings( Items ).ItemEnabled[ Index ] := Value;
    Invalidate;
  end;
end;


function TRzCheckList.GetItemState( Index: Integer ): TCheckBoxState;
begin
  Result := TRzCheckListStrings( Items ).ItemState[ Index ];
end;


procedure TRzCheckList.SetItemState( Index: Integer; Value: TCheckBoxState );
var
  R, ItemRct: TRect;
  Offset: Integer;
begin
  if ItemState[ Index ] <> Value then
  begin
    TRzCheckListStrings( Items ).ItemState[ Index ] := Value;

    // Repaint the Checkbox
    Offset := ( ItemHeight - FGlyphHeight ) div 2;
    R := ItemRect( Index );
    if not UseRightToLeftAlignment then
      ItemRct := Rect( R.Left, R.Top + Offset, R.Left + FGlyphWidth + 4, R.Top + ItemHeight - Offset )
    else
      ItemRct := Rect( R.Right - FGlyphWidth - 4, R.Top + Offset, R.Right, R.Top + ItemHeight - Offset );
    InvalidateRect( Handle, @ItemRct, False );
  end;
end;


function TRzCheckList.GetItemImageIndex( Index: Integer ): Integer;
begin
  Result := TRzCheckListStrings( Items ).ItemImageIndex[ Index ];
end;


procedure TRzCheckList.SetItemImageIndex( Index: Integer; Value: Integer );
begin
  if ItemImageIndex[ Index ] <> Value then
  begin
    TRzCheckListStrings( Items ).ItemImageIndex[ Index ] := Value;
    InvalidateItemImage( Index );
  end;
end;


function TRzCheckList.GetItemDisabledIndex( Index: Integer ): Integer;
begin
  Result := TRzCheckListStrings( Items ).ItemDisabledIndex[ Index ];
end;


procedure TRzCheckList.SetItemDisabledIndex( Index: Integer; Value: Integer );
begin
  if ItemDisabledIndex[ Index ] <> Value then
  begin
    TRzCheckListStrings( Items ).ItemDisabledIndex[ Index ] := Value;
    InvalidateItemImage( Index );
  end;
end;


procedure TRzCheckList.InvalidateItemImage( Index: Integer );
var
  R, ItemRct: TRect;
  VOffset: Integer;
begin
  if FImages <> nil then
  begin
    VOffset := ( ItemHeight - FImages.Height ) div 2;
    R := ItemRect( Index );

    if not UseRightToLeftAlignment then
      ItemRct := Rect( R.Left + FGlyphWidth + 4, R.Top + VOffset,
                       R.Left + OwnerDrawItemIndent, R.Top + ItemHeight - VOffset )
    else
      ItemRct := Rect( R.Right - OwnerDrawItemIndent, R.Top + VOffset,
                       R.Right - FGlyphWidth - 4, R.Top + ItemHeight - VOffset );
    InvalidateRect( Handle, @ItemRct, True );
  end;
end;


procedure TRzCheckList.SetItems( Value: TStrings );
begin
  Items.Assign( Value );
end;


procedure TRzCheckList.SetAllowGrayed( Value: Boolean );
begin
  if FAllowGrayed <> Value then
    FAllowGrayed := Value;
end;


procedure TRzCheckList.SetCustomGlyphs( Value: TBitmap );
begin
  FCustomGlyphs.Assign( Value );
end;

procedure TRzCheckList.CustomGlyphsChanged( Sender: TObject );
begin
  if not ( csLoading in ComponentState ) then
  begin
    UseCustomGlyphs := not FCustomGlyphs.Empty;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetCustomGlyphImages( Value: TCustomImageList );
begin
  if FCustomGlyphImages <> nil then
    FCustomGlyphImages.UnRegisterChanges( FCustomGlyphImagesChangeLink );

  FCustomGlyphImages := Value;

  if FCustomGlyphImages <> nil then
  begin
    FCustomGlyphImages.RegisterChanges( FCustomGlyphImagesChangeLink );
    FCustomGlyphImages.FreeNotification( Self );

    UseCustomGlyphs := FCustomGlyphImages.Count > 0;
  end;
  Invalidate;
end;


procedure TRzCheckList.CustomGlyphImagesChange( Sender: TObject );
begin
  if Sender = CustomGlyphImages then
  begin
    UseCustomGlyphs := FCustomGlyphImages.Count > 0;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetUseCustomGlyphs( Value: Boolean );
begin
  if FUseCustomGlyphs <> Value then
  begin
    FUseCustomGlyphs := Value;
    if FUseCustomGlyphs then
    begin
      if FCustomGlyphImages <> nil then
      begin
        FGlyphWidth := FCustomGlyphImages.Width;
        FGlyphHeight := FCustomGlyphImages.Height;
      end
      else
      begin
        FGlyphWidth := FCustomGlyphs.Width div FNumGlyphs;
        FGlyphHeight := FCustomGlyphs.Height;
      end;
    end
    else
    begin
      FGlyphWidth := DefaultGlyphWidth;
      FGlyphHeight := DefaultGlyphHeight;
    end;
    UpdateItemHeight;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetImages( Value: TCustomImageList );
begin
  if FImages <> nil then
    FImages.UnRegisterChanges( FImagesChangeLink );

  FImages := Value;

  if FImages <> nil then
  begin
    FImages.RegisterChanges( FImagesChangeLink );
    FImages.FreeNotification( Self );
  end;
  Invalidate;
end;


procedure TRzCheckList.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
    Invalidate;
end;


procedure TRzCheckList.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetItemFillColor( Value: TColor );
begin
  if FItemFillColor <> Value then
  begin
    FItemFillColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetItemFocusColor( Value: TColor );
begin
  if FItemFocusColor <> Value then
  begin
    FItemFocusColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetItemFrameColor( Value: TColor );
begin
  if FItemFrameColor <> Value then
  begin
    FItemFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetTransparentColor( Value: TColor );
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SetWinMaskColor( Value: TColor );
begin
  if FWinMaskColor <> Value then
  begin
    FWinMaskColor := Value;
    Invalidate;
  end;
end;


procedure TRzCheckList.SelectGlyph( Index: Integer; Glyph: TBitmap );
var
  R: TRect;
  Offset: Integer;
  DestBmp, SourceBmp: TBitmap;
  ElementDetails: TThemedElementDetails;
  DisplayState: TRzButtonDisplayState;
begin
  R := Rect( 0, 0, FGlyphWidth, FGlyphHeight );

  if not FUseCustomGlyphs then
  begin
    // Test for XP/Vista Themes first...
    if ActiveStyleServicesEnabled then
    begin
      case ItemState[ Index ] of
        cbUnchecked:
        begin
          if ItemEnabled[ Index ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedNormal )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedDisabled );
        end;

        cbChecked:
        begin
          if ItemEnabled[ Index ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedNormal )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedDisabled );
        end;

        else // cbGrayed
        begin
          if ItemEnabled[ Index ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedNormal )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedDisabled );
        end;
      end;
      ActiveStyleServices.DrawElement( Glyph.Canvas.Handle, ElementDetails, R );
    end
    else // No Themes - Use HotTrack Flat Style
    begin
      if ItemEnabled[ Index ] then
        DisplayState := bdsNormal
      else
        DisplayState := bdsDisabled;

      DrawCheckBox( Glyph.Canvas, R, ItemState[ Index ], DisplayState, Focused,
                    htsInterior, ItemFrameColor, HighlightColor, ItemFillColor,
                    ItemFocusColor, DisabledColor, clLime, clRed, False, False,
                    clWindow );
    end;
  end
  else // Use Custom Glyphs
  begin
    DestBmp := TBitmap.Create;
    try
      if FCustomGlyphImages <> nil then
      begin
        DestBmp.Width := FGlyphWidth;
        DestBmp.Height := FGlyphHeight;

        DestBmp.Canvas.Brush.Color := FTransparentColor;
        DestBmp.Canvas.FillRect( Rect( 0, 0, FGlyphWidth, FGlyphHeight ) );

        if ItemEnabled[ Index ] then
        begin
          case ItemState[ Index ] of
            cbUnchecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 0 );

            cbChecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 1 );

            cbGrayed:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 2 );
          end;
        end
        else
        begin
          if FCustomGlyphImages.Count > 6 then
            Offset := 3  // ImageList contains full Check States, so skip "down" states
          else
            Offset := 0;
          case ItemState[ Index ] of
            cbUnchecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 3 + Offset );

            cbChecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 4 + Offset );

            cbGrayed:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 5 + Offset );
          end;
        end;

      end
      else // Use FCustomGlyphs bitmap
      begin
        SourceBmp := FCustomGlyphs;

        if ItemEnabled[ Index ] then
        begin
          case ItemState[ Index ] of
            cbUnchecked:
              ExtractGlyph( 0, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbChecked:
              ExtractGlyph( 1, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbGrayed:
              ExtractGlyph( 2, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
          end;
        end
        else
        begin
          case ItemState[ Index ] of
            cbUnchecked:
              ExtractGlyph( 3, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbChecked:
              ExtractGlyph( 4, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbGrayed:
              ExtractGlyph( 5, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
          end;
        end;
      end;

      Glyph.Assign( DestBmp );

    finally
      DestBmp.Free;
    end;
  end;
end; {= TRzCheckList.SelectGlyph =}



function TRzCheckList.OwnerDrawItemIndent: Integer;
begin
  Result := FGlyphWidth + 8;
  if FImages <> nil then
    Inc( Result, FImages.Width + 4 );
end;


function TRzCheckList.HorzExtentPrefix: string;
begin
  // The leading tab characters offsets the text in the check list to take into
  // account the space occupied by the check box glyph.
  Result := #9;
end;


procedure TRzCheckList.DefaultDrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  TextOffset: Integer;
  TabCount, I: Integer;
  TabArray: TRzTabArray;
  XOrigin: Integer;
begin
  TextOffset := ( ItemHeight - Canvas.TextHeight( 'Pp' ) ) div 2;

  GetTabArray( TabCount, TabArray );
  for I := 0 to TabCount - 1 do
    TabArray[ I ] := Round( TabArray[ I ] * FDialogUnits / 4 );

  if not UseRightToLeftAlignment then
    XOrigin := Rect.Left + 2
  else
  begin
    XOrigin := Rect.Right - 2;
    SetTextAlign( Canvas.Handle, ta_Right or ta_Top or ta_RtlReading );
  end;
  TabbedTextOut( Canvas.Handle, XOrigin, Rect.Top + TextOffset, PChar( Items[ Index ] ),
                 Length( Items[ Index ] ), TabCount, TabArray, 0 );
end;


procedure TRzCheckList.DrawListItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  FGlyph, Phase1Bmp, Phase2Bmp: TBitmap;
  R: TRect;
  L, ImageWidth, ImageVOffset, GlyphVOffset: Integer;
begin
  if not FChangingItems then
  begin
    UpdateGlyphDimensions;

    FGlyph := TBitmap.Create;
    FGlyph.Width := FGlyphWidth;
    FGlyph.Height := FGlyphHeight;

    Phase1Bmp := TBitmap.Create;
    Phase1Bmp.Width := FGlyphWidth;
    Phase1Bmp.Height := FGlyphHeight;

    Phase2Bmp := TBitmap.Create;
    Phase2Bmp.Width := FGlyphWidth;
    Phase2Bmp.Height := FGlyphHeight;

    // If Image List is being used, then we need to move the check box over more.
    if FImages <> nil then
      ImageWidth := FImages.Width + 4
    else
      ImageWidth := 0;

    try
      SelectGlyph( Index, FGlyph );

      Canvas.FillRect( Rect );   { Clear area for icon and text }

      R := Classes.Rect( 0, 0, FGlyphWidth, FGlyphHeight );

      Phase1Bmp.Canvas.Brush.Color := Color;
      Phase1Bmp.Canvas.BrushCopy( R, FGlyph, R, FTransparentColor );

      if FUseCustomGlyphs then
      begin
        // Replace WinMaskColor with clWindow color value
        Phase2Bmp.Canvas.Brush.Color := clWindow;
        Phase2Bmp.Canvas.BrushCopy( R, Phase1Bmp, R, FWinMaskColor );
      end
      else
      begin
        Phase2Bmp.Assign( Phase1Bmp );
      end;

      GlyphVOffset := ( ItemHeight - FGlyphHeight ) div 2;

      if not UseRightToLeftAlignment then
        Canvas.Draw( Rect.Left - FGlyphWidth - ImageWidth - 4, Rect.Top + GlyphVOffset, Phase2Bmp )
      else
        Canvas.Draw( Rect.Right + ImageWidth + 4, Rect.Top + GlyphVOffset, Phase2Bmp );
    finally
      Phase2Bmp.Free;
      Phase1Bmp.Free;
      FGlyph.Free;
    end;

    // Draw the Item Image if specified
    if FImages <> nil then
    begin
      ImageVOffset := ( ItemHeight - FImages.Height ) div 2;

      if not UseRightToLeftAlignment then
        L := Rect.Left - ImageWidth
      else
        L := Rect.Right + 4;

      if ItemDisabledIndex[ Index ] <> -1 then
      begin
        if ItemEnabled[ Index ] then
        begin
          if ItemImageIndex[ Index ] <> -1 then
            FImages.Draw( Canvas, L, Rect.Top + ImageVOffset, ItemImageIndex[ Index ] );
        end
        else
          FImages.Draw( Canvas, L, Rect.Top + ImageVOffset, ItemDisabledIndex[ Index ] );
      end
      else if ItemImageIndex[ Index ] <> -1 then
        FImages.Draw( Canvas, L, Rect.Top + ImageVOffset, ItemImageIndex[ Index ], Enabled );
    end;



    Canvas.Font := Font;

    if UsingSystemStyle then
    begin
      if not ItemEnabled[ Index ] or not Enabled then
      begin
        if ColorToRGB( Color ) = ColorToRGB( clBtnShadow ) then
          Canvas.Font.Color := clBtnFace
        else
          Canvas.Font.Color := clBtnShadow;
      end
      else
        Canvas.Font.Color := Font.Color;
    end
    else // VCL Styles
    begin
      if not ItemEnabled[ Index ] or not Enabled then
        Canvas.Font.Color := ActiveStyleFontColor( sfListItemTextDisabled )
      else
        Canvas.Font.Color := ActiveStyleFontColor( sfListItemTextNormal );
    end;

    // Clip text to Rect
    IntersectClipRect( Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom );
    try
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end;

      if not Assigned( OnDrawItem ) then
        DefaultDrawItem( Index, Rect, State )
      else
        OnDrawItem( Self, Index, Rect, State );
    finally
      // Removing clipping region
      SelectClipRgn( Canvas.Handle, 0 );
    end;
  end;
end; {= TRzCheckList.DrawListItem =}


procedure TRzCheckList.UpdateGlyphDimensions;
{$IFDEF VCL160_OR_HIGHER}
(*
var
  Details: TThemedElementDetails;
  Size: TSize;
*)
{$ENDIF}
begin
  {$IFDEF VCL160_OR_HIGHER}
  if not UsingSystemStyle and not UseCustomGlyphs then
  begin
    // GetElementSize does not return the correct value for a check box.
    // In fact, Size is set to (13,13) regardless of the size of the actual
    // check box image.  Therefore, if custom styles are used
    // we set the glyph width and height to 14, which is the size used for
    // radio buttons in the styles that are distributed with Delphi.
    // Once GetElementSize is fixed, we can change this code.

    if ( FGlyphWidth <> 14 ) or ( FGlyphHeight <> 14 ) then
    begin
      FGlyphWidth := 14;
      FGlyphHeight := 14;
    end;

    (*
    Details := StyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
    StyleServices.GetElementSize( Canvas.Handle, Details, esActual, Size );
    if ( FGlyphWidth <> Size.cx ) or ( FGlyphHeight <> Size.cy ) then
    begin
      FGlyphWidth := Size.cx;
      FGlyphHeight := Size.cy;
    end;
    *)
  end;
  {$ENDIF}
end;


procedure TRzCheckList.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  R: TRect;
  Pt: TPoint;
  Idx: Integer;
begin
  if Button = mbLeft then
  begin
    Idx := ItemAtPos( Point( X, Y ), True );
    if Idx <> -1 then
    begin
      R := ItemRect( Idx );
      if not UseRightToLeftAlignment then
        R.Right := R.Left + FGlyphWidth + 4
      else
        R.Left := R.Right - FGlyphWidth - 4;
      Pt := Point( X, Y );
      if FToggleOnItemClick or PtInRect( R, Pt ) then
      begin
        FChangingState := True;
        FSelectedItem := ItemIndex;
      end;
    end;
  end
  else if Button = mbRight then
  begin
    Idx := ItemAtPos( Point( X, Y ), True );
    if Idx <> -1 then
    begin
      if Idx <> ItemIndex then
      begin
        ItemIndex := Idx;
        Click;
      end;
    end;
  end;

  inherited;
end;


procedure TRzCheckList.ToggleCheckState;
var
  NewState: TCheckBoxState;
begin
  if ( ItemIndex = -1 ) or not ItemEnabled[ ItemIndex ] or ItemIsGroup[ ItemIndex ] then
    Exit;

  case ItemState[ ItemIndex ] of
    cbUnchecked:
      if FAllowGrayed then
        NewState := cbGrayed
      else
        NewState := cbChecked;

    cbChecked:
      NewState := cbUnchecked;

    cbGrayed:
      NewState := cbChecked;
  else
    NewState := cbChecked;
  end;

  if CanChange( ItemIndex, NewState ) then
  begin
    ItemState[ ItemIndex ] := NewState;
    FChangingState := False;
    Change( ItemIndex, NewState );
  end
  else
    FChangingState := False;
end; {= TRzCheckList.ToggleCheckState =}


function TRzCheckList.InitialTabStopOffset: Integer;
begin
  Result := GlyphWidth;
end;


procedure TRzCheckList.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if ( Button = mbLeft ) and FChangingState and ( ItemIndex = FSelectedItem ) and
     PtInRect( ClientRect, Point( X, Y ) ) then
  begin
    ToggleCheckState;
  end;

  inherited;
end;


procedure TRzCheckList.KeyDown( var Key: Word; Shift: TShiftState );
begin
  if Key = vk_Space then
    FChangingState := True;
  FSelectedItem := ItemIndex;

  inherited;
end;


procedure TRzCheckList.KeyUp( var Key: Word; Shift: TShiftState );
begin
  if FChangingState and ( Key = vk_Space ) then
    ToggleCheckState;

  inherited;
end;


procedure TRzCheckList.WMChar( var Msg: TWMChar );
begin
  if Msg.CharCode <> vk_Space then
    inherited;
end;


procedure TRzCheckList.CMEnabledChanged( var Msg: TMessage );
var
  I: Integer;
begin
  inherited;
  for I := 0 to Items.Count - 1 do
    ItemEnabled[ I ] := Enabled;
end;



procedure TRzCheckList.LoadFromFile( const FileName: string );
begin
  Items.LoadFromFile( FileName );
end;


{$IFDEF UNICODE}

procedure TRzCheckList.LoadFromFile( const FileName: string; Encoding: TEncoding );
begin
  Items.LoadFromFile( FileName, Encoding );
end;

{$ENDIF}


procedure TRzCheckList.LoadFromStream( Stream: TStream );
begin
  Items.LoadFromStream( Stream );
end;


{$IFDEF UNICODE}

procedure TRzCheckList.LoadFromStream( Stream: TStream; Encoding: TEncoding );
begin
  Items.LoadFromStream( Stream, Encoding );
end;

{$ENDIF}


procedure TRzCheckList.SaveToFile( const FileName: string );
begin
  Items.SaveToFile( FileName );
end;


{$IFDEF UNICODE}

procedure TRzCheckList.SaveToFile( const FileName: string; Encoding: TEncoding );
begin
  Items.SaveToFile( FileName, Encoding );
end;

{$ENDIF}


procedure TRzCheckList.SaveToStream( Stream: TStream );
begin
  Items.SaveToStream( Stream );
end;


{$IFDEF UNICODE}

procedure TRzCheckList.SaveToStream( Stream: TStream; Encoding: TEncoding );
begin
  Items.SaveToStream( Stream, Encoding );
end;

{$ENDIF}


procedure TRzCheckList.SetGroupState( GroupIndex: Integer; State: TCheckBoxState );
var
  I: Integer;
begin
  for I := ItemIndexOfGroup( GroupIndex ) + 1 to Items.Count - 1 do
  begin
    if ItemIsGroup[ I ] then                               // Found the next category - we're outta here.
      Exit;

    ItemState[ I ] := State;
  end;
end;

procedure TRzCheckList.CheckGroup( GroupIndex: Integer );
var
  I: Integer;
begin
  for I := ItemIndexOfGroup( GroupIndex ) + 1 to Items.Count - 1 do
  begin
    if ItemIsGroup[ I ] then                               // Found the next category - we're outta here.
      Exit;

    ItemChecked[ I ] := True;
  end;
end;


procedure TRzCheckList.UncheckGroup( GroupIndex: Integer );
var
  I: Integer;
begin
  for I := ItemIndexOfGroup( GroupIndex ) + 1 to Items.Count - 1 do
  begin
    if ItemIsGroup[ I ] then                               // Found the next category - we're outta here.
      Exit;

    ItemChecked[ I ] := False;
  end;
end;


procedure TRzCheckList.EnableGroup( GroupIndex: Integer );
var
  I: Integer;
begin
  for I := ItemIndexOfGroup( GroupIndex ) + 1 to Items.Count - 1 do
  begin
    if ItemIsGroup[ I ] then                               // Found the next category - we're outta here.
      Exit;

    ItemEnabled[ I ] := True;
  end;
end;


procedure TRzCheckList.DisableGroup( GroupIndex: Integer );
var
  I: Integer;
begin
  for I := ItemIndexOfGroup( GroupIndex ) + 1 to Items.Count - 1 do
  begin
    if ItemIsGroup[ I ] then                               // Found the next category - we're outta here.
      Exit;

    ItemEnabled[ I ] := False;
  end;
end;


{=================================}
{== TRzCheckListStrings Methods ==}
{=================================}

constructor TRzCheckListStrings.Create;
begin
  inherited;
  FPackets := TList.Create;
end;


destructor TRzCheckListStrings.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPackets.Count - 1 do
    TPacket( FPackets[ I ] ).Free;
  FPackets.Free;
  inherited;
end;


procedure TRzCheckListStrings.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'ItemEnabled', ReadItemEnabled, WriteItemEnabled, Count > 0 );
  Filer.DefineProperty( 'ItemState', ReadItemState, WriteItemState, Count > 0 );
  Filer.DefineProperty( 'ItemImageIndex', ReadItemImageIndex, WriteItemImageIndex,
                        ( Count > 0 ) and ( FCheckList.FImages <> nil ) );
  Filer.DefineProperty( 'ItemDisabledIndex', ReadItemDisabledIndex, WriteItemDisabledIndex,
                        ( Count > 0 ) and ( FCheckList.FImages <> nil ) );
end;


procedure TRzCheckListStrings.ReadItemEnabled( Reader: TReader );
var
  I: Integer;
begin
  Reader.ReadListBegin;
  I := 0;
  while not Reader.EndOfList do
  begin
    ItemEnabled[ I ] := Reader.ReadBoolean;
    Inc( I );
  end;
  Reader.ReadListEnd;
end;


procedure TRzCheckListStrings.WriteItemEnabled( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteBoolean( ItemEnabled[ I ] );
  Writer.WriteListEnd;
end;


procedure TRzCheckListStrings.ReadItemState( Reader: TReader );
var
  I: Integer;
begin
  Reader.ReadListBegin;
  I := 0;
  while not Reader.EndOfList do
  begin
    ItemState[ I ] := TCheckBoxState( Reader.ReadInteger );
    Inc( I );
  end;
  Reader.ReadListEnd;
end;


procedure TRzCheckListStrings.WriteItemState( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger( Ord( ItemState[ I ] ) );
  Writer.WriteListEnd;
end;


procedure TRzCheckListStrings.ReadItemImageIndex( Reader: TReader );
var
  I: Integer;
begin
  Reader.ReadListBegin;
  I := 0;
  while not Reader.EndOfList do
  begin
    ItemImageIndex[ I ] := Reader.ReadInteger;
    Inc( I );
  end;
  Reader.ReadListEnd;
end;


procedure TRzCheckListStrings.WriteItemImageIndex( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger( ItemImageIndex[ I ] );
  Writer.WriteListEnd;
end;


procedure TRzCheckListStrings.ReadItemDisabledIndex( Reader: TReader );
var
  I: Integer;
begin
  Reader.ReadListBegin;
  I := 0;
  while not Reader.EndOfList do
  begin
    ItemDisabledIndex[ I ] := Reader.ReadInteger;
    Inc( I );
  end;
  Reader.ReadListEnd;
end;


procedure TRzCheckListStrings.WriteItemDisabledIndex( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger( ItemDisabledIndex[ I ] );
  Writer.WriteListEnd;
end;


function TRzCheckListStrings.GetCount: Integer;
begin
  if FCheckList = nil then
  begin
    Result := lb_Err;
    Exit;
  end;
  Result := SendMessage( FCheckList.Handle, lb_GetCount, 0, 0 );
end;


function TRzCheckListStrings.Get( Index: Integer ): string;
var
  Len: Integer;
begin
  if FCheckList = nil then
    Exit;

  Len := SendMessage( FCheckList.Handle, lb_GetTextLen, Index, 0 );
  if Len = LB_ERR then
    Error( SListIndexError, Index );

  SetLength( Result, Len );
  if Len <> 0 then
  begin
    Len := SendTextMessage( FCheckList.Handle, lb_GetText, Index, Result );
    SetLength( Result, Len );  // LB_GETTEXTLEN isn't guaranteed to be accurate
  end;
end;



procedure TRzCheckListStrings.Put( Index: Integer; const S: string );
var
  TempState: TCheckBoxState;
  TempEnabled: Boolean;
  TempImageIndex, TempDisabledIndex: Integer;
  I: Integer;
begin
  if FCheckList = nil then
    Exit;
  I := FCheckList.ItemIndex;
  TempState := ItemState[ Index ];
  TempEnabled := ItemEnabled[ Index ];
  TempImageIndex := ItemImageIndex[ Index ];
  TempDisabledIndex := ItemDisabledIndex[ Index ];
  inherited;
  ItemState[ Index ] := TempState;
  ItemEnabled[ Index ] := TempEnabled;
  ItemImageIndex[ Index ] := TempImageIndex;
  ItemDisabledIndex[ Index ] := TempDisabledIndex;
  FCheckList.ItemIndex := I;
end;


function TRzCheckListStrings.GetItemEnabled( Index: Integer ): Boolean;
begin
  Result := TPacket( FPackets[ Index ] ).Enabled;
end;


procedure TRzCheckListStrings.SetItemEnabled( Index: Integer; Value: Boolean );
begin
  TPacket( FPackets[ Index ] ).Enabled := Value;
end;


function TRzCheckListStrings.GetItemState( Index: Integer ): TCheckBoxState;
begin
  Result := TPacket( FPackets[ Index ] ).State;
end;


procedure TRzCheckListStrings.SetItemState( Index: Integer; Value: TCheckBoxState );
begin
  TPacket( FPackets[ Index ] ).State := Value;
end;


function TRzCheckListStrings.GetItemImageIndex( Index: Integer ): Integer;
begin
  Result := TPacket( FPackets[ Index ] ).ImageIndex;
end;


procedure TRzCheckListStrings.SetItemImageIndex( Index: Integer; Value: Integer );
begin
  TPacket( FPackets[ Index ] ).ImageIndex := Value;
end;


function TRzCheckListStrings.GetItemDisabledIndex( Index: Integer ): Integer;
begin
  Result := TPacket( FPackets[ Index ] ).DisabledIndex;
end;


procedure TRzCheckListStrings.SetItemDisabledIndex( Index: Integer; Value: Integer );
begin
  TPacket( FPackets[ Index ] ).DisabledIndex := Value;
end;


function TRzCheckListStrings.GetObject( Index: Integer ): TObject;
begin
  if FCheckList = nil then
  begin
    Result := nil;
    Exit;
  end;
  Result := TObject( SendMessage( FCheckList.Handle, lb_GetItemData, Index, 0 ) );

  if Longint( Result ) = lb_Err then
    raise EStringListError.Create( SListIndexError );
end;


procedure TRzCheckListStrings.PutObject( Index: Integer; AObject: TObject );
begin
  if FCheckList = nil then
    Exit;
  SendMessage( FCheckList.Handle, lb_SetItemData, Index, LParam( AObject ) );
end;


procedure TRzCheckListStrings.Move( CurIndex, NewIndex: Integer );
var
  CurState: TCheckBoxState;
  CurEnabled: Boolean;
  CurImageIndex, CurDisabledIndex: Integer;
begin
  if CurIndex <> NewIndex then
  begin
    CurState := ItemState[ CurIndex ];
    CurEnabled := ItemEnabled[ CurIndex ];
    CurImageIndex := ItemImageIndex[ CurIndex ];
    CurDisabledIndex := ItemDisabledIndex[ CurIndex ];
    inherited;
    ItemState[ NewIndex ] := CurState;
    ItemEnabled[ NewIndex ] := CurEnabled;
    ItemImageIndex[ NewIndex ] := CurImageIndex;
    ItemDisabledIndex[ NewIndex ] := CurDisabledIndex;
  end;
end;


procedure TRzCheckListStrings.Exchange( Index1, Index2: Integer );
var
  CurState1, CurState2: TCheckBoxState;
  CurEnabled1, CurEnabled2: Boolean;
  CurImageIndex1, CurImageIndex2: Integer;
  CurDisabledIndex1, CurDisabledIndex2: Integer;
begin
  if Index1 <> Index2 then
  begin
    BeginUpdate;
    try
      CurState1 := ItemState[ Index1 ];
      CurEnabled1 := ItemEnabled[ Index1 ];
      CurImageIndex1 := ItemImageIndex[ Index1 ];
      CurDisabledIndex1 := ItemDisabledIndex[ Index1 ];
      CurState2 := ItemState[ Index2 ];
      CurEnabled2 := ItemEnabled[ Index2 ];
      CurImageIndex2 := ItemImageIndex[ Index2 ];
      CurDisabledIndex2 := ItemDisabledIndex[ Index2 ];
      inherited;
      // Exchange the states
      ItemState[ Index1 ] := CurState2;
      ItemEnabled[ Index1 ] := CurEnabled2;
      ItemImageIndex[ Index1 ] := CurImageIndex2;
      ItemDisabledIndex[ Index1 ] := CurDisabledIndex2;
      ItemState[ Index2 ] := CurState1;
      ItemEnabled[ Index2 ] := CurEnabled1;
      ItemImageIndex[ Index2 ] := CurImageIndex1;
      ItemDisabledIndex[ Index2 ] := CurDisabledIndex1;
    finally
      EndUpdate;
    end;
  end;
end;


procedure TRzCheckListStrings.ExtractPacketDetailsOrigFormat( Index: Integer; S: string );
var
  E: Boolean;
  St: TCheckBoxState;
begin
  // State Information stored in file
  E := S[ 2 ] = '1';
  St := TCheckBoxState( StrToIntDef( S[ 4 ], 0 ) );

  System.Delete( S, 1, 5 ); // Delete package details

  Add( S );
  ItemEnabled[ Index ] := E;
  ItemState[ Index ] := St;
end;


procedure TRzCheckListStrings.ExtractPacketDetails( Index: Integer; S: string );
var
  E: Boolean;
  St: TCheckBoxState;
  II, DI: Integer;
  IIStr, DIStr: string;
begin
  // State Information stored in file
  E := S[ 2 ] = '1';
  St := TCheckBoxState( StrToIntDef( S[ 4 ], 0 ) );

  IIStr := System.Copy( S, 6, 3 );
  II := StrToIntDef( IIStr, -1 );

  DIStr := System.Copy( S, 10, 3 );
  DI := StrToIntDef( DIStr, -1 );

  System.Delete( S, 1, 13 ); // Delete packet details

  Add( S );
  ItemEnabled[ Index ] := E;
  ItemState[ Index ] := St;
  ItemImageIndex[ Index ] := II;
  ItemDisabledIndex[ Index ] := DI;
end;


function TRzCheckListStrings.EmbedPacketDetails( Index: Integer ): string;
const
  EndOfLine = #13#10;
var
  E, St, II, DI: Integer;
  IIStr, DIStr: string;
begin
  E := Ord( ItemEnabled[ Index ] );
  St := Ord( ItemState[ Index ] );

  II := ItemImageIndex[ Index ];
  if II = -1 then
    IIStr := '---'
  else
    IIStr := Format( '%.3d', [ II ] );

  DI := ItemDisabledIndex[ Index ];
  if DI = -1 then
    DIStr := '---'
  else
    DIStr := Format( '%.3d', [ DI ] );

  // Old Format
  // S := Format( '&%d %d %s %s', [ E, St, Get( Index ), EndOfLine ] );

  Result := Format( '@%d %d %s %s %s%s', [ E, St, IIStr, DIStr, Get( Index ), EndOfLine ] );
end;


procedure TRzCheckListStrings.AddStrings( Strings: TStrings );
var
  I: Integer;
  S: string;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
    begin
      if Strings is TPacketStringList then
        AddObjectPacket( Strings[ I ], Strings.Objects[ I ],
                         TPacketStringList( Strings ).FPackets[ I ] )
      else if Strings is TRzCheckListStrings then
        AddObjectPacket( Strings[ I ], Strings.Objects[ I ],
                         TRzCheckListStrings( Strings ).FPackets[ I ] )
      else
      begin
        S := Strings[ I ];
        if S[ 1 ] = '&' then
          ExtractPacketDetailsOrigFormat( I, S )
        else if S[ 1 ] = '@' then
          ExtractPacketDetails( I, S )
        else
          AddObject( Strings[ I ], Strings.Objects[ I ] );
      end;
    end;
  finally
    EndUpdate;
  end;
end;


function TRzCheckListStrings.AddObjectPacket( const S: string; AObject: TObject; APacket: TPacket ): Integer;
begin
  Result := AddObject( S, AObject );
  SetItemState( Result, APacket.State );
  SetItemEnabled( Result, APacket.Enabled );
  SetItemImageIndex( Result, APacket.ImageIndex );
  SetItemDisabledIndex( Result, APacket.DisabledIndex );
end;


function CreateNewPacket: TPacket;
begin
  Result := TPacket.Create;
  Result.State := cbUnchecked;
  Result.Enabled := True;
  Result.ImageIndex := -1;
  Result.DisabledIndex := -1;
end;

function TRzCheckListStrings.Add( const S: string ): Integer;
begin
  FCheckList.FChangingItems := True;
  try
    if FCheckList = nil then
    begin
      Result := lb_Err;
      Exit;
    end;
    Result := SendTextMessage( FCheckList.Handle, lb_AddString, 0, S );

    if Result < 0 then
      raise EOutOfResources.Create( SInsertLineError );

    FPackets.Insert( Result, CreateNewPacket );
  finally
    FCheckList.FChangingItems := False;
  end;
end;


procedure TRzCheckListStrings.Insert( Index: Integer; const S: string );
var
  LineNum: Integer;
begin
  if FCheckList = nil then
    Exit;

  FCheckList.FChangingItems := True;
  try
    LineNum := SendTextMessage( FCheckList.Handle, lb_InsertString, Index, S );

    if LineNum < 0 then
      raise EOutOfResources.Create( SInsertLineError );

    FPackets.Insert( Index, CreateNewPacket );
  finally
    FCheckList.FChangingItems := False;
  end;
end;


procedure TRzCheckListStrings.Delete( Index: Integer );
begin
  if FCheckList = nil then
    Exit;
  SendMessage( FCheckList.Handle, lb_DeleteString, Index, 0 );
  TPacket( FPackets[ Index ] ).Free;
  FPackets.Delete( Index );
end;


procedure TRzCheckListStrings.Clear;
var
  I: Integer;
begin
  if FCheckList = nil then
    Exit;
  SendMessage( FCheckList.Handle, lb_ResetContent, 0, 0 );
  for I := 0 to FPackets.Count - 1 do
    TPacket( FPackets[ I ] ).Free;
  FPackets.Clear;
end;


procedure TRzCheckListStrings.SetUpdateState(Updating: Boolean);
begin
  if FCheckList = nil then
    Exit;
  SendMessage( FCheckList.Handle, wm_SetRedraw, Ord(not Updating), 0 );
  if not Updating then
    FCheckList.Refresh;
end;


procedure TRzCheckListStrings.LoadFromStream( Stream: TStream );
var
  List: TStringList;
  I: Integer;
  S: string;
begin
  List := TStringList.Create;
  BeginUpdate;
  try
    Clear;
    List.LoadFromStream( Stream );

    for I := 0 to List.Count - 1 do
    begin
      S := List[ I ];
      if S[ 1 ] = '&' then
        ExtractPacketDetailsOrigFormat( I, S )
      else if S[ 1 ] = '@' then
        ExtractPacketDetails( I, S )
      else
        Add( S );
    end;
  finally
    EndUpdate;
    List.Free;
  end;
end;


{$IFDEF UNICODE}

procedure TRzCheckListStrings.LoadFromStream( Stream: TStream; Encoding: TEncoding );
var
  List: TStringList;
  I: Integer;
  S: string;
begin
  List := TStringList.Create;
  BeginUpdate;
  try
    Clear;
    List.LoadFromStream( Stream, Encoding );

    for I := 0 to List.Count - 1 do
    begin
      S := List[ I ];
      if S[ 1 ] = '&' then
        ExtractPacketDetailsOrigFormat( I, S )
      else if S[ 1 ] = '@' then
        ExtractPacketDetails( I, S )
      else
        Add( S );
    end;
  finally
    EndUpdate;
    List.Free;
  end;
end;

{$ENDIF}


procedure TRzCheckListStrings.SaveToStream( Stream: TStream );
var
  I: Integer;
  S: string;
begin
  for I := 0 to Count - 1 do
  begin
    S := EmbedPacketDetails( I );
    Stream.Write( Pointer( S )^, Length( S ) );
  end;
end;


{$IFDEF UNICODE}

procedure TRzCheckListStrings.SaveToStream( Stream: TStream; Encoding: TEncoding );
var
  I: Integer;
  S: string;
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := TEncoding.Default;
  Preamble := Encoding.GetPreamble;
  if Length( Preamble ) > 0 then
    Stream.WriteBuffer( Preamble[ 0 ], Length( Preamble ) );

  for I := 0 to Count - 1 do
  begin
    S := EmbedPacketDetails( I );
    Buffer := Encoding.GetBytes( S );
    Stream.WriteBuffer( Buffer[ 0 ], Length( Buffer ) );
  end;
end;

{$ENDIF}



{===============================}
{== TPacketStringList Methods ==}
{===============================}

constructor TPacketStringList.Create;
begin
  inherited;
  FPackets := TList.Create;
end;

destructor TPacketStringList.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPackets.Count - 1 do
    TPacket( FPackets[ I ] ).Free;
  FPackets.Free;
  inherited;
end;


function TPacketStringList.AddObject( const S: string; AObject: TObject ): Integer;
begin
  Result := inherited AddObject( S, AObject );
  FPackets.Add( CreateNewPacket );
end;


procedure TPacketStringList.Clear;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FPackets.Count - 1 do
    TPacket( FPackets[ I ] ).Free;
  FPackets.Clear;
end;

procedure TPacketStringList.AddStrings( Strings: TStrings );
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
    begin
      AddObjectPacket( Strings[ I ], Strings.Objects[ I ], TRzCheckListStrings( Strings ).FPackets[ I ] )
    end;
  finally
    EndUpdate;
  end;
end;


function TPacketStringList.AddObjectPacket( const S: string; AObject: TObject; APacket: TPacket ): Integer;
begin
  Result := AddObject( S, AObject );
  TPacket( FPackets[ Result ] ).State := APacket.State;
  TPacket( FPackets[ Result ] ).Enabled := APacket.Enabled;
  TPacket( FPackets[ Result ] ).ImageIndex := APacket.ImageIndex;
  TPacket( FPackets[ Result ] ).DisabledIndex := APacket.DisabledIndex;
end;


{&RUIF}
end.
