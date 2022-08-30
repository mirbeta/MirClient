{===============================================================================
  RzRadGrp Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzRadioGroup
    Mimics TRadioGroup but descends from TRzPanel and provides more display
    options.

  TRzCheckGroup
    Similar to TRzRadioGroup, but creates embedded check boxes instead of radio
    buttons.


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzRadioGroup and TRzCheckGroup to fully 
      support VCL Styles introduced in RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Modified the TRzRadioGroup and TRzCheckGroup so that if individual radio
      buttons or check boxes are hidden, the group takes this into account when
      arranging the radio buttons or check boxes.
    * Moved the TRzRadioGroup.ArrangeButtons method to the public section.
    * Moved the TRzCheckGroup.ArrangeChecks method to the public section.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * The TRzRadioGroup and TRzCheckGroup now utilize the updated AutoSize
      behavior in the TRzRadioButton and TRzCheckBox components. Specifically,
      the individual radio buttons and check boxes in TRzRadioGroup and
      TRzCheckGroup now have their AutoSize property set to True and their
      WordWrap property set to False.
    * Added new CaptionFont property to TRzRadioGroup and TRzCheckGroup.
    * The TRzRadioGroup and TRzCheckGroup now automatically adjust the
      ItemHeight property based on the ItemFont property. If the ItemHeight
      property is modified directly, then that value overrides the value
      associated with the height of the ItemFont. This change ensures that as
      the font size increases, the radio buttons and check boxes are completely
      visible in the group and do not overlap.
    * The AlignmentVertical properties of the embedded radio buttons and check
      boxes in the TRzRadioGroup and TRzCheckGroup are now set to avCenter.
      This produces more visually appealing results, especially when the font
      size used increases.
    * Added ReadOnly, ReadOnlyColor, and ReadOnlyColorOnFocus properties to the
      TRzRadioGroup.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzRadioGroup and TRzCheckGroup controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed issue in TRzRadioGroup and TRzCheckGroup where item captions that
      included double ampersands (&&) would be incorrectly displayed with
      line breaks.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new CustomGlyphImages property to TRzRadioGroup and TRzCheckGroup.
      This property is used to reference an ImageList that contains the glyphs
      to be used for the various states of the control. This new property should
      be used instead of the deprecated CustomGlyphs property, which is still
      available strictly for backward compatibility. By referencing an ImageList
      that holds the custom glyphs rather than an embedded bitmap, the actual
      glyph images are stored only once in the application instead of inside
      each instance of the control. When populating a TImageList for use with
      CustomGlyphImages, each index in the ImageList represents a different
      state.  The following tables describe the mapping:

      TRzRadioGroup CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Unchecked - Pressed
          3    Checked   - Pressed
          4    Unchecked - Disabled
          5    Checked   - Disabled
          6    Unchecked - Hot       (Optional)
          7    Checked   - Hot       (Optional)

      TRzCheckGroup CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Grayed
          3    Unchecked - Pressed
          4    Checked   - Pressed
          5    Grayed    - Pressed
          6    Unchecked - Disabled
          7    Checked   - Disabled
          8    Grayed    - Disabled
          9    Unchecked - Hot       (Optional)
          10   Checked   - Hot       (Optional)
          11   Grayed    - Hot       (Optional)

    * As noted in the above table, with the new CustomGlyphImages property, it
      is now possible to specify "hot" glyphs. That is, separate images to be
      displayed for a given state when the mouse is positioned over the control.
    * Added ReadOnly, ReadOnlyColor, and ReadOnlyColorOnFocus properties to the
      TRzCheckGroup.
    * Fixed issue where the widths of individual items (radio buttons in a
      TRzRadioGroup, or check boxes in a TRzCheckGroup) would not get calculated
      correctly if the captions contained tab characters.
    * Added new BannerHeight property to TRzRadioGroup, which is used to control
      the height of the banner area when the gsBanner GroupStyle is used. By
      default, this property is 0, which instructs the control to determine the
      height of the Banner based on the font size. If BannerHeight is set to a
      non-zero value, the height of the banner is sized accordingly.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where clearing the items in a TRzRadioGroup while the focus
      was still on one of the items resulted in an exception.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Fixed problem in TRzRadioGroup where preventing an item selection using
      the OnChanging event when there was no currently selected item would cause
      an Index Out Of Range exception.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where TRzRadioGroup and TRzCheckGroup would not pick up the
      correct settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where TRzCheckGroup would not remember check states of items
      set at design-time.
    * Fixed problem in TRzRadioGroup and TRzCheckGroup where radio buttons/check
      boxes would take up extra space if their captions contained accelerator
      characters (&).
    * Fixed positioning of radio buttons/check boxes when group caption is
      empty and GroupStyle is gsFlat.
    * The TRzRadioGroup and TRzCheckGroup inherit all of the new features and
      functionality added to the TRzCustomPanel and TRzGroupBox components
      including the new ViewStyle property, and the picking up of XP themes
      for Captions for all GroupStyle values.
    * Fixed alignment issue of embedded controls in TRzRadioGroup and
      TRzCheckGroup when running under RTL systems.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where OnClick and OnDblClick events were not getting fired
      by the TRzRadioGroup and TRzCheckGroup controls.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added MouseDown event dispatch method override to TRzGroupButton in order
      to trap mouse clicks if the control cannot be modified.  This is necessary
      when the radio group is a TRzDBRadioGroup and the corresponding dataset is
      not in edit mode (or cannot be placed into edit mode).
    * Added the FrameController property to both the TRzRadioGroup and the
      TRzCheckGroup. If the group has its GroupStyle property set to gsCustom,
      then the appearance of the group is controlled by the settings of the
      TRzFrameController. In addition, if the radio buttons/check boxes
      displayed within the group are set to HotTrack mode (i.e. flat), then the
      associated FrameController determines the coloring of the radio
      button/check box frames.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Surfaced the OnPaint event in TRzRadioGroup and TRzCheckGroup. This is
      useful when GroupStyle is set to gsCustom.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed display problem when under Right-To-Left locales.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomRadioGroup and TRzRadioGroup >>
    * ItemFont only streamed when changed from the default component font.
    * Added OnChanging event.  Developer can now prevent the user from changing
      the selected radio button.
    * Added a SpaceEvenly property. When set to True, the radio buttons
      contained in the group are spaced evenly across the width of the group
      box.  This behavior is similar to the way in which the standard
      TRadioGroup positions its buttons.
    * Other controls can now be dropped onto a TRzRadioGroup.
    * Added a FlatButtons property that controls the Flat properties of the
      embedded TRzRadioButtons.
    * Radio button alignment automatically switched when run under Right-To-Left
      locales.

    << TRzCustomCheckGroup and TRzCheckGroup >>
    * TRzCheckGroup component added.
===============================================================================}

{$I RzComps.inc}

unit RzRadGrp;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  RzPanel,
  Menus,
  ImgList,
  RzCommon,
  RzIntLst,
  ExtCtrls,
  RzRadChk;

type
  TRzIndexChangingEvent = procedure( Sender: TObject; NewIndex: Integer; var AllowChange: Boolean ) of object;

  TRzCustomRadioGroup = class( TRzCustomGroupBox )
  private
    FButtons: TList;
    FItemFont: TFont;
    FItemFontChanged: Boolean;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FSpaceEvenly: Boolean;
    FStartXPos: Integer;
    FStartYPos: Integer;
    FVerticalSpacing: Integer;
    FItemHeight: Integer;
    FItemHeightChanged: Boolean;

    FGlyphWidth: Integer;
    FGlyphHeight: Integer;
    FNumGlyphs: Integer;
    FCustomGlyphs: TBitmap;
    FCustomGlyphImages: TCustomImageList;
    FCustomGlyphImagesChangeLink: TChangeLink;
    FUseCustomGlyphs: Boolean;
    FTransparentColor: TColor;
    FWinMaskColor: TColor;
    FLightTextStyle: Boolean;
    FTextStyle: TTextStyle;
    FTextHighlightColor: TColor;
    FTextShadowColor: TColor;
    FTextShadowDepth: Integer;
    FTabOnEnter: Boolean;

    FItemFrameColor: TColor;
    FItemHotTrack: Boolean;
    FItemHotTrackColor: TColor;
    FItemHotTrackColorType: TRzHotTrackColorType;
    FItemHighlightColor: TColor;

    FReadOnly: Boolean;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;

    FOnChanging: TRzIndexChangingEvent;

    procedure ReadOldFlatProp( Reader: TReader );

    { Internal Event Handlers }
    procedure ButtonClick( Sender: TObject );
    procedure ItemsChange( Sender: TObject );
    procedure ItemFontChanged( Sender: TObject );
    procedure CustomGlyphsChanged( Sender: TObject );
    procedure CustomGlyphImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure ChangeScale( M, D: Integer ); override;
    procedure SetButtonCount( Value: Integer ); virtual;
    procedure UpdateButtons; virtual;

    procedure FocusNextButton;
    procedure FocusPreviousButton;

    procedure ReadState( Reader: TReader ); override;
    function CanModify: Boolean; virtual;

    procedure CustomFramingChanged; override;

    { Event Dispatch Methods }
    procedure KeyPress( var Key: Char ); override;
    function CanChange( NewIndex: Integer ): Boolean; dynamic;

    { Property Access Methods }
    function GetButtons( Index: Integer ): TRzRadioButton; virtual;
    function GetCaption: TCaption; virtual;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetColumns( Value: Integer ); virtual;
    procedure SetCustomGlyphs( Value: TBitmap ); virtual;
    procedure SetCustomGlyphImages( Value: TCustomImageList ); virtual;

    procedure SetItemFrameColor( Value: TColor ); virtual;
    procedure SetItemHotTrack( Value: Boolean ); virtual;
    procedure SetItemHotTrackColor( Value: TColor ); virtual;
    procedure SetItemHotTrackColorType( Value: TRzHotTrackColorType ); virtual;
    procedure SetItemHighlightColor( Value: TColor ); virtual;

    procedure SetBannerHeight( Value: Integer ); override;
    procedure SetGroupBoxStyle( Value: TRzGroupBoxStyle ); override;
    function GetItemEnabled( Index: Integer ): Boolean;
    procedure SetItemEnabled( Index: Integer; Value: Boolean );
    procedure SetItemFont( Value: TFont ); virtual;
    procedure SetItemHeight( Value: Integer ); virtual;
    procedure SetItemIndex( Value: Integer ); virtual;
    procedure SetItems( Value: TStrings ); virtual;
    procedure SetLightTextStyle( Value: Boolean ); virtual;

    procedure SetTextHighlightColor( Value: TColor ); virtual;
    procedure SetTextShadowColor( Value: TColor ); virtual;
    procedure SetTextShadowDepth( Value: Integer ); virtual;

    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetReadOnlyColor( Value: TColor ); virtual;
    procedure SetReadOnlyColorOnFocus( Value: Boolean ); virtual;

    procedure SetSpaceEvenly( Value: Boolean ); virtual;
    procedure SetStartPos( Index: Integer; Value: Integer ); virtual;
    procedure SetTextStyle( Value: TTextStyle ); virtual;
    procedure SetVerticalSpacing( Value: Integer ); virtual;
    procedure SetTransparent( Value: Boolean ); override;
    procedure SetTransparentColor( Value: TColor ); virtual;
    procedure SetUseCustomGlyphs( Value: Boolean ); virtual;
    procedure SetWinMaskColor( Value: TColor ); virtual;

    { Property Declarations }
    property Buttons[ Index: Integer ]: TRzRadioButton
      read GetButtons;

    property Caption: TCaption
      read GetCaption
      write SetCaption;

    property Columns: Integer
      read FColumns
      write SetColumns
      default 1;

    property CustomGlyphs: TBitmap
      read FCustomGlyphs
      write SetCustomGlyphs;

    property CustomGlyphImages: TCustomImageList
      read FCustomGlyphImages
      write SetCustomGlyphImages;

    property ItemFrameColor: TColor
      read FItemFrameColor
      write SetItemFrameColor
      default clBtnShadow;

    property ItemHotTrack: Boolean
      read FItemHotTrack
      write SetItemHotTrack
      default False;

    property ItemHighlightColor: TColor
      read FItemHighlightColor
      write SetItemHighlightColor
      default clHighlight;

    property ItemHotTrackColor: TColor
      read FItemHotTrackColor
      write SetItemHotTrackColor
      default xpHotTrackColor;

    property ItemHotTrackColorType: TRzHotTrackColorType
      read FItemHotTrackColorType
      write SetItemHotTrackColorType
      default htctActual;

    property ItemFont: TFont
      read FItemFont
      write SetItemFont
      stored FItemFontChanged;

    property ItemHeight: Integer
      read FItemHeight
      write SetItemHeight
      stored FItemHeightChanged;

    property ItemIndex: Integer
      read FItemIndex
      write SetItemIndex
      default -1;

    property ItemEnabled[ Index: Integer ]: Boolean
      read GetItemEnabled
      write SetItemEnabled;

    property Items: TStrings
      read FItems
      write SetItems;

    property LightTextStyle: Boolean
      read FLightTextStyle
      write SetLightTextStyle
      default False;

    property TextHighlightColor: TColor
      read FTextHighlightColor
      write SetTextHighlightColor
      default clBtnHighlight;

    property TextShadowColor: TColor
      read FTextShadowColor
      write SetTextShadowColor
      default clBtnShadow;

    property TextShadowDepth: Integer
      read FTextShadowDepth
      write SetTextShadowDepth
      default 2;

    property ReadOnly: Boolean
      read FReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write SetReadOnlyColorOnFocus
      default False;

    property SpaceEvenly: Boolean
      read FSpaceEvenly
      write SetSpaceEvenly
      default False;

    property StartXPos: Integer
      index 1
      read FStartXPos
      write SetStartPos
      default 8;

    property StartYPos: Integer
      index 2
      read FStartYPos
      write SetStartPos
      default 2;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property TextStyle: TTextStyle
      read FTextStyle
      write SetTextStyle
      default tsNormal;

    property TransparentColor: TColor
      read FTransparentColor
      write SetTransparentColor
      default clOlive;

    property UseCustomGlyphs: Boolean
      read FUseCustomGlyphs
      write SetUseCustomGlyphs
      default False;

    property VerticalSpacing: Integer
      read FVerticalSpacing
      write SetVerticalSpacing
      default 3;

    property WinMaskColor: TColor
      read FWinMaskColor
      write SetWinMaskColor
      default clLime;

    property OnChanging: TRzIndexChangingEvent
      read FOnChanging
      write FOnChanging;

    { Inherited Properties & Events }
    property Alignment default taLeftJustify;
    property AlignmentVertical default avTop;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ArrangeButtons; virtual;
    procedure FlipChildren( AllLevels: Boolean ); override;
  end;


  TRzRadioGroup = class( TRzCustomRadioGroup )
  private
    FAboutInfo: TRzAboutInfo;
  public
    property Buttons;
    property ItemEnabled;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property BannerHeight;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameController;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GroupStyle;
    property Height;
    property Hint;
    property ItemFrameColor;
    property ItemHotTrack;
    property ItemHighlightColor;
    property ItemHotTrackColor;
    property ItemHotTrackColorType;
    property ItemFont;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property SpaceEvenly;
    property StartXPos;
    property StartYPos;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextStyle;
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property VerticalSpacing;
    property Visible;
    property VisualStyle;
    property WinMaskColor;

    property OnChanging;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;


  TRzCustomCheckGroup = class( TRzCustomGroupBox )
  private
    FChecks: TList;
    FItemFont: TFont;
    FItemFontChanged: Boolean;
    FItems: TStrings;
    FColumns: Integer;
    FReading: Boolean;
    FSpaceEvenly: Boolean;
    FStartXPos: Integer;
    FStartYPos: Integer;
    FVerticalSpacing: Integer;
    FItemHeight: Integer;
    FItemHeightChanged: Boolean;

    FGlyphWidth: Integer;
    FGlyphHeight: Integer;
    FNumGlyphs: Integer;
    FCustomGlyphs: TBitmap;
    FCustomGlyphImages: TCustomImageList;
    FCustomGlyphImagesChangeLink: TChangeLink;
    FUseCustomGlyphs: Boolean;
    FTransparentColor: TColor;
    FWinMaskColor: TColor;
    FLightTextStyle: Boolean;
    FTextStyle: TTextStyle;
    FTextHighlightColor: TColor;
    FTextShadowColor: TColor;
    FTextShadowDepth: Integer;
    FTabOnEnter: Boolean;
    FAllowGrayed: Boolean;

    FItemFrameColor: TColor;
    FItemHotTrack: Boolean;
    FItemHotTrackColor: TColor;
    FItemHotTrackColorType: TRzHotTrackColorType;
    FItemHighlightColor: TColor;

    FReadOnly: Boolean;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;

    FItemStates: TRzIntegerList;

    FOnChange: TStateChangeEvent;

    procedure ReadCheckStates( Reader: TReader );
    procedure WriteCheckStates( Writer: TWriter );

    { Internal Event Handlers }
    procedure CheckClick( Sender: TObject );
    procedure ItemsChange( Sender: TObject );
    procedure ItemFontChanged( Sender: TObject );
    procedure CustomGlyphsChanged( Sender: TObject );
    procedure CustomGlyphImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    procedure ChangeScale( M, D: Integer ); override;
    procedure SetCheckCount( Value: Integer ); virtual;
    procedure UpdateChecks; virtual;
    function GetIndex( CheckBox: TRzCheckBox ): Integer;

    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure ReadState( Reader: TReader ); override;
    function CanModify: Boolean; virtual;

    procedure CustomFramingChanged; override;

    { Event Dispatch Methods }
    procedure KeyPress( var Key: Char ); override;
    procedure Change( Index: Integer; NewState: TCheckBoxState ); dynamic;

    { Property Access Methods }
    function GetChecks( Index: Integer ): TRzCheckBox; virtual;

    procedure SetAllowGrayed( Value: Boolean ); virtual;
    function GetCaption: TCaption; virtual;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetColumns( Value: Integer ); virtual;
    procedure SetCustomGlyphs( Value: TBitmap ); virtual;
    procedure SetCustomGlyphImages( Value: TCustomImageList ); virtual;
    procedure SetGroupBoxStyle( Value: TRzGroupBoxStyle ); override;
    procedure SetItemFrameColor( Value: TColor ); virtual;
    procedure SetItemHotTrack( Value: Boolean ); virtual;
    procedure SetItemHighlightColor( Value: TColor ); virtual;
    procedure SetItemHotTrackColor( Value: TColor ); virtual;
    procedure SetItemHotTrackColorType( Value: TRzHotTrackColorType ); virtual;
    function GetItemChecked( Index: Integer ): Boolean; virtual;
    procedure SetItemChecked( Index: Integer; Value: Boolean ); virtual;
    function GetItemEnabled( Index: Integer ): Boolean; virtual;
    procedure SetItemEnabled( Index: Integer; Value: Boolean ); virtual;
    procedure SetItemFont( Value: TFont ); virtual;
    procedure SetItemHeight( Value: Integer ); virtual;
    procedure SetItems( Value: TStrings ); virtual;
    function GetItemState( Index: Integer ): TCheckBoxState; virtual;
    procedure SetItemState( Index: Integer; Value: TCheckBoxState ); virtual;

    procedure SetLightTextStyle( Value: Boolean ); virtual;
    procedure SetTextHighlightColor( Value: TColor ); virtual;
    procedure SetTextShadowColor( Value: TColor ); virtual;
    procedure SetTextShadowDepth( Value: Integer ); virtual;

    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetReadOnlyColor( Value: TColor ); virtual;
    procedure SetReadOnlyColorOnFocus( Value: Boolean ); virtual;

    procedure SetSpaceEvenly( Value: Boolean ); virtual;
    procedure SetStartPos( Index: Integer; Value: Integer ); virtual;
    procedure SetTextStyle( Value: TTextStyle ); virtual;
    procedure SetVerticalSpacing( Value: Integer ); virtual;
    procedure SetTransparent( Value: Boolean ); override;
    procedure SetTransparentColor( Value: TColor ); virtual;
    procedure SetUseCustomGlyphs( Value: Boolean ); virtual;
    procedure SetWinMaskColor( Value: TColor ); virtual;

    { Property Declarations }
    property AllowGrayed: Boolean
      read FAllowGrayed
      write SetAllowGrayed
      default False;

    property Checks[ Index: Integer ]: TRzCheckBox
      read GetChecks;

    property Caption: TCaption
      read GetCaption
      write SetCaption;

    property Columns: Integer
      read FColumns
      write SetColumns
      default 1;

    property CustomGlyphs: TBitmap
      read FCustomGlyphs
      write SetCustomGlyphs;

    property CustomGlyphImages: TCustomImageList
      read FCustomGlyphImages
      write SetCustomGlyphImages;

    property ItemFrameColor: TColor
      read FItemFrameColor
      write SetItemFrameColor
      default clBtnShadow;

    property ItemHotTrack: Boolean
      read FItemHotTrack
      write SetItemHotTrack
      default False;

    property ItemHighlightColor: TColor
      read FItemHighlightColor
      write SetItemHighlightColor
      default clHighlight;

    property ItemHotTrackColor: TColor
      read FItemHotTrackColor
      write SetItemHotTrackColor
      default xpHotTrackColor;

    property ItemHotTrackColorType: TRzHotTrackColorType
      read FItemHotTrackColorType
      write SetItemHotTrackColorType
      default htctActual;

    property ItemFont: TFont
      read FItemFont
      write SetItemFont
      stored FItemFontChanged;

    property ItemHeight: Integer
      read FItemHeight
      write SetItemHeight
      stored FItemHeightChanged;

    property ItemChecked[ Index: Integer ]: Boolean
      read GetItemChecked
      write SetItemChecked;

    property ItemEnabled[ Index: Integer ]: Boolean
      read GetItemEnabled
      write SetItemEnabled;

    property ItemState[ Index: Integer ]: TCheckBoxState
      read GetItemState
      write SetItemState;

    property Items: TStrings
      read FItems
      write SetItems;

    property LightTextStyle: Boolean
      read FLightTextStyle
      write SetLightTextStyle
      default False;

    property TextHighlightColor: TColor
      read FTextHighlightColor
      write SetTextHighlightColor
      default clBtnHighlight;

    property TextShadowColor: TColor
      read FTextShadowColor
      write SetTextShadowColor
      default clBtnShadow;

    property TextShadowDepth: Integer
      read FTextShadowDepth
      write SetTextShadowDepth
      default 2;

    property ReadOnly: Boolean
      read FReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write SetReadOnlyColorOnFocus
      default False;

    property SpaceEvenly: Boolean
      read FSpaceEvenly
      write SetSpaceEvenly
      default False;

    property StartXPos: Integer
      index 1
      read FStartXPos
      write SetStartPos
      default 8;

    property StartYPos: Integer
      index 2
      read FStartYPos
      write SetStartPos
      default 2;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property TextStyle: TTextStyle
      read FTextStyle
      write SetTextStyle
      default tsNormal;

    property TransparentColor: TColor
      read FTransparentColor
      write SetTransparentColor
      default clOlive;

    property UseCustomGlyphs: Boolean
      read FUseCustomGlyphs
      write SetUseCustomGlyphs
      default False;

    property VerticalSpacing: Integer
      read FVerticalSpacing
      write SetVerticalSpacing
      default 3;

    property WinMaskColor: TColor
      read FWinMaskColor
      write SetWinMaskColor
      default clLime;

    property OnChange: TStateChangeEvent
      read FOnChange
      write FOnChange;


    { Inherited Properties & Events }
    property Alignment default taLeftJustify;
    property AlignmentVertical default avTop;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ArrangeChecks; virtual;
    procedure FlipChildren(AllLevels: Boolean); override;
  end;


  TRzCheckGroup = class( TRzCustomCheckGroup )
  private
    FAboutInfo: TRzAboutInfo;
  public
    property Checks;
    property ItemChecked;
    property ItemEnabled;
    property ItemState;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameController;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GroupStyle;
    property Height;
    property ItemFrameColor;
    property ItemHighlightColor;
    property ItemHotTrack;
    property ItemHotTrackColor;
    property ItemHotTrackColorType;
    property ItemFont;
    property ItemHeight;
    property Items;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property SpaceEvenly;
    property StartXPos;
    property StartYPos;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextStyle;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property VerticalSpacing;
    property Visible;
    property VisualStyle;
    property WinMaskColor;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
  {&RAS}
  RzButton;


{===================================}
{== TRzGroupButton Internal Class ==}
{===================================}

type
  TRzGroupButton = class( TRzRadioButton )
  private
    FInClick: Boolean;
    procedure WMGetDlgCode( var Msg: TMessage ); message wm_GetDlgCode;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure WMKeyDown(var Msg: TWMKeyDown); message wm_KeyDown;
  protected
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
  public
    constructor CreateInGroup( RadioGroup: TRzCustomRadioGroup );
    destructor Destroy; override;
  end;


constructor TRzGroupButton.CreateInGroup( RadioGroup: TRzCustomRadioGroup );
begin
  inherited Create( RadioGroup );

  RadioGroup.FButtons.Add( Self );
  AlignmentVertical := avCenter;
  AutoSize := True;
  WordWrap := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
  ParentFont := False;
end;


destructor TRzGroupButton.Destroy;
begin
  TRzCustomRadioGroup( Owner ).FButtons.Remove( Self );
  inherited;
end;


procedure TRzGroupButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if TRzCustomRadioGroup( Parent ).CanModify then
        inherited;
    except
      Application.HandleException( Self );
    end;
    FInClick := False;
  end;
end;


procedure TRzGroupButton.WMGetDlgCode( var Msg: TMessage );
begin
  inherited;
  Msg.Result := dlgc_WantArrows;
end;


procedure TRzGroupButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and CanFocus then
  begin
    if TRzCustomRadioGroup( Parent ).CanModify then
      inherited;
  end
  else
    inherited;
end;


procedure TRzGroupButton.WMKeyDown( var Msg: TWMKeyDown );
begin
  case Msg.CharCode of
    vk_Down,
    vk_Right:
    begin
      if TRzCustomRadioGroup( Parent ).CanModify then
      begin
        TRzCustomRadioGroup( Parent ).FocusNextButton;
      end;
    end;

    vk_Up,
    vk_Left:
    begin
      if TRzCustomRadioGroup( Parent ).CanModify then
        TRzCustomRadioGroup( Parent ).FocusPreviousButton;
    end;

    else
      inherited;
  end;
end;




procedure TRzGroupButton.KeyPress( var Key: Char );
begin
  inherited;
  TRzCustomRadioGroup( Parent ).KeyPress( Key );
  if ( Key = #8 ) or ( Key = ' ' ) then
  begin
    if not TRzCustomRadioGroup( Parent ).CanModify then
      Key := #0;
  end;
end;


procedure TRzGroupButton.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  TRzCustomRadioGroup( Parent ).KeyDown( Key, Shift );
end;

{&RT}
{=================================}
{== TRzCustomRadioGroup Methods ==}
{=================================}

constructor TRzCustomRadioGroup.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csReplicatable ];
  FButtons := TList.Create;
  FItemFont := TFont.Create;
  FItemFont.Name := 'Tahoma';
  FItemFont.Size := 8;
  FItemFont.OnChange := ItemFontChanged;
  FItems := TStringList.Create;
  TStringList( FItems ).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
  FSpaceEvenly := False;
  FStartXPos := 8;
  FStartYPos := 2;
  FVerticalSpacing := 3;
  FItemHeight := 17;
  {&RCI}

  FGlyphWidth := DefaultGlyphWidth;
  FGlyphHeight := DefaultGlyphHeight;
  FItemFrameColor := clBtnShadow;
  FItemHotTrack := False;
  FItemHighlightColor := clHighlight;
  FItemHotTrackColor := xpHotTrackColor;
  FItemHotTrackColorType := htctActual;

  FNumGlyphs := DefaultNumGlyphs_RadioButton;
  FCustomGlyphs := TBitmap.Create;
  FCustomGlyphs.OnChange := CustomGlyphsChanged;
  FCustomGlyphImagesChangeLink := TChangeLink.Create;
  FCustomGlyphImagesChangeLink.OnChange := CustomGlyphImagesChange;
  FUseCustomGlyphs := False;

  FLightTextStyle := False;
  FTextStyle := tsNormal;
  FTextShadowDepth := 2;
  FTextShadowColor := clBtnShadow;
  FTextHighlightColor := clBtnHighlight;
  FReadOnlyColor := clInfoBk;

  FTransparentColor := clOlive;
  FWinMaskColor := clLime;
  FTabOnEnter := False;
end;


destructor TRzCustomRadioGroup.Destroy;
begin
  FItemFont.Free;
  SetButtonCount( 0 );
  TStringList( FItems ).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  FCustomGlyphs.Free;
  FCustomGlyphImagesChangeLink.Free;
  inherited;
end;


procedure TRzCustomRadioGroup.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FCustomGlyphImages ) then
    SetCustomGlyphImages( nil );  // Call access method so connections to link object can be cleared
end;


procedure TRzCustomRadioGroup.FlipChildren( AllLevels: Boolean );
begin
  // The radio buttons are flipped using BiDiMode
end;


procedure TRzCustomRadioGroup.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the Flat property was renamed to ItemHotTrack
  Filer.DefineProperty( 'Flat', ReadOldFlatProp, nil, False );
end;


procedure TRzCustomRadioGroup.ReadOldFlatProp( Reader: TReader );
begin
  ItemHotTrack := Reader.ReadBoolean;
end;


procedure TRzCustomRadioGroup.ButtonClick( Sender: TObject );
begin
  if not FUpdating then
  begin
    if CanChange( FButtons.IndexOf( Sender ) ) then
    begin
      FItemIndex := FButtons.IndexOf( Sender );
      Changed;
      Click;
    end
    else
    begin
      // Restore the previous ItemIndex selection
      FUpdating := True;
      try
        if FItemIndex <> -1 then
          Buttons[ FItemIndex ].SetFocus
        else
          TRzGroupButton( Sender ).Checked := False;
      finally
        FUpdating := False;
      end;
    end;
  end;
end;


procedure TRzCustomRadioGroup.ItemsChange( Sender: TObject );
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;


procedure TRzCustomRadioGroup.ItemFontChanged( Sender: TObject );
begin
  FItemHeight := GetMinFontHeight( FItemFont );
  FItemFontChanged := True;
  ArrangeButtons;
  Invalidate;
end;


procedure TRzCustomRadioGroup.ReadState( Reader: TReader );
begin
  FReading := True;
  inherited;
  FReading := False;
  UpdateButtons;
end;


procedure TRzCustomRadioGroup.SetButtonCount( Value: Integer );
begin
  while FButtons.Count < Value do
    TRzGroupButton.CreateInGroup( Self );
  while FButtons.Count > Value do
    TRzGroupButton( FButtons.Last ).Free;
end;


function TRzCustomRadioGroup.GetButtons( Index: Integer ): TRzRadioButton;
begin
  Result := TRzRadioButton( FButtons[ Index ] );
end;

function TRzCustomRadioGroup.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;


procedure TRzCustomRadioGroup.SetCaption( const Value: TCaption );
begin
  inherited Caption := Value;
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.SetColumns( Value: Integer );
begin
  if Value < 1 then
    Value := 1;
  if Value > 16 then
    Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;


procedure TRzCustomRadioGroup.SetItemIndex( Value: Integer );
begin
  if FReading then
    FItemIndex := Value
  else
  begin
    if Value < -1 then
      Value := -1;
    if Value >= FButtons.Count then
      Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TRzGroupButton( FButtons[ FItemIndex ] ).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TRzGroupButton( FButtons[ FItemIndex ] ).Checked := True;
    end;
  end;
end;


procedure TRzCustomRadioGroup.SetItems( Value: TStrings );
begin
  {&RV}
  FItems.Assign( Value );
end;


function TRzCustomRadioGroup.CanModify: Boolean;
begin
  Result := not FReadOnly;
end;


procedure TRzCustomRadioGroup.CustomFramingChanged;
var
  I: Integer;
begin
  if FFrameController.FrameVisible then
  begin
    if ( GroupStyle = gsCustom ) then
      inherited;
    ItemFrameColor := FFrameController.FrameColor;
    for I := 0 to FButtons.Count - 1 do
      Buttons[ I ].DisabledColor := FFrameController.DisabledColor;
  end;
end;


procedure TRzCustomRadioGroup.ArrangeButtons;
var
  VisibleButtons, BtnIdx, ButtonsPerCol, ButtonWidth, TopMargin, I, K, X, W, L: Integer;
  R, CalcRect: TRect;
  ColWidths: array[ 0..15 ] of Integer;
  Btn: TRzGroupButton;

  function AdjustedIndex( Index: Integer ): Integer;
  var
    I: Integer;
    Btn: TRzGroupButton;
  begin
    Result := Index;
    for I := Index - 1 downto 0 do
    begin
      Btn := TRzGroupButton( FButtons[ I ] );
      if not Btn.Visible then
        Dec( Result );
    end;
  end;

begin
  if ( FButtons.Count <> 0 ) and not FReading then
  begin
    VisibleButtons := 0;
    for I := 0 to FButtons.Count - 1 do
    begin
      Btn := TRzGroupButton( FButtons[ I ] );
      if Btn.Visible then
        Inc( VisibleButtons );
    end;

    ButtonsPerCol := ( VisibleButtons + FColumns - 1 ) div FColumns;

    if FSpaceEvenly then
      ButtonWidth := ( ( Width - FStartXPos ) - ( 2 * BorderWidth ) - 10 ) div FColumns
    else
      ButtonWidth := 0;

    R := ClientRect;
    AdjustClientRect( R );
    TopMargin := R.Top + FStartYPos;

    if not FSpaceEvenly then
    begin
      for I := 0 to FColumns - 1 do
        ColWidths[ I ] := 0;

      Canvas.Font := FItemFont;
      for I := 0 to FButtons.Count - 1 do
      begin
        Btn := TRzGroupButton( FButtons[ I ] );
        BtnIdx := AdjustedIndex( I );
        if Btn.Visible then
        begin
          CalcRect := R;
          DrawString( Self.Canvas, RemoveAccelerators( Trim( Btn.Caption ) ), CalcRect,
                      dt_CalcRect or dt_ExpandTabs or dt_NoPrefix );

          W := CalcRect.Right - CalcRect.Left + FGlyphWidth + 4 + 8;

          if W > ColWidths[ BtnIdx div ButtonsPerCol ] then
            ColWidths[ BtnIdx div ButtonsPerCol ] := W;
        end;
      end;
    end;

    for I := 0 to FButtons.Count - 1 do
    begin
      Btn := TRzGroupButton( FButtons[ I ] );
      BtnIdx := AdjustedIndex( I );
      if Btn.Visible then
      begin
        if FSpaceEvenly then
        begin
          L := ( BtnIdx div ButtonsPerCol ) * ButtonWidth + BorderWidth;
          if not UseRightToLeftAlignment then
            L := L + FStartXPos
          else
            L := Self.ClientWidth - L - ButtonWidth - FStartXPos;

          Btn.SetBounds( L, ( BtnIdx mod ButtonsPerCol ) * ( FItemHeight + FVerticalSpacing ) + TopMargin,
                         ButtonWidth, FItemHeight );
        end
        else
        begin
          X := 0;
          for K := ( BtnIdx div ButtonsPerCol ) - 1 downto 0 do
            X := X + ColWidths[ K ];

          L := X + BorderWidth;
          if not UseRightToLeftAlignment then
            L := L + FStartXPos
          else
            L := Self.ClientWidth - L - ColWidths[ BtnIdx div ButtonsPerCol ] - 4 + 8 - FStartXPos;

          Btn.SetBounds( L, ( BtnIdx mod ButtonsPerCol ) * ( FItemHeight + FVerticalSpacing ) + TopMargin,
                         ColWidths[ BtnIdx div ButtonsPerCol ] - 4, FItemHeight );
        end;

        Btn.Font.Assign( FItemFont );
        Btn.BiDiMode := Self.BiDiMode;
        Btn.TextHighlightColor := FTextHighlightColor;
        Btn.TextShadowColor := FTextShadowColor;
        Btn.TextShadowDepth := FTextShadowDepth;
        Btn.LightTextStyle := FLightTextStyle;
        Btn.TextStyle := FTextStyle;
        Btn.Transparent := Self.Transparent;
        Btn.WinMaskColor := FWinMaskColor;
        Btn.TransparentColor := FTransparentColor;
        if not FCustomGlyphs.Empty then
          Btn.CustomGlyphs.Assign( FCustomGlyphs );
        Btn.CustomGlyphImages := FCustomGlyphImages;
        Btn.UseCustomGlyphs := FUseCustomGlyphs;
        Btn.FrameColor := FItemFrameColor;
        Btn.HotTrack := FItemHotTrack;
        Btn.HighlightColor := FItemHighlightColor;
        Btn.HotTrackColor := FItemHotTrackColor;
        Btn.HotTrackColorType := FItemHotTrackColorType;
        Btn.ReadOnly := FReadOnly;
        Btn.ReadOnlyColor := FReadOnlyColor;
        Btn.ReadOnlyColorOnFocus := FReadOnlyColorOnFocus;
      end;
    end;
  end;
end; {= TRzCustomRadioGroup.ArrangeButtons =}



procedure TRzCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount( FItems.Count );
  for I := 0 to FButtons.Count - 1 do
    TRzGroupButton( FButtons[ I ] ).Caption := FItems[ I ];
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TRzGroupButton( FButtons[ FItemIndex ] ).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;


procedure TRzCustomRadioGroup.SetCustomGlyphs( Value: TBitmap );
begin
  FCustomGlyphs.Assign( Value );
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.CustomGlyphsChanged(Sender: TObject);
begin
  UseCustomGlyphs := not FCustomGlyphs.Empty;
  Invalidate;
end;


procedure TRzCustomRadioGroup.SetCustomGlyphImages( Value: TCustomImageList );
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
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.CustomGlyphImagesChange( Sender: TObject );
begin
  if Sender = CustomGlyphImages then
  begin
    UseCustomGlyphs := FCustomGlyphImages.Count > 0;
    ArrangeButtons;
  end;
end;



function TRzCustomRadioGroup.GetItemEnabled( Index: Integer ): Boolean;
begin
  Result := TRzRadioButton( FButtons[ Index ] ).Enabled;
end;

procedure TRzCustomRadioGroup.SetItemEnabled( Index: Integer; Value: Boolean );
begin
  TRzRadioButton( FButtons[ Index ] ).Enabled := Value;
end;


procedure TRzCustomRadioGroup.ChangeScale( M, D: Integer );
begin
  inherited;
  if FItemFontChanged then
    FItemFont.Height := MulDiv( FItemFont.Height, M, D );
  FVerticalSpacing := MulDiv( FVerticalSpacing, M, D );
  FItemHeight := MulDiv( FItemHeight, M, D );
  FStartYPos := MulDiv( FStartYPos, M, D );
  FStartXPos := MulDiv( FStartXPos, M, D );
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.SetItemFont( Value: TFont );
begin
  if FItemFont <> Value then
  begin
    FItemFont.Assign( Value );
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetItemHeight( Value: Integer );
begin
  if FItemHeight <> Value then
  begin
    FItemHeightChanged := True;
    FItemHeight := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetBannerHeight( Value: Integer );
begin
  inherited;
  ArrangeButtons;
  Invalidate;
end;

procedure TRzCustomRadioGroup.SetGroupBoxStyle( Value: TRzGroupBoxStyle );
begin
  inherited;
  ArrangeButtons;
  Invalidate;
end;


procedure TRzCustomRadioGroup.SetItemFrameColor( Value: TColor );
begin
  if FItemFrameColor <> Value then
  begin
    FItemFrameColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetItemHotTrack( Value: Boolean );
begin
  if FItemHotTrack <> Value then
  begin
    FItemHotTrack := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetItemHighlightColor( Value: TColor );
begin
  if FItemHighlightColor <> Value then
  begin
    FItemHighlightColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetItemHotTrackColor( Value: TColor );
begin
  if FItemHotTrackColor <> Value then
  begin
    FItemHotTrackColor := Value;
    ArrangeButtons;
  end;
end;

procedure TRzCustomRadioGroup.SetItemHotTrackColorType( Value: TRzHotTrackColorType );
begin
  if FItemHotTrackColorType <> Value then
  begin
    FItemHotTrackColorType := Value;
    ArrangeButtons;
  end;
end;



procedure TRzCustomRadioGroup.SetLightTextStyle( Value: Boolean );
begin
  if FLightTextStyle <> Value then
  begin
    FLightTextStyle := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetTextHighlightColor( Value: TColor );
begin
  if FTextHighlightColor <> Value then
  begin
    FTextHighlightColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetTextShadowColor( Value: TColor );
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetTextShadowDepth( Value: Integer );
begin
  if FTextShadowDepth <> Value then
  begin
    FTextShadowDepth := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetReadOnly( Value: Boolean );
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetReadOnlyColor( Value: TColor );
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetReadOnlyColorOnFocus( Value: Boolean );
begin
  if FReadOnlyColorOnFocus <> Value then
  begin
    FReadOnlyColorOnFocus := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetSpaceEvenly( Value: Boolean );
begin
  if FSpaceEvenly <> Value then
  begin
    FSpaceEvenly := Value;
    ArrangeButtons;
  end;
end;

procedure TRzCustomRadioGroup.SetStartPos( Index: Integer; Value: Integer );
begin
  if Index = 1 then
  begin
    if FStartXPos <> Value then
    begin
      FStartXPos := Value;
      ArrangeButtons;
    end;
  end
  else
  begin
    if FStartYPos <> Value then
    begin
      FStartYPos := Value;
      ArrangeButtons;
    end;
  end;
end;


procedure TRzCustomRadioGroup.SetTextStyle( Value: TTextStyle );
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetTransparent( Value: Boolean );
begin
  inherited;
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.SetTransparentColor( Value: TColor );
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetUseCustomGlyphs( Value: Boolean );
begin
  // Do not check against previous value b/c assigning an image list will
  // force UseCustomGlyphs to be true and we need the FGlyphWidth and
  // FGlyphHeight values to be updated appropriately.

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
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.SetVerticalSpacing( Value: Integer );
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.SetWinMaskColor( Value: TColor );
begin
  if FWinMaskColor <> Value then
  begin
    FWinMaskColor := Value;
    ArrangeButtons;
  end;
end;


procedure TRzCustomRadioGroup.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and CanFocus then
  begin
    SelectFirst;
    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzCustomRadioGroup.CMEnabledChanged( var Msg: TMessage );
var
  I: Integer;
begin
  inherited;
  Repaint;
  for I := 0 to FButtons.Count - 1 do
    TRzGroupButton( FButtons[ I ] ).Enabled := Enabled;
end;


procedure TRzCustomRadioGroup.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if not FItemFontChanged then
  begin
    FItemFont.Assign( Self.Font );
    // Reset FItemFontChanged b/c internal handler has gotten called from above statements
    FItemFontChanged := False;
  end;
  Invalidate;
end;


procedure TRzCustomRadioGroup.WMSize( var Msg: TWMSize );
begin
  inherited;
  ArrangeButtons;
end;


procedure TRzCustomRadioGroup.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


function TRzCustomRadioGroup.CanChange( NewIndex: Integer ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewIndex, Result );
end;


procedure TRzCustomRadioGroup.FocusNextButton;
var
  Idx, StartIdx: Integer;
begin
  if FButtons.Count <= 1 then
    Exit;

  Idx := FItemIndex;
  StartIdx := Idx;
  repeat
    Inc( Idx );
    if Idx >= FButtons.Count then
      Idx := 0;
  until TRzGroupButton( FButtons[ Idx ] ).Visible or ( Idx = StartIdx );
  TRzGroupButton( FButtons[ Idx ] ).SetFocus;
end;


procedure TRzCustomRadioGroup.FocusPreviousButton;
var
  Idx, StartIdx: Integer;
begin
  if FButtons.Count <= 1 then
    Exit;

  Idx := FItemIndex;
  StartIdx := Idx;
  repeat
    Dec( Idx );
    if Idx < 0 then
      Idx := FButtons.Count - 1;
  until TRzGroupButton( FButtons[ Idx ] ).Visible or ( Idx = StartIdx );
  TRzGroupButton( FButtons[ Idx ] ).SetFocus;
end;


{==================================}
{== TRzGroupCheck Internal Class ==}
{==================================}

type
  TRzGroupCheck = class( TRzCheckBox )
  private
    FInClick: Boolean;
  protected
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
  public
    constructor CreateInGroup( CheckGroup: TRzCustomCheckGroup );
    destructor Destroy; override;
  end;


constructor TRzGroupCheck.CreateInGroup( CheckGroup: TRzCustomCheckGroup );
begin
  inherited Create( CheckGroup );

  CheckGroup.FChecks.Add( Self );
  AlignmentVertical := avCenter;
  AutoSize := True;
  WordWrap := False;
  Enabled := CheckGroup.Enabled;
  ParentShowHint := False;
  OnClick := CheckGroup.CheckClick;
  Parent := CheckGroup;
  ParentFont := False;
end;


destructor TRzGroupCheck.Destroy;
begin
  TRzCustomCheckGroup( Owner ).FChecks.Remove( Self );
  inherited;
end;


procedure TRzGroupCheck.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if TRzCustomCheckGroup( Parent ).CanModify then
        inherited;
    except
      Application.HandleException( Self );
    end;
    FInClick := False;
  end;
end;


procedure TRzGroupCheck.KeyPress( var Key: Char );
begin
  inherited;
  TRzCustomCheckGroup( Parent ).KeyPress( Key );
  if ( Key = #8 ) or ( Key = ' ' ) then
  begin
    if not TRzCustomCheckGroup( Parent ).CanModify then
      Key := #0;
  end;
end;


procedure TRzGroupCheck.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  TRzCustomCheckGroup( Parent ).KeyDown( Key, Shift );
end;


{=================================}
{== TRzCustomCheckGroup Methods ==}
{=================================}

constructor TRzCustomCheckGroup.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csReplicatable ];
  FChecks := TList.Create;
  FItemFont := TFont.Create;
  FItemFont.Name := 'Tahoma';
  FItemFont.Size := 8;
  FItemFont.OnChange := ItemFontChanged;
  FItems := TStringList.Create;
  TStringList( FItems ).OnChange := ItemsChange;
  FColumns := 1;
  FSpaceEvenly := False;
  FStartXPos := 8;
  FStartYPos := 2;
  FVerticalSpacing := 3;
  FItemHeight := 17;
  {&RCI}

  FItemStates := TRzIntegerList.Create;
  FItemStates.Duplicates := dupAccept;

  FGlyphWidth := DefaultGlyphWidth;
  FGlyphHeight := DefaultGlyphHeight;
  FItemFrameColor := clBtnShadow;
  FItemHotTrack := False;
  FItemHighlightColor := clHighlight;
  FItemHotTrackColor := xpHotTrackColor;
  FItemHotTrackColorType := htctActual;

  FAllowGrayed := False;

  FNumGlyphs := DefaultNumGlyphs_CheckBox;
  FCustomGlyphs := TBitmap.Create;
  FCustomGlyphs.OnChange := CustomGlyphsChanged;
  FCustomGlyphImagesChangeLink := TChangeLink.Create;
  FCustomGlyphImagesChangeLink.OnChange := CustomGlyphImagesChange;
  FUseCustomGlyphs := False;

  FLightTextStyle := False;
  FTextStyle := tsNormal;
  FTextShadowDepth := 2;
  FTextShadowColor := clBtnShadow;
  FTextHighlightColor := clBtnHighlight;
  FReadOnlyColor := clInfoBk;

  FTransparentColor := clOlive;
  FWinMaskColor := clLime;
  FTabOnEnter := False;
end;


destructor TRzCustomCheckGroup.Destroy;
begin
  FItemFont.Free;
  SetCheckCount( 0 );
  TStringList( FItems ).OnChange := nil;
  FItems.Free;
  FChecks.Free;
  FCustomGlyphs.Free;
  FCustomGlyphImagesChangeLink.Free;
  FItemStates.Free;
  inherited;
end;


procedure TRzCustomCheckGroup.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FCustomGlyphImages ) then
    SetCustomGlyphImages( nil );  // Call access method so connections to link object can be cleared
end;


procedure TRzCustomCheckGroup.FlipChildren( AllLevels: Boolean );
begin
  // The check boxes are flipped using BiDiMode
end;


function TRzCustomCheckGroup.GetIndex( CheckBox: TRzCheckBox ): Integer;
begin
  Result := FChecks.IndexOf( CheckBox );
end;


procedure TRzCustomCheckGroup.CheckClick( Sender: TObject );
var
  CheckBox: TRzCheckBox;
begin
  CheckBox := TRzCheckBox( Sender );
  Change( GetIndex( CheckBox ), CheckBox.State );
end;


procedure TRzCustomCheckGroup.ItemsChange( Sender: TObject );
begin
  if not FReading then
    UpdateChecks;
end;


procedure TRzCustomCheckGroup.ItemFontChanged( Sender: TObject );
begin
  FItemHeight := GetMinFontHeight( FItemFont );
  FItemFontChanged := True;
  ArrangeChecks;
  Invalidate;
end;


procedure TRzCustomCheckGroup.ReadState( Reader: TReader );
begin
  FReading := True;
  inherited;
  FReading := False;
  UpdateChecks;
end;


procedure TRzCustomCheckGroup.Loaded;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FItemStates.Count - 1 do
    ItemState[ I ] := TCheckBoxState( FItemStates[ I ] );
end;


procedure TRzCustomCheckGroup.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'CheckStates', ReadCheckStates, WriteCheckStates, FChecks.Count > 0 );
end;


procedure TRzCustomCheckGroup.ReadCheckStates( Reader: TReader );
begin
  Reader.ReadListBegin;
  FItemStates.Clear;
  while not Reader.EndOfList do
    FItemStates.Add( Reader.ReadInteger );
  Reader.ReadListEnd;
end;


procedure TRzCustomCheckGroup.WriteCheckStates( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FChecks.Count - 1 do
    Writer.WriteInteger( Ord( ItemState[ I ] ) );
  Writer.WriteListEnd;
end;


procedure TRzCustomCheckGroup.SetCheckCount( Value: Integer );
begin
  while FChecks.Count < Value do
    TRzGroupCheck.CreateInGroup( Self );
  while FChecks.Count > Value do
    TRzGroupCheck( FChecks.Last ).Free;
end;


function TRzCustomCheckGroup.GetChecks( Index: Integer ): TRzCheckBox;
begin
  Result := TRzCheckBox( FChecks[ Index ] );
end;

function TRzCustomCheckGroup.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;


procedure TRzCustomCheckGroup.SetCaption( const Value: TCaption );
begin
  inherited Caption := Value;
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.SetColumns( Value: Integer );
begin
  if Value < 1 then
    Value := 1;
  if Value > 16 then
    Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeChecks;
    Invalidate;
  end;
end;


procedure TRzCustomCheckGroup.SetItems( Value: TStrings );
begin
  {&RV}
  FItems.Assign( Value );
end;


function TRzCustomCheckGroup.CanModify: Boolean;
begin
  Result := not FReadOnly;
end;


procedure TRzCustomCheckGroup.CustomFramingChanged;
var
  I: Integer;
begin
  if FFrameController.FrameVisible then
  begin
    if ( GroupStyle = gsCustom ) then
      inherited;
    ItemFrameColor := FFrameController.FrameColor;
    for I := 0 to FChecks.Count - 1 do
      Checks[ I ].DisabledColor := FFrameController.DisabledColor;
  end;
end;


procedure TRzCustomCheckGroup.ArrangeChecks;
var
  VisibleChecks, ChkIdx, ChecksPerCol, CheckWidth, TopMargin, I, K, X, W, L: Integer;
  R, CalcRect: TRect;
  ColWidths: array[ 0..15 ] of Integer;
  Chk: TRzGroupCheck;

  function AdjustedIndex( Index: Integer ): Integer;
  var
    I: Integer;
    Chk: TRzGroupCheck;
  begin
    Result := Index;
    for I := Index - 1 downto 0 do
    begin
      Chk := TRzGroupCheck( FChecks[ I ] );
      if not Chk.Visible then
        Dec( Result );
    end;
  end;

begin
  if ( FChecks.Count <> 0 ) and not FReading then
  begin
    VisibleChecks := 0;
    for I := 0 to FChecks.Count - 1 do
    begin
      Chk := TRzGroupCheck( FChecks[ I ] );
      if Chk.Visible then
        Inc( VisibleChecks );
    end;

    ChecksPerCol := ( VisibleChecks + FColumns - 1 ) div FColumns;

    if FSpaceEvenly then
      CheckWidth := ( ( Width - FStartXPos ) - ( 2 * BorderWidth ) - 10 ) div FColumns
    else
      CheckWidth := 0;

    R := ClientRect;
    AdjustClientRect( R );
    TopMargin := R.Top + FStartYPos;

    if not FSpaceEvenly then
    begin
      for I := 0 to FColumns - 1 do
        ColWidths[ I ] := 0;

      Canvas.Font := FItemFont;
      for I := 0 to FChecks.Count - 1 do
      begin
        Chk := TRzGroupCheck( FChecks[ I ] );
        ChkIdx := AdjustedIndex( I );
        if Chk.Visible then
        begin
          CalcRect := R;
          DrawString( Self.Canvas, RemoveAccelerators( Trim( Chk.Caption ) ), CalcRect,
                      dt_CalcRect or dt_ExpandTabs or dt_NoPrefix );

          W := CalcRect.Right - CalcRect.Left + FGlyphWidth + 4 + 8;

          if W > ColWidths[ ChkIdx div ChecksPerCol ] then
            ColWidths[ ChkIdx div ChecksPerCol ] := W;
        end;
      end;
    end;

    for I := 0 to FChecks.Count - 1 do
    begin
      Chk := TRzGroupCheck( FChecks[ I ] );
      ChkIdx := AdjustedIndex( I );
      if Chk.Visible then
      begin
        if FSpaceEvenly then
        begin
          L := ( ChkIdx div ChecksPerCol ) * CheckWidth + BorderWidth;
          if not UseRightToLeftAlignment then
            L := L + FStartXPos
          else
            L := Self.ClientWidth - L - CheckWidth - FStartXPos;

          Chk.SetBounds( L, ( ChkIdx mod ChecksPerCol ) * ( FItemHeight + FVerticalSpacing ) + TopMargin,
                         CheckWidth, FItemHeight );
        end
        else
        begin
          X := 0;
          for K := ( ChkIdx div ChecksPerCol ) - 1 downto 0 do
            X := X + ColWidths[ K ];

          L := X + BorderWidth;
          if not UseRightToLeftAlignment then
            L := L + FStartXPos
          else
            L := Self.ClientWidth - L - ColWidths[ ChkIdx div ChecksPerCol ] - 4 + 8 - FStartXPos;

          Chk.SetBounds( L, ( ChkIdx mod ChecksPerCol ) * ( FItemHeight + FVerticalSpacing ) + TopMargin,
                         ColWidths[ ChkIdx div ChecksPerCol ] - 4, FItemHeight );
        end;

        Chk.Font.Assign( FItemFont );
        Chk.BiDiMode := Self.BiDiMode;
        Chk.TextHighlightColor := FTextHighlightColor;
        Chk.TextShadowColor := FTextShadowColor;
        Chk.TextShadowDepth := FTextShadowDepth;
        Chk.LightTextStyle := FLightTextStyle;
        Chk.TextStyle := FTextStyle;
        Chk.Transparent := Self.Transparent;
        Chk.WinMaskColor := FWinMaskColor;
        Chk.TransparentColor := FTransparentColor;
        if not FCustomGlyphs.Empty then
          Chk.CustomGlyphs.Assign( FCustomGlyphs );
        Chk.CustomGlyphImages := FCustomGlyphImages;
        Chk.UseCustomGlyphs := FUseCustomGlyphs;
        Chk.FrameColor := FItemFrameColor;
        Chk.HotTrack := FItemHotTrack;
        Chk.HighlightColor := FItemHighlightColor;
        Chk.HotTrackColor := FItemHotTrackColor;
        Chk.HotTrackColorType := FItemHotTrackColorType;
        Chk.AllowGrayed := FAllowGrayed;
        Chk.ReadOnly := FReadOnly;
        Chk.ReadOnlyColor := FReadOnlyColor;
        Chk.ReadOnlyColorOnFocus := FReadOnlyColorOnFocus;
      end;
    end;
  end;
end; {= TRzCustomCheckGroup.ArrangeChecks =}


procedure TRzCustomCheckGroup.UpdateChecks;
var
  I: Integer;
begin
  SetCheckCount( FItems.Count );
  for I := 0 to FChecks.Count - 1 do
    TRzGroupCheck( FChecks[ I ] ).Caption := FItems[ I ];

  ArrangeChecks;
  Invalidate;
end;



procedure TRzCustomCheckGroup.SetAllowGrayed( Value: Boolean );
begin
  if FAllowGrayed <> Value then
  begin
    FAllowGrayed := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetCustomGlyphs( Value: TBitmap );
begin
  FCustomGlyphs.Assign( Value );
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.CustomGlyphsChanged( Sender: TObject );
begin
  UseCustomGlyphs := not FCustomGlyphs.Empty;
  Invalidate;
end;


procedure TRzCustomCheckGroup.SetCustomGlyphImages( Value: TCustomImageList );
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
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.CustomGlyphImagesChange( Sender: TObject );
begin
  if Sender = CustomGlyphImages then
  begin
    UseCustomGlyphs := FCustomGlyphImages.Count > 0;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemFrameColor( Value: TColor );
begin
  if FItemFrameColor <> Value then
  begin
    FItemFrameColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemHotTrack( Value: Boolean );
begin
  if FItemHotTrack <> Value then
  begin
    FItemHotTrack := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemHighlightColor( Value: TColor );
begin
  if FItemHighlightColor <> Value then
  begin
    FItemHighlightColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemHotTrackColor( Value: TColor );
begin
  if FItemHotTrackColor <> Value then
  begin
    FItemHotTrackColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemHotTrackColorType( Value: TRzHotTrackColorType );
begin
  if FItemHotTrackColorType <> Value then
  begin
    FItemHotTrackColorType := Value;
    ArrangeChecks;
  end;
end;


function TRzCustomCheckGroup.GetItemChecked( Index: Integer ): Boolean;
begin
  Result := TRzCheckBox( FChecks[ Index ] ).Checked;
end;


procedure TRzCustomCheckGroup.SetItemChecked( Index: Integer; Value: Boolean );
begin
  TRzCheckBox( FChecks[ Index ] ).Checked := Value;
end;


function TRzCustomCheckGroup.GetItemEnabled( Index: Integer ): Boolean;
begin
  Result := TRzCheckBox( FChecks[ Index ] ).Enabled;
end;


procedure TRzCustomCheckGroup.SetItemEnabled( Index: Integer; Value: Boolean );
begin
  TRzCheckBox( FChecks[ Index ] ).Enabled := Value;
end;


function TRzCustomCheckGroup.GetItemState( Index: Integer ): TCheckBoxState;
begin
  Result := TRzCheckBox( FChecks[ Index ] ).State;
end;


procedure TRzCustomCheckGroup.SetItemState( Index: Integer; Value: TCheckBoxState );
begin
  TRzCheckBox( FChecks[ Index ] ).State := Value;
end;


procedure TRzCustomCheckGroup.ChangeScale( M, D: Integer );
begin
  inherited;
  if FItemFontChanged then
    FItemFont.Height := MulDiv( FItemFont.Height, M, D );
  FVerticalSpacing := MulDiv( FVerticalSpacing, M, D );
  FItemHeight := MulDiv( FItemHeight, M, D );
  FStartYPos := MulDiv( FStartYPos, M, D );
  FStartXPos := MulDiv( FStartXPos, M, D );

//  for I := 0 to FChecks.Count - 1 do
//    FChecks[ I ].ChangeScale( M, D );

  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.SetItemFont( Value: TFont );
begin
  if FItemFont <> Value then
  begin
    FItemFont.Assign( Value );
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetItemHeight( Value: Integer );
begin
  if FItemHeight <> Value then
  begin
    FItemHeightChanged := True;
    FItemHeight := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetGroupBoxStyle( Value: TRzGroupBoxStyle );
begin
  inherited;
  ArrangeChecks;
  Invalidate;
end;


procedure TRzCustomCheckGroup.SetLightTextStyle( Value: Boolean );
begin
  if FLightTextStyle <> Value then
  begin
    FLightTextStyle := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetTextHighlightColor( Value: TColor );
begin
  if FTextHighlightColor <> Value then
  begin
    FTextHighlightColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetTextShadowColor( Value: TColor );
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetTextShadowDepth( Value: Integer );
begin
  if FTextShadowDepth <> Value then
  begin
    FTextShadowDepth := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetReadOnly( Value: Boolean );
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetReadOnlyColor( Value: TColor );
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetReadOnlyColorOnFocus( Value: Boolean );
begin
  if FReadOnlyColorOnFocus <> Value then
  begin
    FReadOnlyColorOnFocus := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetSpaceEvenly( Value: Boolean );
begin
  if FSpaceEvenly <> Value then
  begin
    FSpaceEvenly := Value;
    ArrangeChecks;
  end;
end;

procedure TRzCustomCheckGroup.SetStartPos( Index: Integer; Value: Integer );
begin
  if Index = 1 then
  begin
    if FStartXPos <> Value then
    begin
      FStartXPos := Value;
      ArrangeChecks;
    end;
  end
  else
  begin
    if FStartYPos <> Value then
    begin
      FStartYPos := Value;
      ArrangeChecks;
    end;
  end;
end;


procedure TRzCustomCheckGroup.SetTextStyle( Value: TTextStyle );
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetTransparent( Value: Boolean );
begin
  inherited;
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.SetTransparentColor( Value: TColor );
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetUseCustomGlyphs( Value: Boolean );
begin
  // Do not check against previous value b/c assigning an image list will
  // force UseCustomGlyphs to be true and we need the FGlyphWidth and
  // FGlyphHeight values to be updated appropriately.

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
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.SetVerticalSpacing( Value: Integer );
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.SetWinMaskColor( Value: TColor );
begin
  if FWinMaskColor <> Value then
  begin
    FWinMaskColor := Value;
    ArrangeChecks;
  end;
end;


procedure TRzCustomCheckGroup.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and CanFocus then
  begin
    SelectFirst;
    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzCustomCheckGroup.CMEnabledChanged( var Msg: TMessage );
var
  I: Integer;
begin
  inherited;
  Repaint;
  for I := 0 to FChecks.Count - 1 do
    TRzGroupCheck( FChecks[ I ] ).Enabled := Enabled;
end;


procedure TRzCustomCheckGroup.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if not FItemFontChanged then
  begin
    FItemFont.Assign( Self.Font );
    // Reset FItemFontChanged b/c internal handler has gotten called from above statements
    FItemFontChanged := False;
  end;
  Invalidate;
end;


procedure TRzCustomCheckGroup.WMSize( var Msg: TWMSize );
begin
  inherited;
  ArrangeChecks;
end;


procedure TRzCustomCheckGroup.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzCustomCheckGroup.Change( Index: Integer; NewState: TCheckBoxState );
begin
  if Assigned( FOnChange ) then
    FOnChange( Self, Index, NewState );
end;



{&RUIF}
end.
