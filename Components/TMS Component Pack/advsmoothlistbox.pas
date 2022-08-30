{*************************************************************************}
{ TAdvSmoothListBox component                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2010-2015                                         }
{           Email : info@tmssoftware.com                                  }
{           Website : http://www.tmssoftware.com/                         }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSmoothListBox;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Comobj, Activex, AdvStyleIF, ImgList, AdvSmoothTheme, StrUtils,
  GDIPPictureContainer, ExtCtrls, Math, GDIPFill, AdvSmoothListBoxDragDrop,
  Buttons, Menus, AdvGDIP, AdvHintInfo, Types
  {$IFDEF DELPHIXE3_LVL}
  ,Generics.Collections, System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : Fixed : issue with resize & detail control
  //          : Fixed : not fully hide detail control
  //          : Fixed : setting Items.SelectedItem programmatically updates list
  //          : Fixed : Items.SelectedItem set to nil when no item is selected
  //          : New : exposed Anchors, Constraints, PopupMenu
  //          : New : exposed OnMouseLeave, OnMouseEnter events
  // v1.0.2.0 : New : ScrollToItem method
  //          : New : Published property SelectedItemIndex
  //          : New : Selection mode to persist selection or auto deselect during scrolling
  //          : Fixed : focused item = last selected item
  // v1.1.0.0 : New : Exposed events OnItemMouseLeave, OnItemMouseEnter
  //          : New : Exposed events OnHeaderClick, OnFooterClick, OnLookupClick
  //          : New : Exposed events OnItemSelected, OnItemSelectionChanged
  //          : New : Exposed event OnItemHint with new item property Hint
  //          : New : function ItemAtXY(x, y: integer): integer to find the item at the given position (returns -1 when nothing is found)
  //          : New : Drag drop and reorganize items.
  //          : New : Exposed events OnItemDragStart, OnItemDragEnd, OnItemDragDrop, OnItemDragOver  
  //          : New : Multiselect items with mouse and keyboard.
  //          : New : Custom categories with improved lookupbar functionality
  //          : Fixed : Delete and free of item Access violation
  // v1.1.0.1 : Fixed : Access Violation Items.SelectedItem := nil;
  //          : Fixed : No keyboard OnItemSelected event & OnItemSelectionChanged event
  // v1.1.0.2 : Fixed : Issue with OnItemInfoClick
  // v1.1.0.3 : Fixed : List index out of bounds when hovering listbox and items.Count = 0;
  // v1.5.0.0 : New : SmoothButton and SmoothProgressBar in ListBoxItem
  //          : New : Mode sPersistAlways to MultiSelect items without keyboard.
  //          : New : Exposed event OnButtonClick.
  //          : New : Visible property
  //          : New : Item Grouping with Levels and Expanded State
  // v1.5.0.1 : Fixed : issue with progressbar with Maximum = Minimum
  // v1.5.0.2 : Fixed : issue with HTML anchor detection
  //          : Improved : ANSI sort order for items
  // v1.5.0.3 : Improved : Added lookupbar support for special characters (Ä, Ê, .. etc)
  // v1.5.0.4 : Fixed : Vertical line paint problem on right side of listbox
  // v1.5.0.5 : Improved : lookup support for special characters (Ä, Ê, .. etc)
  //          : Improved : item position after lookup click
  //          : Fixed : FHoveredItemIndex out of bounds when deleting item at runtime
  //          : Fixed : GetTopIndex and GetBottomIndex always returning 0 and DisplayList.Count - 1 causing slow painting of items
  //          : Fixed : Anchor click of Footer
  //          : Fixed : Wrong itemindex when selecting
  //          : Fixed : ProgressBar text value positioning
  //          : Fixed : OnItemSelected Event not called when Graphic type is set to gtNone
  //          : Fixed : Issue with component initialization during reparenting
  // v1.5.0.6 : New : Public property GraphicClicked which indicates if the graphic left or right is clicked
  //          : Fixed : Issue when repeatedly resizing listbox
  //          : Fixed : Issue with symbols ? < > = , ... in lookupbar
  //          : Fixed : Issue with Initlookupbar when destroying component
  //          : Fixed : Issue with Expanding / Collapsing items when total itemheight exceeds listbox height
  // v1.5.0.7 : Fixed : Issue with font creation in Delphi7
  //          : Improved : Collapse/Expand state of items
  // v1.5.0.8 : Fixed : Issue with flicker with sdOnDblClick
  // v1.5.0.9 : Fixed : Issue with focused item when detail item is animating
  // v1.5.1.0 : New : Enabled Checkbox with space key when item has focus
  //          : Fixed : Issue with multiple group expanding / collapsing on same level
  // v1.5.1.1 : Fixed : Issue with Border rounding
  // v1.5.1.2 : Fixed : Issue with Items.SelectedItem := nil
  //          : Fixed : Issue with VK_F4 in TAdvSmoothComboBox
  //          : Fixed : Issue with multiple group expanding / collapsing on same level
  //          : Fixed : Issue with Events and expanding after mouse move on graphic left
  // v1.5.1.3 : Fixed : Issue with DrawFrameControl rectangle when XP themes disables
  // v1.5.1.4 : Fixed : Default state of properties
  // v1.5.2.0 : New : Added DropDownControlClass to use a drop down control on an item
  //          : New : Exposed events OnItemDropDownHide, OnItemDropDownShow, OnItemDropDownSelect
  //          : New : Graphic left and right type gtDropDownButton to call the drop down control
  //          : New : Support for Windows Vista and Windows Seven Style
  //          : Fixed : issue with destroying combobox and listbox in same project
  //          : Fixed : issue with invalid pointer, invalid destroy order of objects
  // v1.5.3.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Fixed : issue with scrolling when losing focus
  //          : Fixed : issue with calculating item rectangles
  // v1.5.3.1 : Fixed : issue with calculating HTML notes rectangle
  //          : Fixed : issue with keyboard lookup when typing multiple characters
  //          : Fixed : issue with click position of anchors
  //          : Fixed : Access violation when using Item Detail
  // v1.5.4.0 : New : ExpandAll and CollapseAll
  //          : Fixed : Exception with destroying defaultitems
  //          : Fixed : Issue with clicking info, notes and caption rectangle
  //          : Fixed : Issue with dblclick and mouse scrolling
  //          : Improved : Issue with info rectangle and graphic right
  // v1.6.0.0 : New : Functions IndexOfCaption, IndexOfNotes and IndexOfInfo
  //          : New : OLE Drag Drop support
  //          : New : VCL Drag drop support
  //          : Improved HTML notes calculation
  // v1.6.0.1 : Fixed : Access violation in older Delphi versions with custom categories
  // v1.6.0.2 : Fixed : Issue with showing detail when double-clicking on header and footer
  // v1.6.0.3 : Fixed : Issue with OnItemSelected and keyboard lookup
  //          : Improved : Public property Parent to create listbox at runtime
  //          : Fixed : Issue with cursor
  // v2.0.0.0 : New : Database aware version of TAdvSmoothListBox
  //          : Fixed : Issue with GraphicLeft and GraphicRect calculation rectangle
  //          : Fixed : Issue with Selected count
  // v2.0.0.1 : Fixed : Issue with Parent in C++Builder
  // v2.0.0.2 : Improved : Multiselect drag & drop
  // v2.0.1.0 : New : Glow appearance in GDIPFill
  // v2.0.1.1 : Fixed : Issue with Display Rectangle and Graphic interaction
	//          : Fixed : Issue with popupmenu and item select
  // v2.0.2.0 : Fixed : Issue with Notes rectangle calculation
  //          : New : Textrendering property to change rendering of the text
  // v2.0.3.0 : New : property EnableDragging to enabled or disable drag drop
  //          : Fixed : Small issue when performing mouseup after multiselect
  // v2.0.3.1 : Fixed : Issue with ItemObject not accessible
  //          : Fixed : Workaround for OnGesture event called before click and setfocus in Windows XP and older
  // v2.0.3.2 : Fixed : Issue with selectitemindex after deletion
  // v2.0.4.0 : New : Built-in support for Office 2010 colors
  //          : Fixed : Issue with clipping HTML text
  // v2.0.4.1 : Fixed : Issue with destroying DB version
  // v2.0.4.2 : Fixed : Issue in selecting item while destroying
  //          : Fixed : Issue in Custom Category index
  // v2.1.0.0 : New : OnCompare event for custom comparing
  //          : New : Filtering
  //          : New : Iphone style Delete button
  //          : New : ImageList and PictureContainer support for GraphicLeft and GraphicRight
  //          : Fixed : Issue with accessing dataset when destroying
  // v2.2.0.0 : New : ScrollInView procedure
  //          : New : PopupMenu for each item
  //          : Improved Numeric lookupbar order before and after alphabetic characters
  //          : Improved Customizable scrollposition with CurrentScrollPosition and NextScrollPosition properties
  // v2.2.0.1 : Fixed : Issue with multiselect and deleting items
  // v2.2.0.2 : Fixed : Issue with adding items to displaylist with custom categories
  // v2.2.0.3 : Fixed : Issue with checking items after detail is shown
  //          : Fixed : Issue with default value of deletebutton
  // v2.2.0.4 : Fixed : Issue with middle mouse button drag
  //          : Fixed : Issue with speed with non html notes text
  //          : Fixed : Access violation access section items with imagelist
  // v2.2.0.5 : Fixed : Issue with dragging vs item selection
  // v2.2.0.6 : Fixed : Issue with assigning default item
  //          : Fixed : Issue with click called twice
  //          : Fixed : Issue with GraphicLeftMargin not calculated
  // v2.2.0.7 : Fixed : Various fixes and improvements
  // v2.2.1.0 : New : Event OnFilterChange
  //          : Fixed : Issue with initializing office colors
  //          : Fixed : Issue with keyboard events when filtering
  // v2.2.1.1 : Improved : added event OnGetRecordCount in DBAdvSmoothListBox
  //          : Fixed : Issue with setting selecteditemindex in Clear
  // v2.3.0.0 : New : Separator for items, SeparatorLineColor, SeparatorShadowLineColor
  //          : New : ctNode graphic type
  //          : New : ItemAppearance.NodeOpen,ItemAppearance.NodeClosed picture properties
  //          : New : ItemAppearance.InfoFill* properties added
  //          : New : HTML formatting support added in item's caption
  //          : New : ScrollIndicator.Style added to have an always visible scrollbar
  //          : New : Customizable delete button appearance
  // v2.3.0.1 : Fixed : Issue with PageUp, PageDown selecteditemindex
  // v2.3.0.2 : New : Enabled property added to enabled/disable filter functionality
  // v2.3.0.3 : Fixed : Access violation with selecting and assigning items in older delphi versions
  // v2.4.0.0 : New : PictureContainer and ImageList Support in smooth button type
  //          : New : Selected font for Caption, Info and Notes
  //          : New : OfficeHint built-in support
  //          : Improved : Lookupbar MainLevel items only
  //          : Fixed : Issue with setting button color when using office styles
  //          : Fixed : Issue with position of dropdowncontrol
  //          : Fixed : Issue with DropDownControl invalid pointer
  // v2.4.0.1 : Cleanup of outputdebugstring calls
  // v2.4.0.2 : Fixed : Issue with StayOnTop parent form
  // v2.4.0.3 : Fixed : Issue with header and footer rect when not visible.
  // v2.4.0.4 : Fixed : Issue with drawing control through WM_PRINTCLIENT message
  // v2.4.1.0 : Fixed : Issue with Assigning ImageList and PictureContainer
  //          : New : OnItemCustomizeFont, OnItemCustomizeFill and ArrowColor on AdvSmoothCombobox / AdvSmoothListBox
  // v2.4.1.1 : Fixed : Issue with DeleteButton displaying in ComboBox
  // v2.4.2.0 : New : OnGraphicLeftClick and OnGraphicRightClick events added
  //          : Fixed : Issue with font memoryleak
  // v2.4.2.1 : Improved : Move functionality on Items level to move items in the listbox
  // v2.5.0.0 : New : Metro style support
  //          : Fixed : Issue with drag and drop index
  // v2.5.0.1 : Improved : Performance in DB version
  // v2.5.0.2 : Fixed : Issue with strikeout font style
  // v2.6.0.0 : New : Autosizing items, configurable per item
  //          : New : Bubble message list layout with alternate fill per item
  //          : New : OnFilterProcessed event
  //          : Improved Clickmargin, dragmargin, deletemargin properties exposed
  //          : Fixed : issues regarding, selection, focus rectangle, interaction
  // v2.6.1.0 : New : MouseSelect property added
  // v2.6.2.0 : New : OnContextPopup exposed
  // v2.6.3.0 : New : CaptionMarginRight and CaptionWordWrap properties per item
  //          : Improved : Exposed Edit Filter control
  // v2.7.0.0 : New : Windows 8, Office 2013 styles added
  //          : Fixed : Issue with autosizing in full height calculation for scrolling
  // v2.7.0.1 : Fixed : Issue with parenting
  // v2.7.0.2 : Fixed : Issue with shift selecting invisible items
  //          : Fixed : Issue with drawing item controls
  //          : Fixed : Issue with popupmenu and mouse handling
  // v2.7.0.3 : Fixed : Issue with DB master / detail
  // v2.7.0.4 : Fixed : Issue with drawing images
  // v2.8.0.0 : New : Mouse-only dragging
  //          : New : Clickable sections
  //          : New : Section height customization
  //          : New : Section custom drawing
  //          : New : TextHint property for filter edit
  //          : New : Anchor hints
  // v2.8.0.1 : Improved : Anchor hints for header and footer
  // v2.8.0.2 : Fixed : Issue with showing regular hints
  // v2.8.0.3 : Improved : Image list drawing quality
  // v2.8.1.0 : New : Property LookupBar.Margin added
  // v2.8.1.1 : Fixed : Issue with GetBottomIndex
  // v2.8.1.2 : Fixed : Issue with positioning items with sections
  // v2.8.1.3 : Fixed : Issue with selection in combination with hidden items and sections
  // v2.8.1.4 : Improved : override filter edit class
  // v2.8.1.5 : Fixed : Issue with TAdvSmoothComboBox on container controls at designtime
  // v2.8.1.6 : Fixed : Issue with assigning GraphicRightWidth / GraphicRightHeight
  // v2.8.1.7 : Fixed : Issue with Autosizing plain text
  // v2.8.2.0 : New : Public property Item.Key added
  // v2.8.2.1 : Improved : OnItemButtonLeftClick and OnItemButtonRightClick added.
  // v2.8.2.2 : Fixed : Issue with border not painted on selected item
  // v2.9.0.0 : New : Windows 10, Office 2016 styles added
  // v2.9.0.1 : Fixed : Rare issue with access-violation on component destroy
  // v2.9.0.2 : Fixed : Issue with selected caption font when selecting items with keyboard
  // v2.9.0.3 : Fixed : Issue with smooth scrolling and db version not loading items
  // v2.9.0.4 : Improved : ftGraphic support
  // v2.9.0.5 : Fixed : Issue with autosizing rectangle calculation

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothListBox = class;

  TAdvSmoothListBoxLocation = (plTopLeft, plTopCenter, plTopRight, plCenterLeft, plCenterCenter, plCenterRight, plBottomLeft, plBottomCenter, plBottomRight, plCustom);

  TAdvSmoothListBoxShowDetailClick = (sdOnClick, sdOnDetailImageClick, sdOnDblClick, sdOnDetailImageDblClick);

  TAdvSmoothListBoxShowDetailKey = (dkSpace, dkNone, dkF2, dkReturn);

  TAdvSmoothListBoxTextRendering = (tAntiAlias, tAntiAliasGridFit, tClearType);

  TAdvSmoothListBoxItems = class;

  TAdvSmoothListBoxItem = class;

  ISmoothListBox = interface
  ['{72FF309E-17C6-4AFE-BC4A-9565CF873AE5}']
  procedure SetOwner(Owner: TAdvSmoothListBox; Item: TAdvSmoothListBoxItem);
  procedure Show(Item: TAdvSmoothListBoxItem);
  end;

  TAdvSmoothListBoxGraphicClicked = (cLeft, cRight);

  TAdvSmoothListBoxMouseSelect = (lmsLeft,lmsRight,lmsBoth);

  TAdvSmoothListBoxItemAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothListBox;
    FSpacingVert: integer;
    FSpacingHorz: integer;
    FItemHeight: integer;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FFillDisabled: TGDIPFill;
    FFillSelected: TGDIPFill;
    FInfoFill: TGDIPFill;
    FInfoFillSelected: TGDIPFill;
    FInfoFillDisabled: TGDIPFill;
    FProgressAppearance: TGDIPProgress;
    FButtonAppearance: TGDIPButton;
    FDeleteButtonCaption: string;
    FDeleteButtonWidth: Integer;
    FDeleteButtonHeight: Integer;
    FDeleteButtonColor: TColor;
    FDeleteButtonColorDown: TColor;
    FNodeOpen: TAdvGDIPPicture;
    FNodeClosed: TAdvGDIPPicture;
    FNodeClosedName: string;
    FNodeOpenName: string;
    FSeparatorLineColor: TColor;
    FSeparatorLineShadowColor: TColor;
    FUseInfoFill: boolean;
    FFillAlternate: TGDIPFill;
    FFillSelectedAlternate: TGDIPFill;
    procedure SetItemHeight(const Value: integer);
    procedure SetSpacingHorz(const Value: integer);
    procedure SetSpacingVert(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetFillDisabled(const Value: TGDIPFill);
    procedure SetFillSelected(const Value: TGDIPFill);
    procedure SetProgressAppearance(const Value: TGDIPProgress);
    procedure SetButtonAppearance(const Value: TGDIPButton);
    procedure SetDeleteButtonHeight(const Value: Integer);
    procedure SetDeleteButtonWidth(const Value: Integer);
    procedure SetDeleteButtonColor(const Value: TColor);
    procedure SetDeleteButtonColorDown(const Value: TColor);
    procedure SetNodeClosed(const Value: TAdvGDIPPicture);
    procedure SetNodeOpen(const Value: TAdvGDIPPicture);
    procedure SetNodeClosedName(const Value: string);
    procedure SetNodeOpenName(const Value: string);
    procedure SetSeparatorLineColor(const Value: TColor);
    procedure SetSeparatorLineShadowColor(const Value: TColor);
    procedure SetInfoFill(const Value: TGDIPFill);
    procedure SetInfoFillDisabled(const Value: TGDIPFill);
    procedure SetInfoFillSelected(const Value: TGDIPFill);
    procedure SetUseInfoFill(const Value: boolean);
    procedure SetFillAlternate(const Value: TGDIPFill);
    procedure SetFillSelectedAlternate(const Value: TGDIPFill);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure ProgressAppearanceChanged(Sender: TObject);
    procedure ButtonAppearanceChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FillAlternate: TGDIPFill read FFillAlternate write SetFillAlternate;
    property Fill: TGDIPFill read FFill write SetFill;
    property FillSelected: TGDIPFill read FFillSelected write SetFillSelected;
    property FillSelectedAlternate: TGDIPFill read FFillSelectedAlternate write SetFillSelectedAlternate;
    property FillDisabled: TGDIPFill read FFillDisabled write SetFillDisabled;
    property ProgressAppearance: TGDIPProgress read FProgressAppearance write SetProgressAppearance;
    property ButtonAppearance: TGDIPButton read FButtonAppearance write SetButtonAppearance;
    property VerticalSpacing: integer read FSpacingVert write SetSpacingVert default 0;
    property HorizontalSpacing: integer read FSpacingHorz write SetSpacingHorz default 0;
    property Height: integer read FItemHeight write SetItemHeight default 30;

    property InfoFill: TGDIPFill read FInfoFill write SetInfoFill;
    property InfoFillSelected: TGDIPFill read FInfoFillSelected write SetInfoFillSelected;
    property InfoFillDisabled: TGDIPFill read FInfoFillDisabled write SetInfoFillDisabled;

    property DeleteButtonCaption: string read FDeleteButtonCaption write FDeleteButtonCaption;
    property DeleteButtonHeight: Integer read FDeleteButtonHeight write SetDeleteButtonHeight default 22;
    property DeleteButtonWidth: Integer read FDeleteButtonWidth write SetDeleteButtonWidth default 45;
    property DeleteButtonColor: TColor read FDeleteButtonColor write SetDeleteButtonColor default clRed;
    property DeleteButtonColorDown: TColor read FDeleteButtonColorDown write SetDeleteButtonColorDown default $0000D9;
    property NodeOpen: TAdvGDIPPicture read FNodeOpen write SetNodeOpen;
    property NodeOpenName: string read FNodeOpenName write SetNodeOpenName;
    property NodeClosed: TAdvGDIPPicture read FNodeClosed write SetNodeClosed;
    property NodeClosedName: string read FNodeClosedName write SetNodeClosedName;
    property SeparatorLineColor: TColor read FSeparatorLineColor write SetSeparatorLineColor default clNone;
    property SeparatorLineShadowColor: TColor read FSeparatorLineShadowColor write SetSeparatorLineShadowColor default clNone;
    property UseInfoFill: boolean read FUseInfoFill write SetUseInfoFill default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothListBoxItemKind = (ikItem, ikSection);

  TAdvSmoothListBoxGraphicType = (gtCheckBox, gtRadio, gtButton, gtImage, gtDetailImage, gtCommonImage, gtCommonDetailImage,
    gtSmoothButton, gtDropDownButton, gtNone, gtNode);

  TAdvSmoothListBoxGraphicShow = (gsAlways, gsSelected, gsEnabled, gsNever);

  TAdvSmoothListBoxAlternate = (lbaAuto, lbaLeft, lbaRight);

  TAdvSmoothListBoxItem = class(TCollectionItem)
  private
    FDeleteButtonDown: Boolean;
    FLastNode: integer;
    FVisualizeNodes: Boolean;
    FOwner: TAdvSmoothListBox;
    FMouseEntered, FMouseLeft: Boolean;
    fgrRight, fgrLeft: TRect;
    fcaptionr, finfor: TGPRectF;
    fn: String;
    Fhtmlr: TRect;
    FCaption: string;
    FEnabled: Boolean;
    FGraphicLeftType: TAdvSmoothListBoxGraphicType;
    FGraphicLeftShow: TAdvSmoothListBoxGraphicShow;
    FCaptionAlignment: TAlignment;
    FGraphicRightType: TAdvSmoothListBoxGraphicType;
    FNotes: String;
    FGraphicLeft: TAdvGDIPPicture;
    FGraphicLeftSelected: TAdvGDIPPicture;
    FGraphicRightShow: TAdvSmoothListBoxGraphicShow;
    FGraphicRight: TAdvGDIPPicture;
    FGraphicRightSelected: TAdvGDIPPicture;
    FOfficeHint: TAdvHintInfo;
    FInfo: String;
    FCaptionFont: TFont;
    FInfoFont: TFont;
    FControl: TControl;
    FSplitter: Boolean;
    FNotesTop: integer;
    FNotesLeft: integer;
    FNoteshadowColor: TColor;
    FNotesLocation: TAdvSmoothListBoxLocation;
    FNotesURLColor: TColor;
    FNotesShadowOffset: integer;
    FObject: TObject;
    FTag: integer;
    FButtonLeft, FBLD: Boolean;
    FButtonRight, FBRD: Boolean;
    FChecked: Boolean;
    FMargin: integer;
    FGraphicLeftMargin: integer;
    FGraphicRightMargin: integer;
    FNotesFont: TFont;
    FHint: String;
    FCategoryID: integer;
    FProgressMin: Double;
    FProgressValue: Double;
    FProgressHeight: integer;
    FProgressPosition: TAdvSmoothListBoxLocation;
    FProgressMax: Double;
    FProgressWidth: integer;
    FProgressMargin: integer;
    FProgressVisible: Boolean;
    FProgressTop: integer;
    FProgressLeft: integer;
    FGraphicLeftHeight: integer;
    FGraphicRightHeight: integer;
    FGraphicLeftWidth: integer;
    FGraphicRightWidth: integer;
    FButtonColor: TColor;
    FBevelColor: TColor;
    FButtonShadow: Boolean;
    FButtonCaption: String;
    FButtonBevel: Boolean;
    FVisible: Boolean;
    FIndent: integer;
    FLevel: integer;
    FExpanded: Boolean;
    FDropDownControl: TWinControl;
    FDeleteButton: Boolean;
    FGraphicRightIndex: Integer;
    FGraphicLeftName: String;
    FGraphicLeftSelectedName: String;
    FGraphicRightName: String;
    FGraphicRightSelectedName: String;
    FGraphicLeftIndex: Integer;
    FPopupMenu: TPopupMenu;
    FDeleteButtonVisible: Boolean;
    FSeparatorLine: Boolean;
    FNotesSelectedFont: TFont;
    FCaptionSelectedFont: TFont;
    FInfoSelectedFont: TFont;
    FHeight: Integer;
    FAutoSize: Boolean;
    FAlternate: TAdvSmoothListBoxAlternate;
    FMarginRight: integer;
    FCaptionWordWrap: Boolean;
    FKey: string;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(Value: Boolean);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetGraphicLeft(const Value: TAdvGDIPPicture);
    procedure SetGraphicLeftSelected(const Value: TAdvGDIPPicture);
    procedure SetGraphicLeftShow(const Value: TAdvSmoothListBoxGraphicShow);
    procedure SetGraphicLeftType(const Value: TAdvSmoothListBoxGraphicType);
    procedure SetGraphicRight(const Value: TAdvGDIPPicture);
    procedure SetGraphicRightSelected(const Value: TAdvGDIPPicture);
    procedure SetGraphicRightShow(const Value: TAdvSmoothListBoxGraphicShow);
    procedure SetGraphicRightType(const Value: TAdvSmoothListBoxGraphicType);
    procedure SetInfo(const Value: String);
    procedure SetNotes(const Value: String);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetInfoFont(const Value: TFont);
    procedure SetControl(const Value: TControl);
    procedure SetSplitter(const Value: Boolean);
    procedure SetNotesLeft(const Value: integer);
    procedure SetNotesLocation(const Value: TAdvSmoothListBoxLocation);
    procedure SetNotesShadowColor(const Value: TColor);
    procedure SetNotesShadowOffset(const Value: integer);
    procedure SetNotesTop(const Value: integer);
    procedure SetNotesURLColor(const Value: TColor);
    procedure SetChecked(const Value: Boolean);
    procedure SetMargin(const Value: integer);
    procedure SetGraphicLeftMargin(const Value: integer);
    procedure SetGraphicRightMargin(const Value: integer);
    procedure SetNotesFont(const Value: TFont);
    procedure SetHint(const Value: String);
    procedure SetSelected(const Value: Boolean);
    procedure SetCategoryID(const Value: integer);
    procedure SetProgressHeight(const Value: integer);
    procedure SetProgressMax(const Value: Double);
    procedure SetProgressMin(const Value: Double);
    procedure SetProgressPosition(const Value: TAdvSmoothListBoxLocation);
    procedure SetProgressValue(const Value: Double);
    procedure SetProgressWidth(const Value: integer);
    procedure SetProgressMargin(const Value: integer);
    procedure SetProgressVisible(const Value: Boolean);
    procedure SetProgressLeft(const Value: integer);
    procedure SetProgressTop(const Value: integer);
    procedure SetGraphicLeftHeight(const Value: integer);
    procedure SetGraphicLeftWidth(const Value: integer);
    procedure SetGraphicRightHeight(const Value: integer);
    procedure SetGraphicRightWidth(const Value: integer);
    procedure SetBevelColor(const Value: TColor);
    procedure SetButtonCaption(const Value: String);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonShadow(const Value: Boolean);
    procedure SetButtonBevel(const Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetIndent(const Value: integer);
    procedure SetExpanded(const Value: Boolean);
    procedure SetLevel(const Value: integer);
    procedure SetDeleteButton(const Value: Boolean);
    procedure SetGraphicLeftIndex(const Value: Integer);
    procedure SetGraphicLeftName(const Value: String);
    procedure SetGraphicLeftSelectedName(const Value: String);
    procedure SetGraphicRightIndex(const Value: Integer);
    procedure SetGraphicRightName(const Value: String);
    procedure SetGraphicRightSelectedName(const Value: String);
    procedure SetDeleteButtonVisible(const Value: Boolean);
    procedure SetSeparatorLine(const Value: Boolean);
    procedure SetCaptionSelectedFont(const Value: TFont);
    procedure SetInfoSelectedFont(const Value: TFont);
    procedure SetNotesSelectedFont(const Value: TFont);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetHeight(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetAlternate(const Value: TAdvSmoothListBoxAlternate);
    procedure SetMarginRight(const Value: integer);
    procedure SetCaptionWordWrap(const Value: Boolean);
  protected
    FSelected: Boolean;
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure CreateDropDownItem;
    function GetDisplayName: string; override;
    procedure SetInternalChecked(Value: Boolean);
    procedure SetInternalHint(Value: string);
    procedure SetInternalProgressValue(Value: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignVisuals(Source: TPersistent);
    procedure CopySettings(Source: TPersistent); //no repaint
    procedure SaveItemValues(Caption, Info, Notes: String);
    function GetAnchorAt(X, Y: integer; Focus: Boolean = False): String;
    procedure Collapse;
    procedure Expand;
    property Expanded: Boolean read FExpanded write SetExpanded default false;
    //ADVSMOOTHCOMBOBOX
    procedure Draw(ACanvas: TCanvas; R: TRect; DisplayIndex: integer; DragItem: Boolean = false; Transparent: boolean = false; PaintFocus: Boolean = True);
    function IsGraphicLeft(X, Y: integer): Boolean;
    function IsGraphicRight(X, Y: integer): Boolean;
    function IsInfo(X, Y: integer): Boolean;
    function IsCaption(X, Y: integer): Boolean;
    property ItemObject: TObject read FObject write FObject;
    property ButtonLeftDown: Boolean read FButtonLeft write FButtonLeft;
    property ButtonRightDown: Boolean read FButtonRight write FButtonRight;
    property DropDownControl: TWinControl read FDropDownControl write FDropDownControl;
    property Captionr: TGPRectF read Fcaptionr write Fcaptionr;
    property Infor: TGPRectF read finfor write FInfoR;
    property NotesR: TRect read Fhtmlr write Fhtmlr;
    property Key: string read FKey write FKey;
    //
  published
    property Alternate: TAdvSmoothListBoxAlternate read FAlternate write SetAlternate default lbaAuto;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default false;
    property Height: Integer read FHeight write SetHeight default -1;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Indent: integer read FIndent write SetIndent default 0;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property Notes: String read FNotes write SetNotes;
    property NotesURLColor: TColor read FNotesURLColor write SetNotesURLColor default clBlue;
    property NotesShadowColor: TColor read FNoteshadowColor write SetNotesShadowColor default clGray;
    property NotesShadowOffset: integer read FNotesShadowOffset write SetNotesShadowOffset default 5;
    property NotesLocation: TAdvSmoothListBoxLocation read FNotesLocation write SetNotesLocation default plCenterCenter;
    property NotesLeft: integer read FNotesLeft write SetNotesLeft default 0;
    property NotesTop: integer read FNotesTop write SetNotesTop default 0;
    property Info: String read FInfo write SetInfo;
    property Hint: String read FHint write SetHint;
     property GraphicLeftMargin: integer read FGraphicLeftMargin write SetGraphicLeftMargin default 3;
    property GraphicRightMargin: integer read FGraphicRightMargin write SetGraphicRightMargin default 3;
    property GraphicLeftType: TAdvSmoothListBoxGraphicType read FGraphicLeftType write SetGraphicLeftType default gtNone;
    property GraphicRightType: TAdvSmoothListBoxGraphicType read FGraphicRightType write SetGraphicRightType default gtNone;
    property GraphicLeftIndex: Integer read FGraphicLeftIndex write SetGraphicLeftIndex default -1;
    property GraphicLeftName: String read FGraphicLeftName write SetGraphicLeftName;
    property GraphicLeftSelectedName: String read FGraphicLeftSelectedName write SetGraphicLeftSelectedName;
    property GraphicRightIndex: Integer read FGraphicRightIndex write SetGraphicRightIndex default -1;
    property GraphicRightName: String read FGraphicRightName write SetGraphicRightName;
    property GraphicRightSelectedName: String read FGraphicRightSelectedName write SetGraphicRightSelectedName;
    property GraphicLeft: TAdvGDIPPicture read FGraphicLeft write SetGraphicLeft;
    property GraphicLeftSelected: TAdvGDIPPicture read FGraphicLeftSelected write SetGraphicLeftSelected;
    property GraphicRight: TAdvGDIPPicture read FGraphicRight write SetGraphicRight;
    property GraphicRightSelected: TAdvGDIPPicture read FGraphicRightSelected write SetGraphicRightSelected;
    property GraphicLeftShow: TAdvSmoothListBoxGraphicShow read FGraphicLeftShow write SetGraphicLeftShow default gsAlways;
    property GraphicRightShow: TAdvSmoothListBoxGraphicShow read FGraphicRightShow write SetGraphicRightShow default gsAlways;
    property GraphicLeftWidth: integer read FGraphicLeftWidth write SetGraphicLeftWidth default 30;
    property GraphicLeftHeight: integer read FGraphicLeftHeight write SetGraphicLeftHeight default 25;
    property GraphicRightWidth: integer read FGraphicRightWidth write SetGraphicRightWidth default 30;
    property GraphicRightHeight: integer read FGraphicRightHeight write SetGraphicRightHeight default 25;
     property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionSelectedFont: TFont read FCaptionSelectedFont write SetCaptionSelectedFont;
    property InfoFont: TFont read FInfoFont write SetInfoFont;
    property InfoSelectedFont: TFont read FInfoSelectedFont write SetInfoSelectedFont;
    property NotesFont: TFont read FNotesFont write SetNotesFont;
    property NotesSelectedFont: TFont read FNotesSelectedFont write SetNotesSelectedFont;
    property DetailControl: TControl read FControl write SetControl;
    property Tag: integer read FTag write FTag default 0;
    property Splitter: Boolean read FSplitter write SetSplitter default false;
    property CaptionMargin: integer read FMargin write SetMargin default 3;
    property CaptionMarginRight: integer read FMarginRight write SetMarginRight default 3;
    property CaptionWordWrap: Boolean read FCaptionWordWrap write SetCaptionWordWrap default True;
    property Selected: Boolean read FSelected write SetSelected default false;
    property CategoryID: integer read FCategoryID write SetCategoryID default -1;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property ProgressMinimum: Double read FProgressMin write SetProgressMin;
    property ProgressMaximum: Double read FProgressMax write SetProgressMax;
    property ProgressValue: Double read FProgressValue write SetProgressValue;
    property ProgressPosition: TAdvSmoothListBoxLocation read FProgressPosition write SetProgressPosition default plBottomCenter;
    property ProgressHeight: integer read FProgressHeight write SetProgressHeight default 15;
     property ProgressWidth: integer read FProgressWidth write SetProgressWidth default 130;
    property ProgressMargin: integer read FProgressMargin write SetProgressMargin default 3;
    property ProgressVisible: Boolean read FProgressVisible write SetProgressVisible default false;
    property ProgressLeft: integer read FProgressLeft write SetProgressLeft default 0;
    property ProgressTop: integer read FProgressTop write SetProgressTop default 0;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clGray;
    property ButtonBevelColor: TColor read FBevelColor write SetBevelColor default clWhite;
    property ButtonBevel: Boolean read FButtonBevel write SetButtonBevel default true;
    property ButtonShadow: Boolean read FButtonShadow write SetButtonShadow default false;
    property ButtonCaption: String read FButtonCaption write SetButtonCaption;
    property Level: integer read FLevel write SetLevel default 0;
    property DeleteButton: Boolean read FDeleteButton write SetDeleteButton default false;
    property DeleteButtonVisible: Boolean read FDeleteButtonVisible write SetDeleteButtonVisible default False;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property SeparatorLine: boolean read FSeparatorLine write SetSeparatorLine default false;
  end;

  TItemSelectArray = Array of Integer;

  TAdvSmoothListBoxItems = class(TCollection)
  private
    FOwner: TAdvSmoothListBox;
    FOnChange: TNotifyEvent;
    FSelectedItem: TAdvSmoothListBoxItem;
    function GetItem(Index: Integer): TAdvSmoothListBoxItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothListBoxItem);
    procedure SetSelectedItem(const Value: TAdvSmoothListBoxItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Compare(Item1, Item2 : TAdvSmoothListBoxItem) : integer; virtual;
    procedure QuickSort(L, R: Integer);
    function GetOwner: TPersistent; override;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    constructor Create(AOwner: TAdvSmoothListBox);
    property Items[Index: Integer]: TAdvSmoothListBoxItem read GetItem write SetItem; default;
    function Add: TAdvSmoothListBoxItem;
    function Insert(Index: Integer): TAdvSmoothListBoxItem;
    procedure Delete(Index: Integer);
    procedure Move(FromIndex, ToIndex: Integer);
    procedure Sort;
    procedure Clear;
    property SelectedItem: TAdvSmoothListBoxItem read FSelectedItem write SetSelectedItem;
    procedure SelectAll;
    procedure UnSelectAll;
    procedure Select(AItems: Array of Integer);
    function CountSelected: integer;
    function IndexOfCaption(const S: String): Integer;
    function IndexOfNotes(const S: String): Integer;
    function IndexOfInfo(const S: String): Integer;
  end;

  TAdvSmoothListBoxCategoryItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothListBox;
    FText: String;
    FID: integer;
    FTag: integer;
    FImageIndex: integer;
    FLookupText: String;
    procedure SetText(const Value: String);
    procedure SetId(const Value: integer);
    procedure SetImageIndex(const Value: integer);
    procedure SetTag(const Value: integer);
    procedure SetLookupText(const Value: String);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Tag: integer read FTag write SetTag;
    property Text: String read FText write SetText;
    property LookupText: String read FLookupText write SetLookupText;
    property Id: integer read FID write SetId;
  end;

  TAdvSmoothListBoxCategoryItems = class(TCollection)
  private
    FOwner: TAdvSmoothListBox;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothListBoxCategoryItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothListBoxCategoryItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
    function Compare(Item1, Item2 : TAdvSmoothListBoxCategoryItem) : integer; virtual;
    procedure QuickSort(L, R: Integer);    
  public
    constructor Create(AOwner: TAdvSmoothListBox);
    property Items[Index: Integer]: TAdvSmoothListBoxCategoryItem read GetItem write SetItem; default;
    function ItemById(id: integer): TAdvSmoothListBoxCategoryItem;
    function ItemIndexById(id: integer): integer;    
    function Add: TAdvSmoothListBoxCategoryItem;
    function Insert(Index: Integer): TAdvSmoothListBoxCategoryItem;
    procedure Delete(Index: Integer);
    procedure Sort;    
    procedure Clear;
  end;

  TAdvSmoothListBoxLookUpBarPosition = (pLeft, pRight);

  TLookUpBarOrder = (loNumericFirst, loNumericLast);

  TAdvSmoothListBoxLookUpBar = class(TPersistent)
  private
    FChar: array[1..36] of Boolean;
    FCustomChar: array of Boolean;
    Fowner: TAdvSmoothListBox;
    FDisabledFont: TFont;
    FColor: TColor;
    FColorTo: TColor;
    FFont: TFont;
    FVisible: Boolean;
    FNumeric: Boolean;
    FOnChange: TNotifyEvent;
    FPosition: TAdvSmoothListBoxLookUpBarPosition;
    FOpacity: Byte;
    FOpacityTo: Byte;
    FGradientType: TAdvGradientType;
    FHatchStyle: THatchStyle;
    FOnTop: Boolean;
    FAutoSize: Boolean;
    FSpacing: integer;
    FRotated: Boolean;
    FOrder: TLookUpBarOrder;
    FMainLevelOnly: Boolean;
    FMargin: integer;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetFont(const Value: TFont);
    procedure SetNumeric(const Value: Boolean);
    procedure SetPosition(const Value: TAdvSmoothListBoxLookUpBarPosition);
    procedure SetVisible(Value: Boolean);
    procedure SetOpacity(const Value: Byte);
    procedure SetOpacityTo(const Value: Byte);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetOnTop(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetRotated(const Value: Boolean);
    procedure SetSpacing(const Value: integer);
    function GetVisible: Boolean;
    procedure SetOrder(const Value: TLookUpBarOrder);
    procedure SetMainLevelOnly(const Value: Boolean);
    procedure SetMargin(const Value: integer);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure InitLookupBar;
    function GetMinimumLevel: Integer;
  public
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetWidth: integer;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clWhite;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
    property Font: TFont read FFont write SetFont;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtSolid;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property MainLevelOnly: Boolean read FMainLevelOnly write SetMainLevelOnly default False;
    property Margin: integer read FMargin write SetMargin default 4;
    property Numeric: Boolean read FNumeric write SetNumeric default false;
    property Opacity: Byte read FOpacity write SetOpacity default 100;
    property OpacityTo: Byte read FOpacityTo write SetOpacityTo default 100;
    property Order: TLookUpBarOrder read FOrder write SetOrder default loNumericLast;
    property Position: TAdvSmoothListBoxLookUpBarPosition read FPosition write SetPosition default pRight;
    property OnTop: Boolean read FOnTop write SetOnTop default false;
    property Spacing: integer read FSpacing write SetSpacing default 3;
    property Rotated: Boolean read FRotated write SetRotated default false;
    property Visible: Boolean read GetVisible write SetVisible default true;
  end;

  TAdvSmoothListBoxCategoryType = (alphanumeric, custom);

  TAdvSmoothListBoxLayout = (lblNormal, lblBubble);

  TAdvSmoothListBoxImageAlign = (alLeft, alRight);

  TAdvSmoothListBoxSections = class(TPersistent)
  private
    FOwner: TAdvSmoothListBox;
    FOpacity: Byte;
    FBorderColor: TColor;
    FGradientType: TAdvGradientType;
    FOpacityTo: Byte;
    FHatchStyle: THatchStyle;
    FColor: TColor;
    FColorTo: TColor;
    FFont: TFont;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FCategoryType: TAdvSmoothListBoxCategoryType;
    FBorderWidth: integer;
    FCategories: TAdvSmoothListBoxCategoryItems;
    FHeight: Integer;
    FImageAlign: TAdvSmoothListBoxImageAlign;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetOpacityTo(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetBorderWidth(const Value: integer);
    procedure SetHeight(const Value: Integer);
    procedure SetImageAlign(const Value: TAdvSmoothListBoxImageAlign);
  public
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure CategoriesChanged(Sender: TObject);
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write SetHeight default -1;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible default false;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtSolid;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 1;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clSilver;
    property Opacity: Byte read FOpacity write SetOpacity default 180;
    property OpacityTo: Byte read FOpacityTo write SetOpacityTo default 180;
    property ImageAlign: TAdvSmoothListBoxImageAlign read FImageAlign write SetImageAlign default alLeft;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothListBoxDisplayListItem = class(TObject)
  private
    FItemRect: TRect;
    FFloating: Boolean;
    FKind: TAdvSmoothListBoxItemKind;
    FItem: TAdvSmoothListBoxItem;
    FSectionCaption: String;
    FSectionCategoryID: integer;
    procedure SetFloating(const Value: Boolean);
    procedure SetKind(const Value: TAdvSmoothListBoxItemKind);
    procedure SetSectionCaption(const Value: String);
    procedure SetSectionCategoryID(const Value: integer);
  protected
    procedure Changed;
  public
    property Kind: TAdvSmoothListBoxItemKind read FKind write SetKind;
    property SectionCaption: String read FSectionCaption write SetSectionCaption;
    property SectionCategoryID: integer read FSectionCategoryID write SetSectionCategoryID;
    property Floating: Boolean read FFloating write SetFloating;
    property DisplayItem: TAdvSmoothListBoxItem read FItem write FItem;
    property ItemRect: TRect read FItemRect write FItemRect;
  end;

  TAdvSmoothListBoxDisplayList = class(TList)
  public
    procedure Clear; override;
    procedure DeleteItem(index: integer);
    function AddItem: TAdvSmoothListBoxDisplayListItem;
    function GetItem(index: integer): TAdvSmoothListBoxDisplayListItem;
  end;

  TAdvSmoothListBoxCaptionLocation = (cpTopLeft, cpTopCenter, cpTopRight, cpCenterLeft, cpCenterCenter, cpCenterRight, cpBottomLeft, cpBottomCenter, cpBottomRight, cpCustom);

  TAdvSmoothListBoxHeaderFooter = class(TPersistent)
  private
    FOwner: TAdvSmoothListBox;
    FFont: TFont;
    FCaption: String;
    FHeight: integer;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FCaptionTop: integer;
    FCaptionLeft: integer;
    FFill: TGDIPFill;
    FCaptionShadowColor: TColor;
    FCaptionLocation: TAdvSmoothListBoxCaptionLocation;
    FCaptionURLColor: TColor;
    FCaptionShadowOffset: integer;
    procedure SetCaption(const Value: String);
    procedure SetFont(const Value: TFont);
    procedure SetVisible(const Value: Boolean);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionTop(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: integer);
    procedure SetCaptionLocation(const Value: TAdvSmoothListBoxCaptionLocation);
    procedure SetCaptionShadowColor(const Value: TColor);
    procedure SetCaptionShadowOffset(const Value: integer);
    procedure SetCaptionURLColor(const Value: TColor);
  public
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetHeight: integer;
    function GetAnchorAt(r: TRect; X, Y: integer; Focus: Boolean = False): String;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Height: integer read FHeight write SetHeight default 40;
    property Caption: String read FCaption write SetCaption;
    property CaptionURLColor: TColor read FCaptionURLColor write SetCaptionURLColor default clBlue;
    property CaptionShadowColor: TColor read FCaptionShadowColor write SetCaptionShadowColor default clGray;
    property CaptionShadowOffset: integer read FCaptionShadowOffset write SetCaptionShadowOffset default 5;
    property CaptionLocation: TAdvSmoothListBoxCaptionLocation read FCaptionLocation write SetCaptionLocation default cpCenterCenter;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop default 0;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TScrollIndicatorStyle = (ssiPhone, ssAlways);

  TAdvSmoothListBoxIndicator = class(TPersistent)
  private
    FOwner: TAdvSmoothListBox;
    FGradientType: TAdvGradientType;
    FOpacity: Byte;
    FHatchStyle: THatchStyle;
    FColor: TColor;
    FColorTo: TColor;
    FVisible: Boolean;
    FFade: Boolean;
    FOnChange: TNotifyEvent;
    FWidth: integer;
    FHeight: integer;
    FAnimateOpacity: integer;
    FStyle: TScrollIndicatorStyle;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetOpacity(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetFade(const Value: Boolean);
    procedure Setwidth(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetAnimationOpacity: Byte;
    function GetWidth: integer;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorTo: TColor read FColorTo write SetColorTo default clDkGray;
    property Fade: Boolean read FFade write SetFade default true;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtSolid;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property Opacity: Byte read FOpacity write SetOpacity default 100;
    property Style: TScrollIndicatorStyle read FStyle write FStyle default ssiPhone;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: integer read FWidth write Setwidth default 5;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothListBoxDetailStatus = (dsDetailsNotVisible, dsDetailsVisible);

  TAdvSmoothListBoxItemClickEvent = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxSectionClickEvent = procedure(Sender: TObject; Caption: String; CategoryID: Integer) of object;

  TAdvSmoothListBoxSectionHeight = procedure(Sender: TObject; Caption: String; CategoryID: Integer; var AHeight: Integer) of object;

  TAdvSmoothListBoxItemSelectedEvent = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxItemSelectionChangedEvent = procedure(Sender: TObject; previousitemindex, itemindex: integer) of object;

  TAdvSmoothListBoxItemCheckedEvent = procedure(Sender: TObject; itemindex: integer; checked: Boolean) of object;

  TAdvSmoothListBoxScrollEvent = procedure(Sender: TObject; CurrentPosition, EndPosition: Double) of object;

  TAdvSmoothListBoxDetailEvent = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxItemTextEvent = procedure(Sender: TObject; itemindex: integer; var itemcaption: String; var iteminfo: String; var itemnotes: String) of object;

  TAdvSmoothListBoxSectionDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect; Caption: String; CategoryID: Integer; var defaultdraw: Boolean) of object;

  TAdvSmoothListBoxItemBkgDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemRect: TRect; var defaultdraw: Boolean) of object;

  TAdvSmoothListBoxItemDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemrect: TRect; var defaultdraw: Boolean) of object;

  TAdvSmoothListBoxAnchorClickEvent = procedure(Sender: TObject; Anchor: String) of object;

  TAdvSmoothListBoxItemAnchorClickEvent = procedure(Sender: TObject; Anchor: String; ItemIndex: integer) of object;

  TAdvSmoothListBoxItemCaptionClick = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxItemInfoClick = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxItemMouseEvent = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxLookUpClickEvent = procedure(Sender: TObject; lookupindex: integer; lookupvalue: string) of object;

  TAdvSmoothListBoxItemHintEvent = procedure(Sender: TObject; itemindex: integer; var hint: string) of object;

  TAdvSmoothListBoxFooterClickEvent = procedure(Sender: TObject; X, Y: integer) of object;

  TAdvSmoothListBoxHeaderClickEvent = procedure(Sender: TObject; X, Y: integer) of object;

  TAdvSmoothListBoxItemDragStartEvent = procedure(Sender: TObject; DragItemIndex: integer; var allowdrag: Boolean) of object;

  TAdvSmoothListBoxItemDragDropEvent = procedure(Sender: TObject; DragItemIndex, DropItemIndex: integer; var allowdrop: Boolean) of object;

  TAdvSmoothListBoxItemDragEndEvent = procedure(Sender: TObject; DragItemIndex: integer) of object;

  TAdvSmoothListBoxItemChanged = procedure(Sender: TObject; itemindex: integer) of object;

  TAdvSmoothListBoxItemDragOverEvent = procedure(Sender: TObject; DragItemIndex, DropItemIndex: integer) of object;

  TAdvSmoothListBoxItemDropDownSelectEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; ItemIndex: integer; Value: String) of object;

  TAdvSmoothListBoxItemDropDownEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; var Allow: Boolean) of object;

  TAdvSmoothListBoxItemGPRectEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; Text: String; var R: TGPRectF) of object;

  TAdvSmoothListBoxItemRectEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; Text: String; var R: TRect) of object;

  TAdvSmoothListBoxDragOver = procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean) of object;

  TAdvSmoothListBoxDragDrop = procedure(Sender, Source: TObject; X, Y: Integer) of object;

  TAdvSmoothListBoxCompare = procedure(Sender: TObject; Item1, Item2: TAdvSmoothListBoxItem; var CompareResult: Integer) of object;

  TAdvSmoothListBoxItemDeleteClicked = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; var Allow: Boolean) of object;

  TAdvSmoothListBoxFilterChangeEvent = procedure(Sender: TObject; var FilterText: String; var AllowFilter: Boolean) of object;

  TAdvSmoothListBoxFilterProcessedEvent = procedure(Sender: TObject; FilterText: String) of object;

  TAdvSmoothListBoxItemCustomizeFontEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; ACaptionFont, ANotesFont, AInfoFont: TFont) of object;

  TAdvSmoothListBoxItemCustomizeFillEvent = procedure(Sender: TObject; Item: TAdvSmoothListBoxItem; AFill, ADisabledFill, ASelectedFill: TGDIPFill) of object;

  TAdvSmoothListBoxSelectionMode = (sPersistSelection, sAutoDeselect, sPersistSelectionAlways);

  TDragMode = (dmDrag, dmRelease);

  TDropDownControlClass = class of TWinControl;

  TAdvSmoothListBoxDropTarget = class(TListBoxDropTarget)
  private
    FListBox: TAdvSmoothListBox;
  public
    constructor Create(AListBox: TAdvSmoothListBox);
    procedure DropText(pt:TPoint;s:string); override;
    procedure DropCol(pt:TPoint;Col: Integer); override;
    procedure DropRTF(pt:TPoint;s:string); override;
    procedure DropFiles(pt:TPoint;files:tstrings); override;
    procedure DropURL(pt:TPoint;s:string); override;
    procedure DragMouseMove(Source: TObject; pt:TPoint;var Allow: Boolean; DropFormats:TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TEditClass = class of TCustomEdit;
  TCustomEditProtected = class(TCustomEdit);

  TAdvSmoothListBoxFilter = class(TPersistent)
  private
    FEdit: TCustomEdit;
    FButton, FShowButton: TSpeedButton;
    FOwner: TAdvSmoothListBox;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FHeight: integer;
    FAutoFilter: Boolean;
    FCollapseButton: Boolean;
    FEnabled: Boolean;
    FTextHint: String;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetAutoFilter(const Value: Boolean);
    procedure SetCollapseButton(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetTextHint(const Value: String);
  public
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    constructor Create(AOwner: TAdvSmoothListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetHeight: integer;
    procedure UpdateFilter;
    procedure EditChanged(Sender: TObject);
    procedure ButtonClicked(Sender: TObject);
    procedure ShowButtonClicked(Sender: TObject);
    procedure ProcessFilter(Filter: String);
    procedure DrawArrow(g: TGPGraphics; R: TRect; Up: Boolean);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    property Edit: TCustomEdit read FEdit write FEdit;
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Height: integer read FHeight write SetHeight default 40;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default false;
    property AutoFilter: Boolean read FAutoFilter write SetAutoFilter default true;
    property CollapseButton: Boolean read FCollapseButton write SetCollapseButton default false;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property TextHint: String read FTextHint write SetTextHint;
  end;

  TMouseButtons = set of TMouseButton;

   {$IFDEF DELPHIXE2_LVL}
   [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
   {$ENDIF}
  TAdvSmoothListBox = class(TCustomControl, ITMSStyle, ITMSOfficeHint, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FDragCount: Integer;
    FMetroStyle: Boolean;
    FCreatedThroughClass: Boolean;
    fmb: TMouseButton;
    FEnableDetail: Boolean;
    FDragging: Boolean;
    keepoldcursor: Boolean;
    FOfficeHint: TAdvHintInfo;
    FoldCursor: TCursor;
    FOleDropTargetAssigned: Boolean;
    FListBoxDropTarget: TAdvSmoothListBoxDropTarget;
    FMouseDblClick: Boolean;
    FUpdateCount: integer;
    FDropDownItem: TAdvSmoothListBoxItem;
    FTimer, FDragCountTimer: TTimer;
    FDeactivating: Boolean;
    FBypassBitBlt : Boolean;
    FDrawingBitmap: TBitmap;
    FConstructed: boolean;
    FMouseSelect: TAdvSmoothListBoxMouseSelect;
    FTriggerButtons: TMouseButtons;
    FSelectedDragItem: TAdvSmoothListBoxItem;  
    FMode: TDragMode;
    FDragItemForm: TForm;
    FDragAnimateDelta, FDragDelta: integer;
    FDragClickY: integer;
    FDragOldTop: integer;    
    FPrevHoveredItemIndex, FhoveredItemIndex: integer;
    FTimerCount: integer;
    FDesignTime, FFocused: Boolean;
    FCurrentControl: TControl;
    FLookupKey: String;
    FAnimateBitmap: TBitmap;
    FClickX, FClickY: integer;
    FLookUp: Boolean;
    FLookUpSize: integer;
    FSp: Double;
    FTimeStart, FTimeStop: integer;
    FDetailShow, FAnimatingdetail, FAnimatingScroll,
    FMouseUp, FAnimating, FAnimate, FMouseDown: Boolean;
    FDetailIndex: integer; 
    FDragY, FScrollY, FDragX: integer;
    FSmoothTimer, FDragTimer: TTimer;
    FScPosTo: integer;
    FCurrentScPos: integer;
    FSelectedItemIndex, FFocusedItemIndex: integer;
    FItems: TAdvSmoothListBoxItems;
    FDisplayList: TAdvSmoothListBoxDisplayList;
    FItemAppearance: TAdvSmoothListBoxItemAppearance;
    FLookUpBar: TAdvSmoothListBoxLookUpBar;
    FSpeedFactor: integer;
    FSorted: Boolean;
    FSections: TAdvSmoothListBoxSections;
    FHeader: TAdvSmoothListBoxHeaderFooter;
    FScrollIndicator: TAdvSmoothListBoxIndicator;
    FFooter: TAdvSmoothListBoxHeaderFooter;
    FControl: TControl;
    FDetailStatus: TAdvSmoothListBoxDetailStatus;
    FOnItemDblClick: TAdvSmoothListBoxItemClickEvent;
    FOnItemClick: TAdvSmoothListBoxItemClickEvent;
    FKeyBoardLookup: Boolean;
    FSplitterHeight: integer;
    FOnScroll: TAdvSmoothListBoxScrollEvent;
    FOnShowDetail: TAdvSmoothListBoxDetailEvent;
    FOnHideDetail: TAdvSmoothListBoxDetailEvent;
    FOnItemDraw: TAdvSmoothListBoxItemDrawEvent;
    FOnItemText: TAdvSmoothListBoxItemTextEvent;
    FOnItemBkgDraw: TAdvSmoothListBoxItemBkgDrawEvent;
    FContainer: TGDIPPictureContainer;
    FOnAnchorClick: TAdvSmoothListBoxAnchorClickEvent;
    FShowDetailKey: TAdvSmoothListBoxShowDetailKey;
    FShowDetailClick: TAdvSmoothListBoxShowDetailClick;
    FFill: TGDIPFill;
    FShowFocus: Boolean;
    FFocusColor: TColor;
    FDefaultItems: TAdvSmoothListBoxItems;
    FDefaultItem: TAdvSmoothListBoxItem;
    FImages: TCustomImageList;
    FIsWinXP: boolean;
    FDetailItemImage: TAdvGDIPPicture;
    FItemImage: TAdvGDIPPicture;
    FOnItemRadioClick: TAdvSmoothListBoxItemCheckedEvent;
    FOnItemCheckClick: TAdvSmoothListBoxItemCheckedEvent;
    FOnItemImageClick: TAdvSmoothListBoxItemClickEvent;
    FOnItemCaptionClick: TAdvSmoothListBoxItemCaptionClick;
    FOnItemInfoClick: TAdvSmoothListBoxItemInfoClick;
    FOnItemAnchorClick: TAdvSmoothListBoxItemAnchorClickEvent;
    FSelectionMode: TAdvSmoothListBoxSelectionMode;
    FOnItemMouseLeave: TAdvSmoothListBoxItemMouseEvent;
    FOnItemMouseEnter: TAdvSmoothListBoxItemMouseEvent;
    FOnItemHint: TAdvSmoothListBoxItemHintEvent;
    FOnLookUpClick: TAdvSmoothListBoxLookUpClickEvent;
    FOnHeaderClick: TAdvSmoothListBoxHeaderClickEvent;
    FOnFooterClick: TAdvSmoothListBoxFooterClickEvent;
    FOnItemSelected: TAdvSmoothListBoxItemSelectedEvent;
    FOnItemSelectionChanged: TAdvSmoothListBoxItemSelectionChangedEvent;
    FDragAlphaBlend: Boolean;
    FDragOpacity: Byte;
    FDragBorderWidth: integer;
    FDragBorderColor: TColor;
    FOnItemDragStart: TAdvSmoothListBoxItemDragStartEvent;
    FOnItemDragEnd: TAdvSmoothListBoxItemDragEndEvent;
    FItemDragging: Boolean;
    FMultiSelect: Boolean;
    FOnItemDragDrop: TAdvSmoothListBoxItemDragDropEvent;
    FOnItemDragOver: TAdvSmoothListBoxItemDragOverEvent;
    FCategories: TAdvSmoothListBoxCategoryItems;
    FCategoryType: TAdvSmoothListBoxCategoryType;
    FOnItemButtonClick: TAdvSmoothListBoxItemClickEvent;
    FOnItemChanged: TAdvSmoothListBoxItemChanged;
    FGraphicClicked: TAdvSmoothListBoxGraphicClicked;
    FDetailSpeedFactor: integer;
    FDropDownForm: TForm;
    FDropDownControlClass: TDropDownControlClass;
    FOnItemDropDownShow: TAdvSmoothListBoxItemDropDownEvent;
    FOnItemDropDownHide: TAdvSmoothListBoxItemDropDownEvent;
    FOnItemDropDownSelect: TAdvSmoothListBoxItemDropDownSelectEvent;
    FOnItemNotesRect: TAdvSmoothListBoxItemRectEvent;
    FOnItemCaptionRect: TAdvSmoothListBoxItemGPRectEvent;
    FOnItemInfoRect: TAdvSmoothListBoxItemGPRectEvent;
    FOnDragDrop: TAdvSmoothListBoxDragDrop;
    FOnDragOver: TAdvSmoothListBoxDragOver;
    FOleDragDrop: Boolean;
    FSetDisplayItemValues: Boolean;
    FTextRendering: TAdvSmoothListBoxTextRendering;
    FEnableDragging: Boolean;
    FOnCompare: TAdvSmoothListBoxCompare;
    FFilter: TAdvSmoothListBoxFilter;
    FOnItemDeleteClicked: TAdvSmoothListBoxItemDeleteClicked;
    FOnFilterChange: TAdvSmoothListBoxFilterChangeEvent;
    FOnItemCustomizeFont: TAdvSmoothListBoxItemCustomizeFontEvent;
    FOnItemCustomizeFill: TAdvSmoothListBoxItemCustomizeFillEvent;
    FOnGraphicLeftClick: TAdvSmoothListBoxItemClickEvent;
    FOnGraphicRightClick: TAdvSmoothListBoxItemClickEvent;
    FBlockMouseMovement: Boolean;
    FClickMargin: Integer;
    FDeleteMargin: Integer;
    FDragMargin: Integer;
    FLayout: TAdvSmoothListBoxLayout;
    FLayoutIndenting: Integer;
    FOnFilterProcessed: TAdvSmoothListBoxFilterProcessedEvent;
    FOnSectionClick: TAdvSmoothListBoxSectionClickEvent;
    FOnSectionHeight: TAdvSmoothListBoxSectionHeight;
    FOnSectionDraw: TAdvSmoothListBoxSectionDrawEvent;
    FOnSectionDrawContent: TAdvSmoothListBoxSectionDrawEvent;
    FComboUse: boolean;
    FOnItemButtonLeftClick: TAdvSmoothListBoxItemClickEvent;
    FOnItemButtonRightClick: TAdvSmoothListBoxItemClickEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    function GetVersion: string;
    procedure SetItems(const Value: TAdvSmoothListBoxItems);
    procedure SetItemAppearance(const Value: TAdvSmoothListBoxItemAppearance);
    procedure SetLookUpBar(const Value: TAdvSmoothListBoxLookUpBar);
    procedure SetSpeedFactor(const Value: integer);
    procedure SetSorted(const Value: Boolean);
    procedure SetSections(const Value: TAdvSmoothListBoxSections);
    procedure SetHeader(const Value: TAdvSmoothListBoxHeaderFooter);
    procedure SetScrollIndicator(const Value: TAdvSmoothListBoxIndicator);
    procedure SetFooter(const Value: TAdvSmoothListBoxHeaderFooter);
    procedure SetControl(const Value: TControl);
    procedure SetKeyBoardLookup(const Value: Boolean);
    procedure SetSplitterHeight(const Value: integer);
    procedure SetShowDetailClick(const Value: TAdvSmoothListBoxShowDetailClick);
    procedure SetShowDetailKey(const Value: TAdvSmoothListBoxShowDetailKey);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetFocusColor(const Value: TColor);
    procedure SetDefaultItem(const Value: TAdvSmoothListBoxItem);
    procedure SetDetailItemImage(const Value: TAdvGDIPPicture);
    procedure SetItemImage(const Value: TAdvGDIPPicture);
    function GetSelectedItemIndex: integer;
    procedure SetSelectedItemIndex(const Value: integer);
    procedure SetDragAlphaBlend(const Value: Boolean);
    procedure SetDragOpacity(const Value: Byte);
    procedure SetDragBorderColor(const Value: TColor);
    procedure SetDragBorderWidth(const Value: integer);
    procedure SetItemDragging(const Value: Boolean);
    procedure SetCategories(const Value: TAdvSmoothListBoxCategoryItems);
    procedure SetCategoryType(const Value: TAdvSmoothListBoxCategoryType);
    function GetNewCanvas: TCanvas;
    procedure SetDetailSpeedFactor(const Value: integer);
    procedure SetDropDownControlClass(const Value: TDropDownControlClass);
    function GetParentEx: TWinControl;
    procedure SetParentEx(const Value: TWinControl);
    procedure HideParent;
    procedure SetOleDragDrop(const Value: Boolean);
    function GetCursorEx: TCursor;
    procedure SetCursorEx(const Value: TCursor);
    function GetItemCount: Integer;
    procedure SetItemCount(const Value: Integer);
    procedure SetTextRendering(const Value: TAdvSmoothListBoxTextRendering);
    procedure SetEnableDragging(const Value: Boolean);
    procedure SetFilter(const Value: TAdvSmoothListBoxFilter);
    procedure SetScPosTo(const Value: Integer);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetLayout(const Value: TAdvSmoothListBoxLayout);
    procedure SetLayoutIndenting(const Value: Integer);
    procedure SetMouseSelect(const Value: TAdvSmoothListBoxMouseSelect);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    property BlockMouseMovement: Boolean read FBlockMouseMovement write FBlockMouseMovement;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var params: TCreateParams); override;
    procedure CategoriesChanged(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure DropDownFormDeactivate(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure DragCountTimerEvent(Sender: TObject);
    procedure ScrollIndicatorChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure LookupBarChanged(Sender: TObject);
    function GetFilterEditClass: TEditClass; virtual;
    procedure CustomizeFilterEdit(AEdit: TCustomEdit); virtual;
    procedure HeaderFooterChanged(Sender: TObject); virtual;
    procedure SetSelectionMode(const Value: TAdvSmoothListBoxSelectionMode); virtual;
    procedure ItemAppearanceChanged(Sender: TObject); virtual;
    procedure SectionsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoItemDropDownSelect(Sender: TObject; Item: TAdvSmoothListBoxItem; ItemIndex: integer; Value: String);
    procedure DoItemDblClick(Sender: TObject; itemindex: integer);
    procedure DoItemClick(Sender: TObject; itemindex: integer); virtual;
    procedure DoSectionDraw(Sender: TObject; ACanvas: TCanvas; Rect: TRect; Caption: String; CategoryID: Integer; var DefaultDraw: Boolean); virtual;
    procedure DoSectionDrawContent(Sender: TObject; ACanvas: TCanvas; Rect: TRect; Caption: String; CategoryID: Integer; var DefaultDraw: Boolean); virtual;
    procedure DoSectionHeight(Sender: TObject; Caption: String; CategoryID: Integer; var AHeight: Integer); virtual;
    procedure DoSectionClick(Sender: TObject; Caption: String; CategoryID: Integer); virtual;
    procedure DoSelectItem(NewItemIndex: Integer); virtual;
    procedure DoItemImageClick(Sender: TObject; itemindex: integer; graphicleft: Boolean);
    procedure DoItemCheckClick(Sender: TObject; itemindex: integer; checked: Boolean);
    procedure DoItemRadioClick(Sender: TObject; itemindex: integer; checked: Boolean);
    procedure DoItemButtonClick(Sender: TObject; itemindex: integer);
    procedure DoItemButtonLeftClick(Sender: TObject; itemindex: integer);
    procedure DoItemButtonRightClick(Sender: TObject; itemindex: integer);
    procedure DoItemButtonDropDownClick(Sender: TObject; itemindex: integer);
    procedure DoScroll(Sender: TObject; CurrentPosition, EndPosition: Double); virtual;
    procedure DoSmoothScroll(CurrentPosition, EndPosition: Double); virtual;
    procedure DoBoolPropertyChange(Item: TAdvSmoothListBoxItem; PropID: Integer; var Value: Boolean); virtual;
    procedure DoInternalScroll; virtual;
    procedure DoLookup(DispItem: TAdvSmoothListBoxDisplayListItem); virtual;
    procedure DoHideDetail(Sender: TObject; itemindex: integer);
    procedure DoShowDetail(Sender: TObject; itemindex: integer);
    function LookupBarVisible: Boolean; virtual;
    function DoItemCheckToggle(Item: TAdvSmoothListBoxItem; GraphicLeft: Boolean; var Checked: Boolean): Boolean; virtual;
    procedure DoItemText(Sender: TObject; itemindex: integer; var itemcaption: String; var iteminfo: String; var itemnotes: String); virtual;
    procedure DoItemGraphics(Sender: TObject; itemindex: integer); virtual;
    procedure DoItemBkgDraw(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemrect: TRect; var defaultdraw: boolean);
    procedure DoItemDraw(Sender: TObject; Canvas: TCanvas; itemindex: integer; itemrect: TRect; var defaultdraw: boolean);
    procedure DoAnchorClick(Sender: TObject; Anchor: String);
    procedure DoItemAnchorClick(Sender: TObject; Anchor: String; ItemIndex: integer);
    procedure DoItemCaptionClick(Sender: TObject; itemindex: integer);
    procedure DoItemInfoClick(Sender: TObject; itemindex: integer); 
    procedure DrawBackground;
    procedure DrawFilter;
    procedure DrawHeaderFooter(Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean);
    procedure DrawItems;
    procedure DrawLookUpBar;
    procedure DrawScrollIndicator;
    procedure DrawSection(itemRect: TRect; ch: String; catindex: integer);
    procedure Changed;
    procedure CalculateRectTopToBottom;
    procedure CalculateRect(Item: TAdvSmoothListBoxItem);
    procedure CalculateRects(AUpdate: Boolean = False);
    procedure DropDown(Item: TAdvSmoothListBoxItem);
    procedure Click; override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure DblClick; override;
    procedure Resize; override;
    procedure AnimateSmoothPos(Sender: TObject);
    procedure DragSmoothPos(Sender: TObject);
    procedure InitDisplayList(AItemAutoSize: Boolean = False);
    procedure InitPreview; virtual;
    procedure AddDisplaySection(Item: TAdvSmoothListBoxItem; var prevrect: TRect; ht, lookup: integer);
    procedure AddDisplayItem(Item: TAdvSmoothListBoxItem; var prevrect: TRect; ht, lookup: integer);
    procedure GetTextPosition(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothListBoxLocation);
    procedure DoDragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Source: TObject; X, Y: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetVersionNr: Integer; virtual;
    function GetShadowOffset: integer;
    function GetHeaderFooterCaptionRect(Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean): TRect;
    function GetHeaderFooterRect(Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean): TRect;
    function ItemFromDifferentCategory(item1, item2: TAdvSmoothListBoxItem): Boolean; virtual;
    function FindFirstItemWithChar(ch: String): TAdvSmoothListBoxDisplayListItem;
    function FindFirstSectionWithChar(ch: String): TAdvSmoothListBoxDisplayListItem;
    function FindFirstItemWithCategoryID(CategoryID: integer): TAdvSmoothListBoxDisplayListItem;
    function FindFirstSectionWithCategoryID(CategoryID: integer): TAdvSmoothListBoxDisplayListItem;
    function InsideRect: TRect;
    function GetFullHeight: integer; virtual;
    function GetFullWidth: integer; virtual;
    function GetDisplayRect: TRect;
    function GetMaximumCustomTextWidth(ACanvas: TCanvas): integer;
    function GetMaximumCustomTextHeight(ACanvas: TCanvas): integer;
    function GetDisplayIndex(ItemIndex: integer): integer;
    function GetPositionTo: integer;
    function GetPosition: integer;
    function PtInGPRect(R: TGPRectF; pt: TPoint): Boolean;
    procedure SetMultiSelect(const Value: Boolean); virtual;
    function GetVisibleItemCount: Integer;
    function GetItemHeight(AItem: TAdvSmoothListBoxItem;AItemAutoSize: Boolean = False): Integer;
    procedure SetTopIndex(ItemIndex: Integer);
    procedure SetDirectSelectedItemIndex(Value: Integer); virtual;
    property DisplayList: TAdvSmoothListBoxDisplayList read FDisplayList;
    property ItemCount: Integer read GetItemCount write SetItemCount;
    property Animating: Boolean read FAnimating;
    property SetDisplayItemValues: Boolean read FSetDisplayItemValues write FSetDisplayItemValues default False;
    function GetVisibleItemCountByCategory(): Integer;
    procedure GetOfficeHint(PT: TPoint; var HintInfo: TAdvHintInfo); virtual;
    property ComboUse: boolean read FComboUse write FComboUse;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property ClickMargin: Integer read FClickMargin write FClickMargin;
    property DragMargin: Integer read FDragMargin write FDragMargin;
    property DeleteMargin: Integer read FDeleteMargin write FDeleteMargin;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property DropDownControlClass: TDropDownControlClass read FDropDownControlClass write SetDropDownControlClass;
    procedure MouseWheelHandler(var Message: TMessage); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ShowDetails(itemindex: integer = -1; const bypasstimer: Boolean = false);
    procedure HideDetails;
    property DetailStatus: TAdvSmoothListBoxDetailStatus read FDetailStatus;
    function YToItem(X, Y: integer; CountSections: Boolean = false; CheckDisplayRectangle: Boolean = true): integer; virtual;
    function YToDeleteButton(X, Y: integer): integer; virtual;
    function ItemAtXY(X, Y: integer): integer; virtual;
    function SectionAtXY(X, Y: integer): TAdvSmoothListBoxDisplayListItem; virtual;
    function DeleteButtonAtXY(X, Y: integer): integer;
    procedure DoFilterChange(Sender: TObject; var FilterText: String; var AllowFilter: Boolean);
    function GetTopIndex: integer;
    function GetBottomIndex: integer;
    procedure ScrollToItem(ItemIndex: integer);
    procedure ScrollInView(ItemIndex: integer);
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones); virtual;
    procedure SetProgressStyle(AStyle: TTMSStyle; Selected: Boolean);
    //ADVSMOOTHCOMBOBOX
    procedure InitState;
    procedure InitSelection(itemindex: integer);
    function CheckSelection(X, Y: integer): Boolean;
    //
    property GraphicClicked: TAdvSmoothListBoxGraphicClicked read FGraphicClicked;
    property Canvas: TCanvas read GetNewCanvas;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeId: String;
    procedure ExpandAll;
    procedure CollapseAll;
    procedure ApplyFilter(AFilter: String);
    property CurrentScrollPosition: Integer read FCurrentScPos write FCurrentScPos;
    property NextScrollPosition: Integer read FScPosTo write SetScPosTo;
    property Parent: TWinControl read GetParentEx write SetParentEx;
  published
    property Cursor: TCursor read GetCursorEx write SetCursorEx;
    property Version: String read GetVersion;
    property Fill: TGDIPFill read FFill write SetFill;
    property Images: TCustomImageList read FImages write FImages;
    property Items: TAdvSmoothListBoxItems read FItems write SetItems;
    property ItemAppearance: TAdvSmoothListBoxItemAppearance read FItemAppearance write SetItemAppearance;
    property SplitterHeight: integer read FSplitterHeight write SetSplitterHeight default 20;
    property LookupBar: TAdvSmoothListBoxLookUpBar read FLookUpBar write SetLookUpBar;
    property SpeedFactor: integer read FSpeedFactor write SetSpeedFactor default 4;
    property DetailSpeedFactor: integer read FDetailSpeedFactor write SetDetailSpeedFactor default 4;
    property Sorted: Boolean read FSorted write SetSorted default false;
    property Sections: TAdvSmoothListBoxSections read FSections write SetSections;
    property SelectedItemIndex: integer read GetSelectedItemIndex write SetSelectedItemIndex;
    property Header: TAdvSmoothListBoxHeaderFooter read FHeader write SetHeader;
    property Filter: TAdvSmoothListBoxFilter read FFilter write SetFilter;
    property Footer: TAdvSmoothListBoxHeaderFooter read FFooter write SetFooter;
    property ScrollIndicator: TAdvSmoothListBoxIndicator read FScrollIndicator write SetScrollIndicator;
    property DetailControl: TControl read FControl write SetControl;
    property KeyBoardLookup: Boolean read FKeyBoardLookup write SetKeyBoardLookup default false;
    property MouseSelect: TAdvSmoothListBoxMouseSelect read FMouseSelect write SetMouseSelect default lmsLeft;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property ShowDetailClick: TAdvSmoothListBoxShowDetailClick read FShowDetailClick write SetShowDetailClick default sdOnClick;
    property ShowDetailKey: TAdvSmoothListBoxShowDetailKey read FShowDetailKey write SetShowDetailKey default dkSpace;
    property OnDragOver: TAdvSmoothListBoxDragOver read FOnDragOver write FOnDragOver;
    property OnDragDrop: TAdvSmoothListBoxDragDrop read FOnDragDrop write FOnDragDrop;
    property OnCompare: TAdvSmoothListBoxCompare read FOnCompare write FOnCompare;
    property OnItemDropDownSelect: TAdvSmoothListBoxItemDropDownSelectEvent read FOnItemDropDownSelect write FOnItemDropDownSelect;
    property OnItemDropDownHide: TAdvSmoothListBoxItemDropDownEvent read FOnItemDropDownHide write FOnItemDropDownHide;
    property OnItemDropDownShow: TAdvSmoothListBoxItemDropDownEvent read FOnItemDropDownShow write FOnItemDropDownShow;
    property OnHeaderClick: TAdvSmoothListBoxHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnFooterClick: TAdvSmoothListBoxFooterClickEvent read FOnFooterClick write FOnFooterClick;
    property OnItemDragStart: TAdvSmoothListBoxItemDragStartEvent read FOnItemDragStart write FOnItemDragStart;
    property OnItemDragEnd: TAdvSmoothListBoxItemDragEndEvent read FOnItemDragEnd write FOnItemDragEnd;
    property OnItemDragOver: TAdvSmoothListBoxItemDragOverEvent read FOnItemDragOver write FOnItemDragOver;
    property OnItemDragDrop: TAdvSmoothListBoxItemDragDropEvent read FOnItemDragDrop write FOnItemDragDrop;
    property OnItemMouseLeave: TAdvSmoothListBoxItemMouseEvent read FOnItemMouseLeave write FOnItemMouseLeave;
    property OnItemMouseEnter: TAdvSmoothListBoxItemMouseEvent read FOnItemMouseEnter write FOnItemMouseEnter;
    property OnLookUpClick: TAdvSmoothListBoxLookUpClickEvent read FOnLookUpClick write FOnLookUpClick;
    property OnItemHint: TAdvSmoothListBoxItemHintEvent read FOnItemHint write FOnItemHint;
    property OnItemDblClick: TAdvSmoothListBoxItemClickEvent read FOnItemDblClick write FOnItemDblClick;
    property OnItemClick: TAdvSmoothListBoxItemClickEvent read FOnItemClick write FOnItemClick;
    property OnSectionClick: TAdvSmoothListBoxSectionClickEvent read FOnSectionClick write FOnSectionClick;
    property OnSectionHeight: TAdvSmoothListBoxSectionHeight read FOnSectionHeight write FOnSectionHeight;
    property OnFilterChange: TAdvSmoothListBoxFilterChangeEvent read FOnFilterChange write FOnFilterChange;
    property OnFilterProcessed: TAdvSmoothListBoxFilterProcessedEvent read FOnFilterProcessed write FOnFilterProcessed;
    property OnItemSelectionChanged: TAdvSmoothListBoxItemSelectionChangedEvent read FOnItemSelectionChanged write FOnItemSelectionChanged;
    property OnItemSelected: TAdvSmoothListBoxItemSelectedEvent read FOnItemSelected write FOnItemSelected;
    property OnItemCheckClick: TAdvSmoothListBoxItemCheckedEvent read FOnItemCheckClick write FOnItemCheckClick;
    property OnItemRadioClick: TAdvSmoothListBoxItemCheckedEvent read FOnItemRadioClick write FOnItemRadioClick;
    property OnItemImageClick: TAdvSmoothListBoxItemClickEvent read FOnItemImageClick write FOnItemImageClick;
    property OnGraphicLeftClick: TAdvSmoothListBoxItemClickEvent read FOnGraphicLeftClick write FOnGraphicLeftClick;
    property OnGraphicRightClick: TAdvSmoothListBoxItemClickEvent read FOnGraphicRightClick write FOnGraphicRightClick;
    property OnItemButtonClick: TAdvSmoothListBoxItemClickEvent read FOnItemButtonClick write FOnItemButtonClick;
    property OnItemButtonLeftClick: TAdvSmoothListBoxItemClickEvent read FOnItemButtonLeftClick write FOnItemButtonLeftClick;
    property OnItemButtonRightClick: TAdvSmoothListBoxItemClickEvent read FOnItemButtonRightClick write FOnItemButtonRightClick;
    property OnItemCaptionRect: TAdvSmoothListBoxItemGPRectEvent read FOnItemCaptionRect write FOnItemCaptionRect;
    property OnItemCustomizeFont: TAdvSmoothListBoxItemCustomizeFontEvent read FOnItemCustomizeFont write FOnItemCustomizeFont;
    property OnItemCustomizeFill: TAdvSmoothListBoxItemCustomizeFillEvent read FOnItemCustomizeFill write FOnItemCustomizeFill;
    property OnItemInfoRect: TAdvSmoothListBoxItemGPRectEvent read FOnItemInfoRect write FOnItemInfoRect;
    property OnItemNotesRect: TAdvSmoothListBoxItemRectEvent read FOnItemNotesRect write FOnItemNotesRect;
    property OnItemDeleteClicked: TAdvSmoothListBoxItemDeleteClicked read FOnItemDeleteClicked write FOnItemDeleteClicked;
    property OnScroll: TAdvSmoothListBoxScrollEvent read FOnScroll write FOnScroll;
    property OnHideDetail: TAdvSmoothListBoxDetailEvent read FOnHideDetail write FOnHideDetail;
    property OnShowDetail: TAdvSmoothListBoxDetailEvent read FOnShowDetail write FOnShowDetail;
    property OnItemDraw: TAdvSmoothListBoxItemDrawEvent read FOnItemDraw write FOnItemDraw;
    property OnItemBkgDraw: TAdvSmoothListBoxItemBkgDrawEvent read FOnItemBkgDraw write FOnItemBkgDraw;
    property OnItemText: TAdvSmoothListBoxItemTextEvent read FOnItemText write FOnItemText;
    property OnSectionDraw: TAdvSmoothListBoxSectionDrawEvent read FOnSectionDraw write FOnSectionDraw;
    property OnSectionDrawContent: TAdvSmoothListBoxSectionDrawEvent read FOnSectionDrawContent write FOnSectionDrawContent;
    property OnAnchorClick: TAdvSmoothListBoxAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnItemAnchorClick: TAdvSmoothListBoxItemAnchorClickEvent read FOnItemAnchorClick write FOnItemAnchorClick;
    property OnItemCaptionClick: TAdvSmoothListBoxItemCaptionClick read FOnItemCaptionClick write FOnItemCaptionClick;
    property OnItemInfoClick: TAdvSmoothListBoxItemInfoClick read FOnItemInfoClick write FOnItemInfoClick;
    property OnItemChanged: TAdvSmoothListBoxItemChanged read FOnItemChanged write FOnItemChanged;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    property DragOpacity: Byte read FDragOpacity write SetDragOpacity default 200;
    property DragAlphaBlend: Boolean read FDragAlphaBlend write SetDragAlphaBlend default true;
    property DragBorderWidth: integer read FDragBorderWidth write SetDragBorderWidth default 1; 
    property DragBorderColor: TColor read FDragBorderColor write SetDragBorderColor default clBlack;
    property DefaultItem: TAdvSmoothListBoxItem read FDefaultItem write SetDefaultItem;
    property ItemImage: TAdvGDIPPicture read FItemImage write SetItemImage;
    property DetailItemImage: TAdvGDIPPicture read FDetailItemImage write SetDetailItemImage;
    property SelectionMode: TAdvSmoothListBoxSelectionMode read FSelectionMode write SetSelectionMode default sAutoDeselect;
    property ItemDragging: Boolean read FItemDragging write SetItemDragging default true;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property CategoryType: TAdvSmoothListBoxCategoryType read FCategoryType write SetCategoryType default alphanumeric;
    property Layout: TAdvSmoothListBoxLayout read FLayout write SetLayout default lblNormal;
    property LayoutIndenting: Integer read FLayoutIndenting write SetLayoutIndenting default 60;
    property Categories: TAdvSmoothListBoxCategoryItems read FCategories write SetCategories;
    property OleDragDrop: Boolean read FOleDragDrop write SetOleDragDrop default false;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property TextRendering: TAdvSmoothListBoxTextRendering read FTextRendering write SetTextRendering default tClearType;
    property EnableDragging: Boolean read FEnableDragging write SetEnableDragging default true;

    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragMode;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnEndDrag;   
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

uses
  CommCtrl, ShellApi;


{$I GDIPHTMLEngine.pas}

type
  {$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    {$IFDEF DELPHIXE3_LVL}
    FItems: TList<TCollectionItem>;
    {$ELSE}
    FItems: TList;
    {$ENDIF}
  end;
  {$HINTS ON}

{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer; var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}

procedure GetCaptionPosition(var x, y: integer; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothListBoxCaptionLocation);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    cpTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    cpTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    cpBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    cpBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    cpTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    cpBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    cpCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    cpCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    cpCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;
end;

procedure DrawPicture(g: TGPGraphics; picture: TAdvGDIPPicture; location: TFillPicturePosition; r: TRect; x, y: integer);
var
  w, h: integer;
begin
  if not picture.Empty then
  begin
    picture.GetImageSizes;
    w := picture.Width;
    h := picture.Height;
    case location of
    ppTopLeft: picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
    ppTopCenter:
    begin
      r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
      picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
    end;
    ppTopRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Top, r.Right, r.Top + h));
    ppBottomLeft: picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
    ppBottomCenter:
    begin
      r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
      picture.GDIPDraw(g, Rect(r.Left, r.Bottom - h, r.Left + w, r.Bottom));
    end;
    ppBottomRight: picture.GDIPDraw(g, Rect(r.Right - w, r.Bottom - h, r.Right, r.Bottom));
    ppStretched: picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Right, r.Bottom));
    ppCustom: picture.GDIPDraw(g, Bounds(x, y, w, h));
    ppCenterLeft:
      begin
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      end;
    ppCenterRight:
      begin
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Right - w,r.Top,r.Right, r.Top + h));
      end;
    ppCenterCenter:
      begin
        r.Left := r.Left + ((r.Right - r.Left) - w) div 2;
        r.Top := r.Top + ((r.Bottom - r.Top) - h) div 2;
        picture.GDIPDraw(g, Rect(r.Left,r.Top,r.Left + w, r.Top + h));
      end;
    end;
  end;
end;

function AnimateDouble(var Start, Stop: integer; Delta: Double; Margin: integer): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

function TAdvSmoothListBox.GetBottomIndex: integer;
var
  res: integer;
begin
  res := YToItem((InsideRect.Right - InsideRect.Left) div 2, GetDisplayRect.Bottom, false, false);
  if res = -1 then
    result := FDisplayList.Count - 1
  else
    result := res;
end;

function TAdvSmoothListBox.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothListBox.GetCursorEx: TCursor;
begin
  Result := inherited Cursor;
end;

function TAdvSmoothListBox.GetDisplayIndex(ItemIndex: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    if FDisplayList.GetItem(i).DisplayItem <> nil  then
    begin
      if FDisplayList.GetItem(i).DisplayItem.Index = itemindex then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.GetDisplayRect: TRect;
begin
  case LookupBar.Position of
    pLeft:   Result := Bounds(InsideRect.Left + LookupBar.GetWidth, InsideRect.Top + Header.GetHeight + Filter.GetHeight, InsideRect.Right - InsideRect.Left - GetShadowOffset,
    InsideRect.Bottom - InsideRect.Top - Footer.GetHeight - Header.GetHeight - Filter.GetHeight - GetShadowOffset);
    pRight:   Result := Bounds(InsideRect.Left, InsideRect.Top + Header.GetHeight  + Filter.GetHeight, InsideRect.Right - LookupBar.GetWidth - InsideRect.Left - GetShadowOffset,
    InsideRect.Bottom - InsideRect.Top - Footer.GetHeight - Header.GetHeight - GetShadowOffset - Filter.GetHeight);
  end;
end;

function TAdvSmoothListBox.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TAdvSmoothListBox.GetItemHeight(AItem: TAdvSmoothListBoxItem; AItemAutoSize: Boolean = False): Integer;
begin
  Result := Itemappearance.Height;
  if not AItemAutoSize then
    Exit;

  if Assigned(AItem) then
  begin
    if AItem.AutoSize then
      Result := Round(Max(AItem.Infor.Height + 5, AItem.Captionr.Height + 5) + (AItem.NotesR.Bottom - AItem.NotesR.Top))
    else
    begin
      if AItem.Height <> -1 then
        Result :=  AItem.Height
    end
  end;
end;

procedure TAdvSmoothListBox.SetItemCount(const Value: Integer);
begin
 // if (Value <> ItemCount) then
  begin
    Items.BeginUpdate;
    if (Value > ItemCount) then
    begin
      while (Items.Count < Value) do
        Items.Add;
    end
    else // Value < ItemCount
    begin
      while (Items.Count > Value) do
        Items.Delete(Items.Count - 1);
    end;
    Items.EndUpdate;
  end;
end;

function TAdvSmoothListBox.GetVisibleItemCount: Integer;
var
  h: Integer;
begin
  Result := ItemCount;
  if Result > 0 then
  begin
    if (Self.Height > 0) then
    begin
      h := ((Self.Height - Header.GetHeight - Footer.GetHeight - GetShadowOffset - Filter.GetHeight));
      Result := Max(1, h div GetItemHeight(nil));
    end;
  end;
end;

function TAdvSmoothListBox.GetVisibleItemCountByCategory: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
//    if Items[I].Visible and  then

  end;
end;

procedure TAdvSmoothListBox.SetTextRendering(
  const Value: TAdvSmoothListBoxTextRendering);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetTopIndex(ItemIndex: Integer);
var
  I, sh: Integer;
begin
  if (ItemIndex >= 0) then
  begin
    sh := 0;
    if Sections.Visible then
      sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;
    for I := 0 to FDisplayList.Count - 1 do
    begin
      with FDisplayList.GetItem(i) do
      begin
        if DisplayItem <> nil then
        begin
          if DisplayItem.Index = ItemIndex then
          begin
            {
            if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
              FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
            else if ItemRect.Top < GetPosition + Header.GetHeight then
              FScPosTo := ItemRect.Top - Header.GetHeight - sh;
            }
            FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;
            
            if FScPosTo <> FCurrentScPos then
            begin
              FCurrentScPos := FScPosTo;
              ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
              FAnimate := true;
            end;
          end;
        end;
      end;
    end;
    Changed;
  end;
end;

function TAdvSmoothListBox.GetFullHeight: integer;
begin
  result := 0;
  if FDisplayList.Count > 0 then
    Result := FDisplayList.GetItem(FDisplayList.Count - 1).ItemRect.Top + GetItemHeight(FDisplayList.GetItem(FDisplayList.Count - 1).FItem, True);

  Result := Result - Height + Footer.GetHeight + GetShadowOffset;
end;

function TAdvSmoothListBox.GetFullWidth: integer;
begin
  Result := 0;
end;

function TAdvSmoothListBox.GetHeaderFooterCaptionRect(
  Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean): TRect;
var
  g: TGPGraphics;
  r, htmlr: TRect;
  fillr: TGPRectF;
  a, s, k: String;
  XSize, Ysize: integer;
  l, m: integer;
  hr: TRect;
  x, y: integer;
  rc: TRect;
begin
  Result := Bounds(0, 0, 0, 0);
  with Part do
  begin
    if Visible then
    begin
      g := TGPGraphics.Create(Canvas.Handle);

      rc := InsideRect;

      if header then
        r := Bounds(rc.Left, rc.Top, rc.Right - rc.Left - GetShadowOffset, Height)
      else
        r := Bounds(rc.Left, rc.Bottom - Height - GetShadowOffset, rc.Right - rc.Left - GetShadowOffset, Height);

      fillr := MakeRect(R.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);

      if Caption <> '' then
      begin
        htmlr := Rect(0, 0, 10000, 10000);

        HTMLDrawGDIP(g, FFont, Caption,htmlr,FImages, 0,0,-1,-1,CaptionShadowOffset,False,true,false,false,
          False,False,true,1.0,CaptionURLColor,clNone,clNone,CaptionShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);

        if CaptionLocation <> cpCustom then
          GetCaptionPosition(x, y, fillr, XSize, YSize, CaptionLocation)
        else
        begin
          x := CaptionLeft;
          y := CaptionTop;
        end;

        Result := Bounds(Round(fillr.X + x), Round(fillr.Y + y), xsize, ysize);

      end;
      g.Free;
    end;
  end;
end;

function TAdvSmoothListBox.GetHeaderFooterRect(Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean): TRect;
var
  rc: TRect;
begin
  Result := Bounds(0, 0, 0, 0);
  with Part do
  begin
    if Visible then
    begin
      rc := InsideRect;
      if header then
        Result := Bounds(rc.Left, rc.Top, rc.Right - rc.Left - GetShadowOffset, Height)
      else
        Result := Bounds(rc.Left, rc.Bottom - Height - GetShadowOffset, rc.Right - rc.Left - GetShadowOffset, Height);
    end;
  end;
end;

function TAdvSmoothListBox.GetMaximumCustomTextHeight(
  ACanvas: TCanvas): integer;
var
  I: integer;
  tempw: integer;
  tw: integer;
  s: String;
begin
  tempw := 0;
  for I := 0 to Categories.Count - 1 do
  begin
    with Categories[I] do
    begin
      if LookupText <> '' then
        s := LookupText
      else
        s := Text;

      tw := ACanvas.TextWidth(s);
      if tw > tempw then
        tempw := tw
    end;
  end;

  if tempw > 0 then
    tempw := tempw + 4;

  result := tempw;
end;

function TAdvSmoothListBox.GetMaximumCustomTextWidth(ACanvas: TCanvas): integer;
var
  I: integer;
  tempw: integer;
  tw: integer;
  s: string;
begin
  tempw := 0;
  for I := 0 to Categories.Count - 1 do
  begin
    with Categories[I] do
    begin
      if LookupText <> '' then
        s := LookupText
      else
        s := Text;

      if LookupBar.Rotated then
      begin
        tw := ACanvas.TextHeight(s);
      end
      else
      begin
        if Assigned(FImages) then
        begin
          if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
            tw := ACanvas.TextWidth(s) + FImages.Width
          else
            tw := ACanvas.TextWidth(s);
        end
        else
          tw := ACanvas.TextWidth(s);
      end;

      if tw > tempw then
        tempw := tw
    end;
  end;

  if tempw > 0 then
    tempw := tempw + 4;
    
  result := tempw;
end;

function TAdvSmoothListBox.GetNewCanvas: TCanvas;
begin
  if FBypassBitBlt then
  begin
    if not Assigned(FDrawingBitmap) then
      FDrawingBitmap:= TBitmap.Create;

    if Width <> FDrawingBitmap.Width then
      FDrawingBitmap.Width:= Width;

    if Height <> FDrawingBitmap.Height then
      FDrawingBitmap.Height:= Height;

    Result:= FDrawingBitmap.Canvas;
  end
  else
    Result:= inherited Canvas;
end;

procedure TAdvSmoothListBox.GetOfficeHint(PT: TPoint;
  var HintInfo: TAdvHintInfo);
var
  item: integer;
  ditem: TAdvSmoothListBoxItem;
begin
  item := YToItem(PT.X, PT.Y);
  if (item <> -1) then
  begin
    dItem := FDisplayList.GetItem(item).DisplayItem;
    if Assigned(dItem) then
    begin
      if (dItem.Hint = '') and dItem.OfficeHint.IsEmpty then
        HintInfo := OfficeHint
      else
        HintInfo := dItem.OfficeHint;
    end;
  end
  else
    HintInfo := OfficeHint;
end;

function TAdvSmoothListBox.GetParentEx: TWinControl;
begin
  Result := inherited Parent;
end;

function TAdvSmoothListBox.GetPosition: integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FCurrentScPos <> FScPosTo then
    begin
      if FCurrentScPos < -50 then
        FCurrentScPos := -50

      else if FCurrentScPos > GetFullHeight + 50 then
        FCurrentScPos := GetFullHeight + 50;
    end;

    Result := FCurrentScPos;
  end
  else
    Result := 0;

  if GetFullHeight + Height < Height then
    result := 0;
end;

function TAdvSmoothListBox.GetPositionTo: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FScPosTo < 0 then
      FscposTo := 0

    else if FScPosTo > GetFullHeight then
      FscposTo := GetFullHeight;

    result := FScPosTo;
  end
  else
    Result := 0;
end;

function TAdvSmoothListBox.GetSelectedItemIndex: integer;
begin
  if Items.SelectedItem <> nil then
    Result := Items.SelectedItem.Index
  else
    Result := -1;
end;

function TAdvSmoothListBox.GetShadowOffset: integer;
begin
  Result := 0;
  if FFill.ShadowColor <> clNone then
    result := FFill.ShadowOffset;
end;

procedure TAdvSmoothListBox.GetTextPosition(var x, y: integer;
  rectangle: TGPRectF; objectwidth, objectheight: integer;
  location: TAdvSmoothListBoxLocation);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    plTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    plTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    plBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    plBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    plTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    plBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    plCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    plCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    plCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;
end;

function TAdvSmoothListBox.GetThemeId: String;
begin
  Result := ClassName;
end;

function TAdvSmoothListBox.GetTopIndex: integer;
begin
  Result := Max(0, YToItem((InsideRect.Right - InsideRect.Left) div 2, GetDisplayRect.Top, false, true));
end;

function TAdvSmoothListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothListBox.HeaderFooterChanged(Sender: TObject);
begin
  if Assigned(Filter) then
    Filter.UpdateFilter;
  Changed;
end;

procedure TAdvSmoothListBox.HideDetails;
begin
  if Assigned(FCurrentControl) and FDetailShow then
  begin
    FDetailShow := false;
    FDetailStatus := dsDetailsNotVisible;
    FCurrentControl.Left := 0;
    DoHideDetail(self, FDetailIndex);
  end;
end;

procedure TAdvSmoothListBox.HideParent;
var
  Allow: Boolean;
begin
  if (csDesigning in ComponentState) then
    Exit;

  Allow := true;
  if Assigned(FOnItemDropDownHide) then
    FOnItemDropDownHide(Self, FDropDownItem, Allow);

  if Allow then
  begin
    FDeactivating := false;
    if FDropDownForm.HandleAllocated then
    begin
      FDropDownForm.Hide;
      try
        SetFocus;
      except
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.InitDisplayList(AItemAutoSize: Boolean = False);
var
  I: Integer;
  thsection, twlookup: integer;
  prevrect: TRect;
  j: integer;
  bmp: TBitmap;
begin
  if FUpdateCount > 0 then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  FDisplayList.Clear;

  twLookup := 0;
  if Sections.Height = -1 then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Canvas.Font.Assign(Sections.Font);
      thsection := bmp.Canvas.TextHeight('gh') + 4;
    finally
      bmp.Free;
    end;
  end
  else
    thsection := Sections.Height;

  if LookupBarVisible and (not LookupBar.OnTop) then
  begin
    twLookup := LookupBar.GetWidth;
    FLookUpSize := twlookup;
  end;

  prevrect := Rect(0, GetShadowOffset, 0, Header.GetHeight + Filter.GetHeight);

  j := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Sections.Visible then
    begin
      if FDisplayList.Count = 0 then
      begin
        AddDisplaySection(Items[I], prevrect, thsection, twLookup);
        AddDisplayItem(Items[I], prevrect, GetItemHeight(Items[I], AItemAutoSize), twLookup);
        Inc(J);
      end
      else
      begin
        if ItemFromDifferentCategory(Items[I], Items[J - 1]) and (not Items[I].Splitter) then
        begin
          AddDisplaySection(Items[I], prevrect, thsection, twLookup);
          AddDisplayItem(Items[I], prevrect, GetItemHeight(Items[I], AItemAutoSize), twLookup);
          Inc(J);
        end
        else
        begin
          AddDisplayItem(Items[I], prevrect, GetItemHeight(Items[I], AItemAutoSize), twLookup);
          if not Items[I].Splitter then
            Inc(J);
        end;
      end;
    end
    else
    begin
      AddDisplayItem(Items[I], prevrect, GetItemHeight(Items[I], AItemAutoSize), twLookup);
    end;
  end;
end;

procedure TAdvSmoothListBox.InitPreview;
var
  i: integer;
begin
  Items.Clear;
  FFooter.Caption := 'Footer';
  FFooter.Font.Size := 10;
  FFooter.FFont.Color := clWhite;

  FHeader.Caption := 'Header';
  FHeader.Font.Size := 10;
  FHeader.FFont.Color := clWhite;

  for I := 0 to 20 do
    Items.Add;
end;

procedure TAdvSmoothListBox.InitSelection(itemindex: integer);
begin
  FSelectedItemIndex := -1;
  FFocusedItemIndex := -1;
end;

procedure TAdvSmoothListBox.InitState;
begin
  FMouseUp := false;
  FMouseDown := false;
end;

function TAdvSmoothListBox.InsideRect: TRect;
var
  bw: integer;
begin
  Result := Bounds(0, 0, Width, Height);
  // adapt width & height for GDI+ drawing rect
  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (Fill.BorderColor <> clNone) then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;

end;

procedure TAdvSmoothListBox.ItemAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothListBox.ItemAtXY(X, Y: integer): integer;
var
  displ: integer;
  DItem: TAdvSmoothListBoxItem;
begin
  Result := -1;
  displ := YToItem(X, Y, false);
  if displ <> -1 then
  begin
    DItem := FDisplayList.GetItem(displ).DisplayItem;
    if DItem <> nil then
      Result := DItem.Index;
  end;
end;

function TAdvSmoothListBox.YToDeleteButton(X, Y: integer): integer;
var
  i: Integer;
  c, check: Boolean;
  r: TRect;
begin
  Result := -1;
  check := PtInRect(GetDisplayRect, Point(X, Y));
  for i := 0 to FDisplayList.Count - 1 do
  begin
    with FDisplayList.GetItem(i) do
    begin
      c := (DisplayItem <> nil);
      if c then
      begin
        r := Bounds(ItemRect.Right, ItemRect.Top + (ItemRect.Bottom - ItemRect.Top - ItemAppearance.DeleteButtonHeight) div 2, ItemAppearance.DeleteButtonWidth, ItemAppearance.DeleteButtonHeight);
        if DisplayItem.DeleteButton and DisplayItem.DeleteButtonVisible and PtInRect(r, Point(X, Y + GetPosition)) and check then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.YToItem(X, Y: integer; CountSections: Boolean = false; CheckDisplayRectangle: Boolean = true): integer;
var
  i: Integer;
  c, check: Boolean;
begin
  Result := -1;
  if CheckDisplayRectangle then
    check := PtInRect(GetDisplayRect, Point(X, Y))
  else
    check := true;

  for i := 0 to FDisplayList.Count - 1 do
  begin
    with FDisplayList.GetItem(i) do
    begin
      if CountSections then
        c := true
      else
        c := (DisplayItem <> nil);

      if c then
      begin
        if PtInRect(ItemRect, Point(X, Y + GetPosition)) and check then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.ItemFromDifferentCategory(item1,
  item2: TAdvSmoothListBoxItem): Boolean;
begin
  Result := false;
  if (item1 = nil) or (item2 = nil) then
  begin
    Result := true
  end
  else
  begin
    case CategoryType of
      alphanumeric:
      begin
        if (Length(item1.Caption) > 0) and (Length(item2.Caption) > 0) then
        begin
          Result := (Item1.Caption[1] <> item2.Caption[1]);
        end
        else
        begin
          if (Length(item1.Caption) = 0) and (Length(item2.Caption) = 0) then
            Result := false
          else
            Result := true;
        end;
      end;
      custom:
      begin
        if (item1.CategoryID > -1) and (item2.CategoryID > -1) then
        begin
          Result := (Item1.CategoryID <> item2.CategoryID);
        end
        else
        begin
          if (item1.CategoryID = -1) and (item2.CategoryID = -1) then
            Result := false
          else
            Result := true;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.ItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyCheck: Word;
  i, sh: integer;
  prev: integer;
  doelse, NewChkVal, GraphLeft: Boolean;
  oldindex: integer;
begin
  inherited;

  doelse := true;
  if (FFocusedItemIndex >= 0) and (FFocusedItemIndex <= Items.Count - 1) then
  begin
    if ((Items[FFocusedItemIndex].GraphicLeftType = gtCheckBox) or (Items[FFocusedItemIndex].GraphicRightType = gtCheckBox))
      and (Key = VK_SPACE) then
      begin
        NewChkVal := not Items[FFocusedItemIndex].Checked;
        GraphLeft := (Items[FFocusedItemIndex].GraphicLeftType = gtCheckBox);
        if DoItemCheckToggle(Items[FFocusedItemIndex], GraphLeft, NewChkVal) then
          Items[FFocusedItemIndex].Checked := NewChkVal;
      end;
  end;

  if not doelse then
    Exit;


  prev := FSelectedItemIndex;

  if (FDisplayList.Count = 0) or (Key = VK_MENU{alt}) or (Key = VK_CONTROL) or (Key = VK_SHIFT) then
    exit;

  FTimerCount := 0;

  KeyCheck := 0;
  case ShowDetailKey of
    dkSpace: KeyCheck := VK_SPACE;
    dkF2: KeyCheck := VK_F2;
    dkReturn: KeyCheck := VK_RETURN;
  end;

  if FDetailStatus = dsDetailsVisible then
  begin
    if Key = KeyCheck then
      HideDetails;
  end
  else
  begin
    if ((Key = KeyCheck) or (FSelectionMode = sPersistSelectionAlways)) and (FFocusedItemIndex <> -1) then
    begin
      if MultiSelect  then
      begin
        if FFocusedItemIndex <> FSelectedItemIndex then
        begin
          if not (ssCtrl in Shift) and not (ssShift in Shift) then
          begin
            for I := 0 to Items.Count - 1 do
              Items[I].Selected := false;

            FSelectedItemIndex := FFocusedItemIndex;
            Items.SelectedItem := FItems[FSelectedItemIndex];
            Items.SelectedItem.Selected := true;
          end
        end
        else
          ShowDetails;

        if ssCtrl in Shift then
        begin
          Items[FFocusedItemIndex].Selected := not Items[FFocusedItemIndex].Selected;
        end

        else if ssShift in Shift then
        begin
          for I := 0 to Items.Count - 1 do
            Items[i].Selected := false;

          if FFocusedItemIndex < FSelectedItemIndex then
          begin
            if FSelectedItemIndex <> -1 then
	    begin
              for I := FselectedItemIndex downto FFocusedItemIndex do
	      begin
	        if Items[I].Visible then
                  Items[I].Selected := true
	      end
	    end
          end
          else
          begin
            if FSelectedItemIndex <> -1 then
	    begin
              for I := FselectedItemIndex to FFocusedItemIndex do
	      begin
	        if Items[I].Visible then
                  Items[I].Selected := true
	      end
	    end
          end;
        end
      end
      else
        ShowDetails;
    end
    else
    begin
      if MultiSelect then
      begin
        case Key of
          VK_DOWN:
            begin
              oldindex := FFocusedItemIndex;
              repeat
                Inc(FFocusedItemIndex);
                if (FFocusedItemIndex > Items.Count - 1) then
                  break;
              until (Items[FFocusedItemIndex].Visible) or (FFocusedItemIndex = Items.Count - 1);

              if (FFocusedItemIndex = Items.Count - 1) and not Items[FFocusedItemIndex].Visible then
                FFocusedItemIndex := oldindex;
            end;
          VK_UP:
            begin
              repeat
                oldindex := FFocusedItemIndex;
                Dec(FFocusedItemIndex);
                if (FFocusedItemIndex < 0) then
                  break;
              until (Items[FFocusedItemIndex].Visible) or (FFocusedItemIndex = 0);

              if (FFocusedItemIndex = 0) and not Items[FFocusedItemIndex].Visible then
                FFocusedItemIndex := oldindex;
            end;
          VK_HOME: FFocusedItemIndex := 0;
          VK_END: FFocusedItemIndex := FDisplayList.Count - 1;
          VK_NEXT: FFocusedItemIndex := FFocusedItemIndex + 5;
          VK_PRIOR: FFocusedItemIndex := FFocusedItemIndex - 5;
        end;

        FFocusedItemIndex := Min(Items.Count - 1, Max(0, FFocusedItemIndex));

        while Items[FFocusedItemIndex].Splitter and (FFocusedItemIndex > 0) do
        begin
          case Key of
            VK_DOWN: Inc(FFocusedItemIndex);
            VK_UP: Dec(FFocusedItemIndex);
            VK_HOME: FFocusedItemIndex := 0;
            VK_END: FFocusedItemIndex := FDisplayList.Count - 1;
            VK_NEXT: FFocusedItemIndex := FFocusedItemIndex + 5;
            VK_PRIOR: FFocusedItemIndex := FFocusedItemIndex - 5;
          end;
        end;

        sh := 0;
        if Sections.Visible then
          sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;

        FFocusedItemIndex := Min(Items.Count - 1, Max(0, FFocusedItemIndex));

        if ssShift in Shift then
        begin
          for I := 0 to Items.Count - 1 do
            Items[i].Selected := false;

          if FSelectedItemIndex <> - 1 then
          begin
            if FFocusedItemIndex < FSelectedItemIndex then
            begin
              for I := FselectedItemIndex downto FFocusedItemIndex do
                Items[I].Selected := true
            end
            else
            begin
              for I := FselectedItemIndex to FFocusedItemIndex do
                Items[I].Selected := true
            end;
          end;
        end;

        for I := 0 to FDisplayList.Count - 1 do
        begin
          with FDisplayList.GetItem(i) do
          begin
            if DisplayItem <> nil then
            begin
              if DisplayItem.Index = FFocusedItemIndex then
              begin
                if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
                  FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
                else if ItemRect.Top < GetPosition + Header.GetHeight + Filter.GetHeight then
                  FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;

                if FScposTo <> FCurrentScPos then
                begin
                  ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
                  FCurrentScPos := FScPosTo;
                  FAnimate := true;
                end;
              end;
            end;
          end;
        end;
        Changed;
      end
      else if (Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_LEFT) or (Key = VK_Right)
        or (Key = VK_END) or (Key = VK_NEXT) or (Key = VK_PRIOR) or (Key = VK_HOME) then
      begin
        //FSelectedItemIndex := FFocusedItemIndex;
        case Key of
          VK_DOWN, VK_RIGHT:
            begin
              if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= Items.Count - 1) then
              begin
                if (Key = VK_RIGHT) then
                begin
                  if not Items[FSelectedItemIndex].Expanded and (Items[FSelectedItemIndex].GraphicLeftType = gtNode) then
                  begin
                    Items[FSelectedItemIndex].Expanded := true;
                    Exit;
                  end;
                end;
              end;

              oldindex := FSelectedItemIndex;
              repeat
                Inc(FSelectedItemIndex);
                FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
              until ((Items[FSelectedItemIndex].Visible) or (FSelectedItemIndex = Items.Count - 1)) and not (Items[FSelectedItemIndex].Splitter);

              if (FSelectedItemIndex = Items.Count - 1) and not (Items[FSelectedItemIndex].Visible) then
                FSelectedItemIndex := oldindex;

            end;
          VK_UP, VK_LEFT:
            begin
              if (FSelectedItemIndex >= 0) and (FSelectedItemIndex <= Items.Count - 1) then
              begin
                if Key = VK_LEFT then
                begin
                  if Items[FSelectedItemIndex].Expanded and (Items[FSelectedItemIndex].GraphicLeftType = gtNode) then
                  begin
                    Items[FSelectedItemIndex].Expanded := false;
                    Exit;
                  end;
                end;
              end;

              oldindex := FSelectedItemIndex;
              repeat
                Dec(FSelectedItemIndex);
                FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
              until ((Items[FSelectedItemIndex].Visible) or (FSelectedItemIndex = 0)) and not (Items[FSelectedItemIndex].Splitter);

              if ((FSelectedItemIndex = 0) and not (Items[FSelectedItemIndex].Visible)) or (Items[FSelectedItemIndex].Splitter) then
                FSelectedItemIndex := oldindex;

            end;
          VK_HOME:
            begin
              FSelectedItemIndex := 0;
              while (FSelectedItemIndex < Items.Count - 1) and not Items[FSelectedItemIndex].Visible do
                inc(FSelectedItemIndex);
            end;
          VK_END:
            begin
              FSelectedItemIndex := Max(0, Items.Count - 1);
              while (FSelectedItemIndex > 0) and not Items[FSelectedItemIndex].Visible do
                dec(FSelectedItemIndex);
            end;
          VK_NEXT:
            begin
              I := 5;
              FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
              while (I > 0) and (FSelectedItemIndex < Items.Count - 1) do
              begin
                inc(FSelectedItemIndex);
                if Items[FSelectedItemIndex].Visible then Dec(I);
              end;
              while (FSelectedItemIndex > 0) and not Items[FSelectedItemIndex].Visible do
                dec(FSelectedItemIndex);
            end;
          VK_PRIOR:
            begin
              I := 5;
              FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
              while (I > 0) and (FSelectedItemIndex > 0) do
              begin
                dec(FSelectedItemIndex);
                if Items[FSelectedItemIndex].Visible then Dec(I);
              end;
              while (FSelectedItemIndex < Items.Count - 1) and not Items[FSelectedItemIndex].Visible do
                inc(FSelectedItemIndex);
            end;
        end;

        FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
        while Items[FSelectedItemIndex].Splitter and (FSelectedItemIndex > 0) do
        begin
          case Key of
            VK_DOWN, VK_RIGHT: Inc(FSelectedItemIndex);
            VK_UP, VK_LEFT: Dec(FSelectedItemIndex);
            VK_HOME: FSelectedItemIndex := 0;
            VK_END: FSelectedItemIndex := FDisplayList.Count - 1;
            VK_NEXT: FSelectedItemIndex := FSelectedItemIndex + 5;
            VK_PRIOR: FSelectedItemIndex := FSelectedItemIndex - 5;
          end;
        end;

        sh := 0;
        if Sections.Visible then
          sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;

        FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
        for I := 0 to Items.Count - 1 do
          Items[I].Selected := false;
        Items.SelectedItem := FItems[FSelectedItemIndex];
        Items.SelectedItem.Selected := true;

        for I := 0 to FDisplayList.Count - 1 do
        begin
          with FDisplayList.GetItem(i) do
          begin
            if DisplayItem <> nil then
            begin
              if DisplayItem.Index = FSelectedItemIndex then
              begin
                if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
                  FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
                else if ItemRect.Top < GetPosition + Header.GetHeight + Filter.GetHeight + sh then
                  FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;

                if FScposTo <> FCurrentScPos then
                begin
                  ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
                  FCurrentScPos := FScPosTo;
                  FAnimate := true;
                end;
              end;
            end;
          end;
        end;
        Changed;
      end;
    end;
  end;

  if FSelectedItemIndex <> prev then
    if Assigned(FOnItemSelectionChanged) then
      FOnItemSelectionChanged(Self, prev, FSelectedItemIndex);

//  if not (Key = VK_F4) then
  if (Key in [VK_DOWN, VK_UP, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME, VK_END]) then
  begin
    if Assigned(FOnItemSelected) then
      FOnItemSelected(Self, FSelectedItemIndex);
  end
  else
    HideParent;

  if (Key in [VK_DOWN, VK_UP, VK_LEFT, VK_RIGHT, VK_SPACE, VK_RETURN, VK_HOME, VK_END]) then
    FLookupKey := '';
end;

procedure TAdvSmoothListBox.KeyPress(var Key: char);
var
  i, sh, prev: integer;
  flg: boolean;
begin
  inherited;

  if Key = #32 then
  begin
    if Assigned(OnItemClick) then
      OnItemClick(Self, SelectedItemIndex);
  end;
  
  if not KeyBoardLookup then
    Exit;

  FTimerCount := 0;

  if (Key >= '0') and (Key <= 'z') then
  begin
    FLookupKey := FLookupKey + key;
  end;

  if Key = #8 then
  begin
    if Length(FLookupKey) > 0 then
      Delete(FLookupKey, Length(FLookupKey), 1);
  end;


  flg := false;

  sh := 0;
  if Sections.Visible then
    sh := FDisplayList.GetItem(0).ItemRect.Bottom;

  prev := FSelectedItemIndex;
  for i := 0 to FDisplayList.Count - 1 do
  begin
    with FDisplayList.GetItem(i) do
    begin
      if DisplayItem <> nil then
      begin
        if pos(Uppercase(FLookupKey), Uppercase(DisplayItem.Caption)) = 1 then
        begin
          Items.SelectedItem := DisplayItem;
          FAnimate := true;
          
          if Sections.Visible then          
            FScPosTo := ItemRect.Top - sh
          else
            FScPosTo := ItemRect.Top - Header.GetHeight - Filter.GetHeight;

          FSelectedItemIndex := DisplayItem.Index;

          if FSelectedItemIndex <> prev then
            if Assigned(FOnItemSelectionChanged) then
              FOnItemSelectionChanged(Self, prev, FSelectedItemIndex);

          if Assigned(FOnItemSelected) then
            FOnItemSelected(Self, FSelectedItemIndex);
          FSp := FSpeedFactor;
          flg := true;
          break;
          Changed;
        end;
      end;
    end;
  end;

  if not flg then
    FLookupKey := '';
end;

procedure TAdvSmoothListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ssCtrl in Shift then
  begin
    case Key of
      70:
      begin
        if Filter.Enabled then
        begin
          Filter.Visible := not Filter.Visible;
          if Assigned(Filter.FEdit) and Filter.Visible then
            Filter.FEdit.SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  item: integer;
  AnchorI, AnchorF, AnchorH: String;
  rh, rf: TRect;
  delb: Integer;
begin
  inherited;

  fmb := Button;

  if not (fmb in FTriggerButtons) then
    Exit;

  if FAnimatingdetail then
    Exit;

  FMouseDown := not FMouseDblClick;
  FMouseDblClick := false;

  //HEADER
  rh := GetHeaderFooterCaptionRect(Header, true);
  AnchorH := Header.GetAnchorAt(rh, X, Y);

  //FOOTER
  rf := GetHeaderFooterCaptionRect(Footer, false);
  AnchorF := Footer.GetAnchorAt(rf, X, Y);

  //ITEMS
  item := YToItem(X, Y);
  if item <> -1 then
  begin
    with FDisplayList.GetItem(item).DisplayItem do
    begin
      AnchorI := GetAnchorAt(X, Y);
      
      if PtInGPRect(finfor, Point(X, Y + FCurrentScPos)) then
        DoItemInfoClick(Self, Index);

      if PtInGPRect(fcaptionr, Point(X, Y + FCurrentScPos)) then
        DoItemCaptionClick(Self, Index);

      FBLD := false;
      FBRD := false;
      if (PtInRect(fgrLeft, Point(X, Y)) and Enabled) then
      begin
        case GraphicLeftType of
          gtButton, gtSmoothButton, gtDropDownButton:
          begin
            FButtonLeft := not FButtonLeft;
            FBLD := FButtonLeft;
          end;
        end;
        Changed;
      end;

      if (PtInRect(fgrRight, Point(X, Y)) and Enabled) then
      begin
        case GraphicRightType of
          gtButton, gtSmoothButton, gtDropDownButton:
          begin
            FButtonRight := not FButtonRight;
            FBRD := FButtonRight;
          end;
        end;
        Changed;
      end;
    end;
  end
  else
  begin
    delb := DeleteButtonAtXY(X, Y);
    if (delb <> -1) and (delb >= 0) and (delb <= Items.Count - 1) then
    begin
      Items[delb].FDeleteButtonDown := True;
      invalidate;
    end;
  end;

  if (AnchorI <> '') or (AnchorF <> '') or (AnchorH <> '') then
  begin
    if AnchorI <> '' then
    begin
      if item <> -1 then
      begin
        with FDisplayList.GetItem(item).DisplayItem do
          DoItemAnchorClick(Self, AnchorI, Index);
      end;
    end;

    if AnchorF <> '' then
      DoAnchorClick(Self, AnchorF);

    if AnchorH <> '' then
      DoAnchorClick(Self, AnchorH);
  end;

  FDragX := X;
  FDragY := Y;
  FScrollY := Y;
  FTimeStart := GetTickCount;
  FDragClickY := Y;
  FClickY := Y;
  FClickX := X;
  FDragCount := 0;
  FDragCountTimer.Enabled := True;
end;

procedure TAdvSmoothListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  item: integer;
  AnchorF, AnchorH, AnchorI: String;
  rh, rf, dfb, r: TRect;
  dItem: TAdvSmoothListBoxItem;
  di: TImage;
  mindrag, maxdrag: integer;
  AllowDrag: Boolean;
  i, cp: integer;
  it: Integer;
begin
  inherited;

  FDragCountTimer.Enabled := False;

  if BlockMouseMovement then
    Exit;

  if not (fmb in FTriggerButtons) then
    Exit;

  if (csDesigning in ComponentState) or (FDetailStatus = dsDetailsVisible)
    or (FAnimatingdetail) then
  begin
    FMouseDown := false;
    FMouseUp := false;
    Exit;
  end;

  if FLookUp then
  begin
    FTimeStart := 0;
    FTimeStop := 0;
  end;

  if FMouseDown then
  begin
    it := YToItem(X, Y);
    if it <> -1 then
    begin
      FDragging := FDisplayList.GetItem(it).DisplayItem.Selected and EnableDragging;
      FDragging := FDragging and not ((X < FDragX + DRAGMARGIN) and (X > FDragX - DRAGMARGIN));
    end
    else
      FDragging := False;

    if EnableDragging and FDragging then
    begin
      BeginDrag(true);
      Exit;
    end;

    if ((ssAlt in Shift) or (FDragCount >= 5)) and ItemDragging then
    begin
      if not Assigned(FDragItemForm) then
      begin
        //Build Form
        FDragItemForm := nil;
        item := YToItem(X, Y, False);
        if item <> -1 then
        begin
          DItem := FDisplayList.GetItem(item).DisplayItem;
          AllowDrag := true;
          if Assigned(FOnItemDragStart) then
            if DItem <> nil then            
              FOnItemDragStart(Self, DItem.Index, allowdrag);

          if AllowDrag and (DItem <> nil) then
          begin
            FMode := dmDrag;
            FSelectedDragItem := DItem;
            r := FDisplayList.GetItem(item).FItemRect;
            dfb := Bounds(r.Left , r.Top , r.Right - r.Left , r.Bottom - r.Top + GetPosition);

            FDragItemForm := TForm.Create(Application);
            FDragItemForm.Position := poDesigned;
            FDragItemForm.FormStyle := fsStayOnTop;
            FDragItemForm.SetBounds(Self.Parent.ClientOrigin.X + Self.Left + r.Left , dfb.Top + Self.Parent.ClientOrigin.Y + Self.Top - GetPosition,
              r.Right - r.Left, r.Bottom - r.Top);

            FDragOldTop := FDragItemForm.Top;

            FDragItemForm.BorderStyle := bsNone;
            FDragItemForm.BorderWidth := DragBorderWidth;
            FDragItemForm.Brush.Style := bsClear;
            FDragItemForm.Brush.Color := DragBorderColor;
            {$IFDEF DELPHI6_LVL}

            FDragItemForm.AlphaBlend := DragAlphaBlend;
            FDragItemForm.AlphaBlendValue := DragOpacity;
            {$ENDIF}
            di := TImage.Create(FDragItemForm);
            di.Width := r.Right - r.Left;
            di.Height := r.Bottom - R.Top;
            FSelectedDragItem.Draw(di.Canvas, Bounds(0, r.Top, r.Right - r.Left, r.Bottom - r.Top), item, true);
//            BitBlt(di.Canvas.Handle,0,0,r.Right, r.Bottom - getPosition, Self.Canvas.Handle, r.Left, r.Top - GetPosition, SRCCopy);
            di.Parent := FDragItemForm;
            FDragItemForm.Show;
          end;
        end;
      end;
      if FDragItemForm <> nil then
      begin
        mindrag := Self.Top + Self.Parent.ClientOrigin.Y + Header.GetHeight + Filter.GetHeight;
        maxdrag := Self.Top + Self.Parent.ClientOrigin.Y + self.Height - GetShadowOffset - Footer.GetHeight - FDragItemForm.Height;

        FDragDelta := (Y - FDragClickY);
        FDragItemForm.SetBounds(FDragItemForm.Left, Min(maxdrag, Max(FDragDelta + FDragOldTop, mindrag)) , FDragItemForm.Width, FDragItemForm.Height);

        if ((FDragItemForm.Top = mindrag) and (ClientToScreen(Point(X, Y)).Y < FDragItemForm.Top)) or
          ((FDragItemForm.Top = maxdrag) and (ClientToScreen(Point(X, Y)).Y > FDragItemForm.Top + FDragItemForm.Height)) then
        begin
          if (FDragItemForm.Top = mindrag) then
            FDragAnimateDelta := ClientToScreen(Point(X, Y)).Y - FDragItemForm.Top
          else if (FDragItemForm.Top = maxdrag) then
            FDragAnimateDelta := ClientToScreen(Point(X, Y)).Y - FDragItemForm.Top - FDragItemForm.Height
          else
            FDragAnimateDelta := 0;
        end
        else
          FDragAnimateDelta := 0;

        FDragTimer.Enabled := (FDragItemForm.Top = maxdrag) or (FDragItemForm.Top = mindrag);

        if Assigned(FOnItemDragOver) then
        begin
          if FSelectedDragItem <> nil then
          begin
            item := YToItem(X, Y);
            if item <> -1 then
            begin
              DItem := FDisplayList.GetItem(item).DisplayItem;
              if DItem <> nil then
                FOnItemDragOver(Self, FSelectedDragItem.Index, dItem.Index);
            end
            else
              FOnItemDragOver(Self, FSelectedDragItem.Index, -1);
          end;
        end;
      end;
    end
    else
    begin
      if Assigned(FDragItemForm) then
      begin
        FDragItemForm.Free;
        FDragItemForm := nil;
        if Assigned(FOnItemDragEnd) then
          if FSelectedDragItem <> nil then
            FOnItemDragEnd(Self, FSelectedDragItem.Index);
      end;

      FDragTimer.Enabled := false;
      FDragAnimateDelta := 0;
      FMode := dmRelease;      

      if ((FDragY < Y - CLICKMARGIN) or (FDragY > Y + CLICKMARGIN)) then
      begin
        ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
        FSp := 4;
        FHoveredItemIndex := -1;

        case SelectionMode of
          sAutoDeselect:
          begin
            for I := 0 to FItems.Count - 1 do
              Items[I].Selected := false;

            FSelectedItemIndex := -1;
          end;
        end;

        FAnimate := false;

        cp := FCurrentScPos;
        if (Y - FDragY) > 0 then
          FCurrentScPos := GetPosition - Abs(Y - FDragY)
        else
          FCurrentScPos := GetPosition + Abs(Y - FDragY);

        FDragY := Y;
        FScPosTo := GetPosition;
        DoSmoothScroll(cp, FScPosTo);
        Changed;
      end;
    end;
  end
  else
  begin
    if FMouseUp and not FLookUp then
    begin
      FMouseUp := false;
      if ((FTimeStop - FTimeStart) > 500) or ((FTimeStop - FTimeStart) = 0) then
        Exit;

      FSp := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
      if FSp > 0 then
      begin
        if (Y - FScrollY) > 0 then
          FScPosTo := FScPosTo - Round(Abs(Y - FScrollY) * FSp)
        else
          FScPosTo := FScPosTo + Round(Abs(Y - FScrollY) * FSp);
      end;
    end;
  end;

  if FMode = dmRelease then
  begin
    //HEADER
    rh := GetHeaderFooterCaptionRect(Header, true);
    AnchorH := Header.GetAnchorAt(rh, X, Y);

    //FOOTER
    rf := GetHeaderFooterCaptionRect(Footer, false);
    AnchorF := Footer.GetAnchorAt(rf, X, Y);

    //ITEM ANCHOR
    item := YToItem(X, Y);
    if item <> -1 then
    begin
      dItem := FDisplayList.GetItem(item).DisplayItem;
      if dItem <> nil then
      begin
        with dItem do
        begin
          AnchorI := GetAnchorAt(X, Y);
        end;
      end;
    end;

    keepoldcursor := true;
    if (AnchorI <> '') or (AnchorF <> '') or (AnchorH <> '') then
    begin
      Cursor := crHandPoint
    end
    else
    begin
      if Cursor <> FOldCursor then
      begin
        Application.CancelHint;
        Cursor := FoldCursor;
      end;
    end;

    keepoldcursor := false;

    if Items.Count > 0 then
    begin
      //ITEMS
      if item <> -1 then
      begin
        dItem := FDisplayList.GetItem(item).DisplayItem;
        if Ditem <> nil then
        begin
          with dItem do
          begin
            if FhoveredItemIndex <> dItem.Index then
              Application.CancelHint;

            FhoveredItemIndex := DItem.Index;
            if (FPrevHoveredItemIndex <> FhoveredItemIndex) and (FPrevHoveredItemIndex >= 0) and (FPrevHoveredItemIndex <= Items.Count -1)
              and (FhoveredItemIndex >= 0) and (FhoveredItemIndex <= Items.Count -1) then
            begin
              with Items[FPrevHoveredItemIndex] do
              begin
                if not FMouseLeft then
                begin
                  if Assigned(FOnItemMouseLeave) then
                    FOnItemMouseLeave(Self, FPrevHoveredItemIndex);

                  FPrevHoveredItemIndex := FhoveredItemIndex;
                  FMouseLeft := true;
                  FMouseEntered := false;
                end;
              end;
            end;

            if (FhoveredItemIndex >= 0) and (FhoveredItemIndex <= Items.Count -1) then
            begin
              if not FMouseEntered then
              begin
                FPrevHoveredItemIndex := FHoveredItemIndex;
                if Assigned(FOnItemMouseEnter) then
                  FOnItemMouseEnter(Self, FhoveredItemIndex);

                FMouseEntered := true;
                FMouseLeft := false;
              end
            end;
          end;
        end;
      end
      else
      begin
        if (FHoveredItemIndex >= 0) and (FHoveredItemIndex <= Items.Count -1) then
        begin
          with Items[FhoveredItemIndex] do
          begin
            if not FMouseLeft then
            begin
              if Assigned(FOnItemMouseLeave) then
                FOnItemMouseLeave(Self, FHoveredItemIndex);

              FMouseLeft := true;
              FMouseEntered := false;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  selitem: Boolean;
  l, th, i, si, wr, wl, delsi, delb: integer;
  ch: Char;
  s: String;
  stop, sh: integer;
  sdOnImg, NewChkVal: Boolean;
  itemrclick, itemr, rh, rf: TRect;
  item: integer;
  DItem: TAdvSmoothListBoxItem;
  AllowDrop: Boolean;
  catid: integer;
  selecteditem: TAdvSmoothListBoxItem;
  aDispItem: TAdvSmoothListBoxDisplayListItem;
  AllowDelete: Boolean;
  dochange: Boolean;
  md: Boolean;
  it: TAdvSmoothListBoxDisplayListItem;
  sc: TAdvSmoothListBoxDisplayListItem;
begin
  inherited;

  FDragCountTimer.Enabled := False;
  FDragCount := 0;

  fmb := mbLeft;
  md := FMouseDown;

  if not (Button in FTriggerButtons) then
  begin
    FMouseDown := False;
    Exit;
  end;


  if FDragging and EnableDragging then
  begin
    FDragging := false;
    FMouseDown := false;
    if Items.CountSelected > 1 then
      Exit;
  end;

  if FAnimatingdetail then
    Exit;

  if csDesigning in ComponentState then
    Exit;

  if FAnimating and FAnimate and not (GetFullHeight < Height) then
  begin
    FAnimate := false;
    FScrollY := GetPosition;
    FScPosTo := GetPosition;
    FCurrentScPos := GetPosition;
    FTimeStart := 0;
    FTimeStop := 0;
  end;

  FMouseDown := false;
  FMouseUp := true;
  FTimeStop := GetTickCount;
  FAnimate := (FTimeStop - FTimeStart > 0);
  FDragAnimateDelta := 0;
  FDragTimer.Enabled := false;
  selitem := true;


  dochange := False;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i].FDeleteButtonDown then
    begin
      Items[i].FDeleteButtonDown := false;
      dochange := True;
    end;
  end;

  if dochange then
    Invalidate;


  for I := 0 to Items.Count - 1 do
  begin
    case Items[I].GraphicLeftType of
      gtButton, gtSmoothButton, gtDropDownButton: Items[I].FButtonLeft := false;
    end;
    case Items[I].GraphicRightType of
      gtButton, gtSmoothButton, gtDropDownButton: Items[I].FButtonRight := false;
    end;
  end;

  if FMode = dmRelease then
  begin
    if (Abs(X - FClickX) >= DELETEMARGIN) and (Abs(Y - FClickY) < 10) then
    begin
      delsi := ItemAtXY(X, Y);
      if (delsi <> -1) and (delsi <= Items.Count - 1) then
      begin
        if Items[delsi].DeleteButtonVisible then
          Items[delsi].DeleteButton := not Items[delsi].DeleteButton;
        selitem := False;
      end;
    end;

    if LookupBarVisible then
    begin
      Canvas.Font.Assign(LookupBar.Font);
      th := 0;
      case CategoryType of
        alphanumeric:
        begin
          if not Lookupbar.Rotated then
            th := Canvas.TextHeight('gh')
          else
            th := Canvas.TextWidth('W');
        end;
        custom:
        begin
          if not Lookupbar.Rotated then
            th := Canvas.TextHeight('gh')
          else
            th := GetMaximumCustomTextHeight(Canvas);
        end;
      end;

      case LookupBar.Position of
        pLeft:
        begin
          selitem := not (X < FLookUpSize);
        end;
        pRight:
        begin
          selitem := not (X > InsideRect.Right - FLookUpSize - GetShadowOffset);
        end;
      end;

      if not selitem then
      begin
        if ((LookupBar.Position = pLeft) and  (X < FLookUpSize)) or ((LookupBar.Position = pRight) and  (X > InsideRect.Right - FLookUpSize - GetShadowOffset)) then
        begin
          case CategoryType of
            alphanumeric:
            begin
              if LookupBar.Numeric then
                stop := 35
              else
                stop := 26;

              if LookupBar.AutoSize then
                l := 1 + Round((Y - Header.GetHeight - Filter.GetHeight - (th / 2)) / (InsideRect.Bottom - InsideRect.Top - Footer.GetHeight - Header.GetHeight - Filter.GetHeight) * stop)
              else
              begin
                l := 1 + Round((Y - Header.GetHeight - Filter.GetHeight - ((th + LookupBar.Spacing) / 2)) / (th + LookupBar.Spacing));
              end;

              if LookupBar.FChar[l] then
              begin
                if (LookUpBar.Order = loNumericLast) or not LookUpBar.Numeric then
                begin
                  if l < 27 then
                    ch := chr(ord('A') + (l - 1))
                  else
                    ch := chr(ord('0') + (l - 27));
                end
                else
                begin
                  if l < 11 then
                    ch := chr(ord('0') + (l - 1))
                  else
                    ch := chr(ord('A') + (l - 11));
                end;
                
                s := ch;
                if Assigned(FOnLookUpClick) then
                  FOnLookUpClick(Self, l, s);

                if Sections.Visible then
                begin
                  sh := FDisplayList.GetItem(0).ItemRect.Bottom;
                  s := ch;
                  aDispItem := FindFirstItemWithChar(ch);
                  if Assigned(aDispItem) then
                  begin
                    FScPosTo := aDispItem.ItemRect.Top - sh;
                    FCurrentScPos := FScPosTo;
                  end;
                end
                else
                begin
                  aDispItem := FindFirstItemWithChar(ch);
                  if Assigned(aDispItem) then
                  begin
                    FScPosTo := aDispItem.ItemRect.Top - Header.GetHeight - Filter.GetHeight;
                    FCurrentScPos := FScPosTo;
                  end;
                end;

                FLookUp := true;
                FSp := 2;
                if Assigned(aDispItem) then
                  DoLookup(aDispItem);
                //DoInternalScroll;
              end;
            end;
            custom:
            begin
              stop := Categories.Count;
              if LookupBar.AutoSize then
                l := Round((Y - Header.GetHeight - Filter.GetHeight - (th / 2)) / (InsideRect.Bottom - InsideRect.Top - Footer.GetHeight - Header.GetHeight - Filter.GetHeight) * stop)
              else
                l := Round((Y - Header.GetHeight - Filter.GetHeight - ((th + LookupBar.Spacing) / 2)) / (th + LookupBar.Spacing));

              if (l > -1) and (l < Categories.Count) then
              begin
                catid := Categories[l].Id;
                if (catid >= 0) and (catid <= Length(LookupBar.FCustomChar) -1) then
                begin
                  if LookupBar.FCustomChar[catid] then
                  begin
                    if Categories[catid].LookupText <> '' then
                      s := Categories[catid].LookupText
                    else
                      s := Categories[catid].Text;

                    if Assigned(FOnLookUpClick) then
                      FOnLookUpClick(Self, l, s);

                    if Sections.Visible then
                    begin
                      sh := FDisplayList.GetItem(0).ItemRect.Bottom;
                      FScPosTo := FindFirstSectionWithCategoryID(catid).ItemRect.Top - sh ;
                      FCurrentScPos := FScPosTo;
                    end
                    else
                    begin
                      it := FindFirstItemWithCategoryID(catid);
                      FScPosTo := it.ItemRect.Top - GetItemHeight(it.FItem) - ItemAppearance.VerticalSpacing;
                      FCurrentScPos := FScPosTo;
                    end;

                    FLookUp := true;
                    FSp := 2;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    if md and selitem and (PtInRect(GetDisplayRect, Point(X, Y))) and (Button in FTriggerButtons) then
    begin
      si := ItemAtXY(X, Y);
      if (si <> -1) and (si <= Items.Count - 1) then
      begin
        if Items[si].Enabled then
        begin
          if PtInRect(Items[si].fgrLeft, Point(X, Y)) and Items[si].Enabled and (Items[si].GraphicLeftType <> gtNone) then
          begin
            FGraphicClicked := cLeft;
            with Items[si] do
            begin
             case GraphicLeftType of
               gtSmoothButton, gtDropDownButton, gtCommonDetailImage, gtImage, gtNode, gtCommonImage, gtDetailImage: Expanded := not Expanded;
             end;
             case GraphicLeftType of
               gtCheckBox:
               begin
                 NewChkVal := not Checked;
                 if DoItemCheckToggle(Items[si], True, NewChkVal) then
                   Checked := NewChkVal;
                 DoItemCheckClick(Self, Index, Checked);
               end;
               gtRadio:
               begin
                 for I := 0 to Items.Count - 1 do
                   Items[I].Checked := false;

                 Checked := not Checked;
                 DoItemRadioClick(Self, Index, Checked);
               end;
               gtButton, gtSmoothButton:
               begin
                 DoItemButtonClick(Self, Index);
                 DoItemButtonLeftClick(Self, Index);
               end;
               gtDropDownButton: DoItemButtonDropDownClick(Self, Index);
               gtImage, gtCommonImage: DoItemImageClick(Self, Index, True);
               gtDetailImage, gtCommonDetailImage:
               begin
                 DoItemImageClick(Self, Index, True);
               end;
             end;
            end
          end
          else if PtInRect(Items[si].fgrRight, Point(X, Y)) and Items[si].Enabled and (Items[si].GraphicRightType <> gtNone) then
          begin
            FGraphicClicked := cRight;
            with Items[si] do
            begin
              case GraphicRightType of
               gtCheckBox:
               begin
                 NewChkVal := not Checked;
                 if DoItemCheckToggle(Items[si], False, NewChkVal) then
                   Checked := NewChkVal;
                 DoItemCheckClick(Self, Index, Checked);
               end;
               gtRadio:
               begin
                 for I := 0 to Items.Count - 1 do
                   Items[I].Checked := false;

                 Checked := not Checked;
                 DoItemRadioClick(Self, Index, Checked);
               end;
               gtButton, gtSmoothButton:
               begin
                 DoItemButtonClick(Self, Index);
                 DoItemButtonRightClick(Self, Index);
               end;
               gtDropDownButton: DoItemButtonDropDownClick(Self, Index);
               gtImage, gtCommonImage: DoItemImageClick(Self, Index, False);
               gtDetailImage, gtCommonDetailImage:
               begin
                 DoItemImageClick(Self, Index, False);
               end;
              end;
            end;
          end
          else if ((FScrollY > Y - CLICKMARGIN) and (FScrollY < Y + CLICKMARGIN)) then
          begin
            DItem := Items[si];
            if FSelectedItemIndex <> DItem.Index then
            begin
              if Assigned(FOnItemSelectionChanged) then
                FOnItemSelectionChanged(Self, FSelectedItemIndex, DItem.Index);
            end;

            if (ssShift in Shift) and MultiSelect then
            begin
              for I := 0 to Items.Count - 1 do
                Items[i].Selected := false;

              if Ditem.Index < FSelectedItemIndex then
              begin
                if FSelectedItemIndex <> -1 then
                  for I := FselectedItemIndex downto DItem.Index do
                    Items[I].Selected := true
              end
              else
              begin
                if FSelectedItemIndex <> -1 then
                  for I := FselectedItemIndex to DItem.Index do
                    Items[I].Selected := true
              end;

              if Assigned(FOnItemSelected) then
                FOnItemSelected(Self, DItem.Index);
            end
            else if ((ssCtrl in Shift) and MultiSelect) or ((FSelectionMode = sPersistSelectionAlways) and MultiSelect) then
            begin
              Items[DItem.Index].Selected := not Items[DItem.Index].Selected;
              if Assigned(FOnItemSelected) then
                FOnItemSelected(Self, DItem.Index);
            end
            else
            begin
              for I := 0 to Items.Count - 1 do
                Items[i].Selected := false;

              Items.Selecteditem := Ditem;
              Fselecteditemindex := Ditem.Index;
              Items.Selecteditem.Selected := True;
              Doitemclick(Self, Items.Selecteditem.Index);
              If Assigned(Fonitemselected) Then
                Fonitemselected(Self, Items.Selecteditem.Index);
            end;
          end;
        end;
        if (si > -1) and (si <= Items.Count - 1) and ((FScrollY > Y - 2) and (FScrollY < Y + 2)) then
        begin
          with Items[si] do
          begin
            sdOnImg := false;
            case ShowDetailClick of
              sdOnClick:
              begin
                itemr := FDisplayList.GetItem(si).ItemRect;
                wl := fgrLeft.Right - fgrLeft.Left;
                wr := fgrRight.Right - fgrRight.Left;
                itemrclick := Rect(itemr.Left + wl, itemr.Top - GetPosition, itemr.Right - wr, itemr.Bottom);

                if PtInRect(itemrclick, Point(X, Y)) then
                  ShowDetails;
              end;
              sdOnDetailImageClick: sdOnImg := true;
            end;
          end;
          if (PtInRect(Items[si].fgrLeft, Point(X, Y)) and Items[si].Enabled) then
          begin
           with Items[si] do
           begin
             case GraphicLeftType of
               gtDetailImage, gtCommonDetailImage:
               begin
                 if sdOnImg then
                   ShowDetails(si);
               end;
             end;
            end
          end
          else if (PtInRect(Items[si].fgrRight, Point(X, Y)) and Items[si].Enabled) then
          begin
            with Items[si] do
            begin
              case GraphicRightType of
               gtDetailImage, gtCommonDetailImage:
               begin
                 if sdOnImg then
                   ShowDetails(si);
               end;
              end;
            end;
          end;
        end;
      end
      else
      begin
        sc := SectionAtXY(X, Y);
        if Assigned(sc) then
          DoSectionClick(Self, sc.SectionCaption, sc.SectionCategoryID);

        delb := DeleteButtonAtXY(X, Y);
        if (delb <> -1) and (delb >= 0) and (delb <= Items.Count - 1) then
        begin
          AllowDelete := True;
          if Assigned(OnItemDeleteClicked) then
            OnItemDeleteClicked(Self, Items[delb], AllowDelete);

          if AllowDelete then
          begin
            Items[delb].Free;
          end;
        end;
      end;
      Changed;
    end
    else
    begin
      ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
      FSp := 4;
    end;

    //HEADER
    rh := GetHeaderFooterRect(Header, true);
    if PtInRect(rh, Point(X, Y)) and not FComboUse then
      if Assigned(FOnHeaderClick) then
        FOnHeaderClick(Self, X, Y);


    //FOOTER
    rf := GetHeaderFooterRect(Footer, false);

    if PtInRect(rf, Point(X, Y)) then
      if Assigned(FOnFooterClick) then
        FOnFooterClick(Self, X, Y);
  end
  else
  begin
    if Assigned(FDragItemForm) then
    begin
      FDragItemForm.Free;
      FDragItemForm := nil;
      Selecteditem := nil;
      if FSelectedItemIndex <> -1 then
        selecteditem := Items[SelectedItemIndex];

      FMode := dmRelease;
      item := YToItem(X, Y, false);
      if item <> -1 then
      begin
        DItem := FDisplayList.GetItem(item).DisplayItem;
        AllowDrop := true;
        if Assigned(FOnItemDragDrop) then
          if FSelectedDragItem <> nil then
            FOnItemDragDrop(Self, FSelectedDragItem.Index, DItem.Index, AllowDrop);

        if AllowDrop and (FSelectedDragItem <> nil) and (DItem <> nil) then
        begin
          FSelectedDragItem.Index := DItem.Index;

          if (selecteditem <> nil) and (FSelectedItemIndex <> -1) then
            FSelectedItemIndex := selecteditem.Index;

          InitDisplayList;
          CalculateRects;
          Changed;
        end;
      end
      else
      begin
        if Assigned(FOnItemDragEnd) then
          if FSelectedDragItem <> nil then
            FOnItemDragEnd(Self, FSelectedDragItem.Index);
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if FDetailStatus = dsDetailsNotVisible then
      begin
        if integer(Message.WParam) < 0 then
        begin
          FSp := FSpeedFactor;
          FScPosTo := FScPosTo + GetItemHeight(nil)
        end
        else
        begin
          FSp := FSpeedFactor;
          FScPosTo := FScPosTo - GetItemHeight(nil);
        end;

        FAnimate := true;
      end;
    end;
  end;
end;

{$WARNINGS OFF}
// compiler generates incorrect warning
function NormalizeSpecialChar(s: string): string;
begin
  Result := s;

  if (AnsiCompareStr('A',s) < 0) and (AnsiCompareStr('B',s) > 0) then
    Result :=  'A'
  else
  if (AnsiCompareStr('C',s) < 0) and (AnsiCompareStr('D',s) > 0) then
    Result :=  'C'
  else
  if (AnsiCompareStr('E',s) < 0) and (AnsiCompareStr('F',s) > 0) then
    Result :=  'E'
  else
  if (AnsiCompareStr('G',s) < 0) and (AnsiCompareStr('H',s) > 0) then
    Result :=  'G'
  else
  if (AnsiCompareStr('H',s) < 0) and (AnsiCompareStr('I',s) > 0) then
    Result :=  'H'
  else
  if (AnsiCompareStr('I',s) < 0) and (AnsiCompareStr('J',s) > 0) then
    Result :=  'I'
  else
  if (AnsiCompareStr('J',s) < 0) and (AnsiCompareStr('K',s) > 0) then
    Result :=  'J'
  else
  if (AnsiCompareStr('K',s) < 0) and (AnsiCompareStr('L',s) > 0) then
    Result :=  'K'
  else
  if (AnsiCompareStr('L',s) < 0) and (AnsiCompareStr('M',s) > 0) then
    Result :=  'L'
  else
  if (AnsiCompareStr('N',s) < 0) and (AnsiCompareStr('N',s) > 0) then
    Result :=  'N'
  else
  if (AnsiCompareStr('O',s) < 0) and (AnsiCompareStr('P',s) > 0) then
    Result :=  'O'
  else
  if (AnsiCompareStr('P',s) < 0) and (AnsiCompareStr('Q',s) > 0) then
    Result :=  'P'
  else
  if (AnsiCompareStr('R',s) < 0) and (AnsiCompareStr('S',s) > 0) then
    Result :=  'R'
  else
  if (AnsiCompareStr('S',s) < 0) and (AnsiCompareStr('T',s) > 0) then
    Result :=  'S'
  else
  if (AnsiCompareStr('T',s) < 0) and (AnsiCompareStr('U',s) > 0) then
    Result :=  'T'
  else
  if (AnsiCompareStr('U',s) < 0) and (AnsiCompareStr('V',s) > 0) then
    Result :=  'U'
  else
  if (AnsiCompareStr('W',s) < 0) and (AnsiCompareStr('X',s) > 0) then
    Result :=  'W'
  else
  if (AnsiCompareStr('Z',s) < 0) then
    Result :=  'Z'
  else
    Result := s;

end;
{$WARNINGS ON}

procedure TAdvSmoothListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
 
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FControl) then
    FControl := nil;

  inherited;

  if (csDestroying in ComponentState) then
    Exit;


  if Assigned(FItemAppearance) then
    if Assigned(FItemAppearance.FButtonAppearance) then
      FItemAppearance.FButtonAppearance.DoNotification(Self, AComponent, AOperation);

  if (AOperation = opRemove) then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if (AComponent = Items[I].FControl) then
        Items[I].FControl := nil;
      if AComponent = Items[i].FPopupMenu then
        Items[i].FPopupMenu := nil;
    end;
  end;
end;

procedure TAdvSmoothListBox.Paint;
var
  rgn: THandle;
  org: TPoint;
begin
  DrawBackground;

  if FDisplayList.Count > 0 then
  begin
    GetWindowOrgEx(Canvas.Handle, org);
    rgn := CreateRectRgn(
      InsideRect.Left - org.X, InsideRect.Top + Header.GetHeight + Filter.GetHeight - org.Y,
      InsideRect.Right + 1 - org.X, InsideRect.Bottom - Footer.GetHeight - GetShadowOffset + 1 - org.Y);

//    rgn := CreateRectRgn(InsideRect.Left, InsideRect.Top + Header.GetHeight + Filter.GetHeight, InsideRect.Right + 1, InsideRect.Bottom - Footer.GetHeight - GetShadowOffset + 1);
    SelectClipRgn(Canvas.Handle,rgn);
    DrawItems;
    DrawScrollIndicator;
    DrawLookUpBar;
    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(rgn);
  end;

  DrawHeaderFooter(Header, true);
  DrawHeaderFooter(Footer, false);
  DrawFilter;
end;

function TAdvSmoothListBox.PtInGPRect(R: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TAdvSmoothListBox.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothListBox.Resize;
var
  r: TRect;
begin
  inherited;
  InitDisplayList;
  CalculateRects;
  if Assigned(Filter) then
    Filter.UpdateFilter;

  if Assigned(FCurrentControl) then
  begin
    case FDetailStatus of
      dsDetailsNotVisible: FCurrentControl.Left := Self.Width;
      dsDetailsVisible: FCurrentControl.Left := 0;
    end;

    r := Rect(InsideRect.Left, Header.GetHeight + Filter.GetHeight, InsideRect.Right, InsideRect.Bottom - Footer.GetHeight - Header.GetHeight - Filter.GetHeight);

    FCurrentControl.Width := r.Right + 1;
    FCurrentControl.Height := r.Bottom + 1;
  end;
end;

procedure TAdvSmoothListBox.Loaded;
begin
  inherited;
  if FOleDropTargetAssigned then
  begin
    FListBoxDropTarget.AcceptText := true;
    FListBoxDropTarget.AcceptFiles := true;
    FListBoxDropTarget.AcceptURLs := true;
  end;
end;

procedure TAdvSmoothListBox.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothListBox.LookupBarChanged(Sender: TObject);
begin
  InitDisplayList;
  CalculateRects;
  Changed;
end;

function TAdvSmoothListBox.LookupBarVisible: Boolean;
begin
  Result := FLookUpBar.Visible;
end;

procedure TAdvSmoothListBox.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothListBox.ScrollIndicatorChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBox.ScrollInView(ItemIndex: integer);
var
  i, sh: integer;
begin
  if (ItemIndex >= 0) and (ItemIndex <= Items.Count - 1) then
  begin
    sh := 0;
    if Sections.Visible then
      sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;

    for I := 0 to FDisplayList.Count - 1 do
    begin
      with FDisplayList.GetItem(i) do
      begin
        if DisplayItem <> nil then
        begin
          if DisplayItem.Index = ItemIndex then
          begin
            if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
              FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
            else if ItemRect.Top < GetPosition + Header.GetHeight + Filter.GetHeight then
              FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;

            if FScPosTo <> FCurrentScPos then
            begin
              FCurrentScPos := FScPosTo;
              ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
              FAnimate := true;
            end;
          end;
        end;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.ScrollToItem(ItemIndex: integer);
var
  i, sh: integer;
begin
  if (ItemIndex >= 0) and (ItemIndex <= Items.Count - 1) then
  begin

    sh := 0;
    if Sections.Visible then
      sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;

    FSelectedItemIndex := ItemIndex;
    Items.SelectedItem := FItems[FSelectedItemIndex];
    for I := 0 to FDisplayList.Count - 1 do
    begin
      with FDisplayList.GetItem(i) do
      begin
        if DisplayItem <> nil then
        begin
          if DisplayItem.Index = FSelectedItemIndex then
          begin
            if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
              FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
            else if ItemRect.Top < GetPosition + Header.GetHeight + Filter.GetHeight then
              FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;

            if FScPosTo <> FCurrentScPos then
            begin
              FCurrentScPos := FScPosTo;
              ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
              FAnimate := true;
            end;
          end;
        end;
      end;
    end;
    Changed;
  end;
end;

function TAdvSmoothListBox.SectionAtXY(X, Y: integer): TAdvSmoothListBoxDisplayListItem;
var
  i: Integer;
  it: TAdvSmoothListBoxDisplayListItem;
begin
  Result := nil;
  for i := 0 to FDisplayList.Count - 1 do
  begin
    it := FDisplayList.GetItem(i);
    if Assigned(it) and (it.DisplayItem = nil) then
    begin
      if PtInRect(it.ItemRect, Point(X, Y + GetPosition)) then
      begin
        Result := it;
        Break;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.SectionsChanged(Sender: TObject);
begin
  InitDisplayList;
  CalculateRects;
  Changed;
end;

procedure TAdvSmoothListBox.SetCategories(
  const Value: TAdvSmoothListBoxCategoryItems);
begin
  if FCategories <> value then
  begin
    FCategories.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetCategoryType(
  const Value: TAdvSmoothListBoxCategoryType);
begin
  if FCategoryType <> value then
  begin
    FCategoryType := Value;
    if FUpdateCount = 0 then
    begin
      if Assigned(FCurrentControl) then
        FCurrentControl.Left := width;
      LookupBar.InitLookupBar;
      InitDisplayList;
      CalculateRects;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothListBox.SetColorTones(ATones: TColorTones);
var
  I: Integer;
begin
  FMetroStyle := True;
  ItemAppearance.FillSelected.GlowGradientColor:= clNone;
  ItemAppearance.FillSelectedAlternate.GlowGradientColor:= clNone;
  Fill.BorderColor := ATones.Background.BorderColor;

  ScrollIndicator.GradientType := gtSolid;
  ScrollIndicator.Color := ATones.Selected.BrushColor;
  ScrollIndicator.Opacity := 255;

  LookupBar.Color := ATones.background.BrushColor;
  LookupBar.Colorto := ATones.background.BrushColor;
  LookupBar.FDisabledFont.Color := ATones.Background.TextColor;
  LookupBar.FFont.Color := ATones.Selected.BrushColor;

  LookupBar.Font.Name := GetMetroFont;
  LookupBar.Font.Color := ATones.Background.TextColor;

  Fill.Color := ATones.background.BrushColor;

  Header.Fill.Color := ATones.Selected.BrushColor;
  Header.Fill.ColorTo := ATones.Selected.BrushColor;
  Header.Font.Color := ATones.Selected.TextColor;
  Header.Fill.BorderColor := ATones.Selected.bordercolor;
  Header.Fill.ColorMirror := clNone;

  Filter.Fill.Color := ATones.background.BrushColor;
  Filter.Fill.BorderColor := ATones.background.bordercolor;

  Footer.Fill.Color := ATones.Selected.BrushColor;
  Footer.Fill.ColorTo := ATones.Selected.BrushColor;
  Footer.Font.Color := ATones.Selected.TextColor;
  Footer.Fill.BorderColor := ATones.Selected.bordercolor;
  Footer.Fill.ColorMirror := clNone;

  ItemAppearance.Fill.Color := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorTo := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorMirror := ATones.Background.BrushColor;
  ItemAppearance.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  ItemAppearance.Fill.BorderColor := ATones.Background.bordercolor;

  ItemAppearance.FillDisabled.Color := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.ColorTo := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.ColorMirror := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.ColorMirrorTo := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.BorderColor := ATones.Disabled.bordercolor;

  ItemAppearance.FillSelected.Color := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorMirror := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorMirrorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.BorderColor := ATones.Selected.BorderColor;

  ItemAppearance.FillSelectedAlternate.Color := ATones.Selected.BrushColor;
  ItemAppearance.FillSelectedAlternate.ColorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelectedAlternate.ColorMirror := ATones.Selected.BrushColor;
  ItemAppearance.FillSelectedAlternate.ColorMirrorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelectedAlternate.BorderColor := ATones.Selected.BorderColor;

  for I := 0 to Items.Count - 1 do
  begin
    Items[I].CaptionFont.Color := Atones.Background.TextColor;
    Items[I].CaptionSelectedFont.Color := Atones.Selected.TextColor;
    Items[I].NotesFont.Color := Atones.Background.TextColor;
    Items[I].NotesSelectedFont.Color := Atones.Selected.TextColor;
    Items[I].InfoFont.Color := Atones.Background.TextColor;
    Items[I].InfoSelectedFont.Color := Atones.Selected.TextColor;
  end;

  DefaultItem.CaptionFont.Name := GetMetroFont;
  DefaultItem.InfoFont.Name := GetMetroFont;
  Header.Font.Name := GetMetroFont;
  Footer.Font.Name := GetMetroFont;


  with ItemAppearance do
  begin
    ProgressAppearance.Overlays := False;
    ProgressAppearance.Shadows := False;
    ProgressAppearance.BackGroundFill.Color := ATones.Foreground.BrushColor;
    ProgressAppearance.BackGroundFill.ColorTo := ATones.Foreground.BrushColor;
    ProgressAppearance.BackGroundFill.ColorMirror := ATones.Foreground.BrushColor;
    ProgressAppearance.BackGroundFill.ColorMirrorTo := ATones.Foreground.BrushColor;
    ProgressAppearance.BackGroundFill.BorderColor := ATones.Foreground.BorderColor;

    ProgressAppearance.ProgressFill.Color := ATones.Selected.BrushColor;
    ProgressAppearance.ProgressFill.ColorTo := ATones.Selected.BrushColor;
    ProgressAppearance.ProgressFill.ColorMirror := ATones.Selected.BrushColor;
    ProgressAppearance.ProgressFill.ColorMirrorTo := ATones.Selected.BrushColor;
    ProgressAppearance.ProgressFill.BorderColor := ATones.Selected.BorderColor;
    ProgressAppearance.Font.Color := ATones.Selected.TextColor;
  end;
end;

procedure TAdvSmoothListBox.SetComponentStyle(AStyle: TTMSStyle);
var
  I: Integer;
begin
  FTMSStyle := AStyle;
  FMetroStyle := False;
  // TODO : do color settings here
  SetProgressStyle(AStyle, false);

  ItemAppearance.FillSelected.Glow := gmNone;
  ItemAppearance.FillSelectedAlternate.Glow := gmNone;
  ItemAppearance.Fill.Glow := gmNone;
  ItemAppearance.FillDisabled.Glow := gmNone;

  Header.Fill.ColorMirror := clNone;
  Footer.Fill.ColorMirror := clNone;

      for I := 0 to Items.Count - 1 do

      begin
        Items[I].CaptionFont.Color := clBlack;
        Items[I].CaptionSelectedFont.Color := clBlack;
        Items[I].NotesFont.Color := clBlack;
        Items[I].NotesSelectedFont.Color := clBlack;
        Items[I].InfoFont.Color := clBlack;
        Items[I].InfoSelectedFont.Color := clBlack;
      end;
  
  case AStyle of
    tsOffice2003Blue: 
      begin
        Fill.Color := $00FFD2AF;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $D68759;
        Header.Fill.ColorTo := $933803;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $962D00;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $D68759;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $962D00;

        Footer.Fill.Color := $D68759;
        Footer.Fill.ColorTo := $933803;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $962D00;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $FCE1CB;
        ItemAppearance.Fill.ColorTo := $E0A57D;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $962D00;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $962D00;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $94E6FB;
        ItemAppearance.FillSelected.ColorTo := $1595EE;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $962D00;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver: 
      begin
        Fill.Color := $00E6D8D8;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $BDA4A5;
        Header.Fill.ColorTo := $957475;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $947C7C;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $BDA4A5;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $947C7C;

        Footer.Fill.Color := $BDA4A5;
        Footer.Fill.ColorTo := $957475;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $947C7C;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $ECE2E1;
        ItemAppearance.Fill.ColorTo := $B39698;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $947C7C;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $947C7C;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $94E6FB;
        ItemAppearance.FillSelected.ColorTo := $1595EE;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $947C7C;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
                
      end;
    tsOffice2003Olive: 
      begin
        Fill.Color := $CFF0EA;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $82C0AF;
        Header.Fill.ColorTo := $447A63;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $588060;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $82C0AF;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $588060;

        Footer.Fill.Color := $82C0AF;
        Footer.Fill.ColorTo := $447A63;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $588060;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $CFF0EA;
        ItemAppearance.Fill.ColorTo := $8CC0B1;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $588060;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $588060;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $94E6FB;
        ItemAppearance.FillSelected.ColorTo := $1595EE;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $588060;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
               
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := $00F2F2F2;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $808080;
        Header.Fill.ColorTo := $808080;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $808080;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $808080;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $808080;

        Footer.Fill.Color := $808080;
        Footer.Fill.ColorTo := $808080;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $808080;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := $C9D1D5;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $808080;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $D8D5D4;
        ItemAppearance.FillDisabled.ColorTo := $D8D5D4;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $808080;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $B59285;
        ItemAppearance.FillSelected.ColorTo := $B59285;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $962D00;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna: 
      begin          
        Fill.Color := $00FFD2AF;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $FFEFE3;
        Header.Fill.ColorTo := $FFD2AF;
        Header.Font.Color := $723708;
        Header.Fill.BorderColor := $00FFD2AF;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $FFEFE3;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $00FFD2AF;

        Footer.Fill.Color := $FFEFE3;
        Footer.Fill.ColorTo := $FFD2AF;
        Footer.Font.Color := $723708;
        Footer.Fill.BorderColor := $00FFD2AF;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $FFEFE3;
        ItemAppearance.Fill.ColorTo := $FFDDC4;        
        ItemAppearance.Fill.ColorMirror := $FFD1AD;
        ItemAppearance.Fill.ColorMirrorTo := $FFDBC0; 
        ItemAppearance.Fill.BorderColor := $FFD1AD;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FillDisabled.BorderColor := $FFD1AD;//$00B6B6B6;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $FFD1AD;//$42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;        
      end;
    tsOffice2007Obsidian: 
      begin
        Fill.Color := $5C534C;
        Fill.GradientType := gtSolid;
                
        Header.Fill.Color := $F2F1F0;
        Header.Fill.ColorTo := $C9C2BD;
        Header.Font.Color := $433C37;
        Header.Fill.BorderColor := $5C534C;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $F2F1F0;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $5C534C;

        Footer.Fill.Color := $F2F1F0;
        Footer.Fill.ColorTo := $C9C2BD;
        Footer.Font.Color := $433C37;
        Footer.Fill.BorderColor := $5C534C;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $F9F8F8;
        ItemAppearance.Fill.ColorTo := $E4E2DF;
        ItemAppearance.Fill.ColorMirror := $D1CBC7;
        ItemAppearance.Fill.ColorMirrorTo := $E2DEDB;
        ItemAppearance.Fill.BorderColor := clBlack;//$D1CBC7;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.Fillselected.Color := $AAD9FF;
        ItemAppearance.Fillselected.ColorTo := $6EBBFF;
        ItemAppearance.Fillselected.ColorMirror := $42AEFE;
        ItemAppearance.Fillselected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := clBlack;//$42AEFE;          
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;               

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FillDisabled.BorderColor := clBlack;//$00B6B6B6;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
    
      end;
    tsWindowsXP: 
      begin
        Fill.Color := $00B6B6B6;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := clBtnFace;
        Header.Fill.ColorTo := clBtnFace;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clBlack;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := clBtnFace;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := clBlack;

        Footer.Fill.Color := clBtnFace;
        Footer.Fill.ColorTo := clBtnFace;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clBlack;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fillselected.Color := clInActiveCaption;
        ItemAppearance.Fillselected.ColorTo := clInActiveCaption;
        ItemAppearance.Fillselected.ColorMirror := clNone;
        ItemAppearance.Fillselected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := clBlack;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clBtnFace;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := clBlack;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := clBlack;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        Fill.Color := $F5F9FA;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $EBEEEF;
        Header.Fill.ColorTo := $7E9898;
        Header.Font.Color := clWhite;
        Header.Fill.BorderColor := $962D00;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $EBEEEF;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $962D00;

        Footer.Fill.Color := $EBEEEF;
        Footer.Fill.ColorTo := $7E9898;
        Footer.Font.Color := clWhite;
        Footer.Fill.BorderColor := $962D00;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $F5F9FA;
        ItemAppearance.Fill.ColorTo := $A8C0C0;        
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone; 
        ItemAppearance.Fill.BorderColor := $962D00;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $962D00;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $94E6FB;
        ItemAppearance.FillSelected.ColorTo := $1595EE;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $962D00;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver: 
      begin
        Fill.Color := $00CAC1BA;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $F8F7F6;
        Header.Fill.ColorTo := $E8E0DB;
        Header.Font.Color := $8B4215;
        Header.Fill.BorderColor := $74706F;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $F8F7F6;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $74706F;

        Footer.Fill.Color := $F8F7F6;
        Footer.Fill.ColorTo := $E8E0DB;
        Footer.Font.Color := $8B4215;
        Footer.Fill.BorderColor := $74706F;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $FAEEEB;
        ItemAppearance.Fill.ColorTo := $E5DBD7;
        ItemAppearance.Fill.ColorMirror := $E2D8D4;
        ItemAppearance.Fill.ColorMirrorTo := $D1C7C5;
        ItemAppearance.Fill.BorderColor := clBlack;//$E2D8D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.Fillselected.Color := $AAD9FF;
        ItemAppearance.Fillselected.ColorTo := $6EBBFF;
        ItemAppearance.Fillselected.ColorMirror := $42AEFE;
        ItemAppearance.Fillselected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := clBlack;//$42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FillDisabled.BorderColor := clBlack;//$00B6B6B6;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;

      tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.GradientType := gtVertical;

        Header.Fill.Color := $FCF9F2;
        Header.Fill.ColorTo := $FCF9F2;
        Header.Fill.ColorMirror := $F7EED9;
        Header.Fill.ColorMirrorTo := $F7EED9;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $F9D996;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.GradientMirrorType := gtVertical;

        Filter.Fill.Color := $FCF9F2;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $F9D996;

        Footer.Fill.Color := $FCF9F2;
        Footer.Fill.ColorTo := $FCF9F2;
        Footer.Fill.ColorMirror := $F7EED9;
        Footer.Fill.ColorMirrorTo := $F7EED9;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $F9D996;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.Fill.Color := $FEFCF8;
        ItemAppearance.Fill.ColorTo := $FDF5E8;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $FAF0D8;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $FBFAFA;
        ItemAppearance.FillDisabled.ColorTo := $E6E6E6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $D9D9D9;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FDF8F1;
        ItemAppearance.FillSelected.ColorTo := $FCEFD5;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $FDDE99;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.GradientType := gtVertical;

        Header.Fill.Color := $FCF9F2;
        Header.Fill.ColorTo := $FCF9F2;
        Header.Fill.ColorMirror := $F7EED9;
        Header.Fill.ColorMirrorTo := $F7EED9;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $F9D996;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.GradientMirrorType := gtVertical;

        Filter.Fill.Color := $FCF9F2;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $F9D996;

        Footer.Fill.Color := $FCF9F2;
        Footer.Fill.ColorTo := $FCF9F2;
        Footer.Fill.ColorMirror := $F7EED9;
        Footer.Fill.ColorMirrorTo := $F7EED9;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $F9D996;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.Fill.Color := $FCEBDC;
        ItemAppearance.Fill.ColorTo := $FCDBC1;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $CEA27D;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $FBFAFA;
        ItemAppearance.FillDisabled.ColorTo := $E6E6E6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $D9D9D9;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FDFBFA;
        ItemAppearance.FillSelected.ColorTo := $FDF3EB;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $FBD6B8;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := clBtnFace;
        Header.Fill.ColorTo := clBtnFace;
        Header.Fill.ColorMirror := clNone;
        Header.Fill.ColorMirrorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := clGray;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := clBtnFace;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := clGray;

        Footer.Fill.Color := clBtnFace;
        Footer.Fill.ColorTo := clBtnFace;
        Footer.Fill.ColorMirror := clNone;
        Footer.Fill.ColorMirrorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := clGray;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;


        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clWhite;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := clGray;


        ItemAppearance.FillDisabled.Color := clWhite;
        ItemAppearance.FillDisabled.ColorTo := clWhite;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := clNone;


        ItemAppearance.FillSelected.Color := clHighLight;
        ItemAppearance.FillSelected.ColorTo := clHighLight;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := clBlack;
      end;
      tsOffice2010Blue:

        begin
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $67BCF6;

        Fill.Color := $EAD3BF;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $FDF6EF;
        Header.Fill.ColorTo := $F0DAC7;
        Header.Font.Color := $5B391E;
        Header.Fill.BorderColor := $C7B29F;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $FDF6EF;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $C7B29F;

        Footer.Fill.Color := $FDF6EF;
        Footer.Fill.ColorTo := $F0DAC7;
        Footer.Font.Color := $5B391E;
        Footer.Fill.BorderColor := $C7B29F;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $962D00;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $6CD0FF;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $308AC2;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $7BEEFF;

        end;
        tsOffice2010Silver:
        begin
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $67BCF6;

        Fill.Color := $D4CFCB;
        Fill.GradientType := gtSolid;

        Header.Fill.Color := $FFFFFF;
        Header.Fill.ColorTo := $EDE5E0;
        Header.Font.Color := $5B391E;
        Header.Fill.BorderColor := $D2CDC8;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $FFFFFF;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $D2CDC8;

        Footer.Fill.Color := $FFFFFF;
        Footer.Fill.ColorTo := $EDE5E0;
        Footer.Font.Color := $5B391E;
        Footer.Fill.BorderColor := $D2CDC8;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $962D00;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $6CD0FF;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $308AC2;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $7BEEFF;
      end;
      tsOffice2010Black:
      begin
        ItemAppearance.Fill.Glow := gmGradient;
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $67BCF6;

        Fill.Color := $656565;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $BFBFBF;
        Header.Fill.ColorTo := $919191;
        Header.Font.Color := $D7D7D6;
        Header.Fill.BorderColor := $6D6D6D;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $BFBFBF;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $6D6D6D;

        Footer.Fill.Color := $BFBFBF;
        Footer.Fill.ColorTo := $919191;
        Footer.Font.Color := $D7D7D6;
        Footer.Fill.BorderColor := $6D6D6D;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := RGB(237, 239, 241);
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := RGB(236, 237, 237);
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $962D00;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $6CD0FF;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $308AC2;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $7BEEFF;
      end;
    tsWindows8, tsWindows10:
      begin
        ItemAppearance.Fill.Glow := gmGradient;


        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $F7F6F5;
        Header.Fill.ColorTo := $F7F6F5;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $E4E3E2;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := clWhite;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $E4E3E2;

        Footer.Fill.Color := $F7F6F5;
        Footer.Fill.ColorTo := $F7F6F5;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $E4E3E2;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clWhite;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $E4E3E2;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $F7F7F7;
        ItemAppearance.FillDisabled.ColorTo := $F7F7F7;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $DEDEDE;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $F7E0C9;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E4A262;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $F7E0C9;
      end;
    tsOffice2013White:
      begin
        ItemAppearance.Fill.Glow := gmGradient;

        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $D4D4D4;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := clWhite;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $D4D4D4;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $D4D4D4;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $D4D4D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $FCE2C8;
      end;
    tsOffice2013LightGray:
      begin
         ItemAppearance.Fill.Glow := gmGradient;

        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $F6F6F6;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $C6C6C6;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $FAFAFA;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $C6C6C6;

        Footer.Fill.Color := $F6F6F6;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $C6C6C6;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $D4D4D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $FCE2C8;
      end;
    tsOffice2013Gray:
      begin
         ItemAppearance.Fill.Glow := gmGradient;

        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $E5E5E5;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := clBlack;
        Header.Fill.BorderColor := $ABABAB;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $F3F3F3;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $ABABAB;

        Footer.Fill.Color := $E5E5E5;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := clBlack;
        Footer.Fill.BorderColor := $ABABAB;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $D4D4D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $FCE2C8;
        end;
   tsOffice2016White:
      begin
        ItemAppearance.Fill.Glow := gmNone;

        Fill.Color := clWhite;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := clWhite;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $505050;
        Header.Fill.BorderColor := $D4D4D4;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := clWhite;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $D4D4D4;

        Footer.Fill.Color := clWhite;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $505050;
        Footer.Fill.BorderColor := $D4D4D4;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := clWhite;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $D4D4D4;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := clWhite;
        ItemAppearance.FillDisabled.ColorTo := clNone;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $D4D4D4;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $E3BDA3;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E3BDA3;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
      end;
    tsOffice2016Gray:
      begin
        ItemAppearance.Fill.Glow := gmNone;

        Fill.Color := $D4D4D4;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $444444;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $F0F0F0;
        Header.Fill.BorderColor := $444444;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $444444;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $383838;

        Footer.Fill.Color := $444444;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $F0F0F0;
        Footer.Fill.BorderColor := $444444;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $B2B2B2;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $444444;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $B2B2B2;
        ItemAppearance.FillDisabled.ColorTo := clNone;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $444444;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $E3BDA3;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E3BDA3;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
      end;
    tsOffice2016Black:
      begin
        ItemAppearance.Fill.Glow := gmNone;

        Fill.Color := $363636;
        Fill.GradientType := gtSolid;


        Header.Fill.Color := $444444;
        Header.Fill.ColorTo := clNone;
        Header.Font.Color := $F0F0F0;
        Header.Fill.BorderColor := $444444;
        Header.Fill.GradientType := gtVertical;
        Header.Fill.ColorMirror := clNone;

        Filter.Fill.Color := $444444;
        Filter.Fill.GradientType := gtSolid;
        Filter.Fill.BorderColor := $363636;

        Footer.Fill.Color := $444444;
        Footer.Fill.ColorTo := clNone;
        Footer.Font.Color := $F0F0F0;
        Footer.Fill.BorderColor := $444444;
        Footer.Fill.GradientType := gtVertical;
        Footer.Fill.ColorMirror := clNone;

        ItemAppearance.Fill.Color := $363636;
        ItemAppearance.Fill.ColorTo := clNone;
        ItemAppearance.Fill.ColorMirror := clNone;
        ItemAppearance.Fill.ColorMirrorTo := clNone;
        ItemAppearance.Fill.BorderColor := $444444;;
        ItemAppearance.Fill.GradientMirrorType := gtVertical;


        ItemAppearance.FillDisabled.Color := $363636;
        ItemAppearance.FillDisabled.ColorTo := clNone;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $444444;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $444444;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $444444;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.FillSelected.Glow:= gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;

      for I := 0 to Items.Count - 1 do

      begin
        Items[I].CaptionFont.Color := $FFFFFF;
        Items[I].CaptionSelectedFont.Color := $FFFFFF;
        Items[I].NotesFont.Color := $FFFFFF;;
        Items[I].NotesSelectedFont.Color := $FFFFFF;
        Items[I].InfoFont.Color := $FFFFFF;
        Items[I].InfoSelectedFont.Color := $FFFFFF;
      end;

        end;

      end;


      ItemAppearance.FillAlternate.Assign(ItemAppearance.Fill);
      ItemAppearance.FillSelectedAlternate.Assign(ItemAppearance.FillSelected);

      for I := 0 to Items.Count - 1 do
      begin
        Items[I].ButtonColor := Header.Fill.BorderColor;
      end;
  end;


procedure TAdvSmoothListBox.SetControl(const Value: TControl);
begin
  if FControl <> value then
  begin
    FControl := Value;

//    FControl.Left   := InsideRect.Right;
//    FControl.Top    := Header.GetHeight;
//    FControl.Width  := Width;
//    FControl.Height := Height - Header.GetHeight - Footer.GetHeight;
//    FControl.Visible := false;

    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetCursorEx(const Value: TCursor);
begin
  inherited Cursor := Value;
  if not keepoldcursor then
    FoldCursor := Value;
end;

procedure TAdvSmoothListBox.SetDefaultItem(const Value: TAdvSmoothListBoxItem);
begin
  if FDefaultItem <> value then
  begin
    FDefaultItem.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDetailItemImage(const Value: TAdvGDIPPicture);
begin
  if FDetailItemImage <> value then
  begin
    FDetailItemImage.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDetailSpeedFactor(const Value: integer);
begin
  if FDetailSpeedFactor <> value then
  begin
    FDetailSpeedFactor := Max(0, Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDirectSelectedItemIndex(Value: Integer);
begin
  FSelectedItemIndex := Value;
end;

procedure TAdvSmoothListBox.SetDragAlphaBlend(const Value: Boolean);
begin
  if FDragAlphaBlend <> value then
  begin
    FDragAlphaBlend := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDragBorderColor(const Value: TColor);
begin
  if FDragBorderColor <> value then
  begin
    FDragBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDragBorderWidth(const Value: integer);
begin
  if FDragBorderWidth <> value then
  begin
    FDragBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetItemDragging(const Value: Boolean);
begin
  if FItemDragging <> value then
  begin
    FItemDragging := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDragOpacity(const Value: Byte);
begin
  if FDragOpacity <> value then
  begin
    FDragOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetDropDownControlClass(
  const Value: TDropDownControlClass);
var
  I: Integer;
begin
  FDropDownControlClass := value;
  if Assigned(DropDownControlClass) then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      with Items[I] do
      begin
        if Assigned(FDropDownControl) and FCreatedThroughClass then
        begin
          FDropDownControl.Free;
          FDropDownControl := nil;
          FCreatedThroughClass := False;
        end;
        CreateDropDownItem;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.SetEnableDragging(const Value: Boolean);
begin
  if FEnableDragging <> Value then
  begin
    FEnableDragging := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetFilter(const Value: TAdvSmoothListBoxFilter);
begin
  if FFilter <> Value then
  begin
    FFilter.Assign(Value);
    FilterChanged(Self);
  end;
end;

procedure TAdvSmoothListBox.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetFooter(
  const Value: TAdvSmoothListBoxHeaderFooter);
begin
  if FFooter <> value then
  begin
    FFooter.Assign(Value);
    HeaderFooterChanged(self);
  end;
end;

procedure TAdvSmoothListBox.SetHeader(const Value: TAdvSmoothListBoxHeaderFooter);
begin
  if FHeader <> value then
  begin
    FHeader.Assign(Value);
    HeaderFooterChanged(self);
  end;
end;

procedure TAdvSmoothListBox.SetItemAppearance(
  const Value: TAdvSmoothListBoxItemAppearance);
begin
  if FItemAppearance <> value then
  begin
    FItemAppearance.Assign(Value);
    ItemAppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothListBox.SetItemImage(const Value: TAdvGDIPPicture);
begin
  if FItemImage <> value then
  begin
    FItemImage.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetItems(const Value: TAdvSmoothListBoxItems);
begin
  if FItems <> value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetKeyBoardLookup(const Value: Boolean);
begin
  if FKeyBoardLookup <> value then
  begin
    FKeyBoardLookup := Value;
    FLookupKey := '';
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetLayout(const Value: TAdvSmoothListBoxLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if Layout = lblBubble then
    begin
      ItemAppearance.Height := 50;
      ItemAppearance.VerticalSpacing := 10;
      ItemAppearance.Fill.Rounding := 4;
      ItemAppearance.Fill.Glow := gmGradient;
      ItemAppearance.FillAlternate.Rounding := 4;
      ItemAppearance.FillAlternate.Glow := gmGradient;
      ItemAppearance.FillSelected.Rounding := 4;
      ItemAppearance.FillSelected.Glow := gmGradient;
      ItemAppearance.FillSelectedAlternate.Rounding := 4;
      ItemAppearance.FillSelectedAlternate.Glow := gmGradient;
    end
    else
    begin
      ItemAppearance.Height := 30;
      ItemAppearance.VerticalSpacing := 0;
      ItemAppearance.Fill.Rounding := 0;
      ItemAppearance.FillAlternate.Rounding := 0;
      ItemAppearance.FillSelected.Rounding := 0;
      ItemAppearance.FillSelectedAlternate.Rounding := 0;

      ItemAppearance.Fill.Glow := gmNone;
      ItemAppearance.FillAlternate.Glow := gmNone;
      ItemAppearance.FillSelected.Glow := gmNone;
      ItemAppearance.FillSelectedAlternate.Glow := gmNone;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetLayoutIndenting(const Value: Integer);
begin
  if FLayoutIndenting <> Value then
  begin
    FLayoutIndenting := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetLookUpBar(
  const Value: TAdvSmoothListBoxLookUpBar);
begin
  if FLookUpBar <> value then
  begin
    FLookUpBar.Assign(Value);
    LookupBarChanged(Self);
  end;
end;

procedure TAdvSmoothListBox.SetMouseSelect(
  const Value: TAdvSmoothListBoxMouseSelect);
begin
  FMouseSelect := Value;
  case Value of
  lmsLeft: FTriggerButtons := [mbLeft];
  lmsRight: FTriggerButtons := [mbRight];
  lmsBoth: FTriggerButtons := [mbRight,mbLeft];
  end;
end;

procedure TAdvSmoothListBox.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> value then
  begin
    FMultiSelect := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothListBox.SetOleDragDrop(const Value: Boolean);
var
  d: Boolean;
begin
  d := Value;

  if not (csDesigning in ComponentState) then
  begin
    if d and not FOleDragDrop then
    begin
      FOleDragDrop := true;
      FListBoxDropTarget := TAdvSmoothListBoxDropTarget.Create(Self);
      FOleDropTargetAssigned := RegisterDragDrop(Handle, FListBoxDropTarget) = S_OK;
    end
    else if not d and FOleDropTargetAssigned then
    begin
      FOleDragDrop := false;
      RevokeDragDrop(Handle);
    end;
  end
  else
    FOleDragDrop := d;
end;

procedure TAdvSmoothListBox.SetParentEx(const Value: TWinControl);
begin
  inherited Parent := Value;
end;

procedure TAdvSmoothListBox.SetProgressStyle(AStyle: TTMSStyle;
  Selected: Boolean);
begin
  with ItemAppearance.ProgressAppearance do
  begin
    case AStyle of
      tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FCE1CB;
          ProgressFill.ColorTo := $E0A57D;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $ECE2E1;
          ProgressFill.ColorTo := $B39698;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Olive:
      begin
        BackGroundFill.Color := $CFF0EA;
        BackGroundFill.ColorTo := $CFF0EA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $CFF0EA;
          ProgressFill.ColorTo := $8CC0B1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := $C9D1D5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $808080;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $B59285;
          ProgressFill.ColorTo := $B59285;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FFEFE3;
          ProgressFill.ColorTo := $FFDDC4;
          ProgressFill.ColorMirror := $FFD1AD;
          ProgressFill.ColorMirrorTo := $FFDBC0;
          ProgressFill.BorderColor := $FFD1AD;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F9F8F8;
          ProgressFill.ColorTo := $E4E2DF;
          ProgressFill.ColorMirror := $D1CBC7;
          ProgressFill.ColorMirrorTo := $E2DEDB;
          ProgressFill.BorderColor := clBlack;//$D1CBC7;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clBtnFace;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := clInActiveCaption;
          ProgressFill.ColorTo := clInActiveCaption;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F5F9FA;
          ProgressFill.ColorTo := $A8C0C0;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsCustom: ;
      tsOffice2007Silver:
      begin
        BackGroundFill.Color := $00CAC1BA;
        BackGroundFill.ColorTo := $00CAC1BA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FAEEEB;
          ProgressFill.ColorTo := $E5DBD7;
          ProgressFill.ColorMirror := $E2D8D4;
          ProgressFill.ColorMirrorTo := $D1C7C5;
          ProgressFill.BorderColor := clBlack;//$E2D8D4;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsVista:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not Selected then
        begin
          ProgressFill.Color := $FDF8F1;
          ProgressFill.ColorTo := $FCEFD5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FDDE99;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FEF9F0;
          ProgressFill.ColorTo := $FDF0D7;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FEDF9A;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindows7:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not Selected then
        begin
          ProgressFill.Color := $FDFBFA;
          ProgressFill.ColorTo := $FDF3EB;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FBD6B8;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clGray;

        if not Selected then
        begin
          ProgressFill.Color := clSilver;
          ProgressFill.ColorTo := clSilver;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end
        else
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clWhite;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end;
      end;
      tsWindows8, tsWindows10:
      begin
        BackGroundFill.Color := $F7F6F5;
        BackGroundFill.ColorTo := $F7F6F5;
        BackGroundFill.BorderColor := $E4E3E2;

        if not Selected then
        begin
          ProgressFill.Color := $F7E0C9;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clNone;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2013White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        if not Selected then
        begin
          ProgressFill.Color := $FCE2C8;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clNone;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
     tsOffice2013LightGray:
      begin
        BackGroundFill.Color := $F6F6F6;
        BackGroundFill.ColorTo := $F6F6F6;
        BackGroundFill.BorderColor := $C6C6C6;

        if not Selected then
        begin
          ProgressFill.Color := $FCE2C8;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clNone;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2013Gray:
      begin
        BackGroundFill.Color := $E5E5E5;
        BackGroundFill.ColorTo := $E5E5E5;
        BackGroundFill.BorderColor := $ABABAB;

        if not Selected then
        begin
          ProgressFill.Color := $FCE2C8;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clNone;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2016White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        if not Selected then
        begin
          ProgressFill.Color := $F2E1D5;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $F2E1D5;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $E3BDA3;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $E3BDA3;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
     tsOffice2016Gray:
      begin
        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        if not Selected then
        begin
          ProgressFill.Color := $F2E1D5;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $F2E1D5;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $E3BDA3;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $E3BDA3;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
    tsOffice2016Black:
      begin
        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $444444;

        if not Selected then
        begin
          ProgressFill.Color := $6A6A6A;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $6A6A6A;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $444444;
          ProgressFill.ColorTo := clNone;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $444444;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.SetScPosTo(const Value: Integer);
begin
  FSp := SpeedFactor;
  FAnimate := True;
  FScPosTo := Value;
end;

procedure TAdvSmoothListBox.SetScrollIndicator(
  const Value: TAdvSmoothListBoxIndicator);
begin
  if FScrollIndicator <> value then
  begin
    FScrollIndicator.Assign(Value);
    ScrollIndicatorChanged(Self);
  end;
end;

procedure TAdvSmoothListBox.SetSections(const Value: TAdvSmoothListBoxSections);
begin
  if FSections <> value then
  begin
    FSections.Assign(Value);
    SectionsChanged(Self);
  end;
end;

procedure TAdvSmoothListBox.SetSelectedItemIndex(const Value: integer);
var
  prev: integer;
begin
  if (Value >= 0) and (Value <= Items.Count - 1) then
  begin
//    if value <> FSelectedItemIndex then
    begin
      prev := FSelectedItemIndex;
      if (prev >= 0) and (prev <= Items.Count -1) then
        Items[prev].Selected := false;
      Items.SelectedItem := Items[Value];
      Items.SelectedItem.Selected := true;
      if Assigned(FOnItemSelectionChanged) then
        FOnItemSelectionChanged(Self, prev, Value);
    end;
  end
  else if (Value = -1) then
  begin
    FSelectedItemIndex := Value;
    Items.SelectedItem := nil;
  end;
end;

procedure TAdvSmoothListBox.SetSelectionMode(
  const Value: TAdvSmoothListBoxSelectionMode);
begin
  if FSelectionMode <> value then
  begin
    FSelectionMode := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetShowDetailClick(
  const Value: TAdvSmoothListBoxShowDetailClick);
begin
  if FShowDetailClick <> value then
  begin
    FShowDetailClick := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetShowDetailKey(
  const Value: TAdvSmoothListBoxShowDetailKey);
begin
  if FShowDetailKey <> value then
  begin
    FShowDetailKey := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> value then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetSorted(const Value: Boolean);
begin
  if FSorted <> value then
  begin
    FSorted := Value;
    if Value = True then
    begin
      Items.Sort;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetSpeedFactor(const Value: integer);
begin
  if FSpeedFactor <> Value then
  begin
    FSpeedFactor := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBox.SetSplitterHeight(const Value: integer);
begin
  if FSplitterHeight <> value then
  begin
    FSplitterHeight := Value;
    InitDisplayList;
    CalculateRects;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.ShowDetails(itemindex: integer = -1; const bypasstimer: Boolean = false);
var
  op,
  p: TWinControl;
  t: Integer;
  r: TRect;
  ditem: TAdvSmoothListBoxItem;
begin
  if (itemindex = -1) then
    Ditem := Items.SelectedItem
  else if (itemindex >= 0) and (itemindex <= Items.Count - 1) then
    DItem := Items[itemindex]
  else
    Exit;

  if DItem <> nil then
  begin
    if Assigned(DItem.DetailControl) then
      FCurrentControl := DItem.DetailControl
    else if Assigned(DetailControl) then
      FCurrentControl := DetailControl
    else
      Exit;
  end
  else if Assigned(DetailControl) then
    FCurrentControl := DetailControl
  else
    Exit;

  if DItem <> nil then
  begin
    FDetailIndex := DItem.Index;
    FDetailShow := true;
    FEnableDetail := True;
    FDetailStatus := dsDetailsVisible;

    if FCurrentControl.Parent <> self then
    begin
      FCurrentControl.Width := 0;
      FCurrentControl.Height := 0;
      FCurrentControl.Visible := false;
      FCurrentControl.Parent := self;
    end;

    FCurrentControl.Left := InsideRect.Right;

    if not Assigned(FAnimateBitmap) then
    begin
      FAnimateBitmap := TBitmap.Create;
      r := Rect(InsideRect.Left, Header.GetHeight + Filter.GetHeight, InsideRect.Right, InsideRect.Bottom - Footer.GetHeight - Header.GetHeight - Filter.GetHeight);
      FAnimateBitmap.Width := r.Right;
      FAnimateBitmap.Height := r.Bottom;
      FBypassBitBlt:= True;
      try
        if Visible then
        begin
          Paint;
          BitBlt(FAnimateBitmap.Canvas.Handle,0,0,r.Right, r.Bottom, Self.Canvas.Handle, r.Left, r.Top, SRCCopy)
        end
        else
        begin
            op:= Parent;

            p:= Parent;
            while Assigned(p) and not (p is TForm) do
              p:= p.Parent;

            if Assigned(p) then
              Parent:= p;

            Show;
            Invalidate;
            Paint;
            BitBlt(FAnimateBitmap.Canvas.Handle,0,0,r.Right, r.Bottom, Self.Canvas.Handle, r.Left, r.Top, SRCCopy);
            Hide;
            if Parent<>op then
              Parent:= op;
        end;
      finally
        FBypassBitBlt:= False;
      end;

    end;

    DoShowDetail(self, FDetailIndex);

    if bypasstimer then
    begin
      t:= SpeedFactor;
      SpeedFactor:= 1;
      AnimateSmoothPos(FSmoothTimer);
      SpeedFactor:= t;
    end;
  end;
end;

procedure TAdvSmoothListBox.TimerEvent(Sender: TObject);
begin
  FDeactivating := false;
  FTimer.Enabled := false;
end;

procedure TAdvSmoothListBox.WMContextMenu(var Message: TWMContextMenu);
var
  popupsi: Integer;
  shown: Boolean;
  pt: TPoint;
begin
  shown := false;
  pt := Point(Message.XPos, Message.YPos);
  pt := ScreenToClient(pt);
  popupsi := ItemAtXY(pt.X, pt.Y);
  if (popupsi <> -1) and (popupsi <= Items.Count - 1) then
  begin
    if Items[popupsi].Enabled and Assigned(Items[popupsi].PopupMenu) then
    begin
      Items[popupsi].PopupMenu.Popup(Message.XPos, Message.YPos);
      shown := true;
    end;
  end;

  if shown then
    Message.Result := 0
  else
    inherited;
end;

procedure TAdvSmoothListBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothListBox.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure TAdvSmoothListBox.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_DESTROY then
  begin
    if FOleDropTargetAssigned then
      RevokeDragDrop(self.Handle);
  end;
  inherited;
end;

procedure TAdvSmoothListBox.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothListBox.AddDisplayItem(Item: TAdvSmoothListBoxItem; var prevrect: TRect; ht, lookup: integer);
var
  hs, vs, left, right, h: integer;
  chk: Boolean;
begin
  if Item.Visible then
  begin
    left := 0;
    right := 0;
    case LookupBar.Position of
      pLeft: left := lookup;
      pright: right := lookup;
    end;

    if Item.Splitter then
      h := SplitterHeight
    else
      h := ht;

    hs := ItemAppearance.HorizontalSpacing;
    vs := ItemAppearance.VerticalSpacing;

    with FDisplayList.AddItem do
    begin
      Kind := ikItem;
      if Item.DeleteButton and Item.DeleteButtonVisible then
      begin
        ItemRect := Rect(InsideRect.Left + left + hs + Item.Indent, prevrect.Bottom + vs,
          InsideRect.Right {- InsideRect.Left} - right - GetShadowOffset - hs - ItemAppearance.DeleteButtonWidth,
            prevrect.Bottom + vs + h);
      end
      else
      begin
        ItemRect := Rect(InsideRect.Left + left + hs + Item.Indent, prevrect.Bottom + vs,
          InsideRect.Right {- InsideRect.Left} - right - GetShadowOffset - hs,
            prevrect.Bottom + vs + h);
      end;

      if Layout = lblBubble then
      begin
        chk := False;
        case Item.Alternate of
          lbaAuto: chk := Odd(Item.Index);
          lbaLeft: chk := False;
          lbaRight: chk := True;
        end;

        if not chk then
          ItemRect := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Right - LayoutIndenting, ItemRect.Bottom)
        else
          ItemRect := Rect(ItemRect.Left + LayoutIndenting, ItemRect.Top, ItemRect.Right, ItemRect.Bottom);
      end;

      DisplayItem := Item;
      prevrect := ItemRect;
      prevrect.Bottom := prevrect.Bottom;
    end;
  end;
end;

procedure TAdvSmoothListBox.AddDisplaySection(Item: TAdvSmoothListBoxItem; var prevrect: TRect; ht, lookup: integer);
var
  left, right: integer;
  cat: TAdvSmoothListBoxCategoryItem;
begin
  if Item.Visible then
  begin
    left := 0;
    right := 0;
    case LookupBar.Position of
      pLeft: left := lookup;
      pright: right := lookup;
    end;

    with FDisplayList.AddItem do
    begin
      Kind := ikSection;
      case CategoryType of
        alphanumeric:
        begin
          if Length(Item.Caption) > 0 then
            SectionCaption := Item.Caption[1]
          else
            SectionCaption := '';
        end;
        custom:
        begin
          if (Item.CategoryID <> -1) and (Item.CategoryID < Categories.Count) then
          begin
            cat := Categories[Categories.ItemIndexByID(Item.CategoryID)];
            SectionCaption := cat.Text;
            SectionCategoryID := Item.CategoryID;
          end
          else
          begin
            SectionCaption := '';
            SectionCategoryID := -1;
          end;
        end;
      end;

      DoSectionHeight(Self, SectionCaption, SectionCategoryID, ht);

      ItemRect := Rect(InsideRect.Left + left, prevrect.Bottom, InsideRect.Right {- InsideRect.Left} - right - GetShadowOffset, prevrect.Bottom + ht);
      prevrect := ItemRect;
    end;
  end;
end;

procedure TAdvSmoothListBox.AnimateSmoothPos(Sender: TObject);
var
  c: TCanvas;
  d, opc: Double;
  posto, opcto, lto, l: integer;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  if FAnimate and (GetFullHeight >= 0) then
  begin
    //cp := FCurrentScPos;
    posTo := GetPositionTo;
    d := Abs(posto - GetPosition) / Max(1, Abs(FSp) * FSpeedFactor);
    FAnimating := AnimateDouble(FCurrentScPos, posto, d, 1);
    if not FAnimating then
    begin
      opcto := 0;
      opc := Abs(ScrollIndicator.FAnimateOpacity) / FSpeedFactor;
      FAnimatingScroll := AnimateDouble(ScrollIndicator.FAnimateOpacity, opcto, opc, 1);
      if FAnimatingScroll then
      begin
        //DoSmoothScroll(0, 0);
        Changed;
      end;
    end
    else
    begin
      ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
      DoSmoothScroll(GetPosition, posto);
      //DoScroll(self, cp, FScPosTo);
      Changed;
    end;
  end
  else
  begin
    opcto := 0;
    opc := Abs(ScrollIndicator.FAnimateOpacity) / Max(1, fsp);
    FAnimatingScroll := AnimateDouble(ScrollIndicator.FAnimateOpacity, opcto, opc, 1);
    if FAnimatingScroll then
    begin
      //DoSmoothScroll(0, 0);
      Changed;
    end;
  end;

  if FEnableDetail then
  begin
    if FDetailShow then
      lto := 0
    else
      lto := InsideRect.Right;

    if Assigned(FCurrentControl) then
    begin
      l := FCurrentControl.Left;
      if FDetailSpeedFactor = 0 then
        d := Abs(l - lto) / 1
      else
        d := Abs(l - lto) / FDetailSpeedFactor;

      FAnimatingdetail := AnimateDouble(l, lto, d, 1);
      if FAnimatingdetail then
      begin
        FCurrentControl.Visible := true;
        FCurrentControl.Left := l;
        FCurrentControl.Top := Header.GetHeight + Filter.GetHeight;
        FCurrentControl.Width := Width;
        FCurrentControl.Height := Height - Header.GetHeight - Footer.GetHeight - Filter.GetHeight;

        c:= inherited Canvas;
        if lto = 0 then
          c.Draw(FCurrentControl.Left - InsideRect.Right, FCurrentControl.Top, FAnimateBitmap)
        else
          c.Draw(-InsideRect.Right + FCurrentControl.Left, FCurrentControl.Top, FAnimateBitmap)
      end
      else
      begin

        if FDetailStatus = dsDetailsNotVisible then
        begin
          if Assigned(FAnimateBitmap) then
          begin
            FCurrentControl.Visible := false;
            FAnimateBitmap.Free;
            FAnimateBitmap := nil;
            FEnableDetail := False;
            Changed;
          end;
        end;
      end;
    end;
  end;

  Inc(FTimerCount);
  if FTimerCount >= 100 then
  begin
    FLookUp := false;
    FLookupKey := '';
    FTimerCount := 0;
  end;
end;

procedure TAdvSmoothListBox.AppearanceChanged(Sender: TObject);
begin
  InitDisplayList;
  CalculateRects;
  Changed;
end;

procedure TAdvSmoothListBox.ApplyFilter(AFilter: String);
begin
  if Assigned(Filter) then
    Filter.ProcessFilter(AFilter);
end;

procedure TAdvSmoothListBox.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBox then
  begin
    FFill.Assign((Source as TAdvSmoothListBox).Fill);
    FItems.Clear;
    FItems.Assign((Source as TAdvSmoothListBox).Items);
    FItemAppearance.Assign((Source as TAdvSmoothListBox).ItemAppearance);
    FScrollIndicator.Assign((Source as TAdvSmoothListBox).ScrollIndicator);
    FLayout := (Source as TAdvSmoothListBox).Layout;
    FLayoutIndenting := (Source as TAdvSmoothListBox).LayoutIndenting;
    FLookupBar.Assign((Source as TAdvSmoothListBox).LookupBar);
    FSpeedFactor := (Source as TAdvSmoothListBox).SpeedFactor;
    FDetailSpeedFactor := (Source as TAdvSmoothListBox).DetailSpeedFactor;
    FSorted := (Source as TAdvSmoothListBox).Sorted;
    FSections.Assign((Source as TAdvSmoothListBox).Sections);
    FHeader.Assign((Source as TAdvSmoothListBox).Header);
    FFilter.Assign((Source as TAdvSmoothListBox).Filter);
    FFooter.Assign((Source as TAdvSmoothListBox).Footer);
    FKeyBoardLookup := (Source as TAdvSmoothListBox).KeyBoardLookup;
    FSplitterHeight := (Source as TAdvSmoothListBox).SplitterHeight;
    FShowDetailClick := (Source as TAdvSmoothListBox).ShowDetailClick;
    FShowDetailKey := (Source as TAdvSmoothListBox).ShowDetailKey;
    FShowFocus := (Source as TAdvSmoothListBox).ShowFocus;
    FFocusColor := (Source as TAdvSmoothListBox).FocusColor;
    FItemImage.Assign((Source as TAdvSmoothListBox).ItemImage);
    FDetailItemImage.Assign((Source as TAdvSmoothListBox).DetailItemImage);
    FSelectionMode := (Source as TAdvSmoothListBox).SelectionMode;
    FDragAlphaBlend := (Source as TAdvSmoothListBox).DragAlphaBlend;
    FDragOpacity := (Source as TAdvSmoothListBox).DragOpacity;
    FDragBorderWidth := (Source as TAdvSmoothListBox).DragBorderWidth;
    FDragBorderColor := (Source as TAdvSmoothListBox).DragBorderColor;
    FItemDragging := (Source as TAdvSmoothListBox).ItemDragging;
    FMultiSelect := (Source as TAdvSmoothListBox).MultiSelect;
    FCategoryType := (Source as TAdvSmoothListBox).CategoryType;
    FCategories.Assign((Source as TAdvSmoothListBox).Categories);
    FDefaultItem.Assign((Source as TAdvSmoothListBox).DefaultItem);
    FOleDragDrop := (Source as TAdvSmoothListBox).OleDragDrop;
    FImages := (Source as TAdvSmoothListBox).Images;
    FContainer := (Source as TAdvSmoothListBox).PictureContainer;
    Changed;
  end;
end;

procedure TAdvSmoothListBox.BeginUpdate;
begin
  Items.BeginUpdate;
end;

procedure TAdvSmoothListBox.CalculateRectTopToBottom;
var
  it, t, b: Integer;
begin
  t := GetTopIndex;
  b := GetBottomIndex;
  if (t >= 0) and (b >= 0) then
  begin
    for it := t to b do
      CalculateRect(Items[it]);
  end;
end;

procedure TAdvSmoothListBox.CalculateRect(Item: TAdvSmoothListBoxItem);
var
  o: TAdvSmoothListBox;
  gppointf: TGPPointF;
  crl, crt: Single;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  g: TGPGraphics;
  ff: TGPFontFamily;
  sf: TGPStringFormat;
  f: TGPFont;
  fs: integer;
  b: TGPSolidBrush;
  src, sri: TGPRectF;
  htmlr: TRect;
  c, i, n: String;
  ir: TGPRectF;
  x, y, ch: integer;
  cht, iht: integer;
  w: integer;
  sizegr, sizegl: integer;
  bmp: TBitmap;
  it: integer;
  r: TRect;
  UseHTML: Boolean;
  fr: TGPRectF;
  captionf, notesf, infof: TFont;
begin
  if not Assigned(Item) or (csDestroying in ComponentState) then
    Exit;


  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  o := Self;
  it := GetDisplayIndex(Item.Index);
  if (it < 0) then
    Exit;

  with FDisplayList.GetItem(it).DisplayItem do
  begin
    captionf := CaptionFont;
    notesf := NotesFont;
    infof := InfoFont;
    if Selected then
    begin
      captionf := CaptionSelectedFont;
      notesf := NotesSelectedFont;
      infof := InfoSelectedFont;
    end;


    r := FDisplayList.GetItem(it).ItemRect;
    if (o.FFill.BorderWidth > 0) and (o.FFill.BorderColor <> clNone) then
      ir := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - R.Top)
    else
      ir := MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - R.Top);

    c := FDisplayList.GetItem(it).DisplayItem.Caption;
    i := Info;
    n := Notes;

    DoItemText(Self, Index, c, i, n);

    iht := 0;
    if i <> '' then
    begin
      ff := TGPFontFamily.Create(infof.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in infof.Style) then
        fs := fs + 1;
      if (fsItalic in infof.Style) then
        fs := fs + 2;
      if (fsUnderline in infof.Style) then
        fs := fs + 4;
      if fsStrikeOut in infof.Style then
        fs := fs + 8;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, infof.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(infof.Color));

      g.MeasureString(i, length(i), f, ir, sf, sri);

      crl := ir.X + ir.Width - sri.Width;

      w := 0;
      if FGraphicRightType <> gtNone then
        w := FGraphicRightWidth;
      if (n = '') then
      begin
        crt := ir.Y + (ir.Height - sri.Height) / 2;
        gppointf := MakePoint(crl - ScrollIndicator.GetWidth - w, crt);
      end
      else
      begin
        crt := ir.Y;
        gppointf := MakePoint(crl - ScrollIndicator.GetWidth - w, crt);
      end;

      iht := Round(sri.Height);

      finfor := Makerect(gppointf.X - FGraphicRightMargin - 5, gppointf.Y, sri.Width + 2, sri.Height);

      b.free;
      ff.free;
      sf.Free;
      f.free;
    end;

    cht := 0;
    if c <> '' then
    begin
      ff := TGPFontFamily.Create(captionf.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in captionf.Style) then
        fs := fs + 1;
      if (fsItalic in captionf.Style) then
        fs := fs + 2;
      if (fsUnderline in captionf.Style) then
        fs := fs + 4;
      if fsStrikeOut in captionf.Style then
        fs := fs + 8;

      sf := TGPStringFormat.Create;
      if not CaptionWordWrap then
        sf.SetFormatFlags(StringFormatFlagsNoWrap);

      f := TGPFont.Create(ff, captionf.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(captionf.Color));

      g.MeasureString(c, length(c), f, ir, sf, src);

      crl := 0;
      case CaptionAlignment of
        taLeftJustify: crl := ir.X;
        taRightJustify: crl := ir.X + ir.Width - src.Width - sri.Width;
        taCenter: crl := ir.X +  (ir.Width - src.Width) / 2;
      end;

      w := 0;
      if FGraphicLeftType <> gtNone then
        w := FGraphicLeftWidth;

      if (n = '') then
      begin
        crt := ir.Y + (ir.Height - src.Height) / 2;
        gppointf := MakePoint(crl + FMargin + w, crt);
      end
      else
      begin
        crt := ir.Y;
        gppointf := MakePoint(crl + FMargin + w, crt);
      end;

      cht := Round(src.Height);

      fcaptionr := MakeRect(gppointf.X + GraphicLeftMargin, gppointf.Y, src.Width + 3 - CaptionMarginRight, src.Height);

      b.free;
      ff.free;
      sf.Free;
      f.free;
    end;

    if n <> '' then
    begin
      fn := n;

      ch := 0;
      if NotesLocation <> plCustom then
        ch := Max(cht, iht);

      sizegl := 0;
      if FGraphicLeftType <> gtNone then
        sizegl := FGraphicLeftMargin + FGraphicLeftWidth;

      sizegr := 0;
      if FGraphicRightType <> gtNone then
        sizegr := FGraphicRightMargin + FGraphicRightWidth;

      htmlr := Bounds(Round(ir.X + sizegl), Round(ir.Y + ch), Round(ir.Width - sizegl - sizegr), Round(ir.Height - ch));

      UseHTML := (Pos('</', n) > 0) or (Pos('/>', n) > 0) or (Pos('<BR>',uppercase(n)) > 0);
      if UseHTML then
      begin
        HTMLDrawGDIP(g, notesf, n,htmlr,FImages, 0,0,-1,-1,NotesShadowOffset,False,true,false,false,
          False,False,true,1.0,NotesURLColor,clNone,clNone,NotesShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
      end
      else
      begin
        f := g.MakeFont(notesf);
        g.MeasureString(n, Length(n), f, MakeRect(htmlr.Left, htmlr.Top, htmlr.Right - htmlr.Left, htmlr.Bottom - htmlr.Top), fr);
        f.Free;

        XSize := Round(fr.Width) + 3;
        YSize := Round(fr.Height) + 3;
      end;

      if NotesLocation <> plCustom then
      begin
        GetTextPosition(x, y, MakeRect(htmlr.Left, htmlr.Top, htmlr.Right - htmlr.Left, htmlr.Bottom - htmlr.Top), XSize, YSize, NotesLocation);
      end
      else
      begin
        x := NotesLeft;
        y := NotesTop;
      end;

      Fhtmlr := Bounds(x + htmlr.Left, y + htmlr.Top, XSize, YSize)
    end;
  end;
  bmp.free;
  g.free;
end;

procedure TAdvSmoothListBox.CalculateRects(AUpdate: Boolean);
var
  o: TAdvSmoothListBox;
  gppointf: TGPPointF;
  crl, crt: Single;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  g: TGPGraphics;
  ff: TGPFontFamily;
  sf: TGPStringFormat;
  f: TGPFont;
  fs: integer;
  b: TGPSolidBrush;
  src, sri: TGPRectF;
  htmlr: TRect;
  c, i, n: String;
  ir: TGPRectF;
  x, y, ch: integer;
  cht, iht: integer;
  wl, wr: integer;
  sizegr, sizegl: integer;
  bmp: TBitmap;
  it: integer;
  r: TRect;
  UseHTML: Boolean;
  fr: TGPRectF;
  captionf, notesf, infof: TFont;
begin
  if FUpdateCount > 0 then
    Exit;

  if not Assigned(FDisplayList) or (csDestroying in ComponentState) then
    Exit;

  captionf := TFont.Create;
  notesf := TFont.Create;
  infof := TFont.Create;

  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  o := Self;
  for it := 0 to FDisplayList.Count - 1 do
  begin
    if Assigned(FDisplayList.GetItem(it).DisplayItem) then
    begin
      with FDisplayList.GetItem(it).DisplayItem do
      begin

        captionf.Assign(CaptionFont);
        notesf.Assign(NotesFont);
        infof.Assign(InfoFont);
        if Selected then
        begin
          captionf.Assign(CaptionSelectedFont);
          notesf.Assign(NotesSelectedFont);
          infof.Assign(InfoSelectedFont);
        end;


        if Assigned(OnItemCustomizeFont) then
          OnItemCustomizeFont(Self, FDisplayList.GetItem(it).DisplayItem, captionf, notesf, infof);

        r := FDisplayList.GetItem(it).ItemRect;
        if (o.FFill.BorderWidth > 0) and (o.FFill.BorderColor <> clNone) then
          ir := MakeRect(r.Left, r.Top, r.Right - r.Left, r.Bottom - R.Top)
        else
          ir := MakeRect(r.Left, r.Top, r.Right - r.Left - 1, r.Bottom - R.Top);

        c := FDisplayList.GetItem(it).DisplayItem.Caption;
        i := Info;
        n := Notes;

        DoItemText(Self, Index, c, i, n);

        iht := 0;
        if i <> '' then
        begin
          ff := TGPFontFamily.Create(infof.Name);
          if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
          begin
            ff.Free;
            ff := TGPFontFamily.Create('Arial');
          end;

          fs := 0;
          if (fsBold in infof.Style) then
            fs := fs + 1;
          if (fsItalic in infof.Style) then
            fs := fs + 2;
          if (fsUnderline in infof.Style) then
            fs := fs + 4;
          if fsStrikeOut in infof.Style then
            fs := fs + 8;

          sf := TGPStringFormat.Create;
          f := TGPFont.Create(ff, infof.Size, fs, UnitPoint);
          b := TGPSolidBrush.Create(ColorToARGB(infof.Color));

          g.MeasureString(i, length(i), f, ir, sf, sri);

          crl := ir.X + ir.Width - sri.Width;

          wr := 0;
          if FGraphicRightType <> gtNone then
            wr := FGraphicRightWidth;

          if (n = '') then
          begin
            crt := ir.Y + (ir.Height - sri.Height) / 2;
            gppointf := MakePoint(crl - ScrollIndicator.GetWidth - wr, crt);
          end
          else
          begin
            crt := ir.Y;
            gppointf := MakePoint(crl - ScrollIndicator.GetWidth - wr, crt);
          end;

          iht := Round(sri.Height);

          finfor := Makerect(gppointf.X - FGraphicRightMargin - 5, gppointf.Y + 2, sri.Width + 2, sri.Height);

          b.free;
          ff.free;
          sf.Free;
          f.free;
        end;

        cht := 0;
        if c <> '' then
        begin
          ff := TGPFontFamily.Create(captionf.Name);
          if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
          begin
            ff.Free;
            ff := TGPFontFamily.Create('Arial');
          end;

          fs := 0;
          if (fsBold in captionf.Style) then
            fs := fs + 1;
          if (fsItalic in captionf.Style) then
            fs := fs + 2;
          if (fsUnderline in captionf.Style) then
            fs := fs + 4;
          if fsStrikeOut in captionf.Style then
            fs := fs + 8;

          sf := TGPStringFormat.Create;
          if not CaptionWordWrap then
            sf.SetFormatFlags(StringFormatFlagsNoWrap);
          f := TGPFont.Create(ff, captionf.Size, fs, UnitPoint);
          b := TGPSolidBrush.Create(ColorToARGB(captionf.Color));

          wl := 0;
          if FGraphicLeftType <> gtNone then
            wl := FGraphicLeftWidth + GraphicLeftMargin;

          wr := 0;
          if FGraphicRightType <> gtNone then
            wr := FGraphicRightWidth + GraphicRightMargin;

          g.MeasureString(c, length(c), f, MakeRect(ir.X, ir.Y, ir.Width - CaptionMargin - LookupBar.GetWidth - wl - wr, 10000), sf, src);

          crl := 0;
          case CaptionAlignment of
            taLeftJustify: crl := ir.X;
            taRightJustify: crl := ir.X +  ir.Width - src.Width - sri.Width;
            taCenter: crl := ir.X + (ir.Width - src.Width) / 2;
          end;

          if (n = '') then
          begin
            crt := ir.Y + (ir.Height - src.Height) / 2;
            gppointf := MakePoint(crl + FMargin + wl, crt);
          end
          else
          begin
            crt := ir.Y;
            gppointf := MakePoint(crl + FMargin + wl, crt);
          end;

          cht := Round(src.Height);

          fcaptionr := MakeRect(gppointf.X, gppointf.Y, src.Width + 3 - FMarginRight, src.Height);

          b.free;
          ff.free;
          sf.Free;
          f.free;
        end;

        if n <> '' then
        begin
          fn := n;

          ch := 0;
          if NotesLocation <> plCustom then
            ch := Max(cht, iht);

          sizegl := 0;
          if FGraphicLeftType <> gtNone then
            sizegl := FGraphicLeftMargin + FGraphicLeftWidth;

          sizegr := 0;
          if FGraphicRightType <> gtNone then
            sizegr := FGraphicRightMargin + FGraphicRightWidth;

          htmlr := Bounds(Round(ir.X + sizegl + 3), Round(ir.Y + ch), Round(ir.Width - sizegl - sizegr - 3), Round(ir.Height - ch));

          UseHTML := (Pos('</', n) > 0) or (Pos('/>', n) > 0) or (Pos('<BR>',uppercase(n)) > 0);
          if UseHTML then
          begin
            HTMLDrawGDIP(g, notesf, n,htmlr,FImages, 0,0,-1,-1,NotesShadowOffset,False,true,false,false,
              False,False,true,1.0,NotesURLColor,clNone,clNone,NotesShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);
          end
          else
          begin
            f := g.MakeFont(notesf);
            g.MeasureString(n, Length(n), f,MakeRect(htmlr.Left, htmlr.Top, htmlr.Right - htmlr.Left, 10000), fr);
            f.Free;

            XSize := Round(fr.Width) + 3;
            YSize := Round(fr.Height) + 3;
          end;

          if NotesLocation <> plCustom then
          begin
            GetTextPosition(x, y, MakeRect(htmlr.Left, htmlr.Top, htmlr.Right - htmlr.Left, htmlr.Bottom - htmlr.Top), XSize, YSize, NotesLocation);
          end
          else
          begin
            x := NotesLeft;
            y := NotesTop;
          end;
          Fhtmlr := Bounds(x + htmlr.Left, y + htmlr.Top, XSize, YSize)
        end;
      end;
    end;
  end;
  bmp.free;
  g.free;

  captionf.Free;
  notesf.Free;
  infof.Free;


  if not AUpdate then
  begin
    InitDisplayList(True);
    CalculateRects(True)
  end;
end;

procedure TAdvSmoothListBox.CategoriesChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBox.Changed;
begin
  if (csDestroying in componentstate) then
    Exit;
  Invalidate;
end;

function TAdvSmoothListBox.CheckSelection(X, Y: integer): Boolean;
var
  itemindex: integer;
  right, left: Boolean;
begin
  Result := false;
  Itemindex := ItemAtXY(X, Y);
  if (ItemIndex >= 0) and (ItemIndex <= Items.Count - 1) then
  begin
    with Items[ItemIndex] do
    begin
      right := ((GraphicRightType <> gtNone) and PtInRect(fgrRight, Point(X, Y)));
      left := ((GraphicLeftType <> gtNone) and PtInRect(fgrLeft, Point(X, Y)));
      Result := not right and not left;
    end;
  end;
end;

procedure TAdvSmoothListBox.Click;
begin
  inherited;
  if Assigned(Parent) then
  begin
    if Parent.visible then
      SetFocus;
  end;
end;

procedure TAdvSmoothListBox.CMHintShow(var Message: TMessage);
var
  pt: TPoint;
  item: integer;
  ditem: TAdvSmoothListBoxItem;
  aHint: String;
  rh, rf: TRect;
  AnchorH, AnchorF: String;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := self.Hint;
    pt := CursorPos;
    item := YToItem(pt.X, pt.Y);
    if item <> -1 then
    begin
      dItem := FDisplayList.GetItem(item).DisplayItem;
      if dItem <> nil then
      begin
        if dItem.Hint <> '' then
          HintStr := ditem.Hint;

        aHint := ditem.GetAnchorAt(CursorPos.X, CursorPos.Y, True);
        if AHint <> '' then
          HintStr := aHint;

        if Assigned(FOnItemHint) then
          FOnItemHint(Self, item, HintStr);
      end;
    end;

    //HEADER
    rh := GetHeaderFooterCaptionRect(Header, true);
    AnchorH := Header.GetAnchorAt(rh, CursorPos.X, CursorPos.Y, True);

    //FOOTER
    rf := GetHeaderFooterCaptionRect(Footer, false);
    AnchorF := Footer.GetAnchorAt(rf, CursorPos.X, CursorPos.Y, True);

    if AnchorF <> '' then
      HintStr := AnchorF;

    if AnchorH <> '' then
      HintStr := AnchorH;

    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if fmb <> mbLeft then
  begin
    FMouseDown := False;
    FMouseUp := False;
  end;

  if (FHoveredItemIndex >= 0) and (FHoveredItemIndex <= Items.Count - 1) then
  begin
    with Items[FhoveredItemIndex] do
    begin
      if not FMouseLeft then
      begin
        if Assigned(FOnItemMouseLeave) then
          FOnItemMouseLeave(Self, FhoveredItemIndex);

        FMouseEntered := false;
        FMouseLeft := true;
      end
    end;
  end;
  FPrevHoveredItemIndex := -1;
  FHoveredItemIndex := -1;
  Application.CancelHint;
end;

procedure TAdvSmoothListBox.CollapseAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[i].Level = 0) and Items[I].Expanded then
      Items[I].Collapse;
  end;
  Items.EndUpdate;
end;

constructor TAdvSmoothListBox.Create(AOwner: TComponent);
var
  i: integer;
begin
  FConstructed := false;
  inherited Create(AOwner);
  FComboUse := false;
  FClickMargin := 2;
  FDragMargin := 15;
  FDeleteMargin := 20;
  FDragItemForm := nil;
  FLayout := lblNormal;
  FLayoutIndenting := 60;
  FMode := dmRelease;
  FTimerCount := 0;
  FFocusColor := clBlack;
  FShowFocus := true;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FSelectedItemIndex := -1;
  FShowDetailKey := dkSpace;
  FShowDetailClick := sdOnClick;
  FKeyBoardLookup := false;
  FDisplayList := TAdvSmoothListBoxDisplayList.Create;
  FSorted := false;
  FSpeedFactor := 4;
  FDetailSpeedFactor := 4;
  FCurrentScPos := 0;
  FScPosTo := 0;
  DoubleBuffered := true;
  FOfficeHint := TAdvHintInfo.Create;
  FItems := TAdvSmoothListBoxItems.Create(Self);
  FItems.OnChange := ItemsChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := AppearanceChanged;
  FItemAppearance := TAdvSmoothListBoxItemAppearance.Create(Self);
  FItemAppearance.OnChange := ItemAppearanceChanged;
  FLookUpBar := TAdvSmoothListBoxLookUpBar.Create(Self);
  FLookUpBar.OnChange := LookupBarChanged;
  FSections := TAdvSmoothListBoxSections.Create(Self);
  FSections.OnChange := SectionsChanged;
  FHeader := TAdvSmoothListBoxHeaderFooter.Create(Self);
  FHeader.OnChange := HeaderFooterChanged;
  FFilter := TAdvSmoothListBoxFilter.Create(Self);
  FFilter.OnChange := FilterChanged;
  FScrollIndicator := TAdvSmoothListBoxIndicator.Create(Self);
  FScrollIndicator.OnChange := ScrollIndicatorChanged;
  FFooter := TAdvSmoothListBoxHeaderFooter.Create(Self);
  FFooter.OnChange := HeaderFooterChanged;
  Width := 250;
  Height := 350;
  FItemDragging := true;
  FMouseSelect := lmsLeft;
  FTriggerButtons := [mbLeft];
  FMultiSelect := false;
  FCategories := TAdvSmoothListBoxCategoryItems.Create(Self);
  FCategories.OnChange := CategoriesChanged;
  FCategoryType := alphanumeric;
  FSmoothTimer :=  TTimer.Create(self);
  FSmoothTimer.Interval := 1;
  FSmoothTimer.Enabled := not (csDesigning in ComponentState);
  FSmoothTimer.OnTimer := AnimateSmoothPos;
  FEnableDragging := true;

  FDragTimer :=  TTimer.Create(self);
  FDragTimer.Interval := 1;
  FDragTimer.Enabled := false;
  FDragTimer.OnTimer := DragSmoothPos;

  FAnimate := true;
  FDetailShow := false;
  FDetailStatus := dsDetailsNotVisible;
  FSplitterHeight := 20;
  FhoveredItemIndex := -1;
  FPrevHoveredItemIndex := -1;
  FSelectionMode := sAutoDeselect;
  FDragAlphaBlend := true;
  FDragOpacity := 200;
  FDragBorderWidth := 1;
  FDragBorderColor := clBlack;

  FItemImage := TAdvGDIPPicture.Create;
  FItemImage.OnChange := ItemAppearanceChanged;

  FDetailItemImage := TAdvGDIPPicture.Create;
  FDetailItemImage.OnChange := ItemAppearanceChanged;

  if Assigned(Owner) and (Owner.ClassName = 'TAdvSmoothComboBox') then
  begin
    FDesignTime := (csDesigning in Owner.ComponentState) and not
      ((csReading in Owner.Owner.ComponentState) or (csLoading in Owner.Owner.ComponentState));
  end
  else
    FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FDefaultItems := TAdvSmoothListBoxItems.Create(Self);
  FDefaultItem := FDefaultItems.Add;

  FDropDownForm := TForm.Create(Self);
  FDropDownForm.OnDeactivate := DropDownFormDeactivate;
  FDropDownForm.BorderStyle := bsNone;
  FDropDownForm.Width := 0;
  FDropDownForm.Height := 0;

  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := 100;

  FDragCountTimer := TTimer.Create(self);
  FDragCountTimer.Enabled := false;
  FDragCountTimer.OnTimer := DragCountTimerEvent;
  FDragCountTimer.Interval := 100;

  FTextRendering := tClearType;

  TabStop := true;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);
end;

procedure TAdvSmoothListBox.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TAdvSmoothListBox.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;
  if FDesignTime then
  begin
    InitPreview;
    SetComponentStyle(tsOffice2007Luna);
  end;
  FConstructed := true;
end;

procedure TAdvSmoothListBox.DblClick;
var
  item: integer;
  sdOnImg: Boolean;
begin
  inherited;
  if FDisplayList.Count > 0 then
  begin
    item := YToItem(FClickX, FClickY);

    sdOnImg := false;
    if item <> -1 then
    begin
      case ShowDetailClick of
        sdOnDblClick: ShowDetails;
        sdOnDetailImageDblClick: sdOnImg := true;
      end;
    end;

    if (FSelectedItemIndex <> -1) then
    begin
      with Items[FSelectedItemIndex] do
      begin
        if (PtInRect(fgrLeft, Point(FClickX, FClickY))) then
        begin
          case GraphicLeftType of
            gtDetailImage, gtCommonDetailImage:
            begin
              if sdOnImg then
                ShowDetails;
            end;
          end;
        end;

        if (PtInRect(fgrRight, Point(FClickX, FClickY))) then
        begin
          case GraphicRightType of
            gtDetailImage, gtCommonDetailImage:
            begin
              if sdOnImg then
                ShowDetails;
            end;
          end;
        end;
      end;
      if item <> -1 then
        DoItemDblClick(self, FDisplayList.GetItem(item).DisplayItem.Index);
    end;
  end;
  FMouseDblClick := true;
end;

procedure TAdvSmoothListBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

function TAdvSmoothListBox.DeleteButtonAtXY(X, Y: integer): integer;
var
  displ: integer;
  DItem: TAdvSmoothListBoxItem;
begin
  Result := -1;
  displ := YToDeleteButton(X, Y);
  if displ <> -1 then
  begin
    DItem := FDisplayList.GetItem(displ).DisplayItem;
    if DItem <> nil then
      Result := DItem.Index;
  end;
end;

destructor TAdvSmoothListBox.Destroy;
begin
  if Assigned(FAnimateBitmap) then
  begin
    FAnimateBitmap.Free;
    FAnimateBitmap := nil;
  end;

  if Assigned(FDrawingBitmap) then
  begin
    FDrawingBitmap.Free;
    FDrawingBitmap := nil;
  end;

  FDisplayList.Free;
  FDefaultItem.Free;
  FDefaultItem := nil;
  FDefaultItems.Free;
  FItems.Free;
  FTimer.Free;
  FDragCountTimer.Free;
  FOfficeHint.Free;
  FCategories.free;
  FItemImage.Free;
  FDetailItemImage.Free;
  FSmoothTimer.Free;
  FDragTimer.Free;
  FLookUpBar.Free;
  FItemAppearance.Free;
  FFill.Free;
  FSections.Free;
  FHeader.Free;
  FFooter.Free;
  FFilter.Free;
  FScrollIndicator.Free;
  inherited;
end;

procedure TAdvSmoothListBox.DoAnchorClick(Sender: TObject; Anchor: String);
begin
  FAnimate := false;
  FMouseUp := false;
  FMouseDown := false;
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(Sender, Anchor);
end;

procedure TAdvSmoothListBox.DoDragDrop(Source: TObject; X, Y: Integer);
begin
  FMouseDown := false;
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Source, X, Y);
  inherited;
end;

procedure TAdvSmoothListBox.DoDragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TAdvSmoothListBox.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothListBox.DoExit;
begin
  inherited;
  FhoveredItemIndex := -1;
  FPrevHoveredItemIndex := -1;
  Application.CancelHint;
  FFocused := false;
  Changed;  
end;

procedure TAdvSmoothListBox.DoFilterChange(Sender: TObject;
  var FilterText: String; var AllowFilter: Boolean);
begin
  if Assigned(OnFilterChange) then
    OnFilterChange(Sender, FilterText, AllowFilter);
end;

procedure TAdvSmoothListBox.DoHideDetail(Sender: TObject;
  itemindex: integer);
begin
  InitState;
  if Assigned(FOnHideDetail) then
    FOnHideDetail(Sender, itemindex);
end;

procedure TAdvSmoothListBox.DoInternalScroll;
begin

end;

procedure TAdvSmoothListBox.DoItemAnchorClick(Sender: TObject; Anchor: String;
  ItemIndex: integer);
begin
  if Assigned(FOnItemAnchorClick) then
  begin
    InitState;
    FOnItemAnchorClick(Sender, Anchor, ItemIndex);
  end;
end;

procedure TAdvSmoothListBox.DoItemBkgDraw(Sender: TObject; Canvas: TCanvas;
  itemindex: integer; itemrect: TRect; var defaultdraw: boolean);
begin
  if Assigned(FOnItemBkgDraw) then
    FOnItemBkgDraw(Sender, Canvas, itemindex, itemrect, defaultdraw);
end;

procedure TAdvSmoothListBox.DoItemButtonClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(FOnItemButtonClick) then
  begin
    InitState;
    FOnItemButtonClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoItemButtonDropDownClick(Sender: TObject;
  itemindex: integer);
var
  item: TAdvSmoothListBoxItem;
  AllowShow, AllowHide: Boolean;
begin
  InitState;
  if Assigned(FOnItemButtonClick) then
    FOnItemButtonClick(Sender, itemindex);

  item := nil;
  if (itemindex >= 0) and (itemindex <= Items.Count - 1) then
    item := Items[itemindex];

  FDropDownItem := item;

  if Assigned(FDropDownForm) and Assigned(FDropDownItem) then
  begin
    if Assigned(item.FDropDownControl) then
    begin
      if FDeactivating then
      begin
        FDeactivating := false;
        Exit;
      end;

      if FDropDownForm.Visible then
      begin
        AllowHide := true;
        if Assigned(FOnItemDropDownHide) then
          FOnItemDropDownHide(Self, FDropDownItem, AllowHide);

        if AllowHide then
        begin
          FDeactivating := true;
          FDropDownForm.Hide;
          Exit;
        end;
      end
      else
      begin
        AllowShow := true;
        if Assigned(FOnItemDropDownShow) then
          FOnItemDropDownShow(Self, FDropDownItem, AllowShow);

        if AllowShow then
          DropDown(FDropDownItem);
      end;
    end;
  end;
end;

procedure TAdvSmoothListBox.DoItemButtonLeftClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemButtonLeftClick) then
    OnItemButtonLeftClick(Sender, itemindex);
end;

procedure TAdvSmoothListBox.DoItemButtonRightClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(OnItemButtonRightClick) then
    OnItemButtonRightClick(Sender, itemindex);
end;

procedure TAdvSmoothListBox.DoItemCaptionClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(FOnItemCaptionClick) then
  begin
//    InitState;
    FOnItemCaptionClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoItemCheckClick(Sender: TObject;
  itemindex: integer; checked: Boolean);
begin
  if Assigned(FOnItemCheckClick) then
  begin
    InitState;
    FOnItemCheckClick(Sender, itemindex, checked);
  end;
end;

function TAdvSmoothListBox.DoItemCheckToggle(Item: TAdvSmoothListBoxItem;
  GraphicLeft: Boolean; var Checked: Boolean): Boolean;
begin
  Result := True;
end;

procedure TAdvSmoothListBox.DoItemClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(FOnItemClick) then
  begin
    InitState;
    FOnItemClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoItemDblClick(Sender: TObject; itemindex: integer);
begin
  if Assigned(FOnItemDblClick) then
  begin
    InitState;
    FOnItemDblClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoItemDraw(Sender: TObject; Canvas: TCanvas;
  itemindex: integer; itemrect: TRect; var defaultdraw: boolean);
begin
  if Assigned(FOnItemDraw) then
    FOnItemDraw(Sender, Canvas, itemindex, itemrect, defaultdraw);
end;

procedure TAdvSmoothListBox.DoItemDropDownSelect(Sender: TObject;
  Item: TAdvSmoothListBoxItem; ItemIndex: integer; Value: String);
begin
  InitState;
  if Assigned(FOnItemDropDownSelect) then
    FOnItemDropDownSelect(Sender, Item, ItemIndex, Value);

  HideParent;
end;

procedure TAdvSmoothListBox.DoItemGraphics(Sender: TObject; itemindex: integer);
begin

end;

procedure TAdvSmoothListBox.DoItemImageClick(Sender: TObject;
  itemindex: integer; GraphicLeft: Boolean);
begin
  if Assigned(OnGraphicLeftClick) and GraphicLeft then
  begin
    InitState;
    FOnGraphicLeftClick(Sender, ItemIndex);
  end
  else if Assigned(OnGraphicRightClick) and not GraphicLeft then
  begin
    InitState;
    FOnGraphicRightClick(Sender, ItemIndex);
  end
  else
  if Assigned(FOnItemImageClick) then
  begin
    InitState;
    FOnItemImageClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoItemInfoClick(Sender: TObject;
  itemindex: integer);
begin
  if Assigned(FOnItemInfoClick) then
  begin
    InitState;
    FOnItemInfoClick(Sender, itemindex);
  end;
end;

procedure TAdvSmoothListBox.DoBoolPropertyChange(Item: TAdvSmoothListBoxItem;
  PropID: Integer; var Value: Boolean);
begin

end;

procedure TAdvSmoothListBox.DoItemRadioClick(Sender: TObject;
  itemindex: integer; checked: Boolean);
begin
  if Assigned(FOnItemRadioClick) then
  begin
    InitState;
    FOnItemRadioClick(Sender, itemindex, checked);
  end;
end;

procedure TAdvSmoothListBox.DoItemText(Sender: TObject; itemindex: integer;
  var itemcaption, iteminfo, itemnotes: String);
begin
  if Assigned(FOnItemText) then
    FOnItemText(Sender, itemindex, itemcaption, iteminfo, itemnotes);
end;

procedure TAdvSmoothListBox.DoLookup(
  DispItem: TAdvSmoothListBoxDisplayListItem);
begin

end;

procedure TAdvSmoothListBox.DoScroll(Sender: TObject; CurrentPosition,
  EndPosition: Double);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Sender, CurrentPosition, EndPosition);
end;

procedure TAdvSmoothListBox.DoSectionClick(Sender: TObject;
  Caption: String; CategoryID: Integer);
begin
  if Assigned(FOnSectionClick) then
    FOnSectionClick(Sender, Caption, CategoryID);
end;

procedure TAdvSmoothListBox.DoSectionDraw(Sender: TObject; ACanvas: TCanvas;
  Rect: TRect; Caption: String; CategoryID: Integer; var DefaultDraw: Boolean);
begin
  if Assigned(OnSectionDraw) then
    OnSectionDraw(Sender, ACanvas, Rect, Caption, CategoryID, DefaultDraw);
end;

procedure TAdvSmoothListBox.DoSectionDrawContent(Sender: TObject;
  ACanvas: TCanvas; Rect: TRect; Caption: String; CategoryID: Integer;
  var DefaultDraw: Boolean);
begin
  if Assigned(OnSectionDrawContent) then
    OnSectionDrawContent(Sender, ACanvas, Rect, Caption, CategoryID, DefaultDraw);
end;

procedure TAdvSmoothListBox.DoSectionHeight(Sender: TObject;
  Caption: String; CategoryID: Integer; var AHeight: Integer);
begin
  if Assigned(OnSectionHeight) then
    OnSectionHeight(Self, Caption, CategoryID, AHeight);
end;

procedure TAdvSmoothListBox.DoSelectItem(NewItemIndex: Integer);
begin

end;

procedure TAdvSmoothListBox.DoShowDetail(Sender: TObject;
  itemindex: integer);
begin
  InitState;
  if Assigned(FOnShowDetail) then
    FOnShowDetail(Sender, itemindex);
end;

procedure TAdvSmoothListBox.DoSmoothScroll(CurrentPosition,
  EndPosition: Double);
begin

end;

procedure TAdvSmoothListBox.DragCountTimerEvent(Sender: TObject);
begin
  Inc(FDragCount);
end;

procedure TAdvSmoothListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  DoDragDrop(Source, X, Y);
end;

procedure TAdvSmoothListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin                    inherited;
  DoDragOver(Source, X, Y, State, Accept);
end;

procedure TAdvSmoothListBox.DragSmoothPos(Sender: TObject);
begin
  FScPosTo := Min(GetFullHeight, Max(FScPosTo + FDragAnimateDelta, 0));
  FCurrentScPos := FScPosTo;
  Changed;
end;

procedure TAdvSmoothListBox.DrawBackground;
var
  rc: TRect;
  g: TGPGraphics;
begin
  if not Assigned(Fill) then
    Exit;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  rc := ClientRect;
  if (Fill.BorderWidth <> 0) and (Fill.BorderColor <> clNone) then
    fill.Fill(g, MakeRect(rc.Left, rc.Top, rc.Right - rc.Left - 1, rc.Bottom - rc.Top - 1))
  else
    fill.Fill(g, MakeRect(rc.Left, rc.Top, rc.Right - rc.Left, rc.Bottom - rc.Top));

  g.Free;
end;

procedure TAdvSmoothListBox.DrawFilter;
var
  g: TGPGraphics;
  rc: TRect;
begin
  with Filter do
  begin
    if Visible and Enabled then
    begin
      g := TGPGraphics.Create(Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);
      rc := InsideRect;
      Fill.Fill(g, MakeRect(rc.Left, rc.Top + Header.GetHeight, rc.Right - rc.Left - GetShadowOffset, Height));
      g.Free;
    end;
  end;
end;

procedure TAdvSmoothListBox.DrawHeaderFooter(Part: TAdvSmoothListBoxHeaderFooter; Header: Boolean);
var
  rc: TRect;
  x, y: integer;
  g: TGPGraphics;
  fillr: TGPRectF;
  htmlr: TRect;
  a, s, k: String;
  l, m, XSize, YSize: integer;
  hr: TRect;
begin
  if not Assigned(Part) then
    Exit;

  with Part do
  begin
    if Visible then
    begin
      g := TGPGraphics.Create(Canvas.Handle);
      case TextRendering of
        tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
        tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
        tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      end;
      
      rc := InsideRect;

      if header then
        fillr := fill.Fill(g, MakeRect(rc.Left, rc.Top, rc.Right - rc.Left - GetShadowOffset, Height))
      else
        fillr := fill.Fill(g, MakeRect(rc.Left, rc.Bottom - Height - GetShadowOffset, rc.Right - rc.Left - GetShadowOffset, Height));

      if Caption <> '' then
      begin
        htmlr := Rect(0, 0, 10000, 10000);

        HTMLDrawGDIP(g, FFont, Caption,htmlr,FImages, 0,0,-1,-1,CaptionShadowOffset,False,true,false,false,
          False,False,true,1.0,CaptionURLColor,clNone,clNone,CaptionShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);

        if CaptionLocation <> cpCustom then
          GetCaptionPosition(x, y, fillr, XSize, YSize, CaptionLocation)
        else
        begin
          x := CaptionLeft;
          y := CaptionTop;
        end;

        htmlr := Bounds(Round(fillr.X + x), Round(fillr.Y + y), xsize, ysize);

        HTMLDrawGDIP(g, FFont, Caption,htmlr,FImages, 0,0,-1,-1,CaptionShadowOffset,False,false,false,false,
          False,False,true,1.0,CaptionURLColor,clNone,clNone,CaptionShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FContainer,2);

      end;
      g.Free;
    end;
  end;
end;

procedure TAdvSmoothListBox.DrawItems;
var
  i: Integer;
  r: TRect;
  top: integer;
  bottom: integer;
  j, pos, diff: integer;
  nextfloatitem, floatitem: TAdvSmoothListBoxDisplayListItem;
  draws: Boolean;
begin
  top := GetTopIndex;
  bottom := GetBottomIndex;

  floatitem := nil;
  for i := 0 to FDisplayList.Count - 1 do
  begin
    with FDisplayList.GetItem(i) do
    begin
      Floating := false;
      case Kind of
        ikItem:
        begin
          if DisplayItem <> nil then
          begin
            if (I >= top) and (I <= bottom) and not (DisplayItem.Splitter) then
              DisplayItem.Draw(Canvas, ItemRect, i);

            j := i;

            while((j > 0) and (FDisplayList.GetItem(j).Kind = ikItem)) do
            begin
              Dec(j);
            end;

            with FDisplayList.GetItem(j) do
            begin
              Floating := ItemRect.Top <= GetPosition + Header.GetHeight + Filter.GetHeight;
              if Floating then
              begin
                if floatitem <> nil then
                begin
                  if floatitem <> FDisplayList.GetItem(j) then
                    floatitem := FDisplayList.GetItem(j);
                end
                else
                  floatitem := FDisplayList.GetItem(j);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  nextfloatitem := nil;

  for i := 0 to FDisplayList.Count - 1 do
  begin
    with FDisplayList.GetItem(i) do
    begin
      case Kind of
        ikSection:
        begin
          draws := true;
          if floatitem <> nil then
          begin
            draws := floatitem <> FDisplayList.GetItem(i);
            if not draws then
            begin
              j := i + 1;
              while((j < FDisplayList.Count) and (FDisplayList.GetItem(j).Kind = ikItem)) do
              begin
                Inc(j);
              end;
              if j < FDisplayList.Count then
                nextfloatitem := FDisplayList.GetItem(j);
            end;
          end;

          if draws then
          begin
            r := ItemRect;
            DrawSection(r, SectionCaption, SectionCategoryID);
          end;
        end;
      end;
    end;
  end;

  if (floatitem <> nil) then
  begin
    with floatitem do
    begin
      r := ItemRect;
      diff := floatitem.ItemRect.Bottom - floatitem.ItemRect.Top;
      
      pos := GetPosition + Header.GetHeight + Filter.GetHeight;

      if (nextfloatitem <> nil) then
        if nextfloatitem.ItemRect.Top - GetPosition <= diff + Header.GetHeight + Filter.GetHeight then
          pos := nextfloatitem.ItemRect.top - diff;

      DrawSection(Bounds(r.Left, pos, r.Right, r.Bottom - r.Top), SectionCaption, SectionCategoryID);
    end;
  end
end;

procedure TAdvSmoothListBox.DrawLookUpBar;
var
  ch: Char;
  customch: String;
  I: integer;
  f, fd: TGPFont;
  sf: TGPStringFormat;
  p: TGDIPFillParameters;
  pt: TGPPointF;
  ximg, x, y: Single;
  b: TGPBrush;
  tw, th: integer;
  stop: integer;
  pos: integer;
  g: TGPGraphics;
  rf: TGPRectF;
  ca: TCanvas;
  maxtextw: integer;
begin
  if not Assigned(LookupBar) then
    Exit;

  with LookupBar do
  begin
    if LookupBarVisible then
    begin
      ca := TCanvas.Create;
      ca.Handle := Canvas.Handle;
      ca.Font.Assign(Font);
      tw := 0;
      maxtextw := GetMaximumCustomTextWidth(ca);
      case CategoryType of
        alphanumeric: tw := ca.TextWidth('W') + FMargin;
        custom: tw := maxtextw;
      end;

      g := TGPGraphics.Create(ca.Handle);
      case TextRendering of
        tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
        tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
        tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      end;

      p.Graphics := g;
      p.Path := nil;
      p.Fillpath := false;
      case Position of
        pRight: rf := MakeRect(InsideRect.Right - tw - GetShadowOffset, InsideRect.Top + Header.GetHeight + Filter.GetHeight, tw, InsideRect.Bottom - Footer.GetHeight - 1 - GetShadowOffset);
        pLeft: rf := MakeRect(InsideRect.Left, InsideRect.Top + Header.GetHeight + Filter.GetHeight, InsideRect.Left + tw, InsideRect.Bottom - Footer.GetHeight - 1 - GetShadowOffset);
      end;

      p.R := rf;
      p.GT := FGradientType;
      p.Image := nil;
      p.HatchStyle := FHatchStyle;
      p.ColorFrom := FColor;
      p.ColorTo := FColorTo;
      p.OpacityFrom := FOpacity;
      p.OpacityTo := FOpacityTo;
      p.Angle := 0;
      p.BorderColor := clNone;
      p.BorderWidth := 0;
      p.BorderStyle := DashStyleSolid;

      FillGDIP(p);

      if Rotated then
        sf := TGPStringFormat.Create(StringFormatFlagsDirectionVertical)
      else
        sf := TGPStringFormat.Create;

      f := g.MakeFont(Font);
      fd := g.MakeFont(DisabledFont);

      y := 0;
      if not AutoSize then
         y := InsideRect.Top + Header.GetHeight + Filter.GetHeight;

      case CategoryType of
        alphanumeric:
        begin
          if Numeric then
            stop := 37
          else
            stop := 27;

          for I := 1 to stop - 1 do
          begin
            if (Order = loNumericLast) or not Numeric then
            begin
              if I < 27 then
                ch := chr(ord('A') + (i - 1))
              else
                ch := chr(ord('0') + (i - 27));
            end
            else
            begin
              if I < 11 then
                ch := chr(ord('0') + (i - 1))
              else
                ch := chr(ord('A') + (i - 11));
            end;

            if Rotated then
            begin
              th := ca.TextWidth('W');
              tw := ca.TextHeight('gh');
            end
            else
            begin
              tw := ca.TextWidth('W');
              th := ca.TextHeight('gh');
            end;

            x := 0;
            case Position of
              pLeft: x := InsideRect.Left;
              pRight: x := InsideRect.Right - GetShadowOffset - (tw + FMargin div 2);
            end;

            if AutoSize then
            begin
              pos := InsideRect.Top + Header.GetHeight + Filter.GetHeight;
              y := pos + Round(((InsideRect.Bottom - pos - GetShadowOffset - Footer.GetHeight) / (stop - 1)) * (I - 1));
            end;

            pt := MakePoint(x, y);

            if FChar[I] then
            begin
              b := TGPSolidBrush.Create(ColorToARGB(Font.Color));
              g.DrawString(ch, length(ch), f, pt, sf, b);
              b.Free;
            end
            else
            begin
              b := TGPSolidBrush.Create(ColorToARGB(DisabledFont.Color));
              g.DrawString(ch, length(ch), fd, pt, sf, b);
              b.Free;
            end;

            if not AutoSize then
              y := y + th + LookupBar.Spacing
          end;
        end;
        custom:
        begin
          stop := Categories.Count;

          for I := 0 to stop - 1 do
          begin
            with Categories[I] do
            begin
              if LookupText <> '' then
                customch := LookupText
              else
                customch := Text;

              if Rotated then
              begin
                th := ca.TextWidth(customch);
                tw := ca.TextHeight(customch);
              end
              else
              begin
                tw := ca.TextWidth(customch);
                th := ca.TextHeight(customch);
              end;

              x := 0;
              ximg := 0;
              case Position of
                pLeft:
                begin
                  x := InsideRect.Left + (maxtextw - 4 - tw) div 2;
                  ximg := InsideRect.left;
                  if Assigned(FImages) and Rotated then
                    if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                      ximg := InsideRect.Left + (maxtextw - 4 - FImages.Width) div 2;
                end;
                pRight:
                begin
                  x := InsideRect.Right - 1 - GetShadowOffset - (maxtextw + 4 + tw) div 2;                
                  ximg := InsideRect.Right - 1 - maxtextw;
                  if Assigned(FImages) and Rotated then
                    if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                      ximg := InsideRect.Right - 1 - GetShadowOffset - (maxtextw + 4 + FImages.Width) div 2;
                end;
              end;

              if Assigned(FImages) and not Rotated then
                if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                  x := x + Images.Width div 2;

              if AutoSize then
              begin
                pos := InsideRect.Top + Header.GetHeight + Filter.GetHeight;
                y := pos + Round(((InsideRect.Bottom - pos - GetShadowOffset - Footer.GetHeight) / stop) * I);
              end;

              pt := MakePoint(x, y);
              if Assigned(FImages) and LookupBar.Rotated then
                if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                  pt := MakePoint(x, y + FImages.Height);


              if Length(FCustomChar) > 0 then
              begin
                if FCustomChar[ID] then
                begin
                  b := TGPSolidBrush.Create(ColorToARGB(Font.Color));
                  g.DrawString(customch, length(customch), f, pt, sf, b);
                  b.Free;
                end
                else
                begin
                  b := TGPSolidBrush.Create(ColorToARGB(DisabledFont.Color));
                  g.DrawString(customch, length(customch), fd, pt, sf, b);
                  b.Free;
                end;
              end;

              if Assigned(FImages) then
              begin
                if (ImageIndex <> -1) and (ImageIndex < FImages.Count) then
                begin
                  if Rotated then
                    FImages.Draw(Canvas, Round(ximg), Round(y + 1), ImageIndex)
                  else
                    FImages.Draw(Canvas, Round(ximg + 3), Round(y + 1), ImageIndex)
                end;
              end;

              if not AutoSize then
              begin
                y := y + th + LookupBar.Spacing;
                if Assigned(FImages) and LookupBar.Rotated then
                  if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                    y := y + FImages.Height;
              end;
            end;
          end;
        end;
      end;

      f.Free;
      fd.Free;
      sf.Free;
      ca.Free;
      g.Free;
    end;
  end;
end;

procedure TAdvSmoothListBox.DrawScrollIndicator;
var
  p: TGDIPFillParameters;
  pos, h: Single;
  g: TGPGraphics;
  w: integer;
begin
  if not Assigned(ScrollIndicator) then
    Exit;

  if not ScrollIndicator.Visible then
    Exit;

  if GetFullHeight + Height < Height then
    Exit;


  with ScrollIndicator do
  begin
    w := LookupBar.GetWidth;

    g := TGPGraphics.Create(Canvas.Handle);

    p.Graphics := g;
    p.Path := nil;
    p.Fillpath := false;
    h := 0;
    if (GetFullHeight > 0) and (Self.Height > 0) then
      h := ((Self.Height - Header.GetHeight - Filter.GetHeight - Footer.GetHeight - GetShadowOffset) / (GetFullHeight + Height + GetShadowOffset)) * Self.Height;

    pos := 0;

    if (GetPosition > 0) and (GetFullHeight > 0) then
      pos := (Self.Height - Header.GetHeight - Filter.GetHeight - h - Footer.GetHeight - GetShadowOffset) / GetFullHeight * GetPosition;

    pos := pos + Header.GetHeight + Filter.GetHeight;

    if LookupBar.Position = pRight then
      p.R := MakeRect(InsideRect.Right - w - Width - 2 - GetShadowOffset, pos, Width, h)
    else
      p.R := MakeRect(InsideRect.Right - Width - 2  - GetShadowOffset, pos, Width, h);

    p.GT := GradientType;
    p.ColorFrom := Color;
    p.ColorTo := ColorTo;

    if Style = ssiPhone then
    begin
      p.OpacityFrom := GetAnimationOpacity;
      p.OpacityTo := GetAnimationOpacity;
    end
    else
    begin
      p.OpacityFrom := Opacity;
      p.OpacityTo := Opacity;
    end;

    p.HatchStyle := HatchStyle;
    p.Angle := 0;
    p.Image := nil;
    p.BorderStyle := DashStyleSolid;
    p.BorderColor := clNone;
    p.BorderWidth := 0;

    FillGDIP(p);

    g.Free;
  end;
end;

procedure TAdvSmoothListBox.DrawSection(itemRect: TRect; ch: String; catindex: integer);
var
  p: TGDIPFillParameters;
  ff: TGPFontFamily;
  f: TGPFont;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  x, y: Single;
  pt: TGPPointF;
  g: TGPGraphics;
  cat: TAdvSmoothListBoxCategoryItem;
  ca: TCanvas;
  defaultdraw, defaultdrawcontent: Boolean;
begin
  with Sections do
  begin
    if not Visible then
      exit;

    defaultdraw := True;
    DoSectionDraw(Self, Canvas, itemRect, ch, catindex, defaultdraw);

    if defaultdraw then
    begin
      ca := TCanvas.Create;
      ca.Handle := Canvas.Handle;

      g := TGPGraphics.Create(Canvas.Handle);
      case TextRendering of
        tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
        tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
        tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      end;

      p.Graphics := g;
      p.Path := nil;
      p.Fillpath := false;

      p.R := MakeRect(itemrect.Left, itemRect.Top - GetPosition, itemRect.Right - itemRect.Left, itemRect.Bottom - itemRect.Top);

      p.GT := GradientType;
      p.ColorFrom := Color;
      p.ColorTo := ColorTo;
      p.OpacityFrom := Opacity;
      p.OpacityTo := OpacityTo;
      p.HatchStyle := HatchStyle;
      p.Angle := 0;
      p.Image := nil;

      if BorderWidth = 0 then
        p.BorderColor := clNone
      else
        p.BorderColor := BorderColor;

      p.BorderWidth := BorderWidth;
      p.BorderStyle := DashStyleSolid;

      FillGDIP(p);

      defaultdrawcontent := True;
      DoSectionDrawContent(Self, Canvas, itemRect, ch, catindex, defaultdrawcontent);

      if defaultdrawcontent then
      begin
        ff := TGPFontFamily.Create(FFont.Name);
        if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
        begin
          ff.Free;
          ff := TGPFontFamily.Create('Arial');
        end;

        fs := 0;
        if (fsBold in FFont.Style) then
          fs := fs + 1;
        if (fsItalic in FFont.Style) then
          fs := fs + 2;
        if (fsUnderline in FFont.Style) then
          fs := fs + 4;
        if fsStrikeOut in FFont.Style then
          fs := fs + 8;

        sf := TGPStringFormat.Create;
        f := TGPFont.Create(ff, FFont.Size, fs, UnitPoint);

        x := itemRect.Left + 2;
        y := p.R.Y + 2;

        if Assigned(FImages) then
        begin
          cat := Categories.ItemById(catindex);
          if Assigned(cat) then
          begin
            if (cat.ImageIndex > -1) and (cat.ImageIndex < FImages.Count) then
            begin
              case ImageAlign of
                alLeft:
                begin
                  FImages.Draw(ca, Round(x), Round(y - 1), cat.ImageIndex);
                  pt := MakePoint(x + FImages.Width + 4, y);
                end;
                alRight: FImages.Draw(ca, Round(x + itemRect.Right - 4 - FImages.Width), Round(y - 1), cat.ImageIndex);
              end;
            end
            else
              pt := MakePoint(x, y)
          end;
        end
        else
          pt := MakePoint(x, y);


        b := TGPSolidBrush.Create(ColorToARGB(FFont.Color));
        g.DrawString(ch, length(ch), f, pt, sf, b);
        b.Free;
        ff.free;
        sf.free;
        f.free;
      end;

      g.Free;
      ca.free;
    end;
  end;
end;

procedure TAdvSmoothListBox.DropDown(Item: TAdvSmoothListBoxItem);
var
  DpdPos: TPoint;
  r: TRect;
  w, h, xp, yp: integer;
  tmsif: ISmoothListBox;

  function GetParentWnd: HWnd;
  var
    Last, P: HWnd;
  begin
    P := GetParent((Owner as TWinControl).Handle);
    Last := P;
    while P <> 0 do
    begin
      Last := P;
      P := GetParent(P);
    end;
    Result := Last;
  end;

begin
  h := 0;
  w := 0;
  xp := 0;
  yp := 0;
  if Item.FBLD then
  begin
    w := Item.fgrLeft.Right - Item.fgrLeft.Left;
    h := Item.fgrLeft.Bottom - Item.fgrLeft.Top;
    xp := Item.fgrLeft.Left;
    yp := Item.fgrLeft.Top;
  end
  else if Item.FBRD then
  begin
    w := Item.fgrRight.Right - Item.fgrRight.Left;
    h := Item.fgrRight.Bottom - Item.fgrRight.Top;
    xp := Item.fgrRight.Left;
    yp := Item.fgrRight.Top;
  end;

  if (Parent is TForm) then
  begin
    if (Parent as TForm).FormStyle = fsStayOnTop then
      FDropDownForm.FormStyle := fsStayOnTop;
  end
  else
    FDropDownForm.FormStyle := fsStayOnTop;

  DpdPos.x := xp + -2;
  DpdPos.y := yp + h - 3;
  DpdPos := ClientToScreen(DpdPos);

  SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0); //account for taskbar...

  FDropDownForm.Width := 0;
  FDropDownForm.Height := 0;

  if (DpdPos.y + Item.FDropDownControl.Height > r.Bottom) then
    DpdPos.Y := DpdPos.Y - Item.FDropDownControl.Height - h + 3;

  if (DpdPos.x + Item.FDropDownControl.Width > r.right) then
    DpdPos.x := DpdPos.x - (Item.FDropDownControl.Width - w);

  if Assigned(FDropDownItem.FDropDownControl) then
    if FDropDownItem.FDropDownControl.GetInterface(ISmoothListBox, tmsif) then
      tmsif.Show(FDropDownItem);


  if Assigned(Item.FDropDownControl) then
  begin
    Item.FDropDownControl.Visible := False;
    Item.FDropDownControl.Left := 0;
    Item.FDropDownControl.Top := 0;
  end;

  FDropDownForm.Show;

  if Assigned(Item.FDropDownControl) then
  begin
    Item.FDropDownControl.Parent := FDropDownForm;
    Item.FDropDownControl.Visible := true;
  end;

  FDropDownForm.Left := DpdPos.x;
  FDropDownForm.Top := DpdPos.y;
  if Assigned(Item.FDropDownControl) then
  begin
    FDropDownForm.Width := Item.FDropDownControl.Width;
    FDropDownForm.Height := Item.FDropDownControl.Height;
  end;
  SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);

  Item.FDropDownControl.SetFocus;
end;

procedure TAdvSmoothListBox.DropDownFormDeactivate(Sender: TObject);
var
  Allow: Boolean;
begin
  Allow := true;
  if Assigned(FOnItemDropDownHide) then
    FOnItemDropDownHide(Self, FDropDownItem, Allow);

  if Allow then
  begin
    FDropDownItem.FDropDownControl.Visible := false;
    FDropDownItem.FDropDownControl.parent := Self;
    FDeactivating := true;
    (Sender as TForm).Hide;
    FTimer.Enabled := true;
  end;
end;

procedure TAdvSmoothListBox.EndUpdate;
begin
  Items.EndUpdate;
end;

procedure TAdvSmoothListBox.ExpandAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[i].Level = 0) and not (Items[I].Expanded) then
      Items[I].Expand;
  end;
  Items.EndUpdate;
end;

procedure TAdvSmoothListBox.FilterChanged(Sender: TObject);
begin
  InitDisplayList;
  CalculateRects;
  if Assigned(Filter) then
    Filter.UpdateFilter;
  Changed;
end;

function TAdvSmoothListBox.FindFirstItemWithCategoryID(
  CategoryID: integer): TAdvSmoothListBoxDisplayListItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDisplayList.Count - 1 do
  begin
   result := FDisplayList.GetItem(i);
    with result do
    begin
      if DisplayItem <> nil then
      begin
        if DisplayItem.CategoryID > -1 then
        begin
          if CategoryID = DisplayItem.CategoryID then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.FindFirstItemWithChar(ch: String): TAdvSmoothListBoxDisplayListItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDisplayList.Count - 1 do
  begin
   result := FDisplayList.GetItem(i);
    with result do
    begin
      if DisplayItem <> nil then
      begin
        if Length(DisplayItem.Caption) > 0 then
        begin
          if UpCase(NormalizeSpecialChar(DisplayItem.Caption)[1]) = ch then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.FindFirstSectionWithCategoryID(
  CategoryID: integer): TAdvSmoothListBoxDisplayListItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    result := FDisplayList.GetItem(i);
    with Result do
    begin
      if DisplayItem = nil then
      begin
        if SectionCategoryID <> -1 then
        begin
          if SectionCategoryID = CategoryID then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBox.FindFirstSectionWithChar(ch: String): TAdvSmoothListBoxDisplayListItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDisplayList.Count - 1 do
  begin
    result := FDisplayList.GetItem(i);
    with Result do
    begin
      if DisplayItem = nil then
      begin
        if Length(SectionCaption) > 0 then
        begin
          if UpCase(SectionCaption[1]) = ch then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
end;

{ TAdvSmoothListBoxItem }

procedure TAdvSmoothListBoxItem.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxItem then
  begin
    CopySettings(Source);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.AssignVisuals(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxItem then
  begin
    FHeight := (Source as TAdvSmoothListBoxItem).Height;
    FAutoSize := (Source as TAdvSmoothListBoxItem).AutoSize;
    FCaptionFont.Assign((Source as TAdvSmoothListBoxItem).CaptionFont);
    FCaptionSelectedFont.Assign((Source as TAdvSmoothListBoxItem).CaptionSelectedFont);
    FEnabled := (Source as TAdvSmoothListBoxItem).Enabled;
    FChecked := (Source as TAdvSmoothListBoxItem).Checked;
    FCaptionAlignment := (Source as TAdvSmoothListBoxItem).CaptionAlignment;
    FInfoFont.Assign((Source as TAdvSmoothListBoxItem).InfoFont);
    FInfoSelectedFont.Assign((Source as TAdvSmoothListBoxItem).InfoSelectedFont);
    FGraphicLeftType := (Source as TAdvSmoothListBoxItem).GraphicLeftType;
    FGraphicRightType := (Source as TAdvSmoothListBoxItem).GraphicRightType;
    FGraphicLeft.Assign((Source as TAdvSmoothListBoxItem).GraphicLeft);
    FGraphicLeftSelected.Assign((Source as TAdvSmoothListBoxItem).GraphicLeftSelected);
    FGraphicRight.Assign((Source as TAdvSmoothListBoxItem).GraphicRight);
    FGraphicRightSelected.Assign((Source as TAdvSmoothListBoxItem).GraphicRightSelected);
    FGraphicLeftShow := (Source as TAdvSmoothListBoxItem).GraphicLeftShow;
    FGraphicRightShow := (Source as TAdvSmoothListBoxItem).GraphicRightShow;
    FSplitter := (Source as TAdvSmoothListBoxItem).Splitter;
    FNotesFont.Assign((Source as TAdvSmoothListBoxItem).NotesFont);
    FNotesSelectedFont.Assign((Source as TAdvSmoothListBoxItem).NotesSelectedFont);
    FNotesURLColor := (Source as TAdvSmoothListBoxItem).NotesURLColor;
    FNoteShadowColor := (Source as TAdvSmoothListBoxItem).NotesShadowColor;
    FNotesShadowOffset := (Source as TAdvSmoothListBoxItem).NotesShadowOffset;
    FNotesLocation := (Source as TAdvSmoothListBoxItem).NotesLocation;
    FNotesLeft := (Source as TAdvSmoothListBoxItem).NotesLeft;
    FNotesTop := (Source as TAdvSmoothListBoxItem).NotesTop;
    FControl := (Source as TAdvSmoothListBoxItem).DetailControl;
    FMargin := (Source as TAdvSmoothListBoxItem).CaptionMargin;
    FMarginRight := (Source as TAdvSmoothListBoxItem).CaptionMarginRight;
    FGraphicLeftMargin := (Source as TAdvSmoothListBoxItem).GraphicLeftMargin;
    FGraphicRightMargin := (Source as TAdvSmoothListBoxItem).GraphicRightMargin;
    FSelected := (Source as TAdvSmoothListBoxItem).Selected;
    FProgressMin := (Source as TAdvSmoothListBoxItem).ProgressMinimum;
    FProgressMax := (Source as TAdvSmoothListBoxItem).ProgressMaximum;
    FProgressValue := (Source as TAdvSmoothListBoxItem).ProgressValue;
    FProgressHeight := (Source as TAdvSmoothListBoxItem).ProgressHeight;
    FProgressWidth := (Source as TAdvSmoothListBoxItem).ProgressWidth;
    FProgressPosition := (Source as TAdvSmoothListBoxItem).ProgressPosition;
    FProgressVisible := (Source as TAdvSmoothListBoxItem).ProgressVisible;
    FProgressTop := (Source as TAdvSmoothListBoxItem).ProgressTop;
    FProgressLeft := (Source as TAdvSmoothListBoxItem).ProgressLeft;
    FGraphicLeftWidth := (Source as TAdvSmoothListBoxItem).GraphicLeftWidth;
    FGraphicLeftHeight := (Source as TAdvSmoothListBoxItem).GraphicLeftHeight;
    FGraphicRightWidth := (Source as TAdvSmoothListBoxItem).GraphicRightWidth;
    FGraphicRightHeight := (Source as TAdvSmoothListBoxItem).GraphicRightHeight;
    FButtonCaption := (Source as TAdvSmoothListBoxItem).ButtonCaption;
    FButtonColor := (Source as TAdvSmoothListBoxItem).ButtonColor;
    FBevelColor := (Source as TAdvSmoothListBoxItem).ButtonBevelColor;
    FButtonBevel := (Source as TAdvSmoothListBoxItem).ButtonBevel;
    FButtonShadow := (Source as TAdvSmoothListBoxItem).ButtonShadow;
    FIndent := (Source as TAdvSmoothListBoxItem).Indent;
    FLevel := (Source as TAdvSmoothListBoxItem).Level;
    FExpanded := (Source as TAdvSmoothListBoxItem).Expanded;
    FCaptionWordWrap := (Source as TAdvSmoothListBoxItem).CaptionWordWrap;
    FDeleteButton := (Source as TAdvSmoothListBoxItem).DeleteButton;
    FDeleteButtonVisible := (Source as TAdvSmoothListBoxItem).DeleteButtonVisible;
    FGraphicLeftIndex := (Source as TAdvSmoothListBoxItem).GraphicLeftIndex;
    FGraphicLeftName := (Source as TAdvSmoothListBoxItem).GraphicLeftName;
    FGraphicLeftSelectedName := (Source as TAdvSmoothListBoxItem).GraphicLeftSelectedName;
    FGraphicRightIndex := (Source as TAdvSmoothListBoxItem).GraphicRightIndex;
    FGraphicRightName := (Source as TAdvSmoothListBoxItem).GraphicRightName;
    FGraphicRightSelectedName := (Source as TAdvSmoothListBoxItem).GraphicRightSelectedName;
    FSeparatorLine := (Source as TAdvSmoothListBoxItem).SeparatorLine;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.Changed;
begin
  if Assigned(FOwner) and (FOwner.FUpdateCount = 0) then
  begin
    if Assigned(FOwner.FCurrentControl) and not FOwner.FAnimatingdetail then
      FOwner.FCurrentControl.Left := FOwner.Width;
    FOwner.LookupBar.InitLookupBar;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    if (FOwner.HandleAllocated) then
      if Assigned(FOwner.FOnItemChanged) then
        FOwner.FOnItemChanged(Self, index);
    FOwner.Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.Collapse;
var
  i: integer;
begin
  FExpanded := false;
  with FOwner.Items do
  begin
    BeginUpdate;
    for I := (Self.Index + 1) to Count - 1 do
    begin
      if (Items[I].Level > Self.Level) then
        Items[I].Visible := false
      else
        break;
    end;
    EndUpdate;
  end;
end;

procedure TAdvSmoothListBoxItem.CopySettings(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxItem then
  begin
    FHeight := (Source as TAdvSmoothListBoxItem).Height;
    FAutoSize := (Source as TAdvSmoothListBoxItem).AutoSize;
    FCaptionFont.Assign((Source as TAdvSmoothListBoxItem).CaptionFont);
    FCaptionSelectedFont.Assign((Source as TAdvSmoothListBoxItem).CaptionSelectedFont);
    FCaption := (Source as TAdvSmoothListBoxItem).Caption;
    FEnabled := (Source as TAdvSmoothListBoxItem).Enabled;
    FChecked := (Source as TAdvSmoothListBoxItem).Checked;
    FCaptionAlignment := (Source as TAdvSmoothListBoxItem).CaptionAlignment;
    FNotes := (Source as TAdvSmoothListBoxItem).Notes;
    FInfo := (Source as TAdvSmoothListBoxItem).Info;
    FInfoFont.Assign((Source as TAdvSmoothListBoxItem).InfoFont);
    FInfoSelectedFont.Assign((Source as TAdvSmoothListBoxItem).InfoSelectedFont);
    FGraphicLeftType := (Source as TAdvSmoothListBoxItem).GraphicLeftType;
    FGraphicRightType := (Source as TAdvSmoothListBoxItem).GraphicRightType;
    FGraphicLeft.Assign((Source as TAdvSmoothListBoxItem).GraphicLeft);
    FGraphicRight.Assign((Source as TAdvSmoothListBoxItem).GraphicRight);
    FGraphicLeftShow := (Source as TAdvSmoothListBoxItem).GraphicLeftShow;
    FGraphicRightShow := (Source as TAdvSmoothListBoxItem).GraphicRightShow;
    FSplitter := (Source as TAdvSmoothListBoxItem).Splitter;
    FNotesURLColor := (Source as TAdvSmoothListBoxItem).NotesURLColor;
    FNoteshadowColor := (Source as TAdvSmoothListBoxItem).NotesShadowColor;
    FNotesShadowOffset := (Source as TAdvSmoothListBoxItem).NotesShadowOffset;
    FNotesLocation := (Source as TAdvSmoothListBoxItem).NotesLocation;
    FNotesLeft := (Source as TAdvSmoothListBoxItem).NotesLeft;
    FNotesTop := (Source as TAdvSmoothListBoxItem).NotesTop;
    FControl := (Source as TAdvSmoothListBoxItem).DetailControl;
    FMargin := (Source as TAdvSmoothListBoxItem).CaptionMargin;
    FMarginRight := (Source as TAdvSmoothListBoxItem).CaptionMarginRight;
    FOfficeHint.Assign((Source as TAdvSmoothListBoxItem).OfficeHint);
    FProgressMin := (Source as TAdvSmoothListBoxItem).ProgressMinimum;
    FProgressMax := (Source as TAdvSmoothListBoxItem).ProgressMaximum;
    FProgressValue := (Source as TAdvSmoothListBoxItem).ProgressValue;
    FProgressHeight := (Source as TAdvSmoothListBoxItem).ProgressHeight;
    FProgressWidth := (Source as TAdvSmoothListBoxItem).ProgressWidth;
    FProgressPosition := (Source as TAdvSmoothListBoxItem).ProgressPosition;
    FProgressVisible := (Source as TAdvSmoothListBoxItem).ProgressVisible;
    FProgressTop := (Source as TAdvSmoothListBoxItem).ProgressTop;
    FProgressLeft := (Source as TAdvSmoothListBoxItem).ProgressLeft;
    FVisible := (Source as TAdvSmoothListBoxItem).Visible;
    FIndent :=(Source as TAdvSmoothListBoxItem).Indent;
    Fhint := (Source as TAdvSmoothListBoxItem).Hint;
    FCaptionWordWrap := (Source as TAdvSmoothListBoxItem).CaptionWordWrap;
    FGraphicLeftMargin := (Source as TAdvSmoothListBoxItem).GraphicLeftMargin;
    FGraphicRightMargin := (Source as TAdvSmoothListBoxItem).GraphicRightMargin;
    FGraphicLeftWidth := (Source as TAdvSmoothListBoxItem).GraphicLeftWidth;
    FGraphicLeftHeight := (Source as TAdvSmoothListBoxItem).GraphicLeftHeight;
    FGraphicRightWidth := (Source as TAdvSmoothListBoxItem).GraphicRightWidth;
    FGraphicRightHeight := (Source as TAdvSmoothListBoxItem).GraphicRightHeight;
    FNotesFont.Assign((Source as TAdvSmoothListBoxItem).NotesFont);
    FNotesSelectedFont.Assign((Source as TAdvSmoothListBoxItem).NotesSelectedFont);
    FTag := (Source as TAdvSmoothListBoxItem).Tag;
    FObject := (Source as TAdvSmoothListBoxItem).ItemObject;
    FSelected := (Source as TAdvSmoothListBoxItem).Selected;
    FCategoryId := (Source as TAdvSmoothListBoxItem).CategoryID;
    FProgressMargin := (Source as TAdvSmoothListBoxItem).ProgressMargin;
    FButtonColor := (Source as TAdvSmoothListBoxItem).ButtonColor;
    FButtonBevel := (Source as TAdvSmoothListBoxItem).ButtonBevel;
    FBevelColor := (Source as TAdvSmoothListBoxItem).ButtonBevelColor;
    FButtonShadow := (Source as TAdvSmoothListBoxItem).ButtonShadow;
    FButtonCaption := (Source as TAdvSmoothListBoxItem).ButtonCaption;
    FLevel := (Source as TAdvSmoothListBoxItem).Level;
    FExpanded := (Source as TAdvSmoothListBoxItem).Expanded;
    FDeleteButton := (Source as TAdvSmoothListBoxItem).DeleteButton;
    FDeleteButtonVisible := (Source as TAdvSmoothListBoxItem).DeleteButtonVisible;
    FGraphicLeftIndex := (Source as TAdvSmoothListBoxItem).GraphicLeftIndex;
    FGraphicLeftName := (Source as TAdvSmoothListBoxItem).GraphicLeftName;
    FGraphicRightIndex := (Source as TAdvSmoothListBoxItem).GraphicRightIndex;
    FGraphicRightName := (Source as TAdvSmoothListBoxItem).GraphicRightName;
    FAlternate := (Source as TAdvSmoothListBoxItem).Alternate;
    Changed;
  end;
end;

constructor TAdvSmoothListBoxItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothListBoxItems).FOwner;

  FMargin := 3;
  FMarginRight := 3;
  FCaptionWordWrap := True;
  FAlternate := lbaAuto;
  FChecked := false;
  FAutoSize := False;
  FNotesTop := 0;
  FNotesLeft := 0;
  FHeight := -1;
  FNoteshadowColor := clGray;
  FNotesLocation :=  plCenterCenter;
  FNotesURLColor := clBlue;
  FNotesShadowOffset := 5;
  FSplitter := false;
  FEnabled := true;
  FCaptionAlignment := taLeftJustify;
  FNotes := '';
  FInfo := '';
  FSelected := false;
  FGraphicLeftType := gtNone;
  FGraphicLeftShow := gsAlways;
  FGraphicLeft := TAdvGDIPPicture.Create;
  FGraphicLeft.DoubleBuffered := true;
  FGraphicLeft.OnChange := PictureChanged;

  FGraphicLeftSelected := TAdvGDIPPicture.Create;
  FGraphicLeftSelected.DoubleBuffered := true;
  FGraphicLeftSelected.OnChange := PictureChanged;

  FGraphicLeftMargin := 3;
  FGraphicLeftWidth := 30;
  FGraphicLeftHeight := 25;
  FVisible := true;
  FCategoryID := -1;
  FIndent := 0;
  FLevel := 0;
  FExpanded := true;

  FButtonColor := clGray;
  FButtonShadow := false;
  FButtonBevel := true;
  FBevelColor := clWhite;
  FButtonCaption := '';

  FProgressMin := 0;
  FProgressMax := 100;
  FProgressValue := 0;
  FProgressHeight := 15;
  FProgressWidth := 130;
  FProgressPosition := plBottomCenter;
  FProgressMargin := 3;
  FProgressVisible := false;
  FProgressLeft := 0;
  FProgressTop := 0;
  FTag := 0;

  FGraphicRightType := gtNone;
  FGraphicRightShow := gsAlways;
  FGraphicRight := TAdvGDIPPicture.Create;
  FGraphicRight.DoubleBuffered := true;
  FGraphicRight.OnChange := PictureChanged;

  FGraphicRightSelected := TAdvGDIPPicture.Create;
  FGraphicRightSelected.DoubleBuffered := true;
  FGraphicRightSelected.OnChange := PictureChanged;

  FGraphicRightMargin := 3;
  FGraphicRightWidth := 30;
  FGraphicRightHeight := 25;

  FGraphicRightIndex := -1;
  FGraphicLeftIndex := -1;

  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := FontChanged;

  FNotesFont := TFont.Create;
  FNotesFont.OnChange := FontChanged;

  FInfoFont := TFont.Create;
  FInfoFont.OnChange := FontChanged;

  FCaptionSelectedFont := TFont.Create;
  FCaptionSelectedFont.OnChange := FontChanged;

  FNotesSelectedFont := TFont.Create;
  FNotesSelectedFont.OnChange := FontChanged;

  FInfoSelectedFont := TFont.Create;
  FInfoSelectedFont.OnChange := FontChanged;


  FDeleteButton := False;

  FDeleteButtonVisible := False;
  FSeparatorLine := False;

  FOfficeHint := TAdvHintInfo.Create;

  CreateDropDownItem;

  {$IFNDEF DELPHI9_LVL}
  FCaptionFont.Name := 'Tahoma';
  FNotesFont.Name := 'Tahoma';
  FInfoFont.Name := 'Tahoma';
  FCaptionSelectedFont.Name := 'Tahoma';
  FNotesSelectedFont.Name := 'Tahoma';
  FInfoSelectedFont.Name := 'Tahoma';
  {$ENDIF}

  Changed;

  if Assigned(Collection) then
    if Assigned((Collection as TAdvSmoothListBoxItems).FOwner.FDefaultItem) then
      Assign((Collection as TAdvSmoothListBoxItems).FOwner.FDefaultItem);

  if Assigned(FOwner) then
    if (csDesigning in FOwner.ComponentState) and not
      ((csReading in FOwner.Owner.ComponentState) or (csLoading in FOwner.Owner.ComponentState)) then
  begin
    FCaption := 'Item ' + inttostr(Index);
  end;
end;

procedure TAdvSmoothListBoxItem.CreateDropDownItem;
var
  tmsif: ISmoothListBox;
begin
  if Assigned(FOwner) and Assigned(FOwner.DropDownControlClass) then
  begin
    FOwner.FCreatedThroughClass := True;
    FDropDownControl := FOwner.DropDownControlClass.Create(FOwner);
    FDropDownControl.Visible := false;
    if Assigned(FDropDownControl) then
    begin
      if FDropDownControl.GetInterface(ISmoothListBox, tmsif) then
      begin
        tmsif.SetOwner(FOwner, Self);
      end;
    end;
  end;
end;

destructor TAdvSmoothListBoxItem.Destroy;
var
  idx: integer;
begin
  FGraphicLeft.Free;
  FGraphicRight.Free;

  FGraphicLeftSelected.Free;
  FGraphicRightSelected.Free;

  FOfficeHint.Free;
  FCaptionFont.Free;
  FNotesFont.Free;
  FInfoFont.Free;
  FCaptionSelectedFont.Free;
  FNotesSelectedFont.Free;
  FInfoSelectedFont.Free;
  if Assigned(FOwner) and Assigned(FOwner.DropDownControlClass) then
  begin
    if Assigned(FDropDownControl) and FOwner.FCreatedThroughClass then
    begin
      FOwner.FCreatedThroughClass := False;
      FDropDownControl.Free;
      FDropDownControl := nil;
    end;
  end;

  idx := -1;
  if Assigned(FOwner) and not (csDestroying in FOwner.ComponentState) then
  begin
    if FOwner.SelectedItemIndex <> -1 then
      idx := Index;
  end;

  inherited;

  if Assigned(FOwner) and not (csDestroying in FOwner.ComponentState) then
  begin
   if (idx <> -1) and not FOwner.MultiSelect then
      FOwner.SelectedItemIndex := Max(0, Min(idx, FOwner.Items.Count - 1));
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.Draw(ACanvas: TCanvas; R: TRect; DisplayIndex: integer; DragItem: Boolean = false; Transparent: boolean = false; PaintFocus: Boolean = True);
var
  o: TAdvSmoothListBox;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  ca: TCanvas;
  g, gpic: TGPGraphics;
  ff: TGPFontFamily;
  sf: TGPStringFormat;
  f: TGPFont;
  fs: integer;
  b: TGPSolidBrush;
  showgraphic: Boolean;
  defaultdraw: Boolean;
  c, i, n: String;
  ir: TGPRectF;
  x, y: integer;
  DChecked: Integer;
  ntitem, pritem: TAdvSmoothListBoxItem;
  rt: TFillRoundingType;
  htheme: THandle;
  ThemeStyle: dword;
  bx, by, bw, bh: integer;
  focustopitem: Boolean;
  pr, frameleftr, framerightr, fhtmlrvar: TRect;
  rk, infr: TGPRectF;
  GraphicLeftEmpty, GraphicRightEmpty: Boolean;
  rgn: TGPRegion;
  delr: TGPRectF;
  pth, pthinner: TGPGraphicsPath;
  p: TGPPen;
  br: TGPSolidBrush;
  ft: TFont;
  ftg: TGPFont;
  ftsf: TGPStringFormat;
  ftb: TGPSolidBrush;
  delc: TColor;
  imgbmp: TBitmap;
  gpbmp: TGPBitmap;
  gp: TGPGraphics;
  h, himg: HDC;
  cah: TCanvas;
  pic: TAdvGDIPPicture;
  UseHTML: Boolean;
  fr: TGPRectF;
  sb: TGPSolidBrush;
  isSel: boolean;
  captionf, notesf, infof: TFont;
  fnormal, fdisabled, fselected: TGDIPFill;
  chk: Boolean;
begin
  o := FOwner;
  isSel := false;

  with o do
  begin
    if FMetroStyle then
      r := Bounds(r.Left - 1, r.Top, r.Right - r.Left + 1, r.Bottom - r.Top);

    captionf := TFont.Create;
    notesf := TFont.Create;
    infof := TFont.Create;

    fnormal := TGDIPFill.Create;
    fdisabled := TGDIPFill.Create;
    fselected := TGDIPFill.Create;

    fDisabled.Assign(ItemAppearance.FillDisabled);
    chk := False;
    case Alternate of
      lbaAuto: chk := Odd(Index);
      lbaLeft: chk := False;
      lbaRight: chk := True;
    end;

    if chk and (Layout = lblBubble) then
    begin
      fNormal.Assign(ItemAppearance.FillAlternate);
      fSelected.Assign(ItemAppearance.FillSelectedAlternate);
    end
    else
    begin
      fNormal.Assign(ItemAppearance.Fill);
      fSelected.Assign(ItemAppearance.FillSelected);
    end;


    if Assigned(OnItemCustomizeFill) then
      OnItemCustomizeFill(self, self, fnormal, fdisabled, fselected);


    if (DisplayIndex < 0) or (DisplayIndex >= FDisplayList.Count) then
      Exit;

    if (R.Left < 0) and (R.Right < 0) then
      Exit;

    if FDeleteButton and DeleteButtonVisible then
      r.Right := r.Right + ItemAppearance.DeleteButtonWidth;

    defaultdraw := true;
    if (o.FFill.BorderWidth > 0) and (o.FFill.BorderColor <> clNone) then
      DoItemBkgDraw(self, ACanvas, FDisplayList.GetItem(DisplayIndex).DisplayItem.Index, Bounds(r.Left, r.Top - GetPosition, r.Right - r.Left, r.Bottom - r.Top), defaultdraw)
    else
      DoItemBkgDraw(self, ACanvas, FDisplayList.GetItem(DisplayIndex).DisplayItem.Index, Bounds(r.Left, r.Top - GetPosition, r.Right - r.Left - 1, r.Bottom - r.Top), defaultdraw);

    g := TGPGraphics.Create(ACanvas.Handle);
    case TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;

    captionf.Assign(CaptionFont);
    notesf.Assign(NotesFont);
    infof.Assign(InfoFont);

    if Self.Selected then
    begin
      captionf.Assign(CaptionSelectedFont);
      notesf.Assign(NotesSelectedFont);
      infof.Assign(InfoSelectedFont);
    end;

    if Assigned(OnItemCustomizeFont) then
      OnItemCustomizeFont(Self, Self, captionf, notesf, infof);


    if (o.FFill.BorderWidth > 0) and (o.FFill.BorderColor <> clNone) then
    begin
      if not DragItem then
        ir := MakeRect(r.Left, r.Top - GetPosition, r.Right - r.Left, r.Bottom - R.Top)
      else
        ir := MakeRect(0, 0, r.Right - r.Left, r.Bottom - R.Top)
    end
    else
    begin
      if not DragItem then
        ir := MakeRect(r.Left, r.Top - GetPosition, r.Right - r.Left - 1, r.Bottom - R.Top)
      else
        ir := MakeRect(0, 0, r.Right - r.Left - 1, r.Bottom - R.Top)
    end;

    if (FDisplayList.GetItem(DisplayIndex).DisplayItem.Selected) then
    begin
      //ir.Y := ir.Y - 3;
      //ir.Height := ir.Height + 4;
    end;

    if defaultdraw then
    begin

      pritem := Items[Max(0, Index - 1)];
      ntitem := Items[Min(Items.Count - 1, Index + 1)];

      if (pritem.Index <> Index) and (ntitem.Index <> Index) then
      begin
        if (ItemAppearance.VerticalSpacing > 0) or (pritem.Splitter and ntitem.Splitter) then
          rt := rtBoth
        else
        begin
          if pritem.Splitter then
            rt := rtTop
          else if ntitem.Splitter then
            rt := rtBottom
          else
            rt := rtNone;
        end;
      end
      else
      begin
        if ItemAppearance.VerticalSpacing = 0 then
        begin
          if (prItem.Index = Index) and not ntItem.Splitter then
            rt := rtTop
          else if (ntItem.Index = Index) and not prItem.Splitter then
            rt := rtBottom
          else
            rt := rtBoth;
        end
        else
          rt := rtBoth;
      end;

      focustopitem := (FFocusedItemIndex = Index);

      if FEnabled then
      begin
        if ((FSelectedItemIndex = Index) or (MultiSelect and Selected)) and not Transparent then
        begin
          isSel := true;
          fselected.BeginUpdate;
          fselected.RoundingType := rt;
          fselected.Focus := FFocused and TabStop and ShowFocus and PaintFocus and focustopitem;
          fselected.FocusColor := FocusColor;
          fselected.EndUpdate;
          ir.Height := ir.Height - 1;
          ir := fselected.Fill(g, ir);
        end
        else
        begin
          fnormal.BeginUpdate;
          fnormal.RoundingType := rt;
          fnormal.Focus := FFocused and TabStop and ShowFocus and PaintFocus and focustopitem;
          fnormal.FocusColor := FocusColor;
          fnormal.EndUpdate;
          ir := fnormal.Fill(g, ir);
        end;
      end
      else
      begin
        fdisabled.BeginUpdate;
        fdisabled.RoundingType := rt;
        fdisabled.EndUpdate;
        ir := fdisabled.Fill(g, ir);
      end;
    end;

    c := Self.Caption;
    i := Self.Info;
    n := Self.Notes;
    GraphicLeftEmpty := GraphicLeft.Empty;
    GraphicRightEmpty := GraphicRight.Empty;

    DoItemText(Self, FDisplayList.GetItem(DisplayIndex).DisplayItem.Index, c, i, n);

    if SetDisplayItemValues and not (csDesigning in ComponentState) then
    begin
      if (c <> Self.Caption) or (i <> Self.Info) or (n <> Self.Notes)
        or (GraphicLeftEmpty <> GraphicLeft.Empty) or (GraphicRightEmpty <> GraphicRight.Empty) then
      begin
        Self.FCaption := c;
        Self.FInfo := i;
        Self.FNotes := n;
        if (Self.GraphicLeftType = gtNone) and not Self.GraphicLeft.Empty then
          Self.GraphicLeftType := gtImage;
        if (Self.GraphicRightType = gtNone) and not Self.GraphicRight.Empty then
          Self.GraphicRightType := gtImage;
        CalculateRect(Self);
      end;
    end;

    if i <> '' then
    begin
      ff := TGPFontFamily.Create(infof.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in infof.Style) then
        fs := fs + 1;
      if (fsItalic in infof.Style) then
        fs := fs + 2;
      if (fsUnderline in infof.Style) then
        fs := fs + 4;
      if fsStrikeOut in infof.Style then
        fs := fs + 8;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, infof.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(infof.Color));

      if DragItem then
        rk := MakeRect(finfor.X, finfor.Y - R.Top, finfor.Width, finfor.Height)
      else
        rk := MakeRect(finfor.X, finfor.Y - Getposition, finfor.Width, finfor.Height);

      if Assigned(OnItemInfoRect) then
      begin
        OnItemInfoRect(Self, Self, i, rk);
        sf.SetTrimming(StringTrimmingEllipsisWord);
      end;

      if ItemAppearance.UseInfoFill then
      begin
        infr := rk;
        infr.X := infr.X - 2;
        infr.Width := infr.Width + 2;

        if FEnabled then
        begin
          if ((FSelectedItemIndex = Index) or (MultiSelect and Selected)) and not Transparent then
            ItemAppearance.InfoFillSelected.Fill(g, infr)
          else
            ItemAppearance.InfoFill.Fill(g, infr);
        end
        else
          ItemAppearance.InfoFillDisabled.Fill(g, infr);
      end;

      g.DrawString(i, Length(i), f, rk, sf, b);

      b.free;
      ff.free;
      sf.Free;
      f.free;
    end;

    if c <> '' then
    begin
      ff := TGPFontFamily.Create(captionf.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in captionf.Style) then
        fs := fs + 1;
      if (fsItalic in captionf.Style) then
        fs := fs + 2;
      if (fsUnderline in captionf.Style) then
        fs := fs + 4;
      if fsStrikeOut in captionf.Style then
        fs := fs + 8;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, captionf.Size, fs, UnitPoint);
      b := TGPSolidBrush.Create(ColorToARGB(captionf.Color));


      if DragItem then
        rk := MakeRect(fcaptionr.X, fcaptionr.Y - r.Top, fcaptionr.Width , fcaptionr.Height)
      else
        rk := MakeRect(fcaptionr.X, fcaptionr.Y - GetPosition, fcaptionr.Width, fcaptionr.Height);

      if Assigned(OnItemCaptionRect) then
      begin
        OnItemCaptionRect(Self, Self, c, rk);
        sf.SetTrimming(StringTrimmingEllipsisWord);
      end;

      UseHTML := (Pos('</', c) > 0) or (Pos('/>', c) > 0) or (Pos('<BR>',uppercase(c)) > 0);

      if UseHTML then
      begin
        fhtmlrvar := Bounds(round(rk.x), Round(rk.y), round(rk.Width), round(rk.Height));

        HTMLDrawGDIP(g, captionf, c , fhtmlrvar, FImages, 0,0,-1,-1,NotesShadowOffset,False,false,false,false,
          False,False,true,1.0,NotesURLColor,clNone,clNone,NotesShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);
      end
      else
        g.DrawString(c, Length(c), f, rk, sf, b);

      b.free;
      ff.free;
      sf.Free;
      f.free;
    end;

    if n <> '' then
    begin
      fn := n;

      fhtmlrvar := Bounds(Fhtmlr.Left, Fhtmlr.Top - Getposition, Fhtmlr.Right - fhtmlr.Left, Fhtmlr.Bottom - fhtmlr.Top);

      if Assigned(OnItemNotesRect) then
        OnItemNotesRect(Self, Self, n, fhtmlrvar);

      rgn := TGPRegion.Create(MakeRect(r.Left, r.Top - GetPosition, r.Right - r.Left, r.Bottom - r.Top));
      g.SetClip(rgn);
      UseHTML := (Pos('</', n) > 0) or (Pos('/>', n) > 0) or (Pos('<BR>',uppercase(n)) > 0);
      if UseHTML then
      begin
        HTMLDrawGDIP(g, notesf,n, fhtmlrvar,FImages, 0,0,-1,-1,NotesShadowOffset,False,false,false,false,
          False,False,true,1.0,NotesURLColor,clNone,clNone,NotesShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);
      end
      else
      begin
        f := g.MakeFont(notesf);
        sb := TGPSolidBrush.Create(MakeColor(255, notesf.Color));
        sf := TGPStringFormat.Create;
        g.DrawString(n, Length(n), f, MakeRect(fhtmlrvar.Left, fhtmlrvar.Top, fhtmlrvar.Right - fhtmlrvar.Left, fhtmlrvar.Bottom - fhtmlrvar.Top), sf, sb);
        sf.Free;
        sb.Free;
        f.Free;
      end;

      g.ResetClip;
      rgn.Free;
    end;

    if FDisplayList.GetItem(DisplayIndex).DisplayItem.SeparatorLine then
    begin
      fhtmlrvar := Bounds(R.Left, r.Top - GetPosition, R.Right, r.Bottom - r.Top);
      p := TGPPen.Create(MakeColor(255, ItemAppearance.SeparatorLineColor));

      g.DrawLine(p, fhtmlrvar.Left, fhtmlrvar.Top - 2, fhtmlrvar.Right, fhtmlrvar.Top - 2);
      p.Free;
      p := TGPPen.Create(MakeColor(50, ItemAppearance.SeparatorLineShadowColor));
      g.DrawLine(p, fhtmlrvar.Left, fhtmlrvar.Top - 1, fhtmlrvar.Right, fhtmlrvar.Top - 1);
      p.Free;
    end;


    defaultdraw := true;
    DoItemDraw(self, ACanvas, FDisplayList.GetItem(DisplayIndex).DisplayItem.Index, Bounds(R.Left, r.Top - GetPosition, R.Right, r.Bottom - r.Top), defaultdraw);

    if defaultdraw then
    begin
      ca := TCanvas.Create;
      h := g.GetHDC;
      ca.Handle := h;

      DChecked := 0;
      ThemeStyle := 0;

      showgraphic := false;
      case GraphicLeftShow of
        gsAlways: showgraphic := true;
        gsSelected:  showgraphic := (FSelectedItemIndex = Index);
        gsEnabled: showgraphic := FEnabled;
      end;

      if showgraphic then
      begin
        fgrLeft := Bounds(R.Left + GraphicLeftMargin, r.Top - GetPosition + (R.Bottom - R.Top - FGraphicLeftHeight) div 2,
          FGraphicLeftWidth, FGraphicLeftHeight);

        frameleftr := Bounds(fgrLeft.Left + (fgrLeft.Right - fgrLeft.Left - 17) div 2, fgrLeft.Top + (fgrLeft.Bottom - fgrLeft.Top - 17) div 2, 17, 17);

        if Checked or FButtonLeft then
        begin
          case GraphicLeftType of
            gtCheckBox:
              begin
                DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := CBS_CHECKEDNORMAL
                else
                  ThemeStyle := CBS_CHECKEDDISABLED;
              end;
            gtRadio:
              begin
                DChecked := DFCS_BUTTONRADIO or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := RBS_CHECKEDNORMAL
                else
                  ThemeStyle := RBS_CHECKEDDISABLED;
              end;
            gtButton:
              begin
                DChecked := DFCS_BUTTONPUSH;
                if FEnabled then
                  ThemeStyle := PBS_PRESSED
                else
                  ThemeStyle := PBS_DISABLED;
              end;
          end;
        end
        else
        begin
          case GraphicLeftType of
            gtCheckBox:
              begin
                DChecked := DFCS_BUTTONCHECK;
                if FEnabled then
                  ThemeStyle := CBS_UNCHECKEDNORMAL
                else
                  ThemeStyle := CBS_UNCHECKEDDISABLED;

              end;
            gtRadio:
              begin
                DChecked := DFCS_BUTTONRADIO;
                if FEnabled then
                  ThemeStyle := RBS_UNCHECKEDNORMAL
                else
                  ThemeStyle := RBS_UNCHECKEDDISABLED;

              end;
            gtButton:
              begin
                DChecked := DFCS_BUTTONPUSH;
                if FEnabled then
                  ThemeStyle := PBS_NORMAL
                else
                  ThemeStyle := PBS_DISABLED;
              end;
          end;
        end;

        if not Enabled then
          DChecked := DChecked or DFCS_INACTIVE;


        case GraphicLeftType of
          gtCheckBox:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_CHECKBOX,ThemeStyle,@fgrLeft,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,frameleftr,DFC_BUTTON, DChecked);
          gtRadio:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_RADIOBUTTON,ThemeStyle,@fgrLeft,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,frameleftr,DFC_BUTTON, DChecked);
          gtButton:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_PUSHBUTTON,ThemeStyle,@fgrLeft,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,frameleftr,DFC_BUTTON,DChecked);

          gtNode:
            begin
              gpic := TGPGraphics.Create(ca.Handle);

              if Expanded then
              begin
                if Assigned(PictureContainer) and (ItemAppearance.NodeOpenName <> '') then
                begin
                  pic := PictureContainer.FindPicture(ItemAppearance.NodeOpenName);
                  if Assigned(pic) then
                    pic.GDIPDraw(gpic, fgrLeft);
                end
                else
                  ItemAppearance.NodeOpen.GDIPDraw(gpic, fgrLeft)
              end
              else
              begin
                if Assigned(PictureContainer) and (ItemAppearance.NodeClosedName <> '') then
                begin
                  pic := PictureContainer.FindPicture(ItemAppearance.NodeClosedName);
                  if Assigned(pic) then
                    pic.GDIPDraw(gpic, fgrLeft);
                end
                else
                  ItemAppearance.NodeClosed.GDIPDraw(gpic, fgrLeft);
              end;

              gpic.Free;
            end;

          gtDetailImage, gtImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);

            if isSel and not GraphicLeftSelected.Empty then
              GraphicLeftSelected.GDIPDraw(gpic, fgrLeft)
            else
              GraphicLeft.GDIPDraw(gpic, fgrLeft);

            if Assigned(PictureContainer) then
            begin
              if isSel and (GraphicLeftSelectedName <> '') then
                pic := PictureContainer.FindPicture(GraphicLeftSelectedName)
              else
                pic := PictureContainer.FindPicture(GraphicLeftName);

              if Assigned(pic) then
                pic.GDIPDraw(gpic, fgrLeft);
            end;

            if Assigned(Images) then
            begin
              if (GraphicLeftIndex >= -1) and (GraphicLeftIndex <= Images.Count - 1) then
              begin
                imgbmp := TBitmap.Create;
                Images.GetBitmap(GraphicLeftIndex, imgbmp);
                if not imgbmp.Empty then
                begin
                  himg := gpic.GetHDC;
                  cah := TCanvas.Create;
                  cah.Handle := himg;
                  Images.Draw(cah, fgrLeft.Left + (fgrleft.Right - fgrleft.Left - imgbmp.Width) div 2,
                    fgrLeft.Top + (fgrleft.Bottom - fgrleft.Top - imgbmp.Height) div 2, GraphicLeftIndex);
                  cah.Free;
                  gpic.ReleaseHDC(himg);
                end;
                imgbmp.Free;
              end;
            end;
            gpic.Free;
          end;
          gtCommonImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            ItemImage.GDIPDraw(gpic, fgrLeft);
            gpic.Free;
          end;
          gtCommonDetailImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            DetailItemImage.GDIPDraw(gpic, fgrLeft);
            gpic.Free;
          end;
          gtSmoothButton, gtDropDownButton:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            gpic.SetSmoothingMode(SmoothingModeAntiAlias);
            bx := fgrLeft.Left;
            by := fgrLeft.Top;
            bw := GraphicLeftWidth;
            bh := GraphicLeftHeight;
            FOwner.FItemAppearance.FButtonAppearance.Draw(gpic, ButtonCaption, bx, by, bw, bh, 0, 0,
              ButtonColor, clNone, ButtonBevelColor, FOwner.FItemAppearance.FButtonAppearance.Font.Color, ButtonShadow, FButtonLeft and FMouseDown, ButtonBevel, false, false,
              rtBoth, FGraphicLeft, 0, 0, true, GraphicLeftIndex, GraphicLeftName);
            gpic.Free;
          end;
        end;
      end;

      showgraphic := false;
      case GraphicRightShow of
        gsAlways: showgraphic := true;
        gsSelected:  showgraphic := (FSelectedItemIndex = Index);
        gsEnabled: showgraphic := FEnabled;
      end;

      if showgraphic then
      begin
        fgrRight := Bounds(R.Right - GraphicRightMargin - FGraphicRightWidth - ScrollIndicator.GetWidth,
          r.Top - GetPosition + (R.Bottom - R.Top - FGraphicRightHeight) div 2, FGraphicRightWidth, FGraphicRightHeight);

        framerightr := Bounds(fgrRight.Left + (fgrRight.Right - fgrRight.Left - 17) div 2, fgrRight.Top + (fgrRight.Bottom - fgrRight.Top - 17) div 2, 17, 17);

        if Checked or FButtonRight then
        begin
          case GraphicRightType of
            gtCheckBox:
              begin
                DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := CBS_CHECKEDNORMAL
                else
                  ThemeStyle := CBS_CHECKEDDISABLED;
              end;
            gtRadio:
              begin
                DChecked := DFCS_BUTTONRADIO or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := RBS_CHECKEDNORMAL
                else
                  ThemeStyle := RBS_CHECKEDDISABLED;
              end;
            gtButton:
              begin
                DChecked := DFCS_BUTTONPUSH;
                if FEnabled then
                  ThemeStyle := PBS_PRESSED
                else
                  ThemeStyle := PBS_DISABLED;
              end;
          end;
        end
        else
        begin
          case GraphicRightType of
            gtCheckBox:
              begin
                DChecked := DFCS_BUTTONCHECK;
                if FEnabled then
                  ThemeStyle := CBS_UNCHECKEDNORMAL
                else
                  ThemeStyle := CBS_UNCHECKEDDISABLED;

              end;
            gtRadio:
              begin
                DChecked := DFCS_BUTTONRADIO;
                if FEnabled then
                  ThemeStyle := RBS_UNCHECKEDNORMAL
                else
                  ThemeStyle := RBS_UNCHECKEDDISABLED;

              end;
            gtButton:
              begin
                DChecked := DFCS_BUTTONPUSH;
                if FEnabled then
                  ThemeStyle := PBS_NORMAL
                else
                  ThemeStyle := PBS_DISABLED;
              end;
          end;
        end;

        if not Enabled then
          DChecked := DChecked or DFCS_INACTIVE;

        case GraphicRightType of
          gtCheckBox:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_CHECKBOX,ThemeStyle,@fgrRight,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,framerightr,DFC_BUTTON, DChecked);
          gtRadio:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_RADIOBUTTON,ThemeStyle,@fgrRight,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,framerightr,DFC_BUTTON, DChecked);
          gtButton:
            if FIsWinXP and IsThemeActive then
            begin
              htheme := OpenThemeData(Handle,'button');
              DrawThemeBackground(HTheme,ca.Handle, BP_PUSHBUTTON,ThemeStyle,@fgrRight,nil);
              CloseThemeData(htheme);
            end
            else
              DrawFrameControl(ca.Handle,framerightr,DFC_BUTTON,DChecked);

          gtDetailImage, gtImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);

            if isSel and not GraphicRightSelected.Empty then
              GraphicRightSelected.GDIPDraw(gpic, fgrRight)
            else
              GraphicRight.GDIPDraw(gpic, fgrRight);

            if Assigned(PictureContainer) then
            begin
              if isSel and (GraphicRightSelectedName <> '') then
                pic := PictureContainer.FindPicture(GraphicRightSelectedName)
              else
                pic := PictureContainer.FindPicture(GraphicRightName);

              if Assigned(pic) then
                pic.GDIPDraw(gpic, fgrRight);
            end;

            if Assigned(Images) then
            begin
              if (GraphicRightIndex >= -1) and (GraphicRightIndex <= Images.Count - 1) then
              begin
                imgbmp := TBitmap.Create;
                Images.GetBitmap(GraphicRightIndex, imgbmp);
                if not imgbmp.Empty then
                begin
                  himg := gpic.GetHDC;
                  cah := TCanvas.Create;
                  cah.Handle := himg;
                  Images.Draw(cah, fgrRight.Left + (fgrRight.Right - fgrRight.Left - imgbmp.Width) div 2,
                    fgrRight.Top + (fgrRight.Bottom - fgrRight.Top - imgbmp.Height) div 2, GraphicRightIndex);
                  cah.Free;
                  gpic.ReleaseHDC(himg);
                end;
                imgbmp.Free;
              end;
            end;
            gpic.Free;
          end;
          gtCommonDetailImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            DetailItemImage.GDIPDraw(gpic, fgrRight);
            gpic.Free;
          end;
          gtCommonImage:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            ItemImage.GDIPDraw(gpic, fgrRight);
            gpic.Free;
          end;
          gtSmoothButton, gtDropDownButton:
          begin
            gpic := TGPGraphics.Create(ca.Handle);
            gpic.SetSmoothingMode(SmoothingModeAntiAlias);
            bx := fgrRight.Left;
            by := fgrRight.Top;
            bw := GraphicRightWidth;
            bh := GraphicRightHeight;
            FOwner.FItemAppearance.FButtonAppearance.Draw(gpic, ButtonCaption, bx, by, bw, bh, 0, 0,
              ButtonColor, clNone, ButtonBevelColor, FOwner.FItemAppearance.FButtonAppearance.Font.Color, ButtonShadow, FButtonRight and FMouseDown, ButtonBevel, false, false,
              rtBoth, FGraphicRight, 0, 0, true, GraphicLeftIndex, GraphicLeftName);
            gpic.Free;
          end;
        end;
      end;
      g.ReleaseHDC(h);
      ca.Free;
    end;

    g.SetSmoothingMode(SmoothingModeAntiAlias);
    case TextRendering of
      tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
      tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
      tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    end;
    //ProgressBar
    with ItemAppearance.ProgressAppearance do
    begin
      if ProgressVisible then
      begin
        if ProgressPosition <> plCustom then
          GetTextPosition(x, y, MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top),
            ProgressWidth + (ProgressMargin * 2), ProgressHeight + (ProgressMargin * 2), ProgressPosition)
        else
        begin
          x := ProgressLeft;
          y := ProgressTop;
        end;

        pr := Bounds(R.Left + x + ProgressMargin, R.Top - GetPosition + y + ProgressMargin, ProgressWidth, ProgressHeight);
        Draw(g, pr, ProgressMinimum, ProgressMaximum, ProgressValue, pbdHorizontal);
      end;
    end;

    if FDeleteButton and DeleteButtonVisible then
    begin
      delr := MakeRect(r.Right - ItemAppearance.DeleteButtonWidth - 5, r.Top - GetPosition +
        (r.Bottom - r.Top - ItemAppearance.DeleteButtonHeight) / 2, ItemAppearance.DeleteButtonWidth,
          ItemAppearance.DeleteButtonHeight);

      delc := ItemAppearance.DeleteButtonColor;
      if FDeleteButtonDown then
        delc := ItemAppearance.DeleteButtonColorDown;

      pth := GDIPFill.CreateRoundRectangle(delr, 2, rtBoth, false);

      br := TGPSolidBrush.Create(MakeColor(255, delc));
      g.FillPath(br, pth);
      br.Free;

      delr := MakeRect(delr.X, delr.Y, delr.Width, delr.Height / 2);
      pthinner := GDIPFill.CreateRoundRectangle(delr, 2, rtBoth, false);
      br := TGPSolidBrush.Create(MakeColor(120, clWhite));
      g.FillPath(br, pthinner);
      br.Free;


      p := TGPPEN.Create(MakeColor(120, clBlack));
      g.DrawPath(p, pth);
      p.Free;

      pth.Free;
      pthinner.Free;


      delr := MakeRect(delr.X + 1, delr.Y + 1, delr.Width - 2, (delr.Height * 2) - 2);
      pthinner := GDIPFill.CreateRoundRectangle(delr, 2, rtBoth, false);

      p := TGPPEN.Create(MakeColor(120, clWhite));
      g.DrawPath(p, pthinner);
      p.Free;

      pthinner.Free;

      ft := TFont.Create;
      ft.Color := clWhite;
      ft.Style := ft.Style + [fsBold];


      ftsf := TGPStringFormat.Create;
      ftsf.SetLineAlignment(StringAlignmentCenter);
      ftg := g.MakeFont(ft);
      ftb := TGPSolidBrush.Create(MakeColor(255, ft.Color));
      g.DrawString(ItemAppearance.DeleteButtonCaption, Length(ItemAppearance.DeleteButtonCaption), ftg, delr, ftsf, ftb);
      ftb.Free;
      ftg.Free;
      ftsf.Free;
      ft.Free;
    end;

    g.free;
  end;

  fdisabled.Free;
  fnormal.Free;
  fselected.Free;

  infof.Free;
  captionf.Free;
  notesf.Free;
end;

procedure TAdvSmoothListBoxItem.Expand;
var
  i: integer;
begin
  FExpanded := true;
  FVisualizeNodes := true;
  with FOwner.Items do
  begin
    BeginUpdate;
    for I := (Self.Index + 1) to Count - 1 do
    begin
      if (Items[i].Level > Self.Level) and FVisualizeNodes then
      begin
        FLastNode := Items[i].Level + 1;
        if Items[i].Expanded then
          Items[i].Visible := true
        else
        begin
          FVisualizeNodes := false;
          Items[I].Visible := true;
        end;
      end
      else
      begin
        if Items[i].Level < FLastNode then
        begin
          FVisualizeNodes := true;

          if Items[i].Expanded then
            Items[i].Visible := true
          else
          begin
            FVisualizeNodes := false;
            FLastNode := Items[i].Level + 1;
            Items[I].Visible := true;
          end;
        end;
      end;
    end;
    EndUpdate;
  end;
end;

procedure TAdvSmoothListBoxItem.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothListBoxItem.GetAnchorAt(X, Y: integer; Focus: Boolean = False): String;
var
  a, s, k: String;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  g: TGPGraphics;
  r: TRect;
  notesf: TFont;
begin
  with (Collection as TAdvSmoothListBoxItems).FOwner do
  begin
    notesf := NotesFont;
    if Selected then
      notesf := NotesSelectedFont;

    r := Bounds(Fhtmlr.Left, Fhtmlr.Top - GetPosition, Fhtmlr.Right - Fhtmlr.Left, Fhtmlr.Bottom - fhtmlr.Top);
    if PtInRect(r, Point(X, Y)) then
    begin
      g := TGPGraphics.Create(Canvas.Handle);

      a := '';

      HTMLDrawGDIP(g, notesf, fn,r,FImages, X,Y,-1,-1,NotesShadowOffset,true,false,false,false,
        False,False,true,1.0,NotesURLColor,clNone,clNone,NotesShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FContainer,2);

      g.Free;
    end;
  end;

  if Focus then
    Result := k
  else
    Result :=  a;
end;

function TAdvSmoothListBoxItem.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := ClassName + inttostr(index);
end;

function TAdvSmoothListBoxItem.IsCaption(X, Y: integer): Boolean;
begin
  result := FOwner.PtInGPRect(fcaptionr, Point(X, Y));
end;

function TAdvSmoothListBoxItem.IsGraphicLeft(X, Y: integer): Boolean;
begin
  result := PtInRect(fgrLeft, Point(X, Y));
end;

function TAdvSmoothListBoxItem.IsGraphicRight(X, Y: integer): Boolean;
begin
  result := PtInRect(fgrRight, Point(X, Y));
end;

function TAdvSmoothListBoxItem.IsInfo(X, Y: integer): Boolean;
begin
  result := FOwner.PtInGPRect(finfor, Point(X, Y));
end;

procedure TAdvSmoothListBoxItem.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxItem.SaveItemValues(Caption, Info, Notes: String);
begin
  FOwner.Items.BeginUpdate;
  Self.Caption := Caption;
  Self.Info := Info;
  Self.Notes := Notes;
  FOwner.Items.EndUpdate;
end;

procedure TAdvSmoothListBoxItem.SetAlternate(const Value: TAdvSmoothListBoxAlternate);
begin
  if FAlternate <> Value then
  begin
    FAlternate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetBevelColor(const Value: TColor);
begin
  if FBevelColor <> Value then
  begin
    FBevelColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetButtonBevel(const Value: Boolean);
begin
  if FButtonBevel <> value then
  begin
    FButtonBevel := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetButtonCaption(const Value: String);
begin
  if FButtonCaption <> value then
  begin
    FButtonCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> value then
  begin
    FButtonColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetButtonShadow(const Value: Boolean);
begin
  if FButtonShadow <> value then
  begin
    FButtonShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetCaption(const Value: string);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment <> value then
  begin
    FCaptionAlignment := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> value then
  begin
    FCaptionFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetCaptionSelectedFont(const Value: TFont);
begin
  if FCaptionSelectedFont <> Value then
  begin
    FCaptionSelectedFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetCaptionWordWrap(const Value: Boolean);
begin
  if FCaptionWordWrap <> Value then
  begin
    FCaptionWordWrap := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetCategoryID(const Value: integer);
begin
  if FCategoryID <> value then
  begin
    FCategoryID := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetInternalChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TAdvSmoothListBoxItem.SetInternalHint(Value: string);
begin
  FHint := Value;
end;

procedure TAdvSmoothListBoxItem.SetInternalProgressValue(Value: Double);
begin
  FProgressValue := Value;
end;

procedure TAdvSmoothListBoxItem.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> value then
  begin
    FExpanded := Value;
    if FExpanded then
      Expand
    else
      Collapse;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetControl(const Value: TControl);
begin
  if FControl <> value then
  begin
    FControl := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetDeleteButton(const Value: Boolean);
begin
  if FDeleteButton <> Value then
  begin
    FDeleteButton := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetSeparatorLine(const Value: Boolean);
begin
  if (FSeparatorLine <> Value) then
  begin
    FSeparatorLine := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetDeleteButtonVisible(const Value: Boolean);
begin
  if FDeleteButtonVisible <> Value then
  begin
    FDeleteButtonVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetEnabled(Value: Boolean);
begin
  if Assigned(FOwner) then
    FOwner.DoBoolPropertyChange(Self, 2, Value);
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeft(const Value: TAdvGDIPPicture);
begin
  if FGraphicLeft <> Value then
  begin
    FGraphicLeft.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftSelected(const Value: TAdvGDIPPicture);
begin
  if FGraphicLeftSelected <> Value then
  begin
    FGraphicLeftSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftHeight(const Value: integer);
begin
  if FGraphicLeftHeight <> value then
  begin
    FGraphicLeftHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftIndex(const Value: Integer);
begin
  if FGraphicLeftIndex <> Value then
  begin
    FGraphicLeftIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftMargin(const Value: integer);
begin
  if FGraphicLeftMargin <> value then
  begin
    FGraphicLeftMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftName(const Value: String);
begin
  if FGraphicLeftName <> Value then
  begin
    FGraphicLeftName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftSelectedName(const Value: String);
begin
  if FGraphicLeftSelectedName <> Value then
  begin
    FGraphicLeftSelectedName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftShow(const Value: TAdvSmoothListBoxGraphicShow);
begin
  if FGraphicLeftShow <> value then
  begin
    FGraphicLeftShow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftType(const Value: TAdvSmoothListBoxGraphicType);
begin
  if FGraphicLeftType <> value then
  begin
    FGraphicLeftType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicLeftWidth(const Value: integer);
begin
  if FGraphicLeftWidth <> value then
  begin
    FGraphicLeftWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRight(const Value: TAdvGDIPPicture);
begin
  if FGraphicRight <> value then
  begin
    FGraphicRight.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightSelected(const Value: TAdvGDIPPicture);
begin
  if FGraphicRightSelected <> value then
  begin
    FGraphicRightSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightHeight(const Value: integer);
begin
  if FGraphicRightHeight <> value then
  begin
    FGraphicRightHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightIndex(const Value: Integer);
begin
  if FGraphicRightIndex <> Value then
  begin
    FGraphicRightIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightMargin(const Value: integer);
begin
  if FGraphicRightMargin <> value then
  begin
    FGraphicRightMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightName(const Value: String);
begin
  if FGraphicRightName <> Value then
  begin
    FGraphicRightName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightSelectedName(const Value: String);
begin
  if FGraphicRightSelectedName <> Value then
  begin
    FGraphicRightSelectedName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightShow(
  const Value: TAdvSmoothListBoxGraphicShow);
begin
  if FGraphicRightShow <> value then
  begin
    FGraphicRightShow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightType(
  const Value: TAdvSmoothListBoxGraphicType);
begin
  if FGraphicRightType <> value then
  begin
    FGraphicRightType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetGraphicRightWidth(const Value: integer);
begin
  if FGraphicRightWidth <> value then
  begin
    FGraphicRightWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetHint(const Value: String);
begin
  if Fhint <> value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetIndent(const Value: integer);
begin
  if FIndent <> value then
  begin
    FIndent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetInfo(const Value: String);
begin
  if FInfo <> value then
  begin
    FInfo := Value;
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetInfoFont(const Value: TFont);
begin
  if FInfoFont <> value then
  begin
    FInfoFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetInfoSelectedFont(const Value: TFont);
begin
  if FInfoSelectedFont <> Value then
  begin
    FInfoSelectedFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetLevel(const Value: integer);
begin
  if Value >= 0 then
  begin
    FLevel := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetMargin(const Value: Integer);
begin
  if FMargin <> value then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetMarginRight(const Value: integer);
begin
  if FMarginRight <> Value then
  begin
    FMarginRight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotes(const Value: String);
begin
  if FNotes <> Value then
  begin
    FNotes := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesFont(const Value: TFont);
begin
  if FNotesFont <> value then
  begin
    FNotesFont.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesLeft(const Value: integer);
begin
  if FNotesLeft <> value then
  begin
    FNotesLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesLocation(
  const Value: TAdvSmoothListBoxLocation);
begin
  if FNotesLocation <> value then
  begin
    FNotesLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesSelectedFont(const Value: TFont);
begin
  if FNotesSelectedFont <> Value then
  begin
    FNotesSelectedFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesShadowColor(const Value: TColor);
begin
  if FNoteshadowColor <> value then
  begin
    FNoteshadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesShadowOffset(const Value: integer);
begin
  if FNotesShadowOffset <> value then
  begin
    FNotesShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesTop(const Value: integer);
begin
  if FNotesTop <> value then
  begin
    FNotesTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetNotesURLColor(const Value: TColor);
begin
  if FNotesURLColor <> value then
  begin
    FNotesURLColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothListBoxItem.SetProgressHeight(const Value: integer);
begin
  if FProgressHeight <> value then
  begin
    FProgressHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressLeft(const Value: integer);
begin
  if FProgressLeft <> value then
  begin
    FProgressLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressMargin(const Value: integer);
begin
  if FProgressMargin <> value then
  begin
    FProgressMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressMax(const Value: Double);
begin
  if FProgressMax <> Value then
  begin
    FProgressMax := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressMin(const Value: Double);
begin
  if FProgressMin <> value then
  begin
    FProgressMin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressPosition(
  const Value: TAdvSmoothListBoxLocation);
begin
  if FProgressPosition <> value then
  begin
    FProgressPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressTop(const Value: integer);
begin
  if FProgressTop <> value then
  begin
    FProgressTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressValue(const Value: Double);
begin
  if FProgressValue <> value then
  begin
    FProgressValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressVisible(const Value: Boolean);
begin
  if FProgressVisible <> value then
  begin
    FProgressVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetProgressWidth(const Value: integer);
begin
  if FProgressWidth <> value then
  begin
    FProgressWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> value then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetSplitter(const Value: Boolean);
begin
  if FSplitter <> value then
  begin
    FSplitter := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItem.SetVisible(Value: Boolean);
begin
  if Assigned(FOwner) then  
    FOwner.DoBoolPropertyChange(Self, 1, Value);
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxItems }

function TAdvSmoothListBoxItems.Add: TAdvSmoothListBoxItem;
begin
  Result := TAdvSmoothListBoxItem(inherited Add);
end;

procedure TAdvSmoothListBoxItems.BeginUpdate;
begin
  inherited;
  Inc(FOwner.FUpdateCount);
end;

procedure TAdvSmoothListBoxItems.Clear;
begin
  if Count > 0 then
  begin
    FOwner.FDisplayList.Clear;
    BeginUpdate;
    try
      FOwner.SelectedItemIndex := -1;
      while Count > 0 do
        TCollectionItem(Items[Count - 1]).Free;
    finally
      EndUpdate;
    end;
  end;
end;

function TAdvSmoothListBoxItems.Compare(Item1, Item2: TAdvSmoothListBoxItem): integer;
var
  cIdx1, cIdx2: integer;
begin
  Result := 0;

  if Assigned(FOwner.OnCompare) then
  begin
    FOwner.OnCompare(FOwner, Item1, Item2, Result);
  end
  else
  begin

    case FOwner.CategoryType of
      alphanumeric:
      begin
        Result := AnsiCompareStr(item1.Caption, item2.Caption);
        {
        if item1.Caption < item2.Caption then
          result :=  -1
        else if item1.Caption > item2.Caption then
          result := 1
        else result := 0
        }
      end;
      custom:
      begin
        cIdx1 := FOwner.Categories.ItemIndexById(item1.CategoryID);
        cIdx2 := FOwner.Categories.ItemIndexById(item2.CategoryID);

        if cIdx1 < cIdx2 then
          result :=  -1
        else if cIdx1 > cIdx2 then
          result := 1
        else
        begin
          if item1.Caption < item2.Caption then
            result :=  -1
          else if item1.Caption > item2.Caption then
            result := 1
          else result := 0
        end;
      end;
    end;
  end;
end;

function TAdvSmoothListBoxItems.CountSelected: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Self[i].Selected then
      Inc(Result);
end;

constructor TAdvSmoothListBoxItems.Create(AOwner: TAdvSmoothListBox);
begin
  inherited Create(TAdvSmoothListBoxItem);
  FOwner := AOwner;
  FSelectedItem := nil;
end;

procedure TAdvSmoothListBoxItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

procedure TAdvSmoothListBoxItems.EndUpdate;
begin    
  inherited;
  Dec(FOwner.FUpdateCount);
  if FOwner.FUpdateCount = 0 then
  begin
    if Assigned(FOwner.FCurrentControl) then
      FOwner.FCurrentControl.Left := FOwner.width;
    FOwner.LookupBar.InitLookupBar;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    FOwner.Changed;
  end;
end;

function TAdvSmoothListBoxItems.GetItem(Index: Integer): TAdvSmoothListBoxItem;
begin
  Result := TAdvSmoothListBoxItem(inherited Items[Index]);
end;

function TAdvSmoothListBoxItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothListBoxItems.IndexOfCaption(const S: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Caption, S) = 0 then Exit;
  Result := -1;
end;

function TAdvSmoothListBoxItems.IndexOfInfo(const S: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Info, S) = 0 then Exit;
  Result := -1;
end;

function TAdvSmoothListBoxItems.IndexOfNotes(const S: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Notes, S) = 0 then Exit;
  Result := -1;
end;

function TAdvSmoothListBoxItems.Insert(Index: Integer): TAdvSmoothListBoxItem;
begin
  Result := TAdvSmoothListBoxItem(inherited Insert(Index));
end;

procedure TAdvSmoothListBoxItems.Move(FromIndex, ToIndex: Integer);
begin
  Items[FromIndex].Index := Items[ToIndex].Index
end;

procedure TAdvSmoothListBoxItems.QuickSort(L, R: Integer);
var
  I, J, p: Integer;
  Save: TCollectionItem;
  {$IFDEF DELPHIXE3_LVL}
  SortList: TList<TCollectionItem>;
  {$ELSE}
  SortList: TList;
  {$ENDIF}
begin
  //This cast allows us to get at the private elements in the base class
  SortList := TShadowedCollection(Self).FItems;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Items[J], Items[P]) > 0 do
        Dec(J);
      if I <= J then begin
        Save              := SortList.Items[I];
        SortList.Items[I] := SortList.Items[J];
        SortList.Items[J] := Save;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TAdvSmoothListBoxItems.Select(AItems: Array of Integer);
var
  i, si: integer;
begin
  BeginUpdate;
  for I := 0 to Length(AItems) - 1 do
  begin
    si := AItems[i];
    if (si > -1) and (si < Count) then
      self[si].Selected := true;
  end;
  EndUpdate;
end;

procedure TAdvSmoothListBoxItems.SelectAll;
var
  i: integer;
begin
  BeginUpdate;
  for I := 0 to Count - 1 do
    Self[I].Selected := true;
  EndUpdate;
end;

procedure TAdvSmoothListBoxItems.SetItem(Index: Integer;
  const Value: TAdvSmoothListBoxItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothListBoxItems.SetSelectedItem(
  const Value: TAdvSmoothListBoxItem);
var
  i, sh: integer;
begin
  if FSelectedItem <> value then
  begin
    with FOwner do
    begin
      if Value <> nil then
        DoSelectItem(Value.Index)
      else
        DoSelectItem(-1);

      FSelectedItem := Value;
      if Value <> nil then
      begin
        FFocusedItemIndex := Value.Index;
        FSelectedItemIndex := Value.Index;
      end
      else
      begin
        FFocusedItemIndex := Fowner.FSelectedItemIndex;
        FSelectedItemIndex := -1;
        Changed;
        Exit;
      end;

      sh := 0;
      if Sections.Visible then
      begin
        if Assigned(FDisplayList.GetItem(0)) then
          sh := FDisplayList.GetItem(0).ItemRect.Bottom - FDisplayList.GetItem(0).ItemRect.Top;
      end;

      FSelectedItemIndex := Min(Items.Count - 1, Max(0, FSelectedItemIndex));
      Items.SelectedItem := FItems[FSelectedItemIndex];
      for I := 0 to FDisplayList.Count - 1 do
      begin
        with FDisplayList.GetItem(i) do
        begin
          if DisplayItem <> nil then
          begin
            if DisplayItem.Index = FSelectedItemIndex then
            begin
              if ItemRect.Top - GetPosition > Height - Footer.GetHeight - (ItemRect.Bottom-ItemRect.Top) then
                FScPosTo := ItemRect.Bottom - (Height - Footer.GetHeight)
              else if ItemRect.Top < GetPosition + Header.GetHeight + Filter.GetHeight then
                FScPosTo := ItemRect.Top - Header.GetHeight - sh - Filter.GetHeight;

              if FScPosTo <> FCurrentScPos then
              begin
                FCurrentScPos := FScPosTo;
                ScrollIndicator.FAnimateOpacity := ScrollIndicator.Opacity;
                FAnimate := true;
              end;
            end;
          end;
        end;
      end;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothListBoxItems.Sort;
begin
  if Count > 1 then
    QuickSort(0, pred(Count));

  FOwner.InitDisplayList;
  FOwner.CalculateRects;
  FOwner.Invalidate;
end;

procedure TAdvSmoothListBoxItems.UnSelectAll;
var
  i: integer;
begin
  BeginUpdate;
  for I := 0 to Count - 1 do
    Self[I].Selected := false;
  EndUpdate;
end;

{ TAdvSmoothListBoxItemAppearance }

procedure TAdvSmoothListBoxItemAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxItemAppearance then
  begin
    FSpacingVert := (Source as TAdvSmoothListBoxItemAppearance).VerticalSpacing;
    FSpacingHorz := (Source as TAdvSmoothListBoxItemAppearance).HorizontalSpacing;
    FItemHeight := (Source as TAdvSmoothListBoxItemAppearance).Height;
    FFill.Assign((Source as TAdvSmoothListBoxItemAppearance).Fill);
    FFillSelected.Assign((Source as TAdvSmoothListBoxItemAppearance).FillSelected);
    FFillDisabled.Assign((Source as TAdvSmoothListBoxItemAppearance).FillDisabled);
    FProgressAppearance.Assign((Source as TAdvSmoothListBoxItemAppearance).ProgressAppearance);
    FButtonAppearance.Assign((Source as TAdvSmoothListBoxItemAppearance).ButtonAppearance);
    FDeleteButtonWidth := (Source as TAdvSmoothListBoxItemAppearance).DeleteButtonWidth;
    FDeleteButtonHeight := (Source as TAdvSmoothListBoxItemAppearance).DeleteButtonHeight;
    FDeleteButtonColor := (Source as TAdvSmoothListBoxItemAppearance).DeleteButtonColor;
    FDeleteButtonColorDown := (Source as TAdvSmoothListBoxItemAppearance).DeleteButtonColorDown;
    FDeleteButtonCaption := (Source as TAdvSmoothListBoxItemAppearance).DeleteButtonCaption;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.ButtonAppearanceChanged(
  Sender: TObject);
begin
  FOwner.ItemAppearanceChanged(Self);
end;

procedure TAdvSmoothListBoxItemAppearance.Changed;
begin
  FOwner.ItemAppearanceChanged(Self);
end;

constructor TAdvSmoothListBoxItemAppearance.Create(AOwner: TAdvSmoothListBox);
begin
  Fowner := AOwner;
  FSpacingVert := 0;
  FSpacingHorz := 0;
  FItemHeight := 30;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFillSelected := TGDIPFill.Create;
  FFillSelected.OnChange := FillChanged;

  FFillAlternate := TGDIPFill.Create;
  FFillAlternate.OnChange := FillChanged;
  FFillSelectedAlternate := TGDIPFill.Create;
  FFillSelectedAlternate.OnChange := FillChanged;

  FFillDisabled := TGDIPFill.Create;
  FFillDisabled.OnChange := FillChanged;

  FInfoFill := TGDIPFill.Create;
  FInfoFill.OnChange := FillChanged;
  FInfoFillSelected := TGDIPFill.Create;
  FInfoFillSelected.OnChange := FillChanged;
  FInfoFillDisabled := TGDIPFill.Create;
  FInfoFillDisabled.OnChange := FillChanged;

  FProgressAppearance := TGDIPProgress.Create;
  FProgressAppearance.OnChange := ProgressAppearanceChanged;
  FButtonAppearance := TGDIPButton.Create;
  FButtonAppearance.OnChange := ButtonAppearanceChanged;
  FDeleteButtonWidth := 45;
  FDeleteButtonHeight := 22;
  FDeleteButtonColor := clRed;
  FDeleteButtonColorDown := $0000D9;
  FDeleteButtonCaption := 'Delete';
  FNodeOpen := TAdvGDIPPicture.Create;
  FNodeClosed := TAdvGDIPPicture.Create;
  FSeparatorLineColor := clNone;
  FSeparatorLineShadowColor := clNone;
end;

destructor TAdvSmoothListBoxItemAppearance.Destroy;
begin
  FFillAlternate.Free;
  FFillSelectedAlternate.Free;
  FFill.Free;
  FFillDisabled.Free;
  FFillSelected.Free;
  FInfoFill.Free;
  FInfoFillDisabled.Free;
  FInfoFillSelected.Free;
  FProgressAppearance.Free;
  FButtonAppearance.Free;
  FNodeOpen.Free;
  FNodeClosed.Free;
  inherited;
end;

procedure TAdvSmoothListBoxItemAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxItemAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxItemAppearance.ProgressAppearanceChanged(
  Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxItemAppearance.SetButtonAppearance(
  const Value: TGDIPButton);
begin
  if FButtonAppearance <> value then
  begin
    FButtonAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetDeleteButtonColor(
  const Value: TColor);
begin
  if FDeleteButtonColor <> Value then
  begin
    FDeleteButtonColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetDeleteButtonColorDown(
  const Value: TColor);
begin
  if FDeleteButtonColorDown <> Value then
  begin
    FDeleteButtonColorDown := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetDeleteButtonHeight(
  const Value: Integer);
begin
  if FDeleteButtonHeight <> Value then
  begin
    FDeleteButtonHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetDeleteButtonWidth(
  const Value: Integer);
begin
  if FDeleteButtonWidth <> Value then
  begin
    FDeleteButtonWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetFillAlternate(
  const Value: TGDIPFill);
begin
  if FFillAlternate <> Value then
  begin
    FFillAlternate.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetFillDisabled(
  const Value: TGDIPFill);
begin
  if FFillDisabled <> value then
  begin
    FFillDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetFillSelected(
  const Value: TGDIPFill);
begin
  if FFillSelected <> value then
  begin
    FFillSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetFillSelectedAlternate(
  const Value: TGDIPFill);
begin
  if FFillSelectedAlternate <> Value then
  begin
    FFillSelectedAlternate.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetInfoFill(const Value: TGDIPFill);
begin
  if FInfoFill <> Value then
  begin
    FInfoFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetInfoFillDisabled(
  const Value: TGDIPFill);
begin
  if FInfoFillDisabled <> Value then
  begin
    FInfoFillDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetInfoFillSelected(
  const Value: TGDIPFill);
begin
  if FInfoFillSelected <> Value then
  begin
    FInfoFillSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetItemHeight(const Value: integer);
begin
  if FItemHeight <> value then
  begin
    FItemHeight := Value;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetNodeClosed(
  const Value: TAdvGDIPPicture);
begin
  if (FNodeClosed <> Value) then
  begin
    FNodeClosed.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetNodeClosedName(
  const Value: string);
begin
  if (FNodeClosedName <> Value) then
  begin
    FNodeClosedName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetNodeOpen(
  const Value: TAdvGDIPPicture);
begin
  if (FNodeOpen <> Value) then
  begin
    FNodeOpen.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetNodeOpenName(const Value: string);
begin
  if (FNodeOpenName <> Value) then
  begin
    FNodeOpenName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetProgressAppearance(
  const Value: TGDIPProgress);
begin
  if FProgressAppearance <> value then
  begin
    FProgressAppearance.Assign(Value);
    ProgressAppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetSeparatorLineColor(
  const Value: TColor);
begin
  if (FSeparatorLineColor <> Value) then
  begin
    FSeparatorLineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetSeparatorLineShadowColor(
  const Value: TColor);
begin
  if (FSeparatorLineShadowColor <> Value) then
  begin
    FSeparatorLineShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetSpacingHorz(const Value: integer);
begin
  if FSpacingHorz <> value then
  begin
    FSpacingHorz := Value;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetSpacingVert(const Value: integer);
begin
  if FSpacingVert <> value then
  begin
    FSpacingVert := Value;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxItemAppearance.SetUseInfoFill(const Value: boolean);
begin
  FUseInfoFill := Value;
end;

{ TAdvSmoothListBoxLookUpBar }

procedure TAdvSmoothListBoxLookUpBar.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxLookUpBar then
  begin
    FColor := (Source as TAdvSmoothListBoxLookUpBar).Color;
    FColorTo := (Source as TAdvSmoothListBoxLookUpBar).ColorTo;
    FOpacity := (Source as TAdvSmoothListBoxLookUpBar).Opacity;
    FOpacityTo := (Source as TAdvSmoothListBoxLookUpBar).OpacityTo;
    FNumeric := (Source as TAdvSmoothListBoxLookUpBar).Numeric;
    FVisible := (Source as TAdvSmoothListBoxLookUpBar).Visible;
    FPosition := (Source as TAdvSmoothListBoxLookUpBar).Position;
    FFont.Assign((Source as TAdvSmoothListBoxLookUpBar).Font);
    FDisabledFont.Assign((Source as TAdvSmoothListBoxLookUpBar).DisabledFont);
    FOnTop := (Source as TAdvSmoothListBoxLookUpBar).OnTop;
    FGradientType := (Source as TAdvSmoothListBoxLookUpBar).GradientType;
    FHatchStyle := (Source as TAdvSmoothListBoxLookUpBar).HatchStyle;
    FSpacing := (Source as TAdvSmoothListBoxLookUpBar).Spacing;
    FMargin := (Source as TAdvSmoothListBoxLookUpBar).Margin;
    FRotated := (Source as TAdvSmoothListBoxLookUpBar).Rotated;
    FAutoSize := (Source as TAdvSmoothListBoxLookUpBar).AutoSize;
    FOrder := (Source as TAdvSmoothListBoxLookUpBar).Order;
    FMainLevelOnly := (Source as TAdvSmoothListBoxLookUpBar).MainLevelOnly;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.Changed;
begin
  InitLookupBar;
  FOwner.LookupBarChanged(self);
end;

constructor TAdvSmoothListBoxLookUpBar.Create(AOwner: TAdvSmoothListBox);
begin
  FOwner := AOwner;
  FSpacing := 3;
  FMargin := 4;
  FRotated := false;
  FAutoSize := true;
  Fcolor := clWhite;
  FColorTo := clWhite;
  FGradientType := gtSolid;
  FHatchStyle := HatchStyleHorizontal;
  FOnTop := false;
  FOpacity := 100;
  FOpacityTo := 100;
  FNumeric := false;
  FVisible := true;
  FPosition := pRight;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.OnChange := FontChanged;
  FDisabledFont := TFont.Create;
  FDisabledFont.Color := clSilver;
  {$IFNDEF DELPHI9_LVL}
  FDisabledFont.Name := 'Tahoma';
  {$ENDIF}
  FDisabledFont.OnChange := FontChanged;
  FOrder := loNumericLast;
  FMainLevelOnly := False;
end;

destructor TAdvSmoothListBoxLookUpBar.Destroy;
begin
  Font.Free;
  DisabledFont.Free;
  inherited;
end;

procedure TAdvSmoothListBoxLookUpBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothListBoxLookUpBar.GetMinimumLevel: Integer;
var
  i: integer;
begin
  Result := MaxInt;
  for I := 0 to Fowner.Items.Count - 1 do
  begin
    if FOwner.Items[I].Level < Result then
      Result := FOwner.FItems[I].Level;
  end;
end;

function TAdvSmoothListBoxLookUpBar.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TAdvSmoothListBoxLookUpBar.GetWidth: integer;
var
  bmp: TBitmap;
begin
  Result := 0;
  if Fowner.LookupBarVisible then
  begin
    bmp := TBitmap.Create;
    bmp.Canvas.Font.Assign(Font);
    case Fowner.CategoryType of
      alphanumeric: Result := bmp.Canvas.TextWidth('W') + FMargin;
      custom: result := Fowner.GetMaximumCustomTextWidth(bmp.Canvas);
    end;
    bmp.Free;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.InitLookupBar;
var
  I, j: Integer;
  s: String;
  c: Integer;
  minl: integer;
begin
  if (csDestroying in FOwner.ComponentState) then
    Exit;

  minl := GetMinimumLevel;

  case Fowner.CategoryType of
    alphanumeric:
    begin
      for I := 1 to 36 do
        FChar[I] := false;

      for I := 0 to FOwner.Items.Count - 1 do
      begin
        with FOwner.Items[I] do
        begin
          if minl = Level then
          begin
            S := AnsiUpperCase(Caption);
            if not Splitter and (S <> '') and Visible then
            begin
              if (Ord(s[1]) <= 90) then
              begin
                j := Ord(s[1]) - 64;
                if (j < 27) and (J > 0) then
                begin
                  if (Order = loNumericLast) or not Numeric then
                    FChar[j] := true
                  else
                    FChar[10 + j] := true;
                end
                else
                begin
                  j := Ord(s[1]) - 48;
                  if (j < 11) and (j >= 0) then
                  begin
                    if (Order = loNumericLast) or not Numeric then
                      FChar[27 + j] := true
                    else
                      FChar[j + 1] := true;
                  end;
                end;
              end
              else
              begin
                c := 0;
                if Order = loNumericFirst then
                  c := 10;
                
                if (AnsiCompareStr('A',S) < 0) and (AnsiCompareStr('B',S) > 0) then
                  FChar[1+c] := true
                else
                if (AnsiCompareStr('C',S) < 0) and (AnsiCompareStr('D',S) > 0) then
                  FChar[3+c] := true
                else
                if (AnsiCompareStr('E',S) < 0) and (AnsiCompareStr('F',S) > 0) then
                  FChar[5+c] := true
                else
                if (AnsiCompareStr('G',S) < 0) and (AnsiCompareStr('H',S) > 0) then
                  FChar[7+c] := true
                else
                if (AnsiCompareStr('H',S) < 0) and (AnsiCompareStr('I',S) > 0) then
                  FChar[8+c] := true
                else
                if (AnsiCompareStr('I',S) < 0) and (AnsiCompareStr('J',S) > 0) then
                  FChar[9+c] := true
                else
                if (AnsiCompareStr('J',S) < 0) and (AnsiCompareStr('K',S) > 0) then
                  FChar[10+c] := true
                else
                if (AnsiCompareStr('K',S) < 0) and (AnsiCompareStr('L',S) > 0) then
                  FChar[11+c] := true
                else
                if (AnsiCompareStr('L',S) < 0) and (AnsiCompareStr('M',S) > 0) then
                  FChar[12+c] := true
                else
                if (AnsiCompareStr('N',S) < 0) and (AnsiCompareStr('N',S) > 0) then
                  FChar[14+c] := true
                else
                if (AnsiCompareStr('O',S) < 0) and (AnsiCompareStr('P',S) > 0) then
                  FChar[15+c] := true
                else
                if (AnsiCompareStr('P',S) < 0) and (AnsiCompareStr('Q',S) > 0) then
                  FChar[16+c] := true
                else
                if (AnsiCompareStr('R',S) < 0) and (AnsiCompareStr('S',S) > 0) then
                  FChar[18+c] := true
                else
                if (AnsiCompareStr('S',S) < 0) and (AnsiCompareStr('T',S) > 0) then
                  FChar[19+c] := true
                else
                if (AnsiCompareStr('T',S) < 0) and (AnsiCompareStr('U',S) > 0) then
                  FChar[20+c] := true
                else
                if (AnsiCompareStr('U',S) < 0) and (AnsiCompareStr('V',S) > 0) then
                  FChar[21+c] := true
                else
                if (AnsiCompareStr('W',S) < 0) and (AnsiCompareStr('X',S) > 0) then
                  FChar[22+c] := true
                else
                if (AnsiCompareStr('Z',S) < 0) then
                  FChar[26+c] := true
              end;
            end;
          end;
        end;
      end;
    end;
    custom:
    begin
      for I := 0 to Fowner.Categories.Count - 1 do
      begin
        SetLength(FCustomChar, I + 1);
        FCustomChar[I] := false;
      end;

      for I := 0 to Fowner.Items.Count - 1 do
      begin
        with Fowner.Items[I] do
        begin
          if minl = Level then
          begin
            S := Caption;
            if not Splitter and (S <> '') and Visible then
            begin
              j := CategoryID;
              if (j < FOwner.Categories.Count) and (J > -1) then
                FCustomChar[j] := true
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetColorTo(const Value: TColor);
begin
  if FColorTo <> value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetDisabledFont(const Value: TFont);
begin
  if FDisabledFont <> value then
  begin
    FDisabledFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetGradientType(
  const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetMainLevelOnly(const Value: Boolean);
begin
  if FMainLevelOnly <> Value then
  begin
    FMainLevelOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetMargin(const Value: integer);
begin
  if (FMargin <> Value) then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetNumeric(const Value: Boolean);
begin
  if FNumeric <> Value then
  begin
    FNumeric := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetOnTop(const Value: Boolean);
begin
  if FOnTop <> Value then
  begin
    FOnTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetOpacityTo(const Value: Byte);
begin
  if FOpacityTo <> value then
  begin
    FOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetOrder(const Value: TLookUpBarOrder);
begin
  if FOrder <> Value then
  begin
    FOrder := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetPosition(
  const Value: TAdvSmoothListBoxLookUpBarPosition);
begin
  if FPosition <> value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetRotated(const Value: Boolean);
begin
  if FRotated <> value then
  begin
    FRotated := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    if FOwner.FUpdateCount = 0 then
    begin
      if Assigned(FOwner.FCurrentControl) then
        FOwner.FCurrentControl.Left := FOwner.width;
      InitLookupBar;
      FOwner.InitDisplayList;
      FOwner.CalculateRects;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothListBoxLookUpBar.SetVisible(Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxSections }

procedure TAdvSmoothListBoxSections.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxSections then
  begin
    FFont.Assign((Source as TAdvSmoothListBoxSections).Font);
    FVisible := (Source as TAdvSmoothListBoxSections).Visible;
    FGradientType := (Source as TAdvSmoothListBoxSections).GradientType;
    FHatchStyle := (Source as TAdvSmoothListBoxSections).HatchStyle;
    FBorderColor := (Source as TAdvSmoothListBoxSections).BorderColor;
    FColor := (Source as TAdvSmoothListBoxSections).Color;
    FColorTo := (Source as TAdvSmoothListBoxSections).ColorTo;
    FOpacity := (Source as TAdvSmoothListBoxSections).Opacity;
    FOpacityTo := (Source as TAdvSmoothListBoxSections).OpacityTo;
    FBorderWidth := (Source as TAdvSmoothListBoxSections).BorderWidth;
    FImageAlign := (Source as TAdvSmoothListBoxSections).ImageAlign;
    FHeight := (Source as TAdvSmoothListBoxSections).Height;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.CategoriesChanged(Sender: TObject);
begin
  FOwner.Changed;
end;

procedure TAdvSmoothListBoxSections.Changed;
begin
  FOwner.SectionsChanged(self);
end;

constructor TAdvSmoothListBoxSections.Create(AOwner: TAdvSmoothListBox);
begin
  FOwner := AOwner;
  FBorderWidth := 1;
  FImageAlign := alLeft;
  FHeight := -1;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}  
  FFont.OnChange := FontChanged;
  FVisible := false;
  FGradientType := gtSolid;
  FHatchStyle := HatchStyleHorizontal;
  FOpacity := 180;
  FOpacityTo := 180;
  FColor := clWhite;
  FColorTo := clSilver;
  FCategoryType := alphanumeric;
  FCategories := TAdvSmoothListBoxCategoryItems.Create(AOwner);
  FCategories.OnChange := CategoriesChanged;
end;

destructor TAdvSmoothListBoxSections.Destroy;
begin
  FFont.Free;
  FCategories.free;
  inherited;
end;

procedure TAdvSmoothListBoxSections.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxSections.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetGradientType(
  const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> Value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetImageAlign(
  const Value: TAdvSmoothListBoxImageAlign);
begin
  if FImageAlign <> Value then
  begin
    FImageAlign := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetOpacityTo(const Value: Byte);
begin
  if FOpacityTo <> value then
  begin
    FOpacityTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxSections.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxDisplayList }

procedure TAdvSmoothListBoxDisplayListItem.Changed;
begin
//  Fowner.InitDisplayList;
end;

procedure TAdvSmoothListBoxDisplayListItem.SetFloating(const Value: Boolean);
begin
  if FFloating <> Value then
  begin
    FFloating := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxDisplayListItem.SetKind(
  const Value: TAdvSmoothListBoxItemKind);
begin
  if FKind <> value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxDisplayListItem.SetSectionCaption(
  const Value: String);
begin
  if FSectionCaption <> value then
  begin
    FSectionCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxDisplayListItem.SetSectionCategoryID(
  const Value: integer);
begin
  if FSectionCategoryID <> value then
  begin
    FSectionCategoryID := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxDisplayList }

function TAdvSmoothListBoxDisplayList.AddItem: TAdvSmoothListBoxDisplayListItem;
begin
  result := TAdvSmoothListBoxDisplayListItem.Create;
  Add(result);
end;

procedure TAdvSmoothListBoxDisplayList.Clear;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    TAdvSmoothListBoxDisplayListItem(Items[i]).Free;
  inherited;
end;

procedure TAdvSmoothListBoxDisplayList.DeleteItem(index: integer);
begin
  TAdvSmoothListBoxDisplayListItem(Items[Index]).Free;
  Delete(index);
end;

function TAdvSmoothListBoxDisplayList.GetItem(index: integer): TAdvSmoothListBoxDisplayListItem;
begin
  if Index >= Count then
    Index := Count - 1;

  if (Index >= 0) and (Index <= Count - 1) then
    Result := TAdvSmoothListBoxDisplayListItem(Items[Index])
  else
    Result := nil;
end;

{ TAdvSmoothListBoxHeader }

procedure TAdvSmoothListBoxHeaderFooter.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxHeaderFooter then
  begin
    FHeight := (Source as TAdvSmoothListBoxHeaderFooter).Height;
    FCaptionLocation := (Source as TAdvSmoothListBoxHeaderFooter).CaptionLocation;
    FCaptionURLColor := (Source as TAdvSmoothListBoxHeaderFooter).CaptionURLColor;
    FCaptionShadowColor := (Source as TAdvSmoothListBoxHeaderFooter).CaptionShadowColor;
    FCaptionShadowOffset := (Source as TAdvSmoothListBoxHeaderFooter).CaptionShadowOffset;
    FCaption := (Source as TAdvSmoothListBoxHeaderFooter).Caption;
    FFont.Assign((Source as TAdvSmoothListBoxHeaderFooter).Font);
    FVisible := (Source as TAdvSmoothListBoxHeaderFooter).Visible;
    FCaptionLeft := (Source as TAdvSmoothListBoxHeaderFooter).CaptionLeft;
    FCaptionTop := (Source as TAdvSmoothListBoxHeaderFooter).CaptionTop;
    FFill.Assign((Source as TAdvSmoothListBoxHeaderFooter).Fill);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.Changed;
begin
  Fowner.HeaderFooterChanged(Self);
end;

constructor TAdvSmoothListBoxHeaderFooter.Create(AOwner: TAdvSmoothListBox);
begin
  FOwner := AOwner;
  FHeight := 40;
  FCaptionLocation := cpCenterCenter;
  FCaptionShadowColor := clGray;
  FCaptionURLColor := clBlue;
  FCaptionShadowOffset := 5;
  FCaption := '';
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}  
  FFont.OnChange := FontChanged;
  FVisible := true;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FCaptionLeft := 0;
  FCaptionTop := 0;
end;

destructor TAdvSmoothListBoxHeaderFooter.Destroy;
begin
  FFont.Free;
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothListBoxHeaderFooter.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothListBoxHeaderFooter.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothListBoxHeaderFooter.GetAnchorAt(r: TRect; X, Y: integer; Focus: Boolean = False): String;
var
  a, s, k: String;
  g: TGPGraphics;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
begin
  Result := '';
  if PtInRect(r, Point(X, Y)) then
  begin
    g := TGPGraphics.Create(Fowner.Canvas.Handle);

    HTMLDrawGDIP(g, Font, Caption ,r,Fowner.FImages, X,Y,-1,-1,CaptionShadowOffset,true,false,false,false,
      False,False,true,1.0,CaptionURLColor,clNone,clNone,CaptionShadowColor,a,s,k,XSize,YSize,l,m,hr,nil, FOwner.FContainer,2);

    if Focus then
      Result := k
    else
      Result := a;

    g.Free;
  end;
end;

function TAdvSmoothListBoxHeaderFooter.GetHeight: integer;
var
  hb: integer;
begin
  hb := 0;
  if Fill.BorderColor <> clNone then
    hb := Fill.BorderWidth;

  if Visible then
    Result := Height + hb
  else
    Result := 0;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionLeft(const Value: integer);
begin
  if FCaptionLeft <> Value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionLocation(
  const Value: TAdvSmoothListBoxCaptionLocation);
begin
  if FCaptionLocation <> value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionShadowColor(
  const Value: TColor);
begin
  if FCaptionShadowColor <> value then
  begin
    FCaptionShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionShadowOffset(
  const Value: integer);
begin
  if FCaptionShadowOffset <> value then
  begin
    FCaptionShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionTop(const Value: integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetCaptionURLColor(const Value: TColor);
begin
  if FCaptionURLColor <> value then
  begin
    FCaptionURLColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxHeaderFooter.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    FOwner.InitDisplayList;
    FOwner.CalculateRects;
    Changed;
  end;
end;

{ TAdvSmoothListBoxIndicator }

procedure TAdvSmoothListBoxIndicator.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothListBoxIndicator then
  begin
    FVisible := (Source as TAdvSmoothListBoxIndicator).Visible;
    FColor := (Source as TAdvSmoothListBoxIndicator).Color;
    FColorTo := (Source as TAdvSmoothListBoxIndicator).ColorTo;
    FOpacity := (Source as TAdvSmoothListBoxIndicator).Opacity;
    FGradientType := (Source as TAdvSmoothListBoxIndicator).GradientType;
    FHatchStyle := (Source as TAdvSmoothListBoxIndicator).HatchStyle;
    FFade := (Source as TAdvSmoothListBoxIndicator).Fade;
    FWidth := (Source as TAdvSmoothListBoxIndicator).Width;
    FStyle := (Source as TAdvSmoothListBoxIndicator).Style;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.Changed;
begin
  FOwner.ScrollIndicatorChanged(Self);
end;

constructor TAdvSmoothListBoxIndicator.Create(AOwner: TAdvSmoothListBox);
begin
  FAnimateOpacity := 0;
  FOwner := AOwner;
  Fvisible := true;
  FColor := clBlack;
  FColorTo := clDkGray;
  FOpacity := 100;
  FGradientType := gtSolid;
  FHatchStyle := HatchStyleHorizontal;
  FWidth := 5;
  FHeight := 60;
  FFade := true;
  FStyle := ssiPhone;
end;

destructor TAdvSmoothListBoxIndicator.Destroy;
begin
  inherited;
end;

function TAdvSmoothListBoxIndicator.GetAnimationOpacity: Byte;
begin
  if FOwner.FAnimate and FFade then
    result := FAnimateOpacity
  else
    result := FOpacity;
end;

function TAdvSmoothListBoxIndicator.GetWidth: integer;
begin
  Result := 0;
  if Visible then
    Result := Width;
end;

procedure TAdvSmoothListBoxIndicator.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetColorTo(const Value: TColor);
begin
  if FColorTo <> value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetFade(const Value: Boolean);
begin
  if FFade <> value then
  begin
    FFade := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetGradientType(
  const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetOpacity(const Value: Byte);
begin
  if FOpacity <> value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxIndicator.Setwidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxCategoryItem }

procedure TAdvSmoothListBoxCategoryItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothListBoxCategoryItem) then
  begin
    FText := (Source as TAdvSmoothListBoxCategoryItem).Text;
    FLookupText := (Source as TAdvSmoothListBoxCategoryItem).LookupText;
    FId := (Source as TAdvSmoothListBoxCategoryItem).Id;
    FImageIndex := (Source as TAdvSmoothListBoxCategoryItem).ImageIndex;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.Changed;
begin
  with FOwner do
  begin
    if FUpdateCount = 0 then
    begin
      if Assigned(FOwner.FCurrentControl) then
        FCurrentControl.Left := FOwner.Width;
      LookupBar.InitLookupBar;
      InitDisplayList;
      CalculateRects;
      Changed;
    end;
  end;
end;

constructor TAdvSmoothListBoxCategoryItem.Create(Collection: TCollection);
begin
  inherited;
  Fowner := (Collection as TAdvSmoothListBoxCategoryItems).FOwner;
  FID := (Collection as TAdvSmoothListBoxCategoryItems).Count - 1;
  FImageIndex := -1;
  with FOwner do
  begin
    if FUpdateCount = 0 then
    begin
      if Assigned(FCurrentControl) then
        FCurrentControl.Left := FOwner.width;
      LookupBar.InitLookupBar;
      InitDisplayList;
      CalculateRects;
      Changed;
    end;
  end;  
end;

destructor TAdvSmoothListBoxCategoryItem.Destroy;
begin
  inherited;
  with FOwner do
  begin
    if FUpdateCount = 0 then
    begin
      if Assigned(FCurrentControl) then
        FCurrentControl.Left := FOwner.width;
      LookupBar.InitLookupBar;
      InitDisplayList;
      CalculateRects;
      Changed;
    end;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.SetId(const Value: integer);
begin
  if FId <> value then
  begin
    FId := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.SetLookupText(const Value: String);
begin
  if FLookupText <> value then
  begin
    FLookupText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.SetTag(const Value: integer);
begin
  if FTag <> value then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxCategoryItem.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TAdvSmoothListBoxCategoryItems }

function TAdvSmoothListBoxCategoryItems.Add: TAdvSmoothListBoxCategoryItem;
begin
  Result := TAdvSmoothListBoxCategoryItem(inherited Add);
end;

procedure TAdvSmoothListBoxCategoryItems.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

function TAdvSmoothListBoxCategoryItems.Compare(Item1,
  Item2: TAdvSmoothListBoxCategoryItem): integer;
begin
  if item1.Text < item2.Text then
    result :=  -1
  else if item1.Text > item2.Text then
    result := 1
  else result := 0;
end;

constructor TAdvSmoothListBoxCategoryItems.Create(AOwner: TAdvSmoothListBox);
begin
  inherited Create(TAdvSmoothListBoxCategoryItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothListBoxCategoryItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothListBoxCategoryItems.GetItem(
  Index: Integer): TAdvSmoothListBoxCategoryItem;
begin
  Result := TAdvSmoothListBoxCategoryItem(inherited Items[Index]);
end;

function TAdvSmoothListBoxCategoryItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothListBoxCategoryItems.Insert(
  Index: Integer): TAdvSmoothListBoxCategoryItem;
begin
  Result := TAdvSmoothListBoxCategoryItem(inherited Insert(Index));
end;

function TAdvSmoothListBoxCategoryItems.ItemById(
  id: integer): TAdvSmoothListBoxCategoryItem;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = id then
      break;
  end;
end;

function TAdvSmoothListBoxCategoryItems.ItemIndexByID(id: integer): integer;
var
  ci: TAdvSmoothListBoxCategoryItem;
begin
  ci := ItemByID(id);
  if Assigned(ci) then
    result := ci.Index
  else
    result := -1;
end;

procedure TAdvSmoothListBoxCategoryItems.QuickSort(L, R: Integer);
var
  I, J, p: Integer;
  Save: TCollectionItem;
  {$IFDEF DELPHIXE3_LVL}
  SortList: TList<TCollectionItem>;
  {$ELSE}
  SortList: TList;
  {$ENDIF}
begin
  //This cast allows us to get at the private elements in the base class
  SortList := TShadowedCollection(Self).FItems;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Items[J], Items[P]) > 0 do
        Dec(J);
      if I <= J then begin
        Save              := SortList.Items[I];
        SortList.Items[I] := SortList.Items[J];
        SortList.Items[J] := Save;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TAdvSmoothListBoxCategoryItems.SetItem(Index: Integer;
  const Value: TAdvSmoothListBoxCategoryItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothListBoxCategoryItems.Sort;
begin
  if Count > 1 then
    QuickSort(0, pred(Count));

  Fowner.LookupBar.InitLookupBar;
  FOwner.Invalidate;
end;


{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothListBoxDropTarget }

constructor TAdvSmoothListBoxDropTarget.Create(AListBox: TAdvSmoothListBox);
begin
  Inherited Create;
  FListBox := AListBox;
end;

procedure TAdvSmoothListBoxDropTarget.DragMouseLeave;
begin
  inherited;

end;

procedure TAdvSmoothListBoxDropTarget.DragMouseMove(Source: TObject; pt: TPoint; var Allow: Boolean;
  DropFormats: TDropFormats);
begin
  inherited;
  FListBox.DoDragOver(Source, pt.X, pt.Y, dsDragMove, Allow);
end;

procedure TAdvSmoothListBoxDropTarget.DropCol(pt: TPoint; Col: Integer);
begin
  inherited;

end;

procedure TAdvSmoothListBoxDropTarget.DropFiles(pt: TPoint; files: tstrings);
begin
  inherited;
  FListBox.DoDragDrop(files, pt.X, pt.Y);
end;

procedure TAdvSmoothListBoxDropTarget.DropRTF(pt: TPoint; s: string);
begin
  inherited;
end;

procedure TAdvSmoothListBoxDropTarget.DropText(pt: TPoint; s: string);
begin
  inherited;
  FListBox.DoDragDrop(TObject(s), pt.X, pt.Y);
end;

procedure TAdvSmoothListBoxDropTarget.DropURL(pt: TPoint; s: string);
begin
  inherited;
  FListBox.DoDragDrop(TObject(s), pt.X, pt.Y);
end;

{ TAdvSmoothListBoxFilter }

procedure TAdvSmoothListBoxFilter.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothListBoxFilter) then
  begin
    FTextHint := (Source as TAdvSmoothListBoxFilter).TextHint;
    FFill.Assign((Source as TAdvSmoothListBoxFilter).Fill);
    FVisible := (Source as TAdvSmoothListBoxFilter).Visible;
    FHeight := (Source as TAdvSmoothListBoxFilter).Height;
    FAutoFilter := (Source as TAdvSmoothListBoxFilter).AutoFilter;
    FCollapseButton := (Source as TAdvSmoothListBoxFilter).CollapseButton;
    FEnabled := (Source as TAdvSmoothListBoxFilter).Enabled;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.ButtonClicked(Sender: TObject);
begin
  if Assigned(FEdit) then
    ProcessFilter(FEdit.Text);
end;

procedure TAdvSmoothListBoxFilter.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSmoothListBoxFilter.Create(AOwner: TAdvSmoothListBox);
begin
  FOwner := AOWner;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FVisible := false;
  FHeight := 40;
  FEdit := FOwner.GetFilterEditClass.Create(FOwner);
  FEdit.Visible := false;
  TCustomEditProtected(FEdit).OnChange := EditChanged;
  TCustomEditProtected(FEdit).OnKeyUp := EditKeyUp;
  FAutoFilter := true;
  FCollapseButton := false;

  FButton := TSpeedButton.Create(FOwner);
  FButton.Visible := false;
  FButton.OnClick := ButtonClicked;
  FButton.Caption := 'Filter';
  FButton.Width := 50;

  FShowButton := TSpeedButton.Create(FOwner);
  FShowButton.Visible := false;
  FShowButton.OnClick := ShowButtonClicked;
  FShowButton.Height := 20;
  FShowButton.Width := 20;

  FEnabled := True;
end;

procedure TAdvSmoothListBox.CustomizeFilterEdit(AEdit: TCustomEdit);
begin

end;

destructor TAdvSmoothListBoxFilter.Destroy;
begin
  FEdit.Free;
  FButton.Free;
  FShowButton.Free;
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothListBoxFilter.DrawArrow(g: TGPGraphics; R: TRect; Up: Boolean);
var
  path: TGPGraphicsPath;
  b: TGPSolidBrush;
  pts: array[0..3] of TGPPointF;
begin
  path := TGPGraphicsPath.Create;
  if up then
  begin
    pts[0].X := r.Left + (r.Right - r.Left) / 2;
    pts[0].Y := r.Top;

    pts[1].X := r.Left;
    pts[1].Y := r.Bottom;

    pts[2].X := r.Right;
    pts[2].Y := r.Bottom;
  end
  else
  begin
    pts[0].X := r.Left + (r.Right - r.Left) / 2;
    pts[0].Y := r.Bottom;

    pts[1].X := r.Left;
    pts[1].Y := r.Top;

    pts[2].X := r.Right;
    pts[2].Y := r.Top;
  end;

  path.AddPolygon(PGPPointF(@pts), 3);

  b := TGPSolidBrush.Create(MakeColor(255, clBlack));
  g.FillPath(b, path);

  b.Free;
  path.Free;
end;

procedure TAdvSmoothListBoxFilter.EditChanged(Sender: TObject);
var
  allow: Boolean;
  txt: String;
begin
  if Visible and Assigned(FEdit) and Assigned(FOwner) then
  begin
    txt := FEdit.Text;
    allow := True;
    FOwner.DoFilterChange(Fowner, txt, allow);
    Fedit.Text := txt;
    if allow and AutoFilter then
      ProcessFilter(FEdit.Text);
  end;
end;

procedure TAdvSmoothListBoxFilter.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
      70:
      begin
        Visible := not Visible;
        FOwner.SetFocus;
      end;
    end;
  end;
end;

procedure TAdvSmoothListBoxFilter.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothListBox.GetFilterEditClass: TEditClass;
begin
  Result := TEdit;
end;

function TAdvSmoothListBoxFilter.GetHeight: integer;
var
  hb: integer;
begin
  hb := 0;
  if Fill.BorderColor <> clNone then
    hb := Fill.BorderWidth;

  if Visible and Enabled then
    Result := Height + hb
  else
    Result := 0;
end;

procedure TAdvSmoothListBoxFilter.ProcessFilter(Filter: String);
var
  i: integer;
begin
  if Assigned(FOwner) then
  begin
    FOwner.Items.BeginUpdate;
    for I := 0 to FOwner.Items.Count - 1 do
    begin
      FOwner.Items[i].Visible := AnsiContainsText(FOwner.Items[i].Caption, Filter) or (Filter = '');
    end;

    FOwner.Items.EndUpdate;
    if Assigned(FOwner.OnFilterProcessed) then
      FOwner.OnFilterProcessed(Self, Filter);
  end;
end;

procedure TAdvSmoothListBoxFilter.SetAutoFilter(const Value: Boolean);
begin
  if FAutoFilter <> Value then
  begin
    FAutoFilter := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetCollapseButton(const Value: Boolean);
begin
  if FCollapseButton <> Value then
  begin
    FCollapseButton := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetTextHint(const Value: String);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothListBoxFilter.ShowButtonClicked(Sender: TObject);
begin
  Visible := not Visible;
end;

procedure TAdvSmoothListBoxFilter.UpdateFilter;
var
  rc: TRect;
  g: TGPGraphics;
begin
  if Assigned(FEdit) and Assigned(FOwner) then
  begin
    rc := FOwner.InsideRect;

    if Visible and not (csDesigning in FOwner.ComponentState) then
      FEdit.Parent := FOwner
    else
      FEdit.Parent := nil;

    FEdit.Left := rc.Left + 5;
    FEdit.Top := rc.Top + FOwner.Header.GetHeight + (FOwner.Filter.GetHeight - Fedit.Height) div 2;
    if (not AutoFilter) then
      FEdit.Width := rc.Right - rc.Left - FOwner.GetShadowOffset - FButton.Width - 15
    else
      FEdit.Width := rc.Right - rc.Left - FOwner.GetShadowOffset - 10;

    FEdit.Visible := Visible and Enabled;
    {$IFDEF DELPHI_UNICODE}
    FEdit.TextHint := TextHint;
    {$ENDIF}
    FOwner.CustomizeFilterEdit(FEdit);

    FButton.Parent := FEdit.Parent;
    FButton.Left := FEdit.Left + FEdit.Width + 5;
    FButton.Top := rc.Top + FOwner.Header.GetHeight + (FOwner.Filter.GetHeight - FButton.Height) div 2;
    FButton.Visible := (not AutoFilter) and Visible and Enabled;

    if not (csDesigning in FOwner.ComponentState)  then
      FShowButton.Parent := FOwner
    else
      FShowButton.Parent := nil;
    FShowButton.Left := rc.Left + 3;
    FShowButton.Top := rc.Top - FShowButton.Height - 3 + FOwner.Header.GetHeight;

    if Visible and Enabled then
    begin
      FShowButton.Glyph.Assign(nil);
      FShowButton.Glyph.Width := 16;
      FShowButton.Glyph.Height := 16;
      g := TGPGraphics.Create(FShowButton.Glyph.Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);
      DrawArrow(g, Bounds(2, 2, 9, 9), True);
      g.Free;
      FShowButton.Hint := 'Hide Filter';
    end
    else
    begin
      FShowButton.Glyph.Assign(nil);
      FShowButton.Glyph.Width := 16;
      FShowButton.Glyph.Height := 16;
      g := TGPGraphics.Create(FShowButton.Glyph.Canvas.Handle);
      g.SetSmoothingMode(SmoothingModeAntiAlias);
      DrawArrow(g, Bounds(2, 2, 9, 9), False);
      g.Free;
      FShowButton.Hint := 'Show Filter';
    end;

    if not Visible and (FOwner.Header.GetHeight > 0) then
      FShowButton.Visible := CollapseButton
    else if Visible then
      FShowButton.Visible := CollapseButton
    else
      FShowButton.Visible := false;

  end;
end;

end.

