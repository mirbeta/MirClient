{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvTreeViewData;

{$I TMSDEFS.INC}

{$IFDEF LCLLIB}
{$mode objfpc}{$H+}{$modeswitch advancedrecords}
{$ENDIF}

interface

uses
  Classes, {$IFNDEF LCLLIB} Generics.Collections, {$ELSE}fgl,{$ENDIF} Types, AdvTreeViewBase, PictureContainer
  {$IFDEF FMXLIB}
  ,FMX.Graphics
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Graphics
  {$ENDIF}
  ;

resourcestring
  sTMSFMXTreeViewGroup = 'Group';
  sTMSFMXTreeViewColumn = 'Column';

const
  {$IFDEF DELPHI_LLVM}
  MAXLEVELDEPTH = 100;
  MAXNODECOUNT = 1000000;
  {$ELSE}
  MAXLEVELDEPTH = 100;
  MAXNODECOUNT = 2000000;
  {$ENDIF}

type
  TAdvTreeViewCacheItem = class;
  TAdvTreeViewData = class;
  TAdvTreeViewGroup = class;
  TAdvTreeViewColumn = class;

  TAdvTreeViewTextAlign = (tvtaCenter, tvtaLeading, tvtaTrailing);
  TAdvTreeViewTextTrimming = (tvttNone, tvttCharacter, tvttWord);
  TAdvTreeViewNode = class;

  TAdvTreeViewNodeScrollPosition = (tvnspTop, tvnspBottom, tvnspMiddle);

  TAdvTreeViewVirtualNodeRemoveData = record
    RowIndex: Integer;
    Count: Integer;
    ParentNode: Integer;
  end;

  TAdvArrayTRectF = TArray<TRectF>;
  TAdvArrayBoolean = TArray<Boolean>;

  TAdvTreeViewVirtualNode = class
  private
    FTreeView: TAdvTreeViewData;
    FLevel: Integer;
    FChildren: Integer;
    FParentNode: Integer;
    FExpanded: Boolean;
    FTotalChildren: Integer;
    FIndex: Integer;
    FRow: Integer;
    FHeight: Double;
    FCalculated: Boolean;
    FCache: TAdvTreeViewCacheItem;
    FTextRects: TAdvArrayTRectF;
    FBitmapRects: TAdvArrayTRectF;
    FExpandRects: TAdvArrayTRectF;
    FCheckRects: TAdvArrayTRectF;
    FCheckStates: TAdvArrayBoolean;
    FNode: TAdvTreeViewNode;
    FExtended: Boolean;
  protected
    property Cache: TAdvTreeViewCacheItem read FCache;
  public
    function GetParent: TAdvTreeViewVirtualNode; virtual;
    function GetChildCount: Integer; virtual;
    function GetPrevious: TAdvTreeViewVirtualNode; virtual;
    function GetNext: TAdvTreeViewVirtualNode; virtual;
    function GetPreviousChild(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetNextChild(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetNextSibling: TAdvTreeViewVirtualNode; virtual;
    function GetPreviousSibling: TAdvTreeViewVirtualNode; virtual;
    function GetFirstChild: TAdvTreeViewVirtualNode; virtual;
    function GetLastChild: TAdvTreeViewVirtualNode; virtual;
    procedure RemoveChildren; virtual;
    procedure Expand(ARecurse: Boolean = False); virtual;
    procedure Collapse(ARecurse: Boolean = False); virtual;

    property Children: Integer read FChildren;
    property TotalChildren: Integer read FTotalChildren;
    property Expanded: Boolean read FExpanded;
    property Extended: Boolean read FExtended;
    property Level: Integer read FLevel;
    property Row: Integer read FRow;
    property Calculated: Boolean read FCalculated;
    property Height: Double read FHeight;
    property ParentNode: Integer read FParentNode;
    property Index: Integer read FIndex;
    property TextRects: TAdvArrayTRectF read FTextRects;
    property BitmapRects: TAdvArrayTRectF read FBitmapRects;
    property CheckRects: TAdvArrayTRectF read FCheckRects;
    property ExpandRects: TAdvArrayTRectF read FExpandRects;
    property CheckStates: TAdvArrayBoolean read FCheckStates;
    property Node: TAdvTreeViewNode read FNode;
    constructor Create(ATreeView: TAdvTreeViewData);
    destructor Destroy; override;
  end;

  TAdvTreeViewCacheItemKind = (ikNode, ikColumnTop, ikColumnBottom, ikGroupTop, ikGroupBottom);

  TAdvTreeViewCacheItem = class
  private
    FRect: TRectF;
    FDrawRect: TRectF;
    FKind: TAdvTreeViewCacheItemKind;
    FGroup: Integer;
    FColumn: Integer;
    FNode: TAdvTreeViewVirtualNode;
    FRow: Integer;
    FCol: Integer;
    FStartColumn: Integer;
    FEndColumn: Integer;
  public
    class function CreateNode(ARect: TRectF; ANode: TAdvTreeViewVirtualNode): TAdvTreeViewCacheItem;
    class function CreateColumnTop(ARect: TRectF; AColumn: Integer): TAdvTreeViewCacheItem;
    class function CreateGroupTop(ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer): TAdvTreeViewCacheItem;
    class function CreateColumnBottom(ARect: TRectF; AColumn: Integer): TAdvTreeViewCacheItem;
    class function CreateGroupBottom(ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer): TAdvTreeViewCacheItem;

    property Rect: TRectF read FRect write FRect;
    property DrawRect: TRectF read FDrawRect write FDrawRect;
    property Kind: TAdvTreeViewCacheItemKind read FKind write FKind;
    property Node: TAdvTreeViewVirtualNode read FNode write FNode;
    property Group: Integer read FGroup write FGroup;
    property StartColumn: Integer read FStartColumn write FStartColumn;
    property EndColumn: Integer read FEndColumn write FEndColumn;
    property Column: Integer read FColumn write FColumn;
    property Col: Integer read FCol write FCol;
    property Row: Integer read FRow write FRow;
    destructor Destroy; override;
  end;

  TAdvTreeViewCacheItemList = class(TList<TAdvTreeViewCacheItem>);
  TAdvTreeViewIntegerList = class(TList<Integer>);

  TAdvTreeViewColumnEditorType = (tcetEdit, tcetComboBox, tcetMemo, tcetNone);

  TAdvTreeViewColumn = class(TCollectionItem)
  private
    FTag: Integer;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: Integer;
    FTreeView: TAdvTreeViewData;
    FText: String;
    FName: String;
    FDBKey: String;
    FDataBoolean: Boolean;
    FWordWrapping: Boolean;
    FVerticalTextAlign: TAdvTreeViewTextAlign;
    FTrimming: TAdvTreeViewTextTrimming;
    FHorizontalTextAlign: TAdvTreeViewTextAlign;
    FWidth: Double;
    FVisible: Boolean;
    FUseDefaultAppearance: Boolean;
    FBottomFill: TAdvTreeViewBrush;
    FBottomStroke: TAdvTreeViewStrokeBrush;
    FTopFill: TAdvTreeViewBrush;
    FTopStroke: TAdvTreeViewStrokeBrush;
    FTopFontColor: TAdvTreeViewColor;
    FTopFont: TFont;
    FBottomFontColor: TAdvTreeViewColor;
    FBottomFont: TFont;
    FFont: TFont;
    FFill: TAdvTreeViewBrush;
    FFontColor: TAdvTreeViewColor;
    FStroke: TAdvTreeViewStrokeBrush;
    FEditorType: TAdvTreeViewColumnEditorType;
    FDisabledFontColor: TAdvTreeViewColor;
    FSelectedFontColor: TAdvTreeViewColor;
    FEditorItems: TStringList;
    FCustomEditor: Boolean;
    procedure SetText(const Value: String);
    procedure SetName(const Value: String);
    procedure SetHorizontalTextAlign(const Value: TAdvTreeViewTextAlign);
    procedure SetTrimming(const Value: TAdvTreeViewTextTrimming);
    procedure SetVerticalTextAlign(const Value: TAdvTreeViewTextAlign);
    procedure SetWordWrapping(const Value: Boolean);
    procedure SetWidth(const Value: Double);
    procedure SetVisible(const Value: Boolean);
    procedure SetUseDefaultAppearance(const Value: Boolean);
    procedure SetBottomFill(const Value: TAdvTreeViewBrush);
    procedure SetBottomStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetTopFill(const Value: TAdvTreeViewBrush);
    procedure SetTopStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetBottomFont(const Value: TFont);
    procedure SetBottomFontColor(const Value: TAdvTreeViewColor);
    procedure SetTopFont(const Value: TFont);
    procedure SetTopFontColor(const Value: TAdvTreeViewColor);
    procedure SetFill(const Value: TAdvTreeViewBrush);
    procedure SetFont(const Value: TFont);
    procedure SetFontColor(const Value: TAdvTreeViewColor);
    procedure SetStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetEditorType(const Value: TAdvTreeViewColumnEditorType);
    procedure SetDisabledFontColor(const Value: TAdvTreeViewColor);
    procedure SetSelectedFontColor(const Value: TAdvTreeViewColor);
    procedure SetEditorItems(const Value: TStringList);
  protected
    procedure UpdateColumn;
    procedure Changed(Sender: TObject);
    function GetText: String; virtual;
    function GetColumnText: String; virtual;
  public
    function TreeView: TAdvTreeViewData;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateWidth(AWidth: Double); virtual;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
  published
    property EditorType: TAdvTreeViewColumnEditorType read FEditorType write SetEditorType default tcetNone;
    property EditorItems: TStringList read FEditorItems write SetEditorItems;
    property CustomEditor: Boolean read FCustomEditor write FCustomEditor default False;

    property Name: String read FName write SetName;
    property Text: String read FText write SetText;
    property Tag: Integer read FTag write FTag default 0;
    property HorizontalTextAlign: TAdvTreeViewTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default tvtaLeading;
    property VerticalTextAlign: TAdvTreeViewTextAlign read FVerticalTextAlign write SetVerticalTextAlign default tvtaCenter;
    property WordWrapping: Boolean read FWordWrapping write SetWordWrapping default False;
    property Trimming: TAdvTreeViewTextTrimming read FTrimming write SetTrimming default tvttNone;
    property Width: Double read FWidth write SetWidth;
    property Visible: Boolean read FVisible write SetVisible default True;
    property UseDefaultAppearance: Boolean read FUseDefaultAppearance write SetUseDefaultAppearance default True;
    property Fill: TAdvTreeViewBrush read FFill write SetFill;
    property Stroke: TAdvTreeViewStrokeBrush read FStroke write SetStroke;
    property Font: TFont read FFont write SetFont;
    property FontColor: TAdvTreeViewColor read FFontColor write SetFontColor default TAdvTreeViewColorGray;
    property SelectedFontColor: TAdvTreeViewColor read FSelectedFontColor write SetSelectedFontColor default TAdvTreeViewColorWhite;
    property DisabledFontColor: TAdvTreeViewColor read FDisabledFontColor write SetDisabledFontColor default TAdvTreeViewColorSilver;

    property TopFill: TAdvTreeViewBrush read FTopFill write SetTopFill;
    property TopStroke: TAdvTreeViewStrokeBrush read FTopStroke write SetTopStroke;
    property TopFont: TFont read FTopFont write SetTopFont;
    property TopFontColor: TAdvTreeViewColor read FTopFontColor write SetTopFontColor default TAdvTreeViewColorGray;

    property BottomFill: TAdvTreeViewBrush read FBottomFill write SetBottomFill;
    property BottomStroke: TAdvTreeViewStrokeBrush read FBottomStroke write SetBottomStroke;
    property BottomFont: TFont read FBottomFont write SetBottomFont;
    property BottomFontColor: TAdvTreeViewColor read FBottomFontColor write SetBottomFontColor default TAdvTreeViewColorGray;
  end;

  TAdvTreeViewColumns = class(TOwnedCollection)
  private
    FTreeView: TAdvTreeViewData;
    function GetItem(Index: Integer): TAdvTreeViewColumn;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewColumn);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function TreeView: TAdvTreeViewData;
    constructor Create(ATreeView: TAdvTreeViewData);
    function Add: TAdvTreeViewColumn; virtual;
    function Insert(Index: Integer): TAdvTreeViewColumn;
    property Items[Index: Integer]: TAdvTreeViewColumn read GetItem write SetItem; default;
  end;

  TAdvTreeViewNodes = class;
  TAdvTreeViewNodeCheckType = (tvntNone, tvntCheckBox, tvntRadioButton);

  TAdvTreeViewNodeValue = class(TCollectionItem)
  private
    FTag: Integer;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: Integer;
    FTreeView: TAdvTreeViewData;
    FNode: TAdvTreeViewNode;
    FText: String;
    FDBKey: String;
    FDataBoolean: Boolean;
    FCheckType: TAdvTreeViewNodeCheckType;
    FChecked: Boolean;
    FCollapsedIcon: TAdvTreeViewBitmap;
    FCollapsedIconLarge: TAdvTreeViewBitmap;
    FExpandedIcon: TAdvTreeViewBitmap;
    FExpandedIconLarge: TAdvTreeViewBitmap;
    FCollapsedIconName: String;
    FCollapsedIconLargeName: String;
    FExpandedIconName: String;
    FExpandedIconLargeName: String;
    procedure SetText(const Value: String);
    procedure UpdateNodeValue;
    procedure SetChecked(const Value: Boolean);
    procedure SetCheckType(const Value: TAdvTreeViewNodeCheckType);
    procedure SetCollapsedIconLarge(const Value: TAdvTreeViewBitmap);
    procedure SetCollapsedIcon(const Value: TAdvTreeViewBitmap);
    procedure SetExpandedIconLarge(const Value: TAdvTreeViewBitmap);
    procedure SetExpandedIcon(const Value: TAdvTreeViewBitmap);
    procedure SetCollapsedIconLargeName(const Value: String);
    procedure SetCollapsedIconName(const Value: String);
    procedure SetExpandedIconLargeName(const Value: String);
    procedure SetExpandedIconName(const Value: String);
    function GetPictureContainer: TPictureContainer;
  protected
    procedure BitmapChanged(Sender: TObject);
  public
    function TreeView: TAdvTreeViewData;
    function Node: TAdvTreeViewNode;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property PictureContainer: TPictureContainer read GetPictureContainer;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
  published
    property Tag: Integer read FTag write FTag default 0;
    property Text: String read FText write SetText;
    property CollapsedIconName: String read FCollapsedIconName write SetCollapsedIconName;
    property CollapsedIconLargeName: String read FCollapsedIconLargeName write SetCollapsedIconLargeName;
    property ExpandedIconName: String read FExpandedIconName write SetExpandedIconName;
    property ExpandedIconLargeName: String read FExpandedIconLargeName write SetExpandedIconLargeName;

    property CollapsedIcon: TAdvTreeViewBitmap read FCollapsedIcon write SetCollapsedIcon;
    property CollapsedIconLarge: TAdvTreeViewBitmap read FCollapsedIconLarge write SetCollapsedIconLarge;
    property ExpandedIcon: TAdvTreeViewBitmap read FExpandedIcon write SetExpandedIcon;
    property ExpandedIconLarge: TAdvTreeViewBitmap read FExpandedIconLarge write SetExpandedIconLarge;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckType: TAdvTreeViewNodeCheckType read FCheckType write SetCheckType default tvntNone;
  end;

  TAdvTreeViewNodeValues = class(TOwnedCollection)
  private
    FTreeView: TAdvTreeViewData;
    FNode: TAdvTreeViewNode;
    function GetItem(Index: Integer): TAdvTreeViewNodeValue;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewNodeValue);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function TreeView: TAdvTreeViewData;
    function Node: TAdvTreeViewNode;
    procedure UpdateChecked(AIndex: Integer; AValue: Boolean); virtual;
    constructor Create(ATreeView: TAdvTreeViewData; ANode: TAdvTreeViewNode);
    function Add: TAdvTreeViewNodeValue;
    function Insert(Index: Integer): TAdvTreeViewNodeValue;
    property Items[Index: Integer]: TAdvTreeViewNodeValue read GetItem write SetItem; default;
  end;

  TAdvTreeViewNodeTextValues = array of string;
  TAdvTreeViewNodeCheckStates = array of Boolean;
  TAdvTreeViewNodeCheckTypes = array of TAdvTreeViewNodeCheckType;
  TAdvTreeViewNodeIcons = array of TAdvTreeViewBitmap;
  TAdvTreeViewNodeIconNames = array of string;

  TAdvTreeViewNode = class(TCollectionItem)
  private
    FTag: Integer;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: Integer;
    FTreeView: TAdvTreeViewData;
    FValues: TAdvTreeViewNodeValues;
    FDBKey: String;
    FDataBoolean: Boolean;
    FNodes: TAdvTreeViewNodes;
    FVirtualNode: TAdvTreeViewVirtualNode;
    FExpanded: Boolean;
    FEnabled: Boolean;
    FExtended: Boolean;
    procedure SetValues(const Value: TAdvTreeViewNodeValues);
    procedure SetNodes(const Value: TAdvTreeViewNodes);
    procedure SetExpanded(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetExtended(const Value: Boolean);
    function GetChecked(AColumn: integer): Boolean;
    procedure SetChecked(AColumn: integer; const Value: Boolean);
    function GetCheckType(AColumn: Integer): TAdvTreeViewNodeCheckType;
    function GetCollapsedIcon(AColumn: Integer;
      ALarge: Boolean): TAdvTreeViewBitmap;
    function GetCollapsedIconName(AColumn: Integer;
      ALarge: Boolean): String;
    function GetExpandedIcon(AColumn: Integer;
      ALarge: Boolean): TAdvTreeViewBitmap;
    function GetExpandedIconName(AColumn: Integer;
      ALarge: Boolean): String;
    function GetText(AColumn: Integer): String;
    procedure SetCheckType(AColumn: Integer;
      const Value: TAdvTreeViewNodeCheckType);
    procedure SetCollapsedIcon(AColumn: Integer; ALarge: Boolean;
      const Value: TAdvTreeViewBitmap);
    procedure SetCollapsedIconName(AColumn: Integer; ALarge: Boolean;
      const Value: String);
    procedure SetExpandedIcon(AColumn: Integer; ALarge: Boolean;
      const Value: TAdvTreeViewBitmap);
    procedure SetExpandedIconName(AColumn: Integer; ALarge: Boolean;
      const Value: String);
    procedure SetText(AColumn: Integer; const Value: String);
  protected
    procedure UpdateNode;
    function GetValueForColumn(AColumn: Integer): TAdvTreeViewNodeValue; virtual;
    function CreateNodes: TAdvTreeViewNodes; virtual;
    function CreateNodeValues: TAdvTreeViewNodeValues; virtual;
    procedure SetIndex(Value: Integer); override;
    procedure ValuesChanged(Sender: TObject);
  public
    function TreeView: TAdvTreeViewData;
    function GetParent: TAdvTreeViewNode; virtual;
    function GetChildCount: Integer; virtual;
    function GetPrevious: TAdvTreeViewNode; virtual;
    function GetNext: TAdvTreeViewNode; virtual;
    function GetPreviousChild(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetNextChild(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetNextSibling: TAdvTreeViewNode; virtual;
    function GetPreviousSibling: TAdvTreeViewNode; virtual;
    function GetFirstChild: TAdvTreeViewNode; virtual;
    function GetLastChild: TAdvTreeViewNode; virtual;
    procedure RemoveChildren; virtual;
    procedure Expand(ARecurse: Boolean = False); virtual;
    procedure Collapse(ARecurse: Boolean = False); virtual;

    procedure SetTextValues(ATextValues: TAdvTreeViewNodeTextValues); virtual;
    procedure SetCheckStates(ACheckStates: TAdvTreeViewNodeCheckStates); virtual;
    procedure SetCheckTypes(ACheckTypes: TAdvTreeViewNodeCheckTypes); virtual;
    procedure SetCollapsedIcons(AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False); virtual;
    procedure SetExpandedIcons(AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False); virtual;
    procedure SetCollapsedIconNames(AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False); virtual;
    procedure SetExpandedIconNames(AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False); virtual;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    property Checked[AColumn: Integer]: Boolean read GetChecked write SetChecked;
    property Text[AColumn: Integer]: String read GetText write SetText;
    property CheckTypes[AColumn: Integer]: TAdvTreeViewNodeCheckType read GetCheckType write SetCheckType;
    property ExpandedIcons[AColumn: Integer; ALarge: Boolean]: TAdvTreeViewBitmap read GetExpandedIcon write SetExpandedIcon;
    property CollapsedIcons[AColumn: Integer; ALarge: Boolean]: TAdvTreeViewBitmap read GetCollapsedIcon write SetCollapsedIcon;
    property ExpandedIconNames[AColumn: Integer; ALarge: Boolean]: String read GetExpandedIconName write SetExpandedIconName;
    property CollapsedIconNames[AColumn: Integer; ALarge: Boolean]: String read GetCollapsedIconName write SetCollapsedIconName;
    property VirtualNode: TAdvTreeViewVirtualNode read FVirtualNode;
  published
    property Values: TAdvTreeViewNodeValues read FValues write SetValues;
    property Expanded: Boolean read FExpanded write SetExpanded default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Extended: Boolean read FExtended write SetExtended default False;
    property Tag: Integer read FTag write FTag;
    property Nodes: TAdvTreeViewNodes read FNodes write SetNodes;
  end;

  TAdvTreeViewNodes = class(TOwnedCollection)
  private
    FTreeView: TAdvTreeViewData;
    FNode: TAdvTreeViewNode;
    function GetItem(Index: Integer): TAdvTreeViewNode;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewNode);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function TreeView: TAdvTreeViewData;
    function Node: TAdvTreeViewNode;
    constructor Create(ATreeView: TAdvTreeViewData; ANode: TAdvTreeViewNode);
    destructor Destroy; override;
    function Add: TAdvTreeViewNode;
    function Insert(Index: Integer): TAdvTreeViewNode;
    property Items[Index: Integer]: TAdvTreeViewNode read GetItem write SetItem; default;
  end;

  TAdvTreeViewGroup = class(TCollectionItem)
  private
    FTag: Integer;
    FDataString: String;
    FDataObject: TObject;
    FDataInteger: Integer;
    FTreeView: TAdvTreeViewData;
    FText: String;
    FStartColumn: Integer;
    FEndColumn: Integer;
    FName: String;
    FDBKey: String;
    FDataBoolean: Boolean;
    FBottomFill: TAdvTreeViewBrush;
    FBottomStroke: TAdvTreeViewStrokeBrush;
    FTopFill: TAdvTreeViewBrush;
    FTopStroke: TAdvTreeViewStrokeBrush;
    FTopFontColor: TAdvTreeViewColor;
    FTopFont: TFont;
    FBottomFontColor: TAdvTreeViewColor;
    FBottomFont: TFont;
    FUseDefaultAppearance: Boolean;
    procedure SetText(const Value: String);
    procedure SetStartColumn(const Value: Integer);
    procedure SetEndColumn(const Value: Integer);
    procedure SetName(const Value: String);
    procedure SetBottomFill(const Value: TAdvTreeViewBrush);
    procedure SetBottomStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetTopFill(const Value: TAdvTreeViewBrush);
    procedure SetTopStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetBottomFont(const Value: TFont);
    procedure SetBottomFontColor(const Value: TAdvTreeViewColor);
    procedure SetTopFont(const Value: TFont);
    procedure SetTopFontColor(const Value: TAdvTreeViewColor);
    procedure SetUseDefaultAppearance(const Value: Boolean);
  protected
    procedure UpdateGroup;
    procedure Changed(Sender: TObject);
  public
    function TreeView: TAdvTreeViewData;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataBoolean: Boolean read FDataBoolean write FDataBoolean;
    property DataObject: TObject read FDataObject write FDataObject;
    property DataString: String read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DBKey: String read FDBKey write FDBKey;
    function GetText: String; virtual;
  published
    property Name: String read FName write SetName;
    property Text: String read FText write SetText;
    property StartColumn: Integer read FStartColumn write SetStartColumn default 0;
    property EndColumn: Integer read FEndColumn write SetEndColumn default 0;
    property Tag: Integer read FTag write FTag default 0;
    property UseDefaultAppearance: Boolean read FUseDefaultAppearance write SetUseDefaultAppearance default True;

    property TopFill: TAdvTreeViewBrush read FTopFill write SetTopFill;
    property TopStroke: TAdvTreeViewStrokeBrush read FTopStroke write SetTopStroke;
    property TopFont: TFont read FTopFont write SetTopFont;
    property TopFontColor: TAdvTreeViewColor read FTopFontColor write SetTopFontColor;

    property BottomFill: TAdvTreeViewBrush read FBottomFill write SetBottomFill;
    property BottomStroke: TAdvTreeViewStrokeBrush read FBottomStroke write SetBottomStroke;
    property BottomFont: TFont read FBottomFont write SetBottomFont;
    property BottomFontColor: TAdvTreeViewColor read FBottomFontColor write SetBottomFontColor;
  end;

  TAdvTreeViewGroups = class(TOwnedCollection)
  private
    FTreeView: TAdvTreeViewData;
    function GetItem(Index: Integer): TAdvTreeViewGroup;
    procedure SetItem(Index: Integer; const Value: TAdvTreeViewGroup);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
  public
    function TreeView: TAdvTreeViewData;
    constructor Create(ATreeView: TAdvTreeViewData);
    function Add: TAdvTreeViewGroup;
    function Insert(Index: Integer): TAdvTreeViewGroup;
    property Items[Index: Integer]: TAdvTreeViewGroup read GetItem write SetItem; default;
  end;

  TAdvTreeViewNodeTextMode = (tntmDrawing, tntmEditing);
  TAdvTreeViewNodeStructure = class(TObjectList<TAdvTreeViewVirtualNode>);
  TAdvTreeViewVisibleNodes = class(TList<TAdvTreeViewVirtualNode>);

  TAdvTreeViewDisplayGroup = record
    StartColumn: Integer;
    EndColumn: Integer;
    {$IFDEF LCLLIB}
    class operator = (z1, z2 : TAdvTreeViewDisplayGroup) b : boolean;
    {$ENDIF}
  end;

  TAdvTreeViewDisplayGroups = class(TList<TAdvTreeViewDisplayGroup>);

  TAdvTreeViewData = class(TAdvTreeViewBase)
  private
    FBlockUpdate: Boolean;
    FNodeStructure: TAdvTreeViewNodeStructure;
    FVisibleNodes: TAdvTreeViewVisibleNodes;
    FColumns: TAdvTreeViewColumns;
    FGroups: TAdvTreeViewGroups;
    FNodes: TAdvTreeViewNodes;
    FPictureContainer: TPictureContainer;
    FDisplayGroups: TAdvTreeViewDisplayGroups;
    procedure SetColumns(const Value: TAdvTreeViewColumns);
    procedure SetGroups(const Value: TAdvTreeViewGroups);
    procedure SetNodes(const Value: TAdvTreeViewNodes);
  protected
    property PictureContainer: TPictureContainer read FPictureContainer write FPictureContainer;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function CreateColumns: TAdvTreeViewColumns; virtual;
    function CreateGroups: TAdvTreeViewGroups; virtual;
    function CreateNodes: TAdvTreeViewNodes; virtual;
    function GetInsertPosition(ANode: TAdvTreeViewVirtualNode; ARoot: Boolean; AInsert: Boolean): Integer; virtual;
    function GetNodeFromNodeStructure(AIndex: Integer): TAdvTreeViewVirtualNode; virtual;
    procedure BuildNodeList; virtual;
    procedure UpdateColumns; override;
    procedure BuildInternalNodeList(ANode: TAdvTreeViewVirtualNode; AID: String; AParentNode: Integer; ANodeIndex: Integer; ALevel: Integer; AParentExpanded: Boolean; var ATotalChildren: Integer);
    procedure GetNodeForNodeData(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoGetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer); virtual;
    procedure DoGetColumnTrimming(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvTreeViewTextTrimming); virtual;
    procedure DoGetColumnHorizontalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvTreeViewTextAlign); virtual;
    procedure DoGetColumnVerticalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvTreeViewTextAlign); virtual;
    procedure DoGetColumnWordWrapping(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean); virtual;
    procedure DoGetNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String); virtual;
    procedure DoGetNodeTrimming(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvTreeViewTextTrimming); virtual;
    procedure DoGetNodeHorizontalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvTreeViewTextAlign); virtual;
    procedure DoGetNodeVerticalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvTreeViewTextAlign); virtual;
    procedure DoGetNodeWordWrapping(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean); virtual;
    procedure DoGetNodeCheckType(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType); virtual;
    procedure DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvTreeViewIconBitmap); virtual;
    procedure DoGetNodeSelectedColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); virtual;
    procedure DoGetNodeDisabledColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); virtual;
    procedure DoGetNodeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); virtual;
    procedure DoGetNodeSelectedTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); virtual;
    procedure DoGetNodeDisabledTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); virtual;
    procedure DoGetNodeTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); virtual;
    procedure DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean); virtual;
    procedure DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean); virtual;
    procedure DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AChecked: Boolean); virtual;
    procedure DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode; var AVisible: Boolean); virtual;
    procedure DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean); virtual;
    procedure ToggleCheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure CheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure UnCheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure ExpandNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    procedure CollapseNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    function RemoveNodeInternal(ANode: TAdvTreeViewVirtualNode; AUpdateNodes: Boolean = True): TAdvTreeViewVirtualNodeRemoveData; virtual;
    procedure UpdateNodesInternal(ANodeRemoveData: TAdvTreeViewVirtualNodeRemoveData); virtual;
    procedure InsertNodeInternal(AParentNode: TAdvTreeViewVirtualNode; AIndex, AInsertIndex: Integer; ANode: TAdvTreeViewVirtualNode); virtual;
    procedure UpdateVisibleNodes(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure ToggleNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    procedure InsertItemInternal(AItem: TAdvTreeViewNode); virtual;
    function RemoveItemInternal(AItem: TAdvTreeViewNode; AUpdate: Boolean = True): TAdvTreeViewVirtualNodeRemoveData; virtual;
    procedure ToggleNode(ANode: TAdvTreeViewNode); virtual;
    procedure UpdateNodeCalculated(ANode: TAdvTreeViewVirtualNode; ACalculated: Boolean); virtual;
    procedure UpdateNodeHeight(ANode: TAdvTreeViewVirtualNode; AHeight: Double); virtual;
    procedure UpdateNodeCheckStates(ANode: TAdvTreeViewVirtualNode; ACheckStates: TAdvArrayBoolean); virtual;
    procedure UpdateNodeBitmapRects(ANode: TAdvTreeViewVirtualNode; ABitmapRects: TAdvArrayTRectF); virtual;
    procedure UpdateNodeCheckRects(ANode: TAdvTreeViewVirtualNode; ACheckRects: TAdvArrayTRectF); virtual;
    procedure UpdateNodeExpandRects(ANode: TAdvTreeViewVirtualNode; AExpandRects: TAdvArrayTRectF); virtual;
    procedure UpdateNodeTextRects(ANode: TAdvTreeViewVirtualNode; ATextRects: TAdvArrayTRectF); virtual;
    procedure UpdateNodeCacheReference(ANode: TAdvTreeViewVirtualNode; ACache: TAdvTreeViewCacheItem); virtual;
    procedure UpdateNodeExpanded(ANode: TAdvTreeViewVirtualNode; AExpanded: Boolean); virtual;
    procedure UpdateNodeExtended(ANode: TAdvTreeViewVirtualNode; AExtended: Boolean); virtual;
    procedure UpdateNodeTotalChildren(ANode: TAdvTreeViewVirtualNode; ATotalChildren: Integer); virtual;
    procedure UpdateNodeChildren(ANode: TAdvTreeViewVirtualNode; AChildren: Integer); virtual;
    procedure UpdateNodeIndex(ANode: TAdvTreeViewVirtualNode; AIndex: Integer); virtual;
    procedure UpdateNodeRow(ANode: TAdvTreeViewVirtualNode; ARow: Integer); virtual;
    procedure UpdateNodeLevel(ANode: TAdvTreeViewVirtualNode; ALevel: Integer); virtual;
    procedure UpdateNodeParentNode(ANode: TAdvTreeViewVirtualNode; AParentNode: Integer); virtual;
    procedure UpdateNode(ANode: TAdvTreeViewVirtualNode); virtual;
    property Columns: TAdvTreeViewColumns read FColumns write SetColumns;
    property Groups: TAdvTreeViewGroups read FGroups write SetGroups;
    property Nodes: TAdvTreeViewNodes read FNodes write SetNodes;
    property DisplayGroups: TAdvTreeViewDisplayGroups read FDisplayGroups;
    property BlockUpdate: Boolean read FBlockUpdate write FBlockUpdate;
    property NodeStructure: TAdvTreeViewNodeStructure read FNodeStructure;
    property VisibleNodes: TAdvTreeViewVisibleNodes read FVisibleNodes;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsColumnVisible(ACol: Integer): Boolean; override;
    function FindColumnByName(AName: String): TAdvTreeViewColumn; virtual;
    function FindGroupByName(AName: String): TAdvTreeViewGroup; virtual;
    function FindColumnIndexByName(AName: String): Integer; virtual;
    function FindGroupIndexByName(AName: String): Integer; virtual;

    procedure ClearColumns; virtual;
    procedure ClearNodes; virtual;
    procedure ClearNodeList; virtual;
    procedure RemoveVirtualNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure RemoveNode(ANode: TAdvTreeViewNode); virtual;
    procedure ExpandNode(ANode: TAdvTreeViewNode; ARecurse: Boolean = False); virtual;
    procedure ExpandAll; virtual;
    procedure CollapseAll; virtual;
    procedure ExpandAllVirtual; virtual;
    procedure CollapseAllVirtual; virtual;
    procedure CollapseNode(ANode: TAdvTreeViewNode; ARecurse: Boolean = False); virtual;
    procedure ToggleVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    procedure ExpandVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    procedure CollapseVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False); virtual;
    procedure CheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure UnCheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure ToggleCheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False); virtual;

    procedure CheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure UnCheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;
    procedure ToggleCheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False); virtual;

    procedure SetNodeTextValues(ANode: TAdvTreeViewNode; ATextValues: TAdvTreeViewNodeTextValues); virtual;
    procedure SetNodeCheckStates(ANode: TAdvTreeViewNode; ACheckStates: TAdvTreeViewNodeCheckStates); virtual;
    procedure SetNodeCheckTypes(ANode: TAdvTreeViewNode; ACheckTypes: TAdvTreeViewNodeCheckTypes); virtual;
    procedure SetNodeCollapsedIcons(ANode: TAdvTreeViewNode; AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False); virtual;
    procedure SetNodeExpandedIcons(ANode: TAdvTreeViewNode; AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False); virtual;
    procedure SetNodeCollapsedIconNames(ANode: TAdvTreeViewNode; AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False); virtual;
    procedure SetNodeExpandedIconNames(ANode: TAdvTreeViewNode; AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False); virtual;

    procedure RemoveVirtualNodeChildren(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure RemoveNodeChildren(ANode: TAdvTreeViewNode); virtual;
    procedure ScrollToNode(ANode: TAdvTreeViewNode; AScrollIfNotVisible: Boolean = False; AScrollPosition: TAdvTreeViewNodeScrollPosition = tvnspTop); virtual;
    procedure ScrollToVirtualNode(ANode: TAdvTreeViewVirtualNode; AScrollIfNotVisible: Boolean = False; AScrollPosition: TAdvTreeViewNodeScrollPosition = tvnspTop); virtual;

    function AddNode(AParentNode: TAdvTreeViewNode = nil): TAdvTreeViewNode; virtual;
    function AddVirtualNode(AParentNode: TAdvTreeViewVirtualNode = nil): TAdvTreeViewVirtualNode; virtual;
    function GetParentVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetParentNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetVirtualNodeChildCount(ANode: TAdvTreeViewVirtualNode): Integer; virtual;

    function GetNodeChildCount(ANode: TAdvTreeViewNode): Integer; virtual;
    function GetPreviousNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetNextNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetPreviousChildNode(ANode: TAdvTreeViewNode; AStartNode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetNextChildNode(ANode: TAdvTreeViewNode; AStartNode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetNextSiblingNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetPreviousSiblingNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetFirstChildNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;
    function GetLastChildNode(ANode: TAdvTreeViewNode): TAdvTreeViewNode; virtual;

    function GetPreviousVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetNextVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetPreviousChildVirtualNode(ANode: TAdvTreeViewVirtualNode; AStartNode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetNextChildVirtualNode(ANode: TAdvTreeViewVirtualNode; AStartNode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetNextSiblingVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetPreviousSiblingVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetFirstChildVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetLastChildVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;

    function GetFirstRootNode: TAdvTreeViewNode; virtual;
    function GetLastRootNode: TAdvTreeViewNode; virtual;
    function GetLastNode: TAdvTreeViewNode; virtual;
    function GetFirstRootVirtualNode: TAdvTreeViewVirtualNode; virtual;
    function GetLastRootVirtualNode: TAdvTreeViewVirtualNode; virtual;
    function GetLastVirtualNode: TAdvTreeViewVirtualNode; virtual;
    function GetRootNodeByIndex(AIndex: Integer): TAdvTreeViewNode; virtual;
    function GetRootVirtualNodeByIndex(AIndex: Integer): TAdvTreeViewVirtualNode; virtual;

    function InsertNode(AIndex: Integer; AParentNode: TAdvTreeViewNode = nil): TAdvTreeViewNode; virtual;
    function InsertVirtualNode(AIndex: Integer; AParentNode: TAdvTreeViewVirtualNode = nil): TAdvTreeViewVirtualNode; virtual;
  end;

function TranslateTextEx(AText: String): String;

implementation

uses
  SysUtils, Math, StrUtils
  {$IFDEF FMXLIB}
  , FMX.Types
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$ENDIF}
  ;

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := AText;
  {$ENDIF}
end;

{$IFDEF LCLLIB}
class operator TAdvTreeViewDisplayGroup.=(z1, z2: TAdvTreeViewDisplayGroup)b: boolean;
begin
  Result := z1 = z2;
end;
{$ENDIF}

{ TAdvTreeViewData }

procedure TAdvTreeViewData.RemoveVirtualNode(ANode: TAdvTreeViewVirtualNode);
begin
  RemoveNodeInternal(ANode);
end;          

procedure TAdvTreeViewData.ToggleCheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  if Assigned(ANode) then
    ToggleCheckNodeInternal(ANode.VirtualNode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.CheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  if Assigned(ANode) then
    CheckNodeInternal(ANode.VirtualNode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.UnCheckNode(ANode: TAdvTreeViewNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  if Assigned(ANode) then
    UnCheckNodeInternal(ANode.VirtualNode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.ToggleCheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  ToggleCheckNodeInternal(ANode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.CheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  CheckNodeInternal(ANode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.UnCheckVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
begin
  UnCheckNodeInternal(ANode, AColumn, ARecurse);
end;

procedure TAdvTreeViewData.CollapseAll;
  procedure CollapseNodes(n: TAdvTreeViewNodes);
  var
    I: Integer;
  begin
    for I := 0 to n.Count - 1 do
    begin
      n[I].FExpanded := False;
      CollapseNodes(n[I].Nodes);
    end;
  end;
begin
  CollapseNodes(Nodes);
  CollapseAllVirtual;
end;

procedure TAdvTreeViewData.CollapseAllVirtual;
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  VisibleNodes.Clear;
  TotalRowHeight := 1;
  RowCount := 0;
  for I := 0 to NodeStructure.Count - 1 do
  begin
    v := GetNodeFromNodeStructure(I);
    if Assigned(v) then
    begin
      UpdateNodeExpanded(v, False);
      if v.ParentNode = -1 then
      begin
        VisibleNodes.Add(v);
        TotalRowHeight := TotalRowHeight + v.Height;
        RowCount := RowCount + 1;
      end;
    end;
  end;

  ResetNodes;
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvTreeViewData.CollapseNode(ANode: TAdvTreeViewNode; ARecurse: Boolean = False);
begin
  if Assigned(ANode) then
    CollapseVirtualNode(ANode.VirtualNode, ARecurse);
end;

procedure TAdvTreeViewData.ToggleNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    ToggleVirtualNode(ANode.VirtualNode);
end;

procedure TAdvTreeViewData.ExpandAll;
  procedure ExpandNodes(n: TAdvTreeViewNodes);
  var
    I: Integer;
  begin
    for I := 0 to n.Count - 1 do
    begin
      n[I].FExpanded := True;
      ExpandNodes(n[I].Nodes);
    end;
  end;
begin
  ExpandNodes(Nodes);
  ExpandAllVirtual;
end;

procedure TAdvTreeViewData.ExpandAllVirtual;
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  VisibleNodes.Clear;
  TotalRowHeight := 1;
  RowCount := 0;
  for I := 0 to NodeStructure.Count - 1 do
  begin
    v := GetNodeFromNodeStructure(I);
    if Assigned(v) then
    begin
      UpdateNodeExpanded(v, True);
      VisibleNodes.Add(v);
      TotalRowHeight := TotalRowHeight + v.Height;
      RowCount := RowCount + 1;
    end;
  end;

  ResetNodes;
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvTreeViewData.ExpandNode(ANode: TAdvTreeViewNode; ARecurse: Boolean = False);
begin
  if Assigned(ANode) then
    ExpandVirtualNode(ANode.VirtualNode, ARecurse);
end;

procedure TAdvTreeViewData.CollapseVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
begin
  CollapseNodeInternal(ANode, ARecurse);
end;

procedure TAdvTreeViewData.ToggleVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
begin
  ToggleNodeInternal(ANode, ARecurse);
end;

procedure TAdvTreeViewData.ExpandVirtualNode(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
begin
  ExpandNodeInternal(ANode, ARecurse);
end;

procedure TAdvTreeViewData.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TAdvTreeViewData then
  begin
    FColumns.Assign((Source as TAdvTreeViewData).Columns);
    FGroups.Assign((Source as TAdvTreeViewData).Groups);
    FNodes.Assign((Source as TAdvTreeViewData).Nodes);
  end;
end;

procedure TAdvTreeViewData.ClearColumns;
begin
  BeginUpdate;
  Columns.Clear;
  EndUpdate;
end;

procedure TAdvTreeViewData.ClearNodeList;
begin
  BeginUpdate;
  NodeListBuild := False;
  EndUpdate;
end;

procedure TAdvTreeViewData.ClearNodes;
begin
  BeginUpdate;
  NodeListBuild := False;
  Nodes.Clear;
  EndUpdate;
end;

constructor TAdvTreeViewData.Create(AOwner: TComponent);
begin
  inherited;
  FNodeStructure := TAdvTreeViewNodeStructure.Create;
  FVisibleNodes := TAdvTreeViewVisibleNodes.Create;
  FDisplayGroups := TAdvTreeViewDisplayGroups.Create;

  FNodes := CreateNodes;
  FColumns := CreateColumns;
  FGroups := CreateGroups;
end;

function TAdvTreeViewData.CreateGroups: TAdvTreeViewGroups;
begin
  Result := TAdvTreeViewGroups.Create(Self);
end;

function TAdvTreeViewData.GetNodeFromNodeStructure(AIndex: Integer): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= NodeStructure.Count - 1) then
    Result := NodeStructure[AIndex];
end;

function TAdvTreeViewData.GetParentNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetParentVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetPreviousChildNode(
  ANode: TAdvTreeViewNode; AStartNode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) and Assigned(AStartNode) then
  begin
    v := GetPreviousChildVirtualNode(ANode.VirtualNode, AStartNode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetPreviousChildVirtualNode(
  ANode: TAdvTreeViewVirtualNode; AStartNode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := GetPreviousSiblingVirtualNode(AStartNode);
end;

function TAdvTreeViewData.GetPreviousNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetPreviousVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetPreviousSiblingNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetPreviousSiblingVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetPreviousSiblingVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  res: TAdvTreeViewVirtualNode;
begin
  res := ANode;
  repeat
    res := GetPreviousVirtualNode(res);
    Result := res;
  until (Result = nil) or (Assigned(Result) and (Result.ParentNode = ANode.ParentNode));
end;

function TAdvTreeViewData.GetPreviousVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
    Result := GetNodeFromNodeStructure(ANode.Row - 1);
end;

function TAdvTreeViewData.GetRootNodeByIndex(
  AIndex: Integer): TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := GetRootVirtualNodeByIndex(AIndex);
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvTreeViewData.GetRootVirtualNodeByIndex(
  AIndex: Integer): TAdvTreeViewVirtualNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to NodeStructure.Count - 1 do
  begin
    if NodeStructure[I].Index = AIndex then
    begin
      Result := NodeStructure[I];
      Break;
    end;
  end;
end;

procedure TAdvTreeViewData.InsertItemInternal(AItem: TAdvTreeViewNode);
var
  pn: TAdvTreeViewNode;
begin
  if (csDestroying in ComponentState) or not Assigned(AItem) or not NodeListBuild then
    Exit;

  pn := TAdvTreeViewNodes(AItem.Collection).Node;
  if Assigned(pn) then
    InsertVirtualNode(AItem.Index, pn.VirtualNode)
  else
    InsertVirtualNode(AItem.Index);
end;

function TAdvTreeViewData.RemoveItemInternal(AItem: TAdvTreeViewNode; AUpdate: Boolean = True): TAdvTreeViewVirtualNodeRemoveData;
begin
  Result.RowIndex := -1;
  Result.Count := 0;
  Result.ParentNode := -1;

  if (csDestroying in ComponentState) or not Assigned(AItem) or not NodeListBuild then
    Exit;

  Result := RemoveNodeInternal(AItem.VirtualNode, AUpdate);
end;

procedure TAdvTreeViewData.InsertNodeInternal(AParentNode: TAdvTreeViewVirtualNode; AIndex, AInsertIndex: Integer; ANode: TAdvTreeViewVirtualNode);
var
  I: Integer;
  pi: Integer;
  pv, rs: TAdvTreeViewVirtualNode;
  cnt: Integer;
  vi: Boolean;
  chk: Boolean;
  chkst: TAdvArrayBoolean;
  ins: Integer;
  exp, ext: Boolean;
  K: Integer;
begin
  if not NodeListBuild or (csDestroying in ComponentState) then
    Exit;

  if not Assigned(ANode) then
  begin
    ANode.Free;
    Exit;
  end;

  cnt := NodeStructure.Count;
  if Assigned(AParentNode) then
  begin
    pi := AParentNode.Row;
    if (pi >= 0) and (pi <= NodeStructure.Count - 1) then
    begin
      pv := GetNodeFromNodeStructure(pi);
      while Assigned(pv) do
      begin
        UpdateNodeTotalChildren(pv, pv.TotalChildren + 1);
        if pv.Row = AParentNode.Row then
          UpdateNodeChildren(pv, pv.Children + 1);

        pi := pv.ParentNode;
        pv := GetNodeFromNodeStructure(pi);
      end;
    end;
  end;

  UpdateNodeIndex(ANode, AIndex);
  UpdateNodeRow(ANode, AInsertIndex);

  if Assigned(AParentNode) then
  begin
    UpdateNodeParentNode(ANode, AParentNode.Row);
    UpdateNodeLevel(ANode, AParentNode.Level + 1);
  end
  else
  begin
    UpdateNodeParentNode(ANode, -1);
    UpdateNodeLevel(ANode, 0);
  end;

  UpdateNodeHeight(ANode, DefaultRowHeight);

  GetNodeForNodeData(ANode);

  exp := False;
  DoIsNodeExpanded(ANode, exp);
  UpdateNodeExpanded(ANode, exp);

  ext := False;
  DoIsNodeExtended(ANode, ext);
  UpdateNodeExtended(ANode, ext);

  SetLength(chkst, 0);
  for I := 0 to ColumnCount - 1 do
  begin
    chk := False;
    DoIsNodeChecked(ANode, I, chk);
    SetLength(chkst, Length(chkst) + 1);
    chkst[Length(chkst) - 1] := chk;
  end;

  UpdateNodeCheckStates(ANode, chkst);

  if (ANode.Row >= 0) and (ANode.Row < NodeStructure.Count) then
    NodeStructure.Insert(ANode.Row, ANode)
  else
    NodeStructure.Add(ANode);

  vi := True;
  if Assigned(AParentNode) then
    DoIsNodeVisible(AParentNode, vi);

  cnt := cnt - NodeStructure.Count;

  for I := ANode.Row + 1 to NodeStructure.Count - 1 do
  begin
    rs := GetNodeFromNodeStructure(I);
    if Assigned(rs) then
    begin
      UpdateNodeRow(rs, rs.Row - cnt);

      if rs.ParentNode > ANode.ParentNode then
        UpdateNodeParentNode(rs, rs.ParentNode - cnt);

      if rs.ParentNode = ANode.ParentNode then
        UpdateNodeIndex(rs, rs.Index + 1);

      GetNodeForNodeData(rs);

      exp := False;
      DoIsNodeExpanded(rs, exp);
      UpdateNodeExpanded(rs, exp);

      ext := False;
      DoIsNodeExtended(rs, ext);
      UpdateNodeExtended(rs, ext);

      SetLength(chkst, 0);
      for K := 0 to ColumnCount - 1 do
      begin
        chk := False;
        DoIsNodeChecked(RS, K, chk);
        SetLength(chkst, Length(chkst) + 1);
        chkst[Length(chkst) - 1] := chk;
      end;

      UpdateNodeCheckStates(rs, chkst);
    end;
  end;

  if not Assigned(AParentNode) or (Assigned(AParentNode) and AParentNode.Expanded and vi) then
  begin
    if not Assigned(AParentNode) then
    begin
      RowCount := RowCount + 1;
      TotalRowHeight := TotalRowHeight + DefaultRowHeight;
      ins := GetInsertPosition(ANode, True, True);
      if (ins >= 0) and (ins < VisibleNodes.Count) then
        VisibleNodes.Insert(ins, ANode)
      else
        VisibleNodes.Add(ANode);
    end
    else
    begin
      UpdateNodeExpanded(AParentNode, False);
      UpdateVisibleNodes(AParentNode);
      UpdateNodeExpanded(AParentNode, True);
      UpdateVisibleNodes(AParentNode);
    end;
  end;

  ResetNodes(False);
  UpdateTreeView;
  Invalidate;
end;

function TAdvTreeViewData.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvTreeViewNodes.Create(Self, nil);
end;

function TAdvTreeViewData.CreateColumns: TAdvTreeViewColumns;
begin
  Result := TAdvTreeViewColumns.Create(Self);
end;

procedure TAdvTreeViewData.RemoveNodeChildren(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    RemoveVirtualNodeChildren(ANode.VirtualNode);
end;

procedure TAdvTreeViewData.RemoveVirtualNodeChildren(ANode: TAdvTreeViewVirtualNode);
var
  I: Integer;
  s: TAdvTreeViewVirtualNode;
begin
  if Assigned(ANode) then
  begin
    for I := ANode.TotalChildren - 1 downto 0 do
    begin
      s := GetNodeFromNodeStructure(ANode.Row + I);
      if Assigned(s) then
        RemoveVirtualNode(s);    
    end;
  end;
end;

destructor TAdvTreeViewData.Destroy;
begin
  FDisplayGroups.Free;
  FNodeStructure.Free;
  FVisibleNodes.Free;
  FNodes.Free;
  FGroups.Free;
  FColumns.Free;
  inherited;
end;

procedure TAdvTreeViewData.DoGetColumnHorizontalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvTreeViewTextAlign);
begin

end;

procedure TAdvTreeViewData.DoGetColumnTrimming(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvTreeViewTextTrimming);
begin

end;

procedure TAdvTreeViewData.DoGetColumnVerticalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvTreeViewTextAlign);
begin

end;

procedure TAdvTreeViewData.DoGetColumnWordWrapping(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean);
begin

end;

procedure TAdvTreeViewData.DoGetNodeCheckType(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
  begin
     if (AColumn >= 0) and (AColumn <= it.Values.Count - 1) then
       ACheckType := it.Values[AColumn].CheckType;
  end;
end;

procedure TAdvTreeViewData.DoGetNodeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.DoGetNodeDisabledColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.DoGetNodeDisabledTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.BuildInternalNodeList(
  ANode: TAdvTreeViewVirtualNode; AID: String; AParentNode: Integer; ANodeIndex: Integer; ALevel: Integer; AParentExpanded: Boolean; var ATotalChildren: Integer);
var
  I: Integer;
  nd: Integer;
  vi: Boolean;
  v: TAdvTreeViewVirtualNode;
  exp: Boolean;
  tot: Integer;
  chk, ext: Boolean;
  chkst: TAdvArrayBoolean;
begin
  ANode.FRow := FNodeStructure.Count;
  ANode.FParentNode := AParentNode;
  ANode.FIndex := ANodeIndex;
  ANode.FLevel := ALevel;
  if AID <> 'ROOT' then
    GetNodeForNodeData(ANode);

  nd := 0;
  if (ALevel < MAXLEVELDEPTH) then
    DoGetNumberOfNodes(ANode, nd);

  ANode.FChildren := nd;

  ANode.FCalculated := False;
  ANode.FHeight := DefaultRowHeight;

  exp := False;
  if AID <> 'ROOT' then
    DoIsNodeExpanded(ANode, exp);
  ANode.FExpanded := exp;

  ext := False;
  DoIsNodeExtended(ANode, ext);
  UpdateNodeExtended(ANode, ext);

  SetLength(chkst, 0);
  if AID <> 'ROOT' then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      chk := False;
      DoIsNodeChecked(ANode, I, chk);
      SetLength(chkst, Length(chkst) + 1);
      chkst[Length(chkst) - 1] := chk;
    end;
    ANode.FCheckStates := chkst;
  end;

  vi := True;
  if AID <> 'ROOT' then
    DoIsNodeVisible(ANode, vi);

  tot := nd;

  if AID <> 'ROOT' then
  begin
    FNodeStructure.Add(ANode);
    if (AParentExpanded or (ANode.Level = 0)) and vi then
      FVisibleNodes.Add(ANode);
  end;

  for I := 0 to nd - 1 do
  begin
    if FNodeStructure.Count = MAXNODECOUNT then
      Break;

    v := TAdvTreeViewVirtualNode.Create(Self);
    if AID = 'ROOT' then
      BuildInternalNodeList(v, '', AParentNode, I, 0, exp, tot)
    else
      BuildInternalNodeList(v, '', ANode.Row, I, ANode.Level + 1, exp, tot);
  end;

  ATotalChildren := ATotalChildren + tot;

  if AID <> 'ROOT' then
    ANode.FTotalChildren := tot
  else
    ANode.Free;
end;

procedure TAdvTreeViewData.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FPictureContainer) and (AOperation = opRemove) then
    FPictureContainer := nil;
end;

procedure TAdvTreeViewData.BuildNodeList;
var
  v: TAdvTreeViewVirtualNode;
  c: Integer;
begin
  if NodeListBuild then
    Exit;

  FNodeStructure.Clear;
  FVisibleNodes.Clear;
  v := TAdvTreeViewVirtualNode.Create(Self);
  TotalRowHeight := 1;
  RowCount := 0;
  c := 0;
  BuildInternalNodeList(v, 'ROOT', -1, -1, -1, True, c);
  RowCount := FVisibleNodes.Count;
  NodeListBuild := FNodeStructure.Count > 0;
end;

function TAdvTreeViewData.GetNextChildNode(
  ANode: TAdvTreeViewNode; AStartNode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) and Assigned(AStartNode) then
  begin
    v := GetNextChildVirtualNode(ANode.VirtualNode, AStartNode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetNextChildVirtualNode(
  ANode: TAdvTreeViewVirtualNode; AStartNode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := GetNextSiblingVirtualNode(AStartNode);
end;

function TAdvTreeViewData.GetNextNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetNextVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetNextSiblingNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetNextSiblingVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetNextSiblingVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  res: TAdvTreeViewVirtualNode;
begin
  res := ANode;
  repeat
    res := GetNextVirtualNode(res);
    Result := res;
  until (Result = nil) or (Assigned(Result) and (Result.ParentNode = ANode.ParentNode));
end;

function TAdvTreeViewData.GetNextVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
    Result := GetNodeFromNodeStructure(ANode.Row + 1);
end;

function TAdvTreeViewData.GetNodeChildCount(ANode: TAdvTreeViewNode): Integer;
begin
  Result := 0;
  if Assigned(ANode) then
    Result := GetVirtualNodeChildCount(ANode.VirtualNode);
end;

function TAdvTreeViewData.GetFirstChildNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetFirstChildVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetFirstChildVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) and (ANode.Children > 0) then
    Result := GetNodeFromNodeStructure(ANode.Row + 1);
end;

function TAdvTreeViewData.GetFirstRootNode: TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := GetFirstRootVirtualNode;
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvTreeViewData.GetFirstRootVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if NodeStructure.Count > 0 then
    Result := NodeStructure[0];
end;

function TAdvTreeViewData.GetInsertPosition(
  ANode: TAdvTreeViewVirtualNode; ARoot: Boolean; AInsert: Boolean): Integer;
var
  prev: TAdvTreeViewVirtualNode;
  I: Integer;
  r: Integer;
  n: TAdvTreeViewVirtualNode;
begin
  Result := -1;
  if not Assigned(ANode) then
    Exit;

  if (ANode.Row = 0) then
  begin
    Result := 0;
    Exit;
  end;

  if AInsert then
    prev := GetNodeFromNodeStructure(ANode.Row - 1)
  else
    prev := GetNodeFromNodeStructure(ANode.Row);

  if ARoot then
  begin
    while Assigned(prev) and (prev.ParentNode >= 0) and (prev.ParentNode <= NodeStructure.Count - 1) do
    begin
      n := GetNodeFromNodeStructure(prev.ParentNode);
      if n <> prev then
        prev := n
      else
        Break;
    end;
  end;

  if Assigned(prev) then
  begin
    for I := prev.Index to VisibleNodes.Count - 1 do
    begin
      if AInsert then
        r := ANode.Row + 1
      else
        r := ANode.Row;

      if VisibleNodes[I].Row = r then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvTreeViewData.GetLastChildNode(
  ANode: TAdvTreeViewNode): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
  begin
    v := GetLastChildVirtualNode(ANode.VirtualNode);
    if Assigned(v) then
      Result := v.Node;
  end;
end;

function TAdvTreeViewData.GetLastChildVirtualNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := GetFirstChildVirtualNode(ANode);
  if Assigned(Result) then
  begin
    v := Result;
    repeat
      Result := v;
      v := GetNextSiblingVirtualNode(v);
    until v = nil;
  end;
end;

function TAdvTreeViewData.GetLastNode: TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := GetLastVirtualNode;
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvTreeViewData.GetLastRootNode: TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := GetLastRootVirtualNode;
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvTreeViewData.GetLastRootVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if NodeStructure.Count > 0 then
  begin
    Result := NodeStructure[NodeStructure.Count - 1];
    while Assigned(Result) and (Result.ParentNode > -1) do
      Result := NodeStructure[Result.Row - 1];
  end;
end;

function TAdvTreeViewData.GetLastVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if NodeStructure.Count > 0 then
    Result := NodeStructure[NodeStructure.Count - 1];
end;

procedure TAdvTreeViewData.GetNodeForNodeData(
  ANode: TAdvTreeViewVirtualNode);
var
  col: TAdvTreeViewNodes;
  p: TAdvTreeViewVirtualNode;
  it: TAdvTreeViewNode;
begin
  if Assigned(ANode) then
  begin
    p := GetNodeFromNodeStructure(ANode.ParentNode);
    if Assigned(p) and Assigned(p.Node) then
      col := TAdvTreeViewNodes(p.Node.Nodes)
    else
      col := Nodes;
  end
  else
    Exit;

  if Assigned(col) then
  begin
    if (ANode.Index >= 0) and (ANode.Index <= col.Count - 1) then
    begin
      it := col[ANode.Index];
      ANode.FNode := it;
      it.FVirtualNode := ANode;
    end;
  end;
end;

procedure TAdvTreeViewData.DoGetNodeHorizontalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvTreeViewTextAlign);
begin

end;

procedure TAdvTreeViewData.DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; ALarge: Boolean; var AIcon: TAdvTreeViewIconBitmap);
var
  it: TAdvTreeViewNode;
  v: TAdvTreeViewNodeValue;
begin
  it := ANode.Node;
  if Assigned(it) then
  begin
     if (AColumn >= 0) and (AColumn <= it.Values.Count - 1) then
     begin
       v := it.Values[AColumn];
       if ANode.Expanded then
       begin
         if ALarge then
         begin
           {$IFNDEF LCLLIB}
           if Assigned(PictureContainer) and (v.ExpandedIconLargeName <> '') then
             AIcon := PictureContainer.{$IFDEF FMXLIB}FindBitmap{$ENDIF}{$IFDEF VCLLIB}FindPicture{$ENDIF}(v.ExpandedIconLargeName);
           {$ENDIF}

           if not Assigned(AIcon) then
             AIcon := v.ExpandedIconLarge{$IFDEF VCLLIB}.Graphic{$ENDIF};
         end
         else
         begin
           {$IFNDEF LCLLIB}
           if Assigned(PictureContainer) and (v.ExpandedIconName <> '') then
             AIcon := PictureContainer.{$IFDEF FMXLIB}FindBitmap{$ENDIF}{$IFDEF VCLLIB}FindPicture{$ENDIF}(v.ExpandedIconName);
           {$ENDIF}

           if not Assigned(AIcon) then
             AIcon := v.ExpandedIcon{$IFDEF VCLLIB}.Graphic{$ENDIF};
         end;
       end
       else
       begin
         if ALarge then
         begin
           {$IFNDEF LCLLIB}
           if Assigned(PictureContainer) and (v.CollapsedIconLargeName <> '') then
             AIcon := PictureContainer.{$IFDEF FMXLIB}FindBitmap{$ENDIF}{$IFDEF VCLLIB}FindPicture{$ENDIF}(v.CollapsedIconLargeName);
           {$ENDIF}

           if not Assigned(AIcon) then
             AIcon := v.CollapsedIconLarge{$IFDEF VCLLIB}.Graphic{$ENDIF};
         end
         else
         begin
           {$IFNDEF LCLLIB}
           if Assigned(PictureContainer) and (v.CollapsedIconName <> '') then
             AIcon := PictureContainer.{$IFDEF FMXLIB}FindBitmap{$ENDIF}{$IFDEF VCLLIB}FindPicture{$ENDIF}(v.CollapsedIconName);
           {$ENDIF}

           if not Assigned(AIcon) then
             AIcon := v.CollapsedIcon{$IFDEF VCLLIB}.Graphic{$ENDIF};
         end;
       end;
     end;
  end;
end;

procedure TAdvTreeViewData.DoGetNodeSelectedColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.DoGetNodeSelectedTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.DoGetNodeText(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
  begin
     if (AColumn >= 0) and (AColumn <= it.Values.Count - 1) then
       AText := it.Values[AColumn].Text;
  end;
end;

procedure TAdvTreeViewData.DoGetNodeTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATextColor: TAdvTreeViewColor);
begin

end;

procedure TAdvTreeViewData.DoGetNodeTrimming(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; var ATrimming: TAdvTreeViewTextTrimming);
begin

end;

procedure TAdvTreeViewData.DoGetNodeVerticalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AVerticalTextAlign: TAdvTreeViewTextAlign);
begin

end;

procedure TAdvTreeViewData.DoGetNodeWordWrapping(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean);
begin

end;

procedure TAdvTreeViewData.DoGetNumberOfNodes(
  ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer);
begin
  if Assigned(ANode.Node) then
    ANumberOfNodes := ANode.Node.Nodes.Count
  else
    ANumberOfNodes := Nodes.Count;
end;

procedure TAdvTreeViewData.DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AChecked: Boolean);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
  begin
     if (AColumn >= 0) and (AColumn <= it.Values.Count - 1) then
       AChecked := it.Values[AColumn].Checked;
  end;
end;

procedure TAdvTreeViewData.CheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
var
  chkt: TAdvTreeViewNodeCheckType;
  pn, n: TAdvTreeViewVirtualNode;
  st, stp: Integer;
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  if not Assigned(ANode) then
    Exit;

  if (AColumn >= 0) and (AColumn <= Length(ANode.CheckStates) - 1) then
    ANode.CheckStates[AColumn] := True;

  if Assigned(ANode.Node) then
  begin
    if (AColumn >= 0) and (AColumn <= ANode.Node.Values.Count - 1) then
      ANode.Node.Values.UpdateChecked(AColumn, True);
  end;

  chkt := tvntNone;
  DoGetNodeCheckType(ANode, AColumn, chkt);
  if chkt = tvntRadioButton then
  begin
    pn := GetNodeFromNodeStructure(ANode.ParentNode);
    if Assigned(pn) then
    begin
      st := pn.Row + 1;
      stp := st + pn.TotalChildren;
    end
    else
    begin
      st := 0;
      stp := NodeStructure.Count;
    end;

    while st < stp do
    begin
      n := GetNodeFromNodeStructure(st);
      if Assigned(n) then
      begin
        if n <> ANode then
        begin
          chkt := tvntNone;
          DoGetNodeCheckType(n, AColumn, chkt);
          if chkt = tvntRadioButton then
          begin
            if (AColumn >= 0) and (AColumn <= Length(n.CheckStates) - 1) then
              n.CheckStates[AColumn] := False;

            if Assigned(n.Node) then
            begin
              if (AColumn >= 0) and (AColumn <= n.Node.Values.Count - 1) then
                n.Node.Values.UpdateChecked(AColumn, False);
            end;
          end;
        end;
        st := st + n.TotalChildren + 1;
      end
      else
        Break;
    end;
  end;

  if ARecurse then
  begin
    for I := ANode.Row + 1 to ANode.Row + ANode.TotalChildren do
    begin
      v := GetNodeFromNodeStructure(I);
      if Assigned(v) then
        CheckNodeInternal(v, AColumn, ARecurse);
    end;
  end;

  Invalidate;
end;

function TAdvTreeViewData.InsertVirtualNode(AIndex: Integer; AParentNode: TAdvTreeViewVirtualNode = nil): TAdvTreeViewVirtualNode;
var
  v: TAdvTreeViewVirtualNode;
  p, n, c: TAdvTreeViewVirtualNode;
  I, idx: Integer;
begin
  Result := nil;
  if (csDestroying in ComponentState) or not NodeListBuild then
    Exit;

  v := TAdvTreeViewVirtualNode.Create(Self);
  if Assigned(AParentNode) then
  begin
    idx := -1;
    I := AParentNode.Row + 1;
    while i <= AParentNode.Row + AParentNode.TotalChildren do
    begin
      n := GetNodeFromNodeStructure(I);
      if Assigned(n) then
      begin
        if (n.Index = AIndex) and (n.ParentNode = AParentNode.Row) then
        begin
          idx := n.Row;
          Break;
        end;
        I := I + n.Children + 1;
      end
      else
        Break;
    end;

    if idx = -1 then
      InsertNodeInternal(AParentNode, AIndex, AParentNode.Row + AParentNode.TotalChildren + 1, v)
    else
      InsertNodeInternal(AParentNode, AIndex, idx, v);
  end
  else
  begin
    p := nil;
    if NodeStructure.Count > 0 then
    begin
      p := GetNodeFromNodeStructure(NodeStructure.Count - 1);
      while Assigned(p) and (p.ParentNode >= 0) and (p.ParentNode <= NodeStructure.Count - 1) do
        p := GetNodeFromNodeStructure(p.ParentNode);
    end;

    if Assigned(p) then
    begin
      if AIndex > p.Index then
        InsertNodeInternal(nil, AIndex, p.Row + p.TotalChildren + 1, v)
      else
      begin
        n := nil;
        I := 0;
        while not Assigned(n) do
        begin
          c := GetNodeFromNodeStructure(I);
          if Assigned(c) then
          begin
            if c.Index = AIndex then
            begin
              n := c;
              Break;
            end
            else
              I := I + c.TotalChildren + 1;
          end
          else
            Break;
        end;

        if Assigned(n) then
          InsertNodeInternal(nil, AIndex, n.Row, v);
      end;
    end
    else
      InsertNodeInternal(nil, 0, 0, v);
  end;

  Result := v;
end;

function TAdvTreeViewData.GetVirtualNodeChildCount(
  ANode: TAdvTreeViewVirtualNode): Integer;
begin
  Result := 0;
  if Assigned(ANode) then
    Result := ANode.Children;
end;

function TAdvTreeViewData.GetParentVirtualNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(ANode) then
    Result := GetNodeFromNodeStructure(ANode.ParentNode);
end;

function TAdvTreeViewData.AddVirtualNode(AParentNode: TAdvTreeViewVirtualNode = nil): TAdvTreeViewVirtualNode;
var
  p: TAdvTreeViewVirtualNode;
  idx: Integer;
begin
  Result := nil;
  if (csDestroying in ComponentState) or not NodeListBuild then
    Exit;

  if Assigned(AParentNode) then
  begin
    idx := AParentNode.Children;
  end
  else
  begin
    p := nil;
    if NodeStructure.Count > 0 then
    begin
      p := GetNodeFromNodeStructure(NodeStructure.Count - 1);
      while Assigned(p) and (p.ParentNode >= 0) and (p.ParentNode <= NodeStructure.Count - 1) do
        p := GetNodeFromNodeStructure(p.ParentNode);
    end;

    idx := p.Index + 1;
  end;

  Result := InsertVirtualNode(idx, AParentNode);
end;

procedure TAdvTreeViewData.SetNodeTextValues(ANode: TAdvTreeViewNode; ATextValues: TAdvTreeViewNodeTextValues);
begin
  if Assigned(ANode) then
    ANode.SetTextValues(ATextValues);
end;

procedure TAdvTreeViewData.SetNodeExpandedIcons(ANode: TAdvTreeViewNode; AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False);
begin
  if Assigned(ANode) then
    ANode.SetExpandedIcons(AIcons, ALarge);
end;

procedure TAdvTreeViewData.SetNodeExpandedIconNames(ANode: TAdvTreeViewNode; AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False);
begin
  if Assigned(ANode) then
    ANode.SetExpandedIconNames(AIconNames, ALarge);
end;

procedure TAdvTreeViewData.SetNodeCollapsedIcons(ANode: TAdvTreeViewNode; AIcons: TAdvTreeViewNodeIcons; ALarge: Boolean = False);
begin
  if Assigned(ANode) then
    ANode.SetCollapsedIcons(AIcons, ALarge);
end;

procedure TAdvTreeViewData.SetNodeCollapsedIconNames(ANode: TAdvTreeViewNode; AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean = False);
begin
  if Assigned(ANode) then
    ANode.SetCollapsedIconNames(AIconNames, ALarge);
end;

procedure TAdvTreeViewData.SetNodeCheckTypes(ANode: TAdvTreeViewNode; ACheckTypes: TAdvTreeViewNodeCheckTypes);
begin
  if Assigned(ANode) then
    ANode.SetCheckTypes(ACheckTypes);
end;

procedure TAdvTreeViewData.SetNodeCheckStates(ANode: TAdvTreeViewNode; ACheckStates: TAdvTreeViewNodeCheckStates);
begin
  if Assigned(ANode) then
    ANode.SetCheckStates(ACheckStates);
end;

function TAdvTreeViewData.AddNode(AParentNode: TAdvTreeViewNode = nil): TAdvTreeViewNode;
begin
  if Assigned(AParentNode) then
    Result := AParentNode.Nodes.Add
  else
    Result := Nodes.Add;
end;

function TAdvTreeViewData.InsertNode(AIndex: Integer; AParentNode: TAdvTreeViewNode = nil): TAdvTreeViewNode;
begin
  if Assigned(AParentNode) then
    Result := AParentNode.Nodes.Insert(AIndex)
  else
    Result := Nodes.Insert(AIndex);
end;

procedure TAdvTreeViewData.RemoveNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    ANode.Free;
end;

function TAdvTreeViewData.RemoveNodeInternal(ANode: TAdvTreeViewVirtualNode; AUpdateNodes: Boolean = True): TAdvTreeViewVirtualNodeRemoveData;
var
  I: Integer;
  R, pi: Integer;
  subv, pv, rs: TAdvTreeViewVirtualNode;
  cnt: Integer;
  exp, ext: Boolean;
  chkst: TAdvArrayBoolean;
  K: Integer;
  chk: Boolean;
begin
  if (csDestroying in ComponentState) or not NodeListBuild or not Assigned(ANode) then
    Exit;

  cnt := NodeStructure.Count;
  pv := GetNodeFromNodeStructure(ANode.ParentNode);
  while Assigned(pv) do
  begin
    UpdateNodeTotalChildren(pv, pv.TotalChildren - 1 - ANode.TotalChildren);
    if pv.Row = ANode.ParentNode then
      UpdateNodeChildren(pv, pv.Children - 1);

    pi := pv.ParentNode;
    pv := GetNodeFromNodeStructure(pi);
  end;

  for I := ANode.TotalChildren - 1 downto 0 do
  begin
    R := I + ANode.Row + 1;
    subv := GetNodeFromNodeStructure(R);
    if Assigned(subv) then
    begin
      if VisibleNodes.Contains(subv) then
      begin
        VisibleNodes.Remove(subv);
        TotalRowHeight := TotalRowHeight - subv.Height;
        RowCount := RowCount - 1;
      end;
      NodeStructure.Remove(subv);
    end;
  end;

  R := ANode.Row;
  pi := ANode.ParentNode;
  if VisibleNodes.Contains(ANode) then
  begin
    VisibleNodes.Remove(ANode);
    TotalRowHeight := TotalRowHeight - ANode.Height;
    RowCount := RowCount - 1;
  end;

  UpdateNodeChildren(ANode, ANode.Children - 1);
  UpdateNodeTotalChildren(ANode, ANode.TotalChildren - 1);
  NodeStructure.Remove(ANode);

  cnt := cnt - NodeStructure.Count;
  Result.Count := cnt;
  Result.RowIndex := R;
  Result.ParentNode := pi;

  if AUpdateNodes then
  begin
    for I := R to NodeStructure.Count - 1 do
    begin
      rs := GetNodeFromNodeStructure(I);
      if Assigned(rs) then
      begin
        UpdateNodeRow(rs, rs.Row - cnt);

        if rs.ParentNode > pi then
          UpdateNodeParentNode(rs, rs.ParentNode - cnt);

        if rs.ParentNode = pi then
          UpdateNodeIndex(rs, rs.Index - 1);

        GetNodeForNodeData(rs);

        exp := False;
        DoIsNodeExpanded(rs, exp);
        UpdateNodeExpanded(rs, exp);

        ext := False;
        DoIsNodeExtended(rs, ext);
        UpdateNodeExtended(rs, ext);

        SetLength(chkst, 0);
        for K := 0 to ColumnCount - 1 do
        begin
          chk := False;
          DoIsNodeChecked(RS, K, chk);
          SetLength(chkst, Length(chkst) + 1);
          chkst[Length(chkst) - 1] := chk;
        end;

        UpdateNodeCheckStates(rs, chkst);
      end;
    end;

    if not FBlockUpdate then
    begin
      ResetNodes(False);
      UpdateTreeView;
      Invalidate;
    end;
  end;
end;

procedure TAdvTreeViewData.UpdateVisibleNodes(
  ANode: TAdvTreeViewVirtualNode);
var
  add: Boolean;
  R, I, C, idx: Integer;
  subv: TAdvTreeViewVirtualNode;
  en, vi, visub: Boolean;
begin
  add := ANode.Expanded;
  if add then
  begin
    idx := GetInsertPosition(ANode, ANode.ParentNode = -1, False);
    en := True;
    DoIsNodeEnabled(ANode, en);

    vi := True;
    DoIsNodeVisible(ANode, vi);

    if (idx <> -1) and vi and en then
    begin
      I := 0;
      C := idx + 1;
      while I < ANode.TotalChildren do
      begin
        R := I + ANode.Row + 1;
        subv := GetNodeFromNodeStructure(R);
        if Assigned(subv) then
        begin
          visub := True;
          DoIsNodeVisible(subv, visub);
          if visub then
          begin
            if (C >= 0) and (C < VisibleNodes.Count) then
              VisibleNodes.Insert(C, subv)
            else
              VisibleNodes.Add(subv);

            TotalRowHeight := TotalRowHeight + subv.Height;
            RowCount := RowCount + 1;
            Inc(C);
          end;

          if not subv.Expanded or not visub then
            I := I + subv.TotalChildren;
        end;
        Inc(I);
      end;
    end;
  end
  else
  begin
    for I := 0 to ANode.TotalChildren - 1 do
    begin
      R := I + ANode.Row + 1;
      subv := GetNodeFromNodeStructure(R);
      if Assigned(subv) then
      begin
        UpdateNodeCalculated(subv, False);
        if VisibleNodes.Contains(subv) then
        begin
          VisibleNodes.Remove(subv);
          TotalRowHeight := TotalRowHeight - subv.Height;
          RowCount := RowCount - 1;
        end;
      end;
    end;
  end;
end;

procedure TAdvTreeViewData.CollapseNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  if not Assigned(ANode) or not ANode.Expanded or not NodeListBuild or (csDestroying in ComponentState) then
    Exit;

  UpdateNodeExpanded(ANode, False);
  if ARecurse then
  begin
    for I := ANode.Row + 1 to ANode.Row + ANode.TotalChildren do
    begin
      v := GetNodeFromNodeStructure(I);
      if Assigned(v) then
        UpdateNodeExpanded(v, False);
    end;
  end;
  ResetNodes(False);
  UpdateVisibleNodes(ANode);
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvTreeViewData.DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode;
  var AEnabled: Boolean);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
    AEnabled := it.Enabled;
end;

procedure TAdvTreeViewData.DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode;
  var AExpanded: Boolean);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
    AExpanded := it.Expanded;
end;

procedure TAdvTreeViewData.DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode;
  var AExtended: Boolean);
var
  it: TAdvTreeViewNode;
begin
  it := ANode.Node;
  if Assigned(it) then
    AExtended := it.Extended;
end;

procedure TAdvTreeViewData.DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode;
  var AVisible: Boolean);
begin

end;

function TAdvTreeViewData.FindGroupByName(AName: String): TAdvTreeViewGroup;
var
  I: Integer;
  grp: TAdvTreeViewGroup;
begin
  Result := nil;
  for I := 0 to Groups.Count - 1 do
  begin
    grp := Groups[I];
    if grp.Name = AName then
    begin
      Result := grp;
      Break;
    end;
  end;
end;

function TAdvTreeViewData.FindGroupIndexByName(AName: String): Integer;
var
  grp: TAdvTreeViewGroup;
begin
  Result := -1;
  grp := FindGroupByName(AName);
  if Assigned(grp) then
    Result := grp.Index;
end;

function TAdvTreeViewData.IsColumnVisible(ACol: Integer): Boolean;
begin
  Result := inherited;
  if (ACol >= 0) and (ACol <= Columns.Count - 1) then
    Result := Columns[ACol].Visible;
end;

procedure TAdvTreeViewData.ToggleCheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
var
  chk: Boolean;
begin
  if (csDestroying in ComponentState) or not Assigned(ANode) or not NodeListBuild then
    Exit;

  if (AColumn >= 0) and (AColumn <= Length(ANode.CheckStates) - 1) then
  begin
    chk := ANode.CheckStates[AColumn];
    if chk then
      UnCheckNodeInternal(ANode, AColumn)
    else
      CheckNodeInternal(ANode, AColumn);
  end;
end;

procedure TAdvTreeViewData.ToggleNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
begin
  if (csDestroying in ComponentState) or not Assigned(ANode) or not NodeListBuild then
    Exit;

  if ANode.Expanded then
    CollapseNodeInternal(ANode, ARecurse)
  else
    ExpandNodeInternal(ANode, ARecurse);
end;

procedure TAdvTreeViewData.UnCheckNodeInternal(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ARecurse: Boolean = False);
var
  chkt: TAdvTreeViewNodeCheckType;
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  if (csDestroying in ComponentState) or not Assigned(ANode) or not NodeListBuild then
    Exit;

  chkt := tvntNone;
  DoGetNodeCheckType(ANode, AColumn, chkt);
  if chkt = tvntCheckBox then
  begin
    if (AColumn >= 0) and (AColumn <= Length(ANode.CheckStates) - 1) then
      ANode.CheckStates[AColumn] := False;

    if Assigned(ANode.Node) then
    begin
      if (AColumn >= 0) and (AColumn <= ANode.Node.Values.Count - 1) then
        ANode.Node.Values.UpdateChecked(AColumn, False);
    end;
  end;

  if ARecurse then
  begin
    for I := ANode.Row + 1 to ANode.Row + ANode.TotalChildren do
    begin
      v := GetNodeFromNodeStructure(I);
      if Assigned(v) then
        UnCheckNodeInternal(v, AColumn, ARecurse);
    end;
  end;

  Invalidate;
end;


procedure TAdvTreeViewData.UpdateNodeCalculated(ANode: TAdvTreeViewVirtualNode; ACalculated: Boolean);
begin
  ANode.FCalculated := ACalculated;
end;

function TAdvTreeViewData.FindColumnByName(
  AName: String): TAdvTreeViewColumn;
var
  I: Integer;
  res: TAdvTreeViewColumn;
begin
  Result := nil;
  for I := 0 to ColumnCount - 1 do
  begin
    if (I >= 0) and (I <= Columns.Count - 1) then
    begin
      res := Columns[I];
      if res.Name = AName then
      begin
        Result := res;
        Break;
      end;
    end;
  end;
end;

function TAdvTreeViewData.FindColumnIndexByName(AName: String): Integer;
var
  res: TAdvTreeViewColumn;
begin
  Result := -1;
  res := FindColumnByName(AName);
  if Assigned(res) then
    Result := res.Index;
end;

procedure TAdvTreeViewData.SetGroups(const Value: TAdvTreeViewGroups);
begin
  FGroups.Assign(Value);
end;

procedure TAdvTreeViewData.SetNodes(const Value: TAdvTreeViewNodes);
begin
  FNodes.Assign(Value);
end;

procedure TAdvTreeViewData.UpdateColumns;
var
  I: Integer;
  c: TAdvTreeViewColumn;
  g: TAdvTreeViewGroup;
  grp: TAdvTreeViewDisplayGroup;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  DisplayGroups.Clear;
  for I := 0 to Groups.Count - 1 do
  begin
    g := Groups[I];
    grp.StartColumn := Max(0, Min(g.StartColumn, Columns.Count - 1));
    grp.EndColumn := Max(0, Min(g.EndColumn, Columns.Count - 1));
    DisplayGroups.Add(grp);
  end;

  for I := 0 to Columns.Count - 1 do
  begin
    c := Columns[I];
    if c.Visible then
      ColumnWidths[I] := c.Width
    else
      ColumnWidths[I] := 0;
  end;
end;

procedure TAdvTreeViewData.UpdateNode(ANode: TAdvTreeViewVirtualNode);
begin
  if not Assigned(ANode) or (csDestroying in ComponentState) or not NodeListBuild then
    Exit;

  if BlockUpdateNode then
    Exit;

  ANode.FCalculated := False;

  if UpdateCount > 0 then
    Exit;

  UpdateNodesCache;
  Invalidate;
end;

procedure TAdvTreeViewData.UpdateNodeBitmapRects(
  ANode: TAdvTreeViewVirtualNode; ABitmapRects: TAdvArrayTRectF);
begin
  ANode.FBitmapRects := ABitmapRects;
end;

procedure TAdvTreeViewData.UpdateNodeCacheReference(
  ANode: TAdvTreeViewVirtualNode; ACache: TAdvTreeViewCacheItem);
begin
  ANode.FCache := ACache;
end;

procedure TAdvTreeViewData.UpdateNodeCheckRects(
  ANode: TAdvTreeViewVirtualNode; ACheckRects: TAdvArrayTRectF);
begin
  ANode.FCheckRects := ACheckRects;
end;

procedure TAdvTreeViewData.UpdateNodeCheckStates(
  ANode: TAdvTreeViewVirtualNode; ACheckStates: TAdvArrayBoolean);
begin
  ANode.FCheckStates := ACheckStates;
end;

procedure TAdvTreeViewData.UpdateNodeChildren(ANode: TAdvTreeViewVirtualNode;
  AChildren: Integer);
begin
  ANode.FChildren := AChildren;
end;

procedure TAdvTreeViewData.UpdateNodeExpanded(ANode: TAdvTreeViewVirtualNode;
  AExpanded: Boolean);
begin
  ANode.FExpanded := AExpanded;
  if Assigned(ANode.Node) then
    ANode.Node.FExpanded := AExpanded;
end;

procedure TAdvTreeViewData.UpdateNodeExpandRects(
  ANode: TAdvTreeViewVirtualNode; AExpandRects: TAdvArrayTRectF);
begin
  ANode.FExpandRects := AExpandRects;
end;

procedure TAdvTreeViewData.UpdateNodeExtended(
  ANode: TAdvTreeViewVirtualNode; AExtended: Boolean);
begin
  ANode.FExtended := AExtended;
end;

procedure TAdvTreeViewData.UpdateNodeHeight(ANode: TAdvTreeViewVirtualNode;
  AHeight: Double);
begin
  ANode.FHeight := AHeight;
end;

procedure TAdvTreeViewData.ExpandNodeInternal(ANode: TAdvTreeViewVirtualNode; ARecurse: Boolean = False);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  if (csDestroying in ComponentState) or not Assigned(ANode) or ANode.Expanded then
    Exit;

  if Assigned(ANode) then
    ANode.FExpanded := True;

  UpdateNodeExpanded(ANode, True);
  if ARecurse then
  begin
    for I := ANode.Row + 1 to ANode.Row + ANode.TotalChildren do
    begin
      v := GetNodeFromNodeStructure(I);
      if Assigned(v) then
        UpdateNodeExpanded(v, True);
    end;
  end;
  UpdateVisibleNodes(ANode);
  if not FBlockUpdate then
  begin
    ResetNodes(False);
    UpdateTreeView;
    Invalidate;
  end;
end;

procedure TAdvTreeViewData.UpdateNodeIndex(ANode: TAdvTreeViewVirtualNode;
  AIndex: Integer);
begin
  ANode.FIndex := AIndex;
end;

procedure TAdvTreeViewData.UpdateNodeLevel(ANode: TAdvTreeViewVirtualNode;
  ALevel: Integer);
begin
  ANode.FLevel := ALevel;
end;

procedure TAdvTreeViewData.UpdateNodeParentNode(
  ANode: TAdvTreeViewVirtualNode; AParentNode: Integer);
begin
  ANode.FParentNode := AParentNode;
end;

procedure TAdvTreeViewData.UpdateNodeRow(ANode: TAdvTreeViewVirtualNode;
  ARow: Integer);
begin
  ANode.FRow := ARow;
end;

procedure TAdvTreeViewData.UpdateNodesInternal(ANodeRemoveData: TAdvTreeViewVirtualNodeRemoveData);
var
  cnt: Integer;
  I, pi, R: Integer;
  rs: TAdvTreeViewVirtualNode;
  ext, exp: Boolean;
  chkst: TAdvArrayBoolean;
  K: Integer;
  chk: Boolean;
begin
  cnt := ANodeRemoveData.Count;
  pi := ANodeRemoveData.ParentNode;
  R := ANodeRemoveData.RowIndex;

  for I := R to NodeStructure.Count - 1 do
  begin
    rs := GetNodeFromNodeStructure(I);
    if Assigned(rs) then
    begin
      UpdateNodeRow(rs, rs.Row - cnt);

      if rs.ParentNode > pi then
        UpdateNodeParentNode(rs, rs.ParentNode - cnt);

      if rs.ParentNode = pi then
        UpdateNodeIndex(rs, rs.Index - 1);

      GetNodeForNodeData(rs);

      exp := False;
      DoIsNodeExpanded(rs, exp);
      UpdateNodeExpanded(rs, exp);

      ext := False;
      DoIsNodeExtended(rs, ext);
      UpdateNodeExtended(rs, ext);

      SetLength(chkst, 0);
      for K := 0 to ColumnCount - 1 do
      begin
        chk := False;
        DoIsNodeChecked(RS, K, chk);
        SetLength(chkst, Length(chkst) + 1);
        chkst[Length(chkst) - 1] := chk;
      end;

      UpdateNodeCheckStates(rs, chkst);
    end;
  end;

  if not FBlockUpdate then
  begin
    ResetNodes(False);
    UpdateTreeView;
    Invalidate;
  end;
end;

procedure TAdvTreeViewData.UpdateNodeTextRects(
  ANode: TAdvTreeViewVirtualNode; ATextRects: TAdvArrayTRectF);
begin
  ANode.FTextRects := ATextRects;
end;

procedure TAdvTreeViewData.UpdateNodeTotalChildren(
  ANode: TAdvTreeViewVirtualNode; ATotalChildren: Integer);
begin
  ANode.FTotalChildren := ATotalChildren;
end;

procedure TAdvTreeViewData.ScrollToNode(ANode: TAdvTreeViewNode; AScrollIfNotVisible: Boolean = False; AScrollPosition: TAdvTreeViewNodeScrollPosition = tvnspTop);
begin
  if Assigned(ANode) then
    ScrollToVirtualNode(ANode.VirtualNode, AScrollIfNotVisible, AScrollPosition);
end;

procedure TAdvTreeViewData.ScrollToVirtualNode(
  ANode: TAdvTreeViewVirtualNode; AScrollIfNotVisible: Boolean = False; AScrollPosition: TAdvTreeViewNodeScrollPosition = tvnspTop);
var
  I, C: Integer;
  yp, ps: Double;
  v: TAdvTreeViewVirtualNode;
begin
  yp := 0;
  c := 0;
  if Assigned(ANode) then
  begin
    for I := 0 to VisibleNodes.Count - 1 do
    begin
      v := VisibleNodes[I];
      if Assigned(v) then
      begin
        if v.Row = ANode.Row then
          Break;

        if v.Calculated then
          yp := yp + v.Height
        else
          Inc(c);
      end;
    end;

    yp := yp + DefaultRowHeight * c;
    ps := yp;
    case AScrollPosition of
      tvnspBottom: yp := yp - GetVViewPortSize + ANode.Height;
      tvnspMiddle: yp := yp - GetVViewPortSize / 2 + ANode.Height / 2;
    end;

    if AScrollIfNotVisible then
    begin
      if (ps < Abs(GetVScrollValue)) or (ps + ANode.Height > Abs(GetVScrollValue) + GetVViewPortSize) then
        Scroll(GetHScrollValue, yp);
    end
    else
    begin
      Scroll(GetHScrollValue, yp);
    end;
  end;
end;

procedure TAdvTreeViewData.SetColumns(const Value: TAdvTreeViewColumns);
begin
  FColumns.Assign(Value);
end;

{ TAdvTreeViewGroup }

procedure TAdvTreeViewGroup.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewGroup then
  begin
    FTag := (Source as TAdvTreeViewGroup).Tag;
    FText := (Source as TAdvTreeViewGroup).Text;
    FStartColumn := (Source as TAdvTreeViewGroup).StartColumn;
    FEndColumn := (Source as TAdvTreeViewGroup).EndColumn;
    FUseDefaultAppearance := (Source as TAdvTreeViewGroup).UseDefaultAppearance;
    FBottomFill.Assign((Source as TAdvTreeViewGroup).BottomFill);
    FBottomFontColor := (Source as TAdvTreeViewGroup).BottomFontColor;
    FBottomFont.Assign((Source as TAdvTreeViewGroup).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewGroup).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewGroup).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewGroup).TopStroke);
    FTopFontColor := (Source as TAdvTreeViewGroup).TopFontColor;
    FTopFont.Assign((Source as TAdvTreeViewGroup).TopFont);
  end;
end;

procedure TAdvTreeViewGroup.Changed(Sender: TObject);
begin
  UpdateGroup;
end;

constructor TAdvTreeViewGroup.Create(Collection: TCollection);
var
  grp: TAdvTreeViewGroup;
begin
  inherited;
  FTreeView := (Collection as TAdvTreeViewGroups).TreeView;
  FText := TranslateTextEx(sTMSFMXTreeViewGroup) + ' ' + inttostr(FTreeView.Groups.Count - 1);
  if FTreeView.Groups.Count < 2 then
  begin
    FStartColumn := 0;
    FEndColumn := 0
  end
  else
  begin
    grp := FTreeView.Groups[FTreeView.Groups.Count - 2];
    FStartColumn := grp.StartColumn + 1;
    FEndColumn := FStartColumn;
  end;

  FUseDefaultAppearance := True;

  FBottomFill := TAdvTreeViewBrush.CreateBrush(tvbkNone, TAdvTreeViewColorWhite);
  FBottomStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  FTopFill := TAdvTreeViewBrush.CreateBrush(tvbkNone, TAdvTreeViewColorWhite);
  FTopStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);

  FTopFontColor := TAdvTreeViewColorGray;
  FBottomFontColor := TAdvTreeViewColorGray;
  FTopFont := TFont.Create;
  FBottomFont := TFont.Create;

  FBottomFill.OnChanged := Changed;
  FTopFill.OnChanged := Changed;
  FBottomStroke.OnChanged := Changed;
  FTopStroke.OnChanged := Changed;

  {$IFDEF FMXLIB}
  FTopFont.OnChanged := Changed;
  FBottomFont.OnChanged := Changed;
  {$ENDIF}
  {$IFDEF VCLLIB}
  FTopFont.OnChange := Changed;
  FBottomFont.OnChange := Changed;
  {$ENDIF}

  UpdateGroup;
end;

destructor TAdvTreeViewGroup.Destroy;
begin
  FTopFill.Free;
  FTopStroke.Free;
  FTopFont.Free;
  FBottomFill.Free;
  FBottomStroke.Free;
  FBottomFont.Free;
  inherited;
  UpdateGroup;
end;

function TAdvTreeViewGroup.GetText: String;
begin
  Result  := Text;
  if Result = '' then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sTMSFMXTreeViewGroup) + ' ' + inttostr(Index);
  end;
end;

function TAdvTreeViewGroup.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewGroup.SetStartColumn(const Value: Integer);
begin
  if FStartColumn <> Value then
  begin
    FStartColumn := Value;
    UpdateGroup;
  end;
end;

procedure TAdvTreeViewGroup.SetEndColumn(const Value: Integer);
begin
  if FEndColumn <> Value then
  begin
    FEndColumn := Value;
    UpdateGroup;
  end;
end;

procedure TAdvTreeViewGroup.SetBottomFont(const Value: TFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetBottomStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetBottomFill(const Value: TAdvTreeViewBrush);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetBottomFontColor(const Value: TAdvTreeViewColor);
begin
  if FBottomFontColor <> Value then
  begin
    FBottomFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroup.SetTopFont(const Value: TFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetTopStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetUseDefaultAppearance(const Value: Boolean);
begin
  if FUseDefaultAppearance <> Value then
  begin
    FUseDefaultAppearance := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroup.SetTopFill(const Value: TAdvTreeViewBrush);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewGroup.SetTopFontColor(const Value: TAdvTreeViewColor);
begin
  if FTopFontColor <> Value then
  begin
    FTopFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroup.SetName(const Value: String);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

procedure TAdvTreeViewGroup.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateGroup;
  end;
end;

procedure TAdvTreeViewGroup.UpdateGroup;
var
  c: TAdvTreeViewData;
begin
  c := TreeView;
  if Assigned(c) then
  begin
    c.UpdateColumns;
    c.UpdateTreeViewCache;
  end;
end;

{ TAdvTreeViewGroups }

function TAdvTreeViewGroups.Add: TAdvTreeViewGroup;
begin
  Result := TAdvTreeViewGroup(inherited Add);
end;

constructor TAdvTreeViewGroups.Create(ATreeView: TAdvTreeViewData);
begin
  inherited Create(ATreeView, GetItemClass);
  FTreeView := ATreeView;
end;

function TAdvTreeViewGroups.GetItem(Index: Integer): TAdvTreeViewGroup;
begin
  Result := TAdvTreeViewGroup(inherited Items[Index]);
end;

function TAdvTreeViewGroups.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvTreeViewGroup;
end;

function TAdvTreeViewGroups.Insert(Index: Integer): TAdvTreeViewGroup;
begin
  Result := TAdvTreeViewGroup(inherited Insert(Index));
end;

function TAdvTreeViewGroups.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewGroups.SetItem(Index: Integer;
  const Value: TAdvTreeViewGroup);
begin
  inherited Items[Index] := Value;
end;

{ TAdvTreeViewNode }

procedure TAdvTreeViewNode.SetChecked(AColumn: integer;
  const Value: Boolean);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    v.Checked := Value;
end;

procedure TAdvTreeViewNode.SetCheckStates(
  ACheckStates: TAdvTreeViewNodeCheckStates);
var
  I: Integer;
begin
  for I := 0 to Length(ACheckStates) - 1 do
    Checked[I] := ACheckStates[I];
end;

procedure TAdvTreeViewNode.SetCheckType(AColumn: Integer;
  const Value: TAdvTreeViewNodeCheckType);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    v.CheckType := Value;
end;

procedure TAdvTreeViewNode.SetCheckTypes(
  ACheckTypes: TAdvTreeViewNodeCheckTypes);
var
  I: Integer;
begin
  for I := 0 to Length(ACheckTypes) - 1 do
    CheckTypes[I] := ACheckTypes[I];
end;

procedure TAdvTreeViewNode.SetCollapsedIconNames(
  AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(AIconNames) - 1 do
    CollapsedIconNames[I, ALarge] := AIconNames[I];
end;

procedure TAdvTreeViewNode.SetCollapsedIcons(AIcons: TAdvTreeViewNodeIcons;
  ALarge: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(AIcons) - 1 do
    CollapsedIcons[I, ALarge] := AIcons[I];
end;

procedure TAdvTreeViewNode.SetCollapsedIcon(AColumn: Integer;
  ALarge: Boolean; const Value: TAdvTreeViewBitmap);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      v.CollapsedIconLarge.Assign(Value)
    else
      v.CollapsedIcon.Assign(Value);
  end;
end;

procedure TAdvTreeViewNode.SetCollapsedIconName(AColumn: Integer;
  ALarge: Boolean; const Value: String);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      v.CollapsedIconLargeName := Value
    else
      v.CollapsedIconName := Value;
  end;
end;

procedure TAdvTreeViewNode.SetExpandedIconNames(
  AIconNames: TAdvTreeViewNodeIconNames; ALarge: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(AIconNames) - 1 do
    ExpandedIconNames[I, ALarge] := AIconNames[I];
end;

procedure TAdvTreeViewNode.SetExpandedIcons(AIcons: TAdvTreeViewNodeIcons;
  ALarge: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(AIcons) - 1 do
    ExpandedIcons[I, ALarge] := AIcons[I];
end;

procedure TAdvTreeViewNode.SetExpandedIcon(AColumn: Integer;
  ALarge: Boolean; const Value: TAdvTreeViewBitmap);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      v.ExpandedIconLarge.Assign(Value)
    else
      v.ExpandedIcon.Assign(Value);
  end;
end;

procedure TAdvTreeViewNode.SetExpandedIconName(AColumn: Integer;
  ALarge: Boolean; const Value: String);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      v.ExpandedIconLargeName := Value
    else
      v.ExpandedIconName := Value;
  end;
end;

procedure TAdvTreeViewNode.SetText(AColumn: Integer; const Value: String);
var
  v: TAdvTreeViewNodeValue;
begin
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    v.Text := Value;
end;

procedure TAdvTreeViewNode.SetTextValues(
  ATextValues: TAdvTreeViewNodeTextValues);
var
  I: Integer;
begin
  for I := 0 to Length(ATextValues) - 1 do
    Text[I] := ATextValues[I];
end;

procedure TAdvTreeViewNode.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewNode then
  begin
    FTag := (Source as TAdvTreeViewNode).Tag;
    FValues.Assign((Source as TAdvTreeViewNode).Values);
    FExpanded := (Source as TAdvTreeViewNode).Expanded;
    FExtended := (Source as TAdvTreeViewNode).Extended;
    FEnabled := (Source as TAdvTreeViewNode).Enabled;
    FNodes.Assign((Source as TAdvTreeViewNode).Nodes);
    if Assigned(FTreeView) then
    begin
      if FExpanded then
        FTreeView.ExpandNode(Self)
      else
        FTreeView.CollapseNode(Self);
    end;
  end;
end;

procedure TAdvTreeViewNode.Collapse(ARecurse: Boolean);
begin
  if Assigned(FTreeView) then
    FTreeView.CollapseNode(Self, ARecurse);
end;

constructor TAdvTreeViewNode.Create(Collection: TCollection);
begin
  inherited;
  FTreeView := (Collection as TAdvTreeViewNodes).TreeView;
  FValues := CreateNodeValues;

  FExpanded := False;
  FExtended := False;
  FEnabled := True;
  FNodes := CreateNodes;
  if Assigned(FTreeView) then
  begin
    if FTreeView.NodeListBuild then
      FTreeView.InsertItemInternal(Self)
    else
      FTreeView.UpdateTreeViewCache;
  end;
end;

function TAdvTreeViewNode.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvTreeViewNodes.Create(FTreeView, Self);
end;

function TAdvTreeViewNode.CreateNodeValues: TAdvTreeViewNodeValues;
begin
  Result := TAdvTreeViewNodeValues.Create(FTreeView, Self);
end;

destructor TAdvTreeViewNode.Destroy;
var
  ru: TAdvTreeViewVirtualNodeRemoveData;
begin
  ru.RowIndex := -1;
  ru.Count := 0;
  ru.ParentNode := -1;
  if Assigned(FTreeView) then
  begin
    if FTreeView.NodeListBuild and not (FTreeView.BlockRemoveNode > 0) then
      ru := FTreeView.RemoveItemInternal(Self, False);
  end;
  FVirtualNode := nil;

  if Assigned(FTreeView) then
    FTreeView.BlockRemoveNode := FTreeView.BlockRemoveNode + 1;
  FNodes.Free;
  if Assigned(FTreeView) then
    FTreeView.BlockRemoveNode := FTreeView.BlockRemoveNode - 1;
  FValues.Free;

  inherited;

  if (ru.RowIndex <> -1) then
    FTreeView.UpdateNodesInternal(ru);

  if Assigned(FTreeView) then
    FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewNode.Expand(ARecurse: Boolean);
begin
  if Assigned(FTreeView) then
    FTreeView.ExpandNode(Self, ARecurse);
end;

function TAdvTreeViewNode.GetChecked(AColumn: integer): Boolean;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := False;
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    Result := v.Checked;
end;

function TAdvTreeViewNode.GetCheckType(
  AColumn: Integer): TAdvTreeViewNodeCheckType;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := tvntNone;
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    Result := v.CheckType;
end;

function TAdvTreeViewNode.GetChildCount: Integer;
begin
  Result := 0;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNodeChildCount(Self);
end;

function TAdvTreeViewNode.GetCollapsedIcon(AColumn: Integer;
  ALarge: Boolean): TAdvTreeViewBitmap;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := nil;
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      Result := v.CollapsedIconLarge
    else
      Result := v.CollapsedIcon;
  end;
end;

function TAdvTreeViewNode.GetCollapsedIconName(AColumn: Integer;
  ALarge: Boolean): String;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := '';
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      Result := v.CollapsedIconLargeName
    else
      Result := v.CollapsedIconName;
  end;
end;

function TAdvTreeViewNode.GetExpandedIcon(AColumn: Integer;
  ALarge: Boolean): TAdvTreeViewBitmap;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := nil;
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      Result := v.ExpandedIconLarge
    else
      Result := v.ExpandedIcon;
  end;
end;

function TAdvTreeViewNode.GetExpandedIconName(AColumn: Integer;
  ALarge: Boolean): string;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := '';
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
  begin
    if ALarge then
      Result := v.ExpandedIconLargeName
    else
      Result := v.ExpandedIconName;
  end;
end;

function TAdvTreeViewNode.GetFirstChild: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetFirstChildNode(Self);
end;

function TAdvTreeViewNode.GetLastChild: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetLastChildNode(Self);
end;

function TAdvTreeViewNode.GetNext: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextNode(Self);
end;

function TAdvTreeViewNode.GetNextChild(ANode: TAdvTreeViewNode): TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextChildNode(Self, ANode);
end;

function TAdvTreeViewNode.GetNextSibling: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextSiblingNode(Self);
end;

function TAdvTreeViewNode.GetParent: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetParentNode(Self);
end;

function TAdvTreeViewNode.GetPrevious: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousNode(Self);
end;

function TAdvTreeViewNode.GetPreviousChild(ANode: TAdvTreeViewNode): TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousChildNode(Self, ANode);
end;

function TAdvTreeViewNode.GetPreviousSibling: TAdvTreeViewNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousSiblingNode(Self);
end;

function TAdvTreeViewNode.GetText(AColumn: Integer): String;
var
  v: TAdvTreeViewNodeValue;
begin
  Result := '';
  v := GetValueForColumn(AColumn);
  if Assigned(v) then
    Result := v.Text;
end;

function TAdvTreeViewNode.GetValueForColumn(
  AColumn: Integer): TAdvTreeViewNodeValue;
var
  I: Integer;
begin
  Result := nil;
  if (AColumn >= 0) and (AColumn <= Values.Count - 1) then
  begin
    Result := Values[AColumn];
    Exit;
  end;

  FTreeView.BlockUpdateNode := True;
  for I := 0 to AColumn do
  begin
    if I > Values.Count - 1 then
      Result := Values.Add;
  end;
  FTreeView.BlockUpdateNode := False;
end;

procedure TAdvTreeViewNode.RemoveChildren;
begin
  if Assigned(FTreeView) then
    FTreeView.RemoveNodeChildren(Self);
end;

function TAdvTreeViewNode.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewNode.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    UpdateNode;
  end;
end;

procedure TAdvTreeViewNode.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if Assigned(FTreeView) then
      FTreeView.ToggleNode(Self);
  end;
end;

procedure TAdvTreeViewNode.SetExtended(const Value: Boolean);
begin
  if FExtended <> Value then
  begin
    FExtended := Value;
    UpdateNode;
  end;
end;

procedure TAdvTreeViewNode.SetIndex(Value: Integer);

  procedure InsertSubNodes(ANodes: TAdvTreeViewNodes);
  var
    I: Integer;
  begin
    for I := 0 to ANodes.Count - 1 do
    begin
      FTreeView.InsertItemInternal(ANodes[I]);
      InsertSubNodes(ANodes[I].Nodes);
    end;
  end;

begin
  inherited;
  if Assigned(FTreeView) then
  begin
    if FTreeView.NodeListBuild then
    begin
      FTreeView.RemoveItemInternal(Self);
      FTreeView.InsertItemInternal(Self);
      InsertSubNodes(Nodes);
    end
    else
      FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewNode.SetNodes(const Value: TAdvTreeViewNodes);
begin
  if FNodes <> Value then
  begin
    FNodes.Assign(Value);
    UpdateNode;
  end;
end;

procedure TAdvTreeViewNode.SetValues(const Value: TAdvTreeViewNodeValues);
begin
  if FValues <> Value then
  begin
    FValues.Assign(Value);
    UpdateNode;
  end;
end;

procedure TAdvTreeViewNode.UpdateNode;
var
  c: TAdvTreeViewData;
begin
  c := TreeView;
  if Assigned(c) then
  begin
    if FTreeView.NodeListBuild then
      c.UpdateNode(VirtualNode)
    else
      c.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewNode.ValuesChanged(Sender: TObject);
begin
  UpdateNode;
end;

{ TAdvTreeViewNodes }

function TAdvTreeViewNodes.Add: TAdvTreeViewNode;
begin
  Result := TAdvTreeViewNode(inherited Add);
end;

constructor TAdvTreeViewNodes.Create(ATreeView: TAdvTreeViewData; ANode: TAdvTreeViewNode);
begin
  inherited Create(ATreeView, GetItemClass);
  FTreeView := ATreeView;
  FNode := ANode;
end;

destructor TAdvTreeViewNodes.Destroy;
begin
  FNode := nil;
  inherited;
end;

function TAdvTreeViewNodes.GetItem(Index: Integer): TAdvTreeViewNode;
begin
  Result := TAdvTreeViewNode(inherited Items[Index]);
end;

function TAdvTreeViewNodes.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvTreeViewNode;
end;

function TAdvTreeViewNodes.Insert(Index: Integer): TAdvTreeViewNode;
begin
  Result := TAdvTreeViewNode(inherited Insert(Index));
end;

function TAdvTreeViewNodes.Node: TAdvTreeViewNode;
begin
  Result := FNode;
end;

function TAdvTreeViewNodes.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewNodes.SetItem(Index: Integer;
  const Value: TAdvTreeViewNode);
begin
  inherited Items[Index] := Value;
end;

{ TAdvTreeViewColumn }

procedure TAdvTreeViewColumn.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewColumn then
  begin
    FTag := (Source as TAdvTreeViewColumn).Tag;
    FText := (Source as TAdvTreeViewColumn).Text;
    FName := (Source as TAdvTreeViewColumn).Name;
    FTrimming := (Source as TAdvTreeViewColumn).Trimming;
    FWordWrapping := (Source as TAdvTreeViewColumn).WordWrapping;
    FHorizontalTextAlign := (Source as TAdvTreeViewColumn).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvTreeViewColumn).VerticalTextAlign;
    FWidth := (Source as TAdvTreeViewColumn).Width;
    FVisible := (Source as TAdvTreeViewColumn).Visible;

    FUseDefaultAppearance := (Source as TAdvTreeViewColumn).UseDefaultAppearance;
    FFill.Assign((Source as TAdvTreeViewColumn).Fill);
    FFontColor := (Source as TAdvTreeViewColumn).FontColor;
    FDisabledFontColor := (Source as TAdvTreeViewColumn).DisabledFontColor;
    FSelectedFontColor := (Source as TAdvTreeViewColumn).SelectedFontColor;
    FFont.Assign((Source as TAdvTreeViewColumn).Font);
    FStroke.Assign((Source as TAdvTreeViewColumn).Stroke);

    FBottomFill.Assign((Source as TAdvTreeViewColumn).BottomFill);
    FBottomFontColor := (Source as TAdvTreeViewColumn).BottomFontColor;
    FBottomFont.Assign((Source as TAdvTreeViewColumn).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewColumn).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewColumn).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewColumn).TopStroke);
    FTopFontColor := (Source as TAdvTreeViewColumn).TopFontColor;
    FTopFont.Assign((Source as TAdvTreeViewColumn).TopFont);

    FEditorType := (Source as TAdvTreeViewColumn).EditorType;
    FEditorItems := (Source as TAdvTreeViewColumn).EditorItems;
    FCustomEditor := (Source as TAdvTreeViewColumn).CustomEditor;
  end;
end;

procedure TAdvTreeViewColumn.Changed(Sender: TObject);
begin
  UpdateColumn;
end;

constructor TAdvTreeViewColumn.Create(Collection: TCollection);
begin
  inherited;
  FTreeView := (Collection as TAdvTreeViewColumns).TreeView;
  FEditorType := tcetNone;
  FWordWrapping := False;
  FTrimming := tvttNone;
  FCustomEditor := False;
  FHorizontalTextAlign := tvtaLeading;
  FVerticalTextAlign := tvtaCenter;
  FEditorItems := TStringList.Create;
  FText := TranslateTextEx(sTMSFMXTreeViewColumn) + ' ' + inttostr(Collection.Count - 1);
  FName :=  StringReplace(FText, ' ', '', [rfReplaceAll]);
  FWidth := 100;
  FVisible := True;
  FUseDefaultAppearance := True;

  FBottomFill := TAdvTreeViewBrush.CreateBrush(tvbkNone, TAdvTreeViewColorWhite);
  FBottomStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  FTopFill := TAdvTreeViewBrush.CreateBrush(tvbkNone, TAdvTreeViewColorWhite);
  FTopStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);

  FFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorNull);
  FStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);

  FTopFontColor := TAdvTreeViewColorGray;
  FBottomFontColor := TAdvTreeViewColorGray;
  FFontColor := TAdvTreeViewColorGray;
  FSelectedFontColor := TAdvTreeViewColorWhite;
  FDisabledFontColor := TAdvTreeViewColorSilver;

  FFont := TFont.Create;
  FTopFont := TFont.Create;
  FBottomFont := TFont.Create;

  FBottomFill.OnChanged := Changed;
  FTopFill.OnChanged := Changed;
  FBottomStroke.OnChanged := Changed;
  FTopStroke.OnChanged := Changed;
  FStroke.OnChanged := Changed;
  FFill.OnChanged := Changed;

  {$IFDEF FMXLIB}
  FFont.OnChanged := Changed;
  FTopFont.OnChanged := Changed;
  FBottomFont.OnChanged := Changed;
  {$ENDIF}
  {$IFDEF VCLLIB}
  FFont.OnChange := Changed;
  FTopFont.OnChange := Changed;
  FBottomFont.OnChange := Changed;
  {$ENDIF}

  UpdateColumn;
end;

destructor TAdvTreeViewColumn.Destroy;
begin
  FEditorItems.Free;
  FTopFill.Free;
  FTopStroke.Free;
  FTopFont.Free;
  FBottomFill.Free;
  FBottomStroke.Free;
  FBottomFont.Free;
  FFill.Free;
  FStroke.Free;
  FFont.Free;
  inherited;
  UpdateColumn;
end;

function TAdvTreeViewColumn.GetColumnText: String;
begin
  Result := Text;
  if (Result = '') or (AnsiPos('</', Result) > 0) or (AnsiPos('/>', Result) > 0) or (AnsiPos('<BR>', UpperCase(Result)) > 0) then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sTMSFMXTreeViewColumn) + ' ' + inttostr(Index);
  end;
end;

function TAdvTreeViewColumn.GetText: String;
begin
  Result := Text;
  if Result = '' then
  begin
    Result := Name;
    if Result = '' then
      Result := TranslateTextEx(sTMSFMXTreeViewColumn) + ' ' + inttostr(Index);
  end;
end;

function TAdvTreeViewColumn.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewColumn.SetHorizontalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetBottomFont(const Value: TFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetBottomStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetDisabledFontColor(
  const Value: TAdvTreeViewColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumn.SetEditorItems(const Value: TStringList);
begin
  FEditorItems.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetEditorType(
  const Value: TAdvTreeViewColumnEditorType);
begin
  if FEditorType <> Value then
    FEditorType := Value;
end;

procedure TAdvTreeViewColumn.SetBottomFill(const Value: TAdvTreeViewBrush);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetBottomFontColor(const Value: TAdvTreeViewColor);
begin
  if FBottomFontColor <> Value then
  begin
    FBottomFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumn.SetFont(const Value: TFont);
begin
  if FFont <> Value then
    FFont.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetSelectedFontColor(
  const Value: TAdvTreeViewColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumn.SetStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FStroke <> Value then
    FStroke.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetFill(const Value: TAdvTreeViewBrush);
begin
  if FFill <> Value then
    FFill.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetFontColor(const Value: TAdvTreeViewColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumn.SetTopFont(const Value: TFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetTopStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetTopFill(const Value: TAdvTreeViewBrush);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewColumn.SetTopFontColor(const Value: TAdvTreeViewColor);
begin
  if FTopFontColor <> Value then
  begin
    FTopFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumn.SetName(const Value: String);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

procedure TAdvTreeViewColumn.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetTrimming(const Value: TAdvTreeViewTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetUseDefaultAppearance(const Value: Boolean);
begin
  if FUseDefaultAppearance <> Value then
  begin
    FUseDefaultAppearance := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetVerticalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetWidth(const Value: Double);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.SetWordWrapping(const Value: Boolean);
begin
  if FWordWrapping <> Value then
  begin
    FWordWrapping := Value;
    UpdateColumn;
  end;
end;

procedure TAdvTreeViewColumn.UpdateColumn;
var
  t: TAdvTreeViewData;
begin
  t := TreeView;
  if Assigned(t) then
  begin
    t.UpdateColumns;
    t.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumn.UpdateWidth(AWidth: Double);
begin
  FWidth := AWidth;
end;

{ TAdvTreeViewColumns }

function TAdvTreeViewColumns.Add: TAdvTreeViewColumn;
begin
  Result := TAdvTreeViewColumn(inherited Add);
end;

constructor TAdvTreeViewColumns.Create(ATreeView: TAdvTreeViewData);
begin
  inherited Create(ATreeView, GetItemClass);
  FTreeView := ATreeView;
end;

function TAdvTreeViewColumns.GetItem(Index: Integer): TAdvTreeViewColumn;
begin
  Result := TAdvTreeViewColumn(inherited Items[Index]);
end;

function TAdvTreeViewColumns.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvTreeViewColumn;
end;

function TAdvTreeViewColumns.Insert(Index: Integer): TAdvTreeViewColumn;
begin
  Result := TAdvTreeViewColumn(inherited Insert(Index));
end;

function TAdvTreeViewColumns.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewColumns.SetItem(Index: Integer;
  const Value: TAdvTreeViewColumn);
begin
  inherited Items[Index] := Value;
end;

{ TAdvTreeViewNodeValue }

function TAdvTreeViewNodeValue.GetPictureContainer: TPictureContainer;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.PictureContainer;
end;

procedure TAdvTreeViewNodeValue.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewNodeValue then
  begin
    FTag := (Source as TAdvTreeViewNodeValue).Tag;
    FText := (Source as TAdvTreeViewNodeValue).Text;
    FCheckType := (Source as TAdvTreeViewNodeValue).CheckType;
    FChecked := (Source as TAdvTreeViewNodeValue).Checked;
    FCollapsedIcon.Assign((Source as TAdvTreeViewNodeValue).CollapsedIcon);
    FCollapsedIconLarge.Assign((Source as TAdvTreeViewNodeValue).CollapsedIconLarge);
    FExpandedIcon.Assign((Source as TAdvTreeViewNodeValue).ExpandedIcon);
    FExpandedIconLarge.Assign((Source as TAdvTreeViewNodeValue).ExpandedIconLarge);
    FCollapsedIconName := (Source as TAdvTreeViewNodeValue).CollapsedIconName;
    FCollapsedIconLargeName := (Source as TAdvTreeViewNodeValue).CollapsedIconLargeName;
    FExpandedIconName := (Source as TAdvTreeViewNodeValue).ExpandedIconName;
    FExpandedIconLargeName := (Source as TAdvTreeViewNodeValue).ExpandedIconLargeName;
  end;
end;

procedure TAdvTreeViewNodeValue.BitmapChanged(Sender: TObject);
begin
  UpdateNodeValue;
end;

constructor TAdvTreeViewNodeValue.Create(Collection: TCollection);
begin
  inherited;
  FTreeView := (Collection as TAdvTreeViewNodeValues).TreeView;
  FNode := (Collection as TAdvTreeViewNodeValues).Node;
  FChecked := False;
  FCheckType := tvntNone;
  FCollapsedIcon := TAdvTreeViewBitmap.Create;
  FCollapsedIcon.OnChange := BitmapChanged;
  FExpandedIcon := TAdvTreeViewBitmap.Create;
  FExpandedIcon.OnChange := BitmapChanged;

  FCollapsedIconLarge := TAdvTreeViewBitmap.Create;
  FCollapsedIconLarge.OnChange := BitmapChanged;
  FExpandedIconLarge := TAdvTreeViewBitmap.Create;
  FExpandedIconLarge.OnChange := BitmapChanged;

  UpdateNodeValue;
end;

destructor TAdvTreeViewNodeValue.Destroy;
begin
  FCollapsedIcon.Free;
  FCollapsedIconLarge.Free;
  FExpandedIcon.Free;
  FExpandedIconLarge.Free;
  inherited;
  UpdateNodeValue;
end;

function TAdvTreeViewNodeValue.Node: TAdvTreeViewNode;
begin
  Result := FNode;
end;

function TAdvTreeViewNodeValue.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewNodeValue.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetCheckType(
  const Value: TAdvTreeViewNodeCheckType);
begin
  if FCheckType <> Value then
  begin
    FCheckType := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetCollapsedIconName(const Value: String);
begin
  if FCollapsedIconName <> Value then
  begin
    FCollapsedIconName := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetCollapsedIconLargeName(const Value: String);
begin
  if FCollapsedIconLargeName <> Value then
  begin
    FCollapsedIconLargeName := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetExpandedIconName(const Value: String);
begin
  if FExpandedIconName <> Value then
  begin
    FExpandedIconName := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetExpandedIconLargeName(const Value: String);
begin
  if FExpandedIconLargeName <> Value then
  begin
    FExpandedIconLargeName := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetCollapsedIcon(const Value: TAdvTreeViewBitmap);
begin
  if FCollapsedIcon <> Value then
  begin
    FCollapsedIcon.Assign(Value);
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetCollapsedIconLarge(const Value: TAdvTreeViewBitmap);
begin
  if FCollapsedIconLarge <> Value then
  begin
    FCollapsedIconLarge.Assign(Value);
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetExpandedIcon(const Value: TAdvTreeViewBitmap);
begin
  if FExpandedIcon <> Value then
  begin
    FExpandedIcon.Assign(Value);
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetExpandedIconLarge(const Value: TAdvTreeViewBitmap);
begin
  if FExpandedIconLarge <> Value then
  begin
    FExpandedIconLarge.Assign(Value);
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateNodeValue;
  end;
end;

procedure TAdvTreeViewNodeValue.UpdateNodeValue;
var
  c: TAdvTreeViewData;
  n: TAdvTreeViewNode;
begin
  c := TreeView;
  n := Node;
  if Assigned(c) and Assigned(n) then
    c.UpdateNode(n.VirtualNode);
end;

{ TAdvTreeViewNodeValues }

function TAdvTreeViewNodeValues.Add: TAdvTreeViewNodeValue;
begin
  Result := TAdvTreeViewNodeValue(inherited Add);
end;

constructor TAdvTreeViewNodeValues.Create(ATreeView: TAdvTreeViewData; ANode: TAdvTreeViewNode);
begin
  inherited Create(ATreeView, GetItemClass);
  FTreeView := ATreeView;
  FNode := ANode;
end;

function TAdvTreeViewNodeValues.GetItem(Index: Integer): TAdvTreeViewNodeValue;
begin
  Result := TAdvTreeViewNodeValue(inherited Items[Index]);
end;

function TAdvTreeViewNodeValues.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvTreeViewNodeValue;
end;

function TAdvTreeViewNodeValues.Insert(Index: Integer): TAdvTreeViewNodeValue;
begin
  Result := TAdvTreeViewNodeValue(inherited Insert(Index));
end;

function TAdvTreeViewNodeValues.Node: TAdvTreeViewNode;
begin
  Result := FNode;
end;

function TAdvTreeViewNodeValues.TreeView: TAdvTreeViewData;
begin
  Result := FTreeView;
end;

procedure TAdvTreeViewNodeValues.UpdateChecked(AIndex: Integer;
  AValue: Boolean);
begin
  if Assigned(FTreeView) then
    FTreeView.UpdateCount := FTreeView.UpdateCount + 1;
  Items[AIndex].Checked := AValue;
  if Assigned(FTreeView) then
    FTreeView.UpdateCount := FTreeView.UpdateCount - 1;
end;

procedure TAdvTreeViewNodeValues.SetItem(Index: Integer;
  const Value: TAdvTreeViewNodeValue);
begin
  inherited Items[Index] := Value;
end;

{ TAdvTreeViewCacheItem }

class function TAdvTreeViewCacheItem.CreateGroupBottom(ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem.Create;
  Result.Kind := ikGroupBottom;
  Result.Group := AGroup;
  Result.StartColumn := AStartColumn;
  Result.EndColumn := AEndColumn;
  Result.Rect := ARect;
end;

class function TAdvTreeViewCacheItem.CreateGroupTop(
  ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem.Create;
  Result.Kind := ikGroupTop;
  Result.Group := AGroup;
  Result.StartColumn := AStartColumn;
  Result.EndColumn := AEndColumn;
  Result.Rect := ARect;
end;

class function TAdvTreeViewCacheItem.CreateNode(ARect: TRectF; ANode: TAdvTreeViewVirtualNode): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem.Create;
  Result.Kind := ikNode;
  Result.Rect := ARect;
  Result.Node := ANode;
end;

class function TAdvTreeViewCacheItem.CreateColumnTop(ARect: TRectF; AColumn: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem.Create;
  Result.Kind := ikColumnTop;
  Result.Column := AColumn;
  Result.Rect := ARect;
end;

class function TAdvTreeViewCacheItem.CreateColumnBottom(ARect: TRectF; AColumn: Integer): TAdvTreeViewCacheItem;
begin
  Result := TAdvTreeViewCacheItem.Create;
  Result.Kind := ikColumnBottom;
  Result.Column := AColumn;
  Result.Rect := ARect;
end;

destructor TAdvTreeViewCacheItem.Destroy;
begin
  FNode := nil;
  inherited;
end;

{ TAdvTreeViewVirtualNode }

procedure TAdvTreeViewVirtualNode.Collapse(ARecurse: Boolean = False);
begin
  if Assigned(FTreeView) then
    FTreeView.CollapseVirtualNode(Self, ARecurse);
end;

constructor TAdvTreeViewVirtualNode.Create(ATreeView: TAdvTreeViewData);
begin
  FTreeView := ATreeView;
end;

destructor TAdvTreeViewVirtualNode.Destroy;
begin
  FCache := nil;
  FNode := nil;
  inherited;
end;

procedure TAdvTreeViewVirtualNode.Expand(ARecurse: Boolean = False);
begin
  if Assigned(FTreeView) then
    FTreeView.ExpandVirtualNode(Self, ARecurse);
end;

function TAdvTreeViewVirtualNode.GetChildCount: Integer;
begin
  Result := 0;
  if Assigned(FTreeView) then
    Result := FTreeView.GetVirtualNodeChildCount(Self);
end;

function TAdvTreeViewVirtualNode.GetFirstChild: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetFirstChildVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetLastChild: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetLastChildVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetNext: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetNextChild(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextChildVirtualNode(Self, ANode);
end;

function TAdvTreeViewVirtualNode.GetNextSibling: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetNextSiblingVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetParent: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetParentVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetPrevious: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousVirtualNode(Self);
end;

function TAdvTreeViewVirtualNode.GetPreviousChild(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousChildVirtualNode(Self, ANode);
end;

function TAdvTreeViewVirtualNode.GetPreviousSibling: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if Assigned(FTreeView) then
    Result := FTreeView.GetPreviousSiblingVirtualNode(Self);
end;

procedure TAdvTreeViewVirtualNode.RemoveChildren;
begin
  if Assigned(FTreeView) then
    FTreeView.RemoveVirtualNodeChildren(Self);
end;

end.
