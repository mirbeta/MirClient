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

unit AdvCustomTreeView;

{$I TMSDEFS.INC}

{$IFDEF LCLLIB}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$IFNDEF LCLLIB}
{$IFDEF VCLLIB}
{$IFDEF DELPHIXE2_LVL}
{$DEFINE USEUITYPES}
{$ELSE}
{$DEFINE USEOLDERVCL}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE USEUITYPES}
{$ENDIF}

uses
  {$IFDEF VCLLIB}
  Windows,
  {$IFNDEF LCLLIB}
  PNGImage,
  {$ENDIF}
  {$ENDIF}
  Classes, AdvTreeViewData, AdvTreeViewBase, {$IFNDEF LCLLIB}Generics.Collections, {$ELSE} fgl,{$ENDIF}Types
  {$IFDEF VCLLIB}
  ,Graphics, ExtCtrls, Controls
  {$ENDIF}
  {$IFDEF USEUITYPES}
  , UITypes
  {$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Graphics, UIConsts, FMX.Controls, FMX.Types
  {$ENDIF}
  ;

const
  CACHEWIDTH = 1000;
  CACHEHEIGHT = 1000;
  {$IFDEF ANDROID}
  SCROLLINGDELAY = 40;
  {$ELSE}
  SCROLLINGDELAY = 0;
  {$ENDIF}

  {$IFDEF FMXLIB}
  KEY_ESCAPE = VKESCAPE;
  KEY_TAB = VKTAB;
  KEY_F2 = VKF2;
  KEY_PRIOR = VKPRIOR;
  KEY_NEXT = VKNEXT;
  KEY_UP = VKUP;
  KEY_DOWN = VKDOWN;
  KEY_RIGHT = VKRIGHT;
  KEY_LEFT = VKLEFT;
  KEY_HOME = VKHOME;
  KEY_END = VKEND;
  KEY_RETURN = VKRETURN;
  KEY_SPACE = VKSPACE;
  {$ENDIF}
  {$IFDEF VCLLIB}
  KEY_ESCAPE = VK_ESCAPE;
  KEY_TAB = VK_TAB;
  KEY_F2 = VK_F2;
  KEY_PRIOR = VK_PRIOR;
  KEY_NEXT = VK_NEXT;
  KEY_UP = VK_UP;
  KEY_DOWN = VK_DOWN;
  KEY_RIGHT = VK_RIGHT;
  KEY_LEFT = VK_LEFT;
  KEY_HOME = VK_HOME;
  KEY_END = VK_END;
  KEY_RETURN = VK_RETURN;
  KEY_SPACE = VK_SPACE;
  {$ENDIF}

type
  TAdvCustomTreeView = class;

  TAdvTreeViewCache = class(TObjectList<TAdvTreeViewCacheItem>);
  TAdvTreeViewNodeCache = class(TAdvTreeViewCache);
  TAdvTreeViewColumnsCache = class(TAdvTreeViewCache);
  TAdvTreeViewColumnsTopCache = class(TAdvTreeViewColumnsCache);
  TAdvTreeViewColumnsBottomCache = class(TAdvTreeViewColumnsCache);
  TAdvTreeViewGroupsCache = class(TAdvTreeViewCache);
  TAdvTreeViewGroupsTopCache = class(TAdvTreeViewGroupsCache);
  TAdvTreeViewGroupsBottomCache = class(TAdvTreeViewGroupsCache);

  TAdvTreeViewDisplayList = class(TList<TAdvTreeViewCacheItem>);
  TAdvTreeViewNodeDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewColumnsDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewColumnsTopDisplayList = class(TAdvTreeViewColumnsDisplayList);
  TAdvTreeViewColumnsBottomDisplayList = class(TAdvTreeViewColumnsDisplayList);
  TAdvTreeViewGroupsDisplayList = class(TAdvTreeViewDisplayList);
  TAdvTreeViewGroupsTopDisplayList = class(TAdvTreeViewGroupsDisplayList);
  TAdvTreeViewGroupsBottomDisplayList = class(TAdvTreeViewGroupsDisplayList);

  TAdvTreeViewColumnsLayout = (tclTop, tclBottom);
  TAdvTreeViewColumnsLayouts = set of TAdvTreeViewColumnsLayout;

  TAdvTreeViewGroupLayout = (tglTop, tglBottom);
  TAdvTreeViewGroupsLayouts = set of TAdvTreeViewGroupLayout;

  TAdvTreeViewNodeHeightMode = (tnhmFixed, tnhmVariable);

  TAdvTreeViewSelectionArea = (tsaFull, tsaFromLevel, tsaFromText);

  TAdvTreeViewNodesAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FFill: TAdvTreeViewBrush;
    FStroke: TAdvTreeViewStrokeBrush;
    FFont: TFont;
    FDisabledFill: TAdvTreeViewBrush;
    FSelectedFill: TAdvTreeViewBrush;
    FDisabledStroke: TAdvTreeViewStrokeBrush;
    FSelectedStroke: TAdvTreeViewStrokeBrush;
    FSelectionArea: TAdvTreeViewSelectionArea;
    FTextHorizontalTextAlign: TAdvTreeViewTextAlign;
    FTextVerticalTextAlign: TAdvTreeViewTextAlign;
    FExpandColumn: Integer;
    FExpandWidth: Double;
    FExpandHeight: Double;
    FLevelIndent: Double;
    FHeightMode: TAdvTreeViewNodeHeightMode;
    FFixedHeight: Double;
    FVariableMinimumHeight: Double;
    FFontColor: TAdvTreeViewColor;
    FSelectedFontColor: TAdvTreeViewColor;
    FDisabledFontColor: TAdvTreeViewColor;
    FExtendedFontColor: TAdvTreeViewColor;
    FExtendedSelectedFontColor: TAdvTreeViewColor;
    FExtendedDisabledFontColor: TAdvTreeViewColor;
    FExtendedDisabledFill: TAdvTreeViewBrush;
    FExtendedFont: TFont;
    FExtendedSelectedFill: TAdvTreeViewBrush;
    FExtendedDisabledStroke: TAdvTreeViewStrokeBrush;
    FExtendedSelectedStroke: TAdvTreeViewStrokeBrush;
    FExtendedFill: TAdvTreeViewBrush;
    FExtendedStroke: TAdvTreeViewStrokeBrush;
    FShowLines: Boolean;
    FExpandNodeIcon: TAdvTreeViewBitmap;
    FCollapseNodeIcon: TAdvTreeViewBitmap;
    FExpandNodeIconLarge: TAdvTreeViewBitmap;
    FCollapseNodeIconLarge: TAdvTreeViewBitmap;
    FColumnStroke: TAdvTreeViewStrokeBrush;
    FLineStroke: TAdvTreeViewStrokeBrush;
    FShowFocus: Boolean;
    procedure SetFill(const Value: TAdvTreeViewBrush);
    procedure SetStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetFont(const Value: TFont);
    procedure SetDisabledFill(const Value: TAdvTreeViewBrush);
    procedure SetDisabledStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetSelectedFill(const Value: TAdvTreeViewBrush);
    procedure SetSelectedStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetSelectionArea(const Value: TAdvTreeViewSelectionArea);
    procedure SetExtendedFill(const Value: TAdvTreeViewBrush);
    procedure SetExtendedStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetExtendedFont(const Value: TFont);
    procedure SetExtendedDisabledFill(const Value: TAdvTreeViewBrush);
    procedure SetExtendedDisabledStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetExtendedSelectedFill(const Value: TAdvTreeViewBrush);
    procedure SetExtendedSelectedStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetExpandColumn(const Value: Integer);
    procedure SetExpandWidth(const Value: Double);
    procedure SetExpandHeight(const Value: Double);
    procedure SetLevelIndent(const Value: Double);
    procedure SetFixedHeight(const Value: Double);
    procedure SetVariableMinimumHeight(const Value: Double);
    procedure SetFontColor(const Value: TAdvTreeViewColor);
    procedure SetSelectedFontColor(const Value: TAdvTreeViewColor);
    procedure SetDisabledFontColor(const Value: TAdvTreeViewColor);
    procedure SetExtendedFontColor(const Value: TAdvTreeViewColor);
    procedure SetExtendedSelectedFontColor(const Value: TAdvTreeViewColor);
    procedure SetExtendedDisabledFontColor(const Value: TAdvTreeViewColor);
    procedure SetHeightMode(const Value: TAdvTreeViewNodeHeightMode);
    procedure SetShowLines(const Value: Boolean);
    procedure SetCollapseNodeIcon(const Value: TAdvTreeViewBitmap);
    procedure SetExpandNodeIcon(const Value: TAdvTreeViewBitmap);
    procedure SetCollapseNodeIconLarge(const Value: TAdvTreeViewBitmap);
    procedure SetExpandNodeIconLarge(const Value: TAdvTreeViewBitmap);
    procedure SetColumnStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetLineStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetShowFocus(const Value: Boolean);
  protected
    procedure Changed(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property ExpandColumn: Integer read FExpandColumn write SetExpandColumn default 0;
    property ExpandWidth: Double read FExpandWidth write SetExpandWidth;
    property ExpandHeight: Double read FExpandHeight write SetExpandHeight;
    property LevelIndent: Double read FLevelIndent write SetLevelIndent;
    property FixedHeight: Double read FFixedHeight write SetFixedHeight;
    property VariableMinimumHeight: Double read FVariableMinimumHeight write SetVariableMinimumHeight;
    property HeightMode: TAdvTreeViewNodeHeightMode read FHeightMode write SetHeightMode default tnhmFixed;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;

    property Fill: TAdvTreeViewBrush read FFill write SetFill;
    property Stroke: TAdvTreeViewStrokeBrush read FStroke write SetStroke;
    property ColumnStroke: TAdvTreeViewStrokeBrush read FColumnStroke write SetColumnStroke;
    property LineStroke: TAdvTreeViewStrokeBrush read FLineStroke write SetLineStroke;
    property Font: TFont read FFont write SetFont;
    property FontColor: TAdvTreeViewColor read FFontColor write SetFontColor default TAdvTreeViewColorGray;
    property SelectedFontColor: TAdvTreeViewColor read FSelectedFontColor write SetSelectedFontColor default TAdvTreeViewColorWhite;
    property DisabledFontColor: TAdvTreeViewColor read FDisabledFontColor write SetDisabledFontColor default TAdvTreeViewColorSilver;

    property ExtendedFontColor: TAdvTreeViewColor read FExtendedFontColor write SetExtendedFontColor default TAdvTreeViewColorGray;
    property ExtendedSelectedFontColor: TAdvTreeViewColor read FExtendedSelectedFontColor write SetExtendedSelectedFontColor default TAdvTreeViewColorWhite;
    property ExtendedDisabledFontColor: TAdvTreeViewColor read FExtendedDisabledFontColor write SetExtendedDisabledFontColor default TAdvTreeViewColorSilver;

    property SelectedFill: TAdvTreeViewBrush read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TAdvTreeViewStrokeBrush read FSelectedStroke write SetSelectedStroke;
    property SelectionArea: TAdvTreeViewSelectionArea read FSelectionArea write SetSelectionArea default tsaFromText;

    property DisabledFill: TAdvTreeViewBrush read FDisabledFill write SetDisabledFill;
    property DisabledStroke: TAdvTreeViewStrokeBrush read FDisabledStroke write SetDisabledStroke;

    property ExtendedFill: TAdvTreeViewBrush read FExtendedFill write SetExtendedFill;
    property ExtendedStroke: TAdvTreeViewStrokeBrush read FExtendedStroke write SetExtendedStroke;
    property ExtendedFont: TFont read FExtendedFont write SetExtendedFont;

    property ExtendedSelectedFill: TAdvTreeViewBrush read FExtendedSelectedFill write SetExtendedSelectedFill;
    property ExtendedSelectedStroke: TAdvTreeViewStrokeBrush read FExtendedSelectedStroke write SetExtendedSelectedStroke;

    property ExtendedDisabledFill: TAdvTreeViewBrush read FExtendedDisabledFill write SetExtendedDisabledFill;
    property ExtendedDisabledStroke: TAdvTreeViewStrokeBrush read FExtendedDisabledStroke write SetExtendedDisabledStroke;

    property ExpandNodeIcon: TAdvTreeViewBitmap read FExpandNodeIcon write SetExpandNodeIcon;
    property CollapseNodeIcon: TAdvTreeViewBitmap read FCollapseNodeIcon write SetCollapseNodeIcon;
    property ExpandNodeIconLarge: TAdvTreeViewBitmap read FExpandNodeIconLarge write SetExpandNodeIconLarge;
    property CollapseNodeIconLarge: TAdvTreeViewBitmap read FCollapseNodeIconLarge write SetCollapseNodeIconLarge;
  end;

  TAdvTreeViewColumnsAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FLayouts: TAdvTreeViewColumnsLayouts;
    FStretch: Boolean;
    FStretchColumn: Integer;
    FStretchAll: Boolean;
    FBottomSize: Double;
    FTopSize: Double;
    FBottomFill: TAdvTreeViewBrush;
    FBottomStroke: TAdvTreeViewStrokeBrush;
    FTopFill: TAdvTreeViewBrush;
    FTopStroke: TAdvTreeViewStrokeBrush;
    FTopFontColor: TAdvTreeViewColor;
    FTopFont: TFont;
    FBottomFontColor: TAdvTreeViewColor;
    FBottomFont: TFont;
    FTopVerticalTextAlign: TAdvTreeViewTextAlign;
    FTopHorizontalTextAlign: TAdvTreeViewTextAlign;
    FBottomHorizontalTextAlign: TAdvTreeViewTextAlign;
    FBottomVerticalTextAlign: TAdvTreeViewTextAlign;
    FSize: Double;
    FTopVerticalText: Boolean;
    FBottomVerticalText: Boolean;
    FFillEmptySpaces: Boolean;
    procedure SetLayouts(const Value: TAdvTreeViewColumnsLayouts);
    procedure SetStretch(const Value: Boolean);
    procedure SetStretchAll(const Value: Boolean);
    procedure SetStretchColumn(const Value: Integer);
    procedure SetBottomSize(const Value: Double);
    procedure SetTopSize(const Value: Double);
    procedure SetBottomFill(const Value: TAdvTreeViewBrush);
    procedure SetBottomStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetTopFill(const Value: TAdvTreeViewBrush);
    procedure SetTopStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetBottomFont(const Value: TFont);
    procedure SetBottomFontColor(const Value: TAdvTreeViewColor);
    procedure SetTopFont(const Value: TFont);
    procedure SetTopFontColor(const Value: TAdvTreeViewColor);
    procedure SetTopVerticalText(const Value: Boolean);
    procedure SetBottomVerticalText(const Value: Boolean);
    procedure SetFillEmptySpaces(const Value: Boolean);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Layouts: TAdvTreeViewColumnsLayouts read FLayouts write SetLayouts default [tclTop];
    property Stretch: Boolean read FStretch write SetStretch default True;
    property StretchColumn: Integer read FStretchColumn write SetStretchColumn default -1;
    property StretchAll: Boolean read FStretchAll write SetStretchAll default True;
    property TopSize: Double read FTopSize write SetTopSize;
    property BottomSize: Double read FBottomSize write SetBottomSize;
    property TopFontColor: TAdvTreeViewColor read FTopFontColor write SetTopFontColor default TAdvTreeViewColorGray;
    property BottomFontColor: TAdvTreeViewColor read FBottomFontColor write SetBottomFontColor default TAdvTreeViewColorGray;
    property TopFont: TFont read FTopFont write SetTopFont;
    property BottomFont: TFont read FBottomFont write SetBottomFont;
    property TopFill: TAdvTreeViewBrush read FTopFill write SetTopFill;
    property BottomFill: TAdvTreeViewBrush read FBottomFill write SetBottomFill;
    property TopStroke: TAdvTreeViewStrokeBrush read FTopStroke write SetTopStroke;
    property BottomStroke: TAdvTreeViewStrokeBrush read FBottomStroke write SetBottomStroke;
    property TopVerticalText: Boolean read FTopVerticalText write SetTopVerticalText default False;
    property BottomVerticalText: Boolean read FBottomVerticalText write SetBottomVerticalText default False;
    property FillEmptySpaces: Boolean read FFillEmptySpaces write SetFillEmptySpaces default True;
  end;

  TAdvTreeViewColumnEmptySpace = (tcesTopLeft, tcesTopRight, tcesBottomLeft, tcesBottomRight);
  TAdvTreeViewGroupEmptySpace = (tgesTopLeft, tgesTopRight, tgesBottomLeft, tgesBottomRight);

  TAdvTreeViewGroupsAppearance = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FLayouts: TAdvTreeViewGroupsLayouts;
    FBottomSize: Double;
    FTopSize: Double;
    FBottomFill: TAdvTreeViewBrush;
    FBottomStroke: TAdvTreeViewStrokeBrush;
    FTopFill: TAdvTreeViewBrush;
    FTopStroke: TAdvTreeViewStrokeBrush;
    FTopFontColor: TAdvTreeViewColor;
    FTopFont: TFont;
    FBottomFontColor: TAdvTreeViewColor;
    FBottomFont: TFont;
    FBottomHorizontalTextAlign: TAdvTreeViewTextAlign;
    FTopVerticalTextAlign: TAdvTreeViewTextAlign;
    FTopHorizontalTextAlign: TAdvTreeViewTextAlign;
    FBottomVerticalTextAlign: TAdvTreeViewTextAlign;
    FTopVerticalText: Boolean;
    FBottomVerticalText: Boolean;
    FFillEmptySpaces: Boolean;
    procedure SetLayouts(const Value: TAdvTreeViewGroupsLayouts);
    procedure SetBottomSize(const Value: Double);
    procedure SetTopSize(const Value: Double);
    procedure SetBottomFill(const Value: TAdvTreeViewBrush);
    procedure SetBottomStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetTopFill(const Value: TAdvTreeViewBrush);
    procedure SetTopStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetBottomFont(const Value: TFont);
    procedure SetBottomFontColor(const Value: TAdvTreeViewColor);
    procedure SetTopFont(const Value: TFont);
    procedure SetTopFontColor(const Value: TAdvTreeViewColor);
    procedure SetBottomHorizontalTextAlign(
      const Value: TAdvTreeViewTextAlign);
    procedure SetBottomVerticalTextAlign(const Value: TAdvTreeViewTextAlign);
    procedure SetTopHorizontalTextAlign(const Value: TAdvTreeViewTextAlign);
    procedure SetTopVerticalTextAlign(const Value: TAdvTreeViewTextAlign);
    procedure SetTopVerticalText(const Value: Boolean);
    procedure SetBottomVerticalText(const Value: Boolean);
    procedure SetFillEmptySpaces(const Value: Boolean);
  protected
    procedure Changed(Sender: TObject);
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Layouts: TAdvTreeViewGroupsLayouts read FLayouts write SetLayouts default [tglTop];
    property TopSize: Double read FTopSize write SetTopSize;
    property BottomSize: Double read FBottomSize write SetBottomSize;
    property TopFill: TAdvTreeViewBrush read FTopFill write SetTopFill;
    property BottomFill: TAdvTreeViewBrush read FBottomFill write SetBottomFill;
    property TopFontColor: TAdvTreeViewColor read FTopFontColor write SetTopFontColor default TAdvTreeViewColorGray;
    property BottomFontColor: TAdvTreeViewColor read FBottomFontColor write SetBottomFontColor default TAdvTreeViewColorGray;
    property TopFont: TFont read FTopFont write SetTopFont;
    property BottomFont: TFont read FBottomFont write SetBottomFont;
    property TopStroke: TAdvTreeViewStrokeBrush read FTopStroke write SetTopStroke;
    property BottomStroke: TAdvTreeViewStrokeBrush read FBottomStroke write SetBottomStroke;
    property TopHorizontalTextAlign: TAdvTreeViewTextAlign read FTopHorizontalTextAlign write SetTopHorizontalTextAlign default tvtaCenter;
    property TopVerticalTextAlign: TAdvTreeViewTextAlign read FTopVerticalTextAlign write SetTopVerticalTextAlign default tvtaCenter;
    property BottomHorizontalTextAlign: TAdvTreeViewTextAlign read FBottomHorizontalTextAlign write SetBottomHorizontalTextAlign default tvtaCenter;
    property BottomVerticalTextAlign: TAdvTreeViewTextAlign read FBottomVerticalTextAlign write SetBottomVerticalTextAlign default tvtaCenter;
    property TopVerticalText: Boolean read FTopVerticalText write SetTopVerticalText default False;
    property BottomVerticalText: Boolean read FBottomVerticalText write SetBottomVerticalText default False;
    property FillEmptySpaces: Boolean read FFillEmptySpaces write SetFillEmptySpaces default True;
  end;

  TAdvTreeViewBeforeSizeColumnEvent = procedure(Sender: TObject; AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean) of object;
  TAdvTreeViewAfterSizeColumnEvent = procedure(Sender: TObject; AColumn: Integer; AColumnSize: Double) of object;
  TAdvTreeViewBeforeDrawColumnEmptySpaceEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnEmptySpaceEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace) of object;
  TAdvTreeViewBeforeDrawGroupEmptySpaceEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawGroupEmptySpaceEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace) of object;

  TAdvTreeViewBeforeDrawColumnEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer) of object;
  TAdvTreeViewBeforeDrawColumnHeaderEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawColumnHeaderEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind) of object;
  TAdvTreeViewBeforeDrawGroupEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawGroupEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind) of object;

  TAdvTreeViewBeforeDrawNodeEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean) of object;
  TAdvTreeViewAfterDrawNodeEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewGetNumberOfNodesEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer) of object;

  TAdvTreeViewGetColumnTextEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String) of object;
  TAdvTreeViewBeforeDrawColumnTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawColumnTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String) of object;
  TAdvTreeViewGetGroupTextEvent = procedure(Sender: TObject; AGroup: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String) of object;
  TAdvTreeViewBeforeDrawGroupTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawGroupTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String) of object;
  TAdvTreeViewGetTimeTextEvent = procedure(Sender: TObject; AValue: Double; ARow: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String) of object;
  TAdvTreeViewBeforeDrawTimeTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AValue: Double; ARow: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawTimeTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AValue: Double; ARow: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String) of object;
  TAdvTreeViewNodeAnchorClickEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String) of object;
  TAdvTreeViewNodeChangedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewGetNodeTextEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String) of object;
  TAdvTreeViewGetNodeTrimmingEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvTreeViewTextTrimming) of object;
  TAdvTreeViewGetNodeHorizontalTextAlignEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvTreeViewTextAlign) of object;
  TAdvTreeViewGetNodeVerticalTextAlignEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvTreeViewTextAlign) of object;
  TAdvTreeViewGetNodeWordWrappingEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean) of object;
  TAdvTreeViewGetColumnTrimmingEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvTreeViewTextTrimming) of object;
  TAdvTreeViewGetColumnHorizontalTextAlignEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvTreeViewTextAlign) of object;
  TAdvTreeViewGetColumnVerticalTextAlignEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvTreeViewTextAlign) of object;
  TAdvTreeViewGetColumnWordWrappingEvent = procedure(Sender: TObject; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean) of object;
  TAdvTreeViewGetNodeIconEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvTreeViewIconBitmap) of object;
  TAdvTreeViewGetNodeHeightEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double) of object;
  TAdvTreeViewBeforeDrawNodeTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeTextEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String) of object;
  TAdvTreeViewBeforeDrawNodeExpandEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand:TAdvTreeViewBitmap; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeExpandEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand:TAdvTreeViewBitmap) of object;
  TAdvTreeViewBeforeDrawNodeIconEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon:TAdvTreeViewIconBitmap; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeIconEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon:TAdvTreeViewIconBitmap) of object;
  TAdvTreeViewBeforeDrawNodeCheckEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode{$IFDEF FMXLIB}; ACheck: TAdvTreeViewBitmap{$ENDIF}; var AAllow: Boolean) of object;
  TAdvTreeViewAfterDrawNodeCheckEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode{$IFDEF FMXLIB}; ACheck: TAdvTreeViewBitmap{$ENDIF}) of object;
  TAdvTreeViewGetNodeColorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor) of object;
  TAdvTreeViewGetNodeCheckTypeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType) of object;
  TAdvTreeViewGetNodeTextColorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor) of object;
  TAdvTreeViewIsNodeExtendedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean) of object;

  TAdvTreeViewIsNodeDeletableEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ADeletable: Boolean) of object;
  TAdvTreeViewIsNodeCheckedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AChecked: Boolean) of object;
  TAdvTreeViewIsNodeExpandedEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean) of object;
  TAdvTreeViewIsNodeVisibleEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AVisible: Boolean) of object;
  TAdvTreeViewIsNodeEnabledEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean) of object;

  TAdvTreeViewBeforeUpdateNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String; var ACanUpdate: Boolean) of object;
  TAdvTreeViewAfterUpdateNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;
  TAdvTreeViewBeforeCollapseNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean) of object;
  TAdvTreeViewAfterCollapseNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeExpandNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean) of object;
  TAdvTreeViewAfterExpandNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean) of object;
  TAdvTreeViewAfterCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;
  TAdvTreeViewBeforeUnCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean) of object;
  TAdvTreeViewAfterUnCheckNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer) of object;

  TAdvTreeViewBeforeSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanSelect: Boolean) of object;
  TAdvTreeViewAfterSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;
  TAdvTreeViewBeforeUnSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean) of object;
  TAdvTreeViewAfterUnSelectNodeEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewNodeClickEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode) of object;

  TAdvTreeViewScrollEvent = procedure(Sender: TObject; APosition: Single) of object;

  {$IFDEF FMXLIB}
  TAdvTreeViewInplaceEditor = TControl;
  {$ENDIF}
  {$IFDEF VCLLIB}
  TAdvTreeViewInplaceEditor = TWinControl;
  {$ENDIF}

  TAdvTreeViewInplaceEditorClass = class of TAdvTreeViewInplaceEditor;
  TAdvTreeViewCustomizeInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor) of object;
  TAdvTreeViewGetInplaceEditorRectEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; var AInplaceEditorRect: {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF}) of object;
  TAdvTreeViewGetInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer;{$IFDEF FMXLIB} var ATransparent: Boolean; {$ENDIF}var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass) of object;
  TAdvTreeViewBeforeOpenInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean) of object;
  TAdvTreeViewAfterOpenInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF}) of object;
  TAdvTreeViewCloseInplaceEditorEvent = procedure(Sender: TObject; ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean; var ACanClose: Boolean) of object;

  TAdvTreeViewMouseEditMode = (tmemDoubleClick, tmemSingleClick, tmemSingleClickOnSelectedNode);

  TAdvTreeViewInteraction = class(TPersistent)
  private
    FTreeView: TAdvCustomTreeView;
    FMultiSelect: Boolean;
    FTouchScrolling: Boolean;
    FReadOnly: Boolean;
    FColumnSizing: Boolean;
    FColumnAutoSizeOnDblClick: Boolean;
    FExtendedSelectable: Boolean;
    FSelectionFollowsFocus: Boolean;
    FKeyboardEdit: Boolean;
    FMouseEditMode: TAdvTreeViewMouseEditMode;
    FExtendedEditable: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetTouchScrolling(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetColumnSizing(const Value: Boolean);
    procedure SetColumnAutoSizeOnDblClick(const Value: Boolean);
    procedure SetExtendedSelectable(const Value: Boolean);
    procedure SetSelectionFollowsFocus(const Value: Boolean);
    procedure SetMouseEditMode(const Value: TAdvTreeViewMouseEditMode);
    procedure SetExtendedEditable(const Value: Boolean);
  protected
    property SelectionFollowsFocus: Boolean read FSelectionFollowsFocus write SetSelectionFollowsFocus default True;
  public
    constructor Create(ATreeView: TAdvCustomTreeView);
    procedure Assign(Source: TPersistent); override;
  published
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property ExtendedSelectable: Boolean read FExtendedSelectable write SetExtendedSelectable default False;
    property ExtendedEditable: Boolean read FExtendedEditable write SetExtendedEditable default False;
    property MouseEditMode: TAdvTreeViewMouseEditMode read FMouseEditMode write SetMouseEditMode default tmemSingleClickOnSelectedNode;
    property TouchScrolling: Boolean read FTouchScrolling write SetTouchScrolling default True;
    property KeyboardEdit: Boolean read FKeyboardEdit write FKeyboardEdit default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ColumnSizing: Boolean read FColumnSizing write SetColumnSizing default False;
    property ColumnAutoSizeOnDblClick: Boolean read FColumnAutoSizeOnDblClick write SetColumnAutoSizeOnDblClick default False;
  end;

  TAdvTreeViewSceneDrawingScale = record
    SceneScale: Double;
    DrawingScale: TPointF;
  end;

  TAdvTreeViewSelectedNodes = class(TList<TAdvTreeViewVirtualNode>);
  TAdvTreeViewNodeArray = array of TAdvTreeViewNode;
  TAdvTreeViewVirtualNodeArray = array of TAdvTreeViewVirtualNode;

  TAdvTreeViewNodeCheck = record
    AColumn: Integer;
    ANode: TAdvTreeViewVirtualNode;
  end;

  TAdvTreeViewNodeAnchor = record
    AAnchor: String;
    AColumn: Integer;
  end;
  
  TAdvCustomTreeView = class(TAdvTreeViewData)
  private
    FColumnSize: Double;
    FCloseWithDialogKey: Boolean;
    FInplaceEditorClass: TAdvTreeViewInplaceEditorClass;
    FInplaceEditor: TAdvTreeViewInplaceEditor;
    FInplaceEditorActive: Boolean;
    FSizeColumn: Integer;
    FUpdateNodeColumn: Integer;
    FUpdateNode: TAdvTreeViewVirtualNode;
    {$IFDEF FMXLIB}
    FScrollBarTimer: TTimer;
    {$ENDIF}
    FDoubleSelection: Boolean;
    FDownNode, FFocusedNode: TAdvTreeViewVirtualNode;
    FDblClicked: Boolean;
    FDoNodeAnchor: TAdvTreeViewNodeAnchor;
    FDoNodeCheck: TAdvTreeViewNodeCheck;
    FDoNodeExpand: Boolean;
    FScrolling: Boolean;
    FMouseUp, FAnimateVerticalPos, FAnimateHorizontalPos: Boolean;
    FAnimating: Boolean;
    FSpX, FSpY: Double;
    FScrollX, FScrollY, FDownX, FDownY, FMouseX, FMouseY, FSizeX: Double;
    FScrollVTo, FScrollHTo: Double;
    FTimeStart, FTimeStop: Double;
    FAnimateTimer: TTimer;
    FNodeCache: TAdvTreeViewNodeCache;
    FColumnsTopCache: TAdvTreeViewColumnsTopCache;
    FColumnsBottomCache: TAdvTreeViewColumnsBottomCache;
    FGroupsTopCache: TAdvTreeViewGroupsTopCache;
    FGroupsBottomCache: TAdvTreeViewGroupsBottomCache;
    FNodeDisplay: TAdvTreeViewNodeDisplayList;
    FColumnsTopDisplay: TAdvTreeViewColumnsTopDisplayList;
    FColumnsBottomDisplay: TAdvTreeViewColumnsBottomDisplayList;
    FGroupsTopDisplay: TAdvTreeViewGroupsTopDisplayList;
    FGroupsBottomDisplay: TAdvTreeViewGroupsBottomDisplayList;
    FNodesAppearance: TAdvTreeViewNodesAppearance;
    FColumnsCaching: Boolean;
    FGroupsCaching: Boolean;
    FGroupsAppearance: TAdvTreeViewGroupsAppearance;
    FColumnsAppearance: TAdvTreeViewColumnsAppearance;
    FOnAfterDrawGroup: TAdvTreeViewAfterDrawGroupEvent;
    FOnBeforeSizeColumn: TAdvTreeViewBeforeSizeColumnEvent;
    FOnAfterSizeColumn: TAdvTreeViewAfterSizeColumnEvent;
    FOnBeforeDrawColumn: TAdvTreeViewBeforeDrawColumnEvent;
    FOnBeforeDrawColumnHeader: TAdvTreeViewBeforeDrawColumnHeaderEvent;
    FOnAfterDrawColumnHeader: TAdvTreeViewAfterDrawColumnHeaderEvent;
    FOnBeforeDrawNode: TAdvTreeViewBeforeDrawNodeEvent;
    FOnAfterDrawColumn: TAdvTreeViewAfterDrawColumnEvent;
    FOnBeforeDrawGroup: TAdvTreeViewBeforeDrawGroupEvent;
    FOnAfterDrawNode: TAdvTreeViewAfterDrawNodeEvent;
    FOnBeforeDrawNodeText: TAdvTreeViewBeforeDrawNodeTextEvent;
    FOnGetNodeHeight: TAdvTreeViewGetNodeHeightEvent;
    FOnAfterDrawColumnText: TAdvTreeViewAfterDrawColumnTextEvent;
    FOnBeforeDrawGroupText: TAdvTreeViewBeforeDrawGroupTextEvent;
    FOnAfterDrawNodeText: TAdvTreeViewAfterDrawNodeTextEvent;
    FOnAfterDrawGroupText: TAdvTreeViewAfterDrawGroupTextEvent;
    FOnBeforeDrawColumnText: TAdvTreeViewBeforeDrawColumnTextEvent;
    FInteraction: TAdvTreeViewInteraction;
    FOnAfterUpdateNode: TAdvTreeViewAfterUpdateNodeEvent;
    FOnBeforeUpdateNode: TAdvTreeViewBeforeUpdateNodeEvent;
    FOnAfterUnCheckNode: TAdvTreeViewAfterUnCheckNodeEvent;
    FOnBeforeUnCheckNode: TAdvTreeViewBeforeUnCheckNodeEvent;
    FOnAfterCheckNode: TAdvTreeViewAfterCheckNodeEvent;
    FOnBeforeCheckNode: TAdvTreeViewBeforeCheckNodeEvent;
    FOnAfterExpandNode: TAdvTreeViewAfterExpandNodeEvent;
    FOnBeforeExpandNode: TAdvTreeViewBeforeExpandNodeEvent;
    FOnAfterCollapseNode: TAdvTreeViewAfterCollapseNodeEvent;
    FOnBeforeCollapseNode: TAdvTreeViewBeforeCollapseNodeEvent;
    FOnBeforeSelectNode: TAdvTreeViewBeforeSelectNodeEvent;
    FOnAfterSelectNode: TAdvTreeViewAfterSelectNodeEvent;
    FOnNodeClick: TAdvTreeViewNodeClickEvent;
    FOnNodeDblClick: TAdvTreeViewNodeClickEvent;
    FOnGetNodeText: TAdvTreeViewGetNodeTextEvent;
    FOnGetColumnText: TAdvTreeViewGetColumnTextEvent;
    FOnGetGroupText: TAdvTreeViewGetGroupTextEvent;
    FOnNodeAnchorClick: TAdvTreeViewNodeAnchorClickEvent;
    FOnVScroll: TAdvTreeViewScrollEvent;
    FOnHScroll: TAdvTreeViewScrollEvent;
    FOnBeforeDrawColumnEmptySpace: TAdvTreeViewBeforeDrawColumnEmptySpaceEvent;
    FOnAfterDrawColumnEmptySpace: TAdvTreeViewAfterDrawColumnEmptySpaceEvent;
    FOnBeforeDrawGroupEmptySpace: TAdvTreeViewBeforeDrawGroupEmptySpaceEvent;
    FOnAfterDrawGroupEmptySpace: TAdvTreeViewAfterDrawGroupEmptySpaceEvent;
    FOnGetNumberOfNodes: TAdvTreeViewGetNumberOfNodesEvent;
    FOnIsNodeExpanded: TAdvTreeViewIsNodeExpandedEvent;
    FOnIsNodeEnabled: TAdvTreeViewIsNodeEnabledEvent;
    FOnIsNodeVisible: TAdvTreeViewIsNodeVisibleEvent;
    FSelectedNodes: TAdvTreeViewSelectedNodes;
    FOnGetNodeDisabledColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnGetNodeSelectedColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeColor: TAdvTreeViewGetNodeColorEvent;
    FOnGetNodeDisabledTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnGetNodeSelectedTextColor: TAdvTreeViewGetNodeTextColorEvent;
    FOnIsNodeExtended: TAdvTreeViewIsNodeExtendedEvent;
    FOnGetNodeIcon: TAdvTreeViewGetNodeIconEvent;
    FOnGetColumnWordWrapping: TAdvTreeViewGetColumnWordWrappingEvent;
    FOnGetColumnVerticalTextAlign: TAdvTreeViewGetColumnVerticalTextAlignEvent;
    FOnGetColumnTrimming: TAdvTreeViewGetColumnTrimmingEvent;
    FOnGetColumnHorizontalTextAlign: TAdvTreeViewGetColumnHorizontalTextAlignEvent;
    FOnGetNodeWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent;
    FOnGetNodeVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent;
    FOnGetNodeTrimming: TAdvTreeViewGetNodeTrimmingEvent;
    FOnGetNodeHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent;
    FOnIsNodeChecked: TAdvTreeViewIsNodeCheckedEvent;
    FOnBeforeDrawNodeIcon: TAdvTreeViewBeforeDrawNodeIconEvent;
    FOnBeforeDrawNodeExpand: TAdvTreeViewBeforeDrawNodeExpandEvent;
    FOnAfterDrawNodeIcon: TAdvTreeViewAfterDrawNodeIconEvent;
    FOnAfterDrawNodeExpand: TAdvTreeViewAfterDrawNodeExpandEvent;
    FOnGetNodeCheckType: TAdvTreeViewGetNodeCheckTypeEvent;
    FOnAfterDrawNodeCheck: TAdvTreeViewAfterDrawNodeCheckEvent;
    FOnBeforeDrawNodeCheck: TAdvTreeViewBeforeDrawNodeCheckEvent;
    FColumnStroke: TAdvTreeViewStrokeBrush;
    FOnAfterDrawNodeColumn: TAdvTreeViewAfterDrawColumnEvent;
    FOnBeforeDrawNodeColumn: TAdvTreeViewBeforeDrawColumnEvent;
    FOnBeforeUnSelectNode: TAdvTreeViewBeforeUnSelectNodeEvent;
    FOnAfterUnSelectNode: TAdvTreeViewAfterUnSelectNodeEvent;
    FOnAfterOpenInplaceEditor: TAdvTreeViewAfterOpenInplaceEditorEvent;
    FOnGetInplaceEditor: TAdvTreeViewGetInplaceEditorEvent;
    FOnCloseInplaceEditor: TAdvTreeViewCloseInplaceEditorEvent;
    FOnBeforeOpenInplaceEditor: TAdvTreeViewBeforeOpenInplaceEditorEvent;
    FOnNodeChanged: TAdvTreeViewNodeChangedEvent;
    FOnCustomizeInplaceEditor: TAdvTreeViewCustomizeInplaceEditorEvent;
    FOnGetInplaceEditorRect: TAdvTreeViewGetInplaceEditorRectEvent;
    procedure SetNodesAppearance(const Value: TAdvTreeViewNodesAppearance);
    procedure SetGroupsAppearance(const Value: TAdvTreeViewGroupsAppearance);
    procedure SetColumnsAppearance(const Value: TAdvTreeViewColumnsAppearance);
    procedure SetInteraction(const Value: TAdvTreeViewInteraction);
    procedure SetVersion(const Value: string);
    function GetFocusedNode: TAdvTreeViewNode;
    function GetFocusedVirtualNode: TAdvTreeViewVirtualNode;
    function GetSelNode(AIndex: Integer): TAdvTreeViewNode;
    function GetSelVirtualNode(AIndex: Integer): TAdvTreeViewVirtualNode;
    procedure SetColumnStroke(const Value: TAdvTreeViewStrokeBrush);
    procedure SetFocusedNode(const Value: TAdvTreeViewNode);
    procedure SetFocusedVirtualNode(const Value: TAdvTreeViewVirtualNode);
  protected
    procedure CaptureEx; virtual;
    procedure ReleaseCaptureEx; virtual;
    procedure IntersectClipRectEx(ACanvas: TCanvas; AState: TAdvTreeViewCanvasSaveState; ARect: TRectF); virtual;
    procedure AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False); override;
    procedure RestoreStateEx(AState: TAdvTreeViewCanvasSaveState; ACanvas: TCanvas); virtual;
    procedure DoBeforeDrawColumnEmptySpace(ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean);
    procedure DoAfterDrawColumnEmptySpace(ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace);
    procedure DoBeforeDrawGroupEmptySpace(ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow: Boolean; var ADefaultDraw: Boolean);
    procedure DoAfterDrawGroupEmptySpace(ACanvas: TCanvas; ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace);

    procedure DoBeforeDrawColumnHeader(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawColumnHeader(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DoBeforeDrawColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer); virtual;
    procedure DoBeforeDrawNodeColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawNodeColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer); virtual;
    procedure DoBeforeDrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DoBeforeDrawNode(ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean); virtual;
    procedure DoAfterDrawNode(ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode); virtual;

    procedure DoGetColumnText(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String); virtual;
    procedure DoBeforeDrawColumnText(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawColumnText(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String); virtual;
    procedure DoGetGroupText(AGroup: Integer; AKind: TAdvTreeViewCacheItemKind; var AText: String); virtual;
    procedure DoBeforeDrawGroupText(ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawGroupText(ACanvas: TCanvas; ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String); virtual;
    procedure DoNodeClick(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoNodeDblClick(ANode: TAdvTreeViewVirtualNode); virtual;

    procedure DoGetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer); override;
    procedure DoGetNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String); override;
    procedure DoGetNodeTrimming(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATrimming: TAdvTreeViewTextTrimming); override;
    procedure DoGetNodeHorizontalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHorizontalTextAlign: TAdvTreeViewTextAlign); override;
    procedure DoGetNodeVerticalTextAlign(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AVerticalTextAlign: TAdvTreeViewTextAlign); override;
    procedure DoGetNodeWordWrapping(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean); override;
    procedure DoGetColumnTrimming(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var ATrimming: TAdvTreeViewTextTrimming); override;
    procedure DoGetColumnHorizontalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AHorizontalTextAlign: TAdvTreeViewTextAlign); override;
    procedure DoGetColumnVerticalTextAlign(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AVerticalTextAlign: TAdvTreeViewTextAlign); override;
    procedure DoGetColumnWordWrapping(AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean); override;
    procedure DoGetNodeCheckType(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACheckType: TAdvTreeViewNodeCheckType); override;
    procedure DoGetNodeHeight(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double); virtual;
    procedure DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; ALarge: Boolean; var AIcon: TAdvTreeViewIconBitmap); override;
    procedure DoGetNodeSelectedColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); override;
    procedure DoGetNodeDisabledColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); override;
    procedure DoGetNodeColor(ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor); override;
    procedure DoGetNodeSelectedTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); override;
    procedure DoGetNodeDisabledTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); override;
    procedure DoGetNodeTextColor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor); override;
    procedure DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean); override;
    procedure DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode; var AExtended: Boolean); override;
    procedure DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AChecked: Boolean); override;
    procedure DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode; var AVisible: Boolean); override;
    procedure DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode; var AEnabled: Boolean); override;

    procedure DoBeforeDrawNodeText(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeText(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String); virtual;
    procedure DoBeforeDrawNodeIcon(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvTreeViewIconBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeIcon(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AIcon: TAdvTreeViewIconBitmap); virtual;
    procedure DoBeforeDrawNodeCheck(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode{$IFDEF FMXLIB};ACheck: TAdvTreeViewBitmap{$ENDIF}; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeCheck(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode{$IFDEF FMXLIB};ACheck: TAdvTreeViewBitmap{$ENDIF}); virtual;
    procedure DoBeforeDrawNodeExpand(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand: TAdvTreeViewBitmap; var AAllow: Boolean); virtual;
    procedure DoAfterDrawNodeExpand(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AExpand: TAdvTreeViewBitmap); virtual;
    procedure DoBeforeSelectNode(ANode: TAdvTreeViewVirtualNode; var ACanSelect: Boolean); virtual;
    procedure DoBeforeUnSelectNode(ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean); virtual;

    procedure DoBeforeSizeColumn(AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean); virtual;
    procedure DoAfterSizeColumn(AColumn: Integer; AColumnSize: Double); virtual;
    procedure DoBeforeUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String; var ACanUpdate: Boolean); virtual;
    procedure DoNodeChanged(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoBeforeExpandNode(ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean); virtual;
    procedure DoAfterExpandNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeCollapseNode(ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean); virtual;
    procedure DoAfterCollapseNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoBeforeCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean); virtual;
    procedure DoAfterCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoBeforeUnCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean); virtual;
    procedure DoAfterUnCheckNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure DoNodeAnchorClick(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String); virtual;
    {$IFDEF FMXLIB}
    procedure ApplyInplaceEditorStyleLookup(Sender: TObject);
    {$ENDIF}

    procedure DoBeforeOpenInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean); virtual;
    procedure DoAfterOpenInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF}); virtual;
    procedure DoCloseInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean; var ACanClose: Boolean); virtual;
    procedure UpdateInplaceEditorPosition; virtual;

    procedure DoGetInplaceEditor(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;{$IFDEF FMXLIB} var ATransparent: Boolean;{$ENDIF} var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass); virtual;
    procedure CloseInplaceEditor(ACancel: Boolean); virtual;

    procedure DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoAfterUnSelectNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure DoHScroll(APosition: Single); virtual;
    procedure DoVScroll(APosition: Single); virtual;
    procedure ResetNodes(AUpdateAll: Boolean = True); override;

    procedure HandleSelectNode(ANode: TAdvTreeViewVirtualNode; ATriggerEvents: Boolean; AKeyBoard: Boolean; AMultiSelect: Boolean);
    procedure Animate(Sender: TObject);
    {$IFDEF FMXLIB}
    procedure ScrollBarChanged(Sender: TObject);
    {$ENDIF}
    procedure DownTime(Sender: TObject);
    procedure StopAnimationTimer; override;
    procedure HandleNodeToggle(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure HandleNodeToggleCheck(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure HandleNodeExpand(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure HandleNodeCollapse(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure BuildDisplay(ACache: TAdvTreeViewCache; ADisplay: TAdvTreeViewDisplayList); virtual;
    procedure UpdateCalculations; override;
    procedure UpdateAutoSizing; override;
    procedure UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True); override;
    procedure UpdateColumnsCache; override;
    procedure UpdateColumnCache(ACache: TAdvTreeViewCache); overload; virtual;
    procedure UpdateGroupsCache; override;
    procedure UpdateGroupCache(ACache: TAdvTreeViewCache); overload; virtual;
    procedure UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False); override;
    procedure UpdateNodeCache; virtual;
    procedure UpdateDisplay; override;
    procedure UpdateTreeViewCache; override;
    procedure UpdateColumnsDisplay; virtual;
    procedure UpdateGroupsDisplay; virtual;
    procedure UpdateNodeDisplay; virtual;
    procedure VerticalScrollPositionChanged; override;
    procedure HorizontalScrollPositionChanged; override;
    procedure DrawNode(ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False); virtual;
    procedure DrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DrawColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind); virtual;
    procedure DrawNodes; virtual;
    procedure DrawColumns; virtual;
    procedure DrawNodeColumns; virtual;
    procedure DrawGroups; virtual;
    procedure DrawBorders; virtual;
    procedure DrawEmptySpaces; virtual;
    procedure DrawDisplay(ADisplay: TAdvTreeViewDisplayList); virtual;
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure HandleMouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure HandleDblClick(X, Y: Single); virtual;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure HandleKeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    procedure HandleDialogKey(var Key: Word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    procedure HandleNodeEditing(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure CustomizeInplaceEditor(AInplaceEditor: TAdvTreeViewInplaceEditor; ANode: TAdvTreeViewVirtualNode; AColumn: TAdvTreeViewColumn); virtual;
    function GetFirstEditableColumn: Integer; virtual;
    function GetLastEditableColumn: Integer; virtual;
    function GetPreviousEditableColumn(AColumn: Integer): Integer; virtual;
    function GetNextEditableColumn(AColumn: Integer): Integer; virtual;
    function GetInplaceEditorRect(ANode: TAdvTreeViewVirtualNode; AColumn: Integer): {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF};
    function GetNextFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetPreviousFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode; virtual;
    function GetVersion: string; virtual;
    function SaveStateEx(ACanvas: TCanvas): TAdvTreeViewCanvasSaveState; virtual;
    function DrawText(ACanvas: TCanvas; ARect: TRectF; AHorizontalAlign, AVerticalAlign: TAdvTreeViewTextAlign; AText: String; ATrimming: TAdvTreeViewTextTrimming = tvttNone; AAngle: Single = 0;
      AReverseAlignment: Boolean = True; ASupportHTML: Boolean = False; ATestAnchor: Boolean = False; AWordWrapping: Boolean = False; AX: Single = -1; AY: Single = - 1; AMinWidth: Single = -1; AMinHeight: Single = -1): String; virtual;
    function CalculateText(ACanvas: TCanvas; AText: String; AWordWrapping: Boolean; ARect: TRectF): TRectF; virtual;
    function GetRowHeight(ARow: Integer): Double; override;
    function GetColumnText(AColumn: Integer): String; virtual;
    function GetGroupText(AGroup: Integer): String;virtual;
    function GetColumnsTopSize: Double; virtual;
    function GetColumnsBottomSize: Double; virtual;
    function GetGroupsTopSize: Double; virtual;
    function GetGroupsBottomSize: Double; virtual;
    function GetContentClipRect: TRectF; override;
    function GetContentRect: TRectF; override;
    function GetCalculationRect: TRectF; override;
    function GetGroupsTopRect: TRectF; virtual;
    function GetGroupsBottomRect: TRectF; virtual;
    function GetColumnsTopRect: TRectF; virtual;
    function GetColumnTopLeftEmptyRect: TRectF; virtual;
    function GetColumnBottomLeftEmptyRect: TRectF; virtual;
    function GetColumnBottomRightEmptyRect: TRectF; virtual;
    function GetColumnTopRightEmptyRect: TRectF; virtual;
    function GetGroupTopLeftEmptyRect: TRectF; virtual;
    function GetGroupBottomLeftEmptyRect: TRectF; virtual;
    function GetGroupBottomRightEmptyRect: TRectF; virtual;
    function GetGroupTopRightEmptyRect: TRectF; virtual;
    function GetColumnsBottomRect: TRectF; virtual;
    function GetCacheWidth: Integer; virtual;
    function GetCacheHeight: Integer; virtual;
    function GetNodeForRow(ARow: Integer): TAdvTreeViewVirtualNode; virtual;
    function XYToCacheItem(X, Y: Double): TAdvTreeViewCacheItem; virtual;

    property NodesAppearance: TAdvTreeViewNodesAppearance read FNodesAppearance write SetNodesAppearance;
    property ColumnsAppearance: TAdvTreeViewColumnsAppearance read FColumnsAppearance write SetColumnsAppearance;
    property GroupsAppearance: TAdvTreeViewGroupsAppearance read FGroupsAppearance write SetGroupsAppearance;

    property OnBeforeOpenInplaceEditor: TAdvTreeViewBeforeOpenInplaceEditorEvent read FOnBeforeOpenInplaceEditor write FOnBeforeOpenInplaceEditor;
    property OnCloseInplaceEditor: TAdvTreeViewCloseInplaceEditorEvent read FOnCloseInplaceEditor write FOnCloseInplaceEditor;
    property OnAfterOpenInplaceEditor: TAdvTreeViewAfterOpenInplaceEditorEvent read FOnAfterOpenInplaceEditor write FOnAfterOpenInplaceEditor;
    property OnGetInplaceEditor: TAdvTreeViewGetInplaceEditorEvent read FOnGetInplaceEditor write FOnGetInplaceEditor;
    property OnCustomizeInplaceEditor: TAdvTreeViewCustomizeInplaceEditorEvent read FOnCustomizeInplaceEditor write FOnCustomizeInplaceEditor;
    property OnGetInplaceEditorRect: TAdvTreeViewGetInplaceEditorRectEvent read FOnGetInplaceEditorRect write FOnGetInplaceEditorRect;
                
    property OnBeforeSizeColumn: TAdvTreeViewBeforeSizeColumnEvent read FOnBeforeSizeColumn write FOnBeforeSizeColumn;
    property OnAfterSizeColumn: TAdvTreeViewAfterSizeColumnEvent read FOnAfterSizeColumn write FOnAfterSizeColumn;
    property OnBeforeDrawColumnEmptySpace: TAdvTreeViewBeforeDrawColumnEmptySpaceEvent read FOnBeforeDrawColumnEmptySpace write FOnBeforeDrawColumnEmptySpace;
    property OnAfterDrawColumnEmptySpace: TAdvTreeViewAfterDrawColumnEmptySpaceEvent read FOnAfterDrawColumnEmptySpace write FOnAfterDrawColumnEmptySpace;
    property OnBeforeDrawGroupEmptySpace: TAdvTreeViewBeforeDrawGroupEmptySpaceEvent read FOnBeforeDrawGroupEmptySpace write FOnBeforeDrawGroupEmptySpace;
    property OnAfterDrawGroupEmptySpace: TAdvTreeViewAfterDrawGroupEmptySpaceEvent read FOnAfterDrawGroupEmptySpace write FOnAfterDrawGroupEmptySpace;

    property OnBeforeDrawColumn: TAdvTreeViewBeforeDrawColumnEvent read FOnBeforeDrawColumn write FOnBeforeDrawColumn;
    property OnAfterDrawColumn: TAdvTreeViewAfterDrawColumnEvent read FOnAfterDrawColumn write FOnAfterDrawColumn;
    property OnBeforeDrawNodeColumn: TAdvTreeViewBeforeDrawColumnEvent read FOnBeforeDrawNodeColumn write FOnBeforeDrawNodeColumn;
    property OnAfterDrawNodeColumn: TAdvTreeViewAfterDrawColumnEvent read FOnAfterDrawNodeColumn write FOnAfterDrawNodeColumn;
    property OnBeforeDrawColumnHeader: TAdvTreeViewBeforeDrawColumnHeaderEvent read FOnBeforeDrawColumnHeader write FOnBeforeDrawColumnHeader;
    property OnAfterDrawColumnHeader: TAdvTreeViewAfterDrawColumnHeaderEvent read FOnAfterDrawColumnHeader write FOnAfterDrawColumnHeader;
    property OnBeforeDrawGroup: TAdvTreeViewBeforeDrawGroupEvent read FOnBeforeDrawGroup write FOnBeforeDrawGroup;
    property OnAfterDrawGroup: TAdvTreeViewAfterDrawGroupEvent read FOnAfterDrawGroup write FOnAfterDrawGroup;
    property OnBeforeDrawNode: TAdvTreeViewBeforeDrawNodeEvent read FOnBeforeDrawNode write FOnBeforeDrawNode;
    property OnAfterDrawNode: TAdvTreeViewAfterDrawNodeEvent read FOnAfterDrawNode write FOnAfterDrawNode;
    property OnBeforeDrawColumnText: TAdvTreeViewBeforeDrawColumnTextEvent read FOnBeforeDrawColumnText write FOnBeforeDrawColumnText;
    property OnGetColumnText: TAdvTreeViewGetColumnTextEvent read FOnGetColumnText write FOnGetColumnText;
    property OnAfterDrawColumnText: TAdvTreeViewAfterDrawColumnTextEvent read FOnAfterDrawColumnText write FOnAfterDrawColumnText;
    property OnBeforeDrawGroupText: TAdvTreeViewBeforeDrawGroupTextEvent read FOnBeforeDrawGroupText write FOnBeforeDrawGroupText;
    property OnGetGroupText: TAdvTreeViewGetGroupTextEvent read FOnGetGroupText write FOnGetGroupText;
    property OnAfterDrawGroupText: TAdvTreeViewAfterDrawGroupTextEvent read FOnAfterDrawGroupText write FOnAfterDrawGroupText;
    property OnBeforeDrawNodeText: TAdvTreeViewBeforeDrawNodeTextEvent read FOnBeforeDrawNodeText write FOnBeforeDrawNodeText;
    property OnGetNodeHeight: TAdvTreeViewGetNodeHeightEvent read FOnGetNodeHeight write FOnGetNodeHeight;
    property OnGetNodeText: TAdvTreeViewGetNodeTextEvent read FOnGetNodeText write FOnGetNodeText;
    property OnGetNodeTrimming: TAdvTreeViewGetNodeTrimmingEvent read FOnGetNodeTrimming write FOnGetNodeTrimming;
    property OnGetNodeWordWrapping: TAdvTreeViewGetNodeWordWrappingEvent read FOnGetNodeWordWrapping write FOnGetNodeWordWrapping;
    property OnGetNodeHorizontalTextAlign: TAdvTreeViewGetNodeHorizontalTextAlignEvent read FOnGetNodeHorizontalTextAlign write FOnGetNodeHorizontalTextAlign;
    property OnGetNodeVerticalTextAlign: TAdvTreeViewGetNodeVerticalTextAlignEvent read FOnGetNodeVerticalTextAlign write FOnGetNodeVerticalTextAlign;
    property OnGetColumnTrimming: TAdvTreeViewGetColumnTrimmingEvent read FOnGetColumnTrimming write FOnGetColumnTrimming;
    property OnGetColumnWordWrapping: TAdvTreeViewGetColumnWordWrappingEvent read FOnGetColumnWordWrapping write FOnGetColumnWordWrapping;
    property OnGetColumnHorizontalTextAlign: TAdvTreeViewGetColumnHorizontalTextAlignEvent read FOnGetColumnHorizontalTextAlign write FOnGetColumnHorizontalTextAlign;
    property OnGetColumnVerticalTextAlign: TAdvTreeViewGetColumnVerticalTextAlignEvent read FOnGetColumnVerticalTextAlign write FOnGetColumnVerticalTextAlign;
    property OnGetNodeIcon: TAdvTreeViewGetNodeIconEvent read FOnGetNodeIcon write FOnGetNodeIcon;
    property OnGetNumberOfNodes: TAdvTreeViewGetNumberOfNodesEvent read FOnGetNumberOfNodes write FOnGetNumberOfNodes;
    property OnAfterDrawNodeText: TAdvTreeViewAfterDrawNodeTextEvent read FOnAfterDrawNodeText write FOnAfterDrawNodeText;
    property OnAfterDrawNodeIcon: TAdvTreeViewAfterDrawNodeIconEvent read FOnAfterDrawNodeIcon write FOnAfterDrawNodeIcon;
    property OnBeforeDrawNodeIcon: TAdvTreeViewBeforeDrawNodeIconEvent read FOnBeforeDrawNodeIcon write FOnBeforeDrawNodeIcon;
    property OnAfterDrawNodeExpand: TAdvTreeViewAfterDrawNodeExpandEvent read FOnAfterDrawNodeExpand write FOnAfterDrawNodeExpand;
    property OnBeforeDrawNodeExpand: TAdvTreeViewBeforeDrawNodeExpandEvent read FOnBeforeDrawNodeExpand write FOnBeforeDrawNodeExpand;
    property OnAfterDrawNodeCheck: TAdvTreeViewAfterDrawNodeCheckEvent read FOnAfterDrawNodeCheck write FOnAfterDrawNodeCheck;
    property OnBeforeDrawNodeCheck: TAdvTreeViewBeforeDrawNodeCheckEvent read FOnBeforeDrawNodeCheck write FOnBeforeDrawNodeCheck;
    property OnBeforeUpdateNode: TAdvTreeViewBeforeUpdateNodeEvent read FOnBeforeUpdateNode write FOnBeforeUpdateNode;
    property OnAfterUpdateNode: TAdvTreeViewAfterUpdateNodeEvent read FOnAfterUpdateNode write FOnAfterUpdateNode;
    property OnBeforeUnCheckNode: TAdvTreeViewBeforeUnCheckNodeEvent read FOnBeforeUnCheckNode write FOnBeforeUnCheckNode;
    property OnAfterUnCheckNode: TAdvTreeViewAfterUnCheckNodeEvent read FOnAfterUnCheckNode write FOnAfterUnCheckNode;
    property OnBeforeCheckNode: TAdvTreeViewBeforeCheckNodeEvent read FOnBeforeCheckNode write FOnBeforeCheckNode;
    property OnAfterCheckNode: TAdvTreeViewAfterCheckNodeEvent read FOnAfterCheckNode write FOnAfterCheckNode;
    property OnBeforeCollapseNode: TAdvTreeViewBeforeCollapseNodeEvent read FOnBeforeCollapseNode write FOnBeforeCollapseNode;
    property OnAfterCollapseNode: TAdvTreeViewAfterCollapseNodeEvent read FOnAfterCollapseNode write FOnAfterCollapseNode;
    property OnBeforeExpandNode: TAdvTreeViewBeforeExpandNodeEvent read FOnBeforeExpandNode write FOnBeforeExpandNode;
    property OnAfterExpandNode: TAdvTreeViewAfterExpandNodeEvent read FOnAfterExpandNode write FOnAfterExpandNode;
    property OnBeforeSelectNode: TAdvTreeViewBeforeSelectNodeEvent read FOnBeforeSelectNode write FOnBeforeSelectNode;
    property OnAfterSelectNode: TAdvTreeViewAfterSelectNodeEvent read FOnAfterSelectNode write FOnAfterSelectNode;
    property OnNodeClick: TAdvTreeViewNodeClickEvent read FOnNodeClick write FOnNodeClick;
    property OnNodeDblClick: TAdvTreeViewNodeClickEvent read FOnNodeDblClick write FOnNodeDblClick;
    property OnBeforeUnSelectNode: TAdvTreeViewBeforeUnSelectNodeEvent read FOnBeforeUnSelectNode write FOnBeforeUnSelectNode;
    property OnAfterUnSelectNode: TAdvTreeViewAfterUnSelectNodeEvent read FOnAfterUnSelectNode write FOnAfterUnSelectNode;
    property OnIsNodeChecked: TAdvTreeViewIsNodeCheckedEvent read FOnIsNodeChecked write FOnIsNodeChecked;
    property OnIsNodeExpanded: TAdvTreeViewIsNodeExpandedEvent read FOnIsNodeExpanded write FOnIsNodeExpanded;
    property OnIsNodeVisible: TAdvTreeViewIsNodeVisibleEvent read FOnIsNodeVisible write FOnIsNodeVisible;
    property OnIsNodeEnabled: TAdvTreeViewIsNodeEnabledEvent read FOnIsNodeEnabled write FOnIsNodeEnabled;
    property OnGetNodeColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeColor write FOnGetNodeColor;
    property OnGetNodeCheckType: TAdvTreeViewGetNodeCheckTypeEvent read FOnGetNodeCheckType write FOnGetNodeCheckType;
    property OnGetNodeSelectedColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeSelectedColor write FOnGetNodeSelectedColor;
    property OnGetNodeDisabledColor: TAdvTreeViewGetNodeColorEvent read FOnGetNodeDisabledColor write FOnGetNodeDisabledColor;
    property OnGetNodeTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeTextColor write FOnGetNodeTextColor;
    property OnGetNodeSelectedTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeSelectedTextColor write FOnGetNodeSelectedTextColor;
    property OnGetNodeDisabledTextColor: TAdvTreeViewGetNodeTextColorEvent read FOnGetNodeDisabledTextColor write FOnGetNodeDisabledTextColor;
    property OnIsNodeExtended: TAdvTreeViewIsNodeExtendedEvent read FOnIsNodeExtended write FOnIsNodeExtended;
    property OnNodeAnchorClick: TAdvTreeViewNodeAnchorClickEvent read FOnNodeAnchorClick write FOnNodeAnchorClick;
    property OnNodeChanged: TAdvTreeViewNodeChangedEvent read FOnNodeChanged write FOnNodeChanged;
    property Interaction: TAdvTreeViewInteraction read FInteraction write SetInteraction;
    property OnVScroll: TAdvTreeViewScrollEvent read FOnVScroll write FOnVScroll;
    property OnHScroll: TAdvTreeViewScrollEvent read FOnHScroll write FOnHScroll;
    property Version: string read GetVersion write SetVersion;
    property ColumnStroke: TAdvTreeViewStrokeBrush read FColumnStroke write SetColumnStroke;
    property InplaceEditorActive: Boolean read FInplaceEditorActive write FInplaceEditorActive;
    property UpdateNodeColumn: Integer read FUpdateNodeColumn write FUpdateNodeColumn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure InitSample; virtual;
    procedure RemoveSelectedVirtualNodes; virtual;
    procedure RemoveSelectedNodes; virtual;
    procedure UnSelectAllNodes; virtual;
    procedure SelectAllNodes; virtual;
    procedure UnSelectAllVirtualNodes; virtual;
    procedure SelectAllVirtualNodes; virtual;
    procedure SelectNode(ANode: TAdvTreeViewNode); virtual;
    procedure SelectVirtualNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure SelectNodes(ANodes: TAdvTreeViewNodeArray); virtual;
    procedure SelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray); virtual;
    procedure UnSelectNode(ANode: TAdvTreeViewNode); virtual;
    procedure UnSelectVirtualNode(ANode: TAdvTreeViewVirtualNode); virtual;
    procedure UnSelectNodes(ANodes: TAdvTreeViewNodeArray); virtual;
    procedure UnSelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray); virtual;

    procedure EditNode(ANode: TAdvTreeViewNode; AColumn: Integer); virtual;
    procedure EditVirtualNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer); virtual;
    procedure StopEditing; virtual;
    procedure CancelEditing; virtual;
    function IsEditing: Boolean; virtual;
        
    function GetInplaceEditor: TAdvTreeViewInplaceEditor; virtual;    
    function IsNodeSelectable(ANode: TAdvTreeViewNode): Boolean; virtual;
    function IsVirtualNodeSelectable(ANode: TAdvTreeViewVirtualNode): Boolean; virtual;
    function XYToNode(X, Y: Double): TAdvTreeViewVirtualNode; virtual;
    function XYToNodeAnchor(ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeAnchor; virtual;
    function XYToNodeExpand(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Boolean; virtual;
    function XYToNodeCheck(ANode: TAdvTreeViewVirtualNode; X, Y: Single): TAdvTreeViewNodeCheck; virtual;
    function XYToNodeTextColumn(ANode: TAdvTreeViewVirtualNode; X, Y: Single): Integer; virtual;
    function XYToColumnSize(X, Y: Single): Integer; virtual;
    function SelectedNodeCount: Integer; virtual;
    function SelectedVirtualNodeCount: Integer; virtual;
    function IsNodeSelected(ANode: TAdvTreeViewNode): Boolean; virtual;
    function IsVirtualNodeSelected(ANode: TAdvTreeViewVirtualNode): Boolean; virtual;
    property FocusedVirtualNode: TAdvTreeViewVirtualNode read GetFocusedVirtualNode write SetFocusedVirtualNode;
    property FocusedNode: TAdvTreeViewNode read GetFocusedNode write SetFocusedNode;
    property SelectedNodes[AIndex: Integer]: TAdvTreeViewNode read GetSelNode;
    property SelectedVirtualNodes[AIndex: Integer]: TAdvTreeViewVirtualNode read GetSelVirtualNode;
  end;

  TAdvTreeViewPublished = class(TAdvCustomTreeView)
  published
    {$IFDEF VCLLIB}
    property Align;
    {$IFNDEF LCLLIB}
    property AlignWithMargins;
    property Margins;
    {$ENDIF}
    property Anchors;
    property Color;
    property Constraints;
    property Font;

    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    {$ENDIF}

    property PictureContainer;

    {$IFDEF FMXLIB}
    property AdaptToStyle;
    property Fill;
    property Stroke;
    property StrokeThickness;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    {$ENDIF}

    property HorizontalScrollBarVisible;
    property VerticalScrollBarVisible;
    property Groups;
    property GroupsAppearance;
    property ColumnStroke;
    property Columns;
    property Nodes;
    property ColumnsAppearance;
    property NodesAppearance;
    property Interaction;
    property StretchScrollBars;
    property Version;
    property OnBeforeDrawColumn;
    property OnAfterDrawColumn;
    property OnBeforeDrawColumnHeader;
    property OnAfterDrawColumnHeader;
    property OnBeforeDrawNodeColumn;
    property OnAfterDrawNodeColumn;
    property OnBeforeDrawGroup;
    property OnAfterDrawGroup;
    property OnBeforeDrawNode;
    property OnAfterDrawNode;
    property OnBeforeDrawColumnText;
    property OnGetColumnText;
    property OnAfterDrawColumnText;
    property OnBeforeDrawGroupText;
    property OnGetGroupText;
    property OnAfterDrawGroupText;
    property OnBeforeDrawNodeText;
    property OnAfterDrawNodeIcon;
    property OnAfterDrawNodeExpand;
    property OnBeforeDrawNodeIcon;
    property OnBeforeDrawNodeExpand;
    property OnAfterDrawNodeCheck;
    property OnBeforeDrawNodeCheck;
    property OnGetNodeCheckType;
    property OnGetNodeText;
    property OnBeforeSizeColumn;
    property OnAfterSizeColumn;
    property OnGetNodeTrimming;
    property OnGetNodeWordWrapping;
    property OnGetNodeHorizontalTextAlign;
    property OnGetNodeVerticalTextAlign;
    property OnGetColumnTrimming;
    property OnGetColumnWordWrapping;
    property OnGetColumnHorizontalTextAlign;
    property OnGetColumnVerticalTextAlign;
    property OnBeforeUnCheckNode;
    property OnAfterUnCheckNode;
    property OnBeforeCheckNode;
    property OnAfterCheckNode;
    property OnBeforeCollapseNode;
    property OnAfterCollapseNode;
    property OnBeforeExpandNode;
    property OnAfterExpandNode;
    property OnGetNodeIcon;
    property OnGetNumberOfNodes;
    property OnIsNodeExtended;
    property OnIsNodeExpanded;
    property OnAfterDrawNodeText;
    property OnIsNodeChecked;
    property OnNodeAnchorClick;
    property OnBeforeUpdateNode;
    property OnAfterUpdateNode;
    property OnBeforeSelectNode;
    property OnAfterSelectNode;
    property OnNodeClick;
    property OnNodeDblClick;
    property OnBeforeUnSelectNode;
    property OnAfterUnSelectNode;
    property OnBeforeOpenInplaceEditor;
    property OnCloseInplaceEditor;
    property OnAfterOpenInplaceEditor;
    property OnGetInplaceEditor;
    property OnCustomizeInplaceEditor;
    property OnGetInplaceEditorRect;
    property OnNodeChanged;
    property OnHScroll;
    property OnVScroll;
    property OnBeforeDrawColumnEmptySpace;
    property OnAfterDrawColumnEmptySpace;
    property OnBeforeDrawGroupEmptySpace;
    property OnAfterDrawGroupEmptySpace;
//    property OnIsNodeVisible;
    property OnIsNodeEnabled;
    property OnGetNodeColor;
    property OnGetNodeSelectedColor;
    property OnGetNodeDisabledColor;
    property OnGetNodeTextColor;
    property OnGetNodeSelectedTextColor;
    property OnGetNodeDisabledTextColor;
    property OnGetNodeHeight;
  end;

function IsRetina: Boolean;

implementation

{$R AdvTreeView.res}

uses
  SysUtils, Math{$IFNDEF LCLLIB}, Rtti{$ENDIF}
  {$IFDEF FMXLIB}
  ,FMX.Styles.Objects, FMX.Edit, FMX.Memo, FMX.ListBox, FMX.Forms
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,StdCtrls
  {$ENDIF}
  ;

type
  TAdvTreeViewColumnOpen = class(TAdvTreeViewColumn);
  TAdvTreeViewGroupOpen = class(TAdvTreeViewGroup);
  TAdvTreeViewNodeOpen = class(TAdvTreeViewNode);

function IsRetina: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function FindRCData(ModuleHandle: HMODULE  ; Name:string): boolean;
begin
  Result := FindResource(ModuleHandle, PChar(Name), PChar(RT_RCDATA)) <> 0;
end;

function GetResourceStream(AResourceName: String): TResourceStream;
var
  hst: NativeUInt;
begin
  Result := nil;
  hst := HInstance;
  if FindRCData(hst, AResourceName) then
    Result := TResourceStream.Create(hst, AResourceName, RT_RCDATA);
end;

procedure LoadBitmapFromResource(ABitmap: TAdvTreeViewBitmap; AResourceName: String);
var
  r: TResourceStream;
  {$IFDEF VCLLIB}
  png: TPngImage;
  {$ENDIF}
begin
  r := nil;
  try
    r := GetResourceStream(AResourceName);
    if Assigned(r) then
    begin
      {$IFDEF VCLLIB}
      png := TPngImage.Create;
      try
        png.LoadFromStream(r);
        ABitmap.Bitmap.Assign(png);
      finally
        png.Free;
      end;
      {$ENDIF}
      {$IFDEF FMXLIB}
      ABitmap.LoadFromStream(r);
      {$ENDIF}
    end;
  finally
    if Assigned(r) then
      r.Free;
  end;
end;

function CreateBitmapFromResource(
  AResourceName: String): TAdvTreeViewBitmap;
begin
  Result := TAdvTreeViewBitmap.Create;
  LoadBitmapFromResource(Result, AResourceName);
end;

function GetTickCountX: DWORD;
var
  h, m, s, ms: Word;
begin
  DecodeTime(Now, h, m, s, ms);
  Result := ms + s * 1000 + m * 60 * 1000 + h * 60 * 60 * 1000;
end;

function AnimateDouble(var Start, Stop: Double; Delta, Margin: Double): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

{ TAdvCustomTreeView }

function TAdvCustomTreeView.GetPreviousEditableColumn(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AColumn > 0) and (AColumn <= Columns.Count - 1) then
  begin
    for I := AColumn - 1 downto 0 do
    begin
      if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.GetNextEditableColumn(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (AColumn >= 0) and (AColumn < Columns.Count - 1) then
  begin
    for I := AColumn + 1 to Columns.Count - 1 do
    begin
      if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.GetFirstEditableColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.GetLastEditableColumn: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Columns.Count - 1 downto 0 do
  begin
    if (Columns[I].EditorType <> tcetNone) or Columns[I].CustomEditor then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.GetFocusedNode: TAdvTreeViewNode;
var
  n: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  n := FFocusedNode;
  if Assigned(n) then
    Result := n.Node;
end;

function TAdvCustomTreeView.GetFocusedVirtualNode: TAdvTreeViewVirtualNode;
begin
  Result := FFocusedNode;
end;

function TAdvCustomTreeView.GetSelNode(AIndex: Integer): TAdvTreeViewNode;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := nil;
  v := SelectedVirtualNodes[AIndex];
  if Assigned(v) then
    Result := v.Node;
end;

function TAdvCustomTreeView.GetSelVirtualNode(
  AIndex: Integer): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex <= SelectedVirtualNodeCount - 1) then
    Result := FSelectedNodes[AIndex];
end;

{$IFDEF FMXLIB}
procedure TAdvCustomTreeView.ScrollBarChanged(Sender: TObject);
begin
  UpdateScrollBars(True, False);
  FScrollBarTimer.Enabled := False;
end;
{$ENDIF}

procedure TAdvCustomTreeView.DoAfterDrawGroup(ACanvas: TCanvas; ARect: TRectF;
  AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
  if Assigned(OnAfterDrawGroup) then
    OnAfterDrawGroup(Self, ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind);
end;

procedure TAdvCustomTreeView.DoAfterDrawGroupEmptySpace(ACanvas: TCanvas;
  ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace);
begin
  if Assigned(OnAfterDrawGroupEmptySpace) then
    OnAfterDrawGroupEmptySpace(Self, ACanvas, ARect, ASpace);
end;

procedure TAdvCustomTreeView.DoAfterDrawGroupText(ACanvas: TCanvas;
  ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String);
begin
  if Assigned(OnAfterDrawGroupText) then
    OnAfterDrawGroupText(Self, ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoAfterDrawNode(ACanvas: TCanvas; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterDrawNode) then
    OnAfterDrawNode(Self, ACanvas, ARect, ANode);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeCheck(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode
  {$IFDEF FMXLIB};ACheck: TAdvTreeViewBitmap{$ENDIF});
begin
  if Assigned(OnAfterDrawNodeCheck) then
    OnAfterDrawNodeCheck(Self, ACanvas, ARect, AColumn, ANode{$IFDEF FMXLIB}, ACheck{$ENDIF});
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeExpand(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AExpand: TAdvTreeViewBitmap);
begin
  if Assigned(OnAfterDrawNodeExpand) then
    OnAfterDrawNodeExpand(Self, ACanvas, ARect, AColumn, ANode, AExpand);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeIcon(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AIcon: TAdvTreeViewIconBitmap);
begin
  if Assigned(OnAfterDrawNodeIcon) then
    OnAfterDrawNodeIcon(Self, ACanvas, ARect, AColumn, ANode, AIcon);
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeText(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String);
begin
  if Assigned(OnAfterDrawNodeText) then
    OnAfterDrawNodeText(Self, ACanvas, ARect, AColumn, ANode, AText);
end;

procedure TAdvCustomTreeView.DoAfterExpandNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterExpandNode) then
    OnAfterExpandNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterOpenInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  AInplaceEditor: TAdvTreeViewInplaceEditor; AInplaceEditorRect: {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF});
begin
  if Assigned(OnAfterOpenInplaceEditor) then
    OnAfterOpenInplaceEditor(Self, ANode, AColumn, AInplaceEditor, AInplaceEditorRect);  
end;

procedure TAdvCustomTreeView.DoAfterDrawNodeColumn(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer);
begin
  if Assigned(OnAfterDrawNodeColumn) then
    OnAfterDrawNodeColumn(Self, ACanvas, ARect, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumn(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer);
begin
  if Assigned(OnAfterDrawColumn) then
    OnAfterDrawColumn(Self, ACanvas, ARect, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnHeader(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
  if Assigned(OnAfterDrawColumnHeader) then
    OnAfterDrawColumnHeader(Self, ACanvas, ARect, AColumn, AKind);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnEmptySpace(ACanvas: TCanvas;
  ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace);
begin
  if Assigned(OnAfterDrawColumnEmptySpace) then
    OnAfterDrawColumnEmptySpace(Self, ACanvas, ARect, ASpace);
end;

procedure TAdvCustomTreeView.DoAfterDrawColumnText(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String);
begin
  if Assigned(OnAfterDrawColumnText) then
    OnAfterDrawColumnText(Self, ACanvas, ARect, AColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoAfterUnCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  if Assigned(OnAfterUnCheckNode) then
    OnAfterUnCheckNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterUnSelectNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterUnSelectNode) then
    OnAfterUnSelectNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoAfterUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  if Assigned(OnAfterUpdateNode) then
    OnAfterUpdateNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroup(ACanvas: TCanvas; ARect: TRectF;
  AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawGroup) then
    OnBeforeDrawGroup(Self, ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroupEmptySpace(ACanvas: TCanvas;
  ARect: TRectF; ASpace: TAdvTreeViewGroupEmptySpace; var AAllow,
  ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawGroupEmptySpace) then
    OnBeforeDrawGroupEmptySpace(Self, ACanvas, ARect, ASpace, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawGroupText(ACanvas: TCanvas;
  ARect: TRectF; AGroup, AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawGroupText) then
    OnBeforeDrawGroupText(Self, ACanvas, ARect, AGroup, AStartColumn, AEndColumn, AKind, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNode(ACanvas: TCanvas; ARect: TRectF;
  ANode: TAdvTreeViewVirtualNode; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawNode) then
    OnBeforeDrawNode(Self, ACanvas, ARect, ANode, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeCheck(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  {$IFDEF FMXLIB}ACheck: TAdvTreeViewBitmap;{$ENDIF} var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeCheck) then
    OnBeforeDrawNodeCheck(Self, ACanvas, ARect, AColumn, ANode{$IFDEF FMXLIB}, ACheck{$ENDIF}, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeExpand(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AExpand: TAdvTreeViewBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeExpand) then
    OnBeforeDrawNodeExpand(Self, ACanvas, ARect, AColumn, ANode, AExpand, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeIcon(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode;
  AIcon: TAdvTreeViewIconBitmap; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeIcon) then
    OnBeforeDrawNodeIcon(Self, ACanvas, ARect, AColumn, ANode, AIcon, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeText(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; ANode: TAdvTreeViewVirtualNode; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawNodeText) then
    OnBeforeDrawNodeText(Self, ACanvas, ARect, AColumn, ANode, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoBeforeCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanCheck: Boolean);
begin
  if Assigned(OnBeforeCheckNode) then
    OnBeforeCheckNode(Self, ANode, AColumn, ACanCheck);
end;

procedure TAdvCustomTreeView.DoBeforeCollapseNode(
  ANode: TAdvTreeViewVirtualNode; var ACanCollapse: Boolean);
begin
  if Assigned(OnBeforeCollapseNode) then
    OnBeforeCollapseNode(Self, ANode, ACanCollapse);
end;

procedure TAdvCustomTreeView.DoBeforeExpandNode(
  ANode: TAdvTreeViewVirtualNode; var ACanExpand: Boolean);
begin
  if Assigned(OnBeforeExpandNode) then
    OnBeforeExpandNode(Self, ANode, ACanExpand);
end;

procedure TAdvCustomTreeView.DoBeforeOpenInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanOpen: Boolean);
begin
  if Assigned(OnBeforeOpenInplaceEditor) then
    OnBeforeOpenInplaceEditor(Self, ANode, AColumn, ACanOpen);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumn(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumn) then
    OnBeforeDrawColumn(Self, ACanvas, ARect, AColumn, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawNodeColumn(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawNodeColumn) then
    OnBeforeDrawNodeColumn(Self, ACanvas, ARect, AColumn, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnHeader(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AAllow: Boolean; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumnHeader) then
    OnBeforeDrawColumnHeader(Self, ACanvas, ARect, AColumn, AKind, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeSizeColumn(AColumn: Integer; AColumnSize: Double; var ANewColumnSize: Double; var AAllow: Boolean);
begin
  if Assigned(OnBeforeSizeColumn) then
    OnBeforeSizeColumn(Self, AColumn, AColumnSize, ANewColumnSize, AAllow);
end;

procedure TAdvCustomTreeView.DoAfterSizeColumn(AColumn: Integer; AColumnSize: Double);
begin
  if Assigned(OnAfterSizeColumn) then
    OnAfterSizeColumn(Self, AColumn, AColumnSize);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnEmptySpace(ACanvas: TCanvas;
  ARect: TRectF; ASpace: TAdvTreeViewColumnEmptySpace; var AAllow,
  ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawColumnEmptySpace) then
    OnBeforeDrawColumnEmptySpace(Self, ACanvas, ARect, ASpace, AAllow, ADefaultDraw);
end;

procedure TAdvCustomTreeView.DoBeforeDrawColumnText(ACanvas: TCanvas;
  ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; AText: String; var AAllow: Boolean);
begin
  if Assigned(OnBeforeDrawColumnText) then
    OnBeforeDrawColumnText(Self, ACanvas, ARect, AColumn, AKind, AText, AAllow);
end;

procedure TAdvCustomTreeView.DoNodeClick(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeClick) then
    OnNodeClick(Self, ANode);
end;

procedure TAdvCustomTreeView.DoNodeDblClick(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeDblClick) then
    OnNodeDblClick(Self, ANode);
end;

procedure TAdvCustomTreeView.DoBeforeSelectNode(ANode: TAdvTreeViewVirtualNode;
  var ACanSelect: Boolean);
begin
  if Assigned(OnBeforeSelectNode) then
    OnBeforeSelectNode(Self, ANode, ACanSelect);
end;

procedure TAdvCustomTreeView.DoBeforeUnCheckNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ACanUnCheck: Boolean);
begin
  if Assigned(OnBeforeUnCheckNode) then
    OnBeforeUnCheckNode(Self, ANode, AColumn, ACanUnCheck);
end;

procedure TAdvCustomTreeView.DoBeforeUnSelectNode(
  ANode: TAdvTreeViewVirtualNode; var ACanUnSelect: Boolean);
begin
  if Assigned(OnBeforeUnSelectNode) then
    OnBeforeUnSelectNode(Self, ANode, ACanUnSelect);
end;

procedure TAdvCustomTreeView.DoBeforeUpdateNode(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AText: String;
  var ACanUpdate: Boolean);
begin
  if Assigned(OnBeforeUpdateNode) then
    OnBeforeUpdateNode(Self, ANode, AColumn, AText, ACanUpdate);
end;

procedure TAdvCustomTreeView.DoCloseInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AInplaceEditor: TAdvTreeViewInplaceEditor; ACancelled: Boolean;
  var ACanClose: Boolean);
begin
  if Assigned(OnCloseInplaceEditor) then
    OnCloseInplaceEditor(Self, ANode, AColumn, AInplaceEditor, ACancelled, ACanClose);
end;

procedure TAdvCustomTreeView.DoAfterCheckNode(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer);
begin
  if Assigned(OnAfterCheckNode) then
    OnAfterCheckNode(Self, ANode, AColumn);
end;

procedure TAdvCustomTreeView.DoAfterCollapseNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterCollapseNode) then
    OnAfterCollapseNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DoGetGroupText(AGroup: Integer;
  AKind: TAdvTreeViewCacheItemKind; var AText: String);
begin
  if Assigned(OnGetGroupText) then
    OnGetGroupText(Self, AGroup, AKind, AText);
end;

procedure TAdvCustomTreeView.DoGetInplaceEditor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; {$IFDEF FMXLIB}var ATransparent: Boolean;{$ENDIF}
  var AInplaceEditorClass: TAdvTreeViewInplaceEditorClass);
begin
  if Assigned(OnGetInplaceEditor) then
    OnGetInplaceEditor(Self, ANode, AColumn, {$IFDEF FMXLIB}ATransparent, {$ENDIF}AInplaceEditorClass);
end;

procedure TAdvCustomTreeView.DoGetNodeCheckType(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ACheckType: TAdvTreeViewNodeCheckType);
begin
  inherited;
  if Assigned(OnGetNodeCheckType) then
    OnGetNodeCheckType(Self, ANode, AColumn, ACheckType);
end;

procedure TAdvCustomTreeView.DoGetNodeHeight(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AHeight: Double);
begin
  inherited;
  if Assigned(OnGetNodeHeight) then
    OnGetNodeHeight(Self, ANode, AColumn, AHeight);
end;

procedure TAdvCustomTreeView.DoGetNodeColor(ANode: TAdvTreeViewVirtualNode;
  var AColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeColor) then
    OnGetNodeColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledColor) then
    OnGetNodeDisabledColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeDisabledTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeDisabledTextColor) then
    OnGetNodeDisabledTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeHorizontalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AHorizontalTextAlign: TAdvTreeViewTextAlign);
begin
  inherited;
  if Assigned(OnGetNodeHorizontalTextAlign) then
    OnGetNodeHorizontalTextAlign(Self, ANode, AColumn, AHorizontalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeIcon(ANode: TAdvTreeViewVirtualNode;
  AColumn: Integer; ALarge: Boolean; var AIcon: TAdvTreeViewIconBitmap);
begin
  inherited;
  if Assigned(OnGetNodeIcon) then
    OnGetNodeIcon(Self, ANode, AColumn, ALarge, AIcon);
end;

procedure TAdvCustomTreeView.DoIsNodeExtended(ANode: TAdvTreeViewVirtualNode;
  var AExtended: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeExtended) then
    OnIsNodeExtended(Self, ANode, AExtended);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedColor(
  ANode: TAdvTreeViewVirtualNode; var AColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedColor) then
    OnGetNodeSelectedColor(Self, ANode, AColor);
end;

procedure TAdvCustomTreeView.DoGetNodeSelectedTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeSelectedTextColor) then
    OnGetNodeSelectedTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeText(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AMode: TAdvTreeViewNodeTextMode; var AText: String);
begin
  inherited;
  if Assigned(OnGetNodeText) then
    OnGetNodeText(Self, ANode, AColumn, AMode, AText);
end;

procedure TAdvCustomTreeView.DoGetNodeTextColor(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var ATextColor: TAdvTreeViewColor);
begin
  inherited;
  if Assigned(OnGetNodeTextColor) then
    OnGetNodeTextColor(Self, ANode, AColumn, ATextColor);
end;

procedure TAdvCustomTreeView.DoGetNodeTrimming(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var ATrimming: TAdvTreeViewTextTrimming);
begin
  inherited;
  if Assigned(OnGetNodeTrimming) then
    OnGetNodeTrimming(Self, ANode, AColumn, ATrimming);
end;

procedure TAdvCustomTreeView.DoGetNodeVerticalTextAlign(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AVerticalTextAlign: TAdvTreeViewTextAlign);
begin
  if Assigned(OnGetNodeVerticalTextAlign) then
    OnGetNodeVerticalTextAlign(Self, ANode, AColumn, AVerticalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetNodeWordWrapping(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer; var AWordWrapping: Boolean);
begin
  inherited;
  if Assigned(OnGetNodeWordWrapping) then
    OnGetNodeWordWrapping(Self, ANode, AColumn, AWordWrapping);
end;

procedure TAdvCustomTreeView.DoGetColumnTrimming(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var ATrimming: TAdvTreeViewTextTrimming);
begin
  inherited;
  if Assigned(OnGetColumnTrimming) then
    OnGetColumnTrimming(Self, AColumn, AKind, ATrimming);
end;

procedure TAdvCustomTreeView.DoGetColumnVerticalTextAlign(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AVerticalTextAlign: TAdvTreeViewTextAlign);
begin
  if Assigned(OnGetColumnVerticalTextAlign) then
    OnGetColumnVerticalTextAlign(Self, AColumn, AKind, AVerticalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetColumnHorizontalTextAlign(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind;
  var AHorizontalTextAlign: TAdvTreeViewTextAlign);
begin
  if Assigned(OnGetColumnHorizontalTextAlign) then
    OnGetColumnHorizontalTextAlign(Self, AColumn, AKind, AHorizontalTextAlign);
end;

procedure TAdvCustomTreeView.DoGetColumnWordWrapping(
  AColumn: Integer; AKind: TAdvTreeViewCacheItemKind; var AWordWrapping: Boolean);
begin
  inherited;
  if Assigned(OnGetColumnWordWrapping) then
    OnGetColumnWordWrapping(Self, AColumn, AKind, AWordWrapping);
end;

procedure TAdvCustomTreeView.DoGetNumberOfNodes(ANode: TAdvTreeViewVirtualNode; var ANumberOfNodes: Integer);
begin
  inherited;
  if Assigned(OnGetNumberOfNodes) then
  begin
    UpdateCount := UpdateCount + 1;
    NodeListBuild := False;
    Nodes.Clear;
    UpdateCount := UpdateCount - 1;
    OnGetNumberOfNodes(Self, ANode, ANumberOfNodes);
  end;
end;

procedure TAdvCustomTreeView.DoGetColumnText(AColumn: Integer;
  AKind: TAdvTreeViewCacheItemKind; var AText: String);
begin
  if Assigned(OnGetColumnText) then
    OnGetColumnText(Self, AColumn, AKind, AText);
end;

procedure TAdvCustomTreeView.DoHScroll(APosition: Single);
begin
  if Assigned(OnHScroll) then
    OnHScroll(Self, APosition);
end;

procedure TAdvCustomTreeView.DoIsNodeChecked(ANode: TAdvTreeViewVirtualNode; AColumn: Integer;
  var AChecked: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeChecked) then
    OnIsNodeChecked(Self, ANode, AChecked);
end;

procedure TAdvCustomTreeView.DoIsNodeEnabled(ANode: TAdvTreeViewVirtualNode;
  var AEnabled: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeEnabled) then
    OnIsNodeEnabled(Self, ANode, AEnabled);
end;

procedure TAdvCustomTreeView.DoIsNodeExpanded(ANode: TAdvTreeViewVirtualNode; var AExpanded: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeExpanded) then
    OnIsNodeExpanded(Self, ANode, AExpanded);
end;

procedure TAdvCustomTreeView.DoIsNodeVisible(ANode: TAdvTreeViewVirtualNode;
  var AVisible: Boolean);
begin
  inherited;
  if Assigned(OnIsNodeVisible) then
    OnIsNodeVisible(ANode, ANode, AVisible);
end;

procedure TAdvCustomTreeView.DoNodeAnchorClick(ANode: TAdvTreeViewVirtualNode; AColumn: Integer; AAnchor: String);
begin
  if Assigned(OnNodeAnchorClick) then
    OnNodeAnchorClick(Self, ANode, AColumn, AAnchor);
end;

procedure TAdvCustomTreeView.DoNodeChanged(
  ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnNodeChanged) then
    OnNodeChanged(Self, ANode);
end;

procedure TAdvCustomTreeView.DoVScroll(APosition: Single);
begin
  if Assigned(OnVScroll) then
    OnVScroll(Self, APosition);
end;

procedure TAdvCustomTreeView.DoAfterSelectNode(ANode: TAdvTreeViewVirtualNode);
begin
  if Assigned(OnAfterSelectNode) then
    OnAfterSelectNode(Self, ANode);
end;

procedure TAdvCustomTreeView.DownTime(Sender: TObject);
begin
end;

procedure TAdvCustomTreeView.Animate(Sender: TObject);
var
  dx, dy, posx, posy: Double;
  animh, animv: Boolean;
begin
  posy := GetVScrollValue;
  posx := GetHScrollValue;
  dx := Abs(FScrollHTo - posx) / Max(1, Abs(FSpX) * 6);
  dy := Abs(FScrollVTo - posy) / Max(1, Abs(FSpY) * 6);
  animv := False;
  if FAnimateVerticalPos then
    animv := AnimateDouble(posy, FScrollVTo, dy, 0.01);

  animh := False;
  if FAnimateHorizontalPos then
    animh := AnimateDouble(posx, FScrollHTo, dx, 0.01);

  FAnimating := animv or animh;
  if FAnimating then
    Scroll(posx, posy)
  else
  begin
    FAnimateVerticalPos := False;
    FAnimateTimer.Enabled := False;
    FAnimateHorizontalPos := False;
  end;
end;

procedure TAdvCustomTreeView.CustomizeInplaceEditor(
  AInplaceEditor: TAdvTreeViewInplaceEditor;
  ANode: TAdvTreeViewVirtualNode; AColumn: TAdvTreeViewColumn);
{$IFDEF FMXLIB}
var
  ext, sel: Boolean;
  f: TFont;
  c: TAlphaColor;
{$ENDIF}
begin
  if not Assigned(ANode) or not Assigned(AColumn) or not Assigned(AInplaceEditor) then
    Exit;

  {$IFDEF FMXLIB}

  sel := IsVirtualNodeSelected(ANode);

  ext := False;
  DoIsNodeExtended(ANode, ext);

  if Assigned(AColumn) and not AColumn.UseDefaultAppearance and not ext then
  begin
    f := AColumn.Font;
    if sel then
      c := AColumn.SelectedFontColor
    else
      c := AColumn.FontColor;
  end
  else
  begin
    if ext then
    begin
      f := NodesAppearance.ExtendedFont;
       if sel then
         c := NodesAppearance.ExtendedSelectedFontColor
       else
         c := NodesAppearance.ExtendedFontColor;
    end
    else
    begin
      f := NodesAppearance.Font;
      if sel then
        c := NodesAppearance.SelectedFontColor
      else
        c := NodesAppearance.FontColor
    end;
  end;

  if (AInplaceEditor is TEdit) then
  begin
    (AInplaceEditor as TEdit).StyledSettings := [];
    (AInplaceEditor as TEdit).Font.Assign(f);
    (AInplaceEditor as TEdit).FontColor := c;
  end
  else if (AInplaceEditor is TMemo) then
  begin
    (AInplaceEditor as TMemo).StyledSettings := [];
    (AInplaceEditor as TMemo).Font.Assign(f);
    (AInplaceEditor as TMemo).FontColor := c;
  end;
  {$ENDIF}

  if Assigned(OnCustomizeInplaceEditor) then
    OnCustomizeInplaceEditor(Self, ANode, AColumn.Index, FInplaceEditor);
end;

{$IFDEF FMXLIB}
procedure TAdvCustomTreeView.ApplyInplaceEditorStyleLookup(Sender: TObject);
var
  obj: TFmxObject;
  function FindSRes(AObject: TFmxObject; AResName: String): TFMXObject;
  var
    I: Integer;
  begin
    Result := nil;
    if Assigned(AObject) then
    begin
      Result := AObject.FindStyleResource(AResName);
      if not Assigned(Result) then
      begin
        for I := 0 to AObject.ChildrenCount - 1 do
        begin
          Result := FindSRes(AObject.Children[I], AResName);
          if Assigned(Result) then
            Break;
        end;
      end;
    end;
  end;
begin
  if (Sender = FInplaceEditor) and ((FInplaceEditor is TMemo) or (FInplaceEditor is TEdit)) then
  begin
    obj := FindSRes(FInplaceEditor, 'background');
    if Assigned(obj) and (obj is TCustomStyleObject) then
      TCustomStyleObject(obj).Source := nil
  end;
end;
{$ENDIF}

procedure TAdvCustomTreeView.Assign(Source: TPersistent);
begin
  BeginUpdate;
  inherited;
  if Source is TAdvCustomTreeView then
  begin
    FNodesAppearance.Assign((Source as TAdvCustomTreeView).NodesAppearance);
    FColumnsAppearance.Assign((Source as TAdvCustomTreeView).ColumnsAppearance);
    FGroupsAppearance.Assign((Source as TAdvCustomTreeView).GroupsAppearance);
  end;
  EndUpdate;
end;

procedure TAdvCustomTreeView.BuildDisplay(ACache: TAdvTreeViewCache; ADisplay: TAdvTreeViewDisplayList);
var
  x, y: Double;
  I, C: Integer;
  cache: TAdvTreeViewCacheItem;
  r, nr, rrt, rrb, rg, grt, grb: TRectF;
  n: TAdvTreeViewVirtualNode;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(ACache) or not Assigned(ADisplay) then
    Exit;

  ADisplay.Clear;

  x := -GetHorizontalScrollPosition;
  y := -GetVerticalScrollPosition;
  r := GetContentRect;
  grt := GetGroupsTopRect;
  grb := GetGroupsBottomRect;
  rrt := GetColumnsTopRect;
  rrb := GetColumnsBottomRect;
  for I := 0 to ACache.Count - 1 do
  begin
    cache := ACache[I];
    rg := cache.Rect;
    case cache.Kind of
      ikNode:
      begin
        OffsetRectEx(rg, int(x) + r.Left, int(y) + r.Top);
        n := cache.Node;
        if Assigned(n) then
        begin
          for C := 0 to ColumnCount - 1 do
          begin
            if (C >= 0) and (C <= Length(n.TextRects) - 1) then
            begin
              nr := n.TextRects[C];
              OffsetRectEx(nr, Int(x) + r.Left, Int(y) + r.Top);
              n.TextRects[C] := nr;
            end;
            if (C >= 0) and (C <= Length(n.BitmapRects) - 1) then
            begin
              nr := n.BitmapRects[C];
              OffsetRectEx(nr, Int(x) + r.Left, Int(y) + r.Top);
              n.BitmapRects[C] := nr;
            end;
            if (C >= 0) and (C <= Length(n.ExpandRects) - 1) then
            begin
              nr := n.ExpandRects[C];
              OffsetRectEx(nr, Int(x) + r.Left, Int(y) + r.Top);
              n.ExpandRects[C] := nr;
            end;
            if (C >= 0) and (C <= Length(n.CheckRects) - 1) then
            begin
              nr := n.CheckRects[C];
              OffsetRectEx(nr, Int(x) + r.Left, Int(y) + r.Top);
              n.CheckRects[C] := nr;
            end;
          end;
        end;
      end;
      ikGroupTop: OffsetRectEx(rg, int(x) + grt.Left, grt.Top);
      ikGroupBottom: OffsetRectEx(rg, int(x) + grb.Left, grb.Top - 1);
      ikColumnTop: OffsetRectEx(rg, int(x) + rrt.Left, rrt.Top);
      ikColumnBottom: OffsetRectEx(rg, int(x) + rrb.Left, rrb.Top - 1);
    end;

    cache.DrawRect := rg;
    case cache.Kind of
      ikNode:
      begin
        if rg.IntersectsWith(r) then
          ADisplay.Add(cache);
      end;
      ikColumnTop:
      begin
        if rg.IntersectsWith(rrt) then
          ADisplay.Add(cache);
      end;
      ikColumnBottom:
      begin
        if rg.IntersectsWith(rrb) then
          ADisplay.Add(cache);
      end;
      ikGroupTop:
      begin
        if rg.IntersectsWith(grt) then
          ADisplay.Add(cache);
      end;
      ikGroupBottom:
      begin
        if rg.IntersectsWith(grb) then
          ADisplay.Add(cache);
      end;
    end;
  end;
end;

function TAdvCustomTreeView.SaveStateEx(ACanvas: TCanvas): TAdvTreeViewCanvasSaveState;
begin
  {$IFDEF FMXLIB}
  Result := ACanvas.SaveState;
  {$ENDIF}
  {$IFDEF VCLLIB}
  Result := TAdvTreeViewCanvasSaveState.Create;
  Result.Fill.Assign(ACanvas.Brush);
  Result.Stroke.Assign(ACanvas.Pen);
  {$ENDIF}
end;

procedure TAdvCustomTreeView.AutoSizeColumnInternal(ACol: Integer; AUpdate: Boolean = False; ACallEventHandlers: Boolean = False);
var
  I: Integer;
  rcalc: TRectF;
  v: TAdvTreeViewVirtualNode;
  bmp: TBitmap;
  strc: String;
  ww: Boolean;
  maxw: Double;
  c: Boolean;
  colps: Double;
  def: Boolean;
  col: TAdvTreeViewColumn;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if (ACol >= 0) and (ACol <= ColumnCount - 1) and not ColumnsAppearance.Stretch and NodeListBuild then
  begin
    maxw := 0;
    colps := ColumnPositions[ACol];
    {$IFDEF FMXLIB}
    bmp := TBitmap.Create(1, 1);
    {$ENDIF}
    {$IFDEF VCLLIB}
    bmp := TBitmap.Create;
    {$ENDIF}


    def := True;
    col := nil;
    if (ACol >= 0) and (ACol <= Columns.Count - 1) then
    begin
      col := Columns[ACol];
      if not col.UseDefaultAppearance then
        def := False;
    end;

    if tclTop in ColumnsAppearance.Layouts then
    begin
      strc := GetColumnText(ACol);
      DoGetColumnText(ACol, ikColumnTop, strc);
      if def then
        bmp.Canvas.Font.Assign(ColumnsAppearance.TopFont)
      else
        bmp.Canvas.Font.Assign(col.TopFont);

      rcalc := RectF(0, 0, 10000, 10000);
      rcalc := CalculateText(bmp.Canvas, strc, False, rcalc);
      rcalc.Width := rcalc.Width + 4;

      if rcalc.Width > maxw then
        maxw := rcalc.Width;
    end;

    if tclBottom in ColumnsAppearance.Layouts then
    begin
      strc := GetColumnText(ACol);
      DoGetColumnText(ACol, ikColumnBottom, strc);
      if def then
        bmp.Canvas.Font.Assign(ColumnsAppearance.BottomFont)
      else
        bmp.Canvas.Font.Assign(col.BottomFont);

      rcalc := RectF(0, 0, 10000, 10000);
      rcalc := CalculateText(bmp.Canvas, strc, False, rcalc);
      rcalc.Width := rcalc.Width + 4;

      if rcalc.Width > maxw then
        maxw := rcalc.Width;
    end;

    for I := StartRow to StopRow do
    begin
      v := GetNodeForRow(I);
      if Assigned(v) then
      begin
        strc := '';
        DoGetNodeText(v, ACol, tntmDrawing, strc);
        ww := False;
        if (ACol >= 0) and (ACol <= Columns.Count - 1) then
          ww := Columns[ACol].WordWrapping;

        DoGetNodeWordWrapping(v, ACol, ww);

        rcalc := RectF(0, 0, 10000, 10000);
        rcalc := CalculateText(bmp.Canvas, strc, false, rcalc);
        rcalc.Width := rcalc.Width + 4;

        if ACol <= Length(v.BitmapRects) - 1 then
          rcalc.Width := rcalc.Width + v.BitmapRects[ACol].Right - colps + 3;

        if rcalc.Width > maxw then
          maxw := rcalc.Width;
      end;
    end;
    bmp.free;


    maxw := Int(maxw);

    c := True;
    if ACallEventHandlers then
      DoBeforeSizeColumn(ACol, ColumnWidths[ACol], maxw, c);

    if c then
    begin
      ColumnWidths[ACol] := maxw;
      if ACallEventHandlers then
        DoAfterSizeColumn(ACol, maxw);

      if AUpdate then
      begin
        UpdateTreeView;
        Invalidate;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.IntersectClipRectEx(ACanvas: TCanvas; AState: TAdvTreeViewCanvasSaveState; ARect: TRectF);
begin
  {$IFDEF FMXLIB}
  ACanvas.IntersectClipRect(ARect);
  {$ENDIF}
  {$IFDEF VCLLIB}
  if Assigned(AState) then
  begin
    AState.SaveDC := SaveDC(ACanvas.Handle);
    IntersectClipRect(ACanvas.Handle, Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  end;
  {$ENDIF}
end;

procedure TAdvCustomTreeView.RestoreStateEx(AState: TAdvTreeViewCanvasSaveState; ACanvas: TCanvas);
begin
  {$IFDEF FMXLIB}
  ACanvas.RestoreState(AState);
  {$ENDIF}
  {$IFDEF VCLLIB}
  if Assigned(AState) then
  begin
    RestoreDC(ACanvas.Handle, AState.SaveDC);
    ACanvas.Brush.Assign(AState.Fill);
    ACanvas.Pen.Assign(AState.Stroke);
    AState.Free;
  end;
  {$ENDIF}
end;

procedure TAdvCustomTreeView.CaptureEx;
begin
  {$IFDEF FMXLIB}
  Capture;
  {$ENDIF}
  {$IFDEF VCLLIB}
  SetCapture(Self.Handle);
  {$ENDIF}
end;

procedure TAdvCustomTreeView.CloseInplaceEditor(ACancel: Boolean);
var
  {$IFNDEF LCLLIB}
  AContext: TRttiContext;
  rt: TRttiType;
  propt: TRttiProperty;
  {$ENDIF}
  str: String;
  b, c: Boolean;
  n: String;
  {$IFDEF FMXLIB}
  propi: TRttiProperty;
  li: TListBoxItem;
  obj: TObject;
  {$ENDIF}
begin
  if Assigned(FUpdateNode) and (FUpdateNodeColumn <> -1) and Assigned(FInplaceEditor) then
  begin
    if not ACancel then
    begin
      {$IFNDEF LCLLIB}
      AContext := TRttiContext.Create;
      {$ENDIF}
      try
        n := '';
        {$IFNDEF LCLLIB}
        rt := AContext.GetType(FInplaceEditor.ClassInfo);
        propt := rt.GetProperty('Text');
        if Assigned(Propt) then
          str := propt.GetValue(FInplaceEditor).AsString
        else
        {$ENDIF}
        begin
          {$IFDEF FMXLIB}
          propi := rt.GetProperty('Selected');
          if Assigned(propi) then
          begin
            obj := propi.GetValue(FInplaceEditor).AsObject;
            if Assigned(obj) and (obj is TListBoxItem) then
            begin
              li := obj as TListBoxItem;
              if Assigned(li) then
                str := li.Text;
            end;
          end;
          {$ENDIF}
        end;

        n := str;
      finally
        {$IFNDEF LCLLIB}
        AContext.Free;
        {$ENDIF}
      end;
    end;

    c := True;
    DoCloseInplaceEditor(FUpdateNode, FUpdateNodeColumn, FInplaceEditor, ACancel, c);
    if c then
    begin
      b := True;
      DoBeforeUpdateNode(FUpdateNode, FUpdateNodeColumn, n, b);
      if b then
      begin
        UpdateCount := UpdateCount + 1;
        if Assigned(FUpdateNode.Node) then
          FUpdateNode.Node.Text[FUpdateNodeColumn] := n;
        UpdateCount := UpdateCount - 1;

        DoAfterUpdateNode(FUpdateNode, FUpdateNodeColumn);
        DoNodeChanged(FUpdateNode);
      end;

      FInplaceEditor.Parent := nil;
      {$IFDEF FMXLIB}
      FInplaceEditor.Release;
      {$ENDIF}
      {$IFDEF VCLLIB}
      FInplaceEditor.Free;
      {$ENDIF}
      FInplaceEditor := nil;
      FInplaceEditorActive := False;
    end;

    ResetNodes(False);
    UpdateTreeView;
    Invalidate;
    SetFocus;
  end;
end;

procedure TAdvCustomTreeView.SelectVirtualNodes(ANodes: TAdvTreeViewVirtualNodeArray);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
    begin
      en := IsVirtualNodeSelectable(v);
      if en then
        FSelectedNodes.Add(v);
    end;
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.SelectNodes(ANodes: TAdvTreeViewNodeArray);
var
  I: Integer;
  v: TAdvTreeViewNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
    begin
      en := IsVirtualNodeSelectable(TAdvTreeViewNodeOpen(v).VirtualNode);

      if en then
        FSelectedNodes.Add(TAdvTreeViewNodeOpen(v).VirtualNode);
    end;
  end;

  Invalidate;
end;

procedure TAdvCustomTreeView.SelectVirtualNode(ANode: TAdvTreeViewVirtualNode);
var
  en: Boolean;
begin
  FSelectedNodes.Clear;
  if Assigned(ANode) then
  begin
    en := IsVirtualNodeSelectable(ANode);
    if en then
    begin
      FSelectedNodes.Add(ANode);
      FFocusedNode := ANode;
    end;
  end;
  Invalidate;
end;

function TAdvCustomTreeView.IsEditing: Boolean;
begin
  Result := FInplaceEditorActive;
end;

function TAdvCustomTreeView.IsNodeSelectable(
  ANode: TAdvTreeViewNode): Boolean;
begin
  Result := True;
  if Assigned(ANode) then
    Result := IsVirtualNodeSelectable(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

function TAdvCustomTreeView.IsNodeSelected(ANode: TAdvTreeViewNode): Boolean;
begin
  Result := False;
  if Assigned(ANode) then
    Result := IsVirtualNodeSelected(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

function TAdvCustomTreeView.IsVirtualNodeSelectable(
  ANode: TAdvTreeViewVirtualNode): Boolean;
var
  ext: Boolean;
begin
  Result := True;
  if Assigned(ANode) then
  begin
    DoIsNodeEnabled(ANode, Result);
    ext := False;
    DoIsNodeExtended(ANode, ext);
    if (ext and not Interaction.ExtendedSelectable) or not VisibleNodes.Contains(ANode) then
      Result := False;
  end;
end;

function TAdvCustomTreeView.IsVirtualNodeSelected(ANode: TAdvTreeViewVirtualNode): Boolean;
begin
  Result := FSelectedNodes.Contains(ANode);
end;

procedure TAdvCustomTreeView.SelectAllNodes;
begin
  SelectAllVirtualNodes;
end;

procedure TAdvCustomTreeView.SelectAllVirtualNodes;
var
  I: Integer;
  n: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  FSelectedNodes.Clear;
  for I := 0 to VisibleNodes.Count - 1 do
  begin
    n := VisibleNodes[I];
    if Assigned(n) then
    begin
      en := IsVirtualNodeSelectable(n);

      if en then
        FSelectedNodes.Add(n);
    end;
  end;
  Invalidate;
end;

function TAdvCustomTreeView.SelectedNodeCount: Integer;
begin
  Result := SelectedVirtualNodeCount;
end;

function TAdvCustomTreeView.SelectedVirtualNodeCount: Integer;
begin
  Result := FSelectedNodes.Count;
end;

procedure TAdvCustomTreeView.SelectNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    SelectVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode)
  else
    SelectVirtualNode(nil);
end;

procedure TAdvCustomTreeView.RemoveSelectedVirtualNodes;
var
  I: Integer;
  n: TAdvTreeViewVirtualNode;
begin
  BlockUpdate := True;
  for I := SelectedVirtualNodeCount - 1 downto 0 do
  begin
    n := SelectedVirtualNodes[I];
    FSelectedNodes.Remove(n);
    RemoveVirtualNode(n);
  end;
  BlockUpdate := False;

  ResetNodes(False);
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvCustomTreeView.RemoveSelectedNodes;
var
  I: Integer;
  n: TAdvTreeViewNode;
begin
  BlockUpdate := True;
  for I := SelectedNodeCount - 1 downto 0 do
  begin
    n := SelectedNodes[I];
    FSelectedNodes.Remove(TAdvTreeViewNodeOpen(n).VirtualNode);
    RemoveNode(n);
  end;
  BlockUpdate := False;

  ResetNodes(False);
  UpdateTreeView;
  Invalidate;
end;

procedure TAdvCustomTreeView.ReleaseCaptureEx;
begin
  ReleaseCapture;
end;

procedure TAdvCustomTreeView.UpdateInplaceEditorPosition;
var
  r: {$IFDEF FMXLIB}TRectF{$ENDIF}{$IFDEF VCLLIB}TRect{$ENDIF};
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) then
    Exit;

  if Assigned(FUpdateNode) and (FUpdateNodeColumn <> -1) and Assigned(FInplaceEditor) and FInplaceEditorActive then
  begin
    if (FUpdateNode.Row >= StartRow) and (FUpdateNode.Row <= StopRow) then
    begin
      r := GetInplaceEditorRect(FUpdateNode, FUpdateNodeColumn);
      FInplaceEditor.Parent := Self;
      FInplaceEditor.Visible := False;
      FInplaceEditor.BoundsRect := r;
      {$IFDEF FMXLIB}
      FInplaceEditor.BoundsRect := RectF(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2 + FInplaceEditor.Height);
      {$ENDIF}
      {$IFDEF VCLLIB}
      FInplaceEditor.BoundsRect := Rect(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) div 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) div 2 + FInplaceEditor.Height);
      {$ENDIF}
      FInplaceEditor.Visible := True;
    end
    else
      FInplaceEditor.Parent := nil;
  end;
end;

procedure TAdvCustomTreeView.HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  dc: TAdvTreeViewCacheItem;
begin
  inherited;

  if Button = TMouseButton.mbRight then
    Exit;

  CaptureEx;

  if not FDblClicked then
  begin
    SetFocus;
    if FInplaceEditorActive then
      CloseInplaceEditor(False);
  end;    

  FDblClicked := False;
  dc := XYToCacheItem(X, Y);
  FDownNode := nil;
  if Assigned(dc) then
    FDownNode := dc.Node;

  if Assigned(FDownNode) then
  begin
    FDoNodeExpand := XYToNodeExpand(FDownNode, X, Y);
    FDoNodeCheck := XYToNodeCheck(FDownNode, X, Y);
    FDoNodeAnchor := XYToNodeAnchor(FDownNode, X, Y);
  end;

  FSizeColumn := -1;
  if Interaction.ColumnSizing then
  begin
    FSizeColumn := XYToColumnSize(X, Y);
    if (FSizeColumn >= 0) and (FSizeColumn <= ColumnCount - 1) then
      FColumnSize := ColumnWidths[FSizeColumn];
  end;

  IsMouseDown := True;
  FTimeStart := GetTickCountX;
  FTimeStop := FTimeStart;
  FScrollVTo := GetVScrollValue;
  FScrollHTo := GetHScrollValue;
  FScrollX := X;
  FScrollY := Y;
  FDownX := X;
  FDownY := Y;
  FMouseX := X;
  FMouseY := Y;
  FSizeX := X;
  FMouseUp := False;
  FDoubleSelection := not FAnimateTimer.Enabled;
  FScrolling := False;
end;

procedure TAdvCustomTreeView.HandleMouseMove(Shift: TShiftState; X, Y: Single);
var
  f: Double;
  it: TAdvTreeViewVirtualNode;
  doscroll: Boolean;
  dsp: TAdvTreeViewCacheItem;
  sc: Integer;
  c: Boolean;
  colsz, newcolsz: Double;
begin
  inherited;

  if (FSizeColumn >= 0) and (FSizeColumn <= ColumnCount - 1) then
  begin
    c := True;
    colsz := FColumnSize;
    FColumnSize := colsz + (X - FSizeX);
    newcolsz := Max(0, FColumnSize);
    FSizeX := X;
    DoBeforeSizeColumn(FSizeColumn, colsz, newcolsz, c);
    if c then
    begin
      ColumnWidths[FSizeColumn] := newcolsz;
      if (FSizeColumn >= 0) and (FSizeColumn <= Columns.Count - 1) then
        Columns[FSizeColumn].UpdateWidth(newcolsz);
      ResetNodes(True);
      UpdateTreeView;
      Invalidate;
    end;
    Exit;
  end;

  if IsMouseDown and not (FDoNodeAnchor.AAnchor <> '') then
  begin
    doscroll := not FDoNodeExpand and not Assigned(FDoNodeCheck.ANode);
    if doscroll then
    begin
      f := 1;
      case ScrollMode of
        smNodeScrolling: f := 0.1;
      end;

      if (FScrolling or (Abs(FMouseX - X) > 3) or (Abs(FMouseY - Y) > 3)) and Interaction.TouchScrolling then
      begin
        if (Abs(X - FDownX) > SCROLLINGDELAY) or (Abs(Y - FDownY) > SCROLLINGDELAY) then
        begin
          FScrolling := True;
          FDoubleSelection := False;
          if IsMouseDown and not FMouseUp then
          begin
            Scroll(GetHScrollValue - (X - FDownX) * f, GetVScrollValue - (Y - FDownY) * f);
            FDownY := Y;
            FDownX := X;
          end;
        end;
      end
    end
  end
  else
  begin
    it := nil;
    dsp := XYToCacheItem(X, Y);
    if Assigned(dsp) then
      it := dsp.Node;

    if Assigned(it) and Assigned(dsp) then
    begin
      if (XYToNodeAnchor(it, X, Y).AAnchor <> '') then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
    end
    else
    begin
      if Interaction.ColumnSizing then
      begin
        sc := XYToColumnSize(X, Y);
        if (sc >= 0) and (sc <= ColumnCount - 1) then
          Cursor := crSizeWE
        else
          Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  f: Double;
  a: TAdvTreeViewNodeAnchor;
  en, d, dosel, ext: Boolean;
  I: Integer;
  t: Integer;
  prev: TAdvTreeViewVirtualNode;
begin
  inherited;

  if Button = TMouseButton.mbRight then
    Exit;

  ReleaseCaptureEx;

  FSizeColumn := -1;

  if not IsMouseDown then
    Exit;

  f := 1;
  case ScrollMode of
    smNodeScrolling: f := 0.1;
  end;

  IsMouseDown := False;
  FMouseUp := True;
  FScrolling := False;

  begin
    if not FDoNodeExpand and not Assigned(FDoNodeCheck.ANode) and not (FDoNodeAnchor.AAnchor <> '') then
    begin
      if not FDoubleSelection and Interaction.TouchScrolling then
      begin
        FTimeStop := GetTickCountX;
        if ((FTimeStop - FTimeStart) < SWIPECOUNT) and ((FTimeStop - FTimeStart) > 0) then
        begin
          FSpY := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
          if (FSpY > 0) then
          begin
            if (Y - FScrollY) > 0 then
              FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewportSize, FScrollVTo - Round(Abs(Y - FScrollY) * FSpY * f * 3)))
            else
              FScrollVTo := Max(0, Min(VerticalScrollBar.Max - GetVViewportSize, FScrollVTo + Round(Abs(Y - FScrollY) * FSpY * f * 3)));

            FAnimateVerticalPos := True;
            FAnimateTimer.Enabled := True;
          end;

          FSpX := Abs(X - FScrollX) / (FTimeStop - FTimeStart);
          if (FSpX > 0) then
          begin
            if (X - FScrollX) > 0 then
              FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewportSize, FScrollHTo - Round(Abs(X - FScrollX) * FSpX * f * 3)))
            else
              FScrollHTo := Max(0, Min(HorizontalScrollBar.Max - GetHViewportSize, FScrollHTo + Round(Abs(X - FScrollX) * FSpX * f * 3)));

            FAnimateHorizontalPos := True;
            FAnimateTimer.Enabled := True;
          end;
        end;
      end
      else
      begin
        if Assigned(FDownNode) then
        begin
          en := IsVirtualNodeSelectable(FDownNode);
          prev := FFocusedNode;
          dosel := en;

          if en then
          begin
            if (ssShift in Shift) then
            begin
              if Assigned(FFocusedNode) then
              begin
                if FFocusedNode.Row <= FDownNode.Row then
                begin
                  for I := FFocusedNode.Row + 1 to FDownNode.Row do
                    HandleSelectNode(GetNodeFromNodeStructure(I), True, False, {$IFNDEF DELPHI_LLVM}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
                end
                else
                begin
                  for I := FFocusedNode.Row - 1 downto FDownNode.Row do
                    HandleSelectNode(GetNodeFromNodeStructure(I), True, False, {$IFNDEF DELPHI_LLVM}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
                end;
              end;
            end
            else
            begin
              if not (ssCtrl in Shift) and (FSelectedNodes.Count > 0) then
                UnSelectVirtualNode(FDownNode);

              HandleSelectNode(FDownNode, True, False, {$IFNDEF DELPHI_LLVM}(ssCtrl in Shift) and {$ENDIF} Interaction.MultiSelect);
            end;
          end;

          if dosel then
            FFocusedNode := FDownNode;

          if ((Interaction.MouseEditMode = tmemSingleClick) or ((Interaction.MouseEditMode = tmemSingleClickOnSelectedNode) and (FDownNode = prev))) and not Interaction.ReadOnly then
          begin
            if Assigned(FDownNode) then
            begin
              ext := False;
              DoIsNodeExtended(FDownNode, ext);
              d := True;
              DoIsNodeEnabled(FDownNode, d);
              if ((ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext) and d then
              begin
                t := XYToNodeTextColumn(FDownNode, X, Y);
                if t <> -1 then
                  HandleNodeEditing(FDownNode, t);
              end;
            end;
          end;

          Invalidate;
        end;
      end;
    end
    else
    begin
      if FDoNodeExpand then
        HandleNodeToggle(FDownNode)
      else if Assigned(FDoNodeCheck.ANode) then
        HandleNodeToggleCheck(FDownNode, FDoNodeCheck.AColumn)
      else
      begin
        a := XYToNodeAnchor(FDownNode, X, Y);
        if a.AAnchor <> '' then
          DoNodeAnchorClick(FDownNode, a.AColumn, a.AAnchor);
      end;
    end;
  end;

  FDoNodeExpand := False;
  FDoNodeAnchor.AAnchor := '';
  FDoNodeAnchor.AColumn := -1;
  
end;

procedure TAdvCustomTreeView.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  vpos, hpos: Double;
  sz: Single;
  R: Integer;
  nd: TAdvTreeViewVirtualNode;
begin
  inherited;
  vpos := GetVScrollValue;
  hpos := GetHScrollValue;

  if WheelDelta > 0 then
    R := StartRow - 1
  else
    R := StartRow;

  nd := GetNodeForRow(R);

  if Assigned(nd) then
  begin
    if WheelDelta > 0 then
    begin
      sz := nd.Height;
      Scroll(hpos, vpos - sz)
    end
    else
    begin
      sz := nd.Height;
      Scroll(hpos, vpos + sz);
    end;
  end
  else
    Scroll(hpos, 0);

  Handled := True;
end;

procedure TAdvCustomTreeView.UpdateGroupCache(ACache: TAdvTreeViewCache);
var
  h: Double;
  I: Integer;
  grp: TAdvTreeViewDisplayGroup;
  cr: TRectF;
  x, y, bw, bh: Double;
  rt, rc: TRectF;
  cache: TAdvTreeViewCacheItem;
begin
  inherited;
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (ColumnCount = 0) or (DisplayGroups.Count = 0) or not Assigned(ACache) then
    Exit;

  ACache.Clear;

  if (ACache is TAdvTreeViewGroupsTopCache) and (not (tglTop in GroupsAppearance.Layouts) or (GroupsAppearance.TopSize <= 0)) then
    Exit;

  if (ACache is TAdvTreeViewGroupsBottomCache) and (not (tglBottom in GroupsAppearance.Layouts) or (GroupsAppearance.BottomSize <= 0)) then
    Exit;

  cr := GetContentRect;
  if ACache is TAdvTreeViewGroupsTopCache then
    h := GetGroupsTopSize + 1
  else
    h := GetGroupsBottomSize + 1;

  for I := 0 to DisplayGroups.Count - 1 do
  begin
    grp := DisplayGroups[I];
    y := 0;
    x := Int(ColumnPositions[grp.StartColumn]);
    bh := h;
    bw := Int(ColumnPositions[grp.EndColumn + 1] - x);

    if (bw <= 0) or (bh <= 0) then
      Continue;

    rt := RectF(0, 0, bw, bh);

    rc.Top := int(rt.Top) + 0.5;
    rc.Bottom := int(rt.Bottom) - 0.5;
    rc.Left := int(rt.Left) + 0.5;
    rc.Right := int(rt.Right) + 0.5;

    OffsetRectEx(rc, x, y);
    if ACache is TAdvTreeViewGroupsTopCache then
      cache := TAdvTreeViewCacheItem.CreateGroupTop(rc, I, grp.StartColumn, grp.EndColumn)
    else
      cache := TAdvTreeViewCacheItem.CreateGroupBottom(rc, I, grp.StartColumn, grp.EndColumn);

    ACache.Add(cache);
  end;
end;

procedure TAdvCustomTreeView.UpdateGroupsCache;
begin
  UpdateGroupCache(FGroupsTopCache);
  UpdateGroupCache(FGroupsBottomCache);
  UpdateGroupsDisplay;
end;

procedure TAdvCustomTreeView.UpdateGroupsDisplay;
begin
  BuildDisplay(FGroupsTopCache, FGroupsTopDisplay);
  BuildDisplay(FGroupsBottomCache, FGroupsBottomDisplay);
end;

function TAdvCustomTreeView.CalculateText(ACanvas: TCanvas; AText: String; AWordWrapping: Boolean; ARect: TRectF): TRectF;
begin

end;

procedure TAdvCustomTreeView.CancelEditing;
begin
  if FInplaceEditorActive then
    CloseInplaceEditor(True);
end;

procedure TAdvCustomTreeView.UpdateNodeCache;
var
  rt: TRectF;
  bw, bh: Double;
  x, y: Double;
  rc: TRectF;
  cache: TAdvTreeViewCacheItem;
  I, C: Integer;
  cr, urc: TRectF;
  vs: Double;
  v: TAdvTreeViewVirtualNode;
  stty, lvlw: Double;
  bmp: TBitmap;
  rcalc: TRectF;
  strc: String;
  cx, cw: Double;
  maxh: Double;
  st, stp: Integer;
  bmpn: TAdvTreeViewIconBitmap;
  bmpw, bmph, expw, exph: Double;
  ww: Boolean;
  colw: Double;
  chk: TAdvTreeViewNodeCheckType;
  ext: Boolean;
  chksz: Double;
  cl: TAdvTreeViewColumn;
  expr, bmpr, txtr, chkr: TArray<TRectF>;
  vl: Double;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(FNodeCache) then
    Exit;

  FNodeCache.Clear;

  if ColumnCount > 0 then
  begin
    {$IFDEF FMXLIB}
    bmp := TBitmap.Create(1, 1);
    {$ENDIF}
    {$IFDEF VCLLIB}
    bmp := TBitmap.Create;
    {$ENDIF}

    cr := GetContentRect;
    vs := GetVerticalScrollPosition;

    stty := vs + StartOffset;

    for I := StartRow to StopRow do
    begin
      v := GetNodeForRow(I);
      if Assigned(v) then
      begin
        bw := TotalColumnWidth;
        lvlw := NodesAppearance.LevelIndent * v.Level;

        if NodesAppearance.ExpandColumn = 0 then
        begin
          x := lvlw;
          bw := bw - lvlw;
        end
        else
          x := 0;

        maxh := 0;
        y := stty;
        bh := 0;

        rt := RectF(Int(x), Int(y), Int(x + bw), Int(y + bh));

        if HorizontalScrollBar.Visible or ColumnsAppearance.Stretch then
        begin
          if I = 0 then
            rc := RectF(rt.Left + 1.5, rt.Top + 1.5, rt.Right - 1.5, rt.Bottom + 0.5)
          else if I = RowCount - 1 then
            rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 1.5, rt.Bottom + 0.5)
          else
            rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 1.5, rt.Bottom + 0.5);
        end
        else
        begin
          if I = 0 then
            rc := RectF(rt.Left + 1.5, rt.Top + 1.5, rt.Right - 0.5, rt.Bottom + 0.5)
          else if I = RowCount - 1 then
            rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 0.5, rt.Bottom + 0.5)
          else
            rc := RectF(rt.Left + 1.5, rt.Top + 0.5, rt.Right - 0.5, rt.Bottom + 0.5);
        end;

        if v.Calculated then
          bh := v.Height
        else
        begin
          UpdateNodeCalculated(v, True);

          if NodesAppearance.HeightMode = tnhmVariable then
          begin
            st := 0;
            stp := ColumnCount - 1;
            ext := False;
            DoIsNodeExtended(v, ext);

            if ext then
              stp := Min(0, ColumnCount - 1);

            for C := st to stp do
            begin
              colw := ColumnWidths[C];
              if (colw > 0) or ext then
              begin
                strc := '';
                DoGetNodeText(v, C, tntmDrawing, strc);

                cx := ColumnPositions[C];
                if C = st then
                  cx := cx + rc.Left;

                if ext then
                  cw := cx + bw
                else
                  cw := cx + ColumnWidths[C];

                if C = st then
                  cw := cw - rc.Left;

                if (NodesAppearance.ExpandColumn = C) and (C > st) then
                  cx := cx + lvlw;

                if C = NodesAppearance.ExpandColumn then
                begin
                  expw := NodesAppearance.ExpandWidth + 4;
                  exph := NodesAppearance.ExpandHeight + 4;
                  cx := cx + expw;
                  if exph > maxh then
                    maxh := exph;
                end;

                chk := tvntNone;
                DoGetNodeCheckType(v, C, chk);
                if chk <> tvntNone then
                begin
                  {$IFDEF DELPHI_LLVM}
                  chksz := 29;
                  {$ELSE}
                  {$IFDEF VCLLIB}
                  chksz := 20;
                  {$ENDIF}
                  {$IFDEF FMXLIB}
                  chksz := 23;
                  {$ENDIF}
                  {$ENDIF}

                  cx := cx + Max(0, chksz);
                  if chksz > maxh then
                    maxh := chksz;
                end;

                bmpn := nil;
                DoGetNodeIcon(v, C, False, bmpn);
                if not Assigned(bmpn) and IsRetina then
                  DoGetNodeIcon(v, C, True, bmpn);

                if Assigned(bmpn) and (bmpn.Width > 0) and (bmpn.Height > 0) then
                begin
                  bmpw := bmpn.Width + 4;
                  bmph := bmpn.Height + 4;

                  cx := cx + bmpw;
                  if bmph > maxh then
                    maxh := bmph;
                end;

                ww := False;
                if (C >= 0) and (C <= Columns.Count - 1) then
                  ww := Columns[C].WordWrapping;

                DoGetNodeWordWrapping(v, C, ww);

                rcalc := RectF(cx, 0, cw, 10000);
                InflateRectEx(rcalc, -2, 0);

                cl := nil;
                if (C >= 0) and (C <= ColumnCount - 1) then
                  cl := Columns[C];

                if Assigned(cl) and not cl.UseDefaultAppearance and not ext then
                  bmp.Canvas.Font.Assign(cl.Font)
                else
                begin
                  if ext then
                    bmp.Canvas.Font.Assign(NodesAppearance.ExtendedFont)
                  else
                    bmp.Canvas.Font.Assign(NodesAppearance.Font);
                end;

                rcalc := CalculateText(bmp.Canvas, strc, ww, rcalc);
                rcalc.Height := rcalc.Height + 4;

                vl := rcalc.Height;
                DoGetNodeHeight(v, C, vl);

                if vl > maxh then
                  maxh := vl;
              end;
            end;

            bh := Max(DefaultRowHeight, maxh);
            UpdateNodeHeight(v, Int(bh));
            TotalRowHeight := (TotalRowHeight - DefaultRowHeight) + v.Height;
          end
          else
          begin
            bh := DefaultRowHeight;
            UpdateNodeHeight(v, bh);
          end;
        end;

        stty := stty + Int(bh);
        rc.Height := rc.Height + Int(bh);

        urc := rc;
        SetLength(txtr, 0);
        SetLength(bmpr, 0);
        SetLength(expr, 0);
        SetLength(chkr, 0);

        st := 0;
        stp := ColumnCount - 1;
        ext := False;
        DoIsNodeExtended(v, ext);
        if ext then
          stp := Min(0, ColumnCount - 1);

        for C := st to stp do
        begin
          colw := ColumnWidths[C];
          cx := ColumnPositions[C];
          if C = st then
            cx := cx + urc.Left;

          if ext then
            cw := cx + bw
          else
            cw := cx + colw;

          if C = st then
            cw := cw - urc.Left;

          if (NodesAppearance.ExpandColumn = C) and (C > st) then
            cx := cx + lvlw;

          if ext then
            cw := cw + lvlw;

          expw := 0;
          exph := 0;
          if C = NodesAppearance.ExpandColumn then
          begin
            expw := NodesAppearance.ExpandWidth + 4;
            exph := NodesAppearance.ExpandHeight + 4;
          end;

          SetLength(expr, Length(expr) + 1);
          expr[Length(expr) - 1] := RectF(Int(cx + 2), Int(urc.Top + (urc.Height - exph) / 2 + 2), Int(cx + expw - 2), Int(urc.Top + (urc.Height - exph) / 2 + exph - 2));

          cx := cx + expw;

          chk := tvntNone;
          chksz := 0;
          DoGetNodeCheckType(v, C, chk);
          if chk <> tvntNone then
          begin
            {$IFDEF DELPHI_LLVM}
            chksz := 29;
            {$ELSE}
            {$IFDEF VCLLIB}
            chksz := 20;
            {$ENDIF}
            {$IFDEF FMXLIB}
            chksz := 23;
            {$ENDIF}
            {$ENDIF}
          end;

          SetLength(chkr, Length(chkr) + 1);
          chkr[Length(chkr) - 1] := RectF(Int(cx + 4), Int(urc.Top + (urc.Height - chksz) / 2 + 2), Int(cx + chksz), Int(urc.Top + (urc.Height - chksz) / 2 + chksz - 2));

          cx := cx + Max(0, chksz);

          bmpn := nil;
          DoGetNodeIcon(v, C, False, bmpn);
          if not Assigned(bmpn) and IsRetina then
            DoGetNodeIcon(v, C, True, bmpn);

          bmpw := 0;
          bmph := 0;
          if Assigned(bmpn) and (bmpn.Width > 0) and (bmpn.Height > 0) then
          begin
            bmpw := bmpn.Width + 4;
            bmph := bmpn.Height + 4;
          end;

          SetLength(bmpr, Length(bmpr) + 1);
          bmpr[Length(bmpr) - 1] := RectF(Int(cx + 2), Int(urc.Top + (urc.Height - bmph) / 2 + 2), Int(cx + bmpw - 2), Int(urc.Top + (urc.Height - bmph) / 2 + bmph - 2));
          cx := cx + bmpw;

          SetLength(txtr, Length(txtr) + 1);
          txtr[Length(txtr) - 1] := RectF(cx, urc.Top + (urc.Height - rc.Height) / 2, cw, urc.Top + (urc.Height - rc.Height) / 2 + rc.Height);
        end;

        UpdateNodeTextRects(v, txtr);
        UpdateNodeBitmapRects(v, bmpr);
        UpdateNodeExpandRects(v, expr);
        UpdateNodeCheckRects(v, chkr);

        cache := TAdvTreeViewCacheItem.CreateNode(rc, v);
        FNodeCache.Add(cache);
        UpdateNodeCacheReference(v, cache);

        if y - vs > cr.Height then
          Break;
      end;
    end;

    bmp.Free;
  end;
end;

procedure TAdvCustomTreeView.UpdateNodeDisplay;
begin
  BuildDisplay(FNodeCache, FNodeDisplay);
end;

constructor TAdvCustomTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FColumnStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorNull);

  FSizeColumn := -1;
  FGroupsCaching := False;
  FColumnsCaching := False;
  FSelectedNodes := TAdvTreeViewSelectedNodes.Create;
  FNodeCache := TAdvTreeViewNodeCache.Create;
  FColumnsTopCache := TAdvTreeViewColumnsTopCache.Create;
  FGroupsTopCache := TAdvTreeViewGroupsTopCache.Create;
  FColumnsBottomCache := TAdvTreeViewColumnsBottomCache.Create;
  FGroupsBottomCache := TAdvTreeViewGroupsBottomCache.Create;

  FNodeDisplay := TAdvTreeViewNodeDisplayList.Create;
  FColumnsTopDisplay := TAdvTreeViewColumnsTopDisplayList.Create;
  FGroupsTopDisplay := TAdvTreeViewGroupsTopDisplayList.Create;
  FColumnsBottomDisplay := TAdvTreeViewColumnsBottomDisplayList.Create;
  FGroupsBottomDisplay := TAdvTreeViewGroupsBottomDisplayList.Create;

  FNodesAppearance := TAdvTreeViewNodesAppearance.Create(Self);
  FColumnsAppearance := TAdvTreeViewColumnsAppearance.Create(Self);
  FGroupsAppearance := TAdvTreeViewGroupsAppearance.Create(Self);
  FInteraction := TAdvTreeViewInteraction.Create(Self);

  FAnimateTimer := TTimer.Create(Self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.Enabled := False;
  FAnimateTimer.OnTimer := Animate;

  {$IFDEF FMXLIB}
  FScrollBarTimer := TTimer.Create(Self);
  FScrollBarTimer.Interval := 1;
  FScrollBarTimer.Enabled := False;
  FScrollBarTimer.OnTimer := ScrollBarChanged;
  {$ENDIF}

  Width := 300;
  Height := 276;
  if IsDesignTime then
    InitSample;
end;

procedure TAdvCustomTreeView.HandleDblClick(X, Y: Single);
var
  pf: TPointF;
  c: TAdvTreeViewVirtualNode;
  col: Integer;
  en, ext: Boolean;
  t: Integer;
begin
  inherited;
  FDblClicked := True;
  pf := PointF(X, Y);
  FDownNode := nil;
  c := XYToNode(pf.X, pf.Y);
  if Assigned(c) then
  begin
    FDownNode := c;
    if Assigned(FDownNode) then
    begin
      if (Interaction.MouseEditMode = tmemDoubleClick) and not Interaction.ReadOnly  then
      begin
        en := True;
        DoIsNodeEnabled(FDownNode, en);
        if en then
        begin
          ext := False;
          DoIsNodeExtended(FDownNode, ext);
          if (ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext then
          begin
            t := XYToNodeTextColumn(FDownNode, pf.X, pf.Y);
            if t <> -1 then
              HandleNodeEditing(FDownNode, t);
          end;
        end;
      end;
      DoNodeDblClick(FDownNode);
    end;
  end
  else
  begin
    col := XYToColumnSize(pf.X, pf.Y);
    if Interaction.ColumnAutoSizeOnDblClick then
      AutoSizeColumn(col);
  end;
end;

destructor TAdvCustomTreeView.Destroy;
begin
  {$IFDEF FMXLIB}
  FScrollBarTimer.Free;
  {$ENDIF}
  FColumnStroke.Free;
  FAnimateTimer.Free;
  FNodesAppearance.Free;
  FGroupsAppearance.Free;
  FInteraction.Free;
  FColumnsAppearance.Free;
  FGroupsTopDisplay.Free;
  FColumnsTopDisplay.Free;
  FGroupsBottomDisplay.Free;
  FColumnsBottomDisplay.Free;
  FSelectedNodes.Free;
  FNodeDisplay.Free;
  FNodeCache.Free;
  FGroupsTopCache.Free;
  FColumnsTopCache.Free;
  FGroupsBottomCache.Free;
  FColumnsBottomCache.Free;
  inherited;
end;

procedure TAdvCustomTreeView.HandleDialogKey(var Key: Word; Shift: TShiftState);
var
  n: TAdvTreeViewVirtualNode;
  c: Integer;
  ext: Boolean;
begin
  inherited;
  if Assigned(FInplaceEditor) and FInplaceEditorActive and ((Key = KEY_ESCAPE) or (Key = KEY_TAB) or (Key = KEY_F2)
    or ((FInplaceEditor is TEdit) and (Key = KEY_RETURN))) then
  begin
    FCloseWithDialogKey := True;
    CloseInplaceEditor(Key = KEY_ESCAPE);
    if Key = KEY_TAB then
    begin
      if Assigned(FFocusedNode) then
      begin
        if ssShift in Shift then
        begin
          ext := False;
          DoIsNodeExtended(FFocusedNode, ext);
          if (FUpdateNodeColumn > GetFirstEditableColumn) and not ext then
          begin
            HandleNodeEditing(FFocusedNode, GetPreviousEditableColumn(FUpdateNodeColumn));
            Invalidate;
            Key := 0;
          end
          else
          begin
            n := GetPreviousFocusableNode(FFocusedNode);
            if Assigned(n)then
            begin
              ext := False;
              DoIsNodeExtended(n, ext);
              if (ext and Interaction.ExtendedEditable) or not ext then
              begin
                if not ext then
                  c := GetLastEditableColumn
                else
                  c := 0;

                FFocusedNode := n;
                SelectVirtualNode(n);
                ScrollToVirtualNode(n, True, tvnspTop);
                HandleNodeEditing(n, c);
                Invalidate;
                Key := 0;
              end;
            end;
          end;
        end
        else
        begin
          ext := False;
          DoIsNodeExtended(FFocusedNode, ext);
          if (FUpdateNodeColumn < GetLastEditableColumn) and not ext then
          begin
            EditVirtualNode(FFocusedNode, GetNextEditableColumn(FUpdateNodeColumn));
            Key := 0;
          end
          else
          begin
            n := GetNextFocusableNode(FFocusedNode);
            if Assigned(n) then
            begin
              ext := False;
              DoIsNodeExtended(n, ext);
              if (ext and Interaction.ExtendedEditable) or not ext then
              begin
                c := GetFirstEditableColumn;
                FFocusedNode := n;
                SelectVirtualNode(n);
                ScrollToVirtualNode(n, True, tvnspBottom);
                HandleNodeEditing(n, c);
                Invalidate;
                Key := 0;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.DrawBorders;
begin
end;

procedure TAdvCustomTreeView.DrawDisplay(ADisplay: TAdvTreeViewDisplayList);
var
  I: Integer;
  cache: TAdvTreeViewCacheItem;
  st: TAdvTreeViewCanvasSaveState;
  r: TRectF;
begin
  if ADisplay.Count = 0 then
    Exit;

  st := SaveStateEx(Canvas);
  if (ADisplay is TAdvTreeViewNodeDisplayList) then
  begin
    r := GetContentClipRect;
    r.Right := r.Right + 1;
  end
  else if ADisplay is TAdvTreeViewGroupsTopDisplayList then
  begin
    r := GetGroupsTopRect;
    r.Bottom := r.Bottom + 1;
  end
  else if ADisplay is TAdvTreeViewGroupsBottomDisplayList then
  begin
    r := GetGroupsBottomRect;
    r.Top := r.Top - 1;
  end
  else if ADisplay is TAdvTreeViewColumnsTopDisplayList then
  begin
    r := GetColumnsTopRect;
    r.Bottom := r.Bottom + 1;
  end
  else if ADisplay is TAdvTreeViewColumnsBottomDisplayList then
  begin
    r := GetColumnsBottomRect;
    r.Top := r.Top - 1;
  end;

  IntersectClipRectEx(Canvas, st, r);

  for I := 0 to ADisplay.Count - 1 do
  begin
    cache := ADisplay[I];
    if ADisplay is TAdvTreeViewNodeDisplayList then
      DrawNode(Canvas, cache.DrawRect, cache.Node)
    else if ADisplay is TAdvTreeViewColumnsDisplayList then
      DrawColumn(Canvas, cache.DrawRect, cache.Column, cache.Kind)
    else if ADisplay is TAdvTreeViewGroupsDisplayList then
      DrawGroup(Canvas, cache.DrawRect, cache.Group, cache.StartColumn, cache.EndColumn, cache.Kind)
  end;

  RestoreStateEx(st, Canvas);
end;

procedure TAdvCustomTreeView.DrawEmptySpaces;
begin
end;

procedure TAdvCustomTreeView.DrawGroup(ACanvas: TCanvas; ARect: TRectF; AGroup: Integer; AStartColumn, AEndColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
end;

procedure TAdvCustomTreeView.DrawGroups;
begin
  DrawDisplay(FGroupsTopDisplay);
  DrawDisplay(FGroupsBottomDisplay);
end;

procedure TAdvCustomTreeView.DrawNode(ACanvas: TCanvas; ARect: TRectF; ANode: TAdvTreeViewVirtualNode; ACaching: Boolean = False);
begin
end;

procedure TAdvCustomTreeView.DrawNodeColumns;
begin

end;

procedure TAdvCustomTreeView.DrawNodes;
begin
  DrawDisplay(FNodeDisplay);
end;

procedure TAdvCustomTreeView.DrawColumn(ACanvas: TCanvas; ARect: TRectF; AColumn: Integer; AKind: TAdvTreeViewCacheItemKind);
begin
end;

procedure TAdvCustomTreeView.DrawColumns;
begin
  DrawDisplay(FColumnsTopDisplay);
  DrawDisplay(FColumnsBottomDisplay);
end;

function TAdvCustomTreeView.DrawText(ACanvas: TCanvas; ARect: TRectF; AHorizontalAlign, AVerticalAlign: TAdvTreeViewTextAlign; AText: String; ATrimming: TAdvTreeViewTextTrimming = tvttNone; AAngle: Single = 0;
  AReverseAlignment: Boolean = True; ASupportHTML: Boolean = False; ATestAnchor: Boolean = False; AWordWrapping: Boolean = False; AX: Single = -1; AY: Single = - 1; AMinWidth: Single = -1; AMinHeight: Single = -1): String;
begin
end;

procedure TAdvCustomTreeView.EditNode(ANode: TAdvTreeViewNode; AColumn: Integer);
begin
  if Assigned(ANode) then
    EditVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode, AColumn);
end;

procedure TAdvCustomTreeView.EditVirtualNode(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
begin
  HandleNodeEditing(ANode, AColumn);
  Invalidate;
end;

function TAdvCustomTreeView.GetColumnBottomLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsBottomRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetColumnBottomRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsBottomRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right - 1;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetCacheHeight: Integer;
begin
  Result := CACHEHEIGHT;
end;

function TAdvCustomTreeView.GetCacheWidth: Integer;
begin
  Result := CACHEWIDTH
end;

function TAdvCustomTreeView.GetCalculationRect: TRectF;
begin
  Result := inherited GetCalculationRect;
  Result.Top := Result.Top + GetColumnsTopSize + GetGroupsTopSize;
  Result.Bottom := Result.Bottom - GetColumnsBottomSize - GetGroupsBottomSize;
  Result.Width := Max(0, Result.Width);
  Result.Height := Max(0, Result.Height);
end;

function TAdvCustomTreeView.GetContentCliprect: TRectF;
begin
  Result := inherited GetContentClipRect;
  Result.Height := Min(GetTotalRowHeight, Result.Height);
  Result.Width := Min(GetTotalColumnWidth, Result.Width);
  Result.Left := Result.Left + 1;
  Result.Right := Result.Right - 1;
end;

function TAdvCustomTreeView.GetContentRect: TRectF;
begin
  Result := inherited GetContentRect;
  Result.Top := Result.Top + GetColumnsTopSize + GetGroupsTopSize;
  Result.Bottom := Result.Bottom - GetColumnsBottomSize - GetGroupsBottomSize;
end;


function TAdvCustomTreeView.GetGroupBottomLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsBottomRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := nr.Bottom;
end;

function TAdvCustomTreeView.GetGroupBottomRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsBottomRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetGroupsBottomRect: TRectF;
begin
  Result := GetColumnsBottomRect;
  Result.Top := Result.Bottom;
  Result.Bottom := Result.Top + GetGroupsBottomSize
end;

function TAdvCustomTreeView.GetGroupsBottomSize: Double;
begin
  Result := 0;
  if (tglBottom in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (ColumnCount > 0) then
    Result := GroupsAppearance.BottomSize;
end;

function TAdvCustomTreeView.GetGroupsTopRect: TRectF;
begin
  Result := GetColumnsTopRect;
  Result.Bottom := Result.Top;
  Result.Top := Result.Top - GetGroupsTopSize;
end;

function TAdvCustomTreeView.GetGroupsTopSize: Double;
begin
  Result := 0;
  if (tglTop in GroupsAppearance.Layouts) and (DisplayGroups.Count > 0) and (ColumnCount > 0) then
    Result := GroupsAppearance.TopSize;
end;

function TAdvCustomTreeView.GetGroupText(AGroup: Integer): String;
begin
  Result := TranslateTextEx(sTMSFMXTreeViewGroup) + ' ' + inttostr(AGroup);
  if (AGroup >= 0) and (AGroup <= Groups.Count - 1) then
     Result := TAdvTreeViewGroupOpen(Groups[AGroup]).GetText;
end;

function TAdvCustomTreeView.GetGroupTopLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsTopRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetGroupTopRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetGroupsTopRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetInplaceEditor: TAdvTreeViewInplaceEditor;
begin
  Result := FInplaceEditor;
end;

function TAdvCustomTreeView.GetInplaceEditorRect(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer):{$IFDEF VCLLIB}TRect{$ENDIF}{$IFDEF FMXLIB}TRectF{$ENDIF};
var
  cr, r: TRectF;
begin
  {$IFDEF VCLLIB}
  Result := Rect(0, 0, 0, 0);
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := RectF(0, 0, 0, 0);
  {$ENDIF}
  if not Assigned(ANode) then
    Exit;
    
  if (AColumn >= 0) and (AColumn <= Length(ANode.TextRects) - 1) then
  begin
    r := ANode.TextRects[AColumn];
    {$IFDEF VCLLIB}
    InflateRectEx(r, -2, -2);
    {$ENDIF}
    cr := GetContentClipRect;
    r.Top := Max(r.Top, cr.Top);
    r.Bottom := Min(r.Bottom, cr.Bottom);
    r.Left := Max(r.Left, cr.Left);
    r.Right := Min(r.Right, cr.Right);
    {$IFDEF FMXLIB}
    Result := RectF(Floor(r.Left), Floor(r.Top), Round(r.Right), Round(r.Bottom));
    {$ENDIF}
    {$IFDEF VCLLIB}
    Result := Rect(Round(r.Left), Round(r.Top), Round(r.Right), Round(r.Bottom));
    {$ENDIF}

    if Assigned(OnGetInplaceEditorRect) then
      OnGetInplaceEditorRect(Self, ANode, AColumn, FInplaceEditor, Result);
  end;
end;

function TAdvCustomTreeView.GetColumnsBottomRect: TRectF;
var
  nr: TRectF;
begin
  nr := inherited GetContentRect;
  Result.Left := nr.Left;
  Result.Top := nr.Bottom - GetGroupsBottomSize - GetColumnsBottomSize;
  Result.Right := nr.Right;
  Result.Bottom := nr.Bottom - GetGroupsBottomSize;
end;

function TAdvCustomTreeView.GetColumnsBottomSize: Double;
begin
  Result := 0;
  if (tclBottom in ColumnsAppearance.Layouts) and (ColumnCount > 0) then
    Result := ColumnsAppearance.BottomSize;
end;

function TAdvCustomTreeView.GetColumnTopLeftEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsTopRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := nr.Left;
  Result.Right := ptr.Left;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetColumnTopRightEmptyRect: TRectF;
var
  ptr, nr: TRectF;
begin
  ptr := GetColumnsTopRect;
  nr := GetDrawRect;
  Result.Top := ptr.Top;
  Result.Left := ptr.Right;
  Result.Right := nr.Right - 1;
  Result.Bottom := ptr.Bottom;
end;

function TAdvCustomTreeView.GetVersion: string;
begin
end;

function TAdvCustomTreeView.GetColumnsTopRect: TRectF;
var
  nr: TRectF;
begin
  nr := inherited GetContentRect;
  Result.Left := nr.Left;
  Result.Top := nr.Top + GetGroupsTopSize;
  Result.Right := nr.Right;
  Result.Bottom := nr.Top + GetGroupsTopSize + GetColumnsTopSize;
end;

function TAdvCustomTreeView.GetColumnsTopSize: Double;
begin
  Result := 0;
  if (tclTop in ColumnsAppearance.Layouts) and (ColumnCount > 0) then
    Result := ColumnsAppearance.TopSize;
end;

function TAdvCustomTreeView.GetColumnText(AColumn: Integer): String;
begin
  Result := TranslateTextEx(sTMSFMXTreeViewColumn) + ' ' + inttostr(AColumn);
  if (AColumn >= 0) and (AColumn <= ColumnCount - 1) then
  begin
    if (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
    begin
      Result := Columns[AColumn].Text;
      if Result = '' then
        Result := Columns[AColumn].Name;
    end;

    if Result = '' then
      Result := TranslateTextEx(sTMSFMXTreeViewColumn) + ' ' + inttostr(AColumn);
  end;
end;

function TAdvCustomTreeView.GetPreviousFocusableNode(ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  en: Boolean;
begin
  if Assigned(ANode) then
    Result := GetPreviousVirtualNode(ANode)
  else
    Result := GetLastVirtualNode;

  en := IsVirtualNodeSelectable(Result);

  while Assigned(Result) and not en do
  begin
    Result := GetPreviousVirtualNode(Result);
    en := IsVirtualNodeSelectable(Result);
  end;
end;

function TAdvCustomTreeView.GetNodeForRow(
  ARow: Integer): TAdvTreeViewVirtualNode;
begin
  Result := nil;
  if (ARow >= 0) and (ARow <= VisibleNodes.Count - 1) then
    Result := VisibleNodes[ARow];
end;

function TAdvCustomTreeView.GetNextFocusableNode(
  ANode: TAdvTreeViewVirtualNode): TAdvTreeViewVirtualNode;
var
  en: Boolean;
begin
  if Assigned(ANode) then
    Result := GetNextVirtualNode(ANode)
  else
    Result := GetFirstRootVirtualNode;

  en := IsVirtualNodeSelectable(Result);

  while Assigned(Result) and not en do
  begin
    Result := GetNextVirtualNode(Result);
    en := IsVirtualNodeSelectable(Result);
  end;
end;

function TAdvCustomTreeView.GetRowHeight(ARow: Integer): Double;
var
  v: TAdvTreeViewVirtualNode;
begin
  Result := DefaultRowHeight;
  v := GetNodeForRow(ARow);
  if Assigned(v) and (v.Calculated) then
    Result := v.Height;
end;

procedure TAdvCustomTreeView.HorizontalScrollPositionChanged;
begin
  BlockScrollingUpdate := True;
  UpdateDisplay;
  BlockScrollingUpdate := False;
  DoHScroll(GetHScrollValue);
end;

procedure TAdvCustomTreeView.InitSample;
var
  pAudi, pMercedes, pSub, n: TAdvTreeViewNode;
begin
  BeginUpdate(True);
  Columns.Clear;
  Nodes.Clear;

  Columns.Add.Text := 'Model';
  Columns.Add.Text := 'Year';
  Columns.Add.Text := 'Miles';

  pAudi := AddNode;
  pAudi.Text[0] := 'Audi';
  pAudi.Extended := True;

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A3';
  pSub.Text[1] := '2010';
  pSub.Text[2] := '32,300';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A5 series';

  n := AddNode(pSub);
  n.Text[0] := 'S5';
  n.Text[1] := '2015';
  n.Text[2] := '40,000';

  n := AddNode(pSub);
  n.Text[0] := 'RS5';
  n.Text[1] := '2012';
  n.Text[2] := '15,000';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A8';
  pSub.Text[1] := '2005';
  pSub.Text[2] := '80,000';

  pMercedes := AddNode;
  pMercedes.Text[0] := 'Mercedes';
  pMercedes.Extended := True;

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLS';
  pSub.Text[1] := '2000';
  pSub.Text[2] := '300,000';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLK';
  pSub.Text[1] := '2010';
  pSub.Text[2] := '20,000';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'GLA';
  pSub.Text[1] := '2012';
  pSub.Text[2] := '14,500';

  ExpandAll;
  EndUpdate;
end;

procedure TAdvCustomTreeView.HandleKeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  sel, prevsel, fsel: TAdvTreeViewVirtualNode;
  spc, donext, doprev: Boolean;
  I: Integer;
  r: Integer;
begin
  inherited;
  if (ssCtrl in Shift) and (Key = Ord('A')) then
  begin
    SelectAllNodes;
    Exit;
  end;

  sel := FocusedVirtualNode;
  donext := False;
  doprev := False;
  case Key of
    KEY_PRIOR:
    begin
      doprev := True;
      if Assigned(sel) then
      begin
        r := sel.Row - (StopRow - StartRow);
        sel := GetNodeFromNodeStructure(r);
        if Assigned(sel) then
        begin
          if not IsVirtualNodeSelectable(sel) then
            sel := GetPreviousFocusableNode(sel);        
        end;

        if not Assigned(sel) then
          sel := GetNextFocusableNode(nil);          
      end;
    end;
    KEY_NEXT:
    begin
      donext := True;
      if Assigned(sel) then
      begin
        r := sel.Row + (StopRow - StartRow);
        sel := GetNodeFromNodeStructure(r);
        if Assigned(sel) then
        begin
          if not IsVirtualNodeSelectable(sel) then
            sel := GetNextFocusableNode(sel);
        end;        

        if not Assigned(sel) then
          sel := GetPreviousFocusableNode(nil);
      end;
    end;
    KEY_HOME:
    begin
      sel := GetNextFocusableNode(nil);
      doprev := True;
    end;
    KEY_END:
    begin
      sel := GetPreviousFocusableNode(nil);
      doprev := True
    end;
    KEY_DOWN:
    begin
      sel := GetNextFocusableNode(sel);
      donext := True;
    end;
    KEY_UP:
    begin
      sel := GetPreviousFocusableNode(sel);
      doprev := True;
    end;
    KEY_LEFT: HandleNodeCollapse(sel);
    KEY_RIGHT: HandleNodeExpand(sel);
  end;

  spc := False;
  if (Key = KEY_SPACE) or ((Key = 0) and (KeyChar = ' ')) then
    spc := True;

  if Assigned(sel) then
  begin
    if (Interaction.SelectionFollowsFocus and (donext or doprev)) or (spc and (ssCtrl in Shift)) then
    begin
      if (ssShift in Shift) then
      begin
        if (FSelectedNodes.Count > 1) then
        begin
          prevsel := FSelectedNodes[FSelectedNodes.Count - 1];
          fsel := FSelectedNodes[0];
          if (doprev and (sel.Row < prevsel.Row) and (sel.Row >= fsel.Row)) or (donext and (sel.Row > prevsel.Row) and (sel.Row <= fsel.Row)) then
          begin
            if prevsel.Row <= sel.Row then
            begin
              for I := prevsel.Row to sel.Row do
                UnSelectVirtualNode(GetNodeFromNodeStructure(I));
            end
            else
            begin
              for I := prevsel.Row downto sel.Row do
                UnSelectVirtualNode(GetNodeFromNodeStructure(I));
            end;
          end;
        end;
      end
      else if not (ssCtrl in Shift) and (donext or doprev) and (FSelectedNodes.Count > 0) then
        UnSelectVirtualNode(sel);

      if (ssShift in Shift) then
      begin
        if FFocusedNode.Row <= sel.Row then
        begin
          for I := FFocusedNode.Row + 1 to sel.Row do
            HandleSelectNode(GetNodeFromNodeStructure(I), True, True, {$IFNDEF DELPHI_LLVM}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
        end
        else
        begin
          for I := FFocusedNode.Row - 1 downto sel.Row do
            HandleSelectNode(GetNodeFromNodeStructure(I), True, True, {$IFNDEF DELPHI_LLVM}(ssShift in Shift) and {$ENDIF} Interaction.MultiSelect);
        end;
      end
      else
         HandleSelectNode(sel, True, True, {$IFNDEF DELPHI_LLVM}(ssCtrl in Shift) and {$ENDIF} Interaction.MultiSelect);
    end
    else if spc then
      HandleNodeToggleCheck(sel, 0);

    FFocusedNode := sel;
    if donext then
      ScrollToVirtualNode(FFocusedNode, True, tvnspBottom)
    else
      ScrollToVirtualNode(FFocusedNode, True, tvnspTop);
    Invalidate;
  end;
end;

procedure TAdvCustomTreeView.HandleKeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  en, ext: Boolean;
begin
  inherited;
  if FCloseWithDialogKey then
  begin
    FCloseWithDialogKey := False;
    Exit;
  end;

  if Assigned(FFocusedNode) then
  begin
    if Interaction.KeyboardEdit and not Interaction.ReadOnly and (Shift = []) then
    begin
      case Key of
        KEY_F2, KEY_RETURN:
        begin
          en := True;
          DoIsNodeEnabled(FFocusedNode, en);
          if en then
          begin
            ext := False;
            DoIsNodeExtended(FFocusedNode, ext);
            if (ext and Interaction.ExtendedSelectable and Interaction.ExtendedEditable) or not ext then
              HandleNodeEditing(FFocusedNode, GetFirstEditableColumn);
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.Paint;
var
  st: TAdvTreeViewCanvasSaveState;
begin
  inherited;
  DrawEmptySpaces;
  st := SaveStateEx(Canvas);
  IntersectClipRectEx(Canvas, st, GetContentClipRect);
  DrawNodeColumns;
  RestoreStateEx(st, Canvas);
  DrawNodes;
  DrawColumns;
  DrawGroups;
  DrawBorders;
end;

procedure TAdvCustomTreeView.ResetNodes(AUpdateAll: Boolean = True);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
  st, stp: Integer;
begin
  if Assigned(VisibleNodes) then
  begin
    if AUpdateAll then
    begin
      st := 0;
      stp := RowCount - 1;
    end
    else
    begin
      st := StartRow;
      stp := StopRow;
    end;

    for I := st to stp do
    begin
      v := GetNodeForRow(I);
      if Assigned(v) then
      begin
        TotalRowHeight := TotalRowHeight - v.Height + DefaultRowHeight;
        UpdateNodeHeight(v, DefaultRowHeight);
        UpdateNodeCalculated(v, False);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.SetInteraction(const Value: TAdvTreeViewInteraction);
begin
  if FInteraction <> Value then
    FInteraction.Assign(Value);
end;

procedure TAdvCustomTreeView.SetNodesAppearance(const Value: TAdvTreeViewNodesAppearance);
begin
  if FNodesAppearance <> Value then
    FNodesAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetGroupsAppearance(
  const Value: TAdvTreeViewGroupsAppearance);
begin
  if FGroupsAppearance <> Value then
    FGroupsAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetColumnsAppearance(
  const Value: TAdvTreeViewColumnsAppearance);
begin
  if FColumnsAppearance <> Value then
    FColumnsAppearance.Assign(Value);
end;

procedure TAdvCustomTreeView.SetColumnStroke(const Value: TAdvTreeViewStrokeBrush);
begin
  if FColumnStroke <> Value then
    FColumnStroke.Assign(Value);
end;

procedure TAdvCustomTreeView.SetFocusedNode(
  const Value: TAdvTreeViewNode);
begin
  if Assigned(Value) then
    FocusedVirtualNode := TAdvTreeViewNodeOpen(Value).VirtualNode;
end;

procedure TAdvCustomTreeView.SetFocusedVirtualNode(
  const Value: TAdvTreeViewVirtualNode);
begin
  FFocusedNode := Value;
end;

procedure TAdvCustomTreeView.SetVersion(const Value: string);
begin

end;

procedure TAdvCustomTreeView.StopAnimationTimer;
begin
  FAnimateTimer.Enabled := False;
  FAnimating := False;
end;

procedure TAdvCustomTreeView.StopEditing;
begin
  if FInplaceEditorActive then
    CloseInplaceEditor(False);
end;

procedure TAdvCustomTreeView.UnSelectAllNodes;
begin
  UnSelectAllVirtualNodes;
end;

procedure TAdvCustomTreeView.UnSelectAllVirtualNodes;
begin
  FSelectedNodes.Clear;
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectNode(ANode: TAdvTreeViewNode);
begin
  if Assigned(ANode) then
    UnSelectVirtualNode(TAdvTreeViewNodeOpen(ANode).VirtualNode);
end;

procedure TAdvCustomTreeView.UnSelectNodes(ANodes: TAdvTreeViewNodeArray);
var
  I: Integer;
  v: TAdvTreeViewNode;
begin
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    if Assigned(v) then
      FSelectedNodes.Remove(TAdvTreeViewNodeOpen(v).VirtualNode);
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectVirtualNode(
  ANode: TAdvTreeViewVirtualNode);
begin
  FSelectedNodes.Remove(ANode);
  Invalidate;
end;

procedure TAdvCustomTreeView.UnSelectVirtualNodes(
  ANodes: TAdvTreeViewVirtualNodeArray);
var
  I: Integer;
  v: TAdvTreeViewVirtualNode;
begin
  for I := 0 to Length(ANodes) - 1 do
  begin
    v := ANodes[I];
    FSelectedNodes.Remove(v);
  end;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateAutoSizing;
begin
  if ColumnsAppearance.Stretch then
    StretchColumn(ColumnsAppearance.StretchAll, ColumnsAppearance.StretchColumn);
end;

procedure TAdvCustomTreeView.UpdateCalculations;
begin
  if (UpdateCount > 0) or (csDestroying in ComponentState) or (csLoading in ComponentState) then
    Exit;

  ColumnCount := Columns.Count;

  if NodesAppearance.HeightMode = tnhmFixed then
    DefaultRowHeight := NodesAppearance.FixedHeight
  else
    DefaultRowHeight := NodesAppearance.VariableMinimumHeight;

  if (UpdateCount = 0) and not BlockUpdateNodeList and not ((csDestroying in ComponentState) or (csReading in ComponentState) or (csLoading in ComponentState)) then
    BuildNodeList;
end;

procedure TAdvCustomTreeView.UpdateColumnRowCalculations(AUpdateTotalRowHeight: Boolean = True);
var
  I: Integer;
  r, c: Double;
begin
  ColumnP.Clear;
  c := 0;
  ColumnPositions[0] := c;
  for I := 0 to ColumnCount - 1 do
  begin
    c := c + ColumnWidths[I];
    ColumnPositions[I + 1] := c;
  end;

  TotalColumnWidth := c;

  if AUpdateTotalRowHeight then
  begin
    r := RowCount * DefaultRowHeight;
    TotalRowHeight := r + 1;
  end;
end;

procedure TAdvCustomTreeView.UpdateDisplay;
begin
  inherited;
  UpdateNodesCache(False);
  UpdateGroupsDisplay;
  UpdateColumnsDisplay;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateNodesCache(AUpdateNodes: Boolean = True; AResetNodes: Boolean = False);
begin
  if BlockUpdateNode then
    Exit;

  UpdateScrollBars(True, False);
  UpdateVisualRange;
  UpdateNodeCache;
  UpdateScrollBars(True, False);

  {$IFDEF FMXLIB}
  FScrollBarTimer.Enabled := True;
  {$ENDIF}

  if AUpdateNodes then
  begin
    if AResetNodes then
      ResetNodes(False);

    UpdateAutoSizing;
    UpdateColumnRowCalculations(False);
    UpdateNodesCache(False);
    UpdateVisualRange;
  end
  else
  begin
    UpdateNodeDisplay;
    UpdateInplaceEditorPosition;
  end;
end;

procedure TAdvCustomTreeView.UpdateTreeViewCache;
begin
  if (UpdateCount > 0) then
    Exit;

  inherited;
  Invalidate;
end;

procedure TAdvCustomTreeView.UpdateColumnsCache;
begin
  UpdateColumnCache(FColumnsTopCache);
  UpdateColumnCache(FColumnsBottomCache);
  UpdateColumnsDisplay;
end;

procedure TAdvCustomTreeView.UpdateColumnCache(ACache: TAdvTreeViewCache);
var
  w, h, bmpw, bmph, bw, bh: Double;
  rt: TRectF;
  c: Integer;
  dx: Double;
  offsetx: Double;
  ac: Integer;
  rc: TRectF;
  cnt: Integer;
begin
  inherited;
  if (UpdateCount > 0) or (csDestroying in ComponentState) or not Assigned(ACache) then
    Exit;

  ACache.Clear;

  if (ACache is TAdvTreeViewColumnsTopCache) and (not (tclTop in ColumnsAppearance.Layouts) or (ColumnsAppearance.TopSize <= 0)) then
    Exit;

  if (ACache is TAdvTreeViewColumnsBottomCache) and (not (tclBottom in ColumnsAppearance.Layouts) or (ColumnsAppearance.BottomSize <= 0)) then
    Exit;

  w := GetTotalColumnWidth;
  if ACache is TAdvTreeViewColumnsTopCache then
    h := GetColumnsTopSize + 1
  else
    h := GetColumnsBottomSize + 1;

  offsetx := 0;
  bmpw := 0;
  bmph := 0;
  ac := 0;
  c := 0;
  dx := 0;

  cnt := ColumnCount - 1;

  while bmpw < w do
  begin
    bw := w;

    while bmph < h do
    begin
      bh := h;

      rt := RectF(0, 0, bw, bh);

      dx := rt.Left + offsetx;

      for c := ac to cnt do
      begin
        rc.Top := int(rt.Top) + 0.5;
        rc.Bottom := int(rt.Bottom) - 0.5;
        rc.Left := int(dx) + 0.5;
        dx := dx + ColumnWidths[c];
        rc.Right := int(dx) + 0.5;

        OffsetRectEx(rc, bmpw, bmph);
        if ACache is TAdvTreeViewColumnsTopCache then
          ACache.Add(TAdvTreeViewCacheItem.CreateColumnTop(rc, c))
        else
          ACache.Add(TAdvTreeViewCacheItem.CreateColumnBottom(rc, c));

        if dx > rt.Width then
          Break;
      end;

      bmph := bmph + bh;
    end;
    bmpw := bmpw + bw;
    bmph := 0;
    ac := c;
    offsetx := -ColumnWidths[c] - (bw - dx);
  end;
end;

procedure TAdvCustomTreeView.UpdateColumnsDisplay;
begin
  BuildDisplay(FColumnsTopCache, FColumnsTopDisplay);
  BuildDisplay(FColumnsBottomCache, FColumnsBottomDisplay);
end;

procedure TAdvCustomTreeView.VerticalScrollPositionChanged;
begin
  BlockScrollingUpdate := True;
  UpdateDisplay;
  BlockScrollingUpdate := False;
  DoVScroll(GetVScrollValue);
end;

function TAdvCustomTreeView.XYToCacheItem(X,
  Y: Double): TAdvTreeViewCacheItem;
var
  I: Integer;
  dsp: TAdvTreeViewCacheItem;
begin
  Result := nil;
  if not Assigned(FNodeDisplay) then
    Exit;

  if not PtInRectEx(GetContentClipRect, PointF(X, Y)) then
    Exit;

  for I := 0 to FNodeDisplay.Count - 1 do
  begin
    dsp := FNodeDisplay[I];
    if PtInRectEx(dsp.DrawRect, PointF(X, Y)) then
    begin
      Result := dsp;
      Break;
    end;
  end;
end;

function TAdvCustomTreeView.XYToColumnSize(X,
  Y: Single): Integer;
var
  I: Integer;
  topr: TRectF;
  r: TRectF;
  colw, colp: Double;
begin
  Result := -1;
  topr := GetColumnsTopRect;
  topr.Left := topr.Left - GetHorizontalScrollPosition;
  for I := ColumnCount - 1 downto 0 do
  begin
    colw := ColumnWidths[I];
    if colw >= 0 then
    begin
      colp := ColumnPositions[I];
      r := RectF(topr.Left + colp + colw - 4, topr.Top, topr.Left + colp + colw + 4, topr.Bottom);
      if PtInRectEx(r, PointF(X, Y)) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeAnchor(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): TAdvTreeViewNodeAnchor;
var
  I: Integer;
  txtr: TRectF;
  a: String;
  str: String;
  bmp: TBitmap;
  c: TAdvTreeViewColumn;
  ha, va: TAdvTreeViewTextAlign;
  ww: Boolean;
  trim: TAdvTreeViewTextTrimming;
begin
  Result.AAnchor := '';
  Result.AColumn := -1;
  if Assigned(ANode) then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      if (I >= 0) and (I <= Length(ANode.TextRects) - 1) then
      begin
        txtr := ANode.TextRects[I];
        InflateRectEx(txtr, -2, -2);
        if PtInRectEx(txtr, PointF(X, Y)) then
        begin
          c := nil;
          if (I <= Columns.Count - 1) then
            c := Columns[I];

          ha := tvtaLeading;
          va := tvtaCenter;
          ww := False;
          trim := tvttNone;
          if Assigned(c) then
          begin
            ha := c.HorizontalTextAlign;
            va := c.VerticalTextAlign;
            ww := c.WordWrapping;
            trim := c.Trimming;
          end;

          DoGetNodeTrimming(ANode, I, trim);
          DoGetNodeWordWrapping(ANode, I, ww);
          DoGetNodeHorizontalTextAlign(ANode, I, ha);
          DoGetNodeVerticalTextAlign(ANode, I,va);

          str := '';
          DoGetNodeText(ANode, I, tntmDrawing, str);

          {$IFDEF FMXLIB}
          bmp := TBitmap.Create(1, 1);
          {$ENDIF}
          {$IFDEF VCLLIB}
          bmp := TBitmap.Create;
          {$ENDIF}
          a := DrawText(bmp.Canvas, txtr, ha, va, str, trim, 0, False, True, True, ww, X, Y);
          bmp.Free;
          if a <> '' then
          begin
            Result.AAnchor := a;
            Result.AColumn := I;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeCheck(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): TAdvTreeViewNodeCheck;
var
  nrt: TRectF;
  en: Boolean;
  I: Integer;
begin
  Result.AColumn := -1;
  Result.ANode := nil;
  en := True;
  DoIsNodeEnabled(ANode, en);
  if Assigned(ANode) and en then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      if (I >= 0) and (I <= Length(ANode.CheckRects) - 1) then
      begin
        nrt := ANode.CheckRects[I];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result.ANode := ANode;
          Result.AColumn := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeExpand(ANode: TAdvTreeViewVirtualNode; X,
  Y: Single): Boolean;
var
  nrt: TRectF;
  en: Boolean;
begin
  Result := False;
  if Assigned(ANode) and (ANode.Children > 0) then
  begin
    en := True;
    DoIsNodeEnabled(ANode, en);
    if en then
    begin
      if (NodesAppearance.ExpandColumn >= 0) and (NodesAppearance.ExpandColumn <= Length(ANode.ExpandRects) - 1) then
      begin
        nrt := ANode.ExpandRects[NodesAppearance.ExpandColumn];
        Result := PtInRectEx(nrt, PointF(X, Y));
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNodeTextColumn(ANode: TAdvTreeViewVirtualNode;
  X, Y: Single): Integer;
var
  nrt: TRectF;
  I: Integer;
begin
  Result := -1;
  if Assigned(ANode) then
  begin
    for I := 0 to ColumnCount - 1 do
    begin
      if (I >= 0) and (I <= Length(ANode.TextRects) - 1) then
      begin
        nrt := ANode.TextRects[I];
        if PtInRectEx(nrt, PointF(X, Y)) then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
end;

function TAdvCustomTreeView.XYToNode(X, Y: Double): TAdvTreeViewVirtualNode;
var
  dsp: TAdvTreeViewCacheItem;
begin
  Result := nil;
  dsp := XYToCacheItem(X, Y);
  if Assigned(dsp) then
    Result := dsp.Node;
end;

procedure TAdvCustomTreeView.HandleNodeEditing(
  ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  c: TAdvTreeViewColumn;
  ins: Boolean;
  {$IFNDEF LCLLIB}
  AContext: TRttiContext;
  rt: TRttiType;
  propt: TRttiProperty;
  {$ENDIF}
  n: String;
  {$IFDEF FMXLIB}
  r: TRectF;
  trans: Boolean;
  {$ENDIF}
  {$IFDEF VCLLIB}
  r: TRect;
  {$ENDIF}
begin
  if Assigned(ANode) and (AColumn >= 0) and (AColumn <= Columns.Count - 1) then
  begin
    c := Columns[AColumn];
    FInplaceEditorClass := nil;
    if not c.CustomEditor then
    begin
      case c.EditorType of
        tcetEdit: FInplaceEditorClass := TEdit;
        tcetComboBox: FInplaceEditorClass := TComboBox;
        tcetMemo: FInplaceEditorClass := TMemo;
        tcetNone: Exit;
      end;
    end;
    
    ins := True;
    DoBeforeOpenInplaceEditor(ANode, AColumn, ins);
    if ins then
    begin
      {$IFDEF FMXLIB}
      trans := True;
      DoGetInplaceEditor(ANode, AColumn, trans, FInplaceEditorClass);
      {$ENDIF}
      {$IFDEF VCLLIB}
      DoGetInplaceEditor(ANode, AColumn, FInplaceEditorClass);
      {$ENDIF}

      if Assigned(FInplaceEditor) then
      begin
        {$IFDEF FMXLIB}
        FInplaceEditor.Release;
        {$ENDIF}
        {$IFDEF VCLLIB}
        FInplaceEditor.Free;
        {$ENDIF}
        FInplaceEditor := nil;
      end;

      if Assigned(FInplaceEditorClass) then
        FInplaceEditor := FInplaceEditorClass.Create(Self)
      else
        FInplaceEditor := TEdit.Create(Self);

      r := GetInplaceEditorRect(ANode, AColumn);
      if Assigned(FInplaceEditor) then
      begin
        FUpdateNode := ANode;
        FUpdateNodeColumn := AColumn;
        {$IFDEF FMXLIB}
        if trans then
        begin
          FInplaceEditor.DisableFocusEffect := True;
          FInplaceEditor.OnApplyStyleLookup := ApplyInplaceEditorStyleLookup;
        end;
        {$ENDIF}

        FInplaceEditor.Parent := Self;
        FInplaceEditor.Visible := False;
        FInplaceEditor.BoundsRect := r;
        {$IFDEF FMXLIB}
        FInplaceEditor.BoundsRect := RectF(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) / 2 + FInplaceEditor.Height);
        {$ENDIF}
        {$IFDEF VCLLIB}
        FInplaceEditor.BoundsRect := Rect(r.Left, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) div 2, r.Right, r.Top + (r.Bottom - r.Top - FInplaceEditor.Height) div 2 + FInplaceEditor.Height);
        {$ENDIF}
        FInplaceEditor.Visible := True;

        if (c.EditorType = tcetComboBox) and not c.CustomEditor and (FInplaceEditor is TComboBox) then
        begin
          (FInplaceEditor as TComboBox).Items.Assign(c.EditorItems);
      	  {$IFDEF VCLLIB}
          (FInplaceEditor as TComboBox).Style := csDropDownList;
      	  {$ENDIF}
      	end;

        CustomizeInplaceEditor(FInplaceEditor, ANode, c);

        {$IFNDEF LCLLIB}
        AContext := TRttiContext.Create;
        {$ENDIF}
        try
          {$IFNDEF LCLLIB}
          rt := AContext.GetType(FInplaceEditor.ClassInfo);
          {$ENDIF}
          n := '';
          DoGetNodeText(ANode, AColumn, tntmEditing, n);
          {$IFNDEF LCLLIB}
          propt := rt.GetProperty('Text');
          if Assigned(propt) then
            propt.SetValue(FInplaceEditor, n)
          else
          {$ENDIF}
          begin
            if (FInplaceEditor is TComboBox) then
              (FInplaceEditor as TComboBox).ItemIndex := (FInplaceEditor as TComboBox).Items.IndexOf(n);
          end;
        finally
          {$IFNDEF LCLLIB}
          AContext.Free;
          {$ENDIF}
        end;

        FInplaceEditor.SetFocus;
        if FInplaceEditor is TEdit then
          (FInplaceEditor as TEdit).SelStart := Length((FInplaceEditor as TEdit).Text);
        FInplaceEditorActive := True;
        Invalidate;
      end;

      DoAfterOpenInplaceEditor(ANode, AColumn, FInplaceEditor, r);
    end; 
  end;                      
end;

procedure TAdvCustomTreeView.HandleNodeExpand(ANode: TAdvTreeViewVirtualNode);
var
  e: Boolean;
begin
  if Assigned(ANode) then
  begin
    if not ANode.Expanded and (ANode.Children > 0) then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeExpandNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterExpandNode(ANode);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleNodeCollapse(ANode: TAdvTreeViewVirtualNode);
var
  e: Boolean;
begin
  if Assigned(ANode) then
  begin
    if ANode.Expanded and (ANode.Children > 0) then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeCollapseNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterCollapseNode(ANode);
      end;
    end
  end;
end;

procedure TAdvCustomTreeView.HandleNodeToggle(ANode: TAdvTreeViewVirtualNode);
var
  e: Boolean;
begin
  if Assigned(ANode) and (ANode.Children > 0) then
  begin
    if ANode.Expanded then
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeCollapseNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterCollapseNode(ANode);
      end;
    end
    else
    begin
      e := True;
      BlockUpdateNode := True;
      DoBeforeExpandNode(ANode, e);
      BlockUpdateNode := False;
      if e then
      begin
        ToggleNodeInternal(ANode);
        DoAfterExpandNode(ANode);
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleNodeToggleCheck(ANode: TAdvTreeViewVirtualNode; AColumn: Integer);
var
  chk, e: Boolean;
  chkt: TAdvTreeViewNodeCheckType;
begin
  if Assigned(ANode) then
  begin
    if (AColumn >= 0) and (AColumn <= Length(ANode.CheckStates) - 1) then
    begin
      chk := ANode.CheckStates[AColumn];
      if chk then
      begin
        chkt := tvntNone;
        DoGetNodeCheckType(ANode, AColumn, chkt);
        if chkt = tvntCheckBox then
        begin
          e := True;
          DoBeforeUnCheckNode(ANode, AColumn, e);
          if e then
          begin
            ToggleCheckNodeInternal(ANode, AColumn);
            DoAfterUnCheckNode(ANode, AColumn);
          end;
        end;
      end
      else
      begin
        e := True;
        DoBeforeCheckNode(ANode, AColumn, e);
        if e then
        begin
          ToggleCheckNodeInternal(ANode, AColumn);
          DoAfterCheckNode(ANode, AColumn);
        end;
      end;
    end;
  end;
end;

procedure TAdvCustomTreeView.HandleSelectNode(ANode: TAdvTreeViewVirtualNode;
  ATriggerEvents: Boolean; AKeyBoard: Boolean; AMultiSelect: Boolean);
var
  I: Integer;
  it: TAdvTreeViewVirtualNode;
  en: Boolean;
begin
  if not AMultiSelect then
  begin
    for I := FSelectedNodes.Count - 1 downto 0 do
    begin
      it := FSelectedNodes[I];
      if it <> ANode then
      begin
        en := True;
        if ATriggerEvents then
          DoBeforeUnSelectNode(it, en);

        if en then
        begin
          FSelectedNodes.Remove(it);
          if ATriggerEvents then
            DoAfterUnSelectNode(it);
        end;
      end;
    end;
  end;

  if Assigned(ANode) then
  begin
    if FSelectedNodes.Contains(ANode) then
    begin
      en := True;
      if ATriggerEvents then
        DoBeforeUnSelectNode(ANode, en);
      if en then
      begin
        FSelectedNodes.Remove(ANode);
        if ATriggerEvents then
          DoAfterUnSelectNode(ANode);
      end;
    end
    else
    begin
      en := True;
      if ATriggerEvents then
        DoBeforeSelectNode(ANode, en);

      if en then
      begin
        FSelectedNodes.Add(ANode);
        if ATriggerEvents then
        begin
          DoAfterSelectNode(ANode);
          if not AKeyBoard then
            DoNodeClick(ANode);
        end;
      end;
    end;
  end;
end;

{ TAdvTreeViewNodesAppearance }

procedure TAdvTreeViewNodesAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewNodesAppearance then
  begin
    FFill.Assign((Source as TAdvTreeViewNodesAppearance).Fill);
    FStroke.Assign((Source as TAdvTreeViewNodesAppearance).Stroke);
    FFont.Assign((Source as TAdvTreeViewNodesAppearance).Font);
    FSelectedFill.Assign((Source as TAdvTreeViewNodesAppearance).SelectedFill);
    FSelectedStroke.Assign((Source as TAdvTreeViewNodesAppearance).SelectedStroke);
    FDisabledFill.Assign((Source as TAdvTreeViewNodesAppearance).DisabledFill);
    FDisabledStroke.Assign((Source as TAdvTreeViewNodesAppearance).DisabledStroke);
    FExtendedFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedFill);
    FExtendedStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedStroke);
    FExtendedFont.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedFont);
    FExtendedSelectedFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedSelectedFill);
    FExtendedSelectedStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedSelectedStroke);
    FExtendedDisabledFill.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedDisabledFill);
    FExtendedDisabledStroke.Assign((Source as TAdvTreeViewNodesAppearance).ExtendedDisabledStroke);
    FExpandColumn := (Source as TAdvTreeViewNodesAppearance).ExpandColumn;
    FExpandWidth := (Source as TAdvTreeViewNodesAppearance).ExpandWidth;
    FExpandHeight := (Source as TAdvTreeViewNodesAppearance).ExpandHeight;
    FFixedHeight := (Source as TAdvTreeViewNodesAppearance).FixedHeight;
    FVariableMinimumHeight := (Source as TAdvTreeViewNodesAppearance).VariableMinimumHeight;
    FFontColor := (Source as TAdvTreeViewNodesAppearance).FontColor;
    FSelectedFontColor := (Source as TAdvTreeViewNodesAppearance).SelectedFontColor;
    FDisabledFontColor := (Source as TAdvTreeViewNodesAppearance).DisabledFontColor;
    FExtendedFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedFontColor;
    FExtendedSelectedFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedSelectedFontColor;
    FExtendedDisabledFontColor := (Source as TAdvTreeViewNodesAppearance).ExtendedDisabledFontColor;
    FLevelIndent := (Source as TAdvTreeViewNodesAppearance).LevelIndent;
    FHeightMode := (Source as TAdvTreeViewNodesAppearance).HeightMode;
    FShowLines := (Source as TAdvTreeViewNodesAppearance).ShowLines;
    FShowFocus := (Source as TAdvTreeViewNodesAppearance).ShowFocus;
    FColumnStroke.Assign((Source as TAdvTreeViewNodesAppearance).ColumnStroke);
    FLineStroke.Assign((Source as TAdvTreeViewNodesAppearance).LineStroke);
  end;
end;

procedure TAdvTreeViewNodesAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewNodesAppearance.BitmapChanged(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

constructor TAdvTreeViewNodesAppearance.Create(ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FShowFocus := False;
  FSelectionArea := tsaFromText;
  FTextHorizontalTextAlign := tvtaLeading;
  FExpandColumn := 0;
  FExpandWidth := 15;
  FExpandHeight := 15;
  FFixedHeight := 25;
  FVariableMinimumHeight := 25;
  FShowLines := True;
  FHeightMode := tnhmFixed;
  FLevelIndent := 20;
  FTextVerticalTextAlign := tvtaCenter;
  FFontColor := TAdvTreeViewColorGray;
  FSelectedFontColor := TAdvTreeViewColorWhite;
  FDisabledFontColor := TAdvTreeViewColorSilver;

  FExtendedFontColor := TAdvTreeViewColorGray;
  FExtendedSelectedFontColor := TAdvTreeViewColorWhite;
  FExtendedDisabledFontColor := TAdvTreeViewColorSilver;

  FColumnStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorNull);
  FLineStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  {$IFDEF FMXLIB}
  FLineStroke.Dash := TStrokeDash.Dot;
  {$ENDIF}
  {$IFDEF VCLLIB}
  FLineStroke.Style := psDot;
  {$ENDIF}
  FFill :=  TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorNull);
  FStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkNone, TAdvTreeViewColorDarkGray);
  FSelectedFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorSelection);
  FSelectedStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkNone, TAdvTreeViewColorDarkGray);
  FDisabledFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  FDisabledStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkNone, TAdvTreeViewColorDarkGray);

  FExtendedFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorExtended);
  FExtendedStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  FExtendedSelectedFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorSelection);
  FExtendedSelectedStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkNone, TAdvTreeViewColorDarkGray);
  FExtendedDisabledFill := TAdvTreeViewBrush.CreateBrush(tvbkSolid, TAdvTreeViewColorDarkGray);
  FExtendedDisabledStroke := TAdvTreeViewStrokeBrush.CreateBrush(tvbkNone, TAdvTreeViewColorDarkGray);

  FExpandNodeIcon := CreateBitmapFromResource('TAdvTREEVIEWEXPAND');
  FExpandNodeIcon.OnChange := BitmapChanged;
  FCollapseNodeIcon := CreateBitmapFromResource('TAdvTREEVIEWCOLLAPSE');
  FExpandNodeIcon.OnChange := BitmapChanged;
  FExpandNodeIconLarge := CreateBitmapFromResource('TAdvTREEVIEWEXPANDLARGE');
  FExpandNodeIconLarge.OnChange := BitmapChanged;
  FCollapseNodeIconLarge := CreateBitmapFromResource('TAdvTREEVIEWCOLLAPSELARGE');
  FCollapseNodeIconLarge.OnChange := BitmapChanged;

  FFont := TFont.Create;

  FFill.OnChanged := Changed;
  FStroke.OnChanged := Changed;

  FSelectedFill.OnChanged := Changed;
  FSelectedStroke.OnChanged := Changed;

  FDisabledFill.OnChanged := Changed;
  FDisabledStroke.OnChanged := Changed;

  FExtendedFont := TFont.Create;

  FExtendedFill.OnChanged := Changed;
  FExtendedStroke.OnChanged := Changed;

  FExtendedSelectedFill.OnChanged := Changed;
  FExtendedSelectedStroke.OnChanged := Changed;

  FExtendedDisabledFill.OnChanged := Changed;
  FExtendedDisabledStroke.OnChanged := Changed;

  {$IFDEF FMXLIB}
  FFont.OnChanged := Changed;
  FExtendedFont.OnChanged := Changed;
  {$ENDIF}

  {$IFDEF VCLLIB}
  FFont.OnChange := Changed;
  FExtendedFont.OnChange := Changed;
  {$ENDIF}
end;

destructor TAdvTreeViewNodesAppearance.Destroy;
begin
  FLineStroke.Free;
  FColumnStroke.Free;

  FExpandNodeIconLarge.Free;
  FCollapseNodeIconLarge.Free;
  FExpandNodeIcon.Free;
  FCollapseNodeIcon.Free;

  FFill.Free;
  FStroke.Free;
  FFont.Free;

  FSelectedFill.Free;
  FSelectedStroke.Free;

  FDisabledFill.Free;
  FDisabledStroke.Free;

  FExtendedFill.Free;
  FExtendedStroke.Free;
  FExtendedFont.Free;

  FExtendedSelectedFill.Free;
  FExtendedSelectedStroke.Free;

  FExtendedDisabledFill.Free;
  FExtendedDisabledStroke.Free;
  inherited;
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedFill(const Value: TAdvTreeViewBrush);
begin
  if FSelectedFill <> Value then
    FSelectedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedStroke(const Value: TAdvTreeViewStrokeBrush);
begin
  if FSelectedStroke <> Value then
    FSelectedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetSelectionArea(const Value: TAdvTreeViewSelectionArea);
begin
  if FSelectionArea <> Value then
  begin
    FSelectionArea := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetShowLines(const Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    FShowLines := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFill(const Value: TAdvTreeViewBrush);
begin
  if FFill <> Value then
    FFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFontColor(const Value: TAdvTreeViewColor);
begin
  if FExtendedFontColor <> Value then
  begin
    FExtendedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedFontColor(const Value: TAdvTreeViewColor);
begin
  if FExtendedSelectedFontColor <> Value then
  begin
    FExtendedSelectedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledFontColor(const Value: TAdvTreeViewColor);
begin
  if FExtendedDisabledFontColor <> Value then
  begin
    FExtendedDisabledFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFontColor(const Value: TAdvTreeViewColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetSelectedFontColor(const Value: TAdvTreeViewColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledFontColor(const Value: TAdvTreeViewColor);
begin
  if FDisabledFontColor <> Value then
  begin
    FDisabledFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFixedHeight(const Value: Double);
begin
  if FFixedHeight <> Value then
  begin
    FFixedHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetVariableMinimumHeight(const Value: Double);
begin
  if FVariableMinimumHeight <> Value then
  begin
    FVariableMinimumHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetFont(const Value: TFont);
begin
  if FFont <> Value then
    FFont.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledFill(
  const Value: TAdvTreeViewBrush);
begin
  if FExtendedDisabledFill <> Value then
    FExtendedDisabledFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedDisabledStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FExtendedDisabledStroke <> Value then
    FExtendedDisabledStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedFill(
  const Value: TAdvTreeViewBrush);
begin
  if FExtendedSelectedFill <> Value then
    FExtendedSelectedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedSelectedStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FExtendedSelectedStroke <> Value then
    FExtendedSelectedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFill(
  const Value: TAdvTreeViewBrush);
begin
  if FExtendedFill <> Value then
    FExtendedFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedFont(
  const Value: TFont);
begin
  if FExtendedFont <> Value then
    FExtendedFont.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExtendedStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FExtendedStroke <> Value then
    FExtendedStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetHeightMode(
  const Value: TAdvTreeViewNodeHeightMode);
begin
  if FHeightMode <> Value then
  begin
    FHeightMode := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetLevelIndent(const Value: Double);
begin
  if FLevelIndent <> Value then
  begin
    FLevelIndent := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandNodeIcon(const Value: TAdvTreeViewBitmap);
begin
  if FExpandNodeIcon <> Value then
  begin
    FExpandNodeIcon.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandNodeIconLarge(const Value: TAdvTreeViewBitmap);
begin
  if FExpandNodeIconLarge <> Value then
  begin
    FExpandNodeIconLarge.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetCollapseNodeIconLarge(const Value: TAdvTreeViewBitmap);
begin
  if FCollapseNodeIconLarge <> Value then
  begin
    FCollapseNodeIconLarge.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetColumnStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FColumnStroke <> Value then
    FColumnStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetLineStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FLineStroke <> Value then
    FLineStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetExpandColumn(const Value: Integer);
begin
  if FExpandColumn <> Value then
  begin
    FExpandColumn := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandWidth(const Value: Double);
begin
  if FExpandWidth <> Value then
  begin
    FExpandWidth := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetExpandHeight(const Value: Double);
begin
  if FExpandHeight <> Value then
  begin
    FExpandHeight := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetStroke(const Value: TAdvTreeViewStrokeBrush);
begin
  if FStroke <> Value then
    FStroke.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetCollapseNodeIcon(
  const Value: TAdvTreeViewBitmap);
begin
  if FCollapseNodeIcon <> Value then
  begin
    FCollapseNodeIcon.Assign(Value);
    Changed(Self);
  end;
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledFill(const Value: TAdvTreeViewBrush);
begin
  if FDisabledFill <> Value then
    FDisabledFill.Assign(Value);
end;

procedure TAdvTreeViewNodesAppearance.SetDisabledStroke(const Value: TAdvTreeViewStrokeBrush);
begin
  if FDisabledStroke <> Value then
    FDisabledStroke.Assign(Value);
end;

{ TAdvTreeViewColumnsAppearance }

procedure TAdvTreeViewColumnsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewColumnsAppearance then
  begin
    FLayouts := (Source as TAdvTreeViewColumnsAppearance).Layouts;
    FTopSize := (Source as TAdvTreeViewColumnsAppearance).TopSize;
    FBottomSize := (Source as TAdvTreeViewColumnsAppearance).BottomSize;
    FBottomFill.Assign((Source as TAdvTreeViewColumnsAppearance).BottomFill);
    FBottomFontColor := (Source as TAdvTreeViewColumnsAppearance).BottomFontColor;
    FBottomFont.Assign((Source as TAdvTreeViewColumnsAppearance).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewColumnsAppearance).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewColumnsAppearance).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewColumnsAppearance).TopStroke);
    FTopFontColor := (Source as TAdvTreeViewColumnsAppearance).TopFontColor;
    FTopFont.Assign((Source as TAdvTreeViewColumnsAppearance).TopFont);
    FStretch := (Source as TAdvTreeViewColumnsAppearance).Stretch;
    FStretchColumn  := (Source as TAdvTreeViewColumnsAppearance).StretchColumn;
    FStretchAll := (Source as TAdvTreeViewColumnsAppearance).StretchAll;
    FTopVerticalText := (Source as TAdvTreeViewColumnsAppearance).TopVerticalText;
    FBottomVerticalText := (Source as TAdvTreeViewColumnsAppearance).BottomVerticalText;
    FFillEmptySpaces := (Source as TAdvTreeViewColumnsAppearance).FillEmptySpaces;
  end;
end;

constructor TAdvTreeViewColumnsAppearance.Create(
  ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FLayouts := [tclTop];
  FFillEmptySpaces := True;
  FStretch := True;
  FStretchAll := True;
  FStretchColumn := -1;
  FBottomSize := 25;
  FTopVerticalText := False;
  FStretchAll := True;
  FBottomVerticalText := False;
  FTopSize := 25;
  FSize := 150;
  FTopHorizontalTextAlign := tvtaCenter;
  FTopVerticalTextAlign := tvtaCenter;
  FBottomHorizontalTextAlign := tvtaCenter;
  FBottomVerticalTextAlign := tvtaCenter;
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
end;

destructor TAdvTreeViewColumnsAppearance.Destroy;
begin
  FTopFont.Free;
  FBottomFont.Free;
  FBottomFill.Free;
  FTopFill.Free;
  FBottomStroke.Free;
  FTopStroke.Free;
  inherited;
end;

procedure TAdvTreeViewColumnsAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomFill(const Value: TAdvTreeViewBrush);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomFont(const Value: TFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomFontColor(const Value: TAdvTreeViewColor);
begin
  if FBottomFontColor <> Value then
  begin
    FBottomFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomSize(const Value: Double);
begin
  if FBottomSize <> Value then
  begin
    FBottomSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetBottomVerticalText(
  const Value: Boolean);
begin
  if FBottomVerticalText <> Value then
  begin
    FBottomVerticalText := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetFillEmptySpaces(
  const Value: Boolean);
begin
  if FFillEmptySpaces <> Value then
  begin
    FFillEmptySpaces := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetLayouts(
  const Value: TAdvTreeViewColumnsLayouts);
begin
  if FLayouts <> Value then
  begin
    FLayouts := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretchColumn(const Value: Integer);
begin
  if FStretchColumn <> Value then
  begin
    FStretchColumn := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetStretchAll(const Value: Boolean);
begin
  if FStretchAll <> Value then
  begin
    FStretchAll := Value;
    FTreeView.UpdateColumns;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetTopFill(const Value: TAdvTreeViewBrush);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopFont(const Value: TFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopFontColor(const Value: TAdvTreeViewColor);
begin
  if FTopFontColor <> Value then
  begin
    FTopFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetTopSize(const Value: Double);
begin
  if FTopSize <> Value then
  begin
    FTopSize := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewColumnsAppearance.SetTopStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewColumnsAppearance.SetTopVerticalText(
  const Value: Boolean);
begin
  if FTopVerticalText <> Value then
  begin
    FTopVerticalText := Value;
    Changed(Self);
  end;
end;

{ TAdvTreeViewGroupsAppearance }

procedure TAdvTreeViewGroupsAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvTreeViewGroupsAppearance then
  begin
    FLayouts := (Source as TAdvTreeViewGroupsAppearance).Layouts;
    FTopSize := (Source as TAdvTreeViewGroupsAppearance).TopSize;
    FBottomSize := (Source as TAdvTreeViewGroupsAppearance).BottomSize;
    FBottomFill.Assign((Source as TAdvTreeViewGroupsAppearance).BottomFill);
    FBottomFontColor := (Source as TAdvTreeViewGroupsAppearance).BottomFontColor;
    FBottomFont.Assign((Source as TAdvTreeViewGroupsAppearance).BottomFont);
    FBottomStroke.Assign((Source as TAdvTreeViewGroupsAppearance).BottomStroke);
    FTopFill.Assign((Source as TAdvTreeViewGroupsAppearance).TopFill);
    FTopStroke.Assign((Source as TAdvTreeViewGroupsAppearance).TopStroke);
    FTopFontColor := (Source as TAdvTreeViewGroupsAppearance).TopFontColor;
    FTopFont.Assign((Source as TAdvTreeViewGroupsAppearance).TopFont);
    FTopHorizontalTextAlign := (Source as TAdvTreeViewGroupsAppearance).TopHorizontalTextAlign;
    FTopVerticalTextAlign := (Source as TAdvTreeViewGroupsAppearance).TopVerticalTextAlign;
    FBottomHorizontalTextAlign := (Source as TAdvTreeViewGroupsAppearance).BottomHorizontalTextAlign;
    FBottomVerticalTextAlign := (Source as TAdvTreeViewGroupsAppearance).BottomVerticalTextAlign;
    FTopVerticalText := (Source as TAdvTreeViewGroupsAppearance).TopVerticalText;
    FBottomVerticalText := (Source as TAdvTreeViewGroupsAppearance).BottomVerticalText;
    FFillEmptySpaces := (Source as TAdvTreeViewGroupsAppearance).FillEmptySpaces;
  end;
end;

constructor TAdvTreeViewGroupsAppearance.Create(
  ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FLayouts := [tglTop];
  FFillEmptySpaces := True;
  FTopSize := 50;
  FBottomSize := 50;
  FTopVerticalText := False;
  FBottomVerticalText := False;
  FTopHorizontalTextAlign := tvtaCenter;
  FTopVerticalTextAlign := tvtaCenter;
  FBottomHorizontalTextAlign := tvtaCenter;
  FBottomVerticalTextAlign := tvtaCenter;
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
end;

destructor TAdvTreeViewGroupsAppearance.Destroy;
begin
  FBottomFont.Free;
  FTopFont.Free;
  FBottomFill.Free;
  FBottomStroke.Free;
  FTopFill.Free;
  FTopStroke.Free;
  inherited;
end;

procedure TAdvTreeViewGroupsAppearance.Changed(Sender: TObject);
begin
  FTreeView.UpdateTreeViewCache;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomFill(const Value: TAdvTreeViewBrush);
begin
  if FBottomFill <> Value then
    FBottomFill.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomFontColor(const Value: TAdvTreeViewColor);
begin
  if FBottomFontColor <> Value then
  begin
    FBottomFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomHorizontalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FBottomHorizontalTextAlign <> Value then
  begin
    FBottomHorizontalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomFont(const Value: TFont);
begin
  if FBottomFont <> Value then
    FBottomFont.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomSize(const Value: Double);
begin
  if FBottomSize <> Value then
  begin
    FBottomSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FBottomStroke <> Value then
    FBottomStroke.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomVerticalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FBottomVerticalTextAlign <> Value then
  begin
    FBottomVerticalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetBottomVerticalText(
  const Value: Boolean);
begin
  if FBottomVerticalText <> Value then
  begin
    FBottomVerticalText := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetFillEmptySpaces(
  const Value: Boolean);
begin
  if FFillEmptySpaces <> Value then
  begin
    FFillEmptySpaces := Value;
    FTreeView.Invalidate;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetLayouts(
  const Value: TAdvTreeViewGroupsLayouts);
begin
  if FLayouts <> Value then
  begin
    FLayouts := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopFill(const Value: TAdvTreeViewBrush);
begin
  if FTopFill <> Value then
    FTopFill.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopFont(const Value: TFont);
begin
  if FTopFont <> Value then
    FTopFont.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopFontColor(const Value: TAdvTreeViewColor);
begin
  if FTopFontColor <> Value then
  begin
    FTopFontColor := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopHorizontalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FTopHorizontalTextAlign <> Value then
  begin
    FTopHorizontalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopSize(const Value: Double);
begin
  if FTopSize <> Value then
  begin
    FTopSize := Value;
    FTreeView.UpdateTreeViewCache;
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopStroke(
  const Value: TAdvTreeViewStrokeBrush);
begin
  if FTopStroke <> Value then
    FTopStroke.Assign(Value);
end;

procedure TAdvTreeViewGroupsAppearance.SetTopVerticalTextAlign(
  const Value: TAdvTreeViewTextAlign);
begin
  if FTopVerticalTextAlign <> Value then
  begin
    FTopVerticalTextAlign := Value;
    Changed(Self);
  end;
end;

procedure TAdvTreeViewGroupsAppearance.SetTopVerticalText(const Value: Boolean);
begin
  if FTopVerticalText <> Value then
  begin
    FTopVerticalText := Value;
    Changed(Self);
  end;
end;

{ TAdvTreeViewInteraction }

procedure TAdvTreeViewInteraction.Assign(Source: TPersistent);
begin
  if (Source is TAdvTreeViewInteraction) then
  begin
    FMultiSelect := (Source as TAdvTreeViewInteraction).MultiSelect;
    FTouchScrolling := (Source as TAdvTreeViewInteraction).TouchScrolling;
    FReadOnly := (Source as TAdvTreeViewInteraction).ReadOnly;
    FColumnSizing := (Source as TAdvTreeViewInteraction).ColumnSizing;
    FColumnAutoSizeOnDblClick := (Source as TAdvTreeViewInteraction).ColumnAutoSizeOnDblClick;
    FExtendedSelectable := (Source as TAdvTreeViewInteraction).ExtendedSelectable;
    FSelectionFollowsFocus := (Source as TAdvTreeViewInteraction).SelectionFollowsFocus;
    FKeyboardEdit := (Source as TAdvTreeViewInteraction).KeyboardEdit;
    FMouseEditMode := (Source as TAdvTreeViewInteraction).MouseEditMode;
    FExtendedEditable := (Source as TAdvTreeViewInteraction).ExtendedEditable;
  end;
end;

constructor TAdvTreeViewInteraction.Create(ATreeView: TAdvCustomTreeView);
begin
  FTreeView := ATreeView;
  FReadOnly := False;
  FSelectionFollowsFocus := True;
  FTouchScrolling := True;
  FKeyboardEdit := True;
  FExtendedSelectable := False;
  FMultiSelect := False;
  FColumnSizing := False;
  FExtendedEditable := False;
  FColumnAutoSizeOnDblClick := False;
  FMouseEditMode := tmemSingleClickOnSelectedNode;
end;

procedure TAdvTreeViewInteraction.SetColumnAutoSizeOnDblClick(
  const Value: Boolean);
begin
  if FColumnAutoSizeOnDblClick <> Value then  
    FColumnAutoSizeOnDblClick := Value;
end;

procedure TAdvTreeViewInteraction.SetColumnSizing(const Value: Boolean);
begin
  if FColumnSizing <> Value then
    FColumnSizing := Value;
end;

procedure TAdvTreeViewInteraction.SetExtendedEditable(const Value: Boolean);
begin
  if FExtendedEditable <> Value then
    FExtendedEditable := Value;
end;

procedure TAdvTreeViewInteraction.SetExtendedSelectable(
  const Value: Boolean);
begin
  if FExtendedSelectable <> Value then
    FExtendedSelectable := Value;
end;

procedure TAdvTreeViewInteraction.SetMouseEditMode(
  const Value: TAdvTreeViewMouseEditMode);
begin
  if FMouseEditMode <> Value then
    FMouseEditMode := Value;
end;

procedure TAdvTreeViewInteraction.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> Value then
    FMultiSelect := Value;
end;

procedure TAdvTreeViewInteraction.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
    FReadOnly := Value;
end;

procedure TAdvTreeViewInteraction.SetSelectionFollowsFocus(
  const Value: Boolean);
begin
  if FSelectionFollowsFocus <> Value then
    FSelectionFollowsFocus := Value;
end;

procedure TAdvTreeViewInteraction.SetTouchScrolling(const Value: Boolean);
begin
  if FTouchScrolling <> Value then
    FTouchScrolling := Value;
end;

end.
