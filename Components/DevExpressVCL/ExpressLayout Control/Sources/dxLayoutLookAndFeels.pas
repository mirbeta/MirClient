{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl Look & Feel components              }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxLayoutLookAndFeels;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Controls, Graphics, Contnrs,
  dxCore, cxGeometry, cxGraphics, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutCommon, dxCoreGraphics;

type
  TdxCustomLayoutLookAndFeelOptions = class;
  TdxCustomLayoutLookAndFeel = class;
  TdxLayoutWebLookAndFeelGroupOptions = class;
  TdxLayoutLookAndFeelList = class;
  TdxLayoutCxLookAndFeel = class;

  // custom

  IdxLayoutLookAndFeelUser = interface
    ['{651F19FE-CBCB-4C16-8615-BBD57ED7255A}']
    procedure BeginLookAndFeelDestroying; stdcall;
    procedure EndLookAndFeelDestroying; stdcall;
    procedure LookAndFeelChanged; stdcall;
    procedure LookAndFeelDestroyed; stdcall;
  end;

  { TdxCustomLayoutLookAndFeelPart }

  TdxCustomLayoutLookAndFeelPart = class(TPersistent)
  strict private
    FLockCount: Integer;
    FLookAndFeel: TdxCustomLayoutLookAndFeel;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer);
    procedure ChangeScaleCore(M, D: Integer); virtual;

    property LookAndFeel: TdxCustomLayoutLookAndFeel read FLookAndFeel;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); virtual;
  end;

  { TdxLayoutLookAndFeelCaptionOptions }

  TdxLayoutHotTrackStyle = (htsHandPoint, htsUnderlineCold, htsUnderlineHot);
  TdxLayoutHotTrackStyles = set of TdxLayoutHotTrackStyle;

  TdxLayoutLookAndFeelCaptionOptionsClass = class of TdxLayoutLookAndFeelCaptionOptions;
  TdxLayoutLookAndFeelCaptionOptions = class(TPersistent)
  private
    FFont: TFont;
    FHotTrack: Boolean;
    FHotTrackStyles: TdxLayoutHotTrackStyles;
    FOptions: TdxCustomLayoutLookAndFeelOptions;
    FScaledFont: TFont;
    FTextColor: TColor;
    FTextDisabledColor: TColor;
    FTextHotColor: TColor;
    FUseDefaultFont: Boolean;

    function GetScaleFactor: TdxScaleFactor;
    procedure SetFont(Value: TFont);
    procedure SetHotTrack(Value: Boolean);
    procedure SetHotTrackStyles(Value: TdxLayoutHotTrackStyles);
    procedure SetTextColor(Value: TColor);
    procedure SetTextDisabledColor(Value: TColor);
    procedure SetTextHotColor(Value: TColor);
    procedure SetUseDefaultFont(Value: Boolean);

    procedure FontChanged(Sender: TObject);
    function IsFontStored: Boolean;
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    // colors
    function GetDefaultTextColor: TColor; virtual; abstract;
    function GetDefaultTextDisabledColor: TColor; virtual;
    function GetDefaultTextHotColor: TColor; virtual;
    // font
    function GetDefaultFont(AContainer: TComponent): TFont; virtual;

    property Options: TdxCustomLayoutLookAndFeelOptions read FOptions;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOptions: TdxCustomLayoutLookAndFeelOptions); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // colors
    function GetTextColor: TColor; virtual;
    function GetTextDisabledColor: TColor; virtual;
    function GetTextHotColor: TColor; virtual;
    // font
    function GetFont(AContainer: TComponent): TFont; virtual;
  published
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HotTrackStyles: TdxLayoutHotTrackStyles read FHotTrackStyles
      write SetHotTrackStyles default [htsHandPoint, htsUnderlineHot];
    property TextColor: TColor read FTextColor write SetTextColor default clDefault;
    property TextDisabledColor: TColor read FTextDisabledColor write SetTextDisabledColor default clDefault;
    property TextHotColor: TColor read FTextHotColor write SetTextHotColor default clDefault;
    property UseDefaultFont: Boolean read FUseDefaultFont write SetUseDefaultFont default True;
  end;

  { TdxLayoutLookAndFeelPadding }

  TdxLayoutPaddingAssignedValue = (lpavBottom, lpavLeft, lpavRight, lpavTop);
  TdxLayoutPaddingAssignedValues = set of TdxLayoutPaddingAssignedValue;

  TdxLayoutLookAndFeelPaddingClass = class of TdxLayoutLookAndFeelPadding;
  TdxLayoutLookAndFeelPadding = class(TdxCustomLayoutLookAndFeelPart)
  strict private
    FAssignedValues: TdxLayoutPaddingAssignedValues;
    FBottom: Integer;
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;

    function GetScaleFactor: TdxScaleFactor; inline;
    function GetValue(Index: Integer): Integer;
    function IsValueStored(Index: Integer): Boolean;
    procedure SetAssignedValues(Value: TdxLayoutPaddingAssignedValues);
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    function GetDefaultValue(Index: Integer): Integer; virtual;
    procedure SetValue(Index: Integer; Value: Integer); virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    procedure Assign(Source: TPersistent); override;
    function GetPaddingRect: TRect; overload;
    function GetPaddingRect(ATargetScaleFactor: TdxScaleFactor): TRect; overload;
  published
    property AssignedValues: TdxLayoutPaddingAssignedValues read FAssignedValues write SetAssignedValues default [];
    property Bottom: Integer index 1 read GetValue write SetValue stored IsValueStored;
    property Left: Integer index 2 read GetValue write SetValue stored IsValueStored;
    property Right: Integer index 3 read GetValue write SetValue stored IsValueStored;
    property Top: Integer index 4 read GetValue write SetValue stored IsValueStored;
  end;

  { TdxCustomLayoutLookAndFeelOptions }

  TdxCustomLayoutLookAndFeelOptions = class(TdxCustomLayoutLookAndFeelPart)
  strict private
    FCaptionOptions: TdxLayoutLookAndFeelCaptionOptions;
    FPadding: TdxLayoutLookAndFeelPadding;

    procedure SetCaptionOptions(Value: TdxLayoutLookAndFeelCaptionOptions);
    procedure SetPadding(const Value: TdxLayoutLookAndFeelPadding);
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; virtual;
    function GetPaddingClass: TdxLayoutLookAndFeelPaddingClass; virtual;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CaptionOptions: TdxLayoutLookAndFeelCaptionOptions read FCaptionOptions write SetCaptionOptions;
    property Padding: TdxLayoutLookAndFeelPadding read FPadding write SetPadding;
  end;

  { TdxLayoutLookAndFeelGroupOptions }

  TdxLayoutLookAndFeelGroupOptionsClass = class of TdxLayoutLookAndFeelGroupOptions;
  TdxLayoutLookAndFeelGroupOptions = class(TdxCustomLayoutLookAndFeelOptions)
  strict private
    FColor: TColor;
    FSpaceBetweenButtons: Cardinal;

    procedure SetColor(Value: TColor);
    procedure SetSpaceBetweenButtons(AValue: Cardinal);
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    // colors
    function GetDefaultColor: TColor; virtual; abstract;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
    procedure Assign(Source: TPersistent); override;
    // colors
    function GetColor: TColor; virtual;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property SpaceBetweenButtons: Cardinal read FSpaceBetweenButtons write SetSpaceBetweenButtons default 0;
  end;

  { TdxLayoutLookAndFeelItemOptions }

  TdxLayoutBorderStyle = (lbsNone, lbsSingle, lbsFlat, lbsStandard);

  TdxLayoutLookAndFeelItemOptionsClass = class of TdxLayoutLookAndFeelItemOptions;
  TdxLayoutLookAndFeelItemOptions = class(TdxCustomLayoutLookAndFeelOptions)
  strict private
    FControlBorderColor: TColor;
    FControlBorderStyle: TdxLayoutBorderStyle;

    procedure SetControlBorderColor(Value: TColor);
    procedure SetControlBorderStyle(Value: TdxLayoutBorderStyle);
  protected
    // colors
    function GetDefaultControlBorderColor: TColor; virtual;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
    procedure Assign(Source: TPersistent); override;
    // colors
    function GetControlBorderColor: TColor; virtual;
  published
    property ControlBorderColor: TColor read FControlBorderColor write SetControlBorderColor default clDefault;
    property ControlBorderStyle: TdxLayoutBorderStyle read FControlBorderStyle write SetControlBorderStyle default lbsStandard;
  end;

  { TdxLayoutLookAndFeelOffsets }

  TdxLayoutLookAndFeelOffsetsClass = class of TdxLayoutLookAndFeelOffsets;
  TdxLayoutLookAndFeelOffsets = class(TdxCustomLayoutLookAndFeelPart)
  strict private const
    FValuesCount = 9;
  strict private
    FControlOffsetHorz: Integer;
    FControlOffsetVert: Integer;
    FItemOffset: Integer;
    FItemsAreaOffsetHorz: Integer;
    FItemsAreaOffsetVert: Integer;
    FRootItemsAreaOffsetHorz: Integer;
    FRootItemsAreaOffsetVert: Integer;
    FTabSheetClientOffsetHorz: Integer;
    FTabSheetClientOffsetVert: Integer;
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    function GetDefaultValue(Index: Integer): Integer; virtual;
    function GetValue(Index: Integer): Integer; virtual;
    function IsValueStored(Index: Integer): Boolean;
    procedure SetValue(Index: Integer; Value: Integer); virtual;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ControlOffsetHorz: Integer index 0 read GetValue write SetValue stored IsValueStored;
    property ControlOffsetVert: Integer index 1 read GetValue write SetValue stored IsValueStored;
    property ItemOffset: Integer index 2 read GetValue write SetValue stored IsValueStored;
    property ItemsAreaOffsetHorz: Integer index 3 read GetValue write SetValue stored IsValueStored;
    property ItemsAreaOffsetVert: Integer index 4 read GetValue write SetValue stored IsValueStored;
    property RootItemsAreaOffsetHorz: Integer index 5 read GetValue write SetValue stored IsValueStored;
    property RootItemsAreaOffsetVert: Integer index 6 read GetValue write SetValue stored IsValueStored;
    property TabSheetContentOffsetHorz: Integer index 7 read GetValue write SetValue stored IsValueStored;
    property TabSheetContentOffsetVert: Integer index 8 read GetValue write SetValue stored IsValueStored;
  end;

  { TdxCustomLayoutLookAndFeel }

  TdxCustomLayoutLookAndFeelClass = class of TdxCustomLayoutLookAndFeel;
  TdxCustomLayoutLookAndFeel = class(TcxScalableComponent)
  private
    FGroupOptions: TdxLayoutLookAndFeelGroupOptions;
    FItemOptions: TdxLayoutLookAndFeelItemOptions;
    FList: TdxLayoutLookAndFeelList;
    FLockCount: Integer;
    FNotifyingAboutDestroying: Boolean;
    FOffsets: TdxLayoutLookAndFeelOffsets;
    FUsers: TList;

    FLookAndFeel: TcxLookAndFeel;

    function GetIsDesigning: Boolean;
    function GetUser(Index: Integer): IdxLayoutLookAndFeelUser;
    function GetUserCount: Integer;

    procedure SetGroupOptions(Value: TdxLayoutLookAndFeelGroupOptions);
    procedure SetItemOptions(Value: TdxLayoutLookAndFeelItemOptions);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetOffsets(Value: TdxLayoutLookAndFeelOffsets);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;
    procedure Changed; virtual;
    class function GetBaseName: string; virtual;
    procedure GetTextMetric(AFont: TFont; var ATextMetric: TTextMetric);
    function GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer; virtual;
    function GetGroupCaptionFont(AContainer: TComponent): TFont;
    function GetItemCaptionFont(AContainer: TComponent): TFont;
    procedure InitLookAndFeel; virtual;
    procedure NotifyUsersAboutDestroying;

    // Conditions
    function CanDrawSpecificBackground: Boolean; virtual;
    function DoesCxLookAndFeelHavePriority: Boolean; virtual;
    function IsGroupTransparent(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean; virtual;
    function IsNativeStyle: Boolean; virtual;
    function IsSkinPainterUsed: Boolean; virtual;

    // options classes
    function GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass; virtual;
    function GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass; virtual;
    function GetOffsetsClass: TdxLayoutLookAndFeelOffsetsClass; virtual;

    property IsDesigning: Boolean read GetIsDesigning;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property UserCount: Integer read GetUserCount;
    property Users[Index: Integer]: IdxLayoutLookAndFeelUser read GetUser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(var ALookAndFeel: TdxCustomLayoutLookAndFeel; ATargetPPI: Integer = 0); reintroduce; overload;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    function NeedDoubleBuffered: Boolean; virtual;
    function NeedRedrawOnResize: Boolean; virtual;
    procedure InitializeSubControlCxLookAndFeel(AcxLookAndFeel: TcxLookAndFeel); virtual;

    procedure AddUser(AUser: TComponent);
    procedure RemoveUser(AUser: TComponent);

    class function Description: string; virtual;
    function DLUToPixels(AFont: TFont; ADLU: Integer): Integer;
    function HDLUToPixels(AFont: TFont; ADLU: Integer): Integer;
    function VDLUToPixels(AFont: TFont; ADLU: Integer): Integer;

    // painter classes
    function GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass}; virtual; abstract;
    function GetBasicItemPainterClass: TClass{TdxLayoutBasicItemPainterClass}; virtual;
    function GetEmptySpaceItemPainterClass: TClass{TdxLayoutEmptySpaceItemPainter}; virtual;
    function GetItemPainterClass: TClass{TdxLayoutItemPainterClass}; virtual;
    function GetLabeledItemPainterClass: TClass{TdxLayoutLabeledItemPainterClass}; virtual;
    function GetSeparatorItemPainterClass: TClass{TdxLayoutSeparatorItemPainterClass}; virtual;
    function GetSplitterItemPainterClass: TClass{TdxLayoutSplitterItemPainterClass}; virtual;

    // dimensions
    procedure CorrectGroupButtonsAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect); virtual;
    procedure CorrectGroupCaptionAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect); virtual;
    procedure CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation); virtual;
    procedure CorrectGroupMinVisibleHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer); virtual;
    procedure CorrectGroupMinVisibleWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer); virtual;
    procedure CorrectGroupCaptionFont(AViewInfo: TObject {TdxLayoutGroupCaptionViewInfo}; AFont: TFont); virtual;
    procedure CorrectGroupCaptionHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer); virtual;
    procedure CorrectGroupCaptionWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer); virtual;
    function GetElementOffsetHorz(AContainer: TComponent): Integer; virtual;
    function GetElementOffsetVert(AContainer: TComponent): Integer; virtual;
    function GetGroupButtonsOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; virtual;
    function GetGroupBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer; virtual;
    function GetTabControlBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer; virtual;
    function GetGroupBorderWidth(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide;
      AHasCaption, AIsExpanded: Boolean): Integer; virtual;
    function GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; virtual;
    function GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; virtual;
    function GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; virtual;
    function GetGroupRestSpaceBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; virtual;
    function GetGroupCaptionAlignHorz(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TAlignment; virtual;
    function GetGroupCaptionAlignVert(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxAlignmentVert; virtual;
    function GetItemControlBorderWidth(ASide: TdxLayoutSide): Integer; virtual;
    function GetItemOffset(AContainer: TComponent): Integer; virtual;
    function GetItemsAreaOffsetHorz(AContainer: TComponent): Integer; virtual;
    function GetItemsAreaOffsetVert(AContainer: TComponent): Integer; virtual;
    function GetRootItemsAreaOffsetHorz(AContainer: TComponent): Integer; virtual;
    function GetRootItemsAreaOffsetVert(AContainer: TComponent): Integer; virtual;
    function GetTabSheetContentOffsetHorz(AContainer: TComponent): Integer; virtual;
    function GetTabSheetContentOffsetVert(AContainer: TComponent): Integer; virtual;
    function GetSeparatorItemMinWidth: Integer; virtual;
    function GetSplitterItemMinSize: TSize; virtual;
    function IsButtonHotTrack: Boolean; virtual;

    // drawing routines
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawItemControlBorder(ACanvas: TcxCanvas; AItemOptions: TdxLayoutLookAndFeelItemOptions; R: TRect); virtual;
    // colors
    function GetEmptyAreaColor: TColor; virtual;
    function GetGroupCaptionColor(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TColor; virtual;
    function GetGroupButtonColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function GetItemCaptionColorPalette: IdxColorPalette; virtual;
    function GetTabbedGroupCaptionColorPalette(AIsActive: Boolean): IdxColorPalette; virtual;

    property FrameWidths[ASide, ACaptionSide: TdxLayoutSide]: Integer read GetFrameWidth;
    property ItemControlBorderWidths[ASide: TdxLayoutSide]: Integer read GetItemControlBorderWidth;
    property List: TdxLayoutLookAndFeelList read FList;
  published
    property GroupOptions: TdxLayoutLookAndFeelGroupOptions read FGroupOptions write SetGroupOptions;
    property ItemOptions: TdxLayoutLookAndFeelItemOptions read FItemOptions write SetItemOptions;
    property Offsets: TdxLayoutLookAndFeelOffsets read FOffsets write SetOffsets;
  end;

  // standard

  TdxLayoutStandardLookAndFeelGroupCaptionOptions = class(TdxLayoutLookAndFeelCaptionOptions)
  protected
    function GetDefaultTextColor: TColor; override;
  end;

  TdxLayoutStandardLookAndFeelGroupOptions = class(TdxLayoutLookAndFeelGroupOptions)
  protected
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
    function GetDefaultColor: TColor; override;
  end;

  TdxLayoutStandardLookAndFeelItemCaptionOptions = class(TdxLayoutLookAndFeelCaptionOptions)
  protected
    function GetDefaultTextColor: TColor; override;
  end;

  TdxLayoutStandardLookAndFeelItemOptions = class(TdxLayoutLookAndFeelItemOptions)
  protected
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
  end;

  TdxLayoutStandardLookAndFeel = class(TdxCustomLayoutLookAndFeel)
  protected
    function GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass; override;
    function GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass; override;
    function GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer; override;
    procedure InitLookAndFeel; override;
  public
    class function Description: string; override;

    function GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass}; override;

    procedure CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation); override;
    function GetGroupBorderWidth(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide;
      AHasCaption, AIsExpanded: Boolean): Integer; override;
    function GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; override;
    function GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
    function GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
  end;

  // office

  TdxLayoutOfficeLookAndFeel = class(TdxLayoutStandardLookAndFeel)
  protected
    function GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer; override;
    procedure InitLookAndFeel; override;
  public
    class function Description: string; override;

    function GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass}; override;

    procedure CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation); override;
    function GetGroupBorderWidth(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide;
      AHasCaption, AIsExpanded: Boolean): Integer; override;
    function GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; override;
    function GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
    function GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
  end;

  // web

  TdxLayoutWebLookAndFeelGroupCaptionOptions = class(TdxLayoutLookAndFeelCaptionOptions)
  strict private
    FColor: TColor;
    FSeparatorWidth: Integer;

    function GetOptions: TdxLayoutWebLookAndFeelGroupOptions;
    procedure SetColor(Value: TColor);
    procedure SetSeparatorWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
    // colors
    function GetDefaultColor: TColor; virtual;
    function GetDefaultTextColor: TColor; override;
    // font
    function GetDefaultFont(AContainer: TComponent): TFont; override;

    property Options: TdxLayoutWebLookAndFeelGroupOptions read GetOptions;
  public
    constructor Create(AOptions: TdxCustomLayoutLookAndFeelOptions); override;
    // colors
    function GetColor: TColor; virtual;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property SeparatorWidth: Integer read FSeparatorWidth write SetSeparatorWidth default 0;
  end;

  TdxLayoutWebLookAndFeelGroupOptions = class(TdxLayoutLookAndFeelGroupOptions)
  strict private
    FFrameColor: TColor;
    FFrameWidth: Integer;
    FOffsetCaption: Boolean;
    FOffsetItems: Boolean;

    function GetCaptionOptions: TdxLayoutWebLookAndFeelGroupCaptionOptions;
    procedure SetCaptionOptions(Value: TdxLayoutWebLookAndFeelGroupCaptionOptions);
    procedure SetFrameColor(Value: TColor);
    procedure SetFrameWidth(Value: Integer);
    procedure SetOffsetCaption(Value: Boolean);
    procedure SetOffsetItems(Value: Boolean);
  protected
    procedure ChangeScaleCore(M, D: Integer); override;
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
    // colors
    function GetDefaultColor: TColor; override;
    function GetDefaultFrameColor: TColor; virtual;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
    // colors
    function GetFrameColor: TColor; virtual;

    function HasCaptionSeparator(AHasCaption: Boolean): Boolean;
  published
    property CaptionOptions: TdxLayoutWebLookAndFeelGroupCaptionOptions read GetCaptionOptions write SetCaptionOptions;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clDefault;
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth default 1;
    property OffsetCaption: Boolean read FOffsetCaption write SetOffsetCaption default True;
    property OffsetItems: Boolean read FOffsetItems write SetOffsetItems default True;
  end;

  TdxLayoutWebLookAndFeelItemCaptionOptions = class(TdxLayoutLookAndFeelCaptionOptions)
  protected
    function GetDefaultTextColor: TColor; override;
  end;

  TdxLayoutWebLookAndFeelItemOptions = class(TdxLayoutLookAndFeelItemOptions)
  protected
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
  public
    constructor Create(ALookAndFeel: TdxCustomLayoutLookAndFeel); override;
  published
    property ControlBorderStyle default lbsSingle;
  end;

  TdxLayoutWebLookAndFeel = class(TdxCustomLayoutLookAndFeel)
  private
    function GetGroupOptions: TdxLayoutWebLookAndFeelGroupOptions;
    procedure SetGroupOptions(Value: TdxLayoutWebLookAndFeelGroupOptions);
    function HasCaptionSeparator(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean;
  protected
    function CanDrawSpecificBackground: Boolean; override;
    function GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass; override;
    function GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass; override;
    procedure InitLookAndFeel; override;
  public
    class function Description: string; override;

    function GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass}; override;

    procedure CorrectGroupButtonsAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect); override;
    procedure CorrectGroupCaptionAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect); override;
    procedure CorrectGroupCaptionHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer); override;
    procedure CorrectGroupCaptionWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer); override;
    procedure CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation); override;
    function GetGroupBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer; override;
    function GetGroupBorderWidth(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide;
      AHasCaption, AIsExpanded: Boolean): Integer; override;
    function GetGroupCaptionColor(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TColor; override;
    function GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; override;
    function GetGroupCaptionSeparatorAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
    function GetGroupCaptionSeparatorBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
    function GetGroupRestSpaceBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
    function GetGroupCaptionAlignHorz(AViewInfo: TObject {TdxCustomLayoutItemViewInfo}): TAlignment; override;
    function GetGroupCaptionAlignVert(AViewInfo: TObject {TdxCustomLayoutItemViewInfo}): TdxAlignmentVert; override;
  published
    property GroupOptions: TdxLayoutWebLookAndFeelGroupOptions read GetGroupOptions write SetGroupOptions;
  end;

  // CX style

  { TdxLayoutCxLookAndFeelGroupOptions }

  TdxLayoutCxLookAndFeelGroupOptions = class(TdxLayoutStandardLookAndFeelGroupOptions)
  strict private
    function GetLookAndFeel: TdxLayoutCxLookAndFeel;
  protected
    function GetDefaultColor: TColor; override;
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
    property LookAndFeel: TdxLayoutCxLookAndFeel read GetLookAndFeel;
  end;

  { TdxLayoutCxLookAndFeelGroupCaptionOptions }

  TdxLayoutCxLookAndFeelGroupCaptionOptions = class(TdxLayoutStandardLookAndFeelGroupCaptionOptions)
  strict private
    function GetLookAndFeel: TdxLayoutCxLookAndFeel;
  protected
    function GetDefaultTextColor: TColor; override;
    function GetDefaultTextDisabledColor: TColor; override;
    function GetDefaultTextHotColor: TColor; override;
    //
    property LookAndFeel: TdxLayoutCxLookAndFeel read GetLookAndFeel;
  end;

  TdxLayoutCxLookAndFeelItemOptions = class(TdxLayoutLookAndFeelItemOptions)
  protected
    function GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass; override;
  end;

  TdxLayoutCxLookAndFeelItemCaptionOptions = class(TdxLayoutLookAndFeelCaptionOptions)
  private
    function GetLookAndFeel: TdxLayoutCxLookAndFeel;
  protected
    function GetDefaultTextColor: TColor; override;
    function GetDefaultTextDisabledColor: TColor; override;
    function GetDefaultTextHotColor: TColor; override;
    //
    property LookAndFeel: TdxLayoutCxLookAndFeel read GetLookAndFeel;
  end;

  { TdxLayoutCxLookAndFeel } //todo: rename?

  TdxLayoutCxLookAndFeel = class(TdxLayoutStandardLookAndFeel, IdxSkinSupport, IcxLookAndFeelContainer)
  strict private
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    function DoesCxLookAndFeelHavePriority: Boolean; override;
    function IsGroupTransparent(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean; override;
    function IsSkinPainterUsed: Boolean; override;
    function GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer; override;
    function GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass; override;
    function GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass; override;
    procedure InitLookAndFeel; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);

    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel;
  public
    constructor Create(AOwner: TComponent); override;

    class function Description: string; override;
    function NeedRedrawOnResize: Boolean; override;

    // dimensions
    procedure CorrectGroupCaptionFont(ACaptionSide: TdxLayoutSide; AFont: TFont); reintroduce; overload;
    procedure CorrectGroupCaptionFont(AViewInfo: TObject {TdxLayoutGroupCaptionViewInfo}; AFont: TFont); overload; override;
    function GetGroupBorderWidth(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer; override;
    function GetGroupCaptionContentOffsets(AIsVertical: Boolean; ACaptionSide: TdxLayoutSide): TRect; reintroduce; overload;
    function GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; overload; override;
    function GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer; override;
    function GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect; override;
    function GetItemControlBorderWidth(ASide: TdxLayoutSide): Integer; override;
    function IsGroupBoxCaptionTextDrawnOverBorder(ACaptionSide: TdxLayoutSide): Boolean;

    // drawing routines
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawItemControlBorder(ACanvas: TcxCanvas; AItemOptions: TdxLayoutLookAndFeelItemOptions; R: TRect); override;
    function GetEmptyAreaColor: TColor; override;
    function GetGroupButtonColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    function GetItemCaptionColorPalette: IdxColorPalette; override;
    function GetTabbedGroupCaptionColorPalette(AIsActive: Boolean): IdxColorPalette; override;

    function GetGroupPainterClass: TClass {TdxLayoutGroupPainterClass}; override;
    function GetItemPainterClass: TClass{TdxLayoutItemPainterClass}; override;
    //
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  published
    property LookAndFeel;
  end;

  { TdxLayoutSkinLookAndFeel }

  TdxLayoutSkinLookAndFeel = class(TdxLayoutCxLookAndFeel)
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadSkinName(Reader: TReader);
    procedure ReadSkinNameAssigned(Reader: TReader);
  public
    class function Description: string; override;
  end;

  // list and definitions

  TdxLayoutLookAndFeelList = class(TcxCustomComponent)
  private
    FItems: TcxComponentList;
    function GetCount: Integer;
    function GetIsDesigning: Boolean;
    function GetItem(Index: Integer): TdxCustomLayoutLookAndFeel;

    procedure AddItem(AItem: TdxCustomLayoutLookAndFeel);
    procedure RemoveItem(AItem: TdxCustomLayoutLookAndFeel);
    procedure ItemListChanged(Sender: TObject; AComponent: TComponent; AAction: TcxComponentCollectionNotification);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Modified;
    procedure SetName(const Value: TComponentName); override;
    property IsDesigning: Boolean read GetIsDesigning;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem(AClass: TdxCustomLayoutLookAndFeelClass): TdxCustomLayoutLookAndFeel;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxCustomLayoutLookAndFeel read GetItem; default;
  end;

  TdxLayoutLookAndFeelDefs = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxCustomLayoutLookAndFeelClass;
  public
    constructor Create;
    destructor Destroy; override;
    function GetItemByDescription(const Value: string): TdxCustomLayoutLookAndFeelClass;
    procedure Register(AClass: TdxCustomLayoutLookAndFeelClass);
    procedure Unregister(AClass: TdxCustomLayoutLookAndFeelClass);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxCustomLayoutLookAndFeelClass read GetItem; default;
  end;

  // text metric manager

  PdxLayoutTextMetric = ^TdxLayoutTextMetric;
  TdxLayoutTextMetric = record
    Font: TFont;
    TextMetric: TTextMetric;
    PrevFontChangeHandler: TNotifyEvent;
  end;

  TdxLayoutTextMetrics = class
  private
    FItems: TList;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): PdxLayoutTextMetric;
    procedure DestroyItems;
    procedure FontChangeHandler(Sender: TObject);
  protected
    procedure CalculateItem(AIndex: Integer);
    procedure Delete(AIndex: Integer);
    function IndexOf(AFont: TFont): Integer;
    function GetSameFontIndex(AFont: TFont): Integer;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: PdxLayoutTextMetric read GetItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Get(AFont: TFont; var ATextMetric: TTextMetric);
    procedure Unregister(AFont: TFont);
  end;

  IdxLayoutLookAndFeelsDesigner = interface
    ['{EA24F329-D483-4D4F-A72C-1F67EC4F7E21}']
    procedure ComponentNameChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
    procedure ItemsChanged(ALookAndFeelList: TdxLayoutLookAndFeelList);
    procedure Edit(ALookAndFeelList: TdxLayoutLookAndFeelList);
  end;

var
  dxLayoutTextMetrics: TdxLayoutTextMetrics;
  dxLayoutLookAndFeelDefs: TdxLayoutLookAndFeelDefs;
  dxLayoutDefaultLookAndFeel: TdxCustomLayoutLookAndFeel;
  dxLayoutLookAndFeelsDesigner: IdxLayoutLookAndFeelsDesigner;

function dxLayoutDefaultLookAndFeelClass: TdxCustomLayoutLookAndFeelClass;

implementation

uses
  UxTheme, Math, SysUtils, Forms, cxControls, dxLayoutContainer, dxLayoutPainters, dxDPIAwareUtils;

type
  TControlAccess = class(TControl);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TdxLayoutContainerViewInfoAccess = class(TdxLayoutContainerViewInfo);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxLayoutGroupViewInfoAccess = class(TdxLayoutGroupViewInfo);
  TdxLayoutGroupCaptionViewInfoAccess = class(TdxLayoutGroupCaptionViewInfo);
  TdxLayoutGroupButtonsViewInfoAccess = class(TdxLayoutGroupButtonsViewInfo);

const
  CaptionSideToCaptionPosition: array[TdxLayoutSide] of TcxGroupBoxCaptionPosition = (cxgpLeft, cxgpRight, cxgpTop, cxgpBottom);

function dxLayoutDefaultLookAndFeelClass: TdxCustomLayoutLookAndFeelClass;
begin
  Result := dxLayoutLookAndFeelDefs[0];
end;

function GroupViewInfo(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxLayoutGroupViewInfoAccess;
begin
  Result := TdxLayoutGroupViewInfoAccess(AViewInfo as TdxLayoutGroupViewInfo);
end;

function GroupCaptionViewInfo(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxLayoutGroupCaptionViewInfoAccess;
begin
  Result := TdxLayoutGroupCaptionViewInfoAccess(GroupViewInfo(AViewInfo).CaptionViewInfo as TdxLayoutGroupCaptionViewInfo);
end;

function GroupButtonsViewInfo(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxLayoutGroupButtonsViewInfo;
begin
  Result := TdxLayoutGroupButtonsViewInfoAccess(GroupViewInfo(AViewInfo).ButtonsViewInfo as TdxLayoutGroupButtonsViewInfo);
end;

function ContainerViewInfo(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxLayoutContainerViewInfoAccess;
begin
  Result := TdxLayoutContainerViewInfoAccess(GroupViewInfo(AViewInfo).ContainerViewInfo);
end;

function GetOppositeSide(ASide: TdxLayoutSide): TdxLayoutSide;
const
  ResultMap: array[TdxLayoutSide] of TdxLayoutSide = (sdRight, sdLeft, sdBottom, sdTop);
begin
  Result := ResultMap[ASide];
end;

{ TdxCustomLayoutLookAndFeelPart }

constructor TdxCustomLayoutLookAndFeelPart.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited Create;
  FLookAndFeel := ALookAndFeel;
end;

procedure TdxCustomLayoutLookAndFeelPart.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomLayoutLookAndFeelPart.EndUpdate;
begin
  CancelUpdate;
  if FLockCount = 0 then
    Changed;
end;

procedure TdxCustomLayoutLookAndFeelPart.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxCustomLayoutLookAndFeelPart.Changed;
begin
  if FLockCount = 0 then
    FLookAndFeel.Changed;
end;

procedure TdxCustomLayoutLookAndFeelPart.ChangeScale(M, D: Integer);
begin
  BeginUpdate;
  try
    ChangeScaleCore(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomLayoutLookAndFeelPart.ChangeScaleCore(M, D: Integer);
begin
// do nothing
end;

{ TdxLayoutLookAndFeelCaptionOptions }

constructor TdxLayoutLookAndFeelCaptionOptions.Create(AOptions: TdxCustomLayoutLookAndFeelOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FScaledFont := TFont.Create;
  FHotTrackStyles := [htsHandPoint, htsUnderlineHot];
  FTextColor := clDefault;
  FTextDisabledColor := clDefault;
  FTextHotColor := clDefault;
  FUseDefaultFont := True;
end;

destructor TdxLayoutLookAndFeelCaptionOptions.Destroy;
begin
  dxLayoutTextMetrics.Unregister(FScaledFont);
  dxLayoutTextMetrics.Unregister(FFont);
  FreeAndNil(FScaledFont);
  FreeAndNil(FFont);
  inherited;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.Assign(Source: TPersistent);
begin
  if Source is TdxLayoutLookAndFeelCaptionOptions then
  begin
    with Source as TdxLayoutLookAndFeelCaptionOptions do
    begin
      Self.Font := Font;
      Self.HotTrack := HotTrack;
      Self.HotTrackStyles := HotTrackStyles;
      Self.TextColor := TextColor;
      Self.TextDisabledColor := TextDisabledColor;
      Self.TextHotColor := TextHotColor;
      Self.UseDefaultFont := UseDefaultFont;
    end;
  end
  else
    inherited;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetFont(Value: TFont);
begin
  FFont.Assign(Value)
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetHotTrackStyles(Value: TdxLayoutHotTrackStyles);
begin
  if FHotTrackStyles <> Value then
  begin
    FHotTrackStyles := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetTextColor(Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetTextDisabledColor(Value: TColor);
begin
  if FTextDisabledColor <> Value then
  begin
    FTextDisabledColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetTextHotColor(Value: TColor);
begin
  if FTextHotColor <> Value then
  begin
    FTextHotColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.SetUseDefaultFont(Value: Boolean);
begin
  if FUseDefaultFont <> Value then
  begin
    FUseDefaultFont := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.FontChanged(Sender: TObject);
begin
  FUseDefaultFont := False;
  Changed;
end;

function TdxLayoutLookAndFeelCaptionOptions.IsFontStored: Boolean;
begin
  Result := not FUseDefaultFont;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.Changed;
begin
  FOptions.Changed;
end;

procedure TdxLayoutLookAndFeelCaptionOptions.ChangeScale(M, D: Integer);
begin
  if not UseDefaultFont then
    FFont.Height := MulDiv(FFont.Height, M, D);
end;

function TdxLayoutLookAndFeelCaptionOptions.GetDefaultTextDisabledColor: TColor;
begin
  Result := clDefault;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetDefaultTextHotColor: TColor;
begin
  Result := GetHotTrackColor;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetDefaultFont(AContainer: TComponent): TFont;
begin
  Result := TdxLayoutContainerAccess(AContainer).GetDefaultFont;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetTextColor: TColor;
begin
  Result := FTextColor;
  if Result = clDefault then
    Result := GetDefaultTextColor;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetTextDisabledColor: TColor;
begin
  Result := FTextDisabledColor;
  if Result = clDefault then
    Result := GetDefaultTextDisabledColor;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetTextHotColor: TColor;
begin
  Result := FTextHotColor;
  if Result = clDefault then
    Result := GetDefaultTextHotColor;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetFont(AContainer: TComponent): TFont;
var
  AScaleFactor: IdxScaleFactor;
begin
  if FUseDefaultFont then
    Result := GetDefaultFont(AContainer)
  else
    if Supports(AContainer, IdxScaleFactor, AScaleFactor) and not AScaleFactor.Value.Equals(ScaleFactor) then
    begin
      dxAssignFont(FScaledFont, FFont, AScaleFactor.Value, ScaleFactor);
      Result := FScaledFont;
    end
    else
      Result := FFont;
end;

function TdxLayoutLookAndFeelCaptionOptions.GetScaleFactor: TdxScaleFactor;
begin
  Result := Options.LookAndFeel.ScaleFactor;
end;

{ TdxLayoutLookAndFeelPadding }

procedure TdxLayoutLookAndFeelPadding.Assign(Source: TPersistent);
var
  ASource: TdxLayoutLookAndFeelPadding;
begin
  if Source is TdxLayoutLookAndFeelPadding then
  begin
    ASource := TdxLayoutLookAndFeelPadding(Source);
    Bottom := ScaleFactor.Apply(ASource.Bottom, ASource.ScaleFactor);
    Left := ScaleFactor.Apply(ASource.Left, ASource.ScaleFactor);
    Right := ScaleFactor.Apply(ASource.Right, ASource.ScaleFactor);
    Top := ScaleFactor.Apply(ASource.Top, ASource.ScaleFactor);
  end
  else
    inherited;
end;

function TdxLayoutLookAndFeelPadding.GetPaddingRect: TRect;
begin
  Result := Rect(Left, Top, Right, Bottom);
end;

function TdxLayoutLookAndFeelPadding.GetPaddingRect(ATargetScaleFactor: TdxScaleFactor): TRect;
begin
  Result := ATargetScaleFactor.Apply(GetPaddingRect, ScaleFactor);
end;

procedure TdxLayoutLookAndFeelPadding.ChangeScaleCore(M, D: Integer);
begin
  inherited;
  FBottom := MulDiv(FBottom, M, D);
  FLeft := MulDiv(FLeft, M, D);
  FRight := MulDiv(FRight, M, D);
  FTop := MulDiv(FTop, M, D);
end;

function TdxLayoutLookAndFeelPadding.GetDefaultValue(Index: Integer): Integer;
begin
  Result := 0;
end;

procedure TdxLayoutLookAndFeelPadding.SetValue(Index: Integer; Value: Integer);
begin
  if GetValue(Index) <> Value then
  begin
    Include(FAssignedValues, TdxLayoutPaddingAssignedValue(Index - 1));
    case Index of
      1: FBottom := Value;
      2: FLeft := Value;
      3: FRight := Value;
      4: FTop := Value;
    end;
    Changed;
  end;
end;

function TdxLayoutLookAndFeelPadding.GetScaleFactor: TdxScaleFactor;
begin
  Result := LookAndFeel.ScaleFactor;
end;

function TdxLayoutLookAndFeelPadding.GetValue(Index: Integer): Integer;
begin
  if TdxLayoutPaddingAssignedValue(Index - 1) in AssignedValues then
    case Index of
      1: Result := FBottom;
      2: Result := FLeft;
      3: Result := FRight;
      4: Result := FTop;
    else
      Result := 0;
    end
  else
    Result := GetDefaultValue(Index);
end;

function TdxLayoutLookAndFeelPadding.IsValueStored(Index: Integer): Boolean;
begin
  Result := TdxLayoutPaddingAssignedValue(Index - 1) in AssignedValues;
end;

procedure TdxLayoutLookAndFeelPadding.SetAssignedValues(Value: TdxLayoutPaddingAssignedValues);
begin
  if FAssignedValues <> Value then
  begin
    FAssignedValues := Value;
    Changed;
  end;
end;

{ TdxCustomLayoutLookAndFeelOptions }

constructor TdxCustomLayoutLookAndFeelOptions.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited Create(ALookAndFeel);
  FCaptionOptions := GetCaptionOptionsClass.Create(Self);
  FPadding := GetPaddingClass.Create(LookAndFeel);
end;

destructor TdxCustomLayoutLookAndFeelOptions.Destroy;
begin
  FreeAndNil(FPadding);
  FreeAndNil(FCaptionOptions);
  inherited Destroy;
end;

procedure TdxCustomLayoutLookAndFeelOptions.Assign(Source: TPersistent);
begin
  if Source is TdxCustomLayoutLookAndFeelOptions then
  begin
    with Source as TdxCustomLayoutLookAndFeelOptions do
    begin
      Self.CaptionOptions := CaptionOptions;
      Self.Padding := Padding;
    end;
  end
  else
    inherited;
end;

procedure TdxCustomLayoutLookAndFeelOptions.ChangeScaleCore(M, D: Integer);
begin
  inherited;
  CaptionOptions.ChangeScale(M, D);
  Padding.ChangeScale(M, D);
end;

function TdxCustomLayoutLookAndFeelOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutLookAndFeelCaptionOptions;
end;

function TdxCustomLayoutLookAndFeelOptions.GetPaddingClass: TdxLayoutLookAndFeelPaddingClass;
begin
  Result := TdxLayoutLookAndFeelPadding;
end;

procedure TdxCustomLayoutLookAndFeelOptions.SetCaptionOptions(Value: TdxLayoutLookAndFeelCaptionOptions);
begin
  FCaptionOptions.Assign(Value);
end;

procedure TdxCustomLayoutLookAndFeelOptions.SetPadding(const Value: TdxLayoutLookAndFeelPadding);
begin
  FPadding.Assign(Value);
end;

{ TdxLayoutLookAndFeelGroupOptions }

constructor TdxLayoutLookAndFeelGroupOptions.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited;
  FColor := clDefault;
end;

procedure TdxLayoutLookAndFeelGroupOptions.Assign(Source: TPersistent);
begin
  if Source is TdxLayoutLookAndFeelGroupOptions then
  begin
    with Source as TdxLayoutLookAndFeelGroupOptions do
    begin
      Self.Color := Color;
      Self.SpaceBetweenButtons := SpaceBetweenButtons;
    end;
  end;
  inherited;
end;

procedure TdxLayoutLookAndFeelGroupOptions.ChangeScaleCore(M, D: Integer);
begin
  inherited;
  SpaceBetweenButtons := MulDiv(SpaceBetweenButtons, M, D);
end;

procedure TdxLayoutLookAndFeelGroupOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelGroupOptions.SetSpaceBetweenButtons(AValue: Cardinal);
begin
  if FSpaceBetweenButtons <> AValue then
  begin
    FSpaceBetweenButtons := AValue;
    Changed;
  end;
end;

function TdxLayoutLookAndFeelGroupOptions.GetColor: TColor;
begin
  Result := FColor;
  if Result = clDefault then
    Result := GetDefaultColor;
end;

{ TdxLayoutLookAndFeelItemOptions }

constructor TdxLayoutLookAndFeelItemOptions.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited;
  FControlBorderColor := clDefault;
  FControlBorderStyle := lbsStandard;
end;

procedure TdxLayoutLookAndFeelItemOptions.Assign(Source: TPersistent);
begin
  if Source is TdxLayoutLookAndFeelItemOptions then
  begin
    with Source as TdxLayoutLookAndFeelItemOptions do
    begin
      Self.ControlBorderColor := ControlBorderColor;
      Self.ControlBorderStyle := ControlBorderStyle;
    end;
  end;
  inherited;
end;

procedure TdxLayoutLookAndFeelItemOptions.SetControlBorderColor(Value: TColor);
begin
  if FControlBorderColor <> Value then
  begin
    FControlBorderColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutLookAndFeelItemOptions.SetControlBorderStyle(Value: TdxLayoutBorderStyle);
begin
  if FControlBorderStyle <> Value then
  begin
    FControlBorderStyle := Value;
    Changed;
  end;
end;

function TdxLayoutLookAndFeelItemOptions.GetDefaultControlBorderColor: TColor;
begin
  Result := clWindowFrame;
end;

function TdxLayoutLookAndFeelItemOptions.GetControlBorderColor: TColor;
begin
  Result := FControlBorderColor;
  if Result = clDefault then
    Result := GetDefaultControlBorderColor;
end;

{ TdxLayoutLookAndFeelOffsets }

constructor TdxLayoutLookAndFeelOffsets.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
var
  I: Integer;
begin
  inherited;
  BeginUpdate;
  for I := 0 to FValuesCount - 1 do
    SetValue(I, GetDefaultValue(I));
  CancelUpdate;
end;

procedure TdxLayoutLookAndFeelOffsets.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TdxLayoutLookAndFeelOffsets then
    for I := 0 to FValuesCount - 1 do
      SetValue(I, TdxLayoutLookAndFeelOffsets(Source).GetValue(I))
  else
    inherited;
end;

procedure TdxLayoutLookAndFeelOffsets.ChangeScaleCore(M, D: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FValuesCount - 1 do
    SetValue(I, MulDiv(GetValue(I), M, D));
end;

function TdxLayoutLookAndFeelOffsets.GetDefaultValue(Index: Integer): Integer;
begin
  case Index of
   0: Result := 3;
   1: Result := 3;
   2: Result := 4;
   3: Result := 0;
   4: Result := 0;
   5: Result := 7;
   6: Result := 7;
   7: Result := 0;
   8: Result := 0;
  else
    Result := 0;
  end;
  Result := LookAndFeel.ScaleFactor.Apply(Result);
end;

function TdxLayoutLookAndFeelOffsets.GetValue(Index: Integer): Integer;
begin
  case Index of
   0: Result := FControlOffsetHorz;
   1: Result := FControlOffsetVert;
   2: Result := FItemOffset;
   3: Result := FItemsAreaOffsetHorz;
   4: Result := FItemsAreaOffsetVert;
   5: Result := FRootItemsAreaOffsetHorz;
   6: Result := FRootItemsAreaOffsetVert;
   7: Result := FTabSheetClientOffsetHorz;
   8: Result := FTabSheetClientOffsetVert;
  else
    Result := 0;
  end;
end;

function TdxLayoutLookAndFeelOffsets.IsValueStored(Index: Integer): Boolean;
begin
  Result := GetValue(Index) <> GetDefaultValue(Index);
end;

procedure TdxLayoutLookAndFeelOffsets.SetValue(Index: Integer; Value: Integer);
begin
  if GetValue(Index) <> Value then
  begin
    case Index of
     0: FControlOffsetHorz := Value;
     1: FControlOffsetVert := Value;
     2: FItemOffset := Value;
     3: FItemsAreaOffsetHorz := Value;
     4: FItemsAreaOffsetVert := Value;
     5: FRootItemsAreaOffsetHorz := Value;
     6: FRootItemsAreaOffsetVert := Value;
     7: FTabSheetClientOffsetHorz := Value;
     8: FTabSheetClientOffsetVert := Value;
    end;
    Changed;
  end;
end;

{ TdxCustomLayoutLookAndFeel }

constructor TdxCustomLayoutLookAndFeel.Create(AOwner: TComponent);
begin
  inherited;
  FUsers := TList.Create;
  FGroupOptions := GetGroupOptionsClass.Create(Self);
  FItemOptions := GetItemOptionsClass.Create(Self);
  FOffsets := GetOffsetsClass.Create(Self);

  FLookAndFeel := TcxLookAndFeel.Create(Self);
  InitLookAndFeel;
end;

destructor TdxCustomLayoutLookAndFeel.Destroy;
begin
  NotifyUsersAboutDestroying;
  if FList <> nil then
    FList.RemoveItem(Self);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FUsers);
  FreeAndNil(FOffsets);
  FreeAndNil(FItemOptions);
  FreeAndNil(FGroupOptions);
  inherited;
end;

procedure TdxCustomLayoutLookAndFeel.Assign(Source: TPersistent);
begin
  if Source is TdxCustomLayoutLookAndFeel then
  begin
    with Source as TdxCustomLayoutLookAndFeel do
    begin
      Self.GroupOptions := GroupOptions;
      Self.ItemOptions := ItemOptions;
      Self.Offsets := Offsets;
      Self.LookAndFeel := LookAndFeel;
      Self.ScaleFactor.Assign(ScaleFactor);
    end;
  end
  else
    inherited;
end;

procedure TdxCustomLayoutLookAndFeel.AssignTo(var ALookAndFeel: TdxCustomLayoutLookAndFeel; ATargetPPI: Integer = 0);
var
  AOwner: TComponent;
begin
  if ALookAndFeel.ClassType <> ClassType then
  begin
    AOwner := ALookAndFeel.Owner;
    FreeAndNil(ALookAndFeel);
    ALookAndFeel := TdxCustomLayoutLookAndFeelClass(ClassType).Create(AOwner);
  end;
  ALookAndFeel.Assign(Self);
  if InRange(ATargetPPI, dxMinDPI, dxMaxDPI) then
    ALookAndFeel.ChangeScale(ATargetPPI * ScaleFactor.Denominator, dxDefaultDPI * ScaleFactor.Numerator);
end;

function TdxCustomLayoutLookAndFeel.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxCustomLayoutLookAndFeel.GetUser(Index: Integer): IdxLayoutLookAndFeelUser;
begin
  TComponent(FUsers[Index]).GetInterface(IdxLayoutLookAndFeelUser, Result);
end;

function TdxCustomLayoutLookAndFeel.GetUserCount: Integer;
begin
  Result := FUsers.Count;
end;

procedure TdxCustomLayoutLookAndFeel.SetGroupOptions(Value: TdxLayoutLookAndFeelGroupOptions);
begin
  FGroupOptions.Assign(Value);
end;

procedure TdxCustomLayoutLookAndFeel.SetItemOptions(Value: TdxLayoutLookAndFeelItemOptions);
begin
  FItemOptions.Assign(Value);
end;

procedure TdxCustomLayoutLookAndFeel.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TdxCustomLayoutLookAndFeel.SetOffsets(Value: TdxLayoutLookAndFeelOffsets);
begin
  FOffsets.Assign(Value);
end;

procedure TdxCustomLayoutLookAndFeel.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  BeginUpdate;
  try
    GroupOptions.ChangeScale(M, D);
    ItemOptions.ChangeScale(M, D);
    Offsets.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomLayoutLookAndFeel.SetName(const Value: TComponentName);
begin
  inherited;
  if dxLayoutLookAndFeelsDesigner <> nil then
    dxLayoutLookAndFeelsDesigner.ItemsChanged(FList);
end;

procedure TdxCustomLayoutLookAndFeel.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value is TdxLayoutLookAndFeelList then
    TdxLayoutLookAndFeelList(Value).AddItem(Self);
end;

procedure TdxCustomLayoutLookAndFeel.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomLayoutLookAndFeel.EndUpdate;
begin
  CancelUpdate;
  if FLockCount = 0 then
    Changed;
end;

procedure TdxCustomLayoutLookAndFeel.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxCustomLayoutLookAndFeel.Changed;
var
  I: Integer;
begin
  if FLockCount > 0 then
    Exit;
  for I := 0 to UserCount - 1 do
    Users[I].LookAndFeelChanged;
end;

function TdxCustomLayoutLookAndFeel.CanDrawSpecificBackground: Boolean;
begin
  Result := True;
end;

function TdxCustomLayoutLookAndFeel.DoesCxLookAndFeelHavePriority: Boolean;
begin
  Result := False;
end;

class function TdxCustomLayoutLookAndFeel.GetBaseName: string;
begin
  Result := ClassName;
  Delete(Result, 1, 1);
end;

procedure TdxCustomLayoutLookAndFeel.GetTextMetric(AFont: TFont; var ATextMetric: TTextMetric);
begin
  dxLayoutTextMetrics.Get(AFont, ATextMetric);
end;

function TdxCustomLayoutLookAndFeel.GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  Result := 0;
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionFont(AContainer: TComponent): TFont;
begin
  Result := FGroupOptions.CaptionOptions.GetFont(AContainer);
end;

function TdxCustomLayoutLookAndFeel.IsGroupTransparent(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean;
var
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
begin
  AGroupViewInfo := GroupViewInfo(AViewInfo);
  Result := AGroupViewInfo.IsDefaultColor and
    (IsNativeStyle or (ContainerViewInfo(AViewInfo).IsTransparentBackground and ((AGroupViewInfo.ParentViewInfo <> nil) or AGroupViewInfo.Group.IsRoot)));
end;

function TdxCustomLayoutLookAndFeel.IsNativeStyle: Boolean;
begin
  Result := LookAndFeel.NativeStyle;
end;

function TdxCustomLayoutLookAndFeel.IsSkinPainterUsed: Boolean;
begin
  Result := False;
end;

function TdxCustomLayoutLookAndFeel.GetItemCaptionFont(AContainer: TComponent): TFont;
begin
  Result := FItemOptions.CaptionOptions.GetFont(AContainer);
end;

procedure TdxCustomLayoutLookAndFeel.InitLookAndFeel;
begin
// do nothing
end;

procedure TdxCustomLayoutLookAndFeel.NotifyUsersAboutDestroying;

  procedure BeginNotification;
  var
    I: Integer;
  begin
    FNotifyingAboutDestroying := True;
    for I := 0 to UserCount - 1 do
      Users[I].BeginLookAndFeelDestroying;
  end;

  procedure Notify;
  var
    I: Integer;
  begin
    for I := 0 to UserCount - 1 do
      Users[I].LookAndFeelDestroyed;
  end;

  procedure EndNotification;
  var
    I: Integer;
  begin
    for I := 0 to UserCount - 1 do
      Users[I].EndLookAndFeelDestroying;
    FNotifyingAboutDestroying := False;
  end;

begin
  BeginNotification;
  try
    Notify;
  finally
    EndNotification;
  end;
end;

function TdxCustomLayoutLookAndFeel.GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass;
begin
  Result := TdxLayoutLookAndFeelGroupOptions;
end;

function TdxCustomLayoutLookAndFeel.GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass;
begin
  Result := TdxLayoutLookAndFeelItemOptions;
end;

function TdxCustomLayoutLookAndFeel.GetOffsetsClass: TdxLayoutLookAndFeelOffsetsClass;
begin
  Result := TdxLayoutLookAndFeelOffsets;
end;

function TdxCustomLayoutLookAndFeel.GetParentComponent: TComponent;
begin
  Result := FList;
end;

function TdxCustomLayoutLookAndFeel.HasParent: Boolean;
begin
  Result := True;
end;

function TdxCustomLayoutLookAndFeel.NeedDoubleBuffered: Boolean;
begin
  Result := False;
end;

function TdxCustomLayoutLookAndFeel.NeedRedrawOnResize: Boolean;
begin
  Result := False;
end;

procedure TdxCustomLayoutLookAndFeel.InitializeSubControlCxLookAndFeel(AcxLookAndFeel: TcxLookAndFeel);
begin
  AcxLookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxCustomLayoutLookAndFeel.DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, GetEmptyAreaColor);
end;

procedure TdxCustomLayoutLookAndFeel.DrawItemControlBorder(ACanvas: TcxCanvas; AItemOptions: TdxLayoutLookAndFeelItemOptions; R: TRect);

  procedure DrawSingleBorder;
  begin
    ACanvas.FrameRect(R, AItemOptions.GetControlBorderColor);
  end;

  procedure DrawStandardBorder;
  begin
    ACanvas.DrawEdge(R, True, True);
    InflateRect(R, -1, -1);
    ACanvas.DrawEdge(R, True, False);
  end;

  procedure DrawFlatBorder;
  begin
    ACanvas.DrawEdge(R, True, True);
    InflateRect(R, -1, -1);
    ACanvas.FrameRect(R, clBtnFace);
  end;

begin
  case AItemOptions.ControlBorderStyle of
    lbsSingle:
      DrawSingleBorder;
    lbsFlat:
      DrawFlatBorder;
    lbsStandard:
      DrawStandardBorder;
  end;
end;

procedure TdxCustomLayoutLookAndFeel.AddUser(AUser: TComponent);
begin
  FUsers.Add(AUser);
end;

procedure TdxCustomLayoutLookAndFeel.RemoveUser(AUser: TComponent);
begin
  if FNotifyingAboutDestroying then Exit;
  FUsers.Remove(AUser);
end;

class function TdxCustomLayoutLookAndFeel.Description: string;
begin
  Result := '';
end;

function TdxCustomLayoutLookAndFeel.DLUToPixels(AFont: TFont; ADLU: Integer): Integer;
var
  ATextMetric: TTextMetric;
begin
  GetTextMetric(AFont, ATextMetric);
  Result := (MulDiv(ADLU, ATextMetric.tmAveCharWidth, 4) + MulDiv(ADLU, ATextMetric.tmHeight, 8)) div 2;
end;

function TdxCustomLayoutLookAndFeel.HDLUToPixels(AFont: TFont; ADLU: Integer): Integer;
var
  ATextMetric: TTextMetric;
begin
  GetTextMetric(AFont, ATextMetric);
  Result := MulDiv(ADLU, ATextMetric.tmAveCharWidth, 4);
end;

function TdxCustomLayoutLookAndFeel.VDLUToPixels(AFont: TFont; ADLU: Integer): Integer;
var
  ATextMetric: TTextMetric;
begin
  GetTextMetric(AFont, ATextMetric);
  Result := MulDiv(ADLU, ATextMetric.tmHeight, 8);
end;

function TdxCustomLayoutLookAndFeel.GetBasicItemPainterClass: TClass;
begin
  Result := TdxLayoutBasicItemPainter;
end;

function TdxCustomLayoutLookAndFeel.GetEmptySpaceItemPainterClass: TClass;
begin
  Result := TdxLayoutEmptySpaceItemPainter;
end;

function TdxCustomLayoutLookAndFeel.GetItemPainterClass: TClass{TdxLayoutItemPainterClass};
begin
  Result := TdxLayoutItemPainter;
end;

function TdxCustomLayoutLookAndFeel.GetLabeledItemPainterClass: TClass{TdxLayoutLabeledItemPainterClass};
begin
  Result := TdxLayoutLabeledItemPainter;
end;

function TdxCustomLayoutLookAndFeel.GetSeparatorItemPainterClass: TClass{TdxLayoutSeparatorItemPainterClass};
begin
  Result := TdxLayoutSeparatorItemPainter;
end;

function TdxCustomLayoutLookAndFeel.GetSplitterItemPainterClass: TClass{TdxLayoutSplitterItemPainterClass};
begin
  Result := TdxLayoutSplitterItemPainter;
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupButtonsAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect);
begin
// do nothing
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupCaptionAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect);
begin
// do nothing
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation);
begin
// do nothing
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupMinVisibleHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer);
begin
  CorrectGroupMinVisibleSize(AViewInfo, AHeight, orVertical);
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupMinVisibleWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer);
begin
  CorrectGroupMinVisibleSize(AViewInfo, AWidth, orHorizontal);
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupCaptionFont(AViewInfo: TObject {TdxLayoutGroupViewInfo}; AFont: TFont);
begin
  // do nothing
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupCaptionHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer);
begin
  // do nothing
end;

procedure TdxCustomLayoutLookAndFeel.CorrectGroupCaptionWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer);
begin
  // do nothing
end;

function TdxCustomLayoutLookAndFeel.GetElementOffsetHorz(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetItemCaptionFont(AContainer), ScaleFactor.Revert(Offsets.ControlOffsetHorz));
end;

function TdxCustomLayoutLookAndFeel.GetElementOffsetVert(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetItemCaptionFont(AContainer), ScaleFactor.Revert(Offsets.ControlOffsetVert));
end;

function TdxCustomLayoutLookAndFeel.GetGroupBorderWidth(AContainer: TComponent;
  ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer;
begin
  Result := 0;
end;

function TdxCustomLayoutLookAndFeel.GetGroupButtonsOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  Result := GetGroupCaptionOffset(AViewInfo);
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  Result := GroupViewInfo(AViewInfo).GetBorderWidth(sdLeft);
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  Result := GroupCaptionViewInfo(AViewInfo).Padding;
end;

function TdxCustomLayoutLookAndFeel.GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  Result := GroupViewInfo(AViewInfo).Bounds;
end;

function TdxCustomLayoutLookAndFeel.GetGroupRestSpaceBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  Result := GroupViewInfo(AViewInfo).Bounds;
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionAlignHorz(AViewInfo: TObject {TdxCustomLayoutItemViewInfo}): TAlignment;
begin
  Result := GroupViewInfo(AViewInfo).Group.CaptionOptions.AlignHorz;
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionAlignVert(AViewInfo: TObject {TdxCustomLayoutItemViewInfo}): TdxAlignmentVert;
begin
  Result := GroupViewInfo(AViewInfo).Group.CaptionOptions.AlignVert;
end;

function TdxCustomLayoutLookAndFeel.GetItemControlBorderWidth(ASide: TdxLayoutSide): Integer;
begin
  case FItemOptions.ControlBorderStyle of
    lbsSingle:
      Result := 1;
    lbsFlat:
      Result := 2;
    lbsStandard:
      Result := 2;
  else
    Result := 0;
  end
end;

function TdxCustomLayoutLookAndFeel.GetItemOffset(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetItemCaptionFont(AContainer), ScaleFactor.Revert(Offsets.ItemOffset));
end;

function TdxCustomLayoutLookAndFeel.GetGroupBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), 7);
end;

function TdxCustomLayoutLookAndFeel.GetTabControlBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  Result := GetGroupBorderOffset(AContainer, ASide, ACaptionSide);
end;

function TdxCustomLayoutLookAndFeel.GetItemsAreaOffsetHorz(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.ItemsAreaOffsetHorz));
end;

function TdxCustomLayoutLookAndFeel.GetItemsAreaOffsetVert(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.ItemsAreaOffsetVert));
end;

function TdxCustomLayoutLookAndFeel.GetRootItemsAreaOffsetHorz(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.RootItemsAreaOffsetHorz));
end;

function TdxCustomLayoutLookAndFeel.GetRootItemsAreaOffsetVert(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.RootItemsAreaOffsetVert));
end;

function TdxCustomLayoutLookAndFeel.GetTabSheetContentOffsetHorz(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.TabSheetContentOffsetHorz));
end;

function TdxCustomLayoutLookAndFeel.GetTabSheetContentOffsetVert(AContainer: TComponent): Integer;
begin
  Result := DLUToPixels(GetGroupCaptionFont(AContainer), ScaleFactor.Revert(Offsets.TabSheetContentOffsetVert));
end;

function TdxCustomLayoutLookAndFeel.GetSeparatorItemMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(6);
end;

function TdxCustomLayoutLookAndFeel.GetSplitterItemMinSize: TSize;
begin
  Result := LookAndFeel.Painter.GetScaledSplitterSize(True, ScaleFactor);
end;

function TdxCustomLayoutLookAndFeel.IsButtonHotTrack: Boolean;
begin
  Result := LookAndFeel.Painter.IsButtonHotTrack;
end;

function TdxCustomLayoutLookAndFeel.GetEmptyAreaColor: TColor;
begin
  Result := FGroupOptions.GetColor;
end;

function TdxCustomLayoutLookAndFeel.GetGroupCaptionColor(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TColor;
begin
  Result := GroupViewInfo(AViewInfo).Color;
end;

function TdxCustomLayoutLookAndFeel.GetGroupButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TdxCustomLayoutLookAndFeel.GetItemCaptionColorPalette: IdxColorPalette;
begin
  Result := nil;
end;

function TdxCustomLayoutLookAndFeel.GetTabbedGroupCaptionColorPalette(AIsActive: Boolean): IdxColorPalette;
begin
  Result := nil;
end;

{ TdxLayoutStandardLookAndFeelGroupCaptionOptions }

function TdxLayoutStandardLookAndFeelGroupCaptionOptions.GetDefaultTextColor: TColor;
begin
  if cxIsVCLThemesEnabled then
    Result := clDefault
  else
    Result := clBtnText;
end;

{ TdxLayoutStandardLookAndFeelGroupOptions }

function TdxLayoutStandardLookAndFeelGroupOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutStandardLookAndFeelGroupCaptionOptions;
end;

function TdxLayoutStandardLookAndFeelGroupOptions.GetDefaultColor: TColor;
begin
  Result := clBtnFace;
end;

{ TdxLayoutStandardLookAndFeelItemCaptionOptions }

function TdxLayoutStandardLookAndFeelItemCaptionOptions.GetDefaultTextColor: TColor;
begin
  Result := clBtnText;
end;

{ TdxLayoutStandardLookAndFeelItemOptions }

function TdxLayoutStandardLookAndFeelItemOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutStandardLookAndFeelItemCaptionOptions;
end;

{ TdxLayoutStandardLookAndFeel }

function TdxLayoutStandardLookAndFeel.GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass;
begin
  Result := TdxLayoutStandardLookAndFeelGroupOptions;
end;

function TdxLayoutStandardLookAndFeel.GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass;
begin
  Result := TdxLayoutStandardLookAndFeelItemOptions;
end;

function TdxLayoutStandardLookAndFeel.GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  Result := 2;
end;

procedure TdxLayoutStandardLookAndFeel.InitLookAndFeel;
begin
  FLookAndFeel.Kind := lfStandard;
  FLookAndFeel.NativeStyle := cxIsVCLThemesEnabled;
  FLookAndFeel.SkinName := '';
end;

class function TdxLayoutStandardLookAndFeel.Description: string;
begin
  Result := 'Standard';
end;

function TdxLayoutStandardLookAndFeel.GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass};
begin
  Result := TdxLayoutGroupStandardPainter;
end;

procedure TdxLayoutStandardLookAndFeel.CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation);
begin
  if (AOrientation = orVertical) and GroupViewInfo(AViewInfo).IsVerticalCaption or
     (AOrientation = orHorizontal) and not GroupViewInfo(AViewInfo).IsVerticalCaption then
  begin
    if GroupViewInfo(AViewInfo).HasCaption then
      Inc(ASize, 2 * GetGroupCaptionOffset(AViewInfo))
    else
      if GroupViewInfo(AViewInfo).HasButtons then
        Inc(ASize, 2 * GetGroupButtonsOffset(AViewInfo));
  end;
end;

function TdxLayoutStandardLookAndFeel.GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  Result := HDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 7) - 2;
end;

function TdxLayoutStandardLookAndFeel.GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
    Result := Rect(0, 2, 0, 2)
  else
    Result := Rect(2, 0, 2, 0);
end;

function TdxLayoutStandardLookAndFeel.GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
var
  AIndent: Integer;
begin
  Result := GroupViewInfo(AViewInfo).Bounds;
  AIndent := VDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 4) - FrameWidths[GroupViewInfo(AViewInfo).CaptionSide, GroupViewInfo(AViewInfo).CaptionSide] div 2;
  case GroupViewInfo(AViewInfo).CaptionSide of
    sdTop, sdBottom:
      AIndent := AIndent + (GroupCaptionViewInfo(AViewInfo).GetAvailableHeight - GroupCaptionViewInfo(AViewInfo).GetAvailableTextHeight) div 2;
    sdLeft, sdRight:
      AIndent := AIndent + (GroupCaptionViewInfo(AViewInfo).GetAvailableWidth - GroupCaptionViewInfo(AViewInfo).GetAvailableTextWidth) div 2;
  end;
  case GroupViewInfo(AViewInfo).CaptionSide of
    sdLeft:
      Result.Left := GroupViewInfo(AViewInfo).FCaptionAreaBounds.Left + AIndent;
    sdTop:
      Result.Top := GroupViewInfo(AViewInfo).FCaptionAreaBounds.Top + AIndent;
    sdRight:
      Result.Right := GroupViewInfo(AViewInfo).FCaptionAreaBounds.Right - AIndent;
    sdBottom:
      Result.Bottom := GroupViewInfo(AViewInfo).FCaptionAreaBounds.Bottom - AIndent;
  end;
end;

function TdxLayoutStandardLookAndFeel.GetGroupBorderWidth(AContainer: TComponent;
  ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer;
var
  AFont: TFont;
begin
  AFont := GetGroupCaptionFont(AContainer);
  if ASide = ACaptionSide then
    Result := VDLUToPixels(AFont, 4) + FrameWidths[ASide, ACaptionSide] div 2 + GetGroupBorderOffset(AContainer, ASide, ACaptionSide)
  else
    Result := FrameWidths[ASide, ACaptionSide] + IfThen(AIsExpanded or (ASide <> GetOppositeSide(ACaptionSide)), GetGroupBorderOffset(AContainer, ACaptionSide, ASide), 0);
end;

{ TdxLayoutOfficeLookAndFeel }

function TdxLayoutOfficeLookAndFeel.GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  if ASide = ACaptionSide then
    Result := inherited GetFrameWidth(ASide, ACaptionSide)
  else
    Result := 0;
end;

procedure TdxLayoutOfficeLookAndFeel.InitLookAndFeel;
begin
  FLookAndFeel.Kind := lfFlat;
  FLookAndFeel.NativeStyle := False;
  FLookAndFeel.SkinName := '';
end;

class function TdxLayoutOfficeLookAndFeel.Description: string;
begin
  Result := 'Office';
end;

function TdxLayoutOfficeLookAndFeel.GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass};
begin
  Result := TdxLayoutGroupOfficePainter;
end;

procedure TdxLayoutOfficeLookAndFeel.CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation);
begin
  if (GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons) and
    ((AOrientation = orVertical) and GroupViewInfo(AViewInfo).IsVerticalCaption or
     (AOrientation = orHorizontal) and not GroupViewInfo(AViewInfo).IsVerticalCaption) then
    Inc(ASize, 20);
end;

function TdxLayoutOfficeLookAndFeel.GetGroupBorderWidth(AContainer: TComponent;
  ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer;
begin
  if ASide = GetOppositeSide(ACaptionSide) then
    Result := IfThen(AIsExpanded, DLUToPixels(GetGroupCaptionFont(AContainer), 3), 0)
  else
    Result := inherited GetGroupBorderWidth(AContainer, ASide, ACaptionSide, AHasCaption, AIsExpanded);
end;

function TdxLayoutOfficeLookAndFeel.GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  Result := 0;
end;

function TdxLayoutOfficeLookAndFeel.GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
var
  AOffset: Integer;
begin
  AOffset := HDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 5);
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
  begin
    Result := Rect(0, AOffset, 0, AOffset);
    if GroupCaptionViewInfo(AViewInfo).AlignVert = tavTop then
      Result.Top := 0;
    if GroupCaptionViewInfo(AViewInfo).AlignVert = tavBottom then
      Result.Bottom := 0;
  end
  else
  begin
    Result := Rect(AOffset, 0, AOffset, 0);
    if GroupCaptionViewInfo(AViewInfo).AlignHorz = taLeftJustify then
      Result.Left := 0;
    if GroupCaptionViewInfo(AViewInfo).AlignHorz = taRightJustify then
      Result.Right := 0;
  end;
end;

function TdxLayoutOfficeLookAndFeel.GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
const
  BorderWidth = 2;
begin
  Result := inherited GetGroupFrameBounds(AViewInfo);
  case GroupViewInfo(AViewInfo).CaptionSide of
    sdLeft:
      Result.Right := Result.Left + BorderWidth;
    sdRight:
      Result.Left := Result.Right - BorderWidth;
    sdTop:
      Result.Bottom := Result.Top + BorderWidth;
    sdBottom:
      Result.Top := Result.Bottom - BorderWidth;
  end;
end;

{ TdxLayoutWebLookAndFeelGroupCaptionOptions }

constructor TdxLayoutWebLookAndFeelGroupCaptionOptions.Create(AOptions: TdxCustomLayoutLookAndFeelOptions);
begin
  inherited;
  FColor := clDefault;
end;

function TdxLayoutWebLookAndFeelGroupCaptionOptions.GetOptions: TdxLayoutWebLookAndFeelGroupOptions;
begin
  Result := TdxLayoutWebLookAndFeelGroupOptions(inherited Options);
end;

procedure TdxLayoutWebLookAndFeelGroupCaptionOptions.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupCaptionOptions.SetSeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSeparatorWidth <> Value then
  begin
    FSeparatorWidth := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupCaptionOptions.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  FSeparatorWidth := MulDiv(FSeparatorWidth, M, D);
end;

function TdxLayoutWebLookAndFeelGroupCaptionOptions.GetDefaultColor: TColor;
begin
  Result := Options.GetColor;
end;

function TdxLayoutWebLookAndFeelGroupCaptionOptions.GetDefaultTextColor: TColor;
begin
  Result := clWindowText;
end;

function TdxLayoutWebLookAndFeelGroupCaptionOptions.GetDefaultFont(AContainer: TComponent): TFont;
begin
  Result := TdxLayoutContainerAccess(AContainer).GetBoldFont;
end;

function TdxLayoutWebLookAndFeelGroupCaptionOptions.GetColor: TColor;
begin
  Result := FColor;
  if Result = clDefault then
    Result := GetDefaultColor;
end;

{ TdxLayoutWebLookAndFeelGroupOptions }

constructor TdxLayoutWebLookAndFeelGroupOptions.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited;
  FFrameColor := clDefault;
  FFrameWidth := 1;
  FOffsetCaption := True;
  FOffsetItems := True;
end;

function TdxLayoutWebLookAndFeelGroupOptions.GetCaptionOptions: TdxLayoutWebLookAndFeelGroupCaptionOptions;
begin
  Result := TdxLayoutWebLookAndFeelGroupCaptionOptions(inherited CaptionOptions);
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.SetCaptionOptions(Value: TdxLayoutWebLookAndFeelGroupCaptionOptions);
begin
  inherited CaptionOptions := Value;
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.SetFrameWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FFrameWidth <> Value then
  begin
    FFrameWidth := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.SetOffsetCaption(Value: Boolean);
begin
  if FOffsetCaption <> Value then
  begin
    FOffsetCaption := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.SetOffsetItems(Value: Boolean);
begin
  if FOffsetItems <> Value then
  begin
    FOffsetItems := Value;
    Changed;
  end;
end;

procedure TdxLayoutWebLookAndFeelGroupOptions.ChangeScaleCore(M, D: Integer);
begin
  inherited;
  FFrameWidth := MulDiv(FFrameWidth, M, D);
end;

function TdxLayoutWebLookAndFeelGroupOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutWebLookAndFeelGroupCaptionOptions;
end;

function TdxLayoutWebLookAndFeelGroupOptions.GetDefaultColor: TColor;
begin
  Result := clWindow;
end;

function TdxLayoutWebLookAndFeelGroupOptions.GetDefaultFrameColor: TColor;
begin
  Result := clBtnFace;
end;

function TdxLayoutWebLookAndFeelGroupOptions.GetFrameColor: TColor;
begin
  Result := FFrameColor;
  if Result = clDefault then
    Result := GetDefaultFrameColor;
end;

function TdxLayoutWebLookAndFeelGroupOptions.HasCaptionSeparator(AHasCaption: Boolean): Boolean;
begin
  Result := AHasCaption or (FFrameWidth = 0);
end;

{ TdxLayoutWebLookAndFeelItemCaptionOptions }

function TdxLayoutWebLookAndFeelItemCaptionOptions.GetDefaultTextColor: TColor;
begin
  Result := clWindowText;
end;

{ TdxLayoutWebLookAndFeelItemOptions }

constructor TdxLayoutWebLookAndFeelItemOptions.Create(ALookAndFeel: TdxCustomLayoutLookAndFeel);
begin
  inherited;
  ControlBorderStyle := lbsSingle;
end;

function TdxLayoutWebLookAndFeelItemOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutWebLookAndFeelItemCaptionOptions;
end;

{ TdxLayoutWebLookAndFeel }

function TdxLayoutWebLookAndFeel.GetGroupOptions: TdxLayoutWebLookAndFeelGroupOptions;
begin
  Result := TdxLayoutWebLookAndFeelGroupOptions(inherited GroupOptions);
end;

procedure TdxLayoutWebLookAndFeel.SetGroupOptions(Value: TdxLayoutWebLookAndFeelGroupOptions);
begin
  inherited GroupOptions := Value;
end;

function TdxLayoutWebLookAndFeel.HasCaptionSeparator(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean;
begin
  Result := GroupViewInfo(AViewInfo).IsExpanded and GroupOptions.HasCaptionSeparator(GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons);
end;

function TdxLayoutWebLookAndFeel.CanDrawSpecificBackground: Boolean;
begin
  Result := False;
end;

function TdxLayoutWebLookAndFeel.GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass;
begin
  Result := TdxLayoutWebLookAndFeelGroupOptions;
end;

function TdxLayoutWebLookAndFeel.GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass;
begin
  Result := TdxLayoutWebLookAndFeelItemOptions;
end;

procedure TdxLayoutWebLookAndFeel.InitLookAndFeel;
begin
  FLookAndFeel.Kind := lfUltraFlat;
  FLookAndFeel.NativeStyle := False;
  FLookAndFeel.SkinName := '';
end;

class function TdxLayoutWebLookAndFeel.Description: string;
begin
  Result := 'Web';
end;

function TdxLayoutWebLookAndFeel.GetGroupPainterClass: TClass{TdxLayoutGroupPainterClass};
begin
  Result := TdxLayoutGroupWebPainter;
end;

procedure TdxLayoutWebLookAndFeel.CorrectGroupButtonsAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect);
begin
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
    ABounds := cxRectOffset(ABounds, GroupOptions.FrameWidth, -GroupOptions.FrameWidth, GroupViewInfo(AViewInfo).CaptionSide = sdLeft)
  else
    ABounds := cxRectOffset(ABounds, -GroupOptions.FrameWidth, GroupOptions.FrameWidth, GroupViewInfo(AViewInfo).CaptionSide = sdTop);
end;

procedure TdxLayoutWebLookAndFeel.CorrectGroupCaptionAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ABounds: TRect);
begin
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
  begin
    ABounds := cxRectOffset(ABounds, GroupOptions.FrameWidth, 0, GroupViewInfo(AViewInfo).CaptionSide = sdLeft);
    if GroupCaptionViewInfo(AViewInfo).AlignVert in [tavTop, tavBottom] then
      ABounds := cxRectOffset(ABounds, 0, GroupOptions.FrameWidth, GroupCaptionViewInfo(AViewInfo).AlignVert = tavTop);
  end
  else
  begin
    ABounds := cxRectOffset(ABounds, 0, GroupOptions.FrameWidth, GroupViewInfo(AViewInfo).CaptionSide = sdTop);
    if GroupCaptionViewInfo(AViewInfo).AlignHorz in [taLeftJustify, taRightJustify] then
      ABounds := cxRectOffset(ABounds, GroupOptions.FrameWidth, 0, GroupCaptionViewInfo(AViewInfo).AlignHorz = taLeftJustify);
  end;
end;

procedure TdxLayoutWebLookAndFeel.CorrectGroupCaptionHeight(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AHeight: Integer);
begin
  if not GroupCaptionViewInfo(AViewInfo).IsVerticalCaption then
    AHeight := VDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 11{12}) + Max(GroupCaptionViewInfo(AViewInfo).GetAvailableTextHeight, GroupCaptionViewInfo(AViewInfo).GetImageHeight) - GroupCaptionViewInfo(AViewInfo).GetAvailableTextHeight;
end;

procedure TdxLayoutWebLookAndFeel.CorrectGroupCaptionWidth(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var AWidth: Integer);
begin
  if GroupCaptionViewInfo(AViewInfo).IsVerticalCaption then
    AWidth := VDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 11{12}) + Max(GroupCaptionViewInfo(AViewInfo).GetAvailableTextWidth, GroupCaptionViewInfo(AViewInfo).GetImageWidth) - GroupCaptionViewInfo(AViewInfo).GetAvailableTextWidth
end;

procedure TdxLayoutWebLookAndFeel.CorrectGroupMinVisibleSize(AViewInfo: TObject {TdxLayoutGroupViewInfo}; var ASize: Integer; AOrientation: TdxOrientation);
begin
  if (AOrientation = orVertical) and GroupViewInfo(AViewInfo).IsVerticalCaption or
     (AOrientation = orHorizontal) and not GroupViewInfo(AViewInfo).IsVerticalCaption then
  begin
    if GroupViewInfo(AViewInfo).HasCaption then
      Inc(ASize, 2 * GetGroupCaptionOffset(AViewInfo))
    else
      if GroupViewInfo(AViewInfo).HasButtons then
        Inc(ASize, 2 * GetGroupButtonsOffset(AViewInfo));
    Inc(ASize, 2 * GroupOptions.FrameWidth);
  end
  else
    if HasCaptionSeparator(AViewInfo) then
      Inc(ASize, GroupOptions.CaptionOptions.SeparatorWidth);
end;

function TdxLayoutWebLookAndFeel.GetGroupBorderOffset(AContainer: TComponent; ASide, ACaptionSide: TdxLayoutSide): Integer;
begin
  if ASide in [ACaptionSide, GetOppositeSide(ACaptionSide)] then
    Result := DLUToPixels(GetGroupCaptionFont(AContainer), 4)
  else
    Result := DLUToPixels(GetGroupCaptionFont(AContainer), 7);
end;

function TdxLayoutWebLookAndFeel.GetGroupBorderWidth(AContainer: TComponent;
  ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer;
var
  AFont: TFont;
begin
  AFont := GetGroupCaptionFont(AContainer);
  if ASide in [ACaptionSide, GetOppositeSide(ACaptionSide)] then
  begin
    Result := IfThen(AIsExpanded, GetGroupBorderOffset(AContainer, ASide, ACaptionSide), 0);
    if ASide = ACaptionSide then
    begin
      if AHasCaption then
        Inc(Result, VDLUToPixels(AFont, 11{12}));
      if GroupOptions.HasCaptionSeparator(AHasCaption) and AIsExpanded then
        Inc(Result, GroupOptions.CaptionOptions.SeparatorWidth);
    end;
  end
  else
    if GroupOptions.OffsetItems then
      Result := GetGroupBorderOffset(AContainer, ASide, ACaptionSide)
    else
      Result := DLUToPixels(AFont, 2);
  Inc(Result, GroupOptions.FrameWidth);
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionColor(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TColor;
begin
  Result := GroupOptions.CaptionOptions.GetColor;
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  if GroupOptions.OffsetCaption then
    Result := VDLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 5)
  else
    Result := DLUToPixels(GroupCaptionViewInfo(AViewInfo).Font, 2);
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionSeparatorAreaBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  Result := GetGroupRestSpaceBounds(AViewInfo);
  case GroupViewInfo(AViewInfo).CaptionSide of
    sdLeft:
      begin
        Result.Right := Result.Left;
        Dec(Result.Left, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdRight:
      begin
        Result.Left := Result.Right;
        Inc(Result.Right, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdTop:
      begin
        Result.Bottom := Result.Top;
        Dec(Result.Top, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdBottom:
      begin
        Result.Top := Result.Bottom;
        Inc(Result.Bottom, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
  end;
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionSeparatorBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  Result := GetGroupCaptionSeparatorAreaBounds(AViewInfo);
  if not GroupOptions.OffsetCaption and not GroupOptions.OffsetItems and
    (GroupOptions.FrameWidth = 0) and (GroupCaptionViewInfo(AViewInfo).Color = GroupViewInfo(AViewInfo).Color) then
  begin
    Result.Left := GroupViewInfo(AViewInfo).ClientBounds.Left;
    Result.Right := GroupViewInfo(AViewInfo).ClientBounds.Right;
  end;
end;

function TdxLayoutWebLookAndFeel.GetGroupRestSpaceBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
var
  ACaptionWidth: Integer;
  ACaptionHeight: Integer;
begin
  Result := cxRectInflate(inherited GetGroupRestSpaceBounds(AViewInfo), -GroupOptions.FrameWidth, -GroupOptions.FrameWidth);
  case GroupViewInfo(AViewInfo).CaptionSide of
    sdLeft:
      begin
        ACaptionWidth := Max(cxRectWidth(GroupCaptionViewInfo(AViewInfo).Bounds), cxRectWidth(GroupButtonsViewInfo(AViewInfo).Bounds));
        if GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons then
          Inc(Result.Left, ACaptionWidth);
        if HasCaptionSeparator(AViewInfo) then
          Inc(Result.Left, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdRight:
      begin
        ACaptionWidth := Max(cxRectWidth(GroupCaptionViewInfo(AViewInfo).Bounds), cxRectWidth(GroupButtonsViewInfo(AViewInfo).Bounds));
        if GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons then
          Dec(Result.Right, ACaptionWidth);
        if HasCaptionSeparator(AViewInfo) then
          Dec(Result.Right, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdTop:
      begin
        ACaptionHeight := Max(cxRectHeight(GroupCaptionViewInfo(AViewInfo).Bounds), cxRectHeight(GroupButtonsViewInfo(AViewInfo).Bounds));
        if GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons then
          Inc(Result.Top, ACaptionHeight);
        if HasCaptionSeparator(AViewInfo) then
          Inc(Result.Top, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
    sdBottom:
      begin
        ACaptionHeight := Max(cxRectHeight(GroupCaptionViewInfo(AViewInfo).Bounds), cxRectHeight(GroupButtonsViewInfo(AViewInfo).Bounds));
        if GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons then
          Dec(Result.Bottom, ACaptionHeight);
        if HasCaptionSeparator(AViewInfo) then
          Dec(Result.Bottom, GroupOptions.CaptionOptions.SeparatorWidth);
      end;
  end;
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionAlignHorz(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TAlignment;
begin
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
    Result := taCenter
  else
    Result := inherited GetGroupCaptionAlignHorz(AViewInfo);
end;

function TdxLayoutWebLookAndFeel.GetGroupCaptionAlignVert(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TdxAlignmentVert;
begin
  if GroupViewInfo(AViewInfo).IsVerticalCaption then
    Result := inherited GetGroupCaptionAlignVert(AViewInfo)
  else
    Result := tavCenter;
end;

{ TdxLayoutLookAndFeelList }

constructor TdxLayoutLookAndFeelList.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TcxComponentList.Create;
  FItems.OwnsObjects := True;
  FItems.OnComponentListChanged := ItemListChanged;
end;

destructor TdxLayoutLookAndFeelList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TdxLayoutLookAndFeelList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxLayoutLookAndFeelList.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxLayoutLookAndFeelList.GetItem(Index: Integer): TdxCustomLayoutLookAndFeel;
begin
  Result := FItems[Index] as TdxCustomLayoutLookAndFeel;
end;

procedure TdxLayoutLookAndFeelList.AddItem(AItem: TdxCustomLayoutLookAndFeel);
begin
  FItems.Add(AItem);
  AItem.FList := Self;
end;

procedure TdxLayoutLookAndFeelList.RemoveItem(AItem: TdxCustomLayoutLookAndFeel);
begin
  if csDestroying in ComponentState then
    Exit;
  AItem.FList := nil;
  FItems.Extract(AItem);
end;

procedure TdxLayoutLookAndFeelList.ItemListChanged(Sender: TObject; AComponent: TComponent; AAction: TcxComponentCollectionNotification);
begin
  if dxLayoutLookAndFeelsDesigner <> nil then
    dxLayoutLookAndFeelsDesigner.ItemsChanged(Self);
end;

procedure TdxLayoutLookAndFeelList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
  if Owner = Root then
    for I := 0 to Count - 1 do Proc(Items[I]);
end;

procedure TdxLayoutLookAndFeelList.Modified;
var
  AForm: TCustomForm;
begin
  if Owner is TCustomForm then
  begin
    AForm := TCustomForm(Owner);
    if AForm.Designer <> nil then AForm.Designer.Modified;
  end;
end;

procedure TdxLayoutLookAndFeelList.SetName(const Value: TComponentName);
begin
  inherited;
  if dxLayoutLookAndFeelsDesigner <> nil then
    dxLayoutLookAndFeelsDesigner.ComponentNameChanged(Self);
end;

function TdxLayoutLookAndFeelList.CreateItem(AClass: TdxCustomLayoutLookAndFeelClass): TdxCustomLayoutLookAndFeel;
begin
  Result := AClass.Create(Owner);
  AddItem(Result);
  SetComponentName(Result, AClass.GetBaseName, IsDesigning, False);
  Modified;
end;

{ TdxLayoutLookAndFeelDefs }

constructor TdxLayoutLookAndFeelDefs.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TdxLayoutLookAndFeelDefs.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TdxLayoutLookAndFeelDefs.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxLayoutLookAndFeelDefs.GetItem(Index: Integer): TdxCustomLayoutLookAndFeelClass;
begin
  Result := TdxCustomLayoutLookAndFeelClass(FItems[Index]);
end;

function TdxLayoutLookAndFeelDefs.GetItemByDescription(const Value: string): TdxCustomLayoutLookAndFeelClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Description = Value then Exit;
  end;
  Result := nil;
end;

procedure TdxLayoutLookAndFeelDefs.Register(AClass: TdxCustomLayoutLookAndFeelClass);
begin
  FItems.Add(AClass);
  RegisterClass(AClass);
end;

procedure TdxLayoutLookAndFeelDefs.Unregister(AClass: TdxCustomLayoutLookAndFeelClass);
begin
  FItems.Remove(AClass);
end;

{ TdxLayoutTextMetrics }

constructor TdxLayoutTextMetrics.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TdxLayoutTextMetrics.Destroy;
begin
  DestroyItems;
  FItems.Free;
  inherited;
end;

function TdxLayoutTextMetrics.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxLayoutTextMetrics.GetItem(AIndex: Integer): PdxLayoutTextMetric;
begin
  Result := FItems[AIndex];
end;

procedure TdxLayoutTextMetrics.DestroyItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Delete(I);
end;

procedure TdxLayoutTextMetrics.FontChangeHandler(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(Sender as TFont);
  dxTestCheck(AIndex > -1, 'TdxLayoutTextMetrics.FontChangeHandler fails');
  CalculateItem(AIndex);
  dxCallNotify(Items[AIndex].PrevFontChangeHandler, Sender);
end;

procedure TdxLayoutTextMetrics.CalculateItem(AIndex: Integer);
begin
  cxGetTextMetrics(Items[AIndex].Font, Items[AIndex].TextMetric);
end;

procedure TdxLayoutTextMetrics.Delete(AIndex: Integer);
begin
  Dispose(Items[AIndex]);
  FItems.Delete(AIndex);
end;

function TdxLayoutTextMetrics.IndexOf(AFont: TFont): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Font = AFont then Exit;
  Result := -1;
end;

function TdxLayoutTextMetrics.GetSameFontIndex(AFont: TFont): Integer;
begin
  for Result := 0 to Count - 1 do
   if Items[Result].Font.Handle = AFont.Handle then Exit;
  Result := -1;
end;

procedure TdxLayoutTextMetrics.Get(AFont: TFont; var ATextMetric: TTextMetric);
var
  AIndex: Integer;

  function CreateItem: Integer;
  var
    AItem: PdxLayoutTextMetric;
  begin
    New(AItem);
    AItem.Font := AFont;
    AItem.PrevFontChangeHandler := AFont.OnChange;
    AItem.Font.OnChange := FontChangeHandler;
    Result := FItems.Add(AItem);
    CalculateItem(Result);
  end;

begin
  AIndex := GetSameFontIndex(AFont);
  if AIndex = -1 then
    AIndex := CreateItem;
  ATextMetric := Items[AIndex].TextMetric;
end;

procedure TdxLayoutTextMetrics.Unregister(AFont: TFont);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AFont);
  if AIndex <> -1 then
  begin
    AFont.OnChange := Items[AIndex].PrevFontChangeHandler;
    Delete(AIndex);
  end;
end;

{ TdxLayoutCxLookAndFeelGroupOptions }

function TdxLayoutCxLookAndFeelGroupOptions.GetDefaultColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.DefaultGroupColor;
end;

function TdxLayoutCxLookAndFeelGroupOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutCxLookAndFeelGroupCaptionOptions;
end;

function TdxLayoutCxLookAndFeelGroupOptions.GetLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := inherited LookAndFeel as TdxLayoutCxLookAndFeel;
end;

{ TdxLayoutCxLookAndFeelGroupCaptionOptions }

function TdxLayoutCxLookAndFeelGroupCaptionOptions.GetLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := TdxLayoutCxLookAndFeel(Options.LookAndFeel);
end;

function TdxLayoutCxLookAndFeelGroupCaptionOptions.GetDefaultTextColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.GroupBoxTextColor(True, cxgpTop);
  if Result = clDefault then
    Result := clBtnText;
end;

function TdxLayoutCxLookAndFeelGroupCaptionOptions.GetDefaultTextDisabledColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.GroupBoxTextColor(False, cxgpTop);
end;

function TdxLayoutCxLookAndFeelGroupCaptionOptions.GetDefaultTextHotColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.GroupBoxTextColor(True, cxgpTop);
end;

{ TdxLayoutCxLookAndFeelItemOptions }

function TdxLayoutCxLookAndFeelItemOptions.GetCaptionOptionsClass: TdxLayoutLookAndFeelCaptionOptionsClass;
begin
  Result := TdxLayoutCxLookAndFeelItemCaptionOptions;
end;

function TdxLayoutCxLookAndFeelItemCaptionOptions.GetDefaultTextColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.DefaultEditorTextColor(False);
end;

function TdxLayoutCxLookAndFeelItemCaptionOptions.GetDefaultTextDisabledColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.DefaultEditorTextColor(True);
end;

function TdxLayoutCxLookAndFeelItemCaptionOptions.GetDefaultTextHotColor: TColor;
begin
  Result := LookAndFeel.LookAndFeelPainter.DefaultEditorTextColor(False);
end;

function TdxLayoutCxLookAndFeelItemCaptionOptions.GetLookAndFeel: TdxLayoutCxLookAndFeel;
begin
  Result := TdxLayoutCxLookAndFeel(Options.LookAndFeel);
end;

{ TdxLayoutCxLookAndFeel }

constructor TdxLayoutCxLookAndFeel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
end;

class function TdxLayoutCxLookAndFeel.Description: string;
begin
  Result := 'Use TcxLookAndFeel';
end;

procedure TdxLayoutCxLookAndFeel.DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.DrawLayoutControlBackground(ACanvas, R);
end;

procedure TdxLayoutCxLookAndFeel.DrawItemControlBorder(
  ACanvas: TcxCanvas; AItemOptions: TdxLayoutLookAndFeelItemOptions; R: TRect);
begin
  LookAndFeelPainter.DrawBorder(ACanvas, R);
end;

function TdxLayoutCxLookAndFeel.NeedRedrawOnResize: Boolean;
begin
  Result := LookAndFeelPainter.NeedRedrawOnResize;
end;

function TdxLayoutCxLookAndFeel.GetEmptyAreaColor: TColor;
begin
  Result := LookAndFeelPainter.LayoutControlEmptyAreaColor;
  if Result = clDefault then
    Result := inherited GetEmptyAreaColor;
end;

function TdxLayoutCxLookAndFeel.GetGroupButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := LookAndFeelPainter.LayoutControlGetColorPaletteForGroupButton(AState);
end;

function TdxLayoutCxLookAndFeel.GetItemCaptionColorPalette: IdxColorPalette;
begin
  Result := LookAndFeelPainter.LayoutControlGetColorPaletteForItemCaption;
end;

function TdxLayoutCxLookAndFeel.GetTabbedGroupCaptionColorPalette(AIsActive: Boolean): IdxColorPalette;
begin
  Result := LookAndFeelPainter.LayoutControlGetColorPaletteForTabbedGroupCaption(AIsActive);
end;

procedure TdxLayoutCxLookAndFeel.CorrectGroupCaptionFont(ACaptionSide: TdxLayoutSide; AFont: TFont);
begin
  LookAndFeelPainter.GroupBoxAdjustCaptionFont(AFont, CaptionSideToCaptionPosition[ACaptionSide])
end;

procedure TdxLayoutCxLookAndFeel.CorrectGroupCaptionFont(AViewInfo: TObject; AFont: TFont);
begin
  if IsSkinPainterUsed then
    CorrectGroupCaptionFont(GroupViewInfo(AViewInfo).CaptionSide, AFont)
  else
    inherited;
end;

function TdxLayoutCxLookAndFeel.GetGroupBorderWidth(AContainer: TComponent;
  ASide, ACaptionSide: TdxLayoutSide; AHasCaption, AIsExpanded: Boolean): Integer;
const
  CaptionSizeMap: array[Boolean] of Integer = (11, 7);
  FrameSizeMap: array[Boolean] of Integer = (2, 4);
var
  ACaptionFont: TFont;
  AIsCaptionSideOrOpposite: Boolean;
begin
  if not IsSkinPainterUsed then
    Exit(inherited GetGroupBorderWidth(AContainer, ASide, ACaptionSide, AHasCaption, AIsExpanded));

  ACaptionFont := GetGroupCaptionFont(AContainer);
  AIsCaptionSideOrOpposite := ASide in [ACaptionSide, GetOppositeSide(ACaptionSide)];
  if AIsExpanded or not AIsCaptionSideOrOpposite then
    Result := FrameWidths[ASide, ACaptionSide] + DLUToPixels(ACaptionFont, FrameSizeMap[AIsCaptionSideOrOpposite])
  else
    Result := LookAndFeelPainter.GroupBoxCaptionTailSize(CaptionSideToCaptionPosition[ACaptionSide]);

  if (ASide = ACaptionSide) and AHasCaption then
  begin
    if IsGroupBoxCaptionTextDrawnOverBorder(ACaptionSide) then
      Inc(Result, VDLUToPixels(ACaptionFont, CaptionSizeMap[AIsExpanded]))
    else
    begin
      cxScreenCanvas.Font := ACaptionFont;
      CorrectGroupCaptionFont(ACaptionSide, cxScreenCanvas.Font);
      Inc(Result, cxTextHeight(cxScreenCanvas.Font));
      cxScreenCanvas.Dormant;

      if ASide in [sdLeft, sdRight] then
        Inc(Result, cxMarginsWidth(GetGroupCaptionContentOffsets(True, ACaptionSide)))
      else
        Inc(Result, cxMarginsHeight(GetGroupCaptionContentOffsets(False, ACaptionSide)));
    end;
  end;
end;

function TdxLayoutCxLookAndFeel.GetGroupCaptionOffset(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Integer;
begin
  if IsSkinPainterUsed and not IsGroupBoxCaptionTextDrawnOverBorder(GroupViewInfo(AViewInfo).CaptionSide) then
    Result := 0
  else
    Result := inherited GetGroupCaptionOffset(AViewInfo);
end;

function TdxLayoutCxLookAndFeel.GetGroupCaptionContentOffsets(AIsVertical: Boolean; ACaptionSide: TdxLayoutSide): TRect;
begin
  Result := LookAndFeelPainter.GroupBoxBorderSize(True, CaptionSideToCaptionPosition[ACaptionSide]);
  if not IsSkinPainterUsed or IsGroupBoxCaptionTextDrawnOverBorder(ACaptionSide) then
  begin
    if AIsVertical then
    begin
      Result.Top := 2;
      Result.Bottom := 2;
    end
    else
    begin
      Result.Left := 2;
      Result.Right := 2;
    end;
  end;
end;

function TdxLayoutCxLookAndFeel.GetGroupCaptionContentOffsets(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
var
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
begin
  AGroupViewInfo := GroupViewInfo(AViewInfo);
  Result := GetGroupCaptionContentOffsets(AGroupViewInfo.IsVerticalCaption, AGroupViewInfo.CaptionSide);
end;

function TdxLayoutCxLookAndFeel.GetGroupFrameBounds(AViewInfo: TObject {TdxLayoutGroupViewInfo}): TRect;
begin
  if IsSkinPainterUsed then
  begin
    Result := GroupViewInfo(AViewInfo).Bounds;
    if GroupViewInfo(AViewInfo).HasCaption or GroupViewInfo(AViewInfo).HasButtons then
    begin
      case GroupViewInfo(AViewInfo).CaptionSide of
        sdLeft:
          Result.Left := Result.Left + Max(GroupCaptionViewInfo(AViewInfo).GetAvailableWidth, GroupButtonsViewInfo(AViewInfo).CalculateWidth);
        sdRight:
          Result.Right := Result.Right - Max(GroupCaptionViewInfo(AViewInfo).GetAvailableWidth, GroupButtonsViewInfo(AViewInfo).CalculateWidth);
        sdTop:
          Result.Top := Result.Top + Max(GroupCaptionViewInfo(AViewInfo).GetAvailableHeight, GroupButtonsViewInfo(AViewInfo).CalculateHeight);
        sdBottom:
          Result.Bottom := Result.Bottom - Max(GroupCaptionViewInfo(AViewInfo).GetAvailableHeight, GroupButtonsViewInfo(AViewInfo).CalculateHeight);
      end;
    end;
  end
  else
    Result := inherited GetGroupFrameBounds(AViewInfo);
end;

function TdxLayoutCxLookAndFeel.GetItemControlBorderWidth(ASide: TdxLayoutSide): Integer;
begin
  Result := LookAndFeelPainter.BorderSize;
end;

function TdxLayoutCxLookAndFeel.IsGroupBoxCaptionTextDrawnOverBorder(ACaptionSide: TdxLayoutSide): Boolean;
begin
  Result := LookAndFeelPainter.IsGroupBoxCaptionTextDrawnOverBorder(CaptionSideToCaptionPosition[ACaptionSide]);
end;

function TdxLayoutCxLookAndFeel.GetGroupPainterClass: TClass;
begin
  Result := TdxLayoutGroupCxLookAndFeelPainter;
end;

function TdxLayoutCxLookAndFeel.GetItemPainterClass: TClass{TdxLayoutItemPainterClass};
begin
  Result := TdxLayoutItemCxLookAndFeelPainter;
end;

function TdxLayoutCxLookAndFeel.IsGroupTransparent(AViewInfo: TObject {TdxLayoutGroupViewInfo}): Boolean;
var
  AGroupViewInfo: TdxLayoutGroupViewInfoAccess;
begin
  AGroupViewInfo := GroupViewInfo(AViewInfo);
  Result := not IsSkinPainterUsed and AGroupViewInfo.IsDefaultColor and
    (IsNativeStyle or (LookAndFeel.Kind = lfOffice11) or ContainerViewInfo(AViewInfo).IsTransparent);
end;

function TdxLayoutCxLookAndFeel.IsSkinPainterUsed: Boolean;
begin
  Result := Assigned(LookAndFeel.SkinPainter);
end;

function TdxLayoutCxLookAndFeel.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TdxLayoutCxLookAndFeel.DoesCxLookAndFeelHavePriority: Boolean;
begin
  Result := True;
end;

function TdxLayoutCxLookAndFeel.GetFrameWidth(ASide, ACaptionSide: TdxLayoutSide): Integer;
var
  ACaptionBorderSize: TRect;
begin
  if IsSkinPainterUsed then
  begin
    ACaptionBorderSize := LookAndFeelPainter.GroupBoxBorderSize(True, CaptionSideToCaptionPosition[ACaptionSide]);
    if ACaptionSide in [sdTop, sdBottom] then
      Result := cxMarginsHeight(ACaptionBorderSize)
    else
      Result := cxMarginsWidth(ACaptionBorderSize);
  end
  else
    Result := inherited GetFrameWidth(ASide, ACaptionSide);
end;

function TdxLayoutCxLookAndFeel.GetGroupOptionsClass: TdxLayoutLookAndFeelGroupOptionsClass;
begin
  Result := TdxLayoutCxLookAndFeelGroupOptions;
end;

function TdxLayoutCxLookAndFeel.GetItemOptionsClass: TdxLayoutLookAndFeelItemOptionsClass;
begin
  Result := TdxLayoutCxLookAndFeelItemOptions;
end;

function TdxLayoutCxLookAndFeel.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxLayoutCxLookAndFeel.InitLookAndFeel;
begin
// do nothing
end;

procedure TdxLayoutCxLookAndFeel.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  Changed;
end;

{ TdxLayoutSkinLookAndFeel }

class function TdxLayoutSkinLookAndFeel.Description: string;
begin
  Result := 'Skin';
end;

procedure TdxLayoutSkinLookAndFeel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SkinName', ReadSkinName, nil, False);
  Filer.DefineProperty('SkinNameAssigned', ReadSkinNameAssigned, nil, False);
end;

procedure TdxLayoutSkinLookAndFeel.ReadSkinName(Reader: TReader);
begin
  LookAndFeel.SkinName := Reader.ReadString;
end;

procedure TdxLayoutSkinLookAndFeel.ReadSkinNameAssigned(Reader: TReader);
begin
  if Reader.ReadBoolean then
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues + [lfvSkinName]
  else
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues - [lfvSkinName]
end;

procedure RegisterAssistants;
begin
  dxLayoutTextMetrics := TdxLayoutTextMetrics.Create;
  dxLayoutLookAndFeelDefs := TdxLayoutLookAndFeelDefs.Create;

  dxLayoutLookAndFeelDefs.Register(TdxLayoutStandardLookAndFeel);
  dxLayoutLookAndFeelDefs.Register(TdxLayoutOfficeLookAndFeel);
  dxLayoutLookAndFeelDefs.Register(TdxLayoutWebLookAndFeel);
  dxLayoutLookAndFeelDefs.Register(TdxLayoutCxLookAndFeel);
  dxLayoutLookAndFeelDefs.Register(TdxLayoutSkinLookAndFeel);

  dxLayoutDefaultLookAndFeel := dxLayoutDefaultLookAndFeelClass.Create(nil);
end;

procedure UnregisterAssistants;
begin
  FreeAndNil(dxLayoutDefaultLookAndFeel);

  dxLayoutLookAndFeelDefs.Unregister(TdxLayoutSkinLookAndFeel);
  dxLayoutLookAndFeelDefs.Unregister(TdxLayoutCxLookAndFeel);
  dxLayoutLookAndFeelDefs.Unregister(TdxLayoutWebLookAndFeel);
  dxLayoutLookAndFeelDefs.Unregister(TdxLayoutOfficeLookAndFeel);
  dxLayoutLookAndFeelDefs.Unregister(TdxLayoutStandardLookAndFeel);

  FreeAndNil(dxLayoutLookAndFeelDefs);
  FreeAndNil(dxLayoutTextMetrics);
end;

initialization
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);

end.
