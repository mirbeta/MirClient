{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.Types;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Rtti,
  SyncObjs, DateUtils,
  dxCoreClasses, dxCoreGraphics,
  dxGenerics;

const
  dxRunIndexDontCare     = -1;

  dxSectionIndexDontCare = -1;
  dxSectionIndexMinValue = 0;
  dxSectionIndexMaxValue = MaxInt;

  dxHeaderIndexMinValue  = 0;
  dxHeaderIndexZero      = 0;
  dxHeaderIndexInvalid   = -1;
  dxHeaderIndexMaxValue  = MaxInt;

  dxFooterIndexMinValue  = 0;
  dxFooterIndexZero      = 0;
  dxFooterIndexInvalid   = -1;
  dxFooterIndexMaxValue  = MaxInt;

  ExactPage              = $00000001;
  NearestPage            = $00000000;
  ExactPageArea          = $00000002;
  NearestPageArea        = $00000000;
  ExactColumn            = $00000004;
  NearestColumn          = $00000000;
  ExactRow               = $00000008;
  NearestRow             = $00000000;
  ExactBox               = $00000010;
  NearestBox             = $00000000;
  ExactCharacter         = $00000020;
  NearestCharacter       = $00000000;
  ExactTableRow          = $00000040;
  NearestTableRow        = $00000000;
  ExactTableCell         = $00000080;
  NearestTableCell       = $00000000;

  SmallFontStylesMap: array[Boolean, Boolean] of TFontStyles = (([], [fsItalic]),([fsBold], [fsBold, fsItalic]));

type
  TdxParagraphIndex = Integer;
  TdxLayoutUnit = Integer;
  TdxModelUnit  = Integer;
  TdxDocumentLogPosition = Integer;
  TdxRunIndex = Integer;
  TdxSectionIndex = Integer;
  TdxHeaderIndex = Integer;
  TdxFooterIndex = Integer;
  TdxAbstractNumberingListIndex = Integer;
  TdxNumberingListIndex = Integer;

  TdxDocumentCapability = (Default, Disabled, Enabled, Hidden);

  TdxRichEditFormattingMarkVisibility = (
    Auto,
    Visible,
    Hidden);

  TdxHeightUnitTypeValues = class sealed
  public const
    Minimum = 0;
    Auto = 1;
    Exact = 2;
  end;

  TdxHeightUnitType = (
    Minimum = TdxHeightUnitTypeValues.Minimum,
    Auto = TdxHeightUnitTypeValues.Auto,
    Exact = TdxHeightUnitTypeValues.Exact);

  TdxWidthUnitTypeValues = class sealed
  public const
    &Nil = 0;
    Auto = 1;
    FiftiethsOfPercent = 2;
    ModelUnits = 3;
  end;

  TdxWidthUnitType = (
    &Nil = TdxWidthUnitTypeValues.&Nil,
    Auto = TdxWidthUnitTypeValues.Auto,
    FiftiethsOfPercent = TdxWidthUnitTypeValues.FiftiethsOfPercent,
    ModelUnits = TdxWidthUnitTypeValues.ModelUnits
  );

  TdxPageGenerationStrategyType = (
    RunningHeight,
    FirstPageOffset
  );

  TdxDocumentLayoutResetType = (
    None,
    SecondaryLayout,
    AllPrimaryLayout,
    PrimaryLayoutFormPosition
  );

  TdxSectionPageChangeType = (
    None,
    Width,
    Height,
    Landscape,
    PaperKind,
    BatchUpdate
  );

  TdxSectionMarginsChangeType = (
    None,
    Left,
    Right,
    Top,
    Bottom,
    Gutter,
    GutterAlignment,
    HeaderOffset,
    FooterOffset,
    BatchUpdate
  );

  TdxSectionGeneralSettingsChangeType = (
    None,
    OnlyAllowEditingOfFormFields,
    DifferentFirstPage,
    FirstPagePaperSource,
    OtherPagePaperSource,
    TextDirection,
    VerticalTextAlignment,
    StartType,
    BatchUpdate
  );

  TdxCompleteFormattingResult = (
    Success,
    OrphanedFloatingObjects
  );

  TdxSectionPageNumberingChangeType = (
    None,
    ChapterSeparator,
    ChapterHeaderStyle,
    NumberingFormat,
    StartingPageNumber,
    BatchUpdate
  );

  TdxSectionLineNumberingChangeType = (
    None,
    Distance,
    StartingLineNumber,
    Step,
    NumberingRestartType,
    BatchUpdate
  );

  TdxParagraphNumerationCopyOptions = (CopyAlways, CopyIfWholeSelected);

  TdxDocumentModelChangeType = (
    None,
    InsertParagraph,
    InsertText,
    InsertInlinePicture,
    InsertInlineCustomObject,
    InsertFloatingObjectAnchor,
    InsertSection,
    ModifySection,
    CreateEmptyDocument,
    LoadNewDocument,
    DeleteContent,
    JoinRun,
    SplitRun,
    ToggleFieldCodes,
    ToggleFieldLocked,
    Fields
  );

  TdxTableAutoFitBehaviorTypeValues = class sealed
  public const
    FixedColumnWidth = 0;
    AutoFitToContents = 1;
    AutoFitToWindow = 2;
  end;

  TdxTableAutoFitBehaviorType = (
    FixedColumnWidth = TdxTableAutoFitBehaviorTypeValues.FixedColumnWidth,
    AutoFitToContents = TdxTableAutoFitBehaviorTypeValues.AutoFitToContents,
    AutoFitToWindow = TdxTableAutoFitBehaviorTypeValues.AutoFitToWindow);

  TdxDefaultPropertiesCopyOptions = (Always, Never);
  TdxFormattingCopyOptions = (KeepSourceFormatting, UseDestinationStyles);

  TdxFootNotePosition = (
    BottomOfPage,
    BelowText,
    EndOfDocument,
    EndOfSection
  );

  TdxSectionFootNoteChangeType = (
    None,
    Position,
    NumberingFormat,
    NumberingRestartType,
    StartingNumber,
    CustomMark,
    BatchUpdate
  );

  TdxSectionColumnsChangeType = (
    None,
    EqualWidthColumns,
    DrawVerticalSeparator,
    Space,
    ColumnCount,
    BatchUpdate
  );

  TdxCornerViewInfoType = (
    Normal,
    OuterHorizontalStart,
    OuterVerticalStart,
    OuterHorizontalEnd,
    OuterVerticalEnd,
    InnerTopLeft,
    InnerTopMiddle,
    InnerTopRight,
    InnerLeftMiddle,
    InnerRightMiddle,
    InnerBottomLeft,
    InnerBottomMiddle,
    InnerBottomRight,
    InnerNormal
  );

  TdxRowProcessingFlag = (
    ProcessCharacterLines,
    ProcessHiddenText,
    ProcessTextHighlight,
    ProcessSpecialTextBoxes,
    ProcessLayoutDependentText,
    FirstParagraphRow,
    LastParagraphRow,
    ContainsFootNotes,
    ContainsEndNotes,
    LastInvisibleEmptyCellRowAfterNestedTable);

  TdxRowProcessingFlags = set of TdxRowProcessingFlag;

  TdxTextBoxPropertiesChangeType = (
    None,
    LeftMargin,
    RightMargin,
    TopMargin,
    BottomMargin,
    ResizeShapeToFitText,
    WrapText,
    VerticalAlignment,
    Upright,
    BatchUpdate
  );

  TdxTableGridIntervalType = (
    PercentBased,
    ModelUnit,
    NotSet
  );

  TdxTableChangeType = (
    None,
    TableStyle,
    BatchUpdate
  );

  TdxCanFitCurrentRowToColumnResult = (
    RowFitted,
    PlainRowNotFitted,
    FirstCellRowNotFitted,
    TextAreasRecreated,
    RestartDueFloatingObject
  );

  TdxSectionPropertiesApplyType = (
    WholeDocument,
    CurrentSection,
    SelectedSections,
    ThisPointForward
  );
  TdxSectionPropertiesApplyTypes = set of TdxSectionPropertiesApplyType;

  TdxPageSetupFormInitialTabPage = (
    Margins = 0,
    Paper   = 1,
    Layout  = 2
  );

  TdxBorderLineStyleValues = class sealed
  public const
    &Nil = 0;
    None = 1;
    Single = 2;
    Thick = 3;
    Double = 4;
    Dotted = 5;
    Dashed = 6;
    DotDash = 7;
    DotDotDash = 8;
    Triple = 9;
    ThinThickSmallGap = 10;
    ThickThinSmallGap = 11;
    ThinThickThinSmallGap = 12;
    ThinThickMediumGap = 13;
    ThickThinMediumGap = 14;
    ThinThickThinMediumGap = 15;
    ThinThickLargeGap = 16;
    ThickThinLargeGap = 17;
    ThinThickThinLargeGap = 18;
    Wave = 19;
    DoubleWave = 20;
    DashSmallGap = 21;
    DashDotStroked = 22;
    ThreeDEmboss = 23;
    ThreeDEngrave = 24;
    Outset = 25;
    Inset = 26;
  end;

  TdxBorderLineStyle = (
    &Nil = TdxBorderLineStyleValues.&Nil,
    None = TdxBorderLineStyleValues.None,
    Single = TdxBorderLineStyleValues.Single,
    Thick = TdxBorderLineStyleValues.Thick,
    Double = TdxBorderLineStyleValues.Double,
    Dotted = TdxBorderLineStyleValues.Dotted,
    Dashed = TdxBorderLineStyleValues.Dashed,
    DotDash = TdxBorderLineStyleValues.DotDash,
    DotDotDash = TdxBorderLineStyleValues.DotDotDash,
    Triple = TdxBorderLineStyleValues.Triple,
    ThinThickSmallGap = TdxBorderLineStyleValues.ThinThickSmallGap,
    ThickThinSmallGap = TdxBorderLineStyleValues.ThickThinSmallGap,
    ThinThickThinSmallGap = TdxBorderLineStyleValues.ThinThickThinSmallGap,
    ThinThickMediumGap = TdxBorderLineStyleValues.ThinThickMediumGap,
    ThickThinMediumGap = TdxBorderLineStyleValues.ThickThinMediumGap,
    ThinThickThinMediumGap = TdxBorderLineStyleValues.ThinThickThinMediumGap,
    ThinThickLargeGap = TdxBorderLineStyleValues.ThinThickLargeGap,
    ThickThinLargeGap = TdxBorderLineStyleValues.ThickThinLargeGap,
    ThinThickThinLargeGap = TdxBorderLineStyleValues.ThinThickThinLargeGap,
    Wave = TdxBorderLineStyleValues.Wave,
    DoubleWave = TdxBorderLineStyleValues.DoubleWave,
    DashSmallGap = TdxBorderLineStyleValues.DashSmallGap,
    DashDotStroked = TdxBorderLineStyleValues.DashDotStroked,
    ThreeDEmboss = TdxBorderLineStyleValues.ThreeDEmboss,
    ThreeDEngrave = TdxBorderLineStyleValues.ThreeDEngrave,
    Outset = TdxBorderLineStyleValues.Outset,
    Inset = TdxBorderLineStyleValues.Inset,
    Apples,
    ArchedScallops,
    BabyPacifier,
    BabyRattle,
    Balloons3Colors,
    BalloonsHotAir,
    BasicBlackDashes,
    BasicBlackDots,
    BasicBlackSquares,
    BasicThinLines,
    BasicWhiteDashes,
    BasicWhiteDots,
    BasicWhiteSquares,
    BasicWideInline,
    BasicWideMidline,
    BasicWideOutline,
    Bats,
    Birds,
    BirdsFlight,
    Cabins,
    CakeSlice,
    CandyCorn,
    CelticKnotwork,
    CertificateBanner,
    ChainLink,
    ChampagneBottle,
    CheckedBarBlack,
    CheckedBarColor,
    Checkered,
    ChristmasTree,
    CirclesLines,
    CirclesRectangles,
    ClassicalWave,
    Clocks,
    Compass,
    Confetti,
    ConfettiGrays,
    ConfettiOutline,
    ConfettiStreamers,
    ConfettiWhite,
    CornerTriangles,
    CouponCutoutDashes,
    CouponCutoutDots,
    CrazyMaze,
    CreaturesButterfly,
    CreaturesFish,
    CreaturesInsects,
    CreaturesLadyBug,
    CrossStitch,
    Cup,
    DecoArch,
    DecoArchColor,
    DecoBlocks,
    DiamondsGray,
    DoubleD,
    DoubleDiamonds,
    Earth1,
    Earth2,
    EclipsingSquares1,
    EclipsingSquares2,
    EggsBlack,
    Fans,
    Film,
    Firecrackers,
    FlowersBlockPrint,
    FlowersDaisies,
    FlowersModern1,
    FlowersModern2,
    FlowersPansy,
    FlowersRedRose,
    FlowersRoses,
    FlowersTeacup,
    FlowersTiny,
    Gems,
    GingerbreadMan,
    Gradient,
    Handmade1,
    Handmade2,
    HeartBalloon,
    HeartGray,
    Hearts,
    HeebieJeebies,
    Holly,
    HouseFunky,
    Hypnotic,
    IceCreamCones,
    LightBulb,
    Lightning1,
    Lightning2,
    MapleLeaf,
    MapleMuffins,
    MapPins,
    Marquee,
    MarqueeToothed,
    Moons,
    Mosaic,
    MusicNotes,
    Northwest,
    Ovals,
    Packages,
    PalmsBlack,
    PalmsColor,
    PaperClips,
    Papyrus,
    PartyFavor,
    PartyGlass,
    Pencils,
    People,
    PeopleHats,
    PeopleWaving,
    Poinsettias,
    PostageStamp,
    Pumpkin1,
    PushPinNote1,
    PushPinNote2,
    Pyramids,
    PyramidsAbove,
    Quadrants,
    Rings,
    Safari,
    Sawtooth,
    SawtoothGray,
    ScaredCat,
    Seattle,
    ShadowedSquares,
    SharksTeeth,
    ShorebirdTracks,
    Skyrocket,
    SnowflakeFancy,
    Snowflakes,
    Sombrero,
    Southwest,
    Stars,
    Stars3d,
    StarsBlack,
    StarsShadowed,
    StarsTop,
    Sun,
    Swirligig,
    TornPaper,
    TornPaperBlack,
    Trees,
    TriangleParty,
    Triangles,
    Tribal1,
    Tribal2,
    Tribal3,
    Tribal4,
    Tribal5,
    Tribal6,
    TwistedLines1,
    TwistedLines2,
    Vine,
    Waveline,
    WeavingAngles,
    WeavingBraid,
    WeavingRibbon,
    WeavingStrips,
    WhiteFlowers,
    Woodwork,
    XIllusions,
    ZanyTriangles,
    ZigZag,
    ZigZagStitch,
    Disabled
  );

  TdxBorderTypes = record
  private
    Value: Byte;
  public const
    None       = 0;
    Left       = 1;
    Right      = 2;
    Top        = 4;
    Bottom     = 8;
    Horizontal = Top or Bottom;
    Vertical   = Left or Right;
  public
    class operator Implicit(A : Integer): TdxBorderTypes; inline;
    class operator BitwiseAnd(const A, B: TdxBorderTypes): Integer; inline;
    class operator Equal(const A, B: TdxBorderTypes): Boolean; inline;
  end;

  { TdxAction }

  TdxAction = TProc;
  TdxAction<T> = reference to procedure(const Sender: T);
  TdxPredicate<T> = reference to function (const AItem: T): Boolean;

  { TdxFunc<TResult> }

  TdxFunc<TResult> = reference to function: TResult;

  { TdxFunc<T, TResult> }

  TdxFunc<T, TResult> = reference to function(const AArg: T): TResult;

  IdxCloneable<T> = interface
  ['{DD13BA3C-EFB8-430C-A166-69BE57D31543}']
    function Clone: T;
  end;

  IdxSupportsCopyFrom<T> = interface
  ['{732E8117-3D05-44E3-A42F-C4ADF2012F56}']
    procedure CopyFrom(const Source: T);
  end;

  { IdxHighlightableTextRun }

  IdxHighlightableTextRun = interface
  ['{A172E5B6-98ED-4709-B8BE-D0211367EDD7}']
    function GetBackColor: TdxAlphaColor;
    property BackColor: TdxAlphaColor read GetBackColor;
  end;

  { IdxResizeableObject }

  IdxResizeableObject = interface
  ['{7B22D216-624D-440F-9B6D-9A96535F4390}']
    function GetResizeable: Boolean;
    property Resizeable: Boolean read GetResizeable;
  end;

  { IdxRectangularObject }

  IdxRectangularObject = interface
  ['{573FF621-63AA-4274-A6C2-D41A7CE2D1ED}']
    function GetActualSize: TSize;
    procedure SetActualSize(const AValue: TSize);

    property ActualSize: TSize read GetActualSize write SetActualSize;
  end;

  { IdxRectangularScalableObject }

  IdxRectangularScalableObject = interface(IdxRectangularObject)
  ['{2FB8A3CC-7837-49D9-8223-1A74C27153A4}']
    function GetOriginalSize: TSize;
    function GetScaleX: Single;
    function GetScaleY: Single;
    procedure SetScaleX(const AValue: Single);
    procedure SetScaleY(const AValue: Single);

    property OriginalSize: TSize read GetOriginalSize;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
  end;

  { IdxZOrderedObject }

  IdxZOrderedObject = interface
    function GetIsBehindDoc: Boolean;
    procedure SetIsBehindDoc(const Value: Boolean);
    function GetZOrder: Integer;
    procedure SetZOrder(const Value: Integer);

    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property ZOrder: Integer read GetZOrder write SetZOrder;
  end;

  TdxIZOrderedObjectList = TList<IdxZOrderedObject>;

  IdxDateTimeService = interface
  ['{F2B51502-05E2-48F7-A9F1-F469B016E775}']
    function GetCurrentDateTime: TDateTime;
  end;

  { TdxEventArgs }

  TdxEventArgs = class
  public
    class function Empty: TdxEventArgs;
  end;
  TdxEvent = procedure(ASender: TObject; AArgs: TdxEventArgs) of object;
  TdxEventHandler = TdxMulticastMethod<TdxEvent>;

  { TdxCancelEventArgs }

  TdxCancelEventArgs = class abstract(TdxEventArgs)
  private
    FCancel: Boolean;
  public
    constructor Create(ACancel: Boolean); overload;
    property Cancel: Boolean read FCancel  write FCancel;
  end;

  { TdxBeginNextSectionFormattingEventArgs }

  TdxBeginNextSectionFormattingEventArgs = class(TdxEventArgs)
  strict private
    FSectionIndex: TdxSectionIndex;
  public
    constructor Create(ASectionIndex: TdxSectionIndex);

    property SectionIndex: TdxSectionIndex read FSectionIndex;
  end;

  { TdxOptionChangedEventArgs }

  TdxOptionChangedEventArgs = class(TdxEventArgs);

  TdxInterfaceHelper = class
  public
    class function Get<T: IInterface>(AObject: TObject): T; static;
  end;

  { TdxReferencedObject }

  TdxReferencedObject = class(TcxIUnknownObject)
  strict private
    FReferenceCount: Integer;
  protected
    procedure InternalAddReference; inline;
    function InternalRelease: Integer; inline;
  public
    class procedure AddReference(AObject: TdxReferencedObject); static;
    class procedure Release(var Obj); static;
  end;

  { TdxReferencedObjectContainer<T> }

  TdxReferencedObjectContainer<T: TdxReferencedObject> = record
  private
    FReferencedObject: T;
    function GetValue: T;
    procedure SetValue(const Value: T);
  public
    class operator Implicit(const A: TdxReferencedObjectContainer<T>): T;
    property Value: T read GetValue write SetValue;
  end;

  { TdxReferencedObjectList }

  TdxReferencedObjectList<T: class> = class(TdxObjectList<T>)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

  { TValueHelper }

  TValueHelper = record helper for TValue
  public
    function Equals(const Value: TValue): Boolean;
    function IsExtended: Boolean;
    function IsDateTime: Boolean;
    function IsInterface: Boolean;
    function IsString: Boolean;
    function ToFloat: Extended;
  end;

  TdxProgressIndicationState = (
    Unknown,
    Allowed,
    Forbidden);

  { TdxStringBuilderHelper }

  TdxStringBuilderHelper  = class helper for TStringBuilder
  public
    function GetBytes(AEncoding: TEncoding): TBytes;
  end;

  { TdxIUnknownList<T> }

  TdxIUnknownList<T> = class(TList<T>, IUnknown)
  strict private
    FRefCount: Integer;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

function NotImplemented(const AMethodName: string = ''): Pointer;

implementation

uses
  TypInfo, Variants, Math, Windows;

{ TdxBorderTypes }

class operator TdxBorderTypes.Implicit(A: Integer): TdxBorderTypes;
begin
  Result.Value := A;
end;

class operator TdxBorderTypes.BitwiseAnd(const A, B: TdxBorderTypes): Integer;
begin
  Result := A.Value and B.Value;
end;

class operator TdxBorderTypes.Equal(const A, B: TdxBorderTypes): Boolean;
begin
  Result := A.Value = B.Value;
end;

class function TdxInterfaceHelper.Get<T>(AObject: TObject): T;
begin
  if Supports(AObject, GetTypeData(TypeInfo(T))^.GUID, Result) then
    Exit
  else
    Result := nil;
end;

function NotImplemented(const AMethodName: string = ''): Pointer;
begin
  if AMethodName <> '' then
    raise ENotImplemented.Create('Method ' + AMethodName + ' is not implemented')
  else
    raise ENotImplemented.Create('Not implemented');
end;

{ TdxEventArgs }

class function TdxEventArgs.Empty: TdxEventArgs;
begin
  Result := nil;
end;

{ TdxCancelEventArgs }

constructor TdxCancelEventArgs.Create(ACancel: Boolean);
begin
  inherited Create;
  FCancel := ACancel;
end;

{ TdxBeginNextSectionFormattingEventArgs }

constructor TdxBeginNextSectionFormattingEventArgs.Create(ASectionIndex: TdxSectionIndex);
begin
  FSectionIndex := ASectionIndex;
end;

{ TdxReferencedObject }

procedure TdxReferencedObject.InternalAddReference;
begin
  TInterlocked.Increment(FReferenceCount);
end;

function TdxReferencedObject.InternalRelease: Integer;
begin
  Result := TInterlocked.Decrement(FReferenceCount);
end;

class procedure TdxReferencedObject.AddReference(AObject: TdxReferencedObject);
begin
  if AObject <> nil then
    AObject.InternalAddReference;
end;

class procedure TdxReferencedObject.Release(var Obj);
var
  AObject: TObject absolute Obj;
begin
  if AObject <> nil then
  begin
    Assert(AObject is TdxReferencedObject);
    if TdxReferencedObject(AObject).InternalRelease = 0 then
      FreeAndNil(AObject);
  end;
end;

{ TdxReferencedObjectContainer<T> }

function TdxReferencedObjectContainer<T>.GetValue: T;
begin
  Result := FReferencedObject;
end;

class operator TdxReferencedObjectContainer<T>.Implicit(const A: TdxReferencedObjectContainer<T>): T;
begin
  Result := A.FReferencedObject;
end;

procedure TdxReferencedObjectContainer<T>.SetValue(const Value: T);
begin
  if Value = FReferencedObject then
    Exit;
  TdxReferencedObject.Release(FReferencedObject);
  FReferencedObject := Value;
  TdxReferencedObject.AddReference(FReferencedObject);
end;

{ TdxReferencedObjectList<T> }

procedure TdxReferencedObjectList<T>.Notify(Ptr: Pointer; Action: TListNotification);
var
  AItem: TdxReferencedObject;
begin
  if Ptr <> nil then
  begin
    Assert(TObject(Ptr) is TdxReferencedObject, TObject(Ptr).ClassName);
    AItem := TdxReferencedObject(Ptr);
    case Action of
      lnAdded:
        AItem.InternalAddReference;
      lnDeleted:
        if AItem.InternalRelease > 0 then
          Action := lnExtracted;
      lnExtracted:
        AItem.InternalRelease;
    end;
  end;
  inherited Notify(Ptr, Action);
end;

{ TValueHelper }

function TValueHelper.Equals(const Value: TValue): Boolean;
begin
  Result := IsEmpty = Value.IsEmpty;
  if not Result or IsEmpty then
    Exit;
  Result := (Kind = Value.Kind) and
    (TypeInfo = Value.TypeInfo) and
    (TypeData = Value.TypeData);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := (Kind = tkFloat) and
    ((TypeInfo.Name = 'TDateTime') or (TypeInfo.Name = 'TDate') or (TypeInfo.Name = 'TTime'));
end;

function TValueHelper.IsExtended: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := IsType<IInterface>;
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString];
  if not Result then
    Result := IsType<string>;
end;

function TValueHelper.ToFloat: Extended;
begin
  case Kind of
    tkInteger:
      Result := AsInteger;
    tkFloat:
      Result := AsExtended;
    tkInt64:
      Result := AsInt64;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      Result := StrToFloat(AsString)
    else
      Result := AsType<Extended>;
  end;
end;

{ TdxStringBuilderHelper }

function TdxStringBuilderHelper.GetBytes(AEncoding: TEncoding): TBytes;
begin
{$IFDEF DELPHI103}
  SetLength(Result, AEncoding.GetByteCount(FData, 0, Length));
  AEncoding.GetBytes(FData, 0, Length, Result, 0);
{$ELSE}
  Result := AEncoding.GetBytes(FData, 0, Length);
{$ENDIF}
end;

{ TdxIUnknownList<T> }

function TdxIUnknownList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxIUnknownList<T>._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount)
end;

function TdxIUnknownList<T>._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.
