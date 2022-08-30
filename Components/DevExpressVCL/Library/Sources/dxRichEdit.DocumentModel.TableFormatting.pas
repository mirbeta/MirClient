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

unit dxRichEdit.DocumentModel.TableFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCoreGraphics,
  dxCore, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.MergedProperties,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Platform.Font;

type
  TdxTableProperties = class;
  TdxTableRowProperties = class;
  TdxTableBorders = class;
  TdxTableIndent = class;
  TdxCellSpacing =  class;
  TdxTableRowGeneralSettings = class;
  TdxTableCellProperties = class;
  TdxTableCellBorders = class;
  TdxTableCellGeneralSettings = class;
  TdxMergedTableCellProperties = class;
  TdxTableCellGeneralSettingsInfo = class;

  TdxTableRowAlignment = TdxRichEditTableRowAlignment;
  TdxTableLayoutType = TdxRichEditTableLayoutType;
  TdxConditionalTableStyleFormattingType = TdxRichEditConditionalTableStyleFormattingType;
  TdxConditionalTableStyleFormattingTypes = TdxRichEditConditionalTableStyleFormattingTypes;
  TdxTableLookType = TdxRichEditTableLookType;
  TdxTableLookTypes = TdxRichEditTableLookTypes;

  TdxConditionalRowType = (
    Unknown = 0,
    FirstRow,
    LastRow,
    EvenRowBand,
    OddRowBand,
    Normal
  );

  TdxConditionalColumnType = (
    Unknown = 0,
    FirstColumn,
    LastColumn,
    EvenColumnBand,
    OddColumnBand,
    Normal
  );

  TdxShadingPattern = (
    Clear,
    DiagCross,
    DiagStripe,
    HorzCross,
    HorzStripe,
    &Nil,
    Pct5,
    Pct10,
    Pct12,
    Pct15,
    Pct20,
    Pct25,
    Pct30,
    Pct35,
    Pct37,
    Pct40,
    Pct45,
    Pct50,
    Pct55,
    Pct60,
    Pct62,
    Pct65,
    Pct70,
    Pct75,
    Pct80,
    Pct85,
    Pct87,
    Pct90,
    Pct95,
    ReverseDiagStripe,
    Solid,
    ThinDiagCross,
    ThinDiagStripe,
    ThinHorzCross,
    ThinHorzStripe,
    ThinReverseDiagStripe,
    ThinVertStripe,
    VertStripe
  );

  IdxDefaultTablePropertiesContainer = interface
  ['{DCCCFD82-1A7F-437B-938E-42D463A0786C}']
    function GetDefaultTableProperties: TdxTableProperties;
    function GetDefaultTableCellProperties: TdxTableCellProperties;
    function GetDefaultTableRowProperties: TdxTableRowProperties;

    property DefaultTableProperties: TdxTableProperties read GetDefaultTableProperties;
    property DefaultTableRowProperties: TdxTableRowProperties read GetDefaultTableRowProperties;
    property DefaultTableCellProperties: TdxTableCellProperties read GetDefaultTableCellProperties;
  end;

  { IdxPropertiesContainerWithMask }

  IdxPropertiesContainerWithMask = interface(IdxPropertiesContainer)
  ['{958DB55C-A2BD-4BEB-AD5A-8DFDEAA82EBA}']
    function GetUse(AMask: Integer): Boolean;
  end;

  { IdxTableCellBorders }

  IdxTableCellBorders = interface
  ['{F9F4AE61-BE76-4915-906B-62BEFE060AFD}']
    function GetTopBorder: TdxBorderBase;
    function GetBottomBorder: TdxBorderBase;
    function GetLeftBorder: TdxBorderBase;
    function GetRightBorder: TdxBorderBase;

    property TopBorder: TdxBorderBase read GetTopBorder;
    property BottomBorder: TdxBorderBase read GetBottomBorder;
    property LeftBorder: TdxBorderBase read GetLeftBorder;
    property RightBorder: TdxBorderBase read GetRightBorder;
  end;

  { IdxCellPropertiesContainer }

  IdxCellPropertiesContainer = interface(IdxPropertiesContainerWithMask)
  ['{080423F7-0E05-456D-9288-86D4CA892F65}']
  end;

  { IdxCellPropertiesOwner }

  IdxCellPropertiesOwner = interface
  ['{465BC58F-F854-408C-A2EE-9445B83C073E}']
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
  end;

  { TdxBoolPropertyAccessor<T> }

  TdxGetOptionValueDelegate<T> = reference to function(AOptions: T): Boolean;
  TdxSetOptionValueDelegate<T> = reference to procedure(AOptions: T; const Value: Boolean);

  TdxBoolPropertyAccessor<T> = record
  strict private
    FGet: TdxGetOptionValueDelegate<T>;
    FSet: TdxSetOptionValueDelegate<T>;
  public
    constructor Create(const AGet: TdxGetOptionValueDelegate<T>; const ASet: TdxSetOptionValueDelegate<T>);

    property Get: TdxGetOptionValueDelegate<T> read FGet;
    property &Set: TdxSetOptionValueDelegate<T> read FSet;
  end;

  { TdxPropertiesBase<T> }

  TdxPropertiesBase<T: TdxCloneable> = class abstract(TdxRichEditIndexBasedObject<T>, IdxPropertiesContainer)
  private
    FSuspendCount: Integer;
    FDeferredChanges: TdxDocumentModelChangeActions;
    function GetIsSuspendUpdateOptions: Boolean;
    function GetPieceTable: TdxCustomPieceTable;
  protected
    //IPropertiesContainer
    procedure BeginPropertiesUpdate;
    procedure EndPropertiesUpdate;
    procedure BeginChanging(AChangedProperty: TdxProperties);
    procedure ResetPropertyUse(AChangedProperty: TdxProperties);
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure EndChanging;

    function GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<T>; virtual;

    function ResetOptionsCore(AChangedProperty: TdxProperties; out AIsDeferred: Boolean): T; virtual;
    function ChangeOptionsCore(AChangedProperty: TdxProperties; out AIsDeferred: Boolean): T; virtual;

    function ChangePropertiesOptions(const AAccessor: TdxBoolPropertyAccessor<T>; out AIsDeferred: Boolean): T; virtual;
    function ResetPropertiesOptions(const AAccessor: TdxBoolPropertyAccessor<T>; out AIsDeferred: Boolean): T; virtual;

    property IsSuspendUpdateOptions: Boolean read GetIsSuspendUpdateOptions;
  public
    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxTablePropertiesOptions }

  TdxTablePropertiesOptions = class(TdxCloneable)
  public const
    MaskUseNone                   = $00000000;
    MaskUseLeftMargin             = TdxMarginUnitBase.TableMaskUseLeftMargin;
    MaskUseRightMargin            = TdxMarginUnitBase.TableMaskUseRightMargin;
    MaskUseTopMargin              = TdxMarginUnitBase.TableMaskUseTopMargin;
    MaskUseBottomMargin           = TdxMarginUnitBase.TableMaskUseBottomMargin;
    MaskUseCellSpacing            = $00000010;
    MaskUseTableIndent            = $00000020;
    MaskUseTableLayout            = $00000040;
    MaskUseTableLook              = $00000080;
    MaskUsePreferredWidth         = $00000100;
    MaskUseTableStyleColBandSize  = $00000200;
    MaskUseTableStyleRowBandSize  = $00000400;
    MaskUseIsTableOverlap         = $00000800;
    MaskUseFloatingPosition       = $00001000;
    MaskUseLeftBorder             = $00002000;
    MaskUseRightBorder            = $00004000;
    MaskUseTopBorder              = $00008000;
    MaskUseBottomBorder           = $00010000;
    MaskUseInsideHorizontalBorder = $00020000;
    MaskUseInsideVerticalBorder   = $00040000;
    MaskUseBackgroundColor        = $00080000;
    MaskUseTableAlignment         = $00100000;
    MaskUseBorders                = $0007E000;
    MaskUseAvoidDoubleBorders     = $00200000;
    MaskUseAll                    = $7FFFFFFF;
  private
    FValue: Integer;
    function GetUseAvoidDoubleBorders: Boolean;
    function GetUseBackgroundColor: Boolean;
    function GetUseBorders: Boolean;
    function GetUseBottomBorder: Boolean;
    function GetUseBottomMargin: Boolean;
    function GetUseCellSpacing: Boolean;
    function GetUseFloatingPosition: Boolean;
    function GetUseInsideHorizontalBorder: Boolean;
    function GetUseInsideVerticalBorder: Boolean;
    function GetUseIsTableOverlap: Boolean;
    function GetUseLeftBorder: Boolean;
    function GetUseLeftMargin: Boolean;
    function GetUsePreferredWidth: Boolean;
    function GetUseRightBorder: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseTableAlignment: Boolean;
    function GetUseTableIndent: Boolean;
    function GetUseTableLayout: Boolean;
    function GetUseTableLook: Boolean;
    function GetUseTableStyleColBandSize: Boolean;
    function GetUseTableStyleRowBandSize: Boolean;
    function GetUseTopBorder: Boolean;
    function GetUseTopMargin: Boolean;
    procedure SetUseAvoidDoubleBorders(const Value: Boolean);
    procedure SetUseBackgroundColor(const Value: Boolean);
    procedure SetUseBorders(const Value: Boolean);
    procedure SetUseBottomBorder(const Value: Boolean);
    procedure SetUseBottomMargin(const Value: Boolean);
    procedure SetUseCellSpacing(const Value: Boolean);
    procedure SetUseFloatingPosition(const Value: Boolean);
    procedure SetUseInsideHorizontalBorder(const Value: Boolean);
    procedure SetUseInsideVerticalBorder(const Value: Boolean);
    procedure SetUseIsTableOverlap(const Value: Boolean);
    procedure SetUseLeftBorder(const Value: Boolean);
    procedure SetUseLeftMargin(const Value: Boolean);
    procedure SetUsePreferredWidth(const Value: Boolean);
    procedure SetUseRightBorder(const Value: Boolean);
    procedure SetUseRightMargin(const Value: Boolean);
    procedure SetUseTableAlignment(const Value: Boolean);
    procedure SetUseTableIndent(const Value: Boolean);
    procedure SetUseTableLayout(const Value: Boolean);
    procedure SetUseTableLook(const Value: Boolean);
    procedure SetUseTableStyleColBandSize(const Value: Boolean);
    procedure SetUseTableStyleRowBandSize(const Value: Boolean);
    procedure SetUseTopBorder(const Value: Boolean);
    procedure SetUseTopMargin(const Value: Boolean);
  protected
    procedure SetValue(AMask: Integer; ABitVal: Boolean);

    class function GetOptionsUseLeftBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseLeftBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseRightBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseRightBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTopBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseBottomBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBottomBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseInsideVerticalBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseInsideVerticalBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseInsideHorizontalBorder(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseInsideHorizontalBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseLeftMargin(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseLeftMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseRightMargin(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseRightMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTopMargin(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseBottomMargin(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBottomMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseCellSpacing(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseCellSpacing(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUsePreferredWidth(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUsePreferredWidth(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableIndent(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableIndent(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableLayout(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableLayout(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableAlignment(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableAlignment(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableLook(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableLook(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableOverlap(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableOverlap(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableStyleColBandSize(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableStyleColBandSize(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseTableStyleRowBandSize(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTableStyleRowBandSize(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseFloatingPosition(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseFloatingPosition(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseBackgroundColor(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBackgroundColor(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseAvoidDoubleBorders(AOptions: TdxTablePropertiesOptions): Boolean; static;
    class procedure SetOptionsUseAvoidDoubleBorders(AOptions: TdxTablePropertiesOptions; const Value: Boolean); static;
  public
    constructor Create(AValue: Integer = 0); reintroduce;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxTablePropertiesOptions; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetValue(AMask: Integer): Boolean;

    class function OuterOrInside(AMask: Integer; AIsBorderCell: Boolean): Integer;

    property UseLeftMargin: Boolean read GetUseLeftMargin write SetUseLeftMargin;
    property UseRightMargin: Boolean read GetUseRightMargin write SetUseRightMargin;
    property UseTopMargin: Boolean read GetUseTopMargin write SetUseTopMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin write SetUseBottomMargin;
    property UseCellSpacing: Boolean read GetUseCellSpacing write SetUseCellSpacing;
    property UseTableIndent: Boolean read GetUseTableIndent write SetUseTableIndent;
    property UseTableLayout: Boolean read GetUseTableLayout write SetUseTableLayout;
    property UseTableAlignment: Boolean read GetUseTableAlignment write SetUseTableAlignment;
    property UseTableLook: Boolean read GetUseTableLook write SetUseTableLook;
    property UsePreferredWidth: Boolean read GetUsePreferredWidth write SetUsePreferredWidth;
    property UseTableStyleColBandSize: Boolean read GetUseTableStyleColBandSize write SetUseTableStyleColBandSize;
    property UseTableStyleRowBandSize: Boolean read GetUseTableStyleRowBandSize write SetUseTableStyleRowBandSize;
    property UseIsTableOverlap: Boolean read GetUseIsTableOverlap write SetUseIsTableOverlap;
    property UseFloatingPosition: Boolean read GetUseFloatingPosition write SetUseFloatingPosition;
    property UseBackgroundColor: Boolean read GetUseBackgroundColor write SetUseBackgroundColor;
    property UseLeftBorder: Boolean read GetUseLeftBorder write SetUseLeftBorder;
    property UseRightBorder: Boolean read GetUseRightBorder write SetUseRightBorder;
    property UseTopBorder: Boolean read GetUseTopBorder write SetUseTopBorder;
    property UseBottomBorder: Boolean read GetUseBottomBorder write SetUseBottomBorder;
    property UseInsideHorizontalBorder: Boolean read GetUseInsideHorizontalBorder write SetUseInsideHorizontalBorder;
    property UseInsideVerticalBorder: Boolean read GetUseInsideVerticalBorder write SetUseInsideVerticalBorder;
    property UseBorders: Boolean read GetUseBorders write SetUseBorders;
    property UseAvoidDoubleBorders: Boolean read GetUseAvoidDoubleBorders write SetUseAvoidDoubleBorders;
    property Value: Integer read FValue write FValue;
  end;

  { IdxTablePropertiesContainer }

  IdxTablePropertiesContainer = interface(IdxPropertiesContainerWithMask)
  ['{0FEE551B-B04A-412F-B4FA-C47DCBCEBFB1}']
  end;

  TdxHorizontalAlignMode = (
    None,
    Center,
    Inside,
    Left,
    Outside,
    Right
  );

  TdxVerticalAlignMode = (
    None,
    Bottom,
    Center,
    &Inline,
    Inside,
    Outside,
    Top
  );

  TdxVerticalAnchorTypes = (
    Margin,
    Page,
    Paragraph
  );

  TdxHorizontalAnchorTypes = (
    Margin,
    Page,
    Column
  );

  TdxTextWrapping = (
    Never,
    Around
  );

  { TdxTableFloatingPositionInfo }

  TdxTableFloatingPositionInfo = class(TdxCloneable)
  strict private
    FBottomFromText: Integer;
    FLeftFromText: Integer;
    FTopFromText: Integer;
    FRightFromText: Integer;
    FTableHorizontalPosition: Integer;
    FTableVerticalPosition: Integer;
    FHorizAlign: TdxHorizontalAlignMode;
    FVertAlign: TdxVerticalAlignMode;
    FHorizAnchor: TdxHorizontalAnchorTypes;
    FVertAnchor: TdxVerticalAnchorTypes;
    FTextWrapping: TdxTextWrapping;
  public
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxTableFloatingPositionInfo; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function IsHorizontalAbsolutePositionUse: Boolean;
    function IsVerticalAbsolutePositionUse: Boolean;

    property BottomFromText: Integer read FBottomFromText write FBottomFromText;
    property LeftFromText: Integer read FLeftFromText write FLeftFromText;
    property TopFromText: Integer read FTopFromText write FTopFromText;
    property RightFromText: Integer read FRightFromText write FRightFromText;
    property TableHorizontalPosition: Integer read FTableHorizontalPosition write FTableHorizontalPosition;
    property TableVerticalPosition: Integer read FTableVerticalPosition write FTableVerticalPosition;
    property HorizontalAlign: TdxHorizontalAlignMode read FHorizAlign write FHorizAlign;
    property VerticalAlign: TdxVerticalAlignMode read FVertAlign write FVertAlign;
    property HorizontalAnchor: TdxHorizontalAnchorTypes read FHorizAnchor write FHorizAnchor;
    property VerticalAnchor: TdxVerticalAnchorTypes read FVertAnchor write FVertAnchor;
    property TextWrapping: TdxTextWrapping read FTextWrapping write FTextWrapping;
  end;

  TdxTableFloatingPositionChangeType = (
    None,
    LeftFromText,
    RightFromText,
    TopFromText,
    BottomFromText,
    TableHorizontalPosition,
    TableVerticalPosition,
    HorizontalAlign,
    VerticalAlign,
    HorizontalAnchor,
    VerticalAnchor,
    TextWrapping,
    BatchUpdate
  );

  { TdxTableFloatingPositionChangeActionsCalculator }

  TdxTableFloatingPositionChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxTableFloatingPositionChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxTableFloatingPosition }

  TdxTableFloatingPosition = class(TdxRichEditIndexBasedObject<TdxTableFloatingPositionInfo>)
  strict private
    FOwner: IdxPropertiesContainer;
    function GetBottomFromText: Integer;
    function GetHorizontalAlign: TdxHorizontalAlignMode;
    function GetHorizontalAnchor: TdxHorizontalAnchorTypes;
    function GetLeftFromText: Integer;
    function GetRightFromText: Integer;
    function GetTableHorizontalPosition: Integer;
    function GetTableVerticalPosition: Integer;
    function GetTextWrapping: TdxTextWrapping;
    function GetTopFromText: Integer;
    function GetVerticalAlign: TdxVerticalAlignMode;
    function GetVerticalAnchor: TdxVerticalAnchorTypes;
    procedure InnerSetBottomFromText(const Value: Integer);
    procedure InnerSetHorizontalAlign(const Value: TdxHorizontalAlignMode);
    procedure InnerSetHorizontalAnchor(const Value: TdxHorizontalAnchorTypes);
    procedure InnerSetLeftFromText(const Value: Integer);
    procedure InnerSetRightFromText(const Value: Integer);
    procedure InnerSetTableHorizontalPosition(const Value: Integer);
    procedure InnerSetTableVerticalPosition(const Value: Integer);
    procedure InnerSetTextWrapping(const Value: TdxTextWrapping);
    procedure InnerSetTopFromText(const Value: Integer);
    procedure InnerSetVerticalAlign(const Value: TdxVerticalAlignMode);
    procedure InnerSetVerticalAnchor(const Value: TdxVerticalAnchorTypes);
  strict protected
    function SetBottomFromText(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetLeftFromText(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTopFromText(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetRightFromText(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableHorizontalPosition(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableVerticalPosition(const AInfo: TdxTableFloatingPositionInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetHorizontalAlign(const AInfo: TdxTableFloatingPositionInfo; const AValue: TdxHorizontalAlignMode): TdxDocumentModelChangeActions;
    function SetVerticalAlign(const AInfo: TdxTableFloatingPositionInfo; const AValue: TdxVerticalAlignMode): TdxDocumentModelChangeActions;
    function SetHorizontalAnchor(const AInfo: TdxTableFloatingPositionInfo; const AValue: TdxHorizontalAnchorTypes): TdxDocumentModelChangeActions;
    function SetVerticalAnchor(const AInfo: TdxTableFloatingPositionInfo; const AValue: TdxVerticalAnchorTypes): TdxDocumentModelChangeActions;
    function SetTextWrapping(const AInfo: TdxTableFloatingPositionInfo; const AValue: TdxTextWrapping): TdxDocumentModelChangeActions;

    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableFloatingPositionInfo>; override;
    procedure OnBeginAssign; override;
    procedure OnEndAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;

    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;

    function CompareTo(AObj: TObject): Boolean;

    function IsHorizontalRelativePositionUse: Boolean;
    function IsVerticalRelativePositionUse: Boolean;

    property BottomFromText: Integer read GetBottomFromText write InnerSetBottomFromText;
    property LeftFromText: Integer read GetLeftFromText write InnerSetLeftFromText;
    property TopFromText: Integer read GetTopFromText write InnerSetTopFromText;
    property RightFromText: Integer read GetRightFromText write InnerSetRightFromText;
    property TableHorizontalPosition: Integer read GetTableHorizontalPosition write InnerSetTableHorizontalPosition;
    property TableVerticalPosition: Integer read GetTableVerticalPosition write InnerSetTableVerticalPosition;
    property HorizontalAlign: TdxHorizontalAlignMode read GetHorizontalAlign write InnerSetHorizontalAlign;
    property VerticalAlign: TdxVerticalAlignMode read GetVerticalAlign write InnerSetVerticalAlign;
    property HorizontalAnchor: TdxHorizontalAnchorTypes read GetHorizontalAnchor write InnerSetHorizontalAnchor;
    property VerticalAnchor: TdxVerticalAnchorTypes read GetVerticalAnchor write InnerSetVerticalAnchor;
    property TextWrapping: TdxTextWrapping read GetTextWrapping write InnerSetTextWrapping;
  end;

  { TdxTableFloatingPositionInfoCache }

  TdxTableFloatingPositionInfoCache = class(TdxUniqueItemsCache<TdxTableFloatingPositionInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableFloatingPositionInfo; override;
  end;

  { TdxTableGeneralSettingsInfo }

  TdxTableGeneralSettingsInfo = class(TdxCloneable)
  private
    FTableStyleColumnBandSize: Integer;
    FTableStyleRowBandSize: Integer;
    FIsTableOverlap: Boolean;
    FAvoidDoubleBorders: Boolean;
    FTableLayout: TdxTableLayoutType;
    FTableLook: TdxTableLookTypes;
    FBackgroundColor: TdxAlphaColor;
    FTableAlignment: TdxTableRowAlignment;
  public
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxTableGeneralSettingsInfo; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;

    property TableStyleColBandSize: Integer read FTableStyleColumnBandSize write FTableStyleColumnBandSize;
    property TableStyleRowBandSize: Integer read FTableStyleRowBandSize write FTableStyleRowBandSize;
    property IsTableOverlap: Boolean read FIsTableOverlap write FIsTableOverlap;
    property AvoidDoubleBorders: Boolean read FAvoidDoubleBorders write FAvoidDoubleBorders;
    property TableLayout: TdxTableLayoutType read FTableLayout write FTableLayout;
    property TableAlignment: TdxTableRowAlignment read FTableAlignment write FTableAlignment;
    property TableLook: TdxTableLookTypes read FTableLook write FTableLook;
    property BackgroundColor: TdxAlphaColor read FBackgroundColor write FBackgroundColor;
  end;

  TdxTableGeneralSettingsChangeType = (
    None = 0,
    TableLayout,
    TableLook,
    TableStyleColumnBandSize,
    TableStyleRowBandSize,
    IsTableOverlap,
    BackgroundColor,
    BatchUpdate,
    TableAlignment,
    AvoidDoubleBorders
  );

  { TdxTableGeneralSettingsChangeActionsCalculator }

  TdxTableGeneralSettingsChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxTableGeneralSettingsChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxTableGeneralSettingsInfoCache }

  TdxTableGeneralSettingsInfoCache = class(TdxUniqueItemsCache<TdxTableGeneralSettingsInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableGeneralSettingsInfo; override;
  end;

  { TdxTableGeneralSettings }

  TdxTableGeneralSettings = class(TdxRichEditIndexBasedObject<TdxTableGeneralSettingsInfo>)
  strict private
    FOwner: IdxPropertiesContainer;
    function GetAvoidDoubleBorders: Boolean;
    function GetBackgroundColor: TdxAlphaColor;
    function GetIsTableOverlap: Boolean;
    function GetTableAlignment: TdxTableRowAlignment;
    function GetTableLayout: TdxTableLayoutType;
    function GetTableLook: TdxTableLookTypes;
    function GetTableStyleColumnBandSize: Integer;
    function GetTableStyleRowBandSize: Integer;
    procedure InnerSetAvoidDoubleBorders(const Value: Boolean);
    procedure InnerSetBackgroundColor(const Value: TdxAlphaColor);
    procedure InnerSetTableAlignment(const Value: TdxTableRowAlignment);
    procedure InnerSetTableLayout(const Value: TdxTableLayoutType);
    procedure InnerSetTableLook(const Value: TdxTableLookTypes);
    procedure InnerSetTableStyleColumnBandSize(const Value: Integer);
    procedure InnerSetTableStyleRowBandSize(const Value: Integer);
    procedure InnerSetIsTableOverlap(const Value: Boolean);
  strict protected
    function SetTableLayout(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions; overload;
    function SetTableAlignment(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableLook(const ASettings: TdxTableGeneralSettingsInfo; const AValue: TdxTableLookTypes): TdxDocumentModelChangeActions;
    function SetBackgroundColor(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableStyleColumnBandSize(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableStyleRowBandSize(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetTableOverlap(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetAvoidDoubleBorders(const ASettings: TdxTableGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableGeneralSettingsInfo>; override;
    procedure BeginChanging(AChangedProperty: TdxProperties); virtual;
    procedure EndChanging; virtual;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;
    procedure CopyFrom(const ANewSettings: TdxTableGeneralSettings); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const ANewSettings: TdxTableGeneralSettingsInfo); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableGeneralSettingsInfo>); override;
  {$ENDIF}

    property TableLayout: TdxTableLayoutType read GetTableLayout write InnerSetTableLayout;
    property TableAlignment: TdxTableRowAlignment read GetTableAlignment write InnerSetTableAlignment;
    property TableLook: TdxTableLookTypes read GetTableLook write InnerSetTableLook;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write InnerSetBackgroundColor;
    property TableStyleColumnBandSize: Integer read GetTableStyleColumnBandSize write InnerSetTableStyleColumnBandSize;
    property TableStyleRowBandSize: Integer read GetTableStyleRowBandSize write InnerSetTableStyleRowBandSize;
    property IsTableOverlap: Boolean read GetIsTableOverlap write InnerSetIsTableOverlap;
    property AvoidDoubleBorders: Boolean read GetAvoidDoubleBorders write InnerSetAvoidDoubleBorders;
  end;

  { TdxTableProperties }

  TdxTableProperties = class(TdxPropertiesBase<TdxTablePropertiesOptions>,
    IdxTablePropertiesContainer,
    IdxPropertiesContainerWithMask,
    IdxCellMarginsContainer)
  private
    FCellMargins: TdxCellMargins;
    FCellSpacing: TdxCellSpacing;
    FIndent: TdxTableIndent;
    FPreferredWidth: TdxPreferredWidth;
    FGeneralSettings: TdxTableGeneralSettings;
    FBorders: TdxTableBorders;
    FFloatingPosition: TdxTableFloatingPosition;
    function GetAvoidDoubleBorders: Boolean;
    function GetUseValue: Integer;
    function GetBackgroundColor: TdxAlphaColor;
    function GetIsTableOverlap: Boolean;
    function GetTableAlignment: TdxTableRowAlignment;
    function GetTableLayout: TdxTableLayoutType;
    function GetTableLook: TdxTableLookTypes;
    function GetTableStyleColBandSize: Integer;
    function GetTableStyleRowBandSize: Integer;
    function GetUseAvoidDoubleBorders: Boolean;
    function GetUseBackgroundColor: Boolean;
    function GetUseBottomMargin: Boolean;
    function GetUseCellSpacing: Boolean;
    function GetUseFloatingPosition: Boolean;
    function GetUseIsTableOverlap: Boolean;
    function GetUseLeftMargin: Boolean;
    function GetUsePreferredWidth: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseTableAlignment: Boolean;
    function GetUseTableIndent: Boolean;
    function GetUseTableLayout: Boolean;
    function GetUseTableLook: Boolean;
    function GetUseTableStyleColBandSize: Boolean;
    function GetUseTableStyleRowBandSize: Boolean;
    function GetUseTopMargin: Boolean;
    procedure SetAvoidDoubleBorders(const Value: Boolean);
    procedure SetUseValue(const Value: Integer);
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetIsTableOverlap(const Value: Boolean);
    procedure SetTableAlignment(const Value: TdxTableRowAlignment);
    procedure InnerSetTableLayout(const Value: TdxTableLayoutType);
    procedure SetTableLook(const Value: TdxTableLookTypes);
    procedure SetTableStyleColBandSize(const Value: Integer);
    procedure SetTableStyleRowBandSize(const Value: Integer);
  protected
    function GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTablePropertiesOptions>; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTablePropertiesOptions>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable); reintroduce;
    destructor Destroy; override;

    function AreSame(AOtherProperties: TdxTableProperties): Boolean;
    procedure CopyFrom(ANewProperties: TdxTableProperties); overload; virtual;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxTablePropertiesOptions); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTablePropertiesOptions>); override;
  {$ENDIF}
    function GetUse(AMask: Integer): Boolean;
    procedure ResetUse(AMask: Integer);

    procedure Reset;
    procedure ResetAllUse;
    procedure Merge(AProperties: TdxTableProperties); virtual;

    property CellMargins: TdxCellMargins read FCellMargins;
    property UseLeftMargin: Boolean read GetUseLeftMargin;
    property UseRightMargin: Boolean read GetUseRightMargin;
    property UseTopMargin: Boolean read GetUseTopMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin;
    property CellSpacing: TdxCellSpacing read FCellSpacing;
    property UseCellSpacing: Boolean read GetUseCellSpacing;
    property TableIndent: TdxTableIndent read FIndent;
    property UseTableIndent: Boolean read GetUseTableIndent;
    property PreferredWidth: TdxPreferredWidth read FPreferredWidth;
    property UsePreferredWidth: Boolean read GetUsePreferredWidth;
    property Borders: TdxTableBorders read FBorders;
    property TableLayout: TdxTableLayoutType read GetTableLayout write InnerSetTableLayout;
    property UseTableLayout: Boolean read GetUseTableLayout;
    property TableAlignment: TdxTableRowAlignment read GetTableAlignment write SetTableAlignment;
    property UseTableAlignment: Boolean read GetUseTableAlignment;
    property TableLook: TdxTableLookTypes read GetTableLook write SetTableLook;
    property UseTableLook: Boolean read GetUseTableLook;
    property TableStyleColBandSize: Integer read GetTableStyleColBandSize write SetTableStyleColBandSize;
    property UseTableStyleColBandSize: Boolean read GetUseTableStyleColBandSize;
    property TableStyleRowBandSize: Integer read GetTableStyleRowBandSize write SetTableStyleRowBandSize;
    property UseTableStyleRowBandSize: Boolean read GetUseTableStyleRowBandSize;
    property IsTableOverlap: Boolean read GetIsTableOverlap write SetIsTableOverlap;
    property UseIsTableOverlap: Boolean read GetUseIsTableOverlap;
    property FloatingPosition: TdxTableFloatingPosition read FFloatingPosition;
    property UseFloatingPosition: Boolean read GetUseFloatingPosition;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property UseBackgroundColor: Boolean read GetUseBackgroundColor;
    property AvoidDoubleBorders: Boolean read GetAvoidDoubleBorders write SetAvoidDoubleBorders;
    property UseAvoidDoubleBorders: Boolean read GetUseAvoidDoubleBorders;
    property UseValue: Integer read GetUseValue write SetUseValue;


    property GeneralSettings: TdxTableGeneralSettings read FGeneralSettings;
  end;

  { TdxTablePropertiesOptionsCache }

  TdxTablePropertiesOptionsCache = class(TdxUniqueItemsCache<TdxTablePropertiesOptions>)
  public const
    EmptyTableFormattingOptionsItem = 0;
    RootTableFormattingOptionsItem  = 1;
  protected
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); override;
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTablePropertiesOptions; override;
    procedure AddRootStyleOptions;
  end;

  { TdxBordersBase }

  TdxBordersBase = class abstract(TcxIUnknownObject)
  private
    FOwner: IdxPropertiesContainerWithMask;
    FBottomBorder: TdxBottomBorder;
    FLeftBorder: TdxLeftBorder;
    FRightBorder: TdxRightBorder;
    FTopBorder: TdxTopBorder;
    FInsideHorizontalBorder: TdxInsideHorizontalBorder;
    FInsideVerticalBorder: TdxInsideVerticalBorder;
    function GetUseLeftBorder: Boolean;
    function GetUseRightBorder: Boolean;
    function GetUseTopBorder: Boolean;
    function GetUseBottomBorder: Boolean;
    function GetUseInsideHorizontalBorder: Boolean;
    function GetUseInsideVerticalBorder: Boolean;
  protected
    function GetBottomBorder: TdxBorderBase;
    function GetLeftBorder: TdxBorderBase;
    function GetRightBorder: TdxBorderBase;
    function GetTopBorder: TdxBorderBase;

    procedure CopyFromCore(const ABorders: TdxBordersBase); virtual;

    function UseLeftBorderMask: Integer; virtual; abstract;
    function UseRightBorderMask: Integer; virtual; abstract;
    function UseTopBorderMask: Integer; virtual; abstract;
    function UseBottomBorderMask: Integer; virtual; abstract;
    function UseInsideHorizontalBorderMask: Integer; virtual; abstract;
    function UseInsideVerticalBorderMask: Integer; virtual; abstract;
    property Owner: IdxPropertiesContainerWithMask read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainerWithMask);
    destructor Destroy; override;
    procedure CopyFrom(ABorders: TdxBordersBase);
    procedure Merge(ABorders: TdxBordersBase); virtual;

    property TopBorder: TdxTopBorder read FTopBorder;
    property LeftBorder: TdxLeftBorder read FLeftBorder;
    property RightBorder: TdxRightBorder read FRightBorder;
    property BottomBorder: TdxBottomBorder read FBottomBorder;
    property InsideHorizontalBorder: TdxInsideHorizontalBorder read FInsideHorizontalBorder;
    property InsideVerticalBorder: TdxInsideVerticalBorder read FInsideVerticalBorder;

    property UseLeftBorder: Boolean read GetUseLeftBorder;
    property UseRightBorder: Boolean read GetUseRightBorder;
    property UseTopBorder: Boolean read GetUseTopBorder;
    property UseBottomBorder: Boolean read GetUseBottomBorder;
    property UseInsideHorizontalBorder: Boolean read GetUseInsideHorizontalBorder;
    property UseInsideVerticalBorder: Boolean read GetUseInsideVerticalBorder;
  end;

  { TdxTableBorders }

  TdxTableBorders = class(TdxBordersBase)
  protected
    function UseLeftBorderMask: Integer; override;
    function UseRightBorderMask: Integer; override;
    function UseTopBorderMask: Integer; override;
    function UseBottomBorderMask: Integer; override;
    function UseInsideHorizontalBorderMask: Integer; override;
    function UseInsideVerticalBorderMask: Integer; override;
  public
    function AreSame(AOther: TdxTableBorders): Boolean;
  end;

  { TdxTableIndent }

  TdxTableIndent = class(TdxWidthUnit)
  protected
    procedure OnBeginAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo; const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo; const AValue: Integer): TdxDocumentModelChangeActions; override;
  end;

  { TdxCellSpacing }

  TdxCellSpacing = class(TdxWidthUnit)
  protected
    procedure OnBeginAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo;
      const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; override;
  end;

  { TdxTableRowPropertiesOptions }

  TdxTableRowPropertiesOptions = class(TdxCloneable)
  public const
    MaskUseNone              = $00000000;
    MaskUseHeight            = $00000001;
    MaskUseCantSplit         = $00000002;
    MaskUseHideCellMark      = $00000004;
    MaskUseGridBefore        = $00000008;
    MaskUseGridAfter         = $00000010;
    MaskUseWidthBefore       = $00000020;
    MaskUseWidthAfter        = $00000040;
    MaskUseCellSpacing       = $00000080;
    MaskUseTableRowAlignment = $00000100;
    MaskUseHeader            = $00000400;
    MaskUseAll               = $7FFFFFF;
  strict private
    FValue: Integer;
    function GetUseCantSplit: Boolean;
    function GetUseCellSpacing: Boolean;
    function GetUseGridAfter: Boolean;
    function GetUseGridBefore: Boolean;
    function GetUseHeader: Boolean;
    function GetUseHeight: Boolean;
    function GetUseHideCellMark: Boolean;
    function GetUseTableRowAlignment: Boolean;
    function GetUseWidthAfter: Boolean;
    function GetUseWidthBefore: Boolean;
    procedure SetUseCantSplit(const AValue: Boolean);
    procedure SetUseCellSpacing(const AValue: Boolean);
    procedure SetUseGridAfter(const AValue: Boolean);
    procedure SetUseGridBefore(const AValue: Boolean);
    procedure SetUseHeader(const AValue: Boolean);
    procedure SetUseHeight(const AValue: Boolean);
    procedure SetUseHideCellMark(const AValue: Boolean);
    procedure SetUseTableRowAlignment(const AValue: Boolean);
    procedure SetUseWidthAfter(const AValue: Boolean);
    procedure SetUseWidthBefore(const AValue: Boolean);
  protected
    class function GetOptionsUseCantSplit(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseCellSpacing(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseGridAfter(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseGridBefore(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseHeader(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseHeight(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseHideCellMark(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseTableRowAlignment(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseWidthAfter(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class function GetOptionsUseWidthBefore(AOptions: TdxTableRowPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseCantSplit(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseCellSpacing(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseGridAfter(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseGridBefore(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseHeader(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseHeight(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseHideCellMark(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseTableRowAlignment(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseWidthAfter(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;
    class procedure SetOptionsUseWidthBefore(AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean); static;

    procedure SetVal(AMask: Integer; ABitValue: Boolean);
  public
    constructor Create(AValue: Integer = 0); reintroduce;
    function Clone: TdxTableRowPropertiesOptions; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetVal(AMask: Integer): Boolean;

    property UseHeight: Boolean read GetUseHeight write SetUseHeight;
    property UseCantSplit: Boolean read GetUseCantSplit write SetUseCantSplit;
    property UseHideCellMark: Boolean read GetUseHideCellMark write SetUseHideCellMark;
    property UseHeader: Boolean read GetUseHeader write SetUseHeader;
    property UseGridBefore: Boolean read GetUseGridBefore write SetUseGridBefore;
    property UseGridAfter: Boolean read GetUseGridAfter write SetUseGridAfter;
    property UseWidthBefore: Boolean read GetUseWidthBefore write SetUseWidthBefore;
    property UseWidthAfter: Boolean read GetUseWidthAfter write SetUseWidthAfter;
    property UseCellSpacing: Boolean read GetUseCellSpacing write SetUseCellSpacing;
    property UseTableRowAlignment: Boolean read GetUseTableRowAlignment write SetUseTableRowAlignment;
    property Value: Integer read FValue;
  end;

  { TdxTableRowProperties }

  TdxTableRowProperties = class(TdxPropertiesBase<TdxTableRowPropertiesOptions>)
  private
    FHeight: TdxHeightUnit;
    FWidthBefore: TdxWidthUnit;
    FWidthAfter: TdxWidthUnit;
    FCellSpacing: TdxWidthUnit;
    FGeneralSettings: TdxTableRowGeneralSettings;
    function GetCantSplit: Boolean;
    function GetGridAfter: Integer;
    function GetGridBefore: Integer;
    function GetHeader: Boolean;
    function GetHideCellMark: Boolean;
    function GetTableRowAlignment: TdxTableRowAlignment;
    function GetUseCantSplit: Boolean;
    function GetUseCellSpacing: Boolean;
    function GetUseGridAfter: Boolean;
    function GetUseGridBefore: Boolean;
    function GetUseHeader: Boolean;
    function GetUseHeight: Boolean;
    function GetUseHideCellMark: Boolean;
    function GetUseTableRowAlignment: Boolean;
    function GetUseWidthAfter: Boolean;
    function GetUseWidthBefore: Boolean;
    procedure SetCantSplit(const Value: Boolean);
    procedure SetGridAfter(const Value: Integer);
    procedure SetGridBefore(const Value: Integer);
    procedure SetHeader(const Value: Boolean);
    procedure SetHideCellMark(const Value: Boolean);
    procedure SetTableRowAlignment(const Value: TdxTableRowAlignment);
  protected
    function GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableRowPropertiesOptions>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    property GeneralSettings: TdxTableRowGeneralSettings read FGeneralSettings;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable); reintroduce;
    destructor Destroy; override;
    procedure CopyFrom(AProperties: TdxTableRowProperties); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxTableRowPropertiesOptions); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableRowPropertiesOptions>); override;
  {$ENDIF}
    function GetUse(AMask: Integer): Boolean;
    procedure Reset;
    procedure ResetAllUse;
    procedure ResetUse(AMask: Integer);

    procedure Merge(AProperties: TdxTableRowProperties);

    property CantSplit: Boolean read GetCantSplit write SetCantSplit;
    property CellSpacing: TdxWidthUnit read FCellSpacing;
    property GridAfter: Integer read GetGridAfter write SetGridAfter;
    property GridBefore: Integer read GetGridBefore write SetGridBefore;
    property Header: Boolean read GetHeader write SetHeader;
    property Height: TdxHeightUnit read FHeight;
    property HideCellMark: Boolean read GetHideCellMark write SetHideCellMark;
    property TableRowAlignment: TdxTableRowAlignment read GetTableRowAlignment write SetTableRowAlignment;
    property UseCantSplit: Boolean read GetUseCantSplit;
    property UseCellSpacing: Boolean read GetUseCellSpacing;
    property UseGridAfter: Boolean read GetUseGridAfter;
    property UseGridBefore: Boolean read GetUseGridBefore;
    property UseHeader: Boolean read GetUseHeader;
    property UseHeight: Boolean read GetUseHeight;
    property UseHideCellMark: Boolean read GetUseHideCellMark;
    property UseTableRowAlignment: Boolean read GetUseTableRowAlignment;
    property UseWidthAfter: Boolean read GetUseWidthAfter;
    property UseWidthBefore: Boolean read GetUseWidthBefore;
    property WidthAfter: TdxWidthUnit read FWidthAfter;
    property WidthBefore: TdxWidthUnit read FWidthBefore;
  end;

  { TdxTableRowGeneralSettingsInfo }

  TdxTableRowGeneralSettingsInfo = class(TdxCloneable)
  private
    FCantSplit: Boolean;
    FHideCellMark: Boolean;
    FHeader: Boolean;
    FGridBefore: Integer;
    FGridAfter: Integer;
    FTableRowAlignment: TdxTableRowAlignment;
  public
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxTableRowGeneralSettingsInfo; reintroduce; inline;
    function Equals(AObject: TObject): Boolean; override;

    property CantSplit: Boolean read FCantSplit write FCantSplit;
    property HideCellMark: Boolean read FHideCellMark write FHideCellMark;
    property Header: Boolean read FHeader write FHeader;
    property GridBefore: Integer read FGridBefore write FGridBefore;
    property GridAfter: Integer read FGridAfter write FGridAfter;
    property TableRowAlignment: TdxTableRowAlignment read FTableRowAlignment write FTableRowAlignment;
  end;

  { TdxTableRowGeneralSettingsInfoCache }

  TdxTableRowGeneralSettingsInfoCache = class(TdxUniqueItemsCache<TdxTableRowGeneralSettingsInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableRowGeneralSettingsInfo; override;
  end;

  TdxTableRowChangeType = (
    None = 0,
    Header,
    HideCellMark,
    CantSplit,
    TableRowAlignment,
    TableRowConditionalFormatting,
    GridAfter,
    GridBefore,
    BatchUpdate
  );

  { TdxTableRowChangeActionsCalculator }

  TdxTableRowChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxTableRowChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxTableRowGeneralSettings }

  TdxTableRowGeneralSettings = class(TdxRichEditIndexBasedObject<TdxTableRowGeneralSettingsInfo>)
  strict private
    FOwner: IdxPropertiesContainer;
    function GetCantSplit: Boolean;
    function GetGridAfter: Integer;
    function GetGridBefore: Integer;
    function GetHeader: Boolean;
    function GetHideCellMark: Boolean;
    function GetTableRowAlignment: TdxTableRowAlignment;
    procedure InnerSetCantSplit(const Value: Boolean);
    procedure InnerSetGridAfter(const Value: Integer);
    procedure InnerSetGridBefore(const Value: Integer);
    procedure InnerSetHeader(const Value: Boolean);
    procedure InnerSetHideCellMark(const Value: Boolean);
    procedure InnerSetTableRowAlignment(const Value: TdxTableRowAlignment);
  strict protected
    procedure BeginChanging(AChangedProperty: TdxProperties); virtual;
    procedure EndChanging; virtual;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    function SetHeader(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetHideCellMark(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetCantSplit(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetTableRowAlignment(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: TdxTableRowAlignment): TdxDocumentModelChangeActions;
    function SetGridAfter(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetGridBefore(const AInfo: TdxTableRowGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableRowGeneralSettingsInfo>; override;

    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;
    procedure CopyFrom(const ASettings: TdxTableRowGeneralSettings); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxTableRowGeneralSettingsInfo); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableRowGeneralSettingsInfo>); override;
  {$ENDIF}

    property Header: Boolean read GetHeader write InnerSetHeader;
    property HideCellMark: Boolean read GetHideCellMark write InnerSetHideCellMark;
    property CantSplit: Boolean read GetCantSplit write InnerSetCantSplit;
    property TableRowAlignment: TdxTableRowAlignment read GetTableRowAlignment write InnerSetTableRowAlignment;
    property GridAfter: Integer read GetGridAfter write InnerSetGridAfter;
    property GridBefore: Integer read GetGridBefore write InnerSetGridBefore;
  end;

  { TdxCombinedTableRowPropertiesInfo }

  TdxCombinedTableRowPropertiesInfo =
    class(TdxCloneable)
  private
    FIsOwner: Boolean;
    FWidthAfter: TdxWidthUnitInfo;
    FWidthBefore: TdxWidthUnitInfo;
    FCellSpacing: TdxWidthUnitInfo;
    FGeneralSettings: TdxTableRowGeneralSettingsInfo;
    FHeight: TdxHeightUnitInfo;
  public
    constructor Create; overload; override;
    constructor Create(ARowProperties: TdxTableRowProperties); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TdxCombinedTableRowPropertiesInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property Height: TdxHeightUnitInfo read FHeight;
    property WidthBefore: TdxWidthUnitInfo read FWidthBefore;
    property WidthAfter: TdxWidthUnitInfo read FWidthAfter;
    property CellSpacing: TdxWidthUnitInfo read FCellSpacing;
    property GeneralSettings: TdxTableRowGeneralSettingsInfo read FGeneralSettings;
  end;

  { TdxMergedTableRowProperties }

  TdxMergedTableRowProperties = class(TdxMergedCloneableOptionsProperties<TdxCombinedTableRowPropertiesInfo, TdxTableRowPropertiesOptions>)
  protected
    procedure MergeCore(const AInfo: TdxCombinedTableRowPropertiesInfo; const AOptions: TdxTableRowPropertiesOptions);
  public
    constructor Create(AInitialProperties: TdxTableRowProperties); reintroduce; overload;
    constructor Create(AInitialProperties: TdxMergedTableRowProperties); reintroduce; overload;
    procedure Merge(AProperties: TdxTableRowProperties); overload;
    procedure Merge(AProperties: TdxMergedTableRowProperties); overload;
  end;

  { TdxRowHeight }

  TdxRowHeight = class(TdxHeightUnit)
  protected
    procedure OnBeginAssign; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
  end;

  { TdxWidthBefore }

  TdxWidthBefore = class(TdxWidthUnit)
  protected
    procedure OnBeginAssign; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo;
      const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; override;
  end;

  { TdxWidthAfter }

  TdxWidthAfter = class(TdxWidthUnit)
  protected
    procedure OnBeginAssign; override;
    function SetTypeCore(const AUnit: TdxWidthUnitInfo;
      const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions; override;
    function SetValueCore(const AUnit: TdxWidthUnitInfo;
      const AValue: Integer): TdxDocumentModelChangeActions; override;
  end;

  { TdxTableRowPropertiesOptionsCache }

  TdxTableRowPropertiesOptionsCache = class(TdxUniqueItemsCache<TdxTableRowPropertiesOptions>)
  public const
    EmptyRowPropertiesOptionsItem = 0;
    RootRowPropertiesOptionsItem = 1;
  protected
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); override;
    function CreateDefaultItem(
      const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableRowPropertiesOptions; override;
    procedure AddRootStyleOptions;
  end;

  { TdxTableCellPropertiesOptions }

  TdxTableCellPropertiesOptions = class(TdxCloneable)
  public const
    MaskUseNone                   = $00000000;
    MaskUsePreferredWidth         = $00000001;
    MaskUseHideCellMark           = $00000002;
    MaskUseNoWrap                 = $00000004;
    MaskUseFitText                = $00000008;
    MaskUseLeftMargin             = TdxMarginUnitBase.MaskUseLeftMargin;
    MaskUseRightMargin            = TdxMarginUnitBase.MaskUseRightMargin;
    MaskUseTopMargin              = TdxMarginUnitBase.MaskUseTopMargin;
    MaskUseBottomMargin           = TdxMarginUnitBase.MaskUseBottomMargin;
    MaskUseTextDirection          = $00000100;
    MaskUseVerticalAlignment      = $00000200;
    MaskUseCellConditionalFormatting = $00000800;
    MaskUseLeftBorder             = $00001000;
    MaskUseRightBorder            = $00002000;
    MaskUseTopBorder              = $00004000;
    MaskUseBottomBorder           = $00008000;
    MaskUseInsideHorizontalBorder = $00010000;
    MaskUseInsideVerticalBorder   = $000020000;
    MaskUseTopLeftDiagonalBorder  = $00040000;
    MaskUseTopRightDiagonalBorder = $00080000;
    MaskUseBackgroundColor        = $00100000;
    MaskUseForegroundColor        = $00200000;
    MaskUseShading                = $00400000;
    MaskUseAll                    = $7FFFFFF;
  private
    FValue: Integer;
    function GetUseBackgroundColor: Boolean;
    function GetUseBottomBorder: Boolean;
    function GetUseBottomMargin: Boolean;
    function GetUseCellConditionalFormatting: Boolean;
    function GetUseFitText: Boolean;
    function GetUseForegroundColor: Boolean;
    function GetUseHideCellMark: Boolean;
    function GetUseInsideHorizontalBorder: Boolean;
    function GetUseInsideVerticalBorder: Boolean;
    function GetUseLeftBorder: Boolean;
    function GetUseLeftMargin: Boolean;
    function GetUseNoWrap: Boolean;
    function GetUsePreferredWidth: Boolean;
    function GetUseRightBorder: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseShading: Boolean;
    function GetUseTextDirection: Boolean;
    function GetUseTopBorder: Boolean;
    function GetUseTopLeftDiagonalBorder: Boolean;
    function GetUseTopMargin: Boolean;
    function GetUseTopRightDiagonalBorder: Boolean;
    function GetUseVerticalAlignment: Boolean;
    procedure SetUseBackgroundColor(const Value: Boolean);
    procedure SetUseBottomBorder(const Value: Boolean);
    procedure SetUseBottomMargin(const Value: Boolean);
    procedure SetUseCellConditionalFormatting(const Value: Boolean);
    procedure SetUseFitText(const Value: Boolean);
    procedure SetUseForegroundColor(const Value: Boolean);
    procedure SetUseHideCellMark(const Value: Boolean);
    procedure SetUseInsideHorizontalBorder(const Value: Boolean);
    procedure SetUseInsideVerticalBorder(const Value: Boolean);
    procedure SetUseLeftBorder(const Value: Boolean);
    procedure SetUseLeftMargin(const Value: Boolean);
    procedure SetUseNoWrap(const Value: Boolean);
    procedure SetUsePreferredWidth(const Value: Boolean);
    procedure SetUseRightBorder(const Value: Boolean);
    procedure SetUseRightMargin(const Value: Boolean);
    procedure SetUseShading(const Value: Boolean);
    procedure SetUseTextDirection(const Value: Boolean);
    procedure SetUseTopBorder(const Value: Boolean);
    procedure SetUseTopLeftDiagonalBorder(const Value: Boolean);
    procedure SetUseTopMargin(const Value: Boolean);
    procedure SetUseTopRightDiagonalBorder(const Value: Boolean);
    procedure SetUseVerticalAlignment(const Value: Boolean);
  protected
    class function GetOptionsUseCellConditionalFormatting(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseCellConditionalFormatting(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;

    class function GetOptionsUseTopMargin(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopMargin(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseBottomMargin(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBottomMargin(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseLeftMargin(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseLeftMargin(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseRightMargin(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseRightMargin(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;

    class function GetOptionsUseFitText(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseFitText(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseHideCellMark(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseHideCellMark(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseNoWrap(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseNoWrap(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUsePreferredWidth(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUsePreferredWidth(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseTextDirection(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTextDirection(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseVerticalAlignment(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseVerticalAlignment(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseVerticalMerging(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseVerticalMerging(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseColumnSpan(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseColumnSpan(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseBackgroundColor(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBackgroundColor(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseForegroundColor(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseForegroundColor(AOptions: TdxTableCellPropertiesOptions; const Value: Boolean); static;
    class function GetOptionsUseShading(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseShading(AOptions: TdxTableCellPropertiesOptions; const Value: Boolean); static;

    class function GetOptionsUseLeftBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseLeftBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseRightBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseRightBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseTopBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseBottomBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseBottomBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseInsideVerticalBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseInsideVerticalBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseInsideHorizontalBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseInsideHorizontalBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseTopLeftDiagonalBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopLeftDiagonalBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;
    class function GetOptionsUseTopRightDiagonalBorder(AOptions: TdxTableCellPropertiesOptions): Boolean; static;
    class procedure SetOptionsUseTopRightDiagonalBorder(AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean); static;

    procedure SetVal(AMask: Integer; ABitVal: Boolean);
  public
    constructor Create(AValue: Integer = 0); reintroduce;
    function Clone: TdxTableCellPropertiesOptions; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function GetVal(AMask: Integer): Boolean;

    property UseLeftBorder: Boolean read GetUseLeftBorder write SetUseLeftBorder;
    property UseRightBorder: Boolean read GetUseRightBorder write SetUseRightBorder;
    property UseTopBorder: Boolean read GetUseTopBorder write SetUseTopBorder;
    property UseBottomBorder: Boolean read GetUseBottomBorder write SetUseBottomBorder;
    property UseInsideHorizontalBorder: Boolean read GetUseInsideHorizontalBorder write SetUseInsideHorizontalBorder;
    property UseInsideVerticalBorder: Boolean read GetUseInsideVerticalBorder write SetUseInsideVerticalBorder;
    property UseTopLeftDiagonalBorder: Boolean read GetUseTopLeftDiagonalBorder write SetUseTopLeftDiagonalBorder;
    property UseTopRightDiagonalBorder: Boolean read GetUseTopRightDiagonalBorder write SetUseTopRightDiagonalBorder;

    property UsePreferredWidth: Boolean read GetUsePreferredWidth write SetUsePreferredWidth;
    property UseHideCellMark: Boolean read GetUseHideCellMark write SetUseHideCellMark;
    property UseNoWrap: Boolean read GetUseNoWrap write SetUseNoWrap;
    property UseFitText: Boolean read GetUseFitText write SetUseFitText;
    property UseLeftMargin: Boolean read GetUseLeftMargin write SetUseLeftMargin;
    property UseRightMargin: Boolean read GetUseRightMargin write SetUseRightMargin;
    property UseTopMargin: Boolean read GetUseTopMargin write SetUseTopMargin;
    property UseBottomMargin: Boolean read GetUseBottomMargin write SetUseBottomMargin;
    property UseTextDirection: Boolean read GetUseTextDirection write SetUseTextDirection;
    property UseVerticalAlignment: Boolean read GetUseVerticalAlignment write SetUseVerticalAlignment;
    property UseCellConditionalFormatting: Boolean read GetUseCellConditionalFormatting write SetUseCellConditionalFormatting;
    property UseBackgroundColor: Boolean read GetUseBackgroundColor write SetUseBackgroundColor;
    property UseForegroundColor: Boolean read GetUseForegroundColor write SetUseForegroundColor;
    property UseShading: Boolean read GetUseShading write SetUseShading;

    property Value: Integer read FValue write FValue;
  end;

  { TdxTableCellChangeActionsCalculator }

  TdxTableCellChangeType =(
    None = 0,
    HideCellMark,
    NoWrap,
    FitText,
    TextDirection,
    VerticalAlignment,
    ColumnSpan,
    HorizontalMerging,
    VerticalMerging,
    ConditionalFormatting,
    BackgroundColor,
    ForegroundColor,
    Shading,
    BatchUpdate
  );

  { TdxTableCellPropertiesOptionsCache }

  TdxTableCellPropertiesOptionsCache = class(TdxUniqueItemsCache<TdxTableCellPropertiesOptions>)
  public const
    EmptyCellPropertiesOptionsItem = 0;
    RootCellPropertiesOptionsItem = 1;
  protected
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); override;
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableCellPropertiesOptions; override;
    procedure AddRootStyleOptions;
  end;

  { TdxTableCellProperties }

  TdxMergingState = (
    None,
    Continue,
    Restart
  );

  TdxTableCellProperties = class(TdxPropertiesBase<TdxTableCellPropertiesOptions>,
    IdxCellPropertiesContainer,
    IdxCellMarginsContainer)
  private
    FPreferredWidth: TdxPreferredWidth;
    FOwner: IdxCellPropertiesOwner;
    FCellMargins: TdxCellMargins;
    FBorders: TdxTableCellBorders;
    FGeneralSettings: TdxTableCellGeneralSettings;
    FPropertiesChanged: TdxEventHandler;
    function GetBackgroundColor: TdxAlphaColor;
    function GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
    function GetColumnSpan: Integer;
    function GetFitText: Boolean;
    function GetForegroundColor: TdxAlphaColor;
    function GetHideCellMark: Boolean;
    function GetNoWrap: Boolean;
    function GetShading: TdxShadingPattern;
    function GetUseValue: Integer;
    function GetTextDirection: TdxTextDirection;
    function GetUseBackgroundColor: Boolean;
    function GetUseBottomMargin: Boolean;
    function GetUseCellConditionalFormatting: Boolean;
    function GetUseFitText: Boolean;
    function GetUseForegroundColor: Boolean;
    function GetUseHideCellMark: Boolean;
    function GetUseLeftMargin: Boolean;
    function GetUseNoWrap: Boolean;
    function GetUsePreferredWidth: Boolean;
    function GetUseRightMargin: Boolean;
    function GetUseShading: Boolean;
    function GetUseTextDirection: Boolean;
    function GetUseTopMargin: Boolean;
    function GetUseVerticalAlignment: Boolean;
    function GetVerticalAlignment: TdxVerticalAlignment;
    function GetVerticalMerging: TdxMergingState;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetCellConditionalFormatting(const Value: TdxConditionalTableStyleFormattingTypes);
    procedure SetColumnSpan(const Value: Integer);
    procedure SetFitText(const Value: Boolean);
    procedure SetForegroundColor(const Value: TdxAlphaColor);
    procedure SetHideCellMark(const Value: Boolean);
    procedure SetNoWrap(const Value: Boolean);
    procedure SetShading(const Value: TdxShadingPattern);
    procedure SetUseValue(const Value: Integer);
    procedure SetTextDirection(const Value: TdxTextDirection);
    procedure SetVerticalAlignment(const Value: TdxVerticalAlignment);
    procedure SetVerticalMerging(const Value: TdxMergingState);
  protected
    function GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableCellPropertiesOptions>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    procedure RaisePropertiesChanged;
    property Owner: IdxCellPropertiesOwner read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellPropertiesOwner); reintroduce;
    destructor Destroy; override;

    function GetUse(AMask: Integer): Boolean;
    procedure CopyFrom(AProperties: TdxTableCellProperties); overload;
    procedure CopyFrom(AProperties: TdxMergedTableCellProperties); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxTableCellPropertiesOptions); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableCellPropertiesOptions>); override;
  {$ENDIF}
    procedure Reset;
    procedure ResetAllUse;
    procedure ResetUse(AMask: Integer);
    procedure Merge(AProperties: TdxTableCellProperties);

    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property Borders: TdxTableCellBorders read FBorders;
    property CellConditionalFormatting: TdxConditionalTableStyleFormattingTypes read GetCellConditionalFormatting write SetCellConditionalFormatting;
    property CellMargins: TdxCellMargins read FCellMargins;
    property ColumnSpan: Integer read GetColumnSpan write SetColumnSpan;
    property FitText: Boolean read GetFitText write SetFitText;
    property ForegroundColor: TdxAlphaColor read GetForegroundColor write SetForegroundColor;
    property GeneralSettings: TdxTableCellGeneralSettings read FGeneralSettings;
    property HideCellMark: Boolean read GetHideCellMark write SetHideCellMark;
    property NoWrap: Boolean read GetNoWrap write SetNoWrap;
    property PreferredWidth: TdxPreferredWidth read FPreferredWidth;
    property PropertiesChanged: TdxEventHandler read FPropertiesChanged;
    property Shading: TdxShadingPattern read GetShading write SetShading;
    property TextDirection: TdxTextDirection read GetTextDirection write SetTextDirection;
    property UseBackgroundColor: Boolean read GetUseBackgroundColor;
    property UseBottomMargin: Boolean read GetUseBottomMargin;
    property UseCellConditionalFormatting: Boolean read GetUseCellConditionalFormatting;
    property UseFitText: Boolean read GetUseFitText;
    property UseForegroundColor: Boolean read GetUseForegroundColor;
    property UseHideCellMark: Boolean read GetUseHideCellMark;
    property UseLeftMargin: Boolean read GetUseLeftMargin;
    property UseNoWrap: Boolean read GetUseNoWrap;
    property UsePreferredWidth: Boolean read GetUsePreferredWidth;
    property UseRightMargin: Boolean read GetUseRightMargin;
    property UseShading: Boolean read GetUseShading;
    property UseTextDirection: Boolean read GetUseTextDirection;
    property UseTopMargin: Boolean read GetUseTopMargin;
    property UseValue: Integer read GetUseValue write SetUseValue;
    property UseVerticalAlignment: Boolean read GetUseVerticalAlignment;
    property VerticalAlignment: TdxVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property VerticalMerging: TdxMergingState read GetVerticalMerging write SetVerticalMerging;
  end;

  { TdxCombinedCellBordersInfo }

  TdxCombinedCellBordersInfo = class(TdxCloneable)
  private
    FIsOwner: Boolean;
    FBottomBorder: TdxBorderInfo;
    FLeftBorder: TdxBorderInfo;
    FRightBorder: TdxBorderInfo;
    FTopBorder: TdxBorderInfo;
    FInsideHorizontalBorder: TdxBorderInfo;
    FInsideVerticalBorder: TdxBorderInfo;
    FTopLeftDiagonalBorder: TdxBorderInfo;
    FTopRightDiagonalBorder: TdxBorderInfo;
  public
    constructor Create; overload; override;
    constructor Create(ATableBorders: TdxTableCellBorders); reintroduce; overload;
    destructor Destroy; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxCombinedCellBordersInfo; reintroduce; inline;

    property BottomBorder: TdxBorderInfo read FBottomBorder;
    property LeftBorder: TdxBorderInfo read FLeftBorder;
    property RightBorder: TdxBorderInfo read FRightBorder;
    property TopBorder: TdxBorderInfo read FTopBorder;
    property InsideHorizontalBorder: TdxBorderInfo read FInsideHorizontalBorder;
    property InsideVerticalBorder: TdxBorderInfo read FInsideVerticalBorder;
    property TopLeftDiagonalBorder: TdxBorderInfo read FTopLeftDiagonalBorder;
    property TopRightDiagonalBorder: TdxBorderInfo read FTopRightDiagonalBorder;
  end;

  { TdxCombinedTableBordersInfo }

  TdxCombinedTableBordersInfo = class(TdxCloneable)
  private
    FBottomBorder: TdxBorderInfo;
    FInsideVerticalBorder: TdxBorderInfo;
    FInsideHorizontalBorder: TdxBorderInfo;
    FTopBorder: TdxBorderInfo;
    FLeftBorder: TdxBorderInfo;
    FRightBorder: TdxBorderInfo;
  public
    constructor Create; overload; override;
    constructor Create(ATableBorders: TdxTableBorders); reintroduce; overload;
    destructor Destroy; override;

    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxCombinedTableBordersInfo; reintroduce; inline;

    property BottomBorder: TdxBorderInfo read FBottomBorder;
    property LeftBorder: TdxBorderInfo read FLeftBorder;
    property RightBorder: TdxBorderInfo read FRightBorder;
    property TopBorder: TdxBorderInfo read FTopBorder;
    property InsideHorizontalBorder: TdxBorderInfo read FInsideHorizontalBorder;
    property InsideVerticalBorder: TdxBorderInfo read FInsideVerticalBorder;
  end;

  { TdxCombinedTablePropertiesInfo }

  TdxCombinedTablePropertiesInfo = class(TdxCloneable)
  private
    FIndent: TdxWidthUnitInfo;
    FFloatingPosition: TdxTableFloatingPositionInfo;
    FPreferredWidth: TdxWidthUnitInfo;
    FBorders: TdxCombinedTableBordersInfo;
    FCellMargins: TdxCombinedCellMarginsInfo;
    FCellSpacing: TdxWidthUnitInfo;
    FGeneralSettings: TdxTableGeneralSettingsInfo;
  public
    constructor Create; overload; override;
    constructor Create(ATableProperties: TdxTableProperties); reintroduce; overload;
    destructor Destroy; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxCombinedTablePropertiesInfo; reintroduce; inline;

    property CellMargins: TdxCombinedCellMarginsInfo read FCellMargins;
    property Borders: TdxCombinedTableBordersInfo read FBorders;
    property CellSpacing: TdxWidthUnitInfo read FCellSpacing;
    property TableIndent: TdxWidthUnitInfo read FIndent;
    property PreferredWidth: TdxWidthUnitInfo read FPreferredWidth;
    property GeneralSettings: TdxTableGeneralSettingsInfo read FGeneralSettings;
    property FloatingPosition: TdxTableFloatingPositionInfo read FFloatingPosition;
  end;

  { TdxMergedTableProperties }

  TdxMergedTableProperties = class(TdxMergedCloneableOptionsProperties<TdxCombinedTablePropertiesInfo, TdxTablePropertiesOptions>)
  protected
    procedure MergeCore(const AInfo: TdxCombinedTablePropertiesInfo; const AOptions: TdxTablePropertiesOptions);
  public
    constructor Create(AInitialProperties: TdxTableProperties); reintroduce; overload;
    constructor Create(AInitialProperties: TdxMergedTableProperties); reintroduce; overload;

    procedure Merge(AProperties: TdxTableProperties); overload;
    procedure Merge(AProperties: TdxMergedTableProperties); overload;
  end;

  { TdxCombinedCellPropertiesInfo }

  TdxCombinedCellPropertiesInfo = class(TdxCloneable)
  private
    FPreferredWidth: TdxWidthUnitInfo;
    FCellMargins: TdxCombinedCellMarginsInfo;
    FBorders: TdxCombinedCellBordersInfo;
    FGeneralSettings: TdxTableCellGeneralSettingsInfo;
  public
    constructor Create; overload; override;
    constructor Create(ACellProperties: TdxTableCellProperties); reintroduce; overload;
    destructor Destroy; override;
    function Clone: TdxCombinedCellPropertiesInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;

    property PreferredWidth: TdxWidthUnitInfo read FPreferredWidth;
    property CellMargins: TdxCombinedCellMarginsInfo read FCellMargins;
    property Borders: TdxCombinedCellBordersInfo read FBorders;
    property GeneralSettings: TdxTableCellGeneralSettingsInfo read FGeneralSettings;
  end;

  { TdxMergedTableCellProperties }

  TdxMergedTableCellProperties = class(TdxMergedCloneableOptionsProperties<TdxCombinedCellPropertiesInfo, TdxTableCellPropertiesOptions>)
  protected
    procedure MergeCore(const AInfo: TdxCombinedCellPropertiesInfo; const AOptions: TdxTableCellPropertiesOptions);
  public
    constructor Create(AInitialProperties: TdxTableCellProperties); reintroduce; overload;
    constructor Create(AInitialProperties: TdxMergedTableCellProperties); reintroduce; overload;
    procedure Merge(AProperties: TdxTableCellProperties); overload;
    procedure Merge(AProperties: TdxMergedTableCellProperties); overload;
  end;

  { TdxTableCellBorders }

  TdxTableCellBorders = class(TdxBordersBase, IdxTableCellBorders)
  private
    FTopLeftDiagonalBorder: TdxTopLeftDiagonalBorder;
    FTopRightDiagonalBorder: TdxTopRightDiagonalBorder;
    function GetUseTopLeftDiagonalBorder: Boolean;
    function GetUseTopRightDiagonalBorder: Boolean;
  protected
    procedure CopyFromCore(const ABorders: TdxBordersBase); override;
    function UseLeftBorderMask: Integer; override;
    function UseRightBorderMask: Integer; override;
    function UseTopBorderMask: Integer; override;
    function UseBottomBorderMask: Integer; override;
    function UseInsideHorizontalBorderMask: Integer; override;
    function UseInsideVerticalBorderMask: Integer; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellPropertiesContainer);
    destructor Destroy; override;
    procedure Merge(ABorders: TdxBordersBase); override;
    procedure CopyFrom(const ABorders: TdxCombinedCellBordersInfo); overload;

    property TopLeftDiagonalBorder: TdxTopLeftDiagonalBorder read FTopLeftDiagonalBorder;
    property TopRightDiagonalBorder: TdxTopRightDiagonalBorder read FTopRightDiagonalBorder;
    property UseTopLeftDiagonalBorder: Boolean read GetUseTopLeftDiagonalBorder;
    property UseTopRightDiagonalBorder: Boolean read GetUseTopRightDiagonalBorder;
  end;

  { TdxTableCellGeneralSettingsInfo }

  TdxTableCellGeneralSettingsInfo = class(TdxCloneable)
  strict private
    FHideCellMark: Boolean;
    FNoWrap: Boolean;
    FFitText: Boolean;
    FTextDirection: TdxTextDirection;
    FVerticalAlignment: TdxVerticalAlignment;
    FColumnSpan: Integer;
    FHorizontalMerging: TdxMergingState;
    FVerticalMerging: TdxMergingState;
    FCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
    FBackgroundColor: TdxAlphaColor;
    FForegroundColor: TdxAlphaColor;
    FShading: TdxShadingPattern;
  public
    constructor Create; override;
    function Clone: TdxTableCellGeneralSettingsInfo; reintroduce; inline;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Equals(AObject: TObject): Boolean; override;

    property HideCellMark: Boolean read FHideCellMark write FHideCellMark;
    property NoWrap: Boolean read FNoWrap write FNoWrap;
    property FitText: Boolean read FFitText write FFitText;
    property TextDirection: TdxTextDirection read FTextDirection write FTextDirection;
    property VerticalAlignment: TdxVerticalAlignment read FVerticalAlignment write FVerticalAlignment;
    property ColumnSpan: Integer read FColumnSpan write FColumnSpan;
    property HorizontalMerging: TdxMergingState read FHorizontalMerging write FHorizontalMerging;
    property VerticalMerging: TdxMergingState read FVerticalMerging write FVerticalMerging;
    property CellConditionalFormatting: TdxConditionalTableStyleFormattingTypes read FCellConditionalFormatting write FCellConditionalFormatting;
    property BackgroundColor: TdxAlphaColor read FBackgroundColor write FBackgroundColor;
    property ForegroundColor: TdxAlphaColor read FForegroundColor write FForegroundColor;
    property Shading: TdxShadingPattern read FShading write FShading;
  end;

  { TdxTableCellGeneralSettings }

  TdxTableCellGeneralSettings = class(TdxRichEditIndexBasedObject<TdxTableCellGeneralSettingsInfo>)
  strict private
    FOwner: IdxPropertiesContainer;
    function GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
    function GetColumnSpan: Integer;
    function GetFitText: Boolean;
    function GetHideCellMark: Boolean;
    function GetNoWrap: Boolean;
    function GetTextDirection: TdxTextDirection;
    function GetVerticalAlignment: TdxVerticalAlignment;
    function GetVerticalMerging: TdxMergingState;
    function GetBackgroundColor: TdxAlphaColor;
    function GetForegroundColor: TdxAlphaColor;
    function GetShading: TdxShadingPattern;
    procedure InnerSetCellConditionalFormatting(const Value: TdxConditionalTableStyleFormattingTypes);
    procedure InnerSetColumnSpan(const Value: Integer);
    procedure InnerSetFitText(const Value: Boolean);
    procedure InnerSetHideCellMark(const Value: Boolean);
    procedure InnerSetNoWrap(const Value: Boolean);
    procedure InnerSetTextDirection(const Value: TdxTextDirection);
    procedure InnerSetVerticalAlignment(const Value: TdxVerticalAlignment);
    procedure InnerSetVerticalMerging(const Value: TdxMergingState);
    procedure InnerSetBackgroundColor(const Value: TdxAlphaColor);
    procedure InnerSetForegroundColor(const Value: TdxAlphaColor);
    procedure InnerSetShading(const Value: TdxShadingPattern);
  strict protected
    function SetHideCellMark(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetNoWrap(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetFitText(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: Boolean): TdxDocumentModelChangeActions;
    function SetTextDirection(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxTextDirection): TdxDocumentModelChangeActions;
    function SetVerticalAlignment(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions;
    function SetColumnSpan(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: Integer): TdxDocumentModelChangeActions;
    function SetVerticalMerging(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxMergingState): TdxDocumentModelChangeActions;
    function SetCellConditionalFormatting(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxConditionalTableStyleFormattingTypes): TdxDocumentModelChangeActions;
    function SetBackgroundColor(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
    function SetForegroundColor(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
    function SetShading(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxShadingPattern): TdxDocumentModelChangeActions;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableCellGeneralSettingsInfo>; override;
    procedure BeginChanging(AChangedProperty: TdxProperties); virtual;
    procedure EndChanging; virtual;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions); override;
    procedure RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs); override;
    property Owner: IdxPropertiesContainer read FOwner;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer); reintroduce;
    procedure CopyFrom(ANewSettings: TdxTableCellGeneralSettings); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxTableCellGeneralSettingsInfo); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableCellGeneralSettingsInfo>); override;
  {$ENDIF}

    property HideCellMark: Boolean read GetHideCellMark write InnerSetHideCellMark;
    property NoWrap: Boolean read GetNoWrap write InnerSetNoWrap;
    property FitText: Boolean read GetFitText write InnerSetFitText;
    property TextDirection: TdxTextDirection read GetTextDirection write InnerSetTextDirection;
    property VerticalAlignment: TdxVerticalAlignment read GetVerticalAlignment write InnerSetVerticalAlignment;
    property ColumnSpan: Integer read GetColumnSpan write InnerSetColumnSpan;
    property VerticalMerging: TdxMergingState read GetVerticalMerging write InnerSetVerticalMerging;
    property CellConditionalFormatting: TdxConditionalTableStyleFormattingTypes read GetCellConditionalFormatting write InnerSetCellConditionalFormatting;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write InnerSetBackgroundColor;
    property ForegroundColor: TdxAlphaColor read GetForegroundColor write InnerSetForegroundColor;
    property Shading: TdxShadingPattern read GetShading write InnerSetShading;
  end;

  { TdxTableCellGeneralSettingsInfoCache }

  TdxTableCellGeneralSettingsInfoCache = class(TdxUniqueItemsCache<TdxTableCellGeneralSettingsInfo>)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableCellGeneralSettingsInfo; override;
  end;

  TdxTableCellChangeActionsCalculator = class
  public
    class function CalculateChangeActions(AChange: TdxTableCellChangeType): TdxDocumentModelChangeActions; static;
  end;

implementation

uses
  Classes, RTLConsts,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.Exceptions;

{ TdxPropertiesBase<T> }

procedure TdxPropertiesBase<T>.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  if FSuspendCount > 0 then
    FDeferredChanges := FDeferredChanges + AChangeActions
  else
    inherited ApplyChanges(AChangeActions);
end;

procedure TdxPropertiesBase<T>.BeginChanging(AChangedProperty: TdxProperties);
var
  AOptions: T;
  AHistory: TdxDocumentHistory;
  AIsDeferred: Boolean;
begin
  AOptions := ChangeOptionsCore(AChangedProperty, AIsDeferred);
  if not IsSuspendUpdateOptions then
  begin
    AHistory := DocumentModel.History;
    if AHistory <> nil then
      AHistory.BeginTransaction;
  end;
  if AOptions <> nil then
  begin
    ReplaceInfo(AOptions, BatchUpdateChangeActions);
    if not AIsDeferred then
      AOptions.Free;
  end;
end;

procedure TdxPropertiesBase<T>.BeginPropertiesUpdate;
begin
  Inc(FSuspendCount);
  if FSuspendCount > 1 then
    Exit;
  DocumentModel.History.BeginTransaction;
  BeginUpdate;
  FDeferredChanges := [];
end;

function TdxPropertiesBase<T>.ChangeOptionsCore(AChangedProperty: TdxProperties; out AIsDeferred: Boolean): T;
var
  AAccessor: TdxBoolPropertyAccessor<T>;
begin
  try
    AAccessor := GetAccessor(AChangedProperty);
    Result := ChangePropertiesOptions(AAccessor, AIsDeferred);
  except
    Result := Default(T);
  end;
end;

function TdxPropertiesBase<T>.ChangePropertiesOptions(const AAccessor: TdxBoolPropertyAccessor<T>; out AIsDeferred: Boolean): T;
begin
  Result := GetInfoForModification(AIsDeferred);
  if not AAccessor.Get(Result) then
    AAccessor.&Set(Result, True)
  else
  begin
    if not AIsDeferred then
    begin
      Result.Free;
      AIsDeferred := True;
    end;

    Result := Default(T);
  end;
end;

function TdxPropertiesBase<T>.ResetPropertiesOptions(const AAccessor: TdxBoolPropertyAccessor<T>; out AIsDeferred: Boolean): T;
begin
  Result := GetInfoForModification(AIsDeferred);
  if AAccessor.Get(Result) then
    AAccessor.&Set(Result, False)
  else
  begin
    if not AIsDeferred then
    begin
      Result.Free;
      AIsDeferred := True;
    end;

    Result := Default(T);
  end;
end;

procedure TdxPropertiesBase<T>.EndChanging;
var
  AHistory: TdxDocumentHistory;
begin
  if not IsSuspendUpdateOptions then
  begin
    AHistory := DocumentModel.History;
    if (AHistory <> nil) and (AHistory.Transaction <> nil) then
      AHistory.EndTransaction;
  end;
end;

procedure TdxPropertiesBase<T>.EndPropertiesUpdate;
var
  AHistory: TdxDocumentHistory;
begin
  if IsSuspendUpdateOptions then
  begin
    Dec(FSuspendCount);
    if FSuspendCount > 0 then
      Exit;
  end;
  if FDeferredChanges <> [] then
      inherited ApplyChanges(FDeferredChanges);
  FDeferredChanges := [];
  EndUpdate;
  AHistory := DocumentModel.History;
  Assert(AHistory.Transaction <> nil);
  AHistory.EndTransaction;
end;

function TdxPropertiesBase<T>.GetIsSuspendUpdateOptions: Boolean;
begin
  Result := FSuspendCount > 0;
end;

function TdxPropertiesBase<T>.GetPieceTable: TdxCustomPieceTable;
begin
  Result := DocumentModelPart;
end;

function TdxPropertiesBase<T>.GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<T>;
begin
  raise Exception.Create('ThrowInternalException');
end;

function TdxPropertiesBase<T>.ResetOptionsCore(AChangedProperty: TdxProperties; out AIsDeferred: Boolean): T;
var
  AAccessor: TdxBoolPropertyAccessor<T>;
begin
  try
    AAccessor := GetAccessor(AChangedProperty);
    Result := ResetPropertiesOptions(AAccessor, AIsDeferred);
  except
    Result := Default(T);
  end;
end;

procedure TdxPropertiesBase<T>.ResetPropertyUse(AChangedProperty: TdxProperties);
var
  AOptions: T;
  AIsDeferred: Boolean;
begin
  AOptions := ResetOptionsCore(AChangedProperty, AIsDeferred);
  ReplaceInfo(AOptions, BatchUpdateChangeActions);
  if not AIsDeferred then
    AOptions.Free;
end;

{ TdxTablePropertiesOptions }


constructor TdxTablePropertiesOptions.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;


function TdxTablePropertiesOptions.GetValue(AMask: Integer): Boolean;
begin
  Result := (FValue and AMask) <> 0;
end;

class function TdxTablePropertiesOptions.OuterOrInside(AMask: Integer;
  AIsBorderCell: Boolean): Integer;
begin
  if AIsBorderCell then
    Exit(AMask);
  if(AMask and (TdxTablePropertiesOptions.MaskUseLeftBorder or TdxTablePropertiesOptions.MaskUseRightBorder)) <> 0 then
    AMask := AMask or TdxTablePropertiesOptions.MaskUseInsideVerticalBorder;
  if (AMask and (TdxTablePropertiesOptions.MaskUseTopBorder or TdxTablePropertiesOptions.MaskUseBottomBorder)) <> 0 then
    AMask := AMask or TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder;
  Result := AMask and not (TdxTablePropertiesOptions.MaskUseLeftBorder or TdxTablePropertiesOptions.MaskUseRightBorder or
    TdxTablePropertiesOptions.MaskUseTopBorder or TdxTablePropertiesOptions.MaskUseBottomBorder);
end;

procedure TdxTablePropertiesOptions.SetValue(AMask: Integer; ABitVal: Boolean);
begin
  if ABitVal then
    FValue := FValue or AMask
  else
    FValue := FValue and not AMask;
end;

function TdxTablePropertiesOptions.Clone: TdxTablePropertiesOptions;
begin
  Result := TdxTablePropertiesOptions(inherited Clone);
end;

procedure TdxTablePropertiesOptions.CopyFrom(Source: TdxCloneable);
var
  AOptions: TdxTablePropertiesOptions absolute Source;
begin
  FValue := AOptions.FValue;
end;

function TdxTablePropertiesOptions.Equals(AObject: TObject): Boolean;
begin
  if AObject is TdxTablePropertiesOptions then
    Result := Value = TdxTablePropertiesOptions(AObject).Value
  else
    Result :=  False;
end;

function TdxTablePropertiesOptions.GetHashCode: Integer;
begin
  Result := Value;
end;

function TdxTablePropertiesOptions.GetUseAvoidDoubleBorders: Boolean;
begin
  Result := GetValue(MaskUseAvoidDoubleBorders);
end;

function TdxTablePropertiesOptions.GetUseBackgroundColor: Boolean;
begin
  Result := GetValue(MaskUseBackgroundColor);
end;

function TdxTablePropertiesOptions.GetUseBorders: Boolean;
begin
  Result := GetValue(MaskUseBorders);
end;

function TdxTablePropertiesOptions.GetUseBottomBorder: Boolean;
begin
  Result := GetValue(MaskUseBottomBorder);
end;

function TdxTablePropertiesOptions.GetUseBottomMargin: Boolean;
begin
  Result := GetValue(MaskUseBottomMargin);
end;

function TdxTablePropertiesOptions.GetUseCellSpacing: Boolean;
begin
  Result := GetValue(MaskUseCellSpacing);
end;

function TdxTablePropertiesOptions.GetUseFloatingPosition: Boolean;
begin
  Result := GetValue(MaskUseFloatingPosition);
end;

function TdxTablePropertiesOptions.GetUseInsideHorizontalBorder: Boolean;
begin
  Result := GetValue(MaskUseInsideHorizontalBorder);
end;

function TdxTablePropertiesOptions.GetUseInsideVerticalBorder: Boolean;
begin
  Result := GetValue(MaskUseInsideVerticalBorder);
end;

function TdxTablePropertiesOptions.GetUseIsTableOverlap: Boolean;
begin
  Result := GetValue(MaskUseIsTableOverlap);
end;

function TdxTablePropertiesOptions.GetUseLeftBorder: Boolean;
begin
  Result := GetValue(MaskUseLeftBorder);
end;

function TdxTablePropertiesOptions.GetUseLeftMargin: Boolean;
begin
  Result := GetValue(MaskUseLeftMargin);
end;

function TdxTablePropertiesOptions.GetUsePreferredWidth: Boolean;
begin
  Result := GetValue(MaskUsePreferredWidth);
end;

function TdxTablePropertiesOptions.GetUseRightBorder: Boolean;
begin
  Result := GetValue(MaskUseRightBorder);
end;

function TdxTablePropertiesOptions.GetUseRightMargin: Boolean;
begin
  Result := GetValue(MaskUseRightMargin);
end;

function TdxTablePropertiesOptions.GetUseTableAlignment: Boolean;
begin
  Result := GetValue(MaskUseTableAlignment);
end;

function TdxTablePropertiesOptions.GetUseTableIndent: Boolean;
begin
  Result := GetValue(MaskUseTableIndent);
end;

function TdxTablePropertiesOptions.GetUseTableLayout: Boolean;
begin
  Result := GetValue(MaskUseTableLayout);
end;

function TdxTablePropertiesOptions.GetUseTableLook: Boolean;
begin
  Result := GetValue(MaskUseTableLook);
end;

function TdxTablePropertiesOptions.GetUseTableStyleColBandSize: Boolean;
begin
  Result := GetValue(MaskUseTableStyleColBandSize);
end;

function TdxTablePropertiesOptions.GetUseTableStyleRowBandSize: Boolean;
begin
  Result := GetValue(MaskUseTableStyleRowBandSize);
end;

function TdxTablePropertiesOptions.GetUseTopBorder: Boolean;
begin
  Result := GetValue(MaskUseTopBorder);
end;

function TdxTablePropertiesOptions.GetUseTopMargin: Boolean;
begin
  Result := GetValue(MaskUseTopMargin);
end;

procedure TdxTablePropertiesOptions.SetUseAvoidDoubleBorders(const Value: Boolean);
begin
  SetValue(MaskUseAvoidDoubleBorders, Value);
end;

procedure TdxTablePropertiesOptions.SetUseBackgroundColor(const Value: Boolean);
begin
  SetValue(MaskUseBackgroundColor, Value);
end;

procedure TdxTablePropertiesOptions.SetUseBorders(const Value: Boolean);
begin
  SetValue(MaskUseBorders, Value);
end;

procedure TdxTablePropertiesOptions.SetUseBottomBorder(const Value: Boolean);
begin
  SetValue(MaskUseBottomBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseBottomMargin(const Value: Boolean);
begin
  SetValue(MaskUseBottomMargin, Value);
end;

procedure TdxTablePropertiesOptions.SetUseCellSpacing(const Value: Boolean);
begin
  SetValue(MaskUseCellSpacing, Value);
end;

procedure TdxTablePropertiesOptions.SetUseFloatingPosition(const Value: Boolean);
begin
  SetValue(MaskUseFloatingPosition, Value);
end;

procedure TdxTablePropertiesOptions.SetUseInsideHorizontalBorder(const Value: Boolean);
begin
  SetValue(MaskUseInsideHorizontalBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseInsideVerticalBorder(const Value: Boolean);
begin
  SetValue(MaskUseInsideVerticalBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseIsTableOverlap(const Value: Boolean);
begin
  SetValue(MaskUseIsTableOverlap, Value);
end;

procedure TdxTablePropertiesOptions.SetUseLeftBorder(const Value: Boolean);
begin
  SetValue(MaskUseLeftBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseLeftMargin(const Value: Boolean);
begin
  SetValue(MaskUseLeftMargin, Value);
end;

procedure TdxTablePropertiesOptions.SetUsePreferredWidth(const Value: Boolean);
begin
  SetValue(MaskUsePreferredWidth, Value);
end;

procedure TdxTablePropertiesOptions.SetUseRightBorder(const Value: Boolean);
begin
  SetValue(MaskUseRightBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseRightMargin(const Value: Boolean);
begin
  SetValue(MaskUseRightMargin, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableAlignment(const Value: Boolean);
begin
  SetValue(MaskUseTableAlignment, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableIndent(const Value: Boolean);
begin
  SetValue(MaskUseTableIndent, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableLayout(const Value: Boolean);
begin
  SetValue(MaskUseTableLayout, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableLook(const Value: Boolean);
begin
  SetValue(MaskUseTableLook, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableStyleColBandSize(const Value: Boolean);
begin
  SetValue(MaskUseTableStyleColBandSize, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTableStyleRowBandSize(const Value: Boolean);
begin
  SetValue(MaskUseTableStyleRowBandSize, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTopBorder(const Value: Boolean);
begin
  SetValue(MaskUseTopBorder, Value);
end;

procedure TdxTablePropertiesOptions.SetUseTopMargin(const Value: Boolean);
begin
  SetValue(MaskUseTopMargin, Value);
end;

class function TdxTablePropertiesOptions.GetOptionsUseLeftBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseLeftBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseLeftBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseLeftBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseRightBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseRightBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseRightBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseRightBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTopBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTopBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTopBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseBottomBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseBottomBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseBottomBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseBottomBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseInsideVerticalBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseInsideVerticalBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseInsideVerticalBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseInsideVerticalBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseInsideHorizontalBorder(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseInsideHorizontalBorder;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseInsideHorizontalBorder(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseInsideHorizontalBorder := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseLeftMargin(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseLeftMargin;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseLeftMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseLeftMargin := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseRightMargin(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseRightMargin;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseRightMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseRightMargin := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTopMargin(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopMargin;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTopMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTopMargin := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseBottomMargin(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseBottomMargin;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseBottomMargin(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseBottomMargin := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseCellSpacing(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseCellSpacing;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseCellSpacing(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseCellSpacing := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUsePreferredWidth(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UsePreferredWidth;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUsePreferredWidth(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UsePreferredWidth := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableIndent(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableIndent;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableIndent(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableIndent := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableLayout(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableLayout;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableLayout(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableLayout := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableAlignment(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableAlignment;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableAlignment(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableAlignment := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableLook(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableLook;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableLook(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableLook := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableOverlap(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseIsTableOverlap;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableOverlap(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseIsTableOverlap := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableStyleColBandSize(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableStyleColBandSize;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableStyleColBandSize(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableStyleColBandSize := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseTableStyleRowBandSize(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableStyleRowBandSize;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseTableStyleRowBandSize(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseTableStyleRowBandSize := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseFloatingPosition(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseFloatingPosition;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseFloatingPosition(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseFloatingPosition := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseBackgroundColor(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseBackgroundColor;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseBackgroundColor(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseBackgroundColor := Value;
end;

class function TdxTablePropertiesOptions.GetOptionsUseAvoidDoubleBorders(AOptions: TdxTablePropertiesOptions): Boolean;
begin
  Result := AOptions.UseAvoidDoubleBorders;
end;

class procedure TdxTablePropertiesOptions.SetOptionsUseAvoidDoubleBorders(AOptions: TdxTablePropertiesOptions; const Value: Boolean);
begin
  AOptions.UseAvoidDoubleBorders := Value;
end;

{ TdxBoolPropertyAccessor<T> }

constructor TdxBoolPropertyAccessor<T>.Create(const AGet: TdxGetOptionValueDelegate<T>; const ASet: TdxSetOptionValueDelegate<T>);
begin
  FGet := AGet;
  FSet := ASet;
end;

{ TdxTableFloatingPositionInfo }

function TdxTableFloatingPositionInfo.Clone: TdxTableFloatingPositionInfo;
begin
  Result := TdxTableFloatingPositionInfo(inherited Clone);
end;

procedure TdxTableFloatingPositionInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxTableFloatingPositionInfo absolute Source;
begin
  BottomFromText := AInfo.BottomFromText;
  LeftFromText := AInfo.LeftFromText;
  TopFromText := AInfo.TopFromText;
  RightFromText := AInfo.RightFromText;
  TableHorizontalPosition := AInfo.TableHorizontalPosition;
  TableVerticalPosition := AInfo.TableVerticalPosition;
  HorizontalAlign := AInfo.HorizontalAlign;
  VerticalAlign := AInfo.VerticalAlign;
  HorizontalAnchor := AInfo.HorizontalAnchor;
  VerticalAnchor := AInfo.VerticalAnchor;
  TextWrapping := AInfo.TextWrapping;
end;

function TdxTableFloatingPositionInfo.Equals(AObject: TObject): Boolean;
var
  APos: TdxTableFloatingPositionInfo;
begin
  if AObject is TdxTableFloatingPositionInfo then
  begin
    APos := TdxTableFloatingPositionInfo(AObject);
    Result := (BottomFromText = APos.BottomFromText) and
      (LeftFromText = APos.LeftFromText) and
      (TopFromText = APos.TopFromText) and
      (RightFromText = APos.RightFromText) and
      (TableHorizontalPosition = APos.TableHorizontalPosition) and
      (TableVerticalPosition = APos.TableVerticalPosition) and
      (HorizontalAlign = APos.HorizontalAlign) and
      (VerticalAlign = APos.VerticalAlign) and
      (HorizontalAnchor = APos.HorizontalAnchor) and
      (VerticalAnchor = APos.VerticalAnchor) and
      (TextWrapping = APos.TextWrapping);
  end
  else
    Result := False;
end;

function TdxTableFloatingPositionInfo.GetHashCode: Integer;
begin
  Result := inherited GetHashCode;
end;

function TdxTableFloatingPositionInfo.IsHorizontalAbsolutePositionUse: Boolean;
begin
  Result := HorizontalAlign = TdxHorizontalAlignMode.None;
end;

function TdxTableFloatingPositionInfo.IsVerticalAbsolutePositionUse: Boolean;
begin
  Result := VerticalAlign = TdxVerticalAlignMode.None;
end;

{ TdxTableFloatingPosition }

constructor TdxTableFloatingPosition.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  FOwner := AOwner;
end;

function TdxTableFloatingPosition.CompareTo(AObj: TObject): Boolean;
var
  AOther: TdxTableFloatingPosition;
begin
  AOther := Safe<TdxTableFloatingPosition>.Cast(AObj);
  if AOther = nil then
    Exit(False);
  Result := (BottomFromText = AOther.BottomFromText) and
    (HorizontalAlign = AOther.HorizontalAlign) and
    (HorizontalAnchor = AOther.HorizontalAnchor) and
    (LeftFromText = AOther.LeftFromText) and
    (TableHorizontalPosition = AOther.TableHorizontalPosition) and
    (TableVerticalPosition = AOther.TableVerticalPosition) and
    (TopFromText = AOther.TopFromText) and
    (VerticalAlign = AOther.VerticalAlign) and
    (VerticalAnchor = AOther.VerticalAnchor);
end;

function TdxTableFloatingPosition.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.BatchUpdate);
end;

function TdxTableFloatingPosition.GetBottomFromText: Integer;
begin
  Result := Info.BottomFromText;
end;

function TdxTableFloatingPosition.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableFloatingPositionInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableFloatingPositionInfoCache;
end;

function TdxTableFloatingPosition.GetHorizontalAlign: TdxHorizontalAlignMode;
begin
  Result := Info.HorizontalAlign;
end;

function TdxTableFloatingPosition.GetHorizontalAnchor: TdxHorizontalAnchorTypes;
begin
  Result := Info.HorizontalAnchor;
end;

function TdxTableFloatingPosition.GetLeftFromText: Integer;
begin
  Result := Info.LeftFromText;
end;

function TdxTableFloatingPosition.GetRightFromText: Integer;
begin
  Result := Info.RightFromText;
end;

function TdxTableFloatingPosition.GetTableHorizontalPosition: Integer;
begin
  Result := Info.TableHorizontalPosition;
end;

function TdxTableFloatingPosition.GetTableVerticalPosition: Integer;
begin
  Result := Info.TableVerticalPosition;
end;

function TdxTableFloatingPosition.GetTextWrapping: TdxTextWrapping;
begin
  Result := Info.TextWrapping;
end;

function TdxTableFloatingPosition.GetTopFromText: Integer;
begin
  Result := Info.TopFromText;
end;

function TdxTableFloatingPosition.GetVerticalAlign: TdxVerticalAlignMode;
begin
  Result := Info.VerticalAlign;
end;

function TdxTableFloatingPosition.GetVerticalAnchor: TdxVerticalAnchorTypes;
begin
  Result := Info.VerticalAnchor;
end;

procedure TdxTableFloatingPosition.InnerSetBottomFromText(const Value: Integer);
begin
  if Value = Info.BottomFromText then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetBottomFromText, Value);
end;

procedure TdxTableFloatingPosition.InnerSetHorizontalAlign(const Value: TdxHorizontalAlignMode);
begin
  if Value = Info.HorizontalAlign then
    NotifyFakeAssign
  else
    SetPropertyValue<TdxHorizontalAlignMode>(SetHorizontalAlign, Value);
end;

procedure TdxTableFloatingPosition.InnerSetHorizontalAnchor(const Value: TdxHorizontalAnchorTypes);
begin
  if Value = Info.HorizontalAnchor then
    NotifyFakeAssign
  else
    SetPropertyValue<TdxHorizontalAnchorTypes>(SetHorizontalAnchor, Value);
end;

procedure TdxTableFloatingPosition.InnerSetLeftFromText(const Value: Integer);
begin
  if Value = Info.LeftFromText then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetLeftFromText, Value);
end;

procedure TdxTableFloatingPosition.InnerSetRightFromText(const Value: Integer);
begin
  if Value = Info.RightFromText then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetRightFromText, Value);
end;

procedure TdxTableFloatingPosition.InnerSetTableHorizontalPosition(const Value: Integer);
begin
  if Value = Info.TableHorizontalPosition then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetTableHorizontalPosition, Value);
end;

procedure TdxTableFloatingPosition.InnerSetTableVerticalPosition(const Value: Integer);
begin
  if Value = Info.TableVerticalPosition then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetTableVerticalPosition, Value);
end;

procedure TdxTableFloatingPosition.InnerSetTextWrapping(const Value: TdxTextWrapping);
begin
  if Value = Info.TextWrapping then
    NotifyFakeAssign
  else
    SetPropertyValue<TdxTextWrapping>(SetTextWrapping, Value);
end;

procedure TdxTableFloatingPosition.InnerSetTopFromText(const Value: Integer);
begin
  if Value = Info.TopFromText then
    NotifyFakeAssign
  else
    SetPropertyValue<Integer>(SetTopFromText, Value);
end;

procedure TdxTableFloatingPosition.InnerSetVerticalAlign(const Value: TdxVerticalAlignMode);
begin
  if Value = Info.VerticalAlign then
      NotifyFakeAssign
  else
    SetPropertyValue<TdxVerticalAlignMode>(SetVerticalAlign, Value);
end;

procedure TdxTableFloatingPosition.InnerSetVerticalAnchor(const Value: TdxVerticalAnchorTypes);
begin
  if Value = Info.VerticalAnchor then
    NotifyFakeAssign
  else
    SetPropertyValue<TdxVerticalAnchorTypes>(SetVerticalAnchor, Value);
end;

function TdxTableFloatingPosition.IsHorizontalRelativePositionUse: Boolean;
begin
  Result := HorizontalAlign <> TdxHorizontalAlignMode.None;
end;

function TdxTableFloatingPosition.IsVerticalRelativePositionUse: Boolean;
begin
  Result := VerticalAlign <> TdxVerticalAlignMode.None;
end;

procedure TdxTableFloatingPosition.OnBeginAssign;
begin
  inherited OnBeginAssign;
  if Owner <> nil then
    Owner.BeginChanging(TdxProperties.TableFloatingPosition);
end;

procedure TdxTableFloatingPosition.OnEndAssign;
begin
  if Owner <> nil then
    Owner.EndChanging;
  inherited OnEndAssign;
end;

function TdxTableFloatingPosition.SetBottomFromText(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.BottomFromText := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.BottomFromText);
end;

function TdxTableFloatingPosition.SetHorizontalAlign(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: TdxHorizontalAlignMode): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalAlign := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.HorizontalAlign);
end;

function TdxTableFloatingPosition.SetHorizontalAnchor(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: TdxHorizontalAnchorTypes): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalAnchor := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.HorizontalAnchor);
end;

function TdxTableFloatingPosition.SetLeftFromText(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.LeftFromText := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.LeftFromText);
end;

function TdxTableFloatingPosition.SetRightFromText(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.RightFromText := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.RightFromText);
end;

function TdxTableFloatingPosition.SetTableHorizontalPosition(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TableHorizontalPosition := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.TableHorizontalPosition);
end;

function TdxTableFloatingPosition.SetTableVerticalPosition(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TableVerticalPosition := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.TableVerticalPosition);
end;

function TdxTableFloatingPosition.SetTextWrapping(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: TdxTextWrapping): TdxDocumentModelChangeActions;
begin
  AInfo.TextWrapping := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.TextWrapping);
end;

function TdxTableFloatingPosition.SetTopFromText(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.TopFromText := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.TopFromText);
end;

function TdxTableFloatingPosition.SetVerticalAlign(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: TdxVerticalAlignMode): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalAlign := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.VerticalAlign);
end;

function TdxTableFloatingPosition.SetVerticalAnchor(const AInfo: TdxTableFloatingPositionInfo;
  const AValue: TdxVerticalAnchorTypes): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalAnchor := AValue;
  Result := TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(TdxTableFloatingPositionChangeType.VerticalAnchor);
end;

{ TdxTableFloatingPositionInfoCache }

function TdxTableFloatingPositionInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableFloatingPositionInfo;
begin
  Result := TdxTableFloatingPositionInfo.Create;
  Result.HorizontalAlign := TdxHorizontalAlignMode.None;
  Result.VerticalAlign := TdxVerticalAlignMode.None;
  Result.HorizontalAnchor := TdxHorizontalAnchorTypes.Page;
  Result.VerticalAnchor := TdxVerticalAnchorTypes.Page;
  Result.TableHorizontalPosition := 0;
  Result.TableVerticalPosition := 0;
  Result.TextWrapping := TdxTextWrapping.Never;
end;

{ TdxTableGeneralSettingsInfo }

function TdxTableGeneralSettingsInfo.Clone: TdxTableGeneralSettingsInfo;
begin
  Result := TdxTableGeneralSettingsInfo(inherited Clone);
end;

procedure TdxTableGeneralSettingsInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxTableGeneralSettingsInfo absolute Source;
begin
  TableStyleColBandSize := AInfo.TableStyleColBandSize;
  TableStyleRowBandSize := AInfo.TableStyleRowBandSize;
  IsTableOverlap := AInfo.IsTableOverlap;
  TableLayout := AInfo.TableLayout;
  TableLook := AInfo.TableLook;
  BackgroundColor := AInfo.BackgroundColor;
  TableAlignment := AInfo.TableAlignment;
  AvoidDoubleBorders := AInfo.AvoidDoubleBorders;
end;

function TdxTableGeneralSettingsInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxTableGeneralSettingsInfo;
begin
  if AObject is TdxTableGeneralSettingsInfo then
  begin
    AInfo := TdxTableGeneralSettingsInfo(AObject);
    Result := (TableLayout = AInfo.TableLayout) and
      (TableLook = AInfo.TableLook) and
      (IsTableOverlap = AInfo.IsTableOverlap) and
      (TableStyleColBandSize = AInfo.TableStyleColBandSize) and
      (TableStyleRowBandSize = AInfo.TableStyleRowBandSize) and
      (BackgroundColor = AInfo.BackgroundColor) and
      (TableAlignment = AInfo.TableAlignment) and
      (AvoidDoubleBorders = AInfo.AvoidDoubleBorders);
  end
  else
    Result := False;
end;

{ TdxTableGeneralSettingsChangeActionsCalculator }

class function TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxTableGeneralSettingsChangeType): TdxDocumentModelChangeActions;
const
  TableGeneralSettingsChangeActionsMap: array[TdxTableGeneralSettingsChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetSecondaryLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout]
  );
begin
  Result := TableGeneralSettingsChangeActionsMap[AChange];
end;

{ TdxTableGeneralSettingsInfoCache }

function TdxTableGeneralSettingsInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableGeneralSettingsInfo;
begin
  Result := TdxTableGeneralSettingsInfo.Create;
  Result.TableStyleColBandSize := 1;
  Result.TableStyleRowBandSize := 1;
  Result.IsTableOverlap := True;
  Result.TableLayout := TdxTableLayoutType.Autofit;
  Result.TableLook := [];
  Result.BackgroundColor := TdxAlphaColors.Empty;
  Result.TableAlignment := TdxTableRowAlignment.Left;
end;

{ TdxTableGeneralSettings }

procedure TdxTableGeneralSettings.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  Owner.ApplyChanges(AChangeActions);
end;

procedure TdxTableGeneralSettings.CopyFrom(const ANewSettings: TdxTableGeneralSettings);
begin
  Owner.BeginPropertiesUpdate;
  try
    BeginUpdate;
    try
      TableLayout := ANewSettings.TableLayout;
      TableLook := ANewSettings.TableLook;
      IsTableOverlap := ANewSettings.IsTableOverlap;
      TableStyleColumnBandSize := ANewSettings.TableStyleColumnBandSize;
      TableStyleRowBandSize := ANewSettings.TableStyleRowBandSize;
      BackgroundColor := ANewSettings.BackgroundColor;
      TableAlignment := ANewSettings.TableAlignment;
      AvoidDoubleBorders := ANewSettings.AvoidDoubleBorders;
    finally
      EndUpdate;
    end;
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableGeneralSettings.CopyFrom(const ANewSettings: TdxTableGeneralSettingsInfo);
begin
  inherited CopyFrom(ANewSettings);
end;

procedure TdxTableGeneralSettings.CopyFrom(const Source:
  TdxUndoableIndexBasedObject<TdxTableGeneralSettingsInfo>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

procedure TdxTableGeneralSettings.BeginChanging(AChangedProperty: TdxProperties);
begin
  DocumentModel.BeginUpdate;
  Owner.BeginChanging(AChangedProperty);
end;

constructor TdxTableGeneralSettings.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

procedure TdxTableGeneralSettings.EndChanging;
begin
  Owner.EndChanging;
  DocumentModel.EndUpdate;
end;

function TdxTableGeneralSettings.GetAvoidDoubleBorders: Boolean;
begin
  Result := Info.AvoidDoubleBorders;
end;

function TdxTableGeneralSettings.GetBackgroundColor: TdxAlphaColor;
begin
  Result := Info.BackgroundColor;
end;

function TdxTableGeneralSettings.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.BatchUpdate);
end;

function TdxTableGeneralSettings.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableGeneralSettingsInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableGeneralSettingsInfoCache;
end;

function TdxTableGeneralSettings.GetIsTableOverlap: Boolean;
begin
  Result := Info.IsTableOverlap;
end;

function TdxTableGeneralSettings.GetTableAlignment: TdxTableRowAlignment;
begin
  Result := Info.TableAlignment;
end;

function TdxTableGeneralSettings.GetTableLayout: TdxTableLayoutType;
begin
  Result := Info.TableLayout;
end;

function TdxTableGeneralSettings.GetTableLook: TdxTableLookTypes;
begin
  Result := Info.TableLook;
end;

function TdxTableGeneralSettings.GetTableStyleColumnBandSize: Integer;
begin
  Result := Info.TableStyleColBandSize;
end;

function TdxTableGeneralSettings.GetTableStyleRowBandSize: Integer;
begin
  Result := Info.TableStyleRowBandSize;
end;

procedure TdxTableGeneralSettings.InnerSetAvoidDoubleBorders(const Value: Boolean);
begin
  BeginChanging(TdxProperties.AvoidDoubleBorders);
  try
    if Info.AvoidDoubleBorders = Value then
      Exit;
    SetPropertyValue<Boolean>(SetAvoidDoubleBorders, Value);
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetBackgroundColor(const Value: TdxAlphaColor);
begin
  BeginChanging(TdxProperties.BackgroundColor);
  try
    if Info.BackgroundColor = Value then
      Exit;
    SetPropertyValue<Integer>(SetBackgroundColor, Ord(Value));
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetTableAlignment(const Value: TdxTableRowAlignment);
begin
  BeginChanging(TdxProperties.TableAlignment);
  try
    if Info.TableAlignment = Value then
      Exit;
    SetPropertyValue<Integer>(SetTableAlignment, Ord(Value));
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetTableLayout(const Value: TdxTableLayoutType);
begin
  BeginChanging(TdxProperties.TableLayout);
  try
    if Info.TableLayout = Value then
      Exit;
    SetPropertyValue<Integer>(SetTableLayout, Ord(Value));
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetTableLook(const Value: TdxTableLookTypes);
begin
  BeginChanging(TdxProperties.TableLook);
  try
    if Info.TableLook = Value then
      Exit;
    SetPropertyValue<TdxTableLookTypes>(SetTableLook, Value);
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetTableStyleColumnBandSize(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('TableStyleColumnBandSize', Value);

  BeginChanging(TdxProperties.TableStyleColumnBandSize);
  try
    if Info.TableStyleColBandSize = Value then
      Exit;
    SetPropertyValue<Integer>(SetTableStyleColumnBandSize, Value);
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.InnerSetTableStyleRowBandSize(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('TableStyleRowBandSize', Value);

  BeginChanging(TdxProperties.TableStyleRowBandSize);
  try
    if Info.TableStyleRowBandSize = Value then
      Exit;
    SetPropertyValue<Integer>(SetTableStyleRowBandSize, Value);
  finally
    EndChanging;
  end;
end;

procedure TdxTableGeneralSettings.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  Owner.RaiseObtainAffectedRange(AArgs);
end;

function TdxTableGeneralSettings.SetAvoidDoubleBorders(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  ASettings.AvoidDoubleBorders := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.AvoidDoubleBorders);
end;

function TdxTableGeneralSettings.SetBackgroundColor(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  ASettings.BackgroundColor := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.BackgroundColor);
end;

procedure TdxTableGeneralSettings.InnerSetIsTableOverlap(const Value: Boolean);
begin
  BeginChanging(TdxProperties.IsTableOverlap);
  try
    if Info.IsTableOverlap = Value then
      Exit;
    SetPropertyValue<Boolean>(SetTableOverlap, Value);
  finally
    EndChanging;
  end;
end;

function TdxTableGeneralSettings.SetTableAlignment(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  ASettings.TableAlignment := TdxTableRowAlignment(AValue);
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.TableAlignment);
end;

function TdxTableGeneralSettings.SetTableLayout(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  ASettings.TableLayout := TdxTableLayoutType(AValue);
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.TableLayout);
end;

function TdxTableGeneralSettings.SetTableLook(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: TdxTableLookTypes): TdxDocumentModelChangeActions;
begin
  ASettings.TableLook := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.TableLook);
end;

function TdxTableGeneralSettings.SetTableOverlap(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  ASettings.IsTableOverlap := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.IsTableOverlap);
end;

function TdxTableGeneralSettings.SetTableStyleColumnBandSize(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  ASettings.TableStyleColBandSize := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.TableStyleColumnBandSize);
end;

function TdxTableGeneralSettings.SetTableStyleRowBandSize(const ASettings: TdxTableGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  ASettings.TableStyleRowBandSize := AValue;
  Result := TdxTableGeneralSettingsChangeActionsCalculator.CalculateChangeActions(TdxTableGeneralSettingsChangeType.TableStyleRowBandSize);
end;

{ TdxTableProperties }

constructor TdxTableProperties.Create(const ADocumentModelPart: TdxCustomPieceTable);
begin
  inherited Create(ADocumentModelPart);
  FCellMargins := TdxCellMargins.Create(PieceTable, Self);
  FCellSpacing := TdxCellSpacing.Create(PieceTable, Self);
  FIndent := TdxTableIndent.Create(PieceTable, Self);
  FPreferredWidth := TdxPreferredWidth.Create(PieceTable, Self);
  FGeneralSettings := TdxTableGeneralSettings.Create(PieceTable, Self);
  FBorders := TdxTableBorders.Create(PieceTable, Self);
  FFloatingPosition := TdxTableFloatingPosition.Create(PieceTable, Self);
end;

destructor TdxTableProperties.Destroy;
begin
  FFloatingPosition.Free;
  FBorders.Free;
  FGeneralSettings.Free;
  FPreferredWidth.Free;
  FIndent.Free;
  FCellSpacing.Free;
  FCellMargins.Free;
  inherited Destroy;
end;

function TdxTableProperties.AreSame(AOtherProperties: TdxTableProperties): Boolean;
begin
  Result := (Info.Value = AOtherProperties.Info.Value) and
    CellMargins.AreSame(AOtherProperties.CellMargins) and
    (CellSpacing.Index = AOtherProperties.CellSpacing.Index) and
    (TableIndent.Index = AOtherProperties.TableIndent.Index) and
    (PreferredWidth.Index = AOtherProperties.PreferredWidth.Index) and
    (GeneralSettings.Index = AOtherProperties.GeneralSettings.Index) and
    Borders.AreSame(AOtherProperties.Borders) and
    (FloatingPosition.Index = AOtherProperties.FloatingPosition.Index);
end;

procedure TdxTableProperties.CopyFrom(ANewProperties: TdxTableProperties);
var
  AOptions: TdxTablePropertiesOptions;
  AContainer: IdxPropertiesContainer;
  AIsDeferredInfo: Boolean;
begin
  AContainer := Self;
  DocumentModel.BeginUpdate;
  AContainer.BeginPropertiesUpdate;
  try
    if DocumentModel = ANewProperties.DocumentModel then
    begin
      Borders.CopyFrom(ANewProperties.Borders);
      GeneralSettings.CopyFrom(ANewProperties.GeneralSettings);
      CellMargins.CopyFrom(ANewProperties.CellMargins);
      CellSpacing.CopyFrom(ANewProperties.CellSpacing);
      TableIndent.CopyFrom(ANewProperties.TableIndent);
      PreferredWidth.CopyFrom(ANewProperties.PreferredWidth);
      FloatingPosition.CopyFrom(ANewProperties.FloatingPosition);
      AOptions := GetInfoForModification(AIsDeferredInfo);
      AOptions.CopyFrom(ANewProperties.Info);
      ReplaceInfo(AOptions, []);
    end
    else
    begin
      Borders.CopyFrom(ANewProperties.Borders);
      GeneralSettings.CopyFrom(ANewProperties.GeneralSettings.Info);
      CellMargins.CopyFrom(ANewProperties.CellMargins);
      CellSpacing.CopyFrom(ANewProperties.CellSpacing.Info);
      TableIndent.CopyFrom(ANewProperties.TableIndent.Info);
      PreferredWidth.CopyFrom(ANewProperties.PreferredWidth.Info);
      FloatingPosition.CopyFrom(ANewProperties.FloatingPosition.Info);
      Info.CopyFrom(ANewProperties.Info);
    end;
  finally
    AContainer.EndPropertiesUpdate;
    DocumentModel.EndUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableProperties.CopyFrom(const Source: TdxTablePropertiesOptions);
begin
  inherited CopyFrom(Source);
end;

procedure TdxTableProperties.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxTablePropertiesOptions>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

function TdxTableProperties.GetAvoidDoubleBorders: Boolean;
begin
  Result := GeneralSettings.AvoidDoubleBorders;
end;

function TdxTableProperties.GetUseValue: Integer;
begin
  Result := Info.Value;
end;

function TdxTableProperties.GetBackgroundColor: TdxAlphaColor;
begin
  Result := GeneralSettings.BackgroundColor;
end;

function TdxTableProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [];
end;

procedure TdxTableProperties.Reset;
begin
  DocumentModel.History.BeginTransaction;
  try
    CopyFrom((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableProperties);
    ResetAllUse;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxTableProperties.ResetAllUse;
begin
  ReplaceInfo(GetCache(DocumentModel)[TdxTablePropertiesOptionsCache.EmptyTableFormattingOptionsItem], BatchUpdateChangeActions);
end;

function TdxTableProperties.GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTablePropertiesOptions>;
begin
  case AChangedProperty of
    TdxProperties.LeftBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseLeftBorder, TdxTablePropertiesOptions.SetOptionsUseLeftBorder);
    TdxProperties.RightBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseRightBorder, TdxTablePropertiesOptions.SetOptionsUseRightBorder);
    TdxProperties.TopBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTopBorder, TdxTablePropertiesOptions.SetOptionsUseTopBorder);
    TdxProperties.BottomBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseBottomBorder, TdxTablePropertiesOptions.SetOptionsUseBottomBorder);
    TdxProperties.InsideHorizontalBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseInsideHorizontalBorder, TdxTablePropertiesOptions.SetOptionsUseInsideHorizontalBorder);
    TdxProperties.InsideVerticalBorder:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseInsideVerticalBorder, TdxTablePropertiesOptions.SetOptionsUseInsideVerticalBorder);
    TdxProperties.LeftMargin:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseLeftMargin, TdxTablePropertiesOptions.SetOptionsUseLeftMargin);
    TdxProperties.RightMargin:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseRightMargin, TdxTablePropertiesOptions.SetOptionsUseRightMargin);
    TdxProperties.TopMargin:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTopMargin, TdxTablePropertiesOptions.SetOptionsUseTopMargin);
    TdxProperties.BottomMargin:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseBottomMargin, TdxTablePropertiesOptions.SetOptionsUseBottomMargin);
    TdxProperties.CellSpacing:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseCellSpacing, TdxTablePropertiesOptions.SetOptionsUseCellSpacing);
    TdxProperties.PreferredWidth:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUsePreferredWidth, TdxTablePropertiesOptions.SetOptionsUsePreferredWidth);
    TdxProperties.TableIndent:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableIndent, TdxTablePropertiesOptions.SetOptionsUseTableIndent);
    TdxProperties.TableLayout:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableLayout, TdxTablePropertiesOptions.SetOptionsUseTableLayout);
    TdxProperties.TableAlignment:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableAlignment, TdxTablePropertiesOptions.SetOptionsUseTableAlignment);
    TdxProperties.TableLook:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableLook, TdxTablePropertiesOptions.SetOptionsUseTableLook);
    TdxProperties.IsTableOverlap:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableOverlap, TdxTablePropertiesOptions.SetOptionsUseTableOverlap);
    TdxProperties.TableStyleColumnBandSize:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableStyleColBandSize, TdxTablePropertiesOptions.SetOptionsUseTableStyleColBandSize);
    TdxProperties.TableStyleRowBandSize:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseTableStyleRowBandSize, TdxTablePropertiesOptions.SetOptionsUseTableStyleRowBandSize);
    TdxProperties.TableFloatingPosition:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseFloatingPosition, TdxTablePropertiesOptions.SetOptionsUseFloatingPosition);
    TdxProperties.BackgroundColor:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseBackgroundColor, TdxTablePropertiesOptions.SetOptionsUseBackgroundColor);
    TdxProperties.AvoidDoubleBorders:
      Result := TdxBoolPropertyAccessor<TdxTablePropertiesOptions>.Create(TdxTablePropertiesOptions.GetOptionsUseAvoidDoubleBorders, TdxTablePropertiesOptions.SetOptionsUseAvoidDoubleBorders);
  else
    Result := inherited GetAccessor(AChangedProperty);
  end;
end;

function TdxTableProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTablePropertiesOptions>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TablePropertiesOptionsCache;
end;

procedure TdxTableProperties.ResetUse(AMask: Integer);
var
  ANewOptions: TdxTablePropertiesOptions;
begin
  ANewOptions := TdxTablePropertiesOptions.Create(Info.Value and not AMask);
  try
    ReplaceInfo(ANewOptions, BatchUpdateChangeActions);
  finally
    ANewOptions.Free;
  end;
end;

function TdxTableProperties.GetIsTableOverlap: Boolean;
begin
  Result := GeneralSettings.IsTableOverlap;
end;

function TdxTableProperties.GetTableAlignment: TdxTableRowAlignment;
begin
  Result := GeneralSettings.TableAlignment;
end;

function TdxTableProperties.GetTableLayout: TdxTableLayoutType;
begin
  Result := GeneralSettings.TableLayout;
end;

function TdxTableProperties.GetTableLook: TdxTableLookTypes;
begin
  Result := GeneralSettings.TableLook;
end;

function TdxTableProperties.GetTableStyleColBandSize: Integer;
begin
  Result := GeneralSettings.TableStyleColumnBandSize;
end;

function TdxTableProperties.GetTableStyleRowBandSize: Integer;
begin
  Result := GeneralSettings.TableStyleRowBandSize;
end;

function TdxTableProperties.GetUse(AMask: Integer): Boolean;
begin
  Result := Info.GetValue(AMask);
end;

function TdxTableProperties.GetUseAvoidDoubleBorders: Boolean;
begin
  Result := Info.UseAvoidDoubleBorders;
end;

function TdxTableProperties.GetUseBackgroundColor: Boolean;
begin
  Result := Info.UseBackgroundColor;
end;

function TdxTableProperties.GetUseBottomMargin: Boolean;
begin
  Result := Info.UseBottomMargin;
end;

function TdxTableProperties.GetUseCellSpacing: Boolean;
begin
  Result := Info.UseCellSpacing;
end;

function TdxTableProperties.GetUseFloatingPosition: Boolean;
begin
  Result := Info.UseFloatingPosition;
end;

function TdxTableProperties.GetUseIsTableOverlap: Boolean;
begin
  Result := Info.UseIsTableOverlap;
end;

function TdxTableProperties.GetUseLeftMargin: Boolean;
begin
  Result := Info.UseLeftMargin;
end;

function TdxTableProperties.GetUsePreferredWidth: Boolean;
begin
  Result := Info.UsePreferredWidth;
end;

function TdxTableProperties.GetUseRightMargin: Boolean;
begin
  Result := Info.UseRightMargin;
end;

function TdxTableProperties.GetUseTableAlignment: Boolean;
begin
  Result := Info.UseTableAlignment;
end;

function TdxTableProperties.GetUseTableIndent: Boolean;
begin
  Result := Info.UseTableIndent;
end;

function TdxTableProperties.GetUseTableLayout: Boolean;
begin
  Result := Info.UseTableLayout;
end;

function TdxTableProperties.GetUseTableLook: Boolean;
begin
  Result := Info.UseTableLook;
end;

function TdxTableProperties.GetUseTableStyleColBandSize: Boolean;
begin
  Result := Info.UseTableStyleColBandSize;
end;

function TdxTableProperties.GetUseTableStyleRowBandSize: Boolean;
begin
  Result := Info.UseTableStyleRowBandSize;
end;

function TdxTableProperties.GetUseTopMargin: Boolean;
begin
  Result := Info.UseTopMargin;
end;

procedure TdxTableProperties.SetAvoidDoubleBorders(const Value: Boolean);
begin
  GeneralSettings.AvoidDoubleBorders := Value;
end;

procedure TdxTableProperties.SetUseValue(const Value: Integer);
begin
  Info.Value := Value;
end;

procedure TdxTableProperties.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  GeneralSettings.BackgroundColor := Value;
end;

procedure TdxTableProperties.SetIsTableOverlap(const Value: Boolean);
begin
  GeneralSettings.IsTableOverlap := Value;
end;

procedure TdxTableProperties.SetTableAlignment(const Value: TdxTableRowAlignment);
begin
  GeneralSettings.TableAlignment := Value;
end;

procedure TdxTableProperties.InnerSetTableLayout(const Value: TdxTableLayoutType);
begin
  GeneralSettings.TableLayout := Value;
end;

procedure TdxTableProperties.Merge(AProperties: TdxTableProperties);
var
  AContainer: IdxPropertiesContainer;
begin
  AContainer := Self as IdxPropertiesContainer;
  AContainer.BeginPropertiesUpdate;
  try
    Borders.Merge(AProperties.Borders);
    CellMargins.Merge(AProperties.CellMargins);
    if not UseCellSpacing and AProperties.UseCellSpacing then
      CellSpacing.CopyFrom(AProperties.CellSpacing);
    if not UseIsTableOverlap and AProperties.UseIsTableOverlap then
      IsTableOverlap := AProperties.IsTableOverlap;
    if not UsePreferredWidth and AProperties.UsePreferredWidth then
      PreferredWidth.CopyFrom(AProperties.PreferredWidth);
    if not UseTableIndent and AProperties.UseTableIndent then
      TableIndent.CopyFrom(AProperties.TableIndent);
    if not UseTableLayout and AProperties.UseTableLayout then
      TableLayout := AProperties.TableLayout;
    if not UseTableLook and AProperties.UseTableLook then
      TableLook := AProperties.TableLook;
    if not UseTableStyleColBandSize and AProperties.UseTableStyleColBandSize then
      TableStyleColBandSize := AProperties.TableStyleColBandSize;
    if not UseTableStyleRowBandSize and AProperties.UseTableStyleRowBandSize then
      TableStyleRowBandSize := AProperties.TableStyleRowBandSize;
    if not UseFloatingPosition and AProperties.UseFloatingPosition then
      FloatingPosition.CopyFrom(AProperties.FloatingPosition);
    if not UseBackgroundColor and AProperties.UseBackgroundColor then
      BackgroundColor := AProperties.BackgroundColor;
    if not UseTableAlignment and AProperties.UseTableAlignment then
      TableAlignment := AProperties.TableAlignment;
  finally
    AContainer.EndPropertiesUpdate;
  end;
end;

procedure TdxTableProperties.SetTableLook(const Value: TdxTableLookTypes);
begin
  GeneralSettings.TableLook := Value;
end;

procedure TdxTableProperties.SetTableStyleColBandSize(const Value: Integer);
begin
  GeneralSettings.TableStyleColumnBandSize := Value;
end;

procedure TdxTableProperties.SetTableStyleRowBandSize(const Value: Integer);
begin
  GeneralSettings.TableStyleRowBandSize := Value;
end;

{ TdxTablePropertiesOptionsCache }

procedure TdxTablePropertiesOptionsCache.AddRootStyleOptions;
begin
  AppendItem(TdxTablePropertiesOptions.Create(TdxTablePropertiesOptions.MaskUseAll));
end;

function TdxTablePropertiesOptionsCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTablePropertiesOptions;
begin
  Result := TdxTablePropertiesOptions.Create;
end;

procedure TdxTablePropertiesOptionsCache.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
begin
  inherited InitItems(AUnitConverter);
  AddRootStyleOptions;
end;

{ TdxBordersBase }

procedure TdxBordersBase.CopyFrom(ABorders: TdxBordersBase);
begin
  Owner.BeginPropertiesUpdate;
  try
    CopyFromCore(ABorders);
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

procedure TdxBordersBase.CopyFromCore(const ABorders: TdxBordersBase);
begin
  if TopBorder.DocumentModel = ABorders.TopBorder.DocumentModel then
  begin
    if ABorders.UseTopBorder then
      TopBorder.CopyFrom(ABorders.TopBorder);

    if ABorders.UseLeftBorder then
      LeftBorder.CopyFrom(ABorders.LeftBorder);
    if ABorders.UseRightBorder then
      RightBorder.CopyFrom(ABorders.RightBorder);
    if ABorders.UseBottomBorder then
      BottomBorder.CopyFrom(ABorders.BottomBorder);
    if ABorders.UseInsideHorizontalBorder then
      InsideHorizontalBorder.CopyFrom(ABorders.InsideHorizontalBorder);
    if ABorders.UseInsideVerticalBorder then
      InsideVerticalBorder.CopyFrom(ABorders.InsideVerticalBorder);
  end
  else
  begin
    if ABorders.UseTopBorder then
      TopBorder.CopyFrom(ABorders.TopBorder.Info);
    if ABorders.UseLeftBorder then
      LeftBorder.CopyFrom(ABorders.LeftBorder.Info);
    if ABorders.UseRightBorder then
      RightBorder.CopyFrom(ABorders.RightBorder.Info);
    if ABorders.UseBottomBorder then
      BottomBorder.CopyFrom(ABorders.BottomBorder.Info);
    if ABorders.UseInsideHorizontalBorder then
      InsideHorizontalBorder.CopyFrom(ABorders.InsideHorizontalBorder.Info);
    if ABorders.UseInsideVerticalBorder then
      InsideVerticalBorder.CopyFrom(ABorders.InsideVerticalBorder.Info);
  end;
end;

constructor TdxBordersBase.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainerWithMask);
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FTopBorder := TdxTopBorder.Create(APieceTable, AOwner);
  FLeftBorder := TdxLeftBorder.Create(APieceTable, AOwner);
  FRightBorder := TdxRightBorder.Create(APieceTable, AOwner);
  FBottomBorder := TdxBottomBorder.Create(APieceTable, AOwner);
  FInsideHorizontalBorder := TdxInsideHorizontalBorder.Create(APieceTable, AOwner);
  FInsideVerticalBorder := TdxInsideVerticalBorder.Create(APieceTable, AOwner);
end;

destructor TdxBordersBase.Destroy;
begin
  FInsideVerticalBorder.Free;
  FInsideHorizontalBorder.Free;
  FBottomBorder.Free;
  FRightBorder.Free;
  FLeftBorder.Free;
  FTopBorder.Free;
  inherited Destroy;
end;

function TdxBordersBase.GetBottomBorder: TdxBorderBase;
begin
  Result := FBottomBorder;
end;

function TdxBordersBase.GetLeftBorder: TdxBorderBase;
begin
  Result := FLeftBorder;
end;

function TdxBordersBase.GetRightBorder: TdxBorderBase;
begin
  Result := FRightBorder;
end;

function TdxBordersBase.GetTopBorder: TdxBorderBase;
begin
  Result := FTopBorder;
end;

function TdxBordersBase.GetUseBottomBorder: Boolean;
begin
  Result := FOwner.GetUse(UseBottomBorderMask);
end;

function TdxBordersBase.GetUseInsideHorizontalBorder: Boolean;
begin
  Result := FOwner.GetUse(UseInsideHorizontalBorderMask);
end;

function TdxBordersBase.GetUseInsideVerticalBorder: Boolean;
begin
  Result := FOwner.GetUse(UseInsideVerticalBorderMask);
end;

function TdxBordersBase.GetUseLeftBorder: Boolean;
begin
  Result := FOwner.GetUse(UseLeftBorderMask);
end;

function TdxBordersBase.GetUseRightBorder: Boolean;
begin
  Result := FOwner.GetUse(UseRightBorderMask);
end;

function TdxBordersBase.GetUseTopBorder: Boolean;
begin
  Result := FOwner.GetUse(UseTopBorderMask);
end;

procedure TdxBordersBase.Merge(ABorders: TdxBordersBase);
begin
  if not UseLeftBorder and ABorders.UseLeftBorder then
    LeftBorder.CopyFrom(ABorders.LeftBorder);
  if not UseRightBorder and ABorders.UseRightBorder then
    RightBorder.CopyFrom(ABorders.RightBorder);
  if not UseTopBorder and ABorders.UseTopBorder then
    TopBorder.CopyFrom(ABorders.TopBorder);
  if not UseBottomBorder and ABorders.UseBottomBorder then
    BottomBorder.CopyFrom(ABorders.BottomBorder);
  if not UseInsideHorizontalBorder and ABorders.UseInsideHorizontalBorder then
    InsideHorizontalBorder.CopyFrom(ABorders.InsideHorizontalBorder);
  if not UseInsideVerticalBorder and ABorders.UseInsideVerticalBorder then
    InsideVerticalBorder.CopyFrom(ABorders.InsideVerticalBorder);
end;

{ TdxTableCellBorders }

procedure TdxTableCellBorders.CopyFromCore(const ABorders: TdxBordersBase);
var
  ACellBorders: TdxTableCellBorders;
begin
  inherited CopyFromCore(ABorders);

  if not (ABorders is TdxTableCellBorders) then
    Exit;
  ACellBorders := TdxTableCellBorders(ABorders);
  if LeftBorder.DocumentModel = ABorders.LeftBorder.DocumentModel then
  begin
    if ACellBorders.UseTopLeftDiagonalBorder then
      TopLeftDiagonalBorder.CopyFrom(ACellBorders.TopLeftDiagonalBorder);
    if ACellBorders.UseTopRightDiagonalBorder then
      TopRightDiagonalBorder.CopyFrom(ACellBorders.TopRightDiagonalBorder);
  end
  else
  begin
    if ACellBorders.UseTopLeftDiagonalBorder then
      TopLeftDiagonalBorder.CopyFrom(ACellBorders.TopLeftDiagonalBorder.Info);
    if ACellBorders.UseTopRightDiagonalBorder then
      TopRightDiagonalBorder.CopyFrom(ACellBorders.TopRightDiagonalBorder.Info);
  end;
end;

constructor TdxTableCellBorders.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellPropertiesContainer);
begin
  inherited Create(APieceTable, AOwner);
  FTopLeftDiagonalBorder := TdxTopLeftDiagonalBorder.Create(APieceTable, AOwner);
  FTopRightDiagonalBorder := TdxTopRightDiagonalBorder.Create(APieceTable, AOwner);
end;

destructor TdxTableCellBorders.Destroy;
begin
  FTopRightDiagonalBorder.Free;
  FTopLeftDiagonalBorder.Free;
  inherited Destroy;
end;

function TdxTableCellBorders.GetUseTopLeftDiagonalBorder: Boolean;
begin
  Result := Owner.GetUse(TdxTableCellPropertiesOptions.MaskUseTopLeftDiagonalBorder);
end;

function TdxTableCellBorders.GetUseTopRightDiagonalBorder: Boolean;
begin
  Result := Owner.GetUse(TdxTableCellPropertiesOptions.MaskUseTopRightDiagonalBorder);
end;

procedure TdxTableCellBorders.Merge(ABorders: TdxBordersBase);
var
  ACellBorders: TdxTableCellBorders;
begin
  inherited Merge(ABorders);
  if not (ABorders is TdxTableCellBorders) then
    Exit;
  ACellBorders := TdxTableCellBorders(ABorders);
  if not UseTopLeftDiagonalBorder and ACellBorders.UseTopLeftDiagonalBorder then
    TopLeftDiagonalBorder.CopyFrom(ACellBorders.TopLeftDiagonalBorder);
  if not UseTopRightDiagonalBorder and ACellBorders.UseTopRightDiagonalBorder then
    TopRightDiagonalBorder.CopyFrom(ACellBorders.TopRightDiagonalBorder);
end;

function TdxTableCellBorders.UseBottomBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseBottomBorder;
end;

function TdxTableCellBorders.UseInsideHorizontalBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseInsideHorizontalBorder;
end;

function TdxTableCellBorders.UseInsideVerticalBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseInsideVerticalBorder;
end;

function TdxTableCellBorders.UseLeftBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseLeftBorder;
end;

function TdxTableCellBorders.UseRightBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseRightBorder;
end;

function TdxTableCellBorders.UseTopBorderMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseTopBorder;
end;

{ TdxTableBorders }

function TdxTableBorders.AreSame(AOther: TdxTableBorders): Boolean;
begin
  Result := (LeftBorder.Index = AOther.LeftBorder.Index) and
    (RightBorder.Index = AOther.RightBorder.Index) and
    (TopBorder.Index = AOther.TopBorder.Index) and
    (BottomBorder.Index = AOther.BottomBorder.Index) and
    (InsideHorizontalBorder.Index = AOther.InsideHorizontalBorder.Index) and
    (InsideVerticalBorder.Index = AOther.InsideVerticalBorder.Index);
end;

function TdxTableBorders.UseBottomBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseBottomBorder;
end;

function TdxTableBorders.UseInsideHorizontalBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder;
end;

function TdxTableBorders.UseInsideVerticalBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideVerticalBorder;
end;

function TdxTableBorders.UseLeftBorderMask: Integer;
begin
 Result := TdxTablePropertiesOptions.MaskUseLeftBorder;
end;

function TdxTableBorders.UseRightBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseRightBorder;
end;

function TdxTableBorders.UseTopBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseTopBorder;
end;

{ TdxTableCellPropertiesOptions }

constructor TdxTableCellPropertiesOptions.Create(AValue: Integer = 0);
begin
  inherited Create;
  FValue := AValue;
end;

function TdxTableCellPropertiesOptions.GetVal(AMask: Integer): Boolean;
begin
  Result := (FValue and AMask) <> 0;
end;

procedure TdxTableCellPropertiesOptions.SetVal(AMask: Integer; ABitVal: Boolean);
begin
  if ABitVal then
    FValue := FValue or AMask
  else
    FValue := FValue and not AMask;
end;

function TdxTableCellPropertiesOptions.Clone: TdxTableCellPropertiesOptions;
begin
  Result := TdxTableCellPropertiesOptions(inherited Clone);
end;

procedure TdxTableCellPropertiesOptions.CopyFrom(Source: TdxCloneable);
var
  AOptions: TdxTableCellPropertiesOptions absolute Source;
begin
  FValue := AOptions.FValue;
end;

function TdxTableCellPropertiesOptions.Equals(AObject: TObject): Boolean;
begin
  Result := AObject is TdxTableCellPropertiesOptions;
  if Result then
    Result := Value = TdxTableCellPropertiesOptions(AObject).Value;
end;

function TdxTableCellPropertiesOptions.GetHashCode: Integer;
begin
  Result := Value;
end;

function TdxTableCellPropertiesOptions.GetUseBackgroundColor: Boolean;
begin
  Result := GetVal(MaskUseBackgroundColor);
end;

function TdxTableCellPropertiesOptions.GetUseBottomBorder: Boolean;
begin
  Result := GetVal(MaskUseBottomBorder);
end;

function TdxTableCellPropertiesOptions.GetUseBottomMargin: Boolean;
begin
  Result := GetVal(MaskUseBottomMargin);
end;

function TdxTableCellPropertiesOptions.GetUseCellConditionalFormatting: Boolean;
begin
  Result := GetVal(MaskUseCellConditionalFormatting);
end;

function TdxTableCellPropertiesOptions.GetUseFitText: Boolean;
begin
  Result := GetVal(MaskUseFitText);
end;

function TdxTableCellPropertiesOptions.GetUseForegroundColor: Boolean;
begin
  Result := GetVal(MaskUseForegroundColor);
end;

function TdxTableCellPropertiesOptions.GetUseHideCellMark: Boolean;
begin
  Result := GetVal(MaskUseHideCellMark);
end;

function TdxTableCellPropertiesOptions.GetUseInsideHorizontalBorder: Boolean;
begin
  Result := GetVal(MaskUseInsideHorizontalBorder);
end;

function TdxTableCellPropertiesOptions.GetUseInsideVerticalBorder: Boolean;
begin
  Result := GetVal(MaskUseInsideVerticalBorder);
end;

function TdxTableCellPropertiesOptions.GetUseLeftBorder: Boolean;
begin
  Result := GetVal(MaskUseLeftBorder);
end;

function TdxTableCellPropertiesOptions.GetUseLeftMargin: Boolean;
begin
  Result := GetVal(MaskUseLeftMargin);
end;

function TdxTableCellPropertiesOptions.GetUseNoWrap: Boolean;
begin
  Result := GetVal(MaskUseNoWrap);
end;

function TdxTableCellPropertiesOptions.GetUsePreferredWidth: Boolean;
begin
  Result := GetVal(MaskUsePreferredWidth);
end;

function TdxTableCellPropertiesOptions.GetUseRightBorder: Boolean;
begin
  Result := GetVal(MaskUseRightBorder);
end;

function TdxTableCellPropertiesOptions.GetUseRightMargin: Boolean;
begin
  Result := GetVal(MaskUseRightMargin);
end;

function TdxTableCellPropertiesOptions.GetUseShading: Boolean;
begin
  Result := GetVal(MaskUseShading);
end;

function TdxTableCellPropertiesOptions.GetUseTextDirection: Boolean;
begin
  Result := GetVal(MaskUseTextDirection);
end;

function TdxTableCellPropertiesOptions.GetUseTopBorder: Boolean;
begin
  Result := GetVal(MaskUseTopBorder);
end;

function TdxTableCellPropertiesOptions.GetUseTopLeftDiagonalBorder: Boolean;
begin
  Result := GetVal(MaskUseTopLeftDiagonalBorder);
end;

function TdxTableCellPropertiesOptions.GetUseTopMargin: Boolean;
begin
  Result := GetVal(MaskUseTopMargin);
end;

function TdxTableCellPropertiesOptions.GetUseTopRightDiagonalBorder: Boolean;
begin
  Result := GetVal(MaskUseTopRightDiagonalBorder);
end;

function TdxTableCellPropertiesOptions.GetUseVerticalAlignment: Boolean;
begin
  Result := GetVal(MaskUseVerticalAlignment);
end;

procedure TdxTableCellPropertiesOptions.SetUseBackgroundColor(const Value: Boolean);
begin
  SetVal(MaskUseBackgroundColor, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseBottomBorder(const Value: Boolean);
begin
  SetVal(MaskUseBottomBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseBottomMargin(const Value: Boolean);
begin
  SetVal(MaskUseBottomMargin, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseCellConditionalFormatting(const Value: Boolean);
begin
  SetVal(MaskUseCellConditionalFormatting, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseFitText(const Value: Boolean);
begin
  SetVal(MaskUseFitText, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseForegroundColor(const Value: Boolean);
begin
  SetVal(MaskUseForegroundColor, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseHideCellMark(const Value: Boolean);
begin
  SetVal(MaskUseHideCellMark, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseInsideHorizontalBorder(const Value: Boolean);
begin
  SetVal(MaskUseInsideHorizontalBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseInsideVerticalBorder(const Value: Boolean);
begin
  SetVal(MaskUseInsideVerticalBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseLeftBorder(const Value: Boolean);
begin
  SetVal(MaskUseLeftBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseLeftMargin(const Value: Boolean);
begin
  SetVal(MaskUseLeftMargin, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseNoWrap(const Value: Boolean);
begin
  SetVal(MaskUseNoWrap, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUsePreferredWidth(const Value: Boolean);
begin
  SetVal(MaskUsePreferredWidth, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseRightBorder(const Value: Boolean);
begin
  SetVal(MaskUseRightBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseRightMargin(const Value: Boolean);
begin
  SetVal(MaskUseRightMargin, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseShading(const Value: Boolean);
begin
  SetVal(MaskUseShading, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseTextDirection(const Value: Boolean);
begin
  SetVal(MaskUseTextDirection, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseTopBorder(const Value: Boolean);
begin
  SetVal(MaskUseTopBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseTopLeftDiagonalBorder(const Value: Boolean);
begin
  SetVal(MaskUseTopLeftDiagonalBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseTopMargin(const Value: Boolean);
begin
  SetVal(MaskUseTopMargin, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseTopRightDiagonalBorder(const Value: Boolean);
begin
  SetVal(MaskUseTopRightDiagonalBorder, Value);
end;

procedure TdxTableCellPropertiesOptions.SetUseVerticalAlignment(const Value: Boolean);
begin
  SetVal(MaskUseVerticalAlignment, Value);
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseBackgroundColor(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseBackgroundColor;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseBackgroundColor(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseBackgroundColor := AValue;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseForegroundColor(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseForegroundColor;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseForegroundColor(
  AOptions: TdxTableCellPropertiesOptions; const Value: Boolean);
begin
  AOptions.UseForegroundColor := Value;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseShading(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseShading;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseShading(
  AOptions: TdxTableCellPropertiesOptions; const Value: Boolean);
begin
  AOptions.UseShading := Value;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseBottomBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseBottomBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseBottomMargin(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseBottomMargin;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseCellConditionalFormatting(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseCellConditionalFormatting;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseColumnSpan(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := True;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseFitText(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseFitText;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseHideCellMark(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseHideCellMark;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseInsideHorizontalBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseInsideHorizontalBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseInsideVerticalBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseInsideVerticalBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseLeftBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseLeftBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseLeftMargin(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseLeftMargin;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseNoWrap(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseNoWrap;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUsePreferredWidth(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UsePreferredWidth;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseRightBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseRightBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseRightMargin(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseRightMargin;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseTextDirection(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTextDirection;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseTopBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseTopLeftDiagonalBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopLeftDiagonalBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseTopMargin(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopMargin;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseTopRightDiagonalBorder(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTopRightDiagonalBorder;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseVerticalAlignment(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := AOptions.UseVerticalAlignment;
end;

class function TdxTableCellPropertiesOptions.GetOptionsUseVerticalMerging(
  AOptions: TdxTableCellPropertiesOptions): Boolean;
begin
  Result := True;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseBottomBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseBottomBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseBottomMargin(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseBottomMargin := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseCellConditionalFormatting(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseCellConditionalFormatting := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseColumnSpan(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseFitText(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseFitText := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseHideCellMark(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseHideCellMark := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseInsideHorizontalBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseInsideHorizontalBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseInsideVerticalBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseInsideVerticalBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseLeftBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseLeftBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseLeftMargin(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseLeftMargin := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseNoWrap(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseNoWrap := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUsePreferredWidth(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UsePreferredWidth := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseRightBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseRightBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseRightMargin(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseRightMargin := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseTextDirection(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTextDirection := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseTopBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTopBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseTopLeftDiagonalBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTopLeftDiagonalBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseTopMargin(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTopMargin := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseTopRightDiagonalBorder(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTopRightDiagonalBorder := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseVerticalAlignment(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseVerticalAlignment := AValue;
end;

class procedure TdxTableCellPropertiesOptions.SetOptionsUseVerticalMerging(
  AOptions: TdxTableCellPropertiesOptions; const AValue: Boolean);
begin
//do nothing
end;

{ TdxTableCellProperties }

constructor TdxTableCellProperties.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxCellPropertiesOwner);
begin
  inherited Create(APieceTable);
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FPreferredWidth := TdxPreferredWidth.Create(APieceTable, Self);
  FCellMargins := TdxCellMargins.Create(APieceTable, Self);
  FBorders := TdxTableCellBorders.Create(APieceTable, Self);
  FGeneralSettings := TdxTableCellGeneralSettings.Create(APieceTable, Self);
end;

destructor TdxTableCellProperties.Destroy;
begin
  FreeAndNil(FGeneralSettings);
  FreeAndNil(FBorders);
  FreeAndNil(FCellMargins);
  FreeAndNil(FPreferredWidth);
  inherited Destroy;
end;

procedure TdxTableCellProperties.CopyFrom(AProperties: TdxTableCellProperties);
var
  AIsDeferredInfo: Boolean;
  AInfo: TdxTableCellPropertiesOptions;
  AContainer: IdxPropertiesContainer;
begin
  AContainer := Self as IdxPropertiesContainer;
  DocumentModel.BeginUpdate;
  AContainer.BeginPropertiesUpdate;
  try
    if DocumentModel = AProperties.DocumentModel then
    begin
      GeneralSettings.CopyFrom(AProperties.GeneralSettings);
      Borders.CopyFrom(AProperties.Borders);
      CellMargins.CopyFrom(AProperties.CellMargins);
      PreferredWidth.CopyFrom(AProperties.PreferredWidth);
      AInfo := GetInfoForModification(AIsDeferredInfo);
      AInfo.CopyFrom(AProperties.Info);
      ReplaceInfo(AInfo, BatchUpdateChangeActions);
      if not AIsDeferredInfo then AInfo.Free;
    end
    else
    begin
      GeneralSettings.CopyFrom(AProperties.GeneralSettings.Info);
      Borders.CopyFrom(AProperties.Borders);
      CellMargins.CopyFrom(AProperties.CellMargins);
      PreferredWidth.CopyFrom(AProperties.PreferredWidth.Info);
      Info.CopyFrom(AProperties.Info);
    end;
  finally
    AContainer.EndPropertiesUpdate;
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxTableCellProperties.CopyFrom(AProperties: TdxMergedTableCellProperties);
var
  AContainer: IdxPropertiesContainer;
begin
  Supports(Self, IdxPropertiesContainer, AContainer);
  AContainer.BeginPropertiesUpdate;
  try
    GeneralSettings.CopyFrom(AProperties.Info.GeneralSettings);
    Borders.CopyFrom(AProperties.Info.Borders);
    CellMargins.CopyFrom(AProperties.Info.CellMargins);
    PreferredWidth.CopyFrom(AProperties.Info.PreferredWidth);
    Info.CopyFrom(AProperties.Options);
  finally
    AContainer.EndPropertiesUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableCellProperties.CopyFrom(const Source: TdxTableCellPropertiesOptions);
begin
  inherited CopyFrom(Source);
end;

procedure TdxTableCellProperties.CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableCellPropertiesOptions>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

function TdxTableCellProperties.GetBackgroundColor: TdxAlphaColor;
begin
  Result := GeneralSettings.BackgroundColor;
end;

function TdxTableCellProperties.GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>;
begin
  case AChangedProperty of
    TdxProperties.CellConditionalFormatting:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseCellConditionalFormatting, TdxTableCellPropertiesOptions.SetOptionsUseCellConditionalFormatting);
    TdxProperties.PreferredWidth:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUsePreferredWidth, TdxTableCellPropertiesOptions.SetOptionsUsePreferredWidth);
    TdxProperties.HideCellMark:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseHideCellMark, TdxTableCellPropertiesOptions.SetOptionsUseHideCellMark);
    TdxProperties.NoWrap:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseNoWrap, TdxTableCellPropertiesOptions.SetOptionsUseNoWrap);
    TdxProperties.FitText:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseFitText, TdxTableCellPropertiesOptions.SetOptionsUseFitText);
    TdxProperties.TopMargin:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseTopMargin, TdxTableCellPropertiesOptions.SetOptionsUseTopMargin);
    TdxProperties.BottomMargin:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseBottomMargin, TdxTableCellPropertiesOptions.SetOptionsUseBottomMargin);
    TdxProperties.LeftMargin:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseLeftMargin, TdxTableCellPropertiesOptions.SetOptionsUseLeftMargin);
    TdxProperties.RightMargin:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseRightMargin, TdxTableCellPropertiesOptions.SetOptionsUseRightMargin);
    TdxProperties.TextDirection:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseTextDirection, TdxTableCellPropertiesOptions.SetOptionsUseTextDirection);
    TdxProperties.VerticalAlignment:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseVerticalAlignment, TdxTableCellPropertiesOptions.SetOptionsUseVerticalAlignment);
    TdxProperties.VerticalMerging:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseVerticalMerging, TdxTableCellPropertiesOptions.SetOptionsUseVerticalMerging);
    TdxProperties.ColumnSpan:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseColumnSpan, TdxTableCellPropertiesOptions.SetOptionsUseColumnSpan);
    TdxProperties.BackgroundColor:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseBackgroundColor, TdxTableCellPropertiesOptions.SetOptionsUseBackgroundColor);
    TdxProperties.ForegroundColor:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseForegroundColor, TdxTableCellPropertiesOptions.SetOptionsUseForegroundColor);
    TdxProperties.Shading:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseShading, TdxTableCellPropertiesOptions.SetOptionsUseShading);
    TdxProperties.LeftBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseLeftBorder, TdxTableCellPropertiesOptions.SetOptionsUseLeftBorder);
    TdxProperties.RightBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseRightBorder, TdxTableCellPropertiesOptions.SetOptionsUseRightBorder);
    TdxProperties.TopBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseTopBorder, TdxTableCellPropertiesOptions.SetOptionsUseTopBorder);
    TdxProperties.BottomBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseBottomBorder, TdxTableCellPropertiesOptions.SetOptionsUseBottomBorder);
    TdxProperties.InsideHorizontalBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseInsideHorizontalBorder, TdxTableCellPropertiesOptions.SetOptionsUseInsideHorizontalBorder);
    TdxProperties.InsideVerticalBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseInsideVerticalBorder, TdxTableCellPropertiesOptions.SetOptionsUseInsideVerticalBorder);
    TdxProperties.TopLeftDiagonalBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseTopLeftDiagonalBorder, TdxTableCellPropertiesOptions.SetOptionsUseTopLeftDiagonalBorder);
    TdxProperties.TopRightDiagonalBorder:
      Result := TdxBoolPropertyAccessor<TdxTableCellPropertiesOptions>.Create(TdxTableCellPropertiesOptions.GetOptionsUseTopRightDiagonalBorder, TdxTableCellPropertiesOptions.SetOptionsUseTopRightDiagonalBorder);
  else
    Result := inherited GetAccessor(AChangedProperty);
  end;
end;

function TdxTableCellProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableCellPropertiesOptions>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableCellPropertiesOptionsCache;
end;

function TdxTableCellProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [];
end;

function TdxTableCellProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateCellPropertiesChangedHistoryItem(Self);
end;

function TdxTableCellProperties.GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
begin
  Result := GeneralSettings.CellConditionalFormatting;
end;

function TdxTableCellProperties.GetColumnSpan: Integer;
begin
  Result := GeneralSettings.ColumnSpan;
end;

function TdxTableCellProperties.GetFitText: Boolean;
begin
  Result := GeneralSettings.FitText;
end;

function TdxTableCellProperties.GetForegroundColor: TdxAlphaColor;
begin
  Result := GeneralSettings.ForegroundColor;
end;

function TdxTableCellProperties.GetHideCellMark: Boolean;
begin
  Result := GeneralSettings.HideCellMark;
end;

function TdxTableCellProperties.GetNoWrap: Boolean;
begin
  Result := GeneralSettings.NoWrap;
end;

function TdxTableCellProperties.GetShading: TdxShadingPattern;
begin
  Result := GeneralSettings.Shading;
end;

function TdxTableCellProperties.GetUseValue: Integer;
begin
  Result := Info.Value;
end;

function TdxTableCellProperties.GetTextDirection: TdxTextDirection;
begin
  Result := GeneralSettings.TextDirection;
end;

function TdxTableCellProperties.GetUse(AMask: Integer): Boolean;
begin
  Result := Info.GetVal(AMask);
end;

function TdxTableCellProperties.GetUseBackgroundColor: Boolean;
begin
  Result := Info.UseBackgroundColor;
end;

function TdxTableCellProperties.GetUseBottomMargin: Boolean;
begin
  Result := Info.UseBottomMargin;
end;

function TdxTableCellProperties.GetUseCellConditionalFormatting: Boolean;
begin
  Result := Info.UseCellConditionalFormatting;
end;

function TdxTableCellProperties.GetUseFitText: Boolean;
begin
  Result := Info.UseFitText;
end;

function TdxTableCellProperties.GetUseForegroundColor: Boolean;
begin
  Result := Info.UseForegroundColor;
end;

function TdxTableCellProperties.GetUseHideCellMark: Boolean;
begin
  Result := Info.UseHideCellMark;
end;

function TdxTableCellProperties.GetUseLeftMargin: Boolean;
begin
  Result := Info.UseLeftMargin;
end;

function TdxTableCellProperties.GetUseNoWrap: Boolean;
begin
  Result := Info.UseNoWrap;
end;

function TdxTableCellProperties.GetUsePreferredWidth: Boolean;
begin
  Result := Info.UsePreferredWidth;
end;

function TdxTableCellProperties.GetUseRightMargin: Boolean;
begin
  Result := Info.UseRightMargin;
end;

function TdxTableCellProperties.GetUseShading: Boolean;
begin
  Result := Info.UseShading;
end;

function TdxTableCellProperties.GetUseTextDirection: Boolean;
begin
  Result := Info.UseTextDirection;
end;

function TdxTableCellProperties.GetUseTopMargin: Boolean;
begin
  Result := Info.UseTopMargin;
end;

function TdxTableCellProperties.GetUseVerticalAlignment: Boolean;
begin
  Result := Info.UseVerticalAlignment;
end;

function TdxTableCellProperties.GetVerticalAlignment: TdxVerticalAlignment;
begin
  Result := GeneralSettings.VerticalAlignment;
end;

function TdxTableCellProperties.GetVerticalMerging: TdxMergingState;
begin
  Result := GeneralSettings.VerticalMerging;
end;

procedure TdxTableCellProperties.Merge(AProperties: TdxTableCellProperties);
var
  AContainer: IdxPropertiesContainer;
begin
  Supports(Self, IdxPropertiesContainer, AContainer);
  AContainer.BeginPropertiesUpdate;
  try
    Borders.Merge(AProperties.Borders);
    if not UseCellConditionalFormatting and AProperties.UseCellConditionalFormatting then
      CellConditionalFormatting := AProperties.CellConditionalFormatting;
    CellMargins.Merge(AProperties.CellMargins);
    if not UseBackgroundColor and AProperties.UseBackgroundColor then
      BackgroundColor := AProperties.BackgroundColor;
    if not UseForegroundColor and AProperties.UseForegroundColor then
      ForegroundColor := AProperties.ForegroundColor;
    if not UseFitText and AProperties.UseFitText then
      FitText := AProperties.FitText;
    if not UseHideCellMark and AProperties.UseHideCellMark then
      HideCellMark := AProperties.HideCellMark;
    if not UseNoWrap and AProperties.UseNoWrap then
      NoWrap := AProperties.NoWrap;
    if not UsePreferredWidth and AProperties.UsePreferredWidth then
      PreferredWidth.CopyFrom(AProperties.PreferredWidth);
    if not UseTextDirection and AProperties.UseTextDirection then
      TextDirection := AProperties.TextDirection;
    if not UseVerticalAlignment and AProperties.UseVerticalAlignment then
      VerticalAlignment := AProperties.VerticalAlignment;
  finally
    AContainer.EndPropertiesUpdate;
  end;
end;

procedure TdxTableCellProperties.RaisePropertiesChanged;
begin
  if not FPropertiesChanged.Empty then
    FPropertiesChanged.Invoke(Self, nil);
end;

procedure TdxTableCellProperties.Reset;
begin
  DocumentModel.History.BeginTransaction;
  try
    CopyFrom((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties);
    ResetAllUse;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxTableCellProperties.ResetAllUse;
begin
  ReplaceInfo(GetCache(DocumentModel)[TdxTableCellPropertiesOptionsCache.EmptyCellPropertiesOptionsItem], BatchUpdateChangeActions);
end;

procedure TdxTableCellProperties.ResetUse(AMask: Integer);
var
  ANewOptions: TdxTableCellPropertiesOptions;
begin
  ANewOptions := TdxTableCellPropertiesOptions.Create(Info.Value and not AMask);
  try
    ReplaceInfo(ANewOptions, BatchUpdateChangeActions);
  finally
    ANewOptions.Free;
  end;
end;

procedure TdxTableCellProperties.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  GeneralSettings.BackgroundColor := Value;
end;

procedure TdxTableCellProperties.SetCellConditionalFormatting(const Value: TdxConditionalTableStyleFormattingTypes);
begin
  GeneralSettings.CellConditionalFormatting := Value;
end;

procedure TdxTableCellProperties.SetColumnSpan(const Value: Integer);
begin
  GeneralSettings.ColumnSpan := Value;
end;

procedure TdxTableCellProperties.SetFitText(const Value: Boolean);
begin
  GeneralSettings.FitText := Value;
end;

procedure TdxTableCellProperties.SetForegroundColor(const Value: TdxAlphaColor);
begin
  GeneralSettings.ForegroundColor := Value;
end;

procedure TdxTableCellProperties.SetHideCellMark(const Value: Boolean);
begin
  GeneralSettings.HideCellMark := Value;
end;

procedure TdxTableCellProperties.SetNoWrap(const Value: Boolean);
begin
  GeneralSettings.NoWrap := Value;
end;

procedure TdxTableCellProperties.SetShading(const Value: TdxShadingPattern);
begin
  GeneralSettings.Shading := Value;
end;

procedure TdxTableCellProperties.SetUseValue(const Value: Integer);
begin
  Info.Value := Value;
end;

procedure TdxTableCellProperties.SetTextDirection(const Value: TdxTextDirection);
begin
  GeneralSettings.TextDirection := Value;
end;

procedure TdxTableCellProperties.SetVerticalAlignment(const Value: TdxVerticalAlignment);
begin
  GeneralSettings.VerticalAlignment := Value;
end;

procedure TdxTableCellProperties.SetVerticalMerging(const Value: TdxMergingState);
begin
  GeneralSettings.VerticalMerging := Value;
end;

{ TdxTableCellBorders }

procedure TdxTableCellBorders.CopyFrom(const ABorders: TdxCombinedCellBordersInfo);
begin
  Owner.BeginPropertiesUpdate;
  try
    TopBorder.CopyFrom(ABorders.TopBorder);
    LeftBorder.CopyFrom(ABorders.LeftBorder);
    RightBorder.CopyFrom(ABorders.RightBorder);
    BottomBorder.CopyFrom(ABorders.BottomBorder);
    InsideHorizontalBorder.CopyFrom(ABorders.InsideHorizontalBorder);
    InsideVerticalBorder.CopyFrom(ABorders.InsideVerticalBorder);
    TopLeftDiagonalBorder.CopyFrom(ABorders.TopLeftDiagonalBorder);
    TopRightDiagonalBorder.CopyFrom(ABorders.TopRightDiagonalBorder);
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

{ TdxCombinedCellBordersInfo }

constructor TdxCombinedCellBordersInfo.Create;
begin
  inherited Create;
  FBottomBorder := TdxBorderInfo.Create;
  FLeftBorder := TdxBorderInfo.Create;
  FRightBorder := TdxBorderInfo.Create;
  FTopBorder := TdxBorderInfo.Create;
  FInsideHorizontalBorder := TdxBorderInfo.Create;
  FInsideVerticalBorder := TdxBorderInfo.Create;
  FTopLeftDiagonalBorder := TdxBorderInfo.Create;
  FTopRightDiagonalBorder := TdxBorderInfo.Create;
  FIsOwner := True;
end;

constructor TdxCombinedCellBordersInfo.Create(ATableBorders: TdxTableCellBorders);
begin
  inherited Create;
  FBottomBorder := ATableBorders.BottomBorder.Info;
  FLeftBorder := ATableBorders.LeftBorder.Info;
  FRightBorder := ATableBorders.RightBorder.Info;
  FTopBorder := ATableBorders.TopBorder.Info;
  FInsideHorizontalBorder := ATableBorders.InsideHorizontalBorder.Info;
  FInsideVerticalBorder := ATableBorders.InsideVerticalBorder.Info;
  FTopLeftDiagonalBorder := ATableBorders.TopLeftDiagonalBorder.Info;
  FTopRightDiagonalBorder := ATableBorders.TopRightDiagonalBorder.Info;
  FIsOwner := False;
end;

destructor TdxCombinedCellBordersInfo.Destroy;
begin
  if FIsOwner then
  begin
    FreeAndNil(FBottomBorder);
    FreeAndNil(FLeftBorder);
    FreeAndNil(FRightBorder);
    FreeAndNil(FTopBorder);
    FreeAndNil(FInsideHorizontalBorder);
    FreeAndNil(FInsideVerticalBorder);
    FreeAndNil(FTopLeftDiagonalBorder);
    FreeAndNil(FTopRightDiagonalBorder);
  end;
  inherited Destroy;
end;

function TdxCombinedCellBordersInfo.Clone: TdxCombinedCellBordersInfo;
begin
  Result := TdxCombinedCellBordersInfo(inherited Clone);
end;

procedure TdxCombinedCellBordersInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxCombinedCellBordersInfo absolute Source;
begin
  FBottomBorder.CopyFrom(AInfo.BottomBorder);
  FLeftBorder.CopyFrom(AInfo.LeftBorder);
  FRightBorder.CopyFrom(AInfo.RightBorder);
  FTopBorder.CopyFrom(AInfo.TopBorder);
  FInsideHorizontalBorder.CopyFrom(AInfo.InsideHorizontalBorder);
  FInsideVerticalBorder.CopyFrom(AInfo.InsideVerticalBorder);
  FTopLeftDiagonalBorder.CopyFrom(AInfo.TopLeftDiagonalBorder);
  FTopRightDiagonalBorder.CopyFrom(AInfo.TopRightDiagonalBorder);
end;

{ TdxCombinedTableBordersInfo }

constructor TdxCombinedTableBordersInfo.Create;
begin
  inherited Create;
  FBottomBorder := TdxBorderInfo.Create;
  FLeftBorder := TdxBorderInfo.Create;
  FRightBorder := TdxBorderInfo.Create;
  FTopBorder := TdxBorderInfo.Create;
  FInsideHorizontalBorder := TdxBorderInfo.Create;
  FInsideVerticalBorder := TdxBorderInfo.Create;
end;

constructor TdxCombinedTableBordersInfo.Create(ATableBorders: TdxTableBorders);
begin
  Create;
  FBottomBorder.CopyFrom(ATableBorders.BottomBorder.Info);
  FLeftBorder.CopyFrom(ATableBorders.LeftBorder.Info);
  FRightBorder.CopyFrom(ATableBorders.RightBorder.Info);
  FTopBorder.CopyFrom(ATableBorders.TopBorder.Info);
  FInsideHorizontalBorder.CopyFrom(ATableBorders.InsideHorizontalBorder.Info);
  FInsideVerticalBorder.CopyFrom(ATableBorders.InsideVerticalBorder.Info);
end;

destructor TdxCombinedTableBordersInfo.Destroy;
begin
  FreeAndNil(FBottomBorder);
  FreeAndNil(FLeftBorder);
  FreeAndNil(FRightBorder);
  FreeAndNil(FTopBorder);
  FreeAndNil(FInsideHorizontalBorder);
  FreeAndNil(FInsideVerticalBorder);
  inherited Destroy;
end;

function TdxCombinedTableBordersInfo.Clone: TdxCombinedTableBordersInfo;
begin
  Result := TdxCombinedTableBordersInfo(inherited Clone);
end;

procedure TdxCombinedTableBordersInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxCombinedTableBordersInfo absolute Source;
begin
  FBottomBorder.CopyFrom(AInfo.BottomBorder);
  FLeftBorder.CopyFrom(AInfo.LeftBorder);
  FRightBorder.CopyFrom(AInfo.RightBorder);
  FTopBorder.CopyFrom(AInfo.TopBorder);
  FInsideHorizontalBorder.CopyFrom(AInfo.InsideHorizontalBorder);
  FInsideVerticalBorder.CopyFrom(AInfo.InsideVerticalBorder);
end;

{ TdxCombinedTablePropertiesInfo }

constructor TdxCombinedTablePropertiesInfo.Create;
begin
  inherited Create;
  FCellMargins := TdxCombinedCellMarginsInfo.Create;
  FBorders := TdxCombinedTableBordersInfo.Create;
  FCellSpacing := TdxWidthUnitInfo.Create;
  FIndent := TdxWidthUnitInfo.Create;
  FPreferredWidth := TdxWidthUnitInfo.Create;
  FGeneralSettings := TdxTableGeneralSettingsInfo.Create;
  FFloatingPosition := TdxTableFloatingPositionInfo.Create;
end;

constructor TdxCombinedTablePropertiesInfo.Create(ATableProperties: TdxTableProperties);
begin
  inherited Create;
  FCellMargins := TdxCombinedCellMarginsInfo.Create(ATableProperties.CellMargins);
  FBorders := TdxCombinedTableBordersInfo.Create(ATableProperties.Borders);
  FCellSpacing := ATableProperties.CellSpacing.Info.Clone;
  FIndent := ATableProperties.TableIndent.Info.Clone;
  FPreferredWidth := ATableProperties.PreferredWidth.Info.Clone;
  FGeneralSettings := ATableProperties.GeneralSettings.Info.Clone;
  FFloatingPosition := ATableProperties.FloatingPosition.Info.Clone;
end;

destructor TdxCombinedTablePropertiesInfo.Destroy;
begin
  FreeAndNil(FCellMargins);
  FreeAndNil(FBorders);
  FreeAndNil(FCellSpacing);
  FreeAndNil(FIndent);
  FreeAndNil(FPreferredWidth);
  FreeAndNil(FGeneralSettings);
  FreeAndNil(FFloatingPosition);
  inherited Destroy;
end;

function TdxCombinedTablePropertiesInfo.Clone: TdxCombinedTablePropertiesInfo;
begin
  Result := TdxCombinedTablePropertiesInfo(inherited Clone);
end;

procedure TdxCombinedTablePropertiesInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxCombinedTablePropertiesInfo absolute Source;
begin
  CellMargins.CopyFrom(AInfo.CellMargins);
  Borders.CopyFrom(AInfo.Borders);
  CellSpacing.CopyFrom(AInfo.CellSpacing);
  TableIndent.CopyFrom(AInfo.TableIndent);
  PreferredWidth.CopyFrom(AInfo.PreferredWidth);
  GeneralSettings.CopyFrom(AInfo.GeneralSettings);
  FloatingPosition.CopyFrom(AInfo.FloatingPosition);
end;

{ TdxTableCellGeneralSettingsInfo }

constructor TdxTableCellGeneralSettingsInfo.Create;
begin
  inherited Create;
  FColumnSpan := 1;
end;

function TdxTableCellGeneralSettingsInfo.Clone: TdxTableCellGeneralSettingsInfo;
begin
  Result := TdxTableCellGeneralSettingsInfo(inherited Clone);
end;

procedure TdxTableCellGeneralSettingsInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxTableCellGeneralSettingsInfo absolute Source;
begin
  HideCellMark := AInfo.HideCellMark;
  NoWrap := AInfo.NoWrap;
  FitText := AInfo.FitText;
  TextDirection := AInfo.TextDirection;
  VerticalAlignment := AInfo.VerticalAlignment;
  ColumnSpan := AInfo.ColumnSpan;
  HorizontalMerging := AInfo.HorizontalMerging;
  VerticalMerging := AInfo.VerticalMerging;
  CellConditionalFormatting := AInfo.CellConditionalFormatting;
  BackgroundColor := AInfo.BackgroundColor;
  ForegroundColor := AInfo.ForegroundColor;
  Shading := AInfo.Shading;
end;

function TdxTableCellGeneralSettingsInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxTableCellGeneralSettingsInfo;
begin
  AInfo := Safe<TdxTableCellGeneralSettingsInfo>.Cast(AObject);
  if AInfo = nil then
    Exit(False);
  Result := (HideCellMark = AInfo.HideCellMark) and (NoWrap = AInfo.NoWrap) and (FitText = AInfo.FitText) and
    (TextDirection = AInfo.TextDirection) and (VerticalAlignment = AInfo.VerticalAlignment) and
    (ColumnSpan = AInfo.ColumnSpan) and (HorizontalMerging = AInfo.HorizontalMerging) and
    (VerticalMerging = AInfo.VerticalMerging) and (CellConditionalFormatting = AInfo.CellConditionalFormatting) and
    (BackgroundColor = AInfo.BackgroundColor) and (ForegroundColor = AInfo.ForegroundColor) and
    (Shading = AInfo.Shading);
end;

{ TdxTableCellGeneralSettings }

constructor TdxTableCellGeneralSettings.Create(APieceTable: TdxCustomPieceTable; const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

procedure TdxTableCellGeneralSettings.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  Owner.ApplyChanges(AChangeActions);
end;

procedure TdxTableCellGeneralSettings.BeginChanging(AChangedProperty: TdxProperties);
begin
  Owner.BeginChanging(AChangedProperty);
end;

procedure TdxTableCellGeneralSettings.CopyFrom(ANewSettings: TdxTableCellGeneralSettings);
begin
  Owner.BeginPropertiesUpdate;
  try
    BeginUpdate;
    try
      HideCellMark := ANewSettings.HideCellMark;
      NoWrap := ANewSettings.NoWrap;
      FitText := ANewSettings.FitText;
      TextDirection := ANewSettings.TextDirection;
      VerticalAlignment := ANewSettings.VerticalAlignment;
      ColumnSpan := ANewSettings.ColumnSpan;
      VerticalMerging := ANewSettings.VerticalMerging;
      CellConditionalFormatting := ANewSettings.CellConditionalFormatting;
      BackgroundColor := ANewSettings.BackgroundColor;
      ForegroundColor := ANewSettings.ForegroundColor;
      Shading := ANewSettings.Shading;
    finally
      EndUpdate;
    end;
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableCellGeneralSettings.CopyFrom(const Source: TdxTableCellGeneralSettingsInfo);
begin
  inherited CopyFrom(Source);
end;

procedure TdxTableCellGeneralSettings.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxTableCellGeneralSettingsInfo>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

procedure TdxTableCellGeneralSettings.EndChanging;
begin
  Owner.EndChanging;
end;

function TdxTableCellGeneralSettings.GetBackgroundColor: TdxAlphaColor;
begin
  Result := Info.BackgroundColor;
end;

function TdxTableCellGeneralSettings.GetForegroundColor: TdxAlphaColor;
begin
  Result := Info.ForegroundColor;
end;

function TdxTableCellGeneralSettings.GetShading: TdxShadingPattern;
begin
  Result := Info.Shading;
end;

function TdxTableCellGeneralSettings.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.BatchUpdate);
end;

function TdxTableCellGeneralSettings.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableCellGeneralSettingsInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableCellGeneralSettingsInfoCache;
end;

function TdxTableCellGeneralSettings.GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
begin
  Result := Info.CellConditionalFormatting;
end;

function TdxTableCellGeneralSettings.GetColumnSpan: Integer;
begin
  Result := Info.ColumnSpan;
end;

function TdxTableCellGeneralSettings.GetFitText: Boolean;
begin
  Result := Info.FitText;
end;

function TdxTableCellGeneralSettings.GetHideCellMark: Boolean;
begin
  Result := Info.HideCellMark;
end;

function TdxTableCellGeneralSettings.GetNoWrap: Boolean;
begin
  Result := Info.NoWrap;
end;

function TdxTableCellGeneralSettings.GetTextDirection: TdxTextDirection;
begin
  Result := Info.TextDirection;
end;

function TdxTableCellGeneralSettings.GetVerticalAlignment: TdxVerticalAlignment;
begin
  Result := Info.VerticalAlignment;
end;

function TdxTableCellGeneralSettings.GetVerticalMerging: TdxMergingState;
begin
  Result := Info.VerticalMerging;
end;

procedure TdxTableCellGeneralSettings.InnerSetBackgroundColor(const Value: TdxAlphaColor);
begin
  BeginChanging(TdxProperties.BackgroundColor);
  if Value = Info.BackgroundColor then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxAlphaColor>(SetBackgroundColor, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetForegroundColor(const Value: TdxAlphaColor);
begin
  BeginChanging(TdxProperties.ForegroundColor);
  if Value = Info.ForegroundColor then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxAlphaColor>(SetForegroundColor, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetShading(const Value: TdxShadingPattern);
begin
  BeginChanging(TdxProperties.Shading);
  if Value = Info.Shading then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxShadingPattern>(SetShading, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetCellConditionalFormatting(
  const Value: TdxConditionalTableStyleFormattingTypes);
begin
  BeginChanging(TdxProperties.CellConditionalFormatting);
  if Value = Info.CellConditionalFormatting then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxConditionalTableStyleFormattingTypes>(SetCellConditionalFormatting, Value);
  EndChanging;
end;

function TdxTableCellGeneralSettings.SetBackgroundColor(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.BackgroundColor := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.BackgroundColor);
end;

function TdxTableCellGeneralSettings.SetForegroundColor(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.ForegroundColor := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.ForegroundColor);
end;

function TdxTableCellGeneralSettings.SetShading(const AInfo: TdxTableCellGeneralSettingsInfo; const AValue: TdxShadingPattern): TdxDocumentModelChangeActions;
begin
  AInfo.Shading := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.Shading);
end;

function TdxTableCellGeneralSettings.SetCellConditionalFormatting(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: TdxConditionalTableStyleFormattingTypes): TdxDocumentModelChangeActions;
begin
  AInfo.CellConditionalFormatting := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.ConditionalFormatting);
end;

function TdxTableCellGeneralSettings.SetColumnSpan(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.ColumnSpan := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.ColumnSpan);
end;

procedure TdxTableCellGeneralSettings.InnerSetColumnSpan(const Value: Integer);
begin
  if Value <= 0 then
    TdxRichEditExceptions.ThrowArgumentException('ColumnSpan', Value);

  BeginChanging(TdxProperties.ColumnSpan);
  if Value = Info.ColumnSpan then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<Integer>(SetColumnSpan, Value);
  EndChanging;
end;

function TdxTableCellGeneralSettings.SetFitText(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.FitText := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.FitText);
end;

procedure TdxTableCellGeneralSettings.InnerSetFitText(const Value: Boolean);
begin
  BeginChanging(TdxProperties.FitText);
  if Value = Info.FitText then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<Boolean>(SetFitText, Value);
  EndChanging;
end;

function TdxTableCellGeneralSettings.SetHideCellMark(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.HideCellMark := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.HideCellMark);
end;

function TdxTableCellGeneralSettings.SetNoWrap(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.NoWrap := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.NoWrap);
end;

function TdxTableCellGeneralSettings.SetTextDirection(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: TdxTextDirection): TdxDocumentModelChangeActions;
begin
  AInfo.TextDirection := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.TextDirection);
end;

procedure TdxTableCellGeneralSettings.InnerSetHideCellMark(const Value: Boolean);
begin
  BeginChanging(TdxProperties.HideCellMark);
  if Value = Info.HideCellMark then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<Boolean>(SetHideCellMark, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetNoWrap(const Value: Boolean);
begin
  BeginChanging(TdxProperties.NoWrap);
  if Value = Info.NoWrap then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<Boolean>(SetNoWrap, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetTextDirection(const Value: TdxTextDirection);
begin
  BeginChanging(TdxProperties.TextDirection);
  if Value = Info.TextDirection then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxTextDirection>(SetTextDirection, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.InnerSetVerticalAlignment(const Value: TdxVerticalAlignment);
begin
  BeginChanging(TdxProperties.VerticalAlignment);
  if Value = Info.VerticalAlignment then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxVerticalAlignment>(SetVerticalAlignment, Value);
  EndChanging;
end;

function TdxTableCellGeneralSettings.SetVerticalAlignment(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: TdxVerticalAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalAlignment := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.VerticalAlignment);
end;

function TdxTableCellGeneralSettings.SetVerticalMerging(const AInfo: TdxTableCellGeneralSettingsInfo;
  const AValue: TdxMergingState): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalMerging := AValue;
  Result := TdxTableCellChangeActionsCalculator.CalculateChangeActions(TdxTableCellChangeType.VerticalMerging);
end;

procedure TdxTableCellGeneralSettings.InnerSetVerticalMerging(const Value: TdxMergingState);
begin
  BeginChanging(TdxProperties.VerticalMerging);
  if Value = Info.VerticalMerging then
  begin
    EndChanging;
    Exit;
  end;
  SetPropertyValue<TdxMergingState>(SetVerticalMerging, Value);
  EndChanging;
end;

procedure TdxTableCellGeneralSettings.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  Owner.RaiseObtainAffectedRange(AArgs);
end;

{ TdxTableCellChangeActionsCalculator }

class function TdxTableCellChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxTableCellChangeType): TdxDocumentModelChangeActions;
const
  TableCellChangeActionsMap: array[TdxTableCellChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler],
    [TdxDocumentModelChangeAction.Redraw],
    [TdxDocumentModelChangeAction.Redraw{, TdxDocumentModelChangeActions.ResetSecondaryLayout}],
    [TdxDocumentModelChangeAction.Redraw{, TdxDocumentModelChangeActions.ResetSecondaryLayout}],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler]
  );
begin
  Result := TableCellChangeActionsMap[AChange];
end;

{ TdxTableCellGeneralSettingsInfoCache }

function TdxTableCellGeneralSettingsInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableCellGeneralSettingsInfo;
begin
  Result := TdxTableCellGeneralSettingsInfo.Create;
  Result.TextDirection := TdxTextDirection.LeftToRightTopToBottom;
  Result.VerticalAlignment := TdxVerticalAlignment.Top;
  Result.ColumnSpan := 1;
  Result.HorizontalMerging := TdxMergingState.None;
  Result.VerticalMerging := TdxMergingState.None;
  Result.CellConditionalFormatting := [TdxConditionalTableStyleFormattingType.WholeTable];
  Result.BackgroundColor := TdxAlphaColors.Empty;
  Result.ForegroundColor := TdxAlphaColors.Empty;
  Result.Shading := TdxShadingPattern.Clear;
end;

{ TdxTableRowPropertiesOptions }

constructor TdxTableRowPropertiesOptions.Create(AValue: Integer = 0);
begin
  inherited Create;
  FValue := AValue;
end;

function TdxTableRowPropertiesOptions.GetVal(AMask: Integer): Boolean;
begin
  Result := (FValue and AMask) <> 0;
end;

procedure TdxTableRowPropertiesOptions.SetVal(AMask: Integer; ABitValue: Boolean);
begin
  if ABitValue then
    FValue := FValue or AMask
  else
    FValue := FValue and not AMask;
end;

function TdxTableRowPropertiesOptions.Clone: TdxTableRowPropertiesOptions;
begin
  Result := TdxTableRowPropertiesOptions(inherited Clone);
end;

procedure TdxTableRowPropertiesOptions.CopyFrom(Source: TdxCloneable);
var
  AOptions: TdxTableRowPropertiesOptions absolute Source;
begin
  FValue := AOptions.FValue;
end;

function TdxTableRowPropertiesOptions.Equals(AObject: TObject): Boolean;
begin
  if AObject is TdxTableRowPropertiesOptions then
    Result := Value = TdxTableRowPropertiesOptions(AObject).Value
  else
    Result := False;
end;

function TdxTableRowPropertiesOptions.GetHashCode: Integer;
begin
  Result := Value;
end;

function TdxTableRowPropertiesOptions.GetUseCantSplit: Boolean;
begin
  Result := GetVal(MaskUseCantSplit);
end;

function TdxTableRowPropertiesOptions.GetUseCellSpacing: Boolean;
begin
  Result := GetVal(MaskUseCellSpacing);
end;

function TdxTableRowPropertiesOptions.GetUseGridAfter: Boolean;
begin
  Result := GetVal(MaskUseGridAfter);
end;

function TdxTableRowPropertiesOptions.GetUseGridBefore: Boolean;
begin
  Result := GetVal(MaskUseGridBefore);
end;

function TdxTableRowPropertiesOptions.GetUseHeader: Boolean;
begin
  Result := GetVal(MaskUseHeader);
end;

function TdxTableRowPropertiesOptions.GetUseHeight: Boolean;
begin
  Result := GetVal(MaskUseHeight);
end;

function TdxTableRowPropertiesOptions.GetUseHideCellMark: Boolean;
begin
  Result := GetVal(MaskUseHideCellMark);
end;

function TdxTableRowPropertiesOptions.GetUseTableRowAlignment: Boolean;
begin
  Result := GetVal(MaskUseTableRowAlignment);
end;

function TdxTableRowPropertiesOptions.GetUseWidthAfter: Boolean;
begin
  Result := GetVal(MaskUseWidthAfter);
end;

function TdxTableRowPropertiesOptions.GetUseWidthBefore: Boolean;
begin
  Result := GetVal(MaskUseWidthBefore);
end;

procedure TdxTableRowPropertiesOptions.SetUseCantSplit(const AValue: Boolean);
begin
  SetVal(MaskUseCantSplit, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseCellSpacing(const AValue: Boolean);
begin
  SetVal(MaskUseCellSpacing, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseGridAfter(const AValue: Boolean);
begin
  SetVal(MaskUseGridAfter, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseGridBefore(const AValue: Boolean);
begin
  SetVal(MaskUseGridBefore, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseHeader(const AValue: Boolean);
begin
  SetVal(MaskUseHeader, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseHeight(const AValue: Boolean);
begin
  SetVal(MaskUseHeight, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseHideCellMark(const AValue: Boolean);
begin
  SetVal(MaskUseHideCellMark, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseTableRowAlignment(const AValue: Boolean);
begin
  SetVal(MaskUseTableRowAlignment, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseWidthAfter(const AValue: Boolean);
begin
  SetVal(MaskUseWidthAfter, AValue);
end;

procedure TdxTableRowPropertiesOptions.SetUseWidthBefore(const AValue: Boolean);
begin
  SetVal(MaskUseWidthBefore, AValue);
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseCantSplit(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseCantSplit;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseCellSpacing(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseCellSpacing;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseGridAfter(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseGridAfter;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseGridBefore(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseGridBefore;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseHeader(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseHeader;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseHeight(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseHeight;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseHideCellMark(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseHideCellMark;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseTableRowAlignment(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseTableRowAlignment;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseWidthAfter(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseWidthAfter;
end;

class function TdxTableRowPropertiesOptions.GetOptionsUseWidthBefore(
  AOptions: TdxTableRowPropertiesOptions): Boolean;
begin
  Result := AOptions.UseWidthBefore;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseCantSplit(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseCantSplit := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseCellSpacing(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseCellSpacing := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseGridAfter(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseGridAfter := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseGridBefore(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseGridBefore := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseHeader(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseHeader := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseHeight(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseHeight := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseHideCellMark(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseHideCellMark := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseTableRowAlignment(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseTableRowAlignment := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseWidthAfter(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseWidthAfter := AValue;
end;

class procedure TdxTableRowPropertiesOptions.SetOptionsUseWidthBefore(
  AOptions: TdxTableRowPropertiesOptions; const AValue: Boolean);
begin
  AOptions.UseWidthBefore := AValue;
end;

{ TdxCombinedTableRowPropertiesInfo }

constructor TdxCombinedTableRowPropertiesInfo.Create;
begin
  inherited Create;
  FHeight := TdxHeightUnitInfo.Create;
  FWidthBefore := TdxWidthUnitInfo.Create;
  FWidthAfter := TdxWidthUnitInfo.Create;
  FCellSpacing := TdxWidthUnitInfo.Create;
  FGeneralSettings := TdxTableRowGeneralSettingsInfo.Create;
  FIsOwner := True;
end;

constructor TdxCombinedTableRowPropertiesInfo.Create(ARowProperties: TdxTableRowProperties);
begin
  inherited Create;
  FHeight := ARowProperties.Height.Info;
  FWidthBefore := ARowProperties.WidthBefore.Info;
  FWidthAfter := ARowProperties.WidthAfter.Info;
  FCellSpacing := ARowProperties.CellSpacing.Info;
  FGeneralSettings := ARowProperties.GeneralSettings.Info;
  FIsOwner := False;
end;

destructor TdxCombinedTableRowPropertiesInfo.Destroy;
begin
  if FIsOwner then
  begin
    FreeAndNil(FHeight);
    FreeAndNil(FWidthBefore);
    FreeAndNil(FWidthAfter);
    FreeAndNil(FCellSpacing);
    FreeAndNil(FGeneralSettings);
  end;
  inherited Destroy;
end;

function TdxCombinedTableRowPropertiesInfo.Clone: TdxCombinedTableRowPropertiesInfo;
begin
  Result := TdxCombinedTableRowPropertiesInfo(inherited Clone);
end;

procedure TdxCombinedTableRowPropertiesInfo.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxCombinedTableRowPropertiesInfo absolute Source;
begin
  Height.CopyFrom(AValue.Height);
  WidthBefore.CopyFrom(AValue.WidthBefore);
  WidthAfter.CopyFrom(AValue.WidthAfter);
  CellSpacing.CopyFrom(AValue.CellSpacing);
  GeneralSettings.CopyFrom(AValue.GeneralSettings);
end;

{ TdxTableRowProperties }

procedure TdxTableRowProperties.CopyFrom(AProperties: TdxTableRowProperties);
var
  AContainer: IdxPropertiesContainer;
  AInfo: TdxTableRowPropertiesOptions;
  AIsDeferredInfo: Boolean;
begin
  Supports(Self, IdxPropertiesContainer, AContainer);
  AContainer.BeginPropertiesUpdate;
  try
    if DocumentModel = AProperties.DocumentModel then
    begin
      Height.CopyFrom(AProperties.Height);
      WidthBefore.CopyFrom(AProperties.WidthBefore);
      WidthAfter.CopyFrom(AProperties.WidthAfter);
      CellSpacing.CopyFrom(AProperties.CellSpacing);
      GeneralSettings.CopyFrom(AProperties.GeneralSettings);
      AInfo := GetInfoForModification(AIsDeferredInfo);
      AInfo.CopyFrom(AProperties.Info);
      ReplaceInfo(AInfo, BatchUpdateChangeActions);
      if not AIsDeferredInfo then AInfo.Free;
    end
    else
    begin
      Height.CopyFrom(AProperties.Height.Info);
      WidthBefore.CopyFrom(AProperties.WidthBefore.Info);
      WidthAfter.CopyFrom(AProperties.WidthAfter.Info);
      CellSpacing.CopyFrom(AProperties.CellSpacing.Info);
      GeneralSettings.CopyFrom(AProperties.GeneralSettings.Info);
      Info.CopyFrom(AProperties.Info);
    end;
  finally
    AContainer.EndPropertiesUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableRowProperties.CopyFrom(const Source: TdxTableRowPropertiesOptions);
begin
  inherited CopyFrom(Source);
end;

procedure TdxTableRowProperties.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxTableRowPropertiesOptions>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

constructor TdxTableRowProperties.Create(const ADocumentModelPart: TdxCustomPieceTable);
begin
  inherited Create(ADocumentModelPart);
  FHeight := TdxRowHeight.Create(PieceTable, Self);
  FWidthBefore := TdxWidthBefore.Create(PieceTable, Self);
  FWidthAfter := TdxWidthAfter.Create(PieceTable, Self);
  FCellSpacing := TdxCellSpacing.Create(PieceTable, Self);
  FGeneralSettings := TdxTableRowGeneralSettings.Create(PieceTable, Self);
end;

destructor TdxTableRowProperties.Destroy;
begin
  FGeneralSettings.Free;
  FCellSpacing.Free;
  FWidthAfter.Free;
  FWidthBefore.Free;
  FHeight.Free;
  inherited Destroy;
end;

function TdxTableRowProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [];
end;

function TdxTableRowProperties.GetAccessor(AChangedProperty: TdxProperties): TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>;
begin
  case AChangedProperty of
    TdxProperties.CantSplit:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseCantSplit, TdxTableRowPropertiesOptions.SetOptionsUseCantSplit);
    TdxProperties.CellSpacing:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseCellSpacing, TdxTableRowPropertiesOptions.SetOptionsUseCellSpacing);
    TdxProperties.GridAfter:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseGridAfter, TdxTableRowPropertiesOptions.SetOptionsUseGridAfter);
    TdxProperties.GridBefore:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseGridBefore, TdxTableRowPropertiesOptions.SetOptionsUseGridBefore);
    TdxProperties.Header:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseHeader, TdxTableRowPropertiesOptions.SetOptionsUseHeader);
    TdxProperties.RowHeight:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseHeight, TdxTableRowPropertiesOptions.SetOptionsUseHeight);
    TdxProperties.HideCellMark:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseHideCellMark, TdxTableRowPropertiesOptions.SetOptionsUseHideCellMark);
    TdxProperties.TableRowAlignment:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseTableRowAlignment, TdxTableRowPropertiesOptions.SetOptionsUseTableRowAlignment);
    TdxProperties.WidthAfter:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseWidthAfter, TdxTableRowPropertiesOptions.SetOptionsUseWidthAfter);
    TdxProperties.WidthBefore:
      Result := TdxBoolPropertyAccessor<TdxTableRowPropertiesOptions>.Create(TdxTableRowPropertiesOptions.GetOptionsUseWidthBefore, TdxTableRowPropertiesOptions.SetOptionsUseWidthBefore);
  else
    Result := inherited GetAccessor(AChangedProperty);
  end;
end;

function TdxTableRowProperties.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableRowPropertiesOptions>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableRowPropertiesOptionsCache;
end;

function TdxTableRowProperties.GetCantSplit: Boolean;
begin
  Result := GeneralSettings.CantSplit;
end;

function TdxTableRowProperties.GetGridAfter: Integer;
begin
  Result := GeneralSettings.GridAfter;
end;

function TdxTableRowProperties.GetGridBefore: Integer;
begin
  Result := GeneralSettings.GridBefore;
end;

function TdxTableRowProperties.GetHeader: Boolean;
begin
  Result := GeneralSettings.Header;
end;

function TdxTableRowProperties.GetHideCellMark: Boolean;
begin
  Result := GeneralSettings.HideCellMark;
end;

function TdxTableRowProperties.GetTableRowAlignment: TdxTableRowAlignment;
begin
  Result := GeneralSettings.TableRowAlignment;
end;

function TdxTableRowProperties.GetUse(AMask: Integer): Boolean;
begin
  Result := Info.GetVal(AMask);
end;

function TdxTableRowProperties.GetUseCantSplit: Boolean;
begin
  Result := Info.UseCantSplit;
end;

function TdxTableRowProperties.GetUseCellSpacing: Boolean;
begin
  Result := Info.UseCellSpacing;
end;

function TdxTableRowProperties.GetUseGridAfter: Boolean;
begin
  Result := Info.UseGridAfter;
end;

function TdxTableRowProperties.GetUseGridBefore: Boolean;
begin
  Result := Info.UseGridBefore;
end;

function TdxTableRowProperties.GetUseHeader: Boolean;
begin
  Result := Info.UseHeader;
end;

function TdxTableRowProperties.GetUseHeight: Boolean;
begin
  Result := Info.UseHeight;
end;

function TdxTableRowProperties.GetUseHideCellMark: Boolean;
begin
  Result := Info.UseHideCellMark;
end;

function TdxTableRowProperties.GetUseTableRowAlignment: Boolean;
begin
  Result := Info.UseTableRowAlignment;
end;

function TdxTableRowProperties.GetUseWidthAfter: Boolean;
begin
  Result := Info.UseWidthAfter;
end;

function TdxTableRowProperties.GetUseWidthBefore: Boolean;
begin
  Result := Info.UseWidthBefore;
end;

procedure TdxTableRowProperties.Merge(AProperties: TdxTableRowProperties);
var
  AContainer: IdxPropertiesContainer;
begin
  Supports(Self, IdxPropertiesContainer, AContainer);
  AContainer.BeginPropertiesUpdate;
  try
    if not UseCantSplit and AProperties.UseCantSplit then
      CantSplit := AProperties.CantSplit;
    if not UseCellSpacing and AProperties.UseCellSpacing then
      CellSpacing.CopyFrom(AProperties.CellSpacing);
    if not UseGridAfter and AProperties.UseGridAfter then
      GridAfter := AProperties.GridAfter;
    if not UseGridBefore and AProperties.UseGridBefore then
      GridBefore := AProperties.GridBefore;
    if not UseHeader and AProperties.UseHeader then
      Header := AProperties.Header;
    if not UseHeight and AProperties.UseHeight then
      Height.CopyFrom(AProperties.Height);
    if not UseHideCellMark and AProperties.HideCellMark then
      HideCellMark := AProperties.HideCellMark;
    if not UseTableRowAlignment and AProperties.UseTableRowAlignment then
      TableRowAlignment := AProperties.TableRowAlignment;
    if not UseWidthBefore and AProperties.UseWidthBefore then
      WidthBefore.CopyFrom(AProperties.WidthBefore);
    if not UseWidthAfter and AProperties.UseWidthAfter then
      WidthAfter.CopyFrom(AProperties.WidthAfter);
  finally
    AContainer.EndPropertiesUpdate();
  end;
end;

procedure TdxTableRowProperties.Reset;
begin
  DocumentModel.History.BeginTransaction;
  try
    CopyFrom((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableRowProperties);
    ResetAllUse;
  finally
    DocumentModel.History.EndTransaction;
  end;
end;

procedure TdxTableRowProperties.ResetAllUse;
begin
  ReplaceInfo(GetCache(DocumentModel)[TdxTableRowPropertiesOptionsCache.EmptyRowPropertiesOptionsItem], BatchUpdateChangeActions);
end;

procedure TdxTableRowProperties.ResetUse(AMask: Integer);
var
  ANewOptions: TdxTableRowPropertiesOptions;
begin
  ANewOptions := TdxTableRowPropertiesOptions.Create(Info.Value and not AMask);
  try
    ReplaceInfo(ANewOptions, BatchUpdateChangeActions);
  finally
    ANewOptions.Free;
  end;
end;

procedure TdxTableRowProperties.SetCantSplit(const Value: Boolean);
begin
  GeneralSettings.CantSplit := Value;
end;

procedure TdxTableRowProperties.SetGridAfter(const Value: Integer);
begin
  GeneralSettings.GridAfter := Value;
end;

procedure TdxTableRowProperties.SetGridBefore(const Value: Integer);
begin
  GeneralSettings.GridBefore := Value;
end;

procedure TdxTableRowProperties.SetHeader(const Value: Boolean);
begin
  GeneralSettings.Header := Value;
end;

procedure TdxTableRowProperties.SetHideCellMark(const Value: Boolean);
begin
  GeneralSettings.HideCellMark := Value;
end;

procedure TdxTableRowProperties.SetTableRowAlignment(const Value: TdxTableRowAlignment);
begin
  GeneralSettings.TableRowAlignment := Value;
end;

{ TdxTableRowGeneralSettingsInfo }

function TdxTableRowGeneralSettingsInfo.Clone: TdxTableRowGeneralSettingsInfo;
begin
  Result := TdxTableRowGeneralSettingsInfo(inherited Clone);
end;

procedure TdxTableRowGeneralSettingsInfo.CopyFrom(Source: TdxCloneable);
var
  AInfo: TdxTableRowGeneralSettingsInfo absolute Source;
begin
  CantSplit := AInfo.CantSplit;
  HideCellMark := AInfo.HideCellMark;
  Header := AInfo.Header;
  GridBefore := AInfo.GridBefore;
  GridAfter := AInfo.GridAfter;
  TableRowAlignment := AInfo.TableRowAlignment;
end;

function TdxTableRowGeneralSettingsInfo.Equals(AObject: TObject): Boolean;
var
  AInfo: TdxTableRowGeneralSettingsInfo;
begin
  if AObject is TdxTableRowGeneralSettingsInfo then
  begin
    AInfo := TdxTableRowGeneralSettingsInfo(AObject);
    Result := (CantSplit = AInfo.CantSplit) and
      (HideCellMark = AInfo.HideCellMark) and
      (Header = AInfo.Header) and
      (GridBefore = AInfo.GridBefore) and
      (GridAfter = AInfo.GridAfter) and
      (TableRowAlignment = AInfo.TableRowAlignment);
  end
  else
    Result := False;
end;

{ TdxTableRowGeneralSettingsInfoCache }

function TdxTableRowGeneralSettingsInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableRowGeneralSettingsInfo;
begin
  Result := TdxTableRowGeneralSettingsInfo.Create;
  Result.TableRowAlignment := TdxTableRowAlignment.Left;
end;

{ TdxTableRowGeneralSettings }

constructor TdxTableRowGeneralSettings.Create(APieceTable: TdxCustomPieceTable;
  const AOwner: IdxPropertiesContainer);
begin
  inherited Create(APieceTable);
  Assert(AOwner <> nil);
  FOwner := AOwner;
end;

procedure TdxTableRowGeneralSettings.ApplyChanges(const AChangeActions: TdxDocumentModelChangeActions);
begin
  Owner.ApplyChanges(AChangeActions);
end;

procedure TdxTableRowGeneralSettings.BeginChanging(AChangedProperty: TdxProperties);
begin
  Owner.BeginChanging(AChangedProperty);
end;

procedure TdxTableRowGeneralSettings.CopyFrom(const ASettings: TdxTableRowGeneralSettings);
begin
  Owner.BeginPropertiesUpdate;
  try
    BeginUpdate;
    try
      CantSplit := ASettings.CantSplit;
      HideCellMark := ASettings.HideCellMark;
      Header := ASettings.Header;
      GridBefore := ASettings.GridBefore;
      GridAfter := ASettings.GridAfter;
      TableRowAlignment := ASettings.TableRowAlignment;
    finally
      EndUpdate;
    end;
  finally
    Owner.EndPropertiesUpdate;
  end;
end;

{$IFNDEF DELPHI17}
procedure TdxTableRowGeneralSettings.CopyFrom(const Source: TdxTableRowGeneralSettingsInfo);
begin
  inherited CopyFrom(Source);
end;

procedure TdxTableRowGeneralSettings.CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxTableRowGeneralSettingsInfo>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

procedure TdxTableRowGeneralSettings.EndChanging;
begin
  Owner.EndChanging;
end;

function TdxTableRowGeneralSettings.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.BatchUpdate);
end;

function TdxTableRowGeneralSettings.GetCache(
  const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxTableRowGeneralSettingsInfo>;
begin
  Result := TdxDocumentCache(ADocumentModel.Cache).TableRowGeneralSettingsInfoCache;
end;

function TdxTableRowGeneralSettings.GetCantSplit: Boolean;
begin
  Result := Info.CantSplit;
end;

function TdxTableRowGeneralSettings.GetGridAfter: Integer;
begin
  Result := Info.GridAfter;
end;

function TdxTableRowGeneralSettings.GetGridBefore: Integer;
begin
  Result := Info.GridBefore;
end;

function TdxTableRowGeneralSettings.GetHeader: Boolean;
begin
  Result := Info.Header;
end;

function TdxTableRowGeneralSettings.GetHideCellMark: Boolean;
begin
  Result := Info.HideCellMark;
end;

function TdxTableRowGeneralSettings.GetTableRowAlignment: TdxTableRowAlignment;
begin
  Result := Info.TableRowAlignment;
end;

procedure TdxTableRowGeneralSettings.InnerSetCantSplit(const Value: Boolean);
begin
  BeginChanging(TdxProperties.CantSplit);
  if Value <> Info.CantSplit then
    SetPropertyValue<Boolean>(SetCantSplit, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.InnerSetGridAfter(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('GridBefore', Value);

  BeginChanging(TdxProperties.GridAfter);
  if Value <> Info.GridAfter then
    SetPropertyValue<Integer>(SetGridAfter, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.InnerSetGridBefore(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('GridBefore', Value);

  BeginChanging(TdxProperties.GridBefore);
  if Value <> Info.GridBefore then
    SetPropertyValue<Integer>(SetGridBefore, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.InnerSetHeader(const Value: Boolean);
begin
  BeginChanging(TdxProperties.Header);
  if Value <> Info.Header then
    SetPropertyValue<Boolean>(SetHeader, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.InnerSetHideCellMark(const Value: Boolean);
begin
  BeginChanging(TdxProperties.HideCellMark);
  if Value <> Info.HideCellMark then
    SetPropertyValue<Boolean>(SetHideCellMark, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.InnerSetTableRowAlignment(const Value: TdxTableRowAlignment);
begin
  BeginChanging(TdxProperties.TableRowAlignment);
  if Value <> Info.TableRowAlignment then
    SetPropertyValue<TdxTableRowAlignment>(SetTableRowAlignment, Value);
  EndChanging;
end;

procedure TdxTableRowGeneralSettings.RaiseObtainAffectedRange(const AArgs: TdxObtainAffectedRangeEventArgs);
begin
  Owner.RaiseObtainAffectedRange(AArgs);
end;

function TdxTableRowGeneralSettings.SetCantSplit(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.CantSplit := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.CantSplit);
end;

function TdxTableRowGeneralSettings.SetGridAfter(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.GridAfter := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.GridAfter);
end;

function TdxTableRowGeneralSettings.SetGridBefore(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.GridBefore := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.GridBefore);
end;

function TdxTableRowGeneralSettings.SetHeader(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Header := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.Header);
end;

function TdxTableRowGeneralSettings.SetHideCellMark(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.HideCellMark := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.HideCellMark);
end;

function TdxTableRowGeneralSettings.SetTableRowAlignment(const AInfo: TdxTableRowGeneralSettingsInfo;
  const AValue: TdxTableRowAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.TableRowAlignment := AValue;
  Result := TdxTableRowChangeActionsCalculator.CalculateChangeActions(TdxTableRowChangeType.TableRowAlignment);
end;

{ TdxRowHeight }

function TdxRowHeight.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

procedure TdxRowHeight.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.RowHeight);
end;

{ TdxWidthBefore }

procedure TdxWidthBefore.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.WidthBefore);
end;

function TdxWidthBefore.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

function TdxWidthBefore.SetValueCore(const AUnit: TdxWidthUnitInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

{ TdxWidthAfter }

procedure TdxWidthAfter.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.WidthAfter);
end;

function TdxWidthAfter.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

function TdxWidthAfter.SetValueCore(const AUnit: TdxWidthUnitInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

{ TdxTableRowPropertiesOptionsCache }

procedure TdxTableRowPropertiesOptionsCache.AddRootStyleOptions;
begin
  AppendItem(TdxTableRowPropertiesOptions.Create(TdxTableRowPropertiesOptions.MaskUseAll));
end;

function TdxTableRowPropertiesOptionsCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableRowPropertiesOptions;
begin
  Result := TdxTableRowPropertiesOptions.Create;
end;

procedure TdxTableRowPropertiesOptionsCache.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
begin
  inherited InitItems(AUnitConverter);
  AddRootStyleOptions;
end;

{ TdxTableCellPropertiesOptionsCache }

procedure TdxTableCellPropertiesOptionsCache.AddRootStyleOptions;
begin
  AppendItem(TdxTableCellPropertiesOptions.Create(TdxTableCellPropertiesOptions.MaskUseAll));
end;

function TdxTableCellPropertiesOptionsCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxTableCellPropertiesOptions;
begin
  Result := TdxTableCellPropertiesOptions.Create;
end;

procedure TdxTableCellPropertiesOptionsCache.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
begin
  inherited InitItems(AUnitConverter);
  AddRootStyleOptions;
end;

{ TdxTableFloatingPositionChangeActionsCalculator }

class function TdxTableFloatingPositionChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxTableFloatingPositionChangeType): TdxDocumentModelChangeActions;
const
  TableFloatingPositionChangeActionsMap: array[TdxTableFloatingPositionChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler]
  );
begin
  Result := TableFloatingPositionChangeActionsMap[AChange];
end;

{ TdxTableIndent }

function TdxTableIndent.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

procedure TdxTableIndent.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.TableIndent);
end;

function TdxTableIndent.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

function TdxTableIndent.SetValueCore(const AUnit: TdxWidthUnitInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

{ TdxCellSpacing }

function TdxCellSpacing.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

procedure TdxCellSpacing.OnBeginAssign;
begin
  inherited OnBeginAssign;
  Owner.BeginChanging(TdxProperties.CellSpacing);
end;

function TdxCellSpacing.SetTypeCore(const AUnit: TdxWidthUnitInfo;
  const AValue: TdxWidthUnitType): TdxDocumentModelChangeActions;
begin
  Result := inherited SetTypeCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

function TdxCellSpacing.SetValueCore(const AUnit: TdxWidthUnitInfo;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  Result := inherited SetValueCore(AUnit, AValue) + [
    TdxDocumentModelChangeAction.RaiseContentChanged,
    TdxDocumentModelChangeAction.Redraw,
    TdxDocumentModelChangeAction.ResetPrimaryLayout,
    TdxDocumentModelChangeAction.ResetRuler,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout];
end;

{ TdxCombinedCellPropertiesInfo }

constructor TdxCombinedCellPropertiesInfo.Create(ACellProperties: TdxTableCellProperties);
begin
  inherited Create;
  FPreferredWidth := ACellProperties.PreferredWidth.Info.Clone;
  FCellMargins := TdxCombinedCellMarginsInfo.Create(ACellProperties.CellMargins);
  FBorders := TdxCombinedCellBordersInfo.Create(ACellProperties.Borders);
  FGeneralSettings := ACellProperties.GeneralSettings.Info.Clone;
end;

constructor TdxCombinedCellPropertiesInfo.Create;
begin
  inherited Create;
  FPreferredWidth := TdxWidthUnitInfo.Create;
  FCellMargins := TdxCombinedCellMarginsInfo.Create;
  FBorders := TdxCombinedCellBordersInfo.Create;
  FGeneralSettings := TdxTableCellGeneralSettingsInfo.Create;
end;

destructor TdxCombinedCellPropertiesInfo.Destroy;
begin
  FreeAndNil(FPreferredWidth);
  FreeAndNil(FCellMargins);
  FreeAndNil(FBorders);
  FreeAndNil(FGeneralSettings);
  inherited Destroy;
end;

function TdxCombinedCellPropertiesInfo.Clone: TdxCombinedCellPropertiesInfo;
begin
  Result := TdxCombinedCellPropertiesInfo(inherited Clone);
end;

procedure TdxCombinedCellPropertiesInfo.CopyFrom(Source: TdxCloneable);
var
  AValue: TdxCombinedCellPropertiesInfo absolute Source;
begin
  PreferredWidth.CopyFrom(AValue.PreferredWidth);
  CellMargins.CopyFrom(AValue.CellMargins);
  Borders.CopyFrom(AValue.Borders);
  GeneralSettings.CopyFrom(AValue.GeneralSettings);
end;

{ TdxTableRowChangeActionsCalculator }

class function TdxTableRowChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxTableRowChangeType): TdxDocumentModelChangeActions;
const
  TableRowChangeActionsMap: array[TdxTableRowChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler]
  );
begin
  Result := TableRowChangeActionsMap[AChange];
end;

{ TdxMergedTableProperties }

constructor TdxMergedTableProperties.Create(AInitialProperties: TdxTableProperties);
var
  AInfo: TdxCombinedTablePropertiesInfo;
begin
  AInfo := TdxCombinedTablePropertiesInfo.Create(AInitialProperties);
  try
    inherited Create(AInfo, AInitialProperties.Info);
  finally
    AInfo.Free;
  end;
end;

constructor TdxMergedTableProperties.Create(AInitialProperties: TdxMergedTableProperties);
begin
  inherited Create(AInitialProperties.Info, AInitialProperties.Options);
end;

procedure TdxMergedTableProperties.Merge(AProperties: TdxTableProperties);
var
  AInfo: TdxCombinedTablePropertiesInfo;
begin
  AInfo := TdxCombinedTablePropertiesInfo.Create(AProperties);
  try
    MergeCore(AInfo, AProperties.Info);
  finally
    AInfo.Free;
  end;
end;

procedure TdxMergedTableProperties.Merge(AProperties: TdxMergedTableProperties);
begin
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedTableProperties.MergeCore(const AInfo: TdxCombinedTablePropertiesInfo;
  const AOptions: TdxTablePropertiesOptions);
begin
  if not Options.UseBottomBorder and AOptions.UseBottomBorder then
  begin
    Info.Borders.BottomBorder.CopyFrom(AInfo.Borders.BottomBorder);
    Options.UseBottomBorder := True;
  end;
  if not Options.UseTopBorder and AOptions.UseTopBorder then
  begin
    Info.Borders.TopBorder.CopyFrom(AInfo.Borders.TopBorder);
    Options.UseTopBorder := True;
  end;
  if not Options.UseLeftBorder and AOptions.UseLeftBorder then
  begin
    Info.Borders.LeftBorder.CopyFrom(AInfo.Borders.LeftBorder);
    Options.UseLeftBorder := True;
  end;
  if not Options.UseRightBorder and AOptions.UseRightBorder then
  begin
    Info.Borders.RightBorder.CopyFrom(AInfo.Borders.RightBorder);
    Options.UseRightBorder := True;
  end;
  if not Options.UseInsideHorizontalBorder and AOptions.UseInsideHorizontalBorder then
  begin
    Info.Borders.InsideHorizontalBorder.CopyFrom(AInfo.Borders.InsideHorizontalBorder);
    Options.UseInsideHorizontalBorder := True;
  end;
  if not Options.UseInsideVerticalBorder and AOptions.UseInsideVerticalBorder then
  begin
    Info.Borders.InsideVerticalBorder.CopyFrom(AInfo.Borders.InsideVerticalBorder);
    Options.UseInsideVerticalBorder := True;
  end;
  if not Options.UseLeftMargin and AOptions.UseLeftMargin then
  begin
    Info.CellMargins.Left.CopyFrom(AInfo.CellMargins.Left);
    Options.UseLeftMargin := True;
  end;
  if not Options.UseRightMargin and AOptions.UseRightMargin then
  begin
    Info.CellMargins.Right.CopyFrom(AInfo.CellMargins.Right);
    Options.UseRightMargin := True;
  end;
  if not Options.UseTopMargin and AOptions.UseTopMargin then
  begin
    Info.CellMargins.Top.CopyFrom(AInfo.CellMargins.Top);
    Options.UseTopMargin := True;
  end;
  if not Options.UseBottomMargin and AOptions.UseBottomMargin then
  begin
    Info.CellMargins.Bottom.CopyFrom(AInfo.CellMargins.Bottom);
    Options.UseBottomMargin := True;
  end;
  if not Options.UseCellSpacing and AOptions.UseCellSpacing then
  begin
    Info.CellSpacing.CopyFrom(AInfo.CellSpacing);
    Options.UseCellSpacing := True;
  end;
  if not Options.UseFloatingPosition and AOptions.UseFloatingPosition then
  begin
    Info.FloatingPosition.CopyFrom(AInfo.FloatingPosition);
    Options.UseFloatingPosition := True;
  end;
  if not Options.UseIsTableOverlap and AOptions.UseIsTableOverlap then
  begin
    Info.GeneralSettings.IsTableOverlap := AInfo.GeneralSettings.IsTableOverlap;
    Options.UseIsTableOverlap := True;
  end;
  if not Options.UseTableLayout and AOptions.UseTableLayout then
  begin
    Info.GeneralSettings.TableLayout := AInfo.GeneralSettings.TableLayout;
    Options.UseTableLayout := True;
  end;
  if not Options.UseTableLook and AOptions.UseTableLook then
  begin
    Info.GeneralSettings.TableLook := AInfo.GeneralSettings.TableLook;
    Options.UseTableLook := True;
  end;
  if not Options.UseTableStyleColBandSize and AOptions.UseTableStyleColBandSize then
  begin
    Info.GeneralSettings.TableStyleColBandSize := AInfo.GeneralSettings.TableStyleColBandSize;
    Options.UseTableStyleColBandSize := True;
  end;
  if not Options.UseTableStyleRowBandSize and AOptions.UseTableStyleRowBandSize then
  begin
    Info.GeneralSettings.TableStyleRowBandSize := AInfo.GeneralSettings.TableStyleRowBandSize;
    Options.UseTableStyleRowBandSize := True;
  end;
  if not Options.UseTableIndent and AOptions.UseTableIndent then
  begin
    Info.TableIndent.CopyFrom(AInfo.TableIndent);
    Options.UseTableIndent := True;
  end;
  if not Options.UsePreferredWidth and AOptions.UsePreferredWidth then
  begin
    Info.PreferredWidth.CopyFrom(AInfo.PreferredWidth);
    Options.UsePreferredWidth := True;
  end;
end;

{ TdxMergedTableCellProperties }

constructor TdxMergedTableCellProperties.Create(AInitialProperties: TdxTableCellProperties);
var
  AInfo: TdxCombinedCellPropertiesInfo;
begin
  AInfo := TdxCombinedCellPropertiesInfo.Create(AInitialProperties);
  try
    Create(AInfo, AInitialProperties.Info);
  finally
    AInfo.Free;
  end;
end;

constructor TdxMergedTableCellProperties.Create(AInitialProperties: TdxMergedTableCellProperties);
begin
  Create(AInitialProperties.Info, AInitialProperties.Options);
end;

procedure TdxMergedTableCellProperties.Merge(AProperties: TdxTableCellProperties);
var
  AInfo: TdxCombinedCellPropertiesInfo;
begin
  AInfo := TdxCombinedCellPropertiesInfo.Create(AProperties);
  try
    MergeCore(AInfo, AProperties.Info);
  finally
    AInfo.Free;
  end;
end;

procedure TdxMergedTableCellProperties.Merge(AProperties: TdxMergedTableCellProperties);
begin
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedTableCellProperties.MergeCore(const AInfo: TdxCombinedCellPropertiesInfo;
  const AOptions: TdxTableCellPropertiesOptions);
begin
  if not Options.UseBottomBorder and AOptions.UseBottomBorder then
  begin
    Info.Borders.BottomBorder.CopyFrom(AInfo.Borders.BottomBorder);
    Options.UseBottomBorder := True;
  end;
  if not Options.UseTopBorder and AOptions.UseTopBorder then
  begin
    Info.Borders.TopBorder.CopyFrom(AInfo.Borders.TopBorder);
    Options.UseTopBorder := True;
  end;
  if not Options.UseLeftBorder and AOptions.UseLeftBorder then
  begin
    Info.Borders.LeftBorder.CopyFrom(AInfo.Borders.LeftBorder);
    Options.UseLeftBorder := True;
  end;
  if not Options.UseRightBorder and AOptions.UseRightBorder then
  begin
    Info.Borders.RightBorder.CopyFrom(AInfo.Borders.RightBorder);
    Options.UseRightBorder := True;
  end;
  if not Options.UseInsideHorizontalBorder and AOptions.UseInsideHorizontalBorder then
  begin
    Info.Borders.InsideHorizontalBorder.CopyFrom(AInfo.Borders.InsideHorizontalBorder);
    Options.UseInsideHorizontalBorder := True;
  end;
  if not Options.UseInsideVerticalBorder and AOptions.UseInsideVerticalBorder then
  begin
    Info.Borders.InsideVerticalBorder.CopyFrom(AInfo.Borders.InsideVerticalBorder);
    Options.UseInsideVerticalBorder := True;
  end;
  if not Options.UseTopLeftDiagonalBorder and AOptions.UseTopLeftDiagonalBorder then
  begin
    Info.Borders.TopLeftDiagonalBorder.CopyFrom(AInfo.Borders.TopLeftDiagonalBorder);
    Options.UseTopLeftDiagonalBorder := True;
  end;
  if not Options.UseTopRightDiagonalBorder and AOptions.UseTopRightDiagonalBorder then
  begin
    Info.Borders.TopRightDiagonalBorder.CopyFrom(AInfo.Borders.TopRightDiagonalBorder);
    Options.UseTopRightDiagonalBorder := True;
  end;
  if not Options.UseTopMargin and AOptions.UseTopMargin then
  begin
    Info.CellMargins.Top.CopyFrom(AInfo.CellMargins.Top);
    Options.UseTopMargin := True;
  end;
  if not Options.UseLeftMargin and AOptions.UseLeftMargin then
  begin
    Info.CellMargins.Left.CopyFrom(AInfo.CellMargins.Left);
    Options.UseLeftMargin := True;
  end;
  if not Options.UseRightMargin and AOptions.UseRightMargin then
  begin
    Info.CellMargins.Right.CopyFrom(AInfo.CellMargins.Right);
    Options.UseRightMargin := True;
  end;
  if not Options.UseBottomMargin and AOptions.UseBottomMargin then
  begin
    Info.CellMargins.Bottom.CopyFrom(AInfo.CellMargins.Bottom);
    Options.UseBottomMargin := True;
  end;
  if not Options.UseBackgroundColor and AOptions.UseBackgroundColor then
  begin
    Info.GeneralSettings.BackgroundColor := AInfo.GeneralSettings.BackgroundColor;
    Options.UseBackgroundColor := True;
  end;
  if not Options.UseCellConditionalFormatting and AOptions.UseCellConditionalFormatting then
  begin
    Info.GeneralSettings.CellConditionalFormatting := AInfo.GeneralSettings.CellConditionalFormatting;
    Options.UseCellConditionalFormatting := True;
  end;
  if not Options.UseFitText and AOptions.UseFitText then
  begin
    Info.GeneralSettings.FitText := AInfo.GeneralSettings.FitText;
    Options.UseFitText := True;
  end;
  if not Options.UseHideCellMark and AOptions.UseHideCellMark then
  begin
    Info.GeneralSettings.HideCellMark := AInfo.GeneralSettings.HideCellMark;
    Options.UseHideCellMark := True;
  end;
  if not Options.UseNoWrap and AOptions.UseNoWrap then
  begin
    Info.GeneralSettings.NoWrap := AInfo.GeneralSettings.NoWrap;
    Options.UseNoWrap := True;
  end;
  if not Options.UseTextDirection and AOptions.UseTextDirection then
  begin
    Info.GeneralSettings.TextDirection := AInfo.GeneralSettings.TextDirection;
    Options.UseTextDirection := True;
  end;
  if not Options.UseVerticalAlignment and AOptions.UseVerticalAlignment then
  begin
    Info.GeneralSettings.VerticalAlignment := AInfo.GeneralSettings.VerticalAlignment;
    Options.UseVerticalAlignment := True;
  end;
  if not Options.UsePreferredWidth and AOptions.UsePreferredWidth then
  begin
    Info.PreferredWidth.CopyFrom(AInfo.PreferredWidth);
    Options.UsePreferredWidth := True;
  end;
end;

{ TdxMergedTableRowProperties }

constructor TdxMergedTableRowProperties.Create(AInitialProperties: TdxTableRowProperties);
var
  AInfo: TdxCombinedTableRowPropertiesInfo;
begin
  AInfo := TdxCombinedTableRowPropertiesInfo.Create(AInitialProperties);
  try
    Create(AInfo, AInitialProperties.Info);
  finally
    AInfo.Free;
  end;
end;

constructor TdxMergedTableRowProperties.Create(AInitialProperties: TdxMergedTableRowProperties);
begin
  Create(AInitialProperties.Info, AInitialProperties.Options);
end;

procedure TdxMergedTableRowProperties.Merge(AProperties: TdxTableRowProperties);
var
  AInfo: TdxCombinedTableRowPropertiesInfo;
begin
  AInfo := TdxCombinedTableRowPropertiesInfo.Create(AProperties);
  try
    MergeCore(AInfo, AProperties.Info);
  finally
    AInfo.Free;
  end;
end;

procedure TdxMergedTableRowProperties.Merge(AProperties: TdxMergedTableRowProperties);
begin
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedTableRowProperties.MergeCore(const AInfo: TdxCombinedTableRowPropertiesInfo;
  const AOptions: TdxTableRowPropertiesOptions);
begin
  if not Options.UseCellSpacing and AOptions.UseCellSpacing then
  begin
    Info.CellSpacing.CopyFrom(AInfo.CellSpacing);
    Options.UseCellSpacing := True;
  end;
  if not Options.UseHeight and AOptions.UseHeight then
  begin
    Info.Height.CopyFrom(AInfo.Height);
    Options.UseHeight := True;
  end;
  if not Options.UseWidthBefore and AOptions.UseWidthBefore then
  begin
    Info.WidthBefore.CopyFrom(AInfo.WidthBefore);
    Options.UseWidthBefore := True;
  end;
  if not Options.UseWidthAfter and AOptions.UseWidthAfter then
  begin
    Info.WidthAfter.CopyFrom(AInfo.WidthAfter);
    Options.UseWidthAfter := True;
  end;
  if not Options.UseCantSplit and AOptions.UseCantSplit then
  begin
    Info.GeneralSettings.CantSplit := AInfo.GeneralSettings.CantSplit;
    Options.UseCantSplit := True;
  end;
  if not Options.UseGridAfter and AOptions.UseGridAfter then
  begin
    Info.GeneralSettings.GridAfter := AInfo.GeneralSettings.GridAfter;
    Options.UseGridAfter := True;
  end;
  if not Options.UseGridBefore and AOptions.UseGridBefore then
  begin
    Info.GeneralSettings.GridBefore := AInfo.GeneralSettings.GridBefore;
    Options.UseGridBefore := True;
  end;
  if not Options.UseHeader and AOptions.UseHeader then
  begin
    Info.GeneralSettings.Header := AInfo.GeneralSettings.Header;
    Options.UseHeader := True;
  end;
  if not Options.UseHideCellMark and AOptions.UseHideCellMark then
  begin
    Info.GeneralSettings.HideCellMark := AInfo.GeneralSettings.HideCellMark;
    Options.UseHideCellMark := True;
  end;
  if not Options.UseTableRowAlignment and AOptions.UseTableRowAlignment then
  begin
    Info.GeneralSettings.TableRowAlignment := AInfo.GeneralSettings.TableRowAlignment;
    Options.UseTableRowAlignment := True;
  end;
end;

end.
