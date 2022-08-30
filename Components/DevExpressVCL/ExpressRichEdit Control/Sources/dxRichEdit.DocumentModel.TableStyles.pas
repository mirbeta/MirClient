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

unit dxRichEdit.DocumentModel.TableStyles;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting;

type
  TdxTableStyle = class;
  TdxTableConditionalStyle = class;
  TdxTableConditionalStyleProperties = class;
  TdxTableStyleCollection = class;
  TdxTableCellStyleCollection = class;

  { IdxTableStyle }

  IdxTableStyle = interface
  ['{0747EB4A-AAC0-44B4-9A30-8F711DC88686}']
    function GetTableProperties: TdxTableProperties;
    function GetTableRowProperties: TdxTableRowProperties;
    function GetTableCellProperties: TdxTableCellProperties;
    function GetCharacterProperties: TdxCharacterProperties;
    function GetParagraphProperties: TdxParagraphProperties;
    function GetParent: TdxTableStyle;

    property TableProperties: TdxTableProperties read GetTableProperties;
    property TableRowProperties: TdxTableRowProperties read GetTableRowProperties;
    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property Parent: TdxTableStyle read GetParent;
  end;

  { IdxTableStylesContainer }

  IdxTableStylesContainer = interface
  ['{8FEDBE80-414F-439A-9CB7-708DE541ABBF}']
    function GetTableStyles: TdxTableStyleCollection;
    function GetTableCellStyles: TdxTableCellStyleCollection;

    property TableStyles: TdxTableStyleCollection read GetTableStyles;
    property TableCellStyles: TdxTableCellStyleCollection read GetTableCellStyles;
  end;

  { TdxTableStyle }

  TdxTableStyle = class(TdxParagraphPropertiesBasedStyle, IdxCharacterPropertiesContainer, IdxCellPropertiesOwner, IdxTableStyle)
  protected type
    TdxMaskPrediacate = reference to function(AConditionalStyle: TdxTableConditionalStyle): Boolean;
  private
    FTableProperties: TdxTableProperties;
    FTableRowProperties: TdxTableRowProperties;
    FTableCellProperties: TdxTableCellProperties;
    FCharacterProperties: TdxCharacterProperties;

    FConditionalStyleProperties: TdxTableConditionalStyleProperties;

    function GetTableProperties: TdxTableProperties; overload;
    function GetConditionalStyleProperties: TdxTableConditionalStyleProperties;
    function GetTableRowProperties: TdxTableRowProperties; overload;
    function GetTableCellProperties: TdxTableCellProperties; overload;
    function GetCharacterProperties: TdxCharacterProperties; overload;
    function GetHasColumnBandingStyleProperties: Boolean;
    function GetHasRowBandingStyleProperties: Boolean;
    function GetHasConditionalStyleProperties: Boolean;
    function GetParent: TdxTableStyle;
    procedure SetParent(const Value: TdxTableStyle);
  protected
    function GetType: TdxStyleType; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AParent: TdxTableStyle = nil; const AStyleName: string = ''); reintroduce;
    destructor Destroy; override;

    procedure SubscribeCharacterPropertiesEvents; virtual;
    procedure SubscribeTablePropertiesPropertiesEvents; virtual;
    procedure SubscribeTableRowPropertiesEvents; virtual;
    procedure SubscribeTableCellPropertiesEvents; virtual;
    procedure OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
    procedure MergeConditionalProperties(AMergedProperties: TdxMergedCharacterProperties; ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType);
    function GetConditionalPropertiesSource(ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType; AMask: Integer;
      AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableConditionalStyle; overload;
    procedure TryGetConditionalStyle(AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
      AStyleType: TdxConditionalTableStyleFormattingType; out AResult: TdxTableConditionalStyle; const AMaskPredicate: TdxMaskPrediacate); overload;
    procedure TryGetConditionalStyle(AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
      AStyleType: TdxConditionalTableStyleFormattingType; out AResult: TdxTableConditionalStyle; AMask: Integer); overload;
    function GetConditionalPropertiesSource(ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType;
      const AMaskPredicate: TdxMaskPrediacate): TdxTableConditionalStyle; overload;
    function GetConditionalPropertiesMask(ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxConditionalTableStyleFormattingTypes;
    function GetTableProperties(AMask: Integer; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxTableProperties; overload;
    function GetTableProperties(AMask: Integer; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType; AIsBorderCell: Boolean;
        out AInsideBorder: Boolean): TdxTableProperties; overload; virtual;
    function GetMergedCharacterProperties(ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties; overload; virtual;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties; overload; virtual;
    function GetMergedWithDefaultCharacterProperties(ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties; virtual;
    function GetMergedTableProperties: TdxMergedTableProperties; virtual;
    function GetMergedWithDefaultTableProperties: TdxMergedTableProperties; virtual;
    function GetMergedWithDefaultTableRowProperties: TdxMergedTableRowProperties; virtual;
    function GetMergedWithDefaultTableCellProperties: TdxMergedTableCellProperties; virtual;
    function GetTableCellProperties(AMask: Integer; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxTableCellProperties; overload; virtual;
    function GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxCharacterProperties; overload; virtual;
    function GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxParagraphProperties; virtual;
    function GetMergedParagraphProperties(ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxMergedParagraphProperties; reintroduce; overload; virtual;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; overload; override;
    function GetTableRowProperties(AMask: Integer;
      ARowType: TdxConditionalRowType): TdxTableRowProperties; overload; virtual;
    procedure MergePropertiesWithParent; override;
    procedure CopyProperties(ASource: TdxStyleBase); override;
    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; override;
    function CopyTo(ATargetModel: TdxCustomDocumentModel): TdxTableStyle; virtual;
    procedure ApplyPropertiesDiff(AStyle: TdxTableStyle);
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;

    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType); override;
    function HasColumnStyle(AConditionalColumnType: TdxConditionalColumnType): Boolean;
    function HasRowStyle(AConditionalRowType: TdxConditionalRowType): Boolean;
    function GetMergedCharacterProperties(AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedCharacterProperties; overload;
    function GetMergedParagraphProperties(AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedParagraphProperties; reintroduce; overload;
    function GetMergedTableRowProperties: TdxMergedTableRowProperties; overload; virtual;
    function GetMergedTableRowProperties(ARowType: TdxConditionalRowType): TdxMergedTableRowProperties; overload;
    function GetMergedTableRowProperties(AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedTableRowProperties; overload;
    function GetMergedTableCellProperties: TdxMergedTableCellProperties; overload; virtual;
    function GetMergedTableCellProperties(ARowType: TdxConditionalRowType; AColType: TdxConditionalColumnType): TdxMergedTableCellProperties; overload;
    function GetMergedTableCellProperties(AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedTableCellProperties; overload;

    property Parent: TdxTableStyle read GetParent write SetParent;
    property HasConditionalStyleProperties: Boolean read GetHasConditionalStyleProperties;
    property HasRowBandingStyleProperties: Boolean read GetHasRowBandingStyleProperties;
    property HasColumnBandingStyleProperties: Boolean read GetHasColumnBandingStyleProperties;
    property ConditionalStyleProperties: TdxTableConditionalStyleProperties read GetConditionalStyleProperties;
    property TableProperties: TdxTableProperties read GetTableProperties;
    property TableRowProperties: TdxTableRowProperties read GetTableRowProperties;
    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
  end;

  { TdxTableStyleCollection }

  TdxTableStyleCollection = class(TdxStyleCollectionBase)
  public const
    DefaultTableStyleIndex = 0;
    TableSimpleStyleName = 'Table Simple 1';
    DefaultTableStyleName = 'Normal Table';
  private const
    FDefaultLeftMargin  = 108;
    FDefaultRightMargin = 108;
    FDefaultBorderWidth = 10;
    FDefaultBorderLineStyle = TdxBorderLineStyle.Single;
    function GetItem(Index: Integer): TdxTableStyle;
  protected
    function CreateNormalTableStyle(AChangeDefaultTableStyle: Boolean): TdxTableStyle; reintroduce; virtual;
    function CreateTableSimpleStyle: TdxTableStyle; virtual;
    function CreateDefaultItem: TdxStyleBase; override;
    procedure SetDefaultBorder(ABorder: TdxBorderBase; AStyle: TdxBorderLineStyle; AWidthInTwips: Integer);
    procedure SetDefaultMargin(AMargin: TdxWidthUnit; AValueInTwips: Integer);
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase); override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AChangeDefaultTableStyle: Boolean); reintroduce;

    property Self[Index: Integer]: TdxTableStyle read GetItem; default;
  end;

  { TdxTableConditionalStyle }

  TdxTableConditionalStyle = class(TcxIUnknownObject, IdxCellPropertiesOwner, IdxCharacterPropertiesContainer,
    IdxParagraphPropertiesContainer, IdxTableStyle)
  private
    FConditionType: TdxConditionalTableStyleFormattingType;
    FOwner: TdxTableStyle;
    FTableProperties: TdxTableProperties;
    FTableRowProperties: TdxTableRowProperties;
    FTableCellProperties: TdxTableCellProperties;
    FParagraphProperties: TdxParagraphProperties;
    FTabs: TdxTabProperties;
    FCharacterProperties: TdxCharacterProperties;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetInnerPieceTable: TdxCustomPieceTable; overload;
    function GetPieceTable: TdxCustomPieceTable; overload;
    function GetTableProperties: TdxTableProperties; overload;
    function GetTableRowProperties: TdxTableRowProperties; overload;
    function GetTableCellProperties: TdxTableCellProperties; overload;
    function GetCharacterProperties: TdxCharacterProperties;
    function GetParagraphProperties: TdxParagraphProperties;
    function GetTabs: TdxTabProperties;
    function GetParent: TdxTableStyle;
  public
    constructor Create(AOwner: TdxTableStyle; AConditionType: TdxConditionalTableStyleFormattingType);
    destructor Destroy; override;

    function GetTableProperties(AMask: Integer): TdxTableProperties; overload; virtual;
    function GetTableRowProperties(AMask: Integer): TdxTableRowProperties; overload; virtual;
    function GetTableCellProperties(AMask: Integer): TdxTableCellProperties; overload; virtual;
    procedure CopyFrom(ACondition: TdxTableConditionalStyle);
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;

    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;
    function CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnParagraphPropertiesChanged;
    function GetMergedCharacterProperties: TdxMergedCharacterProperties;
    function GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties; virtual;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties; virtual;
    function GetMergedTableProperties: TdxMergedTableProperties; virtual;
    function GetMergedTableRowProperties: TdxMergedTableRowProperties; virtual;
    function GetMergedTableCellProperties: TdxMergedTableCellProperties; virtual;

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property PieceTable: TdxCustomPieceTable read GetInnerPieceTable;
    property ConditionType: TdxConditionalTableStyleFormattingType read FConditionType;

    property TableProperties: TdxTableProperties read GetTableProperties;
    property TableRowProperties: TdxTableRowProperties read GetTableRowProperties;
    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;

    property Parent: TdxTableStyle read GetParent;
    property Tabs: TdxTabProperties read GetTabs;
  end;

  { TdxTableConditionalStyleProperties }

  TdxTableConditionalStyleProperties = class
  public type
    TStyleTypes = array[TdxConditionalTableStyleFormattingType] of TdxConditionalTableStyleFormattingType;
  protected
    class var FStyleTypes: TStyleTypes;
    class constructor Initialize;
  private
    FItems: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
    FOwner: TdxTableStyle;
    function GetDocumentModel: TdxCustomDocumentModel;
    function GetItem(ACondition: TdxConditionalTableStyleFormattingType): TdxTableConditionalStyle;
  protected
    procedure CopyConditionalStyle(ACondition: TdxConditionalTableStyleFormattingType;
      AConditionalProperties: TdxTableConditionalStyleProperties);
    procedure InitializeItems(ADocumentModel: TdxCustomDocumentModel);
    procedure AddConditionalStyle(ADocumentModel: TdxCustomDocumentModel; ACondition: TdxConditionalTableStyleFormattingType);
  public
    constructor Create(AOwner: TdxTableStyle);
    destructor Destroy; override;

    function GetStyleSafe(ACondition: TdxConditionalTableStyleFormattingType): TdxTableConditionalStyle;
    procedure AddStyle(AStyle: TdxTableConditionalStyle);
    procedure CopyFrom(AConditionalProperties: TdxTableConditionalStyleProperties);
    function ContainsStyle(AConditionalTableStyleFormattingType: TdxConditionalTableStyleFormattingType): Boolean; overload;
    function HasNonNullStyle: Boolean;

    procedure ForEachStyle(const Action: TdxAction<TdxTableConditionalStyle>);

    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
    property Self[ACondition: TdxConditionalTableStyleFormattingType]: TdxTableConditionalStyle read GetItem; default;
    property Items: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle> read FItems;
    property Owner: TdxTableStyle read FOwner;
    class property StyleTypes: TStyleTypes read FStyleTypes;
  end;

  { IdxTableCellStyle }

  TdxTableCellStyle = class;

  IdxTableCellStyle = interface
  ['{B93BD106-3B85-4100-A8D1-8D988C0796DC}']
    function GetTableCellProperties: TdxTableCellProperties;
    function GetCharacterProperties: TdxCharacterProperties;
    function GetParagraphProperties: TdxParagraphProperties;
    function GetParent: TdxTableCellStyle;

    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property Parent: TdxTableCellStyle read GetParent;
  end;

  { TdxTableCellStyle }

  TdxTableCellStyle = class(TdxParagraphPropertiesBasedStyle, IdxCharacterPropertiesContainer,
    IdxCellPropertiesOwner, IdxTableCellStyle)
  private
    FTableCellProperties: TdxTableCellProperties;
    FCharacterProperties: TdxCharacterProperties;
    function GetTableCellProperties: TdxTableCellProperties; overload;
    function GetParentStyle: TdxTableCellStyle;
    procedure SetParentStyle(const Value: TdxTableCellStyle);
  strict protected
    function GetCharacterProperties: TdxCharacterProperties; overload;
    function GetParagraphProperties: TdxParagraphProperties; overload;
    function GetParent: TdxTableCellStyle;
  protected
    function GetType: TdxStyleType; override;

    procedure SubscribeCharacterPropertiesEvents; virtual;
    procedure SubscribeTableCellPropertiesEvents; virtual;
    procedure OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs); virtual;
    procedure MergePropertiesWithParent; override;
    function CopyTo(ATargetModel: TdxCustomDocumentModel): TdxTableCellStyle; virtual;
    procedure ApplyPropertiesDiff(AStyle: TdxTableCellStyle);
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
    function GetMergedTableRowProperties(AConditionType: TdxConditionalTableStyleFormattingTypes): TdxMergedTableRowProperties;
    function GetMergedTableCellProperties(ARowType: TdxConditionalRowType; AColType: TdxConditionalColumnType): TdxMergedTableCellProperties; overload;
    function GetMergedTableCellProperties(AConditionType: TdxConditionalTableStyleFormattingTypes): TdxMergedTableCellProperties; overload;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel;
      AParent: TdxTableCellStyle = nil; const AStyleName: string = ''); reintroduce;
    destructor Destroy; override;

    procedure CopyProperties(ASource: TdxStyleBase); override;

    function GetMergedCharacterProperties(ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties; virtual;
    function GetMergedWithDefaultCharacterProperties(ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties; virtual;
    function GetMergedWithDefaultTableCellProperties: TdxMergedTableCellProperties; virtual;
    function GetTableCellProperties(AMask: Integer): TdxTableCellProperties; overload; virtual;
    function GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxCharacterProperties; overload; virtual;
    function GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxParagraphProperties; overload; virtual;
    function GetMergedParagraphProperties(ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType): TdxMergedParagraphProperties; reintroduce; overload; virtual;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties; overload; override;
    function Copy(ATargetModel: TdxCustomDocumentModel): Integer; override;

    property Parent: TdxTableCellStyle read GetParentStyle write SetParentStyle;
    //IdxCharacterPropertiesContainer
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;
    procedure ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType); override;
    function GetMergedTableCellProperties: TdxMergedTableCellProperties; overload; virtual;

    property TableCellProperties: TdxTableCellProperties read GetTableCellProperties;
    property CharacterProperties: TdxCharacterProperties read FCharacterProperties;
  end;

  { TdxTableCellStyleCollection }

  TdxTableCellStyleCollection = class(TdxStyleCollectionBase)
  strict private
    const DefaultLeftMargin = 108;
    const DefaultRightMargin = 108;
    const DefaultBorderWidth = 10;
    const DefaultBorderLineStyle = TdxBorderLineStyle.Single;
  public
    const DefaultTableCellStyleIndex = 0;
    class var TableCellSimpleStyleName: string;
    class var DefaultTableCellStyleName: string;
  strict protected
    class constructor Initialize;
  private
    procedure SetDefaultBorder(ABorder: TdxBorderBase; AStyle: TdxBorderLineStyle; AWidthInTwips: Integer);
    procedure SetDefaultMargin(AMargin: TdxWidthUnit; AValueInTwips: Integer);
    function GetItem(Index: Integer): TdxTableCellStyle;
  protected
    function CreateNormalTableCellStyle(AChangeDefaultTableCellStyle: Boolean): TdxTableCellStyle; virtual;
    function CreateTableSimpleStyle: TdxTableCellStyle; virtual;
    function CreateDefaultItem: TdxStyleBase; override;
    procedure NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase); override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AChangeDefaultTableCellStyle: Boolean); reintroduce;

    property Self[Index: Integer]: TdxTableCellStyle read GetItem; default;
  end;

implementation

uses
  RTLConsts, Contnrs,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.History.Table;

type
  TdxTablePropertiesAccess = class(TdxTableProperties);
  TdxTableCellPropertiesAccess = class(TdxTableCellProperties);

{ TdxTableStyle }

constructor TdxTableStyle.Create(ADocumentModel: TdxCustomDocumentModel;
  AParent: TdxTableStyle = nil; const AStyleName: string = '');
begin
  inherited Create(ADocumentModel, AParent, AStyleName);
  FCharacterProperties := TdxCharacterProperties.Create(Self);
  SubscribeCharacterPropertiesEvents;
end;

destructor TdxTableStyle.Destroy;
begin
  FreeAndNil(FConditionalStyleProperties);
  FreeAndNil(FTableProperties);
  FreeAndNil(FTableRowProperties);
  FreeAndNil(FTableCellProperties);
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

procedure TdxTableStyle.ApplyPropertiesDiff(AStyle: TdxTableStyle);
var
  AStyleMergedWithDefaultParagraphProperties, AMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties;
  AStyleMergedWithDefaultCharacterProperties, AMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  AStyleMergedWithDefaultParagraphProperties := AStyle.GetMergedWithDefaultParagraphProperties;
  try
    AMergedWithDefaultParagraphProperties := GetMergedWithDefaultParagraphProperties;
    try
      ParagraphProperties.ApplyPropertiesDiff(AStyle.ParagraphProperties,
        AStyleMergedWithDefaultParagraphProperties.Info, AMergedWithDefaultParagraphProperties.Info);
    finally
      AMergedWithDefaultParagraphProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultParagraphProperties.Free;
  end;
  AStyleMergedWithDefaultCharacterProperties := AStyle.GetMergedWithDefaultCharacterProperties(
    TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
  try
    AMergedWithDefaultCharacterProperties := GetMergedWithDefaultCharacterProperties(
      TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
    try
      CharacterProperties.ApplyPropertiesDiff(AStyle.CharacterProperties,
        AStyleMergedWithDefaultCharacterProperties.Info,
        AMergedWithDefaultCharacterProperties.Info);
    finally
      AMergedWithDefaultCharacterProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultCharacterProperties.Free;
  end;
end;

function TdxTableStyle.Copy(ATargetModel: TdxCustomDocumentModel): Integer;
var
  I: Integer;
  ATableStyles: TdxTableStyleCollection;
  AContainer: IdxTableStylesContainer;
begin
  AContainer := ATargetModel as IdxTableStylesContainer;
  ATableStyles := AContainer.TableStyles;
  for I := 0 to ATableStyles.Count - 1 do
    if StyleName = ATableStyles[I].StyleName then
      Exit(I);
  Result := ATableStyles.AddNewStyle(CopyTo(ATargetModel));
end;

procedure TdxTableStyle.CopyProperties(ASource: TdxStyleBase);
var
  AInnerSource: TdxTableStyle absolute ASource;
begin
  Assert(ASource is TdxTableStyle);
  inherited CopyProperties(ASource);
  TdxTableConditionalFormattingController.ResetTablesCachedProperties(DocumentModel);
  TableProperties.CopyFrom(AInnerSource.TableProperties);
  TableRowProperties.CopyFrom(AInnerSource.TableRowProperties);
  TableCellProperties.CopyFrom(AInnerSource.TableCellProperties);
  CharacterProperties.CopyFrom(AInnerSource.CharacterProperties.Info);
end;

function TdxTableStyle.CopyTo(ATargetModel: TdxCustomDocumentModel): TdxTableStyle;
var
  AStyle: TdxTableStyle;
  AContainer: IdxTableStylesContainer;
begin
  AContainer := ATargetModel as IdxTableStylesContainer;
  AStyle := TdxTableStyle.Create(ATargetModel);
  AStyle.StyleName := StyleName;
  AStyle.CopyProperties(Self);
  if HasConditionalStyleProperties then
    AStyle.ConditionalStyleProperties.CopyFrom(ConditionalStyleProperties);
  if Parent <> nil then
    AStyle.Parent := AContainer.TableStyles[Parent.Copy(ATargetModel)];
  ApplyPropertiesDiff(AStyle);
  Result := AStyle;
end;

function TdxTableStyle.CreateCellPropertiesChangedHistoryItem(
  AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = TableCellProperties);
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable, AProperties);
end;

function TdxTableStyle.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, CharacterProperties);
end;

function TdxTableStyle.GetConditionalPropertiesSource(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType; AMask: Integer; AIsBorderCell: Boolean;
  out AInsideBorder: Boolean): TdxTableConditionalStyle;
var
  ARowMask, AColumnMask: Integer;
  ARowConditionBorder, AColumnConditionBorder: Boolean;
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
begin
  Result := nil;
  AInsideBorder := not AIsBorderCell;
  if FConditionalStyleProperties = nil then
    Exit;
  AConditionalStyles := FConditionalStyleProperties.Items;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if ARowType = TdxConditionalRowType.FirstRow then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.TopLeftCell, Result, AMask);
      if Result <> nil then
      begin
        AInsideBorder := False;
        Exit;
      end;
    end
    else
      if ARowType = TdxConditionalRowType.LastRow then
      begin
        TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.BottomLeftCell, Result, AMask);
        if Result <> nil then
        begin
          AInsideBorder := False;
          Exit;
        end;
      end;
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      if ARowType = TdxConditionalRowType.FirstRow then
      begin
        TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.TopRightCell, Result, AMask);
        if Result <> nil then
        begin
          AInsideBorder := False;
          Exit;
        end;
      end
      else
        if ARowType = TdxConditionalRowType.LastRow then
        begin
          TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.BottomRightCell, Result, AMask);
          if Result <> nil then
          begin
            AInsideBorder := False;
            Exit;
          end;
        end;
    end;
  ARowConditionBorder := AIsBorderCell or ((AMask and (TdxTablePropertiesOptions.MaskUseBottomBorder or TdxTablePropertiesOptions.MaskUseTopBorder)) <> 0);
  AColumnConditionBorder := AIsBorderCell or ((AMask and (TdxTablePropertiesOptions.MaskUseLeftBorder or TdxTablePropertiesOptions.MaskUseRightBorder)) <> 0);
  ARowMask := TdxTablePropertiesOptions.OuterOrInside(AMask, ARowConditionBorder);
  AColumnMask := TdxTablePropertiesOptions.OuterOrInside(AMask, AColumnConditionBorder);

  if ARowType = TdxConditionalRowType.FirstRow then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.FirstRow, Result, ARowMask);
    if Result <> nil then
    begin
      AInsideBorder := not ARowConditionBorder;
      Exit;
    end;
  end
  else
    if ARowType = TdxConditionalRowType.LastRow then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.LastRow, Result, ARowMask);
      if Result <> nil then
      begin
        AInsideBorder := not ARowConditionBorder;
        Exit;
      end;
    end;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.FirstColumn, Result, AColumnMask);
    if Result <> nil then
    begin
      AInsideBorder := not AColumnConditionBorder;
      Exit;
    end;
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.LastColumn, Result, AColumnMask);
      if Result <> nil then
      begin
        AInsideBorder := not AColumnConditionBorder;
        Exit;
      end;
    end
    else
      if AColumnType = TdxConditionalColumnType.EvenColumnBand then
      begin
        TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.EvenColumnBanding, Result, AColumnMask);
        if Result <> nil then
        begin
          AInsideBorder := not AColumnConditionBorder;
          Exit;
        end;
      end
      else
        if AColumnType = TdxConditionalColumnType.OddColumnBand then
        begin
          TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.OddColumnBanding, Result, AColumnMask);
          if Result <> nil then
          begin
            AInsideBorder := not AColumnConditionBorder;
            Exit;
          end;
        end;
  if ARowType = TdxConditionalRowType.EvenRowBand then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.EvenRowBanding, Result, ARowMask);
    if Result <> nil then
    begin
      AInsideBorder := not ARowConditionBorder;
      Exit;
    end;
  end
  else
    if ARowType = TdxConditionalRowType.OddRowBand then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.OddRowBanding, Result, ARowMask);
      if Result <> nil then
      begin
        AInsideBorder := not ARowConditionBorder;
        Exit;
      end;
    end;
  TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.WholeTable, Result, TdxTablePropertiesOptions.OuterOrInside(AMask, AIsBorderCell));
end;

function TdxTableStyle.GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxCharacterProperties;
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AConditionalStyle := GetConditionalPropertiesSource(ARowType, AColumnType, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      Result := AStyle.CharacterProperties.UseVal(AMask);
    end);
  if AConditionalStyle <> nil then
    Exit(AConditionalStyle.CharacterProperties);
  if (FCharacterProperties <> nil) and FCharacterProperties.UseVal(AMask) then
    Exit(FCharacterProperties);
  if Parent <> nil then
    Result := Parent.GetCharacterProperties(AMask, ARowType, AColumnType)
  else
    Result := nil;
end;

function TdxTableStyle.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := FCharacterProperties;
end;

function TdxTableStyle.GetConditionalPropertiesMask(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxConditionalTableStyleFormattingTypes;
var
  AStyle: TdxTableConditionalStyle;
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
begin
  Result := [];
  if FConditionalStyleProperties = nil then
    Exit;
  AConditionalStyles := FConditionalStyleProperties.Items;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if ARowType = TdxConditionalRowType.FirstRow then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.TopLeftCell, AStyle) and (AStyle <> nil) then
        Include(Result, TdxConditionalTableStyleFormattingType.TopLeftCell);
    end
    else
      if ARowType = TdxConditionalRowType.LastRow then
        if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.BottomLeftCell, AStyle) and (AStyle <> nil) then
          Include(Result, TdxConditionalTableStyleFormattingType.BottomLeftCell);
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      if ARowType = TdxConditionalRowType.FirstRow then
      begin
        if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.TopRightCell, AStyle) and (AStyle <> nil) then
          Include(Result, TdxConditionalTableStyleFormattingType.TopRightCell);
      end
      else
        if ARowType = TdxConditionalRowType.LastRow then
          if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.BottomRightCell, AStyle) and (AStyle <> nil) then
            Include(Result, TdxConditionalTableStyleFormattingType.BottomRightCell);
    end;
  if ARowType = TdxConditionalRowType.FirstRow then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.FirstRow, AStyle) and (AStyle <> nil) then
      Include(Result, TdxConditionalTableStyleFormattingType.FirstRow);
  end
  else
    if ARowType = TdxConditionalRowType.LastRow then
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.LastRow, AStyle) and (AStyle <> nil) then
        Include(Result, TdxConditionalTableStyleFormattingType.LastRow);
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.FirstColumn, AStyle) and (AStyle <> nil) then
      Include(Result, TdxConditionalTableStyleFormattingType.FirstColumn);
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.LastColumn, AStyle) and (AStyle <> nil) then
        Include(Result, TdxConditionalTableStyleFormattingType.LastColumn);
  if AColumnType = TdxConditionalColumnType.EvenColumnBand then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.EvenColumnBanding, AStyle) and (AStyle <> nil) then
      Include(Result, TdxConditionalTableStyleFormattingType.EvenColumnBanding);
  end
  else
    if AColumnType = TdxConditionalColumnType.OddColumnBand then
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.OddColumnBanding, AStyle) and (AStyle <> nil) then
        Include(Result, TdxConditionalTableStyleFormattingType.OddColumnBanding);
  if ARowType = TdxConditionalRowType.EvenRowBand then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.EvenRowBanding, AStyle) and (AStyle <> nil) then
      Include(Result, TdxConditionalTableStyleFormattingType.EvenRowBanding);
  end
  else
    if ARowType = TdxConditionalRowType.OddRowBand then
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.OddRowBanding, AStyle) and (AStyle <> nil) then
        Include(Result, TdxConditionalTableStyleFormattingType.OddRowBanding);
  if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.WholeTable, AStyle) and (AStyle <> nil) then
    Include(Result, TdxConditionalTableStyleFormattingType.WholeTable);
end;

function TdxTableStyle.GetConditionalPropertiesSource(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType; const AMaskPredicate: TdxMaskPrediacate): TdxTableConditionalStyle;
var
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
begin
  Result := nil;
  if FConditionalStyleProperties = nil then
    Exit;
  AConditionalStyles := FConditionalStyleProperties.Items;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if ARowType = TdxConditionalRowType.FirstRow then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.TopLeftCell, Result, AMaskPredicate);
      if Result <> nil then
        Exit;
    end
    else
      if ARowType = TdxConditionalRowType.LastRow then
      begin
          TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.BottomLeftCell, Result, AMaskPredicate);
          if Result <> nil then
            Exit;
      end;
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      if ARowType = TdxConditionalRowType.FirstRow then
      begin
        TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.TopRightCell, Result, AMaskPredicate);
        if Result <> nil then
          Exit;
      end
      else
        if ARowType = TdxConditionalRowType.LastRow then
        begin
          TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.BottomRightCell, Result, AMaskPredicate);
          if Result <> nil then
            Exit;
        end;
    end;
  if ARowType = TdxConditionalRowType.FirstRow then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.FirstRow, Result, AMaskPredicate);
    if Result <> nil then
      Exit;
  end
  else
    if ARowType = TdxConditionalRowType.LastRow then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.LastRow, Result, AMaskPredicate);
      if Result <> nil then
        Exit;
    end;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.FirstColumn, Result, AMaskPredicate);
    if Result <> nil then
      Exit;
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.LastColumn, Result, AMaskPredicate);
      if Result <> nil then
        Exit;
    end;
  if AColumnType = TdxConditionalColumnType.EvenColumnBand then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.EvenColumnBanding, Result, AMaskPredicate);
    if Result <> nil then
      Exit;
  end
  else
    if AColumnType = TdxConditionalColumnType.OddColumnBand then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.OddColumnBanding, Result, AMaskPredicate);
      if Result <> nil then
        Exit;
    end;
  if ARowType = TdxConditionalRowType.EvenRowBand then
  begin
    TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.EvenRowBanding, Result, AMaskPredicate);
    if Result <> nil then
      Exit;
  end
  else
    if ARowType = TdxConditionalRowType.OddRowBand then
    begin
      TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.OddRowBanding, Result, AMaskPredicate);
      if Result <> nil then
        Exit;
    end;
  TryGetConditionalStyle(AConditionalStyles, TdxConditionalTableStyleFormattingType.WholeTable, Result, AMaskPredicate);
end;

function TdxTableStyle.GetConditionalStyleProperties: TdxTableConditionalStyleProperties;
begin
  if FConditionalStyleProperties = nil then
    FConditionalStyleProperties := TdxTableConditionalStyleProperties.Create(Self);
  Result := FConditionalStyleProperties;
end;

function TdxTableStyle.GetHasColumnBandingStyleProperties: Boolean;
begin
  Result := (FConditionalStyleProperties <> nil) and
    (FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.OddColumnBanding) or
      FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.EvenColumnBanding));
end;

function TdxTableStyle.GetHasConditionalStyleProperties: Boolean;
begin
  Result := (FConditionalStyleProperties <> nil) and FConditionalStyleProperties.HasNonNullStyle;
end;

function TdxTableStyle.GetHasRowBandingStyleProperties: Boolean;
begin
  Result := (FConditionalStyleProperties <> nil) and
    (FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.OddRowBanding) or
      FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.EvenRowBanding));
end;

function TdxTableStyle.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := GetMergedCharacterProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableStyle.GetMergedCharacterProperties(
  AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedCharacterProperties;
begin
  if (FConditionalStyleProperties <> nil) and FConditionalStyleProperties.ContainsStyle(AConditionType) then
    Exit(FConditionalStyleProperties.Items[AConditionType].GetMergedCharacterProperties);
  if Parent <> nil then
    Result := Parent.GetMergedCharacterProperties(AConditionType)
  else
    Result := TdxMergedCharacterProperties.Create;
end;

function TdxTableStyle.GetMergedParagraphProperties(
  AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedParagraphProperties;
begin
  if (FConditionalStyleProperties <> nil) and FConditionalStyleProperties.ContainsStyle(AConditionType) then
    Exit(FConditionalStyleProperties.Items[AConditionType].GetMergedParagraphProperties);
  if Parent <> nil then
    Result := Parent.GetMergedParagraphProperties(AConditionType)
  else
    Result := TdxMergedParagraphProperties.Create;
end;

function TdxTableStyle.GetMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := GetMergedParagraphProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableStyle.GetMergedParagraphProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedParagraphProperties;
var
  AMergedProperties: TdxMergedParagraphProperties;
  AParentParagraphProperties: TdxMergedParagraphProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AMergedProperties := TdxMergedParagraphProperties.Create;
  GetConditionalPropertiesSource(ARowType, AColumnType, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      AMergedProperties.Merge(AStyle.ParagraphProperties);
      Result := False;
    end);
  if ParagraphProperties <> nil then
    AMergedProperties.Merge(ParagraphProperties);
  if Parent <> nil then
  begin
    AParentParagraphProperties := Parent.GetMergedParagraphProperties(ARowType, AColumnType);
    try
      AMergedProperties.Merge(AParentParagraphProperties);
    finally
      AParentParagraphProperties.Free;
    end;
  end;
  Result := AMergedProperties;
end;

function TdxTableStyle.GetMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  Result := GetMergedTableCellProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableStyle.GetMergedTableCellProperties(ARowType: TdxConditionalRowType;
  AColType: TdxConditionalColumnType): TdxMergedTableCellProperties;
var
  AMergedProperties: TdxMergedTableCellProperties;
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AMergedProperties := TdxMergedTableCellProperties.Create(TableCellProperties);
  GetConditionalPropertiesSource(ARowType, AColType, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      AMergedProperties.Merge(AStyle.TableCellProperties);
      Result := False;
    end);
  if Parent <> nil then
  begin
    ATableCellProperties := Parent.GetMergedTableCellProperties(ARowType, AColType);
    try
      AMergedProperties.Merge(ATableCellProperties);
    finally
      ATableCellProperties.Free;
    end;
  end;
  Result := AMergedProperties;
end;

function TdxTableStyle.GetMergedTableProperties: TdxMergedTableProperties;
var
  AParentProperties: TdxMergedTableProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedTableProperties.Create(TableProperties);
  if Parent <> nil then
  begin
    AParentProperties := Parent.GetMergedTableProperties;
    try
      Result.Merge(AParentProperties);
    finally
      AParentProperties.Free;
    end;
  end;
end;

function TdxTableStyle.GetMergedTableRowProperties(
  AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedTableRowProperties;
var
  AOptions: TdxTableRowPropertiesOptions;
  AInfo: TdxCombinedTableRowPropertiesInfo;
begin
  if (FConditionalStyleProperties <> nil) and FConditionalStyleProperties.ContainsStyle(AConditionType) then
    Exit(FConditionalStyleProperties.Items[AConditionType].GetMergedTableRowProperties);
  if Parent <> nil then
    Result := Parent.GetMergedTableRowProperties(AConditionType)
  else
  begin
    AInfo := TdxCombinedTableRowPropertiesInfo.Create;
    try
      AOptions := TdxTableRowPropertiesOptions.Create;
      try
        Result := TdxMergedTableRowProperties.Create(AInfo, AOptions);
      finally
        AOptions.Free;
      end;
    finally
      AInfo.Free;
    end;
  end;
end;

function TdxTableStyle.GetMergedTableRowProperties(ARowType: TdxConditionalRowType): TdxMergedTableRowProperties;
var
  AMergedProperties: TdxMergedTableRowProperties;
  ATableRowProperties: TdxMergedTableRowProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AMergedProperties := TdxMergedTableRowProperties.Create(TableRowProperties);
  GetConditionalPropertiesSource(ARowType, TdxConditionalColumnType.Normal, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      AMergedProperties.Merge(AStyle.TableRowProperties);
      Result := False;
    end);
  if Parent <> nil then
  begin
    ATableRowProperties := Parent.GetMergedTableRowProperties(ARowType);
    try
      AMergedProperties.Merge(ATableRowProperties);
    finally
      ATableRowProperties.Free;
    end;
  end;
  Result := AMergedProperties;
end;

function TdxTableStyle.GetMergedTableRowProperties: TdxMergedTableRowProperties;
begin
  Result := GetMergedTableRowProperties(TdxConditionalRowType.Normal);
end;

function TdxTableStyle.GetMergedWithDefaultCharacterProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedCharacterProperties.Create;
  MergeConditionalProperties(Result, ARowType, AColumnType);
  Result.Merge(CharacterProperties);
  Result.Merge(TdxSimpleDocumentModel(DocumentModel).DefaultCharacterProperties);
end;

function TdxTableStyle.GetMergedWithDefaultTableCellProperties: TdxMergedTableCellProperties;
var
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  ATableCellProperties := GetMergedTableCellProperties;
  try
    Result := TdxMergedTableCellProperties.Create(ATableCellProperties);
    Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties);
  finally
    ATableCellProperties.Free;
  end;
end;

function TdxTableStyle.GetMergedWithDefaultTableProperties: TdxMergedTableProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedTableProperties.Create(GetMergedTableProperties);
  Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableProperties);
end;

function TdxTableStyle.GetMergedWithDefaultTableRowProperties: TdxMergedTableRowProperties;
var
  ATableRowProperties: TdxMergedTableRowProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  ATableRowProperties := GetMergedTableRowProperties;
  try
    Result := TdxMergedTableRowProperties.Create(ATableRowProperties);
    Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableRowProperties);
  finally
    ATableRowProperties.Free;
  end;
end;

function TdxTableStyle.GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxParagraphProperties;
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AConditionalStyle := GetConditionalPropertiesSource(ARowType, AColumnType, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      Result := AStyle.ParagraphProperties.UseVal(AMask);
    end);
  if AConditionalStyle <> nil then
    Exit(AConditionalStyle.ParagraphProperties);
  if (ParagraphProperties <> nil) and ParagraphProperties.UseVal(AMask) then
    Exit(ParagraphProperties);
  if Parent <> nil then
    Result := Parent.GetParagraphProperties(AMask, ARowType, AColumnType)
  else
    Result := nil;
end;

function TdxTableStyle.GetParent: TdxTableStyle;
begin
  Result := TdxTableStyle(inherited Parent);
end;

function TdxTableStyle.GetMergedCharacterProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedCharacterProperties.Create;
  MergeConditionalProperties(Result, ARowType, AColumnType);
  Result.Merge(CharacterProperties);
  if Parent <> nil then
  begin
    ACharacterProperties := Parent.GetMergedCharacterProperties(ARowType, AColumnType);
    try
      Result.Merge(ACharacterProperties);
    finally
      ACharacterProperties.Free;
    end;
  end;
end;

function TdxTableStyle.GetTableCellProperties: TdxTableCellProperties;
begin
  if FTableCellProperties = nil then
  begin
    FTableCellProperties := TdxTableCellProperties.Create(DocumentModel.MainPart, Self);
    SubscribeTableCellPropertiesEvents;
  end;
  Result := FTableCellProperties;
end;

function TdxTableStyle.GetTableProperties(AMask: Integer; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxTableProperties;
var
  AInsideBorder: Boolean;
begin
  Result := GetTableProperties(AMask, ARowType, AColumnType, True, AInsideBorder);
end;

function TdxTableStyle.GetTableProperties: TdxTableProperties;
begin
  if FTableProperties = nil then
  begin
    FTableProperties := TdxTableProperties.Create(DocumentModel.MainPart);
    SubscribeTablePropertiesPropertiesEvents;
  end;
  Result := FTableProperties;
end;

function TdxTableStyle.GetTableRowProperties: TdxTableRowProperties;
begin
  if FTableRowProperties = nil then
  begin
    FTableRowProperties := TdxTableRowProperties.Create(DocumentModel.MainPart);
    SubscribeTableRowPropertiesEvents;
  end;
  Result := FTableRowProperties;
end;

procedure TdxTableStyle.MergeConditionalProperties(AMergedProperties: TdxMergedCharacterProperties;
  ARowType: TdxConditionalRowType; AColumnType: TdxConditionalColumnType);
var
  AConditionalStyle: TdxTableConditionalStyle;
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
begin
  if FConditionalStyleProperties = nil then
    Exit;

  AConditionalStyles := FConditionalStyleProperties.Items;
  AConditionalStyle := nil;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if ARowType = TdxConditionalRowType.FirstRow then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.TopLeftCell, AConditionalStyle) and (AConditionalStyle <> nil) then
        AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end
    else
      if ARowType = TdxConditionalRowType.LastRow then
        if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.BottomLeftCell, AConditionalStyle) and (AConditionalStyle <> nil) then
          AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      if ARowType = TdxConditionalRowType.FirstRow then
      begin
        if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.TopRightCell, AConditionalStyle) and (AConditionalStyle <> nil) then
          AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
      end
      else
        if ARowType = TdxConditionalRowType.LastRow then
          if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.BottomRightCell, AConditionalStyle) and (AConditionalStyle <> nil) then
            AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end;
  if ARowType = TdxConditionalRowType.FirstRow then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.FirstRow, AConditionalStyle) and (AConditionalStyle <> nil) then
      AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
  end
  else
    if ARowType = TdxConditionalRowType.LastRow then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.LastRow, AConditionalStyle) and (AConditionalStyle <> nil) then
        AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end;
  if AColumnType = TdxConditionalColumnType.FirstColumn then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.FirstColumn, AConditionalStyle) and (AConditionalStyle <> nil) then
      AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
  end
  else
    if AColumnType = TdxConditionalColumnType.LastColumn then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.LastColumn, AConditionalStyle) and (AConditionalStyle <> nil) then
        AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end;
  if AColumnType = TdxConditionalColumnType.EvenColumnBand then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.EvenColumnBanding, AConditionalStyle) and (AConditionalStyle <> nil) then
      AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
  end
  else
    if AColumnType = TdxConditionalColumnType.OddColumnBand then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.OddColumnBanding, AConditionalStyle) and (AConditionalStyle <> nil) then
        AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end;
  if ARowType = TdxConditionalRowType.EvenRowBand then
  begin
    if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.EvenRowBanding, AConditionalStyle) and (AConditionalStyle <> nil) then
      AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
  end
  else
    if ARowType = TdxConditionalRowType.OddRowBand then
    begin
      if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.OddRowBanding, AConditionalStyle) and (AConditionalStyle <> nil) then
        AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
    end;
  if AConditionalStyles.TryGetValue(TdxConditionalTableStyleFormattingType.WholeTable, AConditionalStyle) and (AConditionalStyle <> nil) then
    AMergedProperties.Merge(AConditionalStyle.CharacterProperties);
end;

procedure TdxTableStyle.MergePropertiesWithParent;
begin
  inherited MergePropertiesWithParent;
  TableProperties.Merge(Parent.TableProperties);
  TableRowProperties.Merge(Parent.TableRowProperties);
  TableCellProperties.Merge(Parent.TableCellProperties);
  CharacterProperties.Merge(Parent.CharacterProperties);
end;

procedure TdxTableStyle.OnCharacterPropertiesChanged;
begin
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Character);
end;

procedure TdxTableStyle.OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxTableStyle.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
end;

procedure TdxTableStyle.SetParent(const Value: TdxTableStyle);
begin
  inherited Parent := Value;
end;

procedure TdxTableStyle.SubscribeCharacterPropertiesEvents;
begin
  CharacterProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxTableStyle.SubscribeTableCellPropertiesEvents;
begin
  FTableCellProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxTableStyle.SubscribeTablePropertiesPropertiesEvents;
begin
  FTableProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxTableStyle.SubscribeTableRowPropertiesEvents;
begin
  FTableRowProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxTableStyle.TryGetConditionalStyle(
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
  AStyleType: TdxConditionalTableStyleFormattingType; out AResult: TdxTableConditionalStyle; AMask: Integer);
begin
  TryGetConditionalStyle(AConditionalStyles, AStyleType, AResult, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      Result := AStyle.TableProperties.GetUse(AMask);
    end);
end;

procedure TdxTableStyle.TryGetConditionalStyle(
  AConditionalStyles: TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>;
  AStyleType: TdxConditionalTableStyleFormattingType; out AResult: TdxTableConditionalStyle;
  const AMaskPredicate: TdxMaskPrediacate);
begin
  AConditionalStyles.TryGetValue(AStyleType, AResult);
  if ((AResult = nil) or not AMaskPredicate(AResult)) and (Parent <> nil) then
    Parent.TryGetConditionalStyle(Parent.ConditionalStyleProperties.Items, AStyleType, AResult, AMaskPredicate);
end;

function TdxTableStyle.GetTableCellProperties(AMask: Integer; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxTableCellProperties;
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AConditionalStyle := GetConditionalPropertiesSource(ARowType, AColumnType, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      Result := AStyle.TableCellProperties.GetUse(AMask);
    end);
  if AConditionalStyle <> nil then
    Exit(AConditionalStyle.TableCellProperties);
  if (FTableCellProperties <> nil) and FTableCellProperties.GetUse(AMask) then
    Exit(FTableCellProperties);
  if Parent <> nil then
    Result := Parent.GetTableCellProperties(AMask, ARowType, AColumnType)
  else
    Result := nil;
end;

function TdxTableStyle.GetTableProperties(AMask: Integer; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType; AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableProperties;
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;

  AConditionalStyle := GetConditionalPropertiesSource(ARowType, AColumnType, AMask, AIsBorderCell, AInsideBorder);
  if AConditionalStyle <> nil then
    Exit(AConditionalStyle.TableProperties);

  AInsideBorder := not AIsBorderCell;

  if (FTableProperties <> nil) and FTableProperties.GetUse(TdxTablePropertiesOptions.OuterOrInside(AMask, AIsBorderCell)) then
    Exit(TableProperties);
  if Parent <> nil then
    Result := Parent.GetTableProperties(AMask, ARowType, AColumnType, AIsBorderCell, AInsideBorder)
  else
    Result := nil;
end;

function TdxTableStyle.GetTableRowProperties(AMask: Integer; ARowType: TdxConditionalRowType): TdxTableRowProperties;
var
  AConditionalStyle: TdxTableConditionalStyle;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  AConditionalStyle := GetConditionalPropertiesSource(ARowType, TdxConditionalColumnType.Normal, function(AStyle: TdxTableConditionalStyle): Boolean
    begin
      Result := AStyle.TableRowProperties.GetUse(AMask);
    end);
  if AConditionalStyle <> nil then
    Exit(AConditionalStyle.TableRowProperties);
  if (FTableRowProperties <> nil) and FTableRowProperties.GetUse(AMask) then
    Exit(FTableRowProperties);
  if Parent <> nil then
    Result := Parent.GetTableRowProperties(AMask, ARowType)
  else
    Result := nil;
end;

function TdxTableStyle.GetType: TdxStyleType;
begin
  Result := TdxStyleType.TableStyle;
end;

function TdxTableStyle.HasColumnStyle(AConditionalColumnType: TdxConditionalColumnType): Boolean;
begin
  if FConditionalStyleProperties <> nil then
  begin
    if AConditionalColumnType = TdxConditionalColumnType.FirstColumn then
      Exit(FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.FirstColumn) or
        FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.TopLeftCell) or
        FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.BottomLeftCell))
    else
      if AConditionalColumnType = TdxConditionalColumnType.LastColumn then
        Exit(FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.LastColumn) or
          FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.TopRightCell) or
          FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.BottomRightCell));
  end;
  if Parent <> nil then
    Result := Parent.HasColumnStyle(AConditionalColumnType)
  else
    Result := False;
end;

function TdxTableStyle.HasRowStyle(AConditionalRowType: TdxConditionalRowType): Boolean;
begin
  if FConditionalStyleProperties <> nil then
  begin
    if AConditionalRowType = TdxConditionalRowType.FirstRow then
      Exit(FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.FirstRow) or
        FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.TopLeftCell) or
        FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.TopRightCell))
    else
      if AConditionalRowType = TdxConditionalRowType.LastRow then
        Exit(FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.LastRow) or
          FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.BottomLeftCell) or
          FConditionalStyleProperties.ContainsStyle(TdxConditionalTableStyleFormattingType.BottomRightCell));
  end;
  if Parent <> nil then
    Result := Parent.HasRowStyle(AConditionalRowType)
  else
    Result := False;
end;

function TdxTableStyle.GetMergedTableCellProperties(
  AConditionType: TdxConditionalTableStyleFormattingType): TdxMergedTableCellProperties;
var
  AInfo: TdxCombinedCellPropertiesInfo;
  AOptions: TdxTableCellPropertiesOptions;
begin
  if (FConditionalStyleProperties <> nil) and FConditionalStyleProperties.ContainsStyle(AConditionType) then
    Exit(FConditionalStyleProperties.Items[AConditionType].GetMergedTableCellProperties);
  if Parent <> nil then
    Result := Parent.GetMergedTableCellProperties(AConditionType)
  else
  begin
    AInfo := TdxCombinedCellPropertiesInfo.Create;
    try
      AOptions := TdxTableCellPropertiesOptions.Create;
      try
        Result := TdxMergedTableCellProperties.Create(AInfo, AOptions);
      finally
        AOptions.Free;
      end;
    finally
      AInfo.Free;
    end;
  end;
end;

{ TdxTableStyleCollection }

constructor TdxTableStyleCollection.Create(ADocumentModel: TdxCustomDocumentModel; AChangeDefaultTableStyle: Boolean);
begin
  inherited Create(ADocumentModel);
  Items.Add(CreateNormalTableStyle(AChangeDefaultTableStyle));
  Items.Add(CreateTableSimpleStyle);
end;

function TdxTableStyleCollection.CreateDefaultItem: TdxStyleBase;
begin
  Result := nil;
end;

function TdxTableStyleCollection.CreateNormalTableStyle(AChangeDefaultTableStyle: Boolean): TdxTableStyle;
var
  AStyle: TdxTableStyle;
  AOptions: TdxTablePropertiesOptions;
  AIsDeferred: Boolean;
begin
  AStyle := TdxTableStyle.Create(DocumentModel, nil, DefaultTableStyleName);
  if AChangeDefaultTableStyle then
  begin
    AStyle.TableProperties.BeginInit;
    try
      SetDefaultMargin(AStyle.TableProperties.CellMargins.Left, FDefaultLeftMargin);
      SetDefaultMargin(AStyle.TableProperties.CellMargins.Top, 0);
      SetDefaultMargin(AStyle.TableProperties.CellMargins.Right, FDefaultRightMargin);
      SetDefaultMargin(AStyle.TableProperties.CellMargins.Bottom, 0);

      AOptions := TdxTablePropertiesAccess(AStyle.TableProperties).GetInfoForModification(AIsDeferred);
      AOptions.UseLeftMargin := True;
      AOptions.UseTopMargin := True;
      AOptions.UseRightMargin := True;
      AOptions.UseBottomMargin := True;
      AStyle.TableProperties.ReplaceInfo(AOptions, []);
      if not AIsDeferred then
        AOptions.Free;
    finally
      AStyle.TableProperties.EndInit;
    end;
  end;
  Result := AStyle;
end;

function TdxTableStyleCollection.CreateTableSimpleStyle: TdxTableStyle;
var
  AIsDeferred: Boolean;
  AStyle: TdxTableStyle;
  AOptions: TdxTablePropertiesOptions;
begin
  AStyle := TdxTableStyle.Create(DocumentModel, Self[DefaultTableStyleIndex], TableSimpleStyleName);
  AStyle.TableProperties.BeginInit;
  try
    SetDefaultMargin(AStyle.TableProperties.CellMargins.Left, FDefaultLeftMargin);
    SetDefaultMargin(AStyle.TableProperties.CellMargins.Right, FDefaultRightMargin);
    SetDefaultBorder(AStyle.TableProperties.Borders.LeftBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);
    SetDefaultBorder(AStyle.TableProperties.Borders.RightBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);
    SetDefaultBorder(AStyle.TableProperties.Borders.TopBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);
    SetDefaultBorder(AStyle.TableProperties.Borders.BottomBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);
    SetDefaultBorder(AStyle.TableProperties.Borders.InsideHorizontalBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);
    SetDefaultBorder(AStyle.TableProperties.Borders.InsideVerticalBorder, FDefaultBorderLineStyle, FDefaultBorderWidth);

    AOptions := TdxTablePropertiesAccess(AStyle.TableProperties).GetInfoForModification(AIsDeferred);
    AOptions.UseLeftMargin := True;
    AOptions.UseRightMargin := True;
    AOptions.UseTopMargin := True;
    AOptions.UseBottomMargin := True;
    AOptions.UseLeftBorder := True;
    AOptions.UseRightBorder := True;
    AOptions.UseTopBorder := True;
    AOptions.UseBottomBorder := True;
    AOptions.UseInsideHorizontalBorder := True;
    AOptions.UseInsideVerticalBorder := True;
    AStyle.TableProperties.ReplaceInfo(AOptions, []);
    if not AIsDeferred then
      AOptions.Free;
  finally
    AStyle.TableProperties.EndInit;
  end;
  Result := AStyle;
end;

function TdxTableStyleCollection.GetItem(Index: Integer): TdxTableStyle;
begin
  Result := TdxTableStyle(inherited Items[Index])
end;

procedure TdxTableStyleCollection.NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable; AStyle: TdxStyleBase);
var
  I: Integer;
  ATables: TdxTableCollection;
  ATableStyle: TdxTableStyle absolute AStyle;
begin
  Assert(APieceTable is TdxPieceTable);
  ATables := TdxPieceTable(APieceTable).Tables;
  for I := 0 to ATables.Count - 1 do
    if ATables[I].TableStyle = ATableStyle then
      ATables[I].StyleIndex := DefaultItemIndex;
end;

procedure TdxTableStyleCollection.SetDefaultBorder(ABorder: TdxBorderBase; AStyle: TdxBorderLineStyle;
  AWidthInTwips: Integer);
begin
  ABorder.BeginInit;
  try
    ABorder.Style := TdxBorderLineStyle.Single;
    ABorder.Width := DocumentModel.UnitConverter.TwipsToModelUnits(AWidthInTwips);
  finally
    ABorder.EndInit;
  end;
end;

procedure TdxTableStyleCollection.SetDefaultMargin(AMargin: TdxWidthUnit; AValueInTwips: Integer);
begin
  AMargin.BeginInit;
  try
    AMargin.&Type := TdxWidthUnitType.ModelUnits;
    AMargin.Value := DocumentModel.UnitConverter.TwipsToModelUnits(AValueInTwips);
  finally
    AMargin.EndInit;
  end;
end;

{ TdxTableConditionalStyleProperties }

constructor TdxTableConditionalStyleProperties.Create(AOwner: TdxTableStyle);
begin
  inherited Create;
  FItems := TdxEnumeratedObjectDictionary<TdxConditionalTableStyleFormattingType, TdxTableConditionalStyle>.Create(True);
  FOwner := AOwner;
  InitializeItems(DocumentModel);
end;

destructor TdxTableConditionalStyleProperties.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

class constructor TdxTableConditionalStyleProperties.Initialize;
var
  I: TdxConditionalTableStyleFormattingType;
begin
  for I := Low(TdxConditionalTableStyleFormattingType) to High(TdxConditionalTableStyleFormattingType) do
    FStyleTypes[I] := I;
end;

procedure TdxTableConditionalStyleProperties.AddConditionalStyle(ADocumentModel: TdxCustomDocumentModel;
  ACondition: TdxConditionalTableStyleFormattingType);
begin
  Items.Add(ACondition, nil);
end;

procedure TdxTableConditionalStyleProperties.AddStyle(AStyle: TdxTableConditionalStyle);
var
  I: Integer;
  AIsColumnBinding, AIsRowBinding: Boolean;
  AOldStyle: TdxTableConditionalStyle;
  AOwnerTableProperties: TdxTableProperties;
  AStyleType: TdxConditionalTableStyleFormattingType;
  ATables: TdxTableCollection;
  AController: TdxTableConditionalFormattingController;
begin
  AStyleType := AStyle.ConditionType;
  AOldStyle := Items[AStyleType];
  if Items[AStyleType] <> nil then
  begin
    AOldStyle.CharacterProperties.OnObtainAffectedRange.Remove(FOwner.OnObtainAffectedRange);
    AOldStyle.ParagraphProperties.OnObtainAffectedRange.Remove(FOwner.OnObtainAffectedRange);
    AOldStyle.TableCellProperties.OnObtainAffectedRange.Remove(FOwner.OnObtainAffectedRange);
    AOldStyle.TableProperties.OnObtainAffectedRange.Remove(FOwner.OnObtainAffectedRange);
    AOldStyle.TableRowProperties.OnObtainAffectedRange.Remove(FOwner.OnObtainAffectedRange);
  end;
  AStyle.CharacterProperties.OnObtainAffectedRange.Add(FOwner.OnObtainAffectedRange);
  AStyle.ParagraphProperties.OnObtainAffectedRange.Add(FOwner.OnObtainAffectedRange);
  AStyle.TableCellProperties.OnObtainAffectedRange.Add(FOwner.OnObtainAffectedRange);
  AStyle.TableProperties.OnObtainAffectedRange.Add(FOwner.OnObtainAffectedRange);
  AStyle.TableRowProperties.OnObtainAffectedRange.Add(FOwner.OnObtainAffectedRange);
  Items[AStyleType] := AStyle;
  AOwnerTableProperties := Owner.TableProperties;
  AIsColumnBinding := AStyleType in [TdxConditionalTableStyleFormattingType.EvenColumnBanding, TdxConditionalTableStyleFormattingType.OddColumnBanding];
  if AIsColumnBinding and not AOwnerTableProperties.UseTableStyleColBandSize then
    AOwnerTableProperties.TableStyleColBandSize := 1;
  AIsRowBinding := AStyleType in [TdxConditionalTableStyleFormattingType.EvenRowBanding, TdxConditionalTableStyleFormattingType.OddRowBanding];
  if AIsRowBinding and not AOwnerTableProperties.UseTableStyleRowBandSize then
    AOwnerTableProperties.TableStyleRowBandSize := 1;
  ATables := TdxDocumentModel(AStyle.DocumentModel).ActivePieceTable.Tables;
  for I := 0 to ATables.Count - 1 do
  begin
    AController.Init(ATables[I]);
    AController.ResetCachedProperties(0);
  end;
end;

function TdxTableConditionalStyleProperties.ContainsStyle(
  AConditionalTableStyleFormattingType: TdxConditionalTableStyleFormattingType): Boolean;
begin
  Result := Items[AConditionalTableStyleFormattingType] <> nil;
end;

procedure TdxTableConditionalStyleProperties.CopyConditionalStyle(ACondition: TdxConditionalTableStyleFormattingType;
  AConditionalProperties: TdxTableConditionalStyleProperties);
var
  ASourceStyle: TdxTableConditionalStyle;
  ATargetStyle: TdxTableConditionalStyle;
begin
  ASourceStyle := AConditionalProperties[ACondition];
  if ASourceStyle <> nil then
  begin
    ATargetStyle := GetStyleSafe(ACondition);
    ATargetStyle.CopyFrom(ASourceStyle);
  end
  else
    Items[ACondition] := nil;
end;

procedure TdxTableConditionalStyleProperties.CopyFrom(AConditionalProperties: TdxTableConditionalStyleProperties);
var
  ACondition: TdxConditionalTableStyleFormattingType;
begin
  for ACondition in StyleTypes do
    CopyConditionalStyle(ACondition, AConditionalProperties);
end;

function TdxTableConditionalStyleProperties.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FOwner.DocumentModel;
end;

function TdxTableConditionalStyleProperties.GetItem(
  ACondition: TdxConditionalTableStyleFormattingType): TdxTableConditionalStyle;
begin
  Result := FItems[ACondition];
end;

function TdxTableConditionalStyleProperties.GetStyleSafe(
  ACondition: TdxConditionalTableStyleFormattingType): TdxTableConditionalStyle;
begin
  Result := Self[ACondition];
  if Result <> nil then
    Exit;

  Result := TdxTableConditionalStyle.Create(Owner, ACondition);
  Items[ACondition] := Result;
end;

function TdxTableConditionalStyleProperties.HasNonNullStyle: Boolean;
var
  ACondition: TdxConditionalTableStyleFormattingType;
begin
  for ACondition in StyleTypes do
    if Items[ACondition] <> nil then
      Exit(True);
  Result := False;
end;

procedure TdxTableConditionalStyleProperties.ForEachStyle(const Action: TdxAction<TdxTableConditionalStyle>);
var
  AStyle: TdxTableConditionalStyle;
begin
  for AStyle in Items.Values do
    Action(AStyle);
end;

procedure TdxTableConditionalStyleProperties.InitializeItems(ADocumentModel: TdxCustomDocumentModel);
var
  ACondition: TdxConditionalTableStyleFormattingType;
begin
  for ACondition in StyleTypes do
    AddConditionalStyle(ADocumentModel, ACondition);
end;

{ TdxTableConditionalStyle }

constructor TdxTableConditionalStyle.Create(AOwner: TdxTableStyle;
  AConditionType: TdxConditionalTableStyleFormattingType);
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FConditionType := AConditionType;
end;

destructor TdxTableConditionalStyle.Destroy;
begin
  FreeAndNil(FTableProperties);
  FreeAndNil(FTableRowProperties);
  FreeAndNil(FTableCellProperties);
  FreeAndNil(FParagraphProperties);
  FreeAndNil(FTabs);
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

procedure TdxTableConditionalStyle.CopyFrom(ACondition: TdxTableConditionalStyle);
begin
  TableProperties.CopyFrom(ACondition.TableProperties);
  TableRowProperties.CopyFrom(ACondition.TableRowProperties);
  TableCellProperties.CopyFrom(ACondition.TableCellProperties);
  if DocumentModel = ACondition.DocumentModel then
  begin
    ParagraphProperties.CopyFrom(ACondition.ParagraphProperties);
    CharacterProperties.CopyFrom(ACondition.CharacterProperties);
  end
  else
  begin
    ParagraphProperties.CopyFrom(ACondition.ParagraphProperties.Info);
    CharacterProperties.CopyFrom(ACondition.CharacterProperties.Info);
  end;
end;

function TdxTableConditionalStyle.CreateCellPropertiesChangedHistoryItem(
  AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = TableCellProperties);
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, AProperties);
end;

function TdxTableConditionalStyle.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, CharacterProperties);
end;

function TdxTableConditionalStyle.CreateParagraphPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, ParagraphProperties);
end;

function TdxTableConditionalStyle.GetCharacterProperties: TdxCharacterProperties;
begin
  if TdxConditionalTableStyleFormattingType.WholeTable = FConditionType then
    Exit(FOwner.CharacterProperties);
  if FCharacterProperties = nil then
    FCharacterProperties := TdxCharacterProperties.Create(Self);
  Result := FCharacterProperties;
end;

function TdxTableConditionalStyle.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FOwner.DocumentModel;
end;

function TdxTableConditionalStyle.GetParagraphProperties: TdxParagraphProperties;
begin
  if TdxConditionalTableStyleFormattingType.WholeTable = FConditionType then
    Exit(FOwner.ParagraphProperties);
  if FParagraphProperties = nil then
    FParagraphProperties := TdxParagraphProperties.Create(Self);
  Result := FParagraphProperties;
end;

function TdxTableConditionalStyle.GetParent: TdxTableStyle;
begin
  Result := FOwner.Parent;
end;

function TdxTableConditionalStyle.GetPieceTable: TdxCustomPieceTable;
begin
  Result := GetInnerPieceTable;
end;

function TdxTableConditionalStyle.GetInnerPieceTable: TdxCustomPieceTable;
begin
  Result := DocumentModel.MainPart;
end;

function TdxTableConditionalStyle.GetMergedCharacterProperties: TdxMergedCharacterProperties;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  if FOwner.Deleted then
    Assert(False);
  Result := TdxMergedCharacterProperties.Create;
  Result.Merge(CharacterProperties);
  if Parent <> nil then
  begin
    ACharacterProperties := Parent.GetMergedCharacterProperties(FConditionType);
    try
      Result.Merge(ACharacterProperties);
    finally
      ACharacterProperties.Free;
    end;
  end;
end;

function TdxTableConditionalStyle.GetMergedParagraphProperties: TdxMergedParagraphProperties;
var
  AParentParagraphProperties: TdxMergedParagraphProperties;
begin
  if FOwner.Deleted then
    Assert(False);
  Result := TdxMergedParagraphProperties.Create;
  Result.Merge(ParagraphProperties);
  if Parent <> nil then
  begin
    AParentParagraphProperties := Parent.GetMergedParagraphProperties(FConditionType);
    try
      Result.Merge(AParentParagraphProperties);
    finally
      AParentParagraphProperties.Free;
    end;
  end;
end;

function TdxTableConditionalStyle.GetMergedTableCellProperties: TdxMergedTableCellProperties;
var
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  if FOwner.Deleted then
    Assert(False);
  Result := TdxMergedTableCellProperties.Create(TableCellProperties);
  if Parent <> nil then
  begin
    ATableCellProperties := Parent.GetMergedTableCellProperties(FConditionType);
    try
      Result.Merge(ATableCellProperties);
    finally
      ATableCellProperties.Free;
    end;
  end;
end;

function TdxTableConditionalStyle.GetMergedTableProperties: TdxMergedTableProperties;
var
  AParentProperties: TdxMergedTableProperties;
begin
  if FOwner.Deleted then
    Assert(False);
  Result := TdxMergedTableProperties.Create(TableProperties);
  if Parent <> nil then
  begin
    AParentProperties := Parent.GetMergedTableProperties;
    try
      Result.Merge(AParentProperties);
    finally
      AParentProperties.Free;
    end;
  end;
end;

function TdxTableConditionalStyle.GetMergedTableRowProperties: TdxMergedTableRowProperties;
var
  ATableRowProperties: TdxMergedTableRowProperties;
begin
  if FOwner.Deleted then
    Assert(False);
  Result := TdxMergedTableRowProperties.Create(TableRowProperties);
  if Parent <> nil then
  begin
    ATableRowProperties := Parent.GetMergedTableRowProperties(FConditionType);
    try
      Result.Merge(ATableRowProperties);
    finally
      ATableRowProperties.Free;
    end;
  end;
end;

function TdxTableConditionalStyle.GetMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create;
  Result.Merge(CharacterProperties);
  Result.Merge(TdxSimpleDocumentModel(DocumentModel).DefaultCharacterProperties);
end;

function TdxTableConditionalStyle.GetMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := TdxMergedParagraphProperties.Create;
  Result.Merge(ParagraphProperties);
  Result.Merge(TdxSimpleDocumentModel(DocumentModel).DefaultParagraphProperties);
end;

function TdxTableConditionalStyle.GetTableCellProperties: TdxTableCellProperties;
begin
  if TdxConditionalTableStyleFormattingType.WholeTable = FConditionType then
    Exit(FOwner.TableCellProperties);
  if FTableCellProperties = nil then
    FTableCellProperties := TdxTableCellProperties.Create(DocumentModel.MainPart, Self);
  Result := FTableCellProperties;
end;

function TdxTableConditionalStyle.GetTableCellProperties(AMask: Integer): TdxTableCellProperties;
begin
  if (FTableCellProperties <> nil) and FTableCellProperties.GetUse(AMask) then
    Result := TableCellProperties
  else
    Result := nil;
end;

function TdxTableConditionalStyle.GetTableProperties(AMask: Integer): TdxTableProperties;
begin
  if (FTableProperties <> nil) and FTableProperties.GetUse(AMask) then
    Result := TableProperties
  else
    Result := nil;
end;

function TdxTableConditionalStyle.GetTableRowProperties(AMask: Integer): TdxTableRowProperties;
begin
  if (FTableRowProperties <> nil) and FTableRowProperties.GetUse(AMask) then
    Result := TableRowProperties
  else
    Result := nil;
end;

function TdxTableConditionalStyle.GetTableProperties: TdxTableProperties;
begin
  if TdxConditionalTableStyleFormattingType.WholeTable = FConditionType then
    Exit(FOwner.TableProperties);
  if FTableProperties = nil then
    FTableProperties := TdxTableProperties.Create(DocumentModel.MainPart);
  Result := FTableProperties;
end;

function TdxTableConditionalStyle.GetTableRowProperties: TdxTableRowProperties;
begin
  if FTableRowProperties = nil then
    FTableRowProperties := TdxTableRowProperties.Create(DocumentModel.MainPart);
  Result := FTableRowProperties;
end;

function TdxTableConditionalStyle.GetTabs: TdxTabProperties;
begin
  if FTabs = nil then
    FTabs := TdxTabProperties.Create(Self);
  Result := FTabs;
end;

procedure TdxTableConditionalStyle.OnCharacterPropertiesChanged;
begin
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Character);
end;

procedure TdxTableConditionalStyle.OnParagraphPropertiesChanged;
begin
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Paragraph);
end;

{ TdxTableCellStyle }

constructor TdxTableCellStyle.Create(ADocumentModel: TdxCustomDocumentModel;
  AParent: TdxTableCellStyle = nil; const AStyleName: string = '');
begin
  inherited Create(ADocumentModel, AParent, AStyleName);
  FCharacterProperties := TdxCharacterProperties.Create(Self);
  SubscribeCharacterPropertiesEvents;
end;

destructor TdxTableCellStyle.Destroy;
begin
  FreeAndNil(FTableCellProperties);
  FreeAndNil(FCharacterProperties);
  inherited Destroy;
end;

procedure TdxTableCellStyle.ApplyPropertiesDiff(AStyle: TdxTableCellStyle);
var
  AStyleMergedWithDefaultParagraphProperties, AMergedWithDefaultParagraphProperties: TdxMergedParagraphProperties;
  AMergedWithDefaultCharacterProperties, AStyleMergedWithDefaultCharacterProperties: TdxMergedCharacterProperties;
begin
  AStyleMergedWithDefaultParagraphProperties := AStyle.GetMergedWithDefaultParagraphProperties;
  try
    AMergedWithDefaultParagraphProperties := GetMergedWithDefaultParagraphProperties;
    try
      ParagraphProperties.ApplyPropertiesDiff(AStyle.ParagraphProperties,
        AStyleMergedWithDefaultParagraphProperties.Info, AMergedWithDefaultParagraphProperties.Info);
    finally
      AMergedWithDefaultParagraphProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultParagraphProperties.Free;
  end;
  AStyleMergedWithDefaultCharacterProperties := AStyle.GetMergedWithDefaultCharacterProperties(
    TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
  try
    AMergedWithDefaultCharacterProperties := GetMergedWithDefaultCharacterProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
    try
      CharacterProperties.ApplyPropertiesDiff(AStyle.CharacterProperties,
        AStyleMergedWithDefaultCharacterProperties.Info, AMergedWithDefaultCharacterProperties.Info);
    finally
      AMergedWithDefaultCharacterProperties.Free;
    end;
  finally
    AStyleMergedWithDefaultCharacterProperties.Free;
  end;
end;

function TdxTableCellStyle.Copy(ATargetModel: TdxCustomDocumentModel): Integer;
var
  I: Integer;
  ATableCellStyles: TdxTableCellStyleCollection;
  AContainer: IdxTableStylesContainer;
begin
  AContainer := ATargetModel as IdxTableStylesContainer;
  ATableCellStyles := AContainer.TableCellStyles;
  for I := 0 to ATableCellStyles.Count - 1 do
    if StyleName = ATableCellStyles[I].StyleName then
      Exit(I);
  Result := ATableCellStyles.AddNewStyle(CopyTo(ATargetModel));
end;

procedure TdxTableCellStyle.CopyProperties(ASource: TdxStyleBase);
var
  ATableCellStyle: TdxTableCellStyle absolute ASource;
begin
  inherited CopyProperties(ATableCellStyle);
  TableCellProperties.CopyFrom(ATableCellStyle.TableCellProperties);
  CharacterProperties.CopyFrom(ATableCellStyle.CharacterProperties.Info);
end;

function TdxTableCellStyle.CopyTo(ATargetModel: TdxCustomDocumentModel): TdxTableCellStyle;
var
  AStyle: TdxTableCellStyle;
  AContainer: IdxTableStylesContainer;
begin
  AContainer := ATargetModel as IdxTableStylesContainer;
  AStyle := TdxTableCellStyle.Create(ATargetModel);
  AStyle.CopyProperties(Self);
  AStyle.StyleName := StyleName;
  if Parent <> nil then
    AStyle.Parent := AContainer.TableCellStyles[Parent.Copy(ATargetModel)];
  ApplyPropertiesDiff(AStyle);
  Result := AStyle;
end;

function TdxTableCellStyle.CreateCellPropertiesChangedHistoryItem(
  AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = TableCellProperties);
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable, AProperties);
end;

function TdxTableCellStyle.CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, CharacterProperties);
end;

function TdxTableCellStyle.GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxCharacterProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  if (FCharacterProperties <> nil) and FCharacterProperties.UseVal(AMask) then
    Exit(FCharacterProperties);
  if Parent <> nil then
    Result := Parent.GetCharacterProperties(AMask, ARowType, AColumnType)
  else
    Result := nil;
end;

function TdxTableCellStyle.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := CharacterProperties;
end;

function TdxTableCellStyle.GetMergedCharacterProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties;
var
  ACharacterProperties: TdxMergedCharacterProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedCharacterProperties.Create(CharacterProperties);
  if Parent <> nil then
  begin
    ACharacterProperties := Parent.GetMergedCharacterProperties(ARowType, AColumnType);
    try
      Result.Merge(ACharacterProperties);
    finally
      ACharacterProperties.Free;
    end;
  end;
end;

function TdxTableCellStyle.GetMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := GetMergedParagraphProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableCellStyle.GetMergedParagraphProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedParagraphProperties;
var
  AParentParagraphProperties: TdxMergedParagraphProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedParagraphProperties.Create;
  if ParagraphProperties <> nil then
    Result.Merge(ParagraphProperties);
  if Parent <> nil then
  begin
    AParentParagraphProperties := Parent.GetMergedParagraphProperties(ARowType, AColumnType);
    try
      Result.Merge(AParentParagraphProperties);
    finally
      AParentParagraphProperties.Free;
    end;
  end;
end;

function TdxTableCellStyle.GetMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  Result := GetMergedTableCellProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTableCellStyle.GetMergedTableCellProperties(ARowType: TdxConditionalRowType;
  AColType: TdxConditionalColumnType): TdxMergedTableCellProperties;
var
  ATableCellProperties: TdxMergedTableCellProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedTableCellProperties.Create(TableCellProperties);
  if Parent <> nil then
  begin
    ATableCellProperties := Parent.GetMergedTableCellProperties(ARowType, AColType);
    try
      Result.Merge(ATableCellProperties);
    finally
      ATableCellProperties.Free;
    end;
  end;
end;

function TdxTableCellStyle.GetMergedTableCellProperties(
  AConditionType: TdxConditionalTableStyleFormattingTypes): TdxMergedTableCellProperties;
var
  AInfo: TdxCombinedCellPropertiesInfo;
  AOptions: TdxTableCellPropertiesOptions;
begin
  if Parent <> nil then
    Result := Parent.GetMergedTableCellProperties(AConditionType)
  else
  begin
    AInfo := TdxCombinedCellPropertiesInfo.Create;
    try
      AOptions := TdxTableCellPropertiesOptions.Create;
      try
        Result := TdxMergedTableCellProperties.Create(AInfo, AOptions);
      finally
        AOptions.Free;
      end;
    finally
      AInfo.Free;
    end;
  end;
end;

function TdxTableCellStyle.GetMergedTableRowProperties(
  AConditionType: TdxConditionalTableStyleFormattingTypes): TdxMergedTableRowProperties;
var
  AOptions: TdxTableRowPropertiesOptions;
  AInfo: TdxCombinedTableRowPropertiesInfo;
begin
  if Parent <> nil then
    Result := Parent.GetMergedTableRowProperties(AConditionType)
  else
  begin
    AInfo := TdxCombinedTableRowPropertiesInfo.Create;
    try
      AOptions := TdxTableRowPropertiesOptions.Create;
      try
        Result := TdxMergedTableRowProperties.Create(AInfo, AOptions);
      finally
        AOptions.Free;
      end;
    finally
      AInfo.Free;
    end;
  end;
end;

function TdxTableCellStyle.GetMergedWithDefaultCharacterProperties(ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxMergedCharacterProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := TdxMergedCharacterProperties.Create(CharacterProperties);
  Result.Merge(TdxSimpleDocumentModel(DocumentModel).DefaultCharacterProperties);
end;

function TdxTableCellStyle.GetMergedWithDefaultTableCellProperties: TdxMergedTableCellProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  Result := GetMergedTableCellProperties;
  Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties);
end;

function TdxTableCellStyle.GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxParagraphProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  if (ParagraphProperties <> nil) and ParagraphProperties.UseVal(AMask) then
    Exit(ParagraphProperties);
  if Parent <> nil then
    Result := Parent.GetParagraphProperties(AMask, ARowType, AColumnType)
  else
    Result := nil;
end;

function TdxTableCellStyle.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := ParagraphProperties;
end;

function TdxTableCellStyle.GetParent: TdxTableCellStyle;
begin
  Result := Parent;
end;

function TdxTableCellStyle.GetParentStyle: TdxTableCellStyle;
begin
  Result := TdxTableCellStyle(inherited GetParentStyle);
end;

function TdxTableCellStyle.GetTableCellProperties: TdxTableCellProperties;
begin
  if FTableCellProperties = nil then
  begin
    FTableCellProperties := TdxTableCellProperties.Create(DocumentModel.MainPart, Self);
    SubscribeTableCellPropertiesEvents;
  end;
  Result := FTableCellProperties;
end;

function TdxTableCellStyle.GetTableCellProperties(AMask: Integer): TdxTableCellProperties;
begin
  if Deleted then
    TdxRichEditExceptions.ThrowInternalException;
  if (FTableCellProperties <> nil) and FTableCellProperties.GetUse(AMask) then
    Exit(FTableCellProperties);
  if Parent <> nil then
    Result := Parent.GetTableCellProperties(AMask)
  else
    Result := nil;
end;

function TdxTableCellStyle.GetType: TdxStyleType;
begin
  Result := TdxStyleType.TableCellStyle;
end;

procedure TdxTableCellStyle.MergePropertiesWithParent;
begin
  inherited MergePropertiesWithParent;
  TableCellProperties.Merge(Parent.TableCellProperties);
  CharacterProperties.Merge(Parent.CharacterProperties);
end;

procedure TdxTableCellStyle.OnCharacterPropertiesChanged;
begin
  DocumentModel.ResetDocumentFormattingCaches(TdxResetFormattingCacheType.Character);
end;

procedure TdxTableCellStyle.OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  E.Start := 0;
  E.&End := MaxInt;
end;

procedure TdxTableCellStyle.ResetCachedIndices(AResetFormattingCacheType: TdxResetFormattingCacheType);
begin
end;

procedure TdxTableCellStyle.SetParentStyle(const Value: TdxTableCellStyle);
begin
  inherited SetParentStyle(Value);
end;

procedure TdxTableCellStyle.SubscribeCharacterPropertiesEvents;
begin
  CharacterProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxTableCellStyle.SubscribeTableCellPropertiesEvents;
begin
  FTableCellProperties.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

{ TdxTableCellStyleCollection }

constructor TdxTableCellStyleCollection.Create(
  ADocumentModel: TdxCustomDocumentModel; AChangeDefaultTableCellStyle: Boolean);
begin
  inherited Create(ADocumentModel);
  Items.Add(CreateNormalTableCellStyle(AChangeDefaultTableCellStyle));
  Items.Add(CreateTableSimpleStyle);
end;

function TdxTableCellStyleCollection.CreateDefaultItem: TdxStyleBase;
begin
  Result := nil;
end;

function TdxTableCellStyleCollection.CreateNormalTableCellStyle(
  AChangeDefaultTableCellStyle: Boolean): TdxTableCellStyle;
var
  AStyle: TdxTableCellStyle;
begin
  AStyle := TdxTableCellStyle.Create(DocumentModel, nil, DefaultTableCellStyleName);
  if AChangeDefaultTableCellStyle then
  begin
    AStyle.TableCellProperties.BeginInit;
    try
    finally
      AStyle.TableCellProperties.EndInit;
    end;
  end;
  Result := AStyle;
end;

function TdxTableCellStyleCollection.CreateTableSimpleStyle: TdxTableCellStyle;
var
  AIsDeferred: Boolean;
  AStyle: TdxTableCellStyle;
  AOptions: TdxTableCellPropertiesOptions;
begin
  AStyle := TdxTableCellStyle.Create(DocumentModel, Self[DefaultTableCellStyleIndex], TableCellSimpleStyleName);
  AStyle.TableCellProperties.BeginInit;
  try
    SetDefaultMargin(AStyle.TableCellProperties.CellMargins.Left, DefaultLeftMargin);
    SetDefaultMargin(AStyle.TableCellProperties.CellMargins.Right, DefaultRightMargin);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.LeftBorder, DefaultBorderLineStyle, DefaultBorderWidth);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.RightBorder, DefaultBorderLineStyle, DefaultBorderWidth);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.TopBorder, DefaultBorderLineStyle, DefaultBorderWidth);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.BottomBorder, DefaultBorderLineStyle, DefaultBorderWidth);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.InsideHorizontalBorder, DefaultBorderLineStyle, DefaultBorderWidth);
    SetDefaultBorder(AStyle.TableCellProperties.Borders.InsideVerticalBorder, DefaultBorderLineStyle, DefaultBorderWidth);

    AOptions := TdxTableCellPropertiesAccess(AStyle.TableCellProperties).GetInfoForModification(AIsDeferred);
    AOptions.UseLeftMargin := True;
    AOptions.UseRightMargin := True;
    AOptions.UseTopMargin := True;
    AOptions.UseBottomMargin := True;
    AOptions.UseLeftBorder := True;
    AOptions.UseRightBorder := True;
    AOptions.UseTopBorder := True;
    AOptions.UseBottomBorder := True;
    AOptions.UseInsideHorizontalBorder := True;
    AOptions.UseInsideVerticalBorder := True;
    AStyle.TableCellProperties.ReplaceInfo(AOptions, []);
    if not AIsDeferred then
      AOptions.Free;
  finally
    AStyle.TableCellProperties.EndInit;
  end;
  Result := AStyle;
end;

function TdxTableCellStyleCollection.GetItem(Index: Integer): TdxTableCellStyle;
begin
  Result := TdxTableCellStyle(inherited Self[Index]);
end;

class constructor TdxTableCellStyleCollection.Initialize;
begin
  TableCellSimpleStyleName := 'Table Cell Simple 1';
  DefaultTableCellStyleName := 'Normal Table Cell';
end;

procedure TdxTableCellStyleCollection.NotifyPieceTableStyleDeleting(APieceTable: TdxCustomPieceTable;
  AStyle: TdxStyleBase);
var
  I, ACount, ADefaultItemIndex: Integer;
  ATables: TdxTableCollection;
  ATableCellStyle: TdxTableCellStyle absolute AStyle;
begin
  ATables := TdxPieceTable(APieceTable).Tables;
  ACount := ATables.Count;
  ADefaultItemIndex := DefaultItemIndex;
  for I := 0 to ACount - 1 do
    ATables[I].ForEachCell(procedure(ACell: TdxTableCell)
      begin
        if ACell.TableCellStyle = ATableCellStyle then
          ACell.StyleIndex := ADefaultItemIndex;
      end);
end;

procedure TdxTableCellStyleCollection.SetDefaultBorder(ABorder: TdxBorderBase; AStyle: TdxBorderLineStyle;
  AWidthInTwips: Integer);
begin
  ABorder.BeginInit;
  try
    ABorder.Style := TdxBorderLineStyle.Single;
    ABorder.Width := DocumentModel.UnitConverter.TwipsToModelUnits(AWidthInTwips);
  finally
    ABorder.EndInit;
  end;
end;

procedure TdxTableCellStyleCollection.SetDefaultMargin(AMargin: TdxWidthUnit; AValueInTwips: Integer);
begin
  AMargin.BeginInit;
  try
    AMargin.&Type := TdxWidthUnitType.ModelUnits;
    AMargin.Value := DocumentModel.UnitConverter.TwipsToModelUnits(AValueInTwips);
  finally
    AMargin.EndInit;
  end;
end;

end.
