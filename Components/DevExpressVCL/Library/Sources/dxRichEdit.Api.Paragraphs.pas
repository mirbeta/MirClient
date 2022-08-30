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

unit dxRichEdit.Api.Paragraphs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTable.InternalAPI;

type
  { TdxNativeParagraph }

  TdxNativeParagraph = class(TInterfacedObject, IdxRichEditParagraph)
  strict private
    FDocument: TdxNativeSubDocument;
    FInnerParagraph: TdxParagraph;
    FIsValid: Boolean;
    function GetInternalAPI: TdxInternalAPI;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  private
    function GetAlignment: TdxRichEditParagraphAlignment;
    function GetBackColor: TdxAlphaColor;
    function GetContextualSpacing: Boolean;
    function GetFirstLineIndent: Single;
    function GetFirstLineIndentType: TdxRichEditParagraphFirstLineIndent;
    function GetIndex: Integer;
    function GetIsInList: Boolean;
    function GetKeepLinesTogether: Boolean;
    function GetLeftIndent: Single;
    function GetLineSpacing: Single;
    function GetLineSpacingMultiplier: Single;
    function GetLineSpacingRule: TdxRichEditParagraphLineSpacing;
    function GetListIndex: Integer;
    function GetListLevel: Integer;
    function GetOutlineLevel: Integer;
    function GetPageBreakBefore: Boolean;
    function GetParagraphIndex: Integer;
    function GetRange: IdxRichEditDocumentRange;
    function GetRightIndent: Single;
    function GetSpacingAfter: Single;
    function GetSpacingBefore: Single;
    function GetStyle: IdxRichEditParagraphStyle;
    function GetSuppressHyphenation: Boolean;
    function GetSuppressLineNumbers: Boolean;

    procedure SetAlignment(const Value: TdxRichEditParagraphAlignment);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetContextualSpacing(const Value: Boolean);
    procedure SetFirstLineIndent(const Value: Single);
    procedure SetFirstLineIndentType(const Value: TdxRichEditParagraphFirstLineIndent);
    procedure SetKeepLinesTogether(const Value: Boolean);
    procedure SetLeftIndent(const Value: Single);
    procedure SetLineSpacing(const Value: Single);
    procedure SetLineSpacingMultiplier(const Value: Single);
    procedure SetLineSpacingRule(const Value: TdxRichEditParagraphLineSpacing);
    procedure SetListIndex(const Value: Integer);
    procedure SetListLevel(const Value: Integer);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetPageBreakBefore(const Value: Boolean);
    procedure SetRightIndent(const Value: Single);
    procedure SetSpacingAfter(const Value: Single);
    procedure SetSpacingBefore(const Value: Single);
    procedure SetStyle(const Value: IdxRichEditParagraphStyle);
    procedure SetSuppressHyphenation(const Value: Boolean);
    procedure SetSuppressLineNumbers(const Value: Boolean);

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument; AInnerParagraph: TdxParagraph);

    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    procedure CheckValid;

    function BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
    procedure EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);

    procedure Reset;

    property ParagraphIndex: Integer read GetParagraphIndex;

    property Alignment: TdxRichEditParagraphAlignment read GetAlignment write SetAlignment;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property ContextualSpacing: Boolean read GetContextualSpacing write SetContextualSpacing;
    property FirstLineIndent: Single read GetFirstLineIndent write SetFirstLineIndent;
    property FirstLineIndentType: TdxRichEditParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
    property Index: Integer read GetIndex;
    property IsInList: Boolean read GetIsInList;
    property KeepLinesTogether: Boolean read GetKeepLinesTogether write SetKeepLinesTogether;
    property LeftIndent: Single read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Single read GetLineSpacing write SetLineSpacing;
    property LineSpacingMultiplier: Single read GetLineSpacingMultiplier write SetLineSpacingMultiplier;
    property LineSpacingRule: TdxRichEditParagraphLineSpacing read GetLineSpacingRule write SetLineSpacingRule;
    property ListIndex: Integer read GetListIndex write SetListIndex;
    property ListLevel: Integer read GetListLevel write SetListLevel;
    property OutlineLevel: Integer read GetOutlineLevel write SetOutlineLevel;
    property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RightIndent: Single read GetRightIndent write SetRightIndent;
    property SpacingAfter: Single read GetSpacingAfter write SetSpacingAfter;
    property SpacingBefore: Single read GetSpacingBefore write SetSpacingBefore;
    property Style: IdxRichEditParagraphStyle read GetStyle write SetStyle;
    property SuppressHyphenation: Boolean read GetSuppressHyphenation write SetSuppressHyphenation;
    property SuppressLineNumbers: Boolean read GetSuppressLineNumbers write SetSuppressLineNumbers;

    property InnerParagraph: TdxParagraph read FInnerParagraph;
    property IsValid: Boolean read FIsValid write FIsValid;
    property InternalAPI: TdxInternalAPI read GetInternalAPI;
    property Document: TdxNativeSubDocument read FDocument;
  end;

  { TdxNativeReadOnlyParagraphCollection }

  TdxNativeReadOnlyParagraphCollection = class(TdxIUnknownList<IdxRichEditParagraph>,
    IdxRichEditReadOnlyParagraphCollection)
  strict private
    function GetCount: Integer;
  protected
    function CreateCollection: TdxNativeReadOnlyParagraphCollection; virtual;
    function Contains(const ARange: IdxRichEditDocumentRange; const AParagraph: IdxRichEditParagraph): Boolean;
  public
    function Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload; virtual;
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection; overload; virtual;
  end;

  { TdxNativeParagraphCollection }

  TdxNativeParagraphCollection = class(TdxNativeReadOnlyParagraphCollection,
    IdxRichEditParagraphCollection)
  strict private
    FDocument: TdxNativeSubDocument;
    function GetPieceTable: TdxPieceTable;
    function GetDocumentModel: TdxDocumentModel;
  protected
    property PieceTable: TdxPieceTable read GetPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(ADocument: TdxNativeSubDocument); reintroduce;

    function Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload; override;
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection; overload; override;

    procedure AddParagraphsToList(const ARange: IdxRichEditDocumentRange;
      const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      const AList: IdxRichEditNumberingList; ALevelIndex: Integer); overload;
    procedure AddParagraphToList(const AParagraph: IdxRichEditParagraph;
      ANumberingListIndex, ALevelIndex: Integer); overload;
    function Append: IdxRichEditParagraph;
    function Insert(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function Insert(const APos: IdxRichEditDocumentPosition; const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph; overload;
    procedure RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
    procedure RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);
  end;

  { TdxNativeAbstractNumberingListBase }

  TdxNativeAbstractNumberingListBase = class abstract(TInterfacedObject, IdxRichEditNumberingListBase)
  strict private
    FDocument: TdxNativeDocument;
    FLevels: IdxReadOnlyListLevelCollection<IdxRichEditListLevel>;

    function GetDocumentModel: TdxDocumentModel;
    function IdxRichEditNumberingListBase.GetLevels = GetNumberingListBaseLevels;
    function GetNumberingListBaseLevels: IdxReadOnlyListLevelCollection<IdxRichEditListLevel>;
  protected
    function GetInnerAbstractNumberingList: TdxAbstractNumberingList; virtual; abstract;

    function GetId: Integer; virtual;
    function GetNumberingType: TdxRichEditNumberingType; virtual;
    procedure SetId(const Value: Integer); virtual;
    procedure SetNumberingType(const Value: TdxRichEditNumberingType); virtual;

    property Document: TdxNativeDocument read FDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property InnerAbstractNumberingList: TdxAbstractNumberingList read GetInnerAbstractNumberingList;
  public
    constructor Create(ADocument: TdxNativeDocument);
    destructor Destroy; override;

    property Id: Integer read GetID write SetId;
    property NumberingType: TdxRichEditNumberingType read GetNumberingType write SetNumberingType;
  end;

  { TdxNativeNumberingList }

  TdxNativeNumberingList = class(TdxNativeAbstractNumberingListBase, IdxRichEditNumberingList)
  strict private
    FInnerNumberingList: TdxNumberingList;
    FOverrideLevels: IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel>;
    function GetAbstractNumberingList: IdxRichEditAbstractNumberingList;
    function GetAbstractNumberingListIndex: Integer;
    function GetIndex: Integer;
    function IdxRichEditNumberingList.GetLevels = GetNumberingListLevels;
    function GetNumberingListLevels: IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel>;
  protected
    function GetInnerAbstractNumberingList: TdxAbstractNumberingList; override;

    function GetId: Integer; override;
    function GetNumberingType: TdxRichEditNumberingType; override;
    procedure SetId(const Value: Integer); override;
    procedure SetNumberingType(const Value: TdxRichEditNumberingType); override;

    property InnerNumberingList: TdxNumberingList read FInnerNumberingList;
  public
    constructor Create(ADocument: TdxNativeDocument;
      ANumberingListIndex: TdxNumberingListIndex); reintroduce; overload;
    constructor Create(ADocument: TdxNativeDocument;
      AModelNumberingList: TdxNumberingList); reintroduce; overload;

    property AbstractNumberingList: IdxRichEditAbstractNumberingList read GetAbstractNumberingList;
    property AbstractNumberingListIndex: Integer read GetAbstractNumberingListIndex;
    property Index: Integer read GetIndex;
  end;

  { TdxNativeNumberedListCollectionBase }

  TdxNativeNumberedListCollectionBase<T: IInterface; TModelList: class> = class abstract(TInterfacedObject, IdxReadOnlyList<T>)
  strict private
    FCachedItems: TDictionary<Integer, T>;
    FDocument: TdxNativeDocument;
    function GetDocumentModel: TdxDocumentModel;
    function GetItem(Index: Integer): T;
  protected
    procedure AddCore(const AInnerList: TModelList); virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetInnerLists: TdxList<TModelList>; virtual; abstract;
    function CreateList(AModelListIndex: Integer): T; virtual; abstract;
    function CreateModelList: TModelList; virtual; abstract;
    function GetModelList(const AList: T): TModelList; virtual; abstract;
    function GetModelListId(const AList: TModelList): Integer; virtual; abstract;
    function GetModelListIndex(const AList: TModelList): Integer; virtual; abstract;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Document: TdxNativeDocument read FDocument;
  public
    constructor Create(ADocument: TdxNativeDocument); virtual;
    destructor Destroy; override;

    procedure Add(const AList: T); overload;
    function Add: T; overload;
    function CreateNew: T; overload;
    function GetList(AHashID: Integer; AIndex: Integer): T; overload;
    function GetList(const AList: TModelList): T; overload;

    procedure Invalidate;

    property Count: Integer read GetCount;
    property InnerLists: TdxList<TModelList> read GetInnerLists;
  end;

  { TdxNativeNumberingListCollection }

  TdxNativeNumberingListCollection = class(TdxNativeNumberedListCollectionBase<IdxRichEditNumberingList, TdxNumberingList>,
    IdxRichEditNumberingListCollection)
  strict private
    FLastIndex: TdxAbstractNumberingListIndex;
  protected
    procedure AddCore(const AInnerList: TdxNumberingList); override;
    function GetCount: Integer; override;
    function GetInnerLists: TdxList<TdxNumberingList>; override;
    function CreateList(AModelListIndex: Integer): IdxRichEditNumberingList; override;
    function CreateModelList: TdxNumberingList; override;
    function GetModelList(const AList: IdxRichEditNumberingList): TdxNumberingList; override;
    function GetModelListId(const AList: TdxNumberingList): Integer; override;
    function GetModelListIndex(const AList: TdxNumberingList): Integer; override;

    function Add(AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList; overload;
    procedure Add(const AList: IdxRichEditNumberingList); overload;
    function CreateNew(AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList; overload;
  public
    constructor Create(ADocument: TdxNativeDocument); override;
  end;

  { TdxNativeReadOnlyListLevelCollection }

  TdxNativeReadOnlyListLevelCollection = class(TInterfacedObject, IdxReadOnlyListLevelCollection<IdxRichEditListLevel>)
  strict private
    FAbstractNumberingList: TdxAbstractNumberingList;
    FDocument: TdxNativeDocument;
    function GetItem(Index: Integer): IdxRichEditListLevel;
    function GetInnerListLevelCollection: TdxListLevelCollection;
  protected
    function GetCount: Integer;

    property Count: Integer read GetCount;
    property Document: TdxNativeDocument read FDocument;
    property InnerListLevelCollection: TdxListLevelCollection read GetInnerListLevelCollection;
  public
    constructor Create(ADocument: TdxNativeDocument; AAbstractNumberingList: TdxAbstractNumberingList);
  end;

  { TdxNativeOverrideListLevelCollection }

  TdxNativeOverrideListLevelCollection = class(TInterfacedObject,
    IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel>)
  strict private
    FDocument: TdxNativeDocument;
    FInnerNumberingList: TdxNumberingList;
    function GetInnerListLevelCollection: TdxListLevelCollection;
    function GetCount: Integer;
    function GetItem(Index: Integer): IdxRichEditOverrideListLevel;
  protected
    property Document: TdxNativeDocument read FDocument;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IdxRichEditOverrideListLevel read GetItem;
    property InnerListLevelCollection: TdxListLevelCollection read GetInnerListLevelCollection;
  public
    constructor Create(ADocument: TdxNativeDocument; AModelNumberingList: TdxNumberingList);

    function Add: IdxRichEditOverrideListLevel; overload;
    procedure Add(const ALevel: IdxRichEditOverrideListLevel); overload;
    function CreateNew: IdxRichEditOverrideListLevel;
  end;

  { TdxNativeListLevel }

  TdxNativeListLevel = class(TInterfacedObject,
    IdxRichEditListLevel, IdxRichEditListLevelProperties)
  strict private
    FCharacterProperties: IdxRichEditCharacterPropertiesBase;
    FDocument: TdxNativeDocument;
    FInnerListLevel: IdxListLevel;
    FParapgraphProperties: IdxRichEditParagraphPropertiesBase;

    function GetConvertPreviousLevelNumberingToDecimal: Boolean;
    function GetDisplayFormatString: string;
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    function GetRelativeRestartLevel: Integer;
    function GetSeparator: Char;
    function GetStart: Integer;
    function GetSuppressBulletResize: Boolean;
    function GetSuppressRestart: Boolean;
    procedure SetConvertPreviousLevelNumberingToDecimal(const Value: Boolean);
    procedure SetDisplayFormatString(const Value: string);
    procedure SetNumberingFormat(const Value: TdxRichEditNumberingFormat);
    procedure SetRelativeRestartLevel(const Value: Integer);
    procedure SetSeparator(const Value: Char);
    procedure SetStart(const Value: Integer);
    procedure SetSuppressBulletResize(const Value: Boolean);
    procedure SetSuppressRestart(const Value: Boolean);

    function GetBulletLevel: Boolean;
    function GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
    function GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
    function GetParagraphStyle: IdxRichEditParagraphStyle;
    procedure SetParagraphStyle(const Value: IdxRichEditParagraphStyle);
  protected
    property InnerListLevel: IdxListLevel read FInnerListLevel;
  public
    constructor Create(ADocument: TdxNativeDocument; const AListLevel: IdxListLevel); virtual;

    property BulletLevel: Boolean read GetBulletLevel;
    property CharacterProperties: IdxRichEditCharacterPropertiesBase read GetCharacterProperties;
    property ConvertPreviousLevelNumberingToDecimal: Boolean read GetConvertPreviousLevelNumberingToDecimal write SetConvertPreviousLevelNumberingToDecimal;
    property DisplayFormatString: string read GetDisplayFormatString write SetDisplayFormatString;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
    property ParagraphProperties: IdxRichEditParagraphPropertiesBase read GetParagraphProperties;
    property ParagraphStyle: IdxRichEditParagraphStyle read GetParagraphStyle write SetParagraphStyle;
    property RelativeRestartLevel: Integer read GetRelativeRestartLevel write SetRelativeRestartLevel;
    property Separator: Char read GetSeparator write SetSeparator;
    property Start: Integer read GetStart write SetStart;
    property SuppressBulletResize: Boolean read GetSuppressBulletResize write SetSuppressBulletResize;
    property SuppressRestart: Boolean read GetSuppressRestart write SetSuppressRestart;
  end;

  { TdxNativeOverrideListLevel }

  TdxNativeOverrideListLevel = class(TdxNativeListLevel,
    IdxRichEditOverrideListLevel, IdxRichEditListLevel, IdxRichEditListLevelProperties)
  strict private
    FModelOverrideListLevel: IdxOverrideListLevel;
    function GetOverrideStart: Boolean;
    function GetNewStart: Integer;
    procedure SetNewStart(const Value: Integer);
  public
    constructor Create(ADocument: TdxNativeDocument; const AListLevel: IdxListLevel); override;
    procedure SetOverrideStart(Value: Boolean);

    property InnerListLevel: IdxOverrideListLevel read FModelOverrideListLevel;
    property OverrideStart: Boolean read GetOverrideStart;
    property NewStart: Integer read GetNewStart write SetNewStart;
  end;

  { TdxNativeAbstractNumberingListCollection }

  TdxNativeAbstractNumberingListCollection = class(
    TdxNativeNumberedListCollectionBase<IdxRichEditAbstractNumberingList, TdxAbstractNumberingList>,
    IdxRichEditAbstractNumberingListCollection)
  strict private
    FBulletedListTemplate: IdxRichEditTemplateAbstractNumberingList;
    FMultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList;
    FNumberedListTemplate: IdxRichEditTemplateAbstractNumberingList;

    FLastIndex: TdxAbstractNumberingListIndex;
    function GetInnerModelLists: TdxAbstractNumberingListCollection;
  protected
    procedure AddCore(const AInnerList: TdxAbstractNumberingList); override;
    function GetCount: Integer; override;
    function GetInnerLists: TdxList<TdxAbstractNumberingList>; override;
    function CreateList(AModelListIndex: Integer): IdxRichEditAbstractNumberingList; override;
    function CreateModelList: TdxAbstractNumberingList; override;
    function GetModelList(const AList: IdxRichEditAbstractNumberingList): TdxAbstractNumberingList; override;
    function GetModelListId(const AList: TdxAbstractNumberingList): Integer; override;
    function GetModelListIndex(const AList: TdxAbstractNumberingList): Integer; override;

    function GetBulletedListTemplate: IdxRichEditTemplateAbstractNumberingList;
    function GetMultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList;
    function GetNumberedListTemplate: IdxRichEditTemplateAbstractNumberingList;

    property InnerModelLists: TdxAbstractNumberingListCollection read GetInnerModelLists;
  public
    constructor Create(ADocument: TdxNativeDocument); override;

    property BulletedListTemplate: IdxRichEditTemplateAbstractNumberingList read GetBulletedListTemplate;
    property MultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList read GetMultiLevelListTemplate;
    property NumberedListTemplate: IdxRichEditTemplateAbstractNumberingList read GetNumberedListTemplate;
  end;

  { TdxNativeAbstractNumberingList }

  TdxNativeAbstractNumberingList = class(TdxNativeAbstractNumberingListBase, IdxRichEditAbstractNumberingList)
  strict private
    FInnerListIndex: TdxAbstractNumberingListIndex;
    function GetIndex: Integer;
  protected
    function GetInnerAbstractNumberingList: TdxAbstractNumberingList; override;
  public
    constructor Create(ADocument: TdxNativeDocument; AListIndex: TdxAbstractNumberingListIndex);

    property Index: Integer read GetIndex;
  end;

  { TdxNativeTemplateAbstractNumberingList }

  TdxNativeTemplateAbstractNumberingList = class(TdxNativeAbstractNumberingListBase, IdxRichEditTemplateAbstractNumberingList)
  strict private
    FType: TdxRichEditNumberingType;
    function GetTemplatesList: TdxAbstractNumberingListCollection;
  protected
    function GetInnerAbstractNumberingList: TdxAbstractNumberingList; override;

    property TemplatesList: TdxAbstractNumberingListCollection read GetTemplatesList;
  public
    constructor Create(ADocument: TdxNativeDocument; AType: TdxRichEditNumberingType);
    function CreateNew: IdxRichEditAbstractNumberingList;
  end;

implementation

uses
  RTLConsts, Contnrs,
  dxRichEdit.Api.Formatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxNativeParagraph }

constructor TdxNativeParagraph.Create(ADocument: TdxNativeSubDocument; AInnerParagraph: TdxParagraph);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerParagraph := AInnerParagraph;
  FIsValid := True;
end;

function TdxNativeParagraph.Equals(Obj: TObject): Boolean;
var
  AObject: TdxNativeParagraph;
begin
  AObject := Obj as TdxNativeParagraph;
  Result := (AObject <> nil) and (AObject.ParagraphIndex = ParagraphIndex)
end;

function TdxNativeParagraph.GetHashCode: Integer;
begin
  Result := ParagraphIndex;
end;

function TdxNativeParagraph.GetInternalAPI: TdxInternalAPI;
begin
  Result := Document.InternalAPI;
end;

function TdxNativeParagraph.GetDocumentModel: TdxDocumentModel;
begin
  Result := FInnerParagraph.DocumentModel;
end;

function TdxNativeParagraph.GetPieceTable: TdxPieceTable;
begin
  Result := FInnerParagraph.PieceTable;
end;

function TdxNativeParagraph.GetIndex: Integer;
begin
  Result := InnerParagraph.Index;
end;

function TdxNativeParagraph.GetLeftIndent: Single;
begin
  CheckValid;
  Result := Document.ModelUnitsToUnits(InnerParagraph.LeftIndent);
end;

procedure TdxNativeParagraph.SetLeftIndent(const Value: Single);
begin
  if LeftIndent = Value then
    Exit;
  InnerParagraph.LeftIndent := Document.UnitsToModelUnits(Value);
end;

function TdxNativeParagraph.GetRightIndent: Single;
begin
  CheckValid;
  Result := Document.ModelUnitsToUnits(InnerParagraph.RightIndent);
end;

procedure TdxNativeParagraph.SetRightIndent(const Value: Single);
begin
  if RightIndent = Value then
    Exit;
  InnerParagraph.RightIndent := Document.UnitsToModelUnits(Value);
end;

function TdxNativeParagraph.GetSpacingBefore: Single;
begin
  CheckValid;
  Result := Document.ModelUnitsToUnits(InnerParagraph.SpacingBefore);
end;

procedure TdxNativeParagraph.SetSpacingBefore(const Value: Single);
begin
  if SpacingBefore = Value then
    Exit;
  InnerParagraph.SpacingBefore := Document.UnitsToModelUnits(Value);
end;

function TdxNativeParagraph.GetSpacingAfter: Single;
begin
  CheckValid;
  Result := Document.ModelUnitsToUnits(InnerParagraph.SpacingAfter);
end;

procedure TdxNativeParagraph.SetSpacingAfter(const Value: Single);
begin
  if SpacingAfter = Value then
    Exit;
  InnerParagraph.SpacingAfter := Document.UnitsToModelUnits(Value);
end;

function TdxNativeParagraph.GetAlignment: TdxRichEditParagraphAlignment;
begin
  CheckValid;
  Result := InnerParagraph.Alignment;
end;

procedure TdxNativeParagraph.SetAlignment(const Value: TdxRichEditParagraphAlignment);
begin
  CheckValid;
  if InnerParagraph.Alignment = Value then
    Exit;
  InnerParagraph.Alignment := Value;
end;

function TdxNativeParagraph.GetLineSpacingRule: TdxRichEditParagraphLineSpacing;
begin
  CheckValid;
  Result := InnerParagraph.LineSpacingType;
end;

procedure TdxNativeParagraph.SetLineSpacingRule(const Value: TdxRichEditParagraphLineSpacing);
begin
  CheckValid;
  if InnerParagraph.LineSpacingType = Value then
    Exit;
  InnerParagraph.LineSpacingType := Value;
end;

function TdxNativeParagraph.GetLineSpacing: Single;
begin
  CheckValid;
  Result := InnerParagraph.LineSpacing;
  if LineSpacingRule = TdxRichEditParagraphLineSpacing.Multiple then
    Result := Document.PointsToModelUnitsF(Result * 12);
  Result := Document.ModelUnitsToUnitsF(Result);
end;

procedure TdxNativeParagraph.SetLineSpacing(const Value: Single);
var
  AValue: Single;
begin
  if LineSpacing = Value then
    Exit;

  AValue := Document.UnitsToModelUnitsF(Value);
  if LineSpacingRule = TdxRichEditParagraphLineSpacing.Multiple then
    AValue := Document.ModelUnitsToPointsF(AValue) / 12;
  InnerParagraph.LineSpacing := AValue;
end;

function TdxNativeParagraph.GetLineSpacingMultiplier: Single;
begin
  CheckValid;
  Result := InnerParagraph.LineSpacing;
end;

procedure TdxNativeParagraph.SetLineSpacingMultiplier(const Value: Single);
begin
  if LineSpacingMultiplier = Value then
    Exit;
  if LineSpacingRule <> TdxRichEditParagraphLineSpacing.Multiple then
    LineSpacingRule := TdxRichEditParagraphLineSpacing.Multiple;
  InnerParagraph.LineSpacing := Value;
end;

function TdxNativeParagraph.GetFirstLineIndentType: TdxRichEditParagraphFirstLineIndent;
begin
  CheckValid;
  Result := InnerParagraph.FirstLineIndentType;
end;

procedure TdxNativeParagraph.SetFirstLineIndentType(const Value: TdxRichEditParagraphFirstLineIndent);
begin
  CheckValid;
  if InnerParagraph.FirstLineIndentType = Value then
    Exit;
  InnerParagraph.FirstLineIndentType := Value;
end;

function TdxNativeParagraph.GetFirstLineIndent: Single;
begin
  CheckValid;
  Result := Document.ModelUnitsToUnits(InnerParagraph.FirstLineIndent);
end;

procedure TdxNativeParagraph.SetFirstLineIndent(const Value: Single);
begin
  if FirstLineIndent = Value then
    Exit;
  InnerParagraph.FirstLineIndent := Document.UnitsToModelUnits(Value);
end;

function TdxNativeParagraph.GetSuppressHyphenation: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.SuppressHyphenation;
end;

procedure TdxNativeParagraph.SetSuppressHyphenation(const Value: Boolean);
begin
  CheckValid;
  if InnerParagraph.SuppressHyphenation = Value then
    Exit;
  InnerParagraph.SuppressHyphenation := Value;
end;

function TdxNativeParagraph.GetSuppressLineNumbers: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.SuppressLineNumbers;
end;

procedure TdxNativeParagraph.SetSuppressLineNumbers(const Value: Boolean);
begin
  CheckValid;
  if InnerParagraph.SuppressLineNumbers = Value then
    Exit;
  InnerParagraph.SuppressLineNumbers := Value;
end;

function TdxNativeParagraph.GetOutlineLevel: Integer;
begin
  CheckValid;
  Result := InnerParagraph.OutlineLevel;
end;

procedure TdxNativeParagraph.SetOutlineLevel(const Value: Integer);
begin
  CheckValid;
  if InnerParagraph.OutlineLevel = Value then
    Exit;
  InnerParagraph.OutlineLevel := Value;
end;

function TdxNativeParagraph.GetKeepLinesTogether: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.KeepLinesTogether;
end;

procedure TdxNativeParagraph.SetKeepLinesTogether(const Value: Boolean);
begin
  CheckValid;
  if InnerParagraph.KeepLinesTogether = Value then
    Exit;
  InnerParagraph.KeepLinesTogether := Value;
end;

function TdxNativeParagraph.GetPageBreakBefore: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.PageBreakBefore;
end;

procedure TdxNativeParagraph.SetPageBreakBefore(const Value: Boolean);
begin
  CheckValid;
  if InnerParagraph.PageBreakBefore = Value then
    Exit;
  InnerParagraph.PageBreakBefore := Value;
end;

function TdxNativeParagraph.GetContextualSpacing: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.ContextualSpacing;
end;

procedure TdxNativeParagraph.SetContextualSpacing(const Value: Boolean);
begin
  CheckValid;
  if InnerParagraph.ContextualSpacing = Value then
    Exit;
  InnerParagraph.ContextualSpacing := Value;
end;

function TdxNativeParagraph.GetBackColor: TdxAlphaColor;
begin
  CheckValid;
  Result := InnerParagraph.BackColor;
end;

procedure TdxNativeParagraph.SetBackColor(const Value: TdxAlphaColor);
begin
  CheckValid;
  if InnerParagraph.BackColor = Value then
    Exit;
  InnerParagraph.BackColor := Value;
end;

function TdxNativeParagraph.GetStyle: IdxRichEditParagraphStyle;
var
  AStyles: TdxNativeParagraphStyleCollection;
begin
  CheckValid;
  AStyles := TdxNativeParagraphStyleCollection(Document.MainDocument.ParagraphStyles);
  Result := AStyles.GetStyle(InnerParagraph.ParagraphStyle);
end;

procedure TdxNativeParagraph.SetStyle(const Value: IdxRichEditParagraphStyle);
var
  AStyle: TdxParagraphStyle;
begin
  CheckValid;
  if Value <> nil then
    AStyle := TdxNativeParagraphStyle(Value).InnerStyle
  else
    AStyle := nil;
  InnerParagraph.ParagraphStyleIndex := DocumentModel.ParagraphStyles.IndexOf(AStyle);
end;

procedure TdxNativeParagraph.CheckValid;
begin
  if not FIsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedParagraphError));
end;

function TdxNativeParagraph.GetRange: IdxRichEditDocumentRange;
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  AStart := TdxDocumentModelPosition.FromParagraphStart(PieceTable, InnerParagraph.Index);
  AEnd := TdxDocumentModelPosition.FromParagraphEnd(PieceTable, InnerParagraph.Index);
  Result := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
end;

function TdxNativeParagraph.GetParagraphIndex: Integer;
begin
  Result := InnerParagraph.Index;
end;

function TdxNativeParagraph.BeginUpdateTabs(AOnlyOwnTabs: Boolean): IdxRichEditTabInfoCollection;
var
  ATabs: TdxTabFormattingInfo;
begin
  CheckValid;
  if AOnlyOwnTabs then
    ATabs := InnerParagraph.GetOwnTabs
  else
    ATabs := InnerParagraph.GetTabs;

  Result := TdxNativeParagraphProperties.CreateTabInfoCollection(Document, ATabs);
end;

procedure TdxNativeParagraph.EndUpdateTabs(const ATabs: IdxRichEditTabInfoCollection);
var
  ATabInfo: TdxTabFormattingInfo;
begin
  CheckValid;
  if ATabs = nil then
    Exit;

  ATabInfo := TdxNativeParagraphProperties.CreateModelTabInfoCollection(Document, ATabs);
  try
    InnerParagraph.SetOwnTabs(ATabInfo);
  finally
    ATabInfo.Free;
  end;
end;

procedure TdxNativeParagraph.Reset;
begin
  CheckValid;
  InnerParagraph.ParagraphProperties.ResetAllUse;
end;

function TdxNativeParagraph.GetListIndex: Integer;
begin
  CheckValid;
  Result := InnerParagraph.GetNumberingListIndex;
end;

procedure TdxNativeParagraph.SetListIndex(const Value: Integer);
var
  AModelParagraph: TdxParagraph;
begin
  CheckValid;
  FInnerParagraph.DocumentModel.BeginUpdate;
  try
    AModelParagraph := FInnerParagraph;
    if AModelParagraph.IsInList then
      AModelParagraph.PieceTable.RemoveNumberingFromParagraph(AModelParagraph);
    InnerParagraph.NumberingListIndex := Value;
  finally
    FInnerParagraph.DocumentModel.EndUpdate;
  end;
end;

function TdxNativeParagraph.GetListLevel: Integer;
begin
  CheckValid;
  Result := InnerParagraph.GetListLevelIndex;
end;

procedure TdxNativeParagraph.SetListLevel(const Value: Integer);
var
  AModelParagraph: TdxParagraph;
begin
  CheckValid;
  InnerParagraph.DocumentModel.BeginUpdate;
  try
    AModelParagraph := FInnerParagraph;
    AModelParagraph.PieceTable.ApplyListLevelIndexToParagraph(AModelParagraph, Value);
  finally
    InnerParagraph.DocumentModel.EndUpdate;
  end;
end;

function TdxNativeParagraph.GetIsInList: Boolean;
begin
  CheckValid;
  Result := InnerParagraph.IsInList;
end;

{ TdxNativeReadOnlyParagraphCollection }

function TdxNativeReadOnlyParagraphCollection.Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
var
  APosition: TdxNativeDocumentPosition;
  I: Integer;
  AIndex: Integer;
begin
  APosition := TdxNativeDocumentPosition(APos);
  AIndex := APosition.Position.ParagraphIndex;
  for I := 0 to Count - 1 do
  begin
    Result := Self[I];
    if Result.Index = AIndex then
      Exit;
  end;
  Result := nil;
end;

function TdxNativeReadOnlyParagraphCollection.Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection;
var
  AResult: TdxNativeReadOnlyParagraphCollection;
  I: Integer;
begin
  AResult := CreateCollection;
  for I := 0 to Count - 1 do
    if Contains(ARange, Self[I]) then
      AResult.Add(Self[I]);
  Result := AResult;
end;

function TdxNativeReadOnlyParagraphCollection.CreateCollection: TdxNativeReadOnlyParagraphCollection;
begin
  Result := TdxNativeReadOnlyParagraphCollection.Create;
end;

function TdxNativeReadOnlyParagraphCollection.Contains(
  const ARange: IdxRichEditDocumentRange; const AParagraph: IdxRichEditParagraph): Boolean;
begin
  Result := ARange.Contains(AParagraph.Range.Start);
end;

function TdxNativeReadOnlyParagraphCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

{ TdxNativeParagraphCollection }

constructor TdxNativeParagraphCollection.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeParagraphCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

function TdxNativeParagraphCollection.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxNativeParagraphCollection.Insert(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
begin
  Result := Insert(APos, TdxRichEditInsertOptions.MatchDestinationFormatting);
end;

function TdxNativeParagraphCollection.Insert(const APos: IdxRichEditDocumentPosition; const AInsertOptions: TdxRichEditInsertOptions): IdxRichEditParagraph;
var
  ANativePosition: TdxNativeDocumentPosition;
  AParagraphIndex: TdxParagraphIndex;
  ALogPosition: TdxDocumentLogPosition;
  AParagraph: TdxParagraph;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);
  ANativePosition := TdxNativeDocumentPosition(APos);
  AParagraphIndex := ANativePosition.Position.ParagraphIndex;
  ALogPosition := FDocument.NormalizeLogPosition(ANativePosition.Position.LogPosition);
  PieceTable.InsertParagraph(ALogPosition);
  AParagraph := PieceTable.Paragraphs[AParagraphIndex + 1];
  if AParagraph.IsInList and (AInsertOptions = TdxRichEditInsertOptions.KeepSourceFormatting) then
    PieceTable.RemoveNumberingFromParagraph(AParagraph);
  Result := Self[AParagraphIndex + 1];
end;

function TdxNativeParagraphCollection.Append: IdxRichEditParagraph;
begin
  Result := Insert(FDocument.EndPosition);
end;

function TdxNativeParagraphCollection.Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);

  AParagraphIndex := PieceTable.FindParagraphIndex(APos.LogPosition, False);
  if AParagraphIndex < 0 then
    Exit(nil);
  Result := Self[AParagraphIndex];
end;

function TdxNativeParagraphCollection.Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection;
var
  AParagraphsRange: TdxParagraphRange;
  AFirstIndex, ALastIndex, I: Integer;
  AResult: TdxNativeParagraphCollection;
begin
  FDocument.CheckValid;

  AParagraphsRange := FDocument.CalculateParagraphsRange(ARange);
  AFirstIndex := AParagraphsRange.Start;
  ALastIndex := AFirstIndex + AParagraphsRange.Length - 1;
  AResult := TdxNativeParagraphCollection.Create(FDocument);
  for I := AFirstIndex to ALastIndex do
    AResult.Add(Self[I]);
  Result := AResult;
end;

procedure TdxNativeParagraphCollection.AddParagraphToList(const AParagraph: IdxRichEditParagraph;
  const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
begin
  AddParagraphsToList(AParagraph.Range, AList, ALevelIndex);
end;

procedure TdxNativeParagraphCollection.AddParagraphToList(const AParagraph: IdxRichEditParagraph;
  ANumberingListIndex, ALevelIndex: Integer);
begin
  AddParagraphsToList(AParagraph.Range, FDocument.MainDocument.NumberingLists[ANumberingListIndex], ALevelIndex);
end;

procedure TdxNativeParagraphCollection.AddParagraphsToList(const ARange: IdxRichEditDocumentRange;
  const AList: IdxRichEditNumberingList; ALevelIndex: Integer);
var
  AParagraphs: IdxRichEditReadOnlyParagraphCollection;
  AInnerNumberingList: TdxNumberingList;
  AIndex, I: Integer;
  ANumberingListIndex: TdxNumberingListIndex;
  AParagraph: TdxNativeParagraph;
  AModelParagraph: TdxParagraph;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentRange(ARange);

  DocumentModel.BeginUpdate;
  try
    AParagraphs := Get(ARange);
    AInnerNumberingList := TdxNativeNumberingList(AList).InnerNumberingList;
    AIndex := TdxNativeNumberingListCollection(FDocument.MainDocument.NumberingLists).InnerLists.IndexOf(AInnerNumberingList);
    if AIndex < 0 then
      TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionNumberingListNotInListCollection));
    ANumberingListIndex := AIndex;
    for I := 0 to AParagraphs.Count - 1 do
    begin
      AParagraph := TdxNativeParagraph(AParagraphs[I]);
      AModelParagraph := AParagraph.InnerParagraph;
      if AModelParagraph.IsInList then
        AModelParagraph.PieceTable.RemoveNumberingFromParagraph(AModelParagraph);
      PieceTable.AddNumberingListToParagraph(AModelParagraph, ANumberingListIndex, ALevelIndex);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeParagraphCollection.RemoveNumberingFromParagraph(const AParagraph: IdxRichEditParagraph);
begin
  RemoveNumberingFromParagraphs(AParagraph.Range);
end;

procedure TdxNativeParagraphCollection.RemoveNumberingFromParagraphs(const ARange: IdxRichEditDocumentRange);
var
  AParagraphs: TdxNativeParagraphCollection;
  I: Integer;
  AParagraph: TdxNativeParagraph;
  AModelParagraph: TdxParagraph;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentRange(ARange);

  DocumentModel.BeginUpdate;
  try
    AParagraphs := TdxNativeParagraphCollection(Get(ARange));
    for I := 0 to AParagraphs.Count - 1 do
    begin
      AParagraph := TdxNativeParagraph(AParagraphs[I]);
      AModelParagraph := AParagraph.InnerParagraph;
      if AModelParagraph.IsInList then
        AModelParagraph.PieceTable.RemoveNumberingFromParagraph(AModelParagraph);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

{ TdxNativeAbstractNumberingListBase }

constructor TdxNativeAbstractNumberingListBase.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

destructor TdxNativeAbstractNumberingListBase.Destroy;
begin
  FLevels := nil;
  inherited Destroy;
end;

function TdxNativeAbstractNumberingListBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Document.DocumentModel;
end;

function TdxNativeAbstractNumberingListBase.GetId: Integer;
begin
  Result := InnerAbstractNumberingList.Id;
end;

function TdxNativeAbstractNumberingListBase.GetNumberingListBaseLevels: IdxReadOnlyListLevelCollection<IdxRichEditListLevel>;
begin
  if FLevels = nil then
    FLevels :=  TdxNativeReadOnlyListLevelCollection.Create(Document, InnerAbstractNumberingList);
  Result := FLevels;
end;

function TdxNativeAbstractNumberingListBase.GetNumberingType: TdxRichEditNumberingType;
begin
  Result := TdxNumberingListHelper.GetListType(InnerAbstractNumberingList);
end;

procedure TdxNativeAbstractNumberingListBase.SetId(const Value: Integer);
begin
  InnerAbstractNumberingList.SetId(Value);
end;

procedure TdxNativeAbstractNumberingListBase.SetNumberingType(const Value: TdxRichEditNumberingType);
begin
 if NumberingType = Value then
    Exit;
 TdxNumberingListHelper.SetListType(InnerAbstractNumberingList, Value);
end;

{ TdxNativeNumberingList }

constructor TdxNativeNumberingList.Create(ADocument: TdxNativeDocument;
  ANumberingListIndex: TdxNumberingListIndex);
begin
  Create(ADocument, ADocument.DocumentModel.NumberingLists[ANumberingListIndex]);
end;

constructor TdxNativeNumberingList.Create(ADocument: TdxNativeDocument;
  AModelNumberingList: TdxNumberingList);
begin
  inherited Create(ADocument);
  FInnerNumberingList := AModelNumberingList;
end;

function TdxNativeNumberingList.GetInnerAbstractNumberingList: TdxAbstractNumberingList;
begin
  Result := InnerNumberingList.AbstractNumberingList;
end;

function TdxNativeNumberingList.GetId: Integer;
begin
  Result := InnerNumberingList.Id;
end;

function TdxNativeNumberingList.GetNumberingType: TdxRichEditNumberingType;
begin
  Result := TdxNumberingListHelper.GetListType(InnerNumberingList)
end;

procedure TdxNativeNumberingList.SetId(const Value: Integer);
begin
  InnerNumberingList.SetId(Value);
end;

procedure TdxNativeNumberingList.SetNumberingType(const Value: TdxRichEditNumberingType);
begin
  if NumberingType = Value then
    Exit;
  TdxNumberingListHelper.SetListType(InnerNumberingList, Value);
end;

function TdxNativeNumberingList.GetAbstractNumberingList: IdxRichEditAbstractNumberingList;
begin
  Result := Document.AbstractNumberingLists[AbstractNumberingListIndex];
end;

function TdxNativeNumberingList.GetAbstractNumberingListIndex: Integer;
begin
  Result := DocumentModel.AbstractNumberingLists.IndexOf(InnerAbstractNumberingList);
end;

function TdxNativeNumberingList.GetIndex: Integer;
begin
  Result := DocumentModel.NumberingLists.IndexOf(InnerNumberingList);
end;

function TdxNativeNumberingList.GetNumberingListLevels: IdxRichEditListLevelCollection<IdxRichEditOverrideListLevel>;
begin
  if FOverrideLevels = nil then
    FOverrideLevels := TdxNativeOverrideListLevelCollection.Create(Document, FInnerNumberingList);
  Result := FOverrideLevels;
end;

{ TdxNativeNumberedListCollectionBase<T, TModelList> }

constructor TdxNativeNumberedListCollectionBase<T, TModelList>.Create(ADocument: TdxNativeDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FCachedItems := TDictionary<Integer, T>.Create;
end;

destructor TdxNativeNumberedListCollectionBase<T, TModelList>.Destroy;
begin
  FreeAndNil(FCachedItems);
  inherited Destroy;
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.GetDocumentModel: TdxDocumentModel;
begin
  Result := FDocument.DocumentModel;
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.GetItem(
  Index: Integer): T;
begin
  Result := GetList(InnerLists[Index]);
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.CreateNew: T;
var
  AInnerList: TModelList;
begin
  AInnerList := CreateModelList;
  AddCore(AInnerList);
  Result := GetList(AInnerList);
end;

procedure TdxNativeNumberedListCollectionBase<T, TModelList>.Add(const AList: T);
var
  AInnerList: TModelList;
begin
  AInnerList := GetModelList(AList);
  GetList(AInnerList);
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.Add: T;
var
  AInnerList: TModelList;
begin
  AInnerList := CreateModelList;
  AddCore(AInnerList);
  Result := GetList(AInnerList);
end;

procedure TdxNativeNumberedListCollectionBase<T, TModelList>.Invalidate;
begin
  FCachedItems.Clear;
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.GetList(AHashID: Integer; AIndex: Integer): T;
begin
  Result := Default(T);
  if AIndex < 0 then
    Exit;
  if AIndex >= Count then
  begin
    if FCachedItems.ContainsKey(AHashID) then
      FCachedItems.Remove(AHashID);
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;
  if not FCachedItems.TryGetValue(AHashID, Result) then
  begin
    Result := CreateList(AIndex);
    FCachedItems.Add(AHashID, Result);
  end;
end;

function TdxNativeNumberedListCollectionBase<T, TModelList>.GetList(const AList: TModelList): T;
var
  AId, AIndex: Integer;
begin
  AId := GetModelListId(AList);
  AIndex := GetModelListIndex(AList);
  Result := GetList(AId, AIndex);
end;

{ TdxNativeNumberingListCollection }

constructor TdxNativeNumberingListCollection.Create(
  ADocument: TdxNativeDocument);
begin
  inherited Create(ADocument);
  FLastIndex := -1;
end;

procedure TdxNativeNumberingListCollection.AddCore(const AInnerList: TdxNumberingList);
begin
  if not InnerLists.Contains(AInnerList) then
    DocumentModel.AddNumberingListUsingHistory(AInnerList);
end;

function TdxNativeNumberingListCollection.GetCount: Integer;
begin
  Result := InnerLists.Count;
end;

function TdxNativeNumberingListCollection.GetInnerLists: TdxList<TdxNumberingList>;
begin
  Result := DocumentModel.NumberingLists;
end;

function TdxNativeNumberingListCollection.CreateList(AModelListIndex: Integer): IdxRichEditNumberingList;
begin
  Result := TdxNativeNumberingList.Create(Document, AModelListIndex);
end;

function TdxNativeNumberingListCollection.CreateModelList: TdxNumberingList;
begin
  Result := TdxNumberingList.Create(DocumentModel, FLastIndex);
end;

function TdxNativeNumberingListCollection.GetModelList(const AList: IdxRichEditNumberingList): TdxNumberingList;
begin
  Result := TdxNativeNumberingList(AList).InnerNumberingList;
end;

function TdxNativeNumberingListCollection.GetModelListId(const AList: TdxNumberingList): Integer;
begin
  Result := AList.Id;
end;

function TdxNativeNumberingListCollection.GetModelListIndex(const AList: TdxNumberingList): Integer;
begin
  Result := InnerLists.IndexOf(AList);
end;

function TdxNativeNumberingListCollection.Add(
  AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList;
begin
  FLastIndex := AAbstractNumberingListIndex;
  Result := inherited Add;
end;

procedure TdxNativeNumberingListCollection.Add(
  const AList: IdxRichEditNumberingList);
begin
  FLastIndex := -1;
  inherited Add(AList);
end;

function TdxNativeNumberingListCollection.CreateNew(
  AAbstractNumberingListIndex: Integer): IdxRichEditNumberingList;
begin
  FLastIndex := AAbstractNumberingListIndex;
  Result := inherited CreateNew;
end;

{ TdxNativeReadOnlyListLevelCollection }

constructor TdxNativeReadOnlyListLevelCollection.Create(
  ADocument: TdxNativeDocument; AAbstractNumberingList: TdxAbstractNumberingList);
begin
  inherited Create;
  FAbstractNumberingList := AAbstractNumberingList;
end;

function TdxNativeReadOnlyListLevelCollection.GetCount: Integer;
begin
  Result := InnerListLevelCollection.Count;
end;

function TdxNativeReadOnlyListLevelCollection.GetInnerListLevelCollection: TdxListLevelCollection;
begin
  Result := FAbstractNumberingList.Levels;
end;

function TdxNativeReadOnlyListLevelCollection.GetItem(
  Index: Integer): IdxRichEditListLevel;
begin
  Result := nil;
  Result := TdxNativeListLevel.Create(Document, InnerListLevelCollection[Index]);
end;

{ TdxNativeOverrideListLevelCollection }

constructor TdxNativeOverrideListLevelCollection.Create(
  ADocument: TdxNativeDocument; AModelNumberingList: TdxNumberingList);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerNumberingList := AModelNumberingList;
end;

procedure TdxNativeOverrideListLevelCollection.Add(
  const ALevel: IdxRichEditOverrideListLevel);
begin
  InnerListLevelCollection.Add(TdxAbstractListLevel(TdxNativeOverrideListLevel(ALevel).InnerListLevel));
end;

function TdxNativeOverrideListLevelCollection.Add: IdxRichEditOverrideListLevel;
begin
  Result := CreateNew;
  InnerListLevelCollection.Add(TdxAbstractListLevel(TdxNativeOverrideListLevel(Result).InnerListLevel));
end;

function TdxNativeOverrideListLevelCollection.CreateNew: IdxRichEditOverrideListLevel;
var
  AModelListLevel: TdxOverrideListLevel;
begin
  AModelListLevel := TdxOverrideListLevel.Create(Document.DocumentModel);
  Result := TdxNativeOverrideListLevel.Create(Document, AModelListLevel);
end;

function TdxNativeOverrideListLevelCollection.GetCount: Integer;
begin
  Result := InnerListLevelCollection.Count;
end;

function TdxNativeOverrideListLevelCollection.GetInnerListLevelCollection: TdxListLevelCollection;
begin
  Result := FInnerNumberingList.Levels;
end;

function TdxNativeOverrideListLevelCollection.GetItem(
  Index: Integer): IdxRichEditOverrideListLevel;
begin
  Result := nil;
  Result := TdxNativeOverrideListLevel.Create(Document, FInnerNumberingList.Levels[index]);
end;

{ TdxNativeListLevel }

constructor TdxNativeListLevel.Create(ADocument: TdxNativeDocument;
  const AListLevel: IdxListLevel);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerListLevel := AListLevel;
end;

function TdxNativeListLevel.GetBulletLevel: Boolean;
begin
  Result := InnerListLevel.BulletLevel;
end;

function TdxNativeListLevel.GetCharacterProperties: IdxRichEditCharacterPropertiesBase;
begin
  if FCharacterProperties = nil then
    FCharacterProperties := TdxNativeSimpleCharacterProperties.Create(InnerListLevel.CharacterProperties);
  Result := FCharacterProperties;
end;

function TdxNativeListLevel.GetConvertPreviousLevelNumberingToDecimal: Boolean;
begin
  Result := InnerListLevel.ListLevelProperties.ConvertPreviousLevelNumberingToDecimal;
end;

function TdxNativeListLevel.GetDisplayFormatString: string;
begin
  Result := InnerListLevel.ListLevelProperties.DisplayFormatString;
end;

function TdxNativeListLevel.GetNumberingFormat: TdxRichEditNumberingFormat;
begin
  Result := InnerListLevel.ListLevelProperties.Format;
end;

function TdxNativeListLevel.GetParagraphProperties: IdxRichEditParagraphPropertiesBase;
begin
  if FParapgraphProperties = nil then
    FParapgraphProperties := TdxNativeSimpleParagraphProperties.Create(FDocument, InnerListLevel.ParagraphProperties);
  Result := FParapgraphProperties;
end;

function TdxNativeListLevel.GetParagraphStyle: IdxRichEditParagraphStyle;
var
  AStyles: TdxNativeParagraphStyleCollection;
begin
  AStyles := TdxNativeParagraphStyleCollection(FDocument.MainDocument.ParagraphStyles);
  Result := AStyles.GetStyle(InnerListLevel.ParagraphStyle);
end;

function TdxNativeListLevel.GetRelativeRestartLevel: Integer;
begin
  Result := InnerListLevel.ListLevelProperties.RelativeRestartLevel;
end;

function TdxNativeListLevel.GetSeparator: Char;
begin
  Result := InnerListLevel.ListLevelProperties.Separator;
end;

function TdxNativeListLevel.GetStart: Integer;
begin
  Result := InnerListLevel.ListLevelProperties.Start;
end;

function TdxNativeListLevel.GetSuppressBulletResize: Boolean;
begin
  Result := InnerListLevel.ListLevelProperties.SuppressBulletResize;
end;

function TdxNativeListLevel.GetSuppressRestart: Boolean;
begin
  Result := InnerListLevel.ListLevelProperties.SuppressRestart;
end;

procedure TdxNativeListLevel.SetConvertPreviousLevelNumberingToDecimal(
  const Value: Boolean);
begin
  InnerListLevel.ListLevelProperties.ConvertPreviousLevelNumberingToDecimal := Value;
end;

procedure TdxNativeListLevel.SetDisplayFormatString(const Value: string);
begin
  InnerListLevel.ListLevelProperties.DisplayFormatString := Value;
end;

procedure TdxNativeListLevel.SetNumberingFormat(
  const Value: TdxRichEditNumberingFormat);
begin
  if NumberingFormat = Value then
    Exit;
  InnerListLevel.ListLevelProperties.Format := Value;
end;

procedure TdxNativeListLevel.SetParagraphStyle(
  const Value: IdxRichEditParagraphStyle);
var
  AStyle: TdxParagraphStyle;
begin
  if Value = nil then
    AStyle := nil
  else
    AStyle := TdxNativeParagraphStyle(Value).InnerStyle;
  TdxListLevel(InnerListLevel).ParagraphStyleIndex := FDocument.DocumentModel.ParagraphStyles.IndexOf(AStyle);
end;

procedure TdxNativeListLevel.SetRelativeRestartLevel(const Value: Integer);
begin
  InnerListLevel.ListLevelProperties.RelativeRestartLevel := Value;
end;

procedure TdxNativeListLevel.SetSeparator(const Value: Char);
begin
  InnerListLevel.ListLevelProperties.Separator := Value;
end;

procedure TdxNativeListLevel.SetStart(const Value: Integer);
begin
  InnerListLevel.ListLevelProperties.Start := Value;
end;

procedure TdxNativeListLevel.SetSuppressBulletResize(const Value: Boolean);
begin
  InnerListLevel.ListLevelProperties.SuppressBulletResize := Value;
end;

procedure TdxNativeListLevel.SetSuppressRestart(const Value: Boolean);
begin
  InnerListLevel.ListLevelProperties.SuppressRestart := Value;
end;

{ TdxNativeOverrideListLevel }

constructor TdxNativeOverrideListLevel.Create(ADocument: TdxNativeDocument; const AListLevel: IdxListLevel);
begin
  inherited Create(ADocument, AListLevel);
  FModelOverrideListLevel := AListLevel as IdxOverrideListLevel;
end;

function TdxNativeOverrideListLevel.GetOverrideStart: Boolean;
begin
  Result := FModelOverrideListLevel.OverrideStart;
end;

function TdxNativeOverrideListLevel.GetNewStart: Integer;
begin
  Result := FModelOverrideListLevel.NewStart;
end;

procedure TdxNativeOverrideListLevel.SetNewStart(const Value: Integer);
begin
  FModelOverrideListLevel.NewStart := Value;
end;

procedure TdxNativeOverrideListLevel.SetOverrideStart(Value: Boolean);
begin
  FModelOverrideListLevel.SetOverrideStart(Value);
end;


{ TdxNativeAbstractNumberingListCollection }

constructor TdxNativeAbstractNumberingListCollection.Create(
  ADocument: TdxNativeDocument);
begin
  inherited Create(ADocument);
  FLastIndex := -1;
end;

procedure TdxNativeAbstractNumberingListCollection.AddCore(
  const AInnerList: TdxAbstractNumberingList);
begin
  if not InnerModelLists.Contains(AInnerList) then
    DocumentModel.AddAbstractNumberingListUsingHistory(AInnerList);
end;

function TdxNativeAbstractNumberingListCollection.CreateList(
  AModelListIndex: Integer): IdxRichEditAbstractNumberingList;
begin
  Result := TdxNativeAbstractNumberingList.Create(Document, AModelListIndex);
end;

function TdxNativeAbstractNumberingListCollection.CreateModelList: TdxAbstractNumberingList;
begin
  Result := TdxAbstractNumberingList.Create(DocumentModel);
  DocumentModel.AddAbstractNumberingListUsingHistory(Result);
  Result.SetId(Result.GenerateNewId);
end;

function TdxNativeAbstractNumberingListCollection.GetBulletedListTemplate: IdxRichEditTemplateAbstractNumberingList;
begin
  if FBulletedListTemplate = nil then
    FBulletedListTemplate := TdxNativeTemplateAbstractNumberingList.Create(Document, TdxRichEditNumberingType.Bullet);
  Result := FBulletedListTemplate;
end;

function TdxNativeAbstractNumberingListCollection.GetCount: Integer;
begin
  Result := InnerModelLists.Count;
end;

function TdxNativeAbstractNumberingListCollection.GetInnerLists: TdxList<TdxAbstractNumberingList>;
begin
  Result := DocumentModel.AbstractNumberingLists;
end;

function TdxNativeAbstractNumberingListCollection.GetInnerModelLists: TdxAbstractNumberingListCollection;
begin
  Result := DocumentModel.AbstractNumberingLists;
end;

function TdxNativeAbstractNumberingListCollection.GetModelList(
  const AList: IdxRichEditAbstractNumberingList): TdxAbstractNumberingList;
begin
  Result := TdxNativeAbstractNumberingList(AList).InnerAbstractNumberingList;
end;

function TdxNativeAbstractNumberingListCollection.GetModelListId(
  const AList: TdxAbstractNumberingList): Integer;
begin
  Result := AList.Id;
end;

function TdxNativeAbstractNumberingListCollection.GetModelListIndex(
  const AList: TdxAbstractNumberingList): Integer;
begin
  Result := InnerModelLists.IndexOf(AList);
end;

function TdxNativeAbstractNumberingListCollection.GetMultiLevelListTemplate: IdxRichEditTemplateAbstractNumberingList;
begin
  if FMultiLevelListTemplate = nil then
    FMultiLevelListTemplate := TdxNativeTemplateAbstractNumberingList.Create(Document, TdxRichEditNumberingType.MultiLevel);
  Result := FMultiLevelListTemplate;
end;

function TdxNativeAbstractNumberingListCollection.GetNumberedListTemplate: IdxRichEditTemplateAbstractNumberingList;
begin
  if FNumberedListTemplate = nil then
    FNumberedListTemplate := TdxNativeTemplateAbstractNumberingList.Create(Document, TdxRichEditNumberingType.Simple);
  Result := FNumberedListTemplate;
end;

{ TdxNativeAbstractNumberingList }

constructor TdxNativeAbstractNumberingList.Create(ADocument: TdxNativeDocument; AListIndex: TdxAbstractNumberingListIndex);
begin
  inherited Create(ADocument);
  Assert(AListIndex >= 0);
  FInnerListIndex := AListIndex;
end;

function TdxNativeAbstractNumberingList.GetInnerAbstractNumberingList: TdxAbstractNumberingList;
begin
  Result := DocumentModel.AbstractNumberingLists[FInnerListIndex];
end;

function TdxNativeAbstractNumberingList.GetIndex: Integer;
begin
  Result := FInnerListIndex;
end;

{ TdxNativeTemplateAbstractNumberingList }

constructor TdxNativeTemplateAbstractNumberingList.Create(ADocument: TdxNativeDocument; AType: TdxNumberingType);
begin
  inherited Create(ADocument);
  FType := AType;
end;

function TdxNativeTemplateAbstractNumberingList.GetTemplatesList: TdxAbstractNumberingListCollection;
begin
  Result := Document.DocumentServer.DocumentModelTemplate.AbstractNumberingLists;
end;

function TdxNativeTemplateAbstractNumberingList.GetInnerAbstractNumberingList: TdxAbstractNumberingList;
begin
  Result := TdxNumberingListHelper.GetAbstractListByType(TemplatesList, FType);
end;

function TdxNativeTemplateAbstractNumberingList.CreateNew: IdxRichEditAbstractNumberingList;
var
  ATemplateList: TdxAbstractNumberingList;
  ACalc: TdxNumberingListIndexCalculator;
  AListIndex: TdxAbstractNumberingListIndex;
begin
  ATemplateList := TemplatesList[TdxNumberingListHelper.GetAbstractListIndexByType(TemplatesList, FType)];
  ACalc := TdxNumberingListIndexCalculator.Create(DocumentModel, TdxNumberingListHelper.GetListType(ATemplateList));
  try
    AListIndex := ACalc.CreateNewAbstractList(ATemplateList);
    Result := TdxNativeAbstractNumberingList.Create(Document, AListIndex);
  finally
    ACalc.Free;
  end;
end;


end.
